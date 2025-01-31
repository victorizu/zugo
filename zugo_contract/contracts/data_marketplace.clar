
;; title: data_marketplace
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Data Marketplace Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-bid (err u103))

;; Data structures
(define-map data-requests 
  { request-id: uint }
  {
    company: principal,
    budget: uint,
    purpose: (string-ascii 256),
    timeframe: uint,
    status: (string-ascii 20)
  }
)

(define-map bids
  { request-id: uint, bidder: principal }
  {
    amount: uint,
    quality-score: uint
  }
)

;; Variables
(define-data-var request-id-nonce uint u0)

;; Private functions
(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

;; Public functions
(define-public (create-data-request (budget uint) (purpose (string-ascii 256)) (timeframe uint))
  (let
    (
      (new-id (+ (var-get request-id-nonce) u1))
    )
    (map-set data-requests
      { request-id: new-id }
      {
        company: tx-sender,
        budget: budget,
        purpose: purpose,
        timeframe: timeframe,
        status: "open"
      }
    )
    (var-set request-id-nonce new-id)
    (ok new-id)
  )
)

(define-public (place-bid (request-id uint) (amount uint) (quality-score uint))
  (let
    (
      (request (unwrap! (map-get? data-requests { request-id: request-id }) err-not-found))
    )
    (asserts! (is-eq (get status request) "open") err-invalid-bid)
    (asserts! (<= amount (get budget request)) err-invalid-bid)
    (map-set bids
      { request-id: request-id, bidder: tx-sender }
      {
        amount: amount,
        quality-score: quality-score
      }
    )
    (ok true)
  )
)

(define-public (accept-bid (request-id uint) (bidder principal))
  (let
    (
      (request (unwrap! (map-get? data-requests { request-id: request-id }) err-not-found))
      (bid (unwrap! (map-get? bids { request-id: request-id, bidder: bidder }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get company request)) err-owner-only)
    (asserts! (is-eq (get status request) "open") err-invalid-bid)
    (map-set data-requests
      { request-id: request-id }
      (merge request { status: "accepted" })
    )
    (ok true)
  )
)

(define-read-only (get-data-request (request-id uint))
  (map-get? data-requests { request-id: request-id })
)

(define-read-only (get-bid (request-id uint) (bidder principal))
  (map-get? bids { request-id: request-id, bidder: bidder })
)

;; Automated pricing calculator
(define-read-only (calculate-suggested-price (request-id uint) (quality-score uint))
  (match (map-get? data-requests { request-id: request-id })
    request 
      (let
        (
          (base-price (/ (get budget request) u10))  ;; 10% of the budget as base price
          (quality-multiplier (/ quality-score u100))  ;; Quality score as a multiplier (0.0 to 1.0)
        )
        (ok (+ base-price (* base-price quality-multiplier)))
      )
    (err err-not-found)
  )
)