;; Enhanced Storage with Reset Option Smart Contract
;; Advanced storage contract with multiple features and security mechanisms

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant DEFAULT_VALUE u0)
(define-constant MAX_VALUE u1000000)
(define-constant MIN_VALUE u0)

;; Error constants
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_INVALID_VALUE (err u101))
(define-constant ERR_CONTRACT_PAUSED (err u102))
(define-constant ERR_UNAUTHORIZED (err u103))
(define-constant ERR_VALUE_TOO_HIGH (err u104))
(define-constant ERR_VALUE_TOO_LOW (err u105))
(define-constant ERR_ALREADY_AUTHORIZED (err u106))
(define-constant ERR_NOT_AUTHORIZED (err u107))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u108))

;; Data storage
(define-data-var stored-value uint DEFAULT_VALUE)
(define-data-var last-value uint DEFAULT_VALUE)
(define-data-var contract-paused bool false)
(define-data-var update-count uint u0)
(define-data-var last-updated-by (optional principal) none)
(define-data-var last-update-block uint u0)
(define-data-var value-history (list 10 uint) (list))
(define-data-var min-payment uint u1000) ;; Minimum STX payment for updates

;; Maps for access control and tracking
(define-map authorized-users principal bool)
(define-map user-update-count principal uint)
(define-map value-timestamps uint uint) ;; value -> block height

;; Events (using print for logging)
(define-private (log-value-change (old-value uint) (new-value uint) (updater principal))
  (print {
    event: "value-changed",
    old-value: old-value,
    new-value: new-value,
    updater: updater,
    block-height: block-height,
    timestamp: block-height
  }))

(define-private (log-authorization (user principal) (authorized bool))
  (print {
    event: "authorization-changed",
    user: user,
    authorized: authorized,
    block-height: block-height
  }))

;; Private utility functions
(define-private (is-owner)
  (is-eq tx-sender CONTRACT_OWNER))

(define-private (is-authorized)
  (or (is-owner) 
      (default-to false (map-get? authorized-users tx-sender))))

(define-private (is-contract-active)
  (not (var-get contract-paused)))



(define-private (update-history (new-value uint))
  (var-set last-value (var-get stored-value)))

(define-private (increment-user-count (user principal))
  (let ((current-count (default-to u0 (map-get? user-update-count user))))
    (map-set user-update-count user (+ current-count u1))))

;; Read-only functions (Getters)
(define-read-only (get-stored-value)
  (var-get stored-value))

(define-read-only (get-owner)
  CONTRACT_OWNER)

(define-read-only (get-default-value)
  DEFAULT_VALUE)

(define-read-only (get-contract-status)
  {
    paused: (var-get contract-paused),
    total-updates: (var-get update-count),
    last-updated-by: (var-get last-updated-by),
    last-update-block: (var-get last-update-block),
    current-value: (var-get stored-value)
  })

(define-read-only (get-value-history)
  (list (var-get last-value) (var-get stored-value)))

(define-read-only (get-user-stats (user principal))
  {
    is-authorized: (default-to false (map-get? authorized-users user)),
    update-count: (default-to u0 (map-get? user-update-count user)),
    is-owner: (is-eq user CONTRACT_OWNER)
  })

(define-read-only (is-value-in-range (value uint))
  (and (>= value MIN_VALUE) (<= value MAX_VALUE)))

(define-read-only (get-min-payment)
  (var-get min-payment))

(define-read-only (caller-is-owner)
  (is-owner))

(define-read-only (caller-is-authorized)
  (is-authorized))

;; Public functions (Setters and Actions)

;; Basic setter - requires authorization and payment
(define-public (set-stored-value (new-value uint))
  (let ((old-value (var-get stored-value))
        (payment-amount (var-get min-payment)))
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-authorized) ERR_UNAUTHORIZED)
    (asserts! (is-value-in-range new-value) ERR_INVALID_VALUE)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer payment to contract owner
    (try! (stx-transfer? payment-amount tx-sender CONTRACT_OWNER))
    
    ;; Update storage
    (var-set stored-value new-value)
    (var-set update-count (+ (var-get update-count) u1))
    (var-set last-updated-by (some tx-sender))
    (var-set last-update-block block-height)
    (map-set value-timestamps new-value block-height)
    
    ;; Update tracking
    (update-history new-value)
    (increment-user-count tx-sender)
    
    ;; Log event
    (log-value-change old-value new-value tx-sender)
    
    (ok new-value)))

;; Owner-only setter (no payment required)
(define-public (owner-set-value (new-value uint))
  (let ((old-value (var-get stored-value)))
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (asserts! (is-value-in-range new-value) ERR_INVALID_VALUE)
    
    (var-set stored-value new-value)
    (var-set update-count (+ (var-get update-count) u1))
    (var-set last-updated-by (some tx-sender))
    (var-set last-update-block block-height)
    (map-set value-timestamps new-value block-height)
    
    (update-history new-value)
    (increment-user-count tx-sender)
    (log-value-change old-value new-value tx-sender)
    
    (ok new-value)))

;; Reset to default (owner only)
(define-public (reset-to-default)
  (let ((old-value (var-get stored-value)))
    (asserts! (is-owner) ERR_OWNER_ONLY)
    
    (var-set stored-value DEFAULT_VALUE)
    (var-set update-count (+ (var-get update-count) u1))
    (var-set last-updated-by (some tx-sender))
    (var-set last-update-block block-height)
    (map-set value-timestamps DEFAULT_VALUE block-height)
    
    (update-history DEFAULT_VALUE)
    (increment-user-count tx-sender)
    (log-value-change old-value DEFAULT_VALUE tx-sender)
    
    (ok DEFAULT_VALUE)))

;; Batch update multiple values (owner only)
(define-public (batch-update (values (list 5 uint)))
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    
    ;; Set to the last value in the list
    (match (element-at values (- (len values) u1))
      final-value (owner-set-value final-value)
      (err u999))))

;; Increment stored value by amount
(define-public (increment-value (amount uint))
  (let ((current-value (var-get stored-value))
        (new-value (+ current-value amount)))
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-authorized) ERR_UNAUTHORIZED)
    (asserts! (is-value-in-range new-value) ERR_INVALID_VALUE)
    
    (set-stored-value new-value)))

;; Decrement stored value by amount
(define-public (decrement-value (amount uint))
  (let ((current-value (var-get stored-value))
        (new-value (if (>= current-value amount) 
                     (- current-value amount) 
                     u0)))
    (asserts! (is-contract-active) ERR_CONTRACT_PAUSED)
    (asserts! (is-authorized) ERR_UNAUTHORIZED)
    
    (set-stored-value new-value)))

;; Access control functions
(define-public (authorize-user (user principal))
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (asserts! (not (default-to false (map-get? authorized-users user))) ERR_ALREADY_AUTHORIZED)
    
    (map-set authorized-users user true)
    (log-authorization user true)
    (ok true)))

(define-public (revoke-user (user principal))
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (asserts! (default-to false (map-get? authorized-users user)) ERR_NOT_AUTHORIZED)
    
    (map-set authorized-users user false)
    (log-authorization user false)
    (ok true)))

;; Contract control functions
(define-public (pause-contract)
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (var-set contract-paused true)
    (print {event: "contract-paused", block-height: block-height})
    (ok true)))

(define-public (unpause-contract)
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (var-set contract-paused false)
    (print {event: "contract-unpaused", block-height: block-height})
    (ok true)))

(define-public (set-min-payment (new-amount uint))
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (var-set min-payment new-amount)
    (print {event: "min-payment-updated", new-amount: new-amount})
    (ok new-amount)))

;; Emergency functions
(define-public (emergency-reset)
  (begin
    (asserts! (is-owner) ERR_OWNER_ONLY)
    
    ;; Reset all values
    (var-set stored-value DEFAULT_VALUE)
    (var-set update-count u0)
    (var-set last-updated-by none)
    (var-set last-update-block u0)
    (var-set last-value DEFAULT_VALUE)
    (var-set contract-paused false)
    
    (print {event: "emergency-reset", block-height: block-height})
    (ok true)))

;; Withdraw contract balance (owner only)
(define-public (withdraw-balance)
  (let ((contract-balance (stx-get-balance (as-contract tx-sender))))
    (asserts! (is-owner) ERR_OWNER_ONLY)
    (asserts! (> contract-balance u0) (err u999))
    
    (as-contract (stx-transfer? contract-balance tx-sender CONTRACT_OWNER))))

;; Advanced query functions
(define-read-only (get-previous-value)
  (var-get last-value))

;; Statistics functions
(define-read-only (get-contract-stats)
  {
    total-updates: (var-get update-count),
    unique-values: u2, ;; simplified - just current and last
    contract-age: (- block-height u0),
    is-paused: (var-get contract-paused),
    current-min-payment: (var-get min-payment)
  })