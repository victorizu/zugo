;; Multi-Send with Detailed Receipts Smart Contract
;; Enables batch token transfers with comprehensive on-chain audit trails

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-batch (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-transfer-failed (err u103))
(define-constant err-batch-not-found (err u104))
(define-constant err-invalid-recipient (err u105))
(define-constant err-zero-amount (err u106))

;; Data Variables
(define-data-var batch-counter uint u0)
(define-data-var contract-paused bool false)

;; Transfer details structure
(define-map transfer-details
  { batch-id: uint, transfer-index: uint }
  {
    recipient: principal,
    amount: uint,
    memo: (string-ascii 128),
    status: (string-ascii 20),
    block-height: uint
  }
)

;; Batch summary structure
(define-map batch-summary
  { batch-id: uint }
  {
    sender: principal,
    total-amount: uint,
    transfer-count: uint,
    successful-transfers: uint,
    failed-transfers: uint,
    block-height: uint,
    batch-status: (string-ascii 20)
  }
)

;; Batch receipts for complete audit trail
(define-map batch-receipts
  { batch-id: uint }
  {
    gas-used: uint,
    total-fees: uint,
    block-height: uint
  }
)

;; User batch history
(define-map user-batches
  { user: principal, batch-index: uint }
  { batch-id: uint }
)

(define-map user-batch-count
  { user: principal }
  { count: uint }
)

;; Events for external monitoring
(define-map batch-events
  { batch-id: uint, event-type: (string-ascii 50) }
  {
    event-data: (string-ascii 256),
    block-height: uint
  }
)

;; Helper Functions

;; Get next batch ID
(define-private (get-next-batch-id)
  (let ((current-id (var-get batch-counter)))
    (var-set batch-counter (+ current-id u1))
    (+ current-id u1)
  )
)

;; Record user batch
(define-private (record-user-batch (user principal) (batch-id uint))
  (let ((current-count (default-to u0 (get count (map-get? user-batch-count { user: user })))))
    (map-set user-batches { user: user, batch-index: current-count } { batch-id: batch-id })
    (map-set user-batch-count { user: user } { count: (+ current-count u1) })
    (ok true)
  )
)

;; Log batch event
(define-private (log-batch-event (batch-id uint) (event-type (string-ascii 50)) (event-data (string-ascii 256)))
  (map-set batch-events 
    { batch-id: batch-id, event-type: event-type }
    {
      event-data: event-data,
      block-height: block-height
    }
  )
)

;; Process individual transfer
(define-private (process-transfer 
  (batch-id uint) 
  (transfer-index uint) 
  (recipient principal) 
  (amount uint) 
  (memo (string-ascii 128))
)
  (let ((transfer-result (stx-transfer? amount tx-sender recipient)))
    (if (is-ok transfer-result)
      (begin
        (map-set transfer-details
          { batch-id: batch-id, transfer-index: transfer-index }
          {
            recipient: recipient,
            amount: amount,
            memo: memo,
            status: "SUCCESS",
            block-height: block-height
          }
        )
        (ok true)
      )
      (begin
        (map-set transfer-details
          { batch-id: batch-id, transfer-index: transfer-index }
          {
            recipient: recipient,
            amount: amount,
            memo: memo,
            status: "FAILED",
            block-height: block-height
          }
        )
        (ok false)
      )
    )
  )
)

;; Public Functions

;; Execute multi-send batch
(define-public (execute-multi-send 
  (recipients (list 50 principal))
  (amounts (list 50 uint))
  (memos (list 50 (string-ascii 128)))
)
  (let (
    (batch-id (get-next-batch-id))
    (recipient-count (len recipients))
    (amount-count (len amounts))
    (memo-count (len memos))
    (total-amount (fold + amounts u0))
  )
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (and (is-eq recipient-count amount-count) (is-eq amount-count memo-count)) err-invalid-batch)
    (asserts! (> recipient-count u0) err-invalid-batch)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) err-insufficient-balance)
    
    ;; Log batch start event
    (log-batch-event batch-id "BATCH_STARTED" "Multi-send batch execution initiated")
    
    ;; Record user batch
    (unwrap! (record-user-batch tx-sender batch-id) err-invalid-batch)
    
    ;; Process transfers
    (let ((results (process-batch-transfers batch-id recipients amounts memos u0)))
      (let (
        (successful-count (get successful results))
        (failed-count (get failed results))
        (batch-status (if (is-eq failed-count u0) "COMPLETED" "PARTIAL"))
      )
        ;; Create batch summary
        (map-set batch-summary
          { batch-id: batch-id }
          {
            sender: tx-sender,
            total-amount: total-amount,
            transfer-count: recipient-count,
            successful-transfers: successful-count,
            failed-transfers: failed-count,
            block-height: block-height,
            batch-status: batch-status
          }
        )
        
        ;; Create batch receipt
        (map-set batch-receipts
          { batch-id: batch-id }
          {
            gas-used: u0, ;; Would be populated by runtime in a real implementation
            total-fees: u0, ;; Would be populated by runtime in a real implementation
            block-height: block-height
          }
        )
        
        ;; Log completion event
        (log-batch-event batch-id "BATCH_COMPLETED" 
          (concat (concat "Batch completed with " (int-to-ascii successful-count)) " successful transfers"))
        
        (ok { batch-id: batch-id, successful: successful-count, failed: failed-count })
      )
    )
  )
)

;; Process batch transfers using map and fold
(define-private (process-batch-transfers 
  (batch-id uint)
  (recipients (list 50 principal))
  (amounts (list 50 uint))
  (memos (list 50 (string-ascii 128)))
  (index uint)
)
  (let (
    (transfer-results (process-transfers-with-index batch-id recipients amounts memos u0))
  )
    transfer-results
  )
)

;; Process transfers with index tracking
(define-private (process-transfers-with-index
  (batch-id uint)
  (recipients (list 50 principal))
  (amounts (list 50 uint))
  (memos (list 50 (string-ascii 128)))
  (start-index uint)
)
  (fold process-transfer-item
    recipients
    {
      batch-id: batch-id,
      amounts: amounts,
      memos: memos,
      index: start-index,
      successful: u0,
      failed: u0
    }
  )
)

;; Process single transfer item in fold
(define-private (process-transfer-item
  (recipient principal)
  (acc { batch-id: uint, amounts: (list 50 uint), memos: (list 50 (string-ascii 128)), index: uint, successful: uint, failed: uint })
)
  (let (
    (batch-id (get batch-id acc))
    (current-index (get index acc))
    (amounts-list (get amounts acc))
    (memos-list (get memos acc))
    (amount (unwrap-panic (element-at amounts-list current-index)))
    (memo (unwrap-panic (element-at memos-list current-index)))
  )
    (let ((transfer-result (process-transfer batch-id current-index recipient amount memo)))
      (if (is-ok transfer-result)
        (if (unwrap-panic transfer-result)
          {
            batch-id: batch-id,
            amounts: amounts-list,
            memos: memos-list,
            index: (+ current-index u1),
            successful: (+ (get successful acc) u1),
            failed: (get failed acc)
          }
          {
            batch-id: batch-id,
            amounts: amounts-list,
            memos: memos-list,
            index: (+ current-index u1),
            successful: (get successful acc),
            failed: (+ (get failed acc) u1)
          }
        )
        {
          batch-id: batch-id,
          amounts: amounts-list,
          memos: memos-list,
          index: (+ current-index u1),
          successful: (get successful acc),
          failed: (+ (get failed acc) u1)
        }
      )
    )
  )
)

;; Read-only Functions

;; Get batch summary
(define-read-only (get-batch-summary (batch-id uint))
  (map-get? batch-summary { batch-id: batch-id })
)

;; Get transfer details
(define-read-only (get-transfer-details (batch-id uint) (transfer-index uint))
  (map-get? transfer-details { batch-id: batch-id, transfer-index: transfer-index })
)

;; Get batch receipt
(define-read-only (get-batch-receipt (batch-id uint))
  (map-get? batch-receipts { batch-id: batch-id })
)

;; Get user batch history
(define-read-only (get-user-batch-history (user principal) (batch-index uint))
  (map-get? user-batches { user: user, batch-index: batch-index })
)

;; Get user batch count
(define-read-only (get-user-batch-count (user principal))
  (default-to u0 (get count (map-get? user-batch-count { user: user })))
)

;; Get batch events
(define-read-only (get-batch-events (batch-id uint) (event-type (string-ascii 50)))
  (map-get? batch-events { batch-id: batch-id, event-type: event-type })
)

;; Get current batch counter
(define-read-only (get-batch-counter)
  (var-get batch-counter)
)

;; Get complete audit trail for a batch
(define-read-only (get-complete-audit-trail (batch-id uint))
  (let (
    (summary (map-get? batch-summary { batch-id: batch-id }))
    (receipt (map-get? batch-receipts { batch-id: batch-id }))
  )
    (match summary
      batch-data (some {
        summary: batch-data,
        receipt: receipt,
        batch-exists: true
      })
      none
    )
  )
)

;; Administrative Functions

;; Pause contract (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Unpause contract (owner only)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused false)
    (ok true)
  )
)