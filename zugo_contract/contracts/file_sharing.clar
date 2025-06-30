;; Enhanced FileSharing Smart Contract
;; Advanced decentralized file sharing with comprehensive features

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-expired (err u103))
(define-constant err-invalid-expiry (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-params (err u106))
(define-constant err-storage-limit (err u107))
(define-constant err-insufficient-payment (err u108))
(define-constant err-download-limit (err u109))
(define-constant err-invalid-category (err u110))
(define-constant err-banned-user (err u111))

;; Configuration
(define-constant max-file-size u1000000000) ;; 1GB in bytes
(define-constant max-files-per-user u1000)
(define-constant base-storage-cost u1000) ;; STX per MB
(define-constant admin-fee-percentage u5) ;; 5%

;; Data Variables
(define-data-var next-token-id uint u1)
(define-data-var next-file-id uint u1)
(define-data-var next-folder-id uint u1)
(define-data-var platform-fee-collector principal contract-owner)
(define-data-var total-platform-revenue uint u0)

;; Data Maps
;; Store file metadata with enhanced features
(define-map files
  { file-id: uint }
  {
    owner: principal,
    file-hash: (string-ascii 64),
    file-name: (string-ascii 256),
    file-size: uint,
    file-type: (string-ascii 32),
    category: (string-ascii 32),
    description: (string-ascii 512),
    tags: (list 10 (string-ascii 32)),
    folder-id: (optional uint),
    created-at: uint,
    updated-at: uint,
    is-active: bool,
    is-public: bool,
    download-count: uint,
    view-count: uint,
    storage-cost: uint,
    encryption-key: (optional (string-ascii 64))
  }
)

;; Enhanced access tokens with more granular permissions
(define-map access-tokens
  { token-id: uint }
  {
    file-id: uint,
    granted-by: principal,
    granted-to: (optional principal),
    token-hash: (string-ascii 64),
    expires-at: uint,
    created-at: uint,
    is-active: bool,
    access-count: uint,
    max-uses: (optional uint),
    permissions: {
      can-view: bool,
      can-download: bool,
      can-share: bool,
      can-comment: bool
    },
    price: uint,
    payment-required: bool
  }
)

;; Folder structure for file organization
(define-map folders
  { folder-id: uint }
  {
    owner: principal,
    name: (string-ascii 128),
    parent-folder: (optional uint),
    created-at: uint,
    is-active: bool,
    file-count: uint
  }
)

;; User profiles and reputation system
(define-map user-profiles
  { user: principal }
  {
    username: (string-ascii 64),
    reputation-score: uint,
    total-uploads: uint,
    total-downloads: uint,
    storage-used: uint,
    storage-limit: uint,
    is-premium: bool,
    is-banned: bool,
    joined-at: uint
  }
)

;; File comments and reviews
(define-map file-comments
  { file-id: uint, comment-id: uint }
  {
    commenter: principal,
    content: (string-ascii 512),
    rating: uint,
    timestamp: uint,
    is-active: bool
  }
)

;; Track file comment counts
(define-map file-comment-counts
  { file-id: uint }
  { count: uint, next-comment-id: uint }
)

;; File sharing statistics
(define-map file-stats
  { file-id: uint }
  {
    unique-viewers: uint,
    total-revenue: uint,
    avg-rating: uint,
    rating-count: uint
  }
)

;; Enhanced access logs with more details
(define-map access-logs
  { file-id: uint, accessor: principal, timestamp: uint }
  {
    token-id: uint,
    access-type: (string-ascii 32),
    ip-hash: (optional (string-ascii 64)),
    user-agent-hash: (optional (string-ascii 64)),
    download-completed: bool
  }
)

;; Favorites system
(define-map favorites
  { user: principal, file-id: uint }
  { added-at: uint }
)

;; File categories
(define-map categories
  { category: (string-ascii 32) }
  { file-count: uint, is-active: bool }
)

;; Payment tracking
(define-map payments
  { payer: principal, token-id: uint, timestamp: uint }
  { amount: uint, status: (string-ascii 16) }
)

;; Bulk operations tracking
(define-map bulk-operations
  { operation-id: uint, user: principal }
  {
    operation-type: (string-ascii 32),
    file-ids: (list 50 uint),
    status: (string-ascii 16),
    created-at: uint
  }
)

;; Lookup maps
(define-map user-files { owner: principal, file-id: uint } { exists: bool })
(define-map token-lookup { token-hash: (string-ascii 64) } { token-id: uint })
(define-map username-lookup { username: (string-ascii 64) } { user: principal })

;; Public Functions

;; Enhanced file registration with categories and metadata
(define-public (register-file 
  (file-hash (string-ascii 64)) 
  (file-name (string-ascii 256)) 
  (file-size uint)
  (file-type (string-ascii 32))
  (category (string-ascii 32))
  (description (string-ascii 512))
  (tags (list 10 (string-ascii 32)))
  (folder-id (optional uint))
  (is-public bool)
  (encryption-key (optional (string-ascii 64)))
)
  (let
    (
      (file-id (var-get next-file-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (storage-cost (* (/ file-size u1000000) base-storage-cost))
      (user-profile (get-user-profile tx-sender))
    )
    ;; Validate inputs
    (asserts! (> (len file-hash) u0) err-invalid-params)
    (asserts! (> (len file-name) u0) err-invalid-params)
    (asserts! (<= file-size max-file-size) err-storage-limit)
    (asserts! (not (get is-banned (default-to { username: "", reputation-score: u0, total-uploads: u0, total-downloads: u0, storage-used: u0, storage-limit: u10000000000, is-premium: false, is-banned: false, joined-at: current-time } user-profile))) err-banned-user)
    
    ;; Verify folder ownership if specified
    (match folder-id
      f-id (let ((folder-data (unwrap! (map-get? folders { folder-id: f-id }) err-not-found)))
        (asserts! (is-eq (get owner folder-data) tx-sender) err-unauthorized))
      true
    )
    
    ;; Store file metadata
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        file-hash: file-hash,
        file-name: file-name,
        file-size: file-size,
        file-type: file-type,
        category: category,
        description: description,
        tags: tags,
        folder-id: folder-id,
        created-at: current-time,
        updated-at: current-time,
        is-active: true,
        is-public: is-public,
        download-count: u0,
        view-count: u0,
        storage-cost: storage-cost,
        encryption-key: encryption-key
      }
    )
    
    ;; Update mappings
    (map-set user-files { owner: tx-sender, file-id: file-id } { exists: true })
    
    ;; Update category count
    (map-set categories
      { category: category }
      (merge 
        (default-to { file-count: u0, is-active: true } (map-get? categories { category: category }))
        { file-count: (+ (get file-count (default-to { file-count: u0, is-active: true } (map-get? categories { category: category }))) u1) }
      )
    )
    
    ;; Update user profile
    (unwrap! (update-user-stats tx-sender u1 u0 file-size) err-invalid-params)
    
    ;; Update folder file count
    (match folder-id
      f-id (let ((folder-data (unwrap-panic (map-get? folders { folder-id: f-id }))))
        (map-set folders 
          { folder-id: f-id }
          (merge folder-data { file-count: (+ (get file-count folder-data) u1) })))
      true
    )
    
    ;; Increment file ID counter
    (var-set next-file-id (+ file-id u1))
    
    (ok file-id)
  )
)

;; Create folder for file organization
(define-public (create-folder (name (string-ascii 128)) (parent-folder (optional uint)))
  (let
    (
      (folder-id (var-get next-folder-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Validate parent folder ownership if specified
    (match parent-folder
      p-id (let ((parent-data (unwrap! (map-get? folders { folder-id: p-id }) err-not-found)))
        (asserts! (is-eq (get owner parent-data) tx-sender) err-unauthorized))
      true
    )
    
    (map-set folders
      { folder-id: folder-id }
      {
        owner: tx-sender,
        name: name,
        parent-folder: parent-folder,
        created-at: current-time,
        is-active: true,
        file-count: u0
      }
    )
    
    (var-set next-folder-id (+ folder-id u1))
    (ok folder-id)
  )
)

;; Enhanced access token generation with permissions and pricing
(define-public (generate-access-token 
  (file-id uint) 
  (token-hash (string-ascii 64)) 
  (expires-at uint)
  (granted-to (optional principal))
  (max-uses (optional uint))
  (permissions { can-view: bool, can-download: bool, can-share: bool, can-comment: bool })
  (price uint)
)
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
      (token-id (var-get next-token-id))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Verify file owner
    (asserts! (is-eq (get owner file-data) tx-sender) err-unauthorized)
    (asserts! (get is-active file-data) err-not-found)
    (asserts! (> expires-at current-time) err-invalid-expiry)
    (asserts! (is-none (map-get? token-lookup { token-hash: token-hash })) err-already-exists)
    
    ;; Create enhanced access token
    (map-set access-tokens
      { token-id: token-id }
      {
        file-id: file-id,
        granted-by: tx-sender,
        granted-to: granted-to,
        token-hash: token-hash,
        expires-at: expires-at,
        created-at: current-time,
        is-active: true,
        access-count: u0,
        max-uses: max-uses,
        permissions: permissions,
        price: price,
        payment-required: (> price u0)
      }
    )
    
    (map-set token-lookup { token-hash: token-hash } { token-id: token-id })
    (var-set next-token-id (+ token-id u1))
    
    (ok token-id)
  )
)

;; Access file with payment support
(define-public (access-file (token-hash (string-ascii 64)) (access-type (string-ascii 32)))
  (let
    (
      (token-lookup-data (unwrap! (map-get? token-lookup { token-hash: token-hash }) err-not-found))
      (token-id (get token-id token-lookup-data))
      (token-data (unwrap! (map-get? access-tokens { token-id: token-id }) err-not-found))
      (file-data (unwrap! (map-get? files { file-id: (get file-id token-data) }) err-not-found))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Validate access
    (asserts! (get is-active token-data) err-unauthorized)
    (asserts! (> (get expires-at token-data) current-time) err-expired)
    (asserts! (get is-active file-data) err-not-found)
    
    ;; Check usage limits
    (match (get max-uses token-data)
      max-count (asserts! (< (get access-count token-data) max-count) err-download-limit)
      true
    )
    
    ;; Verify permissions
    (if (is-eq access-type "download")
      (asserts! (get can-download (get permissions token-data)) err-unauthorized)
      (asserts! (get can-view (get permissions token-data)) err-unauthorized)
    )
    
    ;; Handle payment if required
    (if (get payment-required token-data)
      (unwrap! (process-access-payment token-id (get price token-data)) err-insufficient-payment)
      true
    )
    
    ;; Log access
    (map-set access-logs
      { file-id: (get file-id token-data), accessor: tx-sender, timestamp: current-time }
      {
        token-id: token-id,
        access-type: access-type,
        ip-hash: none,
        user-agent-hash: none,
        download-completed: (is-eq access-type "download")
      }
    )
    
    ;; Update counters
    (map-set access-tokens
      { token-id: token-id }
      (merge token-data { access-count: (+ (get access-count token-data) u1) })
    )
    
    (if (is-eq access-type "download")
      (map-set files 
        { file-id: (get file-id token-data) }
        (merge file-data { download-count: (+ (get download-count file-data) u1) }))
      (map-set files 
        { file-id: (get file-id token-data) }
        (merge file-data { view-count: (+ (get view-count file-data) u1) }))
    )
    
    ;; Update user download stats
    (unwrap! (update-user-stats tx-sender u0 u1 u0) err-invalid-params)
    
    (ok {
      file-id: (get file-id token-data),
      file-hash: (get file-hash file-data),
      file-name: (get file-name file-data),
      file-size: (get file-size file-data),
      file-type: (get file-type file-data),
      encryption-key: (get encryption-key file-data)
    })
  )
)

;; Add file to favorites
(define-public (add-to-favorites (file-id uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (get is-active file-data) err-not-found)
    (asserts! (or (get is-public file-data) (is-eq (get owner file-data) tx-sender)) err-unauthorized)
    
    (map-set favorites
      { user: tx-sender, file-id: file-id }
      { added-at: current-time }
    )
    
    (ok true)
  )
)

;; Comment on a file with rating
(define-public (add-comment (file-id uint) (content (string-ascii 512)) (rating uint))
  (let
    (
      (file-data (unwrap! (map-get? files { file-id: file-id }) err-not-found))
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (comment-count-data (default-to { count: u0, next-comment-id: u1 } (map-get? file-comment-counts { file-id: file-id })))
      (comment-id (get next-comment-id comment-count-data))
    )
    (asserts! (get is-active file-data) err-not-found)
    (asserts! (<= rating u5) err-invalid-params)
    (asserts! (> (len content) u0) err-invalid-params)
    
    ;; Add comment
    (map-set file-comments
      { file-id: file-id, comment-id: comment-id }
      {
        commenter: tx-sender,
        content: content,
        rating: rating,
        timestamp: current-time,
        is-active: true
      }
    )
    
    ;; Update comment count
    (map-set file-comment-counts
      { file-id: file-id }
      { count: (+ (get count comment-count-data) u1), next-comment-id: (+ comment-id u1) }
    )
    
    ;; Update file stats
    (unwrap! (update-file-rating file-id rating) err-invalid-params)
    
    (ok comment-id)
  )
)

;; Bulk file operations
(define-public (bulk-delete-files (file-ids (list 50 uint)))
  (let
    (
      (operation-id (var-get next-file-id)) ;; Reuse counter for simplicity
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    ;; Verify ownership of all files
    (asserts! (is-eq (len file-ids) (len (filter verify-file-ownership file-ids))) err-unauthorized)
    
    ;; Deactivate all files
    (let ((deactivation-results (map deactivate-file-internal file-ids)))
      ;; Log bulk operation
      (map-set bulk-operations
        { operation-id: operation-id, user: tx-sender }
        {
          operation-type: "bulk-delete",
          file-ids: file-ids,
          status: "completed",
          created-at: current-time
        }
      )
      
      (ok true)
    )
  )
)

;; Set user profile
(define-public (set-user-profile (username (string-ascii 64)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (existing-profile (get-user-profile tx-sender))
    )
    ;; Check username availability
    (asserts! (is-none (map-get? username-lookup { username: username })) err-already-exists)
    
    (map-set user-profiles
      { user: tx-sender }
      (merge 
        (default-to { username: "", reputation-score: u100, total-uploads: u0, total-downloads: u0, storage-used: u0, storage-limit: u10000000000, is-premium: false, is-banned: false, joined-at: current-time } existing-profile)
        { username: username }
      )
    )
    
    (map-set username-lookup { username: username } { user: tx-sender })
    (ok true)
  )
)

;; Private Functions

;; Process payment for access
(define-private (process-access-payment (token-id uint) (amount uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (admin-fee (/ (* amount admin-fee-percentage) u100))
      (owner-payment (- amount admin-fee))
    )
    ;; Transfer payment (simplified - in real implementation, use STX transfer)
    ;; (try! (stx-transfer? amount tx-sender file-owner))
    
    ;; Record payment
    (map-set payments
      { payer: tx-sender, token-id: token-id, timestamp: current-time }
      { amount: amount, status: "completed" }
    )
    
    ;; Update platform revenue
    (var-set total-platform-revenue (+ (var-get total-platform-revenue) admin-fee))
    
    (ok true)
  )
)

;; Update user statistics
(define-private (update-user-stats (user principal) (uploads uint) (downloads uint) (storage uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (profile (default-to 
        { username: "", reputation-score: u100, total-uploads: u0, total-downloads: u0, storage-used: u0, storage-limit: u10000000000, is-premium: false, is-banned: false, joined-at: current-time }
        (map-get? user-profiles { user: user })
      ))
    )
    (map-set user-profiles
      { user: user }
      (merge profile {
        total-uploads: (+ (get total-uploads profile) uploads),
        total-downloads: (+ (get total-downloads profile) downloads),
        storage-used: (+ (get storage-used profile) storage),
        reputation-score: (+ (get reputation-score profile) uploads) ;; Simple reputation boost
      })
    )
    (ok true)
  )
)

;; Update file rating
(define-private (update-file-rating (file-id uint) (new-rating uint))
  (let
    (
      (stats (default-to { unique-viewers: u0, total-revenue: u0, avg-rating: u0, rating-count: u0 } (map-get? file-stats { file-id: file-id })))
      (total-score (+ (* (get avg-rating stats) (get rating-count stats)) new-rating))
      (new-count (+ (get rating-count stats) u1))
      (new-avg (/ total-score new-count))
    )
    (map-set file-stats
      { file-id: file-id }
      (merge stats { avg-rating: new-avg, rating-count: new-count })
    )
    (ok true)
  )
)

;; Verify file ownership for bulk operations
(define-private (verify-file-ownership (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (is-eq (get owner file-data) tx-sender)
    false
  )
)

;; Internal file deactivation
(define-private (deactivate-file-internal (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (ok (map-set files 
      { file-id: file-id } 
      (merge file-data { is-active: false, updated-at: (unwrap-panic (get-block-info? time (- block-height u1))) })))
    (err u404)
  )
)

;; Read-only Functions

;; Get enhanced file information
(define-read-only (get-file-details (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data (some (merge file-data {
      stats: (default-to { unique-viewers: u0, total-revenue: u0, avg-rating: u0, rating-count: u0 } (map-get? file-stats { file-id: file-id })),
      comment-count: (get count (default-to { count: u0, next-comment-id: u1 } (map-get? file-comment-counts { file-id: file-id })))
    }))
    none
  )
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get folder contents
(define-read-only (get-folder-contents (folder-id uint))
  (map-get? folders { folder-id: folder-id })
)

;; Search files by category and tags
(define-read-only (get-files-by-category (category (string-ascii 32)))
  (map-get? categories { category: category })
)

;; Get file comments
(define-read-only (get-file-comment (file-id uint) (comment-id uint))
  (map-get? file-comments { file-id: file-id, comment-id: comment-id })
)

;; Check if file is favorited by user
(define-read-only (is-favorited (user principal) (file-id uint))
  (is-some (map-get? favorites { user: user, file-id: file-id }))
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-files: (var-get next-file-id),
    total-tokens: (var-get next-token-id),
    total-folders: (var-get next-folder-id),
    platform-revenue: (var-get total-platform-revenue)
  }
)

;; Advanced token validation with permission check
(define-read-only (validate-token-permissions (token-hash (string-ascii 64)) (required-permission (string-ascii 32)))
  (match (get-token-by-hash token-hash)
    token-data 
    (let
      (
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (permissions (get permissions token-data))
      )
      (and 
        (get is-active token-data)
        (> (get expires-at token-data) current-time)
        (if (is-eq required-permission "download")
          (get can-download permissions)
          (if (is-eq required-permission "view")
            (get can-view permissions)
            (if (is-eq required-permission "share")
              (get can-share permissions)
              (get can-comment permissions)
            )
          )
        )
      )
    )
    false
  )
)

;; Existing read-only functions (keeping compatibility)
(define-read-only (get-file-info (file-id uint))
  (map-get? files { file-id: file-id })
)

(define-read-only (get-token-info (token-id uint))
  (map-get? access-tokens { token-id: token-id })
)

(define-read-only (get-token-by-hash (token-hash (string-ascii 64)))
  (match (map-get? token-lookup { token-hash: token-hash })
    lookup-data (map-get? access-tokens { token-id: (get token-id lookup-data) })
    none
  )
)

(define-read-only (validate-token (token-hash (string-ascii 64)))
  (match (get-token-by-hash token-hash)
    token-data 
    (let
      (
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      )
      (and 
        (get is-active token-data)
        (> (get expires-at token-data) current-time)
      )
    )
    false
  )
)