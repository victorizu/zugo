;; Enhanced Voting Smart Contract with Advanced Features
;; Comprehensive voting system with multiple election support, delegation, and advanced analytics

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-VOTING-CLOSED (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-CANDIDATE-NOT-FOUND (err u103))
(define-constant ERR-CANDIDATE-EXISTS (err u104))
(define-constant ERR-INVALID-CANDIDATE-ID (err u105))
(define-constant ERR-ELECTION-NOT-FOUND (err u106))
(define-constant ERR-ELECTION-ALREADY-EXISTS (err u107))
(define-constant ERR-INVALID-TIME (err u108))
(define-constant ERR-VOTER-NOT-ELIGIBLE (err u109))
(define-constant ERR-DELEGATION-NOT-ALLOWED (err u110))
(define-constant ERR-SELF-DELEGATION (err u111))
(define-constant ERR-INVALID-WEIGHT (err u112))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u113))
(define-constant ERR-VOTING-NOT-STARTED (err u114))
(define-constant ERR-MINIMUM-VOTES-NOT-MET (err u115))

;; Contract owner and admin roles
(define-constant CONTRACT-OWNER tx-sender)
(define-data-var election-admin principal tx-sender)

;; Global settings
(define-data-var next-election-id uint u1)
(define-data-var next-candidate-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var platform-fee uint u100) ;; Fee in microSTX for creating elections
(define-data-var delegation-enabled bool true)
(define-data-var anonymous-voting bool false)

;; Election data structure
(define-map elections
    { election-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 1000),
        creator: principal,
        start-time: uint,
        end-time: uint,
        min-votes: uint,
        max-candidates: uint,
        is-active: bool,
        voting-type: (string-ascii 20), ;; "single", "multiple", "ranked"
        require-registration: bool,
        total-votes: uint,
        winner-id: (optional uint),
        is-finalized: bool
    }
)

;; Enhanced candidate structure
(define-map candidates
    { election-id: uint, candidate-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 500),
        manifesto: (string-ascii 2000),
        contact-info: (string-ascii 200),
        image-url: (string-ascii 200),
        vote-count: uint,
        weighted-vote-count: uint,
        endorsements: uint,
        is-active: bool,
        registration-time: uint
    }
)

;; Voter registration and profiles
(define-map voter-profiles
    { voter: principal }
    {
        display-name: (string-ascii 50),
        bio: (string-ascii 500),
        reputation-score: uint,
        total-votes-cast: uint,
        registration-time: uint,
        is-verified: bool,
        voting-power: uint, ;; Default 1, can be increased based on reputation
        last-activity: uint
    }
)

;; Enhanced voting records
(define-map votes
    { election-id: uint, voter: principal }
    {
        candidate-id: uint,
        vote-weight: uint,
        timestamp: uint,
        is-delegated: bool,
        delegator: (optional principal),
        ranked-choices: (list 10 uint), ;; For ranked voting
        comment: (string-ascii 500)
    }
)

;; Vote delegation system
(define-map delegations
    { election-id: uint, delegator: principal }
    {
        delegate: principal,
        vote-weight: uint,
        timestamp: uint,
        is-active: bool
    }
)

;; Proposal system for governance
(define-map proposals
    { proposal-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 1000),
        proposer: principal,
        votes-for: uint,
        votes-against: uint,
        start-time: uint,
        end-time: uint,
        is-active: bool,
        execution-delay: uint,
        is-executed: bool
    }
)

;; Eligible voters list (for restricted elections)
(define-map eligible-voters
    { election-id: uint, voter: principal }
    { is-eligible: bool, added-by: principal, timestamp: uint }
)

;; Election statistics
(define-map election-stats
    { election-id: uint }
    {
        total-registered-voters: uint,
        voter-turnout: uint,
        average-vote-time: uint,
        most-active-hour: uint,
        geographic-distribution: (string-ascii 500)
    }
)

;; Audit trail
(define-map audit-log
    { log-id: uint }
    {
        action: (string-ascii 50),
        actor: principal,
        target: (string-ascii 100),
        timestamp: uint,
        details: (string-ascii 500)
    }
)

(define-data-var next-log-id uint u1)

;; Events for external monitoring
(define-map event-log
    { event-id: uint }
    {
        event-type: (string-ascii 30),
        election-id: uint,
        participant: principal,
        timestamp: uint,
        data: (string-ascii 500)
    }
)

(define-data-var next-event-id uint u1)

;; =================== READ-ONLY FUNCTIONS ===================

;; Get election information
(define-read-only (get-election (election-id uint))
    (map-get? elections { election-id: election-id })
)

;; Get candidate information
(define-read-only (get-candidate (election-id uint) (candidate-id uint))
    (map-get? candidates { election-id: election-id, candidate-id: candidate-id })
)

;; Get voter profile
(define-read-only (get-voter-profile (voter principal))
    (map-get? voter-profiles { voter: voter })
)

;; Get vote record
(define-read-only (get-vote (election-id uint) (voter principal))
    (map-get? votes { election-id: election-id, voter: voter })
)

;; Check if voter is eligible
(define-read-only (is-voter-eligible (election-id uint) (voter principal))
    (let
        (
            (election-info (unwrap! (map-get? elections { election-id: election-id }) false))
            (requires-registration (get require-registration election-info))
        )
        (if requires-registration
            (match (map-get? eligible-voters { election-id: election-id, voter: voter })
                eligibility-info (get is-eligible eligibility-info)
                false
            )
            true
        )
    )
)

;; Check if voting is currently active for an election
(define-read-only (is-voting-active (election-id uint))
    (match (map-get? elections { election-id: election-id })
        election-info
        (let
            (
                (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
                (start-time (get start-time election-info))
                (end-time (get end-time election-info))
                (is-active (get is-active election-info))
            )
            (and 
                is-active
                (>= current-time start-time)
                (<= current-time end-time)
            )
        )
        false
    )
)

;; Get election results
(define-read-only (get-election-results (election-id uint))
    (let
        (
            (election-info (unwrap! (map-get? elections { election-id: election-id }) (err ERR-ELECTION-NOT-FOUND)))
            (stats (map-get? election-stats { election-id: election-id }))
        )
        (ok {
            election: election-info,
            statistics: stats,
            winner: (get winner-id election-info),
            is-finalized: (get is-finalized election-info)
        })
    )
)

;; Get delegation info
(define-read-only (get-delegation (election-id uint) (delegator principal))
    (map-get? delegations { election-id: election-id, delegator: delegator })
)

;; Get proposal info
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

;; Calculate vote weight (including delegation)
(define-read-only (calculate-vote-weight (election-id uint) (voter principal))
    (let
        (
            (voter-profile (map-get? voter-profiles { voter: voter }))
            (base-weight (match voter-profile
                profile (get voting-power profile)
                u1
            ))
        )
        ;; Add delegated weight (simplified - would need more complex logic)
        base-weight
    )
)

;; Get election statistics
(define-read-only (get-election-statistics (election-id uint))
    (map-get? election-stats { election-id: election-id })
)

;; Get top candidates
(define-read-only (get-top-candidates (election-id uint) (limit uint))
    ;; Simplified version - would need more complex sorting in practice
    (ok "top-candidates-data")
)

;; =================== ADMIN FUNCTIONS ===================

;; Set election admin
(define-public (set-election-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set election-admin new-admin)
        (ok true)
    )
)

;; Set platform fee
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

;; Toggle delegation feature
(define-public (toggle-delegation (enabled bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set delegation-enabled enabled)
        (ok true)
    )
)

;; =================== ELECTION MANAGEMENT ===================

;; Create new election
(define-public (create-election 
    (title (string-ascii 100))
    (description (string-ascii 1000))
    (start-time uint)
    (end-time uint)
    (min-votes uint)
    (max-candidates uint)
    (voting-type (string-ascii 20))
    (require-registration bool)
)
    (let
        (
            (election-id (var-get next-election-id))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        ;; Validate times
        (asserts! (> start-time current-time) ERR-INVALID-TIME)
        (asserts! (> end-time start-time) ERR-INVALID-TIME)
        
        ;; Create election
        (map-set elections
            { election-id: election-id }
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-time: start-time,
                end-time: end-time,
                min-votes: min-votes,
                max-candidates: max-candidates,
                is-active: true,
                voting-type: voting-type,
                require-registration: require-registration,
                total-votes: u0,
                winner-id: none,
                is-finalized: false
            }
        )
        
        ;; Initialize statistics
        (map-set election-stats
            { election-id: election-id }
            {
                total-registered-voters: u0,
                voter-turnout: u0,
                average-vote-time: u0,
                most-active-hour: u0,
                geographic-distribution: ""
            }
        )
        
        ;; Log event
        (unwrap-panic (log-event "ELECTION_CREATED" election-id tx-sender ""))
        
        ;; Update counter
        (var-set next-election-id (+ election-id u1))
        
        (ok election-id)
    )
)

;; Add candidate to election
(define-public (add-candidate-to-election
    (election-id uint)
    (name (string-ascii 50))
    (description (string-ascii 500))
    (manifesto (string-ascii 2000))
    (contact-info (string-ascii 200))
    (image-url (string-ascii 200))
)
    (let
        (
            (candidate-id (var-get next-candidate-id))
            (election-info (unwrap! (map-get? elections { election-id: election-id }) ERR-ELECTION-NOT-FOUND))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        ;; Validate election exists and is active
        (asserts! (get is-active election-info) ERR-VOTING-CLOSED)
        (asserts! (< current-time (get start-time election-info)) ERR-VOTING-NOT-STARTED)
        
        ;; Add candidate
        (map-set candidates
            { election-id: election-id, candidate-id: candidate-id }
            {
                name: name,
                description: description,
                manifesto: manifesto,
                contact-info: contact-info,
                image-url: image-url,
                vote-count: u0,
                weighted-vote-count: u0,
                endorsements: u0,
                is-active: true,
                registration-time: current-time
            }
        )
        
        ;; Log event
        (unwrap-panic (log-event "CANDIDATE_ADDED" election-id tx-sender name))
        
        ;; Update counter
        (var-set next-candidate-id (+ candidate-id u1))
        
        (ok candidate-id)
    )
)

;; =================== VOTER MANAGEMENT ===================

;; Register voter profile
(define-public (register-voter 
    (display-name (string-ascii 50))
    (bio (string-ascii 500))
)
    (let
        (
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        (map-set voter-profiles
            { voter: tx-sender }
            {
                display-name: display-name,
                bio: bio,
                reputation-score: u100,
                total-votes-cast: u0,
                registration-time: current-time,
                is-verified: false,
                voting-power: u1,
                last-activity: current-time
            }
        )
        
        (ok true)
    )
)

;; Add eligible voter (for restricted elections)
(define-public (add-eligible-voter (election-id uint) (voter principal))
    (let
        (
            (election-info (unwrap! (map-get? elections { election-id: election-id }) ERR-ELECTION-NOT-FOUND))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        ;; Only election creator or admin can add eligible voters
        (asserts! (or 
            (is-eq tx-sender (get creator election-info))
            (is-eq tx-sender (var-get election-admin))
        ) ERR-NOT-AUTHORIZED)
        
        (map-set eligible-voters
            { election-id: election-id, voter: voter }
            { is-eligible: true, added-by: tx-sender, timestamp: current-time }
        )
        
        (ok true)
    )
)

;; =================== VOTING FUNCTIONS ===================

;; Enhanced voting function
(define-public (cast-vote 
    (election-id uint) 
    (candidate-id uint)
    (ranked-choices (list 10 uint))
    (comment (string-ascii 500))
)
    (let
        (
            (voter tx-sender)
            (election-info (unwrap! (map-get? elections { election-id: election-id }) ERR-ELECTION-NOT-FOUND))
            (candidate-info (unwrap! (map-get? candidates { election-id: election-id, candidate-id: candidate-id }) ERR-CANDIDATE-NOT-FOUND))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
            (vote-weight (calculate-vote-weight election-id voter))
        )
        ;; Validate voting is active
        (asserts! (is-voting-active election-id) ERR-VOTING-CLOSED)
        
        ;; Check if voter is eligible
        (asserts! (is-voter-eligible election-id voter) ERR-VOTER-NOT-ELIGIBLE)
        
        ;; Check if already voted
        (asserts! (is-none (map-get? votes { election-id: election-id, voter: voter })) ERR-ALREADY-VOTED)
        
        ;; Record vote
        (map-set votes
            { election-id: election-id, voter: voter }
            {
                candidate-id: candidate-id,
                vote-weight: vote-weight,
                timestamp: current-time,
                is-delegated: false,
                delegator: none,
                ranked-choices: ranked-choices,
                comment: comment
            }
        )
        
        ;; Update candidate vote count
        (map-set candidates
            { election-id: election-id, candidate-id: candidate-id }
            (merge candidate-info {
                vote-count: (+ (get vote-count candidate-info) u1),
                weighted-vote-count: (+ (get weighted-vote-count candidate-info) vote-weight)
            })
        )
        
        ;; Update election total votes
        (map-set elections
            { election-id: election-id }
            (merge election-info {
                total-votes: (+ (get total-votes election-info) u1)
            })
        )
        
        ;; Update voter profile
        (match (map-get? voter-profiles { voter: voter })
            profile (map-set voter-profiles
                { voter: voter }
                (merge profile {
                    total-votes-cast: (+ (get total-votes-cast profile) u1),
                    last-activity: current-time
                })
            )
            true ;; No profile exists
        )
        
        ;; Log event
        (unwrap-panic (log-event "VOTE_CAST" election-id voter ""))
        
        (ok true)
    )
)

;; Delegate vote
(define-public (delegate-vote (election-id uint) (delegate principal))
    (let
        (
            (delegator tx-sender)
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
            (vote-weight (calculate-vote-weight election-id delegator))
        )
        ;; Validate delegation is enabled
        (asserts! (var-get delegation-enabled) ERR-DELEGATION-NOT-ALLOWED)
        
        ;; Cannot delegate to self
        (asserts! (not (is-eq delegator delegate)) ERR-SELF-DELEGATION)
        
        ;; Check if voting is active
        (asserts! (is-voting-active election-id) ERR-VOTING-CLOSED)
        
        ;; Check if already voted or delegated
        (asserts! (is-none (map-get? votes { election-id: election-id, voter: delegator })) ERR-ALREADY-VOTED)
        (asserts! (is-none (map-get? delegations { election-id: election-id, delegator: delegator })) ERR-ALREADY-VOTED)
        
        ;; Record delegation
        (map-set delegations
            { election-id: election-id, delegator: delegator }
            {
                delegate: delegate,
                vote-weight: vote-weight,
                timestamp: current-time,
                is-active: true
            }
        )
        
        ;; Log event
        (unwrap-panic (log-event "VOTE_DELEGATED" election-id delegator ""))
        
        (ok true)
    )
)

;; =================== PROPOSAL SYSTEM ===================

;; Create governance proposal
(define-public (create-proposal
    (title (string-ascii 100))
    (description (string-ascii 1000))
    (voting-period uint)
    (execution-delay uint)
)
    (let
        (
            (proposal-id (var-get next-proposal-id))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        (map-set proposals
            { proposal-id: proposal-id }
            {
                title: title,
                description: description,
                proposer: tx-sender,
                votes-for: u0,
                votes-against: u0,
                start-time: current-time,
                end-time: (+ current-time voting-period),
                is-active: true,
                execution-delay: execution-delay,
                is-executed: false
            }
        )
        
        (var-set next-proposal-id (+ proposal-id u1))
        
        (ok proposal-id)
    )
)

;; =================== UTILITY FUNCTIONS ===================

;; Log events
(define-private (log-event (event-type (string-ascii 30)) (election-id uint) (participant principal) (data (string-ascii 500)))
    (let
        (
            (event-id (var-get next-event-id))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        (map-set event-log
            { event-id: event-id }
            {
                event-type: event-type,
                election-id: election-id,
                participant: participant,
                timestamp: current-time,
                data: data
            }
        )
        
        (var-set next-event-id (+ event-id u1))
        (ok true)
    )
)

;; Finalize election results
(define-public (finalize-election (election-id uint))
    (let
        (
            (election-info (unwrap! (map-get? elections { election-id: election-id }) ERR-ELECTION-NOT-FOUND))
            (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        )
        ;; Only after voting period ends
        (asserts! (> current-time (get end-time election-info)) ERR-VOTING-CLOSED)
        
        ;; Only election creator or admin can finalize
        (asserts! (or 
            (is-eq tx-sender (get creator election-info))
            (is-eq tx-sender (var-get election-admin))
        ) ERR-NOT-AUTHORIZED)
        
        ;; Check minimum votes requirement
        (asserts! (>= (get total-votes election-info) (get min-votes election-info)) ERR-MINIMUM-VOTES-NOT-MET)
        
        ;; Mark as finalized (winner calculation would be more complex)
        (map-set elections
            { election-id: election-id }
            (merge election-info {
                is-finalized: true,
                winner-id: (some u1) ;; Simplified - would calculate actual winner
            })
        )
        
        ;; Log event
        (unwrap-panic (log-event "ELECTION_FINALIZED" election-id tx-sender ""))
        
        (ok true)
    )
)

;; Emergency functions
(define-public (emergency-stop-election (election-id uint))
    (let
        (
            (election-info (unwrap! (map-get? elections { election-id: election-id }) ERR-ELECTION-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set elections
            { election-id: election-id }
            (merge election-info { is-active: false })
        )
        
        (ok true)
    )
)