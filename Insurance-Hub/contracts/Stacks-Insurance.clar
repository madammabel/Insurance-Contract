;; DECENTRALIZED INSURANCE PROTOCOL
;; A comprehensive blockchain-based insurance system that enables:
;; - Dynamic policy creation with customizable coverage and premiums
;; - Automated premium collection and payment tracking
;; - Transparent claims submission and processing workflow
;; - Decentralized policy management with holder autonomy
;; - Real-time balance management and emergency controls
;; - Immutable record-keeping for all insurance transactions

;; CORE CONTRACT CONSTANTS

(define-constant insurance-protocol-owner tx-sender)

;; ERROR CONSTANTS - All errors follow ERR-DESCRIPTION format

(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-POLICY-NOT-FOUND (err u101))
(define-constant ERR-POLICY-EXPIRED (err u102))
(define-constant ERR-POLICY-ALREADY-CANCELLED (err u103))
(define-constant ERR-INSUFFICIENT-PREMIUM-AMOUNT (err u104))
(define-constant ERR-CLAIM-NOT-FOUND (err u105))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u106))
(define-constant ERR-INVALID-COVERAGE-AMOUNT (err u107))
(define-constant ERR-INVALID-PREMIUM-AMOUNT (err u108))
(define-constant ERR-POLICY-ALREADY-EXISTS (err u109))
(define-constant ERR-INSUFFICIENT-CONTRACT-FUNDS (err u110))
(define-constant ERR-INVALID-CLAIM-AMOUNT (err u111))
(define-constant ERR-POLICY-INACTIVE (err u112))

;; POLICY STATUS CONSTANTS

(define-constant policy-status-active u1)
(define-constant policy-status-expired u2)
(define-constant policy-status-cancelled u3)

;; CLAIM STATUS CONSTANTS

(define-constant claim-status-pending u1)
(define-constant claim-status-approved u2)
(define-constant claim-status-rejected u3)
(define-constant claim-status-paid u4)

;; STATE VARIABLES

(define-data-var current-policy-identifier uint u1)
(define-data-var current-claim-identifier uint u1)
(define-data-var total-contract-balance uint u0)
(define-data-var total-policies-created uint u0)
(define-data-var total-claims-submitted uint u0)

;; DATA STRUCTURES

;; Primary policy registry
(define-map insurance-policies
  { policy-identifier: uint }
  {
    policy-holder-address: principal,
    maximum-coverage-amount: uint,
    required-premium-amount: uint,
    policy-start-block: uint,
    policy-end-block: uint,
    current-policy-status: uint,
    total-premiums-paid: uint,
    policy-creation-block: uint
  }
)

;; Claims management registry
(define-map insurance-claims
  { claim-identifier: uint }
  {
    associated-policy-id: uint,
    claim-submitter-address: principal,
    requested-claim-amount: uint,
    claim-description-text: (string-ascii 500),
    current-claim-status: uint,
    claim-submission-block: uint,
    claim-processing-block: (optional uint),
    claim-processor-address: (optional principal)
  }
)

;; Premium payment tracking
(define-map premium-transaction-records
  { policy-identifier: uint, payment-sequence-number: uint }
  {
    payment-amount: uint,
    payment-block-height: uint,
    payment-sender-address: principal,
    payment-timestamp: uint
  }
)

;; Payment sequence tracking per policy
(define-map policy-payment-sequences
  { policy-identifier: uint }
  { total-payment-count: uint }
)

;; Policy holder registry for quick lookups
(define-map policyholder-to-policies
  { holder-address: principal }
  { policy-identifiers: (list 100 uint) }
)

;; UTILITY AND VALIDATION FUNCTIONS

;; Get current blockchain height
(define-read-only (get-current-block-height)
  stacks-block-height
)

;; Verify contract owner privileges
(define-private (verify-contract-owner-access (caller-address principal))
  (is-eq caller-address insurance-protocol-owner)
)

;; Comprehensive policy validation
(define-private (validate-policy-existence-and-status (policy-identifier uint))
  (match (map-get? insurance-policies { policy-identifier: policy-identifier })
    policy-details 
      (if (and 
            (is-eq (get current-policy-status policy-details) policy-status-active)
            (< stacks-block-height (get policy-end-block policy-details)))
        (ok policy-details)
        (if (>= stacks-block-height (get policy-end-block policy-details))
          ERR-POLICY-EXPIRED
          ERR-POLICY-INACTIVE))
    ERR-POLICY-NOT-FOUND
  )
)

;; Validate claim ownership and status
(define-private (validate-claim-access (claim-identifier uint) (caller-address principal))
  (match (map-get? insurance-claims { claim-identifier: claim-identifier })
    claim-details
      (if (is-eq (get claim-submitter-address claim-details) caller-address)
        (ok claim-details)
        ERR-UNAUTHORIZED-ACCESS)
    ERR-CLAIM-NOT-FOUND
  )
)

;; POLICY MANAGEMENT FUNCTIONS

;; Create new insurance policy with enhanced validation
(define-public (establish-insurance-policy 
  (desired-coverage-amount uint) 
  (monthly-premium-amount uint) 
  (policy-duration-in-blocks uint))
  (let (
    (new-policy-identifier (var-get current-policy-identifier))
    (policy-start-block stacks-block-height)
    (policy-expiration-block (+ stacks-block-height policy-duration-in-blocks))
    (policy-holder-address tx-sender)
  )
    ;; Input validation
    (asserts! (> desired-coverage-amount u0) ERR-INVALID-COVERAGE-AMOUNT)
    (asserts! (> monthly-premium-amount u0) ERR-INVALID-PREMIUM-AMOUNT)
    (asserts! (> policy-duration-in-blocks u0) ERR-INVALID-COVERAGE-AMOUNT)
    
    ;; Ensure policy doesn't already exist
    (asserts! (is-none (map-get? insurance-policies { policy-identifier: new-policy-identifier })) 
              ERR-POLICY-ALREADY-EXISTS)
    
    ;; Create comprehensive policy record
    (map-set insurance-policies
      { policy-identifier: new-policy-identifier }
      {
        policy-holder-address: policy-holder-address,
        maximum-coverage-amount: desired-coverage-amount,
        required-premium-amount: monthly-premium-amount,
        policy-start-block: policy-start-block,
        policy-end-block: policy-expiration-block,
        current-policy-status: policy-status-active,
        total-premiums-paid: u0,
        policy-creation-block: stacks-block-height
      }
    )
    
    ;; Initialize payment tracking
    (map-set policy-payment-sequences
      { policy-identifier: new-policy-identifier }
      { total-payment-count: u0 }
    )
    
    ;; Update global counters
    (var-set current-policy-identifier (+ new-policy-identifier u1))
    (var-set total-policies-created (+ (var-get total-policies-created) u1))
    
    (ok {
      policy-id: new-policy-identifier,
      coverage-amount: desired-coverage-amount,
      premium-amount: monthly-premium-amount,
      expiration-block: policy-expiration-block
    })
  )
)

;; Process premium payment with detailed tracking
(define-public (submit-premium-payment (target-policy-identifier uint) (payment-amount uint))
  (let (
    ;; Input validation
    (validated-policy-id (if (> target-policy-identifier u0) target-policy-identifier u0))
    (validated-payment-amount (if (> payment-amount u0) payment-amount u0))
    (policy-validation-result (validate-policy-existence-and-status validated-policy-id))
    (current-payment-sequence (default-to { total-payment-count: u0 } 
      (map-get? policy-payment-sequences { policy-identifier: validated-policy-id })))
    (next-payment-sequence (+ (get total-payment-count current-payment-sequence) u1))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    (asserts! (> payment-amount u0) ERR-INVALID-PREMIUM-AMOUNT)
    (match policy-validation-result
      policy-details (begin
        ;; Validate payment amount meets minimum premium
        (asserts! (>= validated-payment-amount (get required-premium-amount policy-details)) 
                  ERR-INSUFFICIENT-PREMIUM-AMOUNT)
        
        ;; Execute STX transfer to contract
        (try! (stx-transfer? validated-payment-amount tx-sender (as-contract tx-sender)))
        
        ;; Record detailed payment transaction
        (map-set premium-transaction-records
          { policy-identifier: validated-policy-id, payment-sequence-number: next-payment-sequence }
          {
            payment-amount: validated-payment-amount,
            payment-block-height: stacks-block-height,
            payment-sender-address: tx-sender,
            payment-timestamp: stacks-block-height
          }
        )
        
        ;; Update payment sequence counter
        (map-set policy-payment-sequences
          { policy-identifier: validated-policy-id }
          { total-payment-count: next-payment-sequence }
        )
        
        ;; Update policy with new total paid amount
        (map-set insurance-policies
          { policy-identifier: validated-policy-id }
          (merge policy-details { 
            total-premiums-paid: (+ (get total-premiums-paid policy-details) validated-payment-amount) 
          })
        )
        
        ;; Update contract balance
        (var-set total-contract-balance (+ (var-get total-contract-balance) validated-payment-amount))
        
        (ok {
          payment-id: next-payment-sequence,
          amount-paid: validated-payment-amount,
          new-total-paid: (+ (get total-premiums-paid policy-details) validated-payment-amount)
        })
      )
      validation-error (err validation-error)
    )
  )
)

;; Cancel policy with proper authorization
(define-public (terminate-insurance-policy (target-policy-identifier uint))
  (let (
    ;; Input validation
    (validated-policy-id (if (> target-policy-identifier u0) target-policy-identifier u0))
    (policy-details-option (map-get? insurance-policies { policy-identifier: validated-policy-id }))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    
    (match policy-details-option
      policy-details (begin
        ;; Verify caller is policy holder
        (asserts! (is-eq tx-sender (get policy-holder-address policy-details)) 
                  ERR-UNAUTHORIZED-ACCESS)
        ;; Verify policy is not already cancelled
        (asserts! (not (is-eq (get current-policy-status policy-details) policy-status-cancelled)) 
                  ERR-POLICY-ALREADY-CANCELLED)
        
        ;; Update policy status to cancelled
        (map-set insurance-policies
          { policy-identifier: validated-policy-id }
          (merge policy-details { current-policy-status: policy-status-cancelled })
        )
        
        (ok { 
          policy-id: validated-policy-id,
          status: "policy-successfully-cancelled",
          cancellation-block: stacks-block-height
        })
      )
      ERR-POLICY-NOT-FOUND
    )
  )
)

;; CLAIMS MANAGEMENT FUNCTIONS

;; Submit insurance claim with comprehensive validation
(define-public (file-insurance-claim 
  (target-policy-identifier uint) 
  (requested-amount uint) 
  (detailed-description (string-ascii 500)))
  (let (
    ;; Input validation
    (validated-policy-id (if (> target-policy-identifier u0) target-policy-identifier u0))
    (validated-amount (if (> requested-amount u0) requested-amount u0))
    (validated-description (if (> (len detailed-description) u0) detailed-description ""))
    (policy-validation-result (validate-policy-existence-and-status validated-policy-id))
    (new-claim-identifier (var-get current-claim-identifier))
  )
    ;; Additional input validation
    (asserts! (> target-policy-identifier u0) ERR-POLICY-NOT-FOUND)
    (asserts! (> requested-amount u0) ERR-INVALID-CLAIM-AMOUNT)
    (asserts! (> (len detailed-description) u0) ERR-INVALID-CLAIM-AMOUNT)
    
    (match policy-validation-result
      policy-details (begin
        ;; Verify claim submitter is policy holder
        (asserts! (is-eq tx-sender (get policy-holder-address policy-details)) 
                  ERR-UNAUTHORIZED-ACCESS)
        ;; Verify claim amount doesn't exceed coverage
        (asserts! (<= validated-amount (get maximum-coverage-amount policy-details)) 
                  ERR-INVALID-CLAIM-AMOUNT)
        
        ;; Create comprehensive claim record
        (map-set insurance-claims
          { claim-identifier: new-claim-identifier }
          {
            associated-policy-id: validated-policy-id,
            claim-submitter-address: tx-sender,
            requested-claim-amount: validated-amount,
            claim-description-text: validated-description,
            current-claim-status: claim-status-pending,
            claim-submission-block: stacks-block-height,
            claim-processing-block: none,
            claim-processor-address: none
          }
        )
        
        ;; Update global counters
        (var-set current-claim-identifier (+ new-claim-identifier u1))
        (var-set total-claims-submitted (+ (var-get total-claims-submitted) u1))
        
        (ok {
          claim-id: new-claim-identifier,
          policy-id: validated-policy-id,
          requested-amount: validated-amount,
          submission-block: stacks-block-height
        })
      )
      validation-error (err validation-error)
    )
  )
)

;; Process claim with enhanced tracking and validation
(define-public (adjudicate-insurance-claim (target-claim-identifier uint) (approval-decision bool))
  (let (
    ;; Input validation
    (validated-claim-id (if (> target-claim-identifier u0) target-claim-identifier u0))
    (claim-details-option (map-get? insurance-claims { claim-identifier: validated-claim-id }))
  )
    ;; Additional input validation
    (asserts! (> target-claim-identifier u0) ERR-CLAIM-NOT-FOUND)
    ;; Verify contract owner authorization
    (asserts! (verify-contract-owner-access tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    (match claim-details-option
      claim-details (begin
        ;; Verify claim is still pending
        (asserts! (is-eq (get current-claim-status claim-details) claim-status-pending) 
                  ERR-CLAIM-ALREADY-PROCESSED)
        
        (if approval-decision
          ;; APPROVAL PROCESS
          (begin
            ;; Verify sufficient contract funds
            (asserts! (>= (var-get total-contract-balance) (get requested-claim-amount claim-details)) 
                      ERR-INSUFFICIENT-CONTRACT-FUNDS)
            
            ;; Execute payout transfer
            (try! (as-contract (stx-transfer? 
              (get requested-claim-amount claim-details) 
              tx-sender 
              (get claim-submitter-address claim-details))))
            
            ;; Update contract balance
            (var-set total-contract-balance 
              (- (var-get total-contract-balance) (get requested-claim-amount claim-details)))
            
            ;; Update claim status to paid
            (map-set insurance-claims
              { claim-identifier: validated-claim-id }
              (merge claim-details {
                current-claim-status: claim-status-paid,
                claim-processing-block: (some stacks-block-height),
                claim-processor-address: (some tx-sender)
              })
            )
            
            (ok {
              claim-id: validated-claim-id,
              status: "claim-approved-and-paid",
              payout-amount: (get requested-claim-amount claim-details),
              processing-block: stacks-block-height
            })
          )
          ;; REJECTION PROCESS
          (begin
            (map-set insurance-claims
              { claim-identifier: validated-claim-id }
              (merge claim-details {
                current-claim-status: claim-status-rejected,
                claim-processing-block: (some stacks-block-height),
                claim-processor-address: (some tx-sender)
              })
            )
            
            (ok {
              claim-id: validated-claim-id,
              status: "claim-rejected",
              payout-amount: u0,
              processing-block: stacks-block-height
            })
          )
        )
      )
      ERR-CLAIM-NOT-FOUND
    )
  )
)

;; ADMINISTRATIVE FUNCTIONS

;; Emergency fund withdrawal with enhanced security
(define-public (execute-emergency-withdrawal (withdrawal-amount uint))
  (begin
    ;; Verify contract owner authorization
    (asserts! (verify-contract-owner-access tx-sender) ERR-UNAUTHORIZED-ACCESS)
    ;; Verify sufficient funds available
    (asserts! (<= withdrawal-amount (var-get total-contract-balance)) 
              ERR-INSUFFICIENT-CONTRACT-FUNDS)
    
    ;; Execute withdrawal transfer
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender insurance-protocol-owner)))
    
    ;; Update contract balance
    (var-set total-contract-balance (- (var-get total-contract-balance) withdrawal-amount))
    
    (ok {
      withdrawn-amount: withdrawal-amount,
      remaining-balance: (var-get total-contract-balance),
      withdrawal-block: stacks-block-height
    })
  )
)

;; READ-ONLY QUERY FUNCTIONS

;; Retrieve comprehensive policy information
(define-read-only (get-policy-details (policy-identifier uint))
  (map-get? insurance-policies { policy-identifier: policy-identifier })
)

;; Retrieve detailed claim information
(define-read-only (get-claim-details (claim-identifier uint))
  (map-get? insurance-claims { claim-identifier: claim-identifier })
)

;; Get current contract financial status
(define-read-only (get-contract-financial-status)
  {
    total-balance: (var-get total-contract-balance),
    total-policies: (var-get total-policies-created),
    total-claims: (var-get total-claims-submitted),
    next-policy-id: (var-get current-policy-identifier),
    next-claim-id: (var-get current-claim-identifier)
  }
)

;; Retrieve specific premium payment details
(define-read-only (get-premium-payment-details (policy-identifier uint) (payment-sequence uint))
  (map-get? premium-transaction-records { 
    policy-identifier: policy-identifier, 
    payment-sequence-number: payment-sequence 
  })
)

;; Get total payment count for a policy
(define-read-only (get-policy-payment-statistics (policy-identifier uint))
  (default-to { total-payment-count: u0 } 
    (map-get? policy-payment-sequences { policy-identifier: policy-identifier }))
)

;; Check if policy is currently active and valid
(define-read-only (verify-policy-active-status (policy-identifier uint))
  (match (map-get? insurance-policies { policy-identifier: policy-identifier })
    policy-details (and 
      (is-eq (get current-policy-status policy-details) policy-status-active)
      (< stacks-block-height (get policy-end-block policy-details)))
    false
  )
)

;; Get policy holder address
(define-read-only (get-policy-holder-address (policy-identifier uint))
  (match (map-get? insurance-policies { policy-identifier: policy-identifier })
    policy-details (some (get policy-holder-address policy-details))
    none
  )
)

;; Get comprehensive policy summary
(define-read-only (get-policy-summary (policy-identifier uint))
  (match (map-get? insurance-policies { policy-identifier: policy-identifier })
    policy-details (some {
      policy-id: policy-identifier,
      holder: (get policy-holder-address policy-details),
      coverage: (get maximum-coverage-amount policy-details),
      premium: (get required-premium-amount policy-details),
      total-paid: (get total-premiums-paid policy-details),
      status: (get current-policy-status policy-details),
      active: (and 
        (is-eq (get current-policy-status policy-details) policy-status-active)
        (< stacks-block-height (get policy-end-block policy-details))),
      blocks-remaining: (if (> (get policy-end-block policy-details) stacks-block-height)
        (- (get policy-end-block policy-details) stacks-block-height)
        u0)
    })
    none
  )
)