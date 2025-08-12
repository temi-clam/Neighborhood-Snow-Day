;; Snow Day Activity Coordinator
;; Core system for neighborhood parents to coordinate activities during school closures

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-ACTIVITY (err u101))
(define-constant ERR-ALREADY-REGISTERED (err u102))
(define-constant ERR-NOT-REGISTERED (err u103))
(define-constant ERR-INVALID-AGE-GROUP (err u104))

;; Data maps
(define-map activities
    { activity-id: uint }
    {
        organizer: principal,
        title: (string-ascii 64),
        age-group: uint,
        max-capacity: uint,
        current-count: uint,
        supervisor: principal,
        location: (string-ascii 128),
        active: bool
    }
)

(define-map registrations
    { participant: principal, activity-id: uint }
    { registered-at: uint }
)

(define-map parent-profiles
    { parent: principal }
    {
        name: (string-ascii 64),
        contact: (string-ascii 128),
        supervision-available: bool
    }
)

;; Data vars
(define-data-var next-activity-id uint u1)
(define-data-var neighborhood-coordinator principal tx-sender)

;; Register parent profile
(define-public (register-parent (name (string-ascii 64)) (contact (string-ascii 128)) (can-supervise bool))
    (begin
        (map-set parent-profiles
            { parent: tx-sender }
            {
                name: name,
                contact: contact,
                supervision-available: can-supervise
            }
        )
        (ok true)
    )
)

;; Create activity
(define-public (create-activity
    (title (string-ascii 64))
    (age-group uint)
    (max-capacity uint)
    (supervisor principal)
    (location (string-ascii 128))
)
    (let
        (
            (activity-id (var-get next-activity-id))
        )
        (asserts! (is-some (map-get? parent-profiles { parent: tx-sender })) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= age-group u1) (<= age-group u4)) ERR-INVALID-AGE-GROUP)

        (map-set activities
            { activity-id: activity-id }
            {
                organizer: tx-sender,
                title: title,
                age-group: age-group,
                max-capacity: max-capacity,
                current-count: u0,
                supervisor: supervisor,
                location: location,
                active: true
            }
        )

        (var-set next-activity-id (+ activity-id u1))
        (ok activity-id)
    )
)

;; Register for activity
(define-public (register-for-activity (activity-id uint))
    (let
        (
            (activity (unwrap! (map-get? activities { activity-id: activity-id }) ERR-INVALID-ACTIVITY))
        )
        (asserts! (is-some (map-get? parent-profiles { parent: tx-sender })) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? registrations { participant: tx-sender, activity-id: activity-id })) ERR-ALREADY-REGISTERED)
        (asserts! (< (get current-count activity) (get max-capacity activity)) ERR-INVALID-ACTIVITY)
        (asserts! (get active activity) ERR-INVALID-ACTIVITY)

        (map-set registrations
            { participant: tx-sender, activity-id: activity-id }
            { registered-at: stacks-block-height }
        )

        (map-set activities
            { activity-id: activity-id }
            (merge activity { current-count: (+ (get current-count activity) u1) })
        )

        (ok true)
    )
)

;; Update supervision assignment
(define-public (assign-supervisor (activity-id uint) (new-supervisor principal))
    (let
        (
            (activity (unwrap! (map-get? activities { activity-id: activity-id }) ERR-INVALID-ACTIVITY))
        )
        (asserts! (is-eq tx-sender (get organizer activity)) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? parent-profiles { parent: new-supervisor })) ERR-NOT-AUTHORIZED)

        (map-set activities
            { activity-id: activity-id }
            (merge activity { supervisor: new-supervisor })
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-activity (activity-id uint))
    (map-get? activities { activity-id: activity-id })
)

(define-read-only (get-parent-profile (parent principal))
    (map-get? parent-profiles { parent: parent })
)

(define-read-only (is-registered (participant principal) (activity-id uint))
    (is-some (map-get? registrations { participant: participant, activity-id: activity-id }))
)
