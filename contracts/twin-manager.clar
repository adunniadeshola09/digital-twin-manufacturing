;; Twin Manager Smart Contract
;; Create and maintain digital twins of manufacturing equipment, predict maintenance needs,
;; coordinate shared manufacturing capacity, track equipment performance, and automate maintenance scheduling and payments

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_TWIN_NOT_FOUND (err u2))
(define-constant ERR_INVALID_PARAMETERS (err u3))
(define-constant ERR_EQUIPMENT_OFFLINE (err u4))
(define-constant ERR_MAINTENANCE_NOT_FOUND (err u5))
(define-constant ERR_INSUFFICIENT_CAPACITY (err u6))
(define-constant ERR_PROVIDER_NOT_FOUND (err u7))
(define-constant ERR_INVALID_STATUS (err u8))
(define-constant ERR_ALREADY_EXISTS (err u9))

;; Equipment status constants
(define-constant STATUS_OPERATIONAL u1)
(define-constant STATUS_MAINTENANCE u2)
(define-constant STATUS_OFFLINE u3)
(define-constant STATUS_DEGRADED u4)

;; Maintenance priority levels
(define-constant PRIORITY_LOW u1)
(define-constant PRIORITY_MEDIUM u2)
(define-constant PRIORITY_HIGH u3)
(define-constant PRIORITY_CRITICAL u4)

;; Performance thresholds
(define-constant MIN_EFFICIENCY u70) ;; 70% minimum efficiency
(define-constant OPTIMAL_EFFICIENCY u95) ;; 95% optimal efficiency
(define-constant MAX_VIBRATION u50) ;; Maximum acceptable vibration level
(define-constant MAX_TEMPERATURE u80) ;; Maximum operating temperature in Celsius

;; Data Variables
(define-data-var twin-counter uint u0)
(define-data-var maintenance-counter uint u0)
(define-data-var provider-counter uint u0)
(define-data-var capacity-request-counter uint u0)
(define-data-var total-equipment-value uint u0)
(define-data-var total-maintenance-cost uint u0)
(define-data-var total-downtime-hours uint u0)

;; Data Maps
(define-map digital-twins
  { twin-id: uint }
  {
    owner: principal,
    equipment-type: (string-ascii 50),
    model: (string-ascii 50),
    serial-number: (string-ascii 30),
    manufacturing-date: uint,
    location: (string-ascii 100),
    capacity-units: uint, ;; Production capacity per hour
    current-status: uint,
    efficiency-rating: uint, ;; Percentage 0-100
    last-maintenance: uint,
    next-predicted-maintenance: uint,
    total-operating-hours: uint,
    created-at: uint,
    active: bool
  }
)

(define-map equipment-sensors
  { twin-id: uint }
  {
    temperature: uint,
    vibration-level: uint,
    power-consumption: uint,
    output-rate: uint,
    error-count: uint,
    last-reading: uint,
    sensor-health: bool,
    automated-alerts: bool
  }
)

(define-map maintenance-records
  { maintenance-id: uint }
  {
    twin-id: uint,
    maintenance-type: (string-ascii 30),
    scheduled-date: uint,
    completed-date: (optional uint),
    technician: (optional principal),
    priority-level: uint,
    estimated-cost: uint,
    actual-cost: (optional uint),
    downtime-hours: (optional uint),
    description: (string-ascii 200),
    completed: bool
  }
)

(define-map maintenance-providers
  { provider: principal }
  {
    company-name: (string-ascii 100),
    specializations: (list 5 (string-ascii 30)),
    certification-level: uint, ;; 1-5 scale
    average-response-time: uint, ;; hours
    success-rate: uint, ;; percentage
    total-jobs-completed: uint,
    total-earnings: uint,
    active: bool,
    registration-date: uint
  }
)

(define-map shared-capacity
  { twin-id: uint }
  {
    available-hours: uint,
    hourly-rate: uint,
    minimum-booking: uint, ;; minimum hours
    quality-standards: (list 3 (string-ascii 20)),
    booking-lead-time: uint, ;; hours notice required
    shared-available: bool,
    utilization-rate: uint ;; percentage
  }
)

(define-map capacity-bookings
  { booking-id: uint }
  {
    twin-id: uint,
    requester: principal,
    start-time: uint,
    duration-hours: uint,
    total-cost: uint,
    production-specs: (string-ascii 200),
    quality-requirements: (string-ascii 100),
    status: (string-ascii 20),
    created-at: uint,
    completed: bool
  }
)

(define-map performance-analytics
  { twin-id: uint }
  {
    oee-score: uint, ;; Overall Equipment Effectiveness percentage
    availability-percentage: uint,
    performance-percentage: uint,
    quality-percentage: uint,
    mttr: uint, ;; Mean Time To Repair in hours
    mtbf: uint, ;; Mean Time Between Failures in hours
    cost-per-hour: uint,
    revenue-generated: uint
  }
)

(define-map predictive-models
  { twin-id: uint }
  {
    failure-probability: uint, ;; percentage over next 30 days
    recommended-maintenance: (string-ascii 100),
    confidence-level: uint, ;; percentage
    data-points-analyzed: uint,
    model-accuracy: uint, ;; percentage
    last-updated: uint,
    algorithm-version: (string-ascii 10)
  }
)

;; Public Functions

;; Create a new digital twin of manufacturing equipment
(define-public (create-digital-twin (equipment-type (string-ascii 50)) (model (string-ascii 50)) (serial-number (string-ascii 30)) (location (string-ascii 100)) (capacity-units uint))
  (let
    (
      (twin-id (+ (var-get twin-counter) u1))
    )
    (asserts! (> capacity-units u0) ERR_INVALID_PARAMETERS)
    
    (map-set digital-twins
      { twin-id: twin-id }
      {
        owner: tx-sender,
        equipment-type: equipment-type,
        model: model,
        serial-number: serial-number,
        manufacturing-date: block-height,
        location: location,
        capacity-units: capacity-units,
        current-status: STATUS_OPERATIONAL,
        efficiency-rating: u100,
        last-maintenance: block-height,
        next-predicted-maintenance: (+ block-height u2160), ;; 30 days
        total-operating-hours: u0,
        created-at: block-height,
        active: true
      }
    )
    
    ;; Initialize sensor data
    (map-set equipment-sensors
      { twin-id: twin-id }
      {
        temperature: u25,
        vibration-level: u10,
        power-consumption: u1000,
        output-rate: capacity-units,
        error-count: u0,
        last-reading: block-height,
        sensor-health: true,
        automated-alerts: true
      }
    )
    
    (var-set twin-counter twin-id)
    (var-set total-equipment-value (+ (var-get total-equipment-value) u100000)) ;; Estimated value
    (ok twin-id)
  )
)

;; Update equipment status and sensor data
(define-public (update-equipment-status (twin-id uint) (temperature uint) (vibration uint) (power-consumption uint) (output-rate uint) (error-count uint))
  (let
    (
      (twin-data (unwrap! (map-get? digital-twins { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
      (current-sensors (unwrap! (map-get? equipment-sensors { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
      (new-efficiency (calculate-efficiency temperature vibration output-rate (get capacity-units twin-data)))
      (needs-maintenance (check-maintenance-needed temperature vibration error-count))
    )
    (asserts! (is-eq tx-sender (get owner twin-data)) ERR_UNAUTHORIZED)
    
    ;; Update sensor readings
    (map-set equipment-sensors
      { twin-id: twin-id }
      (merge current-sensors {
        temperature: temperature,
        vibration-level: vibration,
        power-consumption: power-consumption,
        output-rate: output-rate,
        error-count: error-count,
        last-reading: block-height,
        sensor-health: (and (<= temperature MAX_TEMPERATURE) (<= vibration MAX_VIBRATION))
      })
    )
    
    ;; Update twin status based on sensor data
    (map-set digital-twins
      { twin-id: twin-id }
      (merge twin-data {
        efficiency-rating: new-efficiency,
        current-status: (if needs-maintenance STATUS_DEGRADED STATUS_OPERATIONAL),
        total-operating-hours: (+ (get total-operating-hours twin-data) u1)
      })
    )
    
    (ok true)
  )
)

;; Predict maintenance needs based on current data
(define-public (predict-maintenance-needs (twin-id uint) (failure-probability uint) (recommended-action (string-ascii 100)) (confidence uint))
  (let
    (
      (twin-data (unwrap! (map-get? digital-twins { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner twin-data)) ERR_UNAUTHORIZED)
    (asserts! (<= failure-probability u100) ERR_INVALID_PARAMETERS)
    (asserts! (<= confidence u100) ERR_INVALID_PARAMETERS)
    
    (map-set predictive-models
      { twin-id: twin-id }
      {
        failure-probability: failure-probability,
        recommended-maintenance: recommended-action,
        confidence-level: confidence,
        data-points-analyzed: u1000, ;; Simulated data points
        model-accuracy: u85, ;; 85% accuracy
        last-updated: block-height,
        algorithm-version: "v3.2"
      }
    )
    
    ;; Auto-schedule high-priority maintenance
    (if (>= failure-probability u80)
      (begin
        (unwrap! (schedule-maintenance twin-id "predictive" (+ block-height u720) PRIORITY_HIGH u5000 "High failure probability detected") ERR_INVALID_PARAMETERS)
        (ok true)
      )
      (ok true)
    )
  )
)

;; Schedule maintenance for equipment
(define-public (schedule-maintenance (twin-id uint) (maintenance-type (string-ascii 30)) (scheduled-date uint) (priority uint) (estimated-cost uint) (description (string-ascii 200)))
  (let
    (
      (maintenance-id (+ (var-get maintenance-counter) u1))
      (twin-data (unwrap! (map-get? digital-twins { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner twin-data)) ERR_UNAUTHORIZED)
    (asserts! (<= priority PRIORITY_CRITICAL) ERR_INVALID_PARAMETERS)
    
    (map-set maintenance-records
      { maintenance-id: maintenance-id }
      {
        twin-id: twin-id,
        maintenance-type: maintenance-type,
        scheduled-date: scheduled-date,
        completed-date: none,
        technician: none,
        priority-level: priority,
        estimated-cost: estimated-cost,
        actual-cost: none,
        downtime-hours: none,
        description: description,
        completed: false
      }
    )
    
    (var-set maintenance-counter maintenance-id)
    (ok maintenance-id)
  )
)

;; Register as a maintenance provider
(define-public (register-maintenance-provider (company-name (string-ascii 100)) (specializations (list 5 (string-ascii 30))) (certification-level uint))
  (begin
    (asserts! (<= certification-level u5) ERR_INVALID_PARAMETERS)
    
    (map-set maintenance-providers
      { provider: tx-sender }
      {
        company-name: company-name,
        specializations: specializations,
        certification-level: certification-level,
        average-response-time: u24, ;; 24 hours default
        success-rate: u100, ;; 100% default for new providers
        total-jobs-completed: u0,
        total-earnings: u0,
        active: true,
        registration-date: block-height
      }
    )
    
    (var-set provider-counter (+ (var-get provider-counter) u1))
    (ok true)
  )
)

;; Coordinate shared manufacturing capacity
(define-public (offer-shared-capacity (twin-id uint) (hourly-rate uint) (available-hours uint) (minimum-booking uint) (lead-time uint))
  (let
    (
      (twin-data (unwrap! (map-get? digital-twins { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner twin-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get current-status twin-data) STATUS_OPERATIONAL) ERR_EQUIPMENT_OFFLINE)
    (asserts! (> available-hours u0) ERR_INVALID_PARAMETERS)
    
    (map-set shared-capacity
      { twin-id: twin-id }
      {
        available-hours: available-hours,
        hourly-rate: hourly-rate,
        minimum-booking: minimum-booking,
        quality-standards: (list "ISO-9001" "AS9100" "TS16949"),
        booking-lead-time: lead-time,
        shared-available: true,
        utilization-rate: u0
      }
    )
    
    (ok true)
  )
)

;; Book shared manufacturing capacity
(define-public (book-shared-capacity (twin-id uint) (start-time uint) (duration-hours uint) (production-specs (string-ascii 200)))
  (let
    (
      (booking-id (+ (var-get capacity-request-counter) u1))
      (capacity-data (unwrap! (map-get? shared-capacity { twin-id: twin-id }) ERR_INSUFFICIENT_CAPACITY))
      (total-cost (* duration-hours (get hourly-rate capacity-data)))
    )
    (asserts! (get shared-available capacity-data) ERR_INSUFFICIENT_CAPACITY)
    (asserts! (>= duration-hours (get minimum-booking capacity-data)) ERR_INVALID_PARAMETERS)
    (asserts! (<= duration-hours (get available-hours capacity-data)) ERR_INSUFFICIENT_CAPACITY)
    
    (map-set capacity-bookings
      { booking-id: booking-id }
      {
        twin-id: twin-id,
        requester: tx-sender,
        start-time: start-time,
        duration-hours: duration-hours,
        total-cost: total-cost,
        production-specs: production-specs,
        quality-requirements: "Standard manufacturing quality",
        status: "confirmed",
        created-at: block-height,
        completed: false
      }
    )
    
    ;; Update available capacity
    (map-set shared-capacity
      { twin-id: twin-id }
      (merge capacity-data {
        available-hours: (- (get available-hours capacity-data) duration-hours)
      })
    )
    
    (var-set capacity-request-counter booking-id)
    (ok booking-id)
  )
)

;; Track equipment performance metrics
(define-public (update-performance-metrics (twin-id uint) (availability uint) (performance uint) (quality uint) (revenue uint))
  (let
    (
      (twin-data (unwrap! (map-get? digital-twins { twin-id: twin-id }) ERR_TWIN_NOT_FOUND))
      (oee-score (/ (* (* availability performance) quality) u10000)) ;; Calculate OEE
    )
    (asserts! (is-eq tx-sender (get owner twin-data)) ERR_UNAUTHORIZED)
    
    (map-set performance-analytics
      { twin-id: twin-id }
      {
        oee-score: oee-score,
        availability-percentage: availability,
        performance-percentage: performance,
        quality-percentage: quality,
        mttr: u12, ;; 12 hours average
        mtbf: u720, ;; 30 days average
        cost-per-hour: u100,
        revenue-generated: revenue
      }
    )
    
    (ok oee-score)
  )
)

;; Read-only functions

(define-read-only (get-digital-twin (twin-id uint))
  (map-get? digital-twins { twin-id: twin-id })
)

(define-read-only (get-equipment-sensors (twin-id uint))
  (map-get? equipment-sensors { twin-id: twin-id })
)

(define-read-only (get-maintenance-record (maintenance-id uint))
  (map-get? maintenance-records { maintenance-id: maintenance-id })
)

(define-read-only (get-maintenance-provider (provider principal))
  (map-get? maintenance-providers { provider: provider })
)

(define-read-only (get-shared-capacity (twin-id uint))
  (map-get? shared-capacity { twin-id: twin-id })
)

(define-read-only (get-capacity-booking (booking-id uint))
  (map-get? capacity-bookings { booking-id: booking-id })
)

(define-read-only (get-performance-analytics (twin-id uint))
  (map-get? performance-analytics { twin-id: twin-id })
)

(define-read-only (get-predictive-model (twin-id uint))
  (map-get? predictive-models { twin-id: twin-id })
)

(define-read-only (get-platform-statistics)
  {
    total-twins: (var-get twin-counter),
    total-maintenance-jobs: (var-get maintenance-counter),
    total-providers: (var-get provider-counter),
    total-bookings: (var-get capacity-request-counter),
    total-equipment-value: (var-get total-equipment-value),
    total-maintenance-cost: (var-get total-maintenance-cost),
    total-downtime-hours: (var-get total-downtime-hours)
  }
)

;; Private functions

(define-private (calculate-efficiency (temperature uint) (vibration uint) (output-rate uint) (max-capacity uint))
  (let
    (
      (temp-factor (if (<= temperature u60) u100 (- u100 (- temperature u60))))
      (vibration-factor (if (<= vibration u20) u100 (- u100 (* (- vibration u20) u2))))
      (output-factor (/ (* output-rate u100) max-capacity))
    )
    (/ (+ (+ temp-factor vibration-factor) output-factor) u3)
  )
)

(define-private (check-maintenance-needed (temperature uint) (vibration uint) (error-count uint))
  (or
    (> temperature MAX_TEMPERATURE)
    (> vibration MAX_VIBRATION)
    (> error-count u5)
  )
)
