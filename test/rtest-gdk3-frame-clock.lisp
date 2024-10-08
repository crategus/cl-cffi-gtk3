(in-package :gtk-test)

(def-suite gdk-frame-clock :in gdk-suite)
(in-suite gdk-frame-clock)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameClockPhase

;;;     GdkFrameClock

;;; --- Signals ----------------------------------------------------------------

;;;     void  after-paint      Run Last
;;;     void  before-paint     Run Last
;;;     void  flush-events     Run Last
;;;     void  layout           Run Last
;;;     void  paint            Run Last
;;;     void  resume-events    Run Last
;;;     void  update           Run Last

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_clock_frame_time

(test gdk-frame-clock-frame-time
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))
      (is (typep frame-clock 'gdk:frame-clock))
      (is (integerp (gdk:frame-clock-frame-time frame-clock))))))

;;;     gdk_frame_clock_request_phase

(test gdk-frame-clock-request-phase
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))

      (is-false (gdk:frame-clock-request-phase frame-clock :update))

)))

;;;     gdk_frame_clock_begin_updating
;;;     gdk_frame_clock_end_updating

(test gdk-frame-clock-updating
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))

      (is-false (gdk:frame-clock-begin-updating frame-clock))
      (is-false (gdk:frame-clock-end-updating frame-clock))

)))

;;;     gdk_frame_clock_frame_counter

(test gdk-frame-clock-frame-counter
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))

      (is (integerp (gdk:frame-clock-frame-counter frame-clock)))

)))

;;;     gdk_frame_clock_history_start

(test gdk-frame-clock-frame-counter
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))

      (is (integerp (gdk:frame-clock-history-start frame-clock)))

)))

;;;     gdk_frame_clock_timings

(test gdk-frame-clock-timings
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))
      ;; No gdk:frame-timing instance
      (is-false (gdk:frame-clock-timings frame-clock 0))
)))

;;;     gdk_frame_clock_current_timings

;; TODO: Create an example with a gdk:frame-timings instance

(test gdk-frame-clock-current-timings
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))
      (is (typep frame-clock 'gdk:frame-clock))
      ;; No gdk:frame-timing instance
      (is-false (gdk:frame-clock-current-timings frame-clock))
)))

;;;     gdk_frame_clock_refresh_info

(test gdk-frame-clock-refresh-info
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let ((frame-clock (gtk:widget-frame-clock window)))
      (is (typep frame-clock 'gdk:frame-clock))

      (is (every #'integerp
                 (multiple-value-list
                   (gdk:frame-clock-refresh-info frame-clock 0)))))))

;;; 2024-9-22
