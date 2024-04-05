(in-package :gtk-test)

(def-suite gdk-frame-timings :in gdk-suite)
(in-suite gdk-frame-timings)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameTimings

(test gdk-frame-timings-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkFrameTimings"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFrameTimings")
          (g:gtype (cffi:foreign-funcall "gdk_frame_timings_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:frame-timings
          (glib:symbol-for-gtype "GdkFrameTimings"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_timings_ref
;;;     gdk_frame_timings_unref

;;;     gdk_frame_timings_get_frame_counter

;; TODO: Create an example which works

(test gdk-frame-timings-frame-counter
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let* ((frame-clock (gtk:widget-frame-clock window))
           (timings (gdk:frame-clock-current-timings frame-clock)))
      (is (typep frame-clock 'gdk:frame-clock))
      ;; No gdk:fram-timings instance
      (is-false timings)
)))

;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time

;;; --- 2023-7-19 --------------------------------------------------------------
