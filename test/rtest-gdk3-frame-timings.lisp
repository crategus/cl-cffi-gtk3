(in-package :gtk-test)

(def-suite gdk-frame-timings :in gdk-suite)
(in-suite gdk-frame-timings)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameTimings

(test frame-timings-structure
  ;; Type check
  (is (g:type-is-a (g:gtype "GdkFrameTimings") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkFrameTimings")
          (g:gtype (cffi:foreign-funcall "gdk_frame_timings_get_type" :size)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_timings_ref
;;;     gdk_frame_timings_unref

;;;     gdk_frame_timings_get_frame_counter

(test frame-timings-frame-counter
  (let ((window (make-instance 'gtk:window :type :toplevel)))
    (is-false (gtk:widget-realize window))
    (let* ((frame-clock (gtk:widget-frame-clock window))
           (timings (gdk:frame-clock-current-timings frame-clock)))

      (is (typep frame-clock 'gdk:frame-clock))
      (is (typep timings 'gdk:frame-timings))

;      (is-false (gdk-frame-timings-frame-counter timings))

)))

;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time

;;; 2022-12-12
