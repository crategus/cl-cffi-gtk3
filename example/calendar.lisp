;;;; Calendar Widget
;;;;
;;;; 2023-12-30

(in-package :gtk3-example)

(defun example-calendar (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Calendar"
                                 :type :toplevel
                                 :application application
;                                 :border-width 24
                                 :default-width 250
                                 :default-height 100))
          (frame (make-instance 'gtk:frame))
          (calendar (make-instance 'gtk:calendar
                                   :show-week-numbers t
                                   :show-details nil)))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Connect a signal handler to print the selected day
      (g:signal-connect calendar "day-selected"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "selected: year ~A month ~A day ~A~%"
                                  (gtk:calendar-year calendar)
                                  (gtk:calendar-month calendar)
                                  (gtk:calendar-day calendar))))
      ;; Install a calendar detail function
      (gtk:calendar-set-detail-func calendar
              (lambda (calendar year month day)
                (declare (ignore calendar year month))
                (when (= day 12)
                  "This day has a tooltip.")))
      ;; Mark a day
      (gtk:calendar-mark-day calendar 6)
      (gtk:container-add frame calendar)
      (gtk:container-add window frame)
      (gtk:widget-show-all window))))
