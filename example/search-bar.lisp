;;;; Example Search Bar
;;;;
;;;; 2025-07-21

;; TODO: Does not handle key press events correctly. What is the problem?

(in-package :gtk3-example)

(defun example-search-bar (&optional application)
  (gtk:within-main-loop
    (let* ((window (make-instance 'gtk:application-window
                                  :type :toplevel
                                  :title "Search Bar"
                                  :application application
                                  :default-width 250
                                  :default-height 120))
           (box (make-instance 'gtk:box
                               :orientation :horizontal
                               :spacing 6))
           (entry (make-instance 'gtk:search-entry
                                 :text "Search Entry"))
           (button (make-instance 'gtk:menu-button))
           (searchbar (gtk:search-bar-new)))
      (gtk:search-bar-connect-entry searchbar entry)
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (gtk:widget-destroy widget)
                          (gtk:leave-gtk-main)))
      (g:signal-connect window "key-press-event"
              (lambda (window event)
                (declare (ignore window))
                (format t "in KEY-PRESS-EVENT : ~a~%" event)
                (gtk:search-bar-handle-event searchbar event)))
      (gtk:container-add window searchbar)
      (gtk:container-add searchbar box)
      (gtk:box-pack-start box entry)
      (gtk:box-pack-start box button)
      (gtk:widget-show-all window))))
