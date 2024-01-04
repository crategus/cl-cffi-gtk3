;;;; Example Menu Popup
;;;;
;;; 2024-1-4

(in-package :gtk3-example)

(defun example-menu-popup (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Example Popup Menu"
                                 :type :toplevel
                                 :application application
                                 :default-width 300
                                 :default-height 180))
          (button (gtk:button-new-with-label "Click me")))
      ;; Create pop-up menu for button
      (let ((popup (make-instance 'gtk:menu))
            (bigitem (gtk:menu-item-new-with-label "Larger"))
            (smallitem (gtk:menu-item-new-with-label "Smaller")))
        (gtk:menu-shell-append popup bigitem)
        (gtk:menu-shell-append popup smallitem)
        (gtk:widget-show-all popup)
        ;; Signal handler to pop up the menu
        (g:signal-connect button "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (gtk:menu-popup-at-pointer popup event)
             t)))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
