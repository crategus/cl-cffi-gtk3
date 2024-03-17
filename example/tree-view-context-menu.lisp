;;;; Example Tree View Context Menu
;;;;
;;;; 2024-3-14

(in-package :gtk3-example)

(let* ((statusbar (make-instance 'gtk:statusbar))
       (id (gtk:statusbar-context-id statusbar "Context")))

  (defun create-popup-menu (view event)
    (declare (ignore view))
    (let ((menu (gtk:menu-new))
          (item (gtk:menu-item-new-with-label "Do something")))
      (g:signal-connect item "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:statusbar-pop statusbar id)
                          (gtk:statusbar-push statusbar id "Do something")))
      (gtk:menu-shell-append menu item)
      (gtk:widget-show-all menu)
      (gtk:menu-popup-at-pointer menu event)))

  (defun example-tree-view-context-menu (&optional application)
    (gtk:within-main-loop
      (let ((window (make-instance 'gtk:window
                                   :title "Example Tree View Context Menu"
                                   :application application
                                   :type :toplevel
                                   :default-width 350
                                   :default-height 200))
            (view (create-view-and-model-simple))
            (vbox (make-instance 'gtk:box
                                 :orientation :vertical
                                 :homogeneous nil
                                 :spacing 3)))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        ;; Signal handler for button right clicked
        (g:signal-connect view "button-press-event"
            (lambda (widget event)
              (when (and (eq :button-press (gdk:event-type event))
                         (= 3 (gdk:event-button event)))
                (gtk:statusbar-pop statusbar id)
                (gtk:statusbar-push statusbar id "Right click on tree view")
                (create-popup-menu widget event))))
        ;; Signal handler for keyboard Shift F10
        (g:signal-connect view "popup-menu"
            (lambda (widget)
              (gtk:statusbar-pop statusbar id)
              (gtk:statusbar-push statusbar id "Popup Menu with Shift F10")
              (create-popup-menu widget nil)))
        ;; Pack and show widgets
        (gtk:box-pack-start vbox view)
        (gtk:box-pack-start vbox statusbar :expand nil)
        (gtk:container-add window vbox)
        (gtk:widget-show-all window)))))
