(in-package :gtk-test)

(def-suite gdk-display-manager :in gdk-suite)
(in-suite gdk-display-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplayManager

(test display-manager-class
  ;; Type check
  (is (g:type-is-object "GdkDisplayManager"))
  ;; Check the registered name
  (is (eq 'gdk:display-manager
          (glib:symbol-for-gtype "GdkDisplayManager")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDisplayManager")
          (g:gtype (cffi:foreign-funcall "gdk_display_manager_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplayManager")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkDisplayManager")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDisplayManager")))
  ;; Check the class properties
  (is (equal '("default-display")
             (list-properties "GdkDisplayManager")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplayManager" GDK-DISPLAY-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_manager_get_type")
                       ((DEFAULT-DISPLAY GDK-DISPLAY-MANAGER-DEFAULT-DISPLAY
                         "default-display" "GdkDisplay" T T)))
             (gobject:get-g-type-definition "GdkDisplayManager"))))

;;; --- Properties -------------------------------------------------------------

;;;   gdk-display-manager-default-display

(test display-manager-default-display.1
  (let ((manager (gdk:display-manager-get)))
    (is (typep (gdk:display-manager-default-display manager) 'gdk:display))))

(test display-manager-default-display.2
  (let* ((manager (gdk:display-manager-get))
         (display (gdk:display-manager-default-display manager)))
    (setf (gdk:display-manager-default-display manager) display)
    (is (eq display
            (gdk:display-manager-default-display manager)))))

;;; --- Signals ----------------------------------------------------------------

;;;     display-opened

#+nil
(test display-manager-display-opened-signal
  (let* ((message nil)
         (manager (gdk:display-manager-get))
         (display (gdk:display-manager-default-display manager))
         (handler-id (g-signal-connect manager "display-opened"
                       (lambda (manager display)
                         (setf message "Signal display-opened")
                         (is (typep manager 'gdk:display-manager))
                         (is (typep display 'gdk:display))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit manager "display-opened" display))
    (is (string= "Signal display-opened" message))
    (is-false (g-signal-handler-disconnect manager handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-display-manager-get

(test display-manager-get
  (is (typep (gdk:display-manager-get) 'gdk:display-manager)))

;;;     gdk-display-manager-list-displays

(test display-manager-list-displays
  (let ((manager (gdk:display-manager-get)))
    (is (listp (gdk:display-manager-list-displays manager)))
    (is (every (lambda (x) (typep x 'gdk:display))
               (gdk:display-manager-list-displays manager)))))

;;;     gdk-display-manager-open-display

(test display-manager-open-display
  (let* ((manager (gdk:display-manager-get))
         (name (gdk:display-name (gdk:display-manager-default-display manager))))
    (is-true (gdk:display-manager-open-display manager name))))

;;; --- 2023-5-29 --------------------------------------------------------------
