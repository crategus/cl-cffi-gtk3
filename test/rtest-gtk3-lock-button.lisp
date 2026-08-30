(in-package :gtk-test)

(def-suite gtk-lock-button :in gtk-suite)
(in-suite gtk-lock-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLockButton

(test gtk-lock-button-class
  ;; Check type
  (is (g:type-is-object "GtkLockButton"))
  ;; Check registered name
  (is (eq 'gtk:lock-button
          (glib:symbol-for-gtype "GtkLockButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLockButton")
          (g:gtype (cffi:foreign-funcall "gtk_lock_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkLockButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkLockButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkLockButton")))
  ;; Check class properties
  (is (equal '("permission" "text-lock" "text-unlock" "tooltip-lock"
               "tooltip-not-authorized" "tooltip-unlock")
             (glib-test:list-properties "GtkLockButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkLockButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkLockButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkLockButton")))
  ;; Check CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLockButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLockButton" GTK:LOCK-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_lock_button_get_type")
                      ((PERMISSION LOCK-BUTTON-PERMISSION "permission"
                        "GPermission" T T)
                       (TEXT-LOCK LOCK-BUTTON-TEXT-LOCK "text-lock"
                        "gchararray" T T)
                       (TEXT-UNLOCK LOCK-BUTTON-TEXT-UNLOCK "text-unlock"
                        "gchararray" T T)
                       (TOOLTIP-LOCK LOCK-BUTTON-TOOLTIP-LOCK "tooltip-lock"
                        "gchararray" T T)
                       (TOOLTIP-NOT-AUTHORIZED
                        LOCK-BUTTON-TOOLTIP-NOT-AUTHORIZED
                        "tooltip-not-authorized" "gchararray" T T)
                       (TOOLTIP-UNLOCK LOCK-BUTTON-TOOLTIP-UNLOCK
                        "tooltip-unlock" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkLockButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     permission
;;;     text-lock
;;;     text-unlock
;;;     tooltip-lock
;;;     tooltip-not-authorized
;;;     tooltip-unlock

(test gtk-lock-button-properties
  (glib-test:with-check-memory (button permission)
    (is (typep (setf button
                     (gtk:lock-button-new (setf permission
                                                (g:simple-permission-new t))))
               'gtk:lock-button))
    (is (typep (gtk:lock-button-permission button) 'g:simple-permission))
    (is (stringp (gtk:lock-button-text-lock button)))
    (is (stringp (gtk:lock-button-text-unlock button)))
    (is (stringp (gtk:lock-button-tooltip-lock button)))
    (is (stringp (gtk:lock-button-tooltip-not-authorized button)))
    (is (stringp (gtk:lock-button-tooltip-unlock button)))
    (is-false (setf (gtk:lock-button-permission button) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_lock_button_new

(test gtk-lock-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:lock-button-new (g:simple-permission-new t)))
               'gtk:lock-button))
    (is-false (setf (gtk:lock-button-permission button) nil))))

;;; 2026-06-27
