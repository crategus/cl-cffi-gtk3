(in-package :gtk-test)

(def-suite gtk-accel-group :in gtk-suite)
(in-suite gtk-accel-group)

;;;     GtkAccelFlags

(test accel-flags
  ;; Check the type
  (is (g:type-is-flags "GtkAccelFlags"))
  ;; Check the registered name
  (is (eql 'gtk:accel-flags
           (gobject:symbol-for-gtype "GtkAccelFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccelFlags")
          (g:gtype (cffi:foreign-funcall "gtk_accel_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("GTK_ACCEL_VISIBLE" "GTK_ACCEL_LOCKED" "GTK_ACCEL_MASK")
             (list-flags-item-name "GtkAccelFlags")))
  ;; Check the values
  (is (equal '(1 2 7)
             (list-flags-item-value "GtkAccelFlags")))
  ;; Check the nick names
  (is (equal '("visible" "locked" "mask")
             (list-flags-item-nick "GtkAccelFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GtkAccelFlags"
                              GTK-ACCEL-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gtk_accel_flags_get_type")
                              (:VISIBLE 1)
                              (:LOCKED 2)
                              (:MASK 7))
             (gobject:get-g-type-definition "GtkAccelFlags"))))

;;;     GtkAccelGroup

(test accel-group-class
  ;; Type check
  (is (g:type-is-object "GtkAccelGroup"))
  ;; Check the registered name
  (is (eq 'gtk:accel-group
          (gobject:symbol-for-gtype "GtkAccelGroup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccelGroup")
          (g:gtype (cffi:foreign-funcall "gtk_accel_group_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAccelGroup")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAccelGroup")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAccelGroup")))
  ;; Check the class properties
  (is (equal '("is-locked" "modifier-mask")
             (list-properties "GtkAccelGroup")))
  ;; Check the signals
  (is (equal '("accel-activate" "accel-changed")
             (list-signals "GtkAccelGroup")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAccelGroup" GTK-ACCEL-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_accel_group_get_type")
                       ((IS-LOCKED GTK-ACCEL-GROUP-IS-LOCKED "is-locked"
                         "gboolean" T NIL)
                        (MODIFIER-MASK GTK-ACCEL-GROUP-MODIFIER-MASK
                         "modifier-mask" "GdkModifierType" T NIL)))
             (gobject:get-g-type-definition "GtkAccelGroup"))))

;;; --- Properties -------------------------------------------------------------

(test accel-group-properties
  (let ((group (gtk:accel-group-new)))
    ;; is-locked
    (is-false (gtk:accel-group-is-locked group))
    ;; is-locked is not writable
    (signals (error) (setf (gtk:accel-group-is-locked group) nil))
    ;; modifier-mask
    (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK
                 :HYPER-MASK :META-MASK)
               (gtk:accel-group-modifier-mask group)))
    ;; modifier-mask is not writable
    (signals (error) (setf (gtk:accel-group-modifier-mask group)
                           '(:shift-mask)))))

;;; --- Signals ----------------------------------------------------------------

;;;     accel-activate
;;;     accel-changed

;;; --- Functions --------------------------------------------------------------

;;;   gtk_accel_group_new

(test accel-group-new
  (is (eq 'gtk:accel-group (type-of (gtk:accel-group-new)))))

;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key

;;;   gtk_accel_group_activate

(defun activate-action (action)
  (let ((name (gtk:action-name action))
        (type (g:object-type-name action)))
    (format t "action ~A of type ~A" name type)))

(defvar *entries*
        (list
          (list "FileMenu" nil "_File")               ; name, stock id, label
          (list "OpenMenu" nil "_Open")               ; name, stock id, label
          (list "PreferencesMenu" nil "_Preferences") ; name, stock id, label
          (list "ColorMenu" nil "_Color")             ; name, stock id, label
          (list "ShapeMenu" nil "_Shape")             ; name, stock id, label
          (list "HelpMenu" nil "_Help")               ; name, stock id, label
          (list "New" "gtk-new"                       ; name, stock id
                "_New" "<control>N"                   ; label, accelerator
                "Create a new file"                   ; tooltip
                (lambda (action)
                  (activate-action action)))
          (list "File1" nil                           ; name, stock id
                "File1" nil                           ; label, accelerator
                "Open first file"                     ; tooltip
                #'activate-action)
          (list "Save" "gtk-save"                     ; name, stock id
                "_Save" "<control>S"                  ; label, accelerator
                "Save current file"                   ; tooltip
                #'activate-action)
          (list "SaveAs" "gtk-save"                   ; name, stock id
                "Save _As..." nil                     ; label, accelerator
                "Save to a file"                      ; tooltip
                #'activate-action)
          (list "Quit" "gtk-quit"                     ; name, stock id
                "_Quit" "<control>q"                  ; label, accelerator
                "Quit"                                ; tooltip
                #'activate-action)
          (list "About" nil                           ; name, stock id
                "_About" "<control>A"                 ; label, accelerator
                "About"                               ; tooltip
                #'activate-action)
          (list "Logo" "demo-gtk-logo"                ; name, stock id
                nil nil                               ; label, accelerator
                "GTK+"                                ; tooltip
                #'activate-action)))

(test accel-group-activate
 (let ((window (make-instance 'gtk:window
                              :type :toplevel))
       (action-group (make-instance 'gtk:action-group
                                    :name "AppWindowActions"))
       (ui-info (make-instance 'gtk:ui-manager)))
    (gtk:widget-realize window)
    ;; gtk:action-group-add-actions is not exported
    (gtk::action-group-add-actions action-group *entries*)
    (gtk:ui-manager-insert-action-group ui-info action-group 0)
    (gtk:window-add-accel-group window (gtk:ui-manager-accel-group ui-info))
    (let ((accel-group (gtk:ui-manager-accel-group ui-info)))
      (is (eq 'gtk:accel-group (type-of accel-group)))
; This does not work as expected.
;      (is-true (gtk:accel-group-activate accel-group "<Control>q" window 113 '(:control-mask)))
)))

;;;     gtk_accel_group_lock
;;;     gtk_accel_group_unlock
;;;     gtk_accel_group_get_is_locked

;;;     gtk_accel_group_from_accel_closure
;;;     gtk_accel_group_get_modifier_mask
;;;     gtk_accel_groups_activate
;;;     gtk_accel_groups_from_object
;;;     gtk_accel_group_find

;;;     GtkAccelKey

;;;   gtk_accelerator_valid

(test accelerator-valid
  (is-true (gtk:accelerator-valid 97 '(:control-mask)))
  (is-true (gtk:accelerator-valid 65470 '(:shift-mask :mod1-mask)))
  (is-true (gtk:accelerator-valid 122 '(:release-mask)))
  (is-true (gtk:accelerator-valid 45 '(:control-mask)))
  (is-true (gtk:accelerator-valid 113 '(:control-mask))))

;;;   gtk_accelerator_parse

(test accelerator-parse
  (is (equal '(97 (:control-mask))
             (multiple-value-list (gtk:accelerator-parse "<Control>a"))))
  (is (equal '(65470 (:shift-mask :mod1-mask))
             (multiple-value-list (gtk:accelerator-parse "<Shift><Alt>F1"))))
  (is (equal '(122 (:release-mask))
             (multiple-value-list (gtk:accelerator-parse "<Release>z"))))
  (is (equal '(45 (:control-mask))
             (multiple-value-list (gtk:accelerator-parse "<Control>minus"))))
  (is (equal '(113 (:control-mask))
             (multiple-value-list (gtk:accelerator-parse "<ctrl>q")))))

;;;   gtk_accelerator_name

(test accelerator-name
  (is (string= "<Primary>a"
               (gtk:accelerator-name 97 '(:control-mask))))
  (is (string= "<Shift><Alt>F1"
               (gtk:accelerator-name 65470 '(:shift-mask :mod1-mask))))
  (is (string= "<Release>z"
               (gtk:accelerator-name 122 '(:release-mask))))
  (is (string= "<Primary>minus"
               (gtk:accelerator-name 45 '(:control-mask))))
  (is (string= "<Primary>q"
               (gtk:accelerator-name 113 '(:control-mask)))))

;;;   gtk_accelerator_get_label

#-windows
(test accelerator-label
  (is (string= "Strg+A"
               (gtk:accelerator-label 97 '(:control-mask))))
  (is (string= "Umschalt+Alt+F1"
               (gtk:accelerator-label 65470 '(:shift-mask :mod1-mask))))
  (is (string= "Z"
               (gtk:accelerator-label 122 '(:release-mask))))
  (is (string= "Strg+-"
               (gtk:accelerator-label 45 '(:control-mask))))
  (is (string= "Strg+Q"
               (gtk:accelerator-label 113 '(:control-mask)))))

#+windows
(test accelerator-label
  (is (string= "Ctrl+A"
               (gtk:accelerator-label 97 '(:control-mask))))
  (is (string= "Shift+Alt+F1"
               (gtk:accelerator-label 65470 '(:shift-mask :mod1-mask))))
  (is (string= "Z"
               (gtk:accelerator-label 122 '(:release-mask))))
  (is (string= "Ctrl+-"
               (gtk:accelerator-label 45 '(:control-mask))))
  (is (string= "Ctrl+Q"
               (gtk:accelerator-label 113 '(:control-mask)))))

;;;   gtk_accelerator_parse_with_keycode
;;;   gtk_accelerator_name_with_keycode
;;;   gtk_accelerator_get_label_with_keycode

;;;   gtk_accelerator_set_default_mod_mask
;;;   gtk_accelerator_get_default_mod_mask

(test accelerator-default-mod-mask
  (is (equal '(:SHIFT-MASK
               :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK :META-MASK)
             (gtk:accelerator-default-mod-mask)))
  (is (equal '(:control-mask :shift-mask :mod1-mask)
             (setf (gtk:accelerator-default-mod-mask)
                   '(:control-mask :shift-mask :mod1-mask))))
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK)
             (gtk:accelerator-default-mod-mask)))
  (is (equal '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK :HYPER-MASK
               :META-MASK)
             (setf (gtk:accelerator-default-mod-mask)
                   '(:SHIFT-MASK :CONTROL-MASK :MOD1-MASK :SUPER-MASK
                     :HYPER-MASK :META-MASK)))))

;;; --- 2023-3-1 ---------------------------------------------------------------
