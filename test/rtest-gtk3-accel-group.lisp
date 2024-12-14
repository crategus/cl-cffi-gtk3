(in-package :gtk-test)

(def-suite gtk-accel-group :in gtk-suite)
(in-suite gtk-accel-group)

;;;     GtkAccelFlags

(test gtk-accel-flags
  ;; Check type
  (is (g:type-is-flags "GtkAccelFlags"))
  ;; Check registered name
  (is (eql 'gtk:accel-flags
           (glib:symbol-for-gtype "GtkAccelFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccelFlags")
          (g:gtype (cffi:foreign-funcall "gtk_accel_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_ACCEL_VISIBLE" "GTK_ACCEL_LOCKED" "GTK_ACCEL_MASK")
             (glib-test:list-flags-item-names "GtkAccelFlags")))
  ;; Check values
  (is (equal '(1 2 7)
             (glib-test:list-flags-item-values "GtkAccelFlags")))
  ;; Check nick names
  (is (equal '("visible" "locked" "mask")
             (glib-test:list-flags-item-nicks "GtkAccelFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkAccelFlags" GTK:ACCEL-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_accel_flags_get_type")
                       (:VISIBLE 1)
                       (:LOCKED 2)
                       (:MASK 7))
             (gobject:get-gtype-definition "GtkAccelFlags"))))

;;;     GtkAccelGroup

(test gtk-accel-group-class
  ;; Check type
  (is (g:type-is-object "GtkAccelGroup"))
  ;; Check registered name
  (is (eq 'gtk:accel-group
          (glib:symbol-for-gtype "GtkAccelGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccelGroup")
          (g:gtype (cffi:foreign-funcall "gtk_accel_group_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkAccelGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAccelGroup")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkAccelGroup")))
  ;; Check class properties
  (is (equal '("is-locked" "modifier-mask")
             (glib-test:list-properties "GtkAccelGroup")))
  ;; Check signals
  (is (equal '("accel-activate" "accel-changed")
             (glib-test:list-signals "GtkAccelGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAccelGroup" GTK:ACCEL-GROUP
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_accel_group_get_type")
                       ((IS-LOCKED ACCEL-GROUP-IS-LOCKED
                         "is-locked" "gboolean" T NIL)
                        (MODIFIER-MASK ACCEL-GROUP-MODIFIER-MASK
                         "modifier-mask" "GdkModifierType" T NIL)))
             (gobject:get-gtype-definition "GtkAccelGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-accel-group-properties
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

(test gtk-accel-group-new
  (is (eq 'gtk:accel-group (type-of (gtk:accel-group-new)))))

;;;     gtk_accel_group_connect
;;;     gtk_accel_group_connect_by_path
;;;     gtk_accel_group_disconnect
;;;     gtk_accel_group_disconnect_key

;;;   gtk_accel_group_activate

;; TODO: This test uses the deprecated GTK:UI-MANAGER object. Rework it.

#+nil
(defun activate-action (action)
  (let ((name (gtk:action-name action))
        (type (g:type-from-instance action)))
    (format t "action ~A of type ~A" name type)))

#+nil
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

#+nil
(test gtk-accel-group-activate
 (let ((window (make-instance 'gtk:window
                              :type :toplevel))
       (action-group (make-instance 'gtk:action-group
                                    :name "AppWindowActions"))
       (ui-info (make-instance 'gtk:ui-manager)))
    (gtk:widget-realize window)
    (gtk:action-group-add-actions action-group *entries*)
    (gtk:ui-manager-insert-action-group ui-info action-group 0)
    (gtk:window-add-accel-group window (gtk:ui-manager-accel-group ui-info))
    (let ((accel-group (gtk:ui-manager-accel-group ui-info)))
      (is (eq 'gtk:accel-group (type-of accel-group)))
; This does not work as expected.
;      (is-true (gtk:accel-group-activate accel-group "<Control>q" window 113 '(:control-mask)))

    ;; Check memory management
    (is-false (gtk:window-remove-accel-group window accel-group))
    (is-false (gtk:ui-manager-remove-action-group ui-info action-group))
    (is-false (gtk:widget-destroy window))
    (is (= 1 (g:object-ref-count window)))
    (is (= 1 (g:object-ref-count accel-group)))
    (is (= 1 (g:object-ref-count action-group)))
    (is (= 1 (g:object-ref-count ui-info)))

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

(test gtk-accelerator-valid
  (is-true (gtk:accelerator-valid 97 '(:control-mask)))
  (is-true (gtk:accelerator-valid 65470 '(:shift-mask :mod1-mask)))
  (is-true (gtk:accelerator-valid 122 '(:release-mask)))
  (is-true (gtk:accelerator-valid 45 '(:control-mask)))
  (is-true (gtk:accelerator-valid 113 '(:control-mask))))

;;;   gtk_accelerator_parse

(test gtk-accelerator-parse
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

(test gtk-accelerator-name
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
(test gtk-accelerator-label
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
(test gtk-accelerator-label
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

;;;   gtk_accelerator_parse_with_keycode
;;;   gtk_accelerator_name_with_keycode
;;;   gtk_accelerator_get_label_with_keycode

;;;   gtk_accelerator_set_default_mod_mask
;;;   gtk_accelerator_get_default_mod_mask

(test gtk-accelerator-default-mod-mask
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

;;; 2024-12-14
