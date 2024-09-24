(in-package :gtk-test)

(def-suite gtk-action-group :in gtk-suite)
(in-suite gtk-action-group)

(defvar *verbose-gtk-action-group* nil)

;;;   GtkActionGroup

(test gtk-action-group-class
  ;; Check type
  (is (g:type-is-object "GtkActionGroup"))
  ;; Check registered name
  (is (eq 'gtk:action-group
          (glib:symbol-for-gtype "GtkActionGroup")))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkActionGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkActionGroup")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkActionGroup")))
  ;; Check class properties
  (is (equal '("accel-group" "name" "sensitive" "visible")
             (glib-test:list-properties "GtkActionGroup")))
  ;; Check signals
  (is (equal '("connect-proxy" "disconnect-proxy" "post-activate"
               "pre-activate")
             (glib-test:list-signals "GtkActionGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkActionGroup" GTK:ACTION-GROUP
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_action_group_get_type")
                       ((ACCEL-GROUP ACTION-GROUP-ACCEL-GROUP
                         "accel-group" "GtkAccelGroup" T T)
                        (NAME ACTION-GROUP-NAME "name" "gchararray" T NIL)
                        (SENSITIVE ACTION-GROUP-SENSITIVE
                         "sensitive" "gboolean" T T)
                        (VISIBLE ACTION-GROUP-VISIBLE "visible" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkActionGroup"))))

;;; --- Access the properties --------------------------------------------------

;;;   gtk-action-group-accel-group

(test gtk-action-group-accel-group
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is-false (gtk:action-group-accel-group group))
    (setf (gtk:action-group-accel-group group) (gtk:accel-group-new))
    (is (typep (gtk:action-group-accel-group group) 'gtk:accel-group))))

;;;   gtk-action-group-name

(test gtk-action-group-name
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is (equal "AppWindowActions" (gtk:action-group-name group)))))

;;;   gtk-action-group-sensitive

(test gtk-action-group-sensitive
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is-true (gtk:action-group-sensitive group))
    (setf (gtk:action-group-sensitive group) nil)
    (is-false (gtk:action-group-sensitive group))))

;;;   gtk-action-group-visible

(test gtk-action-group-visible
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is-true (gtk:action-group-visible group))
    (setf (gtk:action-group-visible group) nil)
    (is-false (gtk:action-group-visible group))))

;;; --- Check functions --------------------------------------------------------

;;;   gtk-action-group-new
;;;   gtk_action_group_get_action
;;;   gtk-action-group-list-actions
;;;   gtk_action_group_add_action
;;;   gtk_action_group_add_action_with_accel
;;;   gtk-action-group-remove-action

;;;    GtkActionEntry

;;;    gtk-action-group-add-actions
;;;    gtk-action-group-add-actions-full

;;;    GtkToggleActionEntry

;;;    gtk-action-group-add-toggle-actions
;;;    gtk-action-group-add-toggle-actions-full

;;;    GtkRadioActionEntry

;;;    gtk-action-group-add-radio-actions
;;;    gtk-action-group-add-radio-actions-full
;;;    gtk_action_group_set_translate_func
;;;    gtk_action_group_set_translation_domain
;;;    gtk_action_group_translate_string

;;; 2024-9-23
