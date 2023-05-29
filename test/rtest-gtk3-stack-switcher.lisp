(in-package :gtk-test)

(def-suite gtk-stack-switcher :in gtk-suite)
(in-suite gtk-stack-switcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSwitcher

(test stack-switcher-class
  ;; Type check
  (is (g:type-is-object "GtkStackSwitcher"))
  ;; Check the registered name
  (is (eq 'gtk:stack-switcher
          (glib:symbol-for-gtype "GtkStackSwitcher")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStackSwitcher")
          (g:gtype (cffi:foreign-funcall "gtk_stack_switcher_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkStackSwitcher")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStackSwitcher")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkStackSwitcher")))
  ;; Check the class properties
  (is (equal '("icon-size" "stack")
             (list-properties "GtkStackSwitcher")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-style-properties "GtkStackSwitcher")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkStackSwitcher")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkStackSwitcher")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStackSwitcher" GTK-STACK-SWITCHER
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_stack_switcher_get_type")
                       ((ICON-SIZE GTK-STACK-SWITCHER-ICON-SIZE "icon-size"
                         "gint" T T)
                        (STACK GTK-STACK-SWITCHER-STACK "stack" "GtkStack" T
                         T)))
             (gobject:get-g-type-definition "GtkStackSwitcher"))))

;;; --- Properties -------------------------------------------------------------

;;;     gint      icon-size  Read / Write
;;;     GtkStack  stack      Read / Write / Construct

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_switcher_new
;;;     gtk_stack_switcher_set_stack                       Accessor
;;;     gtk_stack_switcher_get_stack                       Accessor

;;; --- 2023-5-29 --------------------------------------------------------------
