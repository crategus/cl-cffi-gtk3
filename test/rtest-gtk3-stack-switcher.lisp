(in-package :gtk-test)

(def-suite gtk-stack-switcher :in gtk-suite)
(in-suite gtk-stack-switcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSwitcher

(test gtk-stack-switcher-class
  ;; Check type
  (is (g:type-is-object "GtkStackSwitcher"))
  ;; Check registered name
  (is (eq 'gtk:stack-switcher
          (glib:symbol-for-gtype "GtkStackSwitcher")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackSwitcher")
          (g:gtype (cffi:foreign-funcall "gtk_stack_switcher_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkStackSwitcher")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStackSwitcher")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkStackSwitcher")))
  ;; Check class properties
  (is (equal '("icon-size" "stack")
             (glib-test:list-properties "GtkStackSwitcher")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkStackSwitcher")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkStackSwitcher")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStackSwitcher")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStackSwitcher" GTK:STACK-SWITCHER
                       (:SUPERCLASS GTK:BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_stack_switcher_get_type")
                       ((ICON-SIZE STACK-SWITCHER-ICON-SIZE
                         "icon-size" "gint" T T)
                        (STACK STACK-SWITCHER-STACK "stack" "GtkStack" T T)))
             (gobject:get-gtype-definition "GtkStackSwitcher"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-switcher-properties
  (let ((switcher (make-instance 'gtk:stack-switcher)))
    (is (= 1 (gtk:stack-switcher-icon-size switcher)))
    (is-false (gtk:stack-switcher-stack switcher))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_switcher_new

(test gtk-stack-switcher-new
  (is (typep (gtk:stack-switcher-new) 'gtk:stack-switcher)))

;;; 2024-9-21
