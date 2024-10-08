(in-package :gtk-test)

(def-suite gtk-stack-sidebar :in gtk-suite)
(in-suite gtk-stack-sidebar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSidebar

(test gtk-stack-sidebar-class
  ;; Check type
  (is (g:type-is-object "GtkStackSidebar"))
  ;; Check registered name
  (is (eq 'gtk:stack-sidebar
          (glib:symbol-for-gtype "GtkStackSidebar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackSidebar")
          (g:gtype (cffi:foreign-funcall "gtk_stack_sidebar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkStackSidebar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStackSidebar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkStackSidebar")))
  ;; Check class properties
  (is (equal '("stack")
             (glib-test:list-properties "GtkStackSidebar")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkStackSidebar")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkStackSidebar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStackSidebar")))
  ;; CSS information
  (is (string= "stacksidebar"
               (gtk:widget-class-css-name "GtkStackSidebar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStackSidebar" GTK:STACK-SIDEBAR
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_stack_sidebar_get_type")
                       ((STACK STACK-SIDEBAR-STACK "stack" "GtkStack" T T)))
             (gobject:get-gtype-definition "GtkStackSidebar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-sidebar-stack
  (let ((sidebar (make-instance 'gtk:stack-sidebar)))
    (is-false (gtk:stack-sidebar-stack sidebar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_sidebar_new

(test gtk-stack-sidebar-new
  (is (typep (gtk:stack-sidebar-new) 'gtk:stack-sidebar)))

;;; 2024-9-21
