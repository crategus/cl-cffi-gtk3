(in-package :gtk-test)

(def-suite gtk-stack :in gtk-suite)
(in-suite gtk-stack)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackTransitionType

;;;     GtkStack

(test stack-class
  ;; Type check
  (is (g:type-is-object "GtkStack"))
  ;; Check the registered name
  (is (eq 'gtk:stack
          (gobject:symbol-for-gtype "GtkStack")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStack")
          (g:gtype (cffi:foreign-funcall "gtk_stack_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkStack")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStack")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkStack")))
  ;; Check the class properties
  (is (equal '("hhomogeneous" "homogeneous" "interpolate-size"
               "transition-duration" "transition-running" "transition-type"
               "vhomogeneous" "visible-child" "visible-child-name")
             (list-properties "GtkStack")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-style-properties "GtkStack")))
  ;; Get the names of the child properties
  (is (equal '("icon-name" "name" "needs-attention" "position" "title")
             (list-child-properties "GtkStack")))
  ;; Check the signlas
  (is (equal '()
             (list-signals "GtkStack")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkStack" GTK-STACK
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_stack_get_type")
                       ((HHOMOGENEOUS GTK-STACK-HHOMOGENEOUS "hhomogeneous"
                         "gboolean" T T)
                        (HOMOGENEOUS GTK-STACK-HOMOGENEOUS "homogeneous"
                         "gboolean" T T)
                        (INTERPOLATE-SIZE GTK-STACK-INTERPOLATE-SIZE
                         "interpolate-size" "gboolean" T T)
                        (TRANSITION-DURATION GTK-STACK-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-RUNNING GTK-STACK-TRANSITION-RUNNING
                         "transition-running" "gboolean" T NIL)
                        (TRANSITION-TYPE GTK-STACK-TRANSITION-TYPE
                         "transition-type" "GtkStackTransitionType" T T)
                        (VHOMOGENEOUS GTK-STACK-VHOMOGENEOUS "vhomogeneous"
                         "gboolean" T T)
                        (VISIBLE-CHILD GTK-STACK-VISIBLE-CHILD "visible-child"
                         "GtkWidget" T T)
                        (VISIBLE-CHILD-NAME GTK-STACK-VISIBLE-CHILD-NAME
                         "visible-child-name" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkStack"))))

;;; --- Properties -------------------------------------------------------------
;;;
;;;                   gboolean  hhomogeneous         Read / Write
;;;                   gboolean  homogeneous          Read / Write
;;;                   gboolean  interpolate-size     Read / Write
;;;                      guint  transition-duration  Read / Write
;;;                   gboolean  transition-running   Read
;;;     GtkStackTransitionType  transition-type      Read / Write
;;;                   gboolean  vhomogeneous         Read / Write
;;;                  GtkWidget  visible-child        Read / Write
;;;                      gchar  visible-child-name   Read / Write

;;; --- Child Properties -------------------------------------------------------

(test stack-child-properties
  (let ((stack (make-instance 'gtk:stack))
        (child1 (make-instance 'gtk:button))
        (child2 (make-instance 'gtk:button)))
    (is (typep stack 'gtk:stack))
    (is (g:type-is-a (g:type-from-instance stack) "GtkContainer"))
    ;; Add two buttons to the stack
    (is-false (gtk:stack-add-named stack child1 "button1"))
    (is-false (gtk:stack-add-titled stack child2 "button2" "title"))
    ;; Check the child properties for the first button
    (is-false (gtk:stack-child-icon-name stack child1))
    (is (string= "button1" (gtk:stack-child-name stack child1)))
    (is-false (gtk:stack-child-needs-attention stack child1))
    (is (= 0 (gtk:stack-child-position stack child1)))
    (is-false (gtk:stack-child-title stack child1))
    ;; Check the child properties for the second button
    (is-false (gtk:stack-child-icon-name stack child2))
    (is (string= "button2" (gtk:stack-child-name stack child2)))
    (is-false (gtk:stack-child-needs-attention stack child2))
    (is (= 1 (gtk:stack-child-position stack child2)))
    (is (string= "title" (gtk:stack-child-title stack child2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_new
;;;     gtk_stack_add_named
;;;     gtk_stack_add_titled
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_set_visible_child                        Accessor
;;;     gtk_stack_get_visible_child                        Accessor
;;;     gtk_stack_set_visible_child_name                   Accessor
;;;     gtk_stack_get_visible_child_name                   Accessor
;;;     gtk_stack_set_visible_child_full
;;;     gtk_stack_set_homogeneous                          Accessor
;;;     gtk_stack_get_homogeneous                          Accessor
;;;     gtk_stack_set_hhomogeneous                         Accessor
;;;     gtk_stack_get_hhomogeneous                         Accessor
;;;     gtk_stack_set_vhomogeneous                         Accessor
;;;     gtk_stack_get_vhomogeneous                         Accessor
;;;     gtk_stack_set_transition_duration                  Accessor
;;;     gtk_stack_get_transition_duration                  Accessor
;;;     gtk_stack_set_transition_type                      Accessor
;;;     gtk_stack_get_transition_type                      Accessor
;;;     gtk_stack_get_transition_running                   Accessor
;;;     gtk_stack_get_interpolate_size                     Accessor
;;;     gtk_stack_set_interpolate_size                     Accessor

;;; 2021-12-16
