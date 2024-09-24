(in-package :gtk-test)

(def-suite gtk-stack :in gtk-suite)
(in-suite gtk-stack)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackTransitionType

(test gtk-stack-transition-type
  ;; Check type
  (is (g:type-is-enum "GtkStackTransitionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackTransitionType")
          (g:gtype (cffi:foreign-funcall "gtk_stack_transition_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:stack-transition-type
          (glib:symbol-for-gtype "GtkStackTransitionType")))
  ;; Check names
  (is (equal '("GTK_STACK_TRANSITION_TYPE_NONE"
               "GTK_STACK_TRANSITION_TYPE_CROSSFADE"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_LEFT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_UP"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_DOWN"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_LEFT_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_SLIDE_UP_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_UP"
               "GTK_STACK_TRANSITION_TYPE_OVER_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_LEFT"
               "GTK_STACK_TRANSITION_TYPE_OVER_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_UNDER_UP"
               "GTK_STACK_TRANSITION_TYPE_UNDER_DOWN"
               "GTK_STACK_TRANSITION_TYPE_UNDER_LEFT"
               "GTK_STACK_TRANSITION_TYPE_UNDER_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_OVER_UP_DOWN"
               "GTK_STACK_TRANSITION_TYPE_OVER_DOWN_UP"
               "GTK_STACK_TRANSITION_TYPE_OVER_LEFT_RIGHT"
               "GTK_STACK_TRANSITION_TYPE_OVER_RIGHT_LEFT")
             (glib-test:list-enum-item-names "GtkStackTransitionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
             (glib-test:list-enum-item-values "GtkStackTransitionType")))
  ;; Check nick names
  (is (equal '("none" "crossfade" "slide-right" "slide-left" "slide-up"
               "slide-down" "slide-left-right" "slide-up-down" "over-up"
               "over-down" "over-left" "over-right" "under-up" "under-down"
               "under-left" "under-right" "over-up-down" "over-down-up"
               "over-left-right" "over-right-left")
             (glib-test:list-enum-item-nicks "GtkStackTransitionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkStackTransitionType"
                                    GTK:STACK-TRANSITION-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_stack_transition_type_get_type")
                       (:NONE 0)
                       (:CROSSFADE 1)
                       (:SLIDE-RIGHT 2)
                       (:SLIDE-LEFT 3)
                       (:SLIDE-UP 4)
                       (:SLIDE-DOWN 5)
                       (:SLIDE-LEFT-RIGHT 6)
                       (:SLIDE-UP-DOWN 7)
                       (:OVER-UP 8)
                       (:OVER-DOWN 9)
                       (:OVER-LEFT 10)
                       (:OVER-RIGHT 11)
                       (:UNDER-UP 12)
                       (:UNDER-DOWN 13)
                       (:UNDER-LEFT 14)
                       (:UNDER-RIGHT 15)
                       (:OVER-UP-DOWN 16)
                       (:OVER-DOWN-UP 17)
                       (:OVER-LEFT-RIGHT 18)
                       (:OVER-RIGHT-LEFT 19))
             (gobject:get-gtype-definition "GtkStackTransitionType"))))

;;;     GtkStack

(test gtk-stack-class
  ;; Check type
  (is (g:type-is-object "GtkStack"))
  ;; Check registered name
  (is (eq 'gtk:stack
          (glib:symbol-for-gtype "GtkStack")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStack")
          (g:gtype (cffi:foreign-funcall "gtk_stack_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkStack")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStack")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkStack")))
  ;; Check class properties
  (is (equal '("hhomogeneous" "homogeneous" "interpolate-size"
               "transition-duration" "transition-running" "transition-type"
               "vhomogeneous" "visible-child" "visible-child-name")
             (glib-test:list-properties "GtkStack")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkStack")))
  ;; Check child properties
  (is (equal '("icon-name" "name" "needs-attention" "position" "title")
             (gtk-test:list-child-properties "GtkStack")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStack")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStack" GTK:STACK
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_stack_get_type")
                       ((HHOMOGENEOUS STACK-HHOMOGENEOUS
                         "hhomogeneous" "gboolean" T T)
                        (HOMOGENEOUS STACK-HOMOGENEOUS
                         "homogeneous" "gboolean" T T)
                        (INTERPOLATE-SIZE STACK-INTERPOLATE-SIZE
                         "interpolate-size" "gboolean" T T)
                        (TRANSITION-DURATION STACK-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-RUNNING STACK-TRANSITION-RUNNING
                         "transition-running" "gboolean" T NIL)
                        (TRANSITION-TYPE STACK-TRANSITION-TYPE
                         "transition-type" "GtkStackTransitionType" T T)
                        (VHOMOGENEOUS STACK-VHOMOGENEOUS
                         "vhomogeneous" "gboolean" T T)
                        (VISIBLE-CHILD STACK-VISIBLE-CHILD
                         "visible-child" "GtkWidget" T T)
                        (VISIBLE-CHILD-NAME STACK-VISIBLE-CHILD-NAME
                         "visible-child-name" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkStack"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-properties
  (let ((stack (make-instance 'gtk:stack)))
    (is-true (gtk:stack-hhomogeneous stack))
    (is-true (gtk:stack-homogeneous stack))
    (is-false (gtk:stack-interpolate-size stack))
    (is (= 200 (gtk:stack-transition-duration stack)))
    (is-false (gtk:stack-transition-running stack))
    (is (eq :none (gtk:stack-transition-type stack)))
    (is-true (gtk:stack-vhomogeneous stack))
    (is-false (gtk:stack-visible-child stack))
    (is-false (gtk:stack-visible-child-name stack))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-stack-child-properties
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

(test gtk-stack-new
  (is (typep (gtk:stack-new) 'gtk:stack)))

;;;     gtk_stack_add_named
;;;     gtk_stack_add_titled
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_set_visible_child_full

;;; 2024-9-21
