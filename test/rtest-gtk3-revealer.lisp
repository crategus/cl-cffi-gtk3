(in-package :gtk-test)

(def-suite gtk-revealer :in gtk-suite)
(in-suite gtk-revealer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRevealerTransitionType

(test gtk-revealer-transition-type
  ;; Check type
  (is (g:type-is-enum "GtkRevealerTransitionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRevealerTransitionType")
          (g:gtype (cffi:foreign-funcall "gtk_revealer_transition_type_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:revealer-transition-type
          (glib:symbol-for-gtype "GtkRevealerTransitionType")))
  ;; Check names
  (is (equal '("GTK_REVEALER_TRANSITION_TYPE_NONE"
               "GTK_REVEALER_TRANSITION_TYPE_CROSSFADE"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_RIGHT"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_LEFT"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_UP"
               "GTK_REVEALER_TRANSITION_TYPE_SLIDE_DOWN")
             (glib-test:list-enum-item-names "GtkRevealerTransitionType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GtkRevealerTransitionType")))
  ;; Check nick names
  (is (equal '("none" "crossfade" "slide-right" "slide-left" "slide-up"
               "slide-down")
             (glib-test:list-enum-item-nicks "GtkRevealerTransitionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkRevealerTransitionType"
                                    GTK:REVEALER-TRANSITION-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_revealer_transition_type_get_type")
                       (:NONE 0)
                       (:CROSSFADE 1)
                       (:SLIDE-RIGHT 2)
                       (:SLIDE-LEFT 3)
                       (:SLIDE-UP 4)
                       (:SLIDE-DOWN 5))
             (gobject:get-gtype-definition "GtkRevealerTransitionType"))))

;;;     GtkRevealer

(test gtk-revealer-class
  ;; Check type
  (is (g:type-is-object "GtkRevealer"))
  ;; Check registered name
  (is (eq 'gtk:revealer
          (glib:symbol-for-gtype "GtkRevealer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRevealer")
          (g:gtype (cffi:foreign-funcall "gtk_revealer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkRevealer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRevealer")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkRevealer")))
  ;; Check class properties
  (is (equal '("child-revealed" "reveal-child" "transition-duration"
               "transition-type")
             (glib-test:list-properties "GtkRevealer")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkRevealer")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkRevealer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkRevealer")))
  ;; CSS information
  (is (string= "revealer"
               (gtk:widget-class-css-name "GtkRevealer")))
  ;; Check  class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRevealer" GTK:REVEALER
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_revealer_get_type")
                       ((CHILD-REVEALED REVEALER-CHILD-REVEALED
                         "child-revealed" "gboolean" T NIL)
                        (REVEAL-CHILD REVEALER-REVEAL-CHILD
                         "reveal-child" "gboolean" T T)
                        (TRANSITION-DURATION REVEALER-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-TYPE REVEALER-TRANSITION-TYPE
                         "transition-type" "GtkRevealerTransitionType" T T)))
             (gobject:get-gtype-definition "GtkRevealer"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-revealer-properties
  (let ((revealer (make-instance 'gtk:revealer)))
    (is-false (gtk:revealer-child-revealed revealer))
    (is-false (gtk:revealer-reveal-child revealer))
    (is (= 250 (gtk:revealer-transition-duration revealer)))
    (is (eq :slide-down (gtk:revealer-transition-type revealer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_revealer_new

(test gtk-revealer-new
  (is (typep (gtk:revealer-new) 'gtk:revealer)))

;;; 2024-9-21
