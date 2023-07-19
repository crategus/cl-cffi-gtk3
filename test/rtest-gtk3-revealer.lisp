(in-package :gtk-test)

(def-suite gtk-revealer :in gtk-suite)
(in-suite gtk-revealer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRevealerClass
;;;     GtkRevealerTransitionType

;;;     GtkRevealer

(test revealer-class
  ;; Type check
  (is (g:type-is-object "GtkRevealer"))
  ;; Check the registered name
  (is (eq 'gtk:revealer
          (glib:symbol-for-gtype "GtkRevealer")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRevealer")
          (g:gtype (cffi:foreign-funcall "gtk_revealer_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkRevealer")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkRevealer")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkRevealer")))
  ;; Check the class properties
  (is (equal '("child-revealed" "reveal-child" "transition-duration"
               "transition-type")
             (list-properties "GtkRevealer")))
  ;; Get the names of the style properties
  (is (equal '()
             (list-style-properties "GtkRevealer")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkRevealer")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkRevealer")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkRevealer" GTK-REVEALER
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_revealer_get_type")
                       ((CHILD-REVEALED GTK-REVEALER-CHILD-REVEALED
                         "child-revealed" "gboolean" T NIL)
                        (REVEAL-CHILD GTK-REVEALER-REVEAL-CHILD "reveal-child"
                         "gboolean" T T)
                        (TRANSITION-DURATION GTK-REVEALER-TRANSITION-DURATION
                         "transition-duration" "guint" T T)
                        (TRANSITION-TYPE GTK-REVEALER-TRANSITION-TYPE
                         "transition-type" "GtkRevealerTransitionType" T T)))
             (gobject:get-g-type-definition "GtkRevealer"))))

;;; --- Properties -------------------------------------------------------------

;;;     child-revealed
;;;     reveal-child
;;;     transition-duration
;;;     transition-type

;;; --- Functions --------------------------------------------------------------

;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child                      Accessor
;;;     gtk_revealer_set_reveal_child                      Accessor
;;;     gtk_revealer_get_child_revealed                    Accessor
;;;     gtk_revealer_get_transition_duration               Accessor
;;;     gtk_revealer_set_transition_duration               Accessor
;;;     gtk_revealer_get_transition_type                   Accessor
;;;     gtk_revealer_set_transition_type                   Accessor

;;; --- 2023-5-29 --------------------------------------------------------------
