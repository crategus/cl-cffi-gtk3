(in-package :gtk-test)

(def-suite gtk-scrollbar :in gtk-suite)
(in-suite gtk-scrollbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollbar

(test scrollbar-class
  ;; Type check
  (is (g:type-is-object "GtkScrollbar"))
  ;; Check the registered name
  (is (eq 'gtk:scrollbar
          (glib:symbol-for-gtype "GtkScrollbar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrollbar")
          (g:gtype (cffi:foreign-funcall "gtk_scrollbar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkRange")
          (g:type-parent "GtkScrollbar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkScrollbar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkScrollbar")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkScrollbar")))
  ;; Check the style properties.
  (is (equal '("fixed-slider-length" "has-backward-stepper"
               "has-forward-stepper" "has-secondary-backward-stepper"
               "has-secondary-forward-stepper" "min-slider-length")
             (list-style-properties "GtkScrollbar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkScrollbar")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkScrollbar" GTK-SCROLLBAR
                       (:SUPERCLASS GTK-RANGE :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkScrollbar"))))

;;; --- Style Properties -------------------------------------------------------

(test scrollbar-properties
  (let ((window (make-instance 'gtk:scrollbar)))
    (is-false (gtk:widget-style-property window "fixed-slider-length"))
    (is-false (gtk:widget-style-property window "has-backward-stepper"))
    (is-false (gtk:widget-style-property window "has-forward-stepper"))
    (is-false (gtk:widget-style-property window "has-secondary-backward-stepper"))
    (is-false (gtk:widget-style-property window "has-secondary-forward-stepper"))
    (is (= 21 (gtk:widget-style-property window "min-slider-length")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollbar_new

(test scrollbar-new
  (is (eq 'gtk:scrollbar (type-of (gtk:scrollbar-new :horizontal))))
  (is (eq 'gtk:scrollbar
          (type-of (gtk:scrollbar-new :vertical
                                      (make-instance 'gtk:adjustment))))))

;;; --- 2023-5-29 --------------------------------------------------------------
