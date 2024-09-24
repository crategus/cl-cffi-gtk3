(in-package :gtk-test)

(def-suite gtk-scrollbar :in gtk-suite)
(in-suite gtk-scrollbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollbar

(test gtk-scrollbar-class
  ;; Check type
  (is (g:type-is-object "GtkScrollbar"))
  ;; Check registered name
  (is (eq 'gtk:scrollbar
          (glib:symbol-for-gtype "GtkScrollbar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollbar")
          (g:gtype (cffi:foreign-funcall "gtk_scrollbar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkRange")
          (g:type-parent "GtkScrollbar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkScrollbar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkScrollbar")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkScrollbar")))
  ;; Check style properties
  (is (equal '("fixed-slider-length" "has-backward-stepper"
               "has-forward-stepper" "has-secondary-backward-stepper"
               "has-secondary-forward-stepper" "min-slider-length")
             (gtk-test:list-style-properties "GtkScrollbar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkScrollbar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScrollbar" GTK:SCROLLBAR
                       (:SUPERCLASS GTK:RANGE
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkScrollbar"))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-scrollbar-properties
  (let ((window (make-instance 'gtk:scrollbar)))
    (is-false (gtk:widget-style-property window "fixed-slider-length"))
    (is-false (gtk:widget-style-property window "has-backward-stepper"))
    (is-false (gtk:widget-style-property window "has-forward-stepper"))
    (is-false (gtk:widget-style-property window "has-secondary-backward-stepper"))
    (is-false (gtk:widget-style-property window "has-secondary-forward-stepper"))
    (is (= 21 (gtk:widget-style-property window "min-slider-length")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollbar_new

(test gtk-scrollbar-new
  (is (eq 'gtk:scrollbar (type-of (gtk:scrollbar-new :horizontal))))
  (is (eq 'gtk:scrollbar
          (type-of (gtk:scrollbar-new :vertical
                                      (make-instance 'gtk:adjustment))))))

;;; 2024-9-21
