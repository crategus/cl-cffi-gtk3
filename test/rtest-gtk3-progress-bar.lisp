(in-package :gtk-test)

(def-suite gtk-progress-bar :in gtk-suite)
(in-suite gtk-progress-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkProgressBar

(test progress-bar-class
  ;; Type check
  (is (g:type-is-object "GtkProgressBar"))
  ;; Check the registered name
  (is (eq 'gtk:progress-bar
          (glib:symbol-for-gtype "GtkProgressBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkProgressBar")
          (g:gtype (cffi:foreign-funcall "gtk_progress_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkProgressBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkProgressBar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkProgressBar")))
  ;; Check the class properties
  (is (equal '("ellipsize" "fraction" "inverted" "orientation" "pulse-step"
               "show-text" "text")
             (list-properties "GtkProgressBar")))
  ;; Check the style properties.
  (is (equal '("min-horizontal-bar-height" "min-horizontal-bar-width"
               "min-vertical-bar-height" "min-vertical-bar-width" "xspacing"
               "yspacing")
             (list-style-properties "GtkProgressBar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkProgressBar")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkProgressBar" GTK-PROGRESS-BAR
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                       ((ELLIPSIZE GTK-PROGRESS-BAR-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (FRACTION GTK-PROGRESS-BAR-FRACTION "fraction"
                         "gdouble" T T)
                        (INVERTED GTK-PROGRESS-BAR-INVERTED "inverted"
                         "gboolean" T T)
                        (PULSE-STEP GTK-PROGRESS-BAR-PULSE-STEP "pulse-step"
                         "gdouble" T T)
                        (SHOW-TEXT GTK-PROGRESS-BAR-SHOW-TEXT "show-text"
                         "gboolean" T T)
                        (TEXT GTK-PROGRESS-BAR-TEXT "text" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkProgressBar"))))

;;; --- Properties -------------------------------------------------------------

(test progress-bar-propertiers
  (let ((progress-bar (make-instance 'gtk:progress-bar)))
    ;; ellipsize
    (is (eq :none (gtk:progress-bar-ellipsize progress-bar)))
    (is (eq :start (setf (gtk:progress-bar-ellipsize progress-bar) :start)))
    (is (eq :start (gtk:progress-bar-ellipsize progress-bar)))
    ;; fraction
    (is (= 0.0d0 (gtk:progress-bar-fraction progress-bar)))
    (is (= 0.5d0 (setf (gtk:progress-bar-fraction progress-bar) 0.5)))
    (is (= 0.5d0 (gtk:progress-bar-fraction progress-bar)))
    ;; inverted
    (is-false (gtk:progress-bar-inverted progress-bar))
    (is-true (setf (gtk:progress-bar-inverted progress-bar) t))
    (is-true (gtk:progress-bar-inverted progress-bar))
    ;; pulse-step
    (is (= 0.1d0 (gtk:progress-bar-pulse-step progress-bar)))
    (is (= 0.3d0 (setf (gtk:progress-bar-pulse-step progress-bar) 0.3d0)))
    (is (= 0.3d0 (gtk:progress-bar-pulse-step progress-bar)))
    ;; show-text
    (is-false (gtk:progress-bar-show-text progress-bar))
    (is-true (setf (gtk:progress-bar-show-text progress-bar) t))
    (is-true (gtk:progress-bar-show-text progress-bar))
    ;; text
    (is-false (gtk:progress-bar-text progress-bar))
    (is (string= "text" (setf (gtk:progress-bar-text progress-bar) "text")))
    (is (string= "text" (gtk:progress-bar-text progress-bar)))))

;;; --- Style Properties -------------------------------------------------------

(test progress-bar-style-properties
  (let ((bar (make-instance 'gtk:progress-bar)))
    (is (=   6 (gtk:widget-style-property bar "min-horizontal-bar-height")))
    (is (= 150 (gtk:widget-style-property bar "min-horizontal-bar-width")))
    (is (=  80 (gtk:widget-style-property bar "min-vertical-bar-height")))
    (is (=   7 (gtk:widget-style-property bar "min-vertical-bar-width")))
    (is (=   2 (gtk:widget-style-property bar "xspacing")))
    (is (=   2 (gtk:widget-style-property bar "yspacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_progress_bar_new

(test progress-bar-new
  (is (typep (gtk:progress-bar-new) 'gtk:progress-bar)))

;;;     gtk_progress_bar_pulse

;;; --- 2023-5-29 --------------------------------------------------------------
