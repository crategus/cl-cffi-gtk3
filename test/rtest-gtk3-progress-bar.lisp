(in-package :gtk-test)

(def-suite gtk-progress-bar :in gtk-suite)
(in-suite gtk-progress-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkProgressBar

(test gtk-progress-bar-class
  ;; Check type
  (is (g:type-is-object "GtkProgressBar"))
  ;; Check registered name
  (is (eq 'gtk:progress-bar
          (glib:symbol-for-gtype "GtkProgressBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkProgressBar")
          (g:gtype (cffi:foreign-funcall "gtk_progress_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkProgressBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkProgressBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkProgressBar")))
  ;; Check class properties
  (is (equal '("ellipsize" "fraction" "inverted" "orientation" "pulse-step"
               "show-text" "text")
             (glib-test:list-properties "GtkProgressBar")))
  ;; Check style properties
  (is (equal '("min-horizontal-bar-height" "min-horizontal-bar-width"
               "min-vertical-bar-height" "min-vertical-bar-width" "xspacing"
               "yspacing")
             (gtk-test:list-style-properties "GtkProgressBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkProgressBar")))
  ;; CSS information
  (is (string= "progressbar"
               (gtk:widget-class-css-name "GtkProgressBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkProgressBar" GTK:PROGRESS-BAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                      ((ELLIPSIZE PROGRESS-BAR-ELLIPSIZE
                        "ellipsize" "PangoEllipsizeMode" T T)
                       (FRACTION PROGRESS-BAR-FRACTION
                        "fraction" "gdouble" T T)
                       (INVERTED PROGRESS-BAR-INVERTED
                        "inverted" "gboolean" T T)
                       (PULSE-STEP PROGRESS-BAR-PULSE-STEP
                        "pulse-step" "gdouble" T T)
                       (SHOW-TEXT PROGRESS-BAR-SHOW-TEXT
                        "show-text" "gboolean" T T)
                       (TEXT PROGRESS-BAR-TEXT "text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkProgressBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-progress-bar-properties
  (glib-test:with-check-memory (pbar)
    (setf pbar (make-instance 'gtk:progress-bar))
    ;; ellipsize
    (is (eq :none (gtk:progress-bar-ellipsize pbar)))
    (is (eq :start (setf (gtk:progress-bar-ellipsize pbar) :start)))
    (is (eq :start (gtk:progress-bar-ellipsize pbar)))
    ;; fraction
    (is (= 0.0d0 (gtk:progress-bar-fraction pbar)))
    (is (= 0.5d0 (setf (gtk:progress-bar-fraction pbar) 0.5)))
    (is (= 0.5d0 (gtk:progress-bar-fraction pbar)))
    ;; inverted
    (is-false (gtk:progress-bar-inverted pbar))
    (is-true (setf (gtk:progress-bar-inverted pbar) t))
    (is-true (gtk:progress-bar-inverted pbar))
    ;; pulse-step
    (is (= 0.1d0 (gtk:progress-bar-pulse-step pbar)))
    (is (= 0.3d0 (setf (gtk:progress-bar-pulse-step pbar) 0.3d0)))
    (is (= 0.3d0 (gtk:progress-bar-pulse-step pbar)))
    ;; show-text
    (is-false (gtk:progress-bar-show-text pbar))
    (is-true (setf (gtk:progress-bar-show-text pbar) t))
    (is-true (gtk:progress-bar-show-text pbar))
    ;; text
    (is-false (gtk:progress-bar-text pbar))
    (is (string= "text" (setf (gtk:progress-bar-text pbar) "text")))
    (is (string= "text" (gtk:progress-bar-text pbar)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-progress-bar-style-properties
  (glib-test:with-check-memory (pbar)
    (setf pbar (make-instance 'gtk:progress-bar))
    (is (=   6 (gtk:widget-style-property pbar "min-horizontal-bar-height")))
    (is (= 150 (gtk:widget-style-property pbar "min-horizontal-bar-width")))
    (is (=  80 (gtk:widget-style-property pbar "min-vertical-bar-height")))
    (is (=   7 (gtk:widget-style-property pbar "min-vertical-bar-width")))
    (is (=   2 (gtk:widget-style-property pbar "xspacing")))
    (is (=   2 (gtk:widget-style-property pbar "yspacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_progress_bar_new

(test gtk-progress-bar-new
  (glib-test:with-check-memory (pbar)
    (is (typep (setf pbar (gtk:progress-bar-new)) 'gtk:progress-bar))))

;;;     gtk_progress_bar_pulse

;;; 2026-06-20
