(in-package :gtk-test)

(def-suite gtk-level-bar :in gtk-suite)
(in-suite gtk-level-bar)

(defvar *verbose-gtk-level-bar* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLevelBarMode

(test gtk-level-bar-mode
  ;; Check type
  (is (g:type-is-enum "GtkLevelBarMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLevelBarMode")
          (g:gtype (cffi:foreign-funcall "gtk_level_bar_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:level-bar-mode
          (glib:symbol-for-gtype "GtkLevelBarMode")))
  ;; Check names
  (is (equal '("GTK_LEVEL_BAR_MODE_CONTINUOUS" "GTK_LEVEL_BAR_MODE_DISCRETE")
             (glib-test:list-enum-item-names "GtkLevelBarMode")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkLevelBarMode")))
  ;; Check nick names
  (is (equal '("continuous" "discrete")
             (glib-test:list-enum-item-nicks "GtkLevelBarMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkLevelBarMode" GTK:LEVEL-BAR-MODE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_level_bar_mode_get_type")
                                    (:CONTINUOUS 0)
                                    (:DISCRETE 1))
             (gobject:get-gtype-definition "GtkLevelBarMode"))))

;;;     GtkLevelBar

(test gtk-level-bar-class
  ;; Check type
  (is (g:type-is-object "GtkLevelBar"))
  ;; Check registered name
  (is (eq 'gtk:level-bar
          (glib:symbol-for-gtype "GtkLevelBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLevelBar")
          (g:gtype (cffi:foreign-funcall "gtk_level_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
      (g:type-parent "GtkLevelBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkLevelBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkLevelBar")))
  ;; Check class properties
  (is (equal '("inverted" "max-value" "min-value" "mode" "orientation" "value")
             (glib-test:list-properties "GtkLevelBar")))
  ;; Check style properties
  (is (equal '("min-block-height" "min-block-width")
             (gtk-test:list-style-properties "GtkLevelBar")))
  ;; Check signals
  (is (equal '("offset-changed")
             (glib-test:list-signals "GtkLevelBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLevelBar" GTK:LEVEL-BAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_level_bar_get_type")
                      ((INVERTED LEVEL-BAR-INVERTED "inverted" "gboolean" T T)
                       (MAX-VALUE LEVEL-BAR-MAX-VALUE "max-value" "gdouble" T T)
                       (MIN-VALUE LEVEL-BAR-MIN-VALUE "min-value" "gdouble" T T)
                       (MODE LEVEL-BAR-MODE "mode" "GtkLevelBarMode" T T)
                       (VALUE LEVEL-BAR-VALUE "value" "gdouble" T T)))
             (gobject:get-gtype-definition "GtkLevelBar"))))

;;; --- Signals ----------------------------------------------------------------

;;;     offset-changed

;;; --- Properties -------------------------------------------------------------

(test gtk-level-bar-properties
  (glib-test:with-check-memory (levelbar)
    (setf levelbar (make-instance 'gtk:level-bar))
    ;; inverted
    (is-false (gtk:level-bar-inverted levelbar))
    (is-true (setf (gtk:level-bar-inverted levelbar) t))
    (is-true (gtk:level-bar-inverted levelbar))
    ;; max-value
    (is (= 1.0d0 (gtk:level-bar-max-value levelbar)))
    (is (= 5.0d0 (setf (gtk:level-bar-max-value levelbar) 5.0)))
    (is (= 5.0d0 (gtk:level-bar-max-value levelbar)))
    ;; min-value
    (is (= 0.0d0 (gtk:level-bar-min-value levelbar)))
    (is (= 1.0d0 (setf (gtk:level-bar-min-value levelbar) 1.0)))
    (is (= 1.0d0 (gtk:level-bar-min-value levelbar)))
    ;; mode
    (is (eq :continuous (gtk:level-bar-mode levelbar)))
    (is (eq :discrete (setf (gtk:level-bar-mode levelbar) :discrete)))
    (is (eq :discrete (gtk:level-bar-mode levelbar)))
    ;; value (not 0,0d0 because the min-value is 1.0d0)
    (is (= 1.0d0 (gtk:level-bar-value levelbar)))
    (is (= 2.0d0 (setf (gtk:level-bar-value levelbar) 2.0)))
    (is (= 2.0d0 (gtk:level-bar-value levelbar)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-level-bar-style-properties
  (glib-test:with-check-memory (levelbar)
    (setf levelbar (make-instance 'gtk:level-bar))
    (is (= 3 (gtk:widget-style-property levelbar "min-block-height")))
    (is (= 3 (gtk:widget-style-property levelbar "min-block-width")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_level_bar_new

(test gtk-level-bar-new
  (glib-test:with-check-memory (levelbar)
    (is (typep (setf levelbar (gtk:level-bar-new)) 'gtk:level-bar))))

;;;     gtk_level_bar_new_for_interval

(test gtk-level-bar-new-for-interval
  (glib-test:with-check-memory (levelbar)
    (setf levelbar (gtk:level-bar-new-for-interval 1.0 2.0))
    (is (= 1.0d0 (gtk:level-bar-min-value levelbar)))
    (is (= 2.0d0 (gtk:level-bar-max-value levelbar)))))

;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value

(test gtk-level-bar-add-offset-value
  (glib-test:with-check-memory (levelbar)
    (setf levelbar (gtk:level-bar-new-for-interval 0.0 10.0))
    (is-false (gtk:level-bar-add-offset-value levelbar "half" 0.5))
    (is (= 0.5d0 (gtk:level-bar-offset-value levelbar "half")))
    (is-false (gtk:level-bar-remove-offset-value levelbar "half"))
    (is-false (gtk:level-bar-offset-value levelbar "half"))))

;;; 2026-06-20
