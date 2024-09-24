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

;;; --- Properties -------------------------------------------------------------

(test gtk-level-bar-properties
  (let ((level-bar (make-instance 'gtk:level-bar)))
    ;; inverted
    (is-false (gtk:level-bar-inverted level-bar))
    (is-true (setf (gtk:level-bar-inverted level-bar) t))
    (is-true (gtk:level-bar-inverted level-bar))
    ;; max-value
    (is (= 1.0d0 (gtk:level-bar-max-value level-bar)))
    (is (= 5.0d0 (setf (gtk:level-bar-max-value level-bar) 5.0)))
    (is (= 5.0d0 (gtk:level-bar-max-value level-bar)))
    ;; min-value
    (is (= 0.0d0 (gtk:level-bar-min-value level-bar)))
    (is (= 1.0d0 (setf (gtk:level-bar-min-value level-bar) 1.0)))
    (is (= 1.0d0 (gtk:level-bar-min-value level-bar)))
    ;; mode
    (is (eq :continuous (gtk:level-bar-mode level-bar)))
    (is (eq :discrete (setf (gtk:level-bar-mode level-bar) :discrete)))
    (is (eq :discrete (gtk:level-bar-mode level-bar)))
    ;; value (not 0,0d0 because the min-value is 1.0d0)
    (is (= 1.0d0 (gtk:level-bar-value level-bar)))
    (is (= 2.0d0 (setf (gtk:level-bar-value level-bar) 2.0)))
    (is (= 2.0d0 (gtk:level-bar-value level-bar)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-level-bar-style-properties
  (let ((level-bar (make-instance 'gtk:level-bar)))
    (is (= 3 (gtk:widget-style-property level-bar "min-block-height")))
    (is (= 3 (gtk:widget-style-property level-bar "min-block-width")))))

;;; --- Signals ----------------------------------------------------------------

#+nil
(test gtk-level-bar-offset-changed-signal
  (let* ((message nil)
         (level-bar (gtk:level-bar-new-for-interval 0.0 10.0))
         ;; Connect a signal handler
         (handler-id (g-signal-connect level-bar "offset-changed"
                       (lambda (widget name)
                         (declare (ignore widget))
                         (when *verbose-gtk-level-bar*
                           (format t "~&Signal 'offset-changed' for level bar.~%"))
                         (is (string= "high" name))
                         (is (= 0.75 (gtk:level-bar-offset-value level-bar name)))
                         (setf message "Signal 'offset-changed' for level bar")
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (is-false (g-signal-emit level-bar "offset-changed" "high"))
    (is (string= "Signal 'offset-changed' for level bar" message))
    (is-false (g-signal-handler-disconnect level-bar handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_level_bar_new

(test gtk-level-bar-new
  (is (eq 'gtk:level-bar (type-of (gtk:level-bar-new)))))

;;;     gtk_level_bar_new_for_interval

(test gtk-level-bar-new-for-interval
  (let ((level-bar (gtk:level-bar-new-for-interval 1.0 2.0)))
    (is (= 1.0d0 (gtk:level-bar-min-value level-bar)))
    (is (= 2.0d0 (gtk:level-bar-max-value level-bar)))))

;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value

(test gtk-level-bar-add-offset-value
  (let ((level-bar (gtk:level-bar-new-for-interval 0.0 10.0)))
    (is-false (gtk:level-bar-add-offset-value level-bar "half" 0.5))
    (is (= 0.5d0 (gtk:level-bar-offset-value level-bar "half")))
    (is-false (gtk:level-bar-remove-offset-value level-bar "half"))
    (is-false (gtk:level-bar-offset-value level-bar "half"))))

;;; 2024-9-22
