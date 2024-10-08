(in-package :gtk-test)

(def-suite gtk-range :in gtk-suite)
(in-suite gtk-range)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSensitivityType

(test gtk-sensitivity-type
  ;; Check type
  (is (g:type-is-enum "GtkSensitivityType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSensitivityType")
          (g:gtype (cffi:foreign-funcall "gtk_sensitivity_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:sensitivity-type
          (glib:symbol-for-gtype "GtkSensitivityType")))
  ;; Check names
  (is (equal '("GTK_SENSITIVITY_AUTO" "GTK_SENSITIVITY_ON"
               "GTK_SENSITIVITY_OFF")
             (glib-test:list-enum-item-names "GtkSensitivityType")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkSensitivityType")))
  ;; Check nick names
  (is (equal '("auto" "on" "off")
             (glib-test:list-enum-item-nicks "GtkSensitivityType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSensitivityType" GTK:SENSITIVITY-TYPE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_sensitivity_type_get_type")
                       (:AUTO 0)
                       (:ON 1)
                       (:OFF 2))
             (gobject:get-gtype-definition "GtkSensitivityType"))))

;;;     GtkRange

(test gtk-range-class
  ;; Check type
  (is (g:type-is-object "GtkRange"))
  ;; Check registered name
  (is (eq 'gtk:range
          (glib:symbol-for-gtype "GtkRange")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRange")
          (g:gtype (cffi:foreign-funcall "gtk_range_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkRange")))
  ;; Check children
  (is (equal '("GtkScale" "GtkScrollbar")
             (glib-test:list-children "GtkRange")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkRange")))
  ;; Check class properties
  (is (equal '("adjustment" "fill-level" "inverted" "lower-stepper-sensitivity"
               "orientation" "restrict-to-fill-level" "round-digits"
               "show-fill-level" "upper-stepper-sensitivity")
             (glib-test:list-properties "GtkRange")))
  ;; Check style properties
  (is (equal '("arrow-displacement-x" "arrow-displacement-y" "arrow-scaling"
               "slider-width" "stepper-size" "stepper-spacing" "trough-border"
               "trough-under-steppers")
             (gtk-test:list-style-properties "GtkRange")))
  ;; Check signals
  (is (equal '("adjust-bounds" "change-value" "move-slider" "value-changed")
             (glib-test:list-signals "GtkRange")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRange" GTK:RANGE
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_range_get_type")
                       ((ADJUSTMENT RANGE-ADJUSTMENT
                         "adjustment" "GtkAdjustment" T T)
                        (FILL-LEVEL RANGE-FILL-LEVEL
                         "fill-level" "gdouble" T T)
                        (INVERTED RANGE-INVERTED "inverted" "gboolean" T T)
                        (LOWER-STEPPER-SENSITIVITY
                         RANGE-LOWER-STEPPER-SENSITIVITY
                         "lower-stepper-sensitivity" "GtkSensitivityType" T T)
                        (RESTRICT-TO-FILL-LEVEL RANGE-RESTRICT-TO-FILL-LEVEL
                         "restrict-to-fill-level" "gboolean" T T)
                        (ROUND-DIGITS RANGE-ROUND-DIGITS
                         "round-digits" "gint" T T)
                        (SHOW-FILL-LEVEL RANGE-SHOW-FILL-LEVEL
                         "show-fill-level" "gboolean" T T)
                        (UPPER-STEPPER-SENSITIVITY
                         RANGE-UPPER-STEPPER-SENSITIVITY
                         "upper-stepper-sensitivity" "GtkSensitivityType" T T)))
             (gobject:get-gtype-definition "GtkRange"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     fill-level
;;;     inverted
;;;     lower-stepper-sensitivity
;;;     restrict-to-fill-level
;;;     round-digits
;;;     show-fill-level
;;;     upper-stepper-sensitivity

(test gtk-range-properties
  (let ((range (gtk:scale-new-with-range :horizontal 0.0 1.0 0.1)))
    (is (typep (gtk:range-adjustment range) 'gtk:adjustment))
    (is (approx-equal 1.7976931348623157d308 (gtk:range-fill-level range)))
    (is-false (gtk:range-inverted range))
    (is (eq :auto (gtk:range-lower-stepper-sensitivity range)))
    (is-true (gtk:range-restrict-to-fill-level range))
    (is (= 1 (gtk:range-round-digits range)))
    (is-false (gtk:range-show-fill-level range))
    (is (eq :auto (gtk:range-upper-stepper-sensitivity range)))))

;;; --- Style Properties -------------------------------------------------------

;;;     arrow-displacement-x
;;;     arrow-displacement-y
;;;     arrow-scaling
;;;     slider-width
;;;     stepper-size
;;;     stepper-spacing
;;;     trough-border
;;;     trough-under-steppers

(test gtk-range-style-properties
  (let ((range (gtk:scale-new-with-range :horizontal 0.0 1.0 0.1)))
    (is (= 0 (gtk:widget-style-property range "arrow-displacement-x")))
    (is (= 0 (gtk:widget-style-property range "arrow-displacement-y")))
    (is (= 0.5 (gtk:widget-style-property range "arrow-scaling")))
    (is (= 14 (gtk:widget-style-property range "slider-width")))
    (is (= 14 (gtk:widget-style-property range "stepper-size")))
    (is (= 0 (gtk:widget-style-property range "stepper-spacing")))
    (is (= 1 (gtk:widget-style-property range "trough-border")))
    (is-true (gtk:widget-style-property range "trough-under-steppers"))))

;;; --- Signals ----------------------------------------------------------------

;;;     adjust-bounds

(test gtk-range-adjust-bounds-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "adjust-bounds" "GtkRange"))))
    (is (string= "adjust-bounds" (g:signal-query-signal-name query)))
    (is (string= "GtkRange" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gdouble")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     change-value

(test gtk-range-change-value-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "change-value" "GtkRange"))))
    (is (string= "change-value" (g:signal-query-signal-name query)))
    (is (string= "GtkRange" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkScrollType" "gdouble")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     move-slider

(test gtk-range-move-slider-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "move-slider" "GtkRange"))))
    (is (string= "move-slider" (g:signal-query-signal-name query)))
    (is (string= "GtkRange" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkScrollType")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     value-changed

(test gtk-range-value-changed-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "value-changed" "GtkRange"))))
    (is (string= "value-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkRange" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_range_get_fill_level
;;;     gtk_range_get_restrict_to_fill_level
;;;     gtk_range_get_show_fill_level
;;;     gtk_range_set_fill_level
;;;     gtk_range_set_restrict_to_fill_level
;;;     gtk_range_set_show_fill_level
;;;     gtk_range_get_adjustment
;;;     gtk_range_set_adjustment
;;;     gtk_range_get_inverted
;;;     gtk_range_set_inverted
;;;     gtk_range_get_value
;;;     gtk_range_set_value
;;;     gtk_range_set_increments
;;;     gtk_range_set_range
;;;     gtk_range_get_round_digits
;;;     gtk_range_set_round_digits
;;;     gtk_range_set_lower_stepper_sensitivity
;;;     gtk_range_get_lower_stepper_sensitivity
;;;     gtk_range_set_upper_stepper_sensitivity
;;;     gtk_range_get_upper_stepper_sensitivity
;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable
;;;     gtk_range_get_min_slider_size
;;;     gtk_range_get_range_rect
;;;     gtk_range_get_slider_range
;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_min_slider_size
;;;     gtk_range_set_slider_size_fixed

;;; 2024-9-21
