(in-package :gtk-test)

(def-suite gtk-adjustment :in gtk-suite)
(in-suite gtk-adjustment)

;;; --- GtkAdjustment ----------------------------------------------------------

(test gtk-adjustment-class
  ;; Check type
  (is (g:type-is-object "GtkAdjustment"))
  ;; Check registered name
  (is (eq 'gtk:adjustment
          (glib:symbol-for-gtype "GtkAdjustment")))
  ;; Check parent
  (is (eq (g:gtype "GInitiallyUnowned") (g:type-parent "GtkAdjustment")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAdjustment")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkAdjustment")))
  ;; Check class properties
  (is (equal '("lower" "page-increment" "page-size" "step-increment" "upper"
               "value")
             (glib-test:list-properties "GtkAdjustment")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAdjustment" GTK:ADJUSTMENT
                       (:SUPERCLASS G:INITIALLY-UNOWNED
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_adjustment_get_type")
                       ((LOWER ADJUSTMENT-LOWER "lower" "gdouble" T T)
                        (PAGE-INCREMENT ADJUSTMENT-PAGE-INCREMENT
                         "page-increment" "gdouble" T T)
                        (PAGE-SIZE ADJUSTMENT-PAGE-SIZE
                         "page-size" "gdouble" T T)
                        (STEP-INCREMENT ADJUSTMENT-STEP-INCREMENT
                         "step-increment" "gdouble" T T)
                        (UPPER ADJUSTMENT-UPPER "upper" "gdouble" T T)
                        (VALUE ADJUSTMENT-VALUE "value" "gdouble" T T)))
             (gobject:get-gtype-definition "GtkAdjustment"))))

;;; --- gtk-adjustment-properties -----------------------------------------------

(test gtk-adjustment-properties
  (let ((adjustment (make-instance 'gtk:adjustment)))
    (is (=  0.0d0 (gtk:adjustment-lower adjustment)))
    (is (=  0.0d0 (gtk:adjustment-page-increment adjustment)))
    (is (=  0.0d0 (gtk:adjustment-page-size adjustment)))
    (is (=  0.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (=  0.0d0 (gtk:adjustment-upper adjustment)))
    (is (=  0.0d0 (gtk:adjustment-value adjustment)))

    (setf (gtk:adjustment-upper adjustment) 90.0d0)
    (setf (gtk:adjustment-value adjustment) 10.0d0)
    ;; value is clamped
    (is (= 10.0d0 (gtk:adjustment-value adjustment)))
    (setf (gtk:adjustment-value adjustment) 100.0d0)
    (is (= 90.0d0 (gtk:adjustment-value adjustment)))))

;;; --- gtk-adjustment-new -----------------------------------------------------

(test gtk-adjustment-new.1
  (let ((adjustment (gtk:adjustment-new 10.0d0      ; value
                                         0.0d0      ; lower
                                       100.0d0      ; upper
                                         5.0d0      ; step-increment
                                        10.0d0      ; page-increment
                                        10.0d0)))   ; page-size
    (is (=  10.0d0 (gtk:adjustment-value adjustment)))
    (is (=   0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 100.0d0 (gtk:adjustment-upper adjustment)))
    (is (=   5.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (=  10.0d0 (gtk:adjustment-page-increment adjustment)))
    (is (=  10.0d0 (gtk:adjustment-page-size adjustment)))))

(test gtk-adjustment-new.2
  (let ((adjustment (gtk:adjustment-new 10          ; value as integer
                                         0          ; lower as integer
                                       100.0        ; upper as float
                                         5.0        ; step-increment as float
                                        21/2        ; page-increment as rational
                                        21/2)))     ; page-size as rational
    (is (=  10.0d0 (gtk:adjustment-value adjustment)))
    (is (=   0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 100.0d0 (gtk:adjustment-upper adjustment)))
    (is (=   5.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (=  10.5d0 (gtk:adjustment-page-increment adjustment)))
    (is (=  10.5d0 (gtk:adjustment-page-size adjustment)))))

;;;     gtk_adjustment_clamp_page
;;;     gtk_adjustment_changed                           * deprecated
;;;     gtk_adjustment_value_changed                     * deprecated

;;;     gtk_adjustment_configure

(test gtk-adjustment-configure
  (let ((adjustment (make-instance 'gtk:adjustment)))

    (is-false (gtk:adjustment-configure adjustment 10    ; value
                                                    0    ; lower
                                                  100.0  ; upper
                                                    5.0  ; step-increment
                                                   21/2  ; page-increment
                                                 21/2))  ; page-size

    (is (=  10.0d0 (gtk:adjustment-value adjustment)))
    (is (=   0.0d0 (gtk:adjustment-lower adjustment)))
    (is (= 100.0d0 (gtk:adjustment-upper adjustment)))
    (is (=   5.0d0 (gtk:adjustment-step-increment adjustment)))
    (is (=  10.5d0 (gtk:adjustment-page-increment adjustment)))
    (is (=  10.5d0 (gtk:adjustment-page-size adjustment)))))

;;;     gtk_adjustment_get_minimum_increment

(test gtk-adjustment-minimum-increment.1
  (let ((adjustment (gtk:adjustment-new 10.0d0      ; value
                                         0.0d0      ; lower
                                       100.0d0      ; upper
                                         5.0d0      ; step-increment
                                        10.0d0      ; page-increment
                                        10.0d0)))   ; page-size
    (is (= 5.0d0 (gtk:adjustment-minimum-increment adjustment)))))

(test gtk-adjustment-minimum-increment.2
  (let ((adjustment (gtk:adjustment-new 10.0d0      ; value
                                         0.0d0      ; lower
                                       100.0d0      ; upper
                                         5.0d0      ; step-increment
                                         1.0d0      ; page-increment
                                         1.0d0)))   ; page-size
    (is (= 1.0d0 (gtk:adjustment-minimum-increment adjustment)))))

;;; 2024-9-23
