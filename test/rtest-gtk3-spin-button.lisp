(in-package :gtk-test)

(def-suite gtk-spin-button :in gtk-suite)
(in-suite gtk-spin-button)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkSpinButtonUpdatePolicy

(test gtk-spin-button-update-policy
  ;; Check type
  (is (g:type-is-enum "GtkSpinButtonUpdatePolicy"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSpinButtonUpdatePolicy")
          (g:gtype (cffi:foreign-funcall "gtk_spin_button_update_policy_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:spin-button-update-policy
          (glib:symbol-for-gtype "GtkSpinButtonUpdatePolicy")))
  ;; Check names
  (is (equal '("GTK_UPDATE_ALWAYS" "GTK_UPDATE_IF_VALID")
             (glib-test:list-enum-item-names "GtkSpinButtonUpdatePolicy")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkSpinButtonUpdatePolicy")))
  ;; Check nick names
  (is (equal '("always" "if-valid")
             (glib-test:list-enum-item-nicks "GtkSpinButtonUpdatePolicy")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSpinButtonUpdatePolicy"
                                    GTK:SPIN-BUTTON-UPDATE-POLICY
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_spin_button_update_policy_get_type")
                                    (:ALWAYS 0)
                                    (:IF-VALID 1))
             (gobject:get-gtype-definition "GtkSpinButtonUpdatePolicy"))))

;;;     GtkSpinType

(test gtk-spin-type
  ;; Check type
  (is (g:type-is-enum "GtkSpinType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSpinType")
          (g:gtype (cffi:foreign-funcall "gtk_spin_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:spin-type
          (glib:symbol-for-gtype "GtkSpinType")))
  ;; Check names
  (is (equal '("GTK_SPIN_STEP_FORWARD" "GTK_SPIN_STEP_BACKWARD"
               "GTK_SPIN_PAGE_FORWARD" "GTK_SPIN_PAGE_BACKWARD" "GTK_SPIN_HOME"
               "GTK_SPIN_END" "GTK_SPIN_USER_DEFINED")
             (glib-test:list-enum-item-names "GtkSpinType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GtkSpinType")))
  ;; Check nick names
  (is (equal '("step-forward" "step-backward" "page-forward" "page-backward"
               "home" "end" "user-defined")
             (glib-test:list-enum-item-nicks "GtkSpinType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkSpinType" GTK:SPIN-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_spin_type_get_type")
                                    (:STEP-FORWARD 0)
                                    (:STEP-BACKWARD 1)
                                    (:PAGE-FORWARD 2)
                                    (:PAGE-BACKWARD 3)
                                    (:HOME 4)
                                    (:END 5)
                                    (:USER-DEFINED 6))
             (gobject:get-gtype-definition "GtkSpinType"))))

;;;     GtkSpinButton

(test gtk-spin-button-class
  ;; Check type
  (is (g:type-is-object "GtkSpinButton"))
  ;; Check registered name
  (is (eq 'gtk:spin-button
          (glib:symbol-for-gtype "GtkSpinButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSpinButton")
          (g:gtype (cffi:foreign-funcall "gtk_spin_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEntry")
          (g:type-parent "GtkSpinButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSpinButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkEditable"
               "GtkCellEditable" "GtkOrientable")
             (glib-test:list-interfaces "GtkSpinButton")))
  ;; Check class properties
  (is (equal '("adjustment" "climb-rate" "digits" "numeric" "orientation"
               "snap-to-ticks" "update-policy" "value" "wrap")
             (glib-test:list-properties "GtkSpinButton")))
  ;; Check style properties
  (is (equal '("shadow-type")
             (gtk-test:list-style-properties "GtkSpinButton")))
  ;; Check signals
  (is (equal '("change-value" "input" "output" "value-changed" "wrapped")
             (glib-test:list-signals "GtkSpinButton")))
  ;; CSS information
  (is (string= "spinbutton"
               (gtk:widget-class-css-name "GtkSpinButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSpinButton" GTK:SPIN-BUTTON
                      (:SUPERCLASS GTK:ENTRY
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkCellEditable" "GtkEditable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_spin_button_get_type")
                      ((ADJUSTMENT SPIN-BUTTON-ADJUSTMENT "adjustment"
                        "GtkAdjustment" T T)
                       (CLIMB-RATE SPIN-BUTTON-CLIMB-RATE "climb-rate"
                        "gdouble" T T)
                       (DIGITS SPIN-BUTTON-DIGITS "digits" "guint" T T)
                       (NUMERIC SPIN-BUTTON-NUMERIC "numeric" "gboolean" T T)
                       (SNAP-TO-TICKS SPIN-BUTTON-SNAP-TO-TICKS
                        "snap-to-ticks" "gboolean" T T)
                       (UPDATE-POLICY SPIN-BUTTON-UPDATE-POLICY
                        "update-policy" "GtkSpinButtonUpdatePolicy" T T)
                       (VALUE SPIN-BUTTON-VALUE "value" "gdouble" T T)
                       (WRAP SPIN-BUTTON-WRAP "wrap" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSpinButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-value
;;;     input
;;;     output
;;;     value-changed
;;;     wrapped

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     climb-rate
;;;     digits
;;;     numeric
;;;     snap-to-ticks
;;;     update-policy
;;;     value
;;;     wrap

(test gtk-spin-button-properties
  (glib-test:with-check-memory (button :strong 1)
    (is (typep (setf button (make-instance 'gtk:spin-button)) 'gtk:spin-button))
    (is (typep (gtk:spin-button-adjustment button) 'gtk:adjustment))
    (is (= 0.0d0 (gtk:spin-button-climb-rate button)))
    (is (= 0 (gtk:spin-button-digits button)))
    (is-false (gtk:spin-button-numeric button))
    (is-false (gtk:spin-button-snap-to-ticks button))
    (is (eq :always (gtk:spin-button-update-policy button)))
    (is (= 0.0d0 (gtk:spin-button-value button)))
    (is-false (gtk:spin-button-wrap button))))

;;; --- Style Properties -------------------------------------------------------

;;;     shadow-type

(test gtk-spin-button-style-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:spin-button)) 'gtk:spin-button))
    (is (eq :in (gtk:widget-style-property button "shadow-type")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spin_button_new

(test gtk-spin-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:spin-button-new nil 0.5 10)) 'gtk:spin-button))
))

;;;     gtk_spin_button_new_with_range

(test gtk-spin-button-new-with-range
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:spin-button-new-with-range 0.0 10.0 1.0))
               'gtk:spin-button))
))

;;;     gtk_spin_button_configure

;;;     gtk_spin_button_get_increments
;;;     gtk_spin_button_set_increments

;;;     gtk_spin_button_get_range
;;;     gtk_spin_button_set_range

;;;     gtk_spin_button_get_value_as_int
;;;     gtk_spin_button_spin
;;;     gtk_spin_button_update

;;; 2026-06-28
