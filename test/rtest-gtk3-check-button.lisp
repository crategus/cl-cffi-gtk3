(in-package :gtk-test)

(def-suite gtk-check-button :in gtk-suite)
(in-suite gtk-check-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCheckButton

(test gtk-check-button-class
  ;; Check type
  (is (g:type-is-object "GtkCheckButton"))
  ;; Check registered name
  (is (eq 'gtk:check-button
          (glib:symbol-for-gtype "GtkCheckButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCheckButton")
          (g:gtype (cffi:foreign-funcall "gtk_check_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToggleButton")
          (g:type-parent "GtkCheckButton")))
  ;; Check children
  (is (equal '("GtkRadioButton")
             (glib-test:list-children "GtkCheckButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkCheckButton")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkCheckButton")))
  ;; Check style properties
  (is (equal '("indicator-size" "indicator-spacing")
             (gtk-test:list-style-properties "GtkCheckButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkCheckButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCheckButton")))
  ;; Check CSS information
  (is (string= "checkbutton"
               (gtk:widget-class-css-name "GtkCheckButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCheckButton" GTK:CHECK-BUTTON
                      (:SUPERCLASS GTK:TOGGLE-BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_check_button_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkCheckButton"))))

;;; --- Style Properties -------------------------------------------------------

;;;     indicator-size
;;;     indicator-spacing

(test gtk-check-button-style-properties
  (let ((button (make-instance 'gtk:check-button)))
    (is (= 16 (gtk:widget-style-property button "indicator-size")))
    (is (= 2 (gtk:widget-style-property button "indicator-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_check_button_new

(test gtk-check-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:check-button-new)) 'gtk:check-button))))

;;;     gtk_check_button_new_with_label

(test gtk-check-button-new-with-label
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:check-button-new-with-label "label"))
               'gtk:check-button))))

;;;     gtk_check_button_new_with_mnemonic

(test gtk-check-button-new-with-mnemonic
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:check-button-new-with-mnemonic "_label"))
               'gtk:check-button))))

;;; 2025-3-9
