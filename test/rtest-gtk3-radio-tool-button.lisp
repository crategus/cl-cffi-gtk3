(in-package :gtk-test)

(def-suite gtk-radio-tool-button :in gtk-suite)
(in-suite gtk-radio-tool-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioToolButton

(test gtk-radio-tool-button-class
  ;; Check type
  (is (g:type-is-object "GtkRadioToolButton"))
  ;; Check registered name
  (is (eq 'gtk:radio-tool-button
          (glib:symbol-for-gtype "GtkRadioToolButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRadioToolButton")
          (g:gtype (cffi:foreign-funcall "gtk_radio_tool_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToggleToolButton") (g:type-parent "GtkRadioToolButton")))
  ;; Check  children
  (is (equal '()
             (glib-test:list-children "GtkRadioToolButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkRadioToolButton")))
  ;; Check class properties
  (is (equal '("group")
             (glib-test:list-properties "GtkRadioToolButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkRadioToolButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkRadioToolButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkRadioToolButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRadioToolButton" GTK:RADIO-TOOL-BUTTON
                       (:SUPERCLASS GTK:TOGGLE-TOOL-BUTTON
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_tool_button_get_type")
                       ((GROUP RADIO-TOOL-BUTTON-GROUP
                         "group" "GtkRadioToolButton" NIL T)))
             (gobject:get-gtype-definition "GtkRadioToolButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-radio-tool-button-properties
  (let ((button (make-instance 'gtk:radio-tool-button)))
    ;; group is not readable
    (signals (error) (gtk:radio-tool-button-group button))
    ;; group is writable
    (is-false (setf (gtk:radio-tool-button-group button) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_tool_button_new

(test gtk-radio-tool-button-new
  (let (button)
  ;; First radio button
  (is (typep (setf button (gtk:radio-tool-button-new nil))
             'gtk:radio-tool-button))
  ;; Second radio button
  (is (typep (setf button
                   (gtk:radio-tool-button-new
                       (gtk:radio-tool-button-get-group button)))
             'gtk:radio-tool-button))
  ;; Check group list
  (is (= 2 (length (gtk:radio-tool-button-get-group button))))
  (is (typep (first (gtk:radio-tool-button-get-group button))
             'gtk:radio-button))
  ;; No bin child
  (is (typep (gtk:bin-child button) 'gtk:radio-button))))

;;;     gtk_radio_tool_button_new_from_stock

;;;     gtk_radio_tool_button_new_from_widget

(test gtk-radio-tool-button-new-from-widget
  (let (button)
  ;; First radio button
  (is (typep (setf button (gtk:radio-tool-button-new-from-widget nil))
             'gtk:radio-tool-button))
  ;; Second radio button
  (is (typep (setf button (gtk:radio-tool-button-new-from-widget button))
             'gtk:radio-tool-button))
  ;; Check group list
  (is (= 2 (length (gtk:radio-tool-button-get-group button))))
  (is (typep (first (gtk:radio-tool-button-get-group button))
             'gtk:radio-button))
  ;; No bin child
  (is (typep (gtk:bin-child button) 'gtk:radio-button))))

;;;     gtk_radio_tool_button_new_with_stock_from_widget

;;;     gtk_radio_tool_button_get_group
;;;     gtk_radio_tool_button_set_group

(test gtk-radio-tool-button-group
  (let (button)
    ;; First radio button
    (is (typep (setf button (gtk:radio-tool-button-new nil))
               'gtk:radio-tool-button))
    (is (listp (gtk:radio-tool-button-get-group button)))
    (is (= 1 (length (gtk:radio-tool-button-get-group button))))
    (is (typep (first (gtk:radio-tool-button-get-group button))
               'gtk:radio-button))
    ;; Second radio button
    (is (typep (setf button
                     (gtk:radio-tool-button-new
                         (gtk:radio-tool-button-get-group button)))
               'gtk:radio-tool-button))
    (is (listp (gtk:radio-tool-button-get-group button)))
    (is (= 2 (length (gtk:radio-tool-button-get-group button))))
    (is (typep (first (gtk:radio-tool-button-get-group button))
               'gtk:radio-button))
    ;; Third radio button
    (is (typep (setf button
                     (gtk:radio-tool-button-new
                         (gtk:radio-tool-button-get-group button)))
               'gtk:radio-tool-button))
    (is (listp (gtk:radio-tool-button-get-group button)))
    (is (= 3 (length (gtk:radio-tool-button-get-group button))))
    (is (typep (first (gtk:radio-tool-button-get-group button))
               'gtk:radio-button))))

;;; 2024-9-23
