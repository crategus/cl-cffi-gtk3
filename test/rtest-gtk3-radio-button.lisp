(in-package :gtk-test)

(def-suite gtk-radio-button :in gtk-suite)
(in-suite gtk-radio-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioButton

(test radio-button-class
  ;; Type check
  (is (g:type-is-object "GtkRadioButton"))
  ;; Check the registered name
  (is (eq 'gtk:radio-button
          (glib:symbol-for-gtype "GtkRadioButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkRadioButton")
          (g:gtype (cffi:foreign-funcall "gtk_radio_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCheckButton") (g:type-parent "GtkRadioButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkRadioButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable" "GtkActivatable")
             (list-interfaces "GtkRadioButton")))
  ;; Check the class properties
  (is (equal '("group")
             (list-properties "GtkRadioButton")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkRadioButton")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkRadioButton")))
  ;; Check the signals
  (is (equal '("group-changed")
             (list-signals "GtkRadioButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkRadioButton" GTK-RADIO-BUTTON
                       (:SUPERCLASS GTK-CHECK-BUTTON :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_button_get_type")
                       ((GROUP GTK-RADIO-BUTTON-GROUP "group" "GtkRadioButton"
                         NIL T)))
             (gobject:get-g-type-definition "GtkRadioButton"))))

;;; --- Properties -------------------------------------------------------------

(test radio-button-properties
  (let ((button (make-instance 'gtk:radio-button)))
    ;; group is not readable
    (signals (error) (gtk:radio-button-group button))
    ;; group is writable
    (is-false (setf (gtk:radio-button-group button) nil))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    group-changed    Run First

#+nil
(test radio-button-signals
  (let ((result nil)
        (newbutton nil)
        (button (gtk:radio-button-new nil)))
    (g-signal-connect button "group-changed"
                      (lambda (button)
                        (setf result (cons "group-changed" result))
                        (is (eq 'gtk:radio-button (type-of button)))))
    (setf (gtk:radio-button-group button) nil)
    (setf newbutton (gtk:radio-button-new (gtk:radio-button-get-group button)))
    ;; Check if we called the signal handler two times
    (is (equal '("group-changed" "group-changed") result))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_button_new

(test radio-button-new
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new nil)))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new (gtk:radio-button-get-group button))))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; No bin child
  (is-false (gtk:bin-child button))))

;;;     gtk_radio_button_new_from_widget

(test radio-button-new-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-from-widget nil)))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-from-widget button)))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; No bin child
  (is-false (gtk:bin-child button))))

;;;     gtk_radio_button_new_with_label

(test radio-button-new-with-label
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-with-label nil "First Button")))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button
                         (gtk:radio-button-new-with-label (gtk:radio-button-get-group button)
                                                          "Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk:label (type-of (gtk:bin-child button))))
  (is (string= "Second Button" (gtk:label-label (gtk:bin-child button))))))

;;;     gtk_radio_button_new_with_label_from_widget

(test radio-button-new-with-label-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-with-label-from-widget nil "First Button")))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button
                         (gtk:radio-button-new-with-label-from-widget button "Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk:label (type-of (gtk:bin-child button))))
  (is (string= "Second Button" (gtk:label-label (gtk:bin-child button))))))

;;;     gtk_radio_button_new_with_mnemonic

(test radio-button-new-with-mnemonic
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-with-mnemonic nil "_First Button")))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button
                         (gtk:radio-button-new-with-mnemonic (gtk:radio-button-get-group button)
                                                             "_Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk:label (type-of (gtk:bin-child button))))
  (is (string= "_Second Button" (gtk:label-label (gtk:bin-child button))))))

;;;     gtk_radio_button_new_with_mnemonic_from_widget

(test radio-button-new-with-mnemonic-from-widget
  (let (button)
  ;; First radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button (gtk:radio-button-new-with-mnemonic-from-widget nil "_First Button")))))
  ;; Second radio button
  (is (eq 'gtk:radio-button
          (type-of (setf button
                         (gtk:radio-button-new-with-mnemonic-from-widget button
                                                                         "_Second Button")))))
  ;; Check group list
  (is (= 2 (length (gtk:radio-button-get-group button))))
  (is (equal button (first (gtk:radio-button-get-group button))))
  ;; Check bin child
  (is (eq 'gtk:label (type-of (gtk:bin-child button))))
  (is (string= "_Second Button" (gtk:label-label (gtk:bin-child button))))))

;;;     gtk_radio_button_set_group
;;;     gtk_radio_button_get_group

(test radio-button-group
  (let (button)
    ;; First radio button
    (is (eq 'gtk:radio-button (type-of (setf button (gtk:radio-button-new nil)))))
    (is (listp (gtk:radio-button-get-group button)))
    (is (= 1 (length (gtk:radio-button-get-group button))))
    (is (eq 'gtk:radio-button (type-of (first (gtk:radio-button-get-group button)))))
    (is (equal button (first (gtk:radio-button-get-group button))))
    ;; Second radio button
    (is (eq 'gtk:radio-button
            (type-of (setf button (gtk:radio-button-new (gtk:radio-button-get-group button))))))
    (is (listp (gtk:radio-button-get-group button)))
    (is (= 2 (length (gtk:radio-button-get-group button))))
    (is (eq 'gtk:radio-button (type-of (first (gtk:radio-button-get-group button)))))
    (is (equal button (first (gtk:radio-button-get-group button))))
    ;; Third radio button
    (is (eq 'gtk:radio-button
            (type-of (setf button (gtk:radio-button-new (gtk:radio-button-get-group button))))))
    (is (listp (gtk:radio-button-get-group button)))
    (is (= 3 (length (gtk:radio-button-get-group button))))
    (is (eq 'gtk:radio-button (type-of (first (gtk:radio-button-get-group button)))))
    (is (equal button (first (gtk:radio-button-get-group button))))))

;;;     gtk_radio_button_join_group

(test radio-button-join-group.1
  (let (button lastbutton)
    ;; Add three buttons to a group
    (dotimes (i 3)
      (is (eq 'gtk:radio-button (type-of (setf button (gtk:radio-button-new nil)))))
      (is-false (gtk:radio-button-join-group button lastbutton))
      (is (eq 'gtk:radio-button (type-of (setf lastbutton button)))))
    ;; Check radio button group
    (is (listp (gtk:radio-button-get-group button)))
    (is (= 3 (length (gtk:radio-button-get-group button))))
    (is (= 3 (length (gtk:radio-button-get-group lastbutton))))
    (is (eq 'gtk:radio-button (type-of (first (gtk:radio-button-get-group button)))))
    ;; Remove the secion radio button from group
    (is-false (gtk:radio-button-join-group (second (gtk:radio-button-get-group button)) nil))
    (is (= 2 (length (gtk:radio-button-get-group button))))))

(test radio-button-join-group.2
  (let (button lastbutton)
    ;; Add three buttons to a group
    (dolist (label '("First Button" "Second Button" "Third Button"))
      (is (eq 'gtk:radio-button (type-of (setf button (gtk:radio-button-new-with-label nil label)))))
      (is-false (gtk:radio-button-join-group button lastbutton))
      (is (eq 'gtk:radio-button (type-of (setf lastbutton button)))))
    ;; Check radio button group
    (is (listp (gtk:radio-button-get-group button)))
    (is (= 3 (length (gtk:radio-button-get-group button))))
    (is (eq 'gtk:radio-button (type-of (first (gtk:radio-button-get-group button)))))
    ;; Check the bin child
    (is (string= "Third Button"
                 (gtk:label-label (gtk:bin-child (first (gtk:radio-button-get-group button))))))
    (is (string= "Second Button"
                 (gtk:label-label (gtk:bin-child (second (gtk:radio-button-get-group button))))))
    (is (string= "First Button"
                 (gtk:label-label (gtk:bin-child (third (gtk:radio-button-get-group button))))))
    ;; Remove the secion radio button from group
    (is-false (gtk:radio-button-join-group (second (gtk:radio-button-get-group button)) nil))
    (is (= 2 (length (gtk:radio-button-get-group button))))
    (is (string= "Third Button"
                 (gtk:label-label (gtk:bin-child (first (gtk:radio-button-get-group button))))))
    (is (string= "First Button"
                 (gtk:label-label (gtk:bin-child (second (gtk:radio-button-get-group button))))))))

;;; --- 2023-5-29 --------------------------------------------------------------
