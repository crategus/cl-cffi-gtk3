(in-package :gtk-test)

(def-suite gtk-radio-button :in gtk-suite)
(in-suite gtk-radio-button)

;; TOOO:  The following warning is thrown with SBCL:
;;   TOGGLE-NOTIFY: GtkRadioButton at #.(SB-SYS:INT-SAP #X617174B4B290)
;;   has no Lisp side (weak) reference
;;
;; This seems to be a problem with the handling of the memory for groups of
;; buttons. This problem might be in the C library.

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioButton

(test gtk-radio-button-class
  ;; Check type
  (is (g:type-is-object "GtkRadioButton"))
  ;; Check registered name
  (is (eq 'gtk:radio-button
          (glib:symbol-for-gtype "GtkRadioButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRadioButton")
          (g:gtype (cffi:foreign-funcall "gtk_radio_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCheckButton") (g:type-parent "GtkRadioButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRadioButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable" "GtkActivatable")
             (glib-test:list-interfaces "GtkRadioButton")))
  ;; Check class properties
  (is (equal '("group")
             (glib-test:list-properties "GtkRadioButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkRadioButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkRadioButton")))
  ;; Check signals
  (is (equal '("group-changed")
             (glib-test:list-signals "GtkRadioButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRadioButton" GTK:RADIO-BUTTON
                      (:SUPERCLASS GTK:CHECK-BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_radio_button_get_type")
                      ((GROUP RADIO-BUTTON-GROUP
                        "group" "GtkRadioButton" NIL T)))
             (gobject:get-gtype-definition "GtkRadioButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     group-changed

#+nil
(test gtk-radio-button-signals
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

(test gtk-radio-button-group-changed-signal
  (let* ((name "group-changed")
         (gtype (g:gtype "GtkRadioButton"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-radio-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:radio-button)) 'gtk:radio-button))
    ;; group is not readable
    (signals (error) (gtk:radio-button-group button))
    ;; group is writable
    (is-false (setf (gtk:radio-button-group button) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_button_new

(test gtk-radio-button-new
  (glib-test:with-check-memory (button)
    (let (group)
      ;; First radio button
      (is (typep (setf button (gtk:radio-button-new)) 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button
                       (gtk:radio-button-new (gtk:radio-button-get-group button)))
                 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (first group)))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group)))
      ;; No bin child
      (is-false (gtk:bin-child button)))))

;;;     gtk_radio_button_new_from_widget

(test gtk-radio-button-new-from-widget
  (glib-test:with-check-memory (button)
    (let (group)
      ;; First radio button
      (is (typep (setf button (gtk:radio-button-new-from-widget)) 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button (gtk:radio-button-new-from-widget button)) 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (first group)))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group)))
      ;; No bin child
      (is-false (gtk:bin-child button)))))

;;;     gtk_radio_button_new_with_label

(test gtk-radio-button-new-with-label
  (glib-test:with-check-memory (button label)
    (let (group)
      ;; First radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-label nil "First Button"))
                 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-label (gtk:radio-button-get-group button)
                                                        "Second Button"))
                 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (setf button (first group))))
      ;; Check bin child for first button in the group list
      (is (typep (setf label (gtk:bin-child button)) 'gtk:label))
      (is (string= "Second Button" (gtk:label-label (gtk:bin-child button))))
      ;; Remove label from button
      (is-false (gtk:container-remove button label)))))

;;;     gtk_radio_button_new_with_label_from_widget

(test gtk-radio-button-new-with-label-from-widget
  (glib-test:with-check-memory (button label)
    (let (group)
      ;; First radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-label-from-widget nil "First Button"))
                 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-label-from-widget button "Second Button"))
                 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (setf button (first group))))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group)))
      ;; Check bin child
      (is (typep (setf label (gtk:bin-child button)) 'gtk:label))
      (is (string= "Second Button" (gtk:label-label (gtk:bin-child button))))
      ;; Remove label from button
      (is-false (gtk:container-remove button label)))))

;;;     gtk_radio_button_new_with_mnemonic

(test gtk-radio-button-new-with-mnemonic
  (glib-test:with-check-memory (button label)
    (let (group)
      ;; First radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-mnemonic nil "_First Button"))
                 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-mnemonic (gtk:radio-button-get-group button)
                                                           "_Second Button"))
                 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (setf button (first group))))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group)))
      ;; Check bin child
      (is (typep (setf label (gtk:bin-child button)) 'gtk:label))
      (is (string= "_Second Button" (gtk:label-label (gtk:bin-child button))))
      ;; Remove label from button
      (is-false (gtk:container-remove button label)))))

;;;     gtk_radio_button_new_with_mnemonic_from_widget

(test gtk-radio-button-new-with-mnemonic-from-widget
  (glib-test:with-check-memory (button label)
    (let (group)
      ;; First radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-mnemonic-from-widget nil "_First Button"))
                 'gtk:radio-button))
      ;; Second radio button
      (is (typep (setf button
                       (gtk:radio-button-new-with-mnemonic-from-widget button
                                                                       "_Second Button"))
                 'gtk:radio-button))
      ;; Check group list
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (eq button (setf button (first group))))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group)))
      ;; Check bin child
      (is (typep (setf label (gtk:bin-child button)) 'gtk:label))
      (is (string= "_Second Button" (gtk:label-label (gtk:bin-child button))))
      ;; Remove label from button
      (is-false (gtk:container-remove button label)))))

;;;     gtk_radio_button_set_group
;;;     gtk_radio_button_get_group

;;;     gtk_radio_button_join_group

(test gtk-radio-button-join-group.1
  (glib-test:with-check-memory (button lastbutton)
    (let (group)
      ;; Add three buttons to a group
      (dotimes (i 3)
        (is (typep (setf button (gtk:radio-button-new-from-widget)) 'gtk:radio-button))
        (is-false (gtk:radio-button-join-group button lastbutton))
        (is (typep (setf lastbutton button) 'gtk:radio-button)))
      ;; Check radio button group
      (is (= 3 (length (setf group (gtk:radio-button-get-group button)))))
      (is (equal '(1 1 1) (mapcar #'g:object-ref-count group)))
      (is (= 3 (length (setf group (gtk:radio-button-get-group lastbutton)))))
      (is (equal '(1 1 1) (mapcar #'g:object-ref-count group)))
      ;; Remove the second radio button from group
      (is-false (gtk:radio-button-join-group (second group) nil))
      (is (= 2 (length (setf group (gtk:radio-button-get-group button)))))
      (is (equal '(1 1) (mapcar #'g:object-ref-count group))))))

(test gtk-radio-button-join-group.2
  (glib-test:with-check-memory (button lastbutton)
    (let (group)
      ;; Add three buttons to a group
      (dolist (label '("First Button" "Second Button" "Third Button"))
        (is (typep (setf button (gtk:radio-button-new-with-label-from-widget nil label))
                   'gtk:radio-button))
        (is-false (gtk:radio-button-join-group button lastbutton))
        (is (typep (setf lastbutton button) 'gtk:radio-button)))
      ;; Check radio button group
      (is (= 3 (length (setf group (gtk:radio-button-get-group button)))))
      (is (equal '(1 1 1) (mapcar #'g:object-ref-count group)))
      ;; Check the bin child
      (is (string= "Third Button"
                   (gtk:label-label (gtk:bin-child
                                        (first (gtk:radio-button-get-group button))))))
      (is (string= "Second Button"
                   (gtk:label-label (gtk:bin-child
                                        (second (gtk:radio-button-get-group button))))))
      (is (string= "First Button"
                   (gtk:label-label (gtk:bin-child
                                        (third (gtk:radio-button-get-group button))))))
      ;; Remove label from buttons
      (dolist (lastbutton (gtk:radio-button-get-group button))
        (is-false (gtk:container-remove lastbutton (gtk:bin-child lastbutton)))))))

;;; 2026-06-26
