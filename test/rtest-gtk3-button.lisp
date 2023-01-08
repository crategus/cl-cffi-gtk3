(in-package :gtk-test)

(def-suite gtk-button :in gtk-suite)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

(test button-class
  ;; Type check
  (is (g:type-is-object "GtkButton"))
  ;; Check the registered name
  (is (eq 'gtk:button
          (gobject:symbol-for-gtype "GtkButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkButton")
          (g:gtype (cffi:foreign-funcall "gtk_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin") (g:type-parent "GtkButton")))
  ;; Check the children
  (is (equal '("GtkColorButton" "GtkFontButton" "GtkLinkButton" "GtkLockButton"
               "GtkModelButton" "GtkScaleButton" "GtkToggleButton")
             (list-children "GtkButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (list-interfaces "GtkButton")))
  ;; Check the class properties
  (is (equal '("action-name" "action-target" "always-show-image" "image"
               "image-position" "label" "related-action" "relief"
               "use-action-appearance" "use-stock" "use-underline" "xalign"
               "yalign")
             (list-properties "GtkButton")))
  ;; Get the names of the style properties
  (is (equal '("child-displacement-x" "child-displacement-y" "default-border"
               "default-outside-border" "displace-focus" "image-spacing"
               "inner-border")
             (list-style-properties "GtkButton")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkButton")))
  ;; Check the signals
  (is (equal '("activate" "clicked" "enter" "leave" "pressed" "released")
             (list-signals "GtkButton")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButton" GTK-BUTTON
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_button_get_type")
                       ((ALWAYS-SHOW-IMAGE GTK-BUTTON-ALWAYS-SHOW-IMAGE
                         "always-show-image" "gboolean" T T)
                        (IMAGE GTK-BUTTON-IMAGE "image" "GtkWidget" T T)
                        (IMAGE-POSITION GTK-BUTTON-IMAGE-POSITION
                         "image-position" "GtkPositionType" T T)
                        (LABEL GTK-BUTTON-LABEL "label" "gchararray" T T)
                        (RELIEF GTK-BUTTON-RELIEF "relief" "GtkReliefStyle" T
                         T)
                        (USE-STOCK GTK-BUTTON-USE-STOCK "use-stock" "gboolean"
                         T T)
                        (USE-UNDERLINE GTK-BUTTON-USE-UNDERLINE "use-underline"
                         "gboolean" T T)
                        (XALIGN GTK-BUTTON-XALIGN "xalign" "gfloat" T T)
                        (YALIGN GTK-BUTTON-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-g-type-definition "GtkButton"))))

;;; --- Properties -------------------------------------------------------------

(test button-properties
  (let ((button (make-instance 'gtk:button)))
    (is-false (gtk:button-always-show-image button))
    (is-false (gtk:button-image button))
    (is (eq :left (gtk:button-image-position button)))
    (is-false (gtk:button-label button))
    (is (eq :normal (gtk:button-relief button)))
    (is-false (gtk:button-use-stock button))
    (is-false (gtk:button-use-underline button))
    (is (= 0.5 (gtk:button-xalign button)))
    (is (= 0.5 (gtk:button-yalign button)))))

;;; --- Style Properties -------------------------------------------------------

(test button-style-properties
  (let ((button (make-instance 'gtk:button)))
    (is (= 0 (gtk:widget-style-property button "child-displacement-x")))
    (is (= 0 (gtk:widget-style-property button "child-displacement-y")))
    (is-false (gtk:widget-style-property button "default-border"))
    (is-false (gtk:widget-style-property button "default-outside-border"))
    (is-false (gtk:widget-style-property button "displace-focus"))
    (is (= 2 (gtk:widget-style-property button "image-spacing")))
    (is-false (gtk:widget-style-property button "inner-border"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_new

(test button-new
  (let ((button (gtk:button-new)))
    (is-false (gtk:button-always-show-image button))
    (is-false (gtk:button-image button))
    (is (eq :left (gtk:button-image-position button)))
    (is-false (gtk:button-label button))
    (is (eq :normal (gtk:button-relief button)))
    (is-false (gtk:button-use-stock button))
    (is-false (gtk:button-use-underline button))
    (is (= 0.5 (gtk:button-xalign button)))
    (is (= 0.5 (gtk:button-yalign button)))))

;;;     gtk_button_new_with_label

(test button-new-with-label
  (let ((button (gtk:button-new-with-label "Label")))
    (is-false (gtk:button-always-show-image button))
    (is-false (gtk:button-image button))
    (is (eq :left (gtk:button-image-position button)))
    (is (string= "Label" (gtk:button-label button)))
    (is (eq :normal (gtk:button-relief button)))
    (is-false (gtk:button-use-stock button))
    (is-false (gtk:button-use-underline button))
    (is (= 0.5 (gtk:button-xalign button)))
    (is (= 0.5 (gtk:button-yalign button)))))

;;;     gtk_button_new_with_mnemonic

(test button-new-with-mnemonic
  (let ((button (gtk:button-new-with-mnemonic "_Mnemonic")))
    (is-false (gtk:button-always-show-image button))
    (is-false (gtk:button-image button))
    (is (eq :left (gtk:button-image-position button)))
    (is (string= "_Mnemonic" (gtk:button-label button)))
    (is (eq :normal (gtk:button-relief button)))
    (is-false (gtk:button-use-stock button))
    (is-true (gtk:button-use-underline button))
    (is (= 0.5 (gtk:button-xalign button)))
    (is (= 0.5 (gtk:button-yalign button)))))

;;;     gtk_button_new_from_icon_name

(test button-new-from-icon-name
  (let ((button (gtk:button-new-from-icon-name "edit-copy" :button)))
    (is-false (gtk:button-always-show-image button))
    (is (typep (gtk:button-image button) 'gtk:image))
    (is (eq :left (gtk:button-image-position button)))
    (is-false (gtk:button-label button))
    (is (eq :normal (gtk:button-relief button)))
    (is-false (gtk:button-use-stock button))
    (is-false (gtk:button-use-underline button))
    (is (= 0.5 (gtk:button-xalign button)))
    (is (= 0.5 (gtk:button-yalign button)))))

;;;     gtk_button_new_from_stock                          deprecated

;;;     gtk_button_clicked

#+nil
(test button-clicked
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (setf message "Signal clicked")
                         (is (typep widget 'gtk:button))
                         t))))
    (declare (ignore handler-id))
    ;; Emit the signal
    (is-false (gtk:button-clicked button))
    (is (string= "Signal clicked" message))))

;;;     gtk_button_alignment                               deprecated

;;;     gtk_button_event_window

(test button-event-window
  (let ((window (make-instance 'gtk:window :type :toplevel))
        (button (gtk:button-new-from-icon-name "gtk-close" :button)))
    (is-false (gtk:container-add window button))
    (is-false (gtk:widget-realize window))
    (is-false (gtk:widget-realize button))
    (is (typep (gtk:button-event-window button) 'gdk:window))))

;;; --- 2022-12-27 -------------------------------------------------------------
