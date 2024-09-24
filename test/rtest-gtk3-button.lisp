(in-package :gtk-test)

(def-suite gtk-button :in gtk-suite)
(in-suite gtk-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkButton

(test gtk-button-class
  ;; Check type
  (is (g:type-is-object "GtkButton"))
  ;; Check registered name
  (is (eq 'gtk:button
          (glib:symbol-for-gtype "GtkButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButton")
          (g:gtype (cffi:foreign-funcall "gtk_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin") (g:type-parent "GtkButton")))
  ;; Check children
  (is (equal '("GtkColorButton" "GtkFontButton" "GtkLinkButton" "GtkLockButton"
               "GtkModelButton" "GtkScaleButton" "GtkToggleButton")
             (glib-test:list-children "GtkButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkButton")))
  ;; Check class properties
  (is (equal '("action-name" "action-target" "always-show-image" "image"
               "image-position" "label" "related-action" "relief"
               "use-action-appearance" "use-stock" "use-underline" "xalign"
               "yalign")
             (glib-test:list-properties "GtkButton")))
  ;; Check style properties
  (is (equal '("child-displacement-x" "child-displacement-y" "default-border"
               "default-outside-border" "displace-focus" "image-spacing"
               "inner-border")
             (gtk-test:list-style-properties "GtkButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkButton")))
  ;; Check signals
  (is (equal '("activate" "clicked" "enter" "leave" "pressed" "released")
             (glib-test:list-signals "GtkButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkButton" GTK:BUTTON
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_button_get_type")
                       ((ALWAYS-SHOW-IMAGE BUTTON-ALWAYS-SHOW-IMAGE
                         "always-show-image" "gboolean" T T)
                        (IMAGE BUTTON-IMAGE "image" "GtkWidget" T T)
                        (IMAGE-POSITION BUTTON-IMAGE-POSITION
                         "image-position" "GtkPositionType" T T)
                        (LABEL BUTTON-LABEL "label" "gchararray" T T)
                        (RELIEF BUTTON-RELIEF "relief" "GtkReliefStyle" T T)
                        (USE-STOCK BUTTON-USE-STOCK "use-stock" "gboolean" T T)
                        (USE-UNDERLINE BUTTON-USE-UNDERLINE
                         "use-underline" "gboolean" T T)
                        (XALIGN BUTTON-XALIGN "xalign" "gfloat" T T)
                        (YALIGN BUTTON-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-button-properties
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

(test gtk-button-style-properties
  (let ((button (make-instance 'gtk:button)))
    (is (= 0 (gtk:widget-style-property button "child-displacement-x")))
    (is (= 0 (gtk:widget-style-property button "child-displacement-y")))
    (is-false (gtk:widget-style-property button "default-border"))
    (is-false (gtk:widget-style-property button "default-outside-border"))
    (is-false (gtk:widget-style-property button "displace-focus"))
    (is (= 2 (gtk:widget-style-property button "image-spacing")))
    (is-false (gtk:widget-style-property button "inner-border"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     clicked
;;;     enter
;;;     leave
;;;     pressed
;;;     released

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_new

(test gtk-button-new
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

(test gtk-button-new-with-label
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

(test gtk-button-new-with-mnemonic
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

(test gtk-button-new-from-icon-name
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

(test gtk-button-clicked
  (let* ((message nil)
         (button (make-instance 'gtk:button))
         (handler-id (g:signal-connect button "clicked"
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

(test gtk-button-event-window
  (let ((window (make-instance 'gtk:window :type :toplevel))
        (button (gtk:button-new-from-icon-name "gtk-close" :button)))
    (is-false (gtk:container-add window button))
    (is-false (gtk:widget-realize window))
    (is-false (gtk:widget-realize button))
    (is (typep (gtk:button-event-window button) 'gdk:window))))

;;; 2024-9-21
