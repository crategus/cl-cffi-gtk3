(in-package :gtk-test)

(def-suite gtk-info-bar :in gtk-suite)
(in-suite gtk-info-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInfoBar

(test info-bar-class
  ;; Type check
  (is (g:type-is-object "GtkInfoBar"))
  ;; Check the registered name
  (is (eq 'gtk:info-bar
          (glib:symbol-for-gtype "GtkInfoBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkInfoBar")
          (g:gtype (cffi:foreign-funcall "gtk_info_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox") (g:type-parent "GtkInfoBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkInfoBar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkInfoBar")))
  ;; Check the class properties
  (is (equal '("message-type" "revealed" "show-close-button")
             (list-properties "GtkInfoBar")))
  ;; Get the names of the style properties.
  (is (equal '("action-area-border" "button-spacing" "content-area-border"
               "content-area-spacing")
             (list-style-properties "GtkInfoBar")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkInfoBar")))
  ;; Check the signals
  (is (equal '("close" "response")
             (list-signals "GtkInfoBar")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkInfoBar" GTK-INFO-BAR
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_info_bar_get_type")
                       ((MESSAGE-TYPE GTK-INFO-BAR-MESSAGE-TYPE "message-type"
                         "GtkMessageType" T T)
                        (REVEALED GTK-INFO-BAR-REVEALED "revealed" "gboolean" T
                         T)
                        (SHOW-CLOSE-BUTTON GTK-INFO-BAR-SHOW-CLOSE-BUTTON
                         "show-close-button" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkInfoBar"))))

;;; --- Properties -------------------------------------------------------------

(test info-bar-properties
  (let ((info-bar (make-instance 'gtk:info-bar)))
    ;; message-type
    (is (eq :info (gtk:info-bar-message-type info-bar)))
    (is (eq :error (setf (gtk:info-bar-message-type info-bar) :error)))
    (is (eq :error (gtk:info-bar-message-type info-bar)))
    ;; revealed
    (is-true (gtk:info-bar-revealed info-bar))
    (is-false (setf (gtk:info-bar-revealed info-bar) nil))
    (is-false (gtk:info-bar-revealed info-bar))
    ;; show-close-button
    (is-false (gtk:info-bar-show-close-button info-bar))
    (is-true (setf (gtk:info-bar-show-close-button info-bar) t))
    (is-true (gtk:info-bar-show-close-button info-bar))))

;;; --- Style Properties -------------------------------------------------------

(test info-bar-style-properties
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (=  5 (gtk:widget-style-property info-bar "action-area-border")))
    (is (=  6 (gtk:widget-style-property info-bar "button-spacing")))
    (is (=  8 (gtk:widget-style-property info-bar "content-area-border")))
    (is (= 16 (gtk:widget-style-property info-bar "content-area-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_info_bar_new

(test info-bar-new
  (is (eq 'gtk:info-bar (type-of (gtk:info-bar-new)))))

;;;     gtk_info_bar_new_with_buttons

(test info-bar-new-with-buttons
  (let ((info-bar nil))
    (is (eq 'gtk:info-bar
            (type-of (setf info-bar
                           (gtk:info-bar-new-with-buttons "gtk-ok" 1)))))
    (is (= 1
           (length
               (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is (eq 'gtk:info-bar
            (type-of (setf info-bar
                           (gtk:info-bar-new-with-buttons "gtk-ok" 1
                                                          "gtk-cancel" 2)))))
    (is (= 2
           (length
               (gtk:container-children (gtk:info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_action_widget

(test info-bar-add-action-widget
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (= 0
           (length
               (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is-false (gtk:info-bar-add-action-widget info-bar
                                              (make-instance 'gtk:button) 1))
    (is (= 1
           (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is-false (gtk:info-bar-add-action-widget info-bar
                                              (make-instance 'gtk:button) 2))
    (is (= 2
           (length
               (gtk:container-children (gtk:info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_button

(test info-bar-add-button
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (= 0 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is (eq 'gtk:button (type-of (gtk:info-bar-add-button info-bar "gtk-ok" 1))))
    (is (= 1 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is (eq 'gtk:button (type-of (gtk:info-bar-add-button info-bar "gtk-cancel" 2))))
    (is (= 2 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_add_buttons

(test info-bar-add-buttons
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (= 0 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is-false (gtk:info-bar-add-buttons info-bar "gtk-ok" 1))
    (is (= 1 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))
    (is-false (gtk:info-bar-add-buttons info-bar "gtk-cancel" 2 "gtk-no" 3))
    (is (= 3 (length (gtk:container-children (gtk:info-bar-action-area info-bar)))))))

;;;     gtk_info_bar_set_response_sensitive

(test info-bar-set-response-sensitive
  (let ((info-bar (gtk:info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3)))
    (is-false (gtk:info-bar-set-response-sensitive info-bar 1 nil))))

;;;     gtk_info_bar_set_default_response

(test info-bar-set-default-response
  (let ((window (make-instance 'gtk:window))
        (info-bar (gtk:info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3)))
    ;; The info bar must be within a GtkWindow
    (is-false (gtk:container-add window info-bar))
    (is-false (gtk:info-bar-set-default-response info-bar 1))))

;;;     gtk_info_bar_response

;;;     gtk_info_bar_get_action_area

(test info-bar-action-area
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (eq 'gtk:button-box (type-of (gtk:info-bar-action-area info-bar))))))

;;;     gtk_info_bar_get_content_area

(test info-bar-content-area
  (let ((info-bar (make-instance 'gtk:info-bar)))
    (is (eq 'gtk:box (type-of (gtk:info-bar-content-area info-bar))))))

;;; --- 2023-5-29 --------------------------------------------------------------
