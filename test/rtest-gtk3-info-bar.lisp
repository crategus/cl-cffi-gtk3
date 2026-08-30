(in-package :gtk-test)

(def-suite gtk-info-bar :in gtk-suite)
(in-suite gtk-info-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInfoBar

(test gtk-info-bar-class
  ;; Check type
  (is (g:type-is-object "GtkInfoBar"))
  ;; Check registered name
  (is (eq 'gtk:info-bar
          (glib:symbol-for-gtype "GtkInfoBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInfoBar")
          (g:gtype (cffi:foreign-funcall "gtk_info_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox") (g:type-parent "GtkInfoBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkInfoBar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkInfoBar")))
  ;; Check class properties
  (is (equal '("message-type" "revealed" "show-close-button")
             (glib-test:list-properties "GtkInfoBar")))
  ;; Check style properties
  (is (equal '("action-area-border" "button-spacing" "content-area-border"
               "content-area-spacing")
             (gtk-test:list-style-properties "GtkInfoBar")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkInfoBar")))
  ;; Check signals
  (is (equal '("close" "response")
             (glib-test:list-signals "GtkInfoBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkInfoBar" GTK:INFO-BAR
                      (:SUPERCLASS GTK:BOX
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_info_bar_get_type")
                      ((MESSAGE-TYPE INFO-BAR-MESSAGE-TYPE
                        "message-type" "GtkMessageType" T T)
                       (REVEALED INFO-BAR-REVEALED "revealed" "gboolean" T T)
                       (SHOW-CLOSE-BUTTON INFO-BAR-SHOW-CLOSE-BUTTON
                        "show-close-button" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkInfoBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-info-bar-properties
  (glib-test:with-check-memory (infobar)
    (setf infobar (make-instance 'gtk:info-bar))
    ;; message-type
    (is (eq :info (gtk:info-bar-message-type infobar)))
    (is (eq :error (setf (gtk:info-bar-message-type infobar) :error)))
    (is (eq :error (gtk:info-bar-message-type infobar)))
    ;; revealed
    (is-true (gtk:info-bar-revealed infobar))
    (is-false (setf (gtk:info-bar-revealed infobar) nil))
    (is-false (gtk:info-bar-revealed infobar))
    ;; show-close-button
    (is-false (gtk:info-bar-show-close-button infobar))
    (is-true (setf (gtk:info-bar-show-close-button infobar) t))
    (is-true (gtk:info-bar-show-close-button infobar))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-info-bar-style-properties
  (glib-test:with-check-memory (infobar)
    (setf infobar (make-instance 'gtk:info-bar))
    (is (=  5 (gtk:widget-style-property infobar "action-area-border")))
    (is (=  6 (gtk:widget-style-property infobar "button-spacing")))
    (is (=  8 (gtk:widget-style-property infobar "content-area-border")))
    (is (= 16 (gtk:widget-style-property infobar "content-area-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_info_bar_new

(test gtk-info-bar-new
  (glib-test:with-check-memory (infobar)
    (is (typep (setf infobar (gtk:info-bar-new)) 'gtk:info-bar))))

;;;     gtk_info_bar_new_with_buttons

;; 2 strong references for the button box of the action area

(test gtk-info-bar-new-with-buttons
  (glib-test:with-check-memory (infobar :strong 2)
    (is (typep (setf infobar
                     (gtk:info-bar-new-with-buttons "gtk-ok" 1)) 'gtk:info-bar))
    (is (= 1
           (length
               (gtk:container-children (gtk:info-bar-action-area infobar)))))

    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))

    (is (typep (setf infobar
                     (gtk:info-bar-new-with-buttons "gtk-ok" 1
                                                    "gtk-cancel" 2)) 'gtk:info-bar))
    (is (= 2
           (length
               (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))))

;;;     gtk_info_bar_add_action_widget

(test gtk-info-bar-add-action-widget
  (glib-test:with-check-memory (infobar :strong 1)
    (setf infobar (make-instance 'gtk:info-bar))
    (is (= 0
           (length
               (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is-false (gtk:info-bar-add-action-widget infobar
                                              (make-instance 'gtk:button) 1))
    (is (= 1
           (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is-false (gtk:info-bar-add-action-widget infobar
                                              (make-instance 'gtk:button) 2))
    (is (= 2
           (length
               (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))))

;;;     gtk_info_bar_add_button

(test gtk-info-bar-add-button
  (glib-test:with-check-memory (infobar :strong 1)
    (setf infobar (make-instance 'gtk:info-bar))
    (is (= 0 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is (eq 'gtk:button (type-of (gtk:info-bar-add-button infobar "gtk-ok" 1))))
    (is (= 1 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is (eq 'gtk:button (type-of (gtk:info-bar-add-button infobar "gtk-cancel" 2))))
    (is (= 2 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    ;; Remove references
    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))))

;;;     gtk_info_bar_add_buttons

(test gtk-info-bar-add-buttons
  (glib-test:with-check-memory (infobar :strong 1)
    (setf infobar (make-instance 'gtk:info-bar))
    (is (= 0 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is-false (gtk:info-bar-add-buttons infobar "gtk-ok" 1))
    (is (= 1 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    (is-false (gtk:info-bar-add-buttons infobar "gtk-cancel" 2 "gtk-no" 3))
    (is (= 3 (length (gtk:container-children (gtk:info-bar-action-area infobar)))))
    ;; Remove references
    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))))

;;;     gtk_info_bar_set_response_sensitive

(test gtk-info-bar-set-response-sensitive
  (glib-test:with-check-memory (infobar :strong 1)
    (setf infobar (gtk:info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3))
    (is-false (gtk:info-bar-set-response-sensitive infobar 1 nil))
    ;; Remove references
    (dolist (child (gtk:container-children (gtk:info-bar-action-area infobar)))
      (gtk:container-remove (gtk:info-bar-action-area infobar) child))))

;;;     gtk_info_bar_set_default_response

(test gtk-info-bar-set-default-response
  (glib-test:with-check-memory (window infobar)
    (setf window (make-instance 'gtk:window))
    (setf infobar (gtk:info-bar-new-with-buttons "gtk-ok" 1 "gtk-cancel" 2 "gtk-no" 3))
    ;; The info bar must be within a GtkWindow
    (is-false (gtk:container-add window infobar))
    (is-false (gtk:info-bar-set-default-response infobar 1))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_info_bar_response

;;;     gtk_info_bar_get_action_area
;;;     gtk_info_bar_get_content_area

(test gtk-info-bar-action/content-area
  (glib-test:with-check-memory (infobar :strong 2)
    (setf infobar (make-instance 'gtk:info-bar))
    (is (eq 'gtk:button-box (type-of (gtk:info-bar-action-area infobar))))
    (is (eq 'gtk:box (type-of (gtk:info-bar-content-area infobar))))))

;;; 2026-06-20
