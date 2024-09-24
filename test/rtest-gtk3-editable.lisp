(in-package :gtk-test)

(def-suite gtk-editable :in gtk-suite)
(in-suite gtk-editable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEditable

(test gtk-editable-interface
  ;; Check type
  (is (g:type-is-interface "GtkEditable"))
  ;; Check registered name
  (is (eq 'gtk:editable
          (glib:symbol-for-gtype "GtkEditable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEditable")
          (g:gtype (cffi:foreign-funcall "gtk_editable_get_type" :size))))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkPrintOperationPreview")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkPrintOperationPreview"
                                         GTK:PRINT-OPERATION-PREVIEW
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_print_operation_preview_get_type"))
             (gobject:get-gtype-definition "GtkPrintOperationPreview"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed
;;;     delete-text
;;;     insert-text

;;; --- Functions --------------------------------------------------------------

;;;     gtk_editable_select_region
;;;     gtk_editable_get_selection_bounds

(test gtk-editable-select-region/selection-bounds
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    (is-false (gtk:editable-select-region entry :start 8 :end 12))
    (is (equal '(8 12)
               (multiple-value-list (gtk:editable-selection-bounds entry))))
    (is (string= "some" (gtk:editable-chars entry :start 8 :end 12)))
    ;; Use default values for :start and :end
    (is-false (gtk:editable-select-region entry))
    (is (equal '(0 18)
               (multiple-value-list (gtk:editable-selection-bounds entry))))
    ;; Use defalut value for :end
    (is-false (gtk:editable-select-region entry :start 8))
    (is (equal '(8 18)
               (multiple-value-list (gtk:editable-selection-bounds entry))))))

;;;     gtk_editable_insert_text
;;;     gtk_editable_delete_text

(test gtk-editable-insert/delete-text
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    ;; Delete text
    (is-false (gtk:editable-delete-text entry :start 8 :end 12))
    (is (string= "This is  text." (gtk:editable-chars entry)))
    ;; Insert text
    (is (= 13 (gtk:editable-insert-text entry "extra" 8)))
    (is (string= "This is extra text." (gtk:editable-chars entry)))))

;;;     gtk_editable_get_chars

(test gtk-editable-chars
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    (is (string= "This is some text." (gtk:editable-chars entry)))
    (is (string= "some text." (gtk:editable-chars entry :start 8)))
    (is (string= "some" (gtk:editable-chars entry :start 8 :end 12)))))

;;;     gtk_editable_cut_clipboard
;;;     gtk_editable_copy_clipboard
;;;     gtk_editable_paste_clipboard

;; TODO: We have no clipboard. Can we improve this?

;; (sbcl:7881): Gtk-CRITICAL **: 22:45:52.234: gtk_widget_get_clipboard:
;; assertion 'gtk_widget_has_screen (widget)' failed

;; (sbcl:7881): Gtk-CRITICAL **: 22:45:52.234: gtk_clipboard_set_text:
;; assertion 'clipboard != NULL' failed

#+nil
(test gtk-editable-cut/copy/paste-clipboard
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    (is-false (gtk:editable-select-region entry :start 8 :end 12))
    (is-false (gtk:editable-cut-clipboard entry))
    (is-false (gtk:editable-chars entry))))

;;;     gtk_editable_delete_selection

(test gtk-editable-select-region/selection-bounds
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    ;; No text selected
    (is-false (gtk:editable-delete-selection entry))
    (is (string= "This is some text." (gtk:editable-chars entry)))
    ;; Select text and delete the selection
    (is-false (gtk:editable-select-region entry :start 8 :end 12))
    (is-false (gtk:editable-delete-selection entry))
    (is (string= "This is  text." (gtk:editable-chars entry)))))

;;;     gtk_editable_set_position
;;;     gtk_editable_get_position

(test gtk-editable-position
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    (is (= 0 (gtk:editable-position entry)))
    (is (= 8 (setf (gtk:editable-position entry) 8)))
    (is (= 8 (gtk:editable-position entry)))))

;;;     gtk_editable_set_editable
;;;     gtk_editable_get_editable

(test gtk-editable-editable
  (let* ((buffer (gtk:entry-buffer-new "This is some text."))
         (entry (gtk:entry-new-with-buffer buffer)))
    (is-true (gtk:editable-editable entry))
    (is-false (setf (gtk:editable-editable entry) nil))
    (is-false (gtk:editable-editable entry))))

;;; 2024-9-22
