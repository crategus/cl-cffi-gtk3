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
             (glib-test:list-interface-properties "GtkEditable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkEditable" GTK:EDITABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_editable_get_type"))
             (gobject:get-gtype-definition "GtkEditable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-editable-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkEditable"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     delete-text

(test gtk-editable-delete-text-signal
  (let* ((name "delete-text")
         (gtype (g:gtype "GtkEditable"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     insert-text

(test gtk-editable-insert-text-signal
  (let* ((name "insert-text")
         (gtype (g:gtype "GtkEditable"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gchararray" "gint" "gpointer")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_editable_select_region
;;;     gtk_editable_get_selection_bounds

(test gtk-editable-select-region/selection-bounds
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
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
               (multiple-value-list (gtk:editable-selection-bounds entry))))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;;     gtk_editable_insert_text
;;;     gtk_editable_delete_text

(test gtk-editable-insert/delete-text
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    ;; Delete text
    (is-false (gtk:editable-delete-text entry :start 8 :end 12))
    (is (string= "This is  text." (gtk:editable-chars entry)))
    ;; Insert text
    (is (= 13 (gtk:editable-insert-text entry "extra" 8)))
    (is (string= "This is extra text." (gtk:editable-chars entry)))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;;     gtk_editable_get_chars

(test gtk-editable-chars
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    (is (string= "This is some text." (gtk:editable-chars entry)))
    (is (string= "some text." (gtk:editable-chars entry :start 8)))
    (is (string= "some" (gtk:editable-chars entry :start 8 :end 12)))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;;     gtk_editable_cut_clipboard
;;;     gtk_editable_paste_clipboard

(test gtk-editable-cut/copy-clipboard
  (glib-test:with-check-memory (window buffer entry :strong 2)
    (let (clipboard)
      (is (typep (setf window (gtk:window-new :toplevel)) 'gtk:window))
      (is (typep (setf clipboard
                       (gtk:clipboard-default (gtk:widget-display window)))
                 'gtk:clipboard))
      (is (typep (setf buffer
                       (gtk:entry-buffer-new "This is some text."))
                 'gtk:entry-buffer))
      (is (typep (setf entry
                       (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
      ;; Put ENTRY in the toplevel window to access a clipboard
      (is-false (gtk:container-add window entry))
      ;; Cut into clipboard
      (is-false (gtk:editable-select-region entry :start 8 :end 12))
      (is-false (gtk:editable-cut-clipboard entry))
      (is (string= "some" (gtk:clipboard-wait-for-text clipboard)))
      (is (string= "This is  text." (gtk:editable-chars entry)))
      ;; Paste from clipboard
      (is-false (gtk:editable-paste-clipboard entry))
      (is (string= "some" (gtk:clipboard-wait-for-text clipboard)))
      (is (string= "This is some text." (gtk:editable-chars entry)))
      ;; Remove references
      (is-false (setf (gtk:entry-buffer entry) nil))
      (is-false (gtk:widget-destroy window)))))

;;;     gtk_editable_copy_clipboard

(test gtk-editable-copy-clipboard
  (glib-test:with-check-memory (window buffer entry :strong 2)
    (let (clipboard)
      (is (typep (setf window (gtk:window-new :toplevel)) 'gtk:window))
      (is (typep (setf clipboard
                       (gtk:clipboard-default (gtk:widget-display window)))
                 'gtk:clipboard))
      (is (typep (setf buffer
                       (gtk:entry-buffer-new "This is some text."))
                 'gtk:entry-buffer))
      (is (typep (setf entry
                       (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
      ;; Put ENTRY in the toplevel window to access a clipboard
      (is-false (gtk:container-add window entry))
      ;; Copy into clipboard
      (is-false (gtk:editable-select-region entry :start 8 :end 12))
      (is-false (gtk:editable-copy-clipboard entry))
      (is (string= "some" (gtk:clipboard-wait-for-text clipboard)))
      (is (string= "This is some text." (gtk:editable-chars entry)))
      ;; Remove references
      (is-false (setf (gtk:entry-buffer entry) nil))
      (is-false (gtk:widget-destroy window)))))

;;;     gtk_editable_delete_selection

(test gtk-editable-delete-selection
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    ;; No text selected
    (is-false (gtk:editable-delete-selection entry))
    (is (string= "This is some text." (gtk:editable-chars entry)))
    ;; Select text and delete the selection
    (is-false (gtk:editable-select-region entry :start 8 :end 12))
    (is-false (gtk:editable-delete-selection entry))
    (is (string= "This is  text." (gtk:editable-chars entry)))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;;     gtk_editable_set_position
;;;     gtk_editable_get_position

(test gtk-editable-position
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    (is (= 0 (gtk:editable-position entry)))
    (is (= 8 (setf (gtk:editable-position entry) 8)))
    (is (= 8 (gtk:editable-position entry)))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;;     gtk_editable_set_editable
;;;     gtk_editable_get_editable

(test gtk-editable-editable
  (glib-test:with-check-memory (buffer entry)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is some text."))
               'gtk:entry-buffer))
    (is (typep (setf entry
                     (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    (is-false (setf (gtk:editable-editable entry) nil))
    (is-false (gtk:editable-editable entry))
    ;; Remove references
    (is-false (setf (gtk:entry-buffer entry) nil))))

;;; 2025-07-07
