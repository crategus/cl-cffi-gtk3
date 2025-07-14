(in-package :gtk-test)

(def-suite gtk-entry-buffer :in gtk-suite)
(in-suite gtk-entry-buffer)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryBuffer

(test gtk-entry-buffer-class
  ;; Check type
  (is (g:type-is-object "GtkEntryBuffer"))
  ;; Check registered name
  (is (eq 'gtk:entry-buffer
          (glib:symbol-for-gtype "GtkEntryBuffer")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntryBuffer")
          (g:gtype (cffi:foreign-funcall "gtk_entry_buffer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkEntryBuffer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkEntryBuffer")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEntryBuffer")))
  ;; Check class properties
  (is (equal '("length" "max-length" "text")
             (glib-test:list-properties "GtkEntryBuffer")))
  ;; Check signals
  (is (equal '("deleted-text" "inserted-text")
             (glib-test:list-signals "GtkEntryBuffer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEntryBuffer" GTK:ENTRY-BUFFER
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_entry_buffer_get_type")
                      ((LENGTH ENTRY-BUFFER-LENGTH "length" "guint" T NIL)
                       (MAX-LENGTH ENTRY-BUFFER-MAX-LENGTH
                        "max-length" "gint" T T)
                       (TEXT ENTRY-BUFFER-TEXT "text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkEntryBuffer"))))

(test gtk-entry-buffer-properties
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (make-instance 'gtk:entry-buffer :text "text"))
               'gtk:entry-buffer))
    (is (= 4 (gtk:entry-buffer-length buffer)))
    (is (= 0 (gtk:entry-buffer-max-length buffer)))
    (is (equal "text" (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_new

(test gtk-entry-buffer-new.1
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new)) 'gtk:entry-buffer))
    (is (= 0 (gtk:entry-buffer-length buffer)))
    (is (= 0 (gtk:entry-buffer-max-length buffer)))
    (is (equal "" (gtk:entry-buffer-text buffer)))))

(test gtk-entry-buffer-new.2
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new nil)) 'gtk:entry-buffer))
    (is (= 0 (gtk:entry-buffer-length buffer)))
    (is (= 0 (gtk:entry-buffer-max-length buffer)))
    (is (equal "" (gtk:entry-buffer-text buffer)))))

(test gtk-entry-buffer-new.3
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "text")) 'gtk:entry-buffer))
    (is (= 4 (gtk:entry-buffer-length buffer)))
    (is (= 0 (gtk:entry-buffer-max-length buffer)))
    (is (equal "text" (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_get_text

(test gtk-entry-buffer-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "text")) 'gtk:entry-buffer))
    (is (equal "text" (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_set_text

(test gtk-entry-buffer-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "text")) 'gtk:entry-buffer))
    (is (equal "text" (gtk:entry-buffer-text buffer)))
    (setf (gtk:entry-buffer-text buffer) "new text")
    (is (equal "new text" (gtk:entry-buffer-text buffer)))
    (is (= 8 (gtk:entry-buffer-length buffer)))))

;;;   gtk_entry_buffer_get_bytes

(test gtk-entry-buffer-bytes
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "text")) 'gtk:entry-buffer))
    (is (equal "text" (gtk:entry-buffer-text buffer)))
    (is (= 4 (gtk:entry-buffer-bytes buffer)))
    (setf (gtk:entry-buffer-text buffer) "Äpfel")
    (is (equal "Äpfel" (gtk:entry-buffer-text buffer)))
    (is (= 6 (gtk:entry-buffer-bytes buffer)))))

;;;   gtk_entry_buffer_get_length

(test gtk-entry-buffer-length
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new "Äpfel")) 'gtk:entry-buffer))
    (is (= 5 (gtk:entry-buffer-length buffer)))))

;;;   gtk_entry_buffer_get_max_length

(test gtk-entry-buffer-max-length
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is a text.")) 'gtk:entry-buffer))
    (is (= 0 (gtk:entry-buffer-max-length buffer)))))

;;;   gtk_entry_buffer_set_max_length

(test gtk-entry-buffer-max-length
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "This is a text.")) 'gtk:entry-buffer))
    (setf (gtk:entry-buffer-max-length buffer) 9)
    (is (= 9 (gtk:entry-buffer-max-length buffer)))
    (is (equal "This is a" (gtk:entry-buffer-text buffer)))))

;;;  gtk_entry_buffer_insert_text

(test gtk-entry-buffer-insert-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer (gtk:entry-buffer-new)) 'gtk:entry-buffer))
    (is (= 6 (gtk:entry-buffer-insert-text buffer 0 "first ")))
    (is (equal "first " (gtk:entry-buffer-text buffer)))
    (is (= 5 (gtk:entry-buffer-insert-text buffer 6 "third")))
    (is (equal "first third" (gtk:entry-buffer-text buffer)))
    (is (= 7 (gtk:entry-buffer-insert-text buffer 6 "second ")))
    (is (equal "first second third" (gtk:entry-buffer-text buffer)))
    (setf (gtk:entry-buffer-max-length buffer) 27)
    (is (= 9 (gtk:entry-buffer-insert-text buffer 6 "and than a ")))
    (is (equal "first and than second third" (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_delete_text

(test gtk-entry-buffer-delete-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "first second third"))
               'gtk:entry-buffer))
    (is (= 7 (gtk:entry-buffer-delete-text buffer 6 7)))
    (is (equal "first third" (gtk:entry-buffer-text buffer)))
    (is (= 6 (gtk:entry-buffer-delete-text buffer 5 -1)))
    (is (equal "first" (gtk:entry-buffer-text buffer)))))

;;;   gtk_entry_buffer_emit_deleted_text

(test gtk-entry-buffer-emit-deleted-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "first second third"))
               'gtk:entry-buffer))
    (g:signal-connect buffer "deleted-text"
        (lambda (object pos nchars)
          (is (eq buffer object))
          (is (string= "first second third"
                       (gtk:entry-buffer-text object)))
          (is (= 6 pos))
          (is (= 7 nchars))
          nil))
    (is-false (gtk:entry-buffer-emit-deleted-text buffer 6 7))))

;;;   gtk_entry_buffer_emit_inserted_text

(test gtk-entry-buffer-emit-inserted-text
  (glib-test:with-check-memory (buffer)
    (is (typep (setf buffer
                     (gtk:entry-buffer-new "first second third"))
               'gtk:entry-buffer))
    (g:signal-connect buffer "inserted-text"
        (lambda (object pos text nchars)
          (is (eq buffer object))
          (is (= 6 pos))
          (is (equal "text" text))
          (is (= 7 nchars))
          nil))
    (gtk:entry-buffer-emit-inserted-text buffer 6 "text" 7)))

;;; 2025-07-08
