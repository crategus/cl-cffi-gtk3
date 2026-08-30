(in-package :gtk-test)

(def-suite gtk-text-mark :in gtk-suite)
(in-suite gtk-text-mark)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextMark

(test gtk-text-mark-class
  ;; Check type
  (is (g:type-is-object "GtkTextMark"))
  ;; Check registered name
  (is (eq 'gtk:text-mark
          (glib:symbol-for-gtype "GtkTextMark")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextMark")
          (g:gtype (cffi:foreign-funcall "gtk_text_mark_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkTextMark")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextMark")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTextMark")))
  ;; Check class properties
  (is (equal '("left-gravity" "name")
             (glib-test:list-properties "GtkTextMark")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTextMark")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextMark" GTK:TEXT-MARK
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_text_mark_get_type")
                      ((LEFT-GRAVITY TEXT-MARK-LEFT-GRAVITY
                        "left-gravity" "gboolean" T NIL)
                       (NAME TEXT-MARK-NAME "name" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkTextMark"))))

;;; --- Properties -------------------------------------------------------------

;;;     left-gravity
;;;     name

(test gtk-text-mark-properties
  (glib-test:with-check-memory (mark)
    (is (typep (setf mark (make-instance 'gtk:text-mark)) 'gtk:text-mark))
    (is-false (gtk:text-mark-left-gravity mark))
    (is-false (gtk:text-mark-name mark))
    (is (eq 'gtk:text-mark
            (type-of (setq mark (make-instance 'gtk:text-mark
                                               :name "Name"
                                               :left-gravity t)))))
    (is-true (gtk:text-mark-left-gravity mark))
    (is (string= "Name" (gtk:text-mark-name mark)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_mark_new

(test gtk-text-mark-new
  (glib-test:with-check-memory (mark)
    (is (typep (setf mark (gtk:text-mark-new nil nil)) 'gtk:text-mark))
    (is-false (gtk:text-mark-left-gravity mark))
    (is-false (gtk:text-mark-name mark))
    (is (typep (setq mark (gtk:text-mark-new "name" t)) 'gtk:text-mark))
    (is-true (gtk:text-mark-left-gravity mark))
    (is (string= "name" (gtk:text-mark-name mark)))))

;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible

(test gtk-text-mark-visible
  (glib-test:with-check-memory (mark)
    (is (typep (setf mark (make-instance 'gtk:text-mark)) 'gtk:text-mark))
    (is-false (gtk:text-mark-visible mark))
    (is-false (setf (gtk:text-mark-visible mark) nil))
    (is-false (gtk:text-mark-visible mark))))

;;;     gtk_text_mark_get_deleted

(test gtk-text-mark-deleted
  (glib-test:with-check-memory (buffer mark)
    (let (iter)
      (is (typep (setf buffer
                       (make-instance 'gtk:text-buffer :text "Some sample text"))
                 'gtk:text-buffer))
      (is (typep (setf mark (gtk:text-mark-new "Name" t)) 'gtk:text-mark))
      (is (typep (setf iter (gtk:text-buffer-start-iter buffer)) 'gtk:text-iter))
      (is-false (gtk:text-buffer-add-mark buffer mark iter))
      (is-false (gtk:text-mark-deleted mark))
      (is-false (gtk:text-buffer-delete-mark buffer mark))
      (is-true (gtk:text-mark-deleted mark)))))

;;;     gtk_text_mark_get_buffer

(test gtk-text-mark-buffer
  (glib-test:with-check-memory (buffer mark)
    (let (iter)
      (is (typep (setf buffer
                       (make-instance 'gtk:text-buffer :text "Some sample text"))
                 'gtk:text-buffer))
      (is (typep (setf mark (gtk:text-mark-new "Name" t)) 'gtk:text-mark))
      (is (typep (setf iter (gtk:text-buffer-start-iter buffer)) 'gtk:text-iter))
      (is-false (gtk:text-buffer-add-mark buffer mark iter))
      (is (typep (gtk:text-mark-buffer mark) 'gtk:text-buffer))
      (is-false (gtk:text-buffer-delete-mark buffer mark)))))

;;; 2026-06-29
