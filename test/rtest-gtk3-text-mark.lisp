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

;;;     gboolean   left-gravity    Read / Write / Construct Only
;;;        gchar*  name            Read / Write / Construct Only

(test gtk-text-mark-properties
  (let ((mark (make-instance 'gtk:text-mark)))
    (is (eq 'gtk:text-mark (type-of mark)))
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
  (let ((mark (gtk:text-mark-new nil nil)))
    (is (eq 'gtk:text-mark (type-of mark)))
    (is-false (gtk:text-mark-left-gravity mark))
    (is-false (gtk:text-mark-name mark))
    (is (eq 'gtk:text-mark
            (type-of (setq mark (gtk:text-mark-new "name" t)))))
    (is-true (gtk:text-mark-left-gravity mark))
    (is (string= "name" (gtk:text-mark-name mark)))))

;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible

(test gtk-text-mark-visible
  (let ((mark (make-instance 'gtk:text-mark)))
    (is-false (gtk:text-mark-visible mark))
    (is-false (setf (gtk:text-mark-visible mark) nil))
    (is-false (gtk:text-mark-visible mark))))

;;;     gtk_text_mark_get_deleted

(test gtk-text-mark-deleted
  (let* ((buffer (make-instance 'gtk:text-buffer :text "Some sample text"))
         (mark (gtk:text-mark-new "Name" t))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is-false (gtk:text-buffer-add-mark buffer mark iter))
    (is-false (gtk:text-mark-deleted mark))
    (is-false (gtk:text-buffer-delete-mark buffer mark))
    (is-true (gtk:text-mark-deleted mark))))

;;;     gtk_text_mark_get_buffer

(test gtk-text-mark-buffer
  (let* ((buffer (make-instance 'gtk:text-buffer :text "Some sample text"))
         (mark (gtk:text-mark-new "Name" t))
         (iter (gtk:text-buffer-start-iter buffer)))
    (is-false (gtk:text-buffer-add-mark buffer mark iter))
    (is (eq 'gtk:text-buffer (type-of (gtk:text-mark-buffer mark))))))

;;; 2024-9-22
