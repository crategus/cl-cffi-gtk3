(in-package :gtk-test)

(def-suite gtk-text-tag-table :in gtk-suite)
(in-suite gtk-text-tag-table)

(defparameter *verbose-gtk-text-tag-table* nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextTagTable

(test gtk-text-tag-table-class
  ;; Check type
  (is (g:type-is-object "GtkTextTagTable"))
  ;; Check registered name
  (is (eq 'gtk:text-tag-table
          (glib:symbol-for-gtype "GtkTextTagTable")))
  ;; Check type initializer
  (is (eq (g:gtype"GtkTextTagTable")
          (g:gtype (cffi:foreign-funcall "gtk_text_tag_table_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkTextTagTable")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextTagTable")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkTextTagTable")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkTextTagTable")))
  ;; Check signals
  (is (equal '("tag-added" "tag-changed" "tag-removed")
             (glib-test:list-signals "GtkTextTagTable")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextTagTable" GTK:TEXT-TAG-TABLE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_text_tag_table_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkTextTagTable"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_tag_table_new

(test gtk-text-tag-table-new
  (is (typep (gtk:text-tag-table-new) 'gtk:text-tag-table))
  (is (= 0 (gtk:text-tag-table-size (gtk:text-tag-table-new)))))

;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_get_size

(test gtk-text-tag-table-add
  (let ((table (gtk:text-tag-table-new)))
    (is (= 0 (gtk:text-tag-table-size table)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "bold"
                                                       :weight 700)))
    (is (= 1 (gtk:text-tag-table-size table)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "blue-foreground"
                                                       :foreground "blue")))
    (is (= 2 (gtk:text-tag-table-size table)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "italic"
                                                       :style :italic)))
    (is (= 3 (gtk:text-tag-table-size table)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "font"
                                                       :font "fixed")))
    (is (= 4 (gtk:text-tag-table-size table)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "font-italic"
                                                       :font "fixed"
                                                       :style :italic)))
    (is (= 5 (gtk:text-tag-table-size table)))
    ;; Lookup and remove a tag from the tag table
    (let ((tag (gtk:text-tag-table-lookup table "bold")))
      (is (typep tag 'gtk:text-tag))
      (is-false (gtk:text-tag-table-remove table tag))
      (is-false (gtk:text-tag-table-lookup table "bold")
      (is (= 4 (gtk:text-tag-table-size table)))))))

;;;     gtk_text_tag_table_foreach

(test gtk-text-tag-table-foreach
  (let ((table (gtk:text-tag-table-new)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "bold"
                                                       :weight 700)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "blue-foreground"
                                                       :foreground "blue")))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "italic"
                                                       :style :italic)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "font"
                                                       :font "fixed")))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "font-italic"
                                                       :font "fixed"
                                                       :style :italic)))
    (is-false (gtk:text-tag-table-foreach table #'gtk:text-tag-priority))
    (when *verbose-gtk-text-tag-table* (format t "~%"))
    (is-false (gtk:text-tag-table-foreach table
                  (lambda (tag)
                    (when *verbose-gtk-text-tag-table*
                      (format t "  name : ~a~%" (gtk:text-tag-name tag))))))))

;;; --- Signals ----------------------------------------------------------------

;;;     void   tag-added      Run Last
;;;     void   tag-changed    Run Last
;;;     void   tag-removed    Run Last

#+nil
(test gtk-text-tag-table-signals
  (let* ((result nil)
         (table (gtk:text-tag-table-new))
         (added (g-signal-connect table "tag-added"
                                  (lambda (table tag)
                                    (setf result (cons "added" result))
                                    (is (typep table 'gtk:text-tag-table))
                                    (is (typep tag 'gtk:text-tag)))))
         (changed (g-signal-connect table "tag-changed"
                                    (lambda (table tag changed)
                                      (setf result (cons "changed" result))
                                      (is (typep table 'gtk:text-tag-table))
                                      (is (typep tag 'gtk:text-tag))
                                      (is (typep changed 'boolean)))))
         (removed (g-signal-connect table "tag-removed"
                                    (lambda (table tag)
                                      (setf result (cons "removed" result))
                                      (is (typep table 'gtk:text-tag-table))
                                      (is (typep tag 'gtk:text-tag))))))
    (is (every #'integerp (list added changed removed)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "bold" :weight 700)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "font" :font "fixed")))
    (is-false (gtk:text-tag-table-remove table
                                         (gtk:text-tag-table-lookup table
                                                                    "bold")))
    (setf (gtk:text-tag-font (gtk:text-tag-table-lookup table "font")) "italic")
    (is (equal '("changed" "removed" "added" "added") result))))

;;; 2024-9-22
