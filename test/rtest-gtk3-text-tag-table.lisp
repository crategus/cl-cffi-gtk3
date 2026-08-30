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

;;; --- Signals ----------------------------------------------------------------

;;;     tag-added

(test gtk-text-tag-table-tag-added-signal
  (let* ((name "tag-added")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     tag-changed

(test gtk-text-tag-table-tag-changed-signal
  (let* ((name "tag-changed")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag" "gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     tag-removed

(test gtk-text-tag-table-tag-removed-signal
  (let* ((name "tag-removed")
         (gtype (g:gtype "GtkTextTagTable"))
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
    (is (equal '("GtkTextTag")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_tag_table_new

(test gtk-text-tag-table-new
  (glib-test:with-check-memory (table)
    (is (typep (setf table (gtk:text-tag-table-new)) 'gtk:text-tag-table))
    (is (= 0 (gtk:text-tag-table-size (gtk:text-tag-table-new))))))

;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_get_size

(test gtk-text-tag-table-add
  (glib-test:with-check-memory (table)
    (is (typep (setf table (gtk:text-tag-table-new)) 'gtk:text-tag-table))
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
      (is (= 4 (gtk:text-tag-table-size table)))))
    ;; Remove references
    (is-false (gtk:text-tag-table-remove-all table))))

;;;     gtk_text_tag_table_foreach

(test gtk-text-tag-table-foreach
  (glib-test:with-check-memory (table)
    (is (typep (setf table (gtk:text-tag-table-new)) 'gtk:text-tag-table))
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
                      (format t "  name : ~a~%" (gtk:text-tag-name tag))))))
    ;; Remove references
    (is-false (gtk:text-tag-table-remove-all table))))

;;; 2026-07-04
