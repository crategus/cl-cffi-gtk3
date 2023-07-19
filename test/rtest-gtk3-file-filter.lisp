(in-package :gtk-test)

(def-suite gtk-file-filter :in gtk-suite)
(in-suite gtk-file-filter)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileFilterFlags

(test gtk-file-filter-flags
  ;; Check the type
  (is (g:type-is-flags "GtkFileFilterFlags"))
  ;; Check the registered name
  (is (eq 'gtk:file-filter-flags
          (glib:symbol-for-gtype "GtkFileFilterFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileFilterFlags")
          (g:gtype (cffi:foreign-funcall "gtk_file_filter_flags_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_FILE_FILTER_FILENAME" "GTK_FILE_FILTER_URI"
               "GTK_FILE_FILTER_DISPLAY_NAME" "GTK_FILE_FILTER_MIME_TYPE")
             (list-flags-item-name "GtkFileFilterFlags")))
  ;; Check the values
  (is (equal '(1 2 4 8)
             (list-flags-item-value "GtkFileFilterFlags")))
  ;; Check the nick names
  (is (equal '("filename" "uri" "display-name" "mime-type")
             (list-flags-item-nick "GtkFileFilterFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkFileFilterFlags"
                              GTK-FILE-FILTER-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER
                               "gtk_file_filter_flags_get_type")
                              (:FILENAME 1)
                              (:URI 2)
                              (:DISPLAY-NAME 4)
                              (:MIME-TYPE 8))
             (gobject:get-g-type-definition "GtkFileFilterFlags"))))

;;;     GtkFileFilterInfo

;;;     GtkFileFilter

(test gtk-file-filter-class
  ;; Type check
  (is (g:type-is-object "GtkFileFilter"))
  ;; Check the registered name
  (is (eq 'gtk:file-filter
          (glib:symbol-for-gtype "GtkFileFilter")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileFilter")
          (g:gtype (cffi:foreign-funcall "gtk_file_filter_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkFileFilter")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileFilter")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (list-interfaces "GtkFileFilter")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkFileFilter")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileFilter")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileFilter" GTK-FILE-FILTER
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_file_filter_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFileFilter"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_filter_new

(test gtk-file-filter-new
  (is (typep (gtk:file-filter-new) 'gtk:file-filter)))

;;;     gtk_file_filter_set_name
;;;     gtk_file_filter_get_name

(test gtk-file-filter-name
  (let ((filter (gtk:file-filter-new)))
    (is (string= "filter" (setf (gtk:file-filter-name filter) "filter")))
    (is (string= "filter" (gtk:file-filter-name filter)))))

;;;     gtk_file_filter_add_mime_type

;; FIXME: We get a critical error with the following test:
;;   (sbcl:17339): GLib-CRITICAL **: 20:08:20.201:
;;   g_variant_new_string: assertion 'string != NULL' failed

(test gtk-file-filter-add-mime-type
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (string= "('[Invalid UTF-8]', [(1, 'text/plain')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))))

;;;     gtk_file_filter_add_pattern

(test gtk-file-filter-add-pattern
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-pattern filter "doc/*"))
    (is (string= "('[Invalid UTF-8]', [(0, 'doc/*')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))))

;;;     gtk_file_filter_add_pixbuf_formats

#-windows
(test gtk-file-filter-add-pixbuf-formats
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))
    (is (string=
"('[Invalid UTF-8]', [(1, 'image/png'), (1, 'image/jpeg'), (1, 'image/x-wmf'), (1, 'application/x-navi-animation'), (1, 'image/bmp'), (1, 'image/x-bmp'), (1, 'image/x-MS-bmp'), (1, 'image/gif'), (1, 'image/x-icns'), (1, 'image/x-icon'), (1, 'image/x-ico'), (1, 'image/x-win-bitmap'), (1, 'image/vnd.microsoft.icon'), (1, 'application/ico'), (1, 'image/ico'), (1, 'image/icon'), (1, 'text/ico'), (1, 'image/x-portable-anymap'), (1, 'image/x-portable-bitmap'), (1, 'image/x-portable-graymap'), (1, 'image/x-portable-pixmap'), (1, 'image/x-quicktime'), (1, 'image/qtif'), (1, 'image/svg+xml'), (1, 'image/svg'), (1, 'image/svg-xml'), (1, 'image/vnd.adobe.svg+xml'), (1, 'text/xml-svg'), (1, 'image/svg+xml-compressed'), (1, 'image/x-tga'), (1, 'image/tiff'), (1, 'image/webp'), (1, 'audio/x-riff'), (1, 'image/x-xbitmap'), (1, 'image/x-xpixmap')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))))

#+windows
(test gtk-file-filter-add-pixbuf-formats
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-add-pixbuf-formats filter))
    (is (string=
"('[Invalid UTF-8]', [(1, 'image/wmf'), (1, 'image/x-wmf'), (1, 'application/emf'), (1, 'application/x-emf'), (1, 'image/emf'), (1, 'image/x-emf'), (1, 'image/x-mgx-emf'), (1, 'application/x-navi-animation'), (1, 'image/bmp'), (1, 'image/x-bmp'), (1, 'image/x-MS-bmp'), (1, 'image/gif'), (1, 'image/x-icns'), (1, 'image/x-icon'), (1, 'image/x-ico'), (1, 'image/x-win-bitmap'), (1, 'image/vnd.microsoft.icon'), (1, 'application/ico'), (1, 'image/ico'), (1, 'image/icon'), (1, 'text/ico'), (1, 'image/jpeg'), (1, 'image/png'), (1, 'image/x-portable-anymap'), (1, 'image/x-portable-bitmap'), (1, 'image/x-portable-graymap'), (1, 'image/x-portable-pixmap'), (1, 'image/x-quicktime'), (1, 'image/qtif'), (1, 'image/svg+xml'), (1, 'image/svg'), (1, 'image/svg-xml'), (1, 'image/vnd.adobe.svg+xml'), (1, 'text/xml-svg'), (1, 'image/svg+xml-compressed'), (1, 'image/x-tga'), (1, 'image/tiff'), (1, 'image/x-xbitmap'), (1, 'image/x-xpixmap')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))))

;;;     gtk_file_filter_add_custom

;;;     gtk_file_filter_get_needed

(test gtk-file-filter-needed
  (let ((filter (gtk:file-filter-new)))
    (is-false (gtk:file-filter-needed filter))
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (equal '(:mime-type) (gtk:file-filter-needed filter)))
    (is-false (gtk:file-filter-add-pattern filter "doc/*"))
    (is (equal '(:display-name :mime-type) (gtk:file-filter-needed filter)))))

;;;     gtk_file_filter_filter

;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant

(test gtk-file-filter-to-gvariant/from-gvariant
  (let* ((filter (gtk:file-filter-new))
         (variant (gtk:file-filter-to-gvariant filter)))
    (is (string= "('[Invalid UTF-8]', [])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))
    ;; Create a filter from a gvariant
    (is (typep (setf filter (gtk:file-filter-new-from-gvariant variant))
               'gtk:file-filter))
    (is (string= "('[Invalid UTF-8]', [])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))
    ;; Add a MIMI type
    (is-false (gtk:file-filter-add-mime-type filter "text/plain"))
    (is (string= "('[Invalid UTF-8]', [(1, 'text/plain')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))
    (is (cffi:pointerp (setf variant (gtk:file-filter-to-gvariant filter))))
    ;; Create a filter from a gvariant
    (is (typep (setf filter (gtk:file-filter-new-from-gvariant variant))
               'gtk:file-filter))
    (is (string= "('[Invalid UTF-8]', [(1, 'text/plain')])"
                 (g:variant-print (gtk:file-filter-to-gvariant filter))))))

;;; --- 2023-6-11 --------------------------------------------------------------
