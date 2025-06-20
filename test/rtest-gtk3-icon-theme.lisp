(in-package :gtk-test)

(def-suite gtk-icon-theme :in gtk-suite)
(in-suite gtk-icon-theme)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconLookupFlags

(test gtk-icon-loopup-flags
  ;; Check type
  (is (g:type-is-flags "GtkIconLookupFlags"))
  ;; Check registered name
  (is (eq 'gtk:icon-lookup-flags
          (glib:symbol-for-gtype "GtkIconLookupFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconLookupFlags")
          (g:gtype (cffi:foreign-funcall "gtk_icon_lookup_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_ICON_LOOKUP_NO_SVG" "GTK_ICON_LOOKUP_FORCE_SVG"
               "GTK_ICON_LOOKUP_USE_BUILTIN" "GTK_ICON_LOOKUP_GENERIC_FALLBACK"
               "GTK_ICON_LOOKUP_FORCE_SIZE" "GTK_ICON_LOOKUP_FORCE_REGULAR"
               "GTK_ICON_LOOKUP_FORCE_SYMBOLIC" "GTK_ICON_LOOKUP_DIR_LTR"
               "GTK_ICON_LOOKUP_DIR_RTL")
             (glib-test:list-flags-item-names "GtkIconLookupFlags")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256)
             (glib-test:list-flags-item-values "GtkIconLookupFlags")))
  ;; Check nick names
  (is (equal '("no-svg" "force-svg" "use-builtin" "generic-fallback"
               "force-size" "force-regular" "force-symbolic" "dir-ltr"
               "dir-rtl")
             (glib-test:list-flags-item-nicks "GtkIconLookupFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkIconLookupFlags" GTK:ICON-LOOKUP-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_icon_lookup_flags_get_type")
                                     (:NO-SVG 1)
                                     (:FORCE-SVG 2)
                                     (:USE-BUILTIN 4)
                                     (:GENERIC-FALLBACK 8)
                                     (:FORCE-SIZE 16)
                                     (:FORCE-REGULAR 32)
                                     (:FORCE-SYMBOLIC 64)
                                     (:DIR-LTR 128)
                                     (:DIR-RTL 256))
             (gobject:get-gtype-definition "GtkIconLookupFlags"))))

;;;     GtkIconInfo

(test gtk-icon-info-class
  ;; Check type
  (is (g:type-is-object "GtkIconInfo"))
  ;; Check registered name
  (is (eq 'gtk:icon-info
          (glib:symbol-for-gtype "GtkIconInfo")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconInfo")
          (g:gtype (cffi:foreign-funcall "gtk_icon_info_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconInfo")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconInfo")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkIconInfo")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkIconInfo")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkIconInfo")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconInfo" GTK:ICON-INFO
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_icon_info_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkIconInfo"))))

;;;     GtkIconTheme

(test gtk-icon-theme-class
  ;; Check type
  (is (g:type-is-object "GtkIconTheme"))
  ;; Check registered name
  (is (eq 'gtk:icon-theme
          (glib:symbol-for-gtype "GtkIconTheme")))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconTheme")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconTheme")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkIconTheme")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkIconTheme")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkIconTheme")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconTheme" GTK:ICON-THEME
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_icon_theme_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkIconTheme"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_theme_new

(test gtk-icon-theme-new
  (glib-test:with-check-memory (theme)
    (is (typep (setf theme (gtk:icon-theme-new)) 'gtk:icon-theme))))

;;;     gtk_icon_theme_get_default

(test gtk-icon-theme-default
  (glib-test:with-check-memory (:strong 1)
    (let (theme)
      (is (typep (setf theme (gtk:icon-theme-default)) 'gtk:icon-theme)))))

;;;     gtk_icon_theme_get_for_screen
;;;     gtk_icon_theme_set_screen

(test gtk-icon-theme-screen
  (glib-test:with-check-memory (:strong 1)
    (let ((screen (gdk:screen-default))
          (theme (gtk:icon-theme-default)))
      (is (typep (gtk:icon-theme-for-screen screen) 'gtk:icon-theme))
      (is-false (gtk:icon-theme-set-screen theme screen))
      (is (typep (gtk:icon-theme-for-screen screen) 'gtk:icon-theme)))))

;;;     gtk_icon_theme_set_search_path
;;;     gtk_icon_theme_get_search_path
;;;     gtk_icon_theme_append_search_path
;;;     gtk_icon_theme_prepend_search_path

(test gtk-icon-theme-search-path
  (glib-test:with-check-memory (theme)
    (is (typep (setf theme (gtk:icon-theme-new)) 'gtk:icon-theme))
    (is (equal '("path1" "path2")
               (setf (gtk:icon-theme-search-path theme) '("path1" "path2"))))
    (is (equal '("path1" "path2") (gtk:icon-theme-search-path theme)))
    (is-false (gtk:icon-theme-append-search-path theme "path3"))
    (is (equal '("path1" "path2" "path3") (gtk:icon-theme-search-path theme)))
    (is-false (gtk:icon-theme-prepend-search-path theme "path0"))
    (is (equal '("path0" "path1" "path2" "path3")
               (gtk:icon-theme-search-path theme)))))

;;;     gtk_icon_theme_add_resource_path

(test gtk-icon-theme-add-resource-path
  (let ((theme (gtk:icon-theme-default)))
    (is-false (gtk:icon-theme-add-resource-path theme "path"))))

;;;     gtk_icon_theme_set_custom_theme

(test gtk-icon-theme-set-custom-theme
  (glib-test:with-check-memory (theme)
    (is (typep (setf theme (gtk:icon-theme-new)) 'gtk:icon-theme))
    (is-false (gtk:icon-theme-set-custom-theme theme "my-theme"))))

;;;     gtk_icon_theme_has_icon

(test gtk-icon-theme-has-icon
  (let* ((theme (gtk:icon-theme-default))
         (iconname (gtk:icon-theme-example-icon-name theme)))
    (is-true (gtk:icon-theme-has-icon theme iconname))
    (is-false (gtk:icon-theme-has-icon theme "xxxx"))))

;;;     gtk_icon_theme_lookup_icon

(test gtk-icon-theme-lookup-icon
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (iconname (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme iconname 48 :use-builtin))
                 'gtk:icon-info)))))

;;;     gtk_icon_theme_lookup_icon_for_scale

(test gtk-icon-theme-lookup-icon-for-scale
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon-for-scale theme name 48 1
                                                                   :use-builtin))
                 'gtk:icon-info)))))

;;;     gtk_icon_theme_choose_icon

(test gtk-icon-theme-choose-icon
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-choose-icon theme (list name) 48
                                                   :use-builtin))
                 'gtk:icon-info)))))

;;;     gtk_icon_theme_choose_icon_for_scale

(test gtk-icon-theme-choose-icon-for-scale
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-choose-icon-for-scale theme
                                                             (list name)
                                                             48 1
                                                             :use-builtin))
                 'gtk:icon-info)))))

;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_lookup_by_gicon_for_scale

;;;     gtk_icon_theme_load_icon

(test gtk-icon-theme-load-icon
  (glib-test:with-check-memory (pixbuf :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf pixbuf
                       (gtk:icon-theme-load-icon theme name 48 :use-builtin))
                 'gdk:pixbuf)))))

;;;     gtk_icon_theme_load_icon_for_scale

(test gtk-icon-theme-load-icon-for-scale
  (glib-test:with-check-memory (pixbuf :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf pixbuf
                       (gtk:icon-theme-load-icon-for-scale theme name 48 1
                                                           :use-builtin))
                 'gdk:pixbuf)))))

;;;     gtk_icon_theme_load_surface

(test gtk-icon-theme-load-surface
  (let* ((theme (gtk:icon-theme-default))
         (name (gtk:icon-theme-example-icon-name theme))
         surface)
    (is (cffi:pointerp (setf surface
                             (gtk:icon-theme-load-surface theme name
                                                                48 1
                                                                nil
                                                                :use-builtin))))
    (is (eq :success (cairo:surface-status surface)))
    (is-false (cairo:surface-destroy surface))))

;;;     gtk_icon_theme_list_contexts

#-windows
(test gtk-icon-theme-list-contexts
  (let ((theme (gtk:icon-theme-default)))
    (is (equal '("Actions" "Animations" "Applications" "Camera" "Categories" "Devices"
 "Emblems" "Emotes" "Generic-Symbols" "Legacy" "MimeTypes" "Multimedia"
 "Org.Gnome.Nautilus" "Org.Gnome.Settings" "Phosh" "Places" "Status" "Time"
 "UI")
               (sort (gtk:icon-theme-list-contexts theme) #'string<)))))

#+windows
(test gtk-icon-theme-list-contexts
  (let ((theme (gtk:icon-theme-default)))
    (is (equal '("Actions" "Applications" "Categories" "Devices" "Emblems"
                 "Emotes" "Legacy" "MimeTypes" "Places" "Status" "UI")
               (sort (gtk:icon-theme-list-contexts theme) #'string<)))))

;;;     gtk_icon_theme_list_icons

(test gtk-icon-theme-list-icons
  (let* ((theme (gtk:icon-theme-default))
         (name (gtk:icon-theme-example-icon-name theme)))
    (is-true (member name
                     (gtk:icon-theme-list-icons theme "Actions") :test 'string=))))

;;;     gtk_icon_theme_get_icon_sizes

(test gtk-icon-theme-icon-sizes
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-icon-sizes theme "gtk-ok"))
    (is-true (gtk:icon-theme-icon-sizes theme "battery"))
    (is-true (gtk:icon-theme-icon-sizes theme "add"))
    (is-true (gtk:icon-theme-icon-sizes theme "directory-x-normal"))))

;;;     gtk_icon_theme_get_example_icon_name

(test gtk-icon-theme-example-icon-name
  (let ((theme (gtk:icon-theme-default)))
    (is (string= "folder"
                 (gtk:icon-theme-example-icon-name theme)))))

;;;     gtk_icon_theme_rescan_if_needed

(test gtk-icon-theme-resan-if-needed
  (let ((theme (gtk:icon-theme-default)))
    (is-false (gtk:icon-theme-rescan-if-needed theme))))

;;;     gtk_icon_theme_add_builtin_icon

;;;     gtk_icon_info_copy
;;;     gtk_icon_info_free

;;;     gtk_icon_info_new_for_pixbuf

(test gtk-icon-info-new-for-pixbuf
  (glib-test:with-check-memory (info :strong 2)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme))
           (pixbuf (gtk:icon-theme-load-icon theme name 0 0)))
      (is (typep (setf info
                       (gtk:icon-info-new-for-pixbuf theme pixbuf))
                 'gtk:icon-info)))))

;;;     gtk_icon_info_get_base_size

#-windows
(test gtk-icon-info-base-size
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (= 16 (gtk:icon-info-base-size info))))))

#+windows
(test gtk-icon-info-base-size
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (= 128 (gtk:icon-info-base-size info))))))

;;;     gtk_icon_info_get_base_scale

(test gtk-icon-info-base-scale
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (= 1 (gtk:icon-info-base-scale info))))))

;;;     gtk_icon_info_get_filename

(test gtk-icon-info-filename
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (stringp (gtk:icon-info-filename info))))))

;;;     gtk_icon_info_get_builtin_pixbuf

;; This test adds two strong references. We skip it.

(test gtk-icon-info-builtin-pixbuf
  (glib-test:with-check-memory ((info 2) :strong 2)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 :use-builtin))
                 'gtk:icon-info))
      (is-false (gtk:icon-info-builtin-pixbuf info)))))

;;;     gtk_icon_info_load_icon

(test gtk-icon-info-load-icon
  (glib-test:with-check-memory ((info 2) (pixbuf 2) :strong 2)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (typep (setf pixbuf
                       (gtk:icon-info-load-icon info)) 'gdk:pixbuf)))))

;;;     gtk_icon_info_load_surface

(test gtk-icon-info-load-surface
  (glib-test:with-check-memory ((info 2) :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme))
           surface)
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (cffi:pointerp (setf surface
                               (gtk:icon-info-load-surface info nil))))
      (is (eq :success (cairo:surface-status surface)))
      (is-false (cairo:surface-destroy surface)))))

;;;     gtk_icon_info_load_icon_async
;;;     gtk_icon_info_load_icon_finish

;;;     gtk_icon_info_load_symbolic

(test gtk-icon-info-load-symbolic
  (glib-test:with-check-memory ((info 2) :strong 2)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme))
           pixbuf)
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme name 0 0))
                 'gtk:icon-info))
      (is (typep (setf pixbuf
                       (gtk:icon-info-load-symbolic info
                                                    (gdk:rgba-new) nil nil nil))
                 'gdk:pixbuf)))))

;;;     gtk_icon_info_load_symbolic_async
;;;     gtk_icon-info_load_symbolic_finish

;;;     gtk_icon_info_load_symbolic_for_style
;;;     gtk_icon_info_load_symbolic_for_context

;;;     gtk_icon_info_load_symbolic_for_context_async
;;;     gtk_icon_info_load_symbolic_for_context_finish
;;;     gtk_icon_info_set_raw_coordinates
;;;     gtk_icon_info_get_embedded_rect
;;;     gtk_icon_info_get_attach_points
;;;     gtk_icon_info_get_display_name

;;;     gtk_icon_info_is_symbolic

(test gtk-icon-info-is-symbolic
  (glib-test:with-check-memory (info :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (name (gtk:icon-theme-example-icon-name theme)))
      (is (typep (setf info
                       (gtk:icon-theme-lookup-icon theme
                                                   name 0 :force-symbolic))
                 'gtk:icon-info))
      (is-true (gtk:icon-info-is-symbolic info)))))

;;; 2025-06-20
