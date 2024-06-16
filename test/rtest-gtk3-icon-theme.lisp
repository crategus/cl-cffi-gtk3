(in-package :gtk-test)

(def-suite gtk-icon-theme :in gtk-suite)
(in-suite gtk-icon-theme)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconInfo
;;;     GtkIconLookupFlags
;;;     GtkIconThemeError
;;;     GTK_ICON_THEME_ERROR

;;;     GtkIconTheme

(test gtk-icon-theme-class
  ;; Type check
  (is (g:type-is-object "GtkIconTheme"))
  ;; Check the registered name
  (is (eq 'gtk:icon-theme
          (glib:symbol-for-gtype "GtkIconTheme")))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconTheme")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIconTheme")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkIconTheme")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkIconTheme")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkIconTheme")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIconTheme" GTK-ICON-THEME
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_icon_theme_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkIconTheme"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_theme_new

(test gtk-icon-theme-new
  (is (typep (gtk:icon-theme-new) 'gtk:icon-theme)))

;;;     gtk_icon_theme_get_default

(test gtk-icon-theme-default
  (is (typep (gtk:icon-theme-default) 'gtk:icon-theme)))

;;;     gtk_icon_theme_get_for_screen
;;;     gtk_icon_theme_set_screen

(test gtk-icon-theme-screen
  (let ((screen (gdk:screen-default))
        (theme (gtk:icon-theme-default)))
    (is (typep (gtk:icon-theme-for-screen screen) 'gtk:icon-theme))
    (is-false (gtk:icon-theme-set-screen theme screen))
    (is (typep (gtk:icon-theme-for-screen screen) 'gtk:icon-theme))))

;;;     gtk_icon_theme_set_search_path
;;;     gtk_icon_theme_get_search_path
;;;     gtk_icon_theme_append_search_path
;;;     gtk_icon_theme_prepend_search_path

(test gtk-icon-theme-search-path
  (let ((theme (gtk:icon-theme-new)))
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
  (let ((theme (gtk:icon-theme-new)))
    (is-false (gtk:icon-theme-set-custom-theme theme "my-theme"))))

;;;     gtk_icon_theme_has_icon

(test gtk-icon-theme-has-icon
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-has-icon theme "gtk-ok"))
    (is-false (gtk:icon-theme-has-icon theme "xxxx"))))

;;;     gtk_icon_theme_lookup_icon

(test gtk-icon-theme-lookup-icon
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-lookup-icon theme "gtk-ok" 48 :use-builtin))))

;;;     gtk_icon_theme_lookup_icon_for_scale

(test gtk-icon-theme-lookup-icon-for-scale
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-lookup-icon-for-scale theme "gtk-ok" 48 1 :use-builtin))))

;;;     gtk_icon_theme_choose_icon

(test gtk-icon-theme-choose-icon
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-choose-icon theme '("gtk-ok") 48 :use-builtin))))

;;;     gtk_icon_theme_choose_icon_for_scale

(test gtk-icon-theme-choose-icon
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-choose-icon-for-scale theme '("gtk-ok") 48 1 :use-builtin))))

;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_lookup_by_gicon_for_scale

;;;     gtk_icon_theme_load_icon

(test gtk-icon-theme-load-icon
  (let ((theme (gtk:icon-theme-default)))
    (is (typep (gtk:icon-theme-load-icon theme "gtk-ok" 48 :use-builtin)
               'gdk:pixbuf))))

;;;     gtk_icon_theme_load_icon_for_scale

(test gtk-icon-theme-load-icon-for-scale
  (let ((theme (gtk:icon-theme-default)))
    (is (typep (gtk:icon-theme-load-icon-for-scale theme "gtk-ok" 48 1 :use-builtin)
               'gdk:pixbuf))))

;;;     gtk_icon_theme_load_surface

(test gtk-icon-theme-load-surface
  (let ((theme (gtk:icon-theme-default)))
    (is-true (gtk:icon-theme-load-surface theme "gtk-ok" 48 1 nil :use-builtin))))

;;;     gtk_icon_theme_list_contexts

(test gtk-icon-theme-list-contexts
  (let ((theme (gtk:icon-theme-default)))
    #-windows
    (is (equal '("Actions" "Animations" "Applications" "Camera" "Categories" "Devices"
 "Emblems" "Emotes" "Generic-Symbols" "Legacy" "MimeTypes" "Multimedia"
 "Org.Gnome.Nautilus" "Org.Gnome.Settings" "Phosh" "Places" "Status" "Stock"
 "Time" "UI")
               (sort (gtk:icon-theme-list-contexts theme) #'string<)))
    #+windows
    (is (equal '("UI" "Emblems" "Actions" "Places" "Categories" "Legacy"
                 "Devices" "Emotes" "Status" "Applications" "MimeTypes")
               (gtk:icon-theme-list-contexts theme)))))

;;;     gtk_icon_theme_list_icons

(test gtk-icon-theme-list-icons
  (let ((theme (gtk:icon-theme-default)))
    (is-true (member "gtk-ok" (gtk:icon-theme-list-icons theme "Actions") :test 'string=))))

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
    #-windows
    (is (string= "folder"
                 (gtk:icon-theme-example-icon-name theme)))
    #+windows
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

#-windows
(test gtk-icon-info-new-for-pixbuf
  (let* ((theme (gtk:icon-theme-default))
         (pixbuf (gtk:icon-theme-load-icon theme "battery" 0 0)))
      (is-true (cffi:pointerp (gtk:icon-info-new-for-pixbuf theme pixbuf)))))

;;;     gtk_icon_info_get_base_size

#-windows
(test gtk-icon-info-base-size
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 0)))
    #-windows
    (is (= 24 (gtk:icon-info-base-size icon-info)))
    #+windows
    (is (= 16 (gtk:icon-info-base-size icon-info)))))

;;;     gtk_icon_info_get_base_scale

#-windows
(test gtk-icon-info-base-scale
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 0)))
    (is (= 1 (gtk:icon-info-base-scale icon-info)))))

;;;     gtk_icon_info_get_filename

#-windows
(test gtk-icon-info-filename
  (let ((theme (gtk:icon-theme-default)))
    (is (stringp
            (gtk:icon-info-filename
                (gtk:icon-theme-lookup-icon theme "battery" 0 0))))
    (is (stringp
            (gtk:icon-info-filename
                (gtk:icon-theme-lookup-icon theme "edit-cut" 0 0))))))

;;;     gtk_icon_info_get_builtin_pixbuf

#-windows
(test gtk-icon-info-builtin-pixbuf
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 :use-builtin)))
    (is-false (gtk:icon-info-builtin-pixbuf icon-info))))

;;;     gtk_icon_info_load_icon

#-windows
(test gtk-icon-info-load-icon
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 0)))
    (is (typep (gtk:icon-info-load-icon icon-info) 'gdk:pixbuf))))

;;;     gtk_icon_info_load_surface

#-windows
(test gtk-icon-info-load-surface
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 0)))
    (is-true (cffi:pointerp (gtk:icon-info-load-surface icon-info nil)))))

;;;     gtk_icon_info_load_icon_async
;;;     gtk_icon_info_load_icon_finish

;;;     gtk_icon_info_load_symbolic

#-windows
(test gtk-icon-info-load-symbolic
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 0)))
    (is (typep (gtk:icon-info-load-symbolic icon-info (gdk:rgba-new) nil nil nil)
               'gdk:pixbuf))))

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
  (let* ((theme (gtk:icon-theme-default))
         (icon-info (gtk:icon-theme-lookup-icon theme "battery" 0 :force-symbolic)))
    (is-true (gtk:icon-info-is-symbolic icon-info))))

;;; 2024-6-16
