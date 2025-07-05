(in-package :gtk-test)

(def-suite gtk-style-context :in gtk-suite)
(in-suite gtk-style-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconSize

(test gtk-icon-size
  ;; Check type
  (is (g:type-is-enum "GtkIconSize"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconSize")
          (g:gtype (cffi:foreign-funcall "gtk_icon_size_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:icon-size
          (glib:symbol-for-gtype "GtkIconSize")))
  ;; Check names
  (is (equal '("GTK_ICON_SIZE_INVALID" "GTK_ICON_SIZE_MENU"
               "GTK_ICON_SIZE_SMALL_TOOLBAR" "GTK_ICON_SIZE_LARGE_TOOLBAR"
               "GTK_ICON_SIZE_BUTTON" "GTK_ICON_SIZE_DND"
               "GTK_ICON_SIZE_DIALOG")
             (glib-test:list-enum-item-names "GtkIconSize")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GtkIconSize")))
  ;; Check nick names
  (is (equal '("invalid" "menu" "small-toolbar" "large-toolbar" "button" "dnd"
               "dialog")
             (glib-test:list-enum-item-nicks "GtkIconSize")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkIconSize" GTK:ICON-SIZE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_icon_size_get_type")
                       (:INVALID 0)
                       (:MENU 1)
                       (:SMALL-TOOLBAR 2)
                       (:LARGE-TOOLBAR 3)
                       (:BUTTON 4)
                       (:DND 5)
                       (:DIALOG 6))
             (gobject:get-gtype-definition "GtkIconSize"))))

;;;     GtkIconSource

(test gtk-icon-source-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkIconSource"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconSource")
          (g:gtype (cffi:foreign-funcall "gtk_icon_source_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:icon-source
          (glib:symbol-for-gtype "GtkIconSource"))))

;;;     GtkIconSet

(test gtk-icon-set-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkIconSet"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconSet")
          (g:gtype (cffi:foreign-funcall "gtk_icon_set_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:icon-set
          (glib:symbol-for-gtype "GtkIconSet"))))

;;;     GtkIconFactory

(test gtk-icon-factory-class
  ;; Check type
  (is (g:type-is-object "GtkIconFactory"))
  ;; Check registered name
  (is (eq 'gtk:icon-factory
          (glib:symbol-for-gtype "GtkIconFactory")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIconFactory")
          (g:gtype (cffi:foreign-funcall "gtk_icon_factory_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconFactory")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkIconFactory")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkIconFactory")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkIconFactory")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkIconFactory")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkIconFactory" GTK:ICON-FACTORY
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_icon_factory_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkIconFactory"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_icon_source_copy
;;;     gtk_icon_source_free

;;;     gtk_icon_factory_add
;;;     gtk_icon_factory_add_default
;;;     gtk_icon_factory_lookup
;;;     gtk_icon_factory_lookup_default
;;;     gtk_icon_factory_new
;;;     gtk_icon_factory_remove_default

;;;     gtk_icon_set_add_source
;;;     gtk_icon_set_copy
;;;     gtk_icon_set_new
;;;     gtk_icon_set_new_from_pixbuf
;;;     gtk_icon_set_ref
;;;     gtk_icon_set_render_icon
;;;     gtk_icon_set_render_icon_pixbuf
;;;     gtk_icon_set_render_icon_surface
;;;     gtk_icon_set_unref

;;;     gtk_icon_size_lookup
;;;     gtk_icon_size_lookup_for_settings
;;;     gtk_icon_size_register
;;;     gtk_icon_size_register_alias
;;;     gtk_icon_size_from_name
;;;     gtk_icon_size_get_name

;;;     gtk_icon_set_get_sizes

;;;     gtk_icon_source_get_direction
;;;     gtk_icon_source_get_direction_wildcarded
;;;     gtk_icon_source_get_filename
;;;     gtk_icon_source_get_pixbuf
;;;     gtk_icon_source_get_icon_name
;;;     gtk_icon_source_get_size
;;;     gtk_icon_source_get_size_wildcarded
;;;     gtk_icon_source_get_state
;;;     gtk_icon_source_get_state_wildcarded
;;;     gtk_icon_source_new
;;;     gtk_icon_source_set_direction
;;;     gtk_icon_source_set_direction_wildcarded
;;;     gtk_icon_source_set_filename
;;;     gtk_icon_source_set_pixbuf
;;;     gtk_icon_source_set_icon_name
;;;     gtk_icon_source_set_size
;;;     gtk_icon_source_set_size_wildcarded
;;;     gtk_icon_source_set_state
;;;     gtk_icon_source_set_state_wildcarded

;;; 2025-07-05
