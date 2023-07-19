(in-package :gtk-test)

(def-suite gtk-style-context :in gtk-suite)
(in-suite gtk-style-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkIconSource

(test gtk-icon-source
  ;; Type check
  (is (g:type-is-a (g:gtype "GtkIconSource") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconSource")
          (g:gtype (cffi:foreign-funcall "gtk_icon_source_get_type" :size)))))

;;;     GtkIconFactory

(test gtk-icon-factory-class
  ;; Type check
  (is (g:type-is-object "GtkIconFactory"))
  ;; Check the registered name
  (is (eq 'gtk:icon-factory
          (glib:symbol-for-gtype "GtkIconFactory")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconFactory")
          (g:gtype (cffi:foreign-funcall "gtk_icon_factory_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkIconFactory")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkIconFactory")))
  ;; Check the interfaces
  (is (equal '("GtkBuildable")
             (list-interfaces "GtkIconFactory")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkIconFactory")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkIconFactory")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkIconFactory" GTK-ICON-FACTORY
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GtkBuildable") :TYPE-INITIALIZER
                        "gtk_icon_factory_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkIconFactory"))))

;;;     GtkIconSet

(test gtk-icon-set
  ;; Type check
  (is (g:type-is-a (g:gtype "GtkIconSet") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconSet")
          (g:gtype (cffi:foreign-funcall "gtk_icon_set_get_type" :size)))))

;;;     GtkIconSize

(test gtk-icon-size
  ;; Check the type
  (is (g:type-is-enum "GtkIconSize"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkIconSize")
          (g:gtype (cffi:foreign-funcall "gtk_icon_size_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:icon-size
          (glib:symbol-for-gtype "GtkIconSize")))
  ;; Check the names
  (is (equal '("GTK_ICON_SIZE_INVALID" "GTK_ICON_SIZE_MENU"
               "GTK_ICON_SIZE_SMALL_TOOLBAR" "GTK_ICON_SIZE_LARGE_TOOLBAR"
               "GTK_ICON_SIZE_BUTTON" "GTK_ICON_SIZE_DND" 
               "GTK_ICON_SIZE_DIALOG")
             (list-enum-item-name "GtkIconSize")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GtkIconSize")))
  ;; Check the nick names
  (is (equal '("invalid" "menu" "small-toolbar" "large-toolbar" "button" "dnd"
               "dialog")
             (list-enum-item-nick "GtkIconSize")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkIconSize"
                             GTK-ICON-SIZE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_icon_size_get_type")
                             (:INVALID 0)
                             (:MENU 1)
                             (:SMALL-TOOLBAR 2)
                             (:LARGE-TOOLBAR 3)
                             (:BUTTON 4)
                             (:DND 5)
                             (:DIALOG 6))
             (gobject:get-g-type-definition "GtkIconSize"))))

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

;;; --- 2023-7-19 --------------------------------------------------------------
