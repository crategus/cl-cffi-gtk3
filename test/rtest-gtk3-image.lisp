(in-package :gtk-test)

(def-suite gtk-image :in gtk-suite)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test image-type
  ;; Check the type
  (is (g:type-is-enum "GtkImageType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkImageType")
          (g:gtype (cffi:foreign-funcall "gtk_image_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:image-type
          (gobject:symbol-for-gtype "GtkImageType")))
  ;; Check the names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_PIXBUF" "GTK_IMAGE_STOCK"
               "GTK_IMAGE_ICON_SET" "GTK_IMAGE_ANIMATION" "GTK_IMAGE_ICON_NAME"
               "GTK_IMAGE_GICON" "GTK_IMAGE_SURFACE")
             (list-enum-item-name "GtkImageType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GtkImageType")))
  ;; Check the nick names
  (is (equal '("empty" "pixbuf" "stock" "icon-set" "animation" "icon-name"
               "gicon" "surface")
             (list-enum-item-nick "GtkImageType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkImageType"
                             GTK-IMAGE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_image_type_get_type")
                             (:EMPTY 0)
                             (:PIXBUF 1)
                             (:STOCK 2)
                             (:ICON-SET 3)
                             (:ANIMATION 4)
                             (:ICON-NAME 5)
                             (:GICON 6)
                             (:SURFACE 7))
             (gobject:get-g-type-definition "GtkImageType"))))

;;;     GtkImage

(test image-class
  ;; Type check
  (is (g:type-is-object "GtkImage"))
  ;; Check the registered name
  (is (eq 'gtk:image
          (gobject:symbol-for-gtype "GtkImage")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkImage")
          (g:gtype (cffi:foreign-funcall "gtk_image_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkMisc") (g:type-parent "GtkImage")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkImage")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkImage")))
  ;; Check the class properties
  (is (equal '("file" "gicon" "icon-name" "icon-set" "icon-size" "pixbuf"
               "pixbuf-animation" "pixel-size" "resource" "stock" "storage-type"
               "surface" "use-fallback")
             (list-properties "GtkImage")))
  ;; Check the style properties.
  (is (equal '()
             (list-style-properties "GtkImage")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkImage")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkImage" GTK-IMAGE
                       (:SUPERCLASS GTK-MISC :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_image_get_type")
                       ((FILE GTK-IMAGE-FILE "file" "gchararray" T T)
                        (GICON GTK-IMAGE-GICON "gicon" "GIcon" T T)
                        (ICON-NAME GTK-IMAGE-ICON-NAME "icon-name" "gchararray"
                         T T)
                        (ICON-SET GTK-IMAGE-ICON-SET "icon-set" "GtkIconSet" T
                         T)
                        (ICON-SIZE GTK-IMAGE-ICON-SIZE "icon-size" "gint" T T)
                        (PIXBUF GTK-IMAGE-PIXBUF "pixbuf" "GdkPixbuf" T T)
                        (PIXBUF-ANIMATION GTK-IMAGE-PIXBUF-ANIMATION
                         "pixbuf-animation" "GdkPixbufAnimation" T T)
                        (PIXEL-SIZE GTK-IMAGE-PIXEL-SIZE "pixel-size" "gint" T
                         T)
                        (RESOURCE GTK-IMAGE-RESOURCE "resource" "gchararray" T
                         T)
                        (STOCK GTK-IMAGE-STOCK "stock" "gchararray" T T)
                        (STORAGE-TYPE GTK-IMAGE-STORAGE-TYPE "storage-type"
                         "GtkImageType" T NIL)
                        (SURFACE GTK-IMAGE-SURFACE "surface" "CairoSurface" T
                         T)
                        (USE-FALLBACK GTK-IMAGE-USE-FALLBACK "use-fallback"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkImage"))))

;;; --- Properties -------------------------------------------------------------

(test image-properties
  (let ((image (make-instance 'gtk:image)))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 4 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :empty (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

(test image-icon-size
  ;; The accessor gtk:image-icon-size is implemented with an integer.
  ;; This integer can be converted to a gtk:icon-size keyword
  (let ((image (gtk:image-new)))
    (is (eq :button
            (cffi:foreign-enum-keyword 'gtk:icon-size
                                       (gtk:image-icon-size image))))
    (is (= (foreign-enum-value 'gtk:icon-size :button)
           (gtk:image-icon-size image)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_image_get_icon_set
;;;     gtk_image_get_stock

;;;     gtk_image_get_animation

(test image-get-animation
  (let* ((path (sys-path "resource/floppybuddy.gif"))
         (animation (gdk:pixbuf-animation-new-from-file path))
         (image (gtk:image-new-from-animation animation)))
    (is (typep image 'gtk:image))
    (is (typep (gtk:image-get-animation image) 'gdk:pixbuf-animation))))

;;;     gtk_image_get_icon_name

(test image-get-icon-name
  (let ((image (gtk:image-new-from-icon-name "gtk-ok" :dialog)))
    (is (typep image 'gtk:image))
    (is (string= "gtk-ok" (gtk:image-get-icon-name image)))
    (multiple-value-bind (icon-set icon-size)
        (gtk:image-get-icon-name image)
      (is (string= "gtk-ok" icon-set))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_get_gicon

(test image-get-gicon
  (let* ((icon (g:themed-icon-new-from-names "gtk-ok"))
         (image (gtk:image-new-from-gicon icon :dialog)))
    (is (typep image 'gtk:image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (multiple-value-bind (icon-set icon-size)
        (gtk:image-get-gicon image)
      (is (typep icon-set 'g:themed-icon))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_new_from_file

(test image-new-from-file
  (let* ((path (sys-path "resource/gtk-logo-24.png"))
         (image (gtk:image-new-from-file path)))
    (is (typep image 'gtk:image))
    (is (string= "gtk-logo-24" (pathname-name (gtk:image-file image))))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :pixbuf (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_icon_set

;;;     gtk_image_new_from_pixbuf

(test image-new-from-pixbuf
  (let* ((filename (sys-path "resource/gtk-logo-24.png"))
         (pixbuf (gdk:pixbuf-new-from-file filename))
         (image (gtk:image-new-from-pixbuf pixbuf)))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :pixbuf (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_stock

;;;     gtk_image_new_from_animation

(test image-new-from-animation
  (let* ((filename (sys-path "resource/floppybuddy.gif"))
         (animation (gdk:pixbuf-animation-new-from-file filename))
         (image (gtk:image-new-from-animation animation)))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is (typep (gtk:image-pixbuf-animation image) 'gdk:pixbuf-animation))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :animation (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_icon_name

(test image-new-from-icon-name
  (let ((image (gtk:image-new-from-icon-name "gtk-ok" :dialog)))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is (string= "gtk-ok" (gtk:image-icon-name image)))
    (is-false (gtk:image-icon-set image))
    (is (= 6 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :icon-name (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_gicon

(test image-new-from-gicon
  (let* ((icon (g:themed-icon-new-from-names "gtk-ok"))
         (image (gtk:image-new-from-gicon icon :dialog)))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 6 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :gicon (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_resource

(test image-new-from-resource
  (let* ((filename (sys-path "resource/rtest-resource.gresource"))
         (resource (g:resource-load filename)))
    ;; Register the resources
    (is-false (g:resources-register resource))
    (let ((image (gtk:image-new-from-resource "/com/crategus/test/ducky.png")))
      (is (typep image 'gtk:image))
      (is-false (gtk:image-file image))
      (is-false (gtk:image-gicon image))
      (is-false (gtk:image-icon-name image))
      (is-false (gtk:image-icon-set image))
      (is (= 0 (gtk:image-icon-size image)))
      (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
      (is-false (gtk:image-pixbuf-animation image))
      (is (= -1 (gtk:image-pixel-size image)))
      (is (string= "/com/crategus/test/ducky.png" (gtk:image-resource image)))
      (is-false (gtk:image-stock image))
      (is (eq :pixbuf (gtk:image-storage-type image)))
      ;; at this point surface is a null-pointer, this causes an error
      (is-false (gtk:image-surface image))
      (is-false (gtk:image-use-fallback image)))
      ;; Unregister the resources
      (is-false (g:resources-unregister resource))))

;;;     gtk_image_new_from_surface ()

(test image-new-from-surface
  (let* ((theme (gtk:icon-theme-default))
         (surface (gtk:icon-theme-load-surface theme "gtk-ok"
                                                     48 1 nil :use-builtin))
         (image (gtk:image-new-from-surface surface)))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :surface (gtk:image-storage-type image)))
    ;; we have a valid Cairo surface
    (is (typep (gtk:image-surface image) 'gtk:cairo-surface))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_file

(test image-set-from-file
  (let ((filename (sys-path "resource/gtk-logo-24.png"))
        (image (make-instance 'gtk:image)))
    ;; Set image from file
    (is-false (gtk:image-set-from-file image filename))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is (string= "gtk-logo-24" (pathname-name (gtk:image-file image))))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :pixbuf (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_icon_set

;;;     gtk_image_set_from_pixbuf

(test image-set-from-pixbuf
  (let* ((filename (sys-path "resource/gtk-logo-24.png"))
         (pixbuf (gdk:pixbuf-new-from-file filename))
         (image (make-instance 'gtk:image)))
    ;; Set image from pixbuf
    (is-false (gtk:image-set-from-pixbuf image pixbuf))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :pixbuf (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_stock

;;;     gtk_image_set_from_animation

(test image-set-from-animation
  (let* ((filename (sys-path "resource/floppybuddy.gif"))
         (animation (gdk:pixbuf-animation-new-from-file filename))
         (image (make-instance 'gtk:image)))
    ;; Set image from animation
    (is-false (gtk:image-set-from-animation image animation))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is (typep (gtk:image-pixbuf-animation image) 'gdk:pixbuf-animation))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :animation (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_icon_name

(test image-set-from-icon-name
  (let ((image (make-instance 'gtk:image)))
    ;; Set image from icon name
    (is-false (gtk:image-set-from-icon-name image "gtk-ok" :dialog))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is (string= "gtk-ok" (gtk:image-icon-name image)))
    (is-false (gtk:image-icon-set image))
    (is (= 6 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :icon-name (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_gicon

(test image-set-from-gicon
  (let ((icon (g:themed-icon-new-from-names "gtk-ok"))
        (image (make-instance 'gtk:image)))
    ;; Set image from gicon
    (is-false (gtk:image-set-from-gicon image icon :dialog))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is (typep (gtk:image-gicon image) 'g:themed-icon))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 6 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :gicon (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_resource

(test image-set-from-resource
  (let* ((filename (sys-path "resource/rtest-resource.gresource"))
         (resource (g:resource-load filename))
         (image (gtk:image-new)))
    ;; Register the resources
    (is-false (g:resources-register resource))
    ;; Set image from resource
    (is-false (gtk:image-set-from-resource image "/com/crategus/test/ducky.png"))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is (typep (gtk:image-pixbuf image) 'gdk:pixbuf))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is (string= "/com/crategus/test/ducky.png" (gtk:image-resource image)))
    (is-false (gtk:image-stock image))
    (is (eq :pixbuf (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))
    ;; Unregister the resources
    (is-false (g:resources-unregister resource))))

;;;     gtk_image_set_from_surface ()

(test image-set-from-surface
  (let* ((theme (gtk:icon-theme-default))
         (surface (gtk:icon-theme-load-surface theme "gtk-ok"
                                                     48 1 nil :use-builtin))
         (image (make-instance 'gtk:image)))
    ;; Set image from surface
    (is-false (gtk:image-set-from-surface image surface))
    ;; Check the properties
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :surface (gtk:image-storage-type image)))
    ;; we have a valid Cairo surface
    (is (typep (gtk:image-surface image) 'gtk:cairo-surface))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_clear

(test image-clear
  (let ((image (gtk:image-new-from-icon-name "gtk-ok" 4)))
    ;; Create image from icon name
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is (string= "gtk-ok" (gtk:image-icon-name image)))
    (is-false (gtk:image-icon-set image))
    (is (= 4 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :icon-name (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))
    ;; Clear the image
    (is-false (gtk:image-clear image))
    (is (typep image 'gtk:image))
    (is-false (gtk:image-file image))
    (is-false (gtk:image-gicon image))
    (is-false (gtk:image-icon-name image))
    (is-false (gtk:image-icon-set image))
    (is (= 0 (gtk:image-icon-size image)))
    (is-false (gtk:image-pixbuf image))
    (is-false (gtk:image-pixbuf-animation image))
    (is (= -1 (gtk:image-pixel-size image)))
    (is-false (gtk:image-resource image))
    (is-false (gtk:image-stock image))
    (is (eq :empty (gtk:image-storage-type image)))
    ;; at this point surface is a null-pointer, this causes an error
    (is-false (gtk:image-surface image))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new

(test image-new
  (is (typep (gtk:image-new) 'gtk:image)))

;;; --- 2023-1-29 --------------------------------------------------------------
