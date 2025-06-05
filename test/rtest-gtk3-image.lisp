(in-package :gtk-test)

(def-suite gtk-image :in gtk-suite)
(in-suite gtk-image)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkImageType

(test gtk-image-type
  ;; Check type
  (is (g:type-is-enum "GtkImageType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkImageType")
          (g:gtype (cffi:foreign-funcall "gtk_image_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:image-type
          (glib:symbol-for-gtype "GtkImageType")))
  ;; Check names
  (is (equal '("GTK_IMAGE_EMPTY" "GTK_IMAGE_PIXBUF" "GTK_IMAGE_STOCK"
               "GTK_IMAGE_ICON_SET" "GTK_IMAGE_ANIMATION" "GTK_IMAGE_ICON_NAME"
               "GTK_IMAGE_GICON" "GTK_IMAGE_SURFACE")
             (glib-test:list-enum-item-names "GtkImageType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GtkImageType")))
  ;; Check nick names
  (is (equal '("empty" "pixbuf" "stock" "icon-set" "animation" "icon-name"
               "gicon" "surface")
             (glib-test:list-enum-item-nicks "GtkImageType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkImageType" GTK:IMAGE-TYPE
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
             (gobject:get-gtype-definition "GtkImageType"))))

;;;     GtkImage

(test gtk-image-class
  ;; Check type
  (is (g:type-is-object "GtkImage"))
  ;; Check registered name
  (is (eq 'gtk:image
          (glib:symbol-for-gtype "GtkImage")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkImage")
          (g:gtype (cffi:foreign-funcall "gtk_image_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMisc") (g:type-parent "GtkImage")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkImage")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkImage")))
  ;; Check class properties
  (is (equal '("file" "gicon" "icon-name" "icon-set" "icon-size" "pixbuf"
               "pixbuf-animation" "pixel-size" "resource" "stock" "storage-type"
               "surface" "use-fallback")
             (glib-test:list-properties "GtkImage")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkImage")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkImage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkImage" GTK:IMAGE
                      (:SUPERCLASS GTK:MISC
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_image_get_type")
                      ((FILE IMAGE-FILE "file" "gchararray" T T)
                       (GICON IMAGE-GICON "gicon" "GIcon" T T)
                       (ICON-NAME IMAGE-ICON-NAME "icon-name" "gchararray" T T)
                       (ICON-SET IMAGE-ICON-SET "icon-set" "GtkIconSet" T T)
                       (ICON-SIZE IMAGE-ICON-SIZE "icon-size" "gint" T T)
                       (PIXBUF IMAGE-PIXBUF "pixbuf" "GdkPixbuf" T T)
                       (PIXBUF-ANIMATION IMAGE-PIXBUF-ANIMATION
                        "pixbuf-animation" "GdkPixbufAnimation" T T)
                       (PIXEL-SIZE IMAGE-PIXEL-SIZE "pixel-size" "gint" T T)
                       (RESOURCE IMAGE-RESOURCE "resource" "gchararray" T T)
                       (STOCK IMAGE-STOCK "stock" "gchararray" T T)
                       (STORAGE-TYPE IMAGE-STORAGE-TYPE
                        "storage-type" "GtkImageType" T NIL)
                       (SURFACE IMAGE-SURFACE "surface" "CairoSurface" T T)
                       (USE-FALLBACK IMAGE-USE-FALLBACK
                        "use-fallback" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkImage"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-image-properties
  (glib-test:with-check-memory (image)
    (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))))

(test gtk-image-icon-size
  ;; The accessor gtk:image-icon-size is implemented with an integer.
  ;; This integer can be converted to a gtk:icon-size keyword
  (glib-test:with-check-memory (image)
    (is (typep (setf image (gtk:image-new)) 'gtk:image))
    (is (eq :button
            (cffi:foreign-enum-keyword 'gtk:icon-size
                                       (gtk:image-icon-size image))))
    (is (= (cffi:foreign-enum-value 'gtk:icon-size :button)
           (gtk:image-icon-size image)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_image_get_icon_set
;;;     gtk_image_get_stock

;;;     gtk_image_get_animation

(test gtk-image-get-animation
  (glib-test:with-check-memory (image animation)
    (let* ((path (glib-sys:sys-path "test/resource/floppybuddy.gif")))
      (is (typep (setf animation
                       (gdk:pixbuf-animation-new-from-file path))
                 'gdk:pixbuf-animation))
      (is (typep (setf image
                       (gtk:image-new-from-animation animation)) 'gtk:image))
      (is (typep (gtk:image-get-animation image) 'gdk:pixbuf-animation))
      ;; Remove references
      (is-false (setf (gtk:image-pixbuf-animation image) nil)))))

;;;     gtk_image_get_icon_name

(test gtk-image-get-icon-name
  (glib-test:with-check-memory (image)
    (is (typep (setf image
                     (gtk:image-new-from-icon-name "gtk-ok" :dialog))
               'gtk:image))
    (is (string= "gtk-ok" (gtk:image-get-icon-name image)))
    (multiple-value-bind (icon-set icon-size)
        (gtk:image-get-icon-name image)
      (is (string= "gtk-ok" icon-set))
      (is (eq :dialog icon-size)))))

;;;     gtk_image_get_gicon

(test gtk-image-get-gicon
  (glib-test:with-check-memory (icon image)
    (is (typep (setf icon (g:themed-icon-new-from-names "gtk-ok"))
               'g:themed-icon))
    (is (typep (setf image (gtk:image-new-from-gicon icon :dialog)) 'gtk:image))
    (multiple-value-bind (icon-set icon-size)
        (gtk:image-get-gicon image)
      (is (typep icon-set 'g:themed-icon))
      (is (eq :dialog icon-size)))
    ;; Remove references
    (is-false (gtk:image-clear image))))

;;;     gtk_image_new_from_file

(test gtk-image-new-from-file
  (glib-test:with-check-memory (image)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf image (gtk:image-new-from-file path)) 'gtk:image))
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_new_from_icon_set

;;;     gtk_image_new_from_pixbuf

(test gtk-image-new-from-pixbuf
  (glib-test:with-check-memory (pixbuf image)
    (let ((filename (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf pixbuf (gdk:pixbuf-new-from-file filename)) 'gdk:pixbuf))
      (is (typep (setf image (gtk:image-new-from-pixbuf pixbuf)) 'gtk:image))
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_new_from_stock

;;;     gtk_image_new_from_animation

(test gtk-image-new-from-animation
  (glib-test:with-check-memory (animation image)
    (let ((filename (glib-sys:sys-path "test/resource/floppybuddy.gif")))
      (is (typep (setf animation
                       (gdk:pixbuf-animation-new-from-file filename))
                 'gdk:pixbuf-animation))
      (is (typep (setf image
                       (gtk:image-new-from-animation animation)) 'gtk:image))
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_new_from_icon_name

(test gtk-image-new-from-icon-name
  (glib-test:with-check-memory (image)
    (is (typep (setf image
                     (gtk:image-new-from-icon-name "gtk-ok" :dialog))
               'gtk:image))
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new_from_gicon

(test gtk-image-new-from-gicon
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))
    ;; Remove references
    (is-false (gtk:image-clear image))))

;;;     gtk_image_new_from_resource

(test gtk-image-new-from-resource
  (glib-test:with-check-memory (image)
    (let* ((filename (glib-sys:sys-path "test/resource/rtest-resource.gresource"))
           (resource (g:resource-load filename)))
      ;; Register the resources
      (is-false (g:resources-register resource))
      (is (typep (setf image
                       (gtk:image-new-from-resource "/com/crategus/test/ducky.png"))
                 'gtk:image))
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Unregister the resources and remove references
      (is-false (gtk:image-clear image))
      (is-false (g:resources-unregister resource)))))

;;;     gtk_image_new_from_surface

(test gtk-image-new-from-surface
  (glib-test:with-check-memory (image :strong 1)
    (let* ((theme (gtk:icon-theme-default))
           (surface (gtk:icon-theme-load-surface theme "gtk-ok"
                                                       48 1 nil :use-builtin)))
      (is (typep (setf image
                       (gtk:image-new-from-surface surface)) 'gtk:image))
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
      (is (not (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image))))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_set_from_file

(test gtk-image-set-from-file
  (glib-test:with-check-memory (image)
    (let ((filename (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
      ;; Set image from file
      (is-false (gtk:image-set-from-file image filename))
      ;; Check the properties
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_set_from_icon_set

;;;     gtk_image_set_from_pixbuf

(test gtk-image-set-from-pixbuf
  (glib-test:with-check-memory (pixbuf image)
    (let ((filename (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
      (is (typep (setf pixbuf
                       (gdk:pixbuf-new-from-file filename)) 'gdk:pixbuf))
      (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
      ;; Set image from pixbuf
      (is-false (gtk:image-set-from-pixbuf image pixbuf))
      ;; Check the properties
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_set_from_stock

;;;     gtk_image_set_from_animation

(test gtk-image-set-from-animation
  (glib-test:with-check-memory (animation image)
    (let ((filename (glib-sys:sys-path "test/resource/floppybuddy.gif")))
      (is (typep (setf animation
                       (gdk:pixbuf-animation-new-from-file filename))
                 'gdk:pixbuf-animation))
      (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
      ;; Set image from animation
      (is-false (gtk:image-set-from-animation image animation))
      ;; Check the properties
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Remove references
      (is-false (gtk:image-clear image)))))

;;;     gtk_image_set_from_icon_name

(test gtk-image-set-from-icon-name
  (glib-test:with-check-memory (image)
    (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
    ;; Set image from icon name
    (is-false (gtk:image-set-from-icon-name image "gtk-ok" :dialog))
    ;; Check the properties
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_set_from_gicon

(test gtk-image-set-from-gicon
  (glib-test:with-check-memory (icon image)
    (is (typep (setf icon
                     (g:themed-icon-new-from-names "gtk-ok")) 'g:themed-icon))
    (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
    ;; Set image from gicon
    (is-false (gtk:image-set-from-gicon image icon :dialog))
    ;; Check the properties
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))
    ;; Remove references
    (is-false (gtk:image-clear image))))

;;;     gtk_image_set_from_resource

(test gtk-image-set-from-resource
  (glib-test:with-check-memory (image)
    (let* ((filename (glib-sys:sys-path "test/resource/rtest-resource.gresource"))
           (resource (g:resource-load filename)))
      (is (typep (setf image (gtk:image-new)) 'gtk:image))
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
      (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
      (is-false (gtk:image-use-fallback image))
      ;; Unregister the resources and remove references
      (is-false (gtk:image-clear image))
      (is-false (g:resources-unregister resource)))))

;;;     gtk_image_set_from_surface

(test gtk-image-set-from-surface
  (glib-test:with-check-memory (image)
    (let* ((theme (gtk:icon-theme-default))
           (surface (gtk:icon-theme-load-surface theme "gtk-ok"
                                                       48 1 nil :use-builtin)))
      (is (typep (setf image (make-instance 'gtk:image)) 'gtk:image))
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
      (is (not (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image))))
      (is-false (gtk:image-use-fallback image)))))

;;;     gtk_image_clear

(test gtk-image-clear
  (glib-test:with-check-memory (image)
    (is (typep (setf image
                     (gtk:image-new-from-icon-name "gtk-ok" 4)) 'gtk:image))
    ;; Create image from icon name
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))
    ;; Clear the image
    (is-false (gtk:image-clear image))
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
    (is (cffi:pointer-eq (cffi:null-pointer) (gtk:image-surface image)))
    (is-false (gtk:image-use-fallback image))))

;;;     gtk_image_new

(test gtk-image-new
  (glib-test:with-check-memory (image)
    (is (typep (setf image (gtk:image-new)) 'gtk:image))))

;;; 2025-06-04
