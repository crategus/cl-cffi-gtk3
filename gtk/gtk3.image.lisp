;;; ----------------------------------------------------------------------------
;;; gtk3.image.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkImage
;;;
;;;     A widget displaying an image
;;;
;;; Types and Values
;;;
;;;     GtkImage
;;;     GtkImageType
;;;
;;; Accessors
;;;
;;;     gtk_image_get_pixbuf
;;;     gtk_image_set_pixel_size
;;;     gtk_image_get_pixel_size
;;;     gtk_image_get_storage_type
;;;
;;; Functions
;;;
;;;     gtk_image_get_icon_set                              deprecated
;;;     gtk_image_get_stock                                 deprecated
;;;     gtk_image_get_animation
;;;     gtk_image_get_icon_name
;;;     gtk_image_get_gicon
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_icon_set                         deprecated
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_stock                            deprecated
;;;     gtk_image_new_from_animation
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_new_from_resource
;;;     gtk_image_new_from_surface
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_icon_set                         deprecated
;;;     gtk_image_set_from_pixbuf
;;;     gtk_image_set_from_stock                            deprecated
;;;     gtk_image_set_from_animation
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;     gtk_image_set_from_resource
;;;     gtk_image_set_from_surface
;;;     gtk_image_clear
;;;     gtk_image_new
;;;
;;; Properties
;;;
;;;     file
;;;     gicon
;;;     icon-name
;;;     icon-set
;;;     icon-size
;;;     pixbuf
;;;     pixbuf-animation
;;;     pixel-size
;;;     resource
;;;     stock
;;;     storage-type
;;;     surface
;;;     use-fallback
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkImage
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImage implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkImageType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkImageType" image-type
  (:export t
   :type-initializer "gtk_image_type_get_type")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6)
  (:surface 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'image-type)
      "GEnum"
      (liber:symbol-documentation 'image-type)
 "@version{#2025-06-28}
  @begin{declaration}
(gobject:define-genum \"GtkImageType\" image-type
  (:export t
   :type-initializer \"gtk_image_type_get_type\")
  (:empty 0)
  (:pixbuf 1)
  (:stock 2)
  (:icon-set 3)
  (:animation 4)
  (:icon-name 5)
  (:gicon 6)
  (:surface 7))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:empty]{There is no image displayed by the widget.}
      @entry[:pixbuf]{The widget contains a @class{gdk-pixbuf:pixbuf} object.}
      @entry[:stock]{The widget contains a stock icon name.}
      @entry[:icon-set]{The widget contains a @class{gtk:icon-set} instance.}
      @entry[:animation]{The widget contains a
        @class{gdk-pixbuf:pixbuf-animation} object.}
      @entry[:icon-name]{The widget contains a named icon.}
      @entry[:gicon]{The widget contains a @class{g:icon} object.}
      @entry[:surface]{The widget contains a @sym{cairo:surface-t} instance.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes the image data representation used by a @class{gtk:image} widget.
  @end{short}
  If you want to get the image from the widget, you can only get the currently
  stored representation. for example, if the @fun{gtk:image-storage-type}
  function returns the @val[gtk:image-type]{:pixbuf} value, then you can call
  the @fun{gtk:image-pixbuf} function but not the @fun{gtk:image-stock}
  function. For empty images, you can request any storage type, but they will
  all return @code{nil} values.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-class{gtk:icon-set}
  @see-class{g:icon}
  @see-symbol{cairo:surface-t}
  @see-function{gtk:image-storage-type}")

;;; ----------------------------------------------------------------------------
;;; GtkImage
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkImage" image
  (:superclass misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_image_get_type")
  ((file
    image-file
    "file" "gchararray" t t)
   (gicon
    image-gicon
    "gicon" "GIcon" t t)
   (icon-name
    image-icon-name
    "icon-name" "gchararray" t t)
   (icon-set
    image-icon-set
    "icon-set" "GtkIconSet" t t)
   ;; TODO: In the C implementation icon-size has the type :int, the accessor
   ;; image-icon-size returns therefore an integer and not a keyword of the
   ;; gtk:icon-size enumeration. This is not consistent.
   (icon-size
    image-icon-size
    "icon-size" "GtkIconSize" t t)
   (pixbuf
    image-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-animation
    image-pixbuf-animation
    "pixbuf-animation" "GdkPixbufAnimation" t t)
   (pixel-size
    image-pixel-size
    "pixel-size" "gint" t t)
   (resource
    image-resource
    "resource" "gchararray" t t)
   (stock
    image-stock
    "stock" "gchararray" t t)
   (storage-type
    image-storage-type
    "storage-type" "GtkImageType" t nil)
   (surface
    image-surface
    "surface" "CairoSurface" t t)
   (use-fallback
    image-use-fallback
    "use-fallback" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'image 'type)
 "@version{2025-06-28}
  @begin{short}
    The @class{gtk:image} widget displays an image.
  @end{short}
  Various kinds of objects can be displayed as an image. Most typically, you
  would load a @class{gdk-pixbuf:pixbuf} object from a file, and then display
  that.

  @image[image]{Figure: GtkImage}

  There is a convenience @fun{gtk:image-new-from-file} function  to do this,
  used as follows:
  @begin{pre}
(let ((image (gtk:image-new-from-file \"myfile.png\")))
  ... )
  @end{pre}
  If the file is not loaded successfully, the image will contain a
  \"broken image\" icon similar to that used in many web browsers. If you want
  to handle errors in loading the file yourself, for example by displaying an
  error message, then load the image with the
  @fun{gdk-pixbuf:pixbuf-new-from-file} function, then create the
  @class{gtk:image} widget with the @fun{gtk:image-new-from-pixbuf} function.

  The image file may contain an animation, if so the @class{gtk:image} widget
  will display a @class{gdk-pixbuf:pixbuf-animation} object instead of a static
  image.

  The @class{gtk:image} class is a subclass of the @class{gtk:misc} class, which
  implies that you can align it (center, left, right) and add padding to it,
  using the @class{gtk:misc} methods.

  The @class{gtk:image} widget is a \"no window\" widget, has no GDK window of
  its own, so by default does not receive events. If you want to receive events
  on the image, such as button clicks, place the image inside a
  @class{gtk:event-box} widget, then connect to the event signals on the event
  box.
  @begin[Examples]{dictionary}
    Handling button press events on a @class{gtk:image} widget.
    @begin{pre}
(defun create-image ()
  (let ((image (gtk:image-new-from-file (rel-path \"ducky.png\")))
        (event-box (make-instance 'gtk:event-box)))
    ;; Set the event mask for the event box
    (setf (gtk:widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g:signal-connect event-box \"button-press-event\"
                      (lambda (box event)
                        (declare (ignore box))
                        (format t \"Event box clicked at : ~6,2f, ~6,2f~%\"
                                  (gdk:event-button-x event)
                                  (gdk:event-button-y event))
                        gdk:+event-stop+))
    ;; Add the image to the event box
    (gtk:container-add event-box image)
    ;; Return the event box with the image
    event-box))
    @end{pre}
    When handling events on the event box, keep in mind that coordinates in the
    image may be different from event box coordinates due to the alignment and
    padding settings on the image, see the @class{gtk:misc} class. The simplest
    way to solve this is to set the alignment to 0.0 (left/top), and set the
    padding to zero. Then the origin of the image will be the same as the
    origin of the event box.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:image} implementation has a single CSS node with the name
    @code{image}. The @code{.icon-dropshadow} and @code{.lowres-icon} style
    classes may appear on @code{image} CSS nodes.
  @end{dictionary}
  @see-constructor{gtk:image-new}
  @see-slot{gtk:image-file}
  @see-slot{gtk:image-gicon}
  @see-slot{gtk:image-icon-name}
  @see-slot{gtk:image-icon-set}
  @see-slot{gtk:image-icon-size}
  @see-slot{gtk:image-pixbuf}
  @see-slot{gtk:image-pixbuf-animation}
  @see-slot{gtk:image-pixel-size}
  @see-slot{gtk:image-stock}
  @see-slot{gtk:image-storage-type}
  @see-slot{gtk:image-surface}
  @see-slot{gtk:image-use-fallback}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gdk-pixbuf:pixbuf-animation}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:image-file ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'image) t)
 "The @code{file} property of type @code{:string} (Read / Write) @br{}
  The name of the file to load and display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-file)
      "Accessor"
      (documentation 'image-file 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-file object) => filename}
  @syntax{(setf (gtk:image-file object) filename)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[filename]{a string for the name of the file to load and display}
  @begin{short}
    Accessor of the @slot[gtk:image]{file} slot of the @class{gtk:image} class.
  @end{short}
  The name of the file to load and display.
  @see-class{gtk:image}")

;;; --- gtk:image-gicon --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'image) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  The icon displayed in the image. For themed icons, if the
  icon theme is changed, the image will be updated automatically.")

#+liber-documentation
(setf (liber:alias-for-function 'image-gicon)
      "Accessor"
      (documentation 'image-gicon 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:image-gicon object) => gicon}
  @syntax{(setf (gtk:image-gicon object) gicon)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[gicon]{a @class{g:icon} icon}
  @begin{short}
    Accessor of the @slot[gtk:image]{gicon} slot of the @class{gtk:image} class.
  @end{short}
  The icon displayed in the image. For themed icons, if the icon theme is
  changed, the image will be updated automatically.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-class{g:themed-icon}")

;;; --- gtk:image-icon-name ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'image) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-name)
      "Accessor"
      (documentation 'image-icon-name 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-icon-name object) => name}
  @syntax{(setf (gtk:image-icon-name object) name)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[name]{a string for the name of the icon}
  @begin{short}
    Accessor of the @slot[gtk:image]{icon-name} slot of the @class{gtk:image}
    class.
  @end{short}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically.
  @see-class{gtk:image}
  @see-function{gtk:image-get-icon-name}")

;;; --- gtk:image-icon-set -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-set" 'image) t)
 "The @code{icon-set} property of type @class{gtk:icon-set} (Read / Write) @br{}
  The icon set to display. @br{}
  @em{Warning:} The @code{icon-set} poperty has been deprecated since version
  3.10 and should not be used in newly written code. Use the
  @slot[gtk:image]{icon-name} property instead.")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-set)
      "Accessor"
      (documentation 'image-icon-set 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-icon-set object) => icon-set}
  @syntax{(setf (gtk:image-icon-set object) icon-set)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[icon-set]{a @class{gtk:icon-set} instance}
  @begin{short}
    Accessor of the @slot[gtk:image]{icon-set} slot of the @class{gtk:image}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-icon-set} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-class{gtk:icon-set}
  @see-function{gtk:image-icon-name}")

;;; --- gtk:image-icon-size ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size" 'image) t)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  The symbolic size to use for a stock icon, icon set or named icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 4")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-size)
      "Accessor"
      (documentation 'image-icon-size 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-icon-size object) => size}
  @syntax{(setf (gtk:image-icon-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{an integer for the icon size}
  @begin{short}
    Accessor of the @slot[gtk:image]{icon-size} slot of the @class{gtk:image}
    class.
  @end{short}
  Symbolic size to use for a stock icon, icon set or named icon.
  @begin[Notes]{dictionary}
    In C the @slot[gtk:image]{icon-size} property is implemented as an integer
    type. Therefore the @fun{gtk:image-icon-size} accessor returns an integer
    and not a keyword value of the @sym{gtk:icon-size} enumeration.
  @end{dictionary}
  @see-class{gtk:image}")

;;; --- gtk:image-pixbuf -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf" 'image) t)
 "The @code{pixbuf} property of type @class{gdk-pixbuf:pixbuf} (Read / Write)
  @br{}
  The pixbuf to display.")

#+liber-documentation
(setf (liber:alias-for-function 'image-pixbuf)
      "Accessor"
      (documentation 'image-pixbuf 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:image-pixbuf object) => pixbuf}
  @syntax{(setf (gtk:image-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:image]{pixbuf} slot of the @class{gtk:image}
    class.
  @end{short}
  The @fun{gtk:image-pixbuf} function gets the pixbuf being displayed by the
  image. The @setf{gtk:image-pixbuf} function sets the pixbuf.

  The @sym{gtk:image-type} storage type of the image must be
  @val[gtk:image-type]{:empty} or @val[gtk:image-type]{:pixbuf}, see the
  @fun{gtk:image-storage-type} function.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:image-type}
  @see-function{gtk:image-storage-type}")

;;; --- gtk:image-pixbuf-animation ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf-animation" 'image) t)
 "The @code{pixbuf-animation} property of type
  @class{gdk-pixbuf:pixbuf-animation} (Read / Write) @br{}
  The pixbuf animation to display.")

#+liber-documentation
(setf (liber:alias-for-function 'image-pixbuf-animation)
      "Accessor"
      (documentation 'image-pixbuf-animation 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:image-pixbuf-animation object) => animation}
  @syntax{(setf (gtk:image-pixbuf-animation object) animation)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @begin{short}
    Accessor of the @slot[gtk:image]{pixbuf-animation} slot of the
    @class{gtk:image} class.
  @end{short}
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-function{gtk:image-get-animation}")

;;; --- gtk:image-pixel-size ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixel-size" 'image) t)
 "The @code{pixel-size} property of type @code{:int} (Read / Write) @br{}
  Can be used to specify a fixed size overriding the @slot[gtk:image]{icon-size}
  property for images of @val[gtk:image-type]{:icon-name} type. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'image-pixel-size)
      "Accessor"
      (documentation 'image-pixel-size 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-pixel-size object) => size}
  @syntax{(setf (gtk:image-pixel-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{an integer for the new pixel size}
  @begin{short}
    Accessor of the @slot[gtk:image]{pixel-size} slot of the @class{gtk:image}
    class.
  @end{short}
  The @fun{gtk:image-pixel-size} function sets the pixel size used for named
  icons. The @setf{gtk:image-pixel-size} function sets the pixel size. If the
  pixel size is set to a value not equal to -1, it is used instead of the
  @slot[gtk:image]{icon-size} property.
  @see-class{gtk:image}
  @see-function{gtk:image-icon-size}")

;;; --- gtk:image-resource -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resource" 'image) t)
 "The @code{resource} property of type @code{:string} (Read / Write) @br{}
  The path to a resource file to display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-resource)
      "Accessor"
      (documentation 'image-resource 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-resource object) => path}
  @syntax{(setf (gtk:image-resource object) path)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[path]{a string for a resource path}
  @begin{short}
    Accessor of the @slot[gtk:image]{stock} slot of the @class{gtk:image} class.
  @end{short}
  A path to a resource file to display.
  @see-class{gtk:image}")

;;; --- gtk:image-stock --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock" 'image) t)
 "The @code{stock} property of type @code{:string} (Read / Write) @br{}
  The stock ID for a stock image to display. @br{}
  @em{Warning:} The @code{stock} property has been deprecated since version
  3.10 and should not be used in newly written code. Use the
  @slot[gtk:image]{icon-name} property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-stock)
      "Accessor"
      (documentation 'image-stock 'function)
 "@version{#2025-06-17}
  @syntax{(gtk:image-stock object) => stock}
  @syntax{(setf (gtk:image-stock object) stock)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[stock]{a string for a stock ID}
  @begin{short}
    Accessor of the @slot[gtk:image]{stock} slot of the @class{gtk:image} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-stock} function property has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-function{gtk:image-icon-name}")

;;; --- gtk:image-storage-type -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "storage-type" 'image) t)
 "The @code{storage-type} property of type @sym{gtk:image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @val[gtk:image-type]{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'image-storage-type)
      "Accessor"
      (documentation 'image-storage-type 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:image-storage-type object) => type}
  @syntax{(setf (gtk:image-storage-type object) type)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[type]{a value of the @sym{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:image]{storage-type} slot of the @class{gtk:image}
    class.
  @end{short}
  The @fun{gtk:image-storage-type} function gets the type of representation
  being used by the @class{gtk:image} widget to store image data. The
  @setf{gtk:image-storage-type} function sets the image type.

  If the @class{gtk:image} widget has no image data, the return value will be
  @val[gtk:image-type]{:empty}.
  @see-class{gtk:image}
  @see-symbol{gtk:image-type}")

;;; --- gtk:image-surface ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'image) t)
 "The @code{surface} property of type @symol{cairo:surface-t} (Read / Write)
  @br{}
  The Cairo surface instance to display.")

#+liber-documentation
(setf (liber:alias-for-function 'image-surface)
      "Accessor"
      (documentation 'image-surface 'function)
 "@version{2025-06-28}
  @syntax{(gtk:image-surface object) => surface}
  @syntax{(setf (gtk:image-surface object) surface)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @begin{short}
    Accessor of the @slot[gtk:image]{surface} slot of the @class{gtk:image}
    class.
  @end{short}
  @see-class{gtk:image}
  @see-symbol{cairo:surface-t}")

;;; --- gtk:image-use-fallback -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-fallback" 'image) t)
 "The @code{use-fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether the icon displayed in the image will use standard icon names fallback.
  The value of this property is only relevant for images of
  @val[gtk:image-type]{:icon-name} and @val[gtk:image-type]{:gicon} type. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'image-use-fallback)
      "Accessor"
      (documentation 'image-use-fallback 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:image-use-fallback object) => use-fallback}
  @syntax{(setf (gtk:image-use-fallback object) use-fallback)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[use-fallback]{a boolean whether to use standard icon names fallback}
  @begin{short}
    Accessor of the @slot[gtk:image]{use-fallback} slot of the @class{gtk:image}
    class.
  @end{short}
  Whether the icon displayed in the @class{gtk:image} widget will use standard
  icon names fallback. The value of this property is only relevant for images
  of @val[gtk:image-type]{:icon-name} and @val[gtk:image-type]{:gicon} type.
  @see-class{gtk:image}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_set                                  not exported
;;; ----------------------------------------------------------------------------

(defun image-get-icon-set (image)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[image]{a @class{gtk:image} widget}
  @begin{return}
    @arg{icon-set} -- a @class{gtk:icon-set} instance @br{}
    @arg{icon-size} -- a @sym{gtk:icon-size} value
  @end{return}
  @begin{short}
    Gets the icon set and icon size being displayed by the @class{gtk:image}
    widget.
  @end{short}
  The storage type of the image must be the @val[gtk:image-type]{:empty} or
  @val[gtk:image-type]{:icon-set} type. See the @fun{gtk:image-storage-type}
  function.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-get-icon-set} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-get-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-class{gtk:icon-set}
  @see-function{gtk:image-get-icon-name}
  @see-function{gtk:image-storage-type}"
  (values (image-icon-set image)
          (cffi:foreign-enum-keyword 'icon-size (image-icon-size image))))

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_stock                                     not exported
;;; ----------------------------------------------------------------------------

(defun image-get-stock (image)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[image]{a @class{gtk:image} widget}
  @begin{return}
    @arg{stock-id} -- a string for a stock icon name @br{}
    @arg{size} -- a stock icon size of type @sym{gtk:icon-size}
  @end{return}
  @begin{short}
    Gets the stock icon name and icon size being displayed by the image.
  @end{short}
  The @sym{gtk:image-type} storage type of the image must be the
  @val[gtk:image-type]{:empty} or @val[gtk:image-type]{:stock} type, see the
  @fun{gtk:image-storage-type} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-get-stock} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-get-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-symbol{gtk:icon-size}
  @see-symbol{gtk:image-type}
  @see-function{gtk:image-storage-type}
  @see-function{gtk:image-get-icon-name}"
  (values (image-stock image)
          (cffi:foreign-enum-keyword 'icon-size (image-icon-size image))))

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_animation
;;; ----------------------------------------------------------------------------

;; TODO: Check if we can cut out this function.

(cffi:defcfun ("gtk_image_get_animation" image-get-animation)
    (g:object gdk-pixbuf:pixbuf-animation)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[image]{a @class{gtk:image} widget}
  @begin{return}
    The @class{gdk-pixbuf:pixbuf-animation} object for the displayed animation,
    or  @code{nil} if the image is empty.
  @end{return}
  @begin{short}
    Gets the @class{gdk-pixbuf:pixbuf-animation} object being displayed by the
    @class{gtk:image} widget.
  @end{short}
  The @sym{gtk:image-type} storage type of the image must be the
  @val[gtk:image-type]{:empty} or @val[gtk:image-type]{:animation} type, see
  the @fun{gtk:image-storage-type} function.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-symbol{gtk:image-type}
  @see-function{gtk:image-storage-type}"
  (image (g:object image)))

(export 'image-get-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_icon_name
;;; ----------------------------------------------------------------------------

;; TODO: Check if we can cut out this function.

(defun image-get-icon-name (image)
 #+liber-documentation
 "@version{#2025-06-28}
  @syntax{(gtk:image-icon-name image) => name, size}
  @argument[image]{a @class{gtk:image} widget}
  @argument[name]{a string for the icon name}
  @argument[size]{a @sym{gtk:icon-size} value}
  @begin{short}
    Gets the icon name and icon size being displayed by the image.
  @end{short}
  The storage type of the image must be the @val[gtk:image-type]{:empty} or
  @val[gtk:image-type]{:icon-name} type, see the @fun{gtk:image-storage-type}
  function.
  @see-class{gtk:image}
  @see-function{gtk:image-storage-type}"
  (values (image-icon-name image)
          (cffi:foreign-enum-keyword 'icon-size (image-icon-size image))))

(export 'image-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_get_gicon
;;; ----------------------------------------------------------------------------

(defun image-get-gicon (image)
 #+liber-documentation
 "@version{#2025-06-28}
  @syntax{(gtk:image-gicon image) => gicon, size}
  @argument[image]{a @class{gtk:image} widget}
  @argument[gicon]{a @class{g:icon} object}
  @argument[size]{a @sym{gtk:icon-size} value}
  @begin{short}
    Gets the @class{g:icon} icon and icon size being displayed by the image.
  @end{short}
  The storage type of the image must be the @val[gtk:image-type]{:empty} or
  @val[gtk:image-type]{:gicon} type, see the @fun{gtk:image-storage-type}
  function.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-function{gtk:image-storage-type}"
  (values (image-gicon image)
          (cffi:foreign-enum-keyword 'icon-size (image-icon-size image))))

(export 'image-get-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_file" %image-new-from-file) (g:object widget)
  (filename :string))

(defun image-new-from-file (path)
 #+liber-documentation
 "@version{2025-06-17}
  @argument[path]{a pathname or namestring for the name of the file}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying the file.
  @end{short}
  If the file is not found or cannot be loaded, the resulting @class{gtk:image}
  widget will display a \"broken image\" icon. This function never returns
  @code{nil}, it always returns a valid @class{gtk:image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the
  @fun{gdk-pixbuf:pixbuf-new-from-file} function to load the file yourself, then
  create the @class{gtk:image} widget from the pixbuf. Or for animations, use
  the @fun{gdk-pixbuf:pixbuf-animation-new-from-file} function.

  The storage type, see the @fun{gtk:image-storage-type} function, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk:image}
  @see-function{gdk-pixbuf:pixbuf-new-from-file}
  @see-function{gdk-pixbuf:pixbuf-animation-new-from-file}
  @see-function{gtk:image-storage-type}"
  (%image-new-from-file (namestring path)))

(export 'image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_set                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_icon_set" image-new-from-icon-set)
    (g:object image)
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[icon-set]{a @class{gtk:icon-set} instance}
  @argument[icon-size]{a @sym{gtk:icon-size} value}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying an icon set.
  @end{short}
  Sample icon sizes are the @val[gtk:icon-size]{:menu},
  @val[gtk:icon-size]{:small-toolbar} values. Instead of using this function,
  usually it is better to create a @class{gtk:icon-factory} object, put your
  icon sets in the icon factory, add the icon factory to the list of default
  factories with the @fun{gtk:icon-factory-add-default} function, and then use
  the @fun{gtk:image-new-from-stock} function. This will allow themes to
  override the icon you ship with your application.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-new-from-icon-set} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-new-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-class{gtk:icon-factory}
  @see-function{gtk:icon-factory-add-default}
  @see-function{gtk:image-new-from-icon-name}"
  (icon-set (g:boxed icon-set))
  (icon-size icon-size))

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_pixbuf" image-new-from-pixbuf)
    (g:object image)
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying @arg{pixbuf}.
  @end{short}
  Note that this function just creates an image from the pixbuf. The image
  created will not react to state changes. Should you want that, you should use
  the @fun{gtk:image-new-from-icon-name} function.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-new-from-icon-name}"
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_stock                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_stock" image-new-from-stock)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[stock-id]{a string for the stock icon name}
  @argument[icon-size]{a stock icon size from the @sym{gtk:icon-size}
    enumeration}
  @return{The new @class{gtk:image} widget displaying the stock icon.}
  @begin{short}
    Creates a an image displaying a stock icon.
  @end{short}
  Sample stock icon names are @code{\"gtk-open\"}, @code{\"gtk-quit\"}. Sample
  stock sizes are the @val[gtk:icon-size]{:menu},
  @val[gtk:icon-size]{:small-toolbar} values. If the stock icon name is not
  known, the image will be empty. You can register your own stock icon names,
  see the @fun{gtk:icon-factory-add-default} and @fun{gtk:icon-factory-add}
  functions.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-new-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-new-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:icon-factory-add}
  @see-function{gtk:icon-factory-add-default}"
  (stock-id :string)
  (icon-size icon-size))

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_animation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_animation" image-new-from-animation)
    (g:object image)
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates a image displaying the given animation.
  @end{short}
  Note that the animation frames are shown using a timeout with the
  @var{glib:+priority-default+} value. When using animations to indicate
  busyness, keep in mind that the animation will only be shown if the main loop
  is not busy with something that has a higher priority.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-variable{glib:+priority-default+}"
  (animation (g:object gdk-pixbuf:pixbuf-animation)))

(export 'image-new-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_icon_name" image-new-from-icon-name)
    (g:object image)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[name]{a string for an icon name}
  @argument[size]{a @sym{gtk:icon-size} value for the icon size}
  @return{The new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:image}
  @see-function{gtk:image-set-from-icon-name}"
  (name :string)
  (size icon-size))

(export 'image-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_gicon" image-new-from-gicon) (g:object image)
 #+liber-documentation
 "@version{2025-06-28}
  @argument[icon]{a @class{g:icon} object}
  @argument[size]{a @sym{gtk:icon-size} value for the icon size}
  @return{The new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-symbol{gtk:icon-size}"
  (icon (g:object g:icon))
  (size icon-size))

(export 'image-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_resource" image-new-from-resource)
    (g:object image)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[resource]{a string for a resource path}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying the resource file in @arg{resource}.
  @end{short}
  If the file is not found or can not be loaded, the resulting @class{gtk:image}
  widget will display a \"broken image\" icon. This function always returns a
  valid @class{gtk:image} widget.

  If the file contains an animation, the image will contain an animation.

  If you need to detect failures to load the file, use the
  @fun{gdk-pixbuf:pixbuf-new-from-resource} function to load the file yourself,
  then create the @class{gtk:image} widget from the pixbuf, or for animations,
  use the @fun{gdk-pixbuf:pixbuf-animation-new-from-resource} function.

  The storage type, see the @fun{gtk:image-storage-type} function, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk:image}
  @see-function{gdk-pixbuf:pixbuf-new-from-resource}
  @see-function{gdk-pixbuf:pixbuf-animation-new-from-resource}"
  (resource :string))

(export 'image-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_surface" image-new-from-surface)
    (g:object image)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates a new image displaying @arg{surface}.
  @end{short}
  @see-class{gtk:image}
  @see-symbol{cairo:surface-t}"
  (surface (:pointer (:struct cairo:surface-t))))

(export 'image-new-from-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_file" %image-set-from-file) :void
  (image (g:object image))
  (filename :string))

(defun image-set-from-file (image path)
 #+liber-documentation
 "@version{2025-06-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[pathname]{a pathname or namestring for a file to load}
  @begin{short}
    See the @fun{gtk:image-new-from-file} function for details.
  @end{short}
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-file}"
  (%image-set-from-file image (namestring path)))

(export 'image-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_set                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_icon_set" image-set-from-icon-set) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[icon-set]{a @class{gtk:icon-set} instance}
  @argument[icon-size]{a stock icon size of type @sym{gtk:icon-size}}
  @begin{short}
    See the @fun{gtk:image-new-from-icon-set} function for details.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-set-from-icon-set} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-set-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-function{gtk:image-set-from-icon-name}"
  (image (g:object image))
  (icon-set (g:boxed icon-set))
  (icon-size icon-size))

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_pixbuf" image-set-from-pixbuf) :void
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[image]{a @class{gtk:image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Creates an image displaying @arg{pixbuf}.
  @end{short}
  See the @fun{gtk:image-new-from-pixbuf} function for more details.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-new-from-pixbuf}"
  (image (g:object image))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'image-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_stock                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_stock" image-set-from-stock) :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[image]{a @class{gtk:image} widget}
  @argument[stock-id]{a string for a stock icon name}
  @argument[icon-size]{a stock icon size of type @sym{gtk:icon-size}}
  @begin{short}
    See the @fun{gtk:image-new-from-stock} function for details.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-set-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:image-set-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-function{gtk:image-set-from-icon-name}"
  (image (g:object image))
  (stock-id :string)
  (icon-size icon-size))

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_animation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_animation" image-set-from-animation) :void
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[image]{a @class{gtk:image} widget}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @begin{short}
    Causes the image to display the given animation, or display nothing, if
    you set the animation to @code{nil}.
  @end{short}
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf-animation}"
  (image (g:object image))
  (animation (g:object gdk-pixbuf:pixbuf-animation)))

(export 'image-set-from-animation)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_icon_name" image-set-from-icon-name) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[name]{a string for an icon name}
  @argument[size]{a @sym{gtk:icon-size} value for the icon size}
  @begin{short}
    See the @fun{gtk:image-new-from-icon-name} function for details.
  @end{short}
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-icon-name}"
  (image (g:object image))
  (name :string)
  (size icon-size))

(export 'image-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_gicon" image-set-from-gicon) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[icon]{a @class{g:icon} icon}
  @argument[size]{a value of the @sym{gtk:icon-size} enumeration}
  @begin{short}
    See the @fun{gtk:image-new-from-gicon} function for details.
  @end{short}
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:image-new-from-gicon}"
  (image (g:object image))
  (icon (g:object g:icon))
  (size icon-size))

(export 'image-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_resource" image-set-from-resource) :void
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[resource]{a string for a resource path}
  @begin{short}
    See the @fun{gtk:image-new-from-resource} function for details.
  @end{short}
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-resource}"
  (image (g:object image))
  (resource :string))

(export 'image-set-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_surface" image-set-from-surface) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[surface]{a @sym{cairo:surface-t} instance}
  @begin{short}
    See the @fun{gtk:image-new-from-surface} function for details.
  @end{short}
  @see-class{gtk:image}
  @see-symbol{cairo:surface-t}
  @see-function{gtk:image-new-from-surface}"
  (image (g:object image))
  (surface (:pointer (:struct cairo:surface-t))))

(export 'image-set-from-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_clear" image-clear) :void
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[image]{a @class{gtk:image} widget}
  @short{Resets the image to be empty.}
  @see-class{gtk:image}"
  (image (g:object image)))

(export 'image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new
;;; ----------------------------------------------------------------------------

(declaim (inline image-new))

(defun image-new ()
 #+liber-documentation
 "@version{#2023-03-20}
  @return{The newly created @class{gtk:image} widget.}
  @short{Creates a new image.}
  @see-class{gtk:image}"
  (make-instance 'image))

(export 'image-new)

;;; --- End of file gtk3.image.lisp --------------------------------------------
