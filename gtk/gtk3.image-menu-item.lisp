;;; ----------------------------------------------------------------------------
;;; gtk3.image-menu-item.lisp
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
;;; GtkImageMenuItem
;;;
;;;     A deprecated widget for a menu item with an icon.
;;;
;;; Types and Values
;;;
;;;     GtkImageMenuItem
;;;
;;; Accessors
;;;
;;;     gtk_image_menu_item_set_image
;;;     gtk_image_menu_item_get_image
;;;     gtk_image_menu_item_get_use_stock
;;;     gtk_image_menu_item_set_use_stock
;;;     gtk_image_menu_item_get_always_show_image
;;;     gtk_image_menu_item_set_always_show_image
;;;     gtk_image_menu_item_set_accel_group
;;;
;;; Functions
;;;
;;;     gtk_image_menu_item_new
;;;     gtk_image_menu_item_new_from_stock
;;;     gtk_image_menu_item_new_with_label
;;;     gtk_image_menu_item_new_with_mnemonic
;;;
;;; Properties
;;;
;;;     accel-group
;;;     always-show-image
;;;     image
;;;     use-stock
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkImageMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkImageMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkImageMenuItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkImageMenuItem" image-menu-item
  (:superclass menu-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable"
                 "GtkActionable")
    :type-initializer "gtk_image_menu_item_get_type")
  ((accel-group
    image-menu-item-accel-group
    "accel-group" "GtkAccelGroup" nil t)
   (always-show-image
    image-menu-item-always-show-image
    "always-show-image" "gboolean" t t)
   (image
    image-menu-item-image
    "image" "GtkWidget" t t)
   (use-stock
    image-menu-item-use-stock
    "use-stock" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'image-menu-item 'type)
 "@version{2024-06-27}
  @begin{short}
    The @class{gtk:image-menu-item} widget is a menu item which has an icon
    next to the text label.
  @end{short}
  This is functionally equivalent to:
  @begin{pre}
(defun create-image-menu-item ()
  (let ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :spacing 6))
        (icon (make-instance 'gtk:image
                             :icon-name \"folder-music-symbolic\"
                             :icon-size 1))
        (label (make-instance 'gtk:label
                              :label \"Music\"))
        (menuitem (make-instance 'gtk:menu-item)))
    (gtk:container-add box icon)
    (gtk:container-add box label)
    (gtk:container-add menuitem box)
    menuitem))
  @end{pre}
  Note that the user may disable display of menu icons using the
  @slot[gtk:settings]{gtk-menu-images} setting, so make sure to still fill in
  the text label. If you want to ensure that your menu items show an icon you
  are strongly encouraged to use a @class{gtk:menu-item} widget with a
  @class{gtk:image} widget instead.

  Furthermore, if you would like to display keyboard accelerator, you must pack
  the accel label into the box using the @fun{gtk:box-pack-end} function and
  align the label, otherwise the accelerator will not display correctly. The
  following code snippet adds a keyboard accelerator to the menu item, with a
  key binding of the @kbd{Ctrl+M} key:
  @begin{pre}
(defun create-image-menu-item-with-accel ()
  (let ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :spacing 6))
        (icon (make-instance 'gtk:image
                             :icon-name \"folder-music-symbolic\"
                             :icon-size 1))
        (label (make-instance 'gtk:accel-label
                              :label \"Music\"
                              :use-underline t
                              :xalign 0.0))
        (menuitem (make-instance 'gtk:menu-item))
        (accel-group (make-instance 'gtk:accel-group)))
    (gtk:widget-add-accelerator menuitem
                                \"activate\"
                                accel-group
                                (gdk-keyval-from-name \"M\")
                                :control-mask
                                :visible)
    (setf (gtk:accel-label-accel-widget label) menuitem)
    (gtk:container-add box icon)
    (gtk:box-pack-end box label :expand t :fill t :padding 0)
    (gtk:container-add menuitem box)
    menuitem))
  @end{pre}
  @begin[Warning]{dictionary}
    The @class{gtk:image-menu-item} class has been deprecated since GTK 3.10.
    If you want to display an icon in a menu item, you should use the
    @class{gtk:menu-item} widget and pack a @class{gtk:box} widget with a
    @class{gtk:image} widget and a @class{gtk:label} widget instead. You should
    also consider using the @class{gtk:builder} object and the @class{g:menu}
    XML description for creating menus, by following the @class{g:menu} guide.
    You should consider using icons in menu items only sparingly, and for
    \"objects\" or \"nouns\" elements only, like bookmarks, files, and links,
    \"actions\" or \"verbs\" should not have icons.
  @end{dictionary}
  @see-constructor{gtk:image-menu-item-new}
  @see-constructor{gtk:image-menu-item-new-from-stock}
  @see-constructor{gtk:image-menu-item-new-with-label}
  @see-constructor{gtk:image-menu-item-new-with-mnemonic}
  @see-slot{gtk:image-menu-item-accel-group}
  @see-slot{gtk:image-menu-item-always-show-image}
  @see-slot{gtk:image-menu-item-image}
  @see-slot{gtk:image-menu-item-use-stock}
  @see-class{gtk:menu-item}
  @see-class{g:menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:image-menu-item-accel-group ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-group"
                                               'image-menu-item) t)
 "The @code{accel-group} property of type @class{gtk:accel-group} (Write) @br{}
  The accelerator group to use for stock accelerator keys.")

#+liber-documentation
(setf (liber:alias-for-function 'image-menu-item-accel-group)
      "Accessor"
      (documentation 'image-menu-item-accel-group 'function)
 "@version{2024-06-27}
  @syntax{(setf (gtk:image-menu-item-accel-group object) group)}
  @argument[object]{a @class{gtk:image-menu-item} widget}
  @argument[group]{a @class{gtk:accel-group} object}
  @begin{short}
    Accessor of @slot[gtk:image-menu-item]{accel-group} slot of the
    @class{gtk:image-menu-item} class.
  @end{short}
  Specifies an accelerator group to add the menu items accelerator to, this
  only applies to stock items so a stock item must already be set. Make sure to
  call the @fun{gtk:image-menu-item-use-stock} and @fun{gtk:menu-item-label}
  functions with a valid stock item first.

  If you want this menu item to have changeable accelerators then you should
  not need this. See the @fun{gtk:image-menu-item-new-from-stock} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-accel-group} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-class{gtk:accel-group}
  @see-function{gtk:image-menu-item-use-stock}
  @see-function{gtk:menu-item-label}
  @see-function{gtk:image-menu-item-new-from-stock}")

;;; --- gtk:image-menu-item-always-show-image ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "always-show-image"
                                               'image-menu-item) t)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item will ignore the
  @slot[gtk:settings]{gtk-menu-images} setting and always show the image, if
  available. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'image-menu-item-always-show-image)
      "Accessor"
      (documentation 'image-menu-item-always-show-image 'function)
 "@version{2024-06-27}
  @syntax{(gtk:image-menu-item-always-show-image object) => always-show}
  @syntax{(setf (gtk:image-menu-item-always-show-image object) always-show)}
  @argument[object]{a @class{gtk:image-menu-item} widget}
  @argument[always-show]{@em{true} if the menu item should always show the
    image}
  @begin{short}
    Accessor of the @slot[gtk:image-menu-item]{always-show-image} slot of the
    @class{gtk:image-menu-item} class.
  @end{short}
  If @em{true}, the menu item will ignore the
  @slot[gtk:settings]{gtk-menu-images} setting and always show the image, if
  available. Use this property if the menu item would be useless or hard to use
  without the image.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-always-show-image} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-function{gtk:settings-gtk-menu-images}")

;;; --- gtk:image-menu-item-image ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "image" 'image-menu-item) t)
 "The @code{image} property of type @class{gtk:widget} (Read / Write) @br{}
  Child widget to appear next to the menu text.")

#+liber-documentation
(setf (liber:alias-for-function 'image-menu-item-image)
      "Accessor"
      (documentation 'image-menu-item-image 'function)
 "@version{2024-06-27}
  @syntax{(gtk:image-menu-item-image object) => image}
  @syntax{(setf (gtk:image-menu-item-image object) image)}
  @argument[object]{a @class{gtk:image-menu-item} widget}
  @argument[image]{a @class{gtk:widget} object to set as the image for the menu
    item}
  @begin{short}
    Accessor of the @slot[gtk:image-menu-item]{image} slot of the
    @class{gtk:image-menu-item} class.
  @end{short}
  The @fun{gtk:image-menu-item-image} function gets the widget that is currently
  set as the image of the menu item. The @setf{gtk:image-menu-item-image}
  function sets the image. Note that it depends on the
  @slot[gtk:settings]{gtk-menu-images} setting whether the image will be
  displayed or not.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-image} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-class{gtk:widget}
  @see-function{gtk:settings-gtk-menu-images}")

;;; --- gtk:image-menu-item-use-stock ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-stock" 'image-menu-item) t)
 "The @code{use-stock} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the label set in the menu item is used as a stock ID to select
  the stock item for the item. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'image-menu-item-use-stock)
      "Accessor"
      (documentation 'image-menu-item-use-stock 'function)
 "@version{2024-06-27}
  @syntax{(gtk:image-menu-item-use-stock object) => use-stock}
  @syntax{(setf (gtk:image-menu-item-use-stock object) use-stock)}
  @argument[object]{a @class{gtk:image-menu-item} widget}
  @argument[use-stock]{@em{true} if the menu item should use a stock item}
  @begin{short}
    Accessor of the @slot[gtk:image-menu-item]{use-stock} slot of the
    @class{gtk:image-menu-item} class.
  @end{short}
  If @em{true}, the label set in the menu item is used as a stock ID to select
  the stock item for the item.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-use-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new
;;; ----------------------------------------------------------------------------

(declaim (inline image-menu-item-new))

(defun image-menu-item-new ()
 "@version{2024-06-27}
  @return{The new @class{gtk:image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item with an empty label.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-new} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}"
  (make-instance 'image-menu-item))

(export 'image-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_from_stock
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_menu_item_new_from_stock"
               %image-menu-item-new-from-stock) (g:object image-menu-item)
  (stock-id :string)
  (group (g:object accel-group)))

(defun image-menu-item-new-from-stock (stock-id &optional group)
 "@version{2025-07-07}
  @argument[stock-id]{a string for the name of the stock item}
  @argument[group]{a @class{gtk:accel-group} object to add the menu items
    accelerator to, or @code{nil}}
  @return{The new @class{gtk:image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing the image and text from a stock
    item.
  @end{short}
  If you want this menu item to have changeable accelerators, then pass in
  @code{nil} for @arg{group}. Next call the @fun{gtk:menu-item-accel-path}
  function with an appropriate path for the menu item, use the
  @code{gtk_stock_lookup()} function to look up the standard accelerator for
  the stock item, and if one is found, call the @fun{gtk:accel-map-add-entry}
  function to register it.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-new-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-class{gtk:accel-group}
  @see-function{gtk:menu-item-accel-path}
  @see-function{gtk:accel-map-add-entry}"
  (%image-menu-item-new-from-stock stock-id group))

(export 'image-menu-item-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_menu_item_new_with_label"
               image-menu-item-new-with-label) (g:object image-menu-item)
 "@version{2025-07-07}
  @argument[label]{a string for the text of the menu item}
  @return{The new @class{gtk:image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing a label.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-new-with-label} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:menu-item-new-with-label} function instead.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-function{gtk:menu-item-new-with-label}"
  (label :string))

(export 'image-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_image_menu_item_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_menu_item_new_with_mnemonic"
               image-menu-item-new-with-mnemonic) (g:object image-menu-item)
 "@version{2025-07-07}
  @argument[label]{a string for the text of the menu item, with an underscore
    in front of the mnemonic character}
  @return{The new @class{gtk:image-menu-item} widget.}
  @begin{short}
    Creates a new image menu item containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item.
  @begin[Warning]{dictionary}
    The @fun{gtk:image-menu-item-new-with-mnemonic} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:menu-item-new-with-mnemonic} function instead.
  @end{dictionary}
  @see-class{gtk:image-menu-item}
  @see-function{gtk:label-new-with-mnemonic}
  @see-function{gtk:menu-item-new-with-mnemonic}"
  (label :string))

(export 'image-menu-item-new-with-mnemonic)

;;; --- End of file gtk3.image-menu-item.lisp ----------------------------------
