;;; ----------------------------------------------------------------------------
;;; gdk3.cursor.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; Cursors
;;;
;;;     Standard and pixmap cursors
;;;
;;; Types and Values
;;;
;;;     GdkCursor
;;;     GdkCursorType
;;;
;;; Accessors
;;;
;;;     gdk_cursor_get_display                              Accessor
;;;     gdk_cursor_get_cursor_type                          Accessor
;;;
;;; Functions
;;;
;;;     gdk_cursor_new                                      Deprecated 3.16
;;;     gdk_cursor_new_from_pixbuf
;;;     gdk_cursor_new_from_surface
;;;     gdk_cursor_new_from_name
;;;     gdk_cursor_new_for_display
;;;     gdk_cursor_get_image
;;;     gdk_cursor_get_surface
;;;
;;;     gdk_cursor_ref                                      not implemented
;;;     gdk_cursor_unref                                    not implemented
;;;
;;; Properties
;;;
;;;     cursor-type
;;;     display
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkCursor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCursorType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkCursorType" cursor-type
  (:export t
   :type-initializer "gdk_cursor_type_get_type")
  (:x-cursor 0)
  (:arrow 2)
  (:based-arrow-down 4)
  (:based-arrow-up 6)
  (:boat 8)
  (:bogosity 10)
  (:bottom-left-corner 12)
  (:bottom-right-corner 14)
  (:bottom-side 16)
  (:bottom-tee 18)
  (:box-spiral 20)
  (:center-ptr 22)
  (:circle 24)
  (:clock 26)
  (:coffee-mug 28)
  (:cross 30)
  (:cross-reverse 32)
  (:crosshair 34)
  (:diamond-cross 36)
  (:dot 38)
  (:dotbox 40)
  (:double-arrow 42)
  (:draft-large 44)
  (:draft-small 46)
  (:draped-box 48)
  (:exchange 50)
  (:fleur 52)
  (:gobbler 54)
  (:gumby 56)
  (:hand1 58)
  (:hand2 60)
  (:heart 62)
  (:icon 64)
  (:iron-cross 66)
  (:left-ptr 68)
  (:left-side 70)
  (:left-tee 72)
  (:leftbutton 74)
  (:ll-angle 76)
  (:lr-angle 78)
  (:man 80)
  (:middlebutton 82)
  (:mouse 84)
  (:pencil 86)
  (:pirate 88)
  (:plus 90)
  (:question-arrow 92)
  (:right-ptr 94)
  (:right-side 96)
  (:right-tee 98)
  (:rightbutton 100)
  (:rtl-logo 102)
  (:sailboat 104)
  (:sb-down-arrow 106)
  (:sb-h-double-arrow 108)
  (:sb-left-arrow 110)
  (:sb-right-arrow 112)
  (:sb-up-arrow 114)
  (:sb-v-double-arrow 116)
  (:shuttle 118)
  (:sizing 120)
  (:spider 122)
  (:spraycan 124)
  (:star 126)
  (:target 128)
  (:tcross 130)
  (:top-left-arrow 132)
  (:top-left-corner 134)
  (:top-right-corner 136)
  (:top-side 138)
  (:top-tee 140)
  (:trek 142)
  (:ul-angle 144)
  (:umbrella 146)
  (:ur-angle 148)
  (:watch 150)
  (:xterm 152)
  (:last-cursor 153)
  (:blank-cursor -2)
  (:cursor-is-pixmap -1))

#+liber-documentation
(setf (liber:alias-for-symbol 'cursor-type)
      "GEnum"
      (liber:symbol-documentation 'cursor-type)
 "@version{2024-6-29}
  @begin{declaration}
(gobject:define-genum \"GdkCursorType\" cursor-type
  (:export t
   :type-initializer \"gdk_cursor_type_get_type\")
  (:x-cursor 0)
  (:arrow 2)
  (:based-arrow-down 4)
  (:based-arrow-up 6)
  (:boat 8)
  (:bogosity 10)
  (:bottom-left-corner 12)
  (:bottom-right-corner 14)
  (:bottom-side 16)
  (:bottom-tee 18)
  (:box-spiral 20)
  (:center-ptr 22)
  (:circle 24)
  (:clock 26)
  (:coffee-mug 28)
  (:cross 30)
  (:cross-reverse 32)
  (:crosshair 34)
  (:diamond-cross 36)
  (:dot 38)
  (:dotbox 40)
  (:double-arrow 42)
  (:draft-large 44)
  (:draft-small 46)
  (:draped-box 48)
  (:exchange 50)
  (:fleur 52)
  (:gobbler 54)
  (:gumby 56)
  (:hand1 58)
  (:hand2 60)
  (:heart 62)
  (:icon 64)
  (:iron-cross 66)
  (:left-ptr 68)
  (:left-side 70)
  (:left-tee 72)
  (:leftbutton 74)
  (:ll-angle 76)
  (:lr-angle 78)
  (:man 80)
  (:middlebutton 82)
  (:mouse 84)
  (:pencil 86)
  (:pirate 88)
  (:plus 90)
  (:question-arrow 92)
  (:right-ptr 94)
  (:right-side 96)
  (:right-tee 98)
  (:rightbutton 100)
  (:rtl-logo 102)
  (:sailboat 104)
  (:sb-down-arrow 106)
  (:sb-h-double-arrow 108)
  (:sb-left-arrow 110)
  (:sb-right-arrow 112)
  (:sb-up-arrow 114)
  (:sb-v-double-arrow 116)
  (:shuttle 118)
  (:sizing 120)
  (:spider 122)
  (:spraycan 124)
  (:star 126)
  (:target 128)
  (:tcross 130)
  (:top-left-arrow 132)
  (:top-left-corner 134)
  (:top-right-corner 136)
  (:top-side 138)
  (:top-tee 140)
  (:trek 142)
  (:ul-angle 144)
  (:umbrella 146)
  (:ur-angle 148)
  (:watch 150)
  (:xterm 152)
  (:last-cursor 153)
  (:blank-cursor -2)
  (:cursor-is-pixmap -1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:x-cursor]{@image[cursor-x-cursor]{}}
      @entry[:arrow]{@image[cursor-arrow]{}}
      @entry[:based-arrow-down]{@image[cursor-based-arrow-down]{}}
      @entry[:based-arrow-up]{@image[cursor-based-arrow-up]{}}
      @entry[:boat]{@image[cursor-boat]{}}
      @entry[:bogosity]{@image[cursor-bogosity]{}}
      @entry[:bottom-left-corner]{@image[cursor-bottom-left-corner]{}}
      @entry[:bottom-right-corner]{@image[cursor-bottom-right-corner]{}}
      @entry[:bottom-side]{@image[cursor-bottom-side]{}}
      @entry[:bottom-tee]{@image[cursor-bottom-tee]{}}
      @entry[:box-spiral]{@image[cursor-box-spiral]{}}
      @entry[:center-ptr]{@image[cursor-center-ptr]{}}
      @entry[:circle]{@image[cursor-circle]{}}
      @entry[:clock]{@image[cursor-clock]{}}
      @entry[:coffee-mug]{@image[cursor-coffee-mug]{}}
      @entry[:cross]{@image[cursor-cross]{}}
      @entry[:cross-reverse]{@image[cursor-cross-reverse]{}}
      @entry[:crosshair]{@image[cursor-crosshair-stock]{}}
      @entry[:diamond-cross]{@image[cursor-diamond-cross]{}}
      @entry[:dot]{@image[cursor-dot]{}}
      @entry[:dotbox]{@image[cursor-dotbox]{}}
      @entry[:double-arrow]{@image[cursor-double-arrow]{}}
      @entry[:draft-large]{@image[cursor-draft-large]{}}
      @entry[:draft-small]{@image[cursor-draft-small]{}}
      @entry[:draped-box]{@image[cursor-draped-box]{}}
      @entry[:exchange]{@image[cursor-exchange]{}}
      @entry[:fleur]{@image[cursor-fleur]{}}
      @entry[:gobbler]{@image[cursor-gobbler]{}}
      @entry[:gumby]{@image[cursor-gumby]{}}
      @entry[:hand1]{@image[cursor-hand1]{}}
      @entry[:hand2]{@image[cursor-hand2]{}}
      @entry[:heart]{@image[cursor-heart]{}}
      @entry[:icon]{@image[cursor-icon]{}}
      @entry[:iron-cross]{@image[cursor-iron-cross]{}}
      @entry[:left-ptr]{@image[cursor-left-ptr]{}}
      @entry[:left-side]{@image[cursor-left-side]{}}
      @entry[:left-tee]{@image[cursor-left-tee]{}}
      @entry[:leftbutton]{@image[cursor-leftbutton]{}}
      @entry[:ll-angle]{@image[cursor-ll-angle]{}}
      @entry[:lr-angle]{@image[cursor-lr-angle]{}}
      @entry[:man]{@image[cursor-man]{}}
      @entry[:middlebutton]{@image[cursor-middlebutton]{}}
      @entry[:mouse]{@image[cursor-mouse]{}}
      @entry[:pencil]{@image[cursor-pencil]{}}
      @entry[pirate:]{@image[cursor-pirate]{}}
      @entry[:plus]{@image[cursor-plus]{}}
      @entry[:question-arrow]{@image[cursor-question-arrow]{}}
      @entry[:right-ptr]{@image[cursor-right-ptr]{}}
      @entry[:right-side]{@image[cursor-right-side]{}}
      @entry[:right-tee]{@image[cursor-right-tee]{}}
      @entry[:rightbutton]{@image[cursor-rightbutton]{}}
      @entry[:rtl-logo]{@image[cursor-rtl-logo]{}}
      @entry[:sailboat]{@image[cursor-sailboat]{}}
      @entry[:sb-down-arrow]{@image[cursor-sb-down-arrow]{}}
      @entry[:sb-h-double-arrow]{@image[cursor-sb-h-double-arrow]{}}
      @entry[:sb-left-arrow]{@image[cursor-sb-left-arrow]{}}
      @entry[:sb-right-arrow]{@image[cursor-sb-right-arrow]{}}
      @entry[:sb-up-arrow]{@image[cursor-sb-up-arrow]{}}
      @entry[:sb-v-double-arrow]{@image[cursor-sb-v-double-arrow]{}}
      @entry[:shuttle]{@image[cursor-shuttle]{}}
      @entry[:sizing]{@image[cursor-sizing]{}}
      @entry[:spider]{@image[cursor-spider]{}}
      @entry[:spraycan]{@image[cursor-spraycan]{}}
      @entry[:star]{@image[cursor-star]{}}
      @entry[:target]{@image[cursor-target]{}}
      @entry[:tcross]{@image[cursor-tcross]{}}
      @entry[:top-left-arrow]{@image[cursor-top-left-arrow]{}}
      @entry[:top-left-corner]{@image[cursor-top-left-corner]{}}
      @entry[:top-right-corner]{@image[cursor-top-right-corner]{}}
      @entry[:top-side]{@image[cursor-top-side]{}}
      @entry[:top-tee]{@image[cursor-top-tee]{}}
      @entry[:trek]{@image[cursor-trek]{}}
      @entry[:ul-angle]{@image[cursor-ul-angle]{}}
      @entry[:umbrella]{@image[cursor-umbrella]{}}
      @entry[:ur-angle]{@image[cursor-ur-angle]{}}
      @entry[:watch]{@image[cursor-watch]{}}
      @entry[:xterm]{@image[cursor-xterm]{}}
      @entry[:last-cursor]{Last cursor type.}
      @entry[:blank-cursor]{Blank cursor.}
      @entry[:cursor-is-pixmap]{Type of cursors constructed with the
        @fun{gdk:cursor-new-from-pixbuf} function.}
    @end{table}
  @end{values}
  @begin{short}
    The standard cursors available.
  @end{short}
  Note that these IDs are directly taken from the X cursor font, and many of
  these cursors are either not useful, or are not available on other platforms.
  The recommended way to create cursors is to use the
  @fun{gdk:cursor-new-from-name} function.
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-from-name}")

;;; ----------------------------------------------------------------------------
;;; GdkCursor
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkCursor" cursor
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_cursor_get_type")
  ((cursor-type
    cursor-cursor-type
    "cursor-type" "GdkCursorType" t t)
   (display
    cursor-display
    "display" "GdkDisplay" t t)))

#-windows
(gobject:define-gobject "GdkX11Cursor" x11-cursor
  (:superclass cursor
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_cursor_get_type")
  nil)

#+liber-documentation
(setf (documentation 'cursor 'type)
 "@version{2024-6-29}
  @begin{short}
    The @class{gdk:cursor} object represents a cursor.
  @end{short}
  There is a number of standard cursors, but it is also possible to construct
  new cursors from pixbufs. There may be limitations as to what kinds of
  cursors can be constructed on a given display.

  Cursors by themselves are not very interesting, they must be bound to a window
  for users to see them. This is done with the @fun{gdk:window-cursor} function
  or by setting the cursor member of the @symbol{gdk:window-attr} instance
  passed to the @fun{gdk:window-new} function.
  @see-constructor{gdk:cursor-new}
  @see-constructor{gdk:cursor-new-from-pixbuf}
  @see-constructor{gdk:cursor-new-from-surface}
  @see-constructor{gdk:cursor-new-from-name}
  @see-constructor{gdk:cursor-new-for-display}
  @see-slot{gdk:cursor-cursor-type}
  @see-slot{gdk:cursor-display}
  @see-class{gdk:display}
  @see-symbol{gdk:window-attr}
  @see-function{gdk:window-new}
  @see-function{gdk:window-cursor}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:cursor-cursor-type -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-type" 'cursor) t)
 "The @code{cursor-type} property of type @symbol{gdk:cursor-type}
  (Read / Write / Construct) @br{}
  Standard cursor type. @br{}
  Default value: @code{:x-cursor}")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-cursor-type)
      "Accessor"
      (documentation 'cursor-cursor-type 'function)
 "@version{2024-6-29}
  @syntax{(gdk:cursor-cursor-type object) => type}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[type]{a value of the @symbol{gdk:cursor-type} enumeration}
  @begin{short}
    Accessor of the @slot[gdk:cursor]{cursor-type} slot of the
    @class{gdk:cursor} class.
  @end{short}
  The @fun{gdk:cursor-cursor-type} function returns the cursor type for the
  cursor. This is a value from the @symbol{gdk:cursor-type} enumeration.
  @see-class{gdk:cursor}
  @see-symbol{gdk:cursor-type}")

;;; --- gdk:cursor-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'cursor) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  Display of the cursor.")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-display)
      "Accessor"
      (documentation 'cursor-display 'function)
 "@version{2024-6-29}
  @syntax{(gdk:cursor-display object) => display}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:cursor]{display} slot of the @class{gdk:cursor}
    class.
  @end{short}
  The @fun{gdk:cursor-display} function returns the display on which the cursor
  is defined.
  @see-class{gdk:cursor}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new" cursor-new) (g:object cursor)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[type]{a value of the @symbol{gdk:cursor-type} enumeration
    with the cursor to create}
  @return{The new @class{gdk:cursor} object.}
  @begin{short}
    Creates a new cursor from the set of builtin cursors for the default
    display. See the @fun{gdk:cursor-new-for-display} function.
  @end{short}
  To make the cursor invisible, use the @code{:blank-cursor} value of the
  @symbol{gdk:cursor-type} enumeration.
  @begin[Warning]{dictionary}
    The @fun{gdk:cursor-new} function has been deprecated since version 3.16
    and should not be used in newly written code. Use the
    @fun{gdk:cursor-new-for-display} function instead.
  @end{dictionary}
  @see-class{gdk:cursor}
  @see-symbol{gdk:cursor-type}
  @see-function{gdk:cursor-new-for-display}"
  (type cursor-type))

(export 'cursor-new)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_from_pixbuf" cursor-new-from-pixbuf)
    (g:object cursor)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[display]{a @class{gdk:display} object for which the cursor will
    be created}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object containing the cursor
    image}
  @argument[x]{an integer with the horizontal offset of the 'hotspot' of the
    cursor}
  @argument[y]{an integer with the vertical offset of the 'hotspot' of the
    cursor}
  @return{The new @class{gdk:cursor} object.}
  @begin{short}
    Creates a new cursor from a pixbuf.
  @end{short}
  Not all GDK backends support RGBA cursors. If they are not supported, a
  monochrome approximation will be displayed. The
  @fun{gdk:display-supports-cursor-alpha} and
  @fun{gdk:display-supports-cursor-color} functions can be used to determine
  whether RGBA cursors are supported. The @fun{gdk:display-default-cursor-size}
  and @fun{gdk:display-maximal-cursor-size} functions give information about
  cursor sizes.

  If the @arg{x} or @arg{y} arguments are -1, the pixbuf must have options named
  @code{\"x_hot\"} and @code{\"y_hot\"} containing integers between 0 and the
  width and height of the pixbuf.

  On the X backend, support for RGBA cursors requires a sufficently new version
  of the X Render extension.
  @see-class{gdk:cursor}
  @see-class{gdk:display}
  @see-function{gdk:display-supports-cursor-alpha}
  @see-function{gdk:display-supports-cursor-color}
  @see-function{gdk:display-default-cursor-size}
  @see-function{gdk:display-maximal-cursor-size}"
  (display (g:object display))
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (x :int)
  (y :int))

(export 'cursor-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_from_surface" %cursor-new-from-surface)
    (g:object cursor)
  (display (g:object display))
  (surface (:pointer (:struct cairo:surface-t)))
  (x :double)
  (y :double))

(defun cursor-new-from-surface (display surface x y)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[display]{a @class{gdk:display} object for which the cursor will
    be created}
  @argument[surface]{a @symbol{cairo:surface-t} instance containing the
    cursor pixel data}
  @argument[x]{a number coerced to a double float with the horizontal offset of
    the 'hotspot' of the cursor}
  @argument[y]{a number coerced to a double float with the vertical offset of
    the 'hotspot' of the cursor}
  @return{The new @class{gdk:cursor} object.}
  @begin{short}
    Creates a new cursor from a cairo image surface.
  @end{short}
  Not all GDK backends support RGBA cursors. If they are not supported, a
  monochrome approximation will be displayed. The
  @fun{gdk:display-supports-cursor-alpha} and
  @fun{gdk:display-supports-cursor-color} functions can be used to determine
  whether RGBA cursors are supported. The @fun{gdk:display-default-cursor-size}
  and @fun{gdk:display-maximal-cursor-size} functions give information about
  cursor sizes.

  On the X backend, support for RGBA cursors requires a sufficently new version
  of the X Render extension.
  @see-class{gdk:cursor}
  @see-class{gdk:display}
  @see-function{gdk:display-supports-cursor-alpha}
  @see-function{gdk:display-supports-cursor-color}
  @see-function{gdk:display-default-cursor-size}
  @see-function{gdk:display-maximal-cursor-size}"
  (%cursor-new-from-surface display
                            surface
                            (coerce x 'double-float)
                            (coerce y 'double-float)))

(export 'cursor-new-from-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_from_name" cursor-new-from-name)
    (g:object cursor)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[display]{a @class{gdk:display} object for which the cursor will be
    created}
  @argument[name]{a string with the name of the cursor}
  @return{The new @class{gdk:cursor} object, or @code{nil} if there is no cursor
    with the given @arg{name}.}
  @begin{short}
    Creates a new cursor by looking up @arg{name} in the current cursor theme.
  @end{short}
  A recommended set of cursor names that will work across different platforms
  can be found in the CSS specification:
  @begin{table}
    @entry[\"none\"]{}
    @entry[\"default\"]{@image[cursor-default]{}}
    @entry[\"help\"]{@image[cursor-help]{}}
    @entry[\"pointer\"]{@image[cursor-pointer]{}}
    @entry[\"context-menu\"]{@image[cursor-context-menu]{}}
    @entry[\"progress\"]{@image[cursor-progress]{}}
    @entry[\"wait\"]{@image[cursor-wait]{}}
    @entry[\"cell\"]{@image[cursor-cell]{}}
    @entry[\"crosshair\"]{@image[cursor-crosshair]{}}
    @entry[\"text\"]{@image[cursor-text]{}}
    @entry[\"vertical-text\"]{@image[cursor-vertical-text]{}}
    @entry[\"alias\"]{@image[cursor-alias]{}}
    @entry[\"copy\"]{@image[cursor-copy]{}}
    @entry[\"no-drop\"]{@image[cursor-no-drop]{}}
    @entry[\"move\"]{@image[cursor-move]{}}
    @entry[\"not-allowed\"]{@image[cursor-not-allowed]{}}
    @entry[\"grab\"]{@image[cursor-grab]{}}
    @entry[\"grabbing\"]{@image[cursor-grabbing]{}}
    @entry[\"all-scroll\"]{@image[cursor-all-scroll]{}}
    @entry[\"col-resize\"]{@image[cursor-col-resize]{}}
    @entry[\"row-resize\"]{@image[cursor-row-resize]{}}
    @entry[\"n-resize\"]{@image[cursor-n-resize]{}}
    @entry[\"e-resize\"]{@image[cursor-e-resize]{}}
    @entry[\"s-resize\"]{@image[cursor-s-resize]{}}
    @entry[\"w-resize\"]{@image[cursor-w-resize]{}}
    @entry[\"ne-resize\"]{@image[cursor-ne-resize]{}}
    @entry[\"nw-resize\"]{@image[cursor-nw-resize]{}}
    @entry[\"sw-resize\"]{@image[cursor-sw-resize]{}}
    @entry[\"se-resize\"]{@image[cursor-se-resize]{}}
    @entry[\"ew-resize\"]{@image[cursor-ew-resize]{}}
    @entry[\"ns-resize\"]{@image[cursor-ns-resize]{}}
    @entry[\"nesw-resize\"]{@image[cursor-nesw-resize]{}}
    @entry[\"nwse-resize\"]{@image[cursor-nwse-resize]{}}
    @entry[\"zoom-in\"]{@image[cursor-zoom-in]{}}
    @entry[\"zoom-out\"]{@image[cursor-zoom-out]{}}
  @end{table}
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:cursor-new-from-name (gdk:display-default) \"wait\")
=> #<GDK-X11-CURSOR {1001AFE123@}>
(gdk:cursor-new-from-name (gdk:display-default) \"unknown\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{gdk:cursor}
  @see-class{gdk:display}"
  (display (g:object display))
  (name :string))

(export 'cursor-new-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_for_display" cursor-new-for-display)
    (g:object cursor)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[display]{a @class{gdk:display} object for which the cursor will
    be created}
  @argument[type]{a value of the @symbol{gdk:cursor-type} enumeration to create
  the cursor from }
  @return{The new @class{gdk:cursor} object.}
  @begin{short}
    Creates a new cursor from the set of builtin cursors.
  @end{short}
  @see-class{gdk:cursor}
  @see-class{gdk:display}
  @see-symbol{gdk:cursor-type}"
  (display (g:object display))
  (type cursor-type))

(export 'cursor-new-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_get_image" cursor-image) (g:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[cursor]{a @class{gdk:cursor} object}
  @return{The @class{gdk-pixbuf:pixbuf} object representing @arg{cursor}, or
    @code{nil}.}
  @begin{short}
    Returns a @class{gdk-pixbuf:pixbuf} object with the image used to display
    the cursor.
  @end{short}
  Note that depending on the capabilities of the windowing system and on the
  cursor, GDK may not be able to obtain the image data. In this case, @code{nil}
  is returned.
  @see-class{gdk:cursor}
  @see-class{gdk-pixbuf:pixbuf}"
  (cursor (g:object cursor)))

(export 'cursor-image)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_get_surface" %cursor-surface)
    (:pointer (:struct cairo:surface-t))
  (cursor (g:object cursor))
  (xhot (:pointer :double))
  (yhot (:pointer :double)))

(defun cursor-surface (cursor)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[cursor]{a @class{gdk:cursor} object}
  @begin{return}
    @arg{surface} -- a @symbol{cairo:surface-t} instance representing a cursor
    @br{}
    @arg{xhot} -- a double float with the hotspot x position @br{}
    @arg{yhot} -- a double float with the hotspot y position
  @end{return}
  @begin{short}
    Returns a Cairo image surface with the image used to display the cursor.
  @end{short}
  Note that depending on the capabilities of the windowing system and on the
  cursor, GDK may not be able to obtain the image data. In this case, @code{nil}
  is returned.
  @see-class{gdk:cursor}
  @see-symbol{cairo:surface-t}"
  (cffi:with-foreign-objects ((xhot :double) (yhot :double))
    (let ((surface (%cursor-surface cursor xhot yhot)))
      (when surface
        (values surface (cffi:mem-ref xhot :double)
                        (cffi:mem-ref yhot :double))))))

(export 'cursor-surface)

;;; --- End of file gdk3.cursor.lisp -------------------------------------------
