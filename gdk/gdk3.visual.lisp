;;; ----------------------------------------------------------------------------
;;; gdk3.visual.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; Visuals
;;;
;;;     Low-level display hardware information
;;;
;;; Types and Values
;;;
;;;     GdkVisual
;;;     GdkVisualType
;;;     GdkByteOrder
;;;
;;; Functions
;;;
;;;     gdk_query_depths                                   deprecated
;;;     gdk_query_visual_types                             deprecated
;;;     gdk_list_visuals                                   deprecated
;;;     gdk_visual_get_bits_per_rgb                        deprecated
;;;     gdk_visual_get_blue_pixel_details
;;;     gdk_visual_get_byte_order                          deprecated
;;;     gdk_visual_get_colormap_size                       deprecated
;;;     gdk_visual_get_depth
;;;     gdk_visual_get_green_pixel_details
;;;     gdk_visual_get_red_pixel_details
;;;     gdk_visual_get_visual_type
;;;     gdk_visual_get_best_depth                          deprecated
;;;     gdk_visual_get_best_type                           deprecated
;;;     gdk_visual_get_system                              deprecated
;;;     gdk_visual_get_best                                deprecated
;;;     gdk_visual_get_best_with_depth                     deprecated
;;;     gdk_visual_get_best_with_type                      deprecated
;;;     gdk_visual_get_best_with_both                      deprecated
;;;     gdk_visual_get_screen
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkVisual
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkVisualType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkVisualType" visual-type
  (:export t
   :type-initializer "gdk_visual_type_get_type")
  (:static-gray 0)
  (:grayscale 1)
  (:static-color 2)
  (:pseudo-color 3)
  (:true-color 4)
  (:direct-color 5))

#+liber-documentation
(setf (liber:alias-for-symbol 'visual-type)
      "GEnum"
      (liber:symbol-documentation 'visual-type)
 "@version{#2021-12-14}
  @begin{short}
    A set of values that describe the manner in which the pixel values for a
    visual are converted into RGB values for display.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkVisualType\" visual-type
  (:export t
   :type-initializer \"gdk_visual_type_get_type\")
  (:static-gray 0)
  (:grayscale 1)
  (:static-color 2)
  (:pseudo-color 3)
  (:true-color 4)
  (:direct-color 5))
  @end{pre}
  @begin[code]{table}
    @entry[:static-gray]{Each pixel value indexes a grayscale value directly.}
    @entry[:grayscale]{Each pixel is an index into a color map that maps pixel
      values into grayscale values. The color map can be changed by an
      application.}
    @entry[:static-color]{Each pixel value is an index into a predefined,
      unmodifiable color map that maps pixel values into RGB values.}
    @entry[:pseudo-color]{Each pixel is an index into a color map that maps
      pixel values into RGB values. The color map can be changed by an
      application.}
    @entry[:true-color]{Each pixel value directly contains red, green, and blue
      components. Use the @fun{gdk:visual-red-pixel-details} function, etc, to
      obtain information about how the components are assembled into a pixel
      value.}
    @entry[:direct-color]{Each pixel value contains red, green, and blue
      components as for @code{:true-color}, but the components are mapped via a
      color table into the final output table instead of being converted
      directly.}
  @end{table}
  @see-class{gdk:visual}
  @see-function{gdk:visual-red-pixel-details}
  @see-function{gdk:visual-blue-pixel-details}
  @see-function{gdk:visual-green-pixel-details}")

;;; ----------------------------------------------------------------------------
;;; enum GdkByteOrder
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkByteOrder" byte-order
  (:export t
   :type-initializer "gdk_byte_order_get_type")
  (:lsb-first 0)
  (:msb-first 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'byte-order)
      "GEnum"
      (liber:symbol-documentation 'byte-order)
 "@version{#2021-12-14}
  @begin{short}
    A set of values describing the possible byte-orders for storing pixel
    values in memory.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkByteOrder\" byte-order
  (:export t
   :type-initializer \"gdk_byte_order_get_type\")
  (:lsb-first 0)
  (:msb-first 1))
  @end{pre}
  @begin[code]{table}
    @entry[:lsb-first]{The values are stored with the least-significant byte
      first. For instance, the 32-bit value 0xffeecc would be stored in memory
      as 0xcc, 0xee, 0xff, 0x00.}
    @entry[:msb-first]{The values are stored with the most-significant byte
      first. For instance, the 32-bit value 0xffeecc would be stored in memory
      as 0x00, 0xcc, 0xee, 0xff.}
  @end{table}
  @see-class{gdk:visual}
  @see-function{gdk:visual-byte-order}")

;;; ----------------------------------------------------------------------------
;;; GdkVisual
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkVisual" visual
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_visual_get_type")
  nil)

(setf (documentation 'visual 'type)
 "@version{#2021-12-14}
  @begin{short}
    A @class{gdk:visual} object describes a particular video hardware display
    format.
  @end{short}
  It includes information about the number of bits used for each color, the way
  the bits are translated into an RGB value for display, and the way the bits
  are stored in memory. For example, a piece of display hardware might support
  24-bit color, 16-bit color, or 8-bit color; meaning 24/16/8-bit pixel sizes.
  For a given pixel size, pixels can be in different formats; for example the
  \"red\" element of an RGB pixel may be in the top 8 bits of the pixel, or
  may be in the lower 4 bits.

  There are several standard visuals. The visual returned by the
  @fun{gdk:screen-system-visual} function is the default visual of the system.

  A number of functions are provided for determining the \"best\" available
  visual. For the purposes of making this determination, higher bit depths are
  considered better, and for visuals of the same bit depth,
  @code{:pseudo-color} is preferred at 8bpp, otherwise, the visual types
  are ranked in the order of highest to lowest @code{:direct-color},
  @code{:true-color}, @code{:pseudo-color}, @code{:static-color},
  @code{:grayscale}, then @code{:static-gray}.
  @see-function{gdk:screen-system-visual}")

;;; ----------------------------------------------------------------------------

(defmethod print-object ((visual visual) stream)
  (print-unreadable-object (visual stream :type t :identity t)
    (format stream "~S at ~S bpp"
                   (visual-visual-type visual)
                   (visual-depth visual))))

;;; ----------------------------------------------------------------------------
;;; gdk_query_depths ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_query_depths" %query-depths) :void
  (depths (:pointer (:pointer :int)))
  (count (:pointer :int)))

(defun query-depths ()
 #+liber-documentation
 "@version{#2021-12-14}
  @return{A list of integers of the available depths.}
  @begin{short}
    This function returns the available bit depths for the default screen.
  @end{short}
  It is equivalent to listing the visuals with the @fun{gdk:list-visuals}
  function and then looking at the depth field in each visual, removing
  duplicates.
  @begin[Warning]{dictionary}
    The @fun{gdk:query-depths} function has been deprecated since version 3.22
    and should not be used in newly written code. Visual selection should be
    done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:query-depths)
=> (32 24)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:list-visuals}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}"
  (cffi:with-foreign-objects ((count-r :int) (depths-r :pointer))
    (%query-depths depths-r count-r)
    (iter (with count = (cffi:mem-ref count-r :int))
          (with depths = (cffi:mem-ref depths-r :pointer))
          (for i from 0 below count)
          (collect (cffi:mem-aref depths :int i)))))

(export 'query-depths)

;;; ----------------------------------------------------------------------------
;;; gdk_query_visual_types ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_query_visual_types" %query-visual-types) :void
  (depths (:pointer (:pointer visual-type)))
  (count (:pointer :int)))

(defun query-visual-types ()
 #+liber-documentation
 "@version{#2021-12-14}
  @return{A list of the available visual types of type
    @symbol{gdk:visual-type}.}
  @begin{short}
    This function returns the available visual types for the default screen.
  @end{short}
  It is equivalent to listing the visuals with the @fun{gdk:list-visuals}
  function and then looking at the type field in each visual, removing
  duplicates.
  @begin[Warning]{dictionary}
    The @fun{gdk:query-visual-types} function has been deprecated since version
    3.22 and should not be used in newly written code. Visual selection should
    be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-symbol{gdk:visual-type}
  @see-function{gdk:list-visuals}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}"
  (cffi:with-foreign-objects ((count-r :int) (types-r 'visual-type))
    (%query-visual-types types-r count-r)
    (iter (with count = (cffi:mem-ref count-r :int))
          (with types = (cffi:mem-ref types-r :pointer))
          (for i from 0 below count)
          (collect (cffi:mem-aref types 'visual-type i)))))

(export 'query-visual-types)

;;; ----------------------------------------------------------------------------
;;; gdk_list_visuals ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_list_visuals" list-visuals)
    (g:list-t (g:object visual) :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-14}
  @return{A list of @class{gdk:visual} objects.}
  @begin{short}
    Lists the available visuals for the default screen.
  @end{short}
  A visual describes a hardware image data format. See the
  @fun{gdk:screen-list-visuals} function.

  For example, a visual might support 24-bit color, or 8-bit color, and might
  expect pixels to be in a certain format.
  @begin[Warning]{dictionary}
    The @fun{gdk:list-visuals} function has been deprecated since version 3.22
    and should not be used in newly written code. Use the call
    @code{(gdk:screen-list-visuals (gdk:screen-default))}.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:screen-list-visuals}")

(export 'list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_bits_per_rgb () -> visual-bits-per-rgb
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_bits_per_rgb" visual-bits-per-rgb) :int
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{An integer with the number of significant bits per color value for
    visual.}
  @begin{short}
    Returns the number of significant bits per red, green and blue value.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-bits-per-rgb} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:visual-red-pixel-details} function and its variants to learn about
    the pixel layout of TrueColor and DirectColor visuals.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-red-pixel-details}
  @see-function{gdk:visual-blue-pixel-details}
  @see-function{gdk:visual-green-pixel-details}"
  (visual (g:object visual)))

(export 'visual-bits-per-rgb)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_blue_pixel_details () -> visual-blue-pixel-details
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_blue_pixel_details" %visual-blue-pixel-details)
    :void
  (visual (g:object visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun visual-blue-pixel-details (visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @begin{return}
    @arg{mask} -- an unsigned integer, or @code{nil} @br{}
    @arg{shift} -- an integer, or @code{nil} @br{}
    @arg{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate blue pixel values in TrueColor
    and DirectColor.
  @end{short}
  The @arg{mask} argument is the significant bits within the pixel. @arg{shift}
  is the number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk:visual}
  @see-function{gdk:visual-red-pixel-details}
  @see-function{gdk:visual-green-pixel-details}"
  (cffi:with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%visual-blue-pixel-details visual mask shift precision)
    (values (cffi:mem-ref mask :uint32)
            (cffi:mem-ref shift :int)
            (cffi:mem-ref precision :int))))

(export 'visual-blue-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_byte_order () -> visual-byte-order
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_byte_order" visual-byte-order) byte-order
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{A @symbol{gdk:byte-order} value stating the byte order of
    @arg{visual}.}
  @begin{short}
    Returns the byte order of the visual.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-byte-order} function has been deprecated since version
    3.22 and should not be used in newly written code. This information is not
    useful.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-symbol{gdk:byte-order}"
  (visual (g:object visual)))

(export 'visual-byte-order)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_colormap_size () -> visual-colormap-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_colormap_size" visual-colormap-size) :int
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{An integer with the size of a colormap that is suitable for
    @arg{visual}.}
  @begin{short}
    Returns the size of a colormap for the visual.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-colormap-size} function has been deprecated since
    version 3.22 and should not be used in newly written code. This information
    is not useful, since GDK does not provide APIs to operate on colormaps.
  @end{dictionary}
  @see-class{gdk:visual}"
  (visual (g:object visual)))

(export 'visual-colormap-size)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_depth () -> visual-depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_depth" visual-depth) :int
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{An integer with the bit depth of @arg{visual}.}
  @short{Returns the bit depth of the visual.}
  @see-class{gdk:visual}"
  (visual (g:object visual)))

(export 'visual-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_green_pixel_details ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_green_pixel_details" %visual-green-pixel-details)
    :void
  (visual (g:object visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun visual-green-pixel-details (visual)
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @begin{return}
    @arg{mask} -- an unsigned integer, or @code{nil} @br{}
    @arg{shift} -- an integer, or @code{nil} @br{}
    @arg{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate green pixel values in TrueColor
    and DirectColor.
  @end{short}
  The @arg{mask} argument is the significant bits within the pixel. @arg{shift}
  is the number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk:visual}
  @see-function{gdk:visual-red-pixel-details}
  @see-function{gdk:visual-blue-pixel-details}"
  (cffi:with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%visual-green-pixel-details visual mask shift precision)
    (values (cffi:mem-ref mask :uint32)
            (cffi:mem-ref shift :int)
            (cffi:mem-ref precision :int))))

(export 'visual-green-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_red_pixel_details () -> visual-red-pixel-details
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_red_pixel_details" %visual-red-pixel-details)
    :void
  (visual (g:object visual))
  (mask (:pointer :uint32))
  (shift (:pointer :int))
  (precision (:pointer :int)))

(defun visual-red-pixel-details (visual)
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @begin{return}
    @arg{mask} -- an unsigned integer, or @code{nil} @br{}
    @arg{shift} -- an integer, or @code{nil} @br{}
    @arg{precision} -- an integer, or @code{nil}
  @end{return}
  @begin{short}
    Obtains values that are needed to calculate red pixel values in TrueColor
    and DirectColor.
  @end{short}
  The @arg{mask} argument is the significant bits within the pixel. @arg{shift}
  is the number of bits left we must shift a primary for it to be in position
  according to @arg{mask}. Finally, @arg{precision} refers to how much
  precision the pixel value contains for a particular primary.
  @see-class{gdk:visual}
  @see-function{gdk:visual-blue-pixel-details}
  @see-function{gdk:visual-green-pixel-details}"
  (cffi:with-foreign-objects ((mask :uint32) (shift :int) (precision :int))
    (%visual-red-pixel-details visual mask shift precision)
    (values (cffi:mem-ref mask :uint32)
            (cffi:mem-ref shift :int)
            (cffi:mem-ref precision :int))))

(export 'visual-red-pixel-details)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_visual_type () -> visual-visual-type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_visual_type" visual-visual-type) visual-type
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{A @symbol{gdk:visual-type} value stating the type of @arg{visual}.}
  @begin{short}
    Returns the type of visual this is (PseudoColor, TrueColor, etc).
  @end{short}
  @see-class{gdk:visual}
  @see-symbol{gdk:visual-type}"
  (visual (g:object visual)))

(export 'visual-visual-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_depth () -> visual-best-depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best_depth" visual-best-depth) :int
 #+liber-documentation
 "@version{#2021-12-14}
  @return{An integer with the best available depth.}
  @begin{short}
    Get the best available depth for the default GDK screen.
  @end{short}
  \"Best\" means \"largest\", i.e. 32 preferred over 24 preferred over 8 bits
  per pixel.
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best-depth} function has been deprecated since version
    3.22 and should not be used in newly written code. Visual selection should
    be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-best-type}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}")

(export 'visual-best-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_type () -> visual-best-type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best_type" visual-best-type) visual-type
 #+liber-documentation
 "@version{#2021-12-14}
  @return{Best visual type of type @symbol{gdk:visual-type}.}
  @begin{short}
    Return the best available visual type for the default GDK screen.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best-type} function has been deprecated since version
    3.22 and should not be used in newly written code. Visual selection should
    be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-best-depth}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}")

(export 'visual-best-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_system () -> visual-system
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_system" visual-system) (g:object visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @return{The system @class{gdk:visual} object.}
  @begin{short}
    Get the default visual of the system for the default GDK screen.
  @end{short}
  This is the visual for the root window of the display.
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-system} function has been deprecated since version 3.22
    and should not be used in newly written code. Use the call
    @code{(gdk:screen-system-visual (gdk:screen-default))}.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-best}
  @see-function{gdk:screen-default}
  @see-function{gdk:screen-system-visual}")

(export 'visual-system)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best () -> visual-best
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best" visual-best) (g:object visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @return{The best @class{gdk:visual} object.}
  @begin{short}
    Get the visual with the most available colors for the default GDK screen.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best} function has been deprecated since version 3.22
    and should not be used in newly written code. Visual selection should be
    done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-system}
  @see-function{gdk:visual-best-with-depth}
  @see-function{gdk:visual-best-with-type}
  @see-function{gdk:visual-best-with-both}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}")

(export 'visual-best)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_depth () -> visual-best-with-depth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best_with_depth" visual-best-with-depth)
    (g:object visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[depth]{an integer with the bit depth}
  @return{The best @class{gdk:visual} object for the given @arg{depth}.}
  @begin{short}
    Get the best visual with depth for the default GDK screen.
  @end{short}
  Color visuals and visuals with mutable colormaps are preferred over grayscale
  or fixed-colormap visuals. @code{Nil} may be returned if no visual supports
  @arg{depth}.
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best-with-depth} function has been deprecated since
    version 3.22 and should not be used in newly written code. Visual selection
    should be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-system}
  @see-function{gdk:visual-best}
  @see-function{gdk:visual-best-with-type}
  @see-function{gdk:visual-best-with-both}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}"
  (depth :int))

(export 'visual-best-with-depth)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_type () -> visual-best-with-type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best_with_type" visual-best-with-type)
    (g:object visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual-type]{a value of the @symbol{gdk:visual-type} enumeration}
  @return{The best @class{gdk:visual} object of the given @arg{visual-type}.}
  @begin{short}
    Get the best visual of the given visual type for the default GDK screen.
  @end{short}
  Visuals with higher color depths are considered better. @code{Nil} may be
  returned if no visual has type @arg{visual-type}.
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best-with-type} function has been deprecated since
    version 3.22 and should not be used in newly written code. Visual selection
    should be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-system}
  @see-function{gdk:visual-best}
  @see-function{gdk:visual-best-with-depth}
  @see-function{gdk:visual-best-with-both}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-rgba-visual}"
  (visual-type visual-type))

(export 'visual-best-with-type)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_best_with_both () -> visual-best-with-both
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_best_with_both" visual-best-with-both)
    (g:object visual)
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[depth]{an integer with the bit depth}
  @argument[visual-type]{a value of the @symbol{gdk:visual-type} enumeration}
  @return{The best @class{gdk:visual} object with both @arg{depth} and
    @arg{visual-type}, or @code{nil} if none.}
  @begin{short}
    Combines the @fun{gdk:visual-best-with-depth} and
    @fun{gdk:visual-best-with-type} functions.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:visual-best-with-both} function has been deprecated since
    version 3.22 and should not be used in newly written code. Visual selection
    should be done using the @fun{gdk:screen-system-visual} and
    @fun{gdk:screen-rgba-visual} functions.
  @end{dictionary}
  @see-class{gdk:visual}
  @see-function{gdk:visual-system}
  @see-function{gdk:visual-best}
  @see-function{gdk:visual-best-with-depth}
  @see-function{gdk:visual-best-with-type}
  @see-function{gdk:screen-system-visual}"
  (depth :int)
  (visual-type visual-type))

(export 'visual-best-with-both)

;;; ----------------------------------------------------------------------------
;;; gdk_visual_get_screen () -> visual-screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_visual_get_screen" visual-screen) (g:object screen)
 #+liber-documentation
 "@version{#2021-12-14}
  @argument[visual]{a @class{gdk:visual} object}
  @return{The @class{gdk:screen} object to which @arg{visual} belongs.}
  @short{Gets the screen to which the visual belongs.}
  @see-class{gdk:visual}
  @see-class{gdk:screen}"
  (visual (g:object visual)))

(export 'visual-screen)

;;; --- End of file gdk3.visual.lisp -------------------------------------------
