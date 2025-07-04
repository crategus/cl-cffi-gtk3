;;; ----------------------------------------------------------------------------
;;; gdk3.monitor.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GdkMonitor
;;;
;;;     Object representing an output
;;;
;;; Types and Values
;;;
;;;     GdkMonitor
;;;     GdkSubpixelLayout
;;;
;;; Functions
;;;
;;;     gdk_monitor_get_display                            Accessor
;;;     gdk_monitor_get_geometry                           Accessor
;;;     gdk_monitor_get_workarea                           Accessor
;;;     gdk_monitor_get_width_mm                           Accessor
;;;     gdk_monitor_get_height_mm                          Accessor
;;;     gdk_monitor_get_manufacturer                       Accessor
;;;     gdk_monitor_get_model                              Accessor
;;;     gdk_monitor_get_scale_factor                       Accessor
;;;     gdk_monitor_get_refresh_rate                       Accessor
;;;     gdk_monitor_get_subpixel_layout                    Accessor
;;;     gdk_monitor_is_primary
;;;
;;; Properties
;;;
;;;     display
;;;     geometry
;;;     height-mm
;;;     manufacturer
;;;     model
;;;     refresh-rate
;;;     scale-factor
;;;     subpixel-layout
;;;     width-mm
;;;     workarea
;;;
;;; Signals
;;;
;;;     invalidate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkMonitor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkSubpixelLayout
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkSubpixelLayout" subpixel-layout
  (:export t
   :type-initializer "gdk_subpixel_layout_get_type")
  :unknown
  :none
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr)

#+liber-documentation
(setf (liber:alias-for-symbol 'subpixel-layout)
      "GEnum"
      (liber:symbol-documentation 'subpixel-layout)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkSubpixelLayout\" subpixel-layout
  (:export t
   :type-initializer \"gdk_subpixel_layout_get_type\")
  :unkown
  :none
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unkown]{The layout is not known.}
      @entry[:none]{Not organized in this way.}
      @entry[:horizontal-rgb]{The layout is horizontal, the order is RGB.}
      @entry[:horizontal-bgr]{The layout is horizontal, the order is BGR.}
      @entry[:vertical-rgb]{The layout is vertical, the order is RGB.}
      @entry[:verticla-bgr]{The layout is vertical, the order is BGR.}
    @end{simple-table}
  @end{values}
  @begin{short}
    This enumeration describes how the red, green and blue components of
    physical pixels on an output device are laid out.
  @end{short}
  Since 3.22
  @see-class{gdk:monitor}")

;;; ----------------------------------------------------------------------------
;;; GdkMonitor
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkMonitor" monitor
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_monitor_get_type")
  ((display
    monitor-display
    "display" "GdkDisplay" t t)
   (geometry
    monitor-geometry
    "geometry" "GdkRectangle" t nil)
   (height-mm
    monitor-height-mm
    "height-mm" "gint" t nil)
   (manufacturer
    monitor-manufacturer
    "manufacturer" "gchararray" t nil)
   (model
    monitor-model
    "model" "gchararray" t nil)
   (refresh-rate
    monitor-refresh-rate
    "refresh-rate" "gint" t nil)
   (scale-factor
    monitor-scale-factor
    "scale-factor" "gint" t nil)
   (subpixel-layout
    monitor-subpixel-layout
    "subpixel-layout" "GdkSubpixelLayout" t nil)
   (width-mm
    monitor-width-mm
    "width-mm" "gint" t nil)
   (workarea
    monitor-workarea
    "workarea" "GdkRectangle" t nil)))

#+liber-documentation
(setf (documentation 'monitor 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{gdk:monitor} object represents the individual outputs that are
    associated with a @class{gdk:display} object.
  @end{short}
  The @class{gdk:display} class has APIs to enumerate monitors with the
  @fun{gdk:display-n-monitors} and @fun{gdk:display-monitor} functions, and to
  find particular monitors with the @fun{gdk:display-primary-monitor}
  or @fun{gdk:display-monitor-at-window} functions.

  The @class{gdk:monitor} class was introduced in GTK 3.22 and supersedes
  earlier APIs in the @class{gdk:screen} class to obtain monitor-related
  information.
  @begin[Signal Details]{dictionary}
    @begin[monitor::invalidate]{signal}
      @begin{pre}
lambda (monitor)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[monitor]{The @class{gdk:monitor} object on which the signal is
          emitted.}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:monitor-display}
  @see-slot{gdk:monitor-geometry}
  @see-slot{gdk:monitor-height-mm}
  @see-slot{gdk:monitor-manufacturer}
  @see-slot{gdk:monitor-model}
  @see-slot{gdk:monitor-refresh-rate}
  @see-slot{gdk:monitor-scale-factor}
  @see-slot{gdk:monitor-subpixel-layout}
  @see-slot{gdk:monitor-width-mm}
  @see-slot{gdk:monitor-workarea}
  @see-class{gdk:display}
  @see-class{gdk:screen}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:monitor-display ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'monitor) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct Only) @br{}
  The display of the monitor.")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-display)
      "Accessor"
      (documentation 'monitor-display 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-display object) => display}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{display} slot of the @class{gdk:monitor}
    class.
  @end{short}
  The @fun{gdk:monitor-display} function gets the display that the monitor
  belongs to.

  Since 3.22
  @see-class{gdk:monitor}
  @see-class{gdk:display}")

;;; --- gdk:monitor-geometry ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "geometry" 'monitor) t)
 "The @code{geometry} property of type @class{gdk:rectangle} (Read) @br{}
  The geometry of the monitor.")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-geometry)
      "Accessor"
      (documentation 'monitor-geometry 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-geometry object) => geometry}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[geometry]{a @class{gdk:rectangle} instance with the monitor
    geometry}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{geometry} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-geometry} function retrieves the size and position of an
  individual monitor within the display coordinate space. The returned geometry
  is in \"application pixels\", not in \"device pixels\". See the
  @fun{gdk:monitor-scale-factor} function for the internal scale factor that
  maps from monitor coordinates to device pixels.

  Since 3.22
  @see-class{gdk:monitor}
  @see-class{gdk:rectangle}
  @see-function{gdk:monitor-scale-factor}")

;;; --- gdk:monitor-height-mm --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height-mm" 'monitor) t)
 "The @code{height-mm} property of type @code{:int} (Read) @br{}
  The height of the monitor, in millimeters. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-height-mm)
      "Accessor"
      (documentation 'monitor-height-mm 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-height-mm object) => height}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[height]{an integer with the physical height of the monitor}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{height-mm} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-height-mm} function gets the height in millimeters of
  the monitor.

  Since 3.22
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-manufacturer -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "manufacturer" 'monitor) t)
 "The @code{manufacturer} property of type @code{:string} (Read) @br{}
  The manufacturer name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-manufacturer)
      "Accessor"
      (documentation 'monitor-manufacturer 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-manufacturer object) => manufacturer}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[manufacturer]{a string with the name of the manufacturer, or
    @code{nil}}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{manufacturer} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-manufacturer} function gets the name of the monitor's
  manufacturer, if available.

  Since 3.22
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-model ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'monitor) t)
 "The @code{model} property of type @code{:string} (Read) @br{}
  The model name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-model)
      "Accessor"
      (documentation 'monitor-model 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-model object) => model}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[model]{a string with the monitor model, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{model} slot of the @class{gdk:monitor}
    class.
  @end{short}
  The @fun{gdk:monitor-model} function gets a string identifying the monitor
  model, if available.

  Since 3.22
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-refresh-rate -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "refresh-rate" 'monitor) t)
 "The @code{refresh-rate} property of type @code{:int} (Read) @br{}
  The refresh rate, in millihertz. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-refresh-rate)
      "Accessor"
      (documentation 'monitor-refresh-rate 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-refresh-rate object) => refresh-rate}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[refresh-rate]{an integer with the refresh rate in milli-Hertz, or 0}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{refresh-rate} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-refresh-rate} function gets the refresh rate of the
  monitor, if available. The value is in milli-Hertz, so a refresh rate of
  60 Hz is returned as 60000.

  Since 3.22
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-scale-factor -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-factor" 'monitor) t)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-scale-factor)
      "Accessor"
      (documentation 'monitor-scale-factor 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-scale-factor object) => scale-factor}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[scale-factor]{an integer with the scale factor}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{scale-factor} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-scale-factor} function gets the internal scale factor
  that maps from monitor coordinates to the actual device pixels. On traditional
  systems this is 1, but on very high density outputs this can be a higher value
  (often 2).

  This can be used if you want to create pixel based data for a particular
  monitor. But most of the time you are drawing to a window where it is better
  to use the @fun{gdk:window-scale-factor} function instead.

  Since 3.22
  @see-class{gdk:monitor}
  @see-function{gdk:window-scale-factor}")

;;; --- gdk:monitor-subpixel-layout --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "subpixel-layout" 'monitor) t)
 "The @code{subpixel-layout} property of type @symbol{gdk:subpixel-layout}
  (Read) @br{}
  The subpixel layout. @br{}
  Default value: @code{:unknown}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-subpixel-layout)
      "Accessor"
      (documentation 'monitor-subpixel-layout 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-subpixel-layout object) => layout}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[layout]{a @symbol{gdk:subpixel-layout} value with the subpixel
    layout}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{subpixel-layout} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-subpixel-layout} function gets information about the
  layout of red, green and blue primaries for each pixel in this monitor, if
  available.

  Since 3.22
  @see-class{gdk:monitor}
  @see-symbol{gdk:subpixel-layout}")

;;; --- gdk:monitor-width-mm ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-mm" 'monitor) t)
 "The @code{width-mm} property of type @code{:int} (Read) @br{}
  The width of the monitor, in millimeters. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-width-mm)
      "Accessor"
      (documentation 'monitor-width-mm 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-width-mm object) => width}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[width]{an integer with the physical width of the monitor}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{width-mm} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-width-mm} function gets the width in millimeters of the
  monitor.

  Since 3.22
  @see-class{gdk:monitor}")

;;; --- gkd:monitor-workarea ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "workarea" 'monitor) t)
 "The @code{workarea} property of type @class{gdk:rectangle} (Read) @br{}
  The workarea of the monitor.")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-workarea)
      "Accessor"
      (documentation 'monitor-workarea 'function)
 "@version{#2023-3-6}
  @syntax{(gdk:monitor-workarea object) => workarea}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[workarea]{a @class{gdk:rectangle} instance with the monitor
    workarea}
  @begin{short}
    Accessor of the @slot[gdk:monitor]{workarea} slot of the
    @class{gdk:monitor} class.
  @end{short}
  The @fun{gdk:monitor-workarea} function retrieves the size and position of the
  \"work area\" on a monitor within the display coordinate space. The returned
  geometry is in \"application pixels\", not in \"device pixels\". See the
  @fun{gdk:monitor-scale-factor} function for the internal scale factor that
  maps from monitor coordinates to device pixels.

  The work area should be considered when positioning menus and similar popups,
  to avoid placing them below panels, docks or other desktop components.

  Note that not all backends may have a concept of workarea. This function will
  return the monitor geometry if a workarea is not available, or does not apply.

  Since 3.22
  @see-class{gdk:monitor}
  @see-class{gdk:rectangle}
  @see-function{gdk:monitor-scale-factor}")

;;; ----------------------------------------------------------------------------
;;; gdk_monitor_is_primary
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_monitor_is_primary" monitor-is-primary) :boolean
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[monitor]{a @class{gdk:monitor} object}
  @return{@em{True} if @arg{monitor} is the primary monitor.}
  @begin{short}
    Gets whether the monitor should be considered primary.
  @end{short}
  See the @fun{gdk:display-primary-monitor} function.

  Since 3.22
  @see-class{gdk:monitor}
  @see-function{gdk:display-primary-monitor}"
  (monitor (g:object monitor)))

(export 'monitor-is-primary)

;;; --- End of file gdk3.monitor.lisp ------------------------------------------
