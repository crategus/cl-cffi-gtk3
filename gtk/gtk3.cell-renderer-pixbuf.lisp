;;; ----------------------------------------------------------------------------
;;; gtk3.cell-renderer-pixbuf.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkCellRendererPixbuf
;;;
;;;     Renders a pixbuf in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererPixbuf
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_pixbuf_new
;;;
;;; Properties
;;;
;;;     follow-state
;;;     gicon
;;;     icon-name
;;;     pixbuf
;;;     pixbuf-expander-closed
;;;     pixbuf-expander-open
;;;     stock-detail
;;;     stock-id
;;;     stock-size
;;;     surface
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererPixbuf
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererPixbuf
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCellRendererPixbuf" cell-renderer-pixbuf
  (:superclass cell-renderer
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_pixbuf_get_type")
  ((follow-state
    cell-renderer-pixbuf-follow-state
    "follow-state" "gboolean" t t)
   (gicon
    cell-renderer-pixbuf-gicon
    "gicon" "GIcon" t t)
   (icon-name
    cell-renderer-pixbuf-icon-name
    "icon-name" "gchararray" t t)
   (pixbuf
    cell-renderer-pixbuf-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-expander-closed
    cell-renderer-pixbuf-pixbuf-expander-closed
    "pixbuf-expander-closed" "GdkPixbuf" t t)
   (pixbuf-expander-open
    cell-renderer-pixbuf-pixbuf-expander-open
    "pixbuf-expander-open" "GdkPixbuf" t t)
   (stock-detail
    cell-renderer-pixbuf-stock-detail
    "stock-detail" "gchararray" t t)
   (stock-id
    cell-renderer-pixbuf-stock-id
    "stock-id" "gchararray" t t)
   (stock-size
    cell-renderer-pixbuf-stock-size
    "stock-size" "guint" t t)
   (surface
    cell-renderer-pixbuf-surface
    "surface" "CairoSurface" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-pixbuf 'type)
 "@version{2024-3-17}
  @begin{short}
    The @class{gtk:cell-renderer-pixbuf} object can be used to render an image
    in a cell.
  @end{short}
  It allows to render either a given @class{gdk-pixbuf:pixbuf} object, set via
  the @slot[gtk:cell-renderer-pixbuf]{pixbuf} property, or a named icon, set
  via the @slot[gtk:cell-renderer-pixbuf]{icon-name} property.

  To support the tree view, the @class{gtk:cell-renderer-pixbuf} object also
  supports rendering two alternative pixbufs, when the
  @slot[gtk:cell-renderer]{is-expander} property is @em{true}. If the
  @slot[gtk:cell-renderer]{is-expanded} property is @em{true} and the
  @slot[gtk:cell-renderer-pixbuf]{pixbuf-expander-open} property is set to a
  pixbuf, it renders that pixbuf, if the @slot[gtk:cell-renderer]{is-expanded}
  property is @em{false} and the
  @slot[gtk:cell-renderer-pixbuf]{pixbuf-expander-closed} property is set to a
  pixbuf, it renders that one.
  @see-constructor{gtk:cell-renderer-pixbuf-new}
  @see-slot{gtk:cell-renderer-pixbuf-follow-state}
  @see-slot{gtk:cell-renderer-pixbuf-gicon}
  @see-slot{gtk:cell-renderer-pixbuf-icon-name}
  @see-slot{gtk:cell-renderer-pixbuf-pixbuf}
  @see-slot{gtk:cell-renderer-pixbuf-pixbuf-expander-closed}
  @see-slot{gtk:cell-renderer-pixbuf-pixbuf-expander-open}
  @see-slot{gtk:cell-renderer-pixbuf-stock-detail}
  @see-slot{gtk:cell-renderer-pixbuf-stock-id}
  @see-slot{gtk:cell-renderer-pixbuf-stock-size}
  @see-class{gtk:cell-renderer}
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-pixbuf-follow-state ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "follow-state"
                                               'cell-renderer-pixbuf) t)
 "The @code{follow-state} property of type @code{:boolean} (Read / Write) @br{}
  Specifies whether the rendered pixbuf should be colorized according to the
  @symbol{gtk:cell-renderer-state} flags. @br{}
  @em{Warning:} The @code{follow-state} property has been deprecated since
  version 3.16 and should not be used in newly written code. Cell renderers
  always follow state. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-follow-state)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-follow-state 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-follow-state object) => state}
  @syntax{(setf (gtk:cell-renderer-pixbuf-follow-state object) state)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[state]{a boolean whether the rendered pixbuf should be colorized}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{follow-state} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  Specifies whether the rendered pixbuf should be colorized according to the
  @symbol{gtk:cell-renderer-state} flags.
  @begin[Warning]{dictionary}
    The @slot[gtk:cell-renderer-pixbuf]{follow-state} property has been
    deprecated since version 3.16 and should not be used in newly written code.
    Cell renderers always follow state.
  @end{dictionary}
  @see-class{gtk:cell-renderer-pixbuf}
  @see-symbol{gtk:cell-renderer-state}")

;;; --- gtk:cell-renderer-pixbuf-gicon -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'cell-renderer-pixbuf) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  Represents the icon to display. If the icon theme is changed, the image will
  be updated automatically.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-gicon)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-gicon 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-gicon object) => icon}
  @syntax{(setf (gtk:cell-renderer-pixbuf-gicon object) icon)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{gicon} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The @class{g:icon} object representing the icon to display. If the icon theme
  is changed, the image will be updated automatically.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{g:icon}")

;;; --- gtk:cell-renderer-pixbuf-icon-name -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name"
                                               'cell-renderer-pixbuf) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon to display. This property only has an effect if
  not overridden by the @code{stock-id} or @code{pixbuf} properties. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-icon-name)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-icon-name 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-icon-name object) => name}
  @syntax{(setf (gtk:cell-renderer-pixbuf-icon-name object) name)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[name]{a string with the name of the themed icon to display}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{icon-name} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The name of the themed icon to display. This property only has an effect if
  not overridden by the @slot[gtk:cell-renderer-pixbuf]{stock-id} or
  @slot[gtk:cell-renderer-pixbuf]{pixbuf} properties.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-function{gtk:cell-renderer-pixbuf-stock-id}
  @see-function{gtk:cell-renderer-pixbuf-pixbuf}")

;;; --- gtk:cell-renderer-pixbuf-pixbuf ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf"
                                               'cell-renderer-pixbuf) t)
 "The @code{pixbuf} property of type  @class{gdk-pixbuf:pixbuf} (Read / Write)
  @br{}
  The pixbuf to render.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-pixbuf)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-pixbuf 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-pixbuf object) => pixbuf}
  @syntax{(setf (gtk:cell-renderer-pixbuf-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{pixbuf} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The pixbuf to render.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:cell-renderer-pixbuf-pixbuf-expander-closed ------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf-expander-closed"
                                               'cell-renderer-pixbuf) t)
 "The @code{pixbuf-expander-closed} property of type @class{gdk-pixbuf:pixbuf}
  (Read / Write) @br{}
  Pixbuf for closed expander.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-pixbuf-expander-closed)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-pixbuf-expander-closed 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-pixbuf-expander-closed object) => pixbuf}
  @syntax{(setf (gtk:cell-renderer-pixbuf-pixbuf-expander-closed object) pixbuf)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{pixbuf-expander-closed}
    slot of the @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  Pixbuf for closed expander.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:cell-renderer-pixbuf-pixbuf-expander-open --------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf-expander-open"
                                               'cell-renderer-pixbuf) t)
 "The @code{pixbuf-expander-open} property of type @class{gdk-pixbuf:pixbuf}
  (Read / Write) @br{}
  Pixbuf for open expander.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-pixbuf-expander-open)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-pixbuf-expander-open 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-pixbuf-expander-open object) => pixbuf}
  @syntax{(setf (gtk:cell-renderer-pixbuf-pixbuf-expander-open object) pixbuf)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{pixbuf-expander-open} slot
    of the @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The pixbuf for open expander.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:cell-renderer-pixbuf-stock-detail ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-detail"
                                               'cell-renderer-pixbuf) t)
 "The @code{stock-detail} property of type @code{:string} (Read / Write) @br{}
  Render detail to pass to the theme engine. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-stock-detail)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-stock-detail 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-stock-detail object) => detail}
  @syntax{(setf (gtk:cell-renderer-pixbuf-stock-detail object) detail)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[detail]{a string with the render detail.}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{stock-detail} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  Render detail to pass to the theme engine.
  @see-class{gtk:cell-renderer-pixbuf}")

;;; --- gtk:cell-renderer-pixbuf-stock-id --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-id"
                                               'cell-renderer-pixbuf) t)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock ID of the stock icon to render. @br{}
  @em{Warning:} The @slot[gtk:cell-renderer-pixbuf]{stock-id} property has been
  deprecated since version 3.10 and should not be used in newly written code.
  Use the @slot[gtk:cell-renderer-pixbuf]{icon-name} property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-stock-id)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-stock-id 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-stock-id object) => id}
  @syntax{(setf (gtk:cell-renderer-pixbuf-stock-id object) id)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{stock-id} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The stock ID of the stock icon to render.
  @begin[Warning]{dictionary}
    The @slot[gtk:cell-renderer-pixbuf]{stock-id} property has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @slot[gtk:cell-renderer-pixbuf]{icon-name} property instead.
  @end{dictionary}
  @see-class{gtk:cell-renderer-pixbuf}
  @see-function{gtk:cell-renderer-pixbuf-icon-name}")

;;; --- gtk:cell-renderer-pixbuf-stock-size ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-size"
                                               'cell-renderer-pixbuf) t)
 "The @code{stock-size} property of type @code{:uint} (Read / Write) @br{}
  The @symbol{gtk:icon-size} value that specifies the size of the rendered icon.
  @br{}
  Default value: @code{1}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-stock-size)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-stock-size 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-stock-size object) => size}
  @syntax{(setf (gtk:cell-renderer-pixbuf-stock-size object) size)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[size]{an unsigned integer with sitze that the size of the icon}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{stock-size} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The @symbol{gtk:icon-size} value that specifies the size of the rendered icon.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-symbol{gtk:icon-size}")

;;; --- gtk:cell-renderer-pixbuf-surface ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface"
                                               'cell-renderer-pixbuf) t)
 "The @code{surface} property of type @class{gdk:cairo-surface} (Read / Write)
  @br{}
  The Cairo surface to render. @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-pixbuf-surface)
      "Accessor"
      (documentation 'cell-renderer-pixbuf-surface 'function)
 "@version{2024-3-17}
  @syntax{(gtk:cell-renderer-pixbuf-surface object) => surface}
  @syntax{(setf (gtk:cell-renderer-pixbuf-surface object) surface)}
  @argument[object]{a @class{gtk:cell-renderer-pixbuf} object}
  @argument[surface]{a @class{gdk:cairo-surface} instance to render}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-pixbuf]{surface} slot of the
    @class{gtk:cell-renderer-pixbuf} class.
  @end{short}
  The Cairo surface to render.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{gdk:cairo-surface}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_pixbuf_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-pixbuf-new))

(defun cell-renderer-pixbuf-new ()
 #+liber-documentation
 "@version{2024-3-17}
  @return{The new @class{gtk:cell-renderer-pixbuf} object.}
  @begin{short}
    Creates a new cell renderer pixbuf.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can
  be set globally with the @fun{g:object-property} function. Also, with the
  @class{gtk:tree-view-column} widget, you can bind a property to a value in a
  @class{gtk:tree-model} widget. For example, you can bind the
  @slot[gtk:cell-renderer-pixbuf]{pixbuf} property on the cell renderer to a
  pixbuf value in the model, thus rendering a different image in each row of
  the @class{gtk:tree-view} widget.
  @see-class{gtk:cell-renderer-pixbuf}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-model}
  @see-function{g:object-property}
  @see-function{gtk:cell-renderer-pixbuf-pixbuf}"
  (make-instance 'cell-renderer-pixbuf))

(export 'cell-renderer-pixbuf-new)

;;; --- End of file gtk3.cell-renderer-pixbuf.lisp -----------------------------
