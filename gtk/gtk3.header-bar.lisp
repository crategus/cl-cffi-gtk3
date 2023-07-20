;;; ----------------------------------------------------------------------------
;;; gtk3.header-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2016 - 2023 Dieter Kaiser
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
;;; GtkHeaderBar
;;;
;;;     A box with a centered child
;;;
;;; Types and Values
;;;
;;;     GtkHeaderBar
;;;
;;; Functions
;;;
;;;     gtk_header_bar_new
;;;     gtk_header_bar_set_title                           Accessor
;;;     gtk_header_bar_get_title                           Accessor
;;;     gtk_header_bar_set_subtitle                        Accessor
;;;     gtk_header_bar_get_subtitle                        Accessor
;;;     gtk_header_bar_set_has_subtitle                    Accessor
;;;     gtk_header_bar_get_has_subtitle                    Accessor
;;;     gtk_header_bar_set_custom_title                    Accessor
;;;     gtk_header_bar_get_custom_title                    Accessor
;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end
;;;     gtk_header_bar_set_show_close_button               Accessor
;;;     gtk_header_bar_get_show_close_button               Accessor
;;;     gtk_header_bar_set_decoration_layout               Accessor
;;;     gtk_header_bar_get_decoration_layout               Accessor
;;;
;;; Properties
;;;
;;;       GtkWidget*   custom-title             Read / Write
;;;           gchar*   decoration-layout        Read / Write
;;;        gboolean    decoration-layout-set    Read / Write
;;;        gboolean    has-subtitle             Read / Write
;;;        gboolean    show-close-button        Read / Write
;;;            gint    spacing                  Read / Write
;;;           gchar*   subtitle                 Read / Write
;;;           gchar*   title                    Read / Write
;;;
;;; Child Properties
;;;
;;;     GtkPackType    pack-type                Read / Write
;;;            gint    position                 Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkHeaderBar
;;;
;;; Implemented Interfaces
;;;
;;; GtkHeaderBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkHeaderBar
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkHeaderBar" header-bar
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_header_bar_get_type")
  ((custom-title
    header-bar-custom-title
    "custom-title" "GtkWidget" t t)
   (decoration-layout
    header-bar-decoration-layout
    "decoration-layout" "gchararray" t t)
   (decoration-layout-set
    header-bar-decoration-layout-set
    "decoration-layout-set" "gboolean" t t)
   (has-subtitle
    header-bar-has-subtitle
    "has-subtitle" "gboolean" t t)
   (show-close-button
    header-bar-show-close-button
    "show-close-button" "gboolean" t t)
   (spacing
    header-bar-spacing
    "spacing" "gint" t t)
   (subtitle
    header-bar-subtitle
    "subtitle" "gchararray" t t)
   (title
    header-bar-title
    "title" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'header-bar 'type)
 "@version{#2023-3-20}
  @begin{short}
    The @sym{gtk:header-bar} widget is similar to a horizontal @class{gtk:box}
    widget. It allows children to be placed at the start or the end. In
    addition, it allows a title and subtitle to be displayed.
  @end{short}

  @image[headerbar]{Figure: GtkHeaderBar}

  The title will be centered with respect to the width of the box, even if the
  children at either side take up different amounts of space. The height of the
  titlebar will be set to provide sufficient space for the subtitle, even if
  none is currently set. If a subtitle is not needed, the space reservation can
  be turned off with the @fun{gtk:header-bar-has-subtitle} function.

  The @sym{gtk:header-bar} widget can add typical window frame controls, such
  as Minimize, Maximize and Close buttons, or the window icon.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk:pack-type}
        (Read / Write) @br{}
        Whether the child is packed with reference to the start or end of the
        parent. @br{}
        Default value: @code{:start}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child widget in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:header-bar-new}
  @see-slot{gtk:header-bar-custom-title}
  @see-slot{gtk:header-bar-decoration-layout}
  @see-slot{gtk:header-bar-decoration-layout-set}
  @see-slot{gtk:header-bar-has-subtitle}
  @see-slot{gtk:header-bar-show-close-button}
  @see-slot{gtk:header-bar-spacing}
  @see-slot{gtk:header-bar-subtitle}
  @see-slot{gtk:header-bar-title}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- header-bar-custom-title ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "custom-title" 'header-bar) t)
 "The @code{custom-title} property of type @class{gtk:widget}
  (Read / Write / Construct) @br{}
  Custom title widget to display. @br{}")

#+liber-documentation
(setf (liber:alias-for-function'header-bar-custom-title)
      "Accessor"
      (documentation 'header-bar-custom-title 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-custom-title object) => widget}
  @syntax[]{(setf (gtk:header-bar-custom-title object) widget)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[widget]{a @class{gtk:widget} custom widget to use for a title}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{custom-title} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-custom-title} function retrieves the custom title
  widget of the header bar. The @sym{(setf gtk:header-bar-custom-title)}
  function sets a custom title widget.

  The title should help a user identify the current view. This supersedes any
  title set by the @fun{gtk:header-bar-title} or @fun{gtk:header-bar-subtitle}
  functions. To achieve the same style as the built-in title and subtitle, use
  the @code{.title} and @code{.subtitle} style classes.

  You should set the custom title to @code{nil}, for the header bar title label
  to be visible again.
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-title}
  @see-function{gtk:header-bar-subtitle}")

;;; --- header-bar-decoration-layout -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decoration-layout"
                                               'header-bar) t)
 "The @code{decoration-layout} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The decoration layout for buttons. If this property is not set, the
  @slot[gtk:settings]{gtk-decoration-layout} setting is used. See the
  @fun{gtk:header-bar-decoration-layout} function for information about the
  format of this string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-decoration-layout)
      "Accessor"
      (documentation 'header-bar-decoration-layout 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-decoration-layout object) => layout}
  @syntax[]{(setf (gtk:header-bar-decoration-layout object) layout)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[layout]{a string with the decoration layout, or @code{nil} to unset
    the layout}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{decoration-layout} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-decoration-layout} function gets the decoration
  layout. The @sym{(setf gtk:header-bar-decoration-layout)} function sets the
  decoration layout for the header bar, overriding the
  @slot[gtk:settings]{gtk-decoration-layout} setting.

  There can be valid reasons for overriding the setting, such as a header bar
  design that does not allow for buttons to take room on the right, or only
  offers room for a single Close button. Split header bars are another example
  for overriding the setting.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are \"minimize\", \"maximize\", \"close\", \"icon\"
  for the window icon and \"menu\" for a menu button for the fallback
  application menu.

  For example, \"menu:minimize,maximize,close\" specifies a Menu on the left,
  and Minimize, Maximize and Close buttons on the right.
  @see-class{gtk:header-bar}
  @see-function{gtk:settings-gtk-decoration-layout}")

;;; --- header-bar-decoration-layout-set ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decoration-layout-set"
                                               'header-bar) t)
 "The @code{decoration-layout-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the @code{decoration-layout} property is set. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-decoration-layout-set)
      "Accessor"
      (documentation 'header-bar-decoration-layout-set 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-decoration-layout-set object) => setting}
  @syntax[]{(setf (gtk:header-bar-decoration-layout-set object) setting)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[setting]{a boolean whether a decoration layout is set}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{decoration-layout-set} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  Set to @em{true} if the @slot[gtk:header-bar]{decoration-layout} property
  is set.
  @see-class{gtk:header-bar}
  @see-function{gtk:header-bar-decoration-layout}")

;;; --- header-bar-has-subtitle ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-subtitle" 'header-bar) t)
 "The @code{has-subtitle} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, reserve space for a subtitle, even if none is currently set.
  @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-has-subtitle)
      "Accessor"
      (documentation 'header-bar-has-subtitle 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-has-subtitle object) => setting}
  @syntax[]{(setf gtk:header-bar-has-subtitle object) setting)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[setting]{@em{true} to reserve space for a subtitle}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{has-subtitle} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-has-subtitle} function retrieves whether the header
  bar reserves space for a subtitle, regardless if one is currently set or not.
  The @sym{(setf gtk:header-bar-has-subtitle)} function sets whether the header
  bar should reserve space for a subtitle.
  @see-class{gtk:header-bar}")

;;; --- header-bar-show-close-button -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button"
                                               'header-bar) t)
 "The @code{show-close-button} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show window decorations. Which buttons are actually shown and
  where is determined by the @code{decoration-layout} property, and by the
  state of the window, e.g. a Close button will not be shown if the window
  can not be closed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-show-close-button)
      "Accessor"
      (documentation 'header-bar-show-close-button 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-show-close-button object) => setting}
  @syntax[]{(setf gtk:header-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[setting]{@em{true} to show standard window decorations}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{show-close-button} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-show-close-button} function returns whether the header
  bar shows the standard window decorations, including Close, Maximize, and
  Minimize buttons. The @sym{(setf gtk:header-bar-show-close-button)} function
  sets whether the header bar shows decorations.
  @see-class{gtk:header-bar}")

;;; --- header-bar-spacing -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'header-bar) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 6")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-spacing)
      "Accessor"
      (documentation 'header-bar-spacing 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-spacing object) => spacing}
  @syntax[]{(setf gtk:header-bar-spacing object) spacing)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[spacing]{an integer with the amount of space between children}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{spacing} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The amount of space between children in pixels.
  @see-class{gtk:header-bar}")

;;; --- header-bar-subtitle ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "subtitle" 'header-bar) t)
 "The @code{subtitle} property of type @code{:string} (Read / Write) @br{}
  The subtitle to display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-subtitle)
      "Accessor"
      (documentation 'header-bar-subtitle 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-subtitle object) => subtitle}
  @syntax[]{(setf (gtk:header-bar-subtitle object) subtitle)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[subtitle]{a string with the subtitle, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{subtitle} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-subtitle} function retrieves the subtitle of the
  header bar. The @sym{(setf gtk:header-bar-subtitle)} function sets the
  subtitle. The title should give a user an additional detail to help him
  identify the current view.

  Note that the @class{gtk:header-bar} widget by default reserves room for the
  subtitle, even if none is currently set. If this is not desired, set the
  @slot[gtk:header-bar]{has-subtitle} property to the @em{false} value.
  @see-class{gtk:header-bar}
  @see-function{gtk:header-bar-has-subtitle}")

;;; --- header-bar-title -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'header-bar) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title to display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-title)
      "Accessor"
      (documentation 'header-bar-title 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-title object) => title}
  @syntax[]{(setf (gtk:header-bar-title object) title)}
  @argument[object]{a @class{gtk:header-bar} widget}
  @argument[title]{a string with the title, or @code{nil}.}
  @begin{short}
    Accessor of the @slot[gtk:header-bar]{title} slot of the
    @class{gtk:header-bar} class.
  @end{short}
  The @sym{gtk:header-bar-title} function retrieves the title of the header
  bar, or @code{nil} if none has been set explicitly. The
  @sym{(setf gtk:header-bar-title)} function sets the title,

  The title should help a user identify the current view. A good title should
  not include the application name.
  @see-class{gtk:header-bar}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- header-bar-child-pack-type ---------------------------------------------

(define-child-property header-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-child-pack-type)
      "Accessor"
      (documentation 'header-bar-child-pack-type 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-child-pack-type object) => pack-type)}
  @syntax[]{(setf (gtk:header-bar-child-pack-type object) pack-type)}
  @argument[container]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[pack-type]{a value of the @symbol{gtk:pack-type} enumeration for
    the child widget}
  @begin{short}
    Accessor of the @code{pack-type} child property of the
    @class{gtk:header-bar} class.
  @end{short}
  A value of the @symbol{gtk:pack-type} enumeration indicating whether the
  child widget is packed with reference to the start or end of the parent.
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}")

;;; --- header-bar-child-position ----------------------------------------------

(define-child-property header-bar-child-position
                       "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'header-bar-child-position)
      "Accessor"
      (documentation 'header-bar-child-position 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:header-bar-child-position object) => position)}
  @syntax[]{(setf (gtk:header-bar-child-position object) position)}
  @argument[container]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[position]{an integer with the index of the child widget in the
    header bar}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk:header-bar} class.
  @end{short}
  The index of the child widget in the heaer bar.
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline header-bar-new))

(defun header-bar-new ()
 #+liber-documentation
 "@version{#2023-3-20}
  @return{A new @class{gtk:header-bar} widget.}
  @begin{short}
    Creates a new header bar.
  @end{short}
  @see-class{gtk:header-bar}"
  (make-instance 'header-bar))

(export 'header-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_start ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_header_bar_pack_start" header-bar-pack-start) :void
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[header]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the start
    of the header bar.
  @end{short}
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-pack-end}"
  (header (g:object header-bar))
  (child (g:object widget)))

(export 'header-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_header_bar_pack_end ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_header_bar_pack_end" header-bar-pack-end) :void
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[header]{a @class{gtk:header-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to the
    header bar}
  @begin{short}
    Adds a child widget to the header bar, packed with reference to the end of
    the header bar.
  @end{short}
  @see-class{gtk:header-bar}
  @see-class{gtk:widget}
  @see-function{gtk:header-bar-pack-start}"
  (header (g:object header-bar))
  (child (g:object widget)))

(export 'header-bar-pack-end)

;;; --- End of file gtk3.header-bar.lisp ---------------------------------------
