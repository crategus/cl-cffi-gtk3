;;; ----------------------------------------------------------------------------
;;; gtk3.app-chooser-widget.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GtkAppChooserWidget
;;;
;;;     Application chooser widget that can be embedded in other widgets
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_widget_new
;;;     gtk_app_chooser_widget_set_show_default            Accessor
;;;     gtk_app_chooser_widget_get_show_default            Accessor
;;;     gtk_app_chooser_widget_set_show_recommended        Accessor
;;;     gtk_app_chooser_widget_get_show_recommended        Accessor
;;;     gtk_app_chooser_widget_set_show_fallback           Accessor
;;;     gtk_app_chooser_widget_get_show_fallback           Accessor
;;;     gtk_app_chooser_widget_set_show_other              Accessor
;;;     gtk_app_chooser_widget_get_show_other              Accessor
;;;     gtk_app_chooser_widget_set_show_all                Accessor
;;;     gtk_app_chooser_widget_get_show_all                Accessor
;;;     gtk_app_chooser_widget_set_default_text            Accessor
;;;     gtk_app_chooser_widget_get_default_text            Accessor
;;;
;;; Properties
;;;
;;;     default-text
;;;     show-all
;;;     show-default
;;;     show-fallback
;;;     show-other
;;;     show-recommended
;;;
;;; Signals
;;;
;;;     application-activated
;;;     application-selected
;;;     populate-popup
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkAppChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAppChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkAppChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAppChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAppChooserWidget" app-chooser-widget
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_widget_get_type")
  ((default-text
    app-chooser-widget-default-text
    "default-text" "gchararray" t t)
   (show-all
    app-chooser-widget-show-all
    "show-all" "gboolean" t t)
   (show-default
    app-chooser-widget-show-default
    "show-default" "gboolean" t t)
   (show-fallback
    app-chooser-widget-show-fallback
    "show-fallback" "gboolean" t t)
   (show-other
    app-chooser-widget-show-other
    "show-other" "gboolean" t t)
   (show-recommended
    app-chooser-widget-show-recommended
    "show-recommended" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'app-chooser-widget 'type)
 "@version{#2025-07-14}
  @begin{short}
    The @class{gtk:app-chooser-widget} widget is a widget for selecting
    applications.
  @end{short}
  It is the main building block for the @class{gtk:app-chooser-dialog} widget.
  Most applications only need to use the latter. But you can use this widget as
  part of a larger widget if you have special needs.

  The @class{gtk:app-chooser-widget} widget offers detailed control over what
  applications are shown, using the @code{show-default},
  @code{show-recommended}, @code{show-fallback}, @code{show-other} and
  @code{show-all} properties. See the @class{gtk:app-chooser} documentation for
  more information about these groups of applications.

  To keep track of the selected application, use the \"application-selected\"
  and @code{\"application-activated\"} signals.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:app-chooser-widget} implementation has a single CSS node with
    name @code{appchooser}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[app-chooser-widget::application-activated]{signal}
      @begin{pre}
lambda (widget application)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:app-chooser-widget} widget that received
          the signal.}
        @entry[application]{The activated @class{g:app-info} object.}
      @end{simple-table}
      Emitted when an application item is activated from the list of the widget.
      This usually happens when the user double clicks an item, or an item is
      selected and the user presses one of the @kbd{Space}, @kbd{Shift+Space},
      @kbd{Return} or @kbd{Enter} keys.
    @end{signal}
    @begin[app-chooser-widget::application-selected]{signal}
      @begin{pre}
lambda (widget application)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:app-chooser-widget} widget that received
          the signal.}
        @entry[application]{The selected @class{g:app-info} object.}
      @end{simple-table}
      Emitted when an application item is selected from the widget's list.
    @end{signal}
    @begin[app-chooser-widget::populate-popup]{signal}
      @begin{pre}
lambda (widget menu application)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:app-chooser-widget} widget that received
          the signal.}
        @entry[menu]{The @class{gtk:menu} object to populate.}
        @entry[application]{The current @class{g:app-info} object.}
      @end{simple-table}
      Emitted when a context menu is about to popup over an application item.
      Clients can insert menu items into the provided @class{gtk:menu} object
      in the callback of this signal. The context menu will be shown over the
      item if at least one item has been added to the menu.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:app-chooser-widget-new}
  @see-slot{gtk:app-chooser-widget-default-text}
  @see-slot{gtk:app-chooser-widget-show-all}
  @see-slot{gtk:app-chooser-widget-show-default}
  @see-slot{gtk:app-chooser-widget-show-fallback}
  @see-slot{gtk:app-chooser-widget-show-other}
  @see-slot{gtk:app-chooser-widget-show-recommended}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:app-chooser-widget-default-text ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-text"
                                               'app-chooser-widget) t)
 "The @code{default-text} property of type @code{:string} (Read / Write) @br{}
  The @code{default-text} property determines the text that appears in the
  widget when there are no applications for the given content type. See also
  the @fun{gtk:app-chooser-widget-default-text} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-default-text)
      "Accessor"
      (documentation 'app-chooser-widget-default-text 'function)
 "@version{#2025-07-07}
  @syntax{(gtk:app-chooser-widget-default-text object) => text}
  @syntax{(setf (gtk:app-chooser-widget-default-text object) text)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[text]{a string for the text that appears in the widget}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{default-text} slot of the
    @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widger-default-text} function returns the text that
  is shown if there are not applications that can handle the content type. The
  @setf{gtk:app-chooser-widget-default-text} function sets the text that is
  shown if there are not applications that can handle the content type.
  @see-class{gtk:app-chooser-widget}")

;;; --- gtk:app-chooser-widget-show-all ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-all"
                                               'app-chooser-widget) t)
 "The @code{show-all} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If the @code{show-all} property is @em{true}, the application chooser
  presents all applications in a single list, without subsections for default,
  recommended or related applications. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-show-all)
      "Accessor"
      (documentation 'app-chooser-widget-show-all 'function)
 "@version{#2023-2-15}
  @syntax{(gtk:app-chooser-widget-show-all object) => setting}
  @syntax{(setf (gtk:app-chooser-widget-show-all object) setting)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser presents all
    applications in a single list}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{show-all} slot of the
    @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widget-show-all} function sets whether the
  application chooser should show all applications in a flat list. The
  @setf{gtk:app-chooser-widget-show-all} function sets whether the app chooser
  should show all applications in a flat list.
  @see-class{gtk:app-chooser-widget}")

;;; --- gtk:app-chooser-widget-show-default ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-default"
                                               'app-chooser-widget) t)
 "The @code{show-default} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-default} property determines whether the application chooser
  should show the default handler for the content type in a separate section.
  If @em{false}, the default handler is listed among the recommended
  applications. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-show-default)
      "Accessor"
      (documentation 'app-chooser-widget-show-default 'function)
 "@version{#2023-2-15}
  @syntax{(gtk:app-chooser-widget-show-default object) => setting}
  @syntax{(setf (gtk:app-chooser-widget-show-default object) setting)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show the
    default handler}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{show-default} slot of the
    @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widget-show-default} function returns whether the
  application chooser should the default handler for the content type in a
  separate section. The @setf{gtk:app-chooser-widget-show-default} function
  sets whether the application chooser should show the default handler.
  @see-class{gtk:app-chooser-widget}")

;;; --- gtk:app-chooser-widget-show-fallback -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-fallback"
                                               'app-chooser-widget) t)
 "The @code{show-fallback} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-fallback} property determines whether the application chooser
  should show a section for fallback applications. If @em{false}, the fallback
  applications are listed among the other applications. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-show-fallback)
      "Accessor"
      (documentation 'app-chooser-widget-show-fallback 'function)
 "@version{#2023-2-15}
  @syntax{(gtk:app-chooser-widget-show-fallback object) => setting}
  @syntax{(setf (gtk:app-chooser-widget-show-fallback object) setting)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show a
    section for fallback applications}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{show-fallback} slot of the
    @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widget-show-fallback} function returns whether the
  application chooser should show a section for fallback applications. The
  @setf{gtk:app-chooser-widget-show-fallback} function sets whether the
  application chooser should show related applications for the content type in
  a separate section.
  @see-class{gtk:app-chooser-widget}")

;;; --- gtk:app-chooser-widget-show-other --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-other"
                                               'app-chooser-widget) t)
 "The @code{show-other} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-other} property determines whether the application chooser
  should show a section for other applications. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-show-other)
      "Accessor"
      (documentation 'app-chooser-widget-show-other 'function)
 "@version{#2023-2-15}
  @syntax{(gtk:app-chooser-widget-show-other object) => setting}
  @syntax{(setf (gtk:app-chooser-widget-show-other object) setting)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser should show a
    section for other applications}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{show-other} slot of the
    @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widget-show-other} function returns whether the
  application chooser should show a section for other applications. The
  @setf{gtk:app-chooser-widget-show-other} function sets whether the application
  chooser should show applications which are unrelated to the content type.
  @see-class{gtk:app-chooser-widget}")

;;; --- gtk:app-chooser-widget-show-recommended --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-recommended"
                                               'app-chooser-widget) t)
 "The @code{show-recommended} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{show-recommended} property determines whether the application
  chooser should show a section for recommended applications. If @em{false},
  the recommended applications are listed among the other applications. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-widget-show-recommended)
      "Accessor"
      (documentation 'app-chooser-widget-show-recommended 'function)
 "@version{#2023-2-15}
  @syntax{(gtk:app-chooser-widget-show-recommended object) => setting}
  @syntax{(setf (gtk:app-chooser-widget-show-recommended object) setting)}
  @argument[object]{a @class{gtk:app-chooser-widget} widget}
  @argument[setting]{a boolean whether the application chooser shuld show a
    section for recommended applications}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-widget]{show-recommended} slot of
    the @class{gtk:app-chooser-widget} class.
  @end{short}
  The @fun{gtk:app-chooser-widget-show-recommended} function returns whether
  the application chooser should recommended applications. The
  @setf{gtk:app-chooser-widget-show-recommended} function sets whether the
  application chooser should show recommended applications for the content type
  in a separate section.
  @see-class{gtk:app-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline app-chooser-widget-new))

(defun app-chooser-widget-new (content-type)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[content-type]{a string for the content type to show applications
    for}
  @return{The newly created @class{gtk:app-chooser-widget} widget.}
  @begin{short}
    Creates a new application chooser widget for applications that can
    handle content of the given type.
  @end{short}
  @see-class{gtk:app-chooser-widget}"
  (make-instance 'app-chooser-widget
                 :content-type content-type))

(export 'app-chooser-widget-new)

;;; --- End of file gtk3.app-chooser-widget.lisp -------------------------------
