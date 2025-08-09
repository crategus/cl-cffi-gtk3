;;; ----------------------------------------------------------------------------
;;; gtk3.file-chooser-widget.lisp
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
;;; GtkFileChooserWidget
;;;
;;;     File chooser widget that can be embedded in other widgets
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_widget_new
;;;
;;; Properties
;;;
;;;     search-mode
;;;     subtitle
;;;
;;; Signals
;;;
;;;     desktop-folder
;;;     down-folder
;;;     home-folder
;;;     location-popup
;;;     location-popup-on-paste
;;;     location-toggle-popup
;;;     places-shortcut
;;;     quick-bookmark
;;;     recent-shortcut
;;;     search-shortcut
;;;     show-hidden
;;;     up-folder
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFileChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable, GtkFileChooser and GtkFileChooserEmbed.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFileChooserWidget" file-chooser-widget
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFileChooser"
;                TODO: This interface is not documented.
;                We have no implementation.
;                "GtkFileChooserEmbed"
               )
   :type-initializer "gtk_file_chooser_widget_get_type")
  ((search-mode
    file-chooser-widget-search-mode
    "search-mode" "gboolean" t t)
   (subtitle
    file-chooser-widget-subtitle
    "subtitle" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'file-chooser-widget 'type)
 "@version{#2025-07-15}
  @begin{short}
    The @class{gtk:file-chooser-widget} widget is a widget for choosing files.
  @end{short}
  It exposes the @class{gtk:file-chooser} interface, and you should use the
  methods of this interface to interact with the widget.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:file-chooser-widget} widget has a single CSS node with name
    @code{filechooser}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[file-chooser-widget::desktop-folder]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the user's Desktop folder in the file
      list. The default binding for this signal is @kbd{Alt+D}.
    @end{signal}
    @begin[file-chooser-widget::down-folder]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser go to a child of the current folder in the
      file hierarchy. The subfolder that will be used is displayed in the path
      bar widget of the file chooser. For example, if the path bar is showing
      @file{\"/foo/bar/baz\"}, with bar currently displayed, then this will
      cause the file chooser to switch to the @file{\"baz\"} subfolder. The
      default binding for this signal is @kbd{Alt+Down}.
    @end{signal}
    @begin[file-chooser-widget::home-folder]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the user's home folder in the file
      list. The default binding for this signal is the @kbd{Alt+Home} key.
    @end{signal}
    @begin[file-chooser-widget::location-popup]{signal}
      @begin{pre}
lambda (widget path)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
        @entry[path]{A string that gets put in the text entry for the file
          name.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show a \"Location\" prompt which the user
      can use to manually type the name of the file he wishes to select. The
      default bindings for this signal are @kbd{Control+L} with a path string of
      @file{\"\"} (the empty string). It is also bound to @kbd{/} with a path
      string of @file{\"/\"} (a slash), this lets you type @kbd{/} and
      immediately type a path name. On Unix systems, this is bound to @file{~ }
      (tilde) with a path string of @file{\"~\"} itself for access to home
      directories.
    @end{signal}
    @begin[file-chooser-widget::location-popup-on-paste]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show a \"Location\" prompt when the user
      pastes into a @class{gtk:file-chooser-widget} widget. The default binding
      for this signal is @kbd{Control+V}.
    @end{signal}
    @begin[file-chooser-widget::location-toggle-popup]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to toggle the visibility of a \"Location\" prompt which the user can
      use to manually type the name of the file he wishes to select. The default
      binding for this signal is @kbd{Control+L}.
    @end{signal}
    @begin[file-chooser-widget::places-shortcut]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This
      is used to move the focus to the places sidebar. The default binding for
      this signal is @kbd{Alt+P}.
    @end{signal}
    @begin[file-chooser-widget::quick-bookmark]{signal}
      @begin{pre}
lambda (widget bookmark-index)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
        @entry[bookmark-index]{The integer for the number of the bookmark to
          switch to.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser switch to the bookmark specified in the
      @arg{bookmark-index} parameter. For example, if you have three bookmarks,
      you can pass 0, 1, 2 to this signal to switch to each of them,
      respectively. The default binding for this signal is @kbd{Alt+1},
      @kbd{Alt+2}, and so on, until @kbd{Alt+0}. Note that in the default
      binding, that @kbd{Alt+1} is actually defined to switch to the bookmark at
      index 0, and so on successively, @kbd{Alt+0} is defined to switch to the
      bookmark at index 10.
    @end{signal}
    @begin[file-chooser-widget::recent-shortcut]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the Recent location. The default
      binding for this signal is @kbd{Alt+R}.
    @end{signal}
    @begin[file-chooser-widget::search-shortcut]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser show the search entry. The default binding
      for this signal is @kbd{Alt+S}.
    @end{signal}
    @begin[file-chooser-widget::show-hidden]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser display hidden files. The default binding
      for this signal is @kbd{Control+H}.
    @end{signal}
    @begin[file-chooser-widget::up-folder]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-widget} object which received
          the signal.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user asks for it. This is
      used to make the file chooser go to the parent of the current folder in
      the file hierarchy. The default binding for this signal is @kbd{Alt+Up}.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:file-chooser-widget-new}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:file-chooser-widget-search-mode ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-mode"
                                               'file-chooser-widget) t)
 "The @code{search-mode} property of type @code{:boolean} (Read / Write) @br{}
  Search mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-widget-search-mode)
      "Accessor"
      (documentation 'file-chooser-widget-search-mode 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:file-chooser-widget object) => search-mode}
  @syntax{(setf (gtk:file-chooser-widget object) search-mode)}
  @argument[object]{a @class{gtk:file-chooser-widget} widget}
  @argument[search-mode]{a boolean whether in search mode}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-widget]{search-mode} slot of the
    @class{gtk:file-chooser-widget} class.
  @end{short}
  @see-class{gtk:file-chooser-widget}")

;;; --- gtk:file-chooser-widget-subtitle ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "subtitle"
                                               'file-chooser-widget) t)
 "The @code{subtitle} property of type @code{:string} (Read) @br{}
  The subtitle. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-widget-subtitle)
      "Accessor"
      (documentation 'file-chooser-widget-subtitle 'function)
 "@version{#2025-07-07}
  @syntax{(gtk:file-chooser-widget object) => subtitle}
  @syntax{(setf (gtk:file-chooser-widget object) subtitle)}
  @argument[object]{a @class{gtk:file-chooser-widget} widget}
  @argument[subtitle]{a string for the subtitle}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-widget]{subtitle} slot of the
    @class{gtk:file-chooser-widget} class.
  @end{short}
  @see-class{gtk:file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-chooser-widget-new))

(defun file-chooser-widget-new (action)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[action]{a value of the @sym{gtk:file-chooser-action} enumeration
    for open or save mode}
  @return{The new @class{gtk:file-chooser-widget} widget.}
  @begin{short}
    Creates a new file chooser widget.
  @end{short}
  This is a file chooser widget that can be embedded in custom windows, and it
  is the same widget that is used by the @class{gtk:file-chooser-dialog} widget.
  @see-class{gtk:file-chooser-widget}
  @see-class{gtk:file-chooser-dialog}
  @see-symbol{gtk:file-chooser-action}"
  (make-instance 'file-chooser-widget
                 :action action))

(export 'file-chooser-widget-new)

;;; --- End of file gtk3.file-chooser-widget.lisp ------------------------------
