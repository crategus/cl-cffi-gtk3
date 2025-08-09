;;; ----------------------------------------------------------------------------
;;; gtk3.search-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
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
;;; GtkSearchBar
;;;
;;;     A toolbar to integrate a search entry with
;;;
;;; Types and Values
;;;
;;;     GtkSearchBar
;;;
;;; Accessors
;;;
;;;     gtk_search_bar_get_search_mode
;;;     gtk_search_bar_set_search_mode
;;;     gtk_search_bar_get_show_close_button
;;;     gtk_search_bar_set_show_close_button
;;;
;;; Functions
;;;
;;;     gtk_search_bar_new
;;;     gtk_search_bar_connect_entry
;;;     gtk_search_bar_handle_event
;;;
;;; Properties
;;;
;;;     search-mode-enabled
;;;     show-close-button
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkSearchBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSearchBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSearchBar
;;; ----------------------------------------------------------------------------

;; TODO: Replace the example with Lisp code

(gobject:define-gobject "GtkSearchBar" search-bar
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_search_bar_get_type")
  ((search-mode-enabled
    search-bar-search-mode-enabled
    "search-mode-enabled" "gboolean" t t)
   (show-close-button
    search-bar-show-close-button
    "show-close-button" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'search-bar 'type)
 "@version{2025-07-21}
  @begin{short}
    The @class{gtk:search-bar} widget is a container made to have a search entry
    built-in, possibly with additional connex widgets, such as drop-down menus,
    or buttons.
  @end{short}
  The search bar would appear when a search is started through typing on the
  keyboard, or the application’s search mode is toggled on.

  @image[search-bar]{}

  For keyboard presses to start a search, events will need to be forwarded from
  the toplevel window that contains the search bar. See the
  @fun{gtk:search-bar-handle-event} function for example code. Common shortcuts
  such as the @kbd{Ctrl+F} key should be handled as an application action, or
  through the menu items.

  You will also need to tell the search bar about which entry you are using as
  your search entry using the @fun{gtk:search-bar-connect-entry} function. The
  following example shows you how to create a more complex search entry.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:search-bar} implementation has a single CSS node with name
    @code{searchbar}.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Creating a search bar.
    @begin{pre}
(defun example-search-bar (&optional application)
  (gtk:within-main-loop
    (let* ((window (make-instance 'gtk:application-window
                                  :type :toplevel
                                  :title \"Search Bar\"
                                  :application application
                                  :default-width 250
                                  :default-height 120))
           (box (make-instance 'gtk:box
                               :orientation :horizontal
                               :spacing 6))
           (entry (make-instance 'gtk:search-entry
                                 :text \"Search Entry\"))
           (button (make-instance 'gtk:menu-button))
           (searchbar (gtk:search-bar-new)))
      (gtk:search-bar-connect-entry searchbar entry)
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (gtk:widget-destroy widget)
                          (gtk:leave-gtk-main)))
      (g:signal-connect window \"key-press-event\"
              (lambda (window event)
                (declare (ignore window))
                (gtk:search-bar-handle-event searchbar event)))
      (gtk:container-add window searchbar)
      (gtk:container-add searchbar box)
      (gtk:box-pack-start box entry)
      (gtk:box-pack-start box button)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:search-bar-new}
  @see-slot{gtk:search-bar-search-mode-enabled}
  @see-slot{gtk:search-bar-show-close-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:search-bar-search-mode-enabled -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-mode-enabled"
                                               'search-bar) t)
 "The @code{search-mode-enabled} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the search mode is on and the search bar shown. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-search-mode-enabled)
      "Accessor"
      (documentation 'search-bar-search-mode-enabled 'function)
 "@version{2025-07-10}
  @syntax{(gtk:search-bar-search-mode-enabled object) => setting}
  @syntax{(setf (gtk:search-bar-search-mode-enabled object) setting)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[setting]{a boolean for the state of the search mode}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{search-mode-enabled} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  Switches the search mode on or off.
  @see-class{gtk:search-bar}")

;;; --- gtk:search-bar-show-close-button ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button"
                                               'search-bar) t)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to show the Close button in the toolbar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'search-bar-show-close-button)
      "Accessor"
      (documentation 'search-bar-show-close-button 'function)
 "@version{2025-07-10}
  @syntax{(gtk:search-bar-show-close-button object) => visible}
  @syntax{(setf (gtk:search-bar-show-close-button object) visible)}
  @argument[object]{a @class{gtk:search-bar} widget}
  @argument[visible]{a boolean whether the Close button will be shown or not}
  @begin{short}
    Accessor of the @slot[gtk:search-bar]{show-close-button} slot of the
    @class{gtk:search-bar} class.
  @end{short}
  The @fun{gtk:search-bar-show-close-button} function returns whether the Close
  button is shown. The @setf{gtk:search-bar-show-close-button} function
  shows or hides the Close button. Applications that already have a \"search\"
  toggle button should not show a Close button in their search bar, as it
  duplicates the role of the toggle button.
  @see-class{gtk:search-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_new
;;; ----------------------------------------------------------------------------

(defun search-bar-new ()
 #+liber-documentation
 "@version{2025-07-10}
  @return{The new @class{gtk:search-bar} widget.}
  @begin{short}
    Creates a search bar.
  @end{short}
  You will need to tell it about which widget is going to be your text entry
  using the @fun{gtk:search-bar-connect-entry} function.
  @see-class{gtk:search-bar}
  @see-function{gtk:search-bar-connect-entry}"
  (make-instance 'search-bar))

(export 'search-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_connect_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_search_bar_connect_entry" search-bar-connect-entry) :void
 #+liber-documentation
 "@version{#2025-07-10}
  @argument[searchbar]{a @class{gtk:search-bar} widget}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Connects the entry widget passed as the one to be used in this search bar.
  @end{short}
  The entry should be a descendant of the search bar. This is only required if
  the entry is not the direct child of the search bar.
  @see-class{gtk:search-bar}
  @see-class{gtk:entry}"
  (searchbar (g:object search-bar))
  (entry (g:object entry)))

(export 'search-bar-connect-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_get_search_mode
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor search-bar-search-mode-enabled

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_set_search_mode
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor search-bar-search-mode-enabled

;;; ----------------------------------------------------------------------------
;;; gtk_search_bar_handle_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_search_bar_handle_event" search-bar-handle-event) :boolean
 #+liber-documentation
 "@version{#2025-07-21}
  @argument[searchbar]{a @class{gtk:search-bar} widget}
  @argument[event]{a @class{gdk:event} instance containing key press events}
  @begin{return}
    The @var{gdk:+event-stop+} value if the key press event resulted in text
    being entered in the search entry, and revealing the search bar if
    necessary, @var{gdk:+event-propagate+} otherwise.
  @end{return}
  @begin{short}
    This function should be called when the toplevel window which contains the
    search bar received a key event.
  @end{short}
  If the key event is handled by the search bar, the bar will be shown, the
  entry populated with the entered text and the @var{gdk:+event-stop+} value
  will be returned. The caller should ensure that events are not propagated
  further.

  If no entry has been connected to the search bar, using the
  @fun{gtk:search-bar-connect-entry} function, this function will return
  immediately with a warning.
  @see-class{gtk:search-bar}
  @see-function{gtk:search-bar-connect-entry}"
  (searchbar (g:object search-bar))
  (event (g:boxed gdk:event)))

(export 'search-bar-handle-event)

;;; --- End of file gtk3.search-bar.lisp ---------------------------------------
