;;; ----------------------------------------------------------------------------
;;; gtk3.plug.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkPlug
;;;
;;;     Toplevel for embedding into other processes
;;;
;;; Types and Values
;;;
;;;     GtkPlug
;;;
;;; Functions
;;;
;;;     gtk_plug_construct
;;;     gtk_plug_construct_for_display
;;;     gtk_plug_new
;;;     gtk_plug_new_for_display
;;;     gtk_plug_get_id
;;;     gtk_plug_get_embedded                              Accessor
;;;     gtk_plug_get_socket_window                         Accessor
;;;
;;; Properties
;;;
;;;     embedded
;;;     socket-window
;;;
;;; Signals
;;;
;;;     embedded
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkPlug
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPlug implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPlug
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkPlug" plug
  (:superclass window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_plug_get_type")
  ((embedded
    plug-embedded
    "embedded" "gboolean" t nil)
   (socket-window
    plug-socket-window
    "socket-window" "GdkWindow" t nil)))

#+liber-documentation
(setf (documentation 'plug 'type)
 "@version{#2023-2-28}
  @begin{short}
    Together with the @class{gtk:socket} widget, the @sym{gtk:plug} widget
    provides the ability to embed widgets from one process into another process
    in a fashion that is transparent to the user.
  @end{short}
  One process creates a @class{gtk:socket} widget and passes the ID of that
  widget's window to the other process, which then creates a @sym{gtk:plug}
  widget with that window ID. Any widgets contained in the @sym{gtk:plug}
  widget then will appear inside the first application's window.

  The communication between a @class{gtk:socket} and a @sym{gtk:plug} widget
  follows the XEmbed protocol. This protocol has also been implemented in
  other toolkits, e.g. Qt, allowing the same level of integration when embedding
  a Qt widget in GTK or vice versa.

  The @sym{gtk:plug} and @class{gtk:socket} widgets are only available when GTK
  is compiled for the X11 platform and @code{GDK_WINDOWING_X11} is defined. They
  can only be used on a @code{GdkX11Display} object.
  @begin[Signal Details]{dictionary}
    @subheading{The \"embedded\" signal}
      @begin{pre}
lambda (plug)    :run-last
      @end{pre}
      Gets emitted when the plug becomes embedded in a socket.
      @begin[code]{table}
        @entry[plug]{The @sym{gtk:plug} widget on which the signal was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:plug-embedded}
  @see-slot{gtk:plug-socket-window}
  @see-class{gtk:socket}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- plug-embedded ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "embedded" 'plug) t)
 "The @code{embedded} property of type @code{:boolean} (Read) @br{}
  @em{True} if the plug is embedded in a socket. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'plug-embedded)
      "Accessor"
      (documentation 'plug-embedded 'function)
 "@version{#2023-2-28}
  @syntax[]{(gtk:plug-embedded object) => embedded}
  @argument[object]{a @class{gtk:plug} widget}
  @argument[embedded]{a boolean whether the plug is embedded in a socket}
  @begin{short}
    Accessor of the @slot[gtk:plug]{embedded} slot of the @class{gtk:plug}
    class.
  @end{short}
  Determines whether the plug is embedded in a socket.
  @see-class{gtk:plug}")

;;; --- plug-socket-window -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "socket-window" 'plug) t)
 "The @code{socket-window} property of type @class{gdk:window} (Read) @br{}
  The window of the socket the plug is embedded in.")

#+liber-documentation
(setf (liber:alias-for-function 'plug-socket-window)
      "Accessor"
      (documentation 'plug-socket-window 'function)
 "@version{#2023-2-28}
  @syntax[]{(gtk:plug-socket-window object) => socket-window}
  @argument[object]{a @class{gtk:plug} widget}
  @argument[socket-window]{a @class{gdk:window} of the socket}
  @begin{short}
    Accessor of the @slot[gtk:plug]{socket-window} slot of the @class{gtk:plug}
    class.
  @end{short}
  Retrieves the socket the plug is embedded in.
  @see-class{gtk:plug}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gtk_plug_construct ()
;;;
;;; void gtk_plug_construct (GtkPlug *plug, Window socket_id);
;;;
;;; Finish the initialization of plug for a given GtkSocket identified by
;;; socket_id. This function will generally only be used by classes deriving
;;; from GtkPlug.
;;;
;;; plug :
;;;     a GtkPlug.
;;;
;;; socket_id :
;;;     the XID of the socket's window.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_construct_for_display ()
;;;
;;; void gtk_plug_construct_for_display (GtkPlug *plug,
;;;                                      GdkDisplay *display,
;;;                                      Window socket_id);
;;;
;;; Finish the initialization of plug for a given GtkSocket identified by
;;; socket_id which is currently displayed on display. This function will
;;; generally only be used by classes deriving from GtkPlug.
;;;
;;; plug :
;;;     a GtkPlug.
;;;
;;; display :
;;;     the GdkDisplay associated with socket_id's GtkSocket.
;;;
;;; socket_id :
;;;     the XID of the socket's window.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_plug_new" plug-new) (g:object plug)
 #+liber-documentation
 "@version{#2023-2-28}
  @argument[socket-id]{a pointer with the window ID of the socket, or 0}
  @return{The new @class{gtk:plug} widget.}
  @begin{short}
    Creates a new plug widget inside the @class{gtk:socket} widget identified
    by @arg{socket-id}.
  @end{short}
  If @arg{socket-id} is 0, the plug is left \"unplugged\" and can later be
  plugged into a @class{gtk:socket} widget by the @fun{gtk:socket-add-id}
  function.
  @see-class{gtk:plug}
  @see-class{gtk:socket}
  @see-function{gtk:socket-add-id}"
  (socket-id :pointer))

(export 'plug-new)

;;; ----------------------------------------------------------------------------
;;; gtk_plug_new_for_display ()
;;;
;;; GtkWidget * gtk_plug_new_for_display (GdkDisplay *display, Window socket_id)
;;;
;;; Create a new plug widget inside the GtkSocket identified by socket_id.
;;;
;;; display :
;;;     the GdkDisplay on which socket_id is displayed
;;;
;;; socket_id :
;;;     the XID of the socket's window.
;;;
;;; Returns :
;;;     the new GtkPlug widget.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_plug_get_id () -> plug-id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_plug_get_id" plug-id) :pointer
 #+liber-documentation
 "@version{#2023-2-28}
  @argument[plug]{a @class{gtk:plug} widget}
  @return{A pointer with the window ID for the plug.}
  @begin{short}
    Gets the window ID of a @class{gtk:plug} widget, which can then be used to
    embed this window inside another window, for instance with the
    @fun{gtk:socket-add-id} function.
  @end{short}
  @see-class{gtk:plug}
  @see-function{gtk:socket-add-id}"
  (plug (g:object plug)))

(export 'plug-id)

;;; --- End of file gtk3.plug.lisp ---------------------------------------------
