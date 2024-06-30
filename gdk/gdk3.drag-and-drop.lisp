;;; ----------------------------------------------------------------------------
;;; gdk3.drag-and-drop.lisp
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
;;; Drag And Drop
;;;
;;;     Functions for controlling drag and drop handling
;;;
;;; Types and Values
;;;
;;;     GdkDragContext
;;;     GdkDragCancelReason
;;;     GdkDragProtocol
;;;     GdkDragAction
;;;
;;; Functions
;;;
;;;     gdk_drag_get_selection
;;;     gdk_drag_abort
;;;     gdk_drop_reply
;;;     gdk_drag_drop
;;;     gdk_drag_drop_done
;;;     gdk_drag_find_window_for_screen
;;;     gdk_drag_begin
;;;     gdk_drag_begin_for_device
;;;     gdk_drag_begin_from_point
;;;     gdk_drag_motion
;;;     gdk_drop_finish
;;;     gdk_drag_status
;;;     gdk_drag_drop_succeeded
;;;
;;;     gdk_window_get_drag_protocol
;;;
;;;     gdk_drag_context_get_actions
;;;     gdk_drag_context_get_suggested_action
;;;     gdk_drag_context_get_selected_action
;;;     gdk_drag_context_list_targets
;;;     gdk_drag_context_get_device
;;;     gdk_drag_context_set_device
;;;     gdk_drag_context_get_source_window
;;;     gdk_drag_context_get_dest_window
;;;     gdk_drag_context_get_protocol
;;;     gdk_drag_context_get_drag_window
;;;     gdk_drag_context_set_hotspot
;;;     gdk_drag_context_manage_dnd
;;;
;;; Signals
;;;
;;;     action-changed
;;;     cancel
;;;     dnd-finished
;;;     drop-performed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDragContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDragCancelReason
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkDragCancelReason" drag-cancel-reason
  (:export t
   :type-initializer "gdk_drag_cancel_reason_get_type")
  :no-target
  :user-cancelled
  :error)

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-cancel-reason)
      "GEnum"
      (liber:symbol-documentation 'drag-cancel-reason)
 "@version{2024-6-28}
  @begin{declaration}
(gobject:define-g-enum \"GdkDragCancelReason\" drag-cancel-reason
  (:export t
   :type-initializer \"gdk_drag_cancel_reason_get_type\")
  :no-target
  :user-cancelled
  :error)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:no-target]{There is no suitable drop target.}
      @entry[:user-cancelled]{Drag cancelled by the user.}
      @entry[:error]{Unspecified error.}
    @end{table}
  @end{values}
  @begin{short}
    Used in the @class{gdk:drag-context} object to indicate the reason of a
    cancelled DND operation.
  @end{short}
  @see-class{gdk:drag-context}")

;;; ----------------------------------------------------------------------------
;;; GdkDragProtocol
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkDragProtocol" drag-protocol
  (:export t
   :type-initializer "gdk_drag_protocol_get_type")
  (:none 0)
  (:motif 1)
  (:xdnd 2)
  (:rootwin 3)
  (:win32-dropfiles 4)
  (:ole2 5)
  (:local 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-protocol)
      "GEnum"
      (liber:symbol-documentation 'drag-protocol)
 "@version{2024-6-28}
  @begin{declaration}
(gobject:define-g-enum \"GdkDragProtocol\" drag-protocol
  (:export t
   :type-initializer \"gdk_drag_protocol_get_type\")
  (:none 0)
  (:motif 1)
  (:xdnd 2)
  (:rootwin 3)
  (:win32-dropfiles 4)
  (:ole2 5)
  (:local 6))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No protocol.}
      @entry[:motif]{The Motif DND protocol.}
      @entry[:xdnd]{The Xdnd protocol.}
      @entry[:rootwin]{An extension to the Xdnd protocol for unclaimed root
        window drops.}
      @entry[:win32-dropfiles]{The simple @code{WM_DROPFILES} protocol.}
      @entry[:ole2]{The complex OLE2 DND protocol (not implemented).}
      @entry[:local]{Intra-application DND.}
    @end{table}
  @end{values}
  @begin{short}
    Used in the @class{gdk:drag-context} object to indicate the protocol
    according to which DND is done.
  @end{short}
  @see-class{gdk:drag-context}")

;;; ----------------------------------------------------------------------------
;;; GdkDragAction
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkDragAction" drag-action
  (:export t
   :type-initializer "gdk_drag_action_get_type")
  (:default #.(ash 1 0))
  (:copy #.(ash 1 1))
  (:move #.(ash 1 2))
  (:link #.(ash 1 3))
  (:private #.(ash 1 4))
  (:ask #.(ash 1 5)))

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-action)
      "GFlags"
      (liber:symbol-documentation 'drag-action)
 "@version{2024-6-28}
  @begin{declaration}
(gobject:define-g-flags \"GdkDragAction\" drag-action
  (:export t
   :type-initializer \"gdk_drag_action_get_type\")
  (:default 1)
  (:copy 2)
  (:move 4)
  (:link 8)
  (:private 16)
  (:ask 32))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:default]{Means nothing, and should not be used.}
      @entry[:copy]{Copy the data.}
      @entry[:move]{Move the data, that is, first copy it, then delete it from
        the source using the @code{\"DELETE\"} target of the X selection
        protocol.}
      @entry[:link]{Add a link to the data. Note that this is only useful if
        source and destination agree on what it means.}
      @entry[:private]{Special action which tells the source that the
        destination will do something that the source does not understand.}
      @entry[:ask]{Ask the user what to do with the data.}
    @end{table}
  @end{values}
  @begin{short}
    Used in the @class{gdk:drag-context} object to indicate what the
    destination should do with the dropped data.
  @end{short}
  @see-class{gdk:drag-context}")

;;; ----------------------------------------------------------------------------
;;; GdkDragContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDragContext" drag-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_drag_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'drag-context 'type)
 "@version{2024-6-28}
  @begin{short}
    The @class{gdk:drag-context} class and the correspondig functions provide a
    low level interface for drag and drop.
  @end{short}
  The X backend of GDK supports both the Xdnd and Motif drag and drop protocols
  transparently, the Win32 backend supports the @code{WM_DROPFILES} protocol.

  GTK provides a higher level abstraction based on top of these functions,
  and so they are not normally needed in GTK applications. See the Drag and
  Drop section of the GTK documentation for more information.
  @begin[Signal Details]{dictionary}
    @subheading{The \"action-changed\" signal}
    @begin{pre}
 lambda (context action)    :run-last
    @end{pre}
    A new action is being chosen for the drag and drop operation. The signal
    will only be emitted if the @class{gtk-drag-context} object manages the
    drag and drop operation. See the @fun{gdk:drag-context-manage-dnd} function
    for more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @class{gdk:drag-context} object on which the signal
        is emitted.}
      @entry[action]{The @symbol{gdk:drag-action} value currently chosen.}
    @end{table}
    @subheading{The \"cancel\" signal}
    @begin{pre}
 lambda (context reason)    :run-last
    @end{pre}
    The drag and drop operation was cancelled. The signal will only be emitted
    if the @class{gdk:drag-context} object manages the drag and drop operation.
    See the @fun{gdk:drag-context-manage-dnd} function for more information.
    Since 3.20
    @begin[code]{table}
      @entry[context]{The @class{gdk:drag-context} object on which the signal
        is emitted.}
      @entry[reason]{The @symbol{gdk:drag-cancel-reason} value the drag context
        was cancelled.}
    @end{table}
    @subheading{The \"dnd-finished\" signal}
    @begin{pre}
 lambda (context)    :run-last
    @end{pre}
    The drag and drop operation was finished, the drag destination finished
    reading all data. The drag source can now free all miscellaneous data. This
    signal will only be emitted if the @class{gdk:drag-context} object manages
    the drag and drop operation. See the @fun{gdk:drag-context-manage-dnd}
    function for more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @class{gdk:drag-context} object on which the signal
        is emitted.}
    @end{table}
    @subheading{The \"drop-performed\" signal}
    @begin{pre}
 lambda (context time)    :run-last
    @end{pre}
    The drag and drop operation was performed on an accepting client. This
    signal will only be emitted if the @class{gdk:drag-context} object manages
    the drag and drop operation. See the @fun{gdk:drag-context-manage-dnd}
    function for more information. Since 3.20
    @begin[code]{table}
      @entry[context]{The @class{gdk:drag-context} object on which the signal
        is emitted.}
      @entry[time]{The integer with the time at which the drop happened.}
    @end{table}
  @end{dictionary}
  @see-symbol{gdk:drag-cancel-reason}
  @see-symbol{gdk:drag-protocol}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drag-context-manage-dnd}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_get_selection" drag-selection) atom-as-string
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The string for the selection, or @code{\"NONE\"}.}
  @begin{short}
    Returns the selection atom for the current source window.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:atom-as-string}"
  (context (g:object drag-context)))

(export 'drag-selection)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_abort
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_abort" drag-abort) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Aborts a drag without dropping.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (time :uint32))

(export 'drag-abort)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_reply
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_reply" drop-reply) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[accepted]{@em{true} if the drop is accepted}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Accepts or rejects a drop.
  @end{short}
  This function is called by the drag destination in response to a drop
  initiated by the drag source.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (accepted :boolean)
  (time :uint32))

(export 'drop-reply)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_drop" drag-drop) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Drops on the current destination.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (time :uint32))

(export 'drag-drop)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_done
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_drop_done" drag-drop-done) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[sucess]{a boolean whether the drag was ultimatively succesful}
  @begin{short}
    Inform GDK if the drop ended successfully.
  @end{short}
  Passing @em{false} for @arg{success} may trigger a drag cancellation
  animation. This function is called by the drag source, and should be the last
  call before dropping the reference to the drag context. The
  @class{gdk:drag-context} object will only take the first
  @fun{gdk:drag-drop-done} function call as effective, if this function is
  called multiple times, all subsequent calls will be ignored.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (success :boolean))

(export 'drag-drop-done)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_find_window_for_screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_find_window_for_screen" %drag-find-window-for-screen)
    :void
  (context (g:object drag-context))
  (window (g:object window))
  (screen (g:object screen))
  (x :int)
  (y :int)
  (dest (:pointer (g:object window)))
  (protocol (:pointer drag-protocol)))

(defun drag-find-window-for-screen (context window screen x y)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[window]{a @class{gdk:window} object which may be at the pointer
    position, but should be ignored, since it is put up by the drag source as
    an icon}
  @argument[screen]{a @class{gdk:screen} object where the destination window
    is sought}
  @argument[x]{an integer with the x position of the pointer in root
    coordinates}
  @argument[y]{an integer with the y position of the pointer in root
    coordinates}
  @begin{return}
    @code{dest} -- a @class{gdk:window} destination window @br{}
    @code{protocol} -- a @symbol{gdk:drag-protocol} DND protocol
  @end{return}
  @begin{short}
    Finds the destination window and DND protocol to use at the given pointer
    position.
  @end{short}
  This function is called by the drag source to obtain the destination window
  and protocol parameters for the @fun{gdk:drag-motion} function.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-class{gdk:screen}
  @see-symbol{gdk:drag-protocol}
  @see-function{gdk:drag-motion}"
  (cffi:with-foreign-objects ((dest :pointer) (protocol 'drag-protocol))
    (%drag-find-window-for-screen context
                                  window
                                  screen
                                  x
                                  y
                                  dest
                                  protocol)
    (values (cffi:mem-ref dest '(g:object window))
            (cffi:mem-ref protocol 'drag-protocol))))

(export 'drag-find-window-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation with %atom-intern

(cffi:defcfun ("gdk_drag_begin" %drag-begin)
    (g:object drag-context :already-referenced)
  (window (g:object window))
  (targets (g:list-t :pointer)))

(defun drag-begin (window targets)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[window]{a @class{gdk:window} source window for this drag}
  @argument[targets]{a list of strings with the offered targets}
  @return{The newly created @class{gdk:drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function assumes that the drag is controlled by the client pointer
  device, use the @fun{gdk:drag-begin-for-device} function to begin a drag with
  a different device. This function is called by the drag source.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-symbol{gdk:atom}
  @see-function{gdk:drag-begin-for-device}"
  (%drag-begin window
               (mapcar #'atom-intern targets)))

(export 'drag-begin)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_for_device ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_begin_for_device" %drag-begin-for-device)
    (g:object drag-context :already-referenced)
  (window (g:object window))
  (device (g:object device))
  (targets (g:list-t :pointer)))

(defun drag-begin-for-device (window device targets)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[window]{a @class{gdk:window} source window for this drag}
  @argument[device]{a @class{gdk:device} object that controls this drag}
  @argument[targets]{a list of strings with the offered targets}
  @return{The newly created @class{gdk:drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:atom}"
  (%drag-begin-for-device window
                          device
                          (mapcar #'atom-intern targets)))

(export 'drag-begin-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin_from_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_begin_from_point" %drag-begin-from-point)
    (g:object drag-context)
  (window (g:object window))
  (device (g:object device))
  (targets (g:list-t :pointer))
  (x :int)
  (y :int))

(defun drag-begin-from-point (window device targets x y)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[window]{a @class{gdk:window} source window for this drag}
  @argument[device]{a @class{gdk:device} object that controls this drag}
  @argument[targets]{a list of strings with the offered targets}
  @argument[x]{an integer with the x coordinate where the drag nominally
    started}
  @argument[y]{an integer with the y coordinate where the drag nominally
    started}
  @return{The newly created @class{gdk:drag-context} object.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:atom-as-string}"
  (%drag-begin-from-point window
                          device
                          (mapcar #'atom-intern targets)
                          x
                          y))

(export 'drag-begin-from-point)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_motion
;;; ----------------------------------------------------------------------------

;; TODO: The return value gboolean is not documented.

(cffi:defcfun ("gdk_drag_motion" drag-motion) :boolean
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[window]{a new @class{gdk:window} destination window, obtained by
    the @fun{gdk:drag-find-window-for-screen} function}
  @argument[protocol]{a @symbol{gdk:drag-protocol} DND protocol in use,
    obtained by the @fun{gdk:drag-find-window-for-screen} function}
  @argument[x]{an integer with the x position of the pointer in root
    coordinates}
  @argument[y]{an integer with the the y position of the pointer in root
    coordinates}
  @argument[suggested]{a @symbol{gdk:drag-action} value with the suggested
    action}
  @argument[possible]{a @symbol{gdk:drag-action} value with the possible
    actions}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Updates the drag context when the pointer moves or the set of actions
    changes.
  @end{short}
  This function is called by the drag source.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-symbol{gdk:drag-protocol}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drag-find-window-for-screen}"
  (context (g:object drag-context))
  (window (g:object window))
  (protocol drag-protocol)
  (x :int)
  (y :int)
  (suggested drag-action)
  (possible drag-action)
  (time :uint32))

(export 'drag-motion)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_finish" drop-finish) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[success]{@em{true} if the data was successfully received}
  @argument[time]{an unsigned integer with the timestamp for this operation}
  @begin{short}
    Ends the drag operation after a drop.
  @end{short}
  This function is called by the drag destination.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (success :boolean)
  (time :uint32))

(export 'drop-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_status" drag-status) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[action]{a selected @symbol{gdk:drag-action} value which will be
    taken when a drop happens, or 0 to indicate that a drop will not be
    accepted}
  @argument[time]{an integer with the timestamp for this operation}
  @begin{short}
    Selects one of the actions offered by the drag source.
  @end{short}
  This function is called by the drag destination in response to the
  @fun{gdk:drag-motion} function called by the drag source.
  @see-class{gdk:drag-context}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drag-motion}"
  (context (g:object drag-context))
  (action drag-action)
  (time :uint32))

(export 'drag-status)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_succeeded
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_drop_succeeded" drag-drop-succeeded) :boolean
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{@em{True} if the drop was successful.}
  @begin{short}
    Returns whether the dropped data has been successfully transferred.
  @end{short}
  This function is intended to be used while handling a @code{:drop-finished}
  event, its return value is meaningless at other times.
  @see-class{gdk:drag-context}
  @see-class{gdk:event-dnd}
  @see-symbol{gdk:event-type}"
  (context (g:object drag-context)))

(export 'drag-drop-succeeded)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_drag_protocol
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdK_window_get_drag_protocol" window-drag-protocol)
    drag-protocol
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[window]{a @class{gdk:window} destination window}
  @argument[target]{a @class{gdk:window} object where the drop should happen,
    this may be @arg{window} or a proxy window, or @code{nil} if @arg{window}
    does not support drag and drop}
  @return{The supported @symbol{gdk:drag-protocol} DND protocol.}
  @begin{short}
    Finds out the DND protocol supported by a window.
  @end{short}
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-symbol{gdk:drag-protocol}"
  (window (g:object window))
  (target (g:object window)))

(export 'window-drag-protocol)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_actions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_actions" drag-context-actions) drag-action
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @symbol{gdk:drag-action} flags.}
  @begin{short}
    Determines the bitmask of actions proposed by the source if the
    @fun{gdk:drag-context-suggested-action} function returns the @code{:ask}
    value.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drag-context-suggested-action}"
  (context (g:object drag-context)))

(export 'drag-context-actions)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_suggested_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_suggested_action"
          drag-context-suggested-action) drag-action
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @symbol{gdk:drag-action} flags.}
  @begin{short}
    Determines the suggested drag action of the drag context.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:drag-action}"
  (context (g:object drag-context)))

(export 'drag-context-suggested-action)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_selected_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_selected_action"
           drag-context-selected-action) drag-action
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @symbol{gdk:drag-action} flags.}
  @begin{short}
    Determines the action chosen by the drag destination.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:drag-action}"
  (context (g:object drag-context)))

(export 'drag-context-selected-action)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_list_targets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_list_targets" drag-context-list-targets)
    (g:list-t atom-as-string :free-from-foreign nil)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The list of strings with the targets.}
  @begin{short}
    Retrieves the list of targets of the drag context.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:atom-as-string}"
  (context (g:object drag-context)))

(export 'drag-context-list-targets)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_device
;;; gdk_drag_context_set_device
;;; ----------------------------------------------------------------------------

(defun (setf drag-context-device) (device context)
  (cffi:foreign-funcall "gdk_drag_context_set_device"
                   (g:object drag-context) context
                   (g:object device) device
                   :void)
  device)

(cffi:defcfun ("gdk_drag_context_get_device" drag-context-device) (g:object device)
 #+liber-documentation
 "@version{#2024-6-28}
  @syntax[]{(gdk:drag-context-device context) => device}
  @syntax[]{(setf (gdk:drag-context-device context) device)}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[device]{a @class{gdk:device} object associated to @arg{context}}
  @begin{short}
    Associates a device to the drag context, so all drag and drop events for
    the drag context are emitted as if they came from this device.
  @end{short}
  @see-class{gdk:drag-context}
  @see-class{gdk:device}"
  (context (g:object drag-context)))

(export 'drag-context-device)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_source_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_source_window" drag-context-source-window)
    (g:object window)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @class{gdk:window} object.}
  @begin{short}
    Returns the window where the DND operation started.
  @end{short}
  @see-class{gdk:drag-context}
  @see-class{gdk:window}"
  (context (g:object drag-context)))

(export 'drag-context-source-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_dest_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_dest_window" drag-context-dest-window)
    (g:object window)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @class{gdk:window} object.}
  @begin{short}
    Returns the destination windw for the DND operation.
  @end{short}
  @see-class{gdk:drag-context}
  @see-class{gdk:window}"
  (context (g:object drag-context)))

(export 'drag-context-dest-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_protocol
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_protocol" drag-context-protocol)
    drag-protocol
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @symbol{gdk:drag-protocol} drag protocol.}
  @begin{short}
    Returns the drag protocol that is used by this drag context.
  @end{short}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:drag-protocol}"
  (context (g:object drag-context)))

(export 'drag-context-protocol)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_get_drag_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_get_drag_window" drag-context-drag-window)
    (g:object window)
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @return{The @class{gdk:window} drag window, or @code{nil}.}
  @begin{short}
    Returns the window on which the drag icon should be rendered during the
    drag operation.
  @end{short}
  Note that the window may not be available until the drag operation has begun.
  GDK will move the window in accordance with the ongoing drag operation. The
  window is owned by context and will be destroyed when the drag operation is
  over.
  @see-class{gdk:drag-context}
  @see-class{gdk:window}"
  (context (g:object drag-context)))

(export 'drag-context-drag-window)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_set_hotspot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_set_hotspot" drag-context-set-hotspot) :void
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[x]{an integer with the x coordinate of the drag window hotspot}
  @argument[y]{an integer with the y coordinate of the drag window hotspot}
  @begin{short}
    Sets the position of the drag window that will be kept under the cursor
    hotspot.
  @end{short}
  Initially, the hotspot is at the top left corner of the drag window.
  @see-class{gdk:drag-context}"
  (context (g:object drag-context))
  (x :int)
  (y :int))

(export 'drag-context-set-hotspot)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_context_manage_dnd
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_context_manage_dnd" drag-context-manage-dnd) :boolean
 #+liber-documentation
 "@version{#2024-6-28}
  @argument[context]{a @class{gdk:drag-context} object}
  @argument[window]{a @class{gdk:window} object to use for IPC messaging/events}
  @argument[actions]{a @symbol{gdk:drag-action} value supported by the drag
    source}
  @return{@em{True} if the drag and drop operation is managed.}
  @begin{short}
    Requests the drag and drop operation to be managed by @arg{context}.
  @end{short}
  When a drag and drop operation becomes managed, the @class{gdk:drag-context}
  object will internally handle all input and source-side @class{gdk:event-dnd}
  events as required by the windowing system.

  Once the drag and drop operation is managed, the drag context will emit the
  following signals:
  @begin{itemize}
    @begin{item}
      The @code{\"action-changed\"} signal whenever the final action to be
      performed by the drag and drop operation changes.
    @end{item}
    @begin{item}
      The @code{\"drop-performed\"} signal after the user performs the drag and
      drop gesture, typically by releasing the mouse button.
    @end{item}
    @begin{item}
      The @code{\"dnd-finished\"} signal after the drag and drop operation
      concludes, after all GDK selection transfers happen.
    @end{item}
    @begin{item}
      The @code{\"cancel\"} signal if the drag and drop operation is finished
      but does not happen over an accepting destination, or is cancelled through
      other means.
    @end{item}
  @end{itemize}
  @see-class{gdk:drag-context}
  @see-class{gdk:window}
  @see-symbol{gdk:drag-action}
  @see-class{gdk:event-dnd}"
  (context (g:object drag-context))
  (window (g:object gdk:window))
  (actions drag-action))

(export 'drag-context-manage-dnd)

;;; --- End of file gdk3.drag-and-drop.lisp ------------------------------------
