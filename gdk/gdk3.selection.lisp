;;; ----------------------------------------------------------------------------
;;; gdk3.selections.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
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
;;; Selections
;;;
;;;     Functions for transfering data via the X selection mechanism
;;;
;;; Functions
;;;
;;;     gdk_selection_owner_set
;;;     gdk_selection_owner_set_for_display
;;;     gdk_selection_owner_get
;;;     gdk_selection_owner_get_for_display
;;;     gdk_selection_convert
;;;     gdk_selection_property_get
;;;     gdk_selection_send_notify
;;;     gdk_selection_send_notify_for_display
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_owner_set" selection-owner-set) :boolean
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[owner]{a @class{gdk:window} object or @code{nil} to indicate that
    the owner for the given selection should be unset}
  @argument[selection]{a string identifying a selection}
  @argument[time]{an unsigned integer for the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @begin{return}
    @em{True} if the selection owner was successfully changed to @arg{owner},
    otherwise @em{false}.
  @end{return}
  @begin{short}
    Sets the owner of the given selection.
  @end{short}
  If the @arg{time} argument is older than the timestamp given last time the
  owner was set for the given selection, the request will be ignored.
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}"
  (owner (g:object window))
  (selection atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_owner_set_for_display"
           selection-owner-set-for-display) :boolean
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[display]{a @class{gdk:display} object}
  @argument[owner]{a @class{gdk:window} object or @code{nil} to indicate that
    the owner for the given selection should be unset}
  @argument[selection]{a string identifying a selection}
  @argument[time]{an unsigned integer for the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @begin{return}
     @em{True} if the selection owner was successfully changed to @arg{owner},
     otherwise @em{false}.
  @end{return}
  @begin{short}
    Sets the owner of the given selection.
  @end{short}
  If the @arg{time} argument is older than the timestamp given last time the
  owner was set for the given selection, the request will be ignored.
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}"
  (display (g:object display))
  (owner (g:object window))
  (selection atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_owner_get" selection-owner-get) (g:object window)
 #+liber-documentation
 "@version{#2024-06-28}
  @argument[selection]{a string indentifying a selection}
  @begin{return}
    If there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk:window} object that owns the
    selection, otherwise @code{nil}.
  @end{return}
  @begin{short}
    Determines the owner of the given selection.
  @end{short}
  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.
  @see-class{gdk:window}"
  (selection atom-as-string))

(export 'selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_owner_get_for_display"
           selection-owner-get-for-display) (g:object window)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[display]{a @class{gdk:display} object}
  @argument[selection]{a string indentifying a selection}
  @begin{return}
    If there is a selection owner for this window, and it is a window known to
    the current process, the @class{gdk:window} object that owns the selection,
    otherwise @code{nil}.
  @end{return}
  @begin{short}
    Determine the owner of the given selection.
  @end{short}
  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.
  @see-class{gdk:display}
  @see-class{gdk:window}"
  (display (g:object display))
  (selection atom-as-string))

(export 'selection-owner-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_convert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_convert" selection-convert) :void
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[requestor]{a @class{gdk:window} object}
  @argument[selection]{a string identifying the selection to get the contents
    of}
  @argument[target]{a string for the form in which to retrieve the selection}
  @argument[time]{an unsigned integer for the timestamp to use when retrieving
    the selection}
  @begin{short}
    Retrieves the contents of a selection in a given form.
  @end{short}
  The selection owner may refuse the request if it did not own the selection at
  the time indicated by the timestamp.
  @see-class{gdk:window}"
  (requestor (g:object window))
  (selection atom-as-string)
  (target atom-as-string)
  (time :uint32))

(export 'selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_property_get" %selection-property-get) :int
  (requestor (g:object window))
  (data :pointer)
  (ptype :pointer)
  (format :pointer))

(defun selection-property-get (requestor)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[requestor]{a @class{gdk:window} object on which the data is stored}
  @begin{return}
    @code{length} -- an integer for the length of the retrieved data @br{}
    @code{data} -- a foreign pointer to the retrieved data @br{}
    @code{type} -- a string for the type of the property @br{}
    @code{format} -- an integer for the format of the property
  @end{return}
  @begin{short}
    Retrieves selection data that was stored in response to a call to the
    @fun{gdk:selection-convert} function.
  @end{short}
  This function will not be used by applications, who should use the
  @class{gtk:clipboard} API instead.

  If the retrieval failed, @code{nil} will be returned, otherwise, it will be
  non-@code{nil} and the returned data should be freed with the @fun{g:free}
  function when you are finished using it. The length of the allocated memory
  is one more than the length of the returned data, and the final byte will
  always be zero, to ensure nul-termination of strings.
  @see-class{gdk:window}
  @see-class{gtk:clipboard}
  @see-function{gdk:selection-convert}"
  (cffi:with-foreign-objects ((data :pointer)
                              (ptype :pointer)
                              (format :int))
    (let ((length (%selection-property-get requestor data ptype format)))
      (values length
              data
              (cffi:mem-ref ptype 'atom-as-string)
              (cffi:mem-ref format :int)))))

(export 'selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_send_notify" selection-send-notify) :void
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[requestor]{a @class{gdk:window} object to which to deliver response}
  @argument[selection]{a string for the selection that was requested}
  @argument[target]{a string for the target that was selected}
  @argument[property]{a string for the property in which the selection owner
    stored the data, or @code{\"NONE\"} to indicate that the request was
    rejected}
  @argument[time]{an unsigned integer for the timestamp}
  @begin{short}
    Sends a response to the @code{:selection-request} event.
  @end{short}
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}"
  (requestor (g:object window))
  (selection atom-as-string)
  (target atom-as-string)
  (property atom-as-string)
  (time :uint32))

(export 'selection-send-notify)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_selection_send_notify_for_display"
          selection-send-notify-for-display) :void
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[display]{a @class{gdk:display} object where @arg{requestor} is
    realized}
  @argument[requestor]{a @class{gdk:window} object to which to deliver response}
  @argument[selection]{a string for the selection that was requested}
  @argument[target]{a string for the target that was selected}
  @argument[property]{a string for the property in which the selection owner
    stored the data, or @code{\"NONE\"} to indicate that the request was
    rejected}
  @argument[time]{an unsigned integer for the timestamp}
  @begin{short}
    Send a response to the @code{:selection-request} event.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}"
  (display (g:object display))
  (requestor (g:object window))
  (selection atom-as-string)
  (target atom-as-string)
  (property atom-as-string)
  (time :uint32))

(export 'selection-send-notify-for-display)

;;; --- End of file gdk3.selection.lisp ----------------------------------------
