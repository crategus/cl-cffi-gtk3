;;; ----------------------------------------------------------------------------
;;; gdk.selections.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; Selections
;;;
;;;     Functions for transfering data via the X selection mechanism
;;;
;;; Types and Values
;;;
;;;     GDK_SELECTION_PRIMARY
;;;     GDK_SELECTION_SECONDARY
;;;     GDK_SELECTION_CLIPBOARD
;;;
;;;     GDK_TARGET_BITMAP
;;;     GDK_TARGET_COLORMAP
;;;     GDK_TARGET_DRAWABLE
;;;     GDK_TARGET_PIXMAP
;;;     GDK_TARGET_STRING
;;;
;;;     GDK_SELECTION_TYPE_ATOM
;;;     GDK_SELECTION_TYPE_BITMAP
;;;     GDK_SELECTION_TYPE_COLORMAP
;;;     GDK_SELECTION_TYPE_DRAWABLE
;;;     GDK_SELECTION_TYPE_INTEGER
;;;     GDK_SELECTION_TYPE_PIXMAP
;;;     GDK_SELECTION_TYPE_WINDOW
;;;     GDK_SELECTION_TYPE_STRING
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
;;; GDK_SELECTION_PRIMARY                                  not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-primary+ "PRIMARY"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"PRIMARY\"}
  A @symbol{gdk:atom} representing the @code{\"PRIMARY\"} selection.
  @see-symbol{gdk:atom}
  @see-variable{+gdk-selection-secondary+}
  @see-variable{+gdk-selection-clipboard+}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_SECONDARY                                not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-secondary+ "SECONDARY"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"SECONDARY\"}
  A @symbol{gdk:atom} representing the @code{\"SECONDARY\"} selection.
  @see-symbol{gdk:atom}
  @see-variable{+gdk-selection-primary+}
  @see-variable{+gdk-selection-clipboard+}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_CLIPBOARD                                not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-clipboard+ "CLIPBOARD"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"CLIPBOARD\"}
  A @symbol{gdk:atom} representing the @code{\"CLIPBOARD\"} selection.
  @see-symbol{gdk:atom}
  @see-variable{+gdk-selection-primary+}
  @see-variable{+gdk-selection-secondary+}")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_BITMAP                                      not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-bitmap+ "BITMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk:atom} representing the @code{\"BITMAP\"} selection target.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_COLORMAP                                    not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-colormap+ "COLORMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk:atom} representing the @code{\"COLORMAP\"} selection target.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_DRAWABLE                                    not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-drawable+ "DRAWABLE"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk:atom} representing the @code{\"DRAWABLE\"} selection target.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_PIXMAP                                      not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-pixmap+ "PIXMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk:atom} representing the @code{\"PIXMAP\"} selection target.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_TARGET_STRING                                      not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-target-string+ "STRING"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"STRING\"}
  A @symbol{gdk:atom} representing the @code{\"STRING\"} selection target.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_ATOM                                not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-atom+ "ATOM"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"ATOM\"}
  A @symbol{gdk:atom} representing the @code{\"ATOM\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_BITMAP                              not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-bitmap+ "BITMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"BITMAP\"}
  A @symbol{gdk:atom} representing the @code{\"BITMAP\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_COLORMAP                            not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-colormap+ "COLORMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"COLORMAP\"}
  A @symbol{gdk:atom} representing the @code{\"COLORMAP\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_DRAWABLE                            not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-drawable+ "DRAWABLE"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"DRAWABLE\"}
  A @symbol{gdk:atom} representing the @code{\"DRAWABLE\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_INTEGER                             not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-integer+ "INTEGER"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"INTEGER\"}
  A @symbol{gdk:atom} representing the @code{\"INTEGER\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_PIXMAP                              not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-pixmap+ "PIXMAP"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"PIXMAP\"}
  A @symbol{gdk:atom} representing the @code{\"PIXMAP\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_WINDOW                              not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-window+ "WINDOW"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"WINDOW\"}
  A @symbol{gdk:atom} representing the @code{\"WINDOW\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; GDK_SELECTION_TYPE_STRING                              not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +gdk-selection-type-string+ "STRING"
 #+liber-documentation
 "@version{#2013-11-10}
  @variable-value{\"STRING\"}
  A @symbol{gdk:atom} representing the @code{\"STRING\"} selection type.
  @see-symbol{gdk:atom}")

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set" selection-owner-set) :boolean
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[owner]{a @class{gdk:window} object or @code{nil} to indicate that
    the owner for the given selection should be unset}
  @argument[selection]{a @symbol{gdk:atom} as a string identifying a selection}
  @argument[time]{an unsigned integer with the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @return{@em{True} if the selection owner was successfully changed to
    @arg{owner}, otherwise @em{false}.}
  @begin{short}
    Sets the owner of the given selection.
  @end{short}
  If the @arg{time} argument is older than the timestamp given last time the
  owner was set for the given selection, the request will be ignored.
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:atom}"
  (owner (g:object window))
  (selection atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'selection-owner-set)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_set_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_set_for_display"
           selection-owner-set-for-display) :boolean
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[display]{a @class{gdk:display} object}
  @argument[owner]{a @class{gdk:window} object or @code{nil} to indicate that
    the owner for the given selection should be unset}
  @argument[selection]{a @symbol{gdk:atom} as string identifying a selection}
  @argument[time]{an unsigned integer with the timestamp to use when setting
    the selection}
  @argument[send]{if @em{true}, and the new owner is different from the current
    owner, the current owner will be sent a @code{:selection-clear} event}
  @return{@em{True} if the selection owner was successfully changed to
    @arg{owner}, otherwise @em{false}.}
  @begin{short}
    Sets the owner of the given selection.
  @end{short}
  If the @arg{time} argument is older than the timestamp given last time the
  owner was set for the given selection, the request will be ignored.
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:atom}"
  (display (g:object display))
  (owner (g:object window))
  (selection atom-as-string)
  (time :uint32)
  (send :boolean))

(export 'selection-owner-set-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get" selection-owner-get) (g:object window)
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[selection]{a @symbol{gdk:atom} as a string indentifying a selection}
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
  @see-class{gdk:window}
  @see-symbol{gdk:atom}"
  (selection atom-as-string))

(export 'selection-owner-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_owner_get_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_owner_get_for_display"
           selection-owner-get-for-display) (g:object window)
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[display]{a @class{gdk:display} object}
  @argument[selection]{a @symbol{gdk:atom} as a string indentifying a selection}
  @return{If there is a selection owner for this window, and it is a window
    known to the current process, the @class{gdk:window} object that owns the
    selection, otherwise @code{nil}.}
  @begin{short}
    Determine the owner of the given selection.
  @end{short}
  Note that the return value may be owned by a different process if a foreign
  window was previously created for that window, but a new foreign window will
  never be created by this call.
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-symbol{gdk:atom}"
  (display (g:object display))
  (selection atom-as-string))

(export 'selection-owner-get-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_convert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_convert" selection-convert) :void
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[requestor]{a @class{gdk:window} object}
  @argument[selection]{a @symbol{gdk:atom} as a string identifying the selection
    to get the contents of}
  @argument[target]{a @symbol{gdk:atom} as a string with the form in which to
    retrieve the selection}
  @argument[time]{an unsigned integer with the timestamp to use when retrieving
    the selection}
  @begin{short}
    Retrieves the contents of a selection in a given form.
  @end{short}
  The selection owner may refuse the request if it did not own the selection at
  the time indicated by the timestamp.
  @see-class{gdk:window}
  @see-symbol{gdk:atom}"
  (requestor (g:object window))
  (selection atom-as-string)
  (target atom-as-string)
  (time :uint32))

(export 'selection-convert)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_property_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_property_get" %selection-property-get) :int
  (requestor (g:object window))
  (data :pointer)
  (ptype :pointer)
  (format :pointer))

(defun selection-property-get (requestor)
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[requestor]{a @class{gdk:window} object on which the data is stored}
  @begin{return}
    @code{length} -- an integer with the length of the retrieved data @br{}
    @code{data} -- a foreign pointer to the retrieved data @br{}
    @code{type} -- a @symbol{gdk:atom} as as string with the type of the
    property @br{}
    @code{format} -- an integer with the format of the property
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
  @see-symbol{gdk:atom}
  @see-function{gdk:selection-convert}"
  (with-foreign-objects ((data :pointer)
                         (ptype :pointer)
                         (format :int))
    (let ((length (%selection-property-get requestor data ptype format)))
      (values length
              data
              (cffi:mem-ref ptype 'atom-as-string)
              (cffi:mem-ref format :int)))))

(export 'selection-property-get)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify" selection-send-notify) :void
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[requestor]{a @class{gdk:window} object to which to deliver response}
  @argument[selection]{a @symbol{gdk:atom} as a string with the selection that
    was requested}
  @argument[target]{a @symbol{gdk:atom} as a string with the target that was
    selected}
  @argument[property]{a @symbol{gdk:atom} as a string with the property in
    which the selection owner stored the data, or \"NONE\" to indicate that
    the request was rejected}
  @argument[time]{an unsigned integer with the timestamp}
  @begin{short}
    Sends a response to the @code{:selection-request} event.
  @end{short}
  @see-class{gdk:window}
  @see-symbol{gdk:atom}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:event-type}"
  (requestor (g:object window))
  (selection atom-as-string)
  (target atom-as-string)
  (property atom-as-string)
  (time :uint32))

(export 'selection-send-notify)

;;; ----------------------------------------------------------------------------
;;; gdk_selection_send_notify_for_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_selection_send_notify_for_display"
          selection-send-notify-for-display) :void
 #+liber-documentation
 "@version{#2021-10-3}
  @argument[display]{a @class{gdk:display} object where @arg{requestor} is
    realized}
  @argument[requestor]{a @class{gdk:window} object to which to deliver response}
  @argument[selection]{a @symbol{gdk:atom} as a string with the selection that
    was requested}
  @argument[target]{a @symbol{gdk:atom} as a string with the target that was
    selected}
  @argument[property]{a @symbol{gdk:atom} as a string with the property in
    which the selection owner stored the data, or \"NONE\" to indicate that the
    request was rejected}
  @argument[time]{an unsigned integer with the timestamp}
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

;;; --- End of file gdk.selection.lisp -----------------------------------------
