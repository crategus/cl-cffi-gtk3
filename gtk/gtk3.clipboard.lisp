;;; ----------------------------------------------------------------------------
;;; gtk3.clipboard.lisp
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
;;; Clipboards
;;;
;;;     Storing data on clipboards
;;;
;;; Types and Values
;;;
;;;     GtkClipboard
;;;
;;; Functions
;;;
;;;     GtkClipboardReceivedFunc
;;;     GtkClipboardTextReceivedFunc
;;;     GtkClipboardImageReceivedFunc
;;;     GtkClipboardTargetsReceivedFunc
;;;     GtkClipboardRichTextReceivedFunc
;;;     GtkClipboardURIReceivedFunc
;;;     GtkClipboardGetFunc
;;;     GtkClipboardClearFunc
;;;
;;;     gtk_clipboard_get
;;;     gtk_clipboard_get_for_display
;;;     gtk_clipboard_get_display
;;;     gtk_clipboard_get_default
;;;     gtk_clipboard_set_with_data
;;;     gtk_clipboard_set_with_owner                        not implemented
;;;     gtk_clipboard_get_owner                             not implemented
;;;     gtk_clipboard_clear
;;;     gtk_clipboard_set_text
;;;     gtk_clipboard_set_image
;;;     gtk_clipboard_request_contents
;;;     gtk_clipboard_request_text
;;;     gtk_clipboard_request_image
;;;     gtk_clipboard_request_targets
;;;     gtk_clipboard_request_rich_text
;;;     gtk_clipboard_request_uris
;;;     gtk_clipboard_wait_for_contents
;;;     gtk_clipboard_wait_for_text
;;;     gtk_clipboard_wait_for_image
;;;     gtk_clipboard_wait_for_rich_text
;;;     gtk_clipboard_wait_for_uris
;;;     gtk_clipboard_wait_is_text_available
;;;     gtk_clipboard_wait_is_image_available
;;;     gtk_clipboard_wait_is_rich_text_available
;;;     gtk_clipboard_wait_is_uris_available
;;;     gtk_clipboard_wait_for_targets
;;;     gtk_clipboard_wait_is_target_available
;;;     gtk_clipboard_set_can_store
;;;     gtk_clipboard_store
;;;     gtk_clipboard_get_selection
;;;
;;; Signals
;;;
;;;     owner-change
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkClipboard
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkClipboard
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkClipboard" clipboard
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_clipboard_get_type")
  nil)

#+liber-documentation
(setf (documentation 'clipboard 'type)
 "@version{#2025-07-02}
  @begin{short}
    The @class{gtk:clipboard} object represents a clipboard of data shared
    between different processes or between different widgets in the same
    process.
  @end{short}
  Each clipboard is identified by a name encoded as a @type{gdk:atom-as-string}
  type. The default clipboard corresponds to the \"CLIPBOARD\" atom. Another
  commonly used clipboard is the \"PRIMARY\" clipboard, which, in X,
  aditionally contains the currently selected text.

  To support having a number of different formats on the clipboard at the same
  time, the clipboard mechanism allows providing callback functions instead of
  the actual data. When you set the contents of the clipboard, you can either
  supply the data directly via functions like @fun{gtk:clipboard-set-text},
  or you can supply a callback function to be called at a later time when the
  data is needed via the @fun{gtk:clipboard-set-with-data} function. Providing
  a callback function also avoids having to make copies of the data when it is
  not needed.

  Requesting the data from the clipboard is essentially asynchronous. If the
  contents of the clipboard are provided within the same process, then a
  direct function call will be made to retrieve the data, but if they are
  provided by another process, then the data needs to be retrieved from the
  other process, which may take some time. To avoid blocking the user
  interface, the call to request the selection, the
  @fun{gtk:clipboard-request-contents} function takes a callback function that
  will be called when the contents are received or when the request fails. If
  you do not want to deal with providing a separate callback function, you can
  also use the @fun{gtk:clipboard-wait-for-contents} function. What this does is
  run the GLib main loop recursively waiting for the contents. This can simplify
  the code flow, but you still have to be aware that other callback functions
  in your program can be called while this recursive main loop is running.

  Along with the functions to get the clipboard contents as an arbitrary data
  chunk, there are also functions to retrieve it as text, the
  @fun{gtk:clipboard-request-text} and @fun{gtk:clipboard-wait-for-text}
  functions. These functions take care of determining which formats are
  advertised by the clipboard provider, asking for the clipboard in the best
  available format and converting the results into the UTF-8 encoding, which is
  the standard form for representing strings in GTK.
  @begin[Signal Details]{dictionary}
    @begin[clipboard::owner-change]{signal}
      @begin{pre}
lambda (clipboard event)    :run-first
      @end{pre}
      @begin[arg]{simple-table}
        @entry[clipboard]{The @class{gtk:clipboard} object on which the signal
          is emitted.}
        @entry[event]{The @class{gdk:event-owner-change} event.}
      @end{simple-table}
      The signal is emitted when GTK receives an event that indicates that the
      ownership of the selection associated with the clipboard has changed.
    @end{signal}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_get" clipboard-get)
    (g:object clipboard :free-from-foreign nil)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[selection]{a string which identifies the clipboard to use}
  @begin{return}
    The appropriate @class{gtk:clipboard} object. If no clipboard already
    exists, a new one will be created. Once a clipboard has been created, it is
    persistent.
  @end{return}
  @begin{short}
    Returns the clipboard for the given selection.
  @end{short}
  See the @fun{gtk:clipboard-for-display} function for complete details.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-for-display}"
  (selection gdk:atom-as-string))

(export 'clipboard-get)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_get_for_display" clipboard-for-display)
    (g:object clipboard :free-from-foreign nil)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[display]{a @class{gdk:display} object for which the clipboard is
    to be retrieved or created}
  @argument[selection]{a string which identifies the clipboard to use}
  @begin{return}
    The appropriate @class{gtk:clipboard} object. If no clipboard already
    exists, a new one will be created. Once a clipboard has been created, it is
    persistent.
  @end{return}
  @begin{short}
    Returns the clipboard for the given selection.
  @end{short}
  Cut/Copy/Paste menu items and keyboard shortcuts should use the default
  clipboard, returned by the \"CLIPBOARD\" atom for @arg{selection}.
  The \"NONE\" atom is supported as a synonym for the \"CLIPBOARD\" atom for
  backwards compatibility reasons. The currently selected object or text should
  be provided on the clipboard identified by the \"PRIMARY\" atom.
  Cut/Copy/Paste menu items conceptually copy the contents of the \"PRIMARY\"
  clipboard to the default clipboard, that is, they copy the selection to what
  the user sees as the clipboard.

  It is possible to have arbitrary named clipboards. If you do invent new
  clipboards, you should prefix the selection name with an underscore, because
  the ICCCM requires that nonstandard atoms are underscore-prefixed, and
  namespace it as well. For example, if your application called \"Foo\" has a
  special purpose clipboard, you might call it \"_FOO_SPECIAL_CLIPBOARD\".
  @see-class{gtk:clipboard}
  @see-class{gdk:display}
  @see-function{gtk:clipboard-get}"
  (display (g:object gdk:display))
  (selection gdk:atom-as-string))

(export 'clipboard-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_get_display" clipboard-display)
    (g:object gdk:display)
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @return{The @class{gdk:display} object associated with @arg{clipboard}.}
  @begin{short}
    Gets the display associated with the clipboard.
  @end{short}
  @see-class{gtk:clipboard}
  @see-class{gdk:display}"
  (clipboard (g:object clipboard)))

(export 'clipboard-display)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_get_default" clipboard-default)
    (g:object clipboard)
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[display]{a @class{gdk:display} object for which the clipboard is
    to be retrieved}
  @return{The default @class{gtk:clipboard} object.}
  @begin{short}
    Returns the default clipboard for use with Cut/Copy/Paste menu items and
    keyboard shortcuts.
  @end{short}
  @see-class{gtk:clipboard}
  @see-class{gdk:display}"
  (display (g:object gdk:display)))

(export 'clipboard-default)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardClearFunc                                   not exported
;;; ----------------------------------------------------------------------------

;; Implemented for internal use in the CLIPBOARD-SET-WITH-DATA function

(cffi:defcallback %clipboard-clear-func :void
    ((clipboard (g:object clipboard))
     (data :pointer))
  (declare (ignore clipboard))
  (glib:free-stable-pointer data))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-clear-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-clear-func)
 "@version{#2024-03-23}
  @syntax{lambda (clipboard)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{short}
    A callback function that will be called when the contents of the clipboard
    are changed or cleared.
  @end{short}
  @see-class{gtk:clipboard}")

;;; ----------------------------------------------------------------------------
;;; GtkClipboardGetFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-get-func :void
    ((clipboard (g:object clipboard))
     (selection (g:boxed selection-data))
     (info :uint)
     (data :pointer))
  (let ((fn (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall fn clipboard selection info)
      (return-from-clipboard-get-func () nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-get-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-get-func)
 "@version{#2025-07-03}
  @syntax{lambda (clipboard selection info)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[selection]{a @class{gtk:selection-data} instance in which the
    requested data should be stored}
  @argument[info]{an unsigned integer for the info field corresponding to the
    requested target from the target entries passed to the
    @fun{gtk:clipboard-set-with-data} function}
  @begin{short}
    A callback function that will be called to provide the contents of the
    selection.
  @end{short}
  If multiple types of data were advertised, the requested type can be
  determined from the @arg{info} parameter or by checking the @arg{target} field
  of the @arg{selection} argument. If the data could successfully be converted
  into then it should be stored into the @arg{selection} argument by calling the
  @fun{gtk:selection-data-set} function, or related functions such as the
  @fun{gtk:selection-data-text} function. If no data is set, the requestor will
  be informed that the attempt to get the data failed.
  @see-class{gtk:clipboard}
  @see-class{gtk:selection-data}
  @see-function{gtk:selection-data-set}
  @see-function{gtk:selection-data-text}
  @see-function{gtk:clipboard-set-with-data}")

(export 'clipboard-get-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_with_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_set_with_data" %clipboard-set-with-data) :boolean
  (clipboard (g:object clipboard))
  (targets :pointer)
  (n-targets :int)
  (func :pointer)
  (clear-func :pointer)
  (data :pointer))

(defun clipboard-set-with-data (clipboard targets func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[targets]{a list of target entries containing information about the
    available forms for the clipboard data}
  @argument[func]{a @sym{gtk:clipboard-get-func} callback function to call to
    get the actual clipboard data}
  @begin{return}
    @em{True} if setting the clipboard data succeeded. If setting the
    clipboard data failed the provided callback functions will be ignored.
  @end{return}
  @begin{short}
    Virtually sets the contents of the specified clipboard by providing a list
    of supported formats for the clipboard data and a function to call to get
    the actual data when it is requested.
  @end{short}
  See the @class{gtk:target-list} documentation for the syntax of target
  entries.
  @see-class{gtk:clipboard}
  @see-class{gtk:target-list}
  @see-symbol{gtk:clipboard-get-func}"
  (let ((n-targets (length targets)))
    (cffi:with-foreign-object (targets-ptr '(:struct %target-entry) n-targets)
      (loop for i from 0 below n-targets
            for target-ptr = (cffi:mem-aptr targets-ptr
                                            '(:struct %target-entry) i)
            for entry in targets
            do (cffi:with-foreign-slots ((target flags info)
                                         target-ptr
                                         (:struct %target-entry))
                 (setf target (first entry))
                 (setf flags (second entry))
                 (setf info (third entry))))
      (%clipboard-set-with-data clipboard
                                targets-ptr
                                n-targets
                                (cffi:callback clipboard-get-func)
                                (cffi:callback %clipboard-clear-func)
                                (glib:allocate-stable-pointer func)))))

(export 'clipboard-set-with-data)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_with_owner                            not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_owner                                 not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_clear" clipboard-clear) :void
 #+liber-documentation
 "@version{#2023-3-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{short}
    Clears the contents of the clipboard.
  @end{short}
  Generally this should only be called after the time you call the
  @fun{gtk:clipboard-set-with-data} function. Otherwise, the clipboard may be
  owned by someone else.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-set-with-data}"
  (clipboard (g:object clipboard)))

(export 'clipboard-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_set_text" %clipboard-set-text) :void
  (clipboard (g:object clipboard))
  (text :string)
  (len :int))

(defun clipboard-set-text (clipboard text)
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[text]{a UTF-8 string}
  @begin{short}
    Sets the contents of the clipboard to the given UTF-8 string.
  @end{short}
  GTK will make a copy of the @arg{text} argument and take responsibility for
  responding for requests for the text, and for converting the text into the
  requested format.
  @see-class{gtk:clipboard}"
  (%clipboard-set-text clipboard text -1))

(export 'clipboard-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_set_image" clipboard-set-image) :void
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Sets the contents of the clipboard to the given pixbuf.
  @end{short}
  GTK will take responsibility for responding for requests for the image, and
  for converting the image into the requested format.
  @see-class{gtk:clipboard}
  @see-class{gdk-pixbuf:pixbuf}"
  (clipboard (g:object clipboard))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'clipboard-set-image)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-received-func :void
    ((clipboard (g:object clipboard))
     (selection (g:boxed selection-data))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard selection)
      (return () "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-received-func)
 "@version{#2024-03-18}
  @syntax{lambda (clipboard selection)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[selection]{a @class{gtk:selection-data} instance containing the data
    that was received, if retrieving the data failed, then the @arg{length}
    field of the selection data will be negative}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-contents} function are received, or when the
    request fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-class{gtk:selection-data}
  @see-function{gtk:selection-data-length}
  @see-function{gtk:clipboard-request-contents}")

(export 'clipboard-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_contents" %clipboard-request-contents)
    :void
  (clipboard (g:object clipboard))
  (target gdk:atom-as-string)
  (func :pointer)
  (data :pointer))

(defun clipboard-request-contents (clipboard target func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[target]{a string representing the form into which the clipboard
    owner should convert the selection}
  @argument[func]{a @sym{gtk:clipboard-received-func} callback function to call
    when the results are received, or the retrieval fails, if the retrieval
    fails the @arg{length} field of the @class{gtk:selection-data} instance
    will be negative}
  @begin{short}
    Requests the contents of the clipboard as the given target.
  @end{short}
  When the results of the result are later received the supplied callback
  function will be called.
  @see-class{gtk:clipboard}
  @see-class{gtk:selection-data}
  @see-function{gtk:selection-data-length}"
  (%clipboard-request-contents clipboard
                               target
                               (cffi:callback clipboard-received-func)
                               (glib:allocate-stable-pointer func)))

(export 'clipboard-request-contents)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardTextReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-text-received-func :void
    ((clipboard (g:object clipboard))
     (text :string)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard text)
      (return () "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-text-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-text-received-func)
 "@version{#2024-03-18}
  @syntax{lambda (clipboard text)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[text]{a text, received as a UTF-8 encoded string, or @code{nil} if
    retrieving the data failed}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-text} function are received, or when the request
    fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-request-text}")

(export 'clipboard-text-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_text" %clipboard-request-text) :void
  (clipboard (g:object clipboard))
  (func :pointer)
  (data :pointer))

(defun clipboard-request-text (clipboard func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[func]{a @sym{gtk:clipboard-text-received-func} callback function
    to call when the text is received, or the retrieval fails, it will always
    be called one way or the other}
  @begin{short}
    Requests the contents of the clipboard as text.
  @end{short}
  When the text is later received, it will be converted to UTF-8 if necessary,
  and the callback function will be called.

  The @arg{text} parameter to the callback function will contain the resulting
  text if the request succeeded, or @code{nil} if it failed. This could happen
  for various reasons, in particular if the clipboard was empty or if the
  contents of the clipboard could not be converted into text form.
  @see-class{gtk:clipboard}
  @see-symbol{gtk:clipboard-text-received-func}"
  (%clipboard-request-text clipboard
                           (cffi:callback clipboard-text-received-func)
                           (glib:allocate-stable-pointer func)))

(export 'clipboard-request-text)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardImageReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-image-received-func :void
    ((clipboard (g:object clipboard))
     (pixbuf (g:object gdk-pixbuf:pixbuf))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard pixbuf)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-image-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-image-received-func)
 "@version{#2024-03-18}
  @syntax{lambda (clipboard pixbuf)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[pixbuf]{a received @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-image} function are received, or when the
    request fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:clipboard-request-image}")

(export 'clipboard-image-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_image" %clipboard-request-image) :void
  (clipboard (g:object clipboard))
  (func :pointer)
  (data :pointer))

(defun clipboard-request-image (clipboard func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[func]{a @sym{gtk:clipboard-image-received-func} callback function
    to call when the image is received, or the retrieval fails. It will always
    be called one way or the other}
  @begin{short}
    Requests the contents of the clipboard as image.
  @end{short}
  When the image is later received, it will be converted to a
  @class{gdk-pixbuf:pixbuf} object, and the callback function will be called.

  The @arg{pixbuf} parameter to the callback function will contain the resulting
  @class{gdk-pixbuf:pixbuf} object if the request succeeded, or @code{nil} if it
  failed. This could happen for various reasons, in particular if the clipboard
  was empty or if the contents of the clipboard could not be converted into an
  image.
  @see-class{gtk:clipboard}
  @see-symbol{gtk:clipboard-image-received-func}"
  (%clipboard-request-image clipboard
                            (cffi:callback clipboard-image-received-func)
                            (glib:allocate-stable-pointer func)))

(export 'clipboard-request-image)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardTargetsReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-targets-received-func :void
    ((clipboard (g:object clipboard))
     (atoms :pointer)
     (n-atoms :int)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard atoms n-atoms)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-targets-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-targets-received-func)
 "@version{#2025-07-03}
  @syntax{lambda (clipboard atoms n-atoms)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[atoms]{a pointer to a foreign C array of @type{gdk:atom-as-string}
    atoms}
  @argument[n-atoms]{an integer for the length of the atoms array}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-targets} function are received, or when the
    request fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-type{gdk:atom-as-string}
  @see-function{gtk:clipboard-request-targets}")

(export 'clipboard-targets-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_targets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_targets" %clipboard-request-targets) :void
  (clipboard (g:object clipboard))
  (func :pointer)
  (data :pointer))

(defun clipboard-request-targets (clipboard func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[func]{a @sym{gtk:clipboard-targets-received-func} callback function
    to call when the targets are received, or the retrieval fails}
  @begin{short}
    Requests the contents of the clipboard as a list of supported targets.
  @end{short}
  When the list is later received, the callback function will be called.

  The @arg{atoms} parameter of the callback function will contain the
  resulting targets if the request succeeded, or @code{nil} if it failed.
  @see-class{gtk:clipboard}
  @see-symbol{gtk:clipboard-targets-received-func}"
  (%clipboard-request-targets clipboard
                              (cffi:callback clipboard-targets-received-func)
                              (glib:allocate-stable-pointer func)))

(export 'clipboard-request-targets)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardRichTextReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-rich-text-received-func :void
    ((clipboard (g:object clipboard))
     (format gdk:atom-as-string)
     (text :uint8)
     (length :size)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard format text length)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-rich-text-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-rich-text-received-func)
 "@version{#2025-06-19}
  @syntax{lambda (clipboard format text length)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[format]{a string for the format of the rich text}
  @argument[text]{a string for the rich text received, as a UTF-8 encoded
    string, or @code{nil} if retrieving the data failed}
  @argument[length]{an integer for the length of the text}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-rich-text} function are received, or when the
    request fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-request-rich-text}")

(export 'clipboard-rich-text-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_rich_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_rich_text" %clipboard-request-rich-text)
    :void
  (clipboard (g:object clipboard))
  (buffer (g:object buffer))
  (func :pointer)
  (data :pointer))

(defun clipboard-request-rich-text (clipboard buffer func)
 #+liber-documentation
 "@version{#2027-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[func]{a @sym{gtk:clipboard-rich-text-received-func} callback
    function to call when the text is received, or the retrieval fails, it
    will always be called one way or the other}
  @begin{short}
    Requests the contents of the clipboard as rich text.
  @end{short}
  When the rich text is later received, the callback function will be called.

  The @arg{text} parameter to the callback function will contain the resulting
  rich text if the request succeeded, or @code{nil} if it failed. The
  @arg{length} parameter will contain the length of the text. This function can
  fail for various reasons, in particular if the clipboard was empty or if the
  contents of the clipboard could not be converted into rich text form.
  @see-class{gtk:clipboard}
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:clipboard-rich-text-received-func}"
  (%clipboard-request-rich-text
          clipboard
          buffer
          (cffi:callback clipboard-rich-text-received-func)
          (glib:allocate-stable-pointer func)))

(export 'clipboard-request-rich-text)

;;; ----------------------------------------------------------------------------
;;; GtkClipboardURIReceivedFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback clipboard-uri-received-func :void
    ((clipboard (g:object clipboard))
     (uris :pointer)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func clipboard uris)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'clipboard-uri-received-func)
      "Callback"
      (liber:symbol-documentation 'clipboard-uri-received-func)
 "@version{#2024-03-23}
  @syntax{lambda (clipboard uris)}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[uris]{a pointer to the foreign zero-terminated C array of received
      URIs}
  @begin{short}
    A callback function to be called when the results of the
    @fun{gtk:clipboard-request-uris} function are received, or when the request
    fails.
  @end{short}
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-request-uris}")

(export 'clipboard-uri-received-func)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_request_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_request_uris" %clipboard-request-uris) :void
  (clipboard (g:object clipboard))
  (func :pointer)
  (data :pointer))

(defun clipboard-request-uris (clipboard func)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[func]{a @sym{gtk:clipboard-uri-received-func} callback function
    to call when the URIs are received, or the retrieval fails, it will always
    be called one way or the other}
  @begin{short}
    Requests the contents of the clipboard as URIs.
  @end{short}
  When the URIs are later received the callback function will be called.

  The @arg{uris} parameter to the callback function will contain the resulting
  foreign C array of URIs if the request succeeded, or @code{cffi:null-pointer}
  if it failed. This could happen for various reasons, in particular if the
  clipboard was empty or if the contents of the clipboard could not be converted
  into URI form.
  @see-class{gtk:clipboard}
  @see-symbol{gtk:clipboard-uri-received-func}"
  (%clipboard-request-uris clipboard
                           (cffi:callback clipboard-uri-received-func)
                           (glib:allocate-stable-pointer func)))

(export 'clipboard-request-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_for_contents" clipboard-wait-for-contents)
    (g:boxed selection-data :return)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[target]{a string representing the form into which the clipboard
    owner should convert the selection}
  @begin{return}
    The newly allocated @class{gtk:selection-data} instance or @code{nil} if
    retrieving the given target failed.
  @end{return}
  @begin{short}
    Requests the contents of the clipboard using the given target.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}
  @see-class{gtk:selection-data}"
  (clipboard (g:object clipboard))
  (target gdk:atom-as-string))

(export 'clipboard-wait-for-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_for_text" clipboard-wait-for-text) :string
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{return}
    The newly allocated UTF-8 string, or @code{nil} if retrieving the selection
    data failed.
  @end{return}
  @begin{short}
    Requests the contents of the clipboard as text and converts the result to
    UTF-8 if necessary.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-for-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_for_image" clipboard-wait-for-image)
    (g:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{return}
    The newly allocated @class{gdk-pixbuf:pixbuf} object, or @code{nil} if
    retrieving the selection data failed.
  @end{return}
  @begin{short}
    Requests the contents of the clipboard as image and converts the result to
    a @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}
  @see-class{gdk-pixbuf:pixbuf}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-for-image)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_rich_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_for_rich_text" %clipboard-wait-for-rich-text)
    :uint8
  (clipboard (g:object clipboard))
  (buffer (g:object text-buffer))
  (format :pointer)
  (length :pointer))

(defun clipboard-wait-for-rich-text (clipboard buffer)
 #+liber-documentation
 "@version{#2025-07-03}
  @syntax{(gtk:clipboard-wait-for-rich-text clipboard buffer) => data, format,
    length}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[data]{a newly allocated binary block of data which must be freed
    with the @fun{g:free} function, or @code{nil} if retrieving the selection
    data failed}
  @argument[format]{a string for the format of the returned data}
  @argument[length]{an integer for the length of the returned data}
  @begin{short}
    Requests the contents of the clipboard as rich text.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}
  @see-class{gtk:text-buffer}"
  (cffi:with-foreign-objects ((format :pointer) ; GdkAtom
                              (length :int))
    (let ((data (%clipboard-wait-for-rich-text clipboard
                                               buffer
                                               format
                                               length)))
      (when data
        (values data
                (cffi:mem-ref format :pointer) ; gdk:atom
                (cffi:mem-ref length :int))))))

(export 'clipboard-wait-for-rich-text)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_for_uris" clipboard-wait-for-uris) g:strv-t
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{return}
    The list of strings, or @code{nil} if retrieving the selection data failed.
  @end{return}
  @begin{short}
    Requests the contents of the clipboard as URIs.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-for-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_text_available
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_is_text_available"
               clipboard-wait-is-text-available) :boolean
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @return{@em{True} if there is text available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is text available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported text targets. This function waits for the data to be
  received using the main loop, so events, timeouts, and so on, may be
  dispatched during the wait.

  This function is a little faster than calling the
  @fun{gtk:clipboard-wait-for-text} function since it does not need to retrieve
  the actual text.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-wait-for-text}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-is-text-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_image_available
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_is_image_available"
               clipboard-wait-is-image-available) :boolean
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @return{@em{True} if there is an image available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is an image available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported image targets. This function waits for the data to be
  received using the main loop, so events, timeouts, and so on, may be
  dispatched during the wait.

  This function is a little faster than calling the
  @fun{gtk:clipboard-wait-for-image} function since it does not need to retrieve
  the actual image data.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-wait-for-image}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-is-image-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_rich_text_available
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_is_rich_text_available"
               clipboard-wait-is-rich-text-available) :boolean
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{@em{True} if there is rich text available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is rich text available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  any of the supported rich text targets. This function waits for the data to
  be received using the main loop, so events, timeouts, and so on, may be
  dispatched during the wait.

  This function is a little faster than calling the
  @fun{gtk:clipboard-wait-for-rich-text} function since it does not need to
  retrieve the actual text.
  @see-class{gtk:clipboard}
  @see-class{gtk:text-buffer}
  @see-function{gtk:clipboard-wait-for-rich-text}"
  (clipboard (g:object clipboard))
  (buffer (g:object text-buffer)))

(export 'clipboard-wait-is-rich-text-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_uris_available
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_is_uris_available"
               clipboard-wait-is-uris-available) :boolean
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @return{@em{True} if there is an URI list available, @em{false} otherwise.}
  @begin{short}
    Test to see if there is a list of URIs available to be pasted.
  @end{short}
  This is done by requesting the \"TARGETS\" atom and checking if it contains
  the URI targets. This function waits for the data to be received using the
  main loop, so events, timeouts, and so on, may be dispatched during the wait.

  This function is a little faster than calling the
  @fun{gtk:clipboard-wait-for-uris} function since it does not need to retrieve
  the actual URI data.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-wait-for-uris}"
  (clipboard (g:object clipboard)))

(export 'clipboard-wait-is-uris-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_for_targets
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is not complete. Return a list of atoms as strings.

(cffi:defcfun ("gtk_clipboard_wait_for_targets" clipboard-wait-for-targets)
    :boolean
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[targets]{location to store an array of targets. The result stored
    here must be freed with the @fun{g:free} function}
  @argument[n-targets]{location to store the number of items in targets}
  @begin{return}
    @em{True} if any targets are present on the clipboard, otherwise @em{false}.
  @end{return}
  @begin{short}
    Returns a list of targets that are present on the clipboard, or @code{nil}
    if there are not any targets available.
  @end{short}
  This function waits for the data to be received using the main loop, so
  events, timeouts, and so on, may be dispatched during the wait.
  @see-class{gtk:clipboard}
  @see-function{g:free}"
  (clipboard (g:object clipboard))
  (targets :pointer)
  (n-targets :int))

(export 'clipboard-wait-for-targets)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_wait_is_target_available
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_wait_is_target_available"
               clipboard-wait-is-target-available) :boolean
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[target]{a string indicating which target to look for}
  @return{@em{True} if the target is available, @em{false} otherwise.}
  @begin{short}
    Checks if a clipboard supports pasting data of a given type.
  @end{short}
  This function can be used to determine if a \"Paste\" menu item should be
  insensitive or not.

  If you want to see if there is text available on the clipboard, use the
  @fun{gtk:clipboard-wait-is-text-available} function instead.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-wait-is-text-available}"
  (clipboard (g:object clipboard))
  (target gdk:atom-as-string))

(export 'clipboard-wait-is-target-available)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_set_can_store
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is not complete. Return a list of atoms as strings.

(cffi:defcfun ("gtk_clipboard_set_can_store" clipboard-set-can-store) :void
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @argument[targets]{array containing information about which forms should be
    stored or NULL to indicate that all forms should be stored}
  @argument[n-targets]{number of elements in targets}
  @begin{short}
    Hints that the clipboard data should be stored somewhere when the
    application exits or when the @fun{gtk:clipboard-store} function is called.
  @end{short}
  This value is reset when the clipboard owner changes. Where the clipboard
  data is stored is platform dependent, see the
  @fun{gdk:display-store-clipboard} function for more information.
  @see-class{gtk:clipboard}
  @see-function{gtk:clipboard-store}
  @see-function{gdk:display-store-clipboard}"
  (clipboard (g:object clipboard))
  (targets :pointer)
  (n-targets :int))

(export 'clipboard-set-can-store)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_store
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_store" clipboard-store) :void
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{short}
    Stores the current clipboard data somewhere so that it will stay around
    after the application has quit.
  @end{short}
  @see-class{gtk:clipboard}"
  (clipboard (g:object clipboard)))

(export 'clipboard-store)

;;; ----------------------------------------------------------------------------
;;; gtk_clipboard_get_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_clipboard_get_selection" clipboard-selection)
    gdk:atom-as-string
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @return{The string representing the selection.}
  @begin{short}
    Gets the selection that this clipboard is for.
  @end{short}
  @see-class{gtk:clipboard}"
  (clipboard (g:object clipboard)))

(export 'clipboard-selection)

;;; --- End of file gtk3.clipboard.lisp ----------------------------------------
