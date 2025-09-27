;;; ----------------------------------------------------------------------------
;;; gtk3.editable.lisp
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
;;; GtkEditable
;;;
;;;     Interface for text-editing widgets
;;;
;;; Types and Values
;;;
;;;     GtkEditable
;;;
;;; Functions
;;;
;;;     gtk_editable_select_region
;;;     gtk_editable_get_selection_bounds
;;;     gtk_editable_insert_text
;;;     gtk_editable_delete_text
;;;     gtk_editable_get_chars
;;;     gtk_editable_cut_clipboard
;;;     gtk_editable_copy_clipboard
;;;     gtk_editable_paste_clipboard
;;;     gtk_editable_delete_selection
;;;     gtk_editable_set_position
;;;     gtk_editable_get_position
;;;     gtk_editable_set_editable
;;;     gtk_editable_get_editable
;;;
;;; Signals
;;;
;;;     changed
;;;     delete-text
;;;     insert-text
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkEditable
;;;
;;; Known Implementations
;;;
;;;     GtkEditable is implemented by GtkEntry, GtkSearchEntry and GtkSpinButton
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEditable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkEditable" editable
  (:export t
   :type-initializer "gtk_editable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'editable)
      "Interface"
      (documentation 'editable 'type)
 "@version{2025-09-26}
  @begin{short}
    The @class{gtk:editable} interface is an interface which should be
    implemented by text editing widgets, such as the @class{gtk:entry} widget
    and the @class{gtk:spin-button} widget.
  @end{short}
  It contains functions for generically manipulating an editable widget, a large
  number of action signals used for key bindings, and several signals that an
  application can connect to to modify the behavior of a widget.
  @begin[Examples]{dictionary}
    As an example of the latter usage, by connecting the following handler to
    the @sig[gtk:editable]{insert-text} signal, an application can convert all
    entry into a widget into uppercase.
    @begin{pre}
;; Handler for the \"insert-text\" signal
(setf handlerid
      (g:signal-connect entry \"insert-text\"
          (lambda (editable text length position)
            (g:signal-handler-block editable handlerid)
            (gtk:editable-insert-text editable
                                      (string-upcase text)
                                      (cffi:mem-ref position :intptr))
            (g:signal-stop-emission editable \"insert-text\")
            (g:signal-handler-unblock editable handlerid))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[editable::changed]{signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted at the end of a single user visible operation on
      the contents of the @class{gtk:editable} widget. For example, a paste
      operation that replaces the contents of the selection will cause only one
      signal emission, even though it is implemented by first deleting the
      selection, then inserting the new content, and may cause multiple
      @sig[g:object]{notify::text} signals to be emitted.
    @end{signal}
    @begin[editable::delete-text]{signal}
      @begin{pre}
lambda (editable start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
        @entry[start]{The integer for the start position.}
        @entry[end]{The integer for the end position.}
      @end{simple-table}
      The signal is emitted when text is deleted from the widget by the user.
      The default handler for this signal will normally be responsible for
      deleting the text, so by connecting to this signal and then stopping the
      signal with the @fun{g:signal-stop-emission} function, it is possible to
      modify the range of deleted text, or prevent it from being deleted
      entirely. The @arg{start} and @arg{end} parameters are interpreted as for
      the @fun{gtk:editable-delete-text} function.
    @end{signal}
    @begin[editable::insert-text]{signal}
      @begin{pre}
lambda (editable text length pos)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
        @entry[text]{The string for the new text to insert.}
        @entry[length]{The integer for the length of the new text, in bytes,
           or -1 if @arg{text} is nul-terminated.}
        @entry[pos]{The pointer to an integer for the position, in characters,
          at which to insert the new text. This is an in-out parameter. After
          the signal emission is finished, it should point after the newly
          inserted text. The Lisp value of @arg{pos} is returned by the
          @code{(cffi:mem-ref pos :intptr)} call.}
      @end{simple-table}
      The signal is emitted when text is inserted into the widget by the user.
      The default handler for this signal will normally be responsible for
      inserting the text, so by connecting to this signal and then stopping the
      signal with the @fun{g:signal-stop-emission} function, it is possible to
      modify the inserted text, or prevent it from being inserted entirely.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:spin-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_editable_select_region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_select_region" %editable-select-region) :void
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-select-region (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start of the region, the default value
    is 0}
  @argument[end]{an integer for the end of the region, the default value is -1}
  @begin{short}
    Selects a region of text.
  @end{short}
  The characters that are selected are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative, then
  the the characters selected are those characters from @arg{start} to the end
  of the text. Note that positions are specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-select-region editable start end))

(export 'editable-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_get_selection_bounds" %editable-selection-bounds)
    :boolean
  (editable (g:object editable))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun editable-selection-bounds (editable)
 "@version{2025-07-07}
  @syntax{(gtk:editable-selection-bounds editable) => start, end}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start position}
  @argument[end]{an integer for the end position}
  @begin{short}
    Retrieves the selection bound of the editable.
  @end{short}
  If no text was selected @code{nil} will be returned. Note that positions are
  specified in characters, not bytes.
  @see-class{gtk:editable}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (when (%editable-selection-bounds editable start end)
      (values (cffi:mem-ref start :int)
              (cffi:mem-ref end :int)))))

(export 'editable-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_insert_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_insert_text" %editable-insert-text) :void
  (editable (g:object editable))
  (text :string)
  (len :int)
  (pos (:pointer :int)))

(defun editable-insert-text (editable text pos)
 #+liber-documentation
 "@version{2025-07-08}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[text]{a string for the text to append}
  @argument[pos]{an integer for the position the text will be inserted at}
  @return{The integer for the position after the newly inserted text.}
  @begin{short}
    Inserts @arg{text} into the contents of the widget, at position
    @arg{pos}.
  @end{short}
  Note that @arg{pos} is in characters, not in bytes. The function
  returns the position to point after the newly inserted text.
  @see-class{gtk:editable}"
  (cffi:with-foreign-object (pos1 :int)
    (setf (cffi:mem-ref pos1 :int) pos)
    (%editable-insert-text editable text -1 pos1)
    (cffi:mem-ref pos1 :int)))

(export 'editable-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_delete_text" %editable-delete-text) :void
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-delete-text (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start position, the default value is 0}
  @argument[end]{an integer for the end position, the default value is -1}
  @begin{short}
    Deletes a sequence of characters.
  @end{short}
  The characters that are deleted are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative, then
  the characters deleted are those from @arg{start} to the end of the text. Note
  that positions are specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-delete-text editable start end))

(export 'editable-delete-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_chars
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_get_chars" %editable-get-chars) :string
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-chars (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-07-08}
  @argument[editable]{a @class{gtk:editable} object}
  @argument[start]{an integer for the start position, the default value is 0}
  @argument[end]{an integer for the end position, the default value is -1}
  @return{The string for the contents of the editable widget.}
  @begin{short}
    Retrieves a sequence of characters.
  @end{short}
  The characters that are retrieved are those characters at positions from
  @arg{start} up to, but not including @arg{end}. If @arg{end} is negative,
  then the characters retrieved are those characters from @arg{start} to the
  end of the text. Note that positions are specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-get-chars editable start end))

(export 'editable-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_cut_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_cut_clipboard" editable-cut-clipboard) :void
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @begin{short}
    Removes the contents of the currently selected content in the editable and
    puts it on the clipboard.
  @end{short}
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_copy_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_copy_clipboard" editable-copy-clipboard) :void
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @begin{short}
    Copies the contents of the currently selected content in the editable and
    puts it on the clipboard.
  @end{short}
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-copy-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_paste_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_paste_clipboard" editable-paste-clipboard) :void
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @begin{short}
    Pastes the content of the clipboard to the current position of the cursor
    in the editable.
  @end{short}
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_delete_selection" editable-delete-selection) :void
 #+liber-documentation
 "@version{2025-07-07}
  @argument[editable]{a @class{gtk:editable} widget}
  @begin{short}
    Deletes the currently selected text of the editable.
  @end{short}
  This call does not do anything if there is no selected text.
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_set_position
;;; gtk_editable_get_position
;;; ----------------------------------------------------------------------------

(defun (setf editable-position) (pos editable)
  (cffi:foreign-funcall "gtk_editable_set_position"
                        (g:object editable) editable
                        :int pos
                        :void)
  pos)

(cffi:defcfun ("gtk_editable_get_position" editable-position) :int
 #+liber-documentation
 "@version{2025-07-07}
  @syntax{(gtk:editable-position editable) => pos}
  @syntax{(setf (gtk:editable-position editable) pos)}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[pos]{an integer for the position of the cursor}
  @begin{short}
    The @fun{gtk:editable-position} function retrieves the current position of
    the cursor relative to the start of the content of the editable.
  @end{short}
  The @setf{gtk:editable-position} function sets the cursor position in the
  editable to the given value.

  The cursor is displayed before the character with the given (base 0) index
  in the contents of the editable. The value must be less than or equal to the
  number of characters in the editable. A value of -1 indicates that the
  position should be set after the last character of the editable. Note that
  the position is in characters, not in bytes.
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-position)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_set_editable
;;; gtk_editable_get_editable
;;; ----------------------------------------------------------------------------

(defun (setf editable-editable) (setting editable)
  (cffi:foreign-funcall "gtk_editable_set_editable"
                        (g:object editable) editable
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gtk_editable_get_editable" editable-editable) :boolean
 #+liber-documentation
 "@version{2025-07-07}
  @syntax{(gtk:editable-editable editable) => setting}
  @syntax{(setf (gtk:editable-editable editable) setting)}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[setting]{@em{true} if the user is allowed to edit the text in the
    widget}
  @begin{short}
    The @fun{gtk:editable-editable} function retrieves whether the editable
    widget is editable.
  @end{short}
  The @setf{gtk:editable-editable} function determines if the user can edit the
  text in the editable widget or not.
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-editable)

;;; --- End of file gtk3.editable.lisp -----------------------------------------
