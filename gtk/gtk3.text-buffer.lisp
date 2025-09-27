;;; ----------------------------------------------------------------------------
;;; gtk3.text-buffer.lisp
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
;;; GtkTextBuffer
;;;
;;;     Stores attributed text for display in a GtkTextView
;;;
;;; Types and Values
;;;
;;;     GtkTextBuffer
;;;     GtkTextBufferTargetInfo
;;;
;;; Functions
;;;
;;;     gtk_text_buffer_new
;;;     gtk_text_buffer_get_line_count
;;;     gtk_text_buffer_get_char_count
;;;     gtk_text_buffer_get_tag_table                      Accessor
;;;     gtk_text_buffer_insert
;;;     gtk_text_buffer_insert_at_cursor
;;;     gtk_text_buffer_insert_interactive
;;;     gtk_text_buffer_insert_interactive_at_cursor
;;;     gtk_text_buffer_insert_range
;;;     gtk_text_buffer_insert_range_interactive
;;;     gtk_text_buffer_insert_with_tags
;;;     gtk_text_buffer_insert_with_tags_by_name
;;;     gtk_text_buffer_insert_markup
;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive
;;;     gtk_text_buffer_backspace
;;;     gtk_text_buffer_set_text                           Accessor
;;;     gtk_text_buffer_get_text                           Accessor
;;;     gtk_text_buffer_get_slice
;;;     gtk_text_buffer_insert_pixbuf
;;;     gtk_text_buffer_insert_child_anchor
;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name
;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark
;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_get_has_selection                  Accessor
;;;     gtk_text_buffer_place_cursor
;;;     gtk_text_buffer_select_range
;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag_by_name
;;;     gtk_text_buffer_remove_all_tags
;;;     gtk_text_buffer_create_tag
;;;     gtk_text_buffer_get_iter_at_line_offset
;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line
;;;     gtk_text_buffer_get_iter_at_line_index
;;;     gtk_text_buffer_get_iter_at_mark
;;;     gtk_text_buffer_get_iter_at_child_anchor
;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter
;;;     gtk_text_buffer_get_bounds
;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified
;;;     gtk_text_buffer_delete_selection
;;;     gtk_text_buffer_paste_clipboard
;;;     gtk_text_buffer_copy_clipboard
;;;     gtk_text_buffer_cut_clipboard
;;;     gtk_text_buffer_get_selection_bounds
;;;     gtk_text_buffer_begin_user_action
;;;     gtk_text_buffer_end_user_action
;;;     gtk_text_buffer_add_selection_clipboard
;;;     gtk_text_buffer_remove_selection_clipboard
;;;
;;;     gtk_text_buffer_deserialize
;;;     gtk_text_buffer_deserialize_get_can_create_tags
;;;     gtk_text_buffer_deserialize_set_can_create_tags
;;;     gtk_text_buffer_get_copy_target_list               Accessor
;;;     gtk_text_buffer_get_deserialize_formats
;;;     gtk_text_buffer_get_paste_target_list              Accessor
;;;     gtk_text_buffer_get_serialize_formats
;;;     gtk_text_buffer_register_deserialize_format
;;;     gtk_text_buffer_register_deserialize_tagset
;;;     gtk_text_buffer_register_serialize_format
;;;     gtk_text_buffer_register_serialize_tagset
;;;     gtk_text_buffer_serialize
;;;     gtk_text_buffer_unregister_deserialize_format
;;;     gtk_text_buffer_unregister_serialize_format
;;;
;;; Properties
;;;
;;;     copy-target-list
;;;     cursor-position
;;;     has-selection
;;;     paste-target-list
;;;     tag-table
;;;     text
;;;
;;; Signals
;;;
;;;     apply-tag
;;;     begin-user-action
;;;     changed
;;;     delete-range
;;;     end-user-action
;;;     insert-child-anchor
;;;     insert-pixbuf
;;;     insert-text
;;;     mark-deleted
;;;     mark-set
;;;     modified-changed
;;;     paste-done
;;;     remove-tag
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextBuffer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferTargetInfo
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTextBufferTargetInfo" text-buffer-target-info
  (:export t
   :type-initializer "gtk_text_buffer_target_info_get_type")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-buffer-target-info)
      "GEnum"
      (liber:symbol-documentation 'text-buffer-target-info)
 "@version{#2025-06-27}
  @begin{declaration}
(gobject:define-genum \"GtkTextBufferTargetInfo\" gtk:text-buffer-target-info
  (:export t
   :type-initializer \"gtk_text_buffer_target_info_get_type\")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:buffer-contents]{Buffer contents.}
      @entry[:rich-text]{Rich text.}
      @entry[:text]{Text.}
    @end{simple-table}
  @end{values}
  @begin{short}
    These values are used as \"info\" for the targets contained in the lists
    returned by the @fun{gtk:text-buffer-copy-target-list} and
    @fun{gtk:text-buffer-paste-target-list} functions.
  @end{short}

  The values counts down from -1 to avoid clashes with application added drag
  destinations which usually start at 0.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-copy-target-list}
  @see-function{gtk:text-buffer-paste-target-list}")

;;; ----------------------------------------------------------------------------
;;; GtkTextBuffer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTextBuffer" text-buffer
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_text_buffer_get_type")
  ((copy-target-list
    text-buffer-copy-target-list
    "copy-target-list" "GtkTargetList" t nil)
   (cursor-position
    text-buffer-cursor-position
    "cursor-position" "gint" t nil)
   (has-selection
    text-buffer-has-selection
    "has-selection" "gboolean" t nil)
   (paste-target-list
    text-buffer-paste-target-list
    "paste-target-list" "GtkTargetList" t nil)
   (tag-table
    text-buffer-tag-table
    "tag-table" "GtkTextTagTable" t nil)
   (text
    text-buffer-text
    "text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'text-buffer 'type)
 "@version{2025-09-26}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}
  @begin[Signal Details]{dictionary}
    @begin[text-buffer::apply-tag]{signal}
      @begin{pre}
lambda (buffer tag start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[tag]{The @class{gtk:text-tag} applied tag.}
        @entry[start]{The @class{gtk:text-iter} start iterator of the range
          the tag is applied to.}
        @entry[end]{The @class{gtk:text-iter} end iterator of the range
          the tag is applied to.}
      @end{simple-table}
      The signal is emitted to apply a tag to a range of text in a text buffer.
      Applying actually occurs in the default handler. Note that if your handler
      runs before the default handler it must not invalidate the @arg{start} and
      @arg{end} iterators, or has to revalidate them.
    @end{signal}
    @begin[text-buffer::begin-user-action]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
      @end{simple-table}
      The signal is emitted at the beginning of a single user visible operation
      on a text buffer.
    @end{signal}
    @begin[text-buffer::changed]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
      @end{simple-table}
      The signal is emitted when the content of a text buffer has changed.
    @end{signal}
    @begin[text-buffer::delete-range]{signal}
      @begin{pre}
lambda (buffer start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[start]{The @class{gtk:text-iter} start iterator of the range
          to be deleted.}
        @entry[end]{The @class{gtk:text-iter} end iterator of the range
          to be deleted.}
      @end{simple-table}
      The signal is emitted to delete a range from a text buffer. Note that if
      your handler runs before the default handler it must not invalidate the
      @arg{start} and @arg{end} iterators, or has to revalidate them. The
      default signal handler revalidates the @arg{start} and @arg{end} iterators
      to both point to the location where text was deleted. Handlers which run
      after the default handler do not have access to the deleted text.
    @end{signal}
    @begin[text-buffer::end-user-action]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
      @end{simple-table}
      The signal is emitted at the end of a single user visible operation on
      the text buffer.
    @end{signal}
    @begin[text-buffer::insert-child-anchor]{signal}
      @begin{pre}
lambda (buffer location anchor)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert
          @arg{anchor} in @arg{buffer}.}
        @entry[anchor]{The @class{gtk:text-child-anchor} object to be inserted.}
      @end{simple-table}
      The signal is emitted to insert a @class{gtk:text-child-anchor} object
      in a text buffer. Insertion actually occurs in the default handler. Note
      that if your handler runs before the default handler it must not
      invalidate the @arg{location} iterator, or has to revalidate it. The
      default signal handler revalidates it to be placed after the inserted
      @arg{anchor}.
    @end{signal}
    @begin[text-buffer::insert-pixbuf]{signal}
      @begin{pre}
lambda (buffer location pixbuf)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert
          @arg{pixbuf} in @arg{buffer}.}
        @entry[pixbuf]{The @class{gdk-pixbuf:pixbuf} object to be inserted.}
      @end{simple-table}
      The signal is emitted to insert a @class{gdk-pixbuf:pixbuf} object in a
      text buffer. Insertion actually occurs in the default handler. Note that
      if your handler runs before the default handler it must not invalidate
      the @arg{location} iterator, or has to revalidate it. The default signal
      handler revalidates it to be placed after the inserted @arg{pixbuf}.
    @end{signal}
    @begin[text-buffer::insert-text]{signal}
      @begin{pre}
lambda (buffer location text len)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert @arg{text}
          in @arg{buffer}.}
        @entry[text]{The string for the UTF-8 text to be inserted.}
        @entry[len]{The integer for the length of the inserted text in bytes.}
      @end{simple-table}
      The signal is emitted to insert text in a text buffer. Insertion actually
      occurs in the default handler. Note that if your handler runs before the
      default handler it must not invalidate the @arg{location} iterator, or has
      to revalidate it. The default signal handler revalidates it to point to
      the end of the inserted text.
    @end{signal}
    @begin[text-buffer::mark-deleted]{signal}
      @begin{pre}
lambda (buffer mark)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[mark]{The @class{gtk:text-mark} object that was deleted.}
      @end{simple-table}
      The signal is emitted as notification after a @class{gtk:text-mark}
      object is deleted.
    @end{signal}
    @begin[text-buffer::mark-set]{signal}
      @begin{pre}
lambda (buffer location mark)    :run-last
      @end{pre}
      object is set.
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} location of @arg{mark} in
          @arg{buffer}.}
        @entry[mark]{The @class{gtk:text-mark} object that is set.}
      @end{simple-table}
      The signal is emitted as notification after a @class{gtk:text-mark}
    @end{signal}
    @begin[text-buffer::modifed-changed]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
      @end{simple-table}
      The signal is emitted when the modified bit of a text buffer flips.
    @end{signal}
    @begin[text-buffer::paste-done]{signal}
      @begin{pre}
lambda (buffer clipboard)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[clipboard]{The @class{gtk:clipboard} object.}
      @end{simple-table}
      The signal is emitted after paste operation has been completed. This is
      useful to properly scroll the view to the end of the pasted text.
    @end{signal}
    @begin[text-buffer::remove-tag]{signal}
      @begin{pre}
lambda (buffer tag start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object which received the
          signal.}
        @entry[tag]{The @class{gtk:text-tag} object to be removed.}
        @entry[start]{The @class{gtk:text-iter} start iterator of the range
          the tag is removed from.}
        @entry[end]{The @class{gtk:text-iter} end iterator of the range
          the tag is removed from.}
      @end{simple-table}
      The signal is emitted to remove all occurrences of @arg{tag} from a range
      of text in a text buffer. Removal actually occurs in the default handler.
      Note that if your handler runs before the default handler it must not
      invalidate the @arg{start} and @arg{end} iterators, or has to revalidate
      them.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:text-buffer-new}
  @see-slot{gtk:text-buffer-copy-target-list}
  @see-slot{gtk:text-buffer-cursor-position}
  @see-slot{gtk:text-buffer-has-selection}
  @see-slot{gtk:text-buffer-paste-target-list}
  @see-slot{gtk:text-buffer-tag-table}
  @see-slot{gtk:text-buffer-text}
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-child-anchor}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:text-buffer-copy-target-list ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "copy-target-list"
                                               'text-buffer) t)
 "The @code{copy-target-list} property of type @class{gtk:target-list} (Read)
  @br{}
  The list of targets the text buffer supports for clipboard copying and as
  drag and drop source.")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-copy-target-list)
      "Accessor"
      (documentation 'text-buffer-copy-target-list 'function)
 "@version{#2025-07-01}
  @syntax{(gtk:text-buffer-copy-target-list object) => tlist}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[tlist]{a @class{gtk:target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{copy-target-list} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  This function returns the list of targets this text buffer can provide
  for copying and as drag and drag source. The targets in the list are added
  with info values from the @sym{gtk:text-buffer-target-info} enumeration using
  the @fun{gtk:target-list-add-rich-text-targets} and
  @fun{gtk:target-list-add-text-targets} functions.
  @see-class{gtk:text-buffer}
  @see-class{gtk:target-list}
  @see-symbol{gtk:text-buffer-target-info}
  @see-function{gtk:target-list-add-text-targets}
  @see-function{gtk:target-list-add-rich-text-targets}")

;;; --- gtk:text-buffer-cursor-position ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-position"
                                               'text-buffer) t)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The position of the insert mark, as offset from the beginning of the text
  buffer. It is useful for getting notified when the cursor moves. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-cursor-position)
      "Accessor"
      (documentation 'text-buffer-cursor-position 'function)
 "@version{#2025-06-30}
  @syntax{(gtk:text-buffer-cursor-position object) => position}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[position]{an integer for the position of the insert mark}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{cursor-position} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  The position of the insert mark, as offset from the beginning of the text
  buffer. It is useful for getting notified when the cursor moves.
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-has-selection ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-selection"
                                               'text-buffer) t)
 "The @code{has-selection} property of type @code{:boolean} (Read) @br{}
  Whether the text buffer has some text currently selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-has-selection)
      "Accessor"
      (documentation 'text-buffer-has-selection 'function)
 "@version{#2023-03-07}
  @syntax{(gtk:text-buffer-has-selection object) => setting}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[setting]{@em{true} if there is text selected}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{has-selection} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  Indicates whether the text buffer has some text currently selected.
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-paste-target-list --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paste-target-list"
                                               'text-buffer) t)
 "The @code{paste-target-list} property of type @class{gtk:target-list} (Read)
  @br{}
  The list of targets the text buffer supports for clipboard pasting and as
  drag and drop destination.")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-paste-target-list)
      "Accessor"
      (documentation 'text-buffer-paste-target-list 'function)
 "@version{#2025-07-01}
  @syntax{(gtk:text-buffer-paste-target-list object) => tlist}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[tlist]{a @class{gtk:target-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{paste-target-list} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  This function returns the list of targets the text buffer supports for
  pasting and as drag and drop destination. The targets in the list are added
  with info values from the @sym{gtk:text-buffer-target-info} enumeration using
  the @fun{gtk:target-list-add-rich-text-targets} and
  @fun{gtk:target-list-add-text-targets} functions.
  @see-class{gtk:text-buffer}
  @see-class{gtk:target-list}
  @see-symbol{gtk:text-buffer-target-info}
  @see-function{gtk:target-list-add-targets}
  @see-function{gtk:target-list-add-rich-text-targets}")

;;; --- gtk:text-buffer-tag-table ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tag-table" 'text-buffer) t)
 "The @code{tag-table} property of type @class{gtk:text-tag-table}
  (Read / Write / Construct) @br{}
  The tag table associated with the text buffer.")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-tag-table)
      "Accessor"
      (documentation 'text-buffer-tag-table 'function)
 "@version{2024-01-02}
  @syntax{(gtk:text-buffer-tag-table object) => table}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{tag-table} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  Gets the tag table associated with the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag-table}")

;;; --- gtk:text-buffer-text ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'text-buffer) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The text content of the text buffer, without child widgets and images. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-text)
      "Accessor"
      (documentation 'text-buffer-text 'function)
 "@version{2025-06-30}
  @syntax{(gtk:text-buffer-text object) => text}
  @syntax{(setf (gtk:text-buffer-text object) text)}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[text]{a string for the UTF-8 text}
  @begin{short}
    Accessor of the @slot[gtk:text-buffer]{text} slot of the
    @class{gtk:text-buffer} class.
  @end{short}
  The @fun{gtk:text-buffer} function retrieves the text of the text buffer,
  without child widgets and images. The @setf{gtk:text-buffer-text} function
  deletes current contents of the text buffer, and inserts @arg{text} instead.
  The text must be valid UTF-8.
  @begin[Notes]{dictionary}
    Use the @fun{gtk:text-buffer-get-text} function to retrieve a range of text
    from the text buffer and the @fun{gtk:text-buffer-get-slice} function to
    include widgets and images.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-get-text}
  @see-function{gtk:text-buffer-get-slice}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_new
;;; ----------------------------------------------------------------------------

(declaim (inline text-buffer-new))

(defun text-buffer-new (&optional table)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[table]{an optional @class{gtk:text-tag-table} object, or no
    argument to create a new one}
  @return{The new @class{gtk:text-buffer} object.}
  @begin{short}
    Creates a new text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag-table}"
  (make-instance 'text-buffer
                 :tag-table table))

(export 'text-buffer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_line_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_line_count" text-buffer-line-count) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The integer for the number of lines in the text buffer.}
  @begin{short}
    Obtains the number of lines in the text buffer.
  @end{short}
  This value is cached, so the function is very fast.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-char-count}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-line-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_char_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_char_count" text-buffer-char-count) :int
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The integer for the number of characters in the text buffer.}
  @begin{short}
    Gets the number of characters in the text buffer.
  @end{short}
  Note that characters and bytes are not the same, you cannot, for example,
  expect the contents of the text buffer in string form to be this many bytes
  long. The character count is cached, so this function is very fast.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-line-count}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-char-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert" %text-buffer-insert) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (text (:string :free-to-foreign t))
  (len :int))

(defun text-buffer-insert (buffer text &key (position :cursor)
                                            (interactive nil)
                                            (editable t))
 #+liber-documentation
 "@version{#2025-06-30}
  @syntax{(gtk:text-buffer-insert buffer text) => t}
  @syntax{(gtk:text-buffer-insert buffer text :position position) => t}
  @syntax{(gtk:text-buffer-insert buffer text :interactive t) => t}
  @syntax{(gtk:text-buffer-insert buffer text :interactive t :editable nil) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[text]{a string for the text in UTF-8 format}
  @argument[position]{a @class{gtk:text-iter} iterator or the default value
    @code{:cursor}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction, the default value is @em{false}}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default,
    the default value is @em{true}}
  @return{The boolean whether the text was actually inserted.}
  @begin{short}
    Inserts text in the text buffer.
  @end{short}

  If the @arg{position} keyword argument has the @code{:cursor} value, the
  default, inserts the text using the current cursor position as the insertion
  point.

  If the @arg{interactive} keyword argument is @em{true}, the insertion will
  not occur if the iterator is at a non-editable location in the text buffer.
  Usually you want to prevent insertions at ineditable locations if the
  insertion results from a user action (is interactive).

  The @arg{editable} keyword argument indicates the editability of text that
  does not have a tag affecting editability applied to it. Typically the result
  of the @fun{gtk:text-view-editable} function is appropriate here.

  Emits the @sig[gtk:text-buffer]{insert-text} signal. Insertion actually occurs
  in the default handler for the signal. The iterator is invalidated when
  insertion occurs, because the text buffer contents change, but the default
  signal handler revalidates it to point to the end of the inserted text.
  @begin[Notes]{dictionary}
    The @fun{gtk:text-buffer-insert} function combines the
    @code{gtk_text_buffer_insert()}, @code{gtk_text_buffer_insert_at_cursor()},
    @code{gtk_text_buffer_insert_interactive()}, and
    @code{gtk_text_buffer_insert_interactive_at_cursor()} functions into one
    function using the @arg{position}, @arg{interactive}, and @arg{editable}
    keyword arguments. The corresponding Lisp functions except for
    @fun{gtk:text-buffer-insert} are not exported in the Lisp implementation.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-editable}"
  (assert (typep position '(or text-iter (member :cursor))))
  (if interactive
      (if (eq position :cursor)
          (%text-buffer-insert-interactive-at-cursor buffer
                                                     text
                                                     -1
                                                     editable)
          (%text-buffer-insert-interactive buffer
                                           position
                                           text
                                           -1
                                           editable))
      (progn
        (if (eq position :cursor)
            (%text-buffer-insert-at-cursor buffer text -1)
            (%text-buffer-insert buffer position text -1))
        t)))

(export 'text-buffer-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_at_cursor                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_at_cursor" %text-buffer-insert-at-cursor)
    :void
  (buffer (g:object text-buffer))
  (text (:string :free-to-foreign t))
  (len :int))

(defun text-buffer-insert-at-cursor (buffer text)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[text]{a string for the text in UTF-8 format}
  @begin{short}
    Calls the @fun{gtk:text-buffer-insert} function, using the current cursor
    position as the insertion point.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-insert}"
  (%text-buffer-insert-at-cursor buffer text -1))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_interactive"
               %text-buffer-insert-interactive) :boolean
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (text (:string :free-to-foreign t))
  (len :int)
  (editable :boolean))

(defun text-buffer-insert-interactive (buffer iter text editable)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator for a position in the text
    buffer}
  @argument[text]{a string for the UTF-8 text}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{The boolean whether the text was actually inserted.}
  @begin{short}
    Like the @fun{gtk:text-buffer-insert} function, but the insertion will not
    occur if the iterator is at a non-editable location in the text buffer.
  @end{short}
  Usually you want to prevent insertions at ineditable locations if the
  insertion results from a user action (is interactive).

  The @arg{editable} argument indicates the editability of text that does not
  have a tag affecting editability applied to it. Typically the result of the
  @fun{gtk:text-view-editable} function is appropriate here.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-insert}
  @see-function{gtk:text-view-editable}"
  (%text-buffer-insert-interactive buffer iter text -1 editable))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_interactive_at_cursor            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_interactive_at_cursor"
               %text-buffer-insert-interactive-at-cursor) :boolean
  (buffer (g:object text-buffer))
  (text (:string :free-to-foreign t))
  (len :int)
  (editable :boolean))

(defun text-buffer-insert-interactive-at-cursor (buffer text editable)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[text]{a string for the text in UTF-8 format}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{The boolean whether the text was actually inserted.}
  @begin{short}
    Calls the @fun{gtk:text-buffer-insert-interactive} function at the cursor
    position.
  @end{short}
  The @arg{editable} argument indicates the editability of text that does not
  have a tag affecting editability applied to it. Typically the result of the
  @fun{gtk:text-view-editable} function is appropriate here.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-insert}
  @see-function{gtk:text-buffer-insert-interactive}
  @see-function{gtk:text-view-editable}"
  (%text-buffer-insert-interactive-at-cursor buffer text -1 editable))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_range" %text-buffer-insert-range) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-insert-range (buffer iter start end
                                 &key interactive editable)
 #+liber-documentation
 "@version{#2025-06-30}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end) => t}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end :interactive t)
    => t}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} position in text buffer}
  @argument[start]{a @class{gtk:text-iter} start position}
  @argument[end]{a @class{gtk:text-iter} end position}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{The boolean whether an insertion was possible.}
  @begin{short}
    Copies text, tags, and pixbufs between the @arg{start} and @arg{end}
    iterators, the order of @arg{start} and @arg{end} does not matter, and
    inserts the copy at the @arg{iter} iterator.
  @end{short}
  Used instead of simply getting/inserting text because it preserves images and
  tags. If @arg{start} and @arg{end} are in a different text buffer from
  @arg{buffer}, the two buffers must share the same tag table.

  The @arg{interactive} keyword argument with the @em{true} value is the same,
  but does nothing if the insertion point is not editable. The @arg{editable}
  keyword argument indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the @fun{gtk:text-view-editable} function is appropriate here.

  Implemented via emissions of the @sig[gtk:text-buffer]{insert-text} and
  @sig[gtk:text-buffer]{apply-tag} signals, so expect those.
  @begin[Notes]{dictionary}
    The Lisp implementation combines the two
    @code{gtk_text_buffer_insert_range()} and
    @code{gtk_text_buffer_insert_range_interactive()} functions. The second
    function is not exported in the Lisp implementation,
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-editable}"
  (if interactive
      (%text-buffer-insert-range-interactive buffer
                                             iter
                                             start
                                             end
                                             editable)
      (progn
        (%text-buffer-insert-range buffer iter start end)
        t)))

(export 'text-buffer-insert-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range_interactive                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_range_interactive"
               %text-buffer-insert-range-interactive) :boolean
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator for a position in the text
    buffer}
  @argument[start]{a @class{gtk:text-iter} iterator for a position in the text
    buffer}
  @argument[end]{a @class{gtk:text-iter} iterator for another position in the
    same buffer as @arg{start}}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{The boolean whether an insertion was possible at the iterator.}
  @begin{short}
    Same as the @fun{gtk:text-buffer-insert-range} function, but does nothing
    if the insertion point is not editable.
  @end{short}
  The @arg{editable} argument indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the @fun{gtk:text-view-editable} function is appropriate here.
  @begin[Notes]{dictionary}
    The @fun{gtk:text-buffer-insert-range-interactive} function is called from
    the @fun{gtk:text-buffer-insert-range} function.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-insert-range}
  @see-function{gtk:text-view-editable}"
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (editable :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags
;;; ----------------------------------------------------------------------------

(defun text-buffer-insert-with-tags (buffer iter text &rest tags)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator in the text buffer}
  @argument[text]{a string for the UTF-8 text}
  @argument[tags]{@class{gtk:text-tag} objects or strings for the tag names
    to apply to @arg{text}}
  @begin{short}
    Inserts text into the text buffer at the position @arg{iter}, applying the
    list of tags to the newly inserted text.
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-insert} function, then the
  @fun{gtk:text-buffer-apply-tag} function on the inserted text. The
  @fun{gtk:text-buffer-insert-with-tags} function is just a convenience
  function.
  @begin[Notes]{dictionary}
    The Lisp implementation does not call the
    @code{gtk_text_buffer_insert_with_tags()} function, but uses the
    @fun{gtk:text-buffer-insert} and @fun{gtk:text-buffer-apply-tag} functions.
    The @code{gtk_text_buffer_insert_with_tags_by_name()} function is included
    in this function and not implemented in the Lisp library.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-buffer-insert}
  @see-function{gtk:text-buffer-apply-tag}"
  (let ((offset (text-iter-offset iter)))
    (prog1
      (text-buffer-insert buffer text :position iter)
      (let ((start (text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (text-buffer-apply-tag buffer tag start iter))))))

(export 'text-buffer-insert-with-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags_by_name                not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defun text-buffer-insert-with-tags-by-name (buffer iter text &rest tags)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator in text buffer}
  @argument[text]{a string for the UTF-8 text}
  @argument[tags]{strings for the tag names to apply to @arg{text}}
  @begin{short}
    Same as the @fun{gtk:text-buffer-insert-with-tags} function, but allows you
    to pass in tag names instead of tag objects.
  @end{short}
  @begin[Notes]{dictionary}
    The Lisp implementation does not call the C function, but uses the
    @fun{gtk:text-buffer-insert} and @fun{gtk:text-buffer-apply-tag} functions.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-insert-with-tags}
  @see-function{gtk:text-buffer-insert}
  @see-function{gtk:text-buffer-apply-tag-by-name}"
  (let ((offset (text-iter-offset iter)))
    (prog1
      (text-buffer-insert buffer text :position iter)
      (let ((start (text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (text-buffer-apply-tag-by-name buffer tag start iter))))))

#+nil
(export 'text-buffer-insert-with-tags-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_markup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_markup" %text-buffer-insert-markup) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (markup :string)
  (len :int))

(defun text-buffer-insert-markup (buffer iter markup)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator for a position in the text
    buffer}
  @argument[markup]{a UTF-8 string containing Pango markup}
  @begin{short}
    Inserts the text in @arg{markup} at the position of the iterator.
  @end{short}
  The text in @arg{markup} will be inserted in its entirety and must be valid
  UTF-8. Emits the @sig[gtk:text-buffer]{insert-text} signal, possibly multiple
  times. Insertion actually occurs in the default handler for the signal. The
  iterator will point to the end of the inserted text on return.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (%text-buffer-insert-markup buffer iter markup -1))

(export 'text-buffer-insert-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete" %text-buffer-delete) :void
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-delete (buffer start end &key interactive editable)
 #+liber-documentation
 "@version{#2025-06-30}
  @syntax{(gtk:text-buffer-delete buffer start end) => t}
  @syntax{(gtk:text-buffer-delete buffer start end :interactive t) => t}
  @syntax{(gtk:text-buffer-delete buffer start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start position in the text buffer}
  @argument[end]{a @class{gtk:text-iter} end position in the text buffer}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @return{The boolean whether some text was actually deleted.}
  @begin{short}
    Deletes text between the @arg{start} and @arg{end} iterators.
  @end{short}
  The order of the @arg{start} and @arg{end} iterators is not actually relevant.
  The @fun{gtk:text-buffer-delete} function will reorder them. This function
  actually emits the @sig[gtk:text-buffer]{delete-range} signal, and the default
  handler of that signal deletes the text. Because the text buffer is modified,
  all outstanding iterators become invalid after calling this function. However,
  the @arg{start} and @arg{end} interators will be re-initialized to point to
  the location where text was deleted.

  If the @arg{interactive} keyword argument is @em{true} deletes all editable
  text for each editable sub range of [@arg{start}, @arg{end}). The @arg{start}
  and @arg{end} iterators are revalidated to point to the location of the last
  deleted range, or left untouched if no text was deleted.
  @begin[Notes]{dictionary}
    The @code{gtk_text_buffer_delete_interactive()} function is included in
    this function and not implemented in the Lisp libraray.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (if interactive
      (%text-buffer-delete-interactive buffer start end editable)
      (progn
        (%text-buffer-delete buffer start end)
        t)))

(export 'text-buffer-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_interactive                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_interactive"
               %text-buffer-delete-interactive) :boolean
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start of range to delete}
  @argument[end]{a @class{gtk:text-iter} end of range}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{The boolean whether some text was actually deleted.}
  @begin{short}
    Deletes all editable text in the given range.
  @end{short}
  Calls the @fun{gtk:text-buffer-delete} function for each editable sub range
  of [@arg{start}, @arg{end}). @arg{start} and @arg{end} are revalidated to
  point to the location of the last deleted range, or left untouched if no
  text was deleted.
  @begin[Notes]{dictionary}
    In the Lisp implementation this function is called from the
    @fun{gtk:text-buffer-delete} function.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-delete}"
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (editable :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_backspace
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_backspace" %text-buffer-backspace) :boolean
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (interactive :boolean)
  (editable :boolean))

(defun text-buffer-backspace (buffer iter &key interactive editable)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} position in @arg{buffer}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{@em{True} if the text buffer was modified.}
  @begin{short}
    Performs the appropriate action as if the user hit the delete key with the
    cursor at the position specified by @arg{iter}.
  @end{short}
  In the normal case a single character will be deleted, but when combining
  accents are involved, more than one character can be deleted, and when
  precomposed character and accent combinations are involved, less than one
  character will be deleted.

  Because the text buffer is modified, all outstanding iterators become invalid
  after calling this function. However, the iterator will be re-initialized to
  point to the location where text was deleted.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (%text-buffer-backspace buffer iter interactive editable))

(export 'text-buffer-backspace)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_text" %text-buffer-get-text) :string
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (include :boolean))

(defun text-buffer-get-text (buffer start end &optional include)
 #+liber-documentation
 "@version{2024-01-02}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start iterator of a range}
  @argument[end]{a @class{gtk:text-iter} end iterator of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{The allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}. Does not include
  characters representing embedded images, so byte and character indexes into
  the returned string do not correspond to byte and character indexes into the
  text buffer. Contrast with the @fun{gtk:text-buffer-get-slice} function.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-text}
  @see-function{gtk:text-buffer-get-slice}"
  (%text-buffer-get-text buffer start end include))

(export 'text-buffer-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_slice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_slice" %text-buffer-get-slice)
    (:string :free-from-foreign t)
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (include :boolean))

(defun text-buffer-get-slice (buffer start end &optional include)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start of a range}
  @argument[end]{a @class{gtk:text-iter} end of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{The allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}.

  The returned string includes a @code{0xFFFC} character whenever the text
  buffer contains embedded images, so byte and character indexes into the
  returned string do correspond to byte and character indexes into the text
  buffer. Contrast with the @fun{gtk:text-buffer-get-text} function. Note that
  @code{0xFFFC} can occur in normal text as well, so it is not a reliable
  indicator that a pixbuf or widget is in the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-get-text}"
  (%text-buffer-get-slice buffer start end include))

(export 'text-buffer-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_pixbuf" text-buffer-insert-pixbuf) :void
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} location to insert the pixbuf}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Inserts an image into the text buffer at @arg{iter}.
  @end{short}
  The image will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for pixbufs, but the \"text\" variants do not, for example, see the
  @fun{gtk:text-buffer-get-slice} and @fun{gtk:text-buffer-get-text} functions.
  @see-class{gtk:text-buffer}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:text-buffer-get-slice}
  @see-function{gtk:text-buffer-get-text}"
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'text-buffer-insert-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_child_anchor"
          %text-buffer-insert-child-anchor) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (anchor (g:object text-child-anchor)))

(defun text-buffer-insert-child-anchor (buffer position &optional anchor)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} location to insert the anchor}
  @argument[anchor]{an optional @class{gtk:text-child-anchor} object}
  @return{The @class{gtk:text-child-anchor} child widget anchor.}
  @begin{short}
    Inserts a child widget anchor into the text buffer at @arg{iter}.
  @end{short}
  The anchor will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for anchors, but the \"text\" variants do not, for example, see the
  @fun{gtk:text-buffer-get-slice} and @fun{gtk:text-buffer-get-text} functions.
  Consider the @fun{gtk:text-buffer-create-child-anchor} function as a more
  convenient alternative to this function. The text buffer will add a reference
  to the anchor, so you can unref it after insertion.

  If the @arg{anchor} argument is @code{nil} creates an anchor with the
  @fun{gtk:text-child-anchor-new} function and inserts it into @arg{buffer} with
  the @fun{gtk:text-buffer-insert-child-anchor} function. The new anchor is
  owned by the text buffer.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-get-slice}
  @see-function{gtk:text-buffer-get-text}
  @see-function{gtk:text-buffer-child-anchor-new}
  @see-function{gtk:text-buffer-insert-child-anchor}
  @see-function{gtk:text-buffer-create-child-anchor}"
  (if anchor
      (progn
        (%text-buffer-insert-child-anchor buffer position anchor)
        anchor)
      (text-buffer-create-child-anchor buffer position)))

(export 'text-buffer-insert-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_create_child_anchor"
          text-buffer-create-child-anchor) (g:object text-child-anchor)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} location in the text buffer}
  @return{The created @class{gtk:text-child-anchor} anchor.}
  @begin{short}
    This is a convenience function which simply creates an anchor with the
    @fun{gtk:text-child-anchor-new} function and inserts it into the text buffer
    with the @fun{gtk:text-buffer-insert-child-anchor} function.
  @end{short}
  The new anchor is owned by the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-child-anchor}
  @see-function{gtk:text-child-anchor-new}
  @see-function{gtk:text-buffer-insert-child-anchor}"
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(export 'text-buffer-create-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_create_mark" %text-buffer-create-mark)
    (g:object text-mark)
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t))
  (where (g:boxed text-iter))
  (left-gravity :boolean))

(defun text-buffer-create-mark (buffer name pos &optional (gravity t))
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name for the mark, or @code{nil}}
  @argument[pos]{a @class{gtk:text-iter} location to place the mark}
  @argument[gravity]{a boolean whether the mark has left gravity}
  @return{The new @class{gtk:text-mark} object.}
  @begin{short}
    Creates a mark at position @arg{pos}.
  @end{short}
  If the @arg{mark} argument is @code{nil}, the mark is anonymous. Otherwise,
  the mark can be retrieved by name using the @fun{gtk:text-buffer-mark}
  function. If a mark has left gravity, and text is inserted at the current
  location of the mark, the mark will be moved to the left of the newly inserted
  text. If the mark has right gravity, the mark will end up on the right of
  newly inserted text. The standard left-to-right cursor is a mark with right
  gravity, when you type, the cursor stays on the right side of the text you are
  typing.

  The caller of this function does not own a reference to the returned
  @class{gtk:text-mark} object, so you can ignore the return value if you like.
  Marks are owned by the text buffer and go away when the text buffer does.

  Emits the @sig[gtk:text-buffer]{mark-set} signal as notification of the
  initial placement of the mark.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}"
  (%text-buffer-create-mark buffer name pos gravity))

(export 'text-buffer-create-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_move_mark" %text-buffer-move-mark) :void
  (buffer (g:object text-buffer))
  (mark (g:object text-mark))
  (pos (g:boxed text-iter)))

(defun text-buffer-move-mark (buffer mark pos)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the name
    of the mark}
  @argument[pos]{new @class{gtk:text-iter} location for @arg{mark} in the
    text buffer}
  @begin{short}
    Moves the mark to the new location @arg{pos}.
  @end{short}
  Emits the @sig[gtk:text-buffer]{mark-set} signal as notification of the move.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}"
  (if (stringp mark)
      (%text-buffer-move-mark-by-name buffer mark pos)
      (%text-buffer-move-mark buffer mark pos)))

(export 'text-buffer-move-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark_by_name                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_move_mark_by_name"
               %text-buffer-move-mark-by-name) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of a mark}
  @argument[where]{new @class{gtk:text-iter} location for mark}
  @begin{short}
    Moves the mark named @arg{name}, which must exist, to location @arg{where}.
  @end{short}
  See the @fun{gtk:text-buffer-move-mark} function for details.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-move-mark}"
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t))
  (pos (g:boxed text-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_add_mark" text-buffer-add-mark) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object for the mark to add}
  @argument[pos]{a @class{gtk:text-iter} iterator for the location to place
    the mark}
  @begin{short}
    Adds the mark at the given position.
  @end{short}
  The mark must not be added to another text buffer, and if its name is not
  @code{nil} then there must not be another mark in the text buffer with the
  same name.

  Emits the @sig[text-buffer]{mark-set} signal as notification of the initial
  placement of the mark.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-class{gtk:text-iter}"
  (buffer (g:object text-buffer))
  (mark (g:object text-mark))
  (pos (g:boxed text-iter)))

(export 'text-buffer-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_mark" %text-buffer-delete-mark) :void
  (buffer (g:object text-buffer))
  (mark (g:object text-mark)))

(defun text-buffer-delete-mark (buffer mark)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the name
    of a mark in the text buffer}
  @begin{short}
    Deletes the mark, so that it is no longer located anywhere in the text
    buffer.
  @end{short}
  Removes the reference the text buffer holds to the mark. Most operations on
  the mark become invalid, until it gets added to a text buffer again with the
  @fun{gtk:text-buffer-add-mark} function. Use the @fun{gtk:text-mark-deleted}
  function to find out if a mark has been removed from its text buffer. The
  @sig[gtk:text-buffer]{mark-deleted} signal will be emitted as notification
  after the mark is deleted.
  @begin[Notes]{dictionary}
    The @code{gtk_text_buffer_delete_mark_by_name} function is included in
    this function and not exported in the Lisp library.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-add-mark}
  @see-function{gtk:text-mark-deleted}"
  (if (stringp mark)
      (%text-buffer-delete-mark-by-name buffer mark)
      (%text-buffer-delete-mark buffer mark)))

(export 'text-buffer-delete-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark_by_name                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_mark_by_name"
               %text-buffer-delete-mark-by-name) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of a mark in text buffer}
  @begin{short}
    Deletes the mark named @arg{name}.
  @end{short}
  The mark must exist. See the @fun{gtk:text-buffer-delete-mark} function for
  details.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-delete-mark}"
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_mark" text-buffer-mark)
    (g:object text-mark)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for a mark name}
  @return{The @class{gtk:text-mark} object, or @code{nil}.}
  @begin{short}
    Returns the mark named @arg{name} in the text buffer, or @code{nil} if
    no such mark exists in the text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}"
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t)))

(export 'text-buffer-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_insert
;;; ----------------------------------------------------------------------------

;; It is wrong to implement this as text-buffer-insert, we have already a
;; function with this name.

(cffi:defcfun ("gtk_text_buffer_get_insert" text-buffer-get-insert)
    (g:object text-mark)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-mark} insertion point mark.}
  @begin{short}
    Returns the mark that represents the cursor (insertion point).
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-mark} function to get the mark
  named \"insert\", but more efficient.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-get-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bound
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_selection_bound"
               text-buffer-selection-bound) (g:object text-mark)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-mark} selection bound mark.}
  @begin{short}
    Returns the mark that represents the selection bound.
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-mark} function to get the mark
  named \"selection_bound\", but very slightly more efficient, and involves less
  typing.

  The currently selected text in the text buffer is the region between the
  \"selection_bound\" and \"insert\" marks. If the \"selection_bound\" and
  \"insert\" marks are in the same place, then there is no current selection.
  The @fun{gtk:text-buffer-selection-bounds} function is another convenient
  function for handling the selection, if you just want to know whether there
  is a selection and what its bounds are.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}
  @see-function{gtk:text-buffer-selection-bounds}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-selection-bound)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_place_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_place_cursor" text-buffer-place-cursor) :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[pos]{a @class{gtk:text-iter} iterator where to put the cursor}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them to the same place in two steps with the
  @fun{gtk:text-buffer-move-mark} function, you will temporarily select a region
  in between their old and new locations, which can be pretty inefficient since
  the temporarily selected region will force stuff to be recalculated. This
  function moves them as a unit, which can be optimized.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-move-mark}"
  (buffer (g:object text-buffer))
  (pos (g:boxed text-iter)))

(export 'text-buffer-place-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_select_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_select_range" text-buffer-select-range) :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[insertion]{a @class{gtk:text-iter} iterator where to put the
    \"insert\" mark}
  @argument[selection]{a @class{gtk:text-iter} iterator where to put the
    \"selection_bound\" mark}
  @begin{short}
    This function moves the \"insert\" and \"selection_bound\" marks
    simultaneously.
  @end{short}
  If you move them in two steps with the @fun{gtk:text-buffer-move-mark}
  function, you will temporarily select a region in between their old and new
  locations, which can be pretty inefficient since the temporarily selected
  region will force stuff to be recalculated. This function moves them as a
  unit, which can be optimized.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-move-mark}"
  (buffer (g:object text-buffer))
  (insertion (g:boxed text-iter))
  (selection (g:boxed text-iter)))

(export 'text-buffer-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_apply_tag" %text-buffer-apply-tag) :void
  (buffer (g:object text-buffer))
  (tag (g:object text-tag))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-apply-tag (buffer tag start end)
 #+liber-documentation
 "@version{2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[tag]{a @class{gtk:text-tag} object, or a string for the tag name}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of
    the range to be tagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be tagged}
  @begin{short}
    Emits the @sig[gtk:text-buffer]{apply-tag} signal on the text buffer.
  @end{short}
  The default handler for the signal applies @arg{tag} to the given range.
  The @arg{start} and @arg{end} iterators do not have to be in order.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}"
  (if (stringp tag)
      (%text-buffer-apply-tag-by-name buffer tag start end)
      (%text-buffer-apply-tag buffer tag start end)))

(export 'text-buffer-apply-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag_by_name                       no exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_apply_tag_by_name"
               %text-buffer-apply-tag-by-name) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of a named @class{gtk:text-tag} object}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of
    the range to be tagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be tagged}
  @begin{short}
    Calls the @fun{gtk:text-tag-table-lookup} function on the tag table of the
    text buffer to get a @class{gtk:text-tag} object, then calls the
    @fun{gtk:text-buffer-apply-tag} function.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-tag-table-lookup}
  @see-function{gtk:text-buffer-apply-tag}"
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_tag" %text-buffer-remove-tag) :void
  (buffer (g:object text-buffer))
  (tag (g:object text-tag))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-remove-tag (buffer tag start end)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[tag]{a @class{gtk:text-tag} object, or a string for the tag name}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be untagged}
  @begin{short}
    Emits the @sig[text-buffer]{remove-tag} signal.
  @end{short}
  The default handler for the signal removes all occurrences of @arg{tag} from
  the given range. The @arg{start} and @arg{end} iterators do not have to be in
  order.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}"
  (if (stringp tag)
      (%text-buffer-remove-tag-by-name buffer tag start end)
      (%text-buffer-remove-tag buffer tag start end)))

(export 'text-buffer-remove-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag_by_name                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_tag_by_name"
               %text-buffer-remove-tag-by-name) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of a @class{gtk:text-tag} object}
  @argument[start]{a @class{gtk:text-iter} iterator for one bound of range
    to be untagged}
  @argument[end]{a @class{gtk:text-iter} iterator for other bound of range
    to be untagged}
  @begin{short}
    Calls the @fun{gtk:text-tag-table-lookup} function on the tag table of the
    text buffer to get a @class{gtk:text-tag} object, then calls the
    @fun{gtk:text-buffer-remove-tag} function.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-tag-table-lookup}
  @see-function{gtk:text-buffer-remove-tag}"
  (buffer (g:object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_all_tags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_all_tags" text-buffer-remove-all-tags)
    :void
 #+liber-documentation
 "@version{2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be untagged}
  @begin{short}
    Removes all tags in the range between the @arg{start} and @arg{end}
    iterators.
  @end{short}
  Be careful with this function: it could remove tags added in code unrelated
  to the code you are currently writing. That is, using this function is
  probably a bad idea if you have two or more unrelated code sections that add
  tags.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(export 'text-buffer-remove-all-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_tag
;;; ----------------------------------------------------------------------------

(defun text-buffer-create-tag (buffer name &rest args)
 #+liber-documentation
 "@version{2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of the new tag, or @code{nil}}
  @argument[args]{list of property keywords and values}
  @return{The new @class{gtk:text-tag} object.}
  @begin{short}
    Creates a tag and adds it to the tag table for the text buffer.
  @end{short}
  Equivalent to calling the @fun{gtk:text-tag-new} function and then adding the
  tag to the tag table of the text buffer.

  If the @arg{name} argument is @code{nil}, the tag is anonymous. If the
  @arg{name} argument is non-@code{nil}, a tag called @arg{name} must not
  already exist in the tag table for this text buffer.

  The @arg{args} argument is a list of properties and values to set on the
  tag.
  @begin[Examples]{dictionary}
    Create and add a tag with name \"font-italic\" to the text buffer.
    @begin{pre}
(defvar buffer (gtk:text-buffer-new)) => BUFFER
(gtk:text-buffer-create-tag buffer \"font-italic\"
                                   :font \"fixed\" :style :italic)
=> #<gtk:text-TAG {1002193283@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-new}"
  (let ((tag (apply #'make-instance 'text-tag :name name args)))
    (when (text-tag-table-add (text-buffer-tag-table buffer) tag)
      tag)))

(export 'text-buffer-create-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line_offset"
               %text-buffer-iter-at-line-offset) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int)
  (offset :int))

(defun text-buffer-iter-at-line-offset (buffer line offset)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @argument[offset]{an integer for the char offset from the start of the line}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{offset} within the given line.
  @end{short}
  Note characters, not bytes, UTF-8 may encode one character as multiple bytes.

  If the @arg{line} argument is greater than the number of lines in the text
  buffer, the end iterator is returned. And if the @arg{offset} argument is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line-offset buffer iter line offset)
    iter))

(export 'text-buffer-iter-at-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_offset"
               %text-buffer-iter-at-offset) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (offset :int))

(defun text-buffer-iter-at-offset (buffer offset)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[offset]{an integer for the char offset from the start of the text
    buffer, counting from 0, or -1}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to a position @arg{offset} chars from the
    start of the entire text buffer.
  @end{short}
  If the @arg{offset} argument is -1 or greater than the number of characters
  in the text buffer, the iterator is initialized to the end iterator, the
  iterator one past the last valid character in the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-offset buffer iter offset)
    iter))

(export 'text-buffer-iter-at-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line"
               %text-buffer-iter-at-line) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int))

(defun text-buffer-iter-at-line (buffer line)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to the start of the given line.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line buffer iter line)
    iter))

(export 'text-buffer-iter-at-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line_index"
               %text-buffer-iter-at-line-index) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int)
  (index :int))

(defun text-buffer-iter-at-line-index (buffer line index)
 #+liber-documentation
 "@version{2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @argument[index]{an integer for the byte index from the start of the line}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{index} within the given line.
  @end{short}
  The @arg{index} argument must be the start of a UTF-8 character. Note bytes,
  not characters, UTF-8 may encode one character as multiple bytes.

  If the @arg{line} argument is greater than the number of lines in the text
  buffer, the end iterator is returned. And if the @arg{index} argument is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line-index buffer iter line index)
    iter))

(export 'text-buffer-iter-at-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_mark"
               %text-buffer-iter-at-mark) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (mark (g:object text-mark)))

(defun text-buffer-iter-at-mark (buffer mark)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the mark
    name in the text buffer}
  @return{The @class{gtk:text-iter} interator.}
  @begin{short}
    Returns the iterator with the current position of @arg{mark}.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}"
  (let ((iter (make-instance 'text-iter))
        (mark (if (stringp mark) (text-buffer-mark buffer mark) mark)))
    (%text-buffer-iter-at-mark buffer iter mark)
    iter))

(export 'text-buffer-iter-at-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_child_anchor"
               %text-buffer-iter-at-child-anchor) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (anchor (g:object text-child-anchor)))

(defun text-buffer-iter-at-child-anchor (buffer anchor)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[anchor]{a @class{gtk:text-child-anchor} anchor that appears in text
    buffer}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains the location of @arg{anchor} within the text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-child-anchor}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-child-anchor buffer iter anchor)
    iter))

(export 'text-buffer-iter-at-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_start_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_start_iter" %text-buffer-start-iter) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(defun text-buffer-start-iter (buffer)
 #+liber-documentation
 "@version{2024-01-01}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Returns an iterator with the first position in the text buffer.
  @end{short}
  This is the same as using the @fun{gtk:text-buffer-iter-at-offset} function
  to get the itererator at character offset 0.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-iter-at-offset}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-start-iter buffer iter)
    iter))

(export 'text-buffer-start-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_end_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_end_iter" %text-buffer-end-iter) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(defun text-buffer-end-iter (buffer)
 #+liber-documentation
 "@version{2024-01-02}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Returns an iterator with the \"end iterator\", one past the last valid
    character in the text buffer.
  @end{short}
  If dereferenced with the @fun{gtk:text-iter-char} function, the end iterator
  has a character value of 0. The entire text buffer lies in the range from the
  first position in the text buffer to the end iterator. Call the
  @fun{gtk:text-buffer-start-iter} function to get character position 0.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-start-iter}
  @see-function{gtk:text-iter-char}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-end-iter buffer iter)
    iter))

(export 'text-buffer-end-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_bounds" %text-buffer-bounds) :void
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-bounds (buffer)
 #+liber-documentation
 "@version{#2025-07-11}
  @syntax{(gtk:text-buffer-bounds buffer) => start, end}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} iterator for the first position in
    the text buffer}
  @argument[end]{a @class{gtk:text-iter} iterator for the end position in the
    text buffer}
  @begin{short}
    Retrieves the first and last iterators in the text buffer, that is, the
    entire text buffer lies within the range [@arg{start}, @arg{end}).
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (%text-buffer-bounds buffer start end)
    (values start end)))

(export 'text-buffer-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_modified
;;; gtk_text_buffer_set_modified
;;; ----------------------------------------------------------------------------

(defun (setf text-buffer-modified) (setting buffer)
  (cffi:foreign-funcall "gtk_text_buffer_set_modified"
                        (g:object text-buffer) buffer
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gtk_text_buffer_get_modified" text-buffer-modified) :boolean
 "@version{#2025-06-30}
  @syntax{(gtk:text-buffer-modified buffer) => setting}
  @syntax{(setf (gtk:text-buffer-modified buffer) setting)}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[setting]{a boolean for the modification flag setting}
  @begin{short}
    Returns @em{true} if the text buffer has been modified.
  @end{short}
  The @fun{gtk:text-buffer-modified} function indicates whether the text buffer
  has been modified since the last call to the
  @setf{gtk:text-buffer-modified} function.

  Used to keep track of whether the text buffer has been modified since the
  last time it was saved. Whenever the text buffer is saved to disk, call the
  @setf{gtk:text-buffer-modified} function with the @em{false} value. When the
  text buffer is modified, it will automatically toggle on the modified bit
  again. When the modified bit flips, the text buffer emits a
  @sig[text-buffer]{modified-changed} signal.
  @see-class{gtk:text-buffer}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-modified)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_selection" %text-buffer-delete-selection)
    :boolean
  (buffer (g:object text-buffer))
  (interactive :boolean)
  (editable :boolean))

(defun text-buffer-delete-selection (buffer &key interactive editable)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @return{The boolean whether there was a non-empty selection to delete.}
  @begin{short}
    Deletes the range between the \"insert\" and \"selection_bound\" marks,
    that is, the currently selected text.
  @end{short}
  If the @arg{interactive} argument is @em{true}, the editability of the
  selection will be considered, users cannot delete uneditable text.
  @see-class{gtk:text-buffer}"
  (%text-buffer-delete-selection buffer interactive editable))

(export 'text-buffer-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_paste_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_paste_clipboard" %text-buffer-paste-clipboard)
    :void
  (buffer (g:object text-buffer))
  (clipboard (g:object clipboard))
  (override (g:boxed text-iter))
  (editable :boolean))

(defun text-buffer-paste-clipboard (buffer clipboard &key override editable)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gtk:clipboard} object to paste from}
  @argument[override]{a @class{gtk:text-iter} location to insert pasted text,
    or @code{nil} to insert at the cursor}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Pastes the contents of a clipboard at the insertion point, or at
    @arg{override}.
  @end{short}
  @begin[Notes]{dictionary}
    Pasting is asynchronous, that is, we will ask for the paste data and return,
    and at some point later after the main loop runs, the paste data will be
    inserted.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:clipboard}
  @see-class{gtk:text-iter}"
  (%text-buffer-paste-clipboard buffer clipboard override editable))

(export 'text-buffer-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_copy_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_copy_clipboard" text-buffer-copy-clipboard)
    :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gtk:clipboard} object to copy to}
  @begin{short}
    Copies the currently selected text to the clipboard.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object clipboard)))

(export 'text-buffer-copy-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_cut_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_cut_clipboard" text-buffer-cut-clipboard) :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gtk:clipboard} object to cut to}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Copies the currently selected text to a clipboard, then deletes the text
    if it is editable.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object clipboard))
  (editable :boolean))

(export 'text-buffer-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_selection_bounds"
               %text-buffer-selection-bounds) :boolean
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-selection-bounds (buffer)
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{return}
    @arg{start} -- a @class{gtk:text-iter} iterator with the selection start,
      or @code{nil} @br{}
    @arg{end} -- a @class{gtk:text-iter} iterator with the selection end,
      or @code{nil}
  @end{return}
  @begin{short}
    Returns the @arg{start} and @arg{end} iterators if some text is selected.
  @end{short}
  If the selection has length 0, then the @arg{start} and @arg{end} iterators
  are filled in with the same value. The @arg{start} and @arg{end} iterators
  will be in ascending order.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (if (%text-buffer-selection-bounds buffer start end)
        (values start end)
        (values nil nil))))

(export 'text-buffer-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_begin_user_action
;;; ----------------------------------------------------------------------------

;; Example implementation of a macro, not exported at this time.

(defmacro with-text-buffer-user-action ((buffer) &body body)
  (let ((g (gensym)))
    `(let ((,g ,buffer))
       (text-buffer-begin-user-action ,g)
       (unwind-protect
         (progn ,@body)
         (text-buffer-end-user-action ,g)))))

;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_begin_user_action"
               text-buffer-begin-user-action) :void
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Called to indicate that the text buffer operations between here and a call
    to the @fun{gtk:text-buffer-end-user-action} function are part of a single
    user visible operation.
  @end{short}
  The operations between the @fun{gtk:text-buffer-begin-user-action} and
  @fun{gtk:text-buffer-end-user-action} functions can then be grouped when
  creating an undo stack. The text buffer maintains a count of calls to the
  @fun{gtk:text-buffer-begin-user-action} function that have not been closed
  with a call to the @fun{gtk:text-buffer-end-user-action} function, and emits
  the @sig[text-buffer]{begin-user-action} and
  @sig[gtk:text-buffer]{end-user-action} signals only for the outermost pair of
  calls. This allows you to build user actions from other user actions.

  The \"interactive\" text buffer mutation functions automatically call
  begin/end user action around the text buffer operations they perform, so there
  is no need to add extra calls if the user action consists solely of a single
  call to one of those functions.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-end-user-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-begin-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_end_user_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_end_user_action" text-buffer-end-user-action)
    :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Should be paired with a call to the @fun{gtk:text-buffer-begin-user-action}
    function.
  @end{short}
  See that function for a full explanation.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-begin-user-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-end-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_selection_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_add_selection_clipboard"
               text-buffer-add-selection-clipboard) :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gtk:clipboard} object}
  @begin{short}
    Adds a clipboard to the list of clipboards in which the selection contents
    of the text buffer are available.
  @end{short}
  In most cases, the clipboard will be of type \"PRIMARY\" for a view of the
  text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object clipboard)))

(export 'text-buffer-add-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_selection_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_selection_clipboard"
               text-buffer-remove-selection-clipboard) :void
 #+liber-documentation
 "@version{#2023-03-07}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gtk:clipboard} object added to the text buffer}
  @begin{short}
    Removes a clipboard added with the
    @fun{gtk:text-buffer-add-selection-clipboard} function.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:clipboard}
  @see-function{gtk:text-buffer-add-selection-clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object clipboard)))

(export 'text-buffer-remove-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferDeserializeFunc
;;; ----------------------------------------------------------------------------

;; TODO: Check this implementation

(cffi:defcallback text-buffer-deserialize-func :boolean
    ((buffer (g:object text-buffer))
     (content (g:object text-buffer))
     (iter (g:boxed text-iter))
     (data :pointer)
     (len :size)
     (create :boolean)
     (user :pointer)
     (err :pointer))
  (glib:with-catching-to-error (err)
    (let ((func (glib:get-stable-pointer-value user)))
      (restart-case
        (let ((bytes (iter (with bytes = (make-array len
                                                     :element-type
                                                     '(unsigned-byte 8)))
                           (for i from 0 below len)
                           (setf (aref bytes i) (cffi:mem-ref data :uint8 i))
                           (finally (return bytes)))))
          (progn
            (funcall func buffer content iter bytes create)
            t))
        (return ()
            (error 'gerror-condition
                   :domain "cl-cffi-gtk"
                   :code 0
                   :message
                   "return-from-text-buffer-deserialize-func"))))))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-buffer-deserialize-func)
      "Callback"
      (liber:symbol-documentation 'text-buffer-deserialize-func)
 "@version{#2024-11-20}
  @syntax{lambda (buffer content iter data create) => result}
  @argument[buffer]{a @class{gtk:text-buffer} object the format is registered
    with}
  @argument[content]{a @class{gtk:text-buffer} object to deserialize into}
  @argument[iter]{a @class{gtk:text-iter} insertion point for the deserialized
    text}
  @argument[data]{a pointer to the data to deserialize}
  @argument[create]{@em{true} if deserializing may create tags}
  @argument[result]{@em{true} on success, @em{false} otherwise}
  @begin{short}
    A function that is called to deserialize rich text that has been serialized
    with the @fun{gtk:text-buffer-serialize} function, and insert it at
    @arg{iter}.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-serialize}")

(export 'text-buffer-deserialize-func)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_deserialize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_deserialize" %text-buffer-deserialize) :boolean
  (buffer (g:object text-buffer))
  (content (g:object text-buffer))
  (format gdk:atom-as-string)
  (iter (g:boxed text-iter))
  (data :pointer)
  (len :size)
  (err :pointer))

(defun text-buffer-deserialize (buffer content format iter data)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object format is registered with}
  @argument[content]{a @class{gtk:text-buffer} object to deserialize into}
  @argument[format]{a string for the rich text format to use for deserializing}
  @argument[iter]{a @class{gtk:text-iter} insertion point for the deserialized
    text}
  @argument[data]{a pointer to the data to deserialize}
  @return{@em{True} on success, @code{nil} otherwise.}
  @begin{short}
    This function deserializes rich text in @arg{format} and inserts it at
    @arg{iter}.
  @end{short}
  The rich text formats to be used must be registered using the
  @fun{gtk:text-buffer-register-deserialize-format} or
  @fun{gtk:text-buffer-register-deserialize-tagset} functions beforehand.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-register-deserialize-format}
  @see-function{gtk:text-buffer-register-deserialize-tagset}"
  (let ((bytes (cffi:foreign-alloc :uint8 :count (length data))))
    (iter (for i from 0 below (length data))
          (setf (cffi:mem-aref bytes :uint8 i) (aref data i)))
    (unwind-protect
      (glib:with-error (err)
        (%text-buffer-deserialize buffer
                                  content
                                  format
                                  iter
                                  bytes
                                  (length data)
                                  err))
      (cffi:foreign-free bytes))))

(export 'text-buffer-deserialize)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_deserialize_get_can_create_tags
;;; gtk_text_buffer_deserialize_set_can_create_tags
;;; ----------------------------------------------------------------------------

(defun (setf text-buffer-deserialize-can-create-tags) (value buffer format)
  (cffi:foreign-funcall "gtk_text_buffer_deserialize_set_can_create_tags"
                        (g:object text-buffer) buffer
                        gdk:atom-as-string format
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_text_buffer_deserialize_get_can_create_tags"
               text-buffer-deserialize-can-create-tags) :boolean
 #+liber-documentation
 "@version{#2025-07-11}
  @syntax{(gtk:text-buffer-deserialize-can-create-tags buffer format) => create}
  @syntax{(setf (gtk:text-buffer-deserialize-can-create-tags buffer format) create)}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[format]{a string representing a registered rich text format}
  @argument[create]{a boolean whether deserializing this format may create tags}
  @begin{short}
    Use this function to allow a rich text deserialization function to create
    new tags in the receiving text buffer.
  @end{short}
  Note that using this function is almost always a bad idea, because the rich
  text functions you register should know how to map the rich text format they
  handle to your text buffers set of tags.

  The ability of creating new tags in the receiving text buffer is meant for
  special rich text formats like the internal one that is registered using the
  @fun{gtk:text-buffer-register-deserialize-tagset} function, because that
  format is essentially a dump of the internal structure of the source buffer,
  including its tag names.

  You should allow creation of tags only if you know what you are doing, for
  example, if you defined a tagset name for your application text buffers and
  you know that it is fine to receive new tags from these buffers, because you
  know that your application can handle the newly created tags.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-deserialize-tagset}"
  (buffer (g:object text-buffer))
  (format gdk:atom-as-string))

(export 'text-buffer-deserialize-can-create-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_deserialize_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_deserialize_formats"
               %text-buffer-deserialize-formats) (:pointer gdk:atom-as-string)
  (buffer (g:object text-buffer))
  (n-formats (:pointer :int)))

(defun text-buffer-deserialize-formats (buffer)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The list of strings representing the registered formats.}
  @begin{short}
    This function returns the rich text deserialize formats registered with the
    text buffer using the @fun{gtk:text-buffer-register-deserialize-format} or
    @fun{gtk:text-buffer-register-deserialize-tagset} functions.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-deserialize-format}
  @see-function{gtk:text-buffer-register-deserialize-tagset}"
  (cffi:with-foreign-object (n-formats :int)
    (let ((atoms-ptr (%text-buffer-deserialize-formats buffer n-formats)))
      (iter (for i from 0 below (cffi:mem-ref n-formats :int))
            (for atom = (cffi:mem-aref atoms-ptr 'gdk:atom-as-string i))
            (collect atom)))))

(export 'text-buffer-deserialize-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_serialize_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_serialize_formats"
               %text-buffer-serialize-formats) (:pointer gdk:atom-as-string)
  (buffer (g:object text-buffer))
  (n-formats (:pointer :int)))

(defun text-buffer-serialize-formats (buffer)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The list of strings representing the registered formats.}
  @begin{short}
    This function returns the rich text serialize formats registered with
    @arg{buffer} using the @fun{gtk:text-buffer-register-serialize-format} or
    @fun{gtk:text-buffer-register-serialize-tagset} functions.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-format}
  @see-function{gtk:text-buffer-register-serialize-tagset}"
  (cffi:with-foreign-object (n-formats :int)
    (let ((atoms-ptr (%text-buffer-serialize-formats buffer n-formats)))
      (iter (for i from 0 below (cffi:mem-ref n-formats :int))
            (for atom = (cffi:mem-aref atoms-ptr 'gdk:atom-as-string i))
            (collect atom)))))

(export 'text-buffer-serialize-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_deserialize_format
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_register_deserialize_format"
               %text-buffer-register-deserialize-format) gdk:atom-as-string
  (buffer (g:object text-buffer))
  (mime :string)
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun text-buffer-register-deserialize-format (buffer mime func)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mime]{a string for the MIME type of the format}
  @argument[func]{a @sym{gtk:text-buffer-deserialize-func} deserialize function
    to register}
  @begin{return}
    The string that corresponds to the newly registered MIME type of the format.
  @end{return}
  @begin{short}
    This function registers a rich text deserialization function along with its
    MIME type with the passed text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-buffer-deserialize-func}"
  (%text-buffer-register-deserialize-format
          buffer
          mime
          (cffi:callback text-buffer-deserialize-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'text-buffer-register-deserialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_deserialize_tagset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_register_deserialize_tagset"
               text-buffer-register-deserialize-tagset) gdk:atom-as-string
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for an optional tagset name, or @code{nil}}
  @begin{return}
    The string that corresponds to the newly registered MIME type of the format.
  @end{return}
  @begin{short}
    This function registers internal rich text serialization format of GTK with
    the passed text buffer.
  @end{short}
  See the @fun{gtk:text-buffer-register-serialize-tagset} function for details.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-tagset}"
  (buffer (g:object text-buffer))
  (name :string))

(export 'text-buffer-register-deserialize-tagset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_format
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_register_serialize_format"
               %text-buffer-register-serialize-format) gdk:atom-as-string
  (buffer (g:object text-buffer))
  (mime :string)
  (func :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun text-buffer-register-serialize-format (buffer mime func)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mime]{a string for the MIME type of the format}
  @argument[func]{a @sym{gtk:text-buffer-serialize-func} serialize function
    to register}
  @begin{return}
    The string that corresponds to the newly registered MIME type of the format.
  @end{return}
  @begin{short}
    This function registers a rich text serialization function along with its
    MIME type with the passed text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-buffer-serialize-func}"
  (%text-buffer-register-serialize-format
          buffer
          mime
          (cffi:callback text-buffer-serialize-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'text-buffer-register-serialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_register_serialize_tagset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_register_serialize_tagset"
               text-buffer-register-serialize-tagset) gdk:atom-as-string
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for an optional tagset name, or @code{nil}}
  @begin{return}
    The string that corresponds to the newly registered MIME type of the format.
  @end{return}
  @begin{short}
    This function registers the internal rich text serialization format of GTK
    with the passed text buffer.
  @end{short}
  The internal format does not comply to any standard rich text format and only
  works between @class{gtk:text-buffer} objects. It is capable of serializing
  all tags and embedded pixbufs of the text buffer.

  This function is just a wrapper around the
  @fun{gtk:text-buffer-register-serialize-format} function. The MIME type used
  for registering is \"application/x-gtk:text-buffer-rich-text\", or
  \"application/x-gtk:text-buffer-rich-text;format=tagset_name\" if a tagset
  name was passed.

  The @arg{name} argument can be used to restrict the transfer of rich text to
  text buffers with compatible sets of tags, in order to avoid unknown tags from
  being pasted. It is probably the common case to pass an identifier which is
  not @code{nil} here, since the @code{nil} tagset requires the receiving text
  buffer to deal with with pasting of arbitrary tags.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-format}"
  (buffer (g:object text-buffer))
  (name :string))

(export 'text-buffer-register-serialize-tagset)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferSerializeFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback text-buffer-serialize-func :pointer
    ((buffer (g:object text-buffer))
     (content (g:object text-buffer))
     (start (g:boxed text-iter))
     (end (g:boxed text-iter))
     (length (:pointer :size))
     (user-data :pointer))
  (let ((func (glib:get-stable-pointer-value user-data)))
    (restart-case
      (let* ((bytes (funcall func buffer content start end))
             (bytes-ptr (g:malloc (length bytes))))
        (setf (cffi:mem-ref length :size) (length bytes))
        (iter (for i from 0 below (length bytes))
              (setf (cffi:mem-aref bytes-ptr :uint8 i) (aref bytes i)))
        bytes-ptr)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-buffer-serialize-func)
      "Callback"
      (liber:symbol-documentation 'text-buffer-serialize-func)
 "@version{#2024-03-23}
  @syntax{lambda (buffer content start end) => result}
  @argument[buffer]{a @class{gtk:text-buffer} object for which the format is
    registered}
  @argument[content]{a @class{gtk:text-buffer} object to serialize}
  @argument[start]{a @class{gtk:text-iter} start iterator of the block of text
    to serialize}
  @argument[end]{a @class{gtk:text-iter} end iterator of the block of text
    to serialize}
  @argument[result]{a newly allocated array of @code{guint8} which contains the
    serialized data, or @code{nil} if an error occured}
  @begin{short}
    A function that is called to serialize the content of a text buffer. It
    must return the serialized form of the content.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-format}
  @see-function{gtk:text-buffer-serialize}")

(export 'text-buffer-serialize-func)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_serialize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_serialize" %text-buffer-serialize) :pointer
  (buffer (g:object text-buffer))
  (content (g:object text-buffer))
  (format gdk:atom-as-string)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (len (:pointer :size)))

(defun text-buffer-serialize (buffer content format start end)
 #+liber-documentation
 "@version{#2025-06-30}
  @argument[buffer]{a @class{gtk:text-buffer} object the format is registered
    with}
  @argument[content]{a @class{gtk:text-buffer} object to serialize}
  @argument[format]{a string for the rich text format to use for serializing}
  @argument[start]{a @class{gtk:text-iter} start iterator of the block of text
    to serialize}
  @argument[end]{a @class{gtk:text-iter} end iterator of the block of text to
    serialize}
  @return{The list with the serialized data, encoded as @arg{format}.}
  @begin{short}
    This function serializes the portion of text between the @arg{start} and
    @arg{end} iterator in the rich text format represented by @arg{format}.
  @end{short}
  The @arg{format} arguments to be used must be registered using the
  @fun{gtk:text-buffer-register-serialize-format} or
  @fun{gtk:text-buffer-register-serialize-tagset} functions beforehand.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-format}
  @see-function{gtk:text-buffer-register-serialize-tagset}"
  (cffi:with-foreign-object (len :size)
    (let ((bytes (%text-buffer-serialize buffer
                                         content
                                         format
                                         start
                                         end
                                         len)))
      (iter (for i from 0 to (cffi:mem-ref len :size))
            (for byte = (cffi:mem-aref bytes :uint8 i))
            (collect byte result-type vector)
            (finally (g:free bytes))))))

(export 'text-buffer-serialize)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_unregister_deserialize_format
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_unregister_deserialize_format"
               text-buffer-unregister-deserialize-format) :void
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[format]{a string representing a registered rich text format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the @fun{gtk:text-buffer-register-deserialize-format} or
    @fun{gtk:text-buffer-register-deserialize-tagset} functions.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-deserialize-format}
  @see-function{gtk:text-buffer-register-deserialize-tagset}"
  (buffer (g:object text-buffer))
  (format gdk:atom-as-string))

(export 'text-buffer-unregister-deserialize-format)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_unregister_serialize_format
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_unregister_serialize_format"
               text-buffer-unregister-serialize-format) :void
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[format]{a string representing a registered rich text format}
  @begin{short}
    This function unregisters a rich text format that was previously registered
    using the @fun{gtk:text-buffer-register-serialize-format} or
    @fun{gtk:text-buffer-register-serialize-tagset} functions.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-register-serialize-format}
  @see-function{gtk:text-buffer-register-serialize-tagset}"
  (buffer (g:object text-buffer))
  (format gdk:atom-as-string))

(export 'text-buffer-unregister-serialize-format)

;;; --- End of file gtk3.text-buffer.lisp --------------------------------------
