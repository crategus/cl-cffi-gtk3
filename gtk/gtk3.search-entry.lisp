;;; ----------------------------------------------------------------------------
;;; gtk3.seach-entry.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; GtkSearchEntry
;;;
;;;     An entry which shows a search icon
;;;
;;; Types and Values
;;;
;;;     GtkSearchEntry
;;;
;;; Functions
;;;
;;;     gtk_search_entry_new
;;;     gtk_search_entry_handle_event
;;;
;;; Signals
;;;
;;;     next-match
;;;     previous-match
;;;     search-changed
;;;     stop-search
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEntry
;;;                 ╰── GtkSearchEntry
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSearchEntry implements AtkImplementorIface, GtkBuildable, GtkEditable
;;;     and GtkCellEditable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSearchEntry
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSearchEntry" search-entry
  (:superclass entry
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkEditable"
                "GtkCellEditable")
   :type-initializer "gtk_search_entry_get_type")
  nil)

#+liber-documentation
(setf (documentation 'search-entry 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @sym{gtk:search-entry} class is a subclass of the @class{gtk:entry}
    class that has been tailored for use as a search entry.
  @end{short}

  @image[search-entry]{Figure: GtkSearchEntry}

  It will show an inactive symbolic \"find\" icon when the search entry is
  empty, and a symbolic \"clear\" icon when there is text. Clicking on the
  \"clear\" icon will empty the search entry.

  Note that the search/clear icon is shown using a secondary icon, and thus
  does not work if you are using the secondary icon position for some other
  purpose.

  To make filtering appear more reactive, it is a good idea to not react to
  every change in the entry text immediately, but only after a short delay.
  To support this, the @sym{gtk:search-entry} widget emits the
  \"search-changed\" signal which can be used instead of the \"changed\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"next-match\" signal}
      @begin{pre}
 lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the next match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-g} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:search-entry} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"previous-match\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the previous match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-Shift-g} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:search-entry} widget on which the signal was
          emitted.}
        @end{table}
    @subheading{The \"search-changed\" signal}
      @begin{pre}
lambda (entry)    :run-last
      @end{pre}
      The signal is emitted with a short delay of 150 milliseconds after the
      last change to the entry text.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:search-entry} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"stop-search\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user stops
      a search via keyboard input. Applications should connect to it, to
      implement hiding the search entry in this case. The default bindings for
      this signal is the @kbd{Escape} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:search-entry} widget on which the signal was
          emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline search-entry-new))

(defun search-entry-new ()
 #+liber-documentation
 "@version{#2023-3-24}
  @return{A new @class{gtk:search-entry} widget.}
  @begin{short}
    Creates a search entry, with a find icon when the search field is empty,
    and a clear icon when it is not.
  @end{short}
  @see-class{gtk:search-entry}"
  (make-instance 'search-entry))

(export 'search-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_handle_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_search_entry_handle_event" search-entry-handle-event) :boolean
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[entry]{a @class{gtk:entry-search} widget}
  @argument[event]{a key event of type @class{gdk:event}}
  @return{The @var{+gdk-event-stop+} value if the key press event resulted in a
    search beginning or continuing, the @var{+gdk-event-propagate+} value
    otherwise.}
  @begin{short}
    This function should be called when the toplevel window which contains the
    search entry received a key event.
  @end{short}
  If the entry is part of a search bar, it is preferable to call the
  @fun{gtk:search-bar-handle-event} function instead, which will reveal the
  entry in addition to passing the event to this function.

  If the key event is handled by the search entry and starts or continues a
  search, the @var{+gdk-event-stop+} value will be returned. The caller should
  ensure that the entry is shown in this case, and not propagate the event
  further.
  @see-class{gtk:search-entry}"
  (entry (g:object search-entry))
  (event (g:boxed gdk:event)))

(export 'search-entry-handle-event)

;;; --- End of file gtk3.search-entry.lisp -------------------------------------
