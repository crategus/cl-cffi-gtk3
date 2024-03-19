;;; ----------------------------------------------------------------------------
;;; gtk3.im-context-simple.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkIMContextSimple
;;;
;;;     An input method context supporting table-based input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMContextSimple
;;;
;;;     GTK_MAX_COMPOSE_LEN
;;;
;;;
;;; Functions
;;;
;;;     gtk_im_context_simple_new
;;;     gtk_im_context_simple_add_table
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_MAX_COMPOSE_LEN
;;;
;;; #define GTK_MAX_COMPOSE_LEN 7
;;;
;;; The maximum length of sequences in compose tables.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkIMContextSimple" im-context-simple
  (:superclass im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_context_simple_get_type")
  nil)

#+liber-documentation
(setf (documentation 'im-context-simple 'type)
 "@version{#2023-2-28}
  @begin{short}
    The @class{gtk:im-context-simple} object is a simple input method context
    supporting table-based input methods.
  @end{short}
  It has a built-in table of compose sequences that is derived from the X11
  Compose files.

  The @class{gtk:im-context-simple} object reads additional compose sequences
  from the first of the following files that is found:
  @file{~/.config/gtk-3.0/Compose}, @file{~/.XCompose},
  @file{/usr/share/X11/locale/$locale/Compose}, for locales that have a
  nontrivial Compose file. The syntax of these files is described in the
  Compose(5) manual page.

  @subheading{Unicode characters}
  The @class{gtk:im-context-simple} class also supports numeric entry of Unicode
  characters by typing the @kbd{Ctrl-Shift-u} key, followed by a hexadecimal
  Unicode codepoint. For example, @kbd{Ctrl-Shift-u 1 2 3 Enter} yields U+0123
  LATIN SMALL LETTER G WITH CEDILLA, i.e. ģ.
  @see-class{gtk:im-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_new ()
;;;
;;; GtkIMContext * gtk_im_context_simple_new (void);
;;;
;;; Creates a new GtkIMContextSimple.
;;;
;;; Returns :
;;;     a new GtkIMContextSimple.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_add_table ()
;;;
;;; void
;;; gtk_im_context_simple_add_table (GtkIMContextSimple *context_simple,
;;;                                  guint16 *data,
;;;                                  gint max_seq_len,
;;;                                  gint n_seqs)
;;;
;;; Adds an additional table to search to the input context. Each row of the
;;; table consists of max_seq_len key symbols followed by two guint16
;;; interpreted as the high and low words of a gunicode value. Tables are
;;; searched starting from the last added.
;;;
;;; The table must be sorted in dictionary order on the numeric value of the key
;;; symbol fields. (Values beyond the length of the sequence should be zero.)
;;;
;;; context_simple :
;;;     A GtkIMContextSimple
;;;
;;; data :
;;;     the table
;;;
;;; max_seq_len :
;;;     Maximum length of a sequence in the table (cannot be greater than
;;;     GTK_MAX_COMPOSE_LEN)
;;;
;;; n_seqs :
;;;     number of sequences in the table
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.im-context-simple.lisp --------------------------------
