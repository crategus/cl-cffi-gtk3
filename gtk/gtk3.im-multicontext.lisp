;;; ----------------------------------------------------------------------------
;;; gtk3.im-multicontext.lisp
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
;;; GtkIMMulticontext
;;;
;;;     An input method context supporting multiple, loadable input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMMulticontext
;;;
;;; Functions
;;;
;;;     gtk_im_multicontext_new
;;;     gtk_im_multicontext_append_menuitems
;;;     gtk_im_multicontext_get_context_id
;;;     gtk_im_multicontext_set_context_id
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIMMulticontext" im-multicontext
  (:superclass im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_multicontext_get_type")
  nil)

#+liber-documentation
(setf (documentation 'im-multicontext 'type)
 "@version{#2023-2-28}
  @begin{short}
    An input method context supporting multiple, loadable input methods.
  @end{short}
  @see-class{gtk:im-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_new ()
;;;
;;; GtkIMContext * gtk_im_multicontext_new (void);
;;;
;;; Creates a new GtkIMMulticontext.
;;;
;;; Returns :
;;;     a new GtkIMMulticontext.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_append_menuitems ()
;;;
;;; void gtk_im_multicontext_append_menuitems (GtkIMMulticontext *context,
;;;                                            GtkMenuShell *menushell);
;;;
;;; Add menuitems for various available input methods to a menu; the menuitems,
;;; when selected, will switch the input method for the context and the global
;;; default input method.
;;;
;;; Warning
;;;
;;; gtk_im_multicontext_append_menuitems has been deprecated since version 3.10
;;; and should not be used in newly written code.
;;;
;;; It is better to use the system-wide input method framework for changing
;;; input methods. Modern desktop shells offer on-screen displays for this that
;;; can triggered with a keyboard shortcut, e.g. Super-Space.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; menushell :
;;;     a GtkMenuShell
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_get_context_id ()
;;;
;;; const char * gtk_im_multicontext_get_context_id (GtkIMMulticontext *context)
;;;
;;; Gets the id of the currently active slave of the context.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; Returns :
;;;     the id of the currently active slave
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_set_context_id ()
;;;
;;; void gtk_im_multicontext_set_context_id (GtkIMMulticontext *context,
;;;                                          const char *context_id);
;;;
;;; Sets the context id for context.
;;;
;;; This causes the currently active slave of context to be replaced by the
;;; slave corresponding to the new context id.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; context_id :
;;;     the id to use
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.im-multicontext.lisp ----------------------------------
