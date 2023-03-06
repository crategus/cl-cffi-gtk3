;;; ----------------------------------------------------------------------------
;;; gtk3.tool-shell.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkToolShell
;;;
;;;     Interface for containers containing GtkToolItem widgets
;;;
;;; Types and Values
;;;
;;;     GtkToolShell
;;;
;;; Functions
;;;
;;;     gtk_tool_shell_get_ellipsize_mode
;;;     gtk_tool_shell_get_icon_size
;;;     gtk_tool_shell_get_orientation
;;;     gtk_tool_shell_get_relief_style
;;;     gtk_tool_shell_get_style
;;;     gtk_tool_shell_get_text_alignment
;;;     gtk_tool_shell_get_text_orientation
;;;     gtk_tool_shell_rebuild_menu
;;;     gtk_tool_shell_get_text_size_group
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkToolShell
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkToolShell
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkToolShell" tool-shell
  (:export t
   :type-initializer "gtk_tool_shell_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'tool-shell)
      "Interface"
      (documentation 'tool-shell 'type)
 "@version{#2023-2-27}
  @begin{short}
    The @sym{gtk:tool-shell} interface allows container widgets to provide
    additional information when embedding @class{gtk:tool-item} widgets.
  @end{short}
  @see-class{gtk:tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_ellipsize_mode () -> tool-shell-ellipsize-mode
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_ellipsize_mode" tool-shell-ellipsize-mode)
    pango:ellipsize-mode
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @symbol{pango:ellipsize-mode} value of @arg{shell}.}
  @begin{short}
    Retrieves the current ellipsize mode for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-ellipsize-mode} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{pango:ellipsize-mode}
  @see-function{gtk:tool-item-ellipsize-mode}"
  (shell (g:object tool-shell)))

(export 'tool-shell-ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_icon_size () -> tool-shell-icon-size
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_icon_size" tool-shell-icon-size) icon-size
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @symbol{gtk:icon-size} value for icons of @arg{shell}.}
  @begin{short}
    Retrieves the icon size for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-icon-size} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:tool-item-icon-size}"
  (shell (g:object tool-shell)))

(export 'tool-shell-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_orientation () -> tool-shell-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_orientation" tool-shell-orientation) orientation
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @symbol{gtk:orientation} value of @arg{shell}.}
  @begin{short}
    Retrieves the current orientation for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-orientation} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{gtk:orientation}
  @see-function{gtk:tool-item-orientation}"
  (shell (g:object tool-shell)))

(export 'tool-shell-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_relief_style () -> tool-shell-relief-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_relief_style" tool-shell-relief-style)
    relief-style
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The @symbol{gtk:relief-style} value of buttons on @arg{shell}.}
  @begin{short}
    Returns the relief style of buttons on the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-relief-style} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{gtk:relief-style}
  @see-function{gtk:tool-item-relief-style}"
  (shell (g:object tool-shell)))

(export 'tool-shell-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_style () -> tool-shell-style
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_style" tool-shell-style) toolbar-style
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @symbol{gtk:toolbar-style} value of @arg{shell}.}
  @begin{short}
    Retrieves whether the tool shell has text, icons, or both.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-toolbar-style} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{gtk:toolbar-style}
  @see-function{gtk:tool-item-toolbar-style}"
  (shell (g:object tool-shell)))

(export 'tool-shell-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_alignment () -> tool-shell-text-alignment
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_alignment" tool-shell-text-alignment) :float
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{A float with the current text alignment of @arg{shell}.}
  @begin{short}
    Retrieves the current text alignment for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-text-alignment} function instead.
  @see-class{gtk:tool-shell}
  @see-function{gtk:tool-item-text-alignment}"
  (shell (g:object tool-shell)))

(export 'tool-shell-text-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_orientation () -> tool-shell-text-orientation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_orientation" tool-shell-text-orientation)
    orientation
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @symbol{gtk:orientation} value of @arg{shell}.}
  @begin{short}
    Retrieves the current text orientation for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-text-orientation} function instead.
  @see-class{gtk:tool-shell}
  @see-symbol{gtk:orientation}
  @see-function{gtk:tool-item-text-orientation}"
  (shell (g:object tool-shell)))

(export 'tool-shell-text-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_rebuild_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_rebuild_menu" tool-shell-rebuild-menu) :void
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @begin{short}
    Calling this function signals the tool shell that the overflow menu item
    for tool items have changed.
  @end{short}
  If there is an overflow menu and if it is visible when this function it
  called, the menu will be rebuilt.

  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-rebuild-menu} function instead.
  @see-class{gtk:tool-shell}
  @see-function{gtk:tool-item-rebuild-menu}"
  (shell (g:object tool-shell)))

(export 'tool-shell-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_shell_get_text_size_group () -> tool-shell-text-size-group
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_shell_get_text_size_group" tool-shell-text-size-group)
    (g:object size-group)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[shell]{a @class{gtk:tool-shell} widget}
  @return{The current @class{gtk:size-group} object of @arg{shell}.}
  @begin{short}
    Retrieves the current text size group for the tool shell.
  @end{short}
  Tool items must not call this function directly, but rely on the
  @fun{gtk:tool-item-text-size-group} function instead.
  @see-class{gtk:tool-shell}
  @see-class{gtk:size-group}
  @see-function{gtk:tool-item-get-text-size-group}"
  (shell (g:object tool-shell)))

(export 'tool-shell-text-size-group)

;;; --- End of file gtk3.tool-shell.lisp ---------------------------------------
