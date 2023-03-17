;;; ----------------------------------------------------------------------------
;;; gtk3.font-selection-dialog.lisp
;;;
;;; The documentation has been copied from the GTK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
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
;;; GtkFontSelectionDialog
;;;
;;;     Deprecated dialog box for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontSelectionDialog
;;;
;;;     gtk_font_selection_dialog_new
;;;     gtk_font_selection_dialog_get_font_name
;;;     gtk_font_selection_dialog_set_font_name
;;;     gtk_font_selection_dialog_get_preview_text
;;;     gtk_font_selection_dialog_set_preview_text
;;;     gtk_font_selection_dialog_get_cancel_button
;;;     gtk_font_selection_dialog_get_ok_button
;;;     gtk_font_selection_dialog_get_font_selection
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontSelectionDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontSelectionDialog" font-selection-dialog
  (:superclass dialog
   :export nil
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_font_selection_dialog_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation 'font-selection-dialog 'type)
 "@version{#2013-6-18}
  @begin{short}
    The @class{gtk:font-selection-dialog} widget is a dialog box for selecting
    a font.
  @end{short}

  To set the font which is initially selected, use the
  @fun{gtk:font-selection-dialog-set-font-name} function.

  To get the selected font use the @fun{gtk:font-selection-dialog-get-font-name}
  function.

  To change the text which is shown in the preview area, use the
  @fun{gtk:font-selection-dialog-set-preview-text} function.

  In GTK 3.2, the @sym{gtk:font-selection-dialog} widget has been deprecated in
  favor of the @class{gtk:font-chooser-dialog} widget.

  @subheading{GtkFontSelectionDialog as GtkBuildable}
    The @sym{gtk:font-selection-dialog} implementation of the
    @class{gtk:buildable} interface exposes the embedded
    @class{gtk:font-selection} as internal child with the name
    \"font_selection\". It also exposes the buttons with the names
    \"ok_button\", \"cancel_button\" and \"apply_button\".")

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_new ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_new (const gchar *title);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_new has been deprecated since version 3.2 and
;;; should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Creates a new GtkFontSelectionDialog.
;;;
;;; title :
;;;     the title of the dialog
;;;
;;; Returns :
;;;     a new GtkFontSelectionDialog
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_font_name ()
;;;
;;; gchar * gtk_font_selection_dialog_get_font_name
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_font_name has been deprecated since version
;;; 3.2 and should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Gets the currently selected font name.
;;;
;;; Note that this can be a different string than what you set with
;;; gtk_font_selection_dialog_set_font_name(), as the font selection widget may
;;; normalize font names and thus return a string with a different structure.
;;; For example, "Helvetica Italic Bold 12" could be normalized to "Helvetica
;;; Bold Italic 12". Use pango_font_description_equal() if you want to compare
;;; two font descriptions.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     A string with the name of the current font, or NULL if no font is
;;;     selected. You must free this string with g_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_set_font_name ()
;;;
;;; gboolean gtk_font_selection_dialog_set_font_name
;;;                                                (GtkFontSelectionDialog *fsd,
;;;                                                 const gchar *fontname);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_set_font_name has been deprecated since version
;;; 3.2 and should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Sets the currently selected font.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; fontname :
;;;     a font name like "Helvetica 12" or "Times Bold 18"
;;;
;;; Returns :
;;;     TRUE if the font selected in fsd is now the fontname specified, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_preview_text ()
;;;
;;; const gchar * gtk_font_selection_dialog_get_preview_text
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_preview_text has been deprecated since version
;;; 3.2 and should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Gets the text displayed in the preview area.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the text displayed in the preview area. This string is owned by the
;;;     widget and should not be modified or freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_set_preview_text ()
;;;
;;; void gtk_font_selection_dialog_set_preview_text
;;;                                                (GtkFontSelectionDialog *fsd,
;;;                                                 const gchar *text);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_set_preview_text has been deprecated since version
;;; 3.2 and should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Sets the text displayed in the preview area.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; text :
;;;     the text to display in the preview area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_cancel_button ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_cancel_button
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_cancel_button has been deprecated since
;;; version 3.2 and should not be used in newly written code. Use
;;; GtkFontChooserDialog
;;;
;;; Gets the 'Cancel' button.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the GtkWidget used in the dialog for the 'Cancel' button
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_ok_button ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_ok_button
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_ok_button has been deprecated since version
;;; 3.2 and should not be used in newly written code. Use GtkFontChooserDialog
;;;
;;; Gets the 'OK' button.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the GtkWidget used in the dialog for the 'OK' button
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_dialog_get_font_selection ()
;;;
;;; GtkWidget * gtk_font_selection_dialog_get_font_selection
;;;                                               (GtkFontSelectionDialog *fsd);
;;;
;;; Warning
;;;
;;; gtk_font_selection_dialog_get_font_selection has been deprecated since
;;; version 3.2 and should not be used in newly written code. Use
;;; GtkFontChooserDialog
;;;
;;; Retrieves the GtkFontSelection widget embedded in the dialog.
;;;
;;; fsd :
;;;     a GtkFontSelectionDialog
;;;
;;; Returns :
;;;     the embedded GtkFontSelection
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.font-selection-dialog.lisp ----------------------------
