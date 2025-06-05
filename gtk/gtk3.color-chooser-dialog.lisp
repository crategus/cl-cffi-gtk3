;;; ----------------------------------------------------------------------------
;;; gtk3.color-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkColorChooserDialog
;;;
;;;     A dialog for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_dialog_new
;;;
;;; Properties
;;;
;;;     show-editor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkColorChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorChooserDialog implements AtkImplementorIface, GtkBuildable and
;;;     GtkColorChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorChooserDialog" color-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_dialog_get_type")
  ((show-editor
    color-chooser-dialog-show-editor
    "show-editor" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'color-chooser-dialog 'type)
 "@version{2023-6-15}
  @begin{short}
    The @class{gtk:color-chooser-dialog} widget is a dialog for choosing a
    color.
  @end{short}
  It implements the @class{gtk:color-chooser} interface.

  @image[colorchooser]{GtkColorChooser}
  @begin[Examples]{dictionary}
    Clicking on the drawing area opens a color chooser dialog to select a
    background color for the drawing area. The default palettes are replaced
    for this color chooser dialog.
    @begin{pre}
(let ((message \"Click to change the background color.\")
      (bg-color (gdk:rgba-parse \"White\"))
      ;; Color palette with 9 Red RGBA colors
      (palette1 (list (gdk:rgba-parse \"IndianRed\")
                      (gdk:rgba-parse \"LightCoral\")
                      (gdk:rgba-parse \"Salmon\")
                      (gdk:rgba-parse \"DarkSalmon\")
                      (gdk:rgba-parse \"LightSalmon\")
                      (gdk:rgba-parse \"Crimson\")
                      (gdk:rgba-parse \"Red\")
                      (gdk:rgba-parse \"FireBrick\")
                      (gdk:rgba-parse \"DarkRed\")))
      ;; Gray palette with 9 gray RGBA colors
      (palette2 (list (gdk:rgba-parse \"Gainsboro\")
                      (gdk:rgba-parse \"LightGray\")
                      (gdk:rgba-parse \"Silver\")
                      (gdk:rgba-parse \"DarkGray\")
                      (gdk:rgba-parse \"Gray\")
                      (gdk:rgba-parse \"DimGray\")
                      (gdk:rgba-parse \"LightSlateGray\")
                      (gdk:rgba-parse \"SlateGray\")
                      (gdk:rgba-parse \"DarkSlateGray\"))))

  (defun example-color-chooser-dialog (&optional application)
    (gtk:within-main-loop
      (let ((window (make-instance 'gtk:window
                                   :title \"Example Color Chooser Dialog\"
                                   :type :toplevel
                                   :application application
                                   :default-width 400))
            (area (make-instance 'gtk:drawing-area)))
        (g:signal-connect window \"destroy\"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        ;; Draw the background color and a hint on the drawing area
        (g:signal-connect area \"draw\"
            (lambda (widget cr)
              (declare (ignore widget))
              (let ((red (gdk:rgba-red bg-color))
                    (green (gdk:rgba-green bg-color))
                    (blue (gdk:rgba-blue bg-color)))
                    ;; Paint the current color on the drawing area
                    (cairo:set-source-rgb cr red green blue)
                    (cairo:paint cr)
                    ;; Print a hint on the drawing area
                    (cairo:set-source-rgb cr (- 1 red)
                                             (- 1 green) (- 1 blue))
                    (cairo:select-font-face cr \"Sans\")
                    (cairo:set-font-size cr 12)
                    (cairo:move-to cr 12 24)
                    (cairo:show-text cr message))))
        ;; Create and run a color chooser dialog to select a background color
        (g:signal-connect area \"event\"
            (lambda (widget event)
              (declare (ignore widget))
              (when (eq (gdk:event-type event) :button-press)
                (let ((dialog (make-instance 'gtk:color-chooser-dialog
                                             :use-alpha nil)))
                  ;; Add a custom palette to the dialog
                  (gtk:color-chooser-add-palette dialog :vertical 1 palette1)
                  ;; Add a second coustom palette to the dialog
                  (gtk:color-chooser-add-palette dialog :vertical 1 palette2)
                  ;; Set the actual background color for the color chooser
                  (setf (gtk:color-chooser-rgba dialog) bg-color)
                  ;; Run the color chooser dialog
                  (let ((response (gtk:dialog-run dialog)))
                    (when (eq response :ok)
                      ;; Change the background color for the drawing area
                      (setf bg-color (gtk:color-chooser-rgba dialog)))
                      ;; Destroy the color chooser dialog
                      (gtk:widget-destroy dialog))))))
        ;; Set the event mask for the drawing area
        (setf (gtk:widget-events area) :button-press-mask)
        ;; Add the drawing area to the window
        (gtk:container-add window area)
        (gtk:widget-show-all window)))))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:color-chooser-dialog-new}
  @see-slot{gtk:color-chooser-dialog-show-editor}
  @see-class{gtk:color-chooser}
  @see-class{gtk:color-button}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-editor"
                                               'color-chooser-dialog) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} when the color chooser dialog is showing the single-color editor.
  It can be set to switch the color chooser into single-color editing mode.@br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-dialog-show-editor)
      "Accessor"
      (documentation 'color-chooser-dialog-show-editor 'function)
 "@version{2023-6-14}
  @syntax{(gtk:color-chooser-dialog-show-editor object) => show-editor}
  @syntax{(setf (gtk:color-chooser-dialog-show-editor object) show-editor)}
  @argument[object]{a @class{gtk:color-chooser-dialog} widget}
  @argument[show-editor]{a boolean whether to show the single-color editor}
  @begin{short}
    Accessor of the @slot[gtk:color-chooser-dialog]{show-editor} slot of the
    @class{gtk:color-chooser-dialog} class.
  @end{short}
  @em{True} when the color chooser dialog is showing the single-color editor.
  It can be set to switch the color chooser into single-color editing mode.
  @see-class{gtk:color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(defun color-chooser-dialog-new (title parent)
 #+liber-documentation
 "@version{2024-12-29}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog,
    or @code{nil}}
  @return{The new @class{gtk:color-chooser-dialog} widget.}
  @short{Creates a new color chooser dialog.}
  @see-class{gtk:window}
  @see-class{gtk:color-chooser-dialog}"
  (if parent
      (make-instance 'color-chooser-dialog
                     :title (or title (cffi:null-pointer))
                     :parent parent)
      (make-instance 'color-chooser-dialog
                     :title (or title (cffi:null-pointer)))))

(export 'color-chooser-dialog-new)

;;; --- End of file gtk3.color-chooser-dialog.lisp -----------------------------
