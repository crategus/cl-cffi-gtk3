;;; ----------------------------------------------------------------------------
;;; gtk3-demo.lisp
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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

(defpackage :gtk3-demo
  (:use :common-lisp)
  (:export :gtk3-demo))

(in-package :gtk3-demo)

;;; ----------------------------------------------------------------------------

(defvar info-buffer (make-instance 'gtk:text-buffer))
(defvar source-buffer (make-instance 'gtk:text-buffer))
(defvar ui-buffer (make-instance 'gtk:text-buffer))
(defvar css-buffer (make-instance 'gtk:text-buffer))

;;; ----------------------------------------------------------------------------

;; Get the pathname for a file
(defun sys-path (filename &optional (system :gtk3-demo))
  (asdf:system-relative-pathname system filename))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file (filename)
  (with-open-file (stream filename)
    ;; Read the info-header of the file
    (clear-buffer info-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((or (null line)
             (not (>= (length line) 4))
             (not (string= line ";;;;" :start1 0 :end1 4))))
      (gtk:text-buffer-insert info-buffer (string-left-trim ";" line))
      (gtk:text-buffer-insert info-buffer (format nil "~%")))
    ;; Read the source code of the file
    (clear-buffer source-buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert source-buffer line)
      (gtk:text-buffer-insert source-buffer (format nil "~%")))))

(defun load-file-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer line)
      (gtk:text-buffer-insert buffer (format nil "~%")))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;;; ----------------------------------------------------------------------------

(defun create-text (buffer is-source)
  (let* ((scrolled (make-instance 'gtk:scrolled-window
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
         (view (make-instance 'gtk:text-view
                              :buffer buffer
                              :editable nil
                              :cursor-visible nil)))
    (gtk:container-add scrolled view)
    (when is-source
      (gtk:widget-override-font view
                                (pango:font-description-from-string "monospace")))
    ;; return the scrolled window
    scrolled))

;;; ----------------------------------------------------------------------------

(defparameter coltitle 0)
(defparameter coltype 1)
(defparameter colfunc 2)
(defparameter colpackage 3)
(defparameter colfile 4)
(defparameter colui 5)
(defparameter colcss 6)

(defparameter *tree-model*
   '(("gchararray"        ; Title
      "gchararray"        ; Type
      "gchararray"        ; Function name
      "gchararray"        ; Package
      "gchararray"        ; Filename
      "gchararray"        ; UI Definition
      "gchararray")       ; CSS Definition

     "Theming in GTK"
     (("CSS Accordion"
       ":WINDOW"
       "example-css-accordion"
       "gtk3-example"
       "css-accordion.lisp"
       ""
       "resource/css-accordion.css")
      ("CSS Basics"
       ":window"
       "example-css-basics"
       "gtk3-example"
       "css-basics.lisp"
       ""
       "resource/css-basics.css")
      ("CSS Blend Modes"
       ":window"
       "example-css-blendmodes"
       "gtk3-example"
       "css-blendmodes.lisp"
       "resource/css-blendmodes.ui"
       "resource/css-blendmodes.css")
      ("CSS Multiple Backgrounds"
       ":window"
       "example-css-multiplebgs"
       "gtk3-example"
       "css-multiplebgs.lisp"
       ""
       "resource/css-multiplebgs.css")
      ("CSS Pixbufs"
       ":window"
       "example-css-pixbufs"
       "gtk3-example"
       "css-pixbufs.lisp"
       ""
       "resource/css-pixbufs.css")
      ("CSS Shadows"
       ":window"
       "example-css-shadows"
       "gtk3-example"
       "css-shadows.lisp"
       ""
       "resource/css-shadows.css")
      ("Custom Drawing"
       ":window"
       "example-custom-drawing"
       "gtk3-example"
       "custom-drawing.lisp"))

     "Windows"
     (("Simple Window"
       ":window"
       "example-window-simple-demo"
       "gtk3-example"
       "window-simple-demo.lisp")
      ("Message Dialog"
       ":window"
       "example-message-dialog-simple"
       "gtk3-example"
       "message-dialog-simple.lisp")
      ("Dialog Windows"
       ":window"
       "example-dialogs"
       "gtk3-example"
       "dialog.lisp")
      ("Assistant"
       ":window"
       "example-assistant"
       "gtk3-example"
       "assistant.lisp"))

     "Layout Containers"
     (("Simple Box"
       ":window"
       "example-box-simple"
       "gtk3-example"
       "box-simple.lisp")
      ("Box packing"
       ":window"
       "example-box-packing"
       "gtk3-example"
       "box-packing.lisp")
      ("Simple Grid"
       ":window"
       "example-grid-simple"
       "gtk3-example"
       "grid-simple.lisp")
      ("Grid with spacing"
       ":window"
       "example-grid-spacing"
       "gtk3-example"
       "grid-spacing.lisp")
      ("Grid packing"
       ":window"
       "example-grid-packing"
       "gtk3-example"
       "grid-packing.lisp")
      ("Revealer"
       ":window"
       "example-revealer"
       "gtk3-example"
       "revealer.lisp")
      ("Revealer Icon"
       ":window"
       "example-revealer-icon"
       "gtk3-example"
       "revealer-icon.lisp"
       "resource/revealer-icon.ui")
      ("List Box"
       ":window"
       "example-list-box"
       "gtk3-example"
       "list-box.lisp"
       "resource/list-box.ui")
      ("Flow Box"
       ":window"
       "example-flow-box"
       "gtk3-example"
       "flow-box.lisp")
      ("Stack"
       ":window"
       "example-stack"
       "gtk3-example"
       "stack.lisp"
       "resource/stack.ui")
      ("Stack Sidebar"
       ":window"
       "example-stack-sidebar"
       "gtk3-example"
       "stack-sidebar.lisp")
      ("Action Bar"
       ":window"
       "example-action-bar"
       "gtk3-example"
       "action-bar.lisp")
      ("Header Bar"
       ":window"
       "example-header-bar"
       "gtk3-example"
       "header-bar.lisp")
      ("Overlay Interactive"
       ":window"
       "example-overlay-interactive"
       "gtk3-example"
       "overlay-interactive.lisp")
      ("Overlay Decorative"
       ":window"
       "example-overlay-decorative"
       "gtk3-example"
       "overlay-decorative.lisp")
      ("Button Boxes"
       ":window"
       "example-button-box"
       "gtk3-example"
       "button-box.lisp")
      ("Paned Window"
       ":window"
       "example-paned-window"
       "gtk3-example"
       "paned-window.lisp")
      ("Notebook"
       ":window"
       "example-notebook"
       "gtk3-example"
       "notebook.lisp")
      ("Expander"
       ":dialog"
       "create-expander-dialog"
       "gtk3-example"
       "expander.lisp")
      ("Aspect Frame"
       ":window"
       "example-aspect-frame"
       "gtk3-example"
       "aspect-frame.lisp")
      ("Fixed Container"
       ":window"
       "example-fixed"
       "gtk3-example"
       "fixed.lisp"))

     ;; Display Widgets
     "Display Widgets"
     (("Labels"
       ":window"
       "example-label"
       "gtk3-example"
       "label.lisp")
      ("More Labels"
       ":window"
       "example-label-more"
       "gtk3-example"
       "label-more.lisp")
      ("Images"
       ":window"
       "example-image"
       "gtk3-example"
       "image.lisp")
      ("Spinner"
       ":window"
       "example-spinner"
       "gtk3-example"
       "spinner.lisp")
      ("Info Bar"
       ":window"
       "example-info-bar"
       "gtk3-example"
       "info-bar.lisp")
      ("Progress Bar"
       ":window"
       "example-progress-bar"
       "gtk3-example"
       "progress-bar.lisp")
      ("Level Bar"
       ":window"
       "example-level-bar"
       "gtk3-example"
       "level-bar.lisp")
      ("Statusbar"
       ":window"
       "example-statusbar"
       "gtk3-example"
       "statusbar.lisp"))

     "Button and Toggle Widgets"
     (("Simple Button"
       ":window"
       "example-button-image"
       "gtk3-example"
       "button-image.lisp")
      ("More Buttons"
       ":window"
       "example-button-more"
       "gtk3-example"
       "button-more.lisp")
      ("Toggle Buttons"
       ":window"
       "example-toggle-buttons"
       "gtk3-example"
       "toggle-buttons.lisp")
      ("Link Button"
       ":window"
       "example-link-button"
       "gtk3-example"
       "link-button.lisp")
      ("Switch"
       ":window"
       "example-switch"
       "gtk3-example"
       "switch.lisp")
      ("Scale Button"
       ":window"
       "example-scale-button"
       "gtk3-example"
       "scale-button.lisp"))

     "Numeric/Text Data Entry"
     (("Text Entry"
       ":window"
       "example-text-entry"
       "gtk3-example"
       "text-entry.lisp")
      ("Text Entry Buffer"
       ":window"
       "example-text-entry-buffer"
       "gtk3-example"
       "text-entry-buffer.lisp")
      ("Text Entry Completion"
       ":window"
       "example-text-entry-completion"
       "gtk3-example"
       "text-entry-completion.lisp")
      ("Scale Widget"
       ":window"
       "example-scale-widget"
       "gtk3-example"
       "scale-widget.lisp")
      ("Spin Button"
       ":window"
       "example-spin-button"
       "gtk3-example"
       "spin-button.lisp"))

     "Multiline Text Editor"
     (("Simple Text View"
       ":window"
       "example-text-view-simple"
       "gtk3-example"
       "text-view-simple.lisp")
      ("Text View Attributes"
       ":window"
       "example-text-view-attributes"
       "gtk3-example"
       "text-view-attributes.lisp")
      ("Text View Tags"
       ":window"
       "example-text-view-tags"
       "gtk3-example"
       "text-view-tags.lisp")
      ("Text View Search"
       ":window"
       "example-text-view-find-next"
       "gtk3-example"
       "text-view-find-next.lisp")
      ("Text View Insert"
       ":window"
       "example-text-view-insert"
       "gtk3-example"
       "text-view-insert.lisp")
      ("Text View Insert Image"
       ":window"
       "example-text-view-insert-image"
       "gtk3-example"
       "text-view-insert-image.lisp")
      ("Text View Insert Widget"
       ":window"
       "example-text-view-insert-widget"
       "gtk3-example"
       "text-view-insert-widget.lisp")
      ("Text View Tooltip"
       ":window"
       "example-text-view-tooltip"
       "gtk3-example"
       "text-view-tooltip.lisp"))

     "Tree, List and Icon Grid Widgets"
     (("Simple Tree View"
       ":window"
       "example-tree-view-simple"
       "gtk3-example"
       "tree-view-simple.lisp")
      ("Tree View Path"
       ":window"
       "example-tree-view-path"
       "gtk3-example"
       "tree-view-path.lisp")
      ("Tree View Content Type"
       ":window"
       "example-tree-view-content-type"
       "gtk3-example"
       "tree-view-content-type.lisp")
      ("Icon View"
       ":window"
       "example-icon-view"
       "gtk3-example"
       "icon-view.lisp"))

     "Menus, Combo Box, Toolbar"
     (("Combo Box"
       ":window"
       "example-combo-box"
       "gtk3-example"
       "combo-box.lisp")
      ("Combo Box Text"
       ":window"
       "example-combo-box-text"
       "gtk3-example"
       "combo-box-text.lisp")
      ("Menu"
       ":window"
       "example-menu"
       "gtk3-example"
       "menu.lisp")
      ("Menu Item with Image and Accel"
       ":window"
       "example-menu-item"
       "gtk3-example"
       "menu-item.lisp")
      ("Menu Popup"
       ":window"
       "example-menu-popup"
       "gtk3-example"
       "menu-popup.lisp")
      ("Tool Palette"
       ":window"
       "example-tool-palette"
       "gtk3-example"
       "tool-palette.lisp")
      ("Popover"
       ":window"
       "example-popover"
       "gtk3-example"
       "popover.lisp"
       "resource/popover.ui"))

     "Selectors (Color/File/Font)"
     (("Color Button"
       ":window"
       "example-color-button"
       "gtk3-example"
       "color-button.lisp")
      ("Color Button Label"
       ":window"
       "example-color-button-label"
       "gtk3-example"
       "color-button-label.lisp")
      ("Color Chooser Widget"
       ":window"
       "example-color-chooser-widget"
       "gtk3-example"
       "color-chooser-widget.lisp")
      ("Color Chooser Dialog"
       ":window"
       "example-color-chooser-dialog"
       "gtk3-example"
       "color-chooser-dialog.lisp")
      ("Color Chooser Palette"
       ":window"
       "example-color-chooser-palette"
       "gtk3-example"
       "color-chooser-palette.lisp")
      ("File Chooser Button"
       ":window"
       "example-file-chooser-button"
       "gtk3-example"
       "file-chooser-button.lisp")
      ("File Chooser Dialog"
       ":dialog"
       "create-file-chooser-dialog"
       "gtk3-example"
       "file-chooser-dialog.lisp")
      ("File Chooser Preview"
       ":dialog"
       "create-file-chooser-preview"
       "gtk3-example"
       "file-chooser-preview.lisp")
      ("File Chooser Widget"
       ":dialog"
       "create-file-chooser-widget"
       "gtk3-example"
       "file-chooser-widget.lisp")
      ("File Chooser Custom Filter"
       ":dialog"
       "create-file-chooser-custom-filter"
       "gtk3-example"
       "file-chooser-custom-filter.lisp")
      ("Font Button"
       ":window"
       "example-font-button"
       "gtk3-example"
       "font-button.lisp")
      ("Font Button Label"
       ":window"
       "example-font-button-label"
       "gtk3-example"
       "font-button-label.lisp"))

     ;; Ornaments
     "Ornaments"
     (("Frame Widget"
       ":window"
       "example-frame"
       "gtk3-example"
       "frame.lisp")
      ("Frame Properties"
       ":window"
       "example-frame-properties"
       "gtk3-example"
       "frame-properties.lisp"))

     ;; Scrolling
     "Scrolling"
     (("Scrolled Window"
       ":window"
       "example-scrolled-window"
       "gtk3-example"
       "scrolled-window.lisp"))

     ;; Printing
     "Printing"
     (("Page Setup Dialog"
       ":dialog"
       "create-page-setup-dialog"
       "gtk3-example"
       "page-setup-dialog.lisp")
      ("Print Dialog"
       ":dialog"
       "create-print-dialog"
       "gtk3-example"
       "print-dialog.lisp")
      #+nil
      ("Print Operation"
       ":window"
       "do-print-operation"
       "gtk3-example"
       "print-operation.lisp"))

     ;; Drag and Drop, Clipboard
     "Drag and Drop, Clipboard"
     (("Drag and Drop Simple"
       ":window"
       "example-drag-and-drop-simple"
       "gtk3-example"
       "drag-and-drop-simple.lisp")
      ("Drag and Drop with Action"
       ":window"
       "example-drag-and-drop-action"
       "gtk3-example"
       "drag-and-drop-action.lisp")
      ("Clipboard"
       ":window"
       "example-clipboard"
       "gtk3-example"
       "clipboard.lisp"))

     ;; Pango demos
     "Pango"
     (("Draw centered text"
       ":drawfunc"
       "pango-draw-text-centered"
       "pango-example"
       "text-centered.lisp")
      ("Draw text metrics"
       ":drawfunc"
       "pango-draw-text-metrics"
       "pango-example"
       "text-metrics.lisp")
      ("Draw text soulmate"
       ":drawfunc"
       "pango-draw-text-soulmate"
       "pango-example"
       "text-soulmate.lisp")
      ("Pango Cario rendering"
       ":drawfunc"
       "pango-draw-cairo-rendering"
       "pango-example"
       "cairo-rendering.lisp"))

     ;; Cairo demos
     "Cairo"
     (("Cairo Stroke"
       ":drawfunc"
       "cairo-draw-stroke"
       "cairo-example"
       "draw-stroke.lisp")
      ("Cairo Fill"
       ":drawfunc"
       "cairo-draw-fill"
       "cairo-example"
       "draw-fill.lisp")
      ("Cairo Text"
       ":drawfunc"
       "cairo-draw-text"
       "cairo-example"
       "draw-text.lisp")
      ("Cairo Paint"
       ":drawfunc"
       "cairo-draw-paint"
       "cairo-example"
       "draw-paint.lisp")
      ("Cairo Mask"
       ":drawfunc"
       "cairo-draw-mask"
       "cairo-example"
       "draw-mask.lisp")
      ("Cairo Source RGBA"
       ":drawfunc"
       "cairo-draw-source-rgba"
       "cairo-example"
       "draw-source-rgba.lisp")
      ("Cairo Source Gradient"
       ":drawfunc"
       "cairo-draw-source-gradient"
       "cairo-example"
       "draw-source-gradient.lisp")
      ("Cairo Path"
       ":drawfunc"
       "cairo-draw-path"
       "cairo-example"
       "draw-path.lisp")
      ("Cairo Dash"
       ":drawfunc"
       "cairo-draw-dash"
       "cairo-example"
       "draw-path.lisp")
      ("Cairo Dashes"
       ":drawfunc"
       "cairo-draw-dashes"
       "cairo-example"
       "draw-dashes.lisp")
      ("Cairo Joins"
       ":drawfunc"
       "cairo-draw-joins"
       "cairo-example"
       "draw-joins.lisp")
      ("Cairo Text centered"
       ":drawfunc"
       "cairo-draw-text-centered"
       "cairo-example"
       "draw-text-centered.lisp")
      ("Cairo Text Glyph"
       ":drawfunc"
       "cairo-draw-text-glyph"
       "cairo-example"
       "draw-text-glyph.lisp")
      ("Cairo Text Gradient"
       ":drawfunc"
       "cairo-draw-text-gradient"
       "cairo-example"
       "draw-text-gradient.lisp")
      ("Cairo Text Shaded"
       ":drawfunc"
       "cairo-draw-text-shaded"
       "cairo-example"
       "draw-text-shaded.lisp")
      ("Cairo Text Soulmate"
       ":drawfunc"
       "cairo-draw-text-soulmate"
       "cairo-example"
       "draw-text-soulmate.lisp")
      ("Cairo Draw Logo"
       ":drawfunc"
       "cairo-draw-logo"
       "cairo-example"
       "draw-logo.lisp")
      ("Cairo Draw Logo Translate"
       ":drawfunc"
       "cairo-draw-logo-translate"
       "cairo-example"
       "draw-logo.lisp"))

#|
      ("Cairo Clock"
       "cairo-demo"
       "cairo-clock.lisp"
       "DEMO-CAIRO-CLOCK")
|#

     "Miscellaneous"
     (("Drawing Area"
       ":window"
       "example-drawing-area"
       "gtk3-example"
       "drawing-area.lisp")
      ("Drawing in response to input"
       ":window"
       "example-drawing-area-input"
       "gtk3-example"
       "drawing-area-input.lisp")
      ("Calendar"
       ":window"
       "example-calendar"
       "gtk3-example"
       "calendar.lisp")
      ("Cursor"
       ":window"
       "example-cursor"
       "gtk3-example"
       "cursor.lisp")
      ("Event Box"
       ":window"
       "example-event-box"
       "gtk3-example"
       "event-box.lisp")
      ("Example Pixbufs"
       ":window"
       "example-pixbufs"
       "gtk3-example"
       "pixbufs.lisp")
      #+nil
      ("Emblemed Icons"
       "gio-example"
       "emblemed-icon.lisp"
       "EXAMPLE-EMBLEMED-ICON")
      ("Align Widget"
       ":window"
       "example-widget-align"
       "gtk3-example"
       "widget-align.lisp"))

#|
     ;; Examples from the tutorial
     "GTK 3 Tutorial for Lisp"
      ;; Chapter: Getting started
      ("Chapter: Getting started"
       (("Simple Window"
         "gtk3-example"
         "window-simple.lisp"
         "EXAMPLE-WINDOW-SIMPLE")
        ("Getting started"
         "gtk3-example"
         "getting-started.lisp"
         "EXAMPLE-GETTING-STARTED")
        ("Hello World"
         "gtk3-example"
         "hello-world.lisp"
         "EXAMPLE-HELLO-WORLD")
        ("Hello World Upgraded"
         "gtk3-example"
         "hello-world-upgraded.lisp"
         "EXAMPLE-HELLO-WORLD-UPGRADED")
        ("Hello World Upgraded (more Lisp like)"
         "gtk3-example"
         "hello-world-upgraded-2.lisp"
         "EXAMPLE-HELLO-WORLD-UPGRADED-2")
        ("Drawing in response to input"
         "gtk3-example"
         "drawing-area-input.lisp"
         "EXAMPLE-DRAWING-AREA-INPUT")))

      ;; Chapter: Packing Widgets
      ("Chapter: Packing Widgets"
       (("Simple Box"
         "gtk3-example"
         "box-simple.lisp"
         "EXAMPLE-BOX-SIMPLE"
         t)
        ("Box Packing"
         "gtk3-example"
         "box-packing.lisp"
         "EXAMPLE-BOX-PACKING")
        ("Simple Grid"
         "gtk3-example"
         "grid-simple.lisp"
         "EXAMPLE-GRID-SIMPLE")
        ("Simple Grid more Spacing"
         "gtk3-example"
         "grid-spacing.lisp"
         "EXAMPLE-GRID-SPACING")
        ("Packing using GtkGrid"
         "gtk3-example"
         "grid-packing.lisp"
         "EXAMPLE-GRID-PACKING")))

      ;; Chapter: Button Widgets
      ("Chapter: Button Widgets"
       (("Simple Button"
         "gtk3-example"
         "button-image.lisp"
         "EXAMPLE-BUTTON-IMAGE")
        ("More Buttons"
         "gtk3-example"
         "button-more.lisp"
         "EXAMPLE-BUTTON-MORE")
        ("Radio Button"
         "gtk3-example"
         "radio-button.lisp"
         "EXAMPLE-RADIO-BUTTON")
        ("Toggle Buttons"
         "gtk3-example"
         "toggle-buttons.lisp"
         "EXAMPLE-TOGGLE-BUTTONS")
        ("Link Button"
         "gtk3-example"
         "link-button.lisp"
         "EXAMPLE-LINK-BUTTON")
        ("Switch"
         "gtk3-example"
         "switch.lisp"
         "EXAMPLE-SWITCH")
        ("Scale Button"
         "gtk3-example"
         "scale-button.lisp"
         "EXAMPLE-SCALE-BUTTON")))

      ;; Chapter: Display Widgets
      ("Chapter: Display Widgets"
       (("Labels"
         "gtk3-example"
         "label.lisp"
         "EXAMPLE-LABEL")
        ("More Labels"
         "gtk3-example"
         "label-more.lisp"
         "EXAMPLE-LABEL-MORE")
        ("Images"
         "gtk3-example"
         "image.lisp"
         "EXAMPLE-IMAGE")
        ("Info Bar"
         "gtk3-example"
         "info-bar.lisp"
         "EXAMPLE-INFO-BAR")
        ("Progress Bar"
         "gtk3-example"
         "progress-bar.lisp"
         "EXAMPLE-PROGRESS-BAR")
        ("Statusbar"
         "gtk3-example"
         "statusbar.lisp"
         "EXAMPLE-STATUSBAR")))

      ;; Chapter: Layout Widgets
      ("Chapter: Layout Widgets"
       (("Button Boxes"
         "gtk3-example"
         "button-box.lisp"
         "EXAMPLE-BUTTON-BOX")
        ("Paned Window"
         "gtk3-example"
         "paned-window.lisp"
         "EXAMPLE-PANED-WINDOW")
        ("Layout Widget"
         "gtk3-example"
         "layout.lisp"
         "EXAMPLE-LAYOUT")
        ("Notebook"
         "gtk3-example"
         "notebook.lisp"
         "EXAMPLE-NOTEBOOK")
        ("Frame Widget"
         "gtk3-example"
         "frame.lisp"
         "EXAMPLE-FRAME"
         t)
        ("Aspect Frame"
         "gtk3-example"
         "aspect-frame.lisp"
         "EXAMPLE-ASPECT-FRAME")
        ("Fixed Container"
         "gtk3-example"
         "fixed.lisp"
         "EXAMPLE-FIXED")))
|#
     "Deprecated"
     (("Table Packing"
       ":window"
       "example-table-packing"
       "gtk3-example"
       "table-packing.lisp")
      ("Table Packing more spacing"
       ":window"
       "example-table-packing-2"
       "gtk3-example"
       "table-packing-2.lisp")
      ("Numerable Icons"
       ":window"
       "example-numerable-icon"
       "gtk3-example"
       "numerable-icon.lisp")
      ("Arrow Button"
       ":window"
       "example-arrow-button"
       "gtk3-example"
       "arrow-button.lisp")
      ("Alignment"
       ":window"
       "example-alignment"
       "gtk3-example"
       "alignment.lisp")
      ("Alignment Interactive"
       ":window"
       "example-alignment-interactive"
       "gtk3-example"
       "alignment-interactive.lisp"))
))

;;; ----------------------------------------------------------------------------

(defun window-draw-func (title drawfunc application
                         &optional (width 600) (height 600))
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :application application
                                 :title title
                                 :default-width width
                                 :default-height height))
          (area (make-instance 'gtk:drawing-area)))
      ;; Signal handler for the drawing area
      (g:signal-connect area "draw"
          (lambda (widget context)
            (let ((width (gtk:widget-allocated-width widget))
                  (height (gtk:widget-allocated-height widget))
                  (cr (glib:boxed-opaque-pointer context)))
              (funcall drawfunc cr width height))))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window.
      (gtk:container-add window area)
      (gtk:widget-show-all window))))

;;; ----------------------------------------------------------------------------

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-tree-store (data &optional (model nil) (iter nil))
  (unless model
    (setf model (apply #'gtk:tree-store-new (mklist (first data))))
    (setf data (rest data)))
  (let ((parent iter))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry)) ; entry is never an atom?
             (setf parent
                   (apply #'gtk:tree-store-set model
                                               (gtk:tree-store-append model
                                                                      iter)
                                               (mklist entry))))
            ((some #'listp entry)
             (create-and-fill-tree-store entry
                                         model
                                         parent)))))
  model)

;;; ----------------------------------------------------------------------------

(defun create-view-and-model (&optional application)
  (let* ((model (create-and-fill-tree-store *tree-model*))
         (view (make-instance 'gtk:tree-view
                              :model model))
         (selection (gtk:tree-view-selection view)))
    ;; Create renderers for the cells
    (let* ((renderer (gtk:cell-renderer-text-new))
           (column (gtk:tree-view-column-new-with-attributes "Example"
                                                             renderer
                                                             "text"
                                                             coltitle)))
      (gtk:tree-view-append-column view column))
    (g:signal-connect view "row-activated"
       (lambda (view path column)
         (declare (ignore column))
         (let* ((model (gtk:tree-view-model view))
                (iter (gtk:tree-model-iter model path))
                (title (gtk:tree-model-value model iter coltitle))
                (funcname (gtk:tree-model-value model iter colfunc))
                (functype (gtk:tree-model-value model iter coltype))
                (package (gtk:tree-model-value model iter colpackage))
                (func nil))

           (when functype
             (setf functype (read-from-string functype))
             (setf funcname (string-upcase funcname))
             (setf package (string-upcase package))
             (setf func (find-symbol funcname (find-package package))))

           (cond (;; Example as a window for application
                  (eq functype :window)
                  (funcall func application))
                 (;; Example as a parent to the active application window
                  (eq functype :dialog)
                  (funcall func (gtk:application-active-window application)))
                 (;; Example called from a draw handler
                  (eq functype :drawfunc)
                  (window-draw-func title func application))
                 (t (format t "NO function found.")))
               )))
    (setf (gtk:tree-selection-mode selection) :browse)
    ;; The selection has changed.
    (g:signal-connect selection "changed"
       (lambda (tree-selection)
         (let* ((iter (gtk:tree-selection-selected tree-selection))
                (package (gtk:tree-model-value model iter colpackage))
                (filename (gtk:tree-model-value model iter colfile))
                (ui-file (gtk:tree-model-value model iter colui))
                (css-file (gtk:tree-model-value model iter colcss)))
           ;; TODO: Improve this peace of code. Use pathname functions.
           (when package

             (setf package (string-downcase package))

             (format t "Row changed:~%")
             (format t "Package  : ~a~%" package)
             (format t "Filename : ~a~%" filename)
             (format t "Path : ~a~%" (sys-path filename package))

             (when filename
               (setf filename
                     (namestring (sys-path filename package))))
             (when (> (length ui-file) 0)
               (setf ui-file (namestring (sys-path ui-file package))))
             (when (> (length css-file) 0)
               (setf css-file (namestring (sys-path css-file package)))))

           (if (> (length filename) 0)
               (load-file filename))
           (if (> (length ui-file) 0)
               (load-file-buffer ui-buffer ui-file)
               (clear-buffer ui-buffer))
           (if (> (length css-file) 0)
               (load-file-buffer css-buffer css-file)
               (clear-buffer css-buffer)))))
      view))

(defun gtk-demo-activate (application)
  (let ((window (make-instance 'gtk:application-window
                               :application application
                               :type :toplevel
                               :title "GTK Lisp Code Demos"
                               :default-width 1000
                               :default-height 800))
        ;; A horizontal pane
        (content (make-instance 'gtk:paned
                                :orientation :horizontal))
        ;; A scrollable
        (scroller (make-instance 'gtk:scrolled-window
                                 :hscrollbar-policy :never
                                 :vscrollbar-policy :automatic
                                 :hexpand t
                                 :vexpand t))
        ;; A notebook
        (notebook (make-instance 'gtk:notebook
                                 :scrollable t))
        (view (create-view-and-model application)))

    (g:signal-connect window "destroy"
        (lambda (widget)
          (declare (ignore widget))
          (let ((action (g:action-map-lookup-action application "quit")))
            (g:action-activate action))))

    ;; Set an icon for the application
    ;; FIXME: The icon is not visible. What is wrong?
    (let* ((path (sys-path "gtk-logo.png"))
           (pixbuf (gdk:pixbuf-new-from-file path)))
      (setq pixbuf (gdk:pixbuf-add-alpha pixbuf t 255 255 255))
      (setf (gtk:window-default-icon-list) (list pixbuf)))
    ;; Add the widgets to the content of the window
    (gtk:container-add scroller view)
    (gtk:paned-add1 content scroller)
    (gtk:paned-add2 content notebook)
    ;; Add the notebook pages to the notebook
    (gtk:notebook-append-page notebook
                              (create-text info-buffer nil)
                              (gtk:label-new-with-mnemonic "_Info"))
    (gtk:notebook-append-page notebook
                              (create-text source-buffer t)
                              (gtk:label-new-with-mnemonic "_Source"))
    (gtk:notebook-append-page notebook
                              (create-text ui-buffer t)
                              (gtk:label-new-with-mnemonic "_UI Definition"))
    (gtk:notebook-append-page notebook
                              (create-text css-buffer t)
                              (gtk:label-new-with-mnemonic "_CSS Definition"))
    ;; Add the content to the window
    (gtk:container-add window content)
    (gtk:widget-show-all window)))

(defun activate-about-dialog ()
  (let (;; Create an about dialog
        (dialog (make-instance 'gtk:about-dialog
                               :program-name "GTK Lisp Demo"
                               :version "0.9"
                               :copyright "(c) Dieter Kaiser"
                               :website "github.com/crategus/cl-cffi-gtk3"
                               :website-label "Project web site"
                               :license "LLGPL"
                               :authors '("Dieter Kaiser")
                               :documenters '("Dieter Kaiser")
                               :artists '("None")
                               :logo-icon-name "applications-development"
                               :wrap-license t)))
    ;; Run the about dialog
    (gtk:dialog-run dialog)
    ;; Destroy the about dialog
    (gtk:widget-destroy dialog)))

(defvar *appmenu*
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<interface>
  <!-- interface-requires gtk+ 3.10 -->
  <menu id=\"appmenu\">
    <section>
      <item>
        <attribute name=\"label\" translatable=\"yes\">Inspector</attribute>
        <attribute name=\"action\">app.inspector</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name=\"label\" translatable=\"yes\">About</attribute>
        <attribute name=\"action\">app.about</attribute>
      </item>
    </section>
    <section>
      <item>
        <attribute name=\"label\" translatable=\"yes\">_Quit</attribute>
        <attribute name=\"action\">app.quit</attribute>
        <attribute name=\"accel\">&lt;Primary&gt;q</attribute>
      </item>
    </section>
  </menu>
</interface>")

(defun gtk-demo-startup (application)
  ;; Load the application menu
  (let ((builder (make-instance 'gtk:builder)))
    (gtk:builder-add-from-string builder *appmenu*)
    ;; FIXME: Improve the implementation of the menubar.
    ;; The APP-MENU is not shown, the MENUBAR is not correct!
    (setf (gtk:application-menubar application)
          (gtk:builder-object builder "appmenu")))
  ;; Add action "inspector" to the application
  (let ((action (g:simple-action-new "inspector" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (gtk:window-interactive-debugging t)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))
  ;; Add action "about" to the application
  (let ((action (g:simple-action-new "about" nil)))
    ;; Connect a handler to the signal "activate"
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         (activate-about-dialog)))
    ;; Add the action to the action map of the application
    (g:action-map-add-action application action))
  ;; Add action "quit" to the application
  (let ((action (g:simple-action-new "quit" nil)))
    ;; Connect a handler to the signal activate
    (g:signal-connect action "activate"
       (lambda (action parameter)
         (declare (ignore action parameter))
         ;; Destroy all windows of the application
         (dolist (window (gtk:application-windows application))
           (gtk:widget-destroy window))))
    ;; Add the action to action map of the application
    (g:action-map-add-action application action)))

(defun gtk3-demo (&rest argv)
  (unless (string= "GTK Lisp Demo" (g:application-name))
    (setf (g:application-name) "GTK Lisp Demo"))
  (let ((gtk-demo (make-instance 'gtk:application
                                 :application-id "com.crategus.gtk-demo"
                                 :register-session t)))
    ;; Connect signal handlers to the application
    (g:signal-connect gtk-demo "activate" #'gtk-demo-activate)
    (g:signal-connect gtk-demo "startup" #'gtk-demo-startup)
    ;; Start the application
    (g:application-run gtk-demo argv)))

;;; --- End of file gtk3-demo.lisp ---------------------------------------------
