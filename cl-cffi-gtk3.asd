;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk3.asd
;;;
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

(defsystem :cl-cffi-gtk3
  :name "cl-cffi-gtk3"
  :version "0.1.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components
  ((:module gdk
    :serial t
    :components
    ((:file "gdk3.package")
     (:file "gdk3.rectangle")          ; Points and Rectangles
     (:file "gdk3.threads")            ; Using GDK with threads
     (:file "gdk3.properties")         ; Manipulate properties on windows
     (:file "gdk3.event-structures")   ; Data structures for events
     (:file "gdk3.cursor")             ; Standard and pixmap cursors
     (:file "gdk3.device")             ; Representing an input device
     (:file "gdk3.device-pad")         ; Pad device interface
     (:file "gdk3.device-manager")     ; Handling input devices
     (:file "gdk3.screen")             ; Representing a physical screen
     (:file "gdk3.visual")             ; Low-level display information
     (:file "gdk3.color")              ; Colormaps and Colors
     (:file "gdk3.rgba")               ; RGBA colors
     (:file "gdk3.display")            ; Controls the keyboard/mouse
     (:file "gdk3.display-manager")    ; Maintains a list GdkDisplays
     (:file "gdk3.pixbuf")             ; Functions for obtaining pixbufs
     (:file "gdk3.seat")               ; Object representing an user seat
     (:file "gdk3.monitor")            ; Object representing an output.
     (:file "gdk3.window")             ; Onscreen display areas
     (:file "gdk3.frame-timings")      ; Frame timings
     (:file "gdk3.frame-clock")        ; Frame clock
     (:file "gdk3.drawing-context")    ; Drawing context for GDK windows
     (:file "gdk3.gl-context")         ; Open GL context
     (:file "gdk3.events")             ; Functions for handling events
     (:file "gdk3.general")            ; Miscellaneous functions
     (:file "gdk3.key-values")         ; Manipulating keyboard codes
     (:file "gdk3.selection")          ; Transfering data
     (:file "gdk3.drag-and-drop")      ; Drag and drop handling
     (:file "gdk3.app")                ; Notification for applications
     (:file "gdk3.pango")              ; Using Pango in GDK
     (:file "gdk3.cairo")              ; Functions to support using cairo
    ))
   (:module gtk
    :serial t
    :components
    ((:file "gtk3.package")

     ;; Gtk+ Core
     (:file "gtk3.version")               ; Version Information
     (:file "gtk3.enumerations")          ; Standard Enumerations
     (:file "gtk3.main-loop")             ; Main event loop, and events
     (:file "gtk3.accel-group")           ; Accelerator Groups
     (:file "gtk3.accel-map")             ; Loadable keyboard accelerator
     (:file "gtk3.selection")             ; Inter-process communication
     (:file "gtk3.drag-and-drop")         ; Controlling drag and drop
     (:file "gtk3.clipboard")             ; Storing data on clipboards
     (:file "gtk3.settings")              ; Sharing settings
     (:file "gtk3.bindings")              ; Key bindings for widgets
     (:file "gtk3.mount-operation")       ; Filesystem utilities

     ;; Interface builder
     (:file "gtk3.buildable")             ; GtkBuildable
     (:file "gtk3.builder")               ; Build an interface

     ;; Theming in Gtk+
     (:file "gtk3.stock-images")          ; Manipulating stock icons
     (:file "gtk3.widget-path")           ; Widget path abstraction
     (:file "gtk3.style-provider")        ; Interface for style information
     (:file "gtk3.css-provider")          ; CSS-like styling for widgets
     (:file "gtk3.style-context")         ; Rendering UI elements
     (:file "gtk3.icon-theme")            ; Looking up icons by name
;     (:file "gtk3.style")                 ; Functions for drawing widget parts
     (:file "gtk3.numerable-icon")        ; A GIcon that allows numbered emblems

     ;; Inferfaces
     (:file "gtk3.implementor-iface")     ; AtkImplementorIface
     (:file "gtk3.orientable")            ; Interface for flippable widgets
     (:file "gtk3.activatable")           ; Interface for activatable widgets
     (:file "gtk3.scrollable")            ; Interface for scrollable widgets
     (:file "gtk3.actionable")            ; Interface for actions

     ;; Abstract Base Classes
     (:file "gtk3.widget")                ; Base class for all widgets
     (:file "gtk3.misc")                  ; Base class for alignments
;    (:file "gtk3.child-properties")
     (:file "gtk3.container")             ; GtkContainer
     (:file "gtk3.bin")                   ; Container with just one child
     (:file "gtk3.range")                 ; Base class for adjustments
     (:file "gtk3.menu-shell")            ; Base class for menu objects
     (:file "gtk3.im-context")            ; Base class for input contexts
     (:file "gtk3.native-dialog")         ; Integrate with native dialogs

     ;; Layout Containers
     (:file "gtk3.box")                   ; Container box
     (:file "gtk3.grid")                  ; Pack widgets in a rows and columns
     (:file "gtk3.revealer")              ; Hide and show with animation
     (:file "gtk3.list-box")              ; A list container
     (:file "gtk3.flow-box")              ; Allows reflowing its children
     (:file "gtk3.stack")                 ; A stacking container
     (:file "gtk3.stack-switcher")        ; A controller for GtkStack
     (:file "gtk3.stack-sidebar")         ; An automatic sidebar widget
     (:file "gtk3.action-bar")            ; A bar for presenting actions
     (:file "gtk3.header-bar")            ; Box with a centered child
     (:file "gtk3.overlay")               ; Container which overlays widgets
     (:file "gtk3.button-box")            ; Container for arranging buttons
     (:file "gtk3.paned")                 ; Two adjustable panes
     (:file "gtk3.layout")                ; Infinite scrollable
     (:file "gtk3.notebook")              ; Tabbed notebook container
     (:file "gtk3.expander")              ; Container which can hide childs
     (:file "gtk3.aspect-frame")          ; Constrain childs to a aspect ratio
     (:file "gtk3.fixed")                 ; Widgets at fixed coordinates

     ;; Ornaments
     (:file "gtk3.separator")             ; Separator widget
     (:file "gtk3.frame")                 ; Decorative frame

     ;; Scrolling
     (:file "gtk3.scrollbar")             ; GtkScrollbar
     (:file "gtk3.scrolled-window")       ; Adds scrollbars

     ;; Windows
     (:file "gtk3.window")                ; GtkWindow
     (:file "gtk3.dialog")                ; GtkDialog
     (:file "gtk3.invisible")             ; GtkInvisible
     (:file "gtk3.message-dialog")        ; GtkMessageDialog
     (:file "gtk3.window-group")          ; GtkWindowGroup
     (:file "gtk3.about-dialog")          ; GtkAboutDialog
     (:file "gtk3.assistant")             ; GtkAssistant
     (:file "gtk3.offscreen-window")      ; GtkOffscreenWindow

     ;; Display Widgets
     (:file "gtk3.label")                 ; GtkLabel
     (:file "gtk3.accel-label")           ; GtkAccelLabel
     (:file "gtk3.image")                 ; GtkImage
     (:file "gtk3.progress-bar")          ; GtkProgessBar
     (:file "gtk3.statusbar")             ; GTKStatusbar
     (:file "gtk3.level-bar")             ; GtkLevelBar
     (:file "gtk3.info-bar")              ; GtkInfoBar
     (:file "gtk3.status-icon")           ; GtkStatusIcon
     (:file "gtk3.spinner")               ; GtkSpinner

     ;; Buttons and Toggles
     (:file "gtk3.button")                ; GtkButton
     (:file "gtk3.toggle-button")         ; GtkToggleButton
     (:file "gtk3.check-button")          ; GtkCheckButton
     (:file "gtk3.radio-button")          ; GtkRadioButton
     (:file "gtk3.link-button")           ; GtkLinkButton
     (:file "gtk3.menu-button")           ; GtkMenuButton
     (:file "gtk3.scale-button")          ; GtkScaleButton
     (:file "gtk3.volume-button")         ; GtkVolumeButton
     (:file "gtk3.switch")                ; GtkSwitch
     (:file "gtk3.lock-button")           ; GtkLockButton
     (:file "gtk3.model-button")          ; Button that uses a GAction as model

     ;; Multiline Text Editor
;    (:file "gtk3.text-attributes")       ; GtkTextAttributes
     (:file "gtk3.text-iter")             ; GtkTextIter
     (:file "gtk3.text-tag")              ; GtkTextTag
     (:file "gtk3.text-mark")             ; GtkTextMark
     (:file "gtk3.text-buffer")           ; GtkTextBuffer
     (:file "gtk3.text-tag-table")        ; GtkTextTagTable
     (:file "gtk3.text-view")             ; GtkTextView

     ;; Tree, List and Icon Grid Widgets
     (:file "gtk3.tree-model")            ; Tree interface
     (:file "gtk3.cell-layout")           ; Interface for packing cells
     (:file "gtk3.tree-sortable")
     (:file "gtk3.tree-view-drag-and-drop")
     (:file "gtk3.tree-model-sort")       ; GtkTreeModelSort
     (:file "gtk3.tree-model-filter")     ; GtkTreeModelFilter
     (:file "gtk3.tree-view")             ; Displaying both trees and lists
     (:file "gtk3.tree-view-column")      ; Visible column in GtkTreeView
     (:file "gtk3.tree-store")            ; Tree-like data structure
     (:file "gtk3.tree-selection")        ; Selection object for GtkTreeView
     (:file "gtk3.cell-editable")         ; GtkCellEditable
     (:file "gtk3.cell-renderer")         ; Object for rendering a cell
     (:file "gtk3.cell-renderer-text")    ; Renders text in a cell
     (:file "gtk3.cell-renderer-pixbuf")  ; Renders a pixbuf in a cell
     (:file "gtk3.cell-renderer-progress"); Renders numbers as progress bars
     (:file "gtk3.cell-renderer-accel")   ; Renders a keyboard accelerator
     (:file "gtk3.cell-renderer-combo")   ; Renders a combobox in a cell
     (:file "gtk3.cell-renderer-spin")    ; Renders a spin button in a cell
     (:file "gtk3.cell-renderer-toggle")  ; Renders a toggle button in a cell
     (:file "gtk3.cell-renderer-spinner") ; Renders a spinning animation in a cell
     (:file "gtk3.cell-area")             ; Laying out GtkCellRenderers
     (:file "gtk3.cell-area-box")         ; Renders into a row or a column
     (:file "gtk3.cell-area-context")     ; gtkCellAreaContext
     (:file "gtk3.cell-view")             ; Displaying a single row
     (:file "gtk3.icon-view")             ; List of icons in a grid
     (:file "gtk3.list-store")            ; List-like data structure

     ;; Numeric/Text Data Entry
     (:file "gtk3.editable")              ; Interface for text editing widgets
     (:file "gtk3.entry")                 ; A single line text entry field
     (:file "gtk3.entry-buffer")          ; Text buffer for GtkEntry
     (:file "gtk3.entry-completion")      ; Completion functionality for GtkEntry
     (:file "gtk3.scale")                 ; GtkScale, GtkHScale, GtkVScale
     (:file "gtk3.spin-button")           ; GtkSpinButton
     (:file "gtk3.search-entry")          ; GtkSearchEntry
     (:file "gtk3.search-bar")            ; GtkSearchBar

     ;; Menus, Combo Box, Toolbar
     (:file "gtk3.menu-item")             ; Widget used for item in menus
     (:file "gtk3.menu")                  ; Menu widget
     (:file "gtk3.menu-bar")              ; Subclass for GtkMenuItem widgets
     (:file "gtk3.check-menu-item")       ; Menu item with a check box
     (:file "gtk3.radio-menu-item")       ; Choice from multiple check menu items
     (:file "gtk3.image-menu-item")       ; Menu item with an icon
     (:file "gtk3.separator-menu-item")   ; Separator used in menus
     (:file "gtk3.tearoff-menu-item")     ; Menu item used to tear off and reattach
     (:file "gtk3.combo-box")             ; GtkComboBox
     (:file "gtk3.combo-box-text")        ; Simple, text-only combo box
     (:file "gtk3.tool-shell")            ; Interface for GtkToolItem
     (:file "gtk3.tool-item")             ; GtkToolItem
     (:file "gtk3.tool-item-group")       ; GtkToolItemGroup
     (:file "gtk3.toolbar")               ; Create bars of buttons
     (:file "gtk3.tool-palette")          ; Tool palette with categories
     (:file "gtk3.separator-tool-item")   ; Toolbar item that separates groups
     (:file "gtk3.tool-button")           ; GtkToolButton
     (:file "gtk3.toggle-tool-button")    ; GtkToggleToolButton
     (:file "gtk3.radio-tool-button")     ; GtkRadioToolButton
     (:file "gtk3.menu-tool-button")      ; GtkMenuToolButton
     (:file "gtk3.popover")               ; Context dependent bubbles
     (:file "gtk3.popover-menu")          ; Popovers to use as menus

     ;; Selectors
     (:file "gtk3.color-chooser")         ; Interface for choosing colors
     (:file "gtk3.color-button")          ; Launch a color selection dialog
     (:file "gtk3.color-chooser-widget")  ; Widget for choosing colors
     (:file "gtk3.color-chooser-dialog")  ; Dialog for choosing colors
     (:file "gtk3.color-selection")       ; Widget used to select a color
     (:file "gtk3.color-selection-dialog"); Widget used to select a color
     (:file "gtk3.hsv")                   ; GtkHSV
     (:file "gtk3.file-chooser")          ; File chooser interface
     (:file "gtk3.file-chooser-widget")   ; File chooser widget
     (:file "gtk3.file-chooser-button")   ; Button to launch a file selection
     (:file "gtk3.file-chooser-dialog")   ; File chooser dialog
     (:file "gtk3.file-chooser-native")   ; A native file chooser dialog
     (:file "gtk3.file-filter")           ; Selecting a file subset
     (:file "gtk3.font-chooser")          ; Interface for displaying fonts
     (:file "gtk3.font-button")           ; Button to launch a font chooser dialog
     (:file "gtk3.font-chooser-widget")   ; Widget for selecting fonts
     (:file "gtk3.font-chooser-dialog")   ; Dialog for selecting fonts
     (:file "gtk3.font-selection")        ; Deprecated widget for selecting fonts
     (:file "gtk3.font-selection-dialog") ; Deprecated widget for selecting fonts
     (:file "gtk3.places-sidebar")        ; Displays frequently-used places

     ;; Miscellaneous
     (:file "gtk3.adjustment")            ; Representation of a bounded value
     (:file "gtk3.arrow")                 ; Displays an arrow
     (:file "gtk3.calendar")              ; Displays a calendar
     (:file "gtk3.drawing-area")          ; Custom user interface elements
     (:file "gtk3.event-box")             ; Widget used to catch events
     (:file "gtk3.handle-box")            ; Widget for detachable window portions
     (:file "gtk3.im-context-simple")     ; Table-based input methods
     (:file "gtk3.im-multicontext")       ; Multiple, loadable input methods
     (:file "gtk3.size-group")            ; Grouping widgets to the same size
     (:file "gtk3.tooltip")               ; Add tips to your widgets
     (:file "gtk3.viewport")              ; Adapter which makes widgets scrollable

     ;; Cross-process Embedding
     (:file "gtk3.plug"                   ; Embedding into other processes
            :if-feature (:not :windows))
     (:file "gtk3.socket"                 ; For widgets from other processes
            :if-feature (:not :windows))

     ;; Recently Used Documents
     (:file "gtk3.recent-manager")        ; Managing recently used files
     (:file "gtk3.recent-chooser")        ; Displaying recently used files
     (:file "gtk3.recent-chooser-dialog") ; Displays recently used files
     (:file "gtk3.recent-chooser-menu")   ; Displays recently used files in a menu
     (:file "gtk3.recent-chooser-widget") ; Displays recently used files
     (:file "gtk3.recent-filter")         ; Selecting recently used files

     ;; Action-based menus and toolbars
     (:file "gtk3.action-group")          ; Group of actions
     (:file "gtk3.action")                ; GtkAction
     (:file "gtk3.toggle-action")         ; GtkToggleAction
     (:file "gtk3.radio-action")          ; GtkRadioAction
     (:file "gtk3.recent-action")         ; List of recently used files

     ;; Choosing from installed applications
     (:file "gtk3.app-chooser")           ; Interface for choosing an application
     (:file "gtk3.app-chooser-button")    ; Button to launch an application
     (:file "gtk3.app-chooser-dialog")    ; Application chooser dialog
     (:file "gtk3.app-chooser-widget")    ; Application chooser widget

     ;; Gestures and event handling
     (:file "gtk3.event-controller")       ; Handler of series of events
     (:file "gtk3.event-controller-key")   ; Event controller for key events
     (:file "gtk3.event-controller-scroll"); Event controller for scroll events
     (:file "gtk3.event-controller-motion"); Event controller for motion events
     (:file "gtk3.gesture")                ; Base class for gestures
     (:file "gtk3.gesture-single")         ; Base class for single-touch gestures
     (:file "gtk3.gesture-drag")           ; Drag gesture
     (:file "gtk3.gesture-long-press")     ; "Press and Hold" gesture
     (:file "gtk3.gesture-multi-press")    ; Multipress gesture
     (:file "gtk3.gesture-pan")            ; Pan gesture
     (:file "gtk3.gesture-swipe")          ; Swipe gesture
     (:file "gtk3.gesture-rotate")         ; Rotate gesture
     (:file "gtk3.gesture-zoom")           ; Zoom gesture
     (:file "gtk3.gesture-stylus")         ; Gesture for stylus input
     (:file "gtk3.pad-controller")         ; Controller for drawing tablet pads

     ;; Printing
     (:file "gtk3.print-operation")       ; High-level Printing API
     (:file "gtk3.print-context")         ; Encapsulates context for drawing pages
     (:file "gtk3.paper-size")            ; Support for named paper sizes
     (:file "gtk3.print-settings")        ; Stores print settings
     (:file "gtk3.page-setup")            ; Stores page setup information
     (:file "gtk3.print-unix-dialog"      ; A print dialog
            :if-feature (:not :windows))
     (:file "gtk3.page-setup-unix-dialog" ; A page setup dialog
            :if-feature (:not :windows))
     (:file "gtk3.printer"                ; Represents a printer
            :if-feature (:not :windows))
     (:file "gtk3.print-job"              ; Represents a print job
            :if-feature (:not :windows))
     ;; Shortcuts Overview
     (:file "gtk3.shortcuts-window")      ; Toplevel which shows help for shortcuts
     (:file "gtk3.shortcuts-section")     ; An app mode in a GtkShortcutsWindow
     (:file "gtk3.shortcuts-group")       ; Represents a group of shortcuts
     (:file "gtk3.shortcuts-shortcut")    ; Represents a keyboard shortcut

     ;; Application support
     (:file "gtk3.application")           ; Application class
     (:file "gtk3.application-window")    ; GtkApplicationWindow

     ;; Deprecated
     (:file "gtk3.alignment")             ; GtkAlignment
     (:file "gtk3.table")                 ; Pack widgets in regular patterns
     (:file "gtk3.ui-manager")            ; Constructing menus and toolbars

     ;; Lisp
     (:file "gtk3.init"))))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-gtk3/test")))
  :depends-on (:cl-cffi-gtk3/init
               :cl-cffi-glib
               :cl-cffi-gdk-pixbuf
               :cl-cffi-pango
               :cl-cffi-cairo
               :split-sequence))

(defsystem :cl-cffi-gtk3/init
  :name "cl-cffi-gtk3/init"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "cl-cffi-gtk3-init"))         ; Library Initialization
  :depends-on (:cl-cffi-glib/init))

;; Definine a test operation for the library

(defsystem :cl-cffi-gtk3/test
  :name "cl-cffi-gtk3/test"
  :depends-on (:cl-cffi-gtk3 :fiveam)
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :gtk-test :gtk-test)))
  :components
  ((:module test
    :serial nil
    :components
    (;; GTK tests
     (:file "rtest-gtk3")
     ;; Gtk+ Core
     (:file "rtest-gtk3-version")        ; Version Information
     (:file "rtest-gtk3-enumerations")   ; Standard Enumerations
     (:file "rtest-gtk3-main-loop")      ; Main event loop, and events
     (:file "rtest-gtk3-accel-group")    ; Accelerator Groups
     (:file "rtest-gtk3-accel-map")      ; Loadable keyboard accelerator
     (:file "rtest-gtk3-selection")      ; Inter-process communication
     (:file "rtest-gtk3-drag-and-drop")  ; Controlling drag and drop
     (:file "rtest-gtk3-clipboard")      ; Storing data on clipboards
     (:file "rtest-gtk3-settings")       ; Sharing settings
;    (:file "gtk.bindings")              ; Key bindings for widgets
;    (:file "gtk.mount-operation")       ; Filesystem utilities
     ;; Interface builder
     (:file "rtest-gtk3-buildable")      ; GtkBuildable
     (:file "rtest-gtk3-builder")        ; Build an interface
     ;; Theming in Gtk+
     (:file "rtest-gtk3-stock-images")   ; Manipulating stock icons
     (:file "rtest-gtk3-widget-path")    ; Widget path abstraction
     (:file "rtest-gtk3-style-provider") ; Interface for style information
     (:file "rtest-gtk3-css-provider")   ; CSS-like styling for widgets
     (:file "rtest-gtk3-style-context")  ; Rendering UI elements
     (:file "rtest-gtk3-icon-theme")     ; Looking up icons by name
     (:file "rtest-gtk3-numerable-icon") ; A GIcon that allows numbered emblems
     ;; Inferfaces
     (:file "rtest-gtk3-orientable")     ; Interface for flippable widgets
     (:file "rtest-gtk3-activatable")    ; Interface for activatable widgets
     (:file "rtest-gtk3-scrollable")     ; Interface for scrollable widgets
     (:file "rtest-gtk3-actionable")     ; Interface for actions
     ;; Abstract Base Classes
     (:file "rtest-gtk3-widget")         ; Base class for all widgets
     (:file "rtest-gtk3-misc")           ; Base class for alignments
;    (:file "gtk.child-properties")
     (:file "rtest-gtk3-container")      ; GtkContainer
     (:file "rtest-gtk3-bin")            ; Container with just one child
;    (:file "gtk.range")                 ; Base class for adjustments
     (:file "rtest-gtk3-menu-shell")     ; Base class for menu objects
;    (:file "gtk.im-context")            ; Base class for input contexts
;    (:file "gtk3.native-dialog")        ; Integrate with native dialogs

     ;; Layout Containers
     (:file "rtest-gtk3-box")            ; Container box
     (:file "rtest-gtk3-grid")           ; Pack widgets in a rows and columns
     (:file "rtest-gtk3-revealer")       ; Hide and show with animation
     (:file "rtest-gtk3-list-box")       ; A list container
     (:file "rtest-gtk3-flow-box")       ; Allows reflowing its children
     (:file "rtest-gtk3-stack")          ; A stacking container
     (:file "rtest-gtk3-stack-switcher") ; A controller for GtkStack
;    (:file "gtk.stack-sidebar")         ; An automatic sidebar widget
;    (:file "gtk.action-bar")            ; A bar for presenting actions
;    (:file "gtk.header-bar")            ; Box with a centered child
;    (:file "gtk.overlay")               ; Container which overlays widgets
     (:file "rtest-gtk3-button-box")     ; Container for arranging buttons
     (:file "rtest-gtk3-paned")          ; Two adjustable panes
     (:file "rtest-gtk3-layout")         ; Infinite scrollable
     (:file "rtest-gtk3-notebook")       ; Tabbed notebook container
;    (:file "gtk.expander")              ; Container which can hide childs
;    (:file "gtk.aspect-frame")          ; Constrain childs to a aspect ratio
     (:file "rtest-gtk3-fixed")          ; Widgets at fixed coordinates
     ;; Ornaments
;    (:file "gtk.separator")             ; Separator widget
     (:file "rtest-gtk3-frame")          ; Decorative frame
     ;; Scrolling
     (:file "rtest-gtk3-scrollbar")      ; GtkScrollbar
     (:file "rtest-gtk3-scrolled-window"); Adds scrollbars
     ;; Windows
     (:file "rtest-gtk3-window")         ; GtkWindow
     (:file "rtest-gtk3-dialog")         ; GtkDialog
     (:file "rtest-gtk3-invisible")      ; GtkInvisible
     (:file "rtest-gtk3-message-dialog") ; GtkMessageDialog
     (:file "rtest-gtk3-window-group")   ; GtkWindowGroup
     (:file "rtest-gtk3-about-dialog")   ; GtkAboutDialog
     (:file "rtest-gtk3-assistant")      ; GtkAssistant
     (:file "rtest-gtk3-offscreen-window"); GtkOffscreenWindow
     ;; Display Widgets
     (:file "rtest-gtk3-label")          ; GtkLabel
     (:file "rtest-gtk3-accel-label")    ; GtkAccelLabel
     (:file "rtest-gtk3-image")          ; GtkImage
     (:file "rtest-gtk3-progress-bar")   ; GtkProgessBar
     (:file "rtest-gtk3-statusbar")      ; GTKStatusbar
     (:file "rtest-gtk3-level-bar")      ; GtkLevelBar
     (:file "rtest-gtk3-info-bar")       ; GtkInfoBar
;    (:file "gtk.status-icon")           ; GtkStatusIcon
     (:file "rtest-gtk3-spinner")        ; GtkSpinner

     ;; Buttons and Toggles
     (:file "rtest-gtk3-button")         ; GtkButton
;    (:file "gtk.toggle-button")         ; GtkToggleButton
;    (:file "gtk.check-button")          ; GtkCheckButton
     (:file "rtest-gtk3-radio-button")   ; GtkRadioButton
;    (:file "gtk.link-button")           ; GtkLinkButton
;    (:file "gtk.menu-button")           ; GtkMenuButton
;    (:file "gtk.scale-button")          ; GtkScaleButton
;    (:file "gtk.volume-button")         ; GtkVolumeButton
;    (:file "gtk.switch")                ; GtkSwitch
;    (:file "gtk.lock-button")           ; GtkLockButton
;    (:file "gtk.model-button")          ; A button that uses a GAction as model

     ;; Multiline Text Editor
     (:file "rtest-gtk3-text-iter")      ; GtkTextIter
     (:file "rtest-gtk3-text-tag")       ; GtkTextTag
     (:file "rtest-gtk3-text-mark")      ; GtkTextMark
     (:file "rtest-gtk3-text-buffer")    ; GtkTextBuffer
     (:file "rtest-gtk3-text-tag-table") ; GtkTextTagTable
     (:file "rtest-gtk3-text-view")      ; GtkTextView

     ;; Tree, List and Icon Grid Widgets
     (:file "rtest-gtk3-tree-model")     ; Tree interface
     (:file "rtest-gtk3-cell-layout")    ; Interface for packing cells
;    (:file "gtk.tree-sortable")
     (:file "rtest-gtk3-tree-view-drag-and-drop")
;    (:file "gtk.tree-model-sort")       ; GtkTreeModelSort
;    (:file "gtk.tree-model-filter")     ; GtkTreeModelFilter
     (:file "rtest-gtk3-tree-view")      ; Displaying both trees and lists
     (:file "rtest-gtk3-tree-view-column"); Visible column in GtkTreeView
;    (:file "gtk.tree-store")            ; Tree-like data structure
     (:file "rtest-gtk3-tree-selection") ; Selection object for GtkTreeView
;    (:file "gtk.cell-editable")         ; GtkCellEditable
     (:file "rtest-gtk3-cell-renderer")  ; Object for rendering a cell
     (:file "rtest-gtk3-cell-renderer-text") ; Renders text
;    (:file "gtk.cell-renderer-pixbuf")  ; Renders a pixbuf
     (:file "rtest-gtk3-cell-renderer-progress"); Renders numbers as progress bars
     (:file "rtest-gtk3-cell-renderer-accel"); Renders a keyboard accelerator
;    (:file "gtk.cell-renderer-combo")   ; Renders a combobox
     (:file "rtest-gtk3-cell-renderer-spin")    ; Renders a spin button
     (:file "rtest-gtk3-cell-renderer-toggle")  ; Renders a toggle button
     (:file "rtest-gtk3-cell-renderer-spinner") ; Renders a spinning animation
     (:file "rtest-gtk3-cell-area")      ; Laying out GtkCellRenderers
     (:file "rtest-gtk3-cell-area-box")  ; Renders into a row or a column
;    (:file "gtk.cell-area-context")     ; gtkCellAreaContext
     (:file "rtest-gtk3-cell-view")      ; Displaying a single row
     (:file "rtest-gtk3-icon-view")      ; List of icons in a grid
     (:file "rtest-gtk3-list-store")     ; List-like data structure

     ;; Numeric/Text Data Entry
     (:file "rtest-gtk3-editable")       ; Interface for text editing widgets
     (:file "rtest-gtk3-entry")          ; A single line text entry field
     (:file "rtest-gtk3-entry-buffer")   ; Text buffer for GtkEntry
     (:file "rtest-gtk3-entry-completion"); Completion functionality for GtkEntry
;    (:file "gtk.scale")                 ; GtkScale, GtkHScale, GtkVScale
;    (:file "gtk.spin-button")           ; GtkSpinButton
;    (:file "gtk.search-entry")          ; GtkSearchEntry
;    (:file "gtk.search-bar")            ; GtkSearchBar

     ;; Menus, Combo Box, Toolbar
;    (:file "gtk.menu-item")             ; Widget used for item in menus
     (:file "rtest-gtk3-menu")           ; Menu widget
;    (:file "gtk.menu-bar")              ; Subclass for GtkMenuItem widgets
     (:file "rtest-gtk3-check-menu-item"); Menu item with a check box
     (:file "rtest-gtk3-radio-menu-item"); Choice from multiple check menu items
;    (:file "gtk.image-menu-item")       ; Menu item with an icon
;    (:file "gtk.separator-menu-item")   ; Separator used in menus
;    (:file "gtk.tearoff-menu-item")     ; Menu item used to tear off and reattach
;    (:file "gtk.combo-box")             ; GtkComboBox
;     (:file "gtk.combo-box-text")        ; Simple, text-only combo box
;    (:file "gtk.tool-shell")            ; Interface for GtkToolItem
     (:file "rtest-gtk3-tool-item")      ; GtkToolItem
;    (:file "gtk.tool-item-group")       ; GtkToolItemGroup
;    (:file "gtk.toolbar")               ; Create bars of buttons
     (:file "rtest-gtk3-tool-palette")   ; Tool palette with categories
;    (:file "gtk.separator-tool-item")   ; Toolbar item that separates groups
;    (:file "gtk.tool-button")           ; GtkToolButton
;    (:file "gtk.toggle-tool-button")    ; GtkToggleToolButton
     (:file "rtest-gtk3-radio-tool-button"); GtkRadioToolButton
;    (:file "gtk.menu-tool-button")      ; GtkMenuToolButton
;    (:file "gtk.popover")               ; Context dependent bubbles
;    (:file "gtk.popover-menu")          ; Popovers to use as menus

     ;; Selectors
;    (:file "gtk.color-chooser")         ; Interface for choosing colors
;    (:file "gtk.color-button")          ; Launch a color selection dialog
;    (:file "gtk.color-chooser-widget")  ; Widget for choosing colors
;    (:file "gtk.color-chooser-dialog")  ; Dialog for choosing colors
;    (:file "gtk.color-selection")       ; Widget used to select a color
;    (:file "gtk.color-selection-dialog"); Widget used to select a color
;    (:file "gtk.hsv")                   ; GtkHSV
     (:file "rtest-gtk3-file-chooser")   ; File chooser interface
     (:file "rtest-gtk3-file-chooser-widget"); File chooser widget
;    (:file "gtk.file-chooser-button")   ; Button to launch a file selection
;    (:file "gtk.file-chooser-dialog")   ; File chooser dialog
;    (:file "gtk.file-chooser-native")   ; A native file chooser dialog
;    (:file "gtk.file-filter")           ; Selecting a file subset
;    (:file "gtk.font-chooser")          ; Interface for displaying fonts
;    (:file "gtk.font-button")           ; Button to launch a font chooser dialog
;    (:file "gtk.font-chooser-widget")   ; Widget for selecting fonts
;    (:file "gtk.font-chooser-dialog")   ; Dialog for selecting fonts
;    (:file "gtk.font-selection")        ; Deprecated widget for selecting fonts
;    (:file "gtk.font-selection-dialog") ; Deprecated widget for selecting fonts
     (:file "rtest-gtk3-places-sidebar") ; Displays frequently-used places

     ;; Miscellaneous
     (:file "rtest-gtk3-adjustment")     ; Representation of a bounded value
;    (:file "gtk.arrow")                 ; Displays an arrow
;    (:file "gtk.calendar")              ; Displays a calendar
;    (:file "gtk.drawing-area")          ; Custom user interface elements
     (:file "rtest-gtk3-event-box")      ; Widget used to catch events
;    (:file "gtk.handle-box")            ; Widget for detachable window portions
;    (:file "gtk.im-context-simple")     ; Table-based input methods
;    (:file "gtk.im-multicontext")       ; Multiple, loadable input methods
;    (:file "gtk.size-group")            ; Grouping widgets to the same size
;    (:file "gtk.tooltip")               ; Add tips to your widgets
;    (:file "gtk.viewport")              ; Adapter which makes widgets scrollable

     ;; Cross-process Embedding
;    #-win32
;    (:file "gtk.plug")                  ; Embedding into other processes
;    #-win32
;    (:file "gtk.socket")                ; For widgets from other processes

     ;; Recently Used Documents
     (:file "rtest-gtk3-recent-manager") ; Managing recently used files
;    (:file "gtk.recent-chooser")        ; Displaying recently used files
;    (:file "gtk.recent-chooser-dialog") ; Displays recently used files
;    (:file "gtk.recent-chooser-menu")   ; Displays recently used files in a menu
;    (:file "gtk.recent-chooser-widget") ; Displays recently used files
;    (:file "gtk.recent-filter")         ; Selecting recently used files

     ;; Action-based menus and toolbars
     (:file "rtest-gtk3-action-group")   ; Group of actions
     (:file "rtest-gtk3-action")         ; GtkAction
;    (:file "gtk.toggle-action")         ; GtkToggleAction
;    (:file "gtk.radio-action")          ; GtkRadioAction
;    (:file "gtk.recent-action")         ; List of recently used files

     ;; Choosing from installed applications
     (:file "rtest-gtk3-app-chooser")    ; Interface for choosing an application
     (:file "rtest-gtk3-app-chooser-button"); Button to launch an application
     (:file "rtest-gtk3-app-chooser-dialog"); Application chooser dialog
;    (:file "gtk.app-chooser-widget")    ; Application chooser widget

     ;; Gestures and event handling
;    (:file "gtk.event-controller")       ; Handler of series of events
;    (:file "gtk.event-controller-key")   ; Event controller for key events
;    (:file "gtk.event-controller-scroll"); Event controller for scroll events
;    (:file "gtk.event-controller-motion"); Event controller for motion events
     (:file "rtest-gtk3-gesture")         ; Base class for gestures
;    (:file "gtk.gesture-single")         ; Base class for single-touch gestures
;    (:file "gtk.gesture-drag")           ; Drag gesture
;    (:file "gtk.gesture-long-press")     ; "Press and Hold" gesture
;    (:file "gtk.gesture-multi-press")    ; Multipress gesture
;    (:file "gtk.gesture-pan")            ; Pan gesture
;    (:file "gtk.gesture-swipe")          ; Swipe gesture
;    (:file "gtk.gesture-rotate")         ; Rotate gesture
;    (:file "gtk.gesture-zoom")           ; Zoom gesture
;    (:file "gtk.gesture-stylus")         ; Gesture for stylus input
     (:file "rtest-gtk3-pad-controller")  ; Controller for drawing tablet pads

     ;; Printing
     (:file "rtest-gtk3-print-operation") ; High-level Printing API
     (:file "rtest-gtk3-print-context")   ; Encapsulates context for drawing pages
     (:file "rtest-gtk3-paper-size")      ; Support for named paper sizes
     (:file "rtest-gtk3-print-settings")  ; Stores print settings
     (:file "rtest-gtk3-page-setup")      ; Stores page setup information
     #-win32
     (:file "rtest-gtk3-print-unix-dialog"); A print dialog
     #-win32
     (:file "rtest-gtk3-page-setup-unix-dialog"); A page setup dialog
     #-win32
     (:file "rtest-gtk3-printer")         ; Represents a printer
     #-win32
     (:file "rtest-gtk3-print-job")       ; Represents a print job

     ;; Shortcuts Overview
;    (:file "gtk.shortcuts-window")       ; Toplevel which shows help for shortcuts
;    (:file "gtk.shortcuts-section")      ; An app mode in a GtkShortcutsWindow
;    (:file "gtk.shortcuts-group")        ; Represents a group of shortcuts
;    (:file "gtk.shortcuts-shortcut")     ; Represents a keyboard shortcut

     ;; Application support
     (:file "rtest-gtk3-application")           ; Application class
     (:file "rtest-gtk3-application-window")    ; GtkApplicationWindow

     ;; Deprecated
;    (:file "gtk.alignment")             ; GtkAlignment
;    (:file "gtk.table")                 ; Pack widgets in regular patterns
;    (:file "gtk.ui-manager")            ; Constructing menus and toolbars

     ;; GDK
     (:file "rtest-gdk3")
     (:file "rtest-gdk3-rectangle")        ; Points and Rectangles
     (:file "rtest-gdk3-threads")          ; Using GDK with threads
     (:file "rtest-gdk3-properties")       ; Manipulate properties on windows
     (:file "rtest-gdk3-event-structures") ; Data structures for events
     (:file "rtest-gdk3-cursor")           ; Standard and pixmap cursors
     (:file "rtest-gdk3-device")           ; Representing an input device
     (:file "rtest-gdk3-device-pad")       ; Pad device interface
     (:file "rtest-gdk3-device-manager")   ; Handling input devices
     (:file "rtest-gdk3-screen")           ; Representing a physical screen
     (:file "rtest-gdk3-visual")           ; Low-level display information
     (:file "rtest-gdk3-color")            ; Colormaps and Colors
     (:file "rtest-gdk3-rgba")             ; RGBA colors
     (:file "rtest-gdk3-display")          ; Controls the keyboard/mouse
     (:file "rtest-gdk3-display-manager")  ; Maintains a list GdkDisplays
     (:file "rtest-gdk3-pixbuf")           ; Functions for obtaining pixbufs
     (:file "rtest-gdk3-seat")             ; Object representing an user seat
     (:file "rtest-gdk3-monitor")          ; Object representing an output.
     (:file "rtest-gdk3-window")           ; Onscreen display areas
     (:file "rtest-gdk3-frame-timings")    ; Frame timings
     (:file "rtest-gdk3-frame-clock")      ; Frame clock
     (:file "rtest-gdk3-drawing-context")  ; Drawing context for GDK windows
     (:file "rtest-gdk3-gl-context")       ; Open GL context
     (:file "rtest-gdk3-events")           ; Functions for handling events
     (:file "rtest-gdk3-general")          ; Miscellaneous functions
     (:file "rtest-gdk3-key-values")       ; Manipulating keyboard codes
     (:file "rtest-gdk3-selection")        ; Transfering data
     (:file "rtest-gdk3-drag-and-drop")    ; Drag and drop handling
     (:file "rtest-gdk3-app")              ; Notification for applications
     (:file "rtest-gdk3-pango")            ; Using Pango in GDK
     (:file "rtest-gdk3-cairo")            ; Functions to support using cairo
))))

;;; Examples for the Gtk3 library

(asdf:defsystem :cl-cffi-gtk3/example
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on (:cl-cffi-gtk3 :split-sequence)
  :components
  ((:module example
    :serial nil
    :components
    ((:file "gtk3-example")
     (:file "utils")
     (:file "action-bar")
     (:file "css-accordion")
     (:file "css-basics")
     (:file "css-blendmodes")
     (:file "css-multiplebgs")
     (:file "css-pixbufs")
     (:file "css-shadows")
     (:file "cursor")
     (:file "alignment")
     (:file "alignment-interactive")
     (:file "arrow-button")
     (:file "aspect-frame")                  ; Layout Widgets
     (:file "assistant")
     (:file "box-packing")                   ; Packing Widgets
     (:file "box-simple")                    ; Packing Widgets
     (:file "button-box")                    ; Layout Widgets
     (:file "button-image")                  ; Button Widgets
     (:file "button-more")                   ; Button Widgets
     (:file "calendar")
     (:file "cell-renderer-properties")      ; Tree and List Widgets
     (:file "clipboard")
     (:file "color-button")
     (:file "color-button-label")            ; Selecting Colors, ...
     (:file "color-chooser-dialog")          ; Selecting Colors, ...
     (:file "color-chooser-palette")
     (:file "color-chooser-widget")
     (:file "combo-box")
     (:file "combo-box-text")
     (:file "custom-drawing")
     (:file "custom-window")
     (:file "dialog")                        ; Dialog Windows
     (:file "drag-and-drop")
     (:file "drag-and-drop-action")
     (:file "drag-and-drop-simple")
     (:file "drawing-area")
     (:file "drawing-area-input")            ; Getting started
     (:file "event-box")
     (:file "event-handler")
     (:file "expander")                      ; Layout Widgets
     (:file "file-chooser-button")           ; Selecting Colors, ...
     (:file "file-chooser-custom-filter")
     (:file "file-chooser-dialog")
     (:file "file-chooser-preview")
     (:file "file-chooser-widget")
     (:file "fixed")                         ; Layout Widgets
     (:file "flow-box")                      ; Layout Widgets
     (:file "font-button")
     (:file "font-button-label")             ; Selecting Colors, ...
     (:file "frame")                         ; Layout Widgets
     (:file "frame-properties")              ; Layout Widgets
     (:file "getting-started")               ; Getting started
     (:file "grab")
     (:file "grid-packing")                  ; Packing Widgets
     (:file "grid-simple")                   ; Packing Widgets
     (:file "grid-spacing")                  ; Packing Widgets
     (:file "header-bar")                    ; Layout Widgets
     (:file "hello-world")                   ; Getting started
     (:file "hello-world-upgraded")          ; Getting started
     (:file "hello-world-upgraded-2")        ; Getting started
     (:file "icon-view")                     ; Tree and List Widgets
     (:file "image")                         ; Display Widgets
     (:file "image-button-press")
     (:file "info-bar")                      ; Display Widgets
     (:file "label")                         ; Display Widgets
     (:file "label-more")                    ; Display Widgets
     (:file "layout")                        ; Layout Widgets
     (:file "level-bar")
     (:file "link-button")                   ; Button Widgets
     (:file "list-box")
     (:file "list-content-types")
;    (:file "list-store")
     (:file "menu")
     (:file "menu-builder")
     (:file "menu-by-hand")                  ; Menus and Toolbars
     (:file "menu-item")                     ; Menus and Toolbars
     (:file "menu-popup")                    ; Menus and Toolbars
     (:file "message-dialog-new")
     (:file "message-dialog-simple")
     (:file "notebook")                      ; Layout Widgets
     (:file "numerable-icon")
     (:file "overlay-decorative")            ; Layout Widgets
     (:file "overlay-interactive")           ; Layout Widgets
     (:file "overlay-transparent")           ; Layout Widgets
     (:file "page-setup-dialog")
     (:file "paned-window")                  ; Layout Widgets
     (:file "pixbufs")
     (:file "pixbuf-scale")
     (:file "pointer-device")
     (:file "popover")
     (:file "print-dialog")
     (:file "print-operation")
     (:file "progress-bar")                  ; Display Widgets
     (:file "query-settings")
     (:file "radio-button")
     (:file "revealer")
     (:file "revealer-icon")
     (:file "scale-button")
     (:file "scale-widget")
     (:file "scrolled-window")
     (:file "search-entry")
     (:file "show-about-dialog")
     (:file "spin-button")
     (:file "spinner")                       ; Display Widgets
     (:file "stack")                         ; Layout Widgets
     (:file "stack-sidebar")                 ; Layout Widgets
     (:file "statusbar")                     ; Display Widgets
     (:file "switch")                        ; Button widgets
     (:file "table-packing")                 ; Deprecated
     (:file "table-packing-2")               ; Deprecated
     (:file "text-entry")
     (:file "text-entry-buffer")
     (:file "text-entry-completion")
     (:file "text-view-attributes")          ; Multiline Text Widget
     (:file "text-view-find-next")           ; Multiline Text Widget
     (:file "text-view-insert")              ; Multiline Text Widget
     (:file "text-view-insert-image")        ; Multiline Text Widget
     (:file "text-view-insert-widget")       ; Multiline Text Widget
     (:file "text-view-search")              ; Multiline Text Widget
     (:file "text-view-simple")              ; Multiline Text Widget
     (:file "text-view-tags")                ; Multiline Text Widget
     (:file "text-view-tooltip")             ; Multiline Text Widget
     (:file "toggle-buttons")                ; Button Widgets
     (:file "toolbar-by-hand")
     (:file "tool-palette")
     (:file "tree-view-content-type")        ; Tree and List Widgets
     (:file "tree-view-context-menu")        ; Tree and List Widgets
     (:file "tree-view-drag-and-drop")       ; Tree and List Widgets
     (:file "tree-view-dump-model")          ; Tree and List Widgets
     (:file "tree-view-editable")            ; Tree and List Widgets
     (:file "tree-view-example")             ; Tree and List Widgets
     (:file "tree-view-path")                ; Tree and List Widgets
     (:file "tree-view-simple")              ; Tree and List Widgets
     (:file "tree-view-sortable")            ; Tree and List Widgets
     (:file "widget-align")
     (:file "widget-pointer")
;    (:file "window-application")
     (:file "window-simple")                 ; Getting started
     (:file "window-simple-demo")
     ))))

;;; --- End of file cl-cffi-gtk3.asd -------------------------------------------
