;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk3.asd
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

(defsystem :cl-cffi-gtk3
  :name "cl-cffi-gtk3"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :components
  ((:module gdk
    :serial t
    :components
    ((:file "gdk3.package")
     (:file "gdk3.rectangle")
     (:file "gdk3.threads")
     (:file "gdk3.properties")
     (:file "gdk3.event-structures")
     (:file "gdk3.cursor")
     (:file "gdk3.device")
     (:file "gdk3.device-pad")
     (:file "gdk3.device-manager")
     (:file "gdk3.screen")
     (:file "gdk3.visual")
     (:file "gdk3.color")
     (:file "gdk3.rgba")
     (:file "gdk3.display")
     (:file "gdk3.display-manager")
     (:file "gdk3.pixbuf")
     (:file "gdk3.seat")
     (:file "gdk3.monitor")
     (:file "gdk3.window")
     (:file "gdk3.frame-timings")
     (:file "gdk3.frame-clock")
     (:file "gdk3.drawing-context")
     (:file "gdk3.gl-context")
     (:file "gdk3.events")
     (:file "gdk3.general")
     (:file "gdk3.key-values")
     (:file "gdk3.selection")
     (:file "gdk3.drag-and-drop")
     (:file "gdk3.app-launch-context")
     (:file "gdk3.pango")
     (:file "gdk3.cairo")
    ))
   (:module gtk
    :serial t
    :components
    ((:file "gtk3.package")

     ;; Gtk+ Core
     (:file "gtk3.version")
     (:file "gtk3.enumerations")
     (:file "gtk3.main-loop")
     (:file "gtk3.accel-group")
     (:file "gtk3.accel-map")
     (:file "gtk3.selection")
     (:file "gtk3.drag-and-drop")
     (:file "gtk3.clipboard")
     (:file "gtk3.settings")
     (:file "gtk3.bindings")
     (:file "gtk3.mount-operation")

     ;; Interface builder
     (:file "gtk3.buildable")
     (:file "gtk3.builder")

     ;; Theming in Gtk+
     (:file "gtk3.stock-images")
     (:file "gtk3.widget-path")
     (:file "gtk3.style-provider")
     (:file "gtk3.css-provider")
     (:file "gtk3.style-context")
     (:file "gtk3.icon-theme")
;     (:file "gtk3.style")
     (:file "gtk3.numerable-icon")

     ;; Inferfaces
     (:file "gtk3.implementor-iface")
     (:file "gtk3.orientable")
     (:file "gtk3.activatable")
     (:file "gtk3.scrollable")
     (:file "gtk3.actionable")

     ;; Abstract Base Classes
     (:file "gtk3.widget")
     (:file "gtk3.misc")
;    (:file "gtk3.child-properties")
     (:file "gtk3.container")
     (:file "gtk3.bin")
     (:file "gtk3.range")
     (:file "gtk3.menu-shell")
     (:file "gtk3.im-context")
     (:file "gtk3.native-dialog")

     ;; Layout Containers
     (:file "gtk3.box")
     (:file "gtk3.grid")
     (:file "gtk3.revealer")
     (:file "gtk3.list-box")
     (:file "gtk3.flow-box")
     (:file "gtk3.stack")
     (:file "gtk3.stack-switcher")
     (:file "gtk3.stack-sidebar")
     (:file "gtk3.action-bar")
     (:file "gtk3.header-bar")
     (:file "gtk3.overlay")
     (:file "gtk3.button-box")
     (:file "gtk3.paned")
     (:file "gtk3.layout")
     (:file "gtk3.notebook")
     (:file "gtk3.expander")
     (:file "gtk3.aspect-frame")
     (:file "gtk3.fixed")

     ;; Ornaments
     (:file "gtk3.separator")
     (:file "gtk3.frame")

     ;; Scrolling
     (:file "gtk3.scrollbar")
     (:file "gtk3.scrolled-window")

     ;; Windows
     (:file "gtk3.window")
     (:file "gtk3.dialog")
     (:file "gtk3.invisible")
     (:file "gtk3.message-dialog")
     (:file "gtk3.window-group")
     (:file "gtk3.about-dialog")
     (:file "gtk3.assistant")
     (:file "gtk3.offscreen-window")

     ;; Display Widgets
     (:file "gtk3.label")
     (:file "gtk3.accel-label")
     (:file "gtk3.image")
     (:file "gtk3.progress-bar")
     (:file "gtk3.statusbar")
     (:file "gtk3.level-bar")
     (:file "gtk3.info-bar")
     (:file "gtk3.status-icon")
     (:file "gtk3.spinner")

     ;; Buttons and Toggles
     (:file "gtk3.button")
     (:file "gtk3.toggle-button")
     (:file "gtk3.check-button")
     (:file "gtk3.radio-button")
     (:file "gtk3.link-button")
     (:file "gtk3.menu-button")
     (:file "gtk3.scale-button")
     (:file "gtk3.volume-button")
     (:file "gtk3.switch")
     (:file "gtk3.lock-button")
     (:file "gtk3.model-button")

     ;; Multiline Text Editor
;    (:file "gtk3.text-attributes")
     (:file "gtk3.text-iter")
     (:file "gtk3.text-tag")
     (:file "gtk3.text-mark")
     (:file "gtk3.text-buffer")
     (:file "gtk3.text-tag-table")
     (:file "gtk3.text-view")

     ;; Tree, List and Icon Grid Widgets
     (:file "gtk3.tree-model")
     (:file "gtk3.cell-layout")
     (:file "gtk3.tree-sortable")
     (:file "gtk3.tree-view-drag-and-drop")
     (:file "gtk3.tree-model-sort")
     (:file "gtk3.tree-model-filter")
     (:file "gtk3.tree-view")
     (:file "gtk3.tree-view-column")
     (:file "gtk3.tree-store")
     (:file "gtk3.tree-selection")
     (:file "gtk3.cell-editable")
     (:file "gtk3.cell-renderer")
     (:file "gtk3.cell-renderer-text")
     (:file "gtk3.cell-renderer-pixbuf")
     (:file "gtk3.cell-renderer-progress")
     (:file "gtk3.cell-renderer-accel")
     (:file "gtk3.cell-renderer-combo")
     (:file "gtk3.cell-renderer-spin")
     (:file "gtk3.cell-renderer-toggle")
     (:file "gtk3.cell-renderer-spinner")
     (:file "gtk3.cell-area")
     (:file "gtk3.cell-area-box")
     (:file "gtk3.cell-area-context")
     (:file "gtk3.cell-view")
     (:file "gtk3.icon-view")
     (:file "gtk3.list-store")

     ;; Numeric/Text Data Entry
     (:file "gtk3.editable")
     (:file "gtk3.entry")
     (:file "gtk3.entry-buffer")
     (:file "gtk3.entry-completion")
     (:file "gtk3.scale")
     (:file "gtk3.spin-button")
     (:file "gtk3.search-entry")
     (:file "gtk3.search-bar")

     ;; Menus, Combo Box, Toolbar
     (:file "gtk3.menu-item")
     (:file "gtk3.menu")
     (:file "gtk3.menu-bar")
     (:file "gtk3.check-menu-item")
     (:file "gtk3.radio-menu-item")
     (:file "gtk3.image-menu-item")
     (:file "gtk3.separator-menu-item")
     (:file "gtk3.tearoff-menu-item")
     (:file "gtk3.combo-box")
     (:file "gtk3.combo-box-text")
     (:file "gtk3.tool-shell")
     (:file "gtk3.tool-item")
     (:file "gtk3.tool-item-group")
     (:file "gtk3.toolbar")
     (:file "gtk3.tool-palette")
     (:file "gtk3.separator-tool-item")
     (:file "gtk3.tool-button")
     (:file "gtk3.toggle-tool-button")
     (:file "gtk3.radio-tool-button")
     (:file "gtk3.menu-tool-button")
     (:file "gtk3.popover")
     (:file "gtk3.popover-menu")

     ;; Selectors
     (:file "gtk3.color-chooser")
     (:file "gtk3.color-button")
     (:file "gtk3.color-chooser-widget")
     (:file "gtk3.color-chooser-dialog")
     (:file "gtk3.color-selection")
     (:file "gtk3.color-selection-dialog")
     (:file "gtk3.hsv")

     (:file "gtk3.file-filter")
     (:file "gtk3.file-chooser")
     (:file "gtk3.file-chooser-button")
     (:file "gtk3.file-chooser-native")
     (:file "gtk3.file-chooser-dialog")
     (:file "gtk3.file-chooser-widget")

     (:file "gtk3.font-chooser")
     (:file "gtk3.font-button")
     (:file "gtk3.font-chooser-widget")
     (:file "gtk3.font-chooser-dialog")
     (:file "gtk3.font-selection")
     (:file "gtk3.font-selection-dialog")
     (:file "gtk3.places-sidebar")

     ;; Miscellaneous
     (:file "gtk3.adjustment")
     (:file "gtk3.arrow")
     (:file "gtk3.calendar")
     (:file "gtk3.drawing-area")
     (:file "gtk3.event-box")
     (:file "gtk3.handle-box")
     (:file "gtk3.im-context-simple")
     (:file "gtk3.im-multicontext")
     (:file "gtk3.size-group")
     (:file "gtk3.tooltip")
     (:file "gtk3.viewport")

     ;; Cross-process Embedding
     (:file "gtk3.plug"                      :if-feature (:not :windows))
     (:file "gtk3.socket"                    :if-feature (:not :windows))

     ;; Recently Used Documents
     (:file "gtk3.recent-manager")
     (:file "gtk3.recent-chooser")
     (:file "gtk3.recent-chooser-dialog")
     (:file "gtk3.recent-chooser-menu")
     (:file "gtk3.recent-chooser-widget")
     (:file "gtk3.recent-filter")

     ;; Action-based menus and toolbars
     (:file "gtk3.action-group")
     (:file "gtk3.action")
     (:file "gtk3.toggle-action")
     (:file "gtk3.radio-action")
     (:file "gtk3.recent-action")

     ;; Choosing from installed applications
     (:file "gtk3.app-chooser")
     (:file "gtk3.app-chooser-button")
     (:file "gtk3.app-chooser-dialog")
     (:file "gtk3.app-chooser-widget")

     ;; Gestures and event handling
     (:file "gtk3.event-controller")
     (:file "gtk3.event-controller-key")
     (:file "gtk3.event-controller-scroll")
     (:file "gtk3.event-controller-motion")
     (:file "gtk3.gesture")
     (:file "gtk3.gesture-single")
     (:file "gtk3.gesture-drag")
     (:file "gtk3.gesture-long-press")
     (:file "gtk3.gesture-multi-press")
     (:file "gtk3.gesture-pan")
     (:file "gtk3.gesture-swipe")
     (:file "gtk3.gesture-rotate")
     (:file "gtk3.gesture-zoom")
     (:file "gtk3.gesture-stylus")
     (:file "gtk3.pad-controller")

     ;; Printing
     (:file "gtk3.print-operation")
     (:file "gtk3.print-context")
     (:file "gtk3.paper-size")
     (:file "gtk3.print-settings")
     (:file "gtk3.page-setup")
     (:file "gtk3.print-unix-dialog"         :if-feature (:not :windows))
     (:file "gtk3.page-setup-unix-dialog"    :if-feature (:not :windows))
     (:file "gtk3.printer"                   :if-feature (:not :windows))
     (:file "gtk3.print-job"                 :if-feature (:not :windows))
     ;; Shortcuts Overview
     (:file "gtk3.shortcuts-window")
     (:file "gtk3.shortcuts-section")
     (:file "gtk3.shortcuts-group")
     (:file "gtk3.shortcuts-shortcut")

     ;; Application support
     (:file "gtk3.application")
     (:file "gtk3.application-window")

     ;; Deprecated
     (:file "gtk3.alignment")
     (:file "gtk3.table")
     (:file "gtk3.ui-manager")

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
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :serial t
  :components ((:file "cl-cffi-gtk3-init"))         ; Library Initialization
  :depends-on (:cl-cffi-glib-init))

;; Definine a test operation for the library

(defsystem :cl-cffi-gtk3/test
  :name "cl-cffi-gtk3/test"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :depends-on (:cl-cffi-gtk3 :cl-cffi-glib/test :fiveam)
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
     (:file "rtest-gtk3-version")
     (:file "rtest-gtk3-enumerations")
     (:file "rtest-gtk3-main-loop")
     (:file "rtest-gtk3-accel-group")
     (:file "rtest-gtk3-accel-map")
     (:file "rtest-gtk3-selection")
     (:file "rtest-gtk3-drag-and-drop")
     (:file "rtest-gtk3-clipboard")
     (:file "rtest-gtk3-settings")
;    (:file "gtk.bindings")
;    (:file "gtk.mount-operation")
     ;; Interface builder
     (:file "rtest-gtk3-buildable")
     (:file "rtest-gtk3-builder")
     ;; Theming in Gtk+
     (:file "rtest-gtk3-stock-images")
     (:file "rtest-gtk3-widget-path")
     (:file "rtest-gtk3-style-provider")
     (:file "rtest-gtk3-css-provider")
     (:file "rtest-gtk3-style-context")
     (:file "rtest-gtk3-icon-theme")
     (:file "rtest-gtk3-numerable-icon")
     ;; Inferfaces
     (:file "rtest-gtk3-orientable")
     (:file "rtest-gtk3-activatable")
     (:file "rtest-gtk3-scrollable")
     (:file "rtest-gtk3-actionable")
     ;; Abstract Base Classes
     (:file "rtest-gtk3-widget")
     (:file "rtest-gtk3-misc")
;    (:file "gtk.child-properties")
     (:file "rtest-gtk3-container")
     (:file "rtest-gtk3-bin")
     (:file "rtest-gtk3-range")
     (:file "rtest-gtk3-menu-shell")
;    (:file "gtk.im-context")
     (:file "rtest-gtk3-native-dialog")

     ;; Layout Containers
     (:file "rtest-gtk3-box")
     (:file "rtest-gtk3-grid")
     (:file "rtest-gtk3-revealer")
     (:file "rtest-gtk3-list-box")
     (:file "rtest-gtk3-flow-box")
     (:file "rtest-gtk3-stack")
     (:file "rtest-gtk3-stack-switcher")
     (:file "rtest-gtk3-stack-sidebar")
     (:file "rtest-gtk3-action-bar")
     (:file "rtest-gtk3-header-bar")
     (:file "rtest-gtk3-overlay")
     (:file "rtest-gtk3-button-box")
     (:file "rtest-gtk3-paned")
     (:file "rtest-gtk3-layout")
     (:file "rtest-gtk3-notebook")
;    (:file "gtk.expander")
     (:file "rtest-gtk3-aspect-frame")
     (:file "rtest-gtk3-fixed")

     ;; Ornaments
     (:file "rtest-gtk3-separator")
     (:file "rtest-gtk3-frame")
     ;; Scrolling
     (:file "rtest-gtk3-scrollbar")
     (:file "rtest-gtk3-scrolled-window")

     ;; Windows
     (:file "rtest-gtk3-window")
     (:file "rtest-gtk3-dialog")
     (:file "rtest-gtk3-invisible")
     (:file "rtest-gtk3-message-dialog")
     (:file "rtest-gtk3-window-group")
     (:file "rtest-gtk3-about-dialog")
     (:file "rtest-gtk3-assistant")
     (:file "rtest-gtk3-offscreen-window")

     ;; Display Widgets
     (:file "rtest-gtk3-label")
     (:file "rtest-gtk3-accel-label")
     (:file "rtest-gtk3-image")
     (:file "rtest-gtk3-progress-bar")
     (:file "rtest-gtk3-statusbar")
     (:file "rtest-gtk3-level-bar")
     (:file "rtest-gtk3-info-bar")
;    (:file "gtk.status-icon")
     (:file "rtest-gtk3-spinner")

     ;; Buttons and Toggles
     (:file "rtest-gtk3-button")
     (:file "rtest-gtk3-toggle-button")
;    (:file "gtk.check-button")
     (:file "rtest-gtk3-radio-button")
;    (:file "gtk.link-button")
;    (:file "gtk.menu-button")
;    (:file "gtk.scale-button")
;    (:file "gtk.volume-button")
;    (:file "gtk.switch")
;    (:file "gtk.lock-button")
;    (:file "gtk.model-button")

     ;; Multiline Text Editor
     (:file "rtest-gtk3-text-iter")
     (:file "rtest-gtk3-text-tag")
     (:file "rtest-gtk3-text-mark")
     (:file "rtest-gtk3-text-buffer")
     (:file "rtest-gtk3-text-tag-table")
     (:file "rtest-gtk3-text-view")

     ;; Tree, List and Icon Grid Widgets
     (:file "rtest-gtk3-tree-model")
     (:file "rtest-gtk3-cell-layout")
     (:file "rtest-gtk3-tree-sortable")
     (:file "rtest-gtk3-tree-view-drag-and-drop")
;    (:file "gtk.tree-model-sort")
;    (:file "gtk.tree-model-filter")
     (:file "rtest-gtk3-tree-view")
     (:file "rtest-gtk3-tree-view-column")
;    (:file "gtk.tree-store")
     (:file "rtest-gtk3-tree-selection")

     (:file "rtest-gtk3-cell-editable")
     (:file "rtest-gtk3-cell-renderer")
     (:file "rtest-gtk3-cell-renderer-text")
     (:file "rtest-gtk3-cell-renderer-pixbuf")
     (:file "rtest-gtk3-cell-renderer-progress")
     (:file "rtest-gtk3-cell-renderer-accel")
     (:file "rtest-gtk3-cell-renderer-combo")
     (:file "rtest-gtk3-cell-renderer-spin")
     (:file "rtest-gtk3-cell-renderer-toggle")
     (:file "rtest-gtk3-cell-renderer-spinner")

     (:file "rtest-gtk3-cell-area")
     (:file "rtest-gtk3-cell-area-box")
;    (:file "gtk.cell-area-context")
     (:file "rtest-gtk3-cell-view")
     (:file "rtest-gtk3-icon-view")
     (:file "rtest-gtk3-list-store")

     ;; Numeric/Text Data Entry
     (:file "rtest-gtk3-editable")
     (:file "rtest-gtk3-entry")
     (:file "rtest-gtk3-entry-buffer")
     (:file "rtest-gtk3-entry-completion")
;    (:file "gtk.scale")
;    (:file "gtk.spin-button")
;    (:file "gtk.search-entry")
;    (:file "gtk.search-bar")

     ;; Menus, Combo Box, Toolbar
;    (:file "gtk.menu-item")
     (:file "rtest-gtk3-menu")
;    (:file "gtk.menu-bar")
     (:file "rtest-gtk3-check-menu-item")
     (:file "rtest-gtk3-radio-menu-item")
     (:file "rtest-gtk3-image-menu-item")
;    (:file "gtk.separator-menu-item")
;    (:file "gtk.tearoff-menu-item")
;    (:file "gtk.combo-box")
;     (:file "gtk.combo-box-text")
;    (:file "gtk.tool-shell")
     (:file "rtest-gtk3-tool-item")
;    (:file "gtk.tool-item-group")
;    (:file "gtk.toolbar")
     (:file "rtest-gtk3-tool-palette")
;    (:file "gtk.separator-tool-item")
;    (:file "gtk.tool-button")
;    (:file "gtk.toggle-tool-button")
     (:file "rtest-gtk3-radio-tool-button")
;    (:file "gtk.menu-tool-button")
     (:file "rtest-gtk3-popover")
;    (:file "gtk.popover-menu")

     ;; Selectors
     (:file "rtest-gtk3-color-chooser")
     (:file "rtest-gtk3-color-button")
     (:file "rtest-gtk3-color-chooser-widget")
     (:file "rtest-gtk3-color-chooser-dialog")
     (:file "rtest-gtk3-color-selection")
     (:file "rtest-gtk3-color-selection-dialog")
     (:file "rtest-gtk3-hsv")

     (:file "rtest-gtk3-file-filter")
     (:file "rtest-gtk3-file-chooser"             :if-feature (:not :windows))
     (:file "rtest-gtk3-file-chooser-button"      :if-feature (:not :windows))
     (:file "rtest-gtk3-file-chooser-native"      :if-feature (:not :windows))
     (:file "rtest-gtk3-file-chooser-dialog"      :if-feature (:not :windows))
     (:file "rtest-gtk3-file-chooser-widget"      :if-feature (:not :windows))

     (:file "rtest-gtk3-font-chooser")
     (:file "rtest-gtk3-font-button")
     (:file "rtest-gtk3-font-chooser-widget")
     (:file "rtest-gtk3-font-chooser-dialog")
     (:file "rtest-gtk3-places-sidebar")

     ;; Miscellaneous
     (:file "rtest-gtk3-adjustment")
     (:file "rtest-gtk3-arrow")
     (:file "rtest-gtk3-calendar")
;    (:file "gtk.drawing-area")
     (:file "rtest-gtk3-event-box")
;    (:file "gtk.handle-box")
;    (:file "gtk.im-context-simple")
;    (:file "gtk.im-multicontext")
;    (:file "gtk.size-group")
;    (:file "gtk.tooltip")
;    (:file "gtk.viewport")

     ;; Cross-process Embedding
;    #-windows
;    (:file "gtk.plug")
;    #-windows
;    (:file "gtk.socket")

     ;; Recently Used Documents
     (:file "rtest-gtk3-recent-manager")
;    (:file "gtk.recent-chooser")
;    (:file "gtk.recent-chooser-dialog")
;    (:file "gtk.recent-chooser-menu")
;    (:file "gtk.recent-chooser-widget")
;    (:file "gtk.recent-filter")

     ;; Action-based menus and toolbars
     (:file "rtest-gtk3-action-group")
     (:file "rtest-gtk3-action")
     (:file "rtest-gtk3-toggle-action")
     (:file "rtest-gtk3-radio-action")
     (:file "rtest-gtk3-recent-action")

     ;; Choosing from installed applications
     (:file "rtest-gtk3-app-chooser")
     (:file "rtest-gtk3-app-chooser-button")
     (:file "rtest-gtk3-app-chooser-dialog")
;    (:file "gtk.app-chooser-widget")

     ;; Gestures and event handling
;    (:file "gtk.event-controller")
;    (:file "gtk.event-controller-key")
;    (:file "gtk.event-controller-scroll")
;    (:file "gtk.event-controller-motion")
     (:file "rtest-gtk3-gesture")
;    (:file "gtk.gesture-single")
;    (:file "gtk.gesture-drag")
;    (:file "gtk.gesture-long-press")
;    (:file "gtk.gesture-multi-press")
;    (:file "gtk.gesture-pan")
;    (:file "gtk.gesture-swipe")
;    (:file "gtk.gesture-rotate")
;    (:file "gtk.gesture-zoom")
;    (:file "gtk.gesture-stylus")
     (:file "rtest-gtk3-pad-controller")

     ;; Printing
     (:file "rtest-gtk3-print-operation")
     (:file "rtest-gtk3-print-context")
     (:file "rtest-gtk3-paper-size")
     (:file "rtest-gtk3-print-settings")
     (:file "rtest-gtk3-page-setup")

     (:file "rtest-gtk3-print-unix-dialog"        :if-feature (:not :windows))
     (:file "rtest-gtk3-page-setup-unix-dialog"   :if-feature (:not :windows))
     (:file "rtest-gtk3-printer"                  :if-feature (:not :windows))
     (:file "rtest-gtk3-print-job"                :if-feature (:not :windows))

     ;; Shortcuts Overview
     (:file "rtest-gtk3-shortcuts-window")
;    (:file "gtk.shortcuts-section")
;    (:file "gtk.shortcuts-group")
;    (:file "gtk.shortcuts-shortcut")

     ;; Application support
     (:file "rtest-gtk3-application")
     (:file "rtest-gtk3-application-window")

     ;; Deprecated
     (:file "rtest-gtk3-alignment")
     (:file "rtest-gtk3-table")
;    (:file "gtk.ui-manager")

     ;; GDK
     (:file "rtest-gdk3")
     (:file "rtest-gdk3-rectangle")
     (:file "rtest-gdk3-threads")
     (:file "rtest-gdk3-properties")
     (:file "rtest-gdk3-event-structures")
     (:file "rtest-gdk3-cursor")
     (:file "rtest-gdk3-device")
     (:file "rtest-gdk3-device-pad")
     (:file "rtest-gdk3-device-manager")
     (:file "rtest-gdk3-screen")
     (:file "rtest-gdk3-visual")
     (:file "rtest-gdk3-color")
     (:file "rtest-gdk3-rgba")
     (:file "rtest-gdk3-display")
     (:file "rtest-gdk3-display-manager")
     (:file "rtest-gdk3-pixbuf")
     (:file "rtest-gdk3-seat")
     (:file "rtest-gdk3-monitor")
     (:file "rtest-gdk3-window")
     (:file "rtest-gdk3-frame-timings")
     (:file "rtest-gdk3-frame-clock")
     (:file "rtest-gdk3-drawing-context")
     (:file "rtest-gdk3-gl-context")
     (:file "rtest-gdk3-events")
     (:file "rtest-gdk3-general")
     (:file "rtest-gdk3-key-values")
     (:file "rtest-gdk3-selection")
     (:file "rtest-gdk3-drag-and-drop")
     (:file "rtest-gdk3-app-launch-context")
     (:file "rtest-gdk3-pango")
     (:file "rtest-gdk3-cairo")

     (:file "rtest-gtk3-finish")
))))

;;; --- End of file cl-cffi-gtk3.asd -------------------------------------------
