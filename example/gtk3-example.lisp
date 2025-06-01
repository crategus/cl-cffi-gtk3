(defpackage :gtk3-example
  (:use :split-sequence :common-lisp)
  (:export #:create-expander-dialog
           #:create-page-setup-dialog
           #:create-print-dialog
           #:do-print-operation
           #:list-content-types
           #:example-action-bar
           #:example-alignment
           #:example-alignment-interactive
           #:example-arrow-button
           #:example-aspect-frame                      ; Layout Widgets
           #:example-assistant
           #:example-box-packing                       ; Packing Widgets
           #:example-box-simple                        ; Packing Widgets
           #:example-button-box                        ; Layout Widgets
           #:example-button-image                      ; Button Widgets
           #:example-button-more                       ; Button Widgets
           #:example-calendar
           #:example-cell-renderer-properties          ; Tree and List Widgets
           #:example-clipboard
           #:example-color-button
           #:example-color-button-label                ; Selecting Colors, ...
           #:example-color-chooser-dialog              ; Selecting Colors, ...
           #:example-color-chooser-palette
           #:example-color-chooser-widget
           #:example-combo-box
           #:example-combo-box-text
           #:example-css-accordion
           #:example-css-basics
           #:example-css-blendmodes
           #:example-css-multiplebgs
           #:example-css-pixbufs
           #:example-css-shadows
           #:example-cursor
           #:example-custom-drawing
           #:example-custom-window
           #:example-dialogs                           ; Dialog Windows
           #:example-dialog-new
           #:example-dialog-new-with-buttons
           #:example-dialog-ui
           #:example-dialog-toplevel
           #:example-drag-and-drop
           #:example-drag-and-drop-action
           #:example-drag-and-drop-simple
           #:example-drawing-area
           #:example-drawing-area-input                ; Getting started
           #:example-event-box
           #:example-event-handler
           #:example-expander
           #:example-file-chooser-button               ; Selecting Colors, ...
           #:example-fixed                             ; Layout Widgets
           #:example-flow-box                          ; Layout Widgets
           #:example-font-button
           #:example-font-button-label                 ; Selecting Colors, ...
           #:example-frame                             ; Layout Widgets
           #:example-frame-properties                  ; Layout Widgets
           #:example-file-chooser-button
           #:create-file-chooser-custom-filter
           #:create-file-chooser-dialog
           #:create-file-chooser-preview
           #:create-file-chooser-widget
           #:example-getting-started                   ; Getting started
           #:example-grab
           #:example-grid-packing                      ; Layout Containers
           #:example-grid-simple                       ; Packing Widgets
           #:example-grid-spacing                      ; Packing Widgets
           #:example-header-bar                        ; Layout Widgets
           #:example-hello-world                       ; Getting started
           #:example-hello-world-upgraded              ; Getting started
           #:example-hello-world-upgraded-2            ; Getting started
           #:example-icon-view                         ; Tree and List Widgets
           #:example-image                             ; Display Widgets
           #:example-image-button-press
           #:example-info-bar                          ; Display Widgets
           #:example-label                             ; Display Widgets
           #:example-label-more                        ; Display Widgets
           #:example-layout
           #:example-level-bar
           #:example-link-button                       ; Button Widgets
           #:example-list-box
           #:example-menu
           #:example-menu-builder
           #:example-menu-item                         ; Menus and Toolbars
           #:example-menu-by-hand                      ; Menus and Toolbars
           #:example-menu-popup                        ; Menus and Toolbars
           #:example-message-dialog-get-message-area
           #:example-message-dialog-new
           #:example-message-dialog-new-with-markup
           #:example-message-dialog-set-image
           #:example-message-dialog-set-markup
           #:example-message-dialog-simple
           #:example-message-dialog-ui
           #:example-notebook                          ; Layout Widgets
           #:example-numerable-icon
           #:example-overlay-decorative                ; Layout Widgets
           #:example-overlay-interactive               ; Layout Widgets
           #:example-overlay-transparent               ; Layout Widgets
           #:example-paned-window                      ; Layout Widgets
           #:example-pango-drawing
           #:example-pango-drawing-animation
           #:example-pixbufs
           #:example-pixbuf-scale
           #:example-pointer-device
           #:example-print-dialog
           #:example-print-operation
           #:example-print-run-page-setup-dialog
           #:example-print-run-page-setup-dialog-async
           #:example-progress-bar                      ; Display Widgets
           #:example-popover
           #:example-query-settings
           #:example-radio-button                      ; Button Widgets
           #:example-revealer
           #:example-revealer-icon
           #:example-scale-button
           #:example-scale-widget
           #:example-scrolled-window
           #:example-search-entry
           #:example-show-about-dialog
           #:example-simple-list-store
           #:example-spin-button
           #:example-spinner
           #:example-stack                             ; Layout Widgets
           #:example-stack-sidebar                     ; Layout Widgets
           #:example-statusbar                         ; Display Widgets
           #:example-switch                            ; Button Widgets
           #:example-table-packing                     ; Deprecated
           #:example-table-packing-2                   ; Deprecated
           #:example-text-entry
           #:example-text-entry-buffer
           #:example-text-entry-completion
           #:example-text-view-attributes              ; Multiline Text Widget
           #:example-text-view-find-next               ; Multiline Text Widget
           #:example-text-view-insert                  ; Multiline Text Widget
           #:example-text-view-insert-image            ; Multiline Text Widget
           #:example-text-view-insert-widget           ; Multiline Text Widget
           #:example-text-view-search                  ; Multiline Text Widget
           #:example-text-view-simple                  ; Multiline Text Widget
           #:example-text-view-tags                    ; Multiline Text Widget
           #:example-text-view-tooltip                 ; Multiline Text Widget
           #:example-toggle-buttons                    ; Button Widgets
           #:example-tool-palette
           #:example-toolbar-by-hand
           #:example-tree-view-path                    ; Tree and List Widgets
           #:example-tree-view-simple                  ; Tree and List Widgets
           #:example-tree-view-example                 ; Tree and List Widgets
           #:example-tree-view-editable                ; Tree and List Widgets
           #:example-tree-view-sortable                ; Tree and List Widgets
           #:example-tree-view-dump-model              ; Tree and List Widgets
           #:example-tree-view-content-type            ; Tree and List Widgets
           #:example-tree-view-context-menu            ; Tree and List Widgets
           #:example-tree-view-drag-and-drop           ; Tree and List Widgets
           #:example-widget-align
           #:example-widget-pointer
           #:example-window-simple                     ; Getting started
           #:example-window-simple-demo
           #:sys-path
           ))

;;; 2025-06-01
