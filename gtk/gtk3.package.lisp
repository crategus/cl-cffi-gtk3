;;; ----------------------------------------------------------------------------
;;; gtk3.package.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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

(in-package :cl-user)

(defpackage :gtk
  (:use :iterate :common-lisp)
  (:import-from :cffi)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gio)
  (:import-from :gdk)
  (:import-from :gdk-pixbuf)
  (:import-from :pango)
  (:import-from :cairo))

(in-package :gtk)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gtk) t)
 "This is the API documentation of a Lisp binding to GTK.
  GTK is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  @begin[Application support]{section}
    @begin[GtkApplication]{subsection}
      @about-symbol{application-inhibit-flags}
      @about-class{application}
      @about-generic{application-active-window}
      @about-generic{application-app-menu}
      @about-generic{application-menubar}
      @about-generic{application-register-session}
      @about-generic{application-screensaver-active}
      @about-function{application-new}
      @about-function{application-add-window}
      @about-function{application-remove-window}
      @about-function{application-windows}
      @about-function{application-window-by-id}
      @about-function{application-inhibit}
      @about-function{application-uninhibit}
      @about-function{application-is-inhibited}
      @about-function{application-prefers-app-menu}
      @about-function{application-menu-by-id}
      @about-function{application-list-action-descriptions}
      @about-function{application-accels-for-action}
      @about-function{application-actions-for-accel}
    @end{subsection}
    @begin[GtkApplicationWindow]{subsection}
      @about-class{application-window}
      @about-generic{application-window-show-menubar}
      @about-function{application-window-new}
      @about-function{application-window-id}
      @about-function{application-window-help-overlay}
    @end{subsection}
  @end{section}
  @begin[Interface builder]{section}
    @begin[GtkBuilder]{subsection}
      @about-symbol{builder-error}
      @about-class{builder}
      @about-generic{builder-translation-domain}
      @about-function{builder-new}
      @about-function{builder-new-from-file}
      @about-function{builder-new-from-resource}
      @about-function{builder-new-from-string}
      @about-function{builder-add-callback-symbol}
      @about-function{builder-add-callback-symbols}
      @about-function{builder-lookup-callback-symbol}
      @about-function{builder-add-from-file}
      @about-function{builder-add-from-resource}
      @about-function{builder-add-from-string}
      @about-function{builder-add-objects-from-file}
      @about-function{builder-add-objects-from-string}
      @about-function{builder-add-objects-from-resource}
      @about-function{builder-extend-with-template}
      @about-function{builder-object}
      @about-function{builder-objects}
      @about-function{builder-expose-object}
      @about-symbol{builder-connect-func}
      @about-function{builder-connect-signals-full}
      @about-function{builder-connect-signals}
      @about-function{builder-application}
      @about-function{builder-type-from-name}
      @about-function{builder-value-from-string}
      @about-function{builder-value-from-string-type}
    @end{subsection}
  @end{section}
  @begin[Windows]{section}
    @begin[GtkWindow]{subsection}
      @about-symbol{window-type}
      @about-symbol{window-position}
      @about-class{window}
      @about-generic{window-accept-focus}
      @about-generic{window-application}
      @about-generic{window-attached-to}
      @about-generic{window-decorated}
      @about-generic{window-default-height}
      @about-generic{window-default-width}
      @about-generic{window-deletable}
      @about-generic{window-destroy-with-parent}
      @about-generic{window-focus-on-map}
      @about-generic{window-focus-visible}
      @about-generic{window-gravity}
      @about-generic{window-has-resize-grip}
      @about-generic{window-has-toplevel-focus}
      @about-generic{window-hide-titlebar-when-maximized}
      @about-generic{window-icon}
      @about-generic{window-icon-name}
      @about-generic{window-is-active}
      @about-generic{window-is-maximized}
      @about-generic{window-mnemonics-visible}
      @about-generic{window-modal}
      @about-generic{window-opacity}
      @about-generic{window-resizable}
      @about-generic{window-resize-grip-visible}
      @about-generic{window-role}
      @about-generic{window-screen}
      @about-generic{window-skip-pager-hint}
      @about-generic{window-skip-taskbar-hint}
      @about-generic{window-startup-id}
      @about-generic{window-title}
      @about-generic{window-transient-for}
      @about-generic{window-type}
      @about-generic{window-type-hint}
      @about-generic{window-urgency-hint}
      @about-generic{window-window-position}
      @about-function{window-new}
      @about-function{window-set-wmclass}
      @about-function{window-add-accel-group}
      @about-function{window-remove-accel-group}
      @about-function{window-activate-focus}
      @about-function{window-activate-default}
      @about-function{window-default-size}
      @about-function{window-set-default-geometry}
      @about-function{window-set-geometry-hints}
      @about-function{window-list-toplevels}
      @about-function{window-add-mnemonic}
      @about-function{window-remove-mnemonic}
      @about-function{window-mnemonic-activate}
      @about-function{window-activate-key}
      @about-function{window-propagate-key-event}
      @about-function{window-focus}
      @about-function{window-default-widget}
      @about-function{window-set-default}
      @about-function{window-present}
      @about-function{window-present-with-time}
      @about-function{window-close}
      @about-function{window-iconify}
      @about-function{window-deiconify}
      @about-function{window-stick}
      @about-function{window-unstick}
      @about-function{window-maximize}
      @about-function{window-unmaximize}
      @about-function{window-fullscreen}
      @about-function{window-fullscreen-on-monitor}
      @about-function{window-unfullscreen}
      @about-function{window-set-keep-above}
      @about-function{window-set-keep-below}
      @about-function{window-begin-resize-drag}
      @about-function{window-begin-move-drag}
      @about-function{window-mnemonic-modifier}
      @about-function{window-default-icon-list}
      @about-function{window-default-icon-name}
      @about-function{window-icon-list}
      @about-function{window-position}
      @about-function{window-size}
      @about-function{window-group}
      @about-function{window-has-group}
      @about-function{window-move}
      @about-function{window-parse-geometry}
      @about-function{window-reshow-with-initial-size}
      @about-function{window-resize}
      @about-function{window-resize-to-geometry}
      @about-function{window-set-default-icon}
      @about-function{window-set-default-icon-from-file}
      @about-function{window-set-icon-from-file}
      @about-function{window-set-auto-startup-notification}
      @about-function{window-resize-grip-is-visible}
      @about-function{window-resize-grip-area}
      @about-function{window-set-has-user-ref-count}
      @about-function{window-titlebar}
      @about-function{window-interactive-debugging}
    @end{subsection}
    @begin[GtkDialog]{subsection}
      @about-symbol{dialog-flags}
      @about-symbol{response-type}
      @about-class{dialog}
      @about-generic{dialog-use-header-bar}
      @about-function{dialog-new}
      @about-function{dialog-new-with-buttons}
      @about-function{dialog-run}
      @about-function{dialog-response}
      @about-function{dialog-add-button}
      @about-function{dialog-add-buttons}
      @about-function{dialog-add-action-widget}
      @about-function{dialog-set-default-response}
      @about-function{dialog-set-response-sensitive}
      @about-function{dialog-response-for-widget}
      @about-function{dialog-widget-for-response}
      @about-function{dialog-action-area}
      @about-function{dialog-content-area}
      @about-function{dialog-header-bar}
    @end{subsection}
    @begin[GtkMessageDialog]{subsection}
      @about-symbol{message-type}
      @about-symbol{buttons-type}
      @about-class{message-dialog}
      @about-generic{message-dialog-buttons}
      @about-generic{message-dialog-image}
      @about-generic{message-dialog-message-area}
      @about-generic{message-dialog-message-type}
      @about-generic{message-dialog-secondary-text}
      @about-generic{message-dialog-secondary-use-markup}
      @about-generic{message-dialog-text}
      @about-generic{message-dialog-use-markup}
      @about-function{message-dialog-new}
      @about-function{message-dialog-new-with-markup}
      @about-function{message-dialog-set-markup}
      @about-function{message-dialog-format-secondary-text}
      @about-function{message-dialog-format-secondary-markup}
    @end{subsection}
    @begin[GtkAboutDialog]{subsection}
      @about-symbol{license}
      @about-class{about-dialog}
      @about-generic{about-dialog-artists}
      @about-generic{about-dialog-authors}
      @about-generic{about-dialog-comments}
      @about-generic{about-dialog-copyright}
      @about-generic{about-dialog-documenters}
      @about-generic{about-dialog-license}
      @about-generic{about-dialog-license-type}
      @about-generic{about-dialog-logo}
      @about-generic{about-dialog-logo-icon-name}
      @about-generic{about-dialog-program-name}
      @about-generic{about-dialog-translator-credits}
      @about-generic{about-dialog-version}
      @about-generic{about-dialog-website}
      @about-generic{about-dialog-website-label}
      @about-generic{about-dialog-wrap-license}
      @about-function{about-dialog-new}
      @about-function{about-dialog-add-credit-section}
      @about-function{show-about-dialog}
    @end{subsection}
    @begin[GtkAssistant]{subsection}
      @about-symbol{assistant-page-type}
      @about-class{assistant}
      @about-generic{assistant-use-header-bar}
      @about-function{assistant-child-complete}
      @about-function{assistant-child-has-padding}
      @about-function{assistant-child-page-type}
      @about-function{assistant-child-title}
      @about-function{assistant-new}
      @about-function{assistant-current-page}
      @about-function{assistant-n-pages}
      @about-function{assistant-nth-page}
      @about-function{assistant-prepend-page}
      @about-function{assistant-append-page}
      @about-function{assistant-insert-page}
      @about-function{assistant-remove-page}
      @about-symbol{assistant-page-func}
      @about-function{assistant-set-forward-page-func}
      @about-function{assistant-page-type}
      @about-function{assistant-page-title}
      @about-function{assistant-set-page-header-image}
      @about-function{assistant-get-page-header-image}
      @about-function{assistant-set-page-side-image}
      @about-function{assistant-get-page-side-image}
      @about-function{assistant-page-complete}
      @about-function{assistant-page-has-padding}
      @about-function{assistant-add-action-widget}
      @about-function{assistant-remove-action-widget}
      @about-function{assistant-update-buttons-state}
      @about-function{assistant-commit}
      @about-function{assistant-next-page}
      @about-function{assistant-previous-page}
    @end{subsection}
    @begin[GtkInvisible]{subsection}
      @about-class{invisible}
      @about-generic{invisible-screen}
      @about-function{invisible-new}
      @about-function{invisible-new-for-screen}
    @end{subsection}
    @begin[GtkOffscreenWindow]{subsection}
      @about-class{offscreen-window}
      @about-function{offscreen-window-new}
      @about-function{offscreen-window-surface}
      @about-function{offscreen-window-pixbuf}
    @end{subsection}
    @begin[GtkWindowGroup]{subsection}
      @about-class{window-group}
      @about-function{window-group-new}
      @about-function{window-group-add-window}
      @about-function{window-group-remove-window}
      @about-function{window-group-list-windows}
      @about-function{window-group-current-grab}
      @about-function{window-group-current-device-grab}
    @end{subsection}
  @end{section}
  @begin[Layout Containers]{section}
    @begin[GtkBox]{subsection}
      @about-class{box}
      @about-generic{box-baseline-position}
      @about-generic{box-homogeneous}
      @about-generic{box-spacing}
      @about-function{box-child-expand}
      @about-function{box-child-fill}
      @about-function{box-child-pack-type}
      @about-function{box-child-padding}
      @about-function{box-child-position}
      @about-function{box-new}
      @about-function{box-pack-start}
      @about-function{box-pack-end}
      @about-function{box-reorder-child}
      @about-function{box-query-child-packing}
      @about-function{box-child-packing}
      @about-function{box-center-widget}
    @end{subsection}
    @begin[GtkGrid]{subsection}
      @about-class{grid}
      @about-generic{grid-baseline-row}
      @about-generic{grid-column-homogeneous}
      @about-generic{grid-column-spacing}
      @about-generic{grid-row-homogeneous}
      @about-generic{grid-row-spacing}
      @about-function{grid-child-height}
      @about-function{grid-child-left-attach}
      @about-function{grid-child-top-attach}
      @about-function{grid-child-width}
      @about-function{grid-new}
      @about-function{grid-attach}
      @about-function{grid-attach-next-to}
      @about-function{grid-child-at}
      @about-function{grid-insert-row}
      @about-function{grid-insert-column}
      @about-function{grid-remove-row}
      @about-function{grid-remove-column}
      @about-function{grid-insert-next-to}
      @about-function{grid-row-baseline-position}
    @end{subsection}
    @begin[GtkRevealer]{subsection}
      @about-symbol{revealer-transition-type}
      @about-class{revealer}
      @about-generic{revealer-child-revealed}
      @about-generic{revealer-reveal-child}
      @about-generic{revealer-transition-duration}
      @about-generic{revealer-transition-type}
      @about-function{revealer-new}
    @end{subsection}
    @begin[GtkListBox]{subsection}
      @about-class{list-box}
      @about-generic{list-box-activate-on-single-click}
      @about-generic{list-box-selection-mode}
      @about-function{list-box-new}
      @about-function{list-box-prepend}
      @about-function{list-box-insert}
      @about-function{list-box-select-row}
      @about-function{list-box-unselect-row}
      @about-function{list-box-select-all}
      @about-function{list-box-unselect-all}
      @about-function{list-box-selected-row}
      @about-symbol{list-box-foreach-func}
      @about-function{list-box-selected-foreach}
      @about-function{list-box-selected-rows}
      @about-function{list-box-adjustment}
      @about-function{list-box-set-placeholder}
      @about-function{list-box-row-at-index}
      @about-function{list-box-row-at-y}
      @about-function{list-box-invalidate-filter}
      @about-function{list-box-invalidate-headers}
      @about-function{list-box-invalidate-sort}
      @about-symbol{list-box-filter-func}
      @about-function{list-box-set-filter-func}
      @about-symbol{list-box-update-header-func}
      @about-function{list-box-set-header-func}
      @about-symbol{list-box-sort-func}
      @about-function{list-box-set-sort-func}
      @about-function{list-box-drag-highlight-row}
      @about-function{list-box-drag-unhighlight-row}
      @about-symbol{list-box-create-widget-func}
      @about-function{list-box-bind-model}
      @about-class{list-box-row}
      @about-generic{list-box-row-activatable}
      @about-generic{list-box-row-selectable}
      @about-function{list-box-row-new}
      @about-function{list-box-row-changed}
      @about-function{list-box-row-is-selected}
      @about-function{list-box-row-header}
      @about-function{list-box-row-index}
    @end{subsection}
    @begin[GtkFlowBox]{subsection}
      @about-class{flow-box}
      @about-generic{flow-box-activate-on-single-click}
      @about-generic{flow-box-column-spacing}
      @about-generic{flow-box-homogeneous}
      @about-generic{flow-box-max-children-per-line}
      @about-generic{flow-box-min-children-per-line}
      @about-generic{flow-box-row-spacing}
      @about-generic{flow-box-selection-mode}
      @about-function{flow-box-new}
      @about-function{flow-box-insert}
      @about-function{flow-box-child-at-index}
      @about-function{flow-box-child-at-pos}
      @about-function{flow-box-set-hadjustment}
      @about-function{flow-box-set-vadjustment}
      @about-symbol{flow-box-foreach-func}
      @about-function{flow-box-selected-foreach}
      @about-function{flow-box-selected-children}
      @about-function{flow-box-select-child}
      @about-function{flow-box-unselect-child}
      @about-function{flow-box-select-all}
      @about-function{flow-box-unselect-all}
      @about-symbol{flow-box-filter-func}
      @about-function{flow-box-set-filter-func}
      @about-function{flow-box-invalidate-filter}
      @about-symbol{flow-box-sort-func}
      @about-function{flow-box-set-sort-func}
      @about-function{flow-box-invalidate-sort}
      @about-symbol{flow-box-create-widget-func}
      @about-function{flow-box-bind-model}
      @about-class{flow-box-child}
      @about-function{flow-box-child-new}
      @about-function{flow-box-child-index}
      @about-function{flow-box-child-is-selected}
      @about-function{flow-box-child-changed}
    @end{subsection}
    @begin[GtkStack]{subsection}
      @about-symbol{stack-transition-type}
      @about-class{stack}
      @about-generic{stack-homogeneous}
      @about-generic{stack-hhomogeneous}
      @about-generic{stack-interpolate-size}
      @about-generic{stack-transition-duration}
      @about-generic{stack-transition-type}
      @about-generic{stack-transition-running}
      @about-generic{stack-vhomogeneous}
      @about-generic{stack-visible-child}
      @about-generic{stack-visible-child-name}
      @about-function{stack-child-icon-name}
      @about-function{stack-child-name}
      @about-function{stack-child-needs-attention}
      @about-function{stack-child-position}
      @about-function{stack-child-title}
      @about-function{stack-new}
      @about-function{stack-add-named}
      @about-function{stack-add-titled}
      @about-function{stack-child-by-name}
      @about-function{stack-set-visible-child-full}
    @end{subsection}
    @begin[GtkStackSwitcher]{subsection}
      @about-class{stack-switcher}
      @about-generic{stack-switcher-icon-size}
      @about-generic{stack-switcher-stack}
      @about-function{stack-switcher-new}
    @end{subsection}
    @begin[GtkStackSidebar]{subsection}
      @about-class{stack-sidebar}
      @about-generic{stack-sidebar-stack}
      @about-function{stack-sidebar-new}
    @end{subsection}
    @begin[GtkActionBar]{subsection}
      @about-class{action-bar}
      @about-function{action-bar-child-pack-type}
      @about-function{action-bar-child-position}
      @about-function{action-bar-new}
      @about-function{action-bar-pack-start}
      @about-function{action-bar-pack-end}
      @about-function{action-bar-center-widget}
    @end{subsection}
    @begin[GtkHeaderBar]{subsection}
      @about-class{header-bar}
      @about-generic{header-bar-custom-title}
      @about-generic{header-bar-decoration-layout}
      @about-generic{header-bar-decoration-layout-set}
      @about-generic{header-bar-has-subtitle}
      @about-generic{header-bar-show-close-button}
      @about-generic{header-bar-spacing}
      @about-generic{header-bar-subtitle}
      @about-generic{header-bar-title}
      @about-function{header-bar-child-pack-type}
      @about-function{header-bar-child-position}
      @about-function{header-bar-new}
      @about-function{header-bar-pack-start}
      @about-function{header-bar-pack-end}
    @end{subsection}
    @begin[GtkOverlay]{subsection}
      @about-class{overlay}
      @about-function{overlay-child-index}
      @about-function{overlay-child-pass-through}
      @about-function{overlay-new}
      @about-function{overlay-add-overlay}
      @about-function{overlay-reorder-overlay}
    @end{subsection}
    @begin[GtkButtonBox]{subsection}
      @about-symbol{button-box-style}
      @about-class{button-box}
      @about-generic{button-box-layout-style}
      @about-function{button-box-child-non-homogeneous}
      @about-function{button-box-child-secondary}
      @about-function{button-box-new}
      @about-function{button-box-layout}
    @end{subsection}
    @begin[GtkPaned]{subsection}
      @about-class{paned}
      @about-generic{paned-max-position}
      @about-generic{paned-min-position}
      @about-generic{paned-position}
      @about-generic{paned-position-set}
      @about-generic{paned-wide-handle}
      @about-function{paned-child-resize}
      @about-function{paned-child-shrink}
      @about-function{paned-new}
      @about-function{paned-add1}
      @about-function{paned-add2}
      @about-function{paned-pack1}
      @about-function{paned-pack2}
      @about-function{paned-child1}
      @about-function{paned-child2}
      @about-function{paned-handle-window}
    @end{subsection}
    @begin[GtkLayout]{subsection}
      @about-class{layout}
      @about-generic{layout-height}
      @about-generic{layout-width}
      @about-function{layout-child-x}
      @about-function{layout-child-y}
      @about-function{layout-new}
      @about-function{layout-put}
      @about-function{layout-move}
      @about-function{layout-size}
      @about-function{layout-bin-window}
    @end{subsection}
    @begin[GtkNotebook]{subsection}
      @about-symbol{notebook-tab}
      @about-class{notebook}
      @about-generic{notebook-enable-popup}
      @about-generic{notebook-group-name}
      @about-generic{notebook-page}
      @about-generic{notebook-scrollable}
      @about-generic{notebook-show-border}
      @about-generic{notebook-show-tabs}
      @about-generic{notebook-tab-pos}
      @about-function{notebook-child-detachable}
      @about-function{notebook-child-menu-label}
      @about-function{notebook-child-position}
      @about-function{notebook-child-reorderable}
      @about-function{notebook-child-tab-expand}
      @about-function{notebook-child-tab-fill}
      @about-function{notebook-child-tab-label}
      @about-function{notebook-new}
      @about-function{notebook-add-page}
      @about-function{notebook-append-page}
      @about-function{notebook-append-page-menu}
      @about-function{notebook-prepend-page}
      @about-function{notebook-prepend-page-menu}
      @about-function{notebook-insert-page}
      @about-function{notebook-insert-page-menu}
      @about-function{notebook-remove-page}
      @about-function{notebook-detach-tab}
      @about-function{notebook-page-num}
      @about-function{notebook-next-page}
      @about-function{notebook-prev-page}
      @about-function{notebook-reorder-child}
      @about-function{notebook-popup-enable}
      @about-function{notebook-popup-disable}
      @about-function{notebook-current-page}
      @about-function{notebook-menu-label}
      @about-function{notebook-nth-page}
      @about-function{notebook-n-pages}
      @about-function{notebook-tab-label}
      @about-function{notebook-menu-label-text}
      @about-function{notebook-tab-label-text}
      @about-function{notebook-tab-reorderable}
      @about-function{notebook-tab-detachable}
      @about-function{notebook-action-widget}
    @end{subsection}
    @begin[GtkExpander]{subsection}
      @about-class{expander}
      @about-generic{expander-expanded}
      @about-generic{expander-label}
      @about-generic{expander-label-fill}
      @about-generic{expander-label-widget}
      @about-generic{expander-resize-toplevel}
      @about-generic{expander-spacing}
      @about-generic{expander-use-markup}
      @about-generic{expander-use-underline}
      @about-function{expander-new}
      @about-function{expander-new-with-mnemonic}
    @end{subsection}
    @begin[GtkAspectFrame]{subsection}
      @about-class{aspect-frame}
      @about-generic{aspect-frame-obey-child}
      @about-generic{aspect-frame-ratio}
      @about-generic{aspect-frame-xalign}
      @about-generic{aspect-frame-yalign}
      @about-function{aspect-frame-new}
      @about-function{aspect-frame-set}
    @end{subsection}
    @begin[GtkFixed]{subsection}
      @about-class{fixed}
      @about-function{fixed-child-x}
      @about-function{fixed-child-y}
      @about-function{fixed-new}
      @about-function{fixed-put}
      @about-function{fixed-move}
    @end{subsection}
  @end{section}
  @begin[Display Widgets]{section}
    @begin[GtkLabel]{subsection}
      @about-class{label}
      @about-generic{label-angle}
      @about-generic{label-attributes}
      @about-generic{label-cursor-position}
      @about-generic{label-ellipsize}
      @about-generic{label-justify}
      @about-generic{label-label}
      @about-generic{label-lines}
      @about-generic{label-max-width-chars}
      @about-generic{label-mnemonic-keyval}
      @about-generic{label-mnemonic-widget}
      @about-generic{label-pattern}
      @about-generic{label-selectable}
      @about-generic{label-selection-bound}
      @about-generic{label-single-line-mode}
      @about-generic{label-track-visited-links}
      @about-generic{label-use-markup}
      @about-generic{label-use-underline}
      @about-generic{label-width-chars}
      @about-generic{label-wrap}
      @about-generic{label-wrap-mode}
      @about-generic{label-xalign}
      @about-generic{label-yalign}
      @about-function{label-new}
      @about-function{label-text}
      @about-function{label-set-markup}
      @about-function{label-set-markup-with-mnemonic}
      @about-function{label-line-wrap}
      @about-function{label-line-wrap-mode}
      @about-function{label-layout-offsets}
      @about-function{label-new-with-mnemonic}
      @about-function{label-select-region}
      @about-function{label-set-text-with-mnemonic}
      @about-function{label-layout}
      @about-function{label-selection-bounds}
      @about-function{label-current-uri}
    @end{subsection}
    @begin[GtkImage]{subsection}
      @about-symbol{image-type}
      @about-class{image}
      @about-generic{image-file}
      @about-generic{image-gicon}
      @about-generic{image-icon-name}
      @about-generic{image-icon-set}
      @about-generic{image-icon-size}
      @about-generic{image-pixbuf}
      @about-generic{image-pixbuf-animation}
      @about-generic{image-pixel-size}
      @about-generic{image-resource}
      @about-generic{image-stock}
      @about-generic{image-storage-type}
      @about-generic{image-surface}
      @about-generic{image-use-fallback}
      @about-function{image-get-animation}
      @about-function{image-get-icon-name}
      @about-function{image-get-gicon}
      @about-function{image-new-from-file}
      @about-function{image-new-from-pixbuf}
      @about-function{image-new-from-animation}
      @about-function{image-new-from-icon-name}
      @about-function{image-new-from-gicon}
      @about-function{image-new-from-resource}
      @about-function{image-new-from-surface}
      @about-function{image-set-from-file}
      @about-function{image-set-from-pixbuf}
      @about-function{image-set-from-animation}
      @about-function{image-set-from-icon-name}
      @about-function{image-set-from-gicon}
      @about-function{image-set-from-resource}
      @about-function{image-set-from-surface}
      @about-function{image-clear}
      @about-function{image-new}
    @end{subsection}
    @begin[GtkSpinner]{subsection}
      @about-class{spinner}
      @about-generic{spinner-active}
      @about-function{spinner-new}
      @about-function{spinner-start}
      @about-function{spinner-stop}
    @end{subsection}
    @begin[GtkInfoBar]{subsection}
      @about-class{info-bar}
      @about-generic{info-bar-message-type}
      @about-generic{info-bar-revealed}
      @about-generic{info-bar-show-close-button}
      @about-function{info-bar-new}
      @about-function{info-bar-new-with-buttons}
      @about-function{info-bar-add-action-widget}
      @about-function{info-bar-add-button}
      @about-function{info-bar-add-buttons}
      @about-function{info-bar-set-response-sensitive}
      @about-function{info-bar-set-default-response}
      @about-function{info-bar-response}
      @about-function{info-bar-action-area}
      @about-function{info-bar-content-area}
    @end{subsection}
    @begin[GtkProgressBar]{subsection}
      @about-class{progress-bar}
      @about-generic{progress-bar-ellipsize}
      @about-generic{progress-bar-fraction}
      @about-generic{progress-bar-inverted}
      @about-generic{progress-bar-pulse-step}
      @about-generic{progress-bar-show-text}
      @about-generic{progress-bar-text}
      @about-function{progress-bar-new}
      @about-function{progress-bar-pulse}
    @end{subsection}
    @begin[GtkLevelBar]{subsection}
      @about-symbol{level-bar-mode}
      @about-class{level-bar}
      @about-generic{level-bar-inverted}
      @about-generic{level-bar-max-value}
      @about-generic{level-bar-min-value}
      @about-generic{level-bar-mode}
      @about-generic{level-bar-value}
      @about-function{level-bar-new}
      @about-function{level-bar-new-for-interval}
      @about-function{level-bar-add-offset-value}
      @about-function{level-bar-remove-offset-value}
      @about-function{level-bar-offset-value}
    @end{subsection}
    @begin[GtkStatusbar]{subsection}
      @about-class{statusbar}
      @about-function{statusbar-new}
      @about-function{statusbar-context-id}
      @about-function{statusbar-push}
      @about-function{statusbar-pop}
      @about-function{statusbar-remove}
      @about-function{statusbar-remove-all}
      @about-function{statusbar-message-area}
    @end{subsection}
    @begin[GtkAccelLabel]{subsection}
      @about-class{accel-label}
      @about-generic{accel-label-accel-closure}
      @about-generic{accel-label-accel-widget}
      @about-function{accel-label-new}
      @about-function{accel-label-accel-width}
      @about-function{accel-label-set-accel}
      @about-function{accel-label-get-accel}
      @about-function{accel-label-refetch}
    @end{subsection}
  @end{section}
  @begin[Buttons and Toggles]{section}
    @begin[GtkButton]{subsection}
      @about-class{button}
      @about-generic{button-always-show-image}
      @about-generic{button-focus-on-click}
      @about-generic{button-image}
      @about-generic{button-image-position}
      @about-generic{button-label}
      @about-generic{button-relief}
      @about-generic{button-use-stock}
      @about-generic{button-use-underline}
      @about-generic{button-xalign}
      @about-generic{button-yalign}
      @about-function{button-new}
      @about-function{button-new-with-label}
      @about-function{button-new-with-mnemonic}
      @about-function{button-new-from-icon-name}
      @about-function{button-clicked}
      @about-function{button-event-window}
    @end{subsection}
    @begin[GtkCheckButton]{subsection}
      @about-class{check-button}
      @about-function{check-button-new}
      @about-function{check-button-new-with-label}
      @about-function{check-button-new-with-mnemonic}
    @end{subsection}
    @begin[GtkRadioButton]{subsection}
      @about-class{radio-button}
      @about-generic{radio-button-group}
      @about-function{radio-button-new}
      @about-function{radio-button-new-from-widget}
      @about-function{radio-button-new-with-label}
      @about-function{radio-button-new-with-label-from-widget}
      @about-function{radio-button-new-with-mnemonic}
      @about-function{radio-button-new-with-mnemonic-from-widget}
      @about-function{radio-button-set-group}
      @about-function{radio-button-get-group}
      @about-function{radio-button-join-group}
    @end{subsection}
    @begin[GtkToggleButton]{subsection}
      @about-class{toggle-button}
      @about-generic{toggle-button-active}
      @about-generic{toggle-button-draw-indicator}
      @about-generic{toggle-button-inconsistent}
      @about-function{toggle-button-new}
      @about-function{toggle-button-new-with-label}
      @about-function{toggle-button-new-with-mnemonic}
      @about-function{toggle-button-mode}
      @about-function{toggle-button-toggled}
    @end{subsection}
    @begin[GtkLinkButton]{subsection}
      @about-class{link-button}
      @about-generic{link-button-uri}
      @about-generic{link-button-visited}
      @about-function{link-button-new}
      @about-function{link-button-new-with-label}
    @end{subsection}
    @begin[GtkMenuButton]{subsection}
      @about-symbol{arrow-type}
      @about-class{menu-button}
      @about-generic{menu-button-align-widget}
      @about-generic{menu-button-direction}
      @about-generic{menu-button-menu-model}
      @about-generic{menu-button-popover}
      @about-generic{menu-button-popup}
      @about-generic{menu-button-use-popover}
      @about-function{menu-button-new}
    @end{subsection}
    @begin[GtkSwitch]{subsection}
      @about-class{switch}
      @about-generic{switch-active}
      @about-generic{switch-state}
      @about-function{switch-new}
    @end{subsection}
    @begin[GtkScaleButton]{subsection}
      @about-class{scale-button}
      @about-generic{scale-button-adjustment}
      @about-generic{scale-button-icons}
      @about-generic{scale-button-size}
      @about-generic{scale-button-value}
      @about-function{scale-button-new}
      @about-function{scale-button-popup}
      @about-function{scale-button-plus-button}
      @about-function{scale-button-minus-button}
    @end{subsection}
    @begin[GtkVolumeButton]{subsection}
      @about-class{volume-button}
      @about-generic{volume-button-use-symbolic}
      @about-function{volume-button-new}
    @end{subsection}
    @begin[GtkLockButton]{subsection}
      @about-class{lock-button}
      @about-generic{lock-button-permission}
      @about-generic{lock-button-text-lock}
      @about-generic{lock-button-text-unlock}
      @about-generic{lock-button-tooltip-lock}
      @about-generic{lock-button-tooltip-not-authorized}
      @about-generic{lock-button-tooltip-unlock}
      @about-function{lock-button-new}
    @end{subsection}
    @begin[GtkModelButton]{subsection}
      @about-symbol{button-role}
      @about-class{model-button}
      @about-generic{model-button-active}
      @about-generic{model-button-centered}
      @about-generic{model-button-icon}
      @about-generic{model-button-iconic}
      @about-generic{model-button-inverted}
      @about-generic{model-button-menu-name}
      @about-generic{model-button-role}
      @about-generic{model-button-text}
      @about-generic{model-button-use-markup}
      @about-function{model-button-new}
    @end{subsection}
  @end{section}
  @begin[Numeric and Text Data Entry]{section}
    @begin[GtkEditable]{subsection}
      @about-class{editable}
      @about-function{editable-select-region}
      @about-function{editable-selection-bounds}
      @about-function{editable-insert-text}
      @about-function{editable-delete-text}
      @about-function{editable-chars}
      @about-function{editable-cut-clipboard}
      @about-function{editable-copy-clipboard}
      @about-function{editable-paste-clipboard}
      @about-function{editable-delete-selection}
      @about-function{editable-position}
      @about-function{editable-editable}
    @end{subsection}
    @begin[GtkEntryBuffer]{subsection}
      @about-class{entry-buffer}
      @about-generic{entry-buffer-length}
      @about-generic{entry-buffer-max-length}
      @about-generic{entry-buffer-text}
      @about-function{entry-buffer-new}
      @about-function{entry-buffer-bytes}
      @about-function{entry-buffer-insert-text}
      @about-function{entry-buffer-delete-text}
      @about-function{entry-buffer-emit-deleted-text}
      @about-function{entry-buffer-emit-inserted-text}
    @end{subsection}
    @begin[GtkEntry]{subsection}
      @about-symbol{entry-icon-position}
      @about-symbol{input-purpose}
      @about-symbol{input-hints}
      @about-class{entry}
      @about-generic{entry-activates-default}
      @about-generic{entry-attributes}
      @about-generic{entry-buffer}
      @about-generic{entry-caps-lock-warning}
      @about-generic{entry-completion}
      @about-generic{entry-cursor-position}
      @about-generic{entry-editable}
      @about-generic{entry-enable-emoji-completion}
      @about-generic{entry-has-frame}
      @about-generic{entry-im-module}
      @about-generic{entry-inner-border}
      @about-generic{entry-input-hints}
      @about-generic{entry-input-purpose}
      @about-generic{entry-invisible-char}
      @about-generic{entry-invisible-char-set}
      @about-generic{entry-max-length}
      @about-generic{entry-max-width-chars}
      @about-generic{entry-overwrite-mode}
      @about-generic{entry-placeholder-text}
      @about-generic{entry-populate-all}
      @about-generic{entry-primary-icon-activatable}
      @about-generic{entry-primary-icon-gicon}
      @about-generic{entry-primary-icon-name}
      @about-generic{entry-primary-icon-pixbuf}
      @about-generic{entry-primary-icon-sensitive}
      @about-generic{entry-primary-icon-stock}
      @about-generic{entry-primary-icon-storage-type}
      @about-generic{entry-primary-icon-tooltip-markup}
      @about-generic{entry-primary-icon-tooltip-text}
      @about-generic{entry-progress-fraction}
      @about-generic{entry-progress-pulse-step}
      @about-generic{entry-scroll-offset}
      @about-generic{entry-secondary-icon-activatable}
      @about-generic{entry-secondary-icon-gicon}
      @about-generic{entry-secondary-icon-name}
      @about-generic{entry-secondary-icon-pixbuf}
      @about-generic{entry-secondary-icon-sensitive}
      @about-generic{entry-secondary-icon-stock}
      @about-generic{entry-secondary-icon-storage-type}
      @about-generic{entry-secondary-icon-tooltip-markup}
      @about-generic{entry-secondary-icon-tooltip-text}
      @about-generic{entry-selection-bound}
      @about-generic{entry-shadow-type}
      @about-generic{entry-show-emoji-icon}
      @about-generic{entry-tabs}
      @about-generic{entry-text}
      @about-generic{entry-text-length}
      @about-generic{entry-truncate-multiline}
      @about-generic{entry-visibility}
      @about-generic{entry-width-chars}
      @about-generic{entry-xalign}
      @about-function{entry-new}
      @about-function{entry-new-with-buffer}
      @about-function{entry-text-area}
      @about-function{entry-unset-invisible-char}
      @about-function{entry-alignment}
      @about-function{entry-layout}
      @about-function{entry-layout-offsets}
      @about-function{entry-layout-index-to-text-index}
      @about-function{entry-text-index-to-layout-index}
      @about-function{entry-cursor-hadjustment}
      @about-function{entry-progress-pulse}
      @about-function{entry-im-context-filter-keypress}
      @about-function{entry-reset-im-context}
      @about-function{entry-set-icon-from-pixbuf}
      @about-function{entry-set-icon-from-stock}
      @about-function{entry-set-icon-from-icon-name}
      @about-function{entry-set-icon-from-gicon}
      @about-function{entry-icon-storage-type}
      @about-function{entry-icon-pixbuf}
      @about-function{entry-icon-stock}
      @about-function{entry-icon-name}
      @about-function{entry-icon-gicon}
      @about-function{entry-icon-activatable}
      @about-function{entry-icon-sensitive}
      @about-function{entry-icon-at-pos}
      @about-function{entry-icon-tooltip-text}
      @about-function{entry-icon-tooltip-markup}
      @about-function{entry-set-icon-drag-source}
      @about-function{entry-current-icon-drag-source}
      @about-function{entry-icon-area}
      @about-function{entry-grab-focus-without-selecting}
    @end{subsection}
    @begin[GtkEntryCompletion]{subsection}
      @about-class{entry-completion}
      @about-generic{entry-completion-cell-area}
      @about-generic{entry-completion-inline-completion}
      @about-generic{entry-completion-inline-selection}
      @about-generic{entry-completion-minimum-key-length}
      @about-generic{entry-completion-model}
      @about-generic{entry-completion-popup-completion}
      @about-generic{entry-completion-popup-set-width}
      @about-generic{entry-completion-popup-single-match}
      @about-generic{entry-completion-text-column}
      @about-function{entry-completion-new}
      @about-function{entry-completion-new-with-area}
      @about-function{entry-completion-entry}
      @about-symbol{entry-completion-match-func}
      @about-function{entry-completion-set-match-func}
      @about-function{entry-completion-compute-prefix}
      @about-function{entry-completion-complete}
      @about-function{entry-completion-completion-prefix}
      @about-function{entry-completion-insert-prefix}
      @about-function{entry-completion-insert-action-text}
      @about-function{entry-completion-insert-action-markup}
      @about-function{entry-completion-delete-action}
    @end{subsection}
    @begin[GtkScale]{subsection}
      @about-class{scale}
      @about-generic{scale-digits}
      @about-generic{scale-draw-value}
      @about-generic{scale-has-origin}
      @about-generic{scale-value-pos}
      @about-function{scale-new}
      @about-function{scale-new-with-range}
      @about-function{scale-layout}
      @about-function{scale-layout-offsets}
      @about-function{scale-add-mark}
      @about-function{scale-clear-marks}
    @end{subsection}
    @begin[GtkSpinButton]{subsection}
      @about-symbol{spin-button-update-policy}
      @about-symbol{spin-type}
      @about-class{spin-button}
      @about-generic{spin-button-adjustment}
      @about-generic{spin-button-climb-rate}
      @about-generic{spin-button-digits}
      @about-generic{spin-button-numeric}
      @about-generic{spin-button-snap-to-ticks}
      @about-generic{spin-button-update-policy}
      @about-generic{spin-button-value}
      @about-generic{spin-button-wrap}
      @about-function{spin-button-configure}
      @about-function{spin-button-new}
      @about-function{spin-button-new-with-range}
      @about-function{spin-button-increments}
      @about-function{spin-button-range}
      @about-function{spin-button-value-as-int}
      @about-function{spin-button-spin}
      @about-function{spin-button-update}
    @end{subsection}
    @begin[GtkSearchEntry]{subsection}
      @about-class{search-entry}
      @about-function{search-entry-new}
      @about-function{search-entry-handle-event}
    @end{subsection}
    @begin[GtkSearchBar]{subsection}
      @about-class{search-bar}
      @about-generic{search-bar-search-mode-enabled}
      @about-generic{search-bar-show-close-button}
      @about-function{search-bar-new}
      @about-function{search-bar-connect-entry}
      @about-function{search-bar-handle-event}
    @end{subsection}
  @end{section}
  @begin[Multiline Text Editor]{section}
    @begin[Conceptual Overview]{subsection}
      GTK has a powerful framework for multiline text editing. The primary
      objects involved in the process are the @class{gtk:text-buffer} object,
      which represents the text being edited, and the @class{gtk:text-view}
      widget, a widget which can display a @class{gtk:text-buffer} object. Each
      text buffer can be displayed by any number of views.

      One of the important things to remember about text in GTK is that it is
      in the UTF-8 encoding. This means that one character can be encoded as
      multiple bytes. Character counts are usually referred to as offsets, while
      byte counts are called indexes. If you confuse these two, things will work
      fine with ASCII, but as soon as your text buffer contains multibyte
      characters, bad things will happen.

      Text in a text buffer can be marked with tags. A tag is an attribute that
      can be applied to some range of text. For example, a tag might be called
      \"bold\" and make the text inside the tag bold. However, the tag concept
      is more general than that. Tags do not have to affect appearance. They can
      instead affect the behavior of mouse and key presses, \"lock\" a range of
      text so the user cannot edit it, or countless other things. A tag is
      represented by a @class{gtk:text-tag} object. One @class{gtk:text-tag}
      object can be applied to any number of text ranges in any number of
      text buffers.

      Each tag is stored in a @class{gtk:text-tag-table} object. A tag table
      defines a set of tags that can be used together. Each text buffer has one
      tag table associated with it. Only tags from that tag table can be used
      with the text buffer. A single tag table can be shared between multiple
      text buffers, however. Tags can have names, which is convenient sometimes.
      For example, you can name your tag that makes things bold @code{\"bold\"},
      but they can also be anonymous, which is convenient if you are creating
      tags on-the-fly.

      Most text manipulation is accomplished with iterators, represented by a
      @class{gtk:text-iter} instance. An iterator represents a position between
      two characters in the text buffer. The @class{gtk:text-iter} structure is
      a structure designed to be allocated on the stack. It is guaranteed to be
      copiable by value and never contain any heap-allocated data. Iterators are
      not valid indefinitely. Whenever the text buffer is modified in a way that
      affects the number of characters in the text buffer, all outstanding
      iterators become invalid. Note that deleting 5 characters and then
      reinserting 5 still invalidates iterators, though you end up with the same
      number of characters you pass through a state with a different number.

      Because of this, iterators cannot be used to preserve positions across
      buffer modifications. To preserve a position, the @class{gtk:text-mark}
      object is ideal. You can think of a mark as an invisible cursor or
      insertion point. It floats in the text buffer, saving a position. If the
      text surrounding the mark is deleted, the mark remains in the position
      the text once occupied. If text is inserted at the mark, the mark ends up
      either to the left or to the right of the new text, depending on its
      gravity. The standard text cursor in left-to-right languages is a mark
      with right gravity, because it stays to the right of inserted text.

      Like tags, marks can be either named or anonymous. There are two marks
      built-in to the @class{gtk:text-buffer} class. These are named
      @code{\"insert\"} and @code{\"selection_bound\"} and refer to the
      insertion point and the boundary of the selection which is not the
      insertion point, respectively. If no text is selected, these two marks
      will be in the same position. You can manipulate what is selected and
      where the cursor appears by moving these marks around. If you want to
      place the cursor in response to a user action, be sure to use the
      @fun{gtk:text-buffer-place-cursor} function, which moves both at once
      without causing a temporary selection. Moving one then the other
      temporarily selects the range in between the old and new positions.

      Text buffers always contain at least one line, but may be empty, that is,
      buffers can contain zero characters. The last line in the text buffer
      never ends in a line separator (such as newline). The other lines in the
      text buffer always end in a line separator. Line separators count as
      characters when computing character counts and character offsets. Note
      that some Unicode line separators are represented with multiple bytes in
      UTF-8, and the two-character sequence @code{\"\\r\\n\"} is also
      considered a line separator.

      @subheading{Simple Example}
      A simple usage of the @class{gtk:text-view} widget might look like this:
      @begin{pre}
(defun example-text-view-simple ()
  (gtk:within-main-loop
    (let* ((window (make-instance 'gtk:window
                                  :type :toplevel
                                  :title \"Example Simple Text View\"
                                  :default-width 350))
           (view (make-instance 'gtk:text-view))
           (buffer (gtk:text-view-buffer view)))
      (g:signal-connect window \"destroy\"
          (lambda (widget)
            (declare (ignore widget))
            (let ((start (gtk:text-buffer-start-iter buffer))
                  (end (gtk:text-buffer-end-iter buffer))
                  (include-hidden-chars t))
              (print (gtk:text-buffer-get-text buffer
                                               start
                                               end
                                               include-hidden-chars))
              (terpri)
              (gtk:leave-gtk-main))))
      (setf (gtk:text-buffer-text buffer) \"Some text for the text view.\")
      (gtk:container-add window view)
      (gtk:widget-show-all window))))
      @end{pre}
      In many cases it is also convenient to first create the text buffer with
      the @fun{gtk:text-buffer-new} function, then create a text view for that
      text buffer with the @fun{gtk:text-view-new-with-buffer} function. Or you
      can change the text buffer the text view displays after the text view is
      created with the @fun{gtk:text-view-buffer} function.

      @subheading{Example of Changing Text Attributes}
      The way to affect text attributes in the @class{gtk:text-view} widget is
      to apply tags that change the attributes for a region of text. For text
      features that come from the theme - such as font and foreground color -
      use CSS to override their default values.
      @begin{pre}
(defun example-text-view-attributes ()
  (gtk:within-main-loop
    (let* ((window (make-instance 'gtk:window
                                  :type :toplevel
                                  :title \"Example Text View Attributes\"
                                  :default-width 350))
           (provider (gtk:css-provider-new))
           (view (make-instance 'gtk:text-view))
           (buffer (gtk:text-view-buffer view)))
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (setf (gtk:text-buffer-text buffer) \"Hello, this is some text.\")
      ;; Change default font and color throughout the text view
      (gtk:css-provider-load-from-data provider
                                       \"textview, text {
                                          color : Green;
                                          font : 20px Purisa; @}\")
      (gtk:style-context-add-provider (gtk:widget-style-context view)
                                      provider
                                      gtk:+priority-application+)
      ;; Change left margin throughout the text view
      (setf (gtk:text-view-left-margin view) 30)
      ;; Use a tag to change the color for just one part of the text view
      (let ((tag (gtk:text-buffer-create-tag buffer
                                             \"blue_foreground\"
                                             :foreground \"blue\"))
            (start (gtk:text-buffer-iter-at-offset buffer 7))
            (end (gtk:text-buffer-iter-at-offset buffer 12)))
        ;; Apply the tag to a region of the text in the text buffer
        (gtk:text-buffer-apply-tag buffer tag start end))
      ;; Add the text view to the window and show all
      (gtk:container-add window view)
      (gtk:widget-show-all window))))
      @end{pre}
      The GTK3 demo that comes with GTK contains more example code for the
      @class{gtk:text-view} widget.
    @end{subsection}
    @begin[GtkTextIter]{subsection}
      @about-symbol{text-search-flags}
      @about-class{text-iter}
      @about-function{text-iter-buffer}
      @about-function{text-iter-new}
      @about-function{text-iter-copy}
      @about-function{text-iter-assign}
      @about-function{text-iter-free}
      @about-function{text-iter-offset}
      @about-function{text-iter-line}
      @about-function{text-iter-line-offset}
      @about-function{text-iter-line-index}
      @about-function{text-iter-visible-line-offset}
      @about-function{text-iter-visible-line-index}
      @about-function{text-iter-char}
      @about-function{text-iter-slice}
      @about-function{text-iter-text}
      @about-function{text-iter-visible-slice}
      @about-function{text-iter-visible-text}
      @about-function{text-iter-pixbuf}
      @about-function{text-iter-marks}
      @about-function{text-iter-toggled-tags}
      @about-function{text-iter-child-anchor}
      @about-function{text-iter-starts-tag}
      @about-function{text-iter-begins-tag}
      @about-function{text-iter-ends-tag}
      @about-function{text-iter-toggles-tag}
      @about-function{text-iter-has-tag}
      @about-function{text-iter-tags}
      @about-function{text-iter-editable}
      @about-function{text-iter-can-insert}
      @about-function{text-iter-starts-word}
      @about-function{text-iter-ends-word}
      @about-function{text-iter-inside-word}
      @about-function{text-iter-starts-line}
      @about-function{text-iter-ends-line}
      @about-function{text-iter-starts-sentence}
      @about-function{text-iter-ends-sentence}
      @about-function{text-iter-inside-sentence}
      @about-function{text-iter-is-cursor-position}
      @about-function{text-iter-chars-in-line}
      @about-function{text-iter-bytes-in-line}
      @about-function{text-iter-language}
      @about-function{text-iter-is-end}
      @about-function{text-iter-is-start}
      @about-function{text-iter-move}
      @about-function{text-iter-forward-to-end}
      @about-function{text-iter-forward-to-line-end}
      @about-function{text-iter-forward-to-tag-toggle}
      @about-function{text-iter-backward-to-tag-toggle}
      @about-symbol{text-char-predicate}
      @about-function{text-iter-find-char}
      @about-function{text-iter-search}
      @about-function{text-iter-equal}
      @about-function{text-iter-compare}
      @about-function{text-iter-in-range}
      @about-function{text-iter-order}
    @end{subsection}
    @begin[GtkTextMark]{subsection}
      @about-class{text-mark}
      @about-generic{text-mark-left-gravity}
      @about-generic{text-mark-name}
      @about-function{text-mark-new}
      @about-function{text-mark-visible}
      @about-function{text-mark-deleted}
      @about-function{text-mark-buffer}
    @end{subsection}
    @begin[GtkTextBuffer]{subsection}
      @about-symbol{text-buffer-target-info}
      @about-class{text-buffer}
      @about-generic{text-buffer-copy-target-list}
      @about-generic{text-buffer-cursor-position}
      @about-generic{text-buffer-has-selection}
      @about-generic{text-buffer-paste-target-list}
      @about-generic{text-buffer-tag-table}
      @about-generic{text-buffer-text}
      @about-function{text-buffer-new}
      @about-function{text-buffer-line-count}
      @about-function{text-buffer-char-count}
      @about-function{text-buffer-insert}
      @about-function{text-buffer-insert-at-cursor}
      @about-function{text-buffer-insert-interactive}
      @about-function{text-buffer-insert-interactive-at-cursor}
      @about-function{text-buffer-insert-range}
      @about-function{text-buffer-insert-range-interactive}
      @about-function{text-buffer-insert-with-tags}
      @about-function{text-buffer-insert-with-tags-by-name}
      @about-function{text-buffer-insert-markup}
      @about-function{text-buffer-delete}
      @about-function{text-buffer-delete-interactive}
      @about-function{text-buffer-backspace}
      @about-function{text-buffer-get-text}
      @about-function{text-buffer-get-slice}
      @about-function{text-buffer-insert-pixbuf}
      @about-function{text-buffer-insert-child-anchor}
      @about-function{text-buffer-create-child-anchor}
      @about-function{text-buffer-create-mark}
      @about-function{text-buffer-move-mark}
      @about-function{text-buffer-move-mark-by-name}
      @about-function{text-buffer-add-mark}
      @about-function{text-buffer-delete-mark}
      @about-function{text-buffer-delete-mark-by-name}
      @about-function{text-buffer-mark}
      @about-function{text-buffer-get-insert}
      @about-function{text-buffer-selection-bound}
      @about-function{text-buffer-place-cursor}
      @about-function{text-buffer-select-range}
      @about-function{text-buffer-apply-tag}
      @about-function{text-buffer-remove-tag}
      @about-function{text-buffer-apply-tag-by-name}
      @about-function{text-buffer-remove-tag-by-name}
      @about-function{text-buffer-remove-all-tags}
      @about-function{text-buffer-create-tag}
      @about-function{text-buffer-iter-at-line-offset}
      @about-function{text-buffer-iter-at-offset}
      @about-function{text-buffer-iter-at-line}
      @about-function{text-buffer-iter-at-line-index}
      @about-function{text-buffer-iter-at-mark}
      @about-function{text-buffer-iter-at-child-anchor}
      @about-function{text-buffer-start-iter}
      @about-function{text-buffer-end-iter}
      @about-function{text-buffer-bounds}
      @about-function{text-buffer-modified}
      @about-function{text-buffer-delete-selection}
      @about-function{text-buffer-paste-clipboard}
      @about-function{text-buffer-copy-clipboard}
      @about-function{text-buffer-cut-clipboard}
      @about-function{text-buffer-selection-bounds}
      @about-function{text-buffer-begin-user-action}
      @about-function{text-buffer-end-user-action}
      @about-function{text-buffer-add-selection-clipboard}
      @about-function{text-buffer-remove-selection-clipboard}
      @about-symbol{text-buffer-deserialize-func}
      @about-function{text-buffer-deserialize}
      @about-function{text-buffer-deserialize-can-create-tags}
      @about-function{text-buffer-register-deserialize-format}
      @about-function{text-buffer-register-deserialize-tagset}
      @about-symbol{text-buffer-serialize-func}
      @about-function{text-buffer-serialize}
      @about-function{text-buffer-register-serialize-format}
      @about-function{text-buffer-register-serialize-tagset}
      @about-function{text-buffer-deserialize-formats}
      @about-function{text-buffer-serialize-formats}
      @about-function{text-buffer-unregister-deserialize-format}
      @about-function{text-buffer-unregister-serialize-format}
    @end{subsection}
    @begin[GtkTextTag]{subsection}
      @about-symbol{wrap-mode}
      @about-class{text-tag}
      @about-generic{text-tag-accumulative-margin}
      @about-generic{text-tag-background}
      @about-generic{text-tag-background-full-height}
      @about-generic{text-tag-background-full-height-set}
      @about-generic{text-tag-background-gdk}
      @about-generic{text-tag-background-rgba}
      @about-generic{text-tag-background-set}
      @about-generic{text-tag-direction}
      @about-generic{text-tag-editable}
      @about-generic{text-tag-editable-set}
      @about-generic{text-tag-fallback}
      @about-generic{text-tag-fallback-set}
      @about-generic{text-tag-family}
      @about-generic{text-tag-family-set}
      @about-generic{text-tag-font}
      @about-generic{text-tag-font-desc}
      @about-generic{text-tag-font-features}
      @about-generic{text-tag-font-features-set}
      @about-generic{text-tag-foreground}
      @about-generic{text-tag-foreground-gdk}
      @about-generic{text-tag-foreground-rgba}
      @about-generic{text-tag-foreground-set}
      @about-generic{text-tag-indent}
      @about-generic{text-tag-indent-set}
      @about-generic{text-tag-invisible}
      @about-generic{text-tag-invisible-set}
      @about-generic{text-tag-justification}
      @about-generic{text-tag-justification-set}
      @about-generic{text-tag-language}
      @about-generic{text-tag-language-set}
      @about-generic{text-tag-left-margin}
      @about-generic{text-tag-left-margin-set}
      @about-generic{text-tag-letter-spacing}
      @about-generic{text-tag-letter-spacing-set}
      @about-generic{text-tag-name}
      @about-generic{text-tag-paragraph-background}
      @about-generic{text-tag-paragraph-background-gdk}
      @about-generic{text-tag-paragraph-background-rgba}
      @about-generic{text-tag-paragraph-background-set}
      @about-generic{text-tag-pixels-above-lines}
      @about-generic{text-tag-pixels-above-lines-set}
      @about-generic{text-tag-pixels-below-lines}
      @about-generic{text-tag-pixels-below-lines-set}
      @about-generic{text-tag-pixels-inside-wrap}
      @about-generic{text-tag-pixels-inside-wrap-set}
      @about-generic{text-tag-right-margin}
      @about-generic{text-tag-right-margin-set}
      @about-generic{text-tag-rise}
      @about-generic{text-tag-rise-set}
      @about-generic{text-tag-scale}
      @about-generic{text-tag-scale-set}
      @about-generic{text-tag-size}
      @about-generic{text-tag-size-points}
      @about-generic{text-tag-size-set}
      @about-generic{text-tag-stretch}
      @about-generic{text-tag-stretch-set}
      @about-generic{text-tag-strikethrough}
      @about-generic{text-tag-strikethrough-rgba}
      @about-generic{text-tag-strikethrough-rgba-set}
      @about-generic{text-tag-strikethrough-set}
      @about-generic{text-tag-style}
      @about-generic{text-tag-style-set}
      @about-generic{text-tag-tabs}
      @about-generic{text-tag-tabs-set}
      @about-generic{text-tag-underline}
      @about-generic{text-tag-underline-rgba}
      @about-generic{text-tag-underline-rgba-set}
      @about-generic{text-tag-underline-set}
      @about-generic{text-tag-variant}
      @about-generic{text-tag-variant-set}
      @about-generic{text-tag-weight}
      @about-generic{text-tag-weight-set}
      @about-generic{text-tag-wrap-mode}
      @about-generic{text-tag-wrap-mode-set}
      @about-function{text-tag-new}
      @about-function{text-tag-priority}
      @about-function{text-tag-event}
      @about-function{text-tag-changed}
    @end{subsection}
    @begin[GtkTextTagTable]{subsection}
      @about-class{text-tag-table}
      @about-function{text-tag-table-new}
      @about-function{text-tag-table-add}
      @about-function{text-tag-table-remove}
      @about-function{text-tag-table-lookup}
      @about-symbol{text-tag-table-foreach-func}
      @about-function{text-tag-table-foreach}
      @about-function{text-tag-table-size}
    @end{subsection}
    @begin[GtkTextView]{subsection}
      @about-symbol{GTK_TEXT_VIEW_PRIORITY_VALIDATE}
      @about-symbol{text-view-layer}
      @about-symbol{text-window-type}
      @about-symbol{text-extend-selection}
      @about-class{text-child-anchor}
      @about-class{text-view}
      @about-generic{text-view-accepts-tab}
      @about-generic{text-view-bottom-margin}
      @about-generic{text-view-buffer}
      @about-generic{text-view-cursor-visible}
      @about-generic{text-view-editable}
      @about-generic{text-view-im-module}
      @about-generic{text-view-indent}
      @about-generic{text-view-input-hints}
      @about-generic{text-view-input-purpose}
      @about-generic{text-view-justification}
      @about-generic{text-view-left-margin}
      @about-generic{text-view-monospace}
      @about-generic{text-view-overwrite}
      @about-generic{text-view-pixels-above-lines}
      @about-generic{text-view-pixels-below-lines}
      @about-generic{text-view-pixels-inside-wrap}
      @about-generic{text-view-populate-all}
      @about-generic{text-view-right-margin}
      @about-generic{text-view-tabs}
      @about-generic{text-view-top-margin}
      @about-generic{text-view-wrap-mode}
      @about-function{text-view-new}
      @about-function{text-view-new-with-buffer}
      @about-function{text-view-scroll-to-mark}
      @about-function{text-view-scroll-to-iter}
      @about-function{text-view-scroll-mark-onscreen}
      @about-function{text-view-move-mark-onscreen}
      @about-function{text-view-place-cursor-onscreen}
      @about-function{text-view-visible-rect}
      @about-function{text-view-iter-location}
      @about-function{text-view-cursor-locations}
      @about-function{text-view-line-at-y}
      @about-function{text-view-line-yrange}
      @about-function{text-view-iter-at-location}
      @about-function{text-view-iter-at-position}
      @about-function{text-view-buffer-to-window-coords}
      @about-function{text-view-window-to-buffer-coords}
      @about-function{text-view-window}
      @about-function{text-view-window-type}
      @about-function{text-view-border-window-size}
      @about-function{text-view-move-display-line}
      @about-function{text-view-forward-display-line}
      @about-function{text-view-backward-display-line}
      @about-function{text-view-forward-display-line-end}
      @about-function{text-view-backward-display-line-start}
      @about-function{text-view-starts-display-line}
      @about-function{text-view-move-visually}
      @about-function{text-view-add-child-at-anchor}
      @about-function{text-child-anchor-new}
      @about-function{text-child-anchor-widgets}
      @about-function{text-child-anchor-deleted}
      @about-function{text-view-add-child-in-window}
      @about-function{text-view-move-child}
      @about-function{text-view-reset-cursor-blink}
      @about-function{text-view-default-attributes}
      @about-function{text-view-im-context-filter-keypress}
      @about-function{text-view-reset-im-context}
    @end{subsection}
  @end{section}
  @begin[Tree, List and Icon Grid Widgets]{section}
    Overview of the @class{gtk:tree-model} interface, the @class{gtk:tree-view}
    widget, and friends.

    @subheading{Overview}
      To create a tree or list in GTK, use the @class{gtk:tree-model} interface
      in conjunction with the @class{gtk:tree-view} widget. This widget is
      designed around a Model/View/Controller design and consists of four major
      parts:
      @begin{itemize}
        @item{The tree view widget represented by the @class{gtk:tree-view}
          widget.}
        @item{The view column represented by the @class{gtk:tree-view-column}
          object.}
        @item{The cell renderers represented by the @class{gtk:cell-renderer}
          object etc.}
        @item{The model interface represented by the @class{gtk:tree-model}
          interface.}
      @end{itemize}
      The View is composed of the first three objects, while the last is the
      Model. One of the prime benefits of the MVC design is that multiple views
      can be created of a single model. For example, a model mapping the file
      system could be created for a file manager. Many views could be created
      to display various parts of the file system, but only one copy need be
      kept in memory.

      The purpose of the cell renderers is to provide extensibility to the
      widget and to allow multiple ways of rendering the same type of data. For
      example, consider how to render a boolean variable. Should it render it
      as a string of \"True\" or \"False\", \"On\" or \"Off\", or should it be
      rendered as a checkbox?

    @subheading{Creating a model}
      GTK provides two simple models that can be used: the
      @class{gtk:list-store} object and the @class{gtk:tree-store} object.
      The @class{gtk:list-store} object is used to model list widgets, while
      the @class{gtk:tree-store} object models trees. It is possible to develop
      a new type of model, but the existing models should be satisfactory for
      all but the most specialized of situations. Creating the model is quite
      simple:
      @begin{pre}
GtkListStore *store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_BOOLEAN);
      @end{pre}
      This creates a list store with two columns: a string column and a boolean
      column. Typically the 2 is never passed directly like that; usually an
      enum is created wherein the different columns are enumerated, followed by
      a token that represents the total number of columns. The next example will
      illustrate this, only using a tree store instead of a list store. Creating
      a tree store operates almost exactly the same.
      @begin{pre}
enum
{
   TITLE_COLUMN,
   AUTHOR_COLUMN,
   CHECKED_COLUMN,
   N_COLUMNS
@};

GtkTreeStore *store = gtk_tree_store_new (N_COLUMNS,       /* Total number of columns */
                                          G_TYPE_STRING,   /* Book title              */
                                          G_TYPE_STRING,   /* Author                  */
                                          G_TYPE_BOOLEAN); /* Is checked out?         */
      @end{pre}
      Adding data to the model is done using the @fun{gtk:tree-store-set}
      function or the @fun{gtk:list-store-set} function, depending upon which
      sort of model was created. To do this, a @class{gtk:tree-iter} iterator
      must be acquired. The iterator points to the location where data will be
      added.

      Once an iterator has been acquired, the @fun{gtk:tree-store-set} function
      is used to apply data to the part of the model that the iterator points
      to. Consider the following example:
      @begin{pre}
GtkTreeIter   iter;

gtk_tree_store_append (store, &iter, NULL);  /* Acquire an iterator */

gtk_tree_store_set (store, &iter,
                    TITLE_COLUMN, \"The Principle of Reason\",
                    AUTHOR_COLUMN, \"Martin Heidegger\",
                    CHECKED_COLUMN, FALSE,
                    -1);
      @end{pre}
      Notice that the last argument is -1. This is always done because this is
      a variable-argument function and it needs to know when to stop processing
      arguments. It can be used to set the data in any or all columns in a given
      row.

      The third argument to the @fun{gtk:tree-store-append} function is the
      parent iterator. It is used to add a row to a @class{gtk:tree-store}
      object as a child of an existing row. This means that the new row will
      only be visible when its parent is visible and in its expanded state.
      Consider the following example:
      @begin{pre}
GtkTreeIter iter1;  /* Parent iter */
GtkTreeIter iter2;  /* Child iter  */

gtk_tree_store_append (store, &iter1, NULL);  /* Acquire a toplevel iterator */
gtk_tree_store_set (store, &iter1,
                    TITLE_COLUMN, \"The Art of Computer Programming\",
                    AUTHOR_COLUMN, \"Donald E. Knuth\",
                    CHECKED_COLUMN, FALSE,
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);  /* Acquire a child iterator */
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 1: Fundamental Algorithms\",
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 2: Seminumerical Algorithms\",
                    -1);

gtk_tree_store_append (store, &iter2, &iter1);
gtk_tree_store_set (store, &iter2,
                    TITLE_COLUMN, \"Volume 3: Sorting and Searching\",
                    -1);
      @end{pre}

    @subheading{Creating the view component}
      While there are several different models to choose from, there is only one
      view widget to deal with. It works with either the list or the tree store.
      Setting up a @class{gtk:tree-view} widget is not a difficult matter. It
      needs a @class{gtk:tree-model} object to know where to retrieve its data
      from.
      @begin{pre}
GtkWidget *tree;

tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));

Columns and cell renderers
      @end{pre}
      Once the @class{gtk:tree-view} widget has a model, it will need to know
      how to display the model. It does this with columns and cell renderers.

      Cell renderers are used to draw the data in the tree model in a way. There
      are a number of cell renderers that come with GTK, including the
      @class{gtk:cell-renderer-text}, @class{gtk:cell-renderer-pixbuf} and the
      @class{gtk:cell-renderer-toggle} objects. It is relatively easy to write
      a custom renderer.

      A @class{gtk:tree-view-column} object is the object that the
      @class{gtk:tree-view} widget uses to organize the vertical columns in the
      tree view. It needs to know the name of the column to label for the user,
      what type of cell renderer to use, and which piece of data to retrieve
      from the model for a given row.
      @begin{pre}
GtkCellRenderer *renderer;
GtkTreeViewColumn *column;

renderer = gtk_cell_renderer_text_new ();
column = gtk_tree_view_column_new_with_attributes (\"Author\",
                                                   renderer,
                                                   \"text\", AUTHOR_COLUMN,
                                                   NULL);
gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);
      @end{pre}
      At this point, all the steps in creating a displayable tree have been
      covered. The model is created, data is stored in it, a tree view is
      created and columns are added to it.

    @subheading{Selection handling}
      Most applications will need to not only deal with displaying data, but
      also receiving input events from users. To do this, simply get a reference
      to a selection object and connect to the @code{\"changed\"} signal.
      @begin{pre}
/* Prototype for selection handler callback */
static void tree_selection_changed_cb (GtkTreeSelection *selection, gpointer data);

/* Setup the selection handler */
GtkTreeSelection *select;

select = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree));
gtk_tree_selection_set_mode (select, GTK_SELECTION_SINGLE);
g_signal_connect (G_OBJECT (select), \"changed\",
                  G_CALLBACK (tree_selection_changed_cb),
                  NULL);
      @end{pre}
      Then to retrieve data for the row selected:
      @begin{pre}
static void
tree_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
        GtkTreeIter iter;
        GtkTreeModel *model;
        gchar *author;

        if (gtk_tree_selection_get_selected (selection, &model, &iter))
        {
                gtk_tree_model_get (model, &iter, AUTHOR_COLUMN, &author, -1);

                g_print (\"You selected a book by %s\n\", author);

                g_free (author);
        @}
@}
      @end{pre}

    @subheading{Simple Example}
      Here is a simple example of using a @class{gtk:tree-view} widget in
      context of the other widgets. It simply creates a simple model and view,
      and puts them together. Note that the model is never populated with data
      - that is left as an exercise for the reader. More information can be
      found on this in the @class{gtk:tree-model} documentation.
      @begin{pre}
enum
{
   TITLE_COLUMN,
   AUTHOR_COLUMN,
   CHECKED_COLUMN,
   N_COLUMNS
@};

void
setup_tree (void)
{
   GtkTreeStore *store;
   GtkWidget *tree;
   GtkTreeViewColumn *column;
   GtkCellRenderer *renderer;

   /* Create a model.  We are using the store model for now, though we
    * could use any other GtkTreeModel */
   store = gtk_tree_store_new (N_COLUMNS,
                               G_TYPE_STRING,
                               G_TYPE_STRING,
                               G_TYPE_BOOLEAN);

   /* custom function to fill the model with data */
   populate_tree_model (store);

   /* Create a view */
   tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (store));

   /* The view now holds a reference.  We can get rid of our own
    * reference */
   g_object_unref (G_OBJECT (store));

   /* Create a cell render and arbitrarily make it red for demonstration
    * purposes */
   renderer = gtk_cell_renderer_text_new ();
   g_object_set (G_OBJECT (renderer),
                 \"foreground\", \"red\",
                 NULL);

   /* Create a column, associating the \"text\" attribute of the
    * cell_renderer to the first column of the model */
   column = gtk_tree_view_column_new_with_attributes (\"Author\", renderer,
                                                      \"text\", AUTHOR_COLUMN,
                                                      NULL);

   /* Add the column to the view. */
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Second column.. title of the book. */
   renderer = gtk_cell_renderer_text_new ();
   column = gtk_tree_view_column_new_with_attributes (\"Title\",
                                                      renderer,
                                                      \"text\", TITLE_COLUMN,
                                                      NULL);
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Last column.. whether a book is checked out. */
   renderer = gtk_cell_renderer_toggle_new ();
   column = gtk_tree_view_column_new_with_attributes (\"Checked out\",
                                                      renderer,
                                                      \"active\", CHECKED_COLUMN,
                                                      NULL);
   gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

   /* Now we can manipulate the view just like any other GTK widget */
   ...
@}
      @end{pre}
    @begin[GtkTreeModel]{subsection}
      @about-struct{tree-iter}
      @about-function{tree-iter-stamp}
      @about-function{tree-iter-user-data}
      @about-function{tree-iter-copy}
      @about-function{tree-iter-free}
      @about-class{tree-path}
      @about-function{tree-path-new}
      @about-function{tree-path-new-first}
      @about-function{tree-path-new-from-string}
      @about-function{tree-path-new-from-indices}
      @about-function{tree-path-copy}
      @about-function{tree-path-to-string}
      @about-function{tree-path-append-index}
      @about-function{tree-path-prepend-index}
      @about-function{tree-path-depth}
      @about-function{tree-path-indices}
      @about-function{tree-path-indices-with-depth}
      @about-function{tree-path-compare}
      @about-function{tree-path-next}
      @about-function{tree-path-prev}
      @about-function{tree-path-up}
      @about-function{tree-path-down}
      @about-function{tree-path-is-ancestor}
      @about-function{tree-path-is-descendant}
      @about-class{tree-row-reference}
      @about-function{tree-row-reference-new}
      @about-function{tree-row-reference-copy}
      @about-function{tree-row-reference-new-proxy}
      @about-function{tree-row-reference-model}
      @about-function{tree-row-reference-path}
      @about-function{tree-row-reference-valid}
      @about-function{tree-row-reference-inserted}
      @about-function{tree-row-reference-deleted}
      @about-function{tree-row-reference-reordered}
      @about-symbol{tree-model-flags}
      @about-class{tree-model}
      @about-function{tree-model-flags}
      @about-function{tree-model-n-columns}
      @about-function{tree-model-column-type}
      @about-function{tree-model-iter}
      @about-function{tree-model-iter-from-string}
      @about-function{tree-model-iter-first}
      @about-function{tree-model-path}
      @about-function{tree-model-value}
      @about-function{tree-model-iter-next}
      @about-function{tree-model-iter-previous}
      @about-function{tree-model-iter-children}
      @about-function{tree-model-iter-has-child}
      @about-function{tree-model-iter-n-children}
      @about-function{tree-model-iter-nth-child}
      @about-function{tree-model-iter-parent}
      @about-function{tree-model-string-from-iter}
      @about-function{tree-model-ref-node}
      @about-function{tree-model-unref-node}
      @about-function{tree-model-get}
      @about-function{tree-model-get-valist}
      @about-symbol{tree-model-foreach-func}
      @about-function{tree-model-foreach}
      @about-function{tree-model-row-changed}
      @about-function{tree-model-row-inserted}
      @about-function{tree-model-row-has-child-toggled}
      @about-function{tree-model-row-deleted}
      @about-function{tree-model-rows-reordered}
    @end{subsection}
    @begin[GtkTreeSelection]{subsection}
      @about-class{tree-selection}
      @about-generic{tree-selection-mode}
      @about-symbol{tree-selection-func}
      @about-function{tree-selection-set-select-function}
      @about-function{tree-selection-get-select-function}
      @about-function{tree-selection-user-data}
      @about-function{tree-selection-tree-view}
      @about-function{tree-selection-selected}
      @about-symbol{tree-selection-foreach-func}
      @about-function{tree-selection-selected-foreach}
      @about-function{tree-selection-selected-rows}
      @about-function{tree-selection-count-selected-rows}
      @about-function{tree-selection-select-path}
      @about-function{tree-selection-unselect-path}
      @about-function{tree-selection-path-is-selected}
      @about-function{tree-selection-select-iter}
      @about-function{tree-selection-unselect-iter}
      @about-function{tree-selection-iter-is-selected}
      @about-function{tree-selection-select-all}
      @about-function{tree-selection-unselect-all}
      @about-function{tree-selection-select-range}
      @about-function{tree-selection-unselect-range}
    @end{subsection}
    @begin[GtkTreeViewColumn]{subsection}
      @about-symbol{tree-view-column-sizing}
      @about-class{tree-view-column}
      @about-generic{tree-view-column-alignment}
      @about-generic{tree-view-column-cell-area}
      @about-generic{tree-view-column-clickable}
      @about-generic{tree-view-column-expand}
      @about-generic{tree-view-column-fixed-width}
      @about-generic{tree-view-column-max-width}
      @about-generic{tree-view-column-min-width}
      @about-generic{tree-view-column-reorderable}
      @about-generic{tree-view-column-resizable}
      @about-generic{tree-view-column-sizing}
      @about-generic{tree-view-column-sort-column-id}
      @about-generic{tree-view-column-sort-indicator}
      @about-generic{tree-view-column-sort-order}
      @about-generic{tree-view-column-spacing}
      @about-generic{tree-view-column-title}
      @about-generic{tree-view-column-visible}
      @about-generic{tree-view-column-widget}
      @about-generic{tree-view-column-width}
      @about-generic{tree-view-column-x-offset}
      @about-function{tree-view-column-new}
      @about-function{tree-view-column-new-with-area}
      @about-function{tree-view-column-new-with-attributes}
      @about-function{tree-view-column-pack-start}
      @about-function{tree-view-column-pack-end}
      @about-function{tree-view-column-clear}
      @about-function{tree-view-column-add-attribute}
      @about-function{tree-view-column-set-attributes}
      @about-symbol{tree-cell-data-func}
      @about-function{tree-view-column-set-cell-data-func}
      @about-function{tree-view-column-clear-attributes}
      @about-function{tree-view-column-clicked}
      @about-function{tree-view-column-button}
      @about-function{tree-view-column-cell-set-cell-data}
      @about-function{tree-view-column-cell-size}
      @about-function{tree-view-column-cell-position}
      @about-function{tree-view-column-cell-is-visible}
      @about-function{tree-view-column-focus-cell}
      @about-function{tree-view-column-queue-resize}
      @about-function{tree-view-column-tree-view}
    @end{subsection}
    @begin[GtkTreeView]{subsection}
      @about-symbol{tree-view-drop-position}
      @about-symbol{tree-view-grid-lines}
      @about-class{tree-view}
      @about-generic{tree-view-activate-on-single-click}
      @about-generic{tree-view-enable-grid-lines}
      @about-generic{tree-view-enable-search}
      @about-generic{tree-view-enable-tree-lines}
      @about-generic{tree-view-expander-column}
      @about-generic{tree-view-fixed-height-mode}
      @about-generic{tree-view-headers-clickable}
      @about-generic{tree-view-headers-visible}
      @about-generic{tree-view-hover-expand}
      @about-generic{tree-view-hover-selection}
      @about-generic{tree-view-level-indentation}
      @about-generic{tree-view-model}
      @about-generic{tree-view-reorderable}
      @about-generic{tree-view-rubber-banding}
      @about-generic{tree-view-rules-hint}
      @about-generic{tree-view-search-column}
      @about-generic{tree-view-show-expanders}
      @about-generic{tree-view-tooltip-column}
      @about-function{tree-view-new}
      @about-function{tree-view-new-with-model}
      @about-function{tree-view-selection}
      @about-function{tree-view-get-hadjustment}
      @about-function{tree-view-set-hadjustment}
      @about-function{tree-view-get-vadjustment}
      @about-function{tree-view-set-vadjustment}
      @about-function{tree-view-columns-autosize}
      @about-function{tree-view-append-column}
      @about-function{tree-view-remove-column}
      @about-function{tree-view-insert-column}
      @about-function{tree-view-insert-column-with-attributes}
      @about-function{tree-view-insert-column-with-data-func}
      @about-function{tree-view-n-columns}
      @about-function{tree-view-column}
      @about-function{tree-view-columns}
      @about-function{tree-view-move-column-after}
      @about-symbol{tree-view-column-drop-func}
      @about-function{tree-view-set-column-drag-function}
      @about-function{tree-view-scroll-to-point}
      @about-function{tree-view-scroll-to-cell}
      @about-function{tree-view-set-cursor}
      @about-function{tree-view-set-cursor-on-cell}
      @about-function{tree-view-get-cursor}
      @about-function{tree-view-row-activated}
      @about-function{tree-view-expand-all}
      @about-function{tree-view-collapse-all}
      @about-function{tree-view-expand-to-path}
      @about-function{tree-view-expand-row}
      @about-function{tree-view-collapse-row}
      @about-symbol{tree-view-mapping-func}
      @about-function{tree-view-map-expanded-rows}
      @about-function{tree-view-row-expanded}
      @about-function{tree-view-path-at-pos}
      @about-function{tree-view-is-blank-at-pos}
      @about-function{tree-view-cell-area}
      @about-function{tree-view-background-area}
      @about-function{tree-view-visible-rect}
      @about-function{tree-view-visible-range}
      @about-function{tree-view-bin-window}
      @about-function{tree-view-convert-bin-window-to-tree-coords}
      @about-function{tree-view-convert-bin-window-to-widget-coords}
      @about-function{tree-view-convert-tree-to-bin-window-coords}
      @about-function{tree-view-convert-tree-to-widget-coords}
      @about-function{tree-view-convert-widget-to-bin-window-coords}
      @about-function{tree-view-convert-widget-to-tree-coords}
      @about-function{tree-view-enable-model-drag-dest}
      @about-function{tree-view-enable-model-drag-source}
      @about-function{tree-view-unset-rows-drag-source}
      @about-function{tree-view-unset-rows-drag-dest}
      @about-function{tree-view-set-drag-dest-row}
      @about-function{tree-view-get-drag-dest-row}
      @about-function{tree-view-get-dest-row-at-pos}
      @about-function{tree-view-create-row-drag-icon}
      @about-symbol{tree-view-search-equal-func}
      @about-function{tree-view-get-search-equal-func}
      @about-function{tree-view-set-search-equal-func}
      @about-function{tree-view-search-entry}
      @about-symbol{tree-view-search-position-func}
      @about-function{tree-view-get-search-position-func}
      @about-function{tree-view-set-search-position-func}
      @about-function{tree-view-set-destroy-count-func}
      @about-symbol{tree-view-row-separator-func}
      @about-function{tree-view-get-row-separator-func}
      @about-function{tree-view-set-row-separator-func}
      @about-function{tree-view-is-rubber-banding-active}
      @about-function{tree-view-grid-lines}
      @about-function{tree-view-set-tooltip-row}
      @about-function{tree-view-set-tooltip-cell}
      @about-function{tree-view-tooltip-context}
    @end{subsection}
    @begin[GtkTreeView drag and drop]{subsection}
      @about-class{tree-drag-source}
      @about-function{tree-drag-source-drag-data-delete}
      @about-function{tree-drag-source-drag-data-get}
      @about-function{tree-drag-source-row-draggable}
      @about-class{tree-drag-dest}
      @about-function{tree-drag-dest-drag-data-received}
      @about-function{tree-drag-dest-row-drop-possible}
      @about-function{tree-set-row-drag-data}
      @about-function{tree-get-row-drag-data}
    @end{subsection}
    @begin[GtkCellView]{subsection}
      @about-class{cell-view}
      @about-generic{cell-view-background}
      @about-generic{cell-view-background-gdk}
      @about-generic{cell-view-background-rgba}
      @about-generic{cell-view-background-set}
      @about-generic{cell-view-cell-area}
      @about-generic{cell-view-cell-area-context}
      @about-generic{cell-view-draw-sensitive}
      @about-generic{cell-view-fit-model}
      @about-generic{cell-view-model}
      @about-function{cell-view-new}
      @about-function{cell-view-new-with-context}
      @about-function{cell-view-new-with-text}
      @about-function{cell-view-new-with-markup}
      @about-function{cell-view-new-with-pixbuf}
      @about-function{cell-view-displayed-row}
      @about-function{cell-view-size-of-row}
      @about-function{cell-view-set-background-color}
    @end{subsection}
    @begin[GtkIconView]{subsection}
      @about-symbol{icon-view-drop-position}
      @about-class{icon-view}
      @about-generic{icon-view-activate-on-single-click}
      @about-generic{icon-view-cell-area}
      @about-generic{icon-view-column-spacing}
      @about-generic{icon-view-columns}
      @about-generic{icon-view-item-orientation}
      @about-generic{icon-view-item-padding}
      @about-generic{icon-view-item-width}
      @about-generic{icon-view-margin}
      @about-generic{icon-view-markup-column}
      @about-generic{icon-view-model}
      @about-generic{icon-view-pixbuf-column}
      @about-generic{icon-view-reorderable}
      @about-generic{icon-view-row-spacing}
      @about-generic{icon-view-selection-mode}
      @about-generic{icon-view-spacing}
      @about-generic{icon-view-text-column}
      @about-generic{icon-view-tooltip-column}
      @about-function{icon-view-new}
      @about-function{icon-view-new-with-area}
      @about-function{icon-view-new-with-model}
      @about-function{icon-view-path-at-pos}
      @about-function{icon-view-item-at-pos}
      @about-function{icon-view-convert-widget-to-bin-window-coords}
      @about-function{icon-view-set-cursor}
      @about-function{icon-view-get-cursor}
      @about-symbol{icon-view-foreach-func}
      @about-function{icon-view-selected-foreach}
      @about-function{icon-view-cell-rect}
      @about-function{icon-view-select-path}
      @about-function{icon-view-unselect-path}
      @about-function{icon-view-path-is-selected}
      @about-function{icon-view-selected-items}
      @about-function{icon-view-select-all}
      @about-function{icon-view-unselect-all}
      @about-function{icon-view-item-activated}
      @about-function{icon-view-scroll-to-path}
      @about-function{icon-view-visible-range}
      @about-function{icon-view-set-tooltip-item}
      @about-function{icon-view-set-tooltip-cell}
      @about-function{icon-view-tooltip-context}
      @about-function{icon-view-item-row}
      @about-function{icon-view-item-column}
      @about-function{icon-view-enable-model-drag-source}
      @about-function{icon-view-enable-model-drag-dest}
      @about-function{icon-view-unset-model-drag-source}
      @about-function{icon-view-unset-model-drag-dest}
      @about-function{icon-view-set-drag-dest-item}
      @about-function{icon-view-get-drag-dest-item}
      @about-function{icon-view-dest-item-at-pos}
      @about-function{icon-view-create-drag-icon}
    @end{subsection}
    @begin[GtkTreeSortable]{subsection}
      @about-variable{+default-sort-column-id+}
      @about-variable{+unsorted-sort-column-id+}
      @about-class{tree-sortable}
      @about-function{tree-sortable-sort-column-changed}
      @about-function{tree-sortable-sort-column-id}
      @about-symbol{tree-iter-compare-func}
      @about-function{tree-sortable-set-sort-func}
      @about-function{tree-sortable-set-default-sort-func}
      @about-function{tree-sortable-has-default-sort-func}
    @end{subsection}
    @begin[GtkTreeModelSort]{subsection}
      @about-class{tree-model-sort}
      @about-generic{tree-model-sort-model}
      @about-function{tree-model-sort-new-with-model}
      @about-function{tree-model-sort-convert-child-path-to-path}
      @about-function{tree-model-sort-convert-child-iter-to-iter}
      @about-function{tree-model-sort-convert-path-to-child-path}
      @about-function{tree-model-sort-convert-iter-to-child-iter}
      @about-function{tree-model-sort-reset-default-sort-func}
      @about-function{tree-model-sort-clear-cache}
      @about-function{tree-model-sort-iter-is-valid}
    @end{subsection}
    @begin[GtkTreeModelFilter]{subsection}
      @about-class{tree-model-filter}
      @about-generic{tree-model-filter-child-model}
      @about-generic{tree-model-filter-virtual-root}
      @about-function{tree-model-filter-new}
      @about-symbol{tree-model-filter-visible-func}
      @about-function{tree-model-filter-set-visible-func}
      @about-symbol{tree-model-filter-modify-func}
      @about-function{tree-model-filter-set-modify-func}
      @about-function{tree-model-filter-set-visible-column}
      @about-function{tree-model-filter-model}
      @about-function{tree-model-filter-convert-child-iter-to-iter}
      @about-function{tree-model-filter-convert-iter-to-child-iter}
      @about-function{tree-model-filter-convert-child-path-to-path}
      @about-function{tree-model-filter-convert-path-to-child-path}
      @about-function{tree-model-filter-refilter}
      @about-function{tree-model-filter-clear-cache}
    @end{subsection}
    @begin[GtkCellLayout]{subsection}
      @about-class{cell-layout}
      @about-function{cell-layout-pack-start}
      @about-function{cell-layout-pack-end}
      @about-function{cell-layout-area}
      @about-function{cell-layout-cells}
      @about-function{cell-layout-reorder}
      @about-function{cell-layout-clear}
      @about-function{cell-layout-add-attribute}
      @about-function{cell-layout-set-attributes}
      @about-function{cell-layout-clear-attributes}
      @about-symbol{cell-layout-data-func}
      @about-function{cell-layout-set-cell-data-func}
    @end{subsection}
    @begin[GtkCellArea]{subsection}
      @about-class{cell-area}
      @about-generic{cell-area-edit-widget}
      @about-generic{cell-area-edited-cell}
      @about-generic{cell-area-focus-cell}
      @about-function{cell-area-add}
      @about-function{cell-area-remove}
      @about-function{cell-area-has-renderer}
      @about-symbol{cell-callback}
      @about-function{cell-area-foreach}
      @about-symbol{cell-alloc-callback}
      @about-function{cell-area-foreach-alloc}
      @about-function{cell-area-event}
      @about-function{cell-area-render}
      @about-function{cell-area-cell-allocation}
      @about-function{cell-area-cell-at-position}
      @about-function{cell-area-create-context}
      @about-function{cell-area-copy-context}
      @about-function{cell-area-request-mode}
      @about-function{cell-area-preferred-width}
      @about-function{cell-area-preferred-height-for-width}
      @about-function{cell-area-preferred-height}
      @about-function{cell-area-preferred-width-for-height}
      @about-function{cell-area-current-path-string}
      @about-function{cell-area-apply-attributes}
      @about-function{cell-area-attribute-connect}
      @about-function{cell-area-attribute-disconnect}
      @about-function{cell-area-attribute-column}
      @about-function{cell-area-class-install-cell-property}
      @about-function{cell-area-class-find-cell-property}
      @about-function{cell-area-class-list-cell-properties}
      @about-function{cell-area-add-with-properties}
      @about-function{cell-area-cell-set}
      @about-function{cell-area-cell-get}
      @about-function{cell-area-cell-set-valist}
      @about-function{cell-area-cell-get-valist}
      @about-function{cell-area-cell-property}
      @about-function{cell-area-is-activatable}
      @about-function{cell-area-activate}
      @about-function{cell-area-focus}
      @about-function{cell-area-add-focus-sibling}
      @about-function{cell-area-remove-focus-sibling}
      @about-function{cell-area-is-focus-sibling}
      @about-function{cell-area-focus-siblings}
      @about-function{cell-area-focus-from-sibling}
      @about-function{cell-area-activate-cell}
      @about-function{cell-area-stop-editing}
      @about-function{cell-area-inner-cell-area}
      @about-function{cell-area-request-renderer}
    @end{subsection}
    @begin[GtkCellAreaBox]{subsection}
      @about-class{cell-area-box}
      @about-generic{cell-area-box-spacing}
      @about-function{cell-area-box-child-align}
      @about-function{cell-area-box-child-expand}
      @about-function{cell-area-box-child-fixed-size}
      @about-function{cell-area-box-child-pack-type}
      @about-function{cell-area-box-new}
      @about-function{cell-area-box-pack-start}
      @about-function{cell-area-box-pack-end}
    @end{subsection}
    @begin[GtkcellAreaContext]{subsection}
      @about-class{cell-area-context}
      @about-generic{cell-area-context-area}
      @about-generic{cell-area-context-minimum-height}
      @about-generic{cell-area-context-minimum-width}
      @about-generic{cell-area-context-natural-height}
      @about-generic{cell-area-context-natural-width}
      @about-function{cell-area-context-allocate}
      @about-function{cell-area-context-reset}
      @about-function{cell-area-context-preferred-width}
      @about-function{cell-area-context-preferred-height}
      @about-function{cell-area-context-preferred-height-for-width}
      @about-function{cell-area-context-preferred-width-for-height}
      @about-function{cell-area-context-allocation}
      @about-function{cell-area-context-push-preferred-width}
      @about-function{cell-area-context-push-preferred-height}
    @end{subsection}
    @begin[GtkCellRenderer]{subsection}
      @about-symbol{cell-renderer-state}
      @about-symbol{cell-renderer-mode}
      @about-class{cell-renderer}
      @about-generic{cell-renderer-cell-background}
      @about-generic{cell-renderer-cell-background-gdk}
      @about-generic{cell-renderer-cell-background-rgba}
      @about-generic{cell-renderer-cell-background-set}
      @about-generic{cell-renderer-editing}
      @about-generic{cell-renderer-height}
      @about-generic{cell-renderer-is-expanded}
      @about-generic{cell-renderer-is-expander}
      @about-generic{cell-renderer-mode}
      @about-generic{cell-renderer-sensitive}
      @about-generic{cell-renderer-visible}
      @about-generic{cell-renderer-width}
      @about-generic{cell-renderer-xalign}
      @about-generic{cell-renderer-xpad}
      @about-generic{cell-renderer-yalign}
      @about-generic{cell-renderer-ypad}
      @about-function{cell-renderer-class-set-accessible-type}
      @about-function{cell-renderer-aligned-area}
      @about-function{cell-renderer-size}
      @about-function{cell-renderer-render}
      @about-function{cell-renderer-activate}
      @about-function{cell-renderer-start-editing}
      @about-function{cell-renderer-stop-editing}
      @about-function{cell-renderer-fixed-size}
      @about-function{cell-renderer-alignment}
      @about-function{cell-renderer-padding}
      @about-function{cell-renderer-state}
      @about-function{cell-renderer-is-activatable}
      @about-function{cell-renderer-preferred-height}
      @about-function{cell-renderer-preferred-height-for-width}
      @about-function{cell-renderer-preferred-size}
      @about-function{cell-renderer-preferred-width}
      @about-function{cell-renderer-preferred-width-for-height}
      @about-function{cell-renderer-request-mode}
    @end{subsection}
    @begin[GtkCellEditable]{subsection}
      @about-class{cell-editable}
      @about-generic{cell-editable-editing-canceled}
      @about-function{cell-editable-start-editing}
      @about-function{cell-editable-editing-done}
      @about-function{cell-editable-remove-widget}
    @end{subsection}
    @begin[GtkCellRendererAccel]{subsection}
      @about-symbol{cell-renderer-accel-mode}
      @about-class{cell-renderer-accel}
      @about-generic{cell-renderer-accel-accel-key}
      @about-generic{cell-renderer-accel-accel-mode}
      @about-generic{cell-renderer-accel-accel-mods}
      @about-generic{cell-renderer-accel-keycode}
      @about-function{cell-renderer-accel-new}
    @end{subsection}
    @begin[GtkCellRendererCombo]{subsection}
      @about-class{cell-renderer-combo}
      @about-generic{cell-renderer-combo-has-entry}
      @about-generic{cell-renderer-combo-model}
      @about-generic{cell-renderer-combo-text-column}
      @about-function{cell-renderer-combo-new}
    @end{subsection}
    @begin[GtkCellRendererPixbuf]{subsection}
      @about-class{cell-renderer-pixbuf}
      @about-generic{cell-renderer-pixbuf-follow-state}
      @about-generic{cell-renderer-pixbuf-gicon}
      @about-generic{cell-renderer-pixbuf-icon-name}
      @about-generic{cell-renderer-pixbuf-pixbuf}
      @about-generic{cell-renderer-pixbuf-pixbuf-expander-closed}
      @about-generic{cell-renderer-pixbuf-pixbuf-expander-open}
      @about-generic{cell-renderer-pixbuf-stock-detail}
      @about-generic{cell-renderer-pixbuf-stock-id}
      @about-generic{cell-renderer-pixbuf-stock-size}
      @about-generic{cell-renderer-pixbuf-surface}
      @about-function{cell-renderer-pixbuf-new}
    @end{subsection}
    @begin[GtkCellRendererProgress]{subsection}
      @about-class{cell-renderer-progress}
      @about-generic{cell-renderer-progress-inverted}
      @about-generic{cell-renderer-progress-pulse}
      @about-generic{cell-renderer-progress-text}
      @about-generic{cell-renderer-progress-text-xalign}
      @about-generic{cell-renderer-progress-text-yalign}
      @about-generic{cell-renderer-progress-value}
      @about-function{cell-renderer-progress-new}
    @end{subsection}
    @begin[GtkCellRendererSpin]{subsection}
      @about-class{cell-renderer-spin}
      @about-generic{cell-renderer-spin-adjustment}
      @about-generic{cell-renderer-spin-climb-rate}
      @about-generic{cell-renderer-spin-digits}
      @about-function{cell-renderer-spin-new}
    @end{subsection}
    @begin[GtkCellRendererText]{subsection}
      @about-class{cell-renderer-text}
      @about-generic{cell-renderer-text-align-set}
      @about-generic{cell-renderer-text-alignment}
      @about-generic{cell-renderer-text-attributes}
      @about-generic{cell-renderer-text-background}
      @about-generic{cell-renderer-text-background-gdk}
      @about-generic{cell-renderer-text-background-rgba}
      @about-generic{cell-renderer-text-background-set}
      @about-generic{cell-renderer-text-editable}
      @about-generic{cell-renderer-text-editable-set}
      @about-generic{cell-renderer-text-ellipsize}
      @about-generic{cell-renderer-text-ellipsize-set}
      @about-generic{cell-renderer-text-family}
      @about-generic{cell-renderer-text-family-set}
      @about-generic{cell-renderer-text-font}
      @about-generic{cell-renderer-text-font-desc}
      @about-generic{cell-renderer-text-foreground}
      @about-generic{cell-renderer-text-foreground-gdk}
      @about-generic{cell-renderer-text-foreground-rgba}
      @about-generic{cell-renderer-text-foreground-set}
      @about-generic{cell-renderer-text-language}
      @about-generic{cell-renderer-text-language-set}
      @about-generic{cell-renderer-text-markup}
      @about-generic{cell-renderer-text-max-width-chars}
      @about-generic{cell-renderer-text-placeholder-text}
      @about-generic{cell-renderer-text-rise}
      @about-generic{cell-renderer-text-rise-set}
      @about-generic{cell-renderer-text-scale}
      @about-generic{cell-renderer-text-scale-set}
      @about-generic{cell-renderer-text-single-paragraph-mode}
      @about-generic{cell-renderer-text-size}
      @about-generic{cell-renderer-text-size-points}
      @about-generic{cell-renderer-text-size-set}
      @about-generic{cell-renderer-text-stretch}
      @about-generic{cell-renderer-text-stretch-set}
      @about-generic{cell-renderer-text-strikethrough}
      @about-generic{cell-renderer-text-strikethrough-set}
      @about-generic{cell-renderer-text-style}
      @about-generic{cell-renderer-text-style-set}
      @about-generic{cell-renderer-text-text}
      @about-generic{cell-renderer-text-underline}
      @about-generic{cell-renderer-text-underline-set}
      @about-generic{cell-renderer-text-variant}
      @about-generic{cell-renderer-text-variant-set}
      @about-generic{cell-renderer-text-weight}
      @about-generic{cell-renderer-text-weight-set}
      @about-generic{cell-renderer-text-width-chars}
      @about-generic{cell-renderer-text-wrap-mode}
      @about-generic{cell-renderer-text-wrap-width}
      @about-function{cell-renderer-text-new}
      @about-function{cell-renderer-text-set-fixed-height-from-font}
    @end{subsection}
    @begin[GtkCellRendererToggle]{subsection}
      @about-class{cell-renderer-toggle}
      @about-generic{cell-renderer-toggle-activatable}
      @about-generic{cell-renderer-toggle-active}
      @about-generic{cell-renderer-toggle-inconsistent}
      @about-generic{cell-renderer-toggle-indicator-size}
      @about-generic{cell-renderer-toggle-radio}
      @about-function{cell-renderer-toggle-new}
    @end{subsection}
    @begin[GtkCellRendererSpinner]{subsection}
      @about-class{cell-renderer-spinner}
      @about-generic{cell-renderer-spinner-active}
      @about-generic{cell-renderer-spinner-pulse}
      @about-generic{cell-renderer-spinner-size}
      @about-function{cell-renderer-spinner-new}
    @end{subsection}
    @begin[GtkListStore]{subsection}
      @about-class{list-store}
      @about-function{list-store-new}
      @about-function{list-store-newv}
      @about-function{list-store-set-column-types}
      @about-function{list-store-set}
      @about-function{list-store-set-valist}
      @about-function{list-store-set-value}
      @about-function{list-store-set-valuesv}
      @about-function{list-store-remove}
      @about-function{list-store-insert}
      @about-function{list-store-insert-before}
      @about-function{list-store-insert-after}
      @about-function{list-store-insert-with-values}
      @about-function{list-store-insert-with-valuesv}
      @about-function{list-store-prepend}
      @about-function{list-store-append}
      @about-function{list-store-clear}
      @about-function{list-store-iter-is-valid}
      @about-function{list-store-reorder}
      @about-function{list-store-swap}
      @about-function{list-store-move-before}
      @about-function{list-store-move-after}
    @end{subsection}
    @begin[GtkTreeStore]{subsection}
      @about-class{tree-store}
      @about-function{tree-store-new}
      @about-function{tree-store-newv}
      @about-function{tree-store-set-column-types}
      @about-function{tree-store-set}
      @about-function{tree-store-set-valist}
      @about-function{tree-store-set-valuesv}
      @about-function{tree-store-set-value}
      @about-function{tree-store-remove}
      @about-function{tree-store-insert}
      @about-function{tree-store-insert-before}
      @about-function{tree-store-insert-after}
      @about-function{tree-store-insert-with-values}
      @about-function{tree-store-insert-with-valuesv}
      @about-function{tree-store-prepend}
      @about-function{tree-store-append}
      @about-function{tree-store-is-ancestor}
      @about-function{tree-store-iter-depth}
      @about-function{tree-store-clear}
      @about-function{tree-store-iter-is-valid}
      @about-function{tree-store-reorder}
      @about-function{tree-store-swap}
      @about-function{tree-store-move-before}
      @about-function{tree-store-move-after}
    @end{subsection}
  @end{section}
  @begin[Combo Box]{section}
    @begin[GtkComboBox]{subsection}
      @about-class{combo-box}
      @about-generic{combo-box-active}
      @about-generic{combo-box-active-id}
      @about-generic{combo-box-add-tearoffs}
      @about-generic{combo-box-button-sensitivity}
      @about-generic{combo-box-cell-area}
      @about-generic{combo-box-column-span-column}
      @about-generic{combo-box-entry-text-column}
      @about-generic{combo-box-focus-on-click}
      @about-generic{combo-box-has-entry}
      @about-generic{combo-box-has-frame}
      @about-generic{combo-box-id-column}
      @about-generic{combo-box-model}
      @about-generic{combo-box-popup-fixed-width}
      @about-generic{combo-box-popup-shown}
      @about-generic{combo-box-row-span-column}
      @about-generic{combo-box-tearoff-title}
      @about-generic{combo-box-wrap-width}
      @about-function{combo-box-new}
      @about-function{combo-box-new-with-entry}
      @about-function{combo-box-new-with-model}
      @about-function{combo-box-new-with-model-and-entry}
      @about-function{combo-box-new-with-area}
      @about-function{combo-box-new-with-area-and-entry}
      @about-function{combo-box-active-iter}
      @about-function{combo-box-popup-for-device}
      @about-function{combo-box-popup}
      @about-function{combo-box-popdown}
      @about-function{combo-box-popup-accessible}
      @about-function{combo-box-get-row-separator-func}
      @about-function{combo-box-set-row-separator-func}
    @end{subsection}
    @begin[GtkComboBoxText]{subsection}
      @about-class{combo-box-text}
      @about-function{combo-box-text-new}
      @about-function{combo-box-text-new-with-entry}
      @about-function{combo-box-text-append}
      @about-function{combo-box-text-prepend}
      @about-function{combo-box-text-insert}
      @about-function{combo-box-text-append-text}
      @about-function{combo-box-text-prepend-text}
      @about-function{combo-box-text-insert-text}
      @about-function{combo-box-text-remove}
      @about-function{combo-box-text-remove-all}
      @about-function{combo-box-text-active-text}
    @end{subsection}
  @end{section}
  @begin[Menus]{section}
    @begin[GtkMenuShell]{subsection}
      @about-symbol{menu-direction-type}
      @about-class{menu-shell}
      @about-generic{menu-shell-take-focus}
      @about-function{menu-shell-append}
      @about-function{menu-shell-prepend}
      @about-function{menu-shell-insert}
      @about-function{menu-shell-deactivate}
      @about-function{menu-shell-select-item}
      @about-function{menu-shell-select-first}
      @about-function{menu-shell-deselect}
      @about-function{menu-shell-activate-item}
      @about-function{menu-shell-cancel}
      @about-function{menu-shell-selected-item}
      @about-function{menu-shell-parent-shell}
      @about-function{menu-shell-bind-model}
    @end{subsection}
    @begin[GtkMenu]{subsection}
      @about-symbol{arrow-placement}
      @about-class{menu}
      @about-generic{menu-accel-group}
      @about-generic{menu-accel-path}
      @about-generic{menu-active}
      @about-generic{menu-anchor-hints}
      @about-generic{menu-attach-widget}
      @about-generic{menu-menu-type-hint}
      @about-generic{menu-monitor}
      @about-generic{menu-rect-anchor-dx}
      @about-generic{menu-rect-anchor-dy}
      @about-generic{menu-reserve-toggle-size}
      @about-generic{menu-tearoff-state}
      @about-generic{menu-tearoff-title}
      @about-function{menu-child-bottom-attach}
      @about-function{menu-child-left-attach}
      @about-function{menu-child-right-attach}
      @about-function{menu-child-top-attach}
      @about-function{menu-new}
      @about-function{menu-new-from-model}
      @about-function{menu-set-screen}
      @about-function{menu-reorder-child}
      @about-function{menu-attach}
      @about-function{menu-popup-at-rect}
      @about-function{menu-popup-at-widget}
      @about-function{menu-popup-at-pointer}
      @about-symbol{menu-position-func}
      @about-function{menu-popup-for-device}
      @about-function{menu-popup}
      @about-function{menu-place-on-monitor}
      @about-function{menu-popdown}
      @about-function{menu-reposition}
      @about-function{menu-attach-to-widget}
      @about-function{menu-detach}
      @about-function{menu-for-attach-widget}
    @end{subsection}
    @begin[GtkMenuBar]{subsection}
      @about-symbol{pack-direction}
      @about-class{menu-bar}
      @about-generic{menu-bar-child-pack-direction}
      @about-generic{menu-bar-pack-direction}
      @about-function{menu-bar-new}
      @about-function{menu-bar-new-from-model}
    @end{subsection}
    @begin[GtkMenuItem]{subsection}
      @about-class{menu-item}
      @about-generic{menu-item-accel-path}
      @about-generic{menu-item-label}
      @about-generic{menu-item-right-justified}
      @about-generic{menu-item-submenu}
      @about-generic{menu-item-use-underline}
      @about-function{menu-item-new}
      @about-function{menu-item-new-with-label}
      @about-function{menu-item-new-with-mnemonic}
      @about-function{menu-item-select}
      @about-function{menu-item-deselect}
      @about-function{menu-item-activate}
      @about-function{menu-item-toggle-size-request}
      @about-function{menu-item-toggle-size-allocate}
      @about-function{menu-item-reserve-indicator}
    @end{subsection}
    @begin[GtkCheckMenuItem]{subsection}
      @about-class{check-menu-item}
      @about-generic{check-menu-item-active}
      @about-generic{check-menu-item-draw-as-radio}
      @about-generic{check-menu-item-inconsistent}
      @about-function{check-menu-item-new}
      @about-function{check-menu-item-new-with-label}
      @about-function{check-menu-item-new-with-mnemonic}
      @about-function{check-menu-item-get-active}
      @about-function{check-menu-item-set-active}
      @about-function{check-menu-item-toggled}
      @about-function{check-menu-item-get-inconsistent}
      @about-function{check-menu-item-set-inconsistent}
      @about-function{check-menu-item-set-draw-as-radio}
      @about-function{check-menu-item-get-draw-as-radio}
    @end{subsection}
    @begin[GtkRadioMenuItem]{subsection}
      @about-class{radio-menu-item}
      @about-generic{radio-menu-item-group}
      @about-function{radio-menu-item-new}
      @about-function{radio-menu-item-new-with-label}
      @about-function{radio-menu-item-new-with-mnemonic}
      @about-function{radio-menu-item-new-from-widget}
      @about-function{radio-menu-item-new-with-label-from-widget}
      @about-function{radio-menu-item-new-with-mnemonic-from-widget}
      @about-function{radio-menu-item-set-group}
      @about-function{radio-menu-item-get-group}
      @about-function{radio-menu-item-join-group}
    @end{subsection}
    @begin[GtkSeparatorMenuItem]{subsection}
      @about-class{separator-menu-item}
      @about-function{separator-menu-item-new}
    @end{subsection}
  @end{section}
  @begin[Toolbar]{section}
    @begin[GtkToolShell]{subsection}
      @about-class{tool-shell}
      @about-function{tool-shell-ellipsize-mode}
      @about-function{tool-shell-icon-size}
      @about-function{tool-shell-orientation}
      @about-function{tool-shell-relief-style}
      @about-function{tool-shell-style}
      @about-function{tool-shell-text-alignment}
      @about-function{tool-shell-text-orientation}
      @about-function{tool-shell-rebuild-menu}
      @about-function{tool-shell-text-size-group}
    @end{subsection}
    @begin[GtkToolbar]{subsection}
      @about-symbol{toolbar-space-style}
      @about-class{toolbar}
      @about-generic{toolbar-icon-size}
      @about-generic{toolbar-icon-size-set}
      @about-generic{toolbar-show-arrow}
      @about-generic{toolbar-toolbar-style}
      @about-function{toolbar-child-expand}
      @about-function{toolbar-child-homogeneous}
      @about-function{toolbar-new}
      @about-function{toolbar-insert}
      @about-function{toolbar-item-index}
      @about-function{toolbar-n-items}
      @about-function{toolbar-nth-item}
      @about-function{toolbar-drop-index}
      @about-function{toolbar-set-drop-highlight-item}
      @about-function{toolbar-unset-icon-size}
      @about-function{toolbar-relief-style}
      @about-function{toolbar-unset-style}
    @end{subsection}
    @begin[GtkToolItem]{subsection}
      @about-class{tool-item}
      @about-generic{tool-item-is-important}
      @about-generic{tool-item-visible-horizontal}
      @about-generic{tool-item-visible-vertical}
      @about-function{tool-item-new}
      @about-function{tool-item-homogeneous}
      @about-function{tool-item-expand}
      @about-function{tool-item-set-tooltip-text}
      @about-function{tool-item-set-tooltip-markup}
      @about-function{tool-item-use-drag-window}
      @about-function{tool-item-ellipsize-mode}
      @about-function{tool-item-icon-size}
      @about-function{tool-item-orientation}
      @about-function{tool-item-toolbar-style}
      @about-function{tool-item-relief-style}
      @about-function{tool-item-text-alignment}
      @about-function{tool-item-text-orientation}
      @about-function{tool-item-retrieve-proxy-menu-item}
      @about-function{tool-item-proxy-menu-item}
      @about-function{tool-item-rebuild-menu}
      @about-function{tool-item-toolbar-reconfigured}
      @about-function{tool-item-text-size-group}
    @end{subsection}
    @begin[GtkToolPalette]{subsection}
      @about-symbol{tool-palette-drag-targets}
      @about-class{tool-palette}
      @about-generic{tool-palette-icon-size}
      @about-generic{tool-palette-icon-size-set}
      @about-generic{tool-palette-toolbar-style}
      @about-function{tool-palette-child-exclusive}
      @about-function{tool-palette-child-expand}
      @about-function{tool-palette-new}
      @about-function{tool-palette-group-position}
      @about-function{tool-palette-unset-icon-size}
      @about-function{tool-palette-unset-style}
      @about-function{tool-palette-add-drag-dest}
      @about-function{tool-palette-drag-item}
      @about-function{tool-palette-drag-target-group}
      @about-function{tool-palette-drag-target-item}
      @about-function{tool-palette-drop-group}
      @about-function{tool-palette-drop-item}
      @about-function{tool-palette-set-drag-source}
    @end{subsection}
    @begin[GtkToolItemGroup]{subsection}
      @about-class{tool-item-group}
      @about-generic{tool-item-group-collapsed}
      @about-generic{tool-item-group-ellipsize}
      @about-generic{tool-item-group-header-relief}
      @about-generic{tool-item-group-label}
      @about-generic{tool-item-group-label-widget}
      @about-function{tool-item-group-child-expand}
      @about-function{tool-item-group-child-fill}
      @about-function{tool-item-group-child-homogeneous}
      @about-function{tool-item-group-child-new-row}
      @about-function{tool-item-group-child-position}
      @about-function{tool-item-group-new}
      @about-function{tool-item-group-drop-item}
      @about-function{tool-item-group-n-items}
      @about-function{tool-item-group-nth-item}
      @about-function{tool-item-group-insert}
    @end{subsection}
    @begin[GtkSeparatorToolItem]{subsection}
      @about-class{separator-tool-item}
      @about-generic{separator-tool-item-draw}
      @about-function{separator-tool-item-new}
    @end{subsection}
    @begin[GtkToolButton]{subsection}
      @about-class{tool-button}
      @about-generic{tool-button-icon-name}
      @about-generic{tool-button-icon-widget}
      @about-generic{tool-button-label}
      @about-generic{tool-button-label-widget}
      @about-generic{tool-button-stock-id}
      @about-generic{tool-button-use-underline}
      @about-function{tool-button-new}
      @about-function{tool-button-new-from-stock}
    @end{subsection}
    @begin[GtkMenuToolButton]{subsection}
      @about-class{menu-tool-button}
      @about-generic{menu-tool-button-menu}
      @about-function{menu-tool-button-new}
      @about-function{menu-tool-button-new-from-stock}
      @about-function{menu-tool-button-set-arrow-tooltip-text}
      @about-function{menu-tool-button-set-arrow-tooltip-markup}
    @end{subsection}
    @begin[GtkToggleToolButton]{subsection}
      @about-class{toggle-tool-button}
      @about-generic{toggle-tool-button-active}
      @about-function{toggle-tool-button-new}
      @about-function{toggle-tool-button-new-from-stock}
    @end{subsection}
    @begin[GtkRadioToolButton]{subsection}
      @about-class{radio-tool-button}
      @about-generic{radio-tool-button-group}
      @about-function{radio-tool-button-new}
      @about-function{radio-tool-button-new-from-stock}
      @about-function{radio-tool-button-new-from-widget}
      @about-function{radio-tool-button-new-with-stock-from-widget}
      @about-function{radio-tool-button-get-group}
      @about-function{radio-tool-button-set-group}
    @end{subsection}
  @end{section}
  @begin[Popover]{section}
    @begin[GtkPopover]{subsection}
      @about-symbol{popover-constraint}
      @about-class{popover}
      @about-generic{popover-constrain-to}
      @about-generic{popover-modal}
      @about-generic{popover-pointing-to}
      @about-generic{popover-position}
      @about-generic{popover-relative-to}
      @about-generic{popover-transitions-enabled}
      @about-function{popover-new}
      @about-function{popover-new-from-model}
      @about-function{popover-bind-model}
      @about-function{popover-popup}
      @about-function{popover-popdown}
      @about-function{popover-default-widget}
    @end{subsection}
    @begin[GtkPopoverMenu]{subsection}
      @about-class{popover-menu}
      @about-generic{popover-menu-visible-submenu}
      @about-function{popover-menu-child-position}
      @about-function{popover-menu-child-submenu}
      @about-function{popover-menu-new}
      @about-function{popover-menu-open-submenu}
    @end{subsection}
  @end{section}
  @begin[Selectors (Color, File and Font)]{section}
    @begin[GtkColorChooser]{subsection}
      @about-class{color-chooser}
      @about-generic{color-chooser-rgba}
      @about-generic{color-chooser-use-alpha}
      @about-function{color-chooser-add-palette}
    @end{subsection}
    @begin[GtkColorButton]{subsection}
      @about-class{color-button}
      @about-generic{color-button-alpha}
      @about-generic{color-button-color}
      @about-generic{color-button-rgba}
      @about-generic{color-button-show-editor}
      @about-generic{color-button-title}
      @about-generic{color-button-use-alpha}
      @about-function{color-button-new}
      @about-function{color-button-new-with-color}
      @about-function{color-button-new-with-rgba}
    @end{subsection}
    @begin[GtkColorChooserWidget]{subsection}
      @about-class{color-chooser-widget}
      @about-generic{color-chooser-widget-show-editor}
      @about-function{color-chooser-widget-new}
    @end{subsection}
    @begin[GtkColorChooserDialog]{subsection}
      @about-class{color-chooser-dialog}
      @about-generic{color-chooser-dialog-show-editor}
      @about-function{color-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFileFilter]{subsection}
      @about-symbol{file-filter-flags}
      @about-symbol{file-filter-info}
      @about-function{file-filter-info-contains}
      @about-function{file-filter-info-filename}
      @about-function{file-filter-info-uri}
      @about-function{file-filter-info-display-name}
      @about-function{file-filter-info-mime-type}
      @about-class{file-filter}
      @about-function{file-filter-new}
      @about-function{file-filter-name}
      @about-function{file-filter-add-mime-type}
      @about-function{file-filter-add-pattern}
      @about-function{file-filter-add-pixbuf-formats}
      @about-symbol{file-filter-func}
      @about-function{file-filter-add-custom}
      @about-function{file-filter-needed}
      @about-function{file-filter-filter}
      @about-function{file-filter-new-from-gvariant}
      @about-function{file-filter-to-gvariant}
    @end{subsection}
    @begin[GtkFileChooser]{subsection}
      @about-symbol{file-chooser-action}
      @about-symbol{file-chooser-confirmation}
      @about-symbol{GTK_FILE_CHOOSER_ERROR}
      @about-symbol{file-chooser-error}
      @about-class{file-chooser}
      @about-generic{file-chooser-action}
      @about-generic{file-chooser-create-folders}
      @about-generic{file-chooser-do-overwrite-confirmation}
      @about-generic{file-chooser-extra-widget}
      @about-generic{file-chooser-filter}
      @about-generic{file-chooser-local-only}
      @about-generic{file-chooser-preview-widget}
      @about-generic{file-chooser-preview-widget-active}
      @about-generic{file-chooser-select-multiple}
      @about-generic{file-chooser-show-hidden}
      @about-generic{file-chooser-use-preview-label}
      @about-function{file-chooser-current-name}
      @about-function{file-chooser-filename}
      @about-function{file-chooser-select-filename}
      @about-function{file-chooser-unselect-filename}
      @about-function{file-chooser-select-all}
      @about-function{file-chooser-unselect-all}
      @about-function{file-chooser-filenames}
      @about-function{file-chooser-current-folder}
      @about-function{file-chooser-uri}
      @about-function{file-chooser-select-uri}
      @about-function{file-chooser-unselect-uri}
      @about-function{file-chooser-uris}
      @about-function{file-chooser-current-folder-uri}
      @about-function{file-chooser-preview-filename}
      @about-function{file-chooser-preview-uri}
      @about-function{file-chooser-add-filter}
      @about-function{file-chooser-remove-filter}
      @about-function{file-chooser-list-filters}
      @about-function{file-chooser-add-shortcut-folder}
      @about-function{file-chooser-remove-shortcut-folder}
      @about-function{file-chooser-list-shortcut-folders}
      @about-function{file-chooser-add-shortcut-folder-uri}
      @about-function{file-chooser-remove-shortcut-folder-uri}
      @about-function{file-chooser-list-shortcut-folder-uris}
      @about-function{file-chooser-current-folder-file}
      @about-function{file-chooser-file}
      @about-function{file-chooser-files}
      @about-function{file-chooser-preview-file}
      @about-function{file-chooser-select-file}
      @about-function{file-chooser-unselect-file}
    @end{subsection}
    @begin[GtkFileChooserButton]{subsection}
      @about-class{file-chooser-button}
      @about-generic{file-chooser-button-dialog}
      @about-generic{file-chooser-button-focus-on-click}
      @about-generic{file-chooser-button-title}
      @about-generic{file-chooser-button-width-chars}
      @about-function{file-chooser-button-new}
      @about-function{file-chooser-button-new-with-dialog}
    @end{subsection}
    @begin[GtkFileChooserNative]{subsection}
      @about-class{file-chooser-native}
      @about-generic{file-chooser-native-accept-label}
      @about-generic{file-chooser-native-cancel-label}
      @about-function{file-chooser-native-new}
    @end{subsection}
    @begin[GtkFileChooserDialog]{subsection}
      @about-class{file-chooser-dialog}
      @about-function{file-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFileChooserWidget]{subsection}
      @about-class{file-chooser-widget}
      @about-generic{file-chooser-widget-search-mode}
      @about-generic{file-chooser-widget-subtitle}
      @about-function{file-chooser-widget-new}
    @end{subsection}
    @begin[GtkFontChooser]{subsection}
      @about-symbol{font-chooser-level}
      @about-class{font-chooser}
      @about-generic{font-chooser-font}
      @about-generic{font-chooser-font-desc}
      @about-generic{font-chooser-font-features}
      @about-generic{font-chooser-language}
      @about-generic{font-chooser-level}
      @about-generic{font-chooser-preview-text}
      @about-generic{font-chooser-show-preview-entry}
      @about-function{font-chooser-font-family}
      @about-function{font-chooser-font-face}
      @about-function{font-chooser-font-size}
      @about-symbol{font-filter-func}
      @about-function{font-chooser-set-filter-func}
      @about-function{font-chooser-font-map}
    @end{subsection}
    @begin[GtkFontButton]{subsection}
      @about-class{font-button}
      @about-generic{font-button-font-name}
      @about-generic{font-button-show-size}
      @about-generic{font-button-show-style}
      @about-generic{font-button-title}
      @about-generic{font-button-use-font}
      @about-generic{font-button-use-size}
      @about-function{font-button-new}
      @about-function{font-button-new-with-font}
    @end{subsection}
    @begin[GtkFontChooserWidget]{subsection}
      @about-class{font-chooser-widget}
      @about-generic{font-chooser-widget-tweak-action}
      @about-function{font-chooser-widget-new}
    @end{subsection}
    @begin[GtkFontChooserDialog]{subsection}
      @about-class{font-chooser-dialog}
      @about-function{font-chooser-dialog-new}
    @end{subsection}
    @begin[GtkPlacesSidebar]{subsection}
      @about-symbol{places-open-flags}
      @about-class{places-sidebar}
      @about-generic{places-sidebar-local-only}
      @about-generic{places-sidebar-location}
      @about-generic{places-sidebar-open-flags}
      @about-generic{places-sidebar-populate-all}
      @about-generic{places-sidebar-show-connect-to-server}
      @about-generic{places-sidebar-show-desktop}
      @about-generic{places-sidebar-show-enter-location}
      @about-generic{places-sidebar-show-other-locations}
      @about-generic{places-sidebar-show-recent}
      @about-generic{places-sidebar-show-starred-location}
      @about-generic{places-sidebar-show-trash}
      @about-function{places-sidebar-new}
      @about-function{places-sidebar-add-shortcut}
      @about-function{places-sidebar-remove-shortcut}
      @about-function{places-sidebar-list-shortcuts}
      @about-function{places-sidebar-nth-bookmark}
      @about-function{places-sidebar-set-drop-targets-visible}
    @end{subsection}
  @end{section}
  @begin[Ornaments]{section}
    @begin[GtkFrame]{subsection}
      @about-class{frame}
      @about-generic{frame-label}
      @about-generic{frame-label-widget}
      @about-generic{frame-label-xalign}
      @about-generic{frame-label-yalign}
      @about-generic{frame-shadow-type}
      @about-function{frame-new}
      @about-function{frame-label-align}
    @end{subsection}
    @begin[GtkSeparator]{subsection}
      @about-class{separator}
      @about-function{separator-new}
    @end{subsection}
  @end{section}
  @begin[Scrolling]{section}
    @begin[GtkScrollbar]{subsection}
      @about-class{scrollbar}
      @about-function{scrollbar-new}
    @end{subsection}
    @begin[GtkScrolledWindow]{subsection}
      @about-symbol{policy-type}
      @about-symbol{corner-type}
      @about-class{scrolled-window}
      @about-generic{scrolled-window-hadjustment}
      @about-generic{scrolled-window-hscrollbar-policy}
      @about-generic{scrolled-window-kinetic-scrolling}
      @about-generic{scrolled-window-max-content-height}
      @about-generic{scrolled-window-max-content-width}
      @about-generic{scrolled-window-min-content-height}
      @about-generic{scrolled-window-min-content-width}
      @about-generic{scrolled-window-overlay-scrolling}
      @about-generic{scrolled-window-propagate-natural-height}
      @about-generic{scrolled-window-propagate-natural-width}
      @about-generic{scrolled-window-shadow-type}
      @about-generic{scrolled-window-vadjustment}
      @about-generic{scrolled-window-vscrollbar-policy}
      @about-generic{scrolled-window-window-placement}
      @about-generic{scrolled-window-window-placement-set}
      @about-function{scrolled-window-new}
      @about-function{scrolled-window-hscrollbar}
      @about-function{scrolled-window-vscrollbar}
      @about-function{scrolled-window-policy}
      @about-function{scrolled-window-add-with-viewport}
      @about-function{scrolled-window-placement}
      @about-function{scrolled-window-unset-placement}
      @about-function{scrolled-window-capture-button-press}
    @end{subsection}
    @begin[GtkScrollable]{subsection}
      @about-symbol{scrollable-policy}
      @about-class{scrollable}
      @about-generic{scrollable-hadjustment}
      @about-generic{scrollable-vadjustment}
      @about-generic{scrollable-hscroll-policy}
      @about-generic{scrollable-vscroll-policy}
      @about-function{scrollable-border}
    @end{subsection}
  @end{section}
  @begin[Printing]{section}
    @begin[GtkPrintOperation]{subsection}
      @about-symbol{print-status}
      @about-symbol{print-operation-action}
      @about-symbol{print-operation-result}
      @about-symbol{print-error}
      @about-class{print-operation}
      @about-generic{print-operation-allow-async}
      @about-generic{print-operation-current-page}
      @about-generic{print-operation-custom-tab-label}
      @about-generic{print-operation-default-page-setup}
      @about-generic{print-operation-embed-page-setup}
      @about-generic{print-operation-export-filename}
      @about-generic{print-operation-has-selection}
      @about-generic{print-operation-job-name}
      @about-generic{print-operation-n-pages}
      @about-generic{print-operation-n-pages-to-print}
      @about-generic{print-operation-print-settings}
      @about-generic{print-operation-show-progress}
      @about-generic{print-operation-status}
      @about-generic{print-operation-status-string}
      @about-generic{print-operation-support-selection}
      @about-generic{print-operation-track-print-status}
      @about-generic{print-operation-unit}
      @about-generic{print-operation-use-full-page}
      @about-function{print-operation-new}
      @about-function{print-operation-get-error}
      @about-function{print-operation-run}
      @about-function{print-operation-cancel}
      @about-function{print-operation-draw-page-finish}
      @about-function{print-operation-set-defer-drawing}
      @about-function{print-operation-is-finished}
      @about-function{print-run-page-setup-dialog}
      @about-symbol{page-setup-done-func}
      @about-function{print-run-page-setup-dialog-async}
      @about-class{print-operation-preview}
      @about-function{print-operation-preview-end-preview}
      @about-function{print-operation-preview-is-selected}
      @about-function{print-operation-preview-render-page}
    @end{subsection}
    @begin[GtkPrintContext]{subsection}
      @about-class{print-context}
      @about-function{print-context-cairo-context}
      @about-function{print-context-set-cairo-context}
      @about-function{print-context-page-setup}
      @about-function{print-context-width}
      @about-function{print-context-height}
      @about-function{print-context-dpi-x}
      @about-function{print-context-dpi-y}
      @about-function{print-context-pango-fontmap}
      @about-function{print-context-create-pango-context}
      @about-function{print-context-create-pango-layout}
      @about-function{print-context-hard-margins}
    @end{subsection}
    @begin[GtkPrintSettings]{subsection}
      @about-symbol{page-orientation}
      @about-symbol{print-duplex}
      @about-symbol{print-quality}
      @about-symbol{number-up-layout}
      @about-symbol{print-pages}
      @about-symbol{page-set}
      @about-class{print-settings}
      @about-function{print-settings-new}
      @about-function{print-settings-copy}
      @about-function{print-settings-has-key}
      @about-function{print-settings-get}
      @about-function{print-settings-set}
      @about-function{print-settings-unset}
      @about-symbol{print-settings-func}
      @about-function{print-settings-foreach}
      @about-function{print-settings-bool}
      @about-function{print-settings-double}
      @about-function{print-settings-double-with-default}
      @about-function{print-settings-length}
      @about-function{print-settings-int}
      @about-function{print-settings-int-with-default}
      @about-function{print-settings-printer}
      @about-function{print-settings-orientation}
      @about-function{print-settings-paper-size}
      @about-function{print-settings-paper-width}
      @about-function{print-settings-paper-height}
      @about-function{print-settings-use-color}
      @about-function{print-settings-collate}
      @about-function{print-settings-reverse}
      @about-function{print-settings-duplex}
      @about-function{print-settings-quality}
      @about-function{print-settings-n-copies}
      @about-function{print-settings-number-up}
      @about-function{print-settings-number-up-layout}
      @about-function{print-settings-resolution}
      @about-function{print-settings-set-resolution-xy}
      @about-function{print-settings-resolution-x}
      @about-function{print-settings-resolution-y}
      @about-function{print-settings-printer-lpi}
      @about-function{print-settings-scale}
      @about-function{print-settings-print-pages}
      @about-function{print-settings-page-ranges}
      @about-function{print-settings-page-set}
      @about-function{print-settings-default-source}
      @about-function{print-settings-media-type}
      @about-function{print-settings-dither}
      @about-function{print-settings-finishings}
      @about-function{print-settings-output-bin}
      @about-function{print-settings-new-from-file}
      @about-function{print-settings-new-from-key-file}
      @about-function{print-settings-new-from-gvariant}
      @about-function{print-settings-load-file}
      @about-function{print-settings-load-key-file}
      @about-function{print-settings-to-file}
      @about-function{print-settings-to-key-file}
      @about-function{print-settings-to-gvariant}
    @end{subsection}
    @begin[GtkPageSetup]{subsection}
      @about-class{page-setup}
      @about-function{page-setup-new}
      @about-function{page-setup-copy}
      @about-function{page-setup-orientation}
      @about-function{page-setup-paper-size}
      @about-function{page-setup-top-margin}
      @about-function{page-setup-bottom-margin}
      @about-function{page-setup-left-margin}
      @about-function{page-setup-right-margin}
      @about-function{page-setup-set-paper-size-and-default-margins}
      @about-function{page-setup-paper-width}
      @about-function{page-setup-paper-height}
      @about-function{page-setup-page-width}
      @about-function{page-setup-page-height}
      @about-function{page-setup-new-from-file}
      @about-function{page-setup-new-from-key-file}
      @about-function{page-setup-new-from-gvariant}
      @about-function{page-setup-load-file}
      @about-function{page-setup-load-key-file}
      @about-function{page-setup-to-file}
      @about-function{page-setup-to-key-file}
      @about-function{page-setup-to-gvariant}
    @end{subsection}
    @begin[GtkPaperSize]{subsection}
      @about-symbol{unit}
      @about-class{paper-size}
      @about-function{paper-size-new}
      @about-function{paper-size-new-from-ppd}
      @about-function{paper-size-new-from-ipp}
      @about-function{paper-size-new-custom}
      @about-function{paper-size-copy}
      @about-function{paper-size-free}
      @about-function{paper-size-is-equal}
      @about-function{paper-size-paper-sizes}
      @about-function{paper-size-name}
      @about-function{paper-size-display-name}
      @about-function{paper-size-ppd-name}
      @about-function{paper-size-width}
      @about-function{paper-size-height}
      @about-function{paper-size-is-ipp}
      @about-function{paper-size-is-custom}
      @about-function{paper-size-set-size}
      @about-function{paper-size-default-top-margin}
      @about-function{paper-size-default-bottom-margin}
      @about-function{paper-size-default-left-margin}
      @about-function{paper-size-default-right-margin}
      @about-function{paper-size-default}
      @about-function{paper-size-new-from-key-file}
      @about-function{paper-size-new-from-gvariant}
      @about-function{paper-size-to-key-file}
      @about-function{paper-size-to-gvariant}
    @end{subsection}
    @begin[GtkPrinter]{subsection}
      @about-class{print-backend}
      @about-class{printer}
      @about-generic{printer-accepting-jobs}
      @about-generic{printer-accepts-pdf}
      @about-generic{printer-accepts-ps}
      @about-generic{printer-backend}
      @about-generic{printer-icon-name}
      @about-generic{printer-is-virtual}
      @about-generic{printer-job-count}
      @about-generic{printer-location}
      @about-generic{printer-name}
      @about-generic{printer-paused}
      @about-generic{printer-state-message}
      @about-function{printer-new}
      @about-function{printer-description}
      @about-function{printer-is-active}
      @about-function{printer-is-paused}
      @about-function{printer-is-accepting-jobs}
      @about-function{printer-is-default}
      @about-function{printer-list-papers}
      @about-function{printer-compare}
      @about-function{printer-has-details}
      @about-function{printer-request-details}
      @about-function{printer-capabilities}
      @about-function{printer-default-page-size}
      @about-function{printer-hard-margins}
      @about-symbol{printer-func}
      @about-function{enumerate-printers}
    @end{subsection}
    @begin[GtkPrintJob]{subsection}
      @about-class{print-job}
      @about-generic{print-job-page-setup}
      @about-generic{print-job-printer}
      @about-generic{print-job-settings}
      @about-generic{print-job-title}
      @about-generic{print-job-track-print-status}
      @about-function{print-job-new}
      @about-function{print-job-status}
      @about-function{print-job-set-source-file}
      @about-function{print-job-surface}
      @about-symbol{print-job-complete-func}
      @about-function{print-job-send}
      @about-function{print-job-pages}
      @about-function{print-job-page-ranges}
      @about-function{print-job-page-set}
      @about-function{print-job-num-copies}
      @about-function{print-job-scale}
      @about-function{print-job-n-up}
      @about-function{print-job-n-up-layout}
      @about-function{print-job-rotate}
      @about-function{print-job-collate}
      @about-function{print-job-reverse}
    @end{subsection}
    @begin[GtkPrintUnixDialog]{subsection}
      @about-symbol{print-capabilities}
      @about-class{print-unix-dialog}
      @about-generic{print-unix-dialog-current-page}
      @about-generic{print-unix-dialog-embed-page-setup}
      @about-generic{print-unix-dialog-has-selection}
      @about-generic{print-unix-dialog-manual-capabilities}
      @about-generic{print-unix-dialog-page-setup}
      @about-generic{print-unix-dialog-print-settings}
      @about-generic{print-unix-dialog-selected-printer}
      @about-generic{print-unix-dialog-support-selection}
      @about-function{print-unix-dialog-new}
      @about-function{print-unix-dialog-settings}
      @about-function{print-unix-dialog-add-custom-tab}
      @about-function{print-unix-dialog-page-setup-set}
    @end{subsection}
    @begin[GtkPageSetupUnixDialog]{subsection}
      @about-class{page-setup-unix-dialog}
      @about-function{page-setup-unix-dialog-new}
      @about-function{page-setup-unix-dialog-page-setup}
      @about-function{page-setup-unix-dialog-print-settings}
    @end{subsection}
  @end{section}
  @begin[Shortcuts Overview]{section}
    @begin[GtkShortcutsWindow]{subsection}
      @about-class{shortcuts-window}
      @about-generic{shortcuts-window-section-name}
      @about-generic{shortcuts-window-view-name}
    @end{subsection}
    @begin[GtkShortcutsSection]{subsection}
      @about-class{shortcuts-section}
      @about-generic{shortcuts-section-max-height}
      @about-generic{shortcuts-section-section-name}
      @about-generic{shortcuts-section-title}
      @about-generic{shortcuts-section-view-name}
    @end{subsection}
    @begin[GtkShortcutsGroup]{subsection}
      @about-class{shortcuts-group}
      @about-generic{shortcuts-group-accel-size-group}
      @about-generic{shortcuts-group-height}
      @about-generic{shortcuts-group-title}
      @about-generic{shortcuts-group-title-size-group}
      @about-generic{shortcuts-group-view}
    @end{subsection}
    @begin[GtkShortcutsShortcut]{subsection}
      @about-symbol{shortcut-type}
      @about-class{shortcuts-shortcut}
      @about-generic{shortcuts-shortcut-accel-size-group}
      @about-generic{shortcuts-shortcut-accelerator}
      @about-generic{shortcuts-shortcut-action-name}
      @about-generic{shortcuts-shortcut-direction}
      @about-generic{shortcuts-shortcut-icon}
      @about-generic{shortcuts-shortcut-icon-set}
      @about-generic{shortcuts-shortcut-shortcut-type}
      @about-generic{shortcuts-shortcut-subtitle}
      @about-generic{shortcuts-shortcut-subtitle-set}
      @about-generic{shortcuts-shortcut-title}
      @about-generic{shortcuts-shortcut-title-size-group}
    @end{subsection}
  @end{section}
  @begin[Miscellaneous]{section}
    @begin[GtkAdjustment]{subsection}
      @about-class{adjustment}
      @about-generic{adjustment-lower}
      @about-generic{adjustment-page-increment}
      @about-generic{adjustment-page-size}
      @about-generic{adjustment-step-increment}
      @about-generic{adjustment-upper}
      @about-generic{adjustment-value}
      @about-function{adjustment-new}
      @about-function{adjustment-clamp-page}
      @about-function{adjustment-changed}
      @about-function{adjustment-value-changed}
      @about-function{adjustment-configure}
      @about-function{adjustment-minimum-increment}
    @end{subsection}
    @begin[GtkCalendar]{subsection}
      @about-symbol{calendar-display-options}
      @about-class{calendar}
      @about-generic{calendar-day}
      @about-generic{calendar-detail-height-rows}
      @about-generic{calendar-detail-width-chars}
      @about-generic{calendar-month}
      @about-generic{calendar-no-month-change}
      @about-generic{calendar-show-day-names}
      @about-generic{calendar-show-details}
      @about-generic{calendar-show-heading}
      @about-generic{calendar-show-week-numbers}
      @about-generic{calendar-year}
      @about-function{calendar-new}
      @about-function{calendar-select-month}
      @about-function{calendar-select-day}
      @about-function{calendar-mark-day}
      @about-function{calendar-unmark-day}
      @about-function{calendar-day-is-marked}
      @about-function{calendar-clear-marks}
      @about-function{calendar-display-options}
      @about-function{calendar-date}
      @about-symbol{calendar-detail-func}
      @about-function{calendar-set-detail-func}
    @end{subsection}
    @begin[GtkDrawingArea]{subsection}
      @about-class{drawing-area}
      @about-function{drawing-area-new}
    @end{subsection}
    @begin[GtkEventBox]{subsection}
      @about-class{event-box}
      @about-generic{event-box-above-child}
      @about-generic{event-box-visible-window}
      @about-function{event-box-new}
    @end{subsection}
    @begin[GtkIMContexSimple]{subsection}
      @about-class{im-context-simple}
      @about-function{im-context-simple-new}
      @about-function{im-context-simple-add-table}
    @end{subsection}
    @begin[GtkIMMulticontex]{subsection}
      @about-class{im-multicontext}
      @about-function{im-multicontext-new}
      @about-function{im-multicontext-append-menuitems}
      @about-function{im-multicontext-get-context-id}
      @about-function{im-multicontext-set-context-id}
    @end{subsection}
    @begin[GtkSizeGroup]{subsection}
      @about-symbol{size-group-mode}
      @about-class{size-group}
      @about-generic{size-group-ignore-hidden}
      @about-generic{size-group-mode}
      @about-function{size-group-new}
      @about-function{size-group-add-widget}
      @about-function{size-group-remove-widget}
      @about-function{size-group-widgets}
    @end{subsection}
    @begin[GtkTooltip]{subsection}
      @about-class{tooltip}
      @about-function{tooltip-set-markup}
      @about-function{tooltip-set-text}
      @about-function{tooltip-set-icon}
      @about-function{tooltip-set-icon-from-stock}
      @about-function{tooltip-set-icon-from-icon-name}
      @about-function{tooltip-set-icon-from-gicon}
      @about-function{tooltip-set-custom}
      @about-function{tooltip-trigger-tooltip-query}
      @about-function{tooltip-set-tip-area}
    @end{subsection}
    @begin[GtkViewport]{subsection}
      @about-class{viewport}
      @about-generic{viewport-shadow-type}
      @about-function{viewport-new}
      @about-function{viewport-bin-window}
      @about-function{viewport-view-window}
    @end{subsection}
    @begin[GtkAccessible]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Interfaces]{section}
    @begin[GtkActionable]{subsection}
      @about-class{actionable}
      @about-generic{actionable-action-name}
      @about-generic{actionable-action-target}
      @about-function{actionable-set-action-target}
      @about-function{actionable-set-detailed-action-name}
    @end{subsection}
    @begin[GtkBuildable]{subsection}
      @about-class{buildable}
      @about-function{buildable-name}
      @about-function{buildable-add-child}
      @about-function{buildable-set-buildable-property}
      @about-function{buildable-construct-child}
      @about-function{buildable-custom-tag-start}
      @about-function{buildable-custom-tag-end}
      @about-function{buildable-custom-finished}
      @about-function{buildable-parser-finished}
      @about-function{buildable-internal-child}
    @end{subsection}
    @begin[GtkOrientable]{subsection}
      @about-class{orientable}
      @about-generic{orientable-orientation}
    @end{subsection}
  @end{section}
  @begin[Abstract Base Classes]{section}
    @begin[GtkWidget]{subsection}
      @about-symbol{widget-help-type}
      @about-symbol{size-request-mode}
      @about-symbol{requested-size}
      @about-symbol{align}
      @about-struct{requisition}
      @about-function{requisition-width}
      @about-function{requisition-height}
      @about-function{requisition-new}
      @about-function{requisition-copy}
      @about-function{requisition-free}
      @about-class{widget}
      @about-generic{widget-app-paintable}
      @about-generic{widget-can-default}
      @about-generic{widget-can-focus}
      @about-generic{widget-composite-child}
      @about-generic{widget-double-buffered}
      @about-generic{widget-events}
      @about-generic{widget-expand}
      @about-generic{widget-focus-on-click}
      @about-generic{widget-halign}
      @about-generic{widget-has-default}
      @about-generic{widget-has-focus}
      @about-generic{widget-has-tooltip}
      @about-generic{widget-height-request}
      @about-generic{widget-hexpand}
      @about-generic{widget-hexpand-set}
      @about-generic{widget-is-focus}
      @about-generic{widget-margin}
      @about-generic{widget-margin-bottom}
      @about-generic{widget-margin-end}
      @about-generic{widget-margin-left}
      @about-generic{widget-margin-right}
      @about-generic{widget-margin-start}
      @about-generic{widget-margin-top}
      @about-generic{widget-name}
      @about-generic{widget-no-show-all}
      @about-generic{widget-opacity}
      @about-generic{widget-parent}
      @about-generic{widget-receives-default}
      @about-generic{widget-scale-factor}
      @about-generic{widget-sensitive}
      @about-generic{widget-style}
      @about-generic{widget-tooltip-markup}
      @about-generic{widget-tooltip-text}
      @about-generic{widget-valign}
      @about-generic{widget-vexpand}
      @about-generic{widget-vexpand-set}
      @about-generic{widget-visible}
      @about-generic{widget-width-request}
      @about-generic{widget-window}
      @about-function{widget-new}
      @about-function{widget-destroy}
      @about-function{widget-in-destruction}
      @about-function{widget-destroyed}
      @about-function{widget-unparent}
      @about-function{widget-show}
      @about-function{widget-show-now}
      @about-function{widget-hide}
      @about-function{widget-show-all}
      @about-function{widget-map}
      @about-function{widget-unmap}
      @about-function{widget-realize}
      @about-function{widget-unrealize}
      @about-function{widget-draw}
      @about-function{widget-queue-draw}
      @about-function{widget-queue-resize}
      @about-function{widget-queue-resize-no-redraw}
      @about-function{widget-queue-allocate}
      @about-function{widget-frame-clock}
      @about-symbol{tick-callback}
      @about-function{widget-add-tick-callback}
      @about-function{widget-remove-tick-callback}
      @about-function{widget-get-child-requisition}
      @about-function{widget-size-allocate}
      @about-function{widget-size-allocate-with-baseline}
      @about-function{widget-add-accelerator}
      @about-function{widget-remove-accelerator}
      @about-function{widget-set-accel-path}
      @about-function{widget-list-accel-closures}
      @about-function{widget-can-activate-accel}
      @about-function{widget-event}
      @about-function{widget-activate}
      @about-function{widget-reparent}
      @about-function{widget-intersect}
      @about-function{widget-grab-focus}
      @about-function{widget-grab-default}
      @about-function{widget-state}
      @about-function{widget-parent-window}
      @about-function{widget-add-events}
      @about-function{widget-device-events}
      @about-function{widget-add-device-events}
      @about-function{widget-device-enabled}
      @about-function{widget-toplevel}
      @about-function{widget-ancestor}
      @about-function{widget-visual}
      @about-function{widget-pointer}
      @about-function{widget-is-ancestor}
      @about-function{widget-translate-coordinates}
      @about-function{widget-hide-on-delete}
      @about-function{widget-ensure-style}
      @about-function{widget-reset-rc-styles}
      @about-function{widget-default-style}
      @about-function{widget-direction}
      @about-function{widget-default-direction}
      @about-function{widget-shape-combine-region}
      @about-function{widget-input-shape-combine-region}
      @about-function{widget-composite-name}
      @about-function{widget-override-background-color}
      @about-function{widget-override-color}
      @about-function{widget-override-font}
      @about-function{widget-override-symbolic-color}
      @about-function{widget-override-cursor}
      @about-function{widget-create-pango-context}
      @about-function{widget-pango-context}
      @about-function{widget-font-options}
      @about-function{widget-font-map}
      @about-function{widget-create-pango-layout}
      @about-function{widget-queue-draw-area}
      @about-function{widget-queue-draw-region}
      @about-function{widget-set-redraw-on-allocate}
      @about-function{widget-mnemonic-activate}
      @about-function{widget-class-install-style-property}
      @about-function{widget-class-find-style-property}
      @about-function{widget-class-list-style-properties}
      @about-function{widget-region-intersect}
      @about-function{widget-send-expose}
      @about-function{widget-send-focus-change}
      @about-function{widget-style-get}
      @about-function{widget-style-property}
      @about-function{widget-style-get-valist}
      @about-function{widget-style-attach}
      @about-function{widget-class-set-accessible-type}
      @about-function{widget-class-set-accessible-role}
      @about-function{widget-accessible}
      @about-function{widget-child-focus}
      @about-function{widget-child-notify}
      @about-function{widget-freeze-child-notify}
      @about-function{widget-child-visible}
      @about-function{widget-settings}
      @about-function{widget-clipboard}
      @about-function{widget-display}
      @about-function{widget-root-window}
      @about-function{widget-screen}
      @about-function{widget-has-screen}
      @about-function{widget-size-request}
      @about-function{widget-thaw-child-notify}
      @about-function{widget-list-mnemonic-labels}
      @about-function{widget-add-mnemonic-label}
      @about-function{widget-remove-mnemonic-label}
      @about-function{widget-is-composited}
      @about-function{widget-error-bell}
      @about-function{widget-keynav-failed}
      @about-function{widget-tooltip-window}
      @about-function{widget-trigger-tooltip-query}
      @about-function{widget-register-window}
      @about-function{widget-unregister-window}
      @about-function{cairo-should-draw-window}
      @about-function{cairo-transform-to-window}
      @about-function{widget-allocated-width}
      @about-function{widget-allocated-height}
      @about-function{widget-allocation}
      @about-function{widget-allocated-baseline}
      @about-function{widget-clip}
      @about-function{widget-has-window}
      @about-function{widget-is-sensitive}
      @about-function{widget-is-visible}
      @about-function{widget-state-flags}
      @about-function{widget-unset-state-flags}
      @about-function{widget-has-visible-focus}
      @about-function{widget-has-grab}
      @about-function{widget-is-drawable}
      @about-function{widget-is-toplevel}
      @about-function{widget-support-multidevice}
      @about-function{widget-realized}
      @about-function{widget-mapped}
      @about-function{widget-device-is-shadowed}
      @about-function{widget-modifier-mask}
      @about-function{widget-insert-action-group}
      @about-function{widget-list-action-prefixes}
      @about-function{widget-action-group}
      @about-function{widget-path}
      @about-function{widget-style-context}
      @about-function{widget-reset-style}
      @about-function{widget-class-css-name}
      @about-function{widget-preferred-height}
      @about-function{widget-preferred-width}
      @about-function{widget-preferred-height-for-width}
      @about-function{widget-preferred-width-for-height}
      @about-function{widget-preferred-height-and-baseline-for-width}
      @about-function{widget-request-mode}
      @about-function{widget-preferred-size}
      @about-function{distribute-natural-allocation}
      @about-function{widget-valign-with-baseline}
      @about-function{widget-queue-compute-expand}
      @about-function{widget-compute-expand}
      @about-function{widget-init-template}
      @about-function{widget-class-set-template}
      @about-function{widget-class-set-template-from-resource}
      @about-function{widget-template-child}
      @about-function{widget-class-bind-template-child}
      @about-function{widget-class-bind-template-child-internal}
      @about-function{widget-class-bind-template-child-private}
      @about-function{widget-class-bind-template-child-internal-private}
      @about-function{widget-class-bind-template-child-full}
      @about-function{widget-class-bind-template-callback}
      @about-function{widget-class-bind-template-callback-full}
      @about-function{widget-class-set-connect-func}
    @end{subsection}
    @begin[GtkContainer]{subsection}
      @about-symbol{resize-mode}
      @about-class{container}
      @about-generic{container-border-width}
      @about-generic{container-child}
      @about-generic{container-resize-mode}
      @about-function{container-add}
      @about-function{container-remove}
      @about-function{container-add-with-properties}
      @about-function{container-check-resize}
      @about-symbol{gtk-callback}
      @about-function{container-foreach}
      @about-function{container-forall}
      @about-function{container-children}
      @about-function{container-path-for-child}
      @about-function{container-set-reallocate-redraws}
      @about-function{container-focus-child}
      @about-function{container-focus-vadjustment}
      @about-function{container-focus-hadjustment}
      @about-function{container-resize-children}
      @about-function{container-child-type}
      @about-function{container-child-get}
      @about-function{container-child-set}
      @about-function{container-child-property}
      @about-function{container-child-get-valist}
      @about-function{container-child-set-valist}
      @about-function{container-child-notify}
      @about-function{container-child-notify-by-pspec}
      @about-function{container-propagate-draw}
      @about-function{container-focus-chain}
      @about-function{container-unset-focus-chain}
      @about-function{container-class-find-child-property}
      @about-function{container-class-install-child-property}
      @about-function{container-class-install-child-properties}
      @about-function{container-class-list-child-properties}
      @about-function{container-class-handle-border-width}
    @end{subsection}
    @begin[GtkBin]{subsection}
      @about-class{bin}
      @about-function{bin-child}
    @end{subsection}
    @begin[GtkRange]{subsection}
      @about-symbol{sensitivity-type}
      @about-class{range}
      @about-generic{range-adjustment}
      @about-generic{range-fill-level}
      @about-generic{range-inverted}
      @about-generic{range-lower-stepper-sensitivity}
      @about-generic{range-restrict-to-fill-level}
      @about-generic{range-round-digits}
      @about-generic{range-show-fill-level}
      @about-generic{range-upper-stepper-sensitivity}
      @about-function{range-value}
      @about-function{range-set-increments}
      @about-function{range-set-range}
      @about-function{range-flippable}
      @about-function{range-min-slider-size}
      @about-function{range-range-rect}
      @about-function{range-slider-range}
      @about-function{range-slider-size-fixed}
    @end{subsection}
    @begin[GtkIMContext]{subsection}
      @about-class{im-context}
      @about-generic{im-context-input-hints}
      @about-generic{im-context-input-purpose}
      @about-symbol{im-contextInfo}
      @about-function{im-context-set-client-window}
      @about-function{im-context-get-preedit-string}
      @about-function{im-context-filter-keypress}
      @about-function{im-context-focus-in}
      @about-function{im-context-focus-out}
      @about-function{im-context-reset}
      @about-function{im-context-set-cursor-location}
      @about-function{im-context-set-use-preedit}
      @about-function{im-context-set-surrounding}
      @about-function{im-context-get-surrounding}
      @about-function{im-context-delete-surrounding}
    @end{subsection}
    @begin[GtkNativeDialog]{subsection}
      @about-class{native-dialog}
      @about-generic{native-dialog-modal}
      @about-generic{native-dialog-title}
      @about-generic{native-dialog-transient-for}
      @about-generic{native-dialog-visible}
      @about-function{native-dialog-show}
      @about-function{native-dialog-hide}
      @about-function{native-dialog-destroy}
      @about-function{native-dialog-run}
    @end{subsection}
  @end{section}
  @begin[Cross-process Embedding]{section}
    @begin[GtkPlug]{subsection}
      @about-class{plug}
      @about-generic{plug-embedded}
      @about-generic{plug-socket-window}
      @about-function{plug-construct}
      @about-function{plug-construct-for-display}
      @about-function{plug-new}
      @about-function{plug-new-for-display}
      @about-function{plug-id}
    @end{subsection}
    @begin[GtkSocket]{subsection}
      @about-class{socket}
      @about-function{socket-new}
      @about-function{socket-add-id}
      @about-function{socket-id}
      @about-function{socket-plug-window}
    @end{subsection}
  @end{section}
  @begin[Recently Used Documents]{section}
    @begin[GtkRecentManager]{subsection}
      @about-class{recent-manager}
      @about-generic{recent-manager-filename}
      @about-generic{recent-manager-size}
      @about-function{recent-manager-new}
      @about-function{recent-manager-default}
      @about-function{recent-manager-add-item}
      @about-function{recent-manager-add-full}
      @about-function{recent-manager-remove-item}
      @about-function{recent-manager-lookup-item}
      @about-function{recent-manager-has-item}
      @about-function{recent-manager-move-item}
      @about-function{recent-manager-items}
      @about-function{recent-manager-purge-items}
      @about-class{recent-info}
      @about-function{recent-info-uri}
      @about-function{recent-info-display-name}
      @about-function{recent-info-description}
      @about-function{recent-info-mime-type}
      @about-function{recent-info-added}
      @about-function{recent-info-modified}
      @about-function{recent-info-visited}
      @about-function{recent-info-private-hint}
      @about-function{recent-info-application-info}
      @about-function{recent-info-applications}
      @about-function{recent-info-last-application}
      @about-function{recent-info-has-application}
      @about-function{recent-info-create-app-info}
      @about-function{recent-info-groups}
      @about-function{recent-info-has-group}
      @about-function{recent-info-icon}
      @about-function{recent-info-gicon}
      @about-function{recent-info-short-name}
      @about-function{recent-info-uri-display}
      @about-function{recent-info-age}
      @about-function{recent-info-is-local}
      @about-function{recent-info-exists}
      @about-function{recent-info-match}
    @end{subsection}
    @begin[GtkRecentChooser]{subsection}
      @about-symbol{GTK_RECENT_CHOOSER_ERROR}
      @about-symbol{recent-chooser-error}
      @about-symbol{recent-sort-type}
      @about-class{recent-chooser}
      @about-generic{recent-chooser-filter}
      @about-generic{recent-chooser-limit}
      @about-generic{recent-chooser-local-only}
      @about-generic{recent-chooser-recent-manager}
      @about-generic{recent-chooser-select-multiple}
      @about-generic{recent-chooser-show-icons}
      @about-generic{recent-chooser-show-not-found}
      @about-generic{recent-chooser-show-private}
      @about-generic{recent-chooser-show-tips}
      @about-generic{recent-chooser-sort-type}
      @about-symbol{recent-sort-func}
      @about-function{recent-chooser-set-sort-func}
      @about-function{recent-chooser-current-uri}
      @about-function{recent-chooser-current-item}
      @about-function{recent-chooser-select-uri}
      @about-function{recent-chooser-unselect-uri}
      @about-function{recent-chooser-select-all}
      @about-function{recent-chooser-unselect-all}
      @about-function{recent-chooser-items}
      @about-function{recent-chooser-uris}
      @about-function{recent-chooser-add-filter}
      @about-function{recent-chooser-remove-filter}
      @about-function{recent-chooser-list-filters}
    @end{subsection}
    @begin[GtkRecentChooserDialog]{subsection}
      @about-class{recent-chooser-dialog}
      @about-function{recent-chooser-dialog-new}
      @about-function{recent-chooser-dialog-new-for-manager}
    @end{subsection}
    @begin[GtkRecentChooserMenu]{subsection}
      @about-class{recent-chooser-menu}
      @about-function{recent-chooser-menu-new}
      @about-function{recent-chooser-menu-new-for-manager}
    @end{subsection}
    @begin[GtkRecentChooserWidget]{subsection}
      @about-class{recent-chooser-widget}
      @about-function{recent-chooser-widget-new}
      @about-function{recent-chooser-widget-new-for-manager}
    @end{subsection}
    @begin[GtkRecentFilter]{subsection}
      @about-symbol{recent-filter-info}
      @about-symbol{recent-filter-flags}
      @about-class{recent-filter}
      @about-function{recent-filter-new}
      @about-function{recent-filter-name}
      @about-function{recent-filter-add-mime-type}
      @about-function{recent-filter-add-pattern}
      @about-function{recent-filter-add-pixbuf-formats}
      @about-function{recent-filter-add-application}
      @about-function{recent-filter-add-group}
      @about-function{recent-filter-add-age}
      @about-symbol{recent-filter-func}
      @about-function{recent-filter-add-custom}
      @about-function{recent-filter-needed}
      @about-function{recent-filter-filter}
    @end{subsection}
  @end{section}
  @begin[Choosing from installed applications]{section}
    @begin[GtkAppChooser]{subsection}
      @about-class{app-chooser}
      @about-generic{app-chooser-content-type}
      @about-function{app-chooser-app-info}
      @about-function{app-chooser-refresh}
    @end{subsection}
    @begin[GtkAppChooserButton]{subsection}
      @about-class{app-chooser-button}
      @about-generic{app-chooser-button-heading}
      @about-generic{app-chooser-button-show-default-item}
      @about-generic{app-chooser-button-show-dialog-item}
      @about-function{app-chooser-button-new}
      @about-function{app-chooser-button-append-custom-item}
      @about-function{app-chooser-button-append-separator}
      @about-function{app-chooser-button-set-active-custom-item}
    @end{subsection}
    @begin[GtkAppChooserDialog]{subsection}
      @about-class{app-chooser-dialog}
      @about-generic{app-chooser-dialog-gfile}
      @about-generic{app-chooser-dialog-heading}
      @about-function{app-chooser-dialog-new}
      @about-function{app-chooser-dialog-new-for-content-type}
      @about-function{app-chooser-dialog-widget}
    @end{subsection}
    @begin[GtkAppChooserWidget]{subsection}
      @about-class{app-chooser-widget}
      @about-generic{app-chooser-widget-default-text}
      @about-generic{app-chooser-widget-show-all}
      @about-generic{app-chooser-widget-show-default}
      @about-generic{app-chooser-widget-show-fallback}
      @about-generic{app-chooser-widget-show-other}
      @about-generic{app-chooser-widget-show-recommended}
      @about-function{app-chooser-widget-new}
     @end{subsection}
  @end{section}
  @begin[Gestures and event handling]{section}
    @begin[GtkEventController]{subsection}
      @about-symbol{propagation-phase}
      @about-class{event-controller}
      @about-generic{event-controller-propagation-phase}
      @about-generic{event-controller-widget}
      @about-function{event-controller-handle-event}
      @about-function{event-controller-reset}
    @end{subsection}
    @begin[GtkEventControllerKey]{subsection}
      @about-class{event-controller-key}
      @about-function{event-controller-key-new}
    @end{subsection}
    @begin[GtkEventControllerScroll]{subsection}
      @about-symbol{event-controller-scroll-flags}
      @about-class{event-controller-scroll}
      @about-generic{event-controller-scroll-flags}
      @about-function{event-controller-scroll-new}
    @end{subsection}
    @begin[GtkEventControllerMotion]{subsection}
      @about-class{event-controller-motion}
      @about-function{event-controller-motion-new}
    @end{subsection}
    @begin[GtkGesture]{subsection}
      @about-symbol{event-sequence-state}
      @about-class{gesture}
      @about-generic{gesture-n-points}
      @about-generic{gesture-window}
      @about-function{gesture-device}
      @about-function{gesture-is-active}
      @about-function{gesture-is-recognized}
      @about-function{gesture-sequence-state}
      @about-function{gesture-set-state}
      @about-function{gesture-sequences}
      @about-function{gesture-handles-sequence}
      @about-function{gesture-last-updated-sequence}
      @about-function{gesture-last-event}
      @about-function{gesture-point}
      @about-function{gesture-bounding-box}
      @about-function{gesture-bounding-box-center}
      @about-function{gesture-group}
      @about-function{gesture-ungroup}
      @about-function{gesture-get-group}
      @about-function{gesture-is-grouped-with}
    @end{subsection}
    @begin[GtkGestureSingle]{subsection}
      @about-class{gesture-single}
      @about-generic{gesture-single-button}
      @about-generic{gesture-single-exclusive}
      @about-generic{gesture-single-touch-only}
      @about-function{gesture-single-current-button}
      @about-function{gesture-single-current-sequence}
    @end{subsection}
    @begin[GtkGestureDrag]{subsection}
      @about-class{gesture-drag}
      @about-function{gesture-drag-new}
      @about-function{gesture-drag-start-point}
      @about-function{gesture-drag-offset}
    @end{subsection}
    @begin[GtkGestureLongPress]{subsection}
      @about-class{gesture-long-press}
      @about-generic{gesture-long-press-delay-factor}
      @about-function{gesture-long-press-new}
    @end{subsection}
    @begin[GtkGestureMultiPress]{subsection}
      @about-class{gesture-multi-press}
      @about-function{gesture-multi-press-new}
      @about-function{gesture-multi-press-area}
    @end{subsection}
    @begin[GtkGesturePan]{subsection}
      @about-symbol{pan-direction}
      @about-class{gesture-pan}
      @about-generic{gesture-pan-orientation}
      @about-function{gesture-pan-new}
    @end{subsection}
    @begin[GtkGestureSwipe]{subsection}
      @about-class{gesture-swipe}
      @about-function{gesture-swipe-new}
      @about-function{gesture-swipe-velocity}
    @end{subsection}
    @begin[GtkGestureRotate]{subsection}
      @about-class{gesture-rotate}
      @about-function{gesture-rotate-new}
      @about-function{gesture-rotate-angle-delta}
    @end{subsection}
    @begin[GtkGestureZoom]{subsection}
      @about-class{gesture-zoom}
      @about-function{gesture-zoom-new}
      @about-function{gesture-zoom-scale-delta}
    @end{subsection}
    @begin[GtkGestureStylus]{subsection}
      @about-class{gesture-stylus}
      @about-function{gesture-stylus-new}
      @about-function{gesture-stylus-axis}
      @about-function{gesture-stylus-axes}
      @about-function{gesture-stylus-device-tool}
    @end{subsection}
    @begin[GtkPadController]{subsection}
      @about-symbol{pad-action-type}
      @about-symbol{pad-action-entry}
      @about-class{pad-controller}
      @about-generic{pad-controller-action-group}
      @about-generic{pad-controller-pad}
      @about-function{pad-controller-new}
      @about-function{pad-controller-set-action-entries}
      @about-function{pad-controller-set-action}
    @end{subsection}
  @end{section}
  @begin[GTK Core Reference]{section}
    @begin[Main loop and Events]{subsection}
      Library initialization, main event loop, and events.

      Before using GTK, it needs to be initialized. Initialization connects to
      the window system display, and parses some standard command line
      arguments. In the C library the @code{gtk_init()} macro initializes GTK.
      In the Lisp binding to GTK, GTK is initialized, when loading the
      @code{cl-cffi-gtk3} library. Therefore, no functions are exported, which
      initialize GTK.

      Like all GUI toolkits, GTK uses an event-driven programming model. When
      the user is doing nothing, GTK sits in the main loop and waits for input.
      If the user performs some action - say, a mouse click - then the main
      loop \"wakes up\" and delivers an event to GTK. GTK forwards the event to
      one or more widgets.

      In the C library the main loop is executed with the @code{gtk_main()}
      function. In the Lisp binding this function is implemented as
      the @fun{gtk:main} function, but in general it is not used. The
      @code{gtk_main()} function is replaced with the @fun{gtk:within-main-loop}
      macro, which does all necessary work to run the main loop. See the
      example for a typical main function in the Lisp binding.

      When widgets receive an event, they frequently emit one or more signals.
      Signals notify your program that \"something interesting happened\" by
      invoking functions you have connected to the signal with the
      @fun{g:signal-connect} function. Functions connected to a signal are
      often termed callbacks.

      When your callbacks are invoked, you would typically take some action -
      for example, when an Open button is clicked you might display a
      @class{gtk:file-chooser-dialog} window. After a callback finishes, GTK
      will return to the main loop and await more user input.

      @b{Example:} Typical main function in Lisp for a GTK application.
      @begin{pre}
(defun main ()
  (gtk:within-main-loop
    (let (;; Create the main window.
          (window (gtk:window-new :toplevel)))
      ;; Set up the GUI elements
      ...
      ;; Show the application window.
      (gtk:widget-show-all window))))
      @end{pre}
      @about-function{default-language}
      @about-function{locale-direction}
      @about-function{option-group}
      @about-function{events-pending}
      @about-function{main}
      @about-function{main-level}
      @about-function{main-quit}
      @about-macro{within-main-loop}
      @about-function{leave-gtk-main}
      @about-function{join-gtk-main}
      @about-function{main-iteration}
      @about-function{main-iteration-do}
      @about-function{main-do-event}
      @about-function{grab-add}
      @about-function{grab-current}
      @about-function{grab-remove}
      @about-function{device-grab-add}
      @about-function{device-grab-remove}
      @about-function{current-event}
      @about-function{current-event-time}
      @about-function{current-event-state}
      @about-function{current-event-device}
      @about-function{event-widget}
      @about-function{propagate-event}
    @end{subsection}
    @begin[Version Information]{subsection}
      GTK provides version information, primarily useful in configure checks
      for builds that have a configure script. Applications will not typically
      use the features described here.

      @about-function{major-version}
      @about-function{minor-version}
      @about-function{micro-version}
      @about-function{binary-age}
      @about-function{interface-age}
      @about-function{check-version}
      @about-function{cl-cffi-gtk-build-info}
    @end{subsection}
    @begin[Accelerator Groups]{subsection}
      @about-symbol{accel-flags}
      @about-class{accel-group}
      @about-generic{accel-group-is-locked}
      @about-generic{accel-group-modifier-mask}
      @about-function{accel-group-new}
      @about-function{accel-group-connect}
      @about-function{accel-group-connect-by-path}
      @about-function{accel-group-disconnect}
      @about-function{accel-group-disconnect-key}
      @about-function{accel-group-activate}
      @about-function{accel-group-lock}
      @about-function{accel-group-unlock}
      @about-function{accel-group-from-accel-closure}
      @about-function{accel-groups-activate}
      @about-function{accel-groups-from-object}
      @about-function{accel-group-find}
      @about-symbol{accel-key}
      @about-function{accelerator-valid}
      @about-function{accelerator-parse}
      @about-function{accelerator-name}
      @about-function{accelerator-label}
      @about-function{accelerator-parse-with-keycode}
      @about-function{accelerator-name-with-keycode}
      @about-function{accelerator-get-label-with-keycode}
      @about-function{accelerator-default-mod-mask}
    @end{subsection}
    @begin[Accelerator Maps]{subsection}
      @about-class{accel-map}
      @about-function{accel-map-add-entry}
      @about-function{accel-map-lookup-entry}
      @about-function{accel-map-change-entry}
      @about-function{accel-map-load}
      @about-function{accel-map-save}
      @about-function{accel-map-foreach}
      @about-function{accel-map-load-fd}
      @about-function{accel-map-save-fd}
      @about-function{accel-map-load-scanner}
      @about-function{accel-map-add-filter}
      @about-function{accel-map-foreach-unfiltered}
      @about-function{accel-map-get}
      @about-function{accel-map-lock-path}
      @about-function{accel-map-unlock-path}
    @end{subsection}
    @begin[GtkClipboard]{subsection}
      @about-class{clipboard}
      @about-function{clipboard-get}
      @about-function{clipboard-for-display}
      @about-function{clipboard-display}
      @about-function{clipboard-default}
      @about-symbol{clipboard-clear-func}
      @about-symbol{clipboard-get-func}
      @about-function{clipboard-set-with-data}
      @about-function{clipboard-set-with-owner}
      @about-function{clipboard-owner}
      @about-function{clipboard-clear}
      @about-function{clipboard-set-text}
      @about-function{clipboard-set-image}
      @about-symbol{clipboard-received-func}
      @about-function{clipboard-request-contents}
      @about-symbol{clipboard-text-received-func}
      @about-function{clipboard-request-text}
      @about-symbol{clipboard-image-received-func}
      @about-function{clipboard-request-image}
      @about-symbol{clipboard-targets-received-func}
      @about-function{clipboard-request-targets}
      @about-symbol{clipboard-rich-text-received-func}
      @about-function{clipboard-request-rich-text}
      @about-symbol{clipboard-uri-received-func}
      @about-function{clipboard-request-uris}
      @about-function{clipboard-wait-for-contents}
      @about-function{clipboard-wait-for-text}
      @about-function{clipboard-wait-for-image}
      @about-function{clipboard-wait-for-rich-text}
      @about-function{clipboard-wait-for-uris}
      @about-function{clipboard-wait-is-text-available}
      @about-function{clipboard-wait-is-image-available}
      @about-function{clipboard-wait-is-rich-text-available}
      @about-function{clipboard-wait-is-uris-available}
      @about-function{clipboard-wait-for-targets}
      @about-function{clipboard-wait-is-target-available}
      @about-function{clipboard-set-can-store}
      @about-function{clipboard-store}
      @about-function{clipboard-selection}
    @end{subsection}
    @begin[Drag and drop handling]{subsection}
      GTK has a rich set of functions for doing inter-process communication via
      the drag-and-drop metaphor. GTK can do drag-and-drop (DND) via multiple
      protocols. The currently supported protocols are the Xdnd and Motif
      protocols.

      As well as the functions listed here, applications may need to use some
      facilities provided for Selections. Also, the Drag and Drop API makes use
      of signals in the @class{gtk:widget} class.
      @about-symbol{dest-defaults}
      @about-symbol{target-flags}
      @about-symbol{drag-result}
      @about-function{drag-dest-set}
      @about-function{drag-dest-set-proxy}
      @about-function{drag-dest-unset}
      @about-function{drag-dest-find-target}
      @about-function{drag-dest-target-list}
      @about-function{drag-dest-add-text-targets}
      @about-function{drag-dest-add-image-targets}
      @about-function{drag-dest-add-uri-targets}
      @about-function{drag-dest-track-motion}
      @about-function{drag-finish}
      @about-function{drag-data}
      @about-function{drag-source-widget}
      @about-function{drag-highlight}
      @about-function{drag-unhighlight}
      @about-function{drag-begin}
      @about-function{drag-begin-with-coordinates}
      @about-function{drag-cancel}
      @about-function{drag-set-icon-widget}
      @about-function{drag-set-icon-pixbuf}
      @about-function{drag-set-icon-stock}
      @about-function{drag-set-icon-surface}
      @about-function{drag-set-icon-name}
      @about-function{drag-set-icon-gicon}
      @about-function{drag-set-icon-default}
      @about-function{drag-check-threshold}
      @about-function{drag-source-set}
      @about-function{drag-source-set-icon-pixbuf}
      @about-function{drag-source-set-icon-stock}
      @about-function{drag-source-set-icon-name}
      @about-function{drag-source-set-icon-gicon}
      @about-function{drag-source-unset}
      @about-function{drag-source-target-list}
      @about-function{drag-source-add-text-targets}
      @about-function{drag-source-add-image-targets}
      @about-function{drag-source-add-uri-targets}
    @end{subsection}
    @begin[GtkSettings]{subsection}
      @about-symbol{GtkSettingsValue}
      @about-symbol{im-preedit-style}
      @about-symbol{im-status-style}
      @about-class{settings}
      @about-generic{settings-color-hash}
      @about-generic{settings-gtk-alternative-button-order}
      @about-generic{settings-gtk-alternative-sort-arrows}
      @about-generic{settings-gtk-application-prefer-dark-theme}
      @about-generic{settings-gtk-auto-mnemonics}
      @about-generic{settings-gtk-button-images}
      @about-generic{settings-gtk-can-change-accels}
      @about-generic{settings-gtk-color-palette}
      @about-generic{settings-gtk-color-scheme}
      @about-generic{settings-gtk-cursor-aspect-ratio}
      @about-generic{settings-gtk-cursor-blink}
      @about-generic{settings-gtk-cursor-blink-time}
      @about-generic{settings-gtk-cursor-blink-timeout}
      @about-generic{settings-gtk-cursor-theme-name}
      @about-generic{settings-gtk-cursor-theme-size}
      @about-generic{settings-gtk-decoration-layout}
      @about-generic{settings-gtk-dialogs-use-header}
      @about-generic{settings-gtk-dnd-drag-threshold}
      @about-generic{settings-gtk-double-click-distance}
      @about-generic{settings-gtk-double-click-time}
      @about-generic{settings-gtk-enable-accels}
      @about-generic{settings-gtk-enable-animations}
      @about-generic{settings-gtk-enable-event-sounds}
      @about-generic{settings-gtk-enable-input-feedback-sounds}
      @about-generic{settings-gtk-enable-mnemonics}
      @about-generic{settings-gtk-enable-primary-paste}
      @about-generic{settings-gtk-enable-tooltips}
      @about-generic{settings-gtk-entry-password-hint-timeout}
      @about-generic{settings-gtk-entry-select-on-focus}
      @about-generic{settings-gtk-error-bell}
      @about-generic{settings-gtk-fallback-icon-theme}
      @about-generic{settings-gtk-file-chooser-backend}
      @about-generic{settings-gtk-font-name}
      @about-generic{settings-gtk-fontconfig-timestamp}
      @about-generic{settings-gtk-icon-sizes}
      @about-generic{settings-gtk-icon-theme-name}
      @about-generic{settings-gtk-im-module}
      @about-generic{settings-gtk-im-preedit-style}
      @about-generic{settings-gtk-im-status-style}
      @about-generic{settings-gtk-key-theme-name}
      @about-generic{settings-gtk-keynav-cursor-only}
      @about-generic{settings-gtk-keynav-use-caret}
      @about-generic{settings-gtk-keynav-wrap-around}
      @about-generic{settings-gtk-label-select-on-focus}
      @about-generic{settings-gtk-long-press-time}
      @about-generic{settings-gtk-menu-bar-accel}
      @about-generic{settings-gtk-menu-bar-popup-delay}
      @about-generic{settings-gtk-menu-images}
      @about-generic{settings-gtk-menu-popdown-delay}
      @about-generic{settings-gtk-menu-popup-delay}
      @about-generic{settings-gtk-modules}
      @about-generic{settings-gtk-overlay-scrolling}
      @about-generic{settings-gtk-primary-button-warps-slider}
      @about-generic{settings-gtk-print-backends}
      @about-generic{settings-gtk-print-preview-command}
      @about-generic{settings-gtk-recent-files-enabled}
      @about-generic{settings-gtk-recent-files-limit}
      @about-generic{settings-gtk-recent-files-max-age}
      @about-generic{settings-gtk-scrolled-window-placement}
      @about-generic{settings-gtk-shell-shows-app-menu}
      @about-generic{settings-gtk-shell-shows-desktop}
      @about-generic{settings-gtk-shell-shows-menubar}
      @about-generic{settings-gtk-show-input-method-menu}
      @about-generic{settings-gtk-show-unicode-menu}
      @about-generic{settings-gtk-sound-theme-name}
      @about-generic{settings-gtk-split-cursor}
      @about-generic{settings-gtk-theme-name}
      @about-generic{settings-gtk-timeout-expand}
      @about-generic{settings-gtk-timeout-initial}
      @about-generic{settings-gtk-timeout-repeat}
      @about-generic{settings-gtk-titlebar-double-click}
      @about-generic{settings-gtk-titlebar-middle-click}
      @about-generic{settings-gtk-titlebar-right-click}
      @about-generic{settings-gtk-toolbar-icon-size}
      @about-generic{settings-gtk-toolbar-style}
      @about-generic{settings-gtk-tooltip-browse-mode-timeout}
      @about-generic{settings-gtk-tooltip-browse-timeout}
      @about-generic{settings-gtk-tooltip-timeout}
      @about-generic{settings-gtk-touchscreen-mode}
      @about-generic{settings-gtk-visible-focus}
      @about-generic{settings-gtk-xft-antialias}
      @about-generic{settings-gtk-xft-dpi}
      @about-generic{settings-gtk-xft-hinting}
      @about-generic{settings-gtk-xft-hintstyle}
      @about-generic{settings-gtk-xft-rgba}
      @about-function{settings-default}
      @about-function{settings-for-screen}
      @about-function{settings-install-property}
      @about-function{settings-install-property-parser}
      @about-function{rc-property-parse-color}
      @about-function{rc-property-parse-enum}
      @about-function{rc-property-parse-flags}
      @about-function{rc-property-parse-requisition}
      @about-function{rc-property-parse-border}
      @about-function{settings-set-property-value}
      @about-function{settings-set-string-property}
      @about-function{settings-set-long-property}
      @about-function{settings-set-double-property}
    @end{subsection}
    @begin[Bindings]{subsection}
      not implemented
    @end{subsection}
    @begin[Standard Enumerations]{subsection}
      @about-symbol{baseline-position}
      @about-symbol{delete-type}
      @about-symbol{direction-type}
      @about-symbol{justification}
      @about-symbol{movement-step}
      @about-symbol{orientation}
      @about-symbol{pack-type}
      @about-symbol{position-type}
      @about-symbol{relief-style}
      @about-symbol{scroll-step}
      @about-symbol{scroll-type}
      @about-symbol{selection-mode}
      @about-symbol{shadow-type}
      @about-symbol{state-flags}
      @about-symbol{toolbar-style}
      @about-symbol{sort-type}
      @about-symbol{text-direction}
      @about-symbol{expander-style}
      @about-symbol{state-type}
    @end{subsection}
    @begin[Selections]{subsection}
      The selection mechanism provides the basis for different types of
      communication between processes. In particular, drag and drop and the
      @class{gtk:clipboard} implementation work via selections. You will very
      seldom or never need to use most of the functions in this section
      directly. The @class{gtk:clipboard} implementation provides a nicer
      interface to the same functionality.

      Some of the datatypes defined in this section are used in the
      @class{gtk_clipboard} implementation and Drag and Drop APIs as well. The
      @class{gtk:target-list} structure represent lists of data types that are
      supported when sending or receiving data. The @class{gtk:selection-data}
      structure is used to store a chunk of data along with the data type and
      other associated information.
      @about-class{target-list}
      @about-function{target-list-new}
      @about-function{target-list-ref}
      @about-function{target-list-unref}
      @about-function{target-list-add}
      @about-function{target-list-add-table}
      @about-function{target-list-add-text-targets}
      @about-function{target-list-add-image-targets}
      @about-function{target-list-add-uri-targets}
      @about-function{target-list-add-rich-text-targets}
      @about-function{target-list-remove}
      @about-function{target-list-find}
      @about-function{target-table-free}
      @about-function{target-table-new-from-list}
      @about-function{selection-owner-set}
      @about-function{selection-owner-set-for-display}
      @about-function{selection-add-target}
      @about-function{selection-add-targets}
      @about-function{selection-clear-targets}
      @about-function{selection-convert}
      @about-class{selection-data}
      @about-function{selection-data-copy}
      @about-function{selection-data-set}
      @about-function{selection-data-text}
      @about-function{selection-data-pixbuf}
      @about-function{selection-data-uris}
      @about-function{selection-data-targets}
      @about-function{selection-data-targets-include-image}
      @about-function{selection-data-targets-include-text}
      @about-function{selection-data-targets-include-uri}
      @about-function{selection-data-targets-include-rich-text}
      @about-function{selection-data-selection}
      @about-function{selection-data-data}
      @about-function{selection-data-length}
      @about-function{selection-data-data-with-length}
      @about-function{selection-data-data-type}
      @about-function{selection-data-display}
      @about-function{selection-data-format}
      @about-function{selection-data-target}
      @about-function{targets-include-image}
      @about-function{targets-include-text}
      @about-function{targets-include-uri}
      @about-function{targets-include-rich-text}
      @about-function{selection-remove-all}
    @end{subsection}
    @begin[Filesystem utilities]{subsection}
      @about-class{mount-operation}
      @about-function{mount-operation-new}
      @about-function{mount-operation-is-showing}
      @about-function{mount-operation-set-parent}
      @about-function{mount-operation-get-parent}
      @about-function{mount-operation-set-screen}
      @about-function{mount-operation-get-screen}
      @about-function{show-uri}
    @end{subsection}
  @end{section}
  @begin[Theming in GTK]{section}
    @begin[GtkStyleContext]{subsection}
      @about-symbol{junction-sides}
      @about-symbol{region-flags}
      @about-symbol{style-context-print-flags}
      @about-symbol{border-style}
      @about-class{style-context}
      @about-generic{style-context-direction}
      @about-generic{style-context-paint-clock}
      @about-generic{style-context-parent}
      @about-generic{style-context-screen}
      @about-function{style-context-new}
      @about-function{style-context-add-provider}
      @about-function{style-context-add-provider-for-screen}
      @about-function{style-context-get}
      @about-function{style-context-junction-sides}
      @about-function{style-context-path}
      @about-function{style-context-property}
      @about-function{style-context-frame-clock}
      @about-function{style-context-state}
      @about-function{style-context-style}
      @about-function{style-context-style-property}
      @about-function{style-context-style-valist}
      @about-function{style-context-valist}
      @about-function{style-context-section}
      @about-function{style-context-color}
      @about-function{style-context-background-color}
      @about-function{style-context-border-color}
      @about-function{style-context-border}
      @about-function{style-context-padding}
      @about-function{style-context-margin}
      @about-function{style-context-font}
      @about-function{style-context-invalidate}
      @about-function{style-context-state-is-running}
      @about-function{style-context-lookup-color}
      @about-function{style-context-lookup-icon-set}
      @about-function{style-context-notify-state-change}
      @about-function{style-context-pop-animatable-region}
      @about-function{style-context-push-animatable-region}
      @about-function{style-context-cancel-animations}
      @about-function{style-context-scroll-animations}
      @about-function{style-context-remove-provider}
      @about-function{style-context-remove-provider-for-screen}
      @about-function{style-context-reset-widgets}
      @about-function{style-context-set-background}
      @about-function{style-context-restore}
      @about-function{style-context-save}
      @about-function{style-context-add-class}
      @about-function{style-context-remove-class}
      @about-function{style-context-has-class}
      @about-function{style-context-list-classes}
      @about-function{style-context-add-region}
      @about-function{style-context-remove-region}
      @about-function{style-context-has-region}
      @about-function{style-context-list-regions}
      @about-function{style-context-scale}
      @about-function{style-context-to-string}
      @about-struct{border}
      @about-function{border-left}
      @about-function{border-right}
      @about-function{border-top}
      @about-function{border-bottom}
      @about-function{border-new}
      @about-function{border-copy}
      @about-function{render-arrow}
      @about-function{render-background}
      @about-function{render-background-clip}
      @about-function{render-check}
      @about-function{render-expander}
      @about-function{render-extension}
      @about-function{render-focus}
      @about-function{render-frame}
      @about-function{render-frame-gap}
      @about-function{render-handle}
      @about-function{render-layout}
      @about-function{render-line}
      @about-function{render-option}
      @about-function{render-slider}
      @about-function{render-activity}
      @about-function{render-icon-pixbuf}
      @about-function{render-icon-surface}
      @about-function{render-icon}
      @about-function{render-insertion-cursor}
    @end{subsection}
    @begin[GtkCssProvider]{subsection}
      @about-symbol{css-provider-error}
      @about-class{css-provider}
      @about-function{css-provider-default}
      @about-function{css-provider-named}
      @about-function{css-provider-load-from-data}
      @about-function{css-provider-load-from-file}
      @about-function{css-provider-load-from-path}
      @about-function{css-provider-load-from-resource}
      @about-function{css-provider-new}
      @about-function{css-provider-to-string}
      @about-symbol{css-section-type}
      @about-class{css-section}
      @about-function{css-section-end-line}
      @about-function{css-section-end-position}
      @about-function{css-section-file}
      @about-function{css-section-parent}
      @about-function{css-section-section-type}
      @about-function{css-section-start-line}
      @about-function{css-section-start-position}
    @end{subsection}
    @begin[GtkStyleProvider]{subsection}
      @about-variable{+priority-fallback+}
      @about-variable{+priority-theme+}
      @about-variable{+priority-settings+}
      @about-variable{+priority-application+}
      @about-variable{+priority-user+}
      @about-class{style-provider}
      @about-function{style-provider-style-property}
    @end{subsection}
    @begin[GtkWidgetPath]{subsection}
      @about-class{widget-path}
      @about-function{widget-path-append-type}
      @about-function{widget-path-append-with-siblings}
      @about-function{widget-path-append-for-widget}
      @about-function{widget-path-copy}
      @about-function{widget-path-ref}
      @about-function{widget-path-unref}
      @about-function{widget-path-free}
      @about-function{widget-path-object-type}
      @about-function{widget-path-has-parent}
      @about-function{widget-path-is-type}
      @about-function{widget-path-iter-add-class}
      @about-function{widget-path-iter-add-region}
      @about-function{widget-path-iter-clear-classes}
      @about-function{widget-path-iter-clear-regions}
      @about-function{widget-path-iter-name}
      @about-function{widget-path-iter-object-name}
      @about-function{widget-path-iter-object-type}
      @about-function{widget-path-iter-siblings}
      @about-function{widget-path-iter-sibling-index}
      @about-function{widget-path-iter-state}
      @about-function{widget-path-iter-has-class}
      @about-function{widget-path-iter-has-name}
      @about-function{widget-path-iter-has-qclass}
      @about-function{widget-path-iter-has-qname}
      @about-function{widget-path-iter-has-qregion}
      @about-function{widget-path-iter-has-region}
      @about-function{widget-path-iter-list-classes}
      @about-function{widget-path-iter-list-regions}
      @about-function{widget-path-iter-remove-class}
      @about-function{widget-path-iter-remove-region}
      @about-function{widget-path-length}
      @about-function{widget-path-new}
      @about-function{widget-path-prepend-type}
      @about-function{widget-path-to-string}
    @end{subsection}
    @begin[GtkIconTheme]{subsection}
      @about-symbol{icon-lookup-flags}
      @about-symbol{icon-theme-error}
      @about-class{icon-theme}
      @about-function{icon-theme-new}
      @about-function{icon-theme-default}
      @about-function{icon-theme-for-screen}
      @about-function{icon-theme-set-screen}
      @about-function{icon-theme-search-path}
      @about-function{icon-theme-append-search-path}
      @about-function{icon-theme-prepend-search-path}
      @about-function{icon-theme-add-resource-path}
      @about-function{icon-theme-set-custom-theme}
      @about-function{icon-theme-has-icon}
      @about-function{icon-theme-lookup-icon}
      @about-function{icon-theme-lookup-icon-for-scale}
      @about-function{icon-theme-choose-icon}
      @about-function{icon-theme-choose-icon-for-scale}
      @about-function{icon-theme-lookup-by-gicon}
      @about-function{icon-theme-lookup-by-gicon-for-scale}
      @about-function{icon-theme-load-icon}
      @about-function{icon-theme-load-icon-for-scale}
      @about-function{icon-theme-load-surface}
      @about-function{icon-theme-list-contexts}
      @about-function{icon-theme-list-icons}
      @about-function{icon-theme-icon-sizes}
      @about-function{icon-theme-example-icon-name}
      @about-function{icon-theme-rescan-if-needed}
      @about-function{icon-theme-add-builtin-icon}
      @about-symbol{icon-info}
      @about-function{icon-info-copy}
      @about-function{icon-info-free}
      @about-function{icon-info-new-for-pixbuf}
      @about-function{icon-info-base-size}
      @about-function{icon-info-base-scale}
      @about-function{icon-info-filename}
      @about-function{icon-info-builtin-pixbuf}
      @about-function{icon-info-load-icon}
      @about-function{icon-info-load-surface}
      @about-function{icon-info-load-icon-async}
      @about-function{icon-info-load-icon-finish}
      @about-function{icon-info-load-symbolic}
      @about-function{icon-info-load-symbolic-for-style}
      @about-function{icon-info-load-symbolic-for-context}
      @about-function{icon-info-load-symbolic-for-context-async}
      @about-function{icon-info-load-symbolic-for-context-finish}
      @about-function{icon-info-set-raw-coordinates}
      @about-function{icon-info-embedded-rect}
      @about-function{icon-info-attach-points}
      @about-function{icon-info-display-name}
      @about-function{icon-info-is-symbolic}
    @end{subsection}
  @end{section}
  @begin[Deprecated]{section}
    @begin[Deprecated since GTK 3.0]{subsection}@end{subsection}
    @begin[Resource Files]{subsection}
      Deprecated routines for handling resource files.

      In GTK 3.0, resource files have been deprecated and replaced by CSS-like
      style sheets, which are understood by the @class{gtk:css-provider} object.
      In the Lisp binding the implementation was never very complete. The few
      symbols are not exported.
    @end{subsection}
    @begin[GtkStyle]{subsection}
      Deprecated object that holds style information for widgets.

      In GTK 3.0, the GtkStyle object has been deprecated and replaced by the
      @class{gtk:style-context} object. In the Lisp binding the implementation
      was never very complete. The few symbols are not exported.
    @end{subsection}

    @begin[Deprecated since GTK 3.2]{subsection}@end{subsection}
    @begin[GtkHBox]{subsection}
      A deprecated horizontal container box.

      The GtkHBox widget has been deprecated since GTK 3.2. You can use the
      @class{gtk:box} widget with the value @code{:horizontal} for the
      @slot[gtk:orientable]{orientation} property instead, which is a very quick
      and easy change. If you have derived your own classes from the GtkHBox
      class, you can simply change the inheritance to derive directly from the
      @class{gtk:box} class. No further changes are needed, since the default
      value of the @slot[gtk:orientable]{orientation} property is
      @code{:horizontal}.

      If you want your code to be future-proof, the recommendation is to switch
      to the @class{gtk:grid} widget, since the @class{gtk:box} widget is going
      to be deprecated in favor of the more flexible grid widget eventually.

      In the Lisp binding the symbols and functions for the GtkHBox widget are
      not exported.
    @end{subsection}
    @begin[GtkVBox]{subsection}
      A deprecated vertical container box.

      The GtkVBox widget has been deprecated since GTK 3.2. You can use the
      @class{gtk:box} widget instead, which is a very quick and easy change. If
      you have derived your own classes from the GtkVBox class, you can simply
      change the inheritance to derive directly from the @class{gtk:box} class,
      and set the @slot[gtk:orientable]{orientation} property to
      @code{:vertical} in your instance init function.

      If you want your code to be future-proof, the recommendation is to switch
      to the @class{gtk:grid} widget, since the @class{gtk:box} widget is going
      to be deprecated in favor of the more flexible grid widget eventually.

      In the Lisp binding the symbols and functions for the GtkVBox widget are
      not exported.
    @end{subsection}
    @begin[GtkHButtonBox]{subsection}
      A deprecated container for arranging buttons horizontally.

      The GtkHButtonBox widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:button-box} widget with
      the value @code{:horizontal} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkHButtonBox widget
      are not exported.
    @end{subsection}
    @begin[GtkVButtonBox]{subsection}
      A deprecated container for arranging buttons vertically.

      The GtkVButtonBox widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:button-box} widget with
      the value @code{:vertical} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkVButtonBox widget
      are not exported.
    @end{subsection}
    @begin[GtkHPaned]{subsection}
      A deprecated container with two panes arranged horizontally.

      The GtkHPaned widget has been deprecated since GTK 3.2 and should not be
      used in newly written code. Use the @class{gtk:paned} widget with the
      value @code{:horizontal} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkHPaned widget are
      not exported.
    @end{subsection}
    @begin[GtkVPaned]{subsection}
      A deprecated container with two panes arranged vertically.

      The GtkVPaned widget has been deprecated since GTK 3.2 and should not be
      used in newly written code. Use the @class{gtk:paned} widget with the
      value @code{:vertical} for the @slot[gtk:orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for the GtkVPaned widget are
      not exported.
    @end{subsection}
    @begin[GtkHScale]{subsection}
      A deprecated horizontal slider widget for selecting a value from a range.

      The GtkHScale widget has been deprecated since GTK 3.2 and should not be
      used in newly written code. Use the @class{gtk:scale} widget with the
      value @code{:horizontal} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkHScale widget are
      not exported.
    @end{subsection}
    @begin[GtkVScale]{subsection}
      A deprecated vertical slider widget for selecting a value from a range.

      The GtkVScale widget has been deprecated since GTK 3.2 and should not be
      used in newly written code. Use the @class{gtk:scale} widget with the
      value @code{:vertical} for the @slot[gtk:orientable]{orientation} property
      instead.

      In the Lisp binding the symbols and functions for the GtkVScale widget are
      not exported.
    @end{subsection}
    @begin[GtkHSeparator]{subsection}
      A deprecated horizontal separator widget.

      The GtkHSeparator widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:separator} widget with
      the value @code{:horizontal} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkHSeparator widget
      are not exported.
    @end{subsection}
    @begin[GtkVSeparator]{subsection}
      A deprecated vertical separator widget.

      The GtkVSeparator widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:separator} widget with
      the value @code{:vertical} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkVSeparator widget
      are not exported.
    @end{subsection}
    @begin[GtkHScrollbar]{subsection}
      A deprecated horizontal scrollbar.

      The GtkHScrollbar widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:scrollbar} widget with
      the value @code{:horizontal} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkHScrollbar widget
      are not exported.
    @end{subsection}
    @begin[GtkVScrollbar]{subsection}
      A deprecated vertical scrollbar.

      The GtkVScrollbar widget has been deprecated since GTK 3.2 and should not
      be used in newly written code. Use the @class{gtk:scrollbar} widget with
      the value @code{:vertical} for the @slot[gtk:orientable]{orientation}
      property instead.

      In the Lisp binding the symbols and functions for the GtkVScrollbar widget
      are not exported.
    @end{subsection}
    @begin[GtkFontSelection]{subsection}
      Deprecated widget for selecting fonts.

      The GtkFontSelection widget is deprecated since GTK 3.2 and should not be
      used in newly written code. Use the widgets that implement the
      @class{gtk:font-chooser} interface instead.

      In the Lisp binding the symbols and functions for the GtkFontSelection
      widget are not exported.
    @end{subsection}
    @begin[GtkFontSelectionDialog]{subsection}
      Deprecated dialog box for selecting fonts.

      The GtkFontSelectionDialog widget is deprecated since GTK 3.2 and should
      not be used in newly written code. Use the @class{gtk:font-chooser-dialog}
      widget instead.

      In the Lisp binding the symbols and functions for the
      GtkFontSelectionDialog widget are not exported.
    @end{subsection}

    @begin[Deprecated since GTK 3.4]{subsection}@end{subsection}
    @begin[GtkHandleBox]{subsection}
      @about-class{handle-box}
      @about-generic{handle-box-child-detached}
      @about-generic{handle-box-handle-position}
      @about-generic{handle-box-shadow-type}
      @about-generic{handle-box-snap-edge}
      @about-generic{handle-box-snap-edge-set}
      @about-function{handle-box-new}
    @end{subsection}
    @begin[GtkTable]{subsection}
      @about-symbol{attach-options}
      @about-class{table}
      @about-generic{table-column-spacing}
      @about-generic{table-homogeneous}
      @about-generic{table-n-columns}
      @about-generic{table-n-rows}
      @about-generic{table-row-spacing}
      @about-function{table-child-left-attach}
      @about-function{table-child-right-attach}
      @about-function{table-child-top-attach}
      @about-function{table-child-bottom-attach}
      @about-function{table-child-x-options}
      @about-function{table-child-y-options}
      @about-function{table-child-x-padding}
      @about-function{table-child-y-padding}
      @about-function{table-new}
      @about-function{table-resize}
      @about-function{table-size}
      @about-function{table-attach}
      @about-function{table-set-row-spacing}
      @about-function{table-set-col-spacing}
      @about-function{table-get-row-spacing}
      @about-function{table-get-col-spacing}
    @end{subsection}
    @begin[GtkTearoffMenuItem]{subsection}
      A deprecated menu item used to tear off and reattach its menu.
      @about-class{tearoff-menu-item}
      @about-function{tearoff-menu-item-new}
    @end{subsection}
    @begin[GtkColorSelection]{subsection}
      @about-class{color-selection}
      @about-generic{color-selection-current-alpha}
      @about-generic{color-selection-current-color}
      @about-generic{color-selection-current-rgba}
      @about-generic{color-selection-has-opacity-control}
      @about-generic{color-selection-has-palette}
      @about-function{color-selection-new}
      @about-function{color-selection-previous-alpha}
      @about-function{color-selection-previous-color}
      @about-function{color-selection-previous-rgba}
      @about-function{color-selection-is-adjusting}
      @about-function{color-selection-palette-from-string}
      @about-function{color-selection-palette-to-string}
      @about-function{color-selection-set-change-palette-with-screen-hook}
    @end{subsection}
    @begin[GtkColorSelectionDialog]{subsection}
      @about-class{color-selection-dialog}
      @about-generic{color-selection-dialog-cancel-button}
      @about-generic{color-selection-dialog-color-selection}
      @about-generic{color-selection-dialog-help-button}
      @about-generic{color-selection-dialog-ok-button}
      @about-function{color-selection-dialog-new}
    @end{subsection}
    @begin[GtkHSV]{subsection}
      @about-class{hsv}
      @about-function{hsv-new}
      @about-function{hsv-set-color}
      @about-function{hsv-get-color}
      @about-function{hsv-set-metrics}
      @about-function{hsv-get-metrics}
      @about-function{hsv-is-adjusting}
      @about-function{hsv-to-rgb}
      @about-function{rgb-to-hsv}
    @end{subsection}
    @begin[Deprecated since GTK 3.8]{subsection}@end{subsection}
    @begin[GtkSymbolicColor]{subsection}
      GtkSymbolicColor is deprecated since version 3.8. Symbolic colors are
      considered an implementation detail of GTK. In the Lisp binding no
      symbols or functions of GtkSymbolicColor are implemented.
    @end{subsection}
    @begin[GtkGradient]{subsection}
      GtkGradient is deprecated since version 3.8. It was used internally by
      GTK’s CSS engine to represent gradients. As its handling is not conforming
      to modern web standards, it is not used anymore. If you want to use
      gradients in your own code, please use Cairo directly. In the Lisp binding
      no symbols or functions of GtkGradient are implemented.
    @end{subsection}
    @begin[Deprecated since GTK 3.10]{subsection}@end{subsection}
    @begin[GtkUIManager]{subsection}
      @about-symbol{ui-manager-item-type}
      @about-class{ui-manager}
      @about-generic{ui-manager-add-tearoffs}
      @about-generic{ui-manager-ui}
      @about-function{ui-manager-new}
      @about-function{ui-manager-insert-action-group}
      @about-function{ui-manager-remove-action-group}
      @about-function{ui-manager-action-groups}
      @about-function{ui-manager-accel-group}
      @about-function{ui-manager-widget}
      @about-function{ui-manager-toplevels}
      @about-function{ui-manager-action}
      @about-function{ui-manager-add-ui-from-resource}
      @about-function{ui-manager-add-ui-from-string}
      @about-function{ui-manager-add-ui-from-file}
      @about-function{ui-manager-new-merge-id}
      @about-function{ui-manager-add-ui}
      @about-function{ui-manager-remove-ui}
      @about-function{ui-manager-ensure-update}
    @end{subsection}
    @begin[GtkActivatable]{subsection}
      @about-class{activatable}
      @about-generic{activatable-related-action}
      @about-generic{activatable-use-action-appearance}
      @about-function{activatable-do-set-related-action}
      @about-function{activatable-sync-action-properties}
    @end{subsection}
    @begin[GtkActionGroup]{subsection}
      @about-class{action-group}
      @about-generic{action-group-accel-group}
      @about-generic{action-group-name}
      @about-generic{action-group-sensitive}
      @about-generic{action-group-visible}
      @about-function{action-group-new}
      @about-function{action-group-action}
      @about-function{action-group-list-actions}
      @about-function{action-group-add-action}
      @about-function{action-group-add-action-with-accel}
      @about-function{action-group-remove-action}
      @about-symbol{action-entry}
      @about-function{action-group-add-actions}
      @about-function{action-group-add-actions-full}
      @about-symbol{toggle-action-entry}
      @about-function{action-group-add-toggle-actions}
      @about-function{action-group-add-toggle-actions-full}
      @about-symbol{radio-action-entry}
      @about-function{action-group-add-radio-actions}
      @about-function{action-group-add-radio-actions-full}
      @about-symbol{translate-func}
      @about-function{action-group-set-translate-func}
      @about-function{action-group-set-translation-domain}
      @about-function{action-group-translate-string}
    @end{subsection}
    @begin[GtkAction]{subsection}
      @about-class{action}
      @about-generic{action-action-group}
      @about-generic{action-always-show-image}
      @about-generic{action-gicon}
      @about-generic{action-hide-if-empty}
      @about-generic{action-icon-name}
      @about-generic{action-is-important}
      @about-generic{action-label}
      @about-generic{action-name}
      @about-generic{action-sensitive}
      @about-generic{action-short-label}
      @about-generic{action-stock-id}
      @about-generic{action-tooltip}
      @about-generic{action-visible}
      @about-generic{action-visible-horizontal}
      @about-generic{action-visible-overflown}
      @about-generic{action-visible-vertical}
      @about-function{action-new}
      @about-function{action-is-sensitive}
      @about-function{action-is-visible}
      @about-function{action-activate}
      @about-function{action-create-icon}
      @about-function{action-create-menu-item}
      @about-function{action-create-tool-item}
      @about-function{action-create-menu}
      @about-function{action-proxies}
      @about-function{action-connect-accelerator}
      @about-function{action-disconnect-accelerator}
      @about-function{action-block-activate}
      @about-function{action-unblock-activate}
      @about-function{action-accel-path}
      @about-function{action-accel-closure}
      @about-function{action-set-accel-group}
    @end{subsection}
    @begin[GtkToggleAction]{subsection}
      @about-class{toggle-action}
      @about-generic{toggle-action-active}
      @about-generic{toggle-action-draw-as-radio}
      @about-function{toggle-action-new}
      @about-function{toggle-action-toggled}
    @end{subsection}
    @begin[GtkRadioAction]{subsection}
      @about-class{radio-action}
      @about-generic{radio-action-current-value}
      @about-generic{radio-action-group}
      @about-generic{radio-action-value}
      @about-function{radio-action-new}
      @about-function{radio-action-join-group}
    @end{subsection}
    @begin[GtkRecentAction]{subsection}
      @about-class{recent-action}
      @about-generic{recent-action-show-numbers}
      @about-function{recent-action-new}
      @about-function{recent-action-new-for-manager}
    @end{subsection}
    @begin[GtkImageMenuItem]{subsection}
      @about-class{image-menu-item}
      @about-generic{image-menu-item-accel-group}
      @about-generic{image-menu-item-always-show-image}
      @about-generic{image-menu-item-image}
      @about-generic{image-menu-item-use-stock}
      @about-function{image-menu-item-new}
      @about-function{image-menu-item-new-from-stock}
      @about-function{image-menu-item-new-with-label}
      @about-function{image-menu-item-new-with-mnemonic}
      @about-function{image-menu-item-set-accel-group}
    @end{subsection}
    @begin[GtkMisc]{subsection}
      @about-class{misc}
      @about-function{misc-set-alignment}
      @about-function{misc-set-padding}
      @about-function{misc-get-alignment}
      @about-function{misc-get-padding}
    @end{subsection}
    @begin[Stock items]{subsection}
      Deprecated prebuilt common menu/toolbar items and corresponding icons.

      Since GTK 3.10, stock items are deprecated. You should instead set up
      whatever labels and/or icons you need using normal widget API, rather
      than relying on GTK providing ready-made combinations of these.

      In the Lisp binding no symbols and functions are implemented.
    @end{subsection}
    @begin[Themable Stock Images]{subsection}
      @about-symbol{icon-size}
      @about-function{icon-size-lookup}
      @about-function{icon-size-lookup-for-settings}
      @about-function{icon-size-register}
      @about-function{icon-size-register-alias}
      @about-function{icon-size-from-name}
      @about-function{icon-size-get-name}
      @about-function{icon-size-get-sizes}
      @about-class{icon-source}
      @about-function{icon-source-copy}
      @about-function{icon-source-free}
      @about-function{icon-source-get-direction}
      @about-function{icon-source-get-direction-wildcarded}
      @about-function{icon-source-filename}
      @about-function{icon-source-get-pixbuf}
      @about-function{icon-source-icon-name}
      @about-function{icon-source-get-size}
      @about-function{icon-source-get-size-wildcarded}
      @about-function{icon-source-get-state}
      @about-function{icon-source-get-state-wildcarded}
      @about-function{icon-source-new}
      @about-function{icon-source-set-direction}
      @about-function{icon-source-set-direction-wildcarded}
      @about-function{icon-source-set-pixbuf}
      @about-function{icon-source-set-size}
      @about-function{icon-source-set-size-wildcarded}
      @about-function{icon-source-set-state}
      @about-function{icon-source-set-state-wildcarded}
      @about-class{icon-set}
      @about-function{icon-set-add-source}
      @about-function{icon-set-copy}
      @about-function{icon-set-new}
      @about-function{icon-set-new-from-pixbuf}
      @about-function{icon-set-ref}
      @about-function{icon-set-render-icon}
      @about-function{icon-set-render-icon-pixbuf}
      @about-function{icon-set-unref}
      @about-class{icon-factory}
      @about-function{icon-factory-add}
      @about-function{icon-factory-add-default}
      @about-function{icon-factory-lookup}
      @about-function{icon-factory-lookup-default}
      @about-function{icon-factory-new}
      @about-function{icon-factory-remove-default}
    @end{subsection}
    @begin[Deprecated since GTK 3.14]{subsection}@end{subsection}
    @begin[GtkNumerableIcon]{subsection}
      @about-class{numerable-icon}
      @about-generic{numerable-icon-background-icon}
      @about-generic{numerable-icon-background-icon-name}
      @about-generic{numerable-icon-count}
      @about-generic{numerable-icon-label}
      @about-generic{numerable-icon-style-context}
      @about-function{numerable-icon-new}
      @about-function{numerable-icon-new-with-style-context}
    @end{subsection}
    @begin[GtkArrow]{subsection}
      @about-class{arrow}
      @about-generic{arrow-arrow-type}
      @about-generic{arrow-shadow-type}
      @about-function{arrow-new}
      @about-function{arrow-set}
    @end{subsection}
    @begin[GtkStatusIcon]{subsection}
      @about-class{status-icon}
      @about-generic{status-icon-embedded}
      @about-generic{status-icon-file}
      @about-generic{status-icon-gicon}
      @about-generic{status-icon-has-tooltip}
      @about-generic{status-icon-icon-name}
      @about-generic{status-icon-orientation}
      @about-generic{status-icon-pixbuf}
      @about-generic{status-icon-screen}
      @about-generic{status-icon-size}
      @about-generic{status-icon-stock}
      @about-generic{status-icon-storage-type}
      @about-generic{status-icon-title}
      @about-generic{status-icon-tooltip-markup}
      @about-generic{status-icon-tooltip-text}
      @about-generic{status-icon-visible}
      @about-function{status-icon-new}
      @about-function{status-icon-new-from-pixbuf}
      @about-function{status-icon-new-from-file}
      @about-function{status-icon-new-from-stock}
      @about-function{status-icon-new-from-icon-name}
      @about-function{status-icon-new-from-gicon}
      @about-function{status-icon-set-from-pixbuf}
      @about-function{status-icon-set-from-file}
      @about-function{status-icon-set-from-stock}
      @about-function{status-icon-set-from-icon-name}
      @about-function{status-icon-set-from-gicon}
      @about-function{status-icon-set-name}
      @about-function{status-icon-is-embedded}
      @about-function{status-icon-position-menu}
      @about-function{status-icon-get-geometry}
      @about-function{status-icon-get-x11-window-id}
    @end{subsection}
    @begin[GtkThemingEngine]{subsection}
      Deprecated class for theming renderers. The GtkThemingEngine class has
      been deprecated in GTK 3.14 and will be ignored for rendering. The
      advancements in CSS theming are good enough to allow themers to achieve
      their goals without the need to modify source code.

      In the Lisp binding no symbols and functions are implemented.
    @end{subsection}
    @begin[GtkAlignment]{subsection}
      @about-class{alignment}
      @about-generic{alignment-bottom-padding}
      @about-generic{alignment-left-padding}
      @about-generic{alignment-right-padding}
      @about-generic{alignment-top-padding}
      @about-generic{alignment-xalign}
      @about-generic{alignment-xscale}
      @about-generic{alignment-yalign}
      @about-generic{alignment-yscale}
      @about-function{alignment-new}
      @about-function{alignment-set}
      @about-function{alignment-get-padding}
      @about-function{alignment-set-padding}
    @end{subsection}
    @begin[Deprecated since GTK 3.16]{subsection}@end{subsection}
    @begin[GtkStyleProperties]{subsection}
      Deprecated store for style property information.

      GtkStyleProperties has been deprecated in GTK 3.16. The CSS machinery does
      not use it anymore and all users of this object have been deprecated.
      No symbols und functions for GtkStyleProperties are implemented in the
      Lisp binding.
    @end{subsection}
  @end{section}
  @begin[Symbols not implemented or exported]{section}
    The following symbols in the C library have been deprecated since version
    3.16 or longer. These symbols are not implemented in or not exported from
    the Lisp library.
    @begin{pre}
gtk_action_group_new
gtk_action_group_get_action
gtk_action_group_list_actions
gtk_action_group_add_action
gtk_action_group_add_action_with_accel
gtk_action_group_remove_action
gtk_action_group_add_actions
gtk_action_group_add_actions_full
gtk_action_group_add_toggle_actions
gtk_action_group_add_toggle_actions_full
gtk_action_group_add_radio_actions
gtk_action_group_add_radio_actions_full
gtk_action_group_set_translate_func
gtk_action_group_set_translation_domain
gtk_action_group_translate_string

gtk_application_add_accelerator
gtk_application_remove_accelerator

gtk_alternative_dialog_button_order

gtk_assistant_set_page_header_image
gtk_assistant_get_page_header_image
gtk_assistant_set_page_side_image
gtk_assistant_get_page_side_image

gtk_button_new_from_stock
gtk_button_pressed
gtk_button_released
gtk_button_enter
gtk_button_leave
gtk_button_set_alignment
gtk_button_get_alignment

gtk_dialog_set_alternative_button_order
gtk_dialog_set_alternative_button_order_from_array

gtk_image_get_icon_set
gtk_image_get_stock
gtk_image_new_from_icon_set
gtk_image_new_from_stock
gtk_image_set_from_icon_set
gtk_image_set_from_stock

gtk_notebook_get_tab_hborder
gtk_notebook_get_tab_vborder
    @end{pre}
  @end{section}")

;;; --- End of file gtk3.package.lisp ------------------------------------------
