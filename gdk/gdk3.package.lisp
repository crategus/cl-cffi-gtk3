;;; ----------------------------------------------------------------------------
;;; gdk3.package.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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

(defpackage :gdk
  (:use :iterate :common-lisp)
  (:import-from #:cffi      #:defcfun
                            #:defcenum
                            #:defbitfield
                            #:defcallback
                            #:defcstruct
                            #:define-foreign-type
                            #:define-parse-method
                            #:with-foreign-object
                            #:with-foreign-objects
                            #:with-foreign-slots)
  (:import-from #:glib      #:+g-priority-high-idle+
                            #:+g-priority-default+
                            #:+g-priority-default-idle+
                            #:with-g-error
                            #:with-ignore-g-error
                            #:with-stable-pointer)
  (:import-from #:gobject   #:define-g-enum
                            #:define-g-flags
                            #:define-g-object-class
                            #:define-g-interface)
  ;; Import the symbols from GDK-PIXUF
  ;; TODO: We have a problem with the documentation. The symbols are documented
  ;; twice as gdk:pixbuf and as gdk-pixbuf:pixbuf. Resolve this issue.
  (:import-from #:gdk-pixbuf ;; Symbols from gdk-pixbuf.structure.lisp
                             #:colorspace
                             #:pixbuf
                             #:pixbuf-bits-per-sample
                             #:pixbuf-colorspace
                             #:pixbuf-has-alpha
                             #:pixbuf-height
                             #:pixbuf-n-channels
                             #:pixbuf-pixel-bytes
                             #:pixbuf-pixels
                             #:pixbuf-rowstride
                             #:pixbuf-width
                             #:pixbuf-pixels-with-length
                             #:pixbuf-byte-length
                             #:pixbuf-option
                             #:pixbuf-remove-option
                             #:pixbuf-copy-options
                             #:pixbuf-read-pixels
                             ;; Symbols from gdk-pixbuf.load.lisp
                             #:pixbuf-file-info
                             #:pixbuf-new-from-file
                             #:pixbuf-new-from-file-at-size
                             #:pixbuf-new-from-file-at-scale
                             #:pixbuf-new-from-resource
                             #:pixbuf-new-from-resource-at-scale
                             ;; Symbols from gdk-pixbuf.loader.lisp
                             #:pixbuf-loader
                             #:pixbuf-loader-new
                             #:pixbuf-loader-write
                             #:pixbuf-loader-set-size
                             #:pixbuf-loader-pixbuf
                             #:pixbuf-loader-animation
                             #:pixbuf-loader-close
                             ;; Symbols from gdk-pixbuf.save.lisp
                             #:pixbuf-save
                             ;; Symbols from gdk-pixbuf.memory.lisp
                             #:pixbuf-new
                             #:pixbuf-new-subpixbuf
                             #:pixbuf-copy
                             ;; Symbols from gdk-pixbuf.scaling.lisp
                             #:pixbuf-interp-type
                             #:pixbuf-rotation
                             #:pixbuf-scale-simple
                             #:pixbuf-scale
                             #:pixbuf-composite-color-simple
                             #:pixbuf-composite
                             #:pixbuf-composite-color
                             #:pixbuf-rotate-simple
                             #:pixbuf-flip
                             ;; Symbols from gdk-pixbuf.utilities.lisp
                             #:pixbuf-add-alpha
                             #:pixbuf-copy-area
                             #:pixbuf-fill
                             ;; Symbols from gdk-pixbuf.animation.lisp
                             #:pixbuf-animation
                             #:pixbuf-animation-loop
                             #:pixbuf-animation-new-from-file
                             #:pixbuf-animation-new-from-resource
                             #:pixbuf-animation-static-image)
  ;; Export the symbols for GDK-PIXBUF
  (:export                   ;; Symbols from gdk-pixbuf.structure.lisp
                             #:colorspace
                             #:pixbuf
                             #:pixbuf-bits-per-sample
                             #:pixbuf-colorspace
                             #:pixbuf-has-alpha
                             #:pixbuf-height
                             #:pixbuf-n-channels
                             #:pixbuf-pixel-bytes
                             #:pixbuf-pixels
                             #:pixbuf-rowstride
                             #:pixbuf-width
                             #:pixbuf-pixels-with-length
                             #:pixbuf-byte-length
                             #:pixbuf-option
                             #:pixbuf-remove-option
                             #:pixbuf-copy-options
                             #:pixbuf-read-pixels
                             ;; Symbols from gdk-pixbuf.load.lisp
                             #:pixbuf-file-info
                             #:pixbuf-new-from-file
                             #:pixbuf-new-from-file-at-size
                             #:pixbuf-new-from-file-at-scale
                             #:pixbuf-new-from-resource
                             #:pixbuf-new-from-resource-at-scale
                             ;; Symbols from gdk-pixbuf.loader.lisp
                             #:pixbuf-loader
                             #:pixbuf-loader-new
                             #:pixbuf-loader-write
                             #:pixbuf-loader-set-size
                             #:pixbuf-loader-pixbuf
                             #:pixbuf-loader-animation
                             #:pixbuf-loader-close
                             ;; Symbols from gdk-pixbuf.save.lisp
                             #:pixbuf-save
                             ;; Symbols from gdk-pixbuf.memory.lisp
                             #:pixbuf-new
                             #:pixbuf-new-subpixbuf
                             #:pixbuf-copy
                             ;; Symbols from gdk-pixbuf.scaling.lisp
                             #:pixbuf-interp-type
                             #:pixbuf-rotation
                             #:pixbuf-scale-simple
                             #:pixbuf-scale
                             #:pixbuf-composite-color-simple
                             #:pixbuf-composite
                             #:pixbuf-composite-color
                             #:pixbuf-rotate-simple
                             #:pixbuf-flip
                             ;; Symbols from gdk-pixbuf.utilities.lisp
                             #:pixbuf-add-alpha
                             #:pixbuf-copy-area
                             #:pixbuf-fill
                             ;; Symbols from gdk-pixbuf.animation.lisp
                             #:pixbuf-animation
                             #:pixbuf-animation-loop
                             #:pixbuf-animation-new-from-file
                             #:pixbuf-animation-new-from-resource
                             #:pixbuf-animation-static-image))

(in-package :gdk)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gdk) t)
 "GDK is an intermediate layer which isolates GTK from the details of the
  windowing system. This is the API documentation of a Lisp binding to GDK.
  @begin[General]{section}
    This section describes the GDK initialization functions and miscellaneous
    utility functions, as well as deprecation facilities.
    @about-function{init}
    @about-function{init-check}
    @about-function{parse-args}
    @about-function{get-display-arg-name}
    @about-function{notify-startup-complete}
    @about-function{notify-startup-complete-with-id}
    @about-function{set-allowed-backends}
    @about-function{program-class}
    @about-function{flush}
    @about-function{pointer-grab}
    @about-function{pointer-ungrab}
    @about-function{pointer-is-grabbed}
    @about-function{set-double-click-time}
    @about-function{keyboard-grab}
    @about-function{keyboard-ungrab}
    @about-function{beep}
    @about-function{error-trap-push}
    @about-function{error-trap-pop}
    @about-function{error-trap-pop-ignored}
  @end{section}
  @begin[GdkDisplayManager]{section}
    Maintains a list of all open GdkDisplays.
    @about-class{display-manager}
    @about-generic{display-manager-default-display}
    @about-function{display-manager-get}
    @about-function{display-manager-list-displays}
    @about-function{display-manager-open-display}
  @end{section}
  @begin[GdkDisplay]{section}
    Controls a set of GdkScreens and their associated input devices.
    @about-class{display}
    @about-function{display-open}
    @about-function{display-default}
    @about-function{display-name}
    @about-function{display-n-screens}
    @about-function{display-screen}
    @about-function{display-default-screen}
    @about-function{display-device-manager}
    @about-function{display-pointer-ungrab}
    @about-function{display-keyboard-ungrab}
    @about-function{display-pointer-is-grabbed}
    @about-function{display-device-is-grabbed}
    @about-function{display-beep}
    @about-function{display-sync}
    @about-function{display-flush}
    @about-function{display-close}
    @about-function{display-is-closed}
    @about-function{display-event}
    @about-function{display-peek-event}
    @about-function{display-put-event}
    @about-function{display-has-pending}
    @about-function{display-set-double-click-time}
    @about-function{display-set-double-click-distance}
    @about-function{display-pointer}
    @about-function{display-list-devices}
    @about-function{display-window-at-pointer}
    @about-function{display-warp-pointer}
    @about-function{display-supports-cursor-color}
    @about-function{display-supports-cursor-alpha}
    @about-function{display-default-cursor-size}
    @about-function{display-maximal-cursor-size}
    @about-function{display-default-group}
    @about-function{display-supports-selection-notification}
    @about-function{display-request-selection-notification}
    @about-function{display-supports-clipboard-persistence}
    @about-function{display-store-clipboard}
    @about-function{display-supports-shapes}
    @about-function{display-supports-input-shapes}
    @about-function{display-supports-composite}
    @about-function{display-app-launch-context}
    @about-function{display-notify-startup-complete}
    @about-function{display-default-seat}
    @about-function{display-list-seats}
    @about-function{display-n-monitors}
    @about-function{display-monitor}
    @about-function{display-primary-monitor}
    @about-function{display-monitor-at-point}
    @about-function{display-monitor-at-window}
  @end{section}
  @begin[GdkScreen]{section}
    Object representing a physical screen.
    @about-class{screen}
    @about-generic{screen-font-options}
    @about-generic{screen-resolution}
    @about-function{screen-default}
    @about-function{screen-system-visual}
    @about-function{screen-rgba-visual}
    @about-function{screen-is-composited}
    @about-function{screen-root-window}
    @about-function{screen-display}
    @about-function{screen-number}
    @about-function{screen-width}
    @about-function{screen-height}
    @about-function{screen-width-mm}
    @about-function{screen-height-mm}
    @about-function{screen-list-visuals}
    @about-function{screen-toplevel-windows}
    @about-function{screen-make-display-name}
    @about-function{screen-n-monitors}
    @about-function{screen-primary-monitor}
    @about-function{screen-monitor-geometry}
    @about-function{screen-monitor-workarea}
    @about-function{screen-monitor-at-point}
    @about-function{screen-monitor-at-window}
    @about-function{screen-monitor-height-mm}
    @about-function{screen-monitor-width-mm}
    @about-function{screen-monitor-plug-name}
    @about-function{screen-monitor-scale-factor}
    @about-function{screen-setting}
    @about-function{screen-active-window}
    @about-function{screen-window-stack}
  @end{section}
  @begin[GdkSeat]{section}
    Object representing an user seat.
    @about-symbol{seat-capabilities}
    @about-class{seat}
    @about-generic{seat-display}
    @about-symbol{seat-grab-prepare-func}
    @about-function{seat-grab}
    @about-function{seat-ungrab}
    @about-function{seat-capabilities}
    @about-function{seat-pointer}
    @about-function{seat-keyboard}
    @about-function{seat-slaves}
  @end{section}
  @begin[GdkMonitor]{section}
    Object representing an output.
    @about-symbol{subpixel-layout}
    @about-class{monitor}
    @about-generic{monitor-display}
    @about-generic{monitor-geometry}
    @about-generic{monitor-height-mm}
    @about-generic{monitor-manufacturer}
    @about-generic{monitor-model}
    @about-generic{monitor-refresh-rate}
    @about-generic{monitor-scale-factor}
    @about-generic{monitor-subpixel-layout}
    @about-generic{monitor-width-mm}
    @about-generic{monitor-workarea}
    @about-function{monitor-is-primary}
  @end{section}
  @begin[GdkDevice]{section}
    Object representing an input device.
    @about-symbol{input-source}
    @about-symbol{input-mode}
    @about-symbol{axis-use}
    @about-symbol{axis-flags}
    @about-symbol{device-tool-type}
    @about-symbol{device-type}
    @about-symbol{grab-ownership}
    @about-symbol{time-coord}
    @about-symbol{grab-status}
    @about-class{device-tool}
    @about-generic{device-tool-axes}
    @about-generic{device-tool-hardware-id}
    @about-generic{device-tool-serial}
    @about-generic{device-tool-tool-type}
    @about-class{device}
    @about-generic{device-associated-device}
    @about-generic{device-axes}
    @about-generic{device-device-manager}
    @about-generic{device-display}
    @about-generic{device-has-cursor}
    @about-generic{device-input-mode}
    @about-generic{device-input-source}
    @about-generic{device-n-axes}
    @about-generic{device-name}
    @about-generic{device-num-touches}
    @about-generic{device-product-id}
    @about-generic{device-seat}
    @about-generic{device-tool}
    @about-generic{device-type}
    @about-generic{device-vendor-id}
    @about-function{device-key}
    @about-function{device-axis-use}
    @about-function{device-list-slave-devices}
    @about-function{device-n-keys}
    @about-function{device-warp}
    @about-function{device-grab}
    @about-function{device-ungrab}
    @about-function{device-state}
    @about-function{device-position}
    @about-function{device-position-double}
    @about-function{device-window-at-position}
    @about-function{device-window-at-position-double}
    @about-function{device-history}
    @about-function{device-free-history}
    @about-function{device-axis}
    @about-function{device-list-axes}
    @about-function{device-axis-value}
    @about-function{device-last-event-window}
    @about-function{device-tool-get-serial}
    @about-function{device-tool-get-tool-type}
    @about-function{device-tool-get-hardware-id}
  @end{section}
  @begin[GdkDevicePad]{section}
    Pad device interface.
    @about-symbol{device-pad-feature}
    @about-class{device-pad}
    @about-function{device-pad-n-groups}
    @about-function{device-pad-group-n-modes}
    @about-function{device-pad-n-features}
    @about-function{device-pad-feature-group}
  @end{section}
  @begin[Rectangles]{section}
    GDK provides the @class{gdk:rectangle} data type for representing sets of
    pixels on the screen. Together with Cairo's @symbol{cairo:region-t} data
    type, they make up the central types for representing graphical data.
    @about-struct{rectangle}
    @about-function{rectangle-x}
    @about-function{rectangle-y}
    @about-function{rectangle-width}
    @about-function{rectangle-height}
    @about-function{rectangle-new}
    @about-function{rectangle-copy}
    @about-function{rectangle-intersect}
    @about-function{rectangle-union}
    @about-function{rectangle-equal}
  @end{section}
  @begin[Pixbufs]{section}
    Functions for obtaining pixbufs.

    Pixbufs are client-side images. For details on how to create and manipulate
    pixbufs, see the @class{gdk-pixbuf:pixbuf} API documentation. The functions
    described here allow to obtain pixbufs from @class{gdk:window} objects and
    Cairo surfaces.
    @about-function{pixbuf-from-window}
    @about-function{pixbuf-from-surface}
  @end{section}
  @begin[RGBA Colors]{section}
    The @struct{gdk:rgba} structure is a convenient way to pass RGBA colors
    around. It is based on Cairo's way to deal with colors and mirrors its
    behavior. All values are in the range from 0.0 to 1.0 inclusive. So the
    color
    @begin{pre}
(gdk:rgba-new :red 0.0 :green 0.0 :blue 0.0 :alpha 0.0)
    @end{pre}
    represents transparent black and
    @begin{pre}
(gdk:rgba-new :red 1.0 :green 1.0 :blue 1.0 :alpha 1.0)
    @end{pre}
    is opaque white. Other values will be clamped to this range when drawing.
    @about-struct{rgba}
    @about-function{rgba-red}
    @about-function{rgba-green}
    @about-function{rgba-blue}
    @about-function{rgba-alpha}
    @about-function{rgba-new}
    @about-function{rgba-copy}
    @about-function{rgba-parse}
    @about-function{rgba-equal}
    @about-function{rgba-hash}
    @about-function{rgba-to-string}
  @end{section}
  @begin[Visuals]{section}
    Low-level display hardware information
    @about-symbol{visual-type}
    @about-symbol{byte-order}
    @about-class{visual}
    @about-function{query-depths}
    @about-function{query-visual-types}
    @about-function{list-visuals}
    @about-function{visual-bits-per-rgb}
    @about-function{visual-blue-pixel-details}
    @about-function{visual-green-pixel-details}
    @about-function{visual-red-pixel-details}
    @about-function{visual-byte-order}
    @about-function{visual-colormap-size}
    @about-function{visual-depth}
    @about-function{visual-visual-type}
    @about-function{visual-best-depth}
    @about-function{visual-best-type}
    @about-function{visual-system}
    @about-function{visual-best}
    @about-function{visual-best-with-depth}
    @about-function{visual-best-with-type}
    @about-function{visual-best-with-both}
    @about-function{visual-screen}
  @end{section}
  @begin[Cursors]{section}
    Standard and pixmap cursors.
    @about-symbol{cursor-type}
    @about-class{cursor}
    @about-generic{cursor-cursor-type}
    @about-generic{cursor-display}
    @about-function{cursor-new}
    @about-function{cursor-new-from-pixbuf}
    @about-function{cursor-new-from-surface}
    @about-function{cursor-new-from-name}
    @about-function{cursor-new-for-display}
    @about-function{cursor-image}
    @about-function{cursor-surface}
  @end{section}
  @begin[Windows]{section}
    Onscreen display areas in the target window system.
    @about-symbol{window-type}
    @about-symbol{window-window-class}
    @about-symbol{window-hints}
    @about-symbol{gravity}
    @about-symbol{geometry}
    @about-symbol{anchor-hints}
    @about-symbol{window-edge}
    @about-symbol{window-type-hint}
    @about-symbol{window-attr}
    @about-symbol{window-attributes-type}
    @about-symbol{fullscreen-mode}
    @about-symbol{filter-return}
    @about-symbol{modifier-intent}
    @about-symbol{wm-decoration}
    @about-symbol{wm-function}
    @about-class{window}
    @about-generic{window-cursor}
    @about-function{window-new}
    @about-function{window-destroy}
    @about-function{window-window-type}
    @about-function{window-display}
    @about-function{window-screen}
    @about-function{window-visual}
    @about-function{window-at-pointer}
    @about-function{window-show}
    @about-function{window-show-unraised}
    @about-function{window-hide}
    @about-function{window-is-destroyed}
    @about-function{window-is-visible}
    @about-function{window-is-viewable}
    @about-function{window-is-input-only}
    @about-function{window-is-shaped}
    @about-function{window-state}
    @about-function{window-withdraw}
    @about-function{window-iconify}
    @about-function{window-deiconify}
    @about-function{window-stick}
    @about-function{window-unstick}
    @about-function{window-maximize}
    @about-function{window-unmaximize}
    @about-function{window-fullscreen}
    @about-function{window-fullscreen-on-monitor}
    @about-function{window-unfullscreen}
    @about-function{window-fullscreen-mode}
    @about-function{window-set-keep-above}
    @about-function{window-set-keep-below}
    @about-function{window-set-opacity}
    @about-function{window-composited}
    @about-function{window-pass-through}
    @about-function{window-move}
    @about-function{window-resize}
    @about-function{window-move-resize}
    @about-function{window-scroll}
    @about-function{window-move-to-rect}
    @about-function{window-move-region}
    @about-function{window-flush}
    @about-function{window-has-native}
    @about-function{window-ensure-native}
    @about-function{window-reparent}
    @about-function{window-raise}
    @about-function{window-lower}
    @about-function{window-restack}
    @about-function{window-focus}
    @about-function{window-register-dnd}
    @about-function{window-begin-resize-drag}
    @about-function{window-begin-resize-drag-for-device}
    @about-function{window-begin-move-drag}
    @about-function{window-begin-move-drag-for-device}
    @about-function{window-show-window-menu}
    @about-function{window-constrain-size}
    @about-function{window-beep}
    @about-function{window-scale-factor}
    @about-function{window-set-opaque-region}
    @about-function{window-create-gl-context}
    @about-function{window-mark-paint-from-clip}
    @about-function{window-clip-region}
    @about-function{window-begin-paint-rect}
    @about-function{window-begin-paint-region}
    @about-function{window-end-paint}
    @about-function{window-begin-draw-frame}
    @about-function{window-end-draw-frame}
    @about-function{window-visible-region}
    @about-function{GdkWindowInvalidateHandlerFunc}
    @about-function{window-set-invalidate-handler}
    @about-function{window-invalidate-rect}
    @about-function{window-invalidate-region}
    @about-symbol{window-child-func}
    @about-function{window-invalidate-maybe-recurse}
    @about-function{window-update-area}
    @about-function{window-freeze-updates}
    @about-function{window-thaw-updates}
    @about-function{window-process-all-updates}
    @about-function{window-process-updates}
    @about-function{window-set-debug-updates}
    @about-function{window-enable-synchronized-configure}
    @about-function{window-configure-finished}
    @about-function{window-frame-clock}
    @about-function{window-user-data}
    @about-function{window-set-override-redirect}
    @about-function{window-accept-focus}
    @about-function{window-focus-on-map}
    @about-function{window-add-filter}
    @about-function{window-remove-filter}
    @about-function{window-shape-combine-region}
    @about-function{window-set-child-shapes}
    @about-function{window-merge-child-shapes}
    @about-function{window-input-shape-combine-region}
    @about-function{window-set-child-input-shapes}
    @about-function{window-merge-child-input-shapes}
    @about-function{window-set-static-gravities}
    @about-function{window-set-title}
    @about-function{window-set-background}
    @about-function{window-set-background-rgba}
    @about-function{window-background-pattern}
    @about-function{window-geometry}
    @about-function{window-set-geometry-hints}
    @about-function{window-width}
    @about-function{window-height}
    @about-function{window-set-icon-list}
    @about-function{window-modal-hint}
    @about-function{window-type-hint}
    @about-function{window-set-shadow-width}
    @about-function{window-set-skip-taskbar-hint}
    @about-function{window-set-skip-pager-hint}
    @about-function{window-set-urgency-hint}
    @about-function{window-position}
    @about-function{window-root-origin}
    @about-function{window-frame-extents}
    @about-function{window-origin}
    @about-function{window-root-coords}
    @about-function{window-pointer}
    @about-function{window-device-position}
    @about-function{window-device-position-double}
    @about-function{window-parent}
    @about-function{window-toplevel}
    @about-function{window-children}
    @about-function{window-children-with-user-data}
    @about-function{window-peek-children}
    @about-function{window-events}
    @about-function{window-set-icon-name}
    @about-function{window-set-transient-for}
    @about-function{window-set-role}
    @about-function{window-set-startup-id}
    @about-function{window-group}
    @about-function{window-decorations}
    @about-function{window-set-functions}
    @about-function{default-root-window}
    @about-function{window-support-multidevice}
    @about-function{window-device-cursor}
    @about-function{window-device-events}
    @about-function{window-source-events}
    @about-function{window-event-compression}
    @about-function{offscreen-window-surface}
    @about-function{offscreen-window-embedder}
    @about-function{window-geometry-changed}
    @about-function{window-coords-from-parent}
    @about-function{window-coords-to-parent}
    @about-function{window-effective-parent}
    @about-function{window-effective-toplevel}
  @end{section}
  @begin[Frame Clock]{section}
    A @sym{gdk:frame-clock} object tells the application when to update and
    repaint a window.
    @about-symbol{frame-clock-phase}
    @about-class{frame-clock}
    @about-function{frame-clock-frame-time}
    @about-function{frame-clock-request-phase}
    @about-function{frame-clock-begin-updating}
    @about-function{frame-clock-end-updating}
    @about-function{frame-clock-frame-counter}
    @about-function{frame-clock-history-start}
    @about-function{frame-clock-timings}
    @about-function{frame-clock-current-timings}
    @about-function{frame-clock-refresh-info}
  @end{section}
  @begin[Frame timings]{section}
    Object holding timing information for a single frame.
    @about-class{frame-timings}
    @about-function{frame-timings-ref}
    @about-function{frame-timings-unref}
    @about-function{frame-timings-frame-counter}
    @about-function{frame-timings-complete}
    @about-function{frame-timings-frame-time}
    @about-function{frame-timings-presentation-time}
    @about-function{frame-timings-refresh-interval}
    @about-function{frame-timings-predicted-presentation-time}
  @end{section}
  @begin[GdkDrawingContext]{section}
    Drawing context for GDK windows.
    @about-class{drawing-context}
    @about-generic{drawing-context-clip}
    @about-generic{drawing-context-window}
    @about-function{drawing-context-cairo-context}
    @about-function{drawing-context-is-valid}
  @end{section}
  @begin[OpenGL context]{section}
    The @class{gdk:gl-context} object is representing the platform-specific
    OpenGL drawing context.
    @about-symbol{gl-error}
    @about-class{gl-context}
    @about-generic{gl-context-display}
    @about-generic{gl-context-shared-context}
    @about-generic{gl-context-window}
    @about-function{gl-context-get-version}
    @about-function{gl-context-set-required-version}
    @about-function{gl-context-get-required-version}
    @about-function{gl-context-set-debug-enabled}
    @about-function{gl-context-get-debug-enabled}
    @about-function{gl-context-set-forward-compatible}
    @about-function{gl-context-get-forward-compatible}
    @about-function{gl-context-set-use-es}
    @about-function{gl-context-get-use-es}
    @about-function{gl-context-is-legacy}
    @about-function{gl-context-realize}
    @about-function{gl-context-make-current}
    @about-function{gl-context-get-current}
    @about-function{gl-context-clear-current}
  @end{section}
  @begin[Events]{section}
    Functions for handling events from the window system. In GTK applications
    the events are handled automatically in the @fun{gtk:main-do-event} function
    and passed on to the appropriate widgets, so these functions are rarely
    needed. Though some of the fields in the event structures are useful.
    @about-variable{+gdk-current-time+}
    @about-variable{+gdk-priority-events+}
    @about-variable{+gdk-priority-redraw+}
    @about-variable{+gdk-event-propagate+}
    @about-variable{+gdk-event-stop+}
    @about-variable{+gdk-button-primary+}
    @about-variable{+gdk-button-middle+}
    @about-variable{+gdk-button-secondary+}
    @about-symbol{event-type}
    @about-symbol{event-mask}
    @about-symbol{touchpad-gesture-phase}
    @about-class{event-sequence}
    @about-function{events-pending}
    @about-function{event-peek}
    @about-function{event-get}
    @about-function{event-put}
    @about-function{event-new}
    @about-function{event-copy}
    @about-function{event-free}
    @about-function{event-axis}
    @about-function{event-button}
    @about-function{event-click-count}
    @about-function{event-coords}
    @about-function{event-keycode}
    @about-function{event-keyval}
    @about-function{event-root-coords}
    @about-function{event-get-scroll-direction}
    @about-function{event-scroll-deltas}
    @about-function{event-is-scroll-stop-event}
    @about-function{event-state}
    @about-function{event-time}
    @about-function{event-get-window}
    @about-function{event-get-event-type}
    @about-function{event-event-sequence}
    @about-function{event-request-motions}
    @about-function{events-angle}
    @about-function{events-center}
    @about-function{events-distance}
    @about-function{event-triggers-context-menu}
    @about-function{event-seat}
    @about-function{event-scancode}
    @about-function{event-pointer-emulated}
    @about-symbol{event-func}
    @about-function{event-handler-set}
    @about-function{show-events}
    @about-function{event-screen}
    @about-function{event-device}
    @about-function{event-source-device}
    @about-function{event-device-tool}
    @about-function{setting-get}
  @end{section}
  @begin[Event Structures]{section}
    The event structures contain data specific to each type of event in GDK.

    @subheading{Note}
    A common mistake is to forget to set the event mask of a widget so that
    the required events are received. See the @fun{gtk-widget-events} function.
    @about-symbol{scroll-direction}
    @about-symbol{visibility-state}
    @about-symbol{crossing-mode}
    @about-symbol{notify-type}
    @about-symbol{property-state}
    @about-symbol{window-state}
    @about-symbol{setting-action}
    @about-symbol{owner-change}
    @about-symbol{event-type}
    @about-symbol{modifier-type}
    @about-struct{event}
    @about-function{copy-event}
    @about-function{make-event}
    @about-function{event-type}
    @about-function{event-window}
    @about-function{event-send-event}
    @about-struct{event-any}
    @about-struct{event-key}
    @about-function{copy-event-key}
    @about-function{make-event-key}
    @about-function{event-key-type}
    @about-function{event-key-window}
    @about-function{event-key-send-event}
    @about-function{event-key-time}
    @about-function{event-key-state}
    @about-function{event-key-keyval}
    @about-function{event-key-length}
    @about-function{event-key-string}
    @about-function{event-key-hardware-keycode}
    @about-function{event-key-group}
    @about-function{event-key-is-modifier}
    @about-struct{event-button}
    @about-function{copy-event-button}
    @about-function{make-event-button}
    @about-function{event-button-type}
    @about-function{event-button-window}
    @about-function{event-button-send-event}
    @about-function{event-button-time}
    @about-function{event-button-x}
    @about-function{event-button-y}
    @about-function{event-button-axes}
    @about-function{event-button-state}
    @about-function{event-button-button}
    @about-function{event-button-device}
    @about-function{event-button-x-root}
    @about-function{event-button-y-root}
    @about-struct{event-touch}
    @about-function{copy-event-touch}
    @about-function{make-event-touch}
    @about-function{event-touch-type}
    @about-function{event-touch-window}
    @about-function{event-touch-send-event}
    @about-function{event-touch-time}
    @about-function{event-touch-x}
    @about-function{event-touch-y}
    @about-function{event-touch-axes}
    @about-function{event-touch-state}
    @about-function{event-touch-sequence}
    @about-function{event-touch-emulating-pointer}
    @about-function{event-touch-device}
    @about-function{event-touch-x-root}
    @about-function{event-touch-y-root}
    @about-struct{event-scroll}
    @about-function{copy-event-scroll}
    @about-function{make-event-scroll}
    @about-function{event-scroll-type}
    @about-function{event-scroll-window}
    @about-function{event-scroll-send-event}
    @about-function{event-scroll-time}
    @about-function{event-scroll-x}
    @about-function{event-scroll-y}
    @about-function{event-scroll-state}
    @about-function{event-scroll-direction}
    @about-function{event-scroll-device}
    @about-function{event-scroll-x-root}
    @about-function{event-scroll-y-root}
    @about-function{event-scroll-delta-x}
    @about-function{event-scroll-delta-y}
    @about-struct{event-motion}
    @about-function{copy-event-motion}
    @about-function{make-event-motion}
    @about-function{event-motion-type}
    @about-function{event-motion-window}
    @about-function{event-motion-send-event}
    @about-function{event-motion-time}
    @about-function{event-motion-x}
    @about-function{event-motion-y}
    @about-function{event-motion-axes}
    @about-function{event-motion-state}
    @about-function{event-motion-is-hint}
    @about-function{event-motion-device}
    @about-function{event-motion-x-root}
    @about-function{event-motion-y-root}
    @about-struct{event-expose}
    @about-function{copy-event-expose}
    @about-function{make-event-expose}
    @about-function{event-expose-type}
    @about-function{event-expose-window}
    @about-function{event-expose-send-event}
    @about-function{event-expose-area}
    @about-function{event-expose-region}
    @about-function{event-expose-count}
    @about-struct{event-visibility}
    @about-function{copy-event-visibility}
    @about-function{make-event-visibility}
    @about-function{event-visibility-type}
    @about-function{event-visibility-window}
    @about-function{event-visibility-send-event}
    @about-function{event-visibility-state}
    @about-struct{event-crossing}
    @about-function{copy-event-crossing}
    @about-function{make-event-crossing}
    @about-function{event-crossing-type}
    @about-function{event-crossing-window}
    @about-function{event-crossing-send-event}
    @about-function{event-crossing-subwindow}
    @about-function{event-crossing-time}
    @about-function{event-crossing-x}
    @about-function{event-crossing-y}
    @about-function{event-crossing-x-root}
    @about-function{event-crossing-y-root}
    @about-function{event-crossing-mode}
    @about-function{event-crossing-detail}
    @about-function{event-crossing-focus}
    @about-function{event-crossing-state}
    @about-struct{event-focus}
    @about-function{copy-event-focus}
    @about-function{make-event-focus}
    @about-function{event-focus-type}
    @about-function{event-focus-window}
    @about-function{event-focus-send-event}
    @about-function{event-focus-in}
    @about-struct{event-configure}
    @about-function{copy-event-configure}
    @about-function{make-event-configure}
    @about-function{event-configure-type}
    @about-function{event-configure-window}
    @about-function{event-configure-send-event}
    @about-function{event-configure-x}
    @about-function{event-configure-y}
    @about-function{event-configure-width}
    @about-function{event-configure-height}
    @about-struct{event-property}
    @about-function{copy-event-property}
    @about-function{make-event-property}
    @about-function{event-property-type}
    @about-function{event-property-window}
    @about-function{event-property-send-event}
    @about-function{event-property-atom}
    @about-function{event-property-time}
    @about-function{event-property-state}
    @about-struct{event-selection}
    @about-function{copy-event-selection}
    @about-function{make-event-selection}
    @about-function{event-selection-type}
    @about-function{event-selection-window}
    @about-function{event-selection-send-event}
    @about-function{event-selection-selection}
    @about-function{event-selection-target}
    @about-function{event-selection-property}
    @about-function{event-selection-time}
    @about-function{event-selection-requestor}
    @about-struct{event-dnd}
    @about-function{copy-event-dnd}
    @about-function{make-event-dnd}
    @about-function{event-dnd-type}
    @about-function{event-dnd-window}
    @about-function{event-dnd-send-event}
    @about-function{event-dnd-context}
    @about-function{event-dnd-time}
    @about-function{event-dnd-x-root}
    @about-function{event-dnd-y-root}
    @about-struct{event-proximity}
    @about-function{copy-event-proximity}
    @about-function{make-event-proximity}
    @about-function{event-proximity-type}
    @about-function{event-proximity-window}
    @about-function{event-proximity-send-event}
    @about-function{event-proximity-time}
    @about-function{event-proximity-device}
    @about-struct{event-window-state}
    @about-function{copy-event-window-state}
    @about-function{make-event-window-state}
    @about-function{event-window-state-type}
    @about-function{event-window-state-window}
    @about-function{event-window-state-send-event}
    @about-function{event-window-state-changed-mask}
    @about-function{event-window-state-new-window-state}
    @about-struct{event-setting}
    @about-function{copy-event-setting}
    @about-function{make-event-setting}
    @about-function{event-setting-type}
    @about-function{event-setting-window}
    @about-function{event-setting-send-event}
    @about-function{event-setting-action}
    @about-function{event-setting-name}
    @about-struct{event-owner-change}
    @about-function{copy-event-owner-change}
    @about-function{make-event-owner-change}
    @about-function{event-owner-change-type}
    @about-function{event-owner-change-window}
    @about-function{event-owner-change-send-event}
    @about-function{event-owner-change-owner}
    @about-function{event-owner-change-reason}
    @about-function{event-owner-change-selection}
    @about-function{event-owner-change-time}
    @about-function{event-owner-change-selection-time}
    @about-struct{event-grab-broken}
    @about-function{copy-event-grab-broken}
    @about-function{make-event-grab-broken}
    @about-function{event-grab-broken-type}
    @about-function{event-grab-broken-window}
    @about-function{event-grab-broken-send-event}
    @about-function{event-grab-broken-keyboard}
    @about-function{event-grab-broken-implicit}
    @about-function{event-grab-broken-grab-window}
    @about-struct{event-touchpad-swipe}
    @about-function{copy-event-touchpad-swipe}
    @about-function{make-event-touchpad-swipe}
    @about-function{event-touchpad-swipe-type}
    @about-function{event-touchpad-swipe-window}
    @about-function{event-touchpad-swipe-send-event}
    @about-function{event-touchpad-swipe-phase}
    @about-function{event-touchpad-swipe-n-fingers}
    @about-function{event-touchpad-swipe-time}
    @about-function{event-touchpad-swipe-x}
    @about-function{event-touchpad-swipe-y}
    @about-function{event-touchpad-swipe-dx}
    @about-function{event-touchpad-swipe-dy}
    @about-function{event-touchpad-swipe-x-root}
    @about-function{event-touchpad-swipe-y-root}
    @about-function{event-touchpad-swipe-state}
    @about-struct{event-touchpad-pinch}
    @about-function{copy-event-touchpad-pinch}
    @about-function{make-event-touchpad-pinch}
    @about-function{event-touchpad-pinch-type}
    @about-function{event-touchpad-pinch-window}
    @about-function{event-touchpad-pinch-send-event}
    @about-function{event-touchpad-pinch-phase}
    @about-function{event-touchpad-pinch-n-fingers}
    @about-function{event-touchpad-pinch-time}
    @about-function{event-touchpad-pinch-x}
    @about-function{event-touchpad-pinch-y}
    @about-function{event-touchpad-pinch-dx}
    @about-function{event-touchpad-pinch-dy}
    @about-function{event-touchpad-pinch-angle-delta}
    @about-function{event-touchpad-pinch-scale}
    @about-function{event-touchpad-pinch-x-root}
    @about-function{event-touchpad-pinch-y-root}
    @about-function{event-touchpad-pinch-state}
    @about-struct{event-pad-button}
    @about-function{copy-event-pad-button}
    @about-function{make-event-pad-button}
    @about-function{event-pad-button-type}
    @about-function{event-pad-button-window}
    @about-function{event-pad-button-send-event}
    @about-function{event-pad-button-time}
    @about-function{event-pad-button-group}
    @about-function{event-pad-button-button}
    @about-function{event-pad-button-mode}
    @about-struct{event-pad-axis}
    @about-function{copy-event-pad-axis}
    @about-function{make-event-pad-axis}
    @about-function{event-pad-axis-type}
    @about-function{event-pad-axis-window}
    @about-function{event-pad-axis-send-event}
    @about-function{event-pad-axis-time}
    @about-function{event-pad-axis-group}
    @about-function{event-pad-axis-index}
    @about-function{event-pad-axis-mode}
    @about-function{event-pad-axis-value}
    @about-struct{event-pad-group-mode}
    @about-function{copy-event-pad-group-mode}
    @about-function{make-event-pad-group-mode}
    @about-function{event-pad-group-mode-type}
    @about-function{event-pad-group-mode-window}
    @about-function{event-pad-group-mode-send-event}
    @about-function{event-pad-group-mode-time}
    @about-function{event-pad-group-mode-group}
    @about-function{event-pad-group-mode-mode}
  @end{section}
  @begin[Key Values]{section}
    Functions for manipulating keyboard codes.
    @about-class{keymap}
    @about-function{keymap-default}
    @about-function{keymap-for-display}
    @about-function{keymap-lookup-key}
    @about-function{keymap-translate-keyboard-state}
    @about-function{keymap-entries-for-keyval}
    @about-function{keymap-entries-for-keycode}
    @about-function{keymap-direction}
    @about-function{keymap-have-bidi-layouts}
    @about-function{keymap-caps-lock-state}
    @about-function{keymap-num-lock-state}
    @about-function{keymap-scroll-lock-state}
    @about-function{keymap-modifier-state}
    @about-function{keymap-add-virtual-modifiers}
    @about-function{keymap-map-virtual-modifiers}
    @about-function{keymap-modifier-mask}
    @about-function{keyval-name}
    @about-function{keyval-from-name}
    @about-function{keyval-convert-case}
    @about-function{keyval-to-upper}
    @about-function{keyval-to-lower}
    @about-function{keyval-is-upper}
    @about-function{keyval-is-lower}
    @about-function{keyval-to-unicode}
    @about-function{unicode-to-keyval}
  @end{section}
  @begin[Selections]{section}
    Functions for transfering data via the X selection mechanism.

    The X selection mechanism provides a way to transfer arbitrary chunks of
    data between programs. A selection is essentially a named clipboard,
    identified by a string interned as an atom of type @symbol{gdk:atom}. By
    claiming ownership of a selection, an application indicates that it will
    be responsible for supplying its contents. The most common selections are
    \"PRIMARY\" and \"CLIPBOARD\".

    The contents of a selection can be represented in a number of formats,
    called targets. Each target is identified by an atom. A list of all possible
    targets supported by the selection owner can be retrieved by requesting the
    special target \"TARGETS\". When a selection is retrieved, the data is
    accompanied by a type (an atom), and a format (an integer), representing
    the number of bits per item.

    The functions in this section only contain the lowlevel parts of the
    selection protocol. A considerably more complicated implementation is needed
    on top of this. GTK contains such an implementation and programmers should
    use those functions instead of the ones presented here. If you plan to
    implement selection handling directly on top of the functions here, you
    should refer to the X Inter-Client Communication Conventions Manual (ICCCM).

    The C library has the following constants to refer to selections, targets,
    and selection types. In the Lisp library the corresponding strings are used.
    These strings are automatically converted to the corresponding
    @symbol{gdk:atom} type.
    @begin[code]{table}
      @entry[GDK_SELECTION_PRIMARY]{\"PRIMARY\"}
      @entry[GDK_SELECTION_SECONDARY]{\"SECONDARY\"}
      @entry[GDK_SELECTION_CLIPBOARD]{\"CLIPBOARD\"}
    @end{table}
    @begin[code]{table}
      @entry[GDK_TARGET_BITMAP]{\"BITMAP\"}
      @entry[GDK_TARGET_COLORMAP]{\COLORMAP\"}
      @entry[GDK_TARGET_DRAWABLE]{\"DRAWABLE\"}
      @entry[GDK_TARGET_PIXMAP]{\"PIXMAP\"}
      @entry[GDK_TARGET_STRING]{\"STRING\"}
    @end{table}
    @begin[code]{table}
      @entry[GDK_SELECTION_TYPE_ATOM]{\"ATOM\"}
      @entry[GDK_SELECTION_TYPE_BITMAP]{\"BITMAP\"}
      @entry[GDK_SELECTION_TYPE_COLORMAP]{\"COLORMAP\"}
      @entry[GDK_SELECTION_TYPE_DRAWABLE]{\"DRAWABLE\"}
      @entry[GDK_SELECTION_TYPE_INTEGER]{\"INTEGER\"}
      @entry[GDK_SELECTION_TYPE_PIXMAP]{\"PIXMAP\"}
      @entry[GDK_SELECTION_TYPE_WINDOW]{\"WINDOW\"}
      @entry[GDK_SELECTION_TYPE_STRING]{\"STRING\"}
    @end{table}
    @about-function{selection-owner-set}
    @about-function{selection-owner-set-for-display}
    @about-function{selection-owner-get}
    @about-function{selection-owner-get-for-display}
    @about-function{selection-convert}
    @about-function{selection-property-get}
    @about-function{selection-send-notify}
    @about-function{selection-send-notify-for-display}
  @end{section}
  @begin[Drag and Drop]{section}
    Functions for controlling drag and drop handling.
    @about-symbol{drag-cancel-reason}
    @about-symbol{drag-protocol}
    @about-symbol{drag-action}
    @about-class{drag-context}
    @about-function{drag-selection}
    @about-function{drag-abort}
    @about-function{drop-reply}
    @about-function{drag-drop}
    @about-function{drag-drop-done}
    @about-function{drag-find-window-for-screen}
    @about-function{drag-begin}
    @about-function{drag-begin-for-device}
    @about-function{drag-begin-from-point}
    @about-function{drag-motion}
    @about-function{drop-finish}
    @about-function{drag-status}
    @about-function{drag-drop-succeeded}
    @about-function{window-drag-protocol}
    @about-function{drag-context-actions}
    @about-function{drag-context-suggested-action}
    @about-function{drag-context-selected-action}
    @about-function{drag-context-list-targets}
    @about-function{drag-context-device}
    @about-function{drag-context-source-window}
    @about-function{drag-context-dest-window}
    @about-function{drag-context-protocol}
    @about-function{drag-context-drag-window}
    @about-function{drag-context-set-hotspot}
    @about-function{drag-context-manage-dnd}
  @end{section}
  @begin[Properties and Atoms]{section}
    An atom is a numeric index into a string table on the X server. They are
    used to transfer strings efficiently between clients without having to
    transfer the entire string.
    @about-type{atom-as-string}
  @end{section}
  @begin[Threads]{section}
    Functions for using GDK in multi-threaded programs.

    For thread safety, GDK relies on the thread primitives in GLib, and on the
    thread-safe GLib main loop.

    GLib is completely thread safe (all global data is automatically locked),
    but individual data structure instances are not automatically locked for
    performance reasons. So e.g. you must coordinate accesses to the same
    @code{GHashTable} from multiple threads.

    GTK, however, is not thread safe. You should only use GTK and GDK from
    the thread @code{gtk_init()} and @code{gtk_main()} were called on. This is
    usually referred to as the \"main thread\".

    Signals on GTK and GDK types, as well as non-signal callbacks, are emitted
    in the main thread.

    You can schedule work in the main thread safely from other threads by using
    the @fun{gdk:threads-add-idle} and @fun{gdk:threads-add-timeout} functions.
    @begin{pre}
static void
worker_thread (void)
{
  ExpensiveData *expensive_data = do_expensive_computation ();

  gdk_threads_add_idle (got_value, expensive_data);
@}

static gboolean
got_value (gpointer user_data)
{
  ExpensiveData *expensive_data = user_data;

  my_app->expensive_data = expensive_data;
  gtk_button_set_sensitive (my_app->button, TRUE);
  gtk_button_set_label (my_app->button, expensive_data->result_label);

  return G_SOURCE_REMOVE;
@}
    @end{pre}
    You should use the @fun{gdk:threads-add-idle} and
    @fun{gdk:threads-add-timeout} functions instead of the @fun{g:idle-add} and
    @fun{g:timeout-add} functions since libraries not under your control might
    be using the deprecated GDK locking mechanism. If you are sure that none of
    the code in your application and libraries use the deprecated
    @code{gdk_threads_enter()} or @code{gdk_threads_leave()} methods, then you
    can safely use the @fun{g:idle-add} and @fun{g:timeout-add} functions.

    For more information on this \"worker thread\" pattern, you should also
    look at @code{GTask}, which gives you high-level tools to perform expensive
    tasks from worker threads, and will handle thread management for you.
    @about-function{threads-init}
    @about-function{threads-enter}
    @about-function{threads-leave}
    @about-function{threads-set-lock-functions}
    @about-function{threads-add-idle}
    @about-function{threads-add-idle-full}
    @about-function{threads-add-timeout}
    @about-function{threads-add-timeout-full}
    @about-function{threads-add-timeout-seconds}
    @about-function{threads-add-timeout-seconds-full}
  @end{section}
  @begin[Pango Interaction]{section}
    Pango is the text layout system used by GDK and GTK. The functions and
    types in this section are used to obtain clip regions for
    @class{pango-layout} objects, and to get @class{pango-context} objects that
    can be used with GDK.

    Creating a @class{pango-layout} object is the first step in rendering text,
    and requires getting a handle to a @class{pango-context}. For GTK programs,
    you will usually want to use the @fun{gtk-widget-pango-context},
    or @fun{gtk-widget-create-pango-layout} functions, rather than using the
    lowlevel @fun{gdk:pango-context-for-screen} function. Once you have a
    @class{pango-layout} object, you can set the text and attributes of it with
    Pango functions like the @fun{pango-layout-text} function and get its size
    with the @fun{pango-layout-size} function. Note that Pango uses a fixed
    point system internally, so converting between Pango units and pixels using
    the @var{+pango-scale+} constant or the @fun{pango-pixels} function.

    Rendering a Pango layout is done most simply with the
    @fun{pango-cairo-show-layout} function. You can also draw pieces of the
    layout with the @fun{pango-cairo-show-layout-line} function.

    @b{Example:} Draw transformed text with Pango and Cairo

    @image[pango-cairo]{}

    @begin{pre}
(defun demo-pango ()
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title \"Demo Using Pango with Cairo\"
                                 :border-width 12
                                 :default-width 400
                                 :default-height 400))
          (circle 100)
          (n-words 12)
          (font \"Sans Bold 16\"))
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g:signal-connect window \"draw\"
         (lambda (widget cr)
           (let* ((cr (pointer cr))
                  ;; Get the GdkWindow for the widget
                  (window (gtk-widget-window widget))
                  (width (gdk:window-width window))
                  (height (gdk:window-height window))
                  (radius (- (/ (min width height) 2) 20)))
             ;; Set up a transformation matrix so that the user space
             ;; coordinates for where we are drawing are [-RADIUS, RADIUS],
             ;; [-RADIUS, RADIUS] We first center, then change the scale
             (cairo-translate cr
                              (+ radius (/ (- width (* 2 radius)) 2))
                              (+ radius (/ (- height (* 2 radius)) 2)))
             (cairo-scale cr (/ radius circle) (/ radius circle))

           ;; Clear surface
           (cairo-set-source-rgb cr 1 1 1)
           (cairo-paint cr)

           ;; Create a PangoLayout, set the font and text
           (let* ((screen (gdk:window-screen window))
                  (context (gdk:pango-context-for-screen screen))
                  (layout (pango-layout-new context))
                  (desc (pango:font-description-from-string font)))
             (setf (pango-layout-text layout) \"Text\")
             (setf (pango-layout-font-description layout) desc)

             ;; Draw the layout n-words times in a circle
             (do* ((i 0 (+ i 1))
                   (angle 0 (/ (* 360 i) n-words))
                   ;; Gradient from red to blue
                   (red (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)
                        (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)))
                  ((>= i n-words))
               (cairo-save cr)
               (cairo-set-source-rgb cr red 0 (- 1 red))
               (cairo-rotate cr (/ (* angle pi) 180))

               ;; Inform Pango to re-layout the text with the new
               ;; transformation matrix
               (pango-cairo-update-layout cr layout)

               (multiple-value-bind (width height)
                   (pango-layout-size layout)
                 (declare (ignore height))
                 (cairo-move-to cr (- (/ width 2 +pango-scale+)) (- circle)))
               (pango-cairo-show-layout cr layout)
               (cairo-restore cr)))
           (cairo-destroy cr)
           t)))
      (gtk-widget-show-all window))))
    @end{pre}
    @about-function{pango-layout-clip-region}
    @about-function{pango-layout-line-clip-region}
    @about-function{pango-context-get}
    @about-function{pango-context-for-screen}
    @about-function{pango-context-for-display}
  @end{section}
  @begin[Cairo Interaction]{section}
    Types and functions to support using Cairo.

    Cairo is a graphics library that supports vector graphics and image
    compositing that can be used with GDK. GTK does all of its drawing using
    Cairo.

    GDK does not wrap the Cairo API, instead it allows to create Cairo contexts
    which can be used to draw on @class{gdk:window} objects. Additional
    functions allow use @class{gdk:rectangle} instances with Cairo and to use
    @class{gdk:color}, @class{gdk:rgba}, @class{gdk-pixbuf:pixbuf} and
    @class{gdk:window} objects as sources for drawing operations.
    @about-class{cairo-surface}
    @about-class{cairo-context}
    @about-function{window-create-similar-surface}
    @about-function{window-create-similar-image-surface}
    @about-function{cairo-create}
    @about-function{cairo-clip-rectangle}
    @about-function{cairo-drawing-context}
    @about-function{cairo-set-source-color}
    @about-function{cairo-set-source-rgba}
    @about-function{cairo-set-source-pixbuf}
    @about-function{cairo-set-source-window}
    @about-function{cairo-rectangle}
    @about-function{cairo-region}
    @about-function{cairo-region-create-from-surface}
    @about-function{cairo-surface-create-from-pixbuf}
    @about-function{cairo-draw-from-gl}
  @end{section}
  @begin[X Window System Interaction]{section}
    X backend-specific functions
  @end{section}
  @begin[Application launching]{section}
    Startup notification for applications.
    @about-class{app-launch-context}
    @about-generic{app-launch-context-display}
    @about-function{app-launch-context-new}
    @about-function{app-launch-context-set-screen}
    @about-function{app-launch-context-set-desktop}
    @about-function{app-launch-context-set-timestamp}
    @about-function{app-launch-context-set-icon}
    @about-function{app-launch-context-set-icon-name}
  @end{section}
  @begin[Deprecated]{section}
    @begin[Colors]{subsection}
      A @class{gdk:color} structure represents a color. When working with Cairo,
      it is often more convenient to use a @class{gdk:rgba} color instead. The
      @class{gdk:color} structure has been deprecated in favor of the
      @class{gdk:rgba} structure.
      @about-struct{color}
      @about-function{color-red}
      @about-function{color-green}
      @about-function{color-blue}
      @about-function{color-pixel}
      @about-function{color-new}
      @about-function{color-copy}
      @about-function{color-parse}
      @about-function{color-equal}
      @about-function{color-hash}
      @about-function{color-to-string}
    @end{subsection}
    @begin[GdkDeviceManager]{subsection}
      Functions for handling input devices.
      @about-class{device-manager}
      @about-generic{device-manager-display}
      @about-function{disable-multidevice}
      @about-function{device-manager-list-devices}
      @about-function{device-manager-client-pointer}
    @end{subsection}
  @end{section}")

;;; --- End of file gdk3.package.lisp ------------------------------------------
