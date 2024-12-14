;;; ----------------------------------------------------------------------------
;;; gtk3.widget.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
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
;;;
;;; GtkWidget
;;;
;;;     Base class for all widgets
;;;
;;; Types and Values
;;;
;;;     GtkWidget
;;;     GtkRequisition
;;;     GtkAllocation
;;;     GtkWidgetHelpType
;;;     GtkTextDirection                                -> gtk.emumerations.lisp
;;;     GtkStateType                                    -> gtk.enumerations.lisp
;;;     GtkSizeRequestMode
;;;     GtkRequestedSize
;;;     GtkAlign
;;;
;;; Accessors
;;;
;;;     gtk_widget_get_app_app_paintable
;;;     gtk_widget_set_app_paintable
;;;     gtk_widget_get_can_default
;;;     gtk_widget_set_can_default
;;;     gtk_widget_get_can_focus
;;;     gtk_widget_set_can_focus
;;;     gtk_widget_get_double_buffered
;;;     gtk_widget_set_double_buffered
;;;     gtk_widget_set_events
;;;     gtk_widget_get_events
;;;     gtk_widget_get_halign
;;;     gtk_widget_set_halign
;;;     gtk_widget_has_default
;;;     gtk_widget_has_focus
;;;     gtk_widget_get_has_tooltip
;;;     gtk_widget_set_has_tooltip
;;;     gtk_widget_get_hexpand
;;;     gtk_widget_set_hexpand
;;;     gtk_widget_get_hexpand_set
;;;     gtk_widget_set_hexpand_set
;;;     gtk_widget_is_focus
;;;     gtk_widget_get_margin_bottom
;;;     gtk_widget_set_margin_bottom
;;;     gtk_widget_get_margin_end
;;;     gtk_widget_set_margin_end
;;;     gtk_widget_get_margin_left
;;;     gtk_widget_set_margin_left
;;;     gtk_widget_get_margin_right
;;;     gtk_widget_set_margin_right
;;;     gtk_widget_get_margin_start
;;;     gtk_widget_set_margin_start
;;;     gtk_widget_get_margin_top
;;;     gtk_widget_set_margin_top
;;;     gtk_widget_set_name
;;;     gtk_widget_get_name
;;;     gtk_widget_get_opacity
;;;     gtk_widget_set_opacity
;;;     gtk_widget_get_sensitive
;;;     gtk_widget_set_sensitive
;;;     gtk_widget_set_style
;;;     gtk_widget_get_style
;;;     gtk_widget_get_tooltip_markup
;;;     gtk_widget_set_tooltip_markup
;;;     gtk_widget_get_tooltip_text
;;;     gtk_widget_set_tooltip_text
;;;     gtk_widget_get_valign
;;;     gtk_widget_set_valign
;;;     gtk_widget_get_vexpand
;;;     gtk_widget_set_vexpand
;;;     gtk_widget_get_vexpand_set
;;;     gtk_widget_set_vexpand_set
;;;     gtk_widget_get_window
;;;     gtk_widget_set_window
;;;
;;; Functions
;;;
;;;     gtk_widget_new
;;;     gtk_widget_destroy
;;;     gtk_widget_in_destruction
;;;     gtk_widget_destroyed                               not implemented
;;;     gtk_widget_unparent
;;;     gtk_widget_show
;;;     gtk_widget_show_now
;;;     gtk_widget_hide
;;;     gtk_widget_show_all
;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_draw
;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_resize_no_redraw
;;;     gtk_widget_get_frame_clock
;;;     gtk_widget_scale_factor
;;;     gtk_widget_add_tick_callback
;;;     gtk_widget_remove_tick_callback
;;;     gtk_widget_size_request                            deprecated
;;;     gtk_widget_get_child_requisition                   deprecated
;;;     gtk_widget_size_allocate
;;;     gtk_widget_size_allocate_with_baseline
;;;     gtk_widget_add_accelerator
;;;     gtk_widget_remove_accelerator
;;;     gtk_widget_set_accel_path
;;;     gtk_widget_list_accel_closures                     not implemented
;;;     gtk_widget_can_activate_accel
;;;     gtk_widget_event
;;;     gtk_widget_activate
;;;     gtk_widget_reparent                                not exported
;;;     gtk_widget_intersect
;;;     gtk_widget_grab_focus
;;;     gtk_widget_grab_default
;;;     gtk_widget_set_state                               not exported
;;;     gtk_widget_set_parent
;;;     gtk_widget_set_parent_window
;;;     gtk_widget_get_parent_window
;;;     gtk_widget_add_events
;;;     gtk_widget_set_device_events
;;;     gtk_widget_get_device_events
;;;     gtk_widget_add_device_events
;;;     gtk_widget_set_device_enabled
;;;     gtk_widget_get_device_enabled
;;;     gtk_widget_get_toplevel
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_get_visual
;;;     gtk_widget_set_visual
;;;     gtk_widget_get_pointer                             not exported
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates
;;;     gtk_widget_hide_on_delete
;;;     gtk_widget_ensure_style                            not exported
;;;     gtk_widget_reset_rc_styles                         not exported
;;;     gtk_widget_get_default_style                       not exported
;;;     gtk_widget_set_direction
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_shape_combine_region
;;;     gtk_widget_input_shape_combine_region
;;;     gtk_widget_path                                    not implemented
;;;     gtk_widget_class_path                              not implemented
;;;     gtk_widget_get_composite_name                      deprecated
;;;     gtk_widget_override_background_color               deprecated
;;;     gtk_widget_override_color                          deprecated
;;;     gtk_widget_override_font                           deprecated
;;;     gtk_widget_override_symbolic_color                 deprecated
;;;     gtk_widget_override_cursor                         deprecated
;;;     gtk_widget_modify_style                            not implemented
;;;     gtk_widget_get_modifier_style                      not implemented
;;;     gtk_widget_modify_fg                               not exported
;;;     gtk_widget_modify_bg                               not exported
;;;     gtk_widget_modify_text                             not exported
;;;     gtk_widget_modify_base                             not exported
;;;     gtk_widget_modify_font                             not exported
;;;     gtk_widget_modify_cursor                           not exported
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_set_font_options
;;;     gtk_widget_get_font_options
;;;     gtk_widget_set_font_map
;;;     gtk_widget_get_font_map
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_render_icon                             not exported
;;;     gtk_widget_render_icon_pixbuf                      not exported
;;;     gtk_widget_pop_composite_child                     not exported
;;;     gtk_widget_push_composite_child                    not exported
;;;     gtk_widget_queue_draw_area
;;;     gtk_widget_queue_draw_region
;;;     gtk_widget_set_redraw_on_allocate
;;;     gtk_widget_set_composite_name                      deprecated
;;;     gtk_widget_mnemonic_activate
;;;     gtk_widget_class_install_style_property
;;;     gtk_widget_class_install_style_property_parser     not implemented
;;;     gtk_widget_class_find_style_property
;;;     gtk_widget_class_list_style_properties
;;;     gtk_widget_region_intersect                        deprecated
;;;     gtk_widget_send_expose                             not implemented
;;;     gtk_widget_send_focus_change                       not implemented
;;;     gtk_widget_style_get                               not implemented
;;;     gtk_widget_style_get_property
;;;     gtk_widget_style_get_valist                        not implemented
;;;     gtk_widget_style_attach                            not implemented
;;;     gtk_widget_class_set_accessible_type               not implemented
;;;     gtk_widget_class_set_accessible_role               not implemented
;;;     gtk_widget_get_accessible
;;;     gtk_widget_child_focus
;;;     gtk_widget_child_notify
;;;     gtk_widget_freeze_child_notify
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_get_parent
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_root_window                         deprecated
;;;     gtk_widget_get_screen
;;;     gtk_widget_has_screen
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_set_size_request
;;;     gtk_widget_thaw_child_notify
;;;     gtk_widget_set_no_show_all
;;;     gtk_widget_get_no_show_all
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_is_composited
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_get_tooltip_window
;;;     gtk_widget_set_tooltip_window
;;;     gtk_widget_trigger_tooltip_query
;;;     gtk_widget_register_window
;;;     gtk_widget_unregister_window
;;;     gtk_cairo_should_draw_window
;;;     gtk_cairo_transform_to_window
;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_set_allocation
;;;     gtk_widget_get_allocated_baseline
;;;     gtk_widget_get_clip
;;;     gtk_widget_set_clip
;;;     gtk_widget_get_has_window
;;;     gtk_widget_set_has_window
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_get_state                               not exported
;;;     gtk_widget_get_visible
;;;     gtk_widget_is_visible
;;;     gtk_widget_set_visible
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_has_grab
;;;     gtk_widget_has_rc_style                            not implemented
;;;     gtk_widget_is_drawable
;;;     gtk_widget_is_toplevel
;;;     gtk_widget_set_receives_default
;;;     gtk_widget_get_receives_default
;;;     gtk_widget_set_support_multidevice
;;;     gtk_widget_get_support_multidevice
;;;     gtk_widget_set_realized
;;;     gtk_widget_get_realized
;;;     gtk_widget_set_mapped
;;;     gtk_widget_get_mapped
;;;     gtk_widget_get_requisition                         not implemented
;;;     gtk_widget_device_is_shadowed
;;;     gtk_widget_get_modifier_mask
;;;     gtk_widget_insert_action_group
;;;     gtk_widget_list_action_prefixes
;;;     gtk_widget_get_action_group
;;;     gtk_widget_get_path
;;;     gtk_widget_get_style_context
;;;     gtk_widget_reset_style
;;;     gtk_widget_class_get_css_name
;;;     gtk_widget_class_set_css_name
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free
;;;     gtk_widget_get_preferred_height
;;;     gtk_widget_get_preferred_width
;;;     gtk_widget_get_preferred_height_for_width
;;;     gtk_widget_get_preferred_width_for_height
;;;     gtk_widget_get_preferred_height_and_baseline_for_width
;;;     gtk_widget_get_request_mode
;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation                  not implemented
;;;     gtk_widget_get_valign_with_baseline
;;;     gtk_widget_queue_compute_expand
;;;     gtk_widget_compute_expand
;;;     gtk_widget_init_template
;;;     gtk_widget_class_set_template
;;;     gtk_widget_class_set_template_from_resource
;;;     gtk_widget_get_template_child
;;;     gtk_widget_class_bind_template_child
;;;     gtk_widget_class_bind_template_child_internal
;;;     gtk_widget_class_bind_template_child_private
;;;     gtk_widget_class_bind_template_child_internal_private
;;;     gtk_widget_class_bind_template_child_full
;;;     gtk_widget_class_bind_template_callback
;;;     gtk_widget_class_bind_template_callback_full
;;;     gtk_widget_class_set_connect_func
;;;
;;; Properties
;;;
;;;     app-paintable
;;;     can-default
;;;     can-focus
;;;     composite-child
;;;     double-buffered
;;;     events
;;;     expand
;;;     focus-on-click
;;;     halign
;;;     has-default
;;;     has-focus
;;;     has-tooltip
;;;     height-request
;;;     hexpand
;;;     hexpand-set
;;;     is-focus
;;;     margin
;;;     margin-bottom
;;;     margin-end
;;;     margin-left
;;;     margin-right
;;;     margin-start
;;;     margin-top
;;;     name
;;;     no-show-all
;;;     opacity
;;;     parent
;;;     receives-default
;;;     scale-factor
;;;     sensitive
;;;     style
;;;     tooltip-markup
;;;     tooltip-text
;;;     valign
;;;     vexpand
;;;     vexpand-set
;;;     visible
;;;     width-request
;;;     window
;;;
;;; Signals
;;;
;;;     accel-closures-changed
;;;     button-press-event
;;;     button-release-event
;;;     can-activate-accel
;;;     child-notify
;;;     composited-changed
;;;     configure-event
;;;     damage-event
;;;     delete-event
;;;     destroy
;;;     destroy-event
;;;     direction-changed
;;;     drag-begin
;;;     drag-data-delete
;;;     drag-data-get
;;;     drag-data-received
;;;     drag-drop
;;;     drag-end
;;;     drag-failed
;;;     drag-leave
;;;     drag-motion
;;;     draw
;;;     enter-notify-event
;;;     event
;;;     event-after
;;;     focus
;;;     focus-in-event
;;;     focus-out-event
;;;     grab-broken-event
;;;     grab-focus
;;;     grab-notify
;;;     hide
;;;     hierarchy-changed
;;;     key-press-event
;;;     key-release-event
;;;     keynav-failed
;;;     leave-notify-event
;;;     map
;;;     map-event
;;;     mnemonic-activate
;;;     motion-notify-event
;;;     move-focus
;;;     parent-set
;;;     popup-menu
;;;     property-notify-event
;;;     proximity-in-event
;;;     proximity-out-event
;;;     query-tooltip
;;;     realize
;;;     screen-changed
;;;     scroll-event
;;;     selection-clear-event
;;;     selection-get
;;;     selection-notify-event
;;;     selection-received
;;;     selection-request-event
;;;     show
;;;     show-help
;;;     size-allocate
;;;     state-changed
;;;     state-flags-changed
;;;     style-set
;;;     style-updated
;;;     touch-event
;;;     unmap
;;;     unmap-event
;;;     unrealize
;;;     visibility-notify-event
;;;     window-state-event
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAllocation
;;; ----------------------------------------------------------------------------

;; GtkAllocation is not implemented. In the C implementation it is a synonym
;; for GdkRectangle

;;; ----------------------------------------------------------------------------
;;; GtkWidgetHelpType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkWidgetHelpType" widget-help-type
  (:export t
   :type-initializer "gtk_widget_help_type_get_type")
  (:tooltip 0)
  (:whats-this 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'widget-help-type)
      "GEnum"
      (liber:symbol-documentation 'widget-help-type)
 "@version{#2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkWidgetHelpType\" widget-help-type
  (:export t
   :type-initializer \"gtk_widget_help_type_get_type\")
  (:tooltip 0)
  (:whats-this 1))
  @end{declaration}
  @begin{short}
    Kinds of widget-specific help used in the @code{\"show-help\"} signal
    handler.
  @end{short}
  @class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; enum GtkTextDirection
;;; ----------------------------------------------------------------------------

;; --> gtk.enumerations.lisp

;;; ----------------------------------------------------------------------------
;;; GtkSizeRequestMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkSizeRequestMode" size-request-mode
  (:export t
   :type-initializer "gtk_size_request_mode_get_type")
  (:height-for-width 0)
  (:width-for-height 1)
  (:constant-size 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'size-request-mode)
      "GEnum"
      (liber:symbol-documentation 'size-request-mode)
 "@version{#2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkSizeRequestMode\" size-request-mode
  (:export t
   :type-initializer \"gtk_size_request_mode_get_type\")
  (:height-for-width 0)
  (:width-for-height 1)
  (:constant-size 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:height-for-width]{Prefer height-for-width geometry management.}
      @entry[:width-for-height]{Prefer width-for-height geometry management.}
      @entry[:constant-size]{Do not trade height-for-width or width-for-height
        geometry management.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies a preference for height-for-width or width-for-height geometry
    management.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-request-mode}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRequestedSize
;;; ----------------------------------------------------------------------------

;; Only needed in the function gtk_distribute_natual_allocation. This function
;; is not implemented and we do not export this structure.

(cffi:defcstruct requested-size
  (data :pointer)
  (minimum-size :int)
  (natural-size :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'requested-size)
      "CStruct"
      (liber:symbol-documentation 'requested-size)
 "@version{#2021-4-27}
  @begin{short}
    Represents a request of a screen object in a given orientation.
  @end{short}
  These are primarily used in container implementations when allocating a
  natural size for children calling. See the
  @fun{gtk:distribute-natural-allocation} function.
  @begin{pre}
(cffi:defcstruct requested-size
  (data :pointer)
  (minimum-size :int)
  (natural-size :int))
  @end{pre}
  @begin[code]{table}
    @entry[data]{A client pointer.}
    @entry[minimum-size]{The minimum size needed for allocation in a given
      orientation.}
    @entry[natural-size]{The natural size for allocation in a given
      orientation.}
  @end{table}
  @see-function{gtk:distribute-natural-allocation}")

;;; ----------------------------------------------------------------------------
;;; GtkAlign
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkAlign" align
  (:export t
   :type-initializer "gtk_align_get_type")
  (:fill 0)
  (:start 1)
  (:end 2)
  (:center 3)
  (:baseline 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'align)
      "GEnum"
      (liber:symbol-documentation 'align)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkAlign\" align
  (:export t
   :type-initializer \"gtk_align_get_type\")
  (:fill 0)
  (:start 1)
  (:end 2)
  (:center 3)
  (:baseline 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:fill]{Stretch to fill all space if possible, center if no
        meaningful way to stretch.}
      @entry[:start]{Snap to left or top side, leaving space on right or
        bottom.}
      @entry[:end]{Snap to right or bottom side, leaving space on left or top.}
      @entry[:center]{Center natural width of widget inside the allocation.}
      @entry[:baseline]{Align the widget according to the baseline.}
    @end{table}
  @end{values}
  @begin{short}
    Controls how a widget deals with extra space in a single x or y dimension.
  @end{short}
  Alignment only matters if the widget receives a \"too large\" allocation, for
  example if you packed the widget with the @slot[gtk:widget]{expand} flag
  inside a @class{gtk:box} widget, then the widget might get extra space. If you
  have for example a 16 x 16 icon inside a 32 x 32 space, the icon could be
  scaled and stretched, it could be centered, or it could be positioned to one
  side of the space.

  Note that in horizontal context the @code{:start} and @code{:end} values are
  interpreted relative to text direction.

  The @code{:baseline} support is optional for containers and widgets, and it
  is only supported for vertical alignment. When it is not supported by a child
  widget or a container it is treated as the @code{:fill} value.
  @see-class{gtk:widget}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}")

;;; ----------------------------------------------------------------------------
;;; GtkRequisition
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct requisition "GtkRequisition"
  (:export t
   :type-initializer "gtk_requisition_get_type")
  (width :int :initform 0)
  (height :int :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'requisition)
      "GBoxed"
      (documentation 'requisition 'type)
 "@version{#2023-3-8}
  @begin{short}
    A @class{gtk:requisition} structure represents the desired size of a widget.
  @end{short}
  See the section called \"Height-for-width Geometry Management\" in the
  @class{gtk:widget} documentation for more information.
  @begin{pre}
(glib:define-gboxed-cstruct requisition \"GtkRequisition\"
  (:export t
   :type-initializer \"gtk_requistion_get_type\")
  (width :int :initform 0)
  (height :int :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[width]{An integer with the desired width of the widget.}
    @entry[height]{An integer with the desired height of the widget.}
  @end{table}
  @see-slot{gtk:requisition-height}
  @see-slot{gtk:requisition-width}
  @see-constructor{gtk:requisition-new}
  @see-constructor{gtk:requisition-copy}
  @see-class{gtk:widget}
  @see-function{gtk:widget-preferred-size}")

;;; ----------------------------------------------------------------------------
;;; Accessors of GtkRequistion
;;; ----------------------------------------------------------------------------

;;; --- requisition-height -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'requisition-height)
      "Accessor"
      (documentation 'requisition-height 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:requisition-height instance) => height}
  @syntax{(setf (gtk:requisition-height instance) height)}
  @argument[instance]{a @class{gtk:requisition} instance}
  @argument[height]{an integer with the height}
  @begin{short}
    Accessor of the @arg{height} slot of the @class{gtk:requisition} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar requisition (gtk:requisition-new)) => REQUISITION
(setf (gtk:requisition-height requisition) 100) => 100
(gtk:requisition-height requisition) => 100
    @end{pre}
  @end{dictionary}
  @see-class{gtk:requisition}
  @see-function{gtk:requisition-width}")

;;; --- requisition-width ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'requisition-width)
      "Accessor"
      (documentation 'requisition-width 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:requisition-width instance) => width}
  @syntax{(setf (gtk:requisition-width instance) width)}
  @argument[instance]{a @class{gtk:requisition} instance}
  @argument[width]{an integer with the width}
  @begin{short}
    Accessor of the @arg{width} slot of the @class{gtk:requisition} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar requisition (gtk:requisition-new)) => REQUISITION
(setf (gtk:requisition-width requisition) 100) => 100
(gtk:requisition-width requisition) => 100
    @end{pre}
  @end{dictionary}
  @see-class{gtk:requisition}
  @see-function{gtk:requisition-height}")

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline requisition-new))

(defun requisition-new (&key (width 0) (height 0))
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[width]{an integer with the width, default 0}
  @argument[height]{an integer with the height, default 0}
  @begin{return}
    A new @class{gtk:requisition} instance.
  @end{return}
  @begin{short}
    Allocates a new @class{gtk:requisition} instance.
  @end{short}
  @see-class{gtk:requisition}"
  (make-requisition :width width :height height))

(export 'requisition-new)

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline requisition-copy))

(defun requisition-copy (requisition)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[requisition]{a @class{gtk:requisition} instance}
  @return{The copy of @arg{requisition}.}
  @begin{short}
    Copies a @class{gtk:requisition} instance.
  @end{short}
  @see-class{gtk:requisition}"
  (copy-requisition requisition))

(export 'requisition-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_free ()
;;;
;;; void gtk_requisition_free (GtkRequisition *requisition);
;;;
;;; Frees a GtkRequisition.
;;;
;;; requisition :
;;;     a GtkRequisition
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWidget" widget
  (:superclass g:initially-unowned
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_widget_get_type")
  ((app-paintable
    widget-app-paintable
    "app-paintable" "gboolean" t t)
   (can-default
    widget-can-default
    "can-default" "gboolean" t t)
   (can-focus
    widget-can-focus
    "can-focus" "gboolean" t t)
   (composite-child
    widget-composite-child
    "composite-child" "gboolean" t nil)
   (double-buffered
    widget-double-buffered
    "double-buffered" "gboolean" t t)
   (events
    widget-events
    "events" "GdkEventMask" t t)
   (expand
    widget-expand
    "expand" "gboolean" t t)
   (focus-on-click
    widget-focus-on-click
    "focus-on-click" "gboolean" t t)
   (halign
    widget-halign
    "halign" "GtkAlign" t t)
   (has-default
    widget-has-default
    "has-default" "gboolean" t t)
   (has-focus
    widget-has-focus
    "has-focus" "gboolean" t t)
   (has-tooltip
    widget-has-tooltip
    "has-tooltip" "gboolean" t t)
   (height-request
    widget-height-request
    "height-request" "gint" t t)
   (hexpand
    widget-hexpand
    "hexpand" "gboolean" t t)
   (hexpand-set
    widget-hexpand-set
    "hexpand-set" "gboolean" t t)
   (is-focus
    widget-is-focus
    "is-focus" "gboolean" t t)
   (margin
    widget-margin
    "margin" "gint" t t)
   (margin-bottom
    widget-margin-bottom
    "margin-bottom" "gint" t t)
   (margin-end
    widget-margin-end
    "margin-end" "gint" t t)
   (margin-left
    widget-margin-left
    "margin-left" "gint" t t)
   (margin-right
    widget-margin-right
    "margin-right" "gint" t t)
   (margin-start
    widget-margin-start
    "margin-start" "gint" t t)
   (margin-top
    widget-margin-top
    "margin-top" "gint" t t)
   (name
    widget-name
    "name" "gchararray" t t)
   (no-show-all
    widget-no-show-all
    "no-show-all" "gboolean" t t)
   (opacity
    widget-opacity
    "opacity" "gdouble" t t)
   (parent
    widget-parent
    "parent" "GtkContainer" t t)
   (receives-default
    widget-receives-default
    "receives-default" "gboolean" t t)
   (scale-factor
    widget-scale-factor
    "scale-factor" "gint" t nil)
   (sensitive
    widget-sensitive
    "sensitive" "gboolean" t t)
   (style
    widget-style
    "style" "GtkStyle" t t)
   (tooltip-markup
    widget-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text
    widget-tooltip-text
    "tooltip-text" "gchararray" t t)
   (valign
    widget-valign
    "valign" "GtkAlign" t t)
   (vexpand
    widget-vexpand
    "vexpand" "gboolean" t t)
   (vexpand-set
    widget-vexpand-set
    "vexpand-set" "gboolean" t t)
   (visible
    widget-visible
    "visible" "gboolean" t t)
   (width-request
    widget-width-request
    "width-request" "gint" t t)
   (window
    widget-window
    "window" "GdkWindow" t nil)))

#+liber-documentation
(setf (documentation 'widget 'type)
 "@version{#2023-3-8}
  @begin{short}
    The @class{gtk:widget} class is the base class all widgets in GTK derive
    from. It manages the widget life cycle, states and style.
  @end{short}

  @subheading{Height-for-width Geometry Management}
  GTK uses a height-for-width and width-for-height geometry management system.
  Height-for-width means that a widget can change how much vertical space it
  needs, depending on the amount of horizontal space that it is given and
  similar for width-for-height. The most common example is a label that reflows
  to fill up the available width, wraps to fewer lines, and therefore needs
  less height.

  GTK also supports baseline vertical alignment of widgets. This means that
  widgets are positioned such that the typographical baseline of widgets in the
  same row are aligned. This happens if a widget supports baselines, has a
  vertical alignment of @code{:baseline}, and is inside a container that
  supports baselines and has a natural \"row\" that it aligns to the baseline,
  or a baseline assigned to it by the grandparent.

  If a widget ends up baseline aligned it will be allocated all the space in
  the parent as if it was @code{:fill}, but the selected baseline can be found
  via the @fun{gtk:widget-allocated-baseline} function. If this has a value
  other than -1 you need to align the widget such that the baseline appears at
  the position.

  @subheading{Style Properties}
  The @class{gtk:widget} class introduces style properties - these are basically
  object properties that are stored not on the object, but in the style object
  associated to the widget. Style properties are set in resource files. This
  mechanism is used for configuring such things as the location of the
  scrollbar arrows through the theme, giving theme authors more control over
  the look of applications without the need to write a theme engine.

  Use the @fun{gtk:widget-class-find-style-property} or
  @fun{gtk:widget-class-list-style-properties} functions to get information
  about existing style properties and the @fun{gtk:widget-style-property}
  function to obtain the value of a style property.

  @subheading{GtkWidget as GtkBuildable}
  The @class{gtk:widget} implementation of the @class{gtk:buildable} interface
  supports a custom @code{<accelerator>} element, which has attributes named
  @code{key}, @code{modifiers} and @code{signal} and allows to specify
  accelerators.

  @b{Example:} A UI definition fragment specifying an accelerator
  @begin{pre}
<object class=\"GtkButton\">
  <accelerator key=\"q\" modifiers=\"GDK_CONTROL_MASK\" signal=\"clicked\"/>
</object>
  @end{pre}
  In addition to accelerators, the @class{gtk:widget} implementation also
  support a custom @code{<accessible>} element, which supports actions and
  relations. Properties on the accessible implementation of an object can be set
  by accessing the internal child @code{\"accessible\"} of a @class{gtk:widget}
  object.

  @b{Example:} A UI definition fragment specifying an accessible
  @begin{pre}
<object class=\"GtkButton\" id=\"label1\"/>
  <property name=\"label\">I am a Label for a Button</property>
</object>
<object class=\"GtkButton\" id=\"button1\">
  <accessibility>
    <action action_name=\"click\"
            translatable=\"yes\">Click the button.</action>
    <relation target=\"label1\" type=\"labelled-by\"/>
  </accessibility>
  <child internal-child=\"accessible\">
    <object class=\"AtkObject\" id=\"a11y-button1\">
      <property name=\"AtkObject::name\">Clickable Button</property>
    </object>
  </child>
</object>
  @end{pre}
  Finally, the @class{gtk:widget} implementation allows style information such
  as style classes to be associated with widgets, using the custom
  @code{<style>} element:

  @b{Example:} A UI definition fragment specifying an style class
  @begin{pre}
<object class=\"GtkButton\" id=\"button1\">
  <style>
    <class name=\"my-special-button-class\"/>
    <class name=\"dark-button\"/>
  </style>
</object>
  @end{pre}

  @subheading{Building composite widgets from template XML}
  The @class{gtk:widget} implementation exposes some facilities to automate the
  proceedure of creating composite widgets using the @class{gtk:builder}
  interface description language.

  To create composite widgets with @class{gtk:builder} XML, one must associate
  the interface description with the widget class at class initialization time
  using the @fun{gtk:widget-class-set-template} function.

  The interface description semantics expected in composite template
  descriptions is slightly different from regulare @class{gtk:builder} XML.
  Unlike regular interface descriptions, the @fun{gtk:widget-class-set-template}
  function will expect a @code{<template>} tag as a direct child of the toplevel
  @code{<interface>} tag. The @code{<template>} tag must specify the
  @code{\"class\"} attribute which must be the type name of the widget.
  Optionally, the @code{\"parent\"} attribute may be specified to specify the
  direct parent type of the widget type, this is ignored by the
  @class{gtk:builder} object but required for Glade to introspect what kind of
  properties and internal children exist for a given type when the actual type
  does not exist.

  The XML which is contained inside the @code{<template>} tag behaves as if it
  were added to the @code{<object>} tag defining the widget itself. You may set
  properties on the widget by inserting @code{<property>} tags into the
  @code{<template>} tag, and also add @code{<child>} tags to add children and
  extend the widget in the normal way you would with @code{<object>} tags.

  Additionally, @code{<object>} tags can also be added before and after the
  initial @code{<template>} tag in the normal way, allowing one to define
  auxilary objects which might be referenced by other widgets declared as
  children of the @code{<template>} tag.

  @b{Example:} A @class{btk-builder} template definition
    @begin{pre}
<interface>
  <template class=\"FooWidget\" parent=\"GtkBox\">
    <property name=\"orientation\">GTK_ORIENTATION_HORIZONTAL</property>
    <property name=\"spacing\">4</property>
    <child>
      <object class=\"GtkButton\" id=\"hello_button\">
        <property name=\"label\">Hello World</property>
      </object>
    </child>
    <child>
      <object class=\"GtkButton\" id=\"goodbye_button\">
        <property name=\"label\">Goodbye World</property>
      </object>
    </child>
  </template>
</interface>
    @end{pre}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[cursor-aspect-ratio]{entry}
        The @code{cursor-aspect-ratio} style property of type @code{:float}
        (Read) @br{}
        Aspect ratio with which to draw insertion cursor. @br{}
        Allowed values: [0.0,1.0] @br{}
        Default value: 0.04
      @end{entry}
      @begin[cursor-color]{entry}
        The @code{cursor-color} style property of type @class{gdk:color}
        (Read ) @br{}
        Color with which to draw insertion cursor.
      @end{entry}
      @begin[focus-line-pattern]{entry}
        The @code{focus-line-pattern} style property of type @code{:string}
        (Read) @br{}
        Dash pattern used to draw the focus indicator. @br{}
        @em{Warning:} The @code{focus-line-pattern} style property has been
        deprecated since version 3.14 and should not be used in newly written
        code. Use the outline-style CSS property instead. @br{}
        Default value: \"\001\001\"
      @end{entry}
      @begin[focus-line-width]{entry}
        The @code{focus-line-width} style property of type @code{:int}
        (Read) @br{}
        Width, in pixels, of the focus indicator line. @br{}
        @em{Warning:} The @code{focus-line-width} style property has been
        deprecated since version 3.14 and should not be used in newly written
        code. Use the outline-width CSS property instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[focus-padding]{entry}
        The @code{focus-padding} style property of type @code{:int}
        (Read) @br{}
        Width, in pixels, between focus indicator and the widget 'box'. @br{}
        @em{Warning:} The @code{focus-padding} style property has been
        deprecated since version 3.14 and should not be used in newly written
        code. Use the padding CSS property instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[interior-focus]{entry}
        The @code{interior-focus} style property of type @code{:boolean}
        (Read) @br{}
        Whether to draw the focus indicator inside widgets. @br{}
        @em{Warning:} The @code{interior-focus} style property has been
        deprecated since version 3.14 and should not be used in newly written
        code. Use the outline CSS property instead. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[link-color]{entry}
        The @code{link-color} style property of type @class{gdk:color}
        (Read) @br{}
        Defines the color of unvisited links. @br{}
        @em{Warning:} The @code{link-color} style property has been
        deprecated since version 3.12 and should not be used in newly written
        code. Links now use a separate state flags for selecting different
        theming. This style property is ignored.
      @end{entry}
      @begin[scroll-arrow-hlength]{entry}
        The @code{scroll-arrow-hlength} style property of type @code{:int}
        (Read) @br{}
        Defines the length of horizontal scroll arrows. @br{}
        Allowed values: >= 1 @br{}
        Default value: 16
      @end{entry}
      @begin[scroll-arrow-vlength]{entry}
        The @code{scroll-arrow-vlength} style property of type @code{:int}
        (Read) @br{}
        Defines the length of vertical scroll arrows. @br{}
        Allowed values: >= 1 @br{}
        Default value: 16
      @end{entry}
      @begin[secondary-cursor-color]{entry}
        The @code{secondary-cursor-color} style property of type
        @class{gdk:color} (Read) @br{}
        Color with which to draw the secondary insertion cursor when editing
        mixed right-to-left and left-to-right text.
      @end{entry}
      @begin[separator-height]{entry}
        The @code{separator-height} style property of type @code{:int}
        (Read) @br{}
        Defines the height of separators. This property only takes effect if
        the @code{wide-separators} style property is @em{true}. @br{}
        @em{Warning:} The @code{separator-height} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard min-height CSS property on the separator elements
        to size separators. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[separator-width]{entry}
        The @code{separator-width} style property of type @code{:int}
        (Read) @br{}
        Defines the width of separators. This property only takes effect if the
        @code{wide-separators} style property is @em{true}. @br{}
        @em{Warning:} The @code{separator-width} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard min-height CSS property on the separator elements
        to size separators. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[text-handle-height]{entry}
        The @code{text-handle-height} style property of type @code{:int}
        (Read) @br{}
        Height of text selection handles. @br{}
        Allowed values: >= 1 @br{}
        Default value: 24
      @end{entry}
      @begin[text-handle-width]{entry}
        The @code{text-handle-width} style property of type @code{:int}
        (Read) @br{}
        Width of text selection handles. @br{}
        Allowed values: >= 1 @br{}
        Default value: 20
      @end{entry}
      @begin[visited-link-color]{entry}
        The @code{visited-link-color} style property of type @class{gdk:color}
        (Read) @br{}
        Defines the color of visited links. @br{}
        @em{Warning:} The @code{visited-link-color} style property has been
        deprecated since version 3.12 and should not be used in newly written
        code. Links now use a separate state flags for selecting different
        theming. This style property is ignored.
      @end{entry}
      @begin[wide-separators]{entry}
        The @code{wide-separators} style property of type @code{:boolean}
        (Read) @br{}
        Defines whether separators have configurable width and should be drawn
        using a box instead of a line. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[window-dragging]{entry}
        The @code{window-dragging} style property of type @code{:boolean}
        (Read) @br{}
        Whether windows can be dragged by clicking on empty areas. @br{}
        Default value: @em{false}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"accel-closures-changed\" signal}
      @begin{pre}
lambda (widget)
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"button-press-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a button typically from a mouse is pressed. To receive this
      signal, the @class{gdk:window} object associated to the widget needs to
      enable the @code{:button-press-mask} mask of the @symbol{gdk:event-mask}
      flags. The signal will be sent to the grab widget if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"button-release-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a button typically from a mouse is released. To receive this
      signal, the @class{gdk:window} object associated to the widget needs to
      enable the @code{:button-realease-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"can-activate-accel\" signal}
      @begin{pre}
lambda (widget signal)    :run-last
      @end{pre}
      Determines whether an accelerator that activates the signal identified by
      @arg{signal} can currently be activated. The signal is present to allow
      applications and derived widgets to override the default handling for
      determining whether an accelerator can be activated.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[signal]{An unsigned integer with the ID of a signal installed on
          the widget.}
        @entry[Returns]{@em{True} if the signal can be activated.}
      @end{table}
    @subheading{The \"child-notify\" signal}
      @begin{pre}
lambda (widget pspec)    :no-hooks
      @end{pre}
      Emitted for each child property that has changed on an object. The
      detail of the signal holds the property name.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[pspec]{The @symbol{g:param-spec} instance of the changed child
          property.}
      @end{table}
    @subheading{The \"composited-changed\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      Emitted when the composited status of widgets screen changes.@br{}
      @em{Warning:} The @code{\"composited-changed\"} signal has been deprecated
      since version 3.22 and should not be used in newly written code. Use the
      @code{\"composited-changed\"} signal of the @class{gdk:screen} class
      instead.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"configure-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the size, position or stacking of the GDK window of the
      widget has changed. To receive this signal, the @class{gdk:window} object
      associated to the widget needs to enable the @code{:structure-mask} mask
      of the @symbol{gdk:event-mask} flags. GDK will enable this mask
      automatically for all new windows.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-configure} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"damage-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a redirected window belonging to the widget gets drawn into.
      The region/area members of the event shows what area of the redirected
      drawable was drawn into.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-expose} event.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"delete-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted if a user requests that a toplevel window is closed. The default
      handler for this signal destroys the window. Connecting the
      @fun{gtk:widget-hide-on-delete} function to this signal will cause the
      window to be hidden instead, so that it can later be shown again without
      reconstructing it.
    @begin[code]{table}
      @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @entry[event]{The @class{gdk:event} event which triggered this signal.}
      @entry[Returns]{@em{True} to stop other handlers from being invoked for
        the event, @em{false} to propagate the event further.}
    @end{table}
    @subheading{The \"destroy\" signal}
      @begin{pre}
lambda (widget)    :no-hooks
      @end{pre}
      Signals that all holders of a reference to the widget should release the
      reference that they hold. May result in finalization of the widget if all
      references are released.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
    @end{table}
    @subheading{The \"destroy-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a @class{gdk:window} object is destroyed. You rarely get this
      signal, because most widgets disconnect themselves from their GDK window
      before they destroy it, so no widget owns the GDK window at destroy time.
      To receive this signal, the @class{gdk:window} object associated to the
      widget needs to enable the @code{:structure-mask} mask of the
      @symbol{gdk:event-mask} flags. GDK will enable this mask automatically
      for all new windows.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"direction-changed\" signal}
      @begin{pre}
lambda (widget direction)    :run-first
      @end{pre}
      Emitted when the text direction of a widget changes.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[direction]{The previous @symbol{gtk:text-direction} text
          direction of the widget.}
      @end{table}
    @subheading{The \"drag-begin\" signal}
      @begin{pre}
lambda (widget context)    :run-last
      @end{pre}
      Emitted on the drag source when a drag is started. A typical reason to
      connect to this signal is to set up a custom drag icon with e.g. the
      @fun{gtk:drag-source-set-icon-pixbuf} function. Note that some widgets set
      up a drag icon in the default handler of this signal, so you may have to
      use the @fun{g:signal-connect} function to override what the default
      handler did.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} object.}
      @end{table}
    @subheading{The \"drag-data-delete\" signal}
      @begin{pre}
lambda (widget context)    :run-last
      @end{pre}
      Emitted on the drag source when a drag with the @code{:move} action of
      the @symbol{gdk:drag-action} flags is successfully completed. The signal
      handler is responsible for deleting the data that has been dropped. What
      \"delete\" means depends on the context of the drag operation.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} object.}
      @end{table}
    @subheading{The \"drag-data-get\" signal}
      @begin{pre}
lambda (widget context selection info time)    :run-last
      @end{pre}
      Emitted on the drag source when the drop site requests the data which
      is dragged. It is the responsibility of the signal handler to fill the
      @arg{selection} argument with the data in the format which is indicated
      by the @arg{info} argument. See the @fun{gtk:selection-data-set} and
      @fun{gtk:selection-data-text} functions.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} object.}
        @entry[selection]{The @class{gtk:selection-data} instance to be filled
          with the dragged data.}
        @entry[info]{An unsigned integer with the info that has been registered
          with the target in the @class{gtk:target-list} instance.}
      @entry[time]{An unsigned integer with the timestamp at which the data was
        requested.}
    @end{table}
    @subheading{The \"drag-data-received\" signal}
      @begin{pre}
lambda (widget context x y selection info time)    :run-last
      @end{pre}
      Emitted on the drop site when the dragged data has been received. If the
      data was received in order to determine whether the drop will be accepted,
      the handler is expected to call the @fun{gdk:drag-status} function and not
      finish the drag. If the data was received in response to a
      @code{\"drag-drop\"} signal and this is the last target to be received,
      the handler for this signal is expected to process the received data and
      then call the @fun{gtk:drag-finish} function, setting the success
      parameter depending on whether the data was processed successfully.
      Applications must create some means to determine why the signal was
      emitted and therefore whether to call the @fun{gdk:drag-status} or
      @fun{gtk:drag-finish} functions. The handler may inspect the selected
      action with the @fun{gdk:drag-context-selected-action} function before
      calling the @fun{gtk:drag-finish} function, e.g. to implement the
      @code{:ask} value of the @symbol{gdk:drag-action} flags as shown in the
      following example:
      @begin{pre}
void
drag_data_received (GtkWidget          *widget,
                    GdkDragContext     *context,
                    gint                x,
                    gint                y,
                    GtkSelectionData   *data,
                    guint               info,
                    guint               time)
{
  if ((data->length >= 0) && (data->format == 8))
    {
      GdkDragAction action;

      // handle data here

      action = gdk_drag_context_get_selected_action (context);
      if (action == GDK_ACTION_ASK)
        {
          GtkWidget *dialog;
          gint response;

          dialog = gtk_message_dialog_new (NULL,
                                           GTK_DIALOG_MODAL |
                                           GTK_DIALOG_DESTROY_WITH_PARENT,
                                           GTK_MESSAGE_INFO,
                                           GTK_BUTTONS_YES_NO,
                                           \"Move the data ?\n\");
          response = gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);

          if (response == GTK_RESPONSE_YES)
            action = GDK_ACTION_MOVE;
          else
            action = GDK_ACTION_COPY;
         @}

      gtk_drag_finish (context, TRUE, action == GDK_ACTION_MOVE, time);
    @}
  else
    gtk_drag_finish (context, FALSE, FALSE, time);
@}
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
        @entry[x]{An integer where the drop happened.}
        @entry[y]{An integer where the drop happened.}
        @entry[selection]{The received @class{gtk:selection-data} data.}
        @entry[info]{An unsigned integer with the info that has been registered
          with the target in the @class{gtk:target-list} instance.}
        @entry[time]{An unsigned integer with the timestamp at which the data
          was received.}
      @end{table}
    @subheading{The \"drag-drop\" signal}
      @begin{pre}
lambda (widget context x y time)    :run-last
      @end{pre}
      Emitted on the drop site when the user drops the data onto the widget.
      The signal handler must determine whether the cursor position is in a
      drop zone or not. If it is not in a drop zone, it returns @em{false} and
      no further processing is necessary. Otherwise, the handler returns
      @em{true}. In this case, the handler must ensure that the
      @fun{gtk:drag-finish} function is called to let the source know that the
      drop is done. The call to the @fun{gtk:drag-finish} function can be done
      either directly or in a @code{\"drag-data-received\"} signal handler which
      gets triggered by calling the @fun{gtk:drag-data} function to receive the
      data for one or more of the supported targets.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
        @entry[x]{An integer with the x coordinate of the current cursor
          position.}
        @entry[y]{An integer with the y coordinate of the current cursor
          position.}
        @entry[time]{An unsigned integer with the timestamp of the motion
          event.}
        @entry[Returns]{A boolean whether the cursor position is in a drop
          zone.}
      @end{table}
    @subheading{The \"drag-end\" signal}
      @begin{pre}
lambda (widget context)    :run-last
      @end{pre}
      Emitted on the drag source when a drag is finished. A typical reason to
      connect to this signal is to undo things done in the @code{\"drag-begin\"}
      signal handler.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
      @end{table}
    @subheading{The \"drag-failed\" signal}
      @begin{pre}
lambda (widget context result)    :run-last
      @end{pre}
      Emitted on the drag source when a drag has failed. The signal handler may
      hook custom code to handle a failed DND operation based on the type of
      error, it returns @em{true} if the failure has been already handled, not
      showing the default \"drag operation failed\" animation, otherwise it
      returns @em{false}.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
        @entry[result]{The @symbol{gtk:drag-result} result of the drag
          operation.}
        @entry[Returns]{@em{True} if the failed drag operation has been already
          handled.}
      @end{table}
    @subheading{The \"drag-leave\" signal}
      @begin{pre}
lambda (widget context time)    :run-last
      @end{pre}
      Emitted on the drop site when the cursor leaves the widget. A typical
      reason to connect to this signal is to undo things done in a
      @code{\"drag-motion\"} signal handler, e.g. undo highlighting with the
      @fun{gtk:drag-unhighlight} function. Likewise, the @code{\"drag-leave\"}
      signal is also emitted before the @code{\"drag-drop\"} signal, for
      instance to allow cleaning up of a preview item created in the
      @code{\"drag-motion\"} signal handler.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
        @entry[time]{An unsigned integer with the timestamp of the motion
          event.}
      @end{table}
    @subheading{The \"drag-motion\" signal}
      @begin{pre}
lambda (widget context x y time)    :run-last
      @end{pre}
      Emitted on the drop site when the user moves the cursor over the widget
      during a drag. The signal handler must determine whether the cursor
      position is in a drop zone or not. If it is not in a drop zone, it returns
      @em{false} and no further processing is necessary. Otherwise, the handler
      returns @em{true}. In this case, the handler is responsible for providing
      the necessary information for displaying feedback to the user, by calling
      the @fun{gdk:drag-status} function.

      If the decision whether the drop will be accepted or rejected cannot be
      made based solely on the cursor position and the type of the data, the
      handler may inspect the dragged data by calling the @fun{gtk:drag-data}
      function and defer the @fun{gdk:drag-status} function call to the
      @code{\"drag-data-received\"} signal handler. Note that you cannot pass
      the @code{:drop}, @code{:motion} or @code{:all} values of the
      @symbol{gtk:dest-defaults} flags to the @fun{gtk:drag-dest-set} function
      when using the @code{\"drag-motion\"} signal that way.

      Also note that there is no @code{\"drag-enter\"} signal. The drag receiver
      has to keep track of whether he has received any @code{\"drag-motion\"}
      signals since the last @code{\"drag-leave\"} signal and if not, treat the
      @code{\"drag-motion\"} signal as an @code{\"enter\"} signal. Upon an
      @code{\"enter\"}, the handler will typically highlight the drop site with
      the @fun{gtk:drag-highlight} function.
      @begin{pre}
static void
drag_motion (GtkWidget *widget,
             GdkDragContext *context,
             gint x,
             gint y,
             guint time)
{
  GdkAtom target;

  PrivateData *private_data = GET_PRIVATE_DATA (widget);

  if (!private_data->drag_highlight)
   {
     private_data->drag_highlight = 1;
     gtk_drag_highlight (widget);
   @}

  target = gtk_drag_dest_find_target (widget, context, NULL);
  if (target == GDK_NONE)
    gdk_drag_status (context, 0, time);
  else
   {
     private_data->pending_status = context->suggested_action;
     gtk_drag_get_data (widget, context, target, time);
   @}

  return TRUE;
@}

static void
drag_data_received (GtkWidget        *widget,
                    GdkDragContext   *context,
                    gint              x,
                    gint              y,
                    GtkSelectionData *selection_data,
                    guint             info,
                    guint             time)
{
  PrivateData *private_data = GET_PRIVATE_DATA (widget);

  if (private_data->suggested_action)
   {
     private_data->suggested_action = 0;

    /* We are getting this data due to a request in drag_motion,
     * rather than due to a request in drag_drop, so we are just
     * supposed to call gdk_drag_status (), not actually paste in
     * the data.
     */
     str = gtk_selection_data_get_text (selection_data);
     if (!data_is_acceptable (str))
       gdk_drag_status (context, 0, time);
     else
       gdk_drag_status (context, private_data->suggested_action, time);
   @}
  else
   {
     /* accept the drop */
   @}
@}
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[context]{The @class{gdk:drag-context} drag context.}
        @entry[x]{An integer with the x coordinate of the current cursor
          position.}
        @entry[y]{An integer with the y coordinate of the current cursor
          position.}
        @entry[time]{An unsigned integer with the timestamp of the motion
          event.}
        @entry[Returns]{A boolean whether the cursor position is in a drop
          zone.}
      @end{table}
    @subheading{The \"draw\" signal}
      @begin{pre}
lambda (widget cr)    :run-last
      @end{pre}
      Emitted when a widget is supposed to render itself. The top left corner
      of the widget must be painted at the origin of the passed in Cairo context
      and be sized to the values returned by the
      @fun{gtk:widget-allocated-width} and @fun{gtk:widget-allocated-height}
      functions. Signal handlers connected to this signal can modify the Cairo
      context in any way they like and do not need to restore it. The signal
      emission takes care of calling the @fun{cairo:save} function before and
      the @fun{cairo:restore} function after invoking the handler.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[cr]{The @class{gdk:cairo-context} Cairo context to draw to.}
      @end{table}
    @subheading{The \"enter-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the pointer enters the GDK window of the widget. To receive
      this signal, the @class{gdk:window} object associated to the widget needs
      to enable the @code{:enter-notify-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-crossing} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      The GTK main loop will emit three signals for each GDK event delivered
      to a widget: one generic @code{\"event\"} signal, another, more specific,
      signal that matches the type of event delivered, e.g. the
      @code{\"key-press-event\"} signal, and finally a generic
      @code{\"event-after\"} signal.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event and to cancel the emission of the second specific
          @code{\"event\"} signal. @em{False} to propagate the event further and
          to allow the emission of the second signal. The @code{\"event-after\"}
          signal is emitted regardless of the return value.}
      @end{table}
    @subheading{The \"event-after\" signal}
      @begin{pre}
lambda (widget event)
      @end{pre}
      After the emission of the @code{\"event\"} signal and optionally the
      second more specific signal, the @code{\"event-after\"} signal will be
      emitted regardless of the previous two signals handlers return values.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered this signal.}
      @end{table}
    @subheading{The \"focus\" signal}
      @begin{pre}
lambda (widget direction)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[direction]{The @symbol{gtk:direction-type} direction.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked
          for the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"focus-in-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the keyboard focus enters the GDK window of the widget. To
      receive this signal, the @class{gdk:window} object associated to the
      widget needs to enable the @code{:focus-change-mask} mask of the
      @symbol{gdk:event-mask} flags.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-focus} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
        the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"focus-out-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the keyboard focus leaves the GDK window of the widget. To
      receive this signal, the @class{gdk:window} object associated to the
      widget needs to enable the @code{:focus-change-mask} mask of the
      @symbol{gdk:event-mask} flags.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-focus} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
        the event, @em{false} topropagate the event further.}
      @end{table}
    @subheading{The \"grab-broken-event\" signal}
      @begin{pre}
lambda (widget event)    : run-last
      @end{pre}
      Emitted when a pointer or keyboard grab on a GDK window belonging to
      the widget gets broken. On X11, this happens when the grab window becomes
      unviewable, i.e. it or one of its ancestors is unmapped, or if the same
      application grabs the pointer or keyboard again.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-grab-broken} event.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"grab-focus\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"grab-notify\" signal}
      @begin{pre}
lambda (widget grabbed)    :run-first
      @end{pre}
      Emitted when a widget becomes shadowed by a GTK grab, not a pointer or
      keyboard grab, on another widget, or when it becomes unshadowed due to a
      grab being removed. A widget is shadowed by a the @fun{gtk:grab-add}
      function when the topmost grab widget in the grab stack of its window
      group is not its ancestor.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[grabbed]{@em{False} if the widget becomes shadowed, @em{true}
        if it becomes unshadowed.}
      @end{table}
    @subheading{The \"hide\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      Emitted when the widget is hidden, for example with the
      @fun{gtk:widget-hide} function.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"hierarchy-changed\" signal}
      @begin{pre}
lambda (widget toplevel)    :run-last
      @end{pre}
      Emitted when the anchored state of a widget changes. A widget is anchored
      when its toplevel ancestor is a @class{gtk:window} widget. The signal is
      emitted when a widget changes from un-anchored to anchored or vice-versa.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[toplevel]{The previous @class{gtk:widget} toplevel ancestor, or
          @code{nil} if the widget was previously unanchored.}
      @end{table}
    @subheading{The \"key-press-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a key is pressed. The signal emission will reoccur at the
      key-repeat rate when the key is kept pressed. To receive this signal, the
      @class{gdk:window} object associated to the widget needs to enable the
      @code{:key-press-mask} mask of the @symbol{gdk:event-mask} flags. This
      signal will be sent to the grab widget if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-key} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"key-release-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a key is released. To receive this signal, the
      @class{gdk:window} object associated to the widget needs to enable the
      @code{:key-release-mask} mask of the @symbol{gdk:event-mask} flags. This
      signal will be sent to the grab widget if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-key} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"keynav-failed\" signal}
      @begin{pre}
lambda (widget direction)    :run-last
      @end{pre}
      Gets emitted if keyboard navigation fails.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[direction]{The @symbol{gtk:direction-type} direction of
          movement.}
        @entry[Returns]{@em{True} if stopping keyboard navigation is fine,
          @em{false} if the emitting widget should try to handle the keyboard
          navigation attempt in its parent container(s).}
      @end{table}
    @subheading{The \"leave-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the pointer leaves the GDK window of the widget. To receive
      this signal, the @class{gdk:window} object associated to the widget needs
      to enable the @code{:leave-notify-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-crossing} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"map\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      Emitted when the widget is going to be mapped, that is when the widget
      is visible, which is controlled with the @fun{gtk:widget-visible}
      function, and all its parents up to the toplevel widget are also visible.
      Once the map has occurred, the @code{\"map-event\"} signal will be
      emitted. The @code{\"map\"} signal can be used to determine whether a
      widget will be drawn, for instance it can resume an animation that was
      stopped during the emission of the @code{\"unmap\"} signal.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"map-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the GDK window of the widget is mapped. A window is mapped
      when it becomes visible on the screen. To receive this signal, the
      @class{gdk:window} object associated to the widget needs to enable the
      @code{:structure-mask} mask of the @symbol{gdk:event-mask} flags. GDK will
      enable this mask automatically for all new windows.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"mnemonic-activate\" signal}
      The default handler for this signal activates the widget if the
      @arg{cycling} argument is @em{false}, or just makes the widget grab focus
      if the @arg{cycling} argument is @em{true}.
      @begin{pre}
lambda (widget cycling)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[cycling]{@em{True} if there are other widgets with the same
          mnemonic.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked
          for the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"motion-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the pointer moves over the GDK window of the widget. To
      receive this signal, the @class{gdk:window} object associated to the
      widget needs to enable the @code{:pointer-motion-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-motion} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"move-focus\" signal}
      @begin{pre}
lambda (widget direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[direction]{The @symbol{gtk:direction-type} direction.}
      @end{table}
    @subheading{The \"parent-set\" signal}
      @begin{pre}
lambda (widget parent)    :run-first
      @end{pre}
      Emitted when a new parent has been set on a widget.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[parent]{The previous @class{gtk:widget} parent, or @code{nil} if
          the widget just got its initial parent.}
      @end{table}
    @subheading{The \"popup-menu\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      Gets emitted whenever a widget should pop up a context menu. This usually
      happens through the standard key binding mechanism. By pressing a certain
      key while a widget is focused, the user can cause the widget to pop up a
      menu.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[Returns]{@em{True} if a menu was activated.}
      @end{table}
    @subheading{The \"property-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a property on the GDK window of the widget has been changed
      or deleted. To receive this signal, the @class{gdk:window} object
      associated to the widget needs to enable the @code{:property-change-mask}
      mask of the @symbol{gdk:event-mask} flags.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-property} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
    @end{table}
    @subheading{The \"proximity-in-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      To receive this signal the @class{gdk:window} object associated to the
      widget needs to enable the @code{:proximity-in-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-proximity} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"proximity-out-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      To receive this signal the @class{gdk:window} object associated to the
      widget needs to enable the @code{:proximity-out-mask} mask of the
      @symbol{gdk:event-mask} flags. The signal will be sent to the grab widget
      if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-proximity} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
     @end{table}
   @subheading{The \"query-tooltip\" signal}
     @begin{pre}
lambda (widget x y mode tooltip)    :run-last
     @end{pre}
     Emitted when the @code{has-tooltip} property is @em{true} and the
     @slot[gtk:settings]{gtk-tooltip-timeout} setting has expired with the
     cursor hovering \"above\" widget, or emitted when the widget got focus in
     keyboard mode. Using the given coordinates, the signal handler should
     determine whether a tooltip should be shown for the widget. If this is the
     case @em{true} should be returned, @em{false} otherwise. Note that if the
     @arg{mode} argument is @em{true}, the @arg{x} and @arg{y} values are
     undefined and should not be used. The signal handler is free to manipulate
     the @arg{tooltip} argument with the therefore destined function calls.
     @begin[code]{table}
       @entry[widget]{The @class{gtk:widget} object which received the signal.}
       @entry[x]{An integer with the x coordinate of the cursor position where
         the request has been emitted, relative to the left side of the widget.}
       @entry[y]{An integer with the y coordinate of the cursor position where
         the request has been emitted, relative to the top of the widget.}
       @entry[mode]{@em{True} if the tooltip was trigged using the keyboard.}
       @entry[tooltip]{A @class{gtk:tooltip} object.}
       @entry[Returns]{@em{True} if tooltip should be shown right now,
         @em{false} otherwise.}
     @end{table}
    @subheading{The \"realize\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      Emitted when the widget is associated with a @class{gdk:window} object,
      which means that the @fun{gtk:widget-realize} function has been called or
      the widget has been mapped, that is, it is going to be drawn.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"screen-changed\" signal}
      @begin{pre}
lambda (widget screen)    :run-last
      @end{pre}
      Gets emitted when the screen of a widget has changed.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[screen]{The previous @class{gdk:screen} object, or @code{nil} if
          the widget was not associated with a screen before.}
      @end{table}
    @subheading{The \"scroll-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when a button in the 4 to 7 range is pressed. Wheel mice are
      usually configured to generate button press events for buttons 4 and 5
      when the wheel is turned. To receive this signal, the @class{gdk:window}
      object associated to the widget needs to enable the
      @code{:button-press-mask} mask of the @symbol{gdk:event-mask} flags. This
      signal will be sent to the grab widget if there is one.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-scroll} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"selection-clear-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the the GDK window of the widget has lost ownership of a
      selection.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-selection} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"selection-get\" signal}
      @begin{pre}
lambda (widget data info time)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[data]{The @class{gtk:selection-data} instance.}
        @entry[info]{An unsigned integer with the info that has been registered
          with the target.}
        @entry[time]{An unsigned integer with the timestamp at which the data
          was requested.}
      @end{table}
    @subheading{The \"selection-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-selection} event.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"selection-received\" signal}
      @begin{pre}
lambda (widget data time)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[data]{The @class{gtk:selection-data} instance.}
        @entry[time]{An unsigned integer with the timestamp.}
      @end{table}
    @subheading{The \"selection-request-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when another client requests ownership of the selection owned by
      the GDK window of the widget.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-selection} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"show\" signal}
      @begin{pre}
lambda (widget)
      @end{pre}
      Emitted when the widget is shown, for example with the
      @fun{gtk:widget-show} function.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"show-help\" signal}
      @begin{pre}
lambda (widget help)    :action
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[help]{A value of the @symbol{gtk:widget-help-type} enumeration.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked
          for the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"size-allocate\" signal}
      @begin{pre}
lambda (widget allocation)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[allocation]{A @class{gdk:rectangle} instance with the region
          which has been allocated to the widget.}
      @end{table}
    @subheading{The \"state-changed\" signal}
      @begin{pre}
lambda (widget state)    :run-first
      @end{pre}
      Emitted when the widget state changes. @br{}
      @em{Warning:} The @code{\"state-changed\"} signal is deprecated since
      version 3.0 and should not be used in newly written code. Use the
      @code{\"state-flags-changed\"} signal instead.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[state]{The previous @symbol{gtk:state-type} state.}
      @end{table}
    @subheading{The \"state-flags-changed\" signal}
      @begin{pre}
lambda (widget flags)    :run-first
      @end{pre}
      Emitted when the widget state changes.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[flags]{The previous @symbol{gtk:state-flags} state flags.}
      @end{table}
    @subheading{The \"style-set\" signal}
      @begin{pre}
lambda (widget style)    :run-first
      @end{pre}
      Emitted when a new style has been set on a widget. @br{}
      @em{Warning:} The @code{\"style-set\"} signal has been deprecated since
      version 3.0 and should not be used in newly written code. Use the
      @code{\"style-updated\"} signal.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[style]{The previous @code{GtkStyle} style, or @code{nil} if the
          widget just got its initial style.}
      @end{table}
    @subheading{The \"style-updated\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      Emitted when the @class{gtk:style-context} object of a widget is changed.
      Note that style-modifying functions like the
      @fun{gtk:widget-override-color} function also cause this signal to be
      emitted.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"touch-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[event]{The @class{gdk:event} event.}
      @end{table}
    @subheading{The \"unmap\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      Emitted when the widget is going to be unmapped, which means that either
      it or any of its parents up to the toplevel widget have been set as
      hidden. As the  @code{\"unmap\"} signal indicates that a widget will not
      be shown any longer, it can be used to, for example, stop an animation on
      the widget.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"unmap-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the GDK window of the widget is unmapped. A window is
      unmapped when it becomes invisible on the screen. To receive this signal,
      the @class{gdk:window} object associated to the widget needs to enable the
      @code{:structure-mask} mask of the @symbol{gdk:event-mask} flags. GDK will
      enable this mask automatically for all new windows.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"unrealize\" signal}
      @begin{pre}
lambda (widget)    :run-last
      @end{pre}
      Emitted when the @class{gdk:window} object associated with the widget
      is destroyed, which means that the @fun{gtk:widget-unrealize} function has
      been called or the widget has been unmapped, that is, it is going to be
      hidden.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
    @subheading{The \"visibility-notify-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the GDK window of the widget is obscured or unobscured. To
      receive this signal the @class{gdk:window} object associated to the widget
      needs to enable the @code{:visibility-notify-mask} mask of the
      @symbol{gdk:event-mask} flags. @br{}
      @em{Warning:} The @code{\"visibility-notify-event\"} signal has been
      deprecated since version 3.12 and should not be used in newly written
      code. Modern composited windowing systems with pervasive transparency make
      it impossible to track the visibility of a window reliably, so this signal
      can not be guaranteed to provide useful information.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-visibility} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
    @subheading{The \"window-state-event\" signal}
      @begin{pre}
lambda (widget event)    :run-last
      @end{pre}
      Emitted when the state of the toplevel window associated to the widget
      changes. To receive this signal the @class{gdk:window} object associated
      to the widget needs to enable the @code{:structure-mask} mask of the
      @symbol{gdk:event-mask} flags. GDK will enable this mask automatically
      for all new windows.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[event]{The @class{gdk:event-window-state} event which triggered
          this signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:widget-app-paintable}
  @see-slot{gtk:widget-can-default}
  @see-slot{gtk:widget-can-focus}
  @see-slot{gtk:widget-composite-child}
  @see-slot{gtk:widget-double-buffered}
  @see-slot{gtk:widget-events}
  @see-slot{gtk:widget-expand}
  @see-slot{gtk:widget-focus-on-click}
  @see-slot{gtk:widget-halign}
  @see-slot{gtk:widget-has-default}
  @see-slot{gtk:widget-has-focus}
  @see-slot{gtk:widget-has-tooltip}
  @see-slot{gtk:widget-height-request}
  @see-slot{gtk:widget-hexpand}
  @see-slot{gtk:widget-hexpand-set}
  @see-slot{gtk:widget-is-focus}
  @see-slot{gtk:widget-margin}
  @see-slot{gtk:widget-margin-bottom}
  @see-slot{gtk:widget-margin-end}
  @see-slot{gtk:widget-margin-left}
  @see-slot{gtk:widget-margin-right}
  @see-slot{gtk:widget-margin-start}
  @see-slot{gtk:widget-margin-top}
  @see-slot{gtk:widget-name}
  @see-slot{gtk:widget-no-show-all}
  @see-slot{gtk:widget-opacity}
  @see-slot{gtk:widget-parent}
  @see-slot{gtk:widget-receives-default}
  @see-slot{gtk:widget-scale-factor}
  @see-slot{gtk:widget-sensitive}
  @see-slot{gtk:widget-style}
  @see-slot{gtk:widget-tooltip-markup}
  @see-slot{gtk:widget-tooltip-text}
  @see-slot{gtk:widget-valign}
  @see-slot{gtk:widget-vexpand}
  @see-slot{gtk:widget-vexpand-set}
  @see-slot{gtk:widget-visible}
  @see-slot{gtk:widget-width-request}
  @see-slot{gtk:widget-window}
  @see-class{gtk:container}
  @see-class{gtk:buildable}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:widget-app-paintable -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "app-paintable" 'widget) t)
 "The @code{app-paintable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the application will paint directly on the widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-app-paintable)
      "Accessor"
      (documentation 'widget-app-paintable 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-app-paintable object) => paintable}
  @syntax{(setf (gtk:widget-app-paintable object) paintable)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[paintable]{a boolean that is @em{true} if the application will
    paint on the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{app-paintable} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-app-paintable} function returns @em{true} if the widget
  will paint on the widget in a @code{\"draw\"} handler. The
  @setf{gtk:widget-app-paintable} function sets whether the application intends
  to draw on the widget.

  This is a hint to the widget and does not affect the behavior of the GTK
  core. Many widgets ignore this flag entirely. For widgets that do pay
  attention to the flag, such as @class{gtk:event-box} and @class{gtk:window}
  widgets, the effect is to suppress default themed drawing of the background
  of the widget. Children of the widget will still be drawn. The application is
  then entirely responsible for drawing the widget background.

  Note that the background is still drawn when the widget is mapped.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gtk:event-box}")

;;; --- gtk:widget-can-default -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-default" 'widget) t)
 "The @code{can-default} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget can be the default widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-can-default)
      "Accessor"
      (documentation 'widget-can-default 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-can-default object) => setting}
  @syntax{(setf (gtk:widget-can-default object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether or not the widget can be a default
    widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{can-default} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-can-default} function returns @em{true} if the widget can
  be a default widget, @em{false} otherwise. The @setf{gtk:widget-can-default}
  function specifies whether the widget can be a default widget.

  See the @fun{gtk:widget-grab-default} function for details about the meaning
  of \"default\".
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-default}")

;;; --- gtk:widget-can-focus ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-focus" 'widget) t)
 "The @code{can-focus} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget can accept the input focus. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-can-focus)
      "Accessor"
      (documentation 'widget-can-focus 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-can-focus object) => setting}
  @syntax{(setf (gtk:widget-can-focus object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether or not @arg{widget} can own the input
    focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{can-focus} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-can-focus} function returns @em{true} if the widget can
  own the input focus, @em{false} otherwise. The @setf{gtk:widget-can-focus}
  function sets whether the widget can own the input focus.

  See the @fun{gtk:widget-grab-focus} function for actually setting the input
  focus on a widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}")

;;; --- gtk:widget-composite-child ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "composite-child" 'widget) t)
 "The @code{composite-child} property of type @code{:boolean} (Read) @br{}
  Whether the widget is part of a composite widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-composite-child)
      "Accessor"
      (documentation 'widget-composite-child 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-composite-child object) => setting}
  @syntax{(setf (gtk:widget-composite-child object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is part of a composite
    widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{composite-child} slot of the
    @class{gtk:widget} class.
  @end{short}
  Whether the widget is part of a composite widget.
  @see-class{gtk:widget}")

;;; --- gtk:widget-double-buffered ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "double-buffered" 'widget) t)
 "The @code{double-buffered} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the widget is double buffered. @br{}
  @em{Warning:} The @code{double-buffered} property has been deprecated since
  version 3.14 and should not be used in newly written code. Widgets should
  not use this property. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-double-buffered)
      "Accessor"
      (documentation 'widget-double-buffered 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-double-buffered object) => setting}
  @syntax{(setf (gtk:widget-double-buffered object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is double buffered}
  @begin{short}
    Accessor of the @slot[gtk:widget]{double-buffered} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-double-buffered} function returns @em{true} if the widget
  is double buffered. You can use the @setf{gtk:widget-double-buffered} function
  to turn off the buffering. Widgets are double buffered by default.
  @begin[Warning]{dictionary}
    The @slot[gtk:widget]{double-buffered} property has been deprecated since
    version 3.14 and should not be used in newly written code. This function
    does not work under non-X11 backends or with non-native windows. Widgets
    should not use this property.
  @end{dictionary}
  @see-class{gtk:widget}")

;;; --- gtk:widget-events ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "events" 'widget) t)
 "The @code{events} property of type @symbol{gdk:event-mask} (Read / Write)@br{}
  The event mask that decides what kind of @class{gdk:event} events this widget
  gets. @br{}
  Default value: @code{:structure-mask}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-events)
      "Accessor"
      (documentation 'widget-events 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-events object) => events}
  @syntax{(setf (gtk:widget-events object) events)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[events]{a @symbol{gdk:event-mask} event mask}
  @begin{short}
    Accessor of the @slot[gtk:widget]{events} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-events} function gets the event mask for the widget. The
  @setf{gtk:widget-events} function sets the event mask. The event mask is a
  bitfield containing flags from the @symbol{gdk:event-mask} flags. These are
  the events that the widget will receive.

  Keep in mind that different widgets have different default event masks, and
  by changing the event mask you may disrupt the functionality of a widget, so
  be careful. This function must be called while a widget is unrealized.
  Consider the @fun{gtk:widget-add-events} function for widgets that are already
  realized, or if you want to preserve the existing event mask. This function
  cannot be used with widgets that have no window. To get events on those
  widgets, place them inside a @class{gtk:event-box} widget and receive events
  on the event box.
  @begin[Notes]{dictionary}
    Internally, the widget event mask will be the logical @code{OR} of the
    event mask set through the @setf{gtk:widget-events} or
    @fun{gtk:widget-add-events} functions , and the event mask necessary to
    cater for every @class{gtk:event-controller} object created for the widget.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:event-box}
  @see-class{gtk:event-controller}
  @see-symbol{gdk:event-mask}
  @see-function{gtk:widget-add-events}")

;;; --- gtk:widget-expand ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expand" 'widget) t)
 "The @code{expand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand in both directions. Setting the @code{expand} property sets
  both @code{hexpand} and @code{vexpand} properties. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-expand)
      "Accessor"
      (documentation 'widget-expand 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-expand object) => setting}
  @syntax{(setf (gtk:widget-expand object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to expand in both directions}
  @begin{short}
    Accessor of the @slot[gtk:widget]{expand} slot of the @class{gtk:widget}
    class.
  @end{short}
  Whether to expand the widget in both directions. Setting the @code{expand}
  property sets both the @slot[gtk:widget]{hexpand} and
  @slot[gtk:widget]{vexpand} properties.
  @see-class{gtk:widget}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand}")

;;; --- gtk:widget-focus-on-click ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-click" 'widget) t)
 "The @code{focus-on-click} property of type @code{:boolean} (Read / Write)@br{}
  Whether the widget should grab focus when it is clicked with the mouse. This
  property is only relevant for widgets that can take focus. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-focus-on-click)
      "Accessor"
      (documentation 'widget-focus-on-click 'function)
 "@version{#2023-3-13}
  @syntax{(gtk:widget-focus-on-click object) => setting}
  @syntax{(setf (gtk:widget-focus-on-click object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget should grab focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{focus-on-click} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-focus-on-click} function returns @em{true} if the widget
  should grab focus when it is clicked with the mouse. The
  @setf{gtk:widget-focus-on-click} function sets whether the widget should grab
  focus.

  Making mouse clicks not grab focus is useful in places like toolbars where you
  do not want the keyboard focus removed from the main area of the application.
  @see-class{gtk:widget}")

;;; --- gtk:widget-halign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "halign" 'widget) t)
 "The @code{halign} property of type @symbol{gtk:align} (Read / Write) @br{}
  How to distribute horizontal space if the widget gets extra space. @br{}
  Default value: @code{:fill}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-halign)
      "Accessor"
      (documentation 'widget-halign 'function)
 "@version{2024-1-1}
  @syntax{(gtk:widget-halign object) => align}
  @syntax{(setf (gtk:widget-halign object) align)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[align]{a @symbol{gtk:align} value}
  @begin{short}
    Accessor of the @slot[gtk:widget]{halign} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-halign} function returns the horizontal alignment of the
  widget. The @setf{gtk:widget-halign} function sets the horizontal alignment.
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-valign}")

;;; --- gtk:widget-has-default -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-default" 'widget) t)
 "The @code{has-default} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget is the default widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-default)
      "Accessor"
      (documentation 'widget-has-default 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-has-default object) => setting}
  @syntax{(setf (gtk:widget-has-default object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is the default widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-default} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-has-default} function returns @em{true} if the widget is
  the current default widget within its toplevel, @em{false} otherwise. The
  @setf{gtk:widget-has-default} function sets whether the widget is the default
  widget.

  See the @fun{gtk:widget-can-default} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-can-default}")

;;; --- gtk:widget-has-focus ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-focus" 'widget) t)
 "The @code{has-focus} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget has the input focus. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-focus)
      "Accessor"
      (documentation 'widget-has-focus 'function)
 "@version{#2023-3-29}
  @syntax{(gtk:widget-has-focus object) => setting}
  @syntax{(setf (gtk:widget-has-focus object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget has the input focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-focus} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-has-focus} function returns @em{true} if the widget has
  the global input focus. The @setf{gtk:widget-has-focus} function sets whether
  the widget has the input focus.

  See the @fun{gtk:widget-is-focus} function for the difference between having
  the global input focus, and only having the focus within a toplevel.
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-focus}")

;;; --- gtk:widget-has-tooltip -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-tooltip" 'widget) t)
 "The @code{has-tooltip} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the emission of the @code{\"query-tooltip\"} signal on the
  widget. A @em{true} value indicates that the widget can have a tooltip, in
  this case the widget will be queried using the @code{\"query-tooltip\"} signal
  to determine whether it will provide a tooltip or not. Note that setting this
  property to @em{true} for the first time will change the event masks of the
  @class{gdk:window} objects of this widget to include \"leave-notify\" and
  \"motion-notify\" events. This cannot and will not be undone when the property
  is set to @em{false} again. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-tooltip)
      "Accessor"
      (documentation 'widget-has-tooltip 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-has-tooltip object) => setting}
  @syntax{(setf (gtk:widget-has-tooltip object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the emission of the
    @code{\"query-toolip\"} signal is enabled or disabled}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-tooltip} slot of the
    @class{gtk:widget} class.
  @end{short}
  Enables or disables the emission of the @code{\"query-tooltip\"} signal on the
  widget. A @em{true} value indicates that the widget can have a tooltip, in
  this case the widget will be queried using the @code{\"query-tooltip\"} signal
  to determine whether it will provide a tooltip or not. Note that setting this
  property to @em{true} for the first time will change the event masks of the
  @class{gdk:window} objects of this widget to include \"leave-notify\" and
  \"motion-notify\" events. This cannot and will not be undone when the property
  is set to @em{false} again.
  @see-class{gtk:widget}
  @see-class{gdk:window}")

;;; --- gtk:widget-height-request ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height-request" 'widget) t)
 "The @code{height-request} property of type @code{:int} (Read / Write) @br{}
  Override for height request of the widget, or -1 if natural request should be
  used. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-height-request)
      "Accessor"
      (documentation 'widget-height-request 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-height-request object) => height}
  @syntax{(setf (gtk:widget-height-request object) height)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[height]{an integer with the height request}
  @begin{short}
    Accessor of the @slot[gtk:widget]{height-request} slot of the
    @class{gtk:widget} class.
  @end{short}
  See the @fun{gtk:widget-size-request} function or details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-width-request}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-hexpand -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hexpand" 'widget) t)
 "The @code{hexpand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand horizontally. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-hexpand)
      "Accessor"
      (documentation 'widget-hexpand 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-hexpand object) => setting}
  @syntax{(setf (gtk:widget-hexpand object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to expand horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{hexpand} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-hexpand} function gets whether the widget would like any
  available extra horizontal space. This function only looks at the own
  @slot[gtk:widget]{hexpand} flag of the widget, rather than computing whether
  the entire widget tree rooted at this widget wants to expand.

  The @setf{gtk:widget-hexpand} function sets whether the widget would like any
  available extra horizontal space. Call this function to set the expand flag if
  you would like your widget to become larger horizontally when the window has
  extra room.

  By default, widgets automatically expand if any of their children want to
  expand. To see if a widget will automatically expand given its current
  children and state, call the @fun{gtk:widget-compute-expand} function. A
  container can decide how the expandability of children affects the expansion
  of the container by overriding the @code{compute_expand} virtual method on the
  @class{gtk:widget} class.

  Setting the @slot[gtk:widget]{hexpand} property explicitly with this function
  will override the automatic expand behavior. This function forces the widget
  to expand or not to expand, regardless of children. The override occurs
  because the @fun{gtk:widget-hexpand} function sets the
  @slot[gtk:widget]{hexpand-set} property, which causes the
  @slot[gtk:widget]{hexpand} property of the widget to be used, rather than
  looking at children and widget state.
  @see-class{gtk:widget}
  @see-function{gtk:widget-expand}
  @see-function{gtk:widget-vexpand}
  @see-function{gtk:widget-hexpand-set}
  @see-function{gtk:widget-compute-expand}")

;;; --- gtk:widget-hexpand-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hexpand-set" 'widget) t)
 "The @code{hexpand-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use the @code{hexpand} property. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-hexpand-set)
      "Accessor"
      (documentation 'widget-hexpand-set 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-hexpand-set object) => setting}
  @syntax{(setf (gtk:widget-hexpand-set object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to use the @code{hexpand} property}
  @begin{short}
    Accessor of the @slot[gtk:widget]{hexpand-set} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-hexpand-set} function gets whether the
  @fun{gtk:widget-hexpand} function has been used to explicitly set the expand
  flag on this widget.

  The @slot[gtk:widget]{hexpand-set} property will be set automatically when
  you call the @fun{gtk:widget-hexpand} function to set the
  @slot[gtk:widget]{hexpand} property, so the most likely reason to use the
  @setf{gtk:widget-hexpand-set} function would be to unset an explicit expand
  flag.

  If the @slot[gtk:widget]{hexpand} property is set, then it overrides any
  computed expand value based on child widgets. If the
  @slot[gtk:widget]{hexpand} property is not set, then the expand value depends
  on whether any children of the widget would like to expand. There are few
  reasons to use this function, but it is here for completeness and consistency.
  @see-class{gtk:widget}
  @see-function{gtk:widget-hexpand}")

;;; --- gtk:widget-is-focus ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-focus" 'widget) t)
 "The @code{is-focus} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget is the focus widget within the toplevel. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-is-focus)
      "Accessor"
      (documentation 'widget-is-focus 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-is-focus object) => setting}
  @syntax{(setf (gtk:widget-is-focus object) setting)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is the focus widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{is-focus} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-is-focus} function determines if the widget is the focus
  widget within its toplevel. The @setf{gtk:widget-is-focus} function sets the
  property.

  This does not mean that the @slot[gtk:widget]{has-focus} property is
  necessarily set. The @slot[gtk:widget]{has-focus} property will only be set
  if the toplevel widget additionally has the global input focus.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-focus}")

;;; --- gtk:widget-margin ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin" 'widget) t)
 "The @code{margin} property of type @code{:int} (Read / Write) @br{}
  Sets the margin of all four sides at once. If read, returns the maximum margin
  on any side. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin)
      "Accessor"
      (documentation 'widget-margin 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin object) => margin}
  @syntax{(setf (gtk:widget-margin object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin} slot of the @class{gtk:widget}
    class.
  @end{short}
  Sets the margin of all four sides at once. If read, returns the maximum
  margin on any side.
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin-start}
  @see-function{gtk:widget-margin-end}
  @see-function{gtk:widget-margin-top}
  @see-function{gtk:widget-margin-bottom}")

;;; --- gtk:widget-margin-bottom -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-bottom" 'widget) t)
 "The @code{margin-bottom} property of type @code{:int} (Read / Write) @br{}
  Margin on bottom side of the widget. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-bottom)
      "Accessor"
      (documentation 'widget-margin-bottom 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-bottom object) => margin}
  @syntax{(setf (gtk:widget-margin-bottom object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on bottom side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-bottom} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-bottom} function gets the bottom marging of the
  widget. The @setf{gtk:widget-margin-bottom} function sets the bottom margin.

  This property adds margin outside of the normal size request of the widget.
  The margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-end --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-end" 'widget) t)
 "The @code{margin-end} property of type @code{:int} (Read / Write) @br{}
  Margin on end of the widget, horizontally. This property supports
  left-to-right text directions. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-end)
      "Accessor"
      (documentation 'widget-margin-end 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-end object) => margin}
  @syntax{(setf (gtk:widget-margin-end object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on end of the widget,
    horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-end} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-end} function gets the value of the end margin of
  the widget. The @setf{gtk:widget-margin-end} function sets the end margin.

  This property supports left-to-right text directions. This property adds
  margin outside of the normal size request of the widget. The margin will be
  added in addition to the size from the @fun{gtk:widget-size-request} function
  for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-left -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-left" 'widget) t)
 "The @code{margin-left} property of type @code{:int} (Read / Write) @br{}
  Margin on left side of the widget. @br{}
  @em{Warning:} The @code{margin-left} property has been deprecated since
  version 3.12 and should not be used in newly written code. Use the
  @code{margin-start} property instead. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-left)
      "Accessor"
      (documentation 'widget-margin-left 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-left object) => margin}
  @syntax{(setf (gtk:widget-margin-left object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on left side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-left} slot of the
    @class{gtk:widget} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-margin-left} function has been deprecated since version
    3.12 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-start} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-margin-start}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-right ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-right" 'widget) t)
 "The @code{margin-right} property of type @code{:int} (Read / Write) @br{}
  Margin on right side of the widget. @br{}
  @em{Warning:} The @code{margin-right} property has been deprecated since
  version 3.12 and should not be used in newly written code. Use the
  @code{margin-end} property instead. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-right)
      "Accessor"
      (documentation 'widget-margin-right 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-right object) => margin}
  @syntax{(setf (gtk:widget-margin-right object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on right side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-right} slot of the
    @class{gtk:widget} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-margin-right} function has been deprecated since
    version 3.12 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-end} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-margin-end}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-start ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-start" 'widget) t)
 "The @code{margin-start} property of type @code{:int} (Read / Write) @br{}
  Margin on start of the widget, horizontally. This property supports
  left-to-right and right-to-left text directions. This property adds margin
  outside of the normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-start)
      "Accessor"
      (documentation 'widget-margin-start 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-start object) => margin}
  @syntax{(setf (gtk:widget-margin-start object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on start of the widget,
    horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-start} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-start} function returns the start margin of the
  widget. The @setf{gtk:widget-margin-start} function sets the start margin.

  This property supports left-to-right and right-to-left text directions. This
  property adds margin outside of the normal size request of the widget. The
  margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-top --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-top" 'widget) t)
 "The @code{margin-top} property of type @code{:int} (Read / Write) @br{}
  Margin on top side of the widget. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-top)
      "Accessor"
      (documentation 'widget-margin-top 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-margin-top object) => margin}
  @syntax{(setf (gtk:widget-margin-top object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer with the margin on top side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-top} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-top} function returns the top margin of widget. The
  @setf{gtk:widget-margin-top} function sets the top margin.

  This property adds margin outside of the normal size request of the widget.
  The margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-margin}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'widget) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  The name of the widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-name)
      "Accessor"
      (documentation 'widget-name 'function)
 "@version{2024-1-2}
  @syntax{(gtk:widget-name object) => name}
  @syntax{(setf (gtk:widget-name object) name)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[name]{a string with the name of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{name} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-name} function retrieves the name of a widget. The
  @setf{gtk:widget-name} function sets the name.

  Widgets can be named, which allows you to refer to them from a CSS file. You
  can apply a style to widgets with a particular name in the CSS file. Note
  that the CSS syntax has certain special characters to delimit and represent
  elements in a selector (period, #, >, *...), so using these will make your
  widget impossible to match by name. Any combination of alphanumeric symbols,
  dashes and underscores will suffice.
  @see-class{gtk:widget}
  @see-class{gtk:style-context}")

;;; --- gtk:widget-no-show-all -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "no-show-all" 'widget) t)
 "The @code{no-show-all} property of type @code{:boolean} (Read / Write) @br{}
  Whether the @fun{gtk:widget-show-all} function should not affect this widget.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-no-show-all)
      "Accessor"
      (documentation 'widget-no-show-all 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-no-show-all object) => setting}
  @syntax{(setf (gtk:widget-no-show-all object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the @fun{gtk:widget-show-all}
    function should not affect this widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{no-show-all} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @slot[gtk:widget]{no-show-all} property determines whether calls
  to the @fun{gtk:widget-show-all} function will affect this widget. This is
  mostly for use in constructing widget hierarchies with externally controlled
  visibility, see the @class{gtk:ui-manager} class.
  @see-class{gtk:widget}
  @see-class{gtk:ui-manager}
  @see-function{gtk:widget-show-all}")

;;; --- gtk:widget-opacity -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "opacity" 'widget) t)
 "The @code{opacity} property of type @code{:double} (Read / Write) @br{}
  The requested opacity of the widget. @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-opacity)
      "Accessor"
      (documentation 'widget-opacity 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-opacity object) => opacity}
  @syntax{(setf (gtk:widget-opacity object) opacity)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[opacity]{a double float with the opacity of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{opacity} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-opacity} function fetches the requested opacity for the
  widget. The @setf{gtk:widget-opacity} function request the widget to be
  rendered partially transparent, with opacity 0.0 being fully transparent and
  1.0 fully opaque.

  Opacity values are clamped to the [0.0,1.0] range. This works on both toplevel
  widget, and child widgets, although there are some limitations:
  @begin{itemize}
    @begin{item}
      For toplevel widgets this depends on the capabilities of the windowing
      system. On X11 this has any effect only on X screens with a compositing
      manager running. See the @fun{gtk:widget-is-composited} function. On
      Windows it should work always, although setting the opacity of the window
      after the window has been shown causes it to flicker once on Windows.
    @end{item}
    @begin{item}
      For child widgets it does not work if any affected widget has a native
      window, or disables double buffering.
    @end{item}
  @end{itemize}
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-composited}")

;;; --- gtk:widget-parent ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parent" 'widget) t)
 "The @code{parent} property of type @class{gtk:container} (Read / Write) @br{}
  The parent widget of this widget, which must be a container.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-parent)
      "Accessor"
      (documentation 'widget-parent 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-parent object) => parent}
  @syntax{(setf (gtk:widget-parent object) parent)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[parent]{a @class{gtk:widget} parent container}
  @begin{short}
    Accessor of the @slot[gtk:widget]{parent} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-parent} function returns the parent container of the
  widget. The @setf{gtk:widget-parent} function sets the container as the
  parent of the widget, and takes care of some details such as updating the
  state and style of the child to reflect its new location. The opposite is the
  @fun{gtk:widget-unparent} function.

  This function is useful only when implementing subclasses of the
  @class{gtk:container} class.
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-function{gtk:widget-unparent}")

;;; --- gtk:widget-receives-default --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "receives-default" 'widget) t)
 "The @code{receives-default} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, the widget will receive the default action when it is focused.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-receives-default)
      "Accessor"
      (documentation 'widget-receives-default 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-receives-default object) => setting}
  @syntax{(setf (gtk:widget-receives-default object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget will receive the default
    action}
  @begin{short}
    Accessor of the @slot[gtk:widget]{receives-default} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-receives-default} function determines whether the widget
  is alyways treated as default widget within its toplevel when it has the
  focus, even if another widget is the default. The
  @setf{gtk:widget-receives-default} function specifies whether the widget will
  be treated as the default widget.

  See the @fun{gtk:widget-grab-default} function for details about the meaning
  of \"default\".
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-default}")

;;; --- gtk:widget-scale-factor ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-factor" 'widget) t)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor of the widget. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-scale-factor)
      "Accessor"
      (documentation 'widget-scale-factor 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-scale-factor object) => scale}
  @argument[object]{a @class{gtk:widget} object}
  @argument[scale]{an integer with the scale factor}
  @begin{short}
    The @fun{gtk:widget-scale-factor} function retrieves the internal scale
    factor that maps from window coordinates to the actual device pixels.
  @end{short}
  On traditional systems this is 1, on high density outputs, it can be a higher
  value (typically 2).
  @see-class{gtk:widget}
  @see-function{gdk:window-scale-factor}")

;;; --- gtk:widget-sensitive ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sensitive" 'widget) t)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget responds to input. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-sensitive)
      "Accessor"
      (documentation 'widget-sensitive 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-sensitive object) => setting}
  @syntax{(setf (gtk:widget-sensitive object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget responds to input}
  @begin{short}
    Accessor of the @slot[gtk:widget]{sensitive} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-sensitive} function returns the sensitivity of the widget.
  The @setf{gtk:widget-sensitive} function sets the sensitivity.

  A widget is sensitive if the user can interact with it. Insensitive widgets
  are \"grayed out\" and the user cannot interact with them. Insensitive widgets
  are known as \"inactive\", \"disabled\", or \"ghosted\" in some other
  toolkits.

  The effective sensitivity of a widget is however determined by both its own
  and its parent sensitivity of the widget. See the
  @fun{gtk:widget-is-sensitive} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-sensitive}")

;;; --- gtk:widget-style -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style" 'widget) t)
 "The @code{style} property of type @code{GtkStyle} (Read / Write) @br{}
  The style of the widget, which contains information about how it will look.
  @br{}
  @em{Warning:} The @code{style} property is deprecated since version 3.0 and
  should not be used in newly written code. Use the @class{gtk:style-context}
  class instead.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-style)
      "Accessor"
      (documentation 'widget-style 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-style object) => style}
  @syntax{(setf (gtk:widget-style object) style)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[style]{a deprecated @code{GtkStyle} object}
  @begin{short}
    Accessor of the @slot[gtk:widget]{style} slot of the @class{gtk:widget}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-style} object has been deprecated since version 3.0 and
    should not be used in newly written code. Use the
    @class{gtk:style-context} class instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}")

;;; --- gtk:widget-tooltip-markup ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-markup" 'widget) t)
 "The @code{tooltip-markup} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string, which is marked up with
  the Pango text markup language. This is a convenience property which will take
  care of getting the tooltip shown if the given string is not @code{nil}. The
  @slot[gtk:widget]{has-tooltip} property will automatically be set to @em{true}
  and there will be taken care of the @code{\"query-tooltip\"} signal in the
  default signal handler. Note that if both the @slot[gtk:widget]{tooltip-text}
  and @slot[gtk:widget]{tooltip-markup} properties are set, the last one wins.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-tooltip-markup)
      "Accessor"
      (documentation 'widget-tooltip-markup 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-tooltip-markup object) => markup}
  @syntax{(setf (gtk:widget-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[markup]{a string with the text of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:widget]{tooltip-markup} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-tooltip-markup} function gets the contents of the tooltip.
  The @setf{gtk:widget-tooltip-markup} function sets @arg{markup} as the
  contents of the tooltip, which is marked up with the Pango text markup
  language.

  This function will take care of setting the @slot[gtk:widget]{has-tooltip}
  property to @em{true} and of the default handler for the
  @code{\"query-tooltip\"} signal.

  See also the @slot[gtk:widget]{tooltip-markup} property and the
  @fun{gtk:tooltip-set-markup} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-toolip}
  @see-function{gtk:tooltip-set-markup}")

;;; --- gtk:widget-tooltip-text ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-text" 'widget) t)
 "The @code{tooltip-text} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string. This is a convenience
  property which will take care of getting the tooltip shown if the given string
  is not @code{nil}. The @code{has-tooltip} property will automatically be set
  to @em{true} and there will be taken care of the @code{\"query-tooltip\"}
  signal in the default signal handler. Note that if both the
  @slot[gtk:widget]{tooltip-text} and @slot[gtk:widget]{tooltip-markup}
  properties are set, the last one wins. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-tooltip-text)
      "Accessor"
      (documentation 'widget-tooltip-text 'function)
 "@version{2023-12-30}
  @syntax{(gtk:widget-tooltip-text object) => text}
  @syntax{(setf (gtk:widget-tooltip-text object) text)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[text]{a string with the text of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:widget]{tooltip-text} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-tooltip-text} function gets the contents of the tooltip.
  The @setf{gtk:widget-tooltip-text} function sets @arg{text} as the contents
  of the tooltip.

  This function will take care of setting the @slot[gtk:widget]{has-tooltip}
  property to @em{true} and of the default handler for the
  @code{\"query-tooltip\"} signal.

  See also the @slot[gtk:widget]{tooltip-text} property and the
  @fun{gtk:tooltip-set-text} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-tooltip}
  @see-function{gtk:tooltip-set-text}")

;;; --- gtk:widget-valign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "valign" 'widget) t)
 "The @code{valign} property of type @symbol{gtk:align} (Read / Write) @br{}
  How to distribute vertical space if the widget gets extra space. @br{}
  Default value: @code{:fill}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-valign)
      "Accessor"
      (documentation 'widget-valign 'function)
 "@version{2024-1-1}
  @syntax{(gtk:widget-valign object) => align}
  @syntax{(setf (gtk:widget-valign object) align)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[align]{a @symbol{gtk:align} value}
  @begin{short}
    Accessor of the @slot[gtk:widget]{valign} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-valign} function gets the vertical alignment of the
  widget. The @setf{gtk:widget-valign} function sets the vertical alignment.
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-halign}")

;;; --- gtk:widget-vexpand -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vexpand" 'widget) t)
 "The @code{vexpand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand vertically. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-vexpand)
      "Accessor"
      (documentation 'widget-vexpand 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-vexpand object) => setting}
  @syntax{(setf (gtk:widget-vexpand object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to expand vertically}
  @begin{short}
    Accessor of the @slot[gtk:widget]{vexpand} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-vexpand} function gets whether the widget would like any
  available extra vertical space. The @setf{gtk:widget-vexpand} function sets
  whether the widget would like any available extra vertical space.

  See the @fun{gtk:widget-hexpand} function for more detail.
  @see-class{gtk:widget}
  @see-function{gtk:widget-expand}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand-set}")

;;; --- gtk:widget-vexpand-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vexpand-set" 'widget) t)
 "The @code{vexpand-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use the @code{vexpand} property. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-vexpand-set)
      "Accessor"
      (documentation 'widget-vexpand-set 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-vexpand-set object) => setting}
  @syntax{(setf (gtk:widget-vexpand-set object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to use the @slot[gtk:widget]{vexpand}
    property}
  @begin{short}
    Accessor of the @slot[gtk:widget]{vexpand-set} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-vexpand-set} function gets whether the
  @setf{gtk:widget-vexpand} function has been used to explicitly set the expand
  flag on this widget. The @setf{gtk:widget-vexpand-set} function sets whether
  the @slot[gtk:widget]{vexpand} property will be used.

  See the @fun{gtk:widget-hexpand-set} function for more detail.
  @see-class{gtk:widget}
  @see-function{gtk:widget-vexpand}
  @see-function{gtk:widget-hexpand-set}")

;;; --- gtk:widget-visible -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'widget) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget is visible. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-visible)
      "Accessor"
      (documentation 'widget-visible 'function)
 "@version{2024-1-1}
  @syntax{(gtk:widget-visible object) => setting}
  @syntax{(setf (gtk:widget-visible object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is visible}
  @begin{short}
    Accessor of the @slot[gtk:widget]{visible} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-visible} function determines whether the widget is
  visible. The @setf{gtk:widget-visible} function sets the visibility state.
  Note that this does not take into account whether the parent of the widget is
  also visible or the widget is obscured in any way.

  This function simply calls the @fun{gtk:widget-show} or @fun{gtk:widget-hide}
  functions but is nicer to use when the visibility of the widget depends on
  some condition.
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-hide}")

;;; --- gtk:widget-width-request -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-request" 'widget) t)
 "The @code{width-request} property of type @code{:int} (Read / Write) @br{}
  Override for width request of the widget, or -1 if natural request should be
  used. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-width-request)
      "Accessor"
      (documentation 'widget-width-request 'function)
 "@version{#2023-3-8}
  @syntax{(gtk:widget-width-request object) => width}
  @syntax{(setf (gtk:widget-width-request object) width)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[width]{an integer with the width request}
  @begin{short}
    Accessor of the @slot[gtk:widget]{width-request} slot of the
    @class{gtk:widget} class.
  @end{short}
  See the @fun{gtk:widget-size-request} function for details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-height-request}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-window ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window" 'widget) t)
 "The @code{window} property of type @class{gdk:window} (Read) @br{}
  The GDK window of the widget if it is realized, @code{nil} otherwise.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-window)
      "Accessor"
      (documentation 'widget-window 'function)
 "@version{2023-12-26}
  @syntax{(gtk:widget-window object) => window}
  @argument[object]{a @class{gtk:widget} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Accessor of the @slot[gtk:widget]{window} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-window} function returns the GDK window of the widget if
  it is realized, @code{nil} otherwise.
  @begin[Lisp implementation]{dictionary}
    The @slot[gtk:widget]{window} slot is only readable. The C library has the
    @code{gtk_widget_set_window()} function to set a GDK window. This function
    is only used in a \"realize\" implementation of a widget and not
    implemented in the Lisp library.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_new ()
;;; ----------------------------------------------------------------------------

(defun widget-new (gtype &rest args)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[gtype]{a @class{g:type-t} type ID of the widget to create}
  @argument[args]{pairs of the property keyword and value}
  @return{The new @class{gtk:widget} object of @arg{gtype} type.}
  @begin{short}
    This is a function for creating a widget and setting its properties in one
    go.
  @end{short}
  This function is equivalent to the @fun{g:object-new} function.
  @begin[Examples]{dictionary}
    Create a left-aligned label.
    @begin{pre}
(gtk:widget-new \"GtkLabel\" :label \"Hello World\" :xalign 0.0)
=> #<GTK:LABEL {1003142FD3@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{g:object-new}"
  (let ((lisp-type (glib:symbol-for-gtype gtype)))
    (apply 'make-instance lisp-type args)))

(export 'widget-new)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_destroy" widget-destroy) :void
 #+liber-documentation
 "@version{2023-12-29}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Destroys a widget.
  @end{short}
  When a widget is destroyed, it will break any references it holds to other
  objects. If the widget is inside a container, the widget will be removed from
  the container. If the widget is a toplevel, derived from the
  @class{gtk:window} class, it will be removed from the list of toplevels, and
  the reference GTK holds to it will be removed. Removing a widget from its
  container or the list of toplevels results in the widget being finalized.

  In most cases, only toplevel windows require explicit destruction,
  because when you destroy a toplevel its children will be destroyed as well.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gtk:widget-in-destruction}"
  (widget (g:object widget)))

(export 'widget-destroy)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_in_destruction ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_in_destruction" widget-in-destruction) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is being destroyed.}
  @begin{short}
    Returns whether the widget is currently being destroyed.
  @end{short}
  This information can sometimes be used to avoid doing unnecessary work.
  @see-class{gtk:widget}
  @see-function{gtk:widget-destroy}"
  (widget (g:object widget)))

(export 'widget-in-destruction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_destroyed ()
;;;
;;; void gtk_widget_destroyed (GtkWidget *widget, GtkWidget **widget_pointer);
;;;
;;; This function sets *widget_pointer to NULL if widget_pointer != NULL. It's
;;; intended to be used as a callback connected to the "destroy" signal of a
;;; widget. You connect gtk_widget_destroyed() as a signal handler, and pass the
;;; address of your widget variable as user data. Then when the widget is
;;; destroyed, the variable will be set to NULL. Useful for example to avoid
;;; multiple copies of the same dialog.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; widget_pointer :
;;;     address of a variable that contains widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unparent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unparent" widget-unparent) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Should be called by implementations of the @code{remove} method on
    a @class{gtk:container} implementation, to dissociate a child from the
    container.
  @end{short}
  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-function{gtk:widget-parent}"
  (widget (g:object widget)))

(export 'widget-unparent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_show" widget-show) :void
 #+liber-documentation
 "@version{2023-12-30}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags a widget to be displayed.
  @end{short}
  Any widget that is not shown will not appear on the screen. If you want to
  show all the widgets in a container, it is easier to call the
  @fun{gtk:widget-show-all} function on the container, instead of individually
  showing the widgets. Remember that you have to show the containers containing
  a widget, in addition to the widget itself, before it will appear onscreen.

  When a toplevel container is shown, it is immediately realized and mapped.
  Other shown widgets are realized and mapped when their toplevel container is
  realized and mapped.
  @see-class{gtk:widget}
  @see-function{gtk:widget-hide}
  @see-function{gtk:widget-show-all}"
  (widget (g:object widget)))

(export 'widget-show)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show_now ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_show_now" widget-show-now) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Shows a widget.
  @end{short}
  If the widget is an unmapped toplevel widget, i.e. a @class{gtk:window}
  widget that has not yet been shown, enter the main loop and wait for the
  window to actually be mapped. Be careful, because the main loop is running,
  anything can happen during this function.
  @see-class{gtk:widget}
  @see-class{gtk:window}"
  (widget (g:object widget)))

(export 'widget-show-now)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_hide" widget-hide) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Reverses the effects of the @fun{gtk:widget-show} function, causing the
    widget to be hidden, so it is invisible to the user.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-show-all}"
  (widget (g:object widget)))

(export 'widget-hide)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show_all ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_show_all" widget-show-all) :void
 #+liber-documentation
 "@version{2023-7-7}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Recursively shows a widget, and any child widgets if the widget is a
    container.
  @end{short}
  See the @fun{gtk:widget-show} function for more information and the
  @fun{gtk:widget-hide} function to hide the widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-hide}"
  (widget (g:object widget)))

(export 'widget-show-all)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_map ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_map" widget-map) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be mapped if it is not already.
  @end{short}
  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-unmap}"
  (widget (g:object widget)))

(export 'widget-map)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unmap ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unmap" widget-unmap ) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be unmapped if it is currently mapped.
  @end{short}
  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-map}"
  (widget (g:object widget)))

(export 'widget-unmap)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_realize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_realize" widget-realize) :void
 #+liber-documentation
 "@version{2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Creates the GDK resources associated with a widget.
  @end{short}
  For example, the GDK window will be created when a widget is realized.
  Normally realization happens implicitly. If you show a widget and all its
  parent containers, then the widget will be realized and mapped automatically.

  Realizing a widget requires all the parent widgets of the widget to be
  realized. Calling the @fun{gtk:widget-realize} function realizes the parents
  of the widget in addition to the widget itself. If a widget is not yet
  inside a toplevel window when you realize it, bad things will happen.

  This function is primarily used in widget implementations, and is not very
  useful otherwise. Many times when you think you might need it, a better
  approach is to connect to a signal that will be called after the widget is
  realized automatically, such as the @code{\"draw\"} signal. Or simply use the
  @fun{g:signal-connect} function with the @code{\"realize\"} signal.
  @see-class{gtk:widget}
  @see-function{gtk:widget-unrealize}
  @see-function{g:signal-connect}"
  (widget (g:object widget)))

(export 'widget-realize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unrealize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unrealize" widget-unrealize) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be unrealized.
  @end{short}
  Frees all GDK resources associated with the widget This function is only
  useful in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-realize}"
  (widget (g:object widget)))

(export 'widget-unrealize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_draw ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_draw" widget-draw) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object to draw}
  @argument[cr]{a @symbol{cairo:context-t} Cairo context to draw to}
  @begin{short}
    Draws the widget to a Cairo Context.
  @end{short}
  The widget must be drawable, see the @fun{gtk:widget-is-drawable} function,
  and a size must have been allocated. The top left corner of the widget will
  be drawn to the currently set origin point of the Cairo context.

  You should pass a Cairo context as @arg{cr} argument that is in an original
  state. Otherwise the resulting drawing is undefined. For example changing the
  operator using the @fun{cairo:operator} function or the line width using the
  @fun{cairo:line-width} function might have unwanted side effects. You may
  however change the transform matrix of the Cairo context - like with the
  @fun{cairo:scale}, @fun{cairo:translate} or @fun{cairo:matrix} functions and
  clip region with the @fun{cairo:clip} function prior to calling this function.
  Also, it is fine to modify the Cairo context with the @fun{cairo:save} and
  @fun{cairo:push-group} functions  prior to calling this function.
  @begin[Notes]{dictionary}
    Special purpose widgets may contain special code for rendering to the
    screen and might appear differently on screen and when rendered using the
    @fun{gtk:widget-draw} function.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-symbol{cairo:context-t}
  @see-function{gtk:widget-is-drawable}
  @see-function{cairo:operator}
  @see-function{cairo:line-width}
  @see-function{cairo:scale}
  @see-function{cairo:translate}
  @see-function{cairo:matrix}
  @see-function{cairo:clip}
  @see-function{cairo:save}
  @see-function{cairo:push-group}"
  (widget (g:object widget))
  (cr (:pointer (:struct cairo:context-t))))

(export 'widget-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_draw" widget-queue-draw) :void
#+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Equivalent to calling the @fun{gtk:widget-queue-draw-area} function for the
    entire area of a widget.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-queue-draw-area}"
  (widget (g:object widget)))

(export 'widget-queue-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_resize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_resize" widget-queue-resize) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags a widget to have its size renegotiated.
  @end{short}
  Should be called when a widget for some reason has a new size request. For
  example, when you change the text in a label, the @class{gtk:label} widget
  queues a resize to ensure there is enough space for the new text.

  This function is only for use in widget implementations.
  @begin[Notes]{dictionary}
    You cannot call the @fun{gtk:widget-queue-resize} function on a widget from
    inside its implementation of the @code{size_allocate} virtual method. Calls
    to the @fun{gtk:widget-queue-resize} function from inside the
    @code{size_allocate} virtual method will be silently ignored.
  @end{dictionary}
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_resize_no_redraw ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_resize_no_redraw"
                widget-queue-resize-no-redraw) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    This function works like the @fun{gtk:widget-queue-resize} function, except
    that the widget is not invalidated.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-queue-resize}"
  (widget (g:object widget)))

(export 'widget-queue-resize-no-redraw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_allocate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_allocate" widget-queue-allocate) :void
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags the widget for a rerun of the @code{size_allocate} function.
  @end{short}
  Use this function instead of the @fun{gtk:widget-queue-resize} function when
  the  size request of the widget did not change but it wants to reposition its
  contents. An example user of this function is the @fun{gtk:widget-halign}
  function.

  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-queue-resize}
  @see-function{gtk:widget-halign}"
  (widget (g:object widget)))

(export 'widget-queue-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_frame_clock () -> widget-frame-clock
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_frame_clock" widget-frame-clock)
    (g:object gdk:frame-clock)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:frame-clock} object, or @code{nil} if @arg{widget} is
    unrealized.}
  @begin{short}
    Obtains the frame clock for a widget.
  @end{short}
  The frame clock is a global \"ticker\" that can be used to drive animations
  and repaints. The most common reason to get the frame clock is to call the
  @fun{gdk:frame-clock-frame-time} function, in order to get a time to use for
  animating. For example you might record the start of the animation with an
  initial value from the @fun{gdk:frame-clock-frame-time} function, and then
  update the animation by calling the @fun{gdk:frame-clock-frame-time} function
  again during each repaint.

  The @fun{gdk:frame-clock-request-phase} function will result in a new frame
  on the clock, but will not necessarily repaint any widgets. To repaint a
  widget, you have to use the @fun{gtk:widget-queue-draw} function which
  invalidates the widget, thus scheduling it to receive a draw on the next
  frame. The @fun{gtk:widget-queue-draw} function will also end up requesting a
  frame on the appropriate frame clock.

  A frame clock of the widget will not change while the widget is mapped.
  Reparenting a widget, which implies a temporary unmap, can change the frame
  clock of the widget.

  Unrealized widgets do not have a frame clock.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gdk:frame-clock-frame-time}
  @see-function{gdk:frame-clock-request-phase}
  @see-function{gtk:widget-queue-draw}"
  (widget (g:object widget)))

(export 'widget-frame-clock)

;;; ----------------------------------------------------------------------------
;;; GtkTickCallback ()
;;; ----------------------------------------------------------------------------

(cffi:defcallback tick-callback :boolean
    ((widget (g:object widget))
     (clock (g:object gdk:frame-clock))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func widget clock)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tick-callback)
      "Callback"
      (liber:symbol-documentation 'tick-callback)
 "@version{#2024-3-23}
  @syntax{lambda (widget clock) => result}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[clock]{a @class{gdk:frame-clock} object for the widget, same as
    calling the @fun{gtk:widget-frame-clock} function}
  @argument[result]{@em{true} if the tick callback function should continue to
    be called, @em{false} if the tick callback function should be removed}
  @begin{short}
    Callback type for adding a function to update animations.
  @end{short}
  See the @fun{gtk:widget-add-tick-callback} function.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gtk:widget-add-tick-callback}
  @see-function{gtk:widget-frame-clock}")

(export 'tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_tick_callback ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_tick_callback" %widget-add-tick-callback) :uint
  (widget (g:object widget))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun widget-add-tick-callback (widget func)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[func]{a @symbol{gtk:tick-callback} callback function to call for
    updating animations}
  @return{The unsigned integer ID for the connection of this callback function,
    remove the callback function by passing it to the
    @fun{gtk:widget-remove-tick-callback} function.}
  @begin{short}
    Queues an animation frame update and adds a callback function to be called
    before each frame.
  @end{short}
  Until the tick callback function is removed, it will be called frequently,
  usually at the frame rate of the output device or as quickly as the
  application can be repainted, whichever is slower. For this reason, it is most
  suitable for handling graphics that change every frame or every few frames.
  The tick callback function does not automatically imply a relayout or repaint.
  If you want a repaint or relayout, and are not changing widget properties that
  would trigger that, for example, changing the text of a @class{gtk:label}
  widget, then you will have to call the @fun{gtk:widget-queue-resize} function
  or the @fun{gtk:widget-queue-draw-area} function yourself.

  The @fun{gdk:frame-clock-frame-time} function should generally be used for
  timing continuous animations and the
  @fun{gdk:frame-timings-predicted-presentation-time} function if you are
  trying to display isolated frames at particular times.

  This is a more convenient alternative to connecting directly to the
  @code{\"update\"} signal of the @class{gdk:frame-clock} object, since you do
  not have to worry about when the @class{gdk:frame-clock} object is assigned
  to a widget.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gtk:widget-remove-tick-callback}
  @see-function{gtk:widget-queue-resize}
  @see-function{gtk:widget-queue-draw-area}
  @see-function{gdk:frame-clock-frame-time}
  @see-function{gdk:frame-timings-predicted-presentation-time}"
  (%widget-add-tick-callback widget
          (cffi:callback tick-callback)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'widget-add-tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_tick_callback ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_tick_callback" widget-remove-tick-callback)
    :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[id]{an unsigned integer with the ID returned by the
    @fun{gtk:widget-add-tick-callback} function}
  @begin{short}
    Removes a tick callback function previously registered with the
    @fun{gtk:widget-add-tick-callback} function.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-tick-callback}"
  (widget (g:object widget))
  (id :uint))

(export 'widget-remove-tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_request ()
;;; ----------------------------------------------------------------------------

;; deprecated since 3.0 and not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_requisition ()
;;;
;;; void gtk_widget_get_child_requisition (GtkWidget *widget,
;;;                                        GtkRequisition *requisition);
;;;
;;; Warning
;;;
;;; gtk_widget_get_child_requisition has been deprecated since version 3.0 and
;;; should not be used in newly written code.
;;; Use gtk_widget_get_preferred_size() instead.
;;;
;;; This function is only for use in widget implementations. Obtains
;;; widget->requisition, unless someone has forced a particular geometry on the
;;; widget (e.g. with gtk_widget_set_size_request()), in which case it returns
;;; that geometry instead of the widget's requisition.
;;;
;;; This function differs from gtk_widget_size_request() in that it retrieves
;;; the last size request value from widget->requisition, while
;;; gtk_widget_size_request() actually calls the "size_request" method on widget
;;; to compute the size request and fill in widget->requisition, and only then
;;; returns widget->requisition.
;;;
;;; Because this function does not call the "size_request" method, it can only
;;; be used when you know that widget->requisition is up-to-date, that is,
;;; gtk_widget_size_request() has been called since the last time a resize was
;;; queued. In general, only container implementations have this information;
;;; applications should use gtk_widget_size_request().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; requisition :
;;;     a GtkRequisition to be filled in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_allocate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_size_allocate" widget-size-allocate) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[allocation]{a @class{gdk:rectangle} instance with the position and
    size to be allocated to the widget}
  @begin{short}
    This function is only used by @class{gtk:container} subclasses, to assign a
    size and position to their child widgets.
  @end{short}
  In this function, the allocation may be adjusted. It will be forced to a 1 x 1
  minimum size, and the @code{adjust_size_allocation()} virtual method on the
  child will be used to adjust the allocation. Standard adjustments include
  removing the margins of the widget, and applying the @slot[gtk:widget]{halign}
  and @slot[gtk:widget]{valign} properties of the widget.
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}"
  (widget (g:object widget))
  (allocation (g:boxed gdk:rectangle)))

(export 'widget-size-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_allocate_with_baseline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_size_allocate_with_baseline"
                widget-size-allocate-with-baseline) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[allocation]{a @class{gdk:rectangle} instance with the position and
    size to be allocated to @arg{widget}}
  @argument[baseline]{an integer with the baseline of the child, or -1}
  @begin{short}
    This function is only used by @class{gtk:container} subclasses, to assign a
    size, position and (optionally) baseline to their child widgets.
  @end{short}

  In this function, the allocation and baseline may be adjusted. It will be
  forced to a 1 x 1 minimum size, and the @code{adjust_size_allocation}
  and @code{adjust_baseline_allocation} virtual methods on the child will be
  used to adjust the allocation and baseline. Standard adjustments include
  removing the  margins of the widget, and applying the
  @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties of the
  widget.

  If the child widget does not have a @code{:baseline} vertical alignment the
  baseline argument is ignored and -1 is used instead.
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}"
  (widget (g:object widget))
  (allocation (g:boxed gdk:rectangle))
  (baseline :int))

(export 'widget-size-allocate-with-baseline)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_accelerator ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_accelerator" widget-add-accelerator) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object to install an accelerator on}
  @argument[signal]{a string with the widget signal to emit on accelerator
    activation}
  @argument[group]{a @class{gtk:accel-group} object for @arg{widget}, added to
    its toplevel}
  @argument[key]{an unsigned integer with the GDK keyval of the accelerator}
  @argument[mods]{a @symbol{gdk:modifier-type} modifier key combination of the
    accelerator}
  @argument[flags]{a @symbol{gtk:accel-flags} value with the accelerators}
  @begin{short}
    Installs an accelerator for this widget in the accelerator group that
    causes the signal to be emitted if the accelerator is activated.
  @end{short}
  The accelerator group needs to be added to the toplevel of the widget via the
  @fun{gtk:window-add-accel-group} function, and the signal must be of the
  @code{:action} type. Accelerators added through this function are not user
  changeable during runtime. If you want to support accelerators that can be
  changed by the user, use the @fun{gtk:accel-map-add-entry} and
  @fun{gtk:widget-set-accel-path} or @fun{gtk:menu-item-accel-path} functions
  instead.
  @see-class{gtk:widget}
  @see-class{gtk:accel-group}
  @see-symbol{gdk:modifier-type}
  @see-symbol{gtk:accel-flags}
  @see-function{gtk:widget-remove-accelerator}
  @see-function{gtk:window-add-accel-group}
  @see-function{gtk:accel-map-add-entry}
  @see-function{gtk:widget-set-accel-path}
  @see-function{gtk:menu-item-accel-path}"
  (widget (g:object widget))
  (signal :string)
  (group (g:object accel-group))
  (key :uint)
  (mods gdk:modifier-type)
  (flags accel-flags))

(export 'widget-add-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_accelerator ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_accelerator" widget-remove-accelerator)
    :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object to remove an accelerator from}
  @argument[group]{a @class{gtk:accel-group} object for @arg{widget}}
  @argument[key]{an unsigned integer with the GDK keyval of the accelerator}
  @argument[mods]{a @symbol{gdk:modifier-type} value with the modifier key
    combination of the accelerator}
  @return{The boolean whether an accelerator was installed and could be
    removed.}
  @begin{short}
    Removes an accelerator from the widget, previously installed with the
    @fun{gtk:widget-add-accelerator} function.
  @end{short}
  @see-class{gtk:widget}
  @see-class{gtk:accel-group}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:widget-add-accelerator}"
  (widget (g:object widget))
  (group (g:object accel-group))
  (key :uint)
  (mods gdk:modifier-type))

(export 'widget-remove-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_accel_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_set_accel_path" widget-set-accel-path) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[path]{a string with the path used to look up the accelerator}
  @argument[group]{a @class{gtk:accel-group} object}
  @begin{short}
    Given an accelerator group and an accelerator path sets up an accelerator
    in the accelerator group so whenever the key binding that is defined for
    path is pressed, the widget will be activated.
  @end{short}
  This removes any accelerators, for any accelerator group, installed by
  previous calls to the @fun{gtk:widget-set-accel-path} function. Associating
  accelerators with paths allows them to be modified by the user and the
  modifications to be saved for future use. See the @fun{gtk:accel-map-save}
  function.

  This function is a low level function that would most likely be used by a
  menu creation system like the @class{gtk:ui-manager} class. If you use the
  @class{gtk:ui-manager} class, setting up accelerator paths will be done
  automatically.

  Even when you are not using the @class{gtk:ui-manager} class, if you only
  want to set up accelerators on menu items the @fun{gtk:menu-item-accel-path}
  function provides a somewhat more convenient interface.
  @see-class{gtk:widget}
  @see-class{gtk:accel-group}
  @see-function{gtk:accel-map-save}
  @see-class{gtk:ui-manager}
  @see-function{gtk:menu-item-accel-path}"
  (widget (g:object widget))
  (path :string)
  (group (g:object accel-group)))

(export 'widget-set-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_accel_closures ()
;;;
;;; GList * gtk_widget_list_accel_closures (GtkWidget *widget);
;;;
;;; Lists the closures used by widget for accelerator group connections with
;;; gtk_accel_group_connect_by_path() or gtk_accel_group_connect(). The closures
;;; can be used to monitor accelerator changes on widget, by connecting to the
;;; GtkAccelGroup::accel-changed signal of the GtkAccelGroup of a closure which
;;; can be found out with gtk_accel_group_from_accel_closure().
;;;
;;; widget :
;;;     widget to list accelerator closures for
;;;
;;; Returns :
;;;     a newly allocated GList of closures
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_can_activate_accel ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_can_activate_accel" %widget-can-activate-accel)
    :boolean
  (widget (g:object widget))
  (signal :uint))

(defun widget-can-activate-accel (widget signal)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[signal]{an unsigned integer with the ID or a string with the
    name of a signal installed on @arg{widget}}
  @return{@em{True} if the accelerator can be activated.}
  @begin{short}
    Determines whether an accelerator that activates the signal identified by
    @arg{signal} can currently be activated.
  @end{short}
  This is done by emitting the @code{\"can-activate-accel\"} signal on the
  widget. If the signal is not overridden by a handler or in a derived widget,
  then the default check is that the widget must be sensitive, and the widget
  and all its ancestors mapped.
  @see-class{gtk:widget}"
  (when (stringp signal)
    (setf signal (g:signal-lookup signal (g:type-from-instance widget))))
  (%widget-can-activate-accel widget signal))

(export 'widget-can-activate-accel)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_event" widget-event) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[event]{a @class{gdk:event} event}
  @begin{return}
    Return from the event signal emission, @em{true} if the event was handled.
  @end{return}
  @begin{short}
    Rarely used function. This function is used to emit the event signals on a
    widget, those signals should never be emitted without using this function
    to do so.
  @end{short}
  If you want to synthesize an event though, do not use this function. Instead,
  use the @fun{gtk:main-do-event} function so the event will behave as if it
  were in the event queue. Do not synthesize expose events. Instead, use the
  @fun{gdk:window-invalidate-rect} function to invalidate a region of the
  window.
  @see-class{gtk:widget}
  @see-class{gdk:event}
  @see-function{gtk:main-do-event}
  @see-function{gdk:window-invalidate-rect}"
  (widget (g:object widget))
  (event (g:boxed gdk:event)))

(export 'widget-event)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_activate" widget-activate) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object that is activatable}
  @return{@em{True} if the widget was activatable.}
  @begin{short}
    For widgets that can be \"activated\", buttons, menu items, etc., this
    function activates them.
  @end{short}
  Activation is what happens when you press the @kbd{Enter} key on a widget
  during key navigation. If the widget is not activatable, the function returns
  @em{false}.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reparent ()                                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_reparent" widget-reparent) :void
 #+liber-documentation
 "@version{#2021-9-16}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[parent]{a @class{gtk:container} widget to move the widget into}
  @begin{short}
    Moves a widget from one @class{gtk:container} widget to another.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-reparent} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the
    @fun{gtk:container-remove} and @fun{gtk:container-add} functions.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-function{gtk:container-add}
  @see-function{gtk:container-remove}"
  (widget (g:object widget))
  (parent (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_intersect ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_intersect" %widget-intersect) :boolean
  (widget (g:object widget))
  (area (g:boxed gdk:rectangle))
  (intersection (g:boxed gdk:rectangle)))

(defun widget-intersect (widget area)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[area]{a @class{gdk:rectangle} instance}
  @return{Returns the intersection as a @class{gdk:rectangle} instance,
    if there was an intersection or @code{nil}.}
  @begin{short}
    Computes the intersection of a widgets area and @arg{area}, and
    returns the intersection as a rectangle if there was an intersection.
  @end{short}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}"
  (let ((intersection (gdk:rectangle-new)))
    (when (%widget-intersect widget area intersection)
      intersection)))

(export 'widget-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_grab_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_grab_focus" widget-grab-focus) :void
 #+liber-documentation
 "@version{2024-1-2}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes the widget to have the keyboard focus for the @class{gtk:window}
    widget it is inside.
  @end{short}
  The widget must be a focusable widget, such as a @class{gtk:entry} widget.
  Something like the @class{gtk:frame} widget will not work.

  More precisely, it must have the @slot[gtk:widget]{can-focus} property set.
  Use the @fun{gtk:widget-can-focus} function to modify that property.

  The widget also needs to be realized and mapped. This is indicated by the
  related signals. Grabbing the focus immediately after creating the widget
  will likely fail and cause critical warnings.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gtk:widget-can-focus}"
  (widget (g:object widget)))

(export 'widget-grab-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_grab_default ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_grab_default" widget-grab-default) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes the widget to become the default widget.
  @end{short}
  The widget must be able to be a default widget. Typically you would ensure
  this yourself by calling the @fun{gtk:widget-can-default} function with a
  @em{true} value.

  The default widget is activated when the user presses the @kbd{Enter} key in
  a window. Default widgets must be activatable, that is, the
  @fun{gtk:widget-activate} function should affect them. Note that
  @class{gtk:entry} widgets require the @slot[gtk:widget]{activates-default}
  property set to @em{true} before they activate the default widget when the
  @kbd{Enter} key is pressed and the @class{gtk:entry} widget is focused.
  @see-class{gtk:widget}
  @see-class{gtk:entry}
  @see-function{gtk:widget-activate}
  @see-function{gtk:widget-can-default}"
  (widget (g:object widget)))

(export 'widget-grab-default)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state ()                                not exported
;;; gtk_widget_set_state () -> widget-state
;;; ----------------------------------------------------------------------------

(defun (setf widget-state) (state widget)
  (cffi:foreign-funcall "gtk_widget_set_state"
                        (g:object widget) widget
                        state-type state
                        :void)
  state)

(cffi:defcfun ("gtk_widget_get_state" widget-state) state-type
 #+liber-documentation
 "@version{#2021-9-16}
  @syntax{(gtk:widget-state widget) => state}
  @syntax{(setf (gtk:widget-state widget) state)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-type} state for @arg{widget}}
  @begin{short}
    Accessor of the state of the widget.
  @end{short}

  The @fun{gtk:widget-state} function returns the state of the widget. The
  @setf{gtk:widget-state} function sets the state of a widget, insensitive,
  prelighted, etc. This function is for use in widget implementations.

  Usually you should set the state using wrapper functions such as the
  @fun{gtk:widget-sensitive} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-state} function is deprecated since version 3.0 and
    should not be used in newly written code. Use the
    @fun{gtk:widget-state-flags} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-symbol{gtk:state-type}
  @see-function{gtk:widget-state-flags}
  @see-function{gtk:widget-sensitive}"
  (widget (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_parent_window ()
;;; gtk_widget_set_parent_window () -> widget-parent-window
;;; ----------------------------------------------------------------------------

(defun (setf widget-parent-window) (window widget)
  (cffi:foreign-funcall "gtk_widget_set_parent_window"
                        (g:object widget) widget
                        (g:object gdk:window) window
                        :void)
  window)

(cffi:defcfun ("gtk_widget_get_parent_window" widget-parent-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-parent-window widget) => window}
  @syntax{(setf (gtk:widget-parent-window widget) window)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[window]{a @class{gdk:window} parent window}
  @begin{short}
    Accessor of the parent window of the widget.
  @end{short}
  The @fun{gtk:widget-parent-window} function gets the parent window of the
  widget. The @setf{gtk:widget-parent-window} function sets a non default
  parent window for the widget.

  For @class{gtk:window} classes, setting a parent window effects whether
  the window is a toplevel window or can be embedded into other widgets.
  @begin[Notes]{dictionary}
    For @class{gtk:window} classes, this needs to be called before the window
    is realized.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:window}"
  (widget (g:object window)))

(export 'widget-parent-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_events ()
;;; ----------------------------------------------------------------------------

(defun widget-add-events (widget events)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[events]{a @symbol{gdk:event-mask} event mask}
  @begin{short}
    Adds the events in the @arg{events} bitfield to the event mask for the
    widget.
  @end{short}
  See the @fun{gtk:widget-events} function for details.
  @see-class{gtk:widget}
  @see-symbol{gdk:event-mask}
  @see-function{gtk:widget-events}"
  (setf (widget-events widget)
        (append (widget-events widget) events)))

(export 'widget-add-events)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_events ()
;;; gtk_widget_set_device_events () -> widget-device-events
;;; ----------------------------------------------------------------------------

(defun (setf widget-device-events) (events widget device)
  (cffi:foreign-funcall "gtk_widget_set_device_events"
                        (g:object widget) widget
                        (g:object gdk:device) device
                        gdk:event-mask events
                        :void)
  events)

(cffi:defcfun ("gtk_widget_get_device_events" widget-device-events)
    gdk:event-mask
#+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-device-events widget device) => events}
  @syntax{(setf (gtk:widget-device-events widget device) events)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[device]{a @class{gdk:device} object}
  @argument[events]{a @symbol{gdk:event-mask} event mask}
  @begin{short}
    Accessor of the events mask for the widget corresponding to a specific
    device.
  @end{short}
  The @fun{gtk:widget-device-events} function returns the events mask for the
  widget corresponding to an specific device. These are the events that the
  widget will receive when device operates on it. The
  @setf{gtk:widget-device-events} function sets the device event mask for a
  widget.

  The event mask determines which events a widget will receive from the device.
  Keep in mind that different widgets have different default event masks, and by
  changing the event mask you may disrupt the functionality of the widget, so be
  careful. This function must be called while a widget is unrealized. Consider
  the @fun{gtk:widget-add-device-events} function for widgets that are already
  realized, or if you want to preserve the existing event mask. This function
  cannot be used with windowless widgets. To get events on those widgets, place
  them inside a @class{gtk:event-box} widget and receive events on the event
  box.
  @see-class{gtk:widget}
  @see-class{gdk:device}
  @see-class{gtk:event-box}
  @see-symbol{gdk:event-mask}
  @see-function{gtk:widget-add-device-events}"
  (widget (g:object widget))
  (device (g:object gdk:device)))

(export 'widget-device-events)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_device_events ()
;;; ----------------------------------------------------------------------------

(defun widget-add-device-events (widget device events)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[device]{a @class{gdk:device} object}
  @argument[events]{a @symbol{gdk:event-mask} event mask}
  @begin{short}
    Adds the device events in the @arg{events} bitfield to the event mask for
    the widget.
  @end{short}
  See the @fun{gtk:widget-device-events} function for details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-device-events}"
  (setf (widget-device-events widget device)
        (append (widget-device-events widget device) events)))

(export 'widget-add-device-events)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_device_enabled ()
;;; gtk_widget_set_device_enabled () -> widget-device-enabled
;;; ----------------------------------------------------------------------------

(defun (setf widget-device-enabled) (enabled widget device)
  (cffi:foreign-funcall "gtk_widget_set_device_enabled"
                        (g:object widget) widget
                        (g:object gdk:device) device
                        :boolean enabled
                        :void)
  enabled)

(cffi:defcfun ("gtk_widget_get_device_enabled" widget-device-enabled) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-device-enabled widget devive) => enabled}
  @syntax{(setf (gtk:widget-device-enabled widget devide) enabled)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[device]{a @class{gdk:device} object}
  @argument[enabled]{a boolean whether to enable the device}
  @begin{short}
    The @fun{gtk:widget-device-enabled} function returns whether the device can
    interact with the widget and its children.
  @end{short}
  The @setf{gtk:widget-device-enabled} function enables or disables the device
  to interact with the widget and all its children.

  It does so by descending through the @class{gdk:window} object hierarchy and
  enabling the same mask that is has for core events, i.e. the one that the
  @fun{gdk:window-events} function returns.
  @see-class{gtk:widget}
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-function{gdk:window-events}"
  (widget (g:object widget))
  (device (g:object gdk:device)))

(export 'widget-device-enabled)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_toplevel () -> widget-toplevel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_toplevel" widget-toplevel) (g:object widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The topmost ancestor of @arg{widget}, or @arg{widget} itself if
    there is no ancestor.}
  @begin{short}
    This function returns the topmost widget in the container hierarchy
    the widget is a part of.
  @end{short}
  If the widget has no parent widgets, it will be returned as the topmost
  widget.

  Note the difference in behavior versus the @fun{gtk:widget-ancestor} function.
  The call @code{(gtk:widget-ancestor widget \"GtkWindow\")} would return
  @code{nil} if the widget was not inside a toplevel window, and if the window
  was inside a @class{gtk:window} derived widget which was in turn inside
  the toplevel @class{gtk:window} widget. While the second case may seem
  unlikely, it actually happens when a @class{gtk:plug} widget is embedded
  inside a @class{gtk:socket} widget within the same application.

  To reliably find the toplevel @class{gtk:window} widget, use the
  @fun{gtk:widget-toplevel} function and check the type of the result to be
  of \"GtkWindow\" type. For instance, to get the title of the toplevel window
  of the widget, one might use:
  @begin{pre}
(defun get-widget-toplevel-title (widget)
  (let ((toplevel (gtk:widget-toplevel widget)))
    (when (eq (gtype \"GtkWindow\") (g:object-type toplevel))
      (gtk:window-title toplevel))))
  @end{pre}
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-toplevel}
  @see-function{gtk:widget-ancestor}"
  (widget (g:object widget)))

(export 'widget-toplevel)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_ancestor () -> widget-ancestor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_ancestor" widget-ancestor) (g:object widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[gtype]{an ancestor @class{g:type-t} type ID}
  @return{The @class{gtk:widget} ancestor widget, or @arg{nil} if not found.}
  @begin{short}
    Gets the first ancestor of the widget with type @arg{gtype}.
  @end{short}
  For example, the call @code{(gtk:widget-ancestor widget \"GtkBbox\")} gets
  the first @class{gtk:box} widget that is an ancestor of the widget. See note
  about checking for a @class{gtk:window} toplevel in the docs for the
  @fun{gtk:widget-toplevel} function.

  Note that unlike the @fun{gtk:widget-is-ancestor} function, the
  @fun{gtk:widget-ancestor} function considers the widget to be an ancestor of
  itself.
  @see-class{gtk:widget}
  @see-function{gtk:widget-toplevel}
  @see-function{gtk:widget-is-ancestor}"
  (widget (g:object widget))
  (gtype g:type-t))

(export 'widget-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_visual ()
;;; gtk_widget_set_visual () -> widget-visual
;;; ----------------------------------------------------------------------------

(defun (setf widget-visual) (visual widget)
  (cffi:foreign-funcall "gtk_widget_set_visual"
                        (g:object widget) widget
                        (g:object gdk:visual) visual
                        :void)
  visual)

(cffi:defcfun ("gtk_widget_get_visual" widget-visual) (g:object gdk:visual)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-visual object) => visual}
  @syntax{(setf (gtk:widget-visual object) visual)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[visual]{a @class{gdk:visual} object to be used or @code{nil} to
    unset a previous one}
  @begin{short}
    Accessor of the visual be used to render the widget.
  @end{short}
  The @fun{gtk:widget-visual} function gets the visual that will be used to
  render the widget. The @setf{gtk:widget-visual} function sets the visual that
  should be used for by the widget and its children for creating
  @class{gdk:window} objects.

  The visual must be on the same @class{gdk:screen} object as returned by the
  @fun{gtk:widget-screen} function, so handling the @code{\"screen-changed\"}
  signal is necessary.

  Setting a new visual will not cause the widget to recreate its windows, so
  you should call this function before the widget is realized.
  @see-class{gtk:widget}
  @see-class{gdk:visual}
  @see-class{gdk:window}
  @see-class{gdk:screen}
  @see-function{gtk:widget-screen}"
  (widget (g:object widget)))

(export 'widget-visual)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pointer () -> widget-pointer        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_pointer" %widget-pointer) :void
  (widget (g:object widget))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun widget-pointer (widget)
 #+liber-documentation
 "@version{#2021-9-19}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @code{x} -- an integer with the x coordinate @br{}
    @code{y} -- an integer with the y coordinate
  @end{return}
  @begin{short}
    Obtains the location of the mouse pointer in widget coordinates.
  @end{short}

  Widget coordinates are a bit odd. For historical reasons, they are defined as
  GDK window coordinates for widgets that return @em{true} for the
  @fun{gtk:widget-has-window} function, and are relative to
  @code{widget->allocation.x}, @code{widget->allocation.y} coordinates
  otherwise.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-pointer} function has been deprecated since version 3.4
    and should not be used in newly written code. Use the
    @fun{gdk:window-device-position} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gdk:window-device-position}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%widget-pointer widget x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_ancestor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_ancestor" widget-is-ancestor) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[ancestor]{another @class{gtk:widget} object}
  @return{@em{True} if @arg{ancestor} contains the widget as a child,
    grandchild, great grandchild, etc.}
  @begin{short}
    Determines whether the widget is somewhere inside @arg{ancestor}, possibly
    with intermediate containers.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-ancestor}"
  (widget (g:object widget))
  (ancestor (g:object widget)))

(export 'widget-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_translate_coordinates ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_translate_coordinates" %widget-translate-coordinates)
    :boolean
  (src-widget g:object)
  (dst-widget g:object)
  (src-x :int)
  (src-y :int)
  (dst-x (:pointer :int))
  (dst-y (:pointer :int)))

(defun widget-translate-coordinates (src dst xsrc ysrc)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[src]{a @class{gtk:widget} object}
  @argument[dest]{a @class{gtk:widget} object}
  @argument[xsrc]{an integer with the x position relative to @arg{src}}
  @argument[ysrc]{an integer with the y position relative to @arg{src}}
  @return{@em{False} if either the widget was not realized, or there was no
    common ancestor. Otherwise the x position and the y position relative to
    @arg{dest}.}
  @begin{short}
    Translate coordinates relative to the allocation of @arg{src} to
    coordinates relative to the allocations of  @arg{dest}.
  @end{short}
  In order to perform this operation, both widgets must be realized, and must
  share a common toplevel.
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((xdst :int) (ydst :int))
    (when (%widget-translate-coordinates src dst xsrc ysrc xdst ydst)
      (values (cffi:mem-ref xdst :int)
              (cffi:mem-ref ydst :int)))))

(export 'widget-translate-coordinates)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide_on_delete ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_hide_on_delete" widget-hide-on-delete) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True}.}
  @begin{short}
    Intended to be connected to the @code{\"delete-event\"} signal on a
    @class{gtk:window} widget.
  @end{short}
  The function calls the @fun{gtk:widget-hide} function on its argument, then
  returns @em{true}. If connected to the @code{\"delete-event\"} signal, the
  result is that clicking the Close button for a window will hide but not
  destroy the window. By default, GTK destroys windows when the
  @code{\"delete-event\"} signal is received.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gtk:widget-hide}"
  (widget (g:object widget)))

(export 'widget-hide-on-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_ensure_style ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_ensure_style" widget-ensure-style) :void
 #+liber-documentation
 "@version{#2021-9-19}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Ensures that the widget has a style.
  @end{short}
  Not a very useful function. Most of the time, if you want the style, the
  widget is realized, and realized widgets are guaranteed to have a style
  already.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-ensure-style} function has been deprecated since
    version 3.0 and should not be used in newly written code.
    Use the @class{gtk:style-context} class instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}"
  (widget (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_rc_styles ()                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_reset_rc_styles" widget-reset-rc-styles) :void
 #+liber-documentation
 "@version{#2021-9-19}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Reset the styles of the widget and all descendents, so when they are
    looked up again, they get the correct values for the currently loaded RC
    file settings.
  @end{short}
  This function is not useful for applications.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-reset-rc-styles} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @class{gtk:style-context} class instead, and the
    @fun{gtk:widget-reset-style} function.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}
  @see-function{gtk:widget-reset-style}"
  (widget (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_default_style () -> widget-default-style     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_default_style" widget-default-style)
    (g:object style)
 #+liber-documentation
 "@version{#2021-9-19}
  @return{The deprecated default @code{GtkStyle} object.}
  @begin{short}
    Returns the default style used by all widgets initially.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-default-style} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @class{gtk:style-context} class instead, and the @fun{gtk:css-provider-new}
    function to obtain a @class{gtk:style-provider} object with the default
    widget style information.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:css-provider-new}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_direction ()
;;; gtk_widget_set_direction () -> widget-direction
;;; ----------------------------------------------------------------------------

(defun (setf widget-direction) (direction widget)
  (cffi:foreign-funcall "gtk_widget_set_direction"
                        (g:object widget) widget
                        text-direction direction
                        :void)
  direction)

(cffi:defcfun ("gtk_widget_get_direction" widget-direction) text-direction
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-direction widget) => direction}
  @syntax{(setf (gtk:widget-direction widget) direction)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration}
  @begin{short}
    Accessor of the text direction of the widget.
  @end{short}
  The @fun{gtk:widget-direction} function gets the reading direction for a
  widget. The @setf{gtk:widget-direction} function sets the reading direction.

  This direction controls the primary direction for widgets containing text,
  and also the direction in which the children of a container are packed. The
  ability to set the direction is present in order so that correct localization
  into languages with right-to-left reading directions can be done. Generally,
  applications will let the default reading direction present, except for
  containers where the containers are arranged in an order that is explicitely
  visual rather than logical, such as buttons for text justification.

  If the direction is set to the @code{:none} value, then the value set by the
  @fun{gtk:widget-default-direction} function will be used.
  @see-class{gtk:widget}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-default-direction}"
  (widget (g:object widget)))

(export 'widget-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_default_direction ()
;;; gtk_widget_set_default_direction () -> widget-default-direction
;;; ----------------------------------------------------------------------------

(defun (setf widget-default-direction) (direction)
  (cffi:foreign-funcall "gtk_widget_set_default_direction"
                        text-direction direction
                        :void)
  direction)

(cffi:defcfun ("gtk_widget_get_default_direction" widget-default-direction)
    text-direction
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-default-direction) => direction}
  @syntax{(setf (gtk:widget-default-direction) direction)}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration
    for the default direction, this cannot be @code{:none}.}
  @begin{short}
    Accessor of the default reading direction.
  @end{short}
  The @fun{gtk:widget-default-direction} function obtains the current default
  reading direction. The @setf{gtk:widget-default-direction} function sets the
  default reading direction for widgets where the direction has not been
  explicitly set by the @fun{gtk:widget-direction} function.
  @see-class{gtk:widget}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-direction}")

(export 'widget-default-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_shape_combine_region" widget-shape-combine-region)
    :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[region]{a @symbol{cairo:region-t} shape to be added, or @code{nil}
    to remove an existing shape}
  @begin{short}
    Sets a shape for the GDK window of this widget.
  @end{short}
  This allows for transparent windows etc., see the
  @fun{gdk:window-shape-combine-region} function for more information.
  @see-class{gtk:widget}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-shape-combine-region}"
  (widget (g:object widget))
  (region (:pointer (:struct cairo:region-t))))

(export 'widget-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_input_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_input_shape_combine_region"
                widget-input-shape-combine-region) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[region]{a @symbol{cairo:region-t} shape to be added, or @code{nil}
    to remove an existing shape}
  @begin{short}
    Sets an input shape for the GDK window of the widget.
  @end{short}
  This allows for windows which react to mouse click in a nonrectangular region,
  see the @fun{gdk:window-input-shape-combine-region} function for more
  information.
  @see-class{gtk:widget}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-input-shape-combine-region}"
  (widget (g:object widget))
  (region (:pointer (:struct cairo:region-t))))

(export 'widget-input-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path ()
;;; ----------------------------------------------------------------------------

;; This deprecated function is not implemented, but the function
;; gtk_widget_get_path with the Lisp name gtk:widget-path

#+nil
(cffi:defcfun ("gtk_widget_path" %widget-path) :void
  (widget (g:object widget))
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

#+nil
(defun widget-path (widget &key (path-type :name))
 #+liber-documentation
 "@version{#2013-11-25}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[path-type]{@code{:name} or @code{:class}, the default value is
    @code{:name}}
  @return{Returns the path string, or @code{nil}}
  @begin{short}
    Obtains the full path to @arg{widget}.
  @end{short}
  The path is simply the name of a widget and all its parents in the container
  hierarchy, separated by periods. The name of a widget comes from the
  @fun{gtk:widget-name} function. Paths are used to apply styles to a widget in
  gtkrc configuration files. Widget names are the type of the widget by default
  (e.g. \"GtkButton\") or can be set to an application specific value with the
  @fun{gtk:widget-name} function. By setting the name of a widget, you allow
  users or theme authors to apply styles to that specific widget in their gtkrc
  file.

  With a @code{:class} value for the @arg{path-type} argument always uses the
  name of the type of a widget, never uses a custom name set with the
  @fun{gtk:widget-name} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path} function has been deprecated since version 3.0 and
    should not be used in newly written code. Use the @fun{gtk:;widget-path}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-path}
  @see-function{gtk:widget-name}"
  (assert (typep path-type '(member :name :class)))
  (cffi:with-foreign-object (path :pointer)
    (ecase path-type
      (:name (%widget-path widget (cffi:null-pointer) path (cffi:null-pointer)))
      (:class (%widget-class-path widget
                                      (cffi:null-pointer)
                                      path
                                      (cffi:null-pointer))))
    (cffi:mem-ref path '(:string :free-from-foreign t))))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_path ()
;;;
;;; void gtk_widget_class_path (GtkWidget *widget,
;;;                             guint *path_length,
;;;                             gchar **path,
;;;                             gchar **path_reversed);
;;;
;;; Warning
;;;
;;; gtk_widget_class_path has been deprecated since version 3.0 and should not
;;; be used in newly written code. Use gtk_widget_get_path() instead
;;;
;;; Same as gtk_widget_path(), but always uses the name of a widget's type,
;;; never uses a custom name set with gtk_widget_set_name().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; path_length :
;;;     location to store the length of the class path, or NULL
;;;
;;; path :
;;;     location to store the class path as an allocated string, or NULL
;;;
;;; path_reversed :
;;;     location to store the reverse class path as an allocated string,
;;;     or NULL
;;; ----------------------------------------------------------------------------

;;; Implemented only for use of widget-path and not exported

#+nil
(cffi:defcfun ("gtk_widget_class_path" %widget-class-path) :void
  (widget (g:object widget))
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_composite_name ()
;;; gtk_widget_set_composite_name () -> widget-composite-name
;;; ----------------------------------------------------------------------------

(defun (setf widget-composite-name) (name widget)
  (cffi:foreign-funcall "gtk_widget_set_composite_name"
                        (g:object widget) widget
                        :string name
                        :void)
  name)

(cffi:defcfun ("gtk_widget_get_composite_name" widget-composite-name) :string
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-composite-name widget) => name}
  @syntax{(setf gtk:widget-composite-name widget) name)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[name]{a string with the name to set}
  @begin{short}
    Accessor of the composite name of the widget.
  @end{short}
  The @fun{gtk:widget-composite-name} function obtains the composite name of a
  widget. The @setf{gtk:widget-composite-name} function sets a widgets composite
  name. The widget must be a composite child of its parent.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-composite-name} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:widget-class-set-template} function, or do not use this API at all.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-class-set-template}"
  (widget (g:object widget)))

(export 'widget-composite-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_background_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_override_background_color"
                widget-override-background-color) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-flags} value for which to set the
    background color}
  @argument[color]{a @class{gdk:rgba} color to assign, or @code{nil} to undo
    the effect of previous calls}
  @begin{short}
    Sets the background color to use for a widget.
  @end{short}
  All other style values are left untouched. See the
  @fun{gtk:widget-override-color} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-override-background-color} function has been deprecated
    since version 3.16 and should not be used in newly written code. This
    function is not useful in the context of CSS based rendering. If you wish
    to change the way a widget renders its background you should use a custom
    CSS style, through an application specific @class{gtk:style-provider} object
    and a CSS style class. You can also override the default drawing of a widget
    through the @code{\"draw\"} signal, and use Cairo to draw a specific color,
    regardless of the CSS style.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-struct{gdk:rgba}
  @see-class{gtk:style-provider}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-override-color}"
  (widget (g:object widget))
  (state state-flags)
  (color (g:boxed gdk:rgba)))

(export 'widget-override-background-color)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_override_color" widget-override-color) :void
 #+liber-documentation
 "@version{#2023-3-26}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-flags} value for which to set the
    color}
  @argument[color]{a @class{gdk:rgba} color to assign, or @code{nil} to undo
    the effect of previous calls}
  @begin{short}
    Sets the color to use for a widget.
  @end{short}
  All other style values are left untouched.
  @begin[Notes]{dictionary}
    @begin{itemize}
      @begin{item}
        This API is mostly meant as a quick way for applications to change a
        widget appearance. If you are developing a widgets library and intend
        this change to be themeable, it is better done by setting meaningful
        CSS classes and regions in your widget/container implementation through
        the @fun{gtk:style-context-add-class} and
        @fun{gtk:style-context-add-region} functions. This way, your widget
        library can install a @class{gtk:css-provider} object with the
        @var{gtk:+priority-fallback+} priority in order to provide a default
        styling for those widgets that need so, and this theming may fully
        overridden by the theme of the user.
      @end{item}
      @begin{item}
        Note that for complex widgets this may bring in undesired results, such
        as uniform background color everywhere, in these cases it is better to
        fully style such widgets through a @class{gtk:css-provider} object
        with the @var{gtk:+priority-application+} priority.
      @end{item}
    @end{itemize}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-override-color} function has been deprecated since
    version 3.16 and should not be used in newly written code. Use a custom
    style provider and style classes instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:rgba}
  @see-symbol{gtk:state-flags}
  @see-class{gtk:css-provider}
  @see-function{gtk:style-context-add-class}
  @see-function{gtk:style-context-add-region}"
  (widget (g:object widget))
  (state state-flags)
  (color (g:boxed gdk:rgba)))

(export 'widget-override-color)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_font ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_override_font" widget-override-font) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[desc]{a @class{pango:font-description} instance to use,
    or @code{nil} to undo the effect of previous calls}
  @begin{short}
    Sets the font to use for a widget.
  @end{short}
  All other style values are left untouched. See the
  @fun{gtk:widget-override-color} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-override-font} function has been deprecated since
    version 3.16 and should not be used in newly written code. This function is
    not useful in the context of CSS based rendering. If you wish to change the
    font a widget uses to render its text you should use a custom CSS style,
    through an application specific style provider and a CSS style class.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{pango:font-description}
  @see-function{gtk:widget-override-color}"
  (widget (g:object widget))
  (desc (g:boxed pango:font-description)))

(export 'widget-override-font)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_symbolic_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_override_symbolic_color"
                widget-override-symbolic-color) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[name]{a string with the name of the symbolic color to modify}
  @argument[color]{a @class{gdk:rgba} color to assign, or @code{nil} to undo
    the effect of previous calls}
  @begin{short}
    Sets a symbolic color for a widget.
  @end{short}
  All other style values are left untouched. See the
  @fun{gtk:widget-override-color} for overriding the foreground or background
  color.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-override-symbolic-color} function has been deprecated
    since version 3.16 and should not be used in newly written code. This
    function is not useful in the context of CSS-based rendering. If you wish to
    change the color used to render symbolic icons you should use a custom CSS
    style, through an application specific @class{gtk:style-provider} object
    and a CSS style class.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-provider}
  @see-class{gdk:rgba}
  @see-function{gtk:widget-override-color}"
  (widget (g:object widget))
  (name :string)
  (color (g:boxed gdk:rgba)))

(export 'widget-override-symbolic-color)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_override_cursor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_override_cursor" widget-override-cursor) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[primary]{a @class{gdk:rgba} color to use for the primary cursor, or
    @code{nil} to undo the effect of previous calls to the
    @fun{gtk:widget-override-cursor} function}
  @argument[secondary]{a @class{gdk:rgba} color to use for the secondary cursor,
    or @code{nil} to undo the effect of previous calls}
  @begin{short}
    Sets the cursor color to use in a widget, overriding the \"cursor-color\"
    and \"secondary-cursor-color\" style properties.
  @end{short}
  All other style values are left untouched.

  Note that the underlying properties have the @class{gdk:color} color type, so
  the alpha value in the @arg{primary} and @arg{secondary} arguments will be
  ignored.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-override-cursor} function has been deprecated since
    version 3.16 and should not be used in newly written code. This function is
    not useful in the context of CSS-based rendering. If you wish to change the
    color used to render the primary and seconday cursors you should use a
    custom CSS style, through an application specific @class{gtk:style-provider}
    object and a CSS style class.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-provider}
  @see-class{gdk:rgba}"
  (widget (g:object widget))
  (primary (g:boxed gdk:rgba))
  (secondary (g:boxed gdk:rgba)))

(export 'widget-override-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_style ()
;;;
;;; void gtk_widget_modify_style (GtkWidget *widget, GtkRcStyle *style);
;;;
;;; Warning
;;;
;;; gtk_widget_modify_style has been deprecated since version 3.0 and should not
;;; be used in newly written code. Use GtkStyleContext with a custom
;;; GtkStyleProvider instead
;;;
;;; Modifies style values on the widget.
;;;
;;; Modifications made using this technique take precedence over style values
;;; set via an RC file, however, they will be overridden if a style is
;;; explicitely set on the widget using gtk_widget_set_style(). The GtkRcStyle
;;; structure is designed so each field can either be set or unset, so it is
;;; possible, using this function, to modify some style values and leave the
;;; others unchanged.
;;;
;;; Note that modifications made with this function are not cumulative with
;;; previous calls to gtk_widget_modify_style() or with such functions as
;;; gtk_widget_modify_fg(). If you wish to retain previous values, you must
;;; first call gtk_widget_get_modifier_style(), make your modifications to the
;;; returned style, then call gtk_widget_modify_style() with that style. On the
;;; other hand, if you first call gtk_widget_modify_style(), subsequent calls to
;;; such functions gtk_widget_modify_fg() will have a cumulative effect with the
;;; initial modifications.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; style :
;;;     the GtkRcStyle holding the style modifications
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_modifier_style ()
;;;
;;; GtkRcStyle * gtk_widget_get_modifier_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_get_modifier_style has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use GtkStyleContext with a custom
;;; GtkStyleProvider instead.
;;;
;;; Returns the current modifier style for the widget. (As set by
;;; gtk_widget_modify_style().) If no style has previously set, a new GtkRcStyle
;;; will be created with all values unset, and set as the modifier style for the
;;; widget. If you make changes to this rc style, you must call
;;; gtk_widget_modify_style(), passing in the returned rc style, to make sure
;;; that your changes take effect.
;;;
;;; Caution: passing the style back to gtk_widget_modify_style() will normally
;;; end up destroying it, because gtk_widget_modify_style() copies the passed-in
;;; style and sets the copy as the new modifier style, thus dropping any
;;; reference to the old modifier style. Add a reference to the modifier style
;;; if you want to keep it alive.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     the modifier style for the widget. This rc style is owned by the widget.
;;;     If you want to keep a pointer to value this around, you must add a
;;;     refcount using g_object_ref()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_fg ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_fg" widget-modify-fg) :void
 #+liber-documentation
 "@version{#2021-9-19}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-type} value for which to set the
    foreground color}
  @argument[color]{the @class{gdk:color} color to assign, does not need to be
    allocated, or @code{nil} to undo the effect of previous calls to the
    function}
  @begin{short}
    Sets the foreground color for a widget in a particular state.
  @end{short}
  All other style values are left untouched.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-fg} function has been deprecated since version
    3.0 and should not be used in newly written code. Use a custom style
    provider and style classes instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:color}
  @see-symbol{gtk:state-type}"
  (widget (g:object widget))
  (state state-type)
  (color (g:boxed gdk:color)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_bg ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_bg" widget-modify-bg) :void
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-type} value for which to set the
    background color}
  @argument[color]{the @class{gdk:color} color to assign, does not need to be
    allocated), or @code{nil} to undo the effect of previous calls of the
    function}
  @short{Sets the background color for the widget in a particular state.}
  All other style values are left untouched.

  Note that \"no window\" widgets, which have the @code{:no-window} flag set,
  draw on their parent container's window and thus may not draw any background
  themselves. This is the case for e.g. the @class{gtk:label} widget.

  To modify the background of such widgets, you have to set the background
  color on their parent. If you want to set the background of a rectangular
  area around a label, try placing the label in a @class{gtk:event-box} widget
  and setting the background color on that.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-bg} function has been deprecated since version
    3.0 and should not be used in newly written code. This function is not
    useful in the context of CSS based rendering. If you wish to change the way
    a widget renders its background you should use a custom CSS style, through
    an application specific @class{gtk:style-provider} object and a CSS style
    class. You can also override the default drawing of a widget through the
    @code{\"draw\"} signal, and use Cairo to draw a specific color, regardless
    of the CSS style.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:color}
  @see-symbol{gtk:state-type}
  @see-class{gtk:style-provider}"
  (widget (g:object widget))
  (state state-type)
  (color (g:boxed gdk:color)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_text ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_text" widget-modify-text) :void
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{the @symbol{gtk:state-type} value for which to set the text
    color}
  @argument[color]{the @class{gdk:color} color to assign, does not need to be
    allocated, or @code{nil} to undo the effect of previous calls of the
    function}
  @short{Sets the text color for a widget in a particular state.}

  All other style values are left untouched. The text color is the foreground
  color used along with the base color, see the function
  @fun{gtk:widget-modify-base}, for widgets such as @class{gtk:entry} and
  @class{gtk:text-view}.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-text} function has been deprecated since version
    3.0 and should not be used in newly written code. Use a custom style
    provider and style classes instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:color}
  @see-symbol{gtk:state-type}
  @see-function{gtk:widget-modify-base}"
  (widget (g:object widget))
  (state state-type)
  (color (g:boxed gdk:color)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_base ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_base" widget-modify-base) :void
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[state]{a @symbol{gtk:state-type} value for which to set the base
    color}
  @argument[color]{the @class{gdk:color} color to assign, does not need to be
    allocated, or @code{nil} to undo the effect of previous calls to the
    function}
  @begin{short}
    Sets the base color for a widget in a particular state.
  @end{short}
  All other style values are left untouched. The base color is the background
  color used along with the text color, see the function
  @fun{gtk:widget-modify-text}, for widgets such as @class{gtk:entry} and
  @class{gtk:text-view}.

  Note that \"no window\" widgets, which have the @code{:no-window} flag set,
  draw on their parent container's window and thus may not draw any background
  themselves. This is the case for e.g. the @class{gtk:label} widget.

  To modify the background of such widgets, you have to set the base color on
  their parent. If you want to set the background of a rectangular area around
  a label, try placing the label in a @class{gtk:event-box} widget and setting
  the base color on that.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-base} function has been deprecated since version
    3.0 and should not be used in newly written code. This function is not
    useful in the context of CSS based rendering. If you wish to change the way
    a widget renders its background you should use a custom CSS style, through
    an application specific @class{gtk:style-provider} object and a CSS style
    class. You can also override the default drawing of a widget through the
    @code{\"draw\"} signal, and use Cairo to draw a specific color, regardless
    of the CSS style.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:color}
  @see-symbol{gtk:state-type}
  @see-class{gtk:style-provider}
  @see-function{gtk:widget-modify-text}"
  (widget (g:object widget))
  (state state-type)
  (color (g:boxed gdk:color)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_font ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_font" widget-modify-font) :void
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[desc]{a @class{pango:font-description} instance to use,
    or @code{nil} to undo the effect of previous calls}
  @short{Sets the font to use for a widget.}
  All other style values are left untouched.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-font} function has been deprecated since version
    3.0 and should not be used in newly written code. This function is not
    useful in the context of CSS based rendering. If you wish to change the font
    a widget uses to render its text you should use a custom CSS style, through
    an application specific style provider and a CSS style class.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{pango:font-description}"
  (widget (g:object widget))
  (desc (g:boxed pango:font-description)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_modify_cursor ()                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_modify_cursor" widget-modify-cursor) :void
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[primary]{the @class{gdk:color} color to use for primary cursor,
    does not need to be allocated, or @code{nil} to undo the effect of previous
    calls to the function}
  @argument[secondary]{the @class{gdk:color} color to use for secondary cursor,
    does not need to be allocated, or @code{nil} to undo the effect of previous
    calls to of the function}
  @begin{short}
    Sets the cursor color to use in a widget, overriding the @code{cursor-color}
    and @code{secondary-cursor-color} style properties.
  @end{short}
  All other style values are left untouched.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-modify-cursor} function is deprecated since version 3.0
    and should not be used in newly written code. This function is not useful in
    the context of CSS based rendering. If you wish to change the color used to
    render the primary and seconday cursors you should use a custom CSS style,
    through an application specific @class{gtk:style-provider} object and a CSS
    style class.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:color}
  @see-class{gtk:style-provider}"
  (widget (g:object widget))
  (primary (g:boxed gdk:color))
  (secondary (g:boxed gdk:color)))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_create_pango_context" widget-create-pango-context)
    (g:object pango:context :return)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The new @class{pango:context} object.}
  @begin{short}
    Creates a new Pango context with the appropriate font map, font description,
    and base direction for drawing text for this widget.
  @end{short}
  See also the @fun{gtk:widget-pango-context} function.
  @see-class{gtk:widget}
  @see-class{pango:context}
  @see-function{gtk:widget-pango-context}"
  (widget (g:object widget)))

(export 'widget-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pango_context () -> widget-pango-context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_pango_context" widget-pango-context)
    (g:object pango:context)
 #+liber-documentation
 "@version{2023-12-26}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{pango:context} object for the widget.}
  @begin{short}
    Gets a Pango context with the appropriate font map, font description, and
    base direction for this widget.
  @end{short}
  Unlike the Pango context returned by the @fun{gtk:widget-create-pango-context}
  function, this Pango context is owned by the widget, it can be used until the
  screen for the widget changes or the widget is removed from its toplevel, and
  will be updated to match any changes to the attributes of the widget.

  If you create and keep a @class{pango:layout} object using this context, you
  must deal with changes to the context by calling the
  @fun{pango:layout-context-changed} function on the layout in response to the
  @code{\"style-updated\"} and @code{\"direction-changed\"} signals for the
  widget.
  @see-class{gtk:widget}
  @see-class{pango:layout}
  @see-function{gtk:widget-create-pango-context}
  @see-function{pango:layout-context-changed}"
  (widget (g:object widget)))

(export 'widget-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_font_options ()
;;; gtk_widget_set_font_options () -> widget-font-options
;;; ----------------------------------------------------------------------------

(defun (setf widget-font-options) (options widget)
  (let ((options1 (if options options (cffi:null-pointer))))
    (cffi:foreign-funcall "gtk_widget_set_font_options"
                          (g:object widget) widget
                          (:pointer (:struct cairo:font-options-t)) options1
                          :void)
    options))

(cffi:defcfun ("gtk_widget_get_font_options" %widget-font-options)
    (:pointer (:struct cairo:font-options-t))
  (widget (g:object widget)))

(defun widget-font-options (widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-font-options widget) => options}
  @syntax{(setf (gtk:widget-font-options widget) options)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[options]{a @symbol{cairo:font-options-t} instance, or
    @code{nil} to unset any previously set default font options}
  @begin{short}
    The @fun{gtk:widget-font-options} function returns the font options used
    for Pango rendering.
  @end{short}
  The @setf{gtk:widget-font-options} function sets the font options. When not
  set, the default font options for the GDK screen will be used.
  @see-class{gtk:widget}
  @see-class{gdk:screen}
  @see-symbol{cairo:font-options-t}"
  (let ((options (%widget-font-options widget)))
    (unless (cffi:null-pointer-p options)
      options)))

(export 'widget-font-options)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_font_map ()
;;; gtk_widget_set_font_map () -> widget-font-map
;;; ----------------------------------------------------------------------------

(defun (setf widget-font-map) (fontmap widget)
  (cffi:foreign-funcall "gtk_widget_set_font_map"
                        (g:object widget) widget
                        (g:object pango:font-map) fontmap
                        :void)
  fontmap)

(cffi:defcfun ("gtk_widget_get_font_map" widget-font-map)
    (g:object pango:font-map)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-font-map widget) => fontmap}
  @syntax{(setf (gtk:widget-font-map widget) fontmap)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[fontmap]{a @class{pango:font-map} object, or @code{nil} to unset
    any previously set font map}
  @begin{short}
    The @fun{gtk:widget-font-map} function gets the font map that has been set.
  @end{short}
  The @setf{gtk:widget-font-map} function sets the font map to use for Pango
  rendering. When not set, the widget will inherit the font map from its parent.
  @see-class{gtk:widget}
  @see-class{pango:font-map}"
  (widget (g:object widget)))

(export 'widget-font-map)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_layout ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_create_pango_layout" %widget-create-pango-layout)
    (g:object pango:layout :return)
  (widget (g:object widget))
  (text :string))

(defun widget-create-pango-layout (widget text)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[text]{a string with the text to set on the layout, can be
    @code{nil}}
  @return{The new @class{pango:layout} object.}
  @begin{short}
    Creates a new @class{pango:layout} object with the appropriate font map,
    font description, and base direction for drawing text for this widget.
  @end{short}

  If you keep a @class{pango:layout} object created in this way around, in order
  to notify the layout of changes to the base direction or font of this widget,
  you must call the @fun{pango:layout-context-changed} function in response to
  the @code{\"style-updated\"} and @code{\"direction-changed\"} signals for the
  widget.
  @see-class{gtk:widget}
  @see-class{pango:layout}
  @see-function{pango:layout-context-changed}"
  (%widget-create-pango-layout widget
                               (if text text (cffi:null-pointer))))

(export 'widget-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_render_icon ()                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_render_icon" widget-render-icon)
    (g:object gdk:pixbuf)
 #+liber-documentation
 "@version{#2021-3-20}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[id]{a string with a stock ID}
  @argument[size]{a @symbol{gtk:icon-size} stock size, if there are multiple
    source sizes, GTK picks one of the available sizes}
  @argument[detail]{a string with render detail to pass to theme engine}
  @return{The new @see-class{gdk:pixbuf} object, or @code{nil} if the stock ID
    was not known.}
  @begin{short}
    A convenience function that uses the theme settings for @arg{widget} to
    look up @arg{id} and render it to a pixbuf.
  @end{short}
  The argument @arg{id} should be a stock icon ID such as
  @code{\"gtk-open\"} or @code{\"gtk-ok\"}. The argument @arg{size} should be
  a size such as @code{:menu}. The argument @arg{detail} should be a string that
  identifies the widget or code doing the rendering, so that theme engines can
  special-case rendering for that widget or code.

  The pixels in the returned @class{gdk:pixbuf} object are shared with the rest
  of the application and should not be modified.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-render-icon} function has been deprecated since version
    3.0 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-load-icon} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:pixbuf}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:icon-theme-load-icon}"
  (widget (g:object widget))
  (id :string)
  (size icon-size)
  (detail :string))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_render_icon_pixbuf ()                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_render_icon_pixbuf" widget-render-icon-pixbuf)
    (g:object gdk:pixbuf)
 #+liber-documentation
 "@version{#2021-9-19}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[id]{a string with a stock ID}
  @argument[size]{a @symbol{gtk:icon-size} stock size, if there are multiple
    source sizes, GTK picks one of the available sizes}
  @return{The new @class{gdk:pixbuf} object, or @code{nil} if the stock ID was
    not known.}
  @begin{short}
    A convenience function that uses the theme engine and style settings for
    @arg{widget} to look up @arg{id} and render it to a pixbuf.
  @end{short}
  The @arg{id} argument should be a stock icon ID such as @code{\"gtk-open\"}
  or @code{\"gtk-ok\"}. The @arg{size} argument should be a size such as
  @code{:menu}.

  The pixels in the returned @class{gdk:pixbuf} object are shared with the rest
  of the application and should not be modified.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-render-icon-pixbuf} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-load-icon} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:pixbuf}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:icon-theme-load-icon}"
  (widget (g:object widget))
  (id :string)
  (size icon-size))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_pop_composite_child ()                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_pop_composite_child" widget-pop-composite-child)
    :void
 #+liber-documentation
 "@version{#2021-4-17}
  @begin{short}
    Cancels the effect of a previous call to the function
    @fun{gtk:widget-push-composite-child}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-pop-composite-child} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the function
    @fun{gtk:widget-class-set-template}, or do not use this API at all.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-push-composite-child}
  @see-function{gtk:widget-class-set-template}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_push_composite_child ()                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_push_composite_child" widget-push-composite-child)
    :void
 #+liber-documentation
 "@version{#2021-4-17}
  @begin{short}
    Makes all newly created widgets as composite children until the
    corresponding @fun{gtk:widget-pop-composite-child} function call.
  @end{short}

  A composite child is a child that is an implementation detail of the
  container it is inside and should not be visible to people using the
  container. Composite children are not treated differently by GTK, but e.g.
  GUI builders might want to treat them in a different way.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-push-composite-child} function has been deprecated
    since version 3.10 and should not be used in newly written code. This API
    never really worked well and was mostly unused, now we have a more complete
    mechanism for composite children, see the function
    @fun{gtk:widget-class-set-template}.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-pop-composite-child}
  @see-function{gtk:widget-class-set-template}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_area ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_draw_area" widget-queue-draw-area) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[x]{an integer with the x coordinate of upper-left corner of
    rectangle to redraw}
  @argument[y]{an integer with the y coordinate of upper-left corner of
    rectangle to redraw}
  @argument[width]{an integer with the width of region to draw}
  @argument[height]{an integer with the height of region to draw}
  @begin{short}
    Convenience function that calls the @fun{gtk:widget-queue-draw-region}
    functionon the region created from the given coordinates.
  @end{short}

  The region here is specified in widget coordinates. Widget coordinates are a
  bit odd. For historical reasons, they are defined as GDK window coordinates
  for widgets that return @em{true} for the @fun{gtk:widget-has-window}
  function, and are relative to @code{widget->allocation.x},
  @code{widget->allocation.y} otherwise.

  The @arg{width} or @arg{height} arguments may be 0, in this case this function
  does nothing. Negative values for @arg{width} and @arg{height} are not
  allowed.
  @see-class{gtk:widget}
  @see-function{gtk:widget-queue-draw-region}
  @see-function{gtk:widget-has-window}"
  (widget (g:object widget))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'widget-queue-draw-area)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_draw_region" widget-queue-draw-region) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[region]{a @symbol{cairo:region-t} region to draw}
  @begin{short}
    Invalidates the rectangular area of @arg{widget} defined by @arg{region} by
    calling the @fun{gdk:window-invalidate-region} function on the window of the
    widget and all its child windows.
  @end{short}
  Once the main loop becomes idle, after the current batch of events has been
  processed, roughly, the window will receive expose events for the union of
  all regions that have been invalidated.

  Normally you would only use this function in widget implementations. You
  might also use it to schedule a redraw of a @class{gtk:drawing-area} widget
  or some portion thereof.
  @see-class{gtk:widget}
  @see-symbol{cairo:region-t}
  @see-class{gtk:drawing-area}
  @see-function{gdk:window-invalidate-region}"
  (widget (g:object widget))
  (region (:pointer (:struct cairo:region-t))))

(export 'widget-queue-draw-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_redraw_on_allocate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_set_redraw_on_allocate"
                widget-set-redraw-on-allocate) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[redraw]{if @em{true}, the entire widget will be redrawn when it is
    allocated to a new size, otherwise, only the new portion of the widget will
    be redrawn}
  @begin{short}
    Sets whether the entire widget is queued for drawing when its size
    allocation changes.
  @end{short}
  By default, this setting is @em{true} and the entire widget is redrawn on
  every size change. If your widget leaves the upper left unchanged when made
  bigger, turning this setting off will improve performance.

  Note that for widgets where the @fun{gtk:widget-has-window} function is
  @em{false} setting this flag to @em{false} turns off all allocation on
  resizing: the widget will not even redraw if its position changes. This is to
  allow containers that do not draw anything to avoid excess invalidations. If
  you set this flag on a widget with no window that does draw on the GDK
  window, you are responsible for invalidating both the old and new allocation
  of the widget when the widget is moved and responsible for invalidating
  regions newly when the widget increases size.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-window}"
  (widget (g:object widget))
  (redraw :boolean))

(export 'widget-set-redraw-on-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_mnemonic_activate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_mnemonic_activate" widget-mnemonic-activate) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[cycling]{@em{true} if there are other widgets with the same
    mnemonic}
  @return{@em{True} if the signal has been handled.}
  @begin{short}
    Emits the @code{\"mnemonic-activate\"} signal.
  @end{short}
  The default handler for this signal activates the widget if @arg{cycling} is
  @em{false}, and just grabs the focus if @arg{cycling} is @em{true}.
  @see-class{gtk:widget}"
  (widget (g:object widget))
  (cycling :boolean))

(export 'widget-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_install_style_property"
                widget-class-install-style-property) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[class]{a pointer to a widget class structure}
  @argument[pspec]{a @symbol{g:param-spec} instance for the property}
  @begin{short}
    Installs a style property on a widget class.
  @end{short}
  The parser for the style property is determined by the value type of
  @arg{pspec}.
  @see-class{gtk:widget}
  @see-symbol{g:param-spec}"
  (class :pointer)
  (pspec (:pointer (:struct g:param-spec))))

(export 'widget-class-install-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_style_property_parser ()
;;;
;;; void gtk_widget_class_install_style_property_parser
;;;                                                 (GtkWidgetClass *klass,
;;;                                                  GParamSpec *pspec,
;;;                                                  GtkRcPropertyParser parser)
;;;
;;; Installs a style property on a widget class.
;;;
;;; klass :
;;;     a GtkWidgetClass
;;;
;;; pspec :
;;;     the GParamSpec for the style property
;;;
;;; parser :
;;;     the parser for the style property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_find_style_property ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_find_style_property"
                %widget-class-find-style-property)
    (:pointer (:struct g:param-spec))
  (class (:pointer (:struct g:type-class)))
  (name :string))

(defun widget-class-find-style-property (gtype name)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[class]{a @class{g:type-t} type ID for a @class{gtk:widget} class}
  @argument[name]{a string with the name of the style property to find}
  @return{The @symbol{g:param-spec} instance of the style property or a
    @code{null-pointer} if @arg{class} has no style property with that property
    name.}
  @short{Finds a style property of a widget class by property name.}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:widget-class-find-style-property \"GtkNotebook\" \"arrow-spacing\")
=> #.(SB-SYS:INT-SAP #X00E8BAE0)
(g:param-spec-type *)
=> #<GTYPE :name \"GParamInt\" :id 14620672>
(g:param-spec-value-type **)
=> #<GTYPE :name \"gint\" :id 24>
(gtk:widget-class-find-style-property \"GtkNotebook\" \"unknown\")
=> #.(SB-SYS:INT-SAP #X00000000)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-symbol{g:type-t}
  @see-symbol{g:param-spec}
  @see-function{gtk:widget-class-list-style-properties}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%widget-class-find-style-property class name)))
        (unless (cffi:null-pointer-p pspec) pspec))
      (g:type-class-unref class))))

(export 'widget-class-find-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_list_style_properties ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_list_style_properties"
                %widget-class-list-style-properties)
    (:pointer (:pointer (:struct g:param-spec)))
  (class (:pointer (:struct g:type-class)))
  (n-props (:pointer :uint)))

(defun widget-class-list-style-properties (gtype)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[gtype]{a @class{g:type-t} widget class type ID}
  @return{The list of @symbol{g:param-spec} instances.}
  @begin{short}
    Returns all style properties of a widget class.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar #'g:param-spec-name
        (gtk:widget-class-list-style-properties \"GtkButton\"))
=> (\"cursor-aspect-ratio\" \"cursor-color\" ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-symbol{g:param-spec}
  @see-function{gtk:widget-class-find-style-property}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%widget-class-list-style-properties class n-props)))
          (unwind-protect
            (loop for count from 0 below (cffi:mem-ref n-props :uint)
                  for pspec = (cffi:mem-aref pspecs :pointer count)
                  collect pspec)
            (g:free pspecs))))
      (g:type-class-unref class))))

(export 'widget-class-list-style-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_region_intersect ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_region_intersect" widget-region-intersect)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[region]{a @symbol{cairo:region-t} region, in the same coordinate
    system as the widget allocation, that is, relative to the GDK window
    for windowless widgets, relative to the parent window of the GDK window for
    widgets with their own window}
  @return{The newly allocated region holding the intersection of @arg{widget}
    and @arg{region}.}
  @begin{short}
    Computes the intersection of the area of the widget and @arg{region},
    returning the intersection.
  @end{short}
  The result may be empty, use the @fun{cairo:region-is-empty} function to
  check.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-region-intersect} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-allocation} and @fun{cairo:region-intersect-rectangle}
    functions to get the same behavior.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-is-empty}
  @see-function{gtk:widget-allocation}
  @see-function{cairo:region-intersect-rectangle}"
  (widget (g:object widget))
  (region (:pointer (:struct cairo:region-t))))

(export 'widget-region-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_expose ()
;;;
;;; gint gtk_widget_send_expose (GtkWidget *widget, GdkEvent *event);
;;;
;;; Very rarely-used function. This function is used to emit an expose event on
;;; a widget. This function is not normally used directly. The only time it is
;;; used is when propagating an expose event to a child NO_WINDOW widget, and
;;; that is normally done using gtk_container_propagate_draw().
;;;
;;; If you want to force an area of a window to be redrawn, use
;;; gdk_window_invalidate_rect() or gdk_window_invalidate_region(). To cause
;;; the redraw to be done immediately, follow that call with a call to
;;; gdk_window_process_updates().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; event :
;;;     a expose GdkEvent
;;;
;;; Returns :
;;;     return from the event signal emission (TRUE if the event was handled)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_send_focus_change ()
;;;
;;; gboolean gtk_widget_send_focus_change (GtkWidget *widget, GdkEvent *event);
;;;
;;; Sends the focus change event to widget
;;;
;;; This function is not meant to be used by applications. The only time it
;;; should be used is when it is necessary for a GtkWidget to assign focus to a
;;; widget that is semantically owned by the first widget even though it's not a
;;; direct child - for instance, a search entry in a floating window similar to
;;; the quick search in GtkTreeView.
;;;
;;; An example of its usage is:
;;;
;;; GdkEvent *fevent = gdk_event_new (GDK_FOCUS_CHANGE);
;;;
;;; fevent->focus_change.type = GDK_FOCUS_CHANGE;
;;; fevent->focus_change.in = TRUE;
;;; fevent->focus_change.window = gtk_widget_get_window (widget);
;;; if (fevent->focus_change.window != NULL)
;;;   g_object_ref (fevent->focus_change.window);
;;;
;;; gtk_widget_send_focus_change (widget, fevent);
;;;
;;; gdk_event_free (event);
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; event :
;;;     a GdkEvent of type GDK_FOCUS_CHANGE
;;;
;;; Returns :
;;;     the return value from the event signal emission: TRUE if the event was
;;;     handled, and FALSE otherwise
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get ()
;;;
;;; void gtk_widget_style_get (GtkWidget *widget,
;;;                            const gchar *first_property_name,
;;;                            ...);
;;;
;;; Gets the values of a multiple style properties of widget.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; ... :
;;;     pairs of property names and locations to return the property values,
;;;     starting with the location for first_property_name, terminated by NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_property () -> widget-style-property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_style_get_property" %widget-style-property) :void
  (widget (g:object widget))
  (property :string)
  (value (:pointer (:struct g:value))))

(defun widget-style-property (widget property)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[property]{a string with the name of a style property}
  @return{The style property value.}
  @begin{short}
    Gets the value of a style property of the widget.
  @end{short}
  If the style property does not exist on the widget class @code{nil} is
  returned.
  @begin[Examples]{dictionary}
    Get the default value for the @code{arrow-spacing} style property of a
    notebook container.
    @begin{pre}
(defvar notebook (make-instance 'gtk:notebook))
=> NOTEBOOK
(gtk:widget-style-property notebook \"arrow-spacing\")
=> 0
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-class-find-style-property}"
  (let* ((pspec (widget-class-find-style-property
                    (g:type-from-instance widget)
                    property))
         (gtype (if pspec
                    (g:param-spec-value-type pspec)
                    nil)))
    ;; TODO: Returns nil for an invalid property. Consider to throw an error.
    (when gtype
      (cffi:with-foreign-object (value '(:struct g:value))
        (g:value-init value gtype)
        (prog2
          (%widget-style-property widget property value)
          (gobject:parse-g-value value)
          (g:value-unset value))))))

(export 'widget-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_get_valist ()
;;;
;;; void gtk_widget_style_get_valist (GtkWidget *widget,
;;;                                   const gchar *first_property_name,
;;;                                   va_list var_args);
;;;
;;; Non-vararg variant of gtk_widget_style_get(). Used primarily by language
;;; bindings.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; var_args :
;;;     a va_list of pairs of property names and locations to return the
;;;     property values, starting with the location for first_property_name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_style_attach ()
;;;
;;; void gtk_widget_style_attach (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_style_attach is deprecated and should not be used in
;;; newly written code. 3.0. This step is unnecessary with GtkStyleContext.
;;;
;;; This function attaches the widget's GtkStyle to the widget's GdkWindow. It
;;; is a replacement for
;;;
;;; widget->style = gtk_style_attach (widget->style, widget->window);
;;;
;;; and should only ever be called in a derived widget's "realize"
;;; implementation which does not chain up to its parent class' "realize"
;;; implementation, because one of the parent classes (finally GtkWidget) would
;;; attach the style itself.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_type ()
;;;
;;; void gtk_widget_class_set_accessible_type (GtkWidgetClass *widget_class,
;;;                                            GType type);
;;;
;;; Sets the type to be used for creating accessibles for widgets of
;;; widget_class. The given type must be a subtype of the type used for
;;; accessibles of the parent class.
;;;
;;; This function should only be called from class init functions of widgets.
;;;
;;; widget_class :
;;;     class to set the accessible type for
;;;
;;; type :
;;;     The object type that implements the accessible for widget_class
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_accessible_role ()
;;;
;;; void gtk_widget_class_set_accessible_role (GtkWidgetClass *widget_class,
;;;                                            AtkRole role);
;;;
;;; Sets the default AtkRole to be set on accessibles created for widgets of
;;; widget_class. Accessibles may decide to not honor this setting if their role
;;; reporting is more refined. Calls to gtk_widget_class_set_accessible_type()
;;; will reset this value.
;;;
;;; In cases where you want more fine-grained control over the role of
;;; accessibles created for widget_class, you should provide your own accessible
;;; type and use gtk_widget_class_set_accessible_type() instead.
;;;
;;; If role is ATK_ROLE_INVALID, the default role will not be changed and the
;;; accessible's default role will be used instead.
;;;
;;; This function should only be called from class init functions of widgets.
;;;
;;; widget_class :
;;;     class to set the accessible role for
;;;
;;; role :
;;;     The role to use for accessibles created for widget_class
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_accessible () -> widget-accessible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_accessible" widget-accessible) g:object
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @code{AtkObject} associated with @arg{widget}.}
  @begin{short}
    Returns the accessible object that describes the widget to an assistive
    technology.
  @end{short}
  If accessibility support is not available, this @code{AtkObject} instance may
  be a no-op. Likewise, if no class-specific @code{AtkObject} implementation is
  available for the widget instance in question, it will inherit an
  @code{AtkObject} implementation from the first ancestor class for which such
  an implementation is defined.

  The documentation of the ATK library contains more information about
  accessible objects and their uses.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-accessible)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_child_focus" widget-child-focus) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a @symbol{gtk:direction-type} value with the direction
    of focus movement}
  @return{@em{True} if focus ended up inside widget.}
  @begin{short}
    This function is used by custom widget implementations.
  @end{short}
  If you are writing an application, you would use the
  @fun{gtk:widget-grab-focus} function to move the focus to a particular widget,
  and the @fun{gtk:container-focus-chain} function to change the focus tab
  order. So you may want to investigate those functions instead.

  The @fun{gtk:widget-child-focus} function is called by containers as the user
  moves around the window using keyboard shortcuts. The @arg{direction} argument
  indicates what kind of motion is taking place (up, down, left, right, tab
  forward, tab backward). The @fun{gtk:widget-child-focus} function emits the
  @code{\"focus\"} signal. Widgets override the default handler for this signal
  in order to implement appropriate focus behavior.

  The default @code{\"focus\"} handler for a widget should return @em{true} if
  moving in direction left the focus on a focusable location inside that widget,
  and @em{false} if moving in direction moved the focus outside the widget. If
  returning @em{true}, widgets normally call the @fun{gtk:widget-grab-focus}
  function to place the focus accordingly. If returning @em{false}, they do not
  modify the current focus location.
  @see-class{gtk:widget}
  @see-symbol{gtk:direction-type}
  @see-function{gtk:widget-grab-focus}
  @see-function{gtk:container-focus-chain}"
  (widget (g:object widget))
  (direction direction-type))

(export 'widget-child-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_notify ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_child_notify" widget-child-notify) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[property]{a string with the name of a child property installed
    on the class of the parent of @arg{widget}}
  @begin{short}
    Emits a @code{\"child-notify\"} signal for the child property @arg{property}
    on @arg{widget}.
  @end{short}
  This is the analogue of the @fun{g:object-notify} function for child
  properties. Also see the @fun{gtk:container-child-notify} function.
  @see-class{gtk:widget}
  @see-function{g:object-notify}
  @see-function{gtk:container-child-notify}"
  (widget (g:object widget))
  (property :string))

(export 'widget-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_freeze_child_notify ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_freeze_child_notify" widget-freeze-child-notify)
    :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Stops emission of @code{\"child-notify\"} signals on the widget.
  @end{short}
  The signals are queued until the @fun{gtk:widget-thaw-child-notify} function
  is called on the widget.

  This is the analogue of the @fun{g:object-freeze-notify} function for child
  properties.
  @see-class{gtk:widget}
  @see-function{gtk:widget-thaw-child-notify}
  @see-function{g:object-freeze-notify}"
  (widget (g:object widget)))

(export 'widget-freeze-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_visible ()
;;; gtk_widget_set_child_visible () -> widget-child-visible
;;; ----------------------------------------------------------------------------

(defun (setf widget-child-visible) (is-visible widget)
  (cffi:foreign-funcall "gtk_widget_set_child_visible"
                        (g:object widget) widget
                        :boolean is-visible
                        :void)
  is-visible)

(cffi:defcfun ("gtk_widget_get_child_visible" widget-child-visible) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-child-visible widget) => visible}
  @syntax{(setf (gtk:widget-child-visible widget) visible)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[visible]{if @em{true}, @arg{widget} should be mapped along with its
    parent}
  @begin{short}
    The @fun{gtk:widget-child-visible} function returns @em{true} if the widget
    is mapped with the parent.
  @end{short}
  The @setf{gtk:widget-child-visible} function sets whether the widget should
  be mapped along with its parent when its parent is mapped and the widget has
  been shown with the @fun{gtk:widget-show} function.

  The child visibility can be set for the widget before it is added to a
  container with the @fun{gtk:widget-parent} function, to avoid mapping children
  unnecessary before immediately unmapping them. However it will be reset to its
  default state of @em{true} when the widget is removed from a container.

  Note that changing the child visibility of a widget does not queue a resize
  on the widget. Most of the time, the size of a widget is computed from all
  visible children, whether or not they are mapped. If this is not the case,
  the container can queue a resize itself.

  This function is only useful for container implementations and never should
  be called by an application.
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-parent}"
  (widget (g:object widget)))

(export 'widget-child-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_settings () -> widget-settings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_settings" widget-settings) (g:object settings)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The relevant @class{gtk:settings} object.}
  @begin{short}
    Gets the settings object holding the settings used for this widget.
  @end{short}
  Note that this function can only be called when the widget is attached to a
  toplevel, since the settings object is specific to a particular
  @class{gdk:screen} object.
  @see-class{gtk:widget}
  @see-class{gtk:settings}
  @see-class{gdk:screen}"
  (widget (g:object widget)))

(export 'widget-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_clipboard () -> widget-clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_clipboard" widget-clipboard) (g:object clipboard)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[selection]{an atom as a string which identifies the clipboard to
    use, @code{\"CLIPBOARD\"} gives the default clipboard, another common value
    is @code{\"PRIMARY\"}, which gives the primary X selection}
  @return{The appropriate @class{gtk:clipboard} object. If no clipboard already
    exists, a new one will be created. Once a clipboard object has been created,
    it is persistent for all time.}
  @begin{short}
    Returns the clipboard object for the given selection to be used with the
    widget.
  @end{short}
  The @arg{widget} argument must have a @class{gdk:display} object associated
  with it, so must be attached to a toplevel window.
  @see-class{gtk:widget}
  @see-class{gtk:clipboard}
  @see-class{gdk:display}"
  (widget (g:object widget))
  (selection gdk:atom-as-string))

(export 'widget-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_display () -> widget-display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_display" widget-display) (g:object gdk:display)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:display} object for the toplevel for this widget.}
  @begin{short}
    Get the display for the toplevel window associated with the widget.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk:window} widget at the top.

  In general, you should only create display specific resources when a widget
  has been realized, and you should free those resources when the widget is
  unrealized.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gdk:display}"
  (widget (g:object widget)))

(export 'widget-display)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_root_window () -> widget-root-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_root_window" widget-root-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:window} root window for the toplevel for this widget.}
  @begin{short}
    Get the root window where the widget is located.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk:window} widget at the top.

  The root window is useful for such purposes as creating a popup
  @class{gdk:window} object associated with the window. In general, you should
  only create display specific resources when a widget has been realized, and
  you should free those resources when the widget is unrealized.
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-root-window} function has been deprecated since version
    3.12 and should not be used in newly written code. Use the
    @fun{gdk:screen-root-window} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-function{gdk:screen-root-window}"
  (widget (g:object widget)))

(export 'widget-root-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_screen () -> widget-screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_screen" widget-screen) (g:object gdk:screen)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:screen} object for the toplevel for the widget.}
  @begin{short}
    Get the screen from the toplevel window associated with the widget.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk:window} widget at the top.

  In general, you should only create screen specific resources when a widget
  has been realized, and you should free those resources when the widget is
  unrealized.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gdk:screen}
  @see-function{gtk:widget-has-screen}"
  (widget (g:object widget)))

(export 'widget-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_screen ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_has_screen" widget-has-screen) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @em{True} if there is a @class{gdk:screen} object associcated with the
    widget.
  @end{return}
  @begin{short}
    Checks whether there is a screen associated with the widget.
  @end{short}
  All toplevel widgets have an associated screen, and all widgets added into a
  hierarchy with a toplevel window at the top.
  @see-class{gtk:widget}
  @see-function{gtk:widget-screen}"
  (widget (g:object widget)))

(export 'widget-has-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_size_request ()
;;; gtk_widget_set_size_request () -> widget-size-request
;;; ----------------------------------------------------------------------------

(defun (setf widget-size-request) (size widget)
  (destructuring-bind (width height) size
    (cffi:foreign-funcall "gtk_widget_set_size_request"
                     (g:object widget) widget
                     :int width
                     :int height
                     :void)
    (values width height)))

(cffi:defcfun ("gtk_widget_get_size_request" %widget-size-request) :void
  (widget (g:object widget))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun widget-size-request (widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-size-request object) => width, height}
  @syntax{(setf (gtk:widget-size-request object) (list width height))}
  @argument[object]{a @class{gtk:widget} object}
  @argument[width]{an integer with the width}
  @argument[height]{an integer with the height}
  @begin{short}
    Accessor of the size request of the widget.
  @end{short}
  A value of -1 returned in @arg{width} or @arg{height} indicates that that
  dimension has not been set explicitly and the natural requisition of the
  widget will be used instead. To get the size a widget will actually request,
  call the @fun{gtk:widget-preferred-size} function instead of this function.

  The @setf{gtk:widget-size-request} function sets the minimum size of a widget.
  That is, the size request of the widget will be @arg{width} by @arg{height}.
  You can use this function to force a widget to be either larger or smaller
  than it normally would be.

  In most cases, the @fun{gtk:window-default-size} function is a better choice
  for toplevel windows than this function. Setting the default size will still
  allow users to shrink the window. Setting the size request will force them to
  leave the window at least as large as the size request. When dealing with
  window sizes, the @fun{gtk:window-set-geometry-hints} function can be a
  useful function as well.

  Note the inherent danger of setting any fixed size - themes, translations
  into other languages, different fonts, and user action can all change the
  appropriate size for a given widget. So, it is basically impossible to
  hardcode a size that will always be correct.

  The size request of a widget is the smallest size a widget can accept while
  still functioning well and drawing itself correctly. However in some strange
  cases a widget may be allocated less than its requested size, and in many
  cases a widget may be allocated more space than it requested.

  If the size request in a given direction is -1 (unset), then the \"natural\"
  size request of the widget will be used instead.

  Widgets cannot actually be allocated a size less than 1 by 1, but you can
  pass 0 by 0 to this function to mean \"as small as possible\".

  The size request set here does not include any margin from the
  @slot[gtk:widget]{margin-start}, @slot[gtk:widget]{margin-end},
  @slot[gtk:widget]{margin-top}, and @slot[gtk:widget]{margin-bottom}
  properties, but it does include pretty much all other padding or border
  properties set by any subclass of the @class{gtk:widget} class.
  @see-class{gtk:widget}
  @see-function{gtk:widget-preferred-size}
  @see-function{gtk:window-default-size}
  @see-function{gtk:window-set-geometry-hints}
  @see-function{gtk:widget-margin-start}
  @see-function{gtk:widget-margin-end}
  @see-function{gtk:widget-margin-top}
  @see-function{gtk:widget-margin-bottom}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%widget-size-request widget width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'widget-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_thaw_child_notify ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_thaw_child_notify" widget-thaw-child-notify) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Reverts the effect of a previous call to the
    @fun{gtk:widget-freeze-child-notify} function.
  @end{short}
  This causes all queued @code{\"child-notify\"} signals on the widget to be
  emitted.
  @see-class{gtk:widget}
  @see-function{gtk:widget-freeze-child-notify}"
  (widget (g:object widget)))

(export 'widget-thaw-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_mnemonic_labels ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_list_mnemonic_labels" widget-list-mnemonic-labels)
    (g:list-t (g:object widget))
 #+liber-documentation
 "@version{2023-6-17}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The list of @class{gtk:widget} mnemonic labels.}
  @begin{short}
    Returns a list of the widgets, normally labels, for which this widget is
    the target of a mnemonic.
  @end{short}
  See for example the @fun{gtk:label-mnemonic-widget} function for more
  information about mnemonic labels.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq button (gtk:button-new-with-mnemonic \"_Hello\"))
=> #<GTK:BUTTON {C2794C9@}>
(gtk:widget-list-mnemonic-labels button)
=> (#<gtk:LABEL {C292FE1@}>)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-mnemonic-label}
  @see-function{gtk:label-mnemonic-widget}"
  (widget (g:object widget)))

(export 'widget-list-mnemonic-labels)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_mnemonic_label ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_mnemonic_label" widget-add-mnemonic-label) :void
 #+liber-documentation
 "@version{2023-6-17}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[label]{a @class{gtk:widget} object that acts as a mnemonic label
    for @arg{widget}}
  @begin{short}
    Adds a widget to the list of mnemonic labels for this widget.
  @end{short}
  See the @fun{gtk:widget-list-mnemonic-labels} function for a list of mnemonic
  labels for this widget.

  Note the list of mnemonic labels for the widget is cleared when the widget is
  destroyed, so the caller must make sure to update its internal state at this
  point as well, by using a connection to the @code{\"destroy\"} signal or a
  weak notifier.
  @see-class{gtk:widget}
  @see-function{gtk:widget-list-mnemonic-labels}
  @see-function{gtk:widget-remove-mnemonic-label}"
  (widget (g:object widget))
  (label (g:object widget)))

(export 'widget-add-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_mnemonic_label ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_mnemonic_label"
                widget-remove-mnemonic-label) :void
 #+liber-documentation
 "@version{2023-6-17}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[label]{a @class{gtk:widget} object that was previously set as a
    mnemonic label for @arg{widget}}
  @begin{short}
    Removes a widget from the list of mnemonic labels for this widget.
  @end{short}
  See the @fun{gtk:widget-list-mnemonic-labels} function for a list of mnemonic
  labels for the widget. The widget must have previously been added to the list
  with the @fun{gtk:widget-add-mnemonic-label} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-mnemonic-label}
  @see-function{gtk:widget-list-mnemonic-labels}"
  (widget (g:object widget))
  (label (g:object widget)))

(export 'widget-remove-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_composited ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_composited" widget-is-composited) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} can rely on its alpha channel being drawn
    correctly.}
  @begin{short}
    Whether the widget can rely on having its alpha channel drawn correctly.
  @end{short}
  On X11 this function returns whether a compositing manager is running for
  the screen of the widget.

  Please note that the semantics of this call will change in the future if
  used on a widget that has a composited window in its hierarchy as set by the
  @fun{gdk:window-composited} function.
  @see-class{gtk:widget}
  @see-function{gdk:window-composited}"
  (widget (g:object widget)))

(export 'widget-is-composited)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_error_bell ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_error_bell" widget-error-bell) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Notifies the user about an input-related error on this widget.
  @end{short}
  If the @slot[gtk:settings]{gtk-error-bell} setting is @em{true}, it calls the
  @fun{gdk:window-beep} function, otherwise it does nothing.

  Note that the effect of the @fun{gdk:window-beep} function can be configured
  in many ways, depending on the windowing backend and the desktop environment
  or window manager that is used.
  @see-class{gtk:widget}
  @see-function{gdk:window-beep}
  @see-function{gtk:settings-gtk-error-bell}"
  (widget (g:object widget)))

(export 'widget-error-bell)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_keynav_failed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_keynav_failed" widget-keynav-failed) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a @symbol{gtk:direction-type} value for the direction
    of focus movement}
  @return{@em{True} if stopping keyboard navigation is fine, @em{false} if the
    emitting widget should try to handle the keyboard navigation attempt in its
    parent container(s).}
  @begin{short}
    This function should be called whenever keyboard navigation within a single
    widget hits a boundary.
  @end{short}
  The function emits the @code{\"keynav-failed\"} signal on the widget and its
  return value should be interpreted in a way similar to the return value of the
  @fun{gtk:widget-child-focus} function:
  @begin{itemize}
    @item{When @em{true} is returned, stay in the widget, the failed keyboard
      navigation is Ok and/or there is nowhere we can/should move the focus
      to.}
    @item{When @em{false} is returned, the caller should continue with keyboard
      navigation outside the widget, e.g. by calling the
      @fun{gtk:widget-child-focus} function on the toplevel of the widget.}
  @end{itemize}
  The default @code{\"keynav-failed\"} handler returns @em{true} for
  @code{:tab-forward} and @code{:tab-backward}. For the other
  @symbol{gtk:direction-type} values, it looks at the
  @slot[gtk:settings]{gtk-keynav-cursor-only} setting and returns @em{false}
  if the setting is @em{true}. This way the entire user interface becomes
  cursor-navigatable on input devices such as mobile phones which only have
  cursor keys but no tab key.

  Whenever the default handler returns @em{true}, it also calls the
  @fun{gtk:widget-error-bell} function to notify the user of the failed
  keyboard navigation.

  A use case for providing an own implementation of \"keynav-failed\", either
  by connecting to it or by overriding it, would be a row of @class{gtk:entry}
  widgets where the user should be able to navigate the entire row with the
  cursor keys, as e.g. known from user interfaces that require entering license
  keys.
  @see-class{gtk:widget}
  @see-function{gtk:widget-child-focus}
  @see-function{gtk:widget-error-bell}
  @see-function{gtk:settings-gtk-keynav-cursor-only}"
  (widget (g:object widget))
  (direction direction-type))

(export 'widget-keynav-failed)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_tooltip_window ()
;;; gtk_widget_set_tooltip_window () -> widget-tooltip-window
;;; ----------------------------------------------------------------------------

(defun (setf widget-tooltip-window) (custom-window widget)
  (cffi:foreign-funcall "gtk_widget_set_tooltip_window"
                        (g:object widget) widget
                        (g:object window) custom-window
                        :void)
  custom-window)

(cffi:defcfun ("gtk_widget_get_tooltip_window" widget-tooltip-window)
    (g:object window)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-tooltip-window widget) => window}
  @syntax{(setf (gtk:widget-tooltip-window widget) window)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[window]{a @class{gtk:window} widget, or @arg{nil}}
  @begin{short}
    Accessor of the custom window of the current tooltip.
  @end{short}
  The @fun{gtk:widget-tooltip-window} function returns the custom windw of the
  current tooltip. The @setf{gtk:widget-tooltip-window} function replaces the
  default, usually yellow, window used for displaying tooltips with the custom
  window.

  GTK will take care of showing and hiding the custom window at the right
  moment, to behave likewise as the default tooltip window. If the custom
  window is @code{nil}, the default tooltip window will be used.

  If the custom window should have the default theming it needs to have the
  name \"gtk-tooltip\", see the @fun{gtk:widget-name} function.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gtk:widget-name}"
  (widget (g:object window)))

(export 'widget-tooltip-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_trigger_tooltip_query ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_trigger_tooltip_query" widget-trigger-tooltip-query)
    :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Triggers a tooltip query on the display where the toplevel of @arg{widget}
    is located.
  @end{short}
  See the @fun{gtk:tooltip-trigger-tooltip-query} function for more information.
  @see-class{gtk:widget}
  @see-function{gtk:tooltip-trigger-tooltip-query}"
  (widget (g:object widget)))

(export 'widget-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_register_window ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_register_window" widget-register-window) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Registers a GDK window with the widget and sets it up so that the widget
    recieves events for it.
  @end{short}
  Call the @fun{gtk:widget-unregister-window} function when destroying the
  GDK window.

  Before 3.8 you needed to call the @fun{gdk:window-user-data} function
  directly to set this up. This is now deprecated and you should use the
  @fun{gtk:widget-register-window} function instead. Old code will keep working
  as is, although some new features like transparency might not work perfectly.
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-function{gtk:widget-unregister-window}
  @see-function{gdk:window-user-data}"
  (widget (g:object widget))
  (window (g:object gdk:window)))

(export 'widget-register-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unregister_window ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unregister_window" widget-unregister-window) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Unregisters a GDK window from the widget that was previously set up with
    the @fun{gtk:widget-register-window} function.
  @end{short}
  You need to call this when the window is no longer used by the widget, such
  as when you destroy it.
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-function{gtk:widget-register-window}"
  (widget (g:object widget))
  (window (g:object gdk:window)))

(export 'widget-unregister-window)

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_should_draw_window ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cairo_should_draw_window" cairo-should-draw-window) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[window]{a @class{gdk:window} object to check, @arg{window} may
    not be an input-only window}
  @return{@em{True} if @arg{window} should be drawn.}
  @begin{short}
    This function is supposed to be called in \"draw\" implementations for
    widgets that support multiple windows.
  @end{short}
  The @arg{cr} argument must be untransformed from invoking of the draw
  function. This function will return @em{true} if the contents of the given
  window are supposed to be drawn and @em{false} otherwise. Note that when the
  drawing was not initiated by the windowing system this function will return
  @em{true} for all windows, so you need to draw the bottommost window first.
  Also, do not use \"else if\" statements to check which window should be drawn.
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct cairo:context-t)))
  (window (g:object gdk:window)))

(export 'cairo-should-draw-window)

;;; ----------------------------------------------------------------------------
;;; gtk_cairo_transform_to_window ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cairo_transform_to_window" cairo-transform-to-window) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[cr]{a @class{cairo:context-t} context to transform}
  @argument[widget]{the @class{gtk:widget} object the context is currently
    centered for}
  @argument[window]{the @class{gdk:window} object to transform the context to}
  @begin{short}
    Transforms the given Cairo context from widget-relative coordinates to
    window-relative coordinates.
  @end{short}
  If the window of the widget is not an ancestor of @arg{window}, no
  modification will be applied.

  This is the inverse to the transformation GTK applies when preparing an
  expose event to be emitted with the @code{\"draw\"} signal. It is intended to
  help porting multiwindow widgets from GTK 2 to the rendering architecture of
  GTK 3.
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-symbol{cairo:context-t}"
  (cr (:pointer (:struct cairo:context-t)))
  (widget (g:object widget))
  (window (g:object gdk:window)))

(export 'cairo-transform-to-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_width () -> widget-allocated-width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_width" widget-allocated-width) :int
 #+liber-documentation
 "@version{2023-12-26}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the width of the widget.}
  @begin{short}
    Returns the width that has currently been allocated to @arg{widget}.
  @end{short}
  This function is intended to be used when implementing handlers for the
  \"draw\" function.
  @begin[Notes]{dictionary}
    The @fun{gtk:widget-allocated-width} function is equivalent to the call
    @begin{pre}
(gdk:rectangle-width (gtk:widget-allocation widget))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-allocated-height}
  @see-function{gtk:widget-allocation}"
  (widget (g:object widget)))

(export 'widget-allocated-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_height () -> widget-allocated-height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_height" widget-allocated-height) :int
 #+liber-documentation
 "@version{2023-12-26}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the height of the widget.}
  @begin{short}
    Returns the height that has currently been allocated to @arg{widget}.
  @end{short}
  This function is intended to be used when implementing handlers for the
  \"draw\" function.
  @begin[Notes]{dictionary}
    The @fun{gtk:widget-allocated-height} function is equivalent to the call
    @begin{pre}
(gdk:rectangle-height (gtk:widget-allocation widget))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-allocated-width}
  @see-function{gtk:widget-allocation}"
  (widget (g:object widget)))

(export 'widget-allocated-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocation ()
;;; gtk_widget_set_allocation () -> widget-allocation
;;; ----------------------------------------------------------------------------

;; With the type gtk:allocation we get an error.
;; It works with the type gdk:rectangle. In the C implementation the
;; new type GtkAllocation is a synonym for GdkRectangle

(defun (setf widget-allocation) (allocation widget)
  (cffi:foreign-funcall "gtk_widget_set_allocation"
                        (g:object widget) widget
                        (g:boxed gdk:rectangle) allocation
                        :void)
  allocation)

(cffi:defcfun ("gtk_widget_get_allocation" %widget-allocation) :void
  (widget (g:object widget))
  (allocation (g:boxed gdk:rectangle)))

(defun widget-allocation (widget)
 #+liber-documentation
 "@version{2023-12-26}
  @syntax{(gtk:widget-allocation widget) => allocation}
  @syntax{(setf (gtk:widget-allocation widget) allocation}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[allocation]{a @class{gdk:rectangle} instance}
  @begin{short}
    Accessor of the allocation of the widget.
  @end{short}
  The @fun{gtk:widget-allocation} function retrieves the allocation of the
  widget. The @setf{gtk:widget-allocation} function sets the allocation. This
  should not be used directly, but from within a @code{size_allocate} method of
  the widget.

  Note, when implementing a @class{gtk:container} widget an allocation of the
  widget will be its \"adjusted\" allocation, that is, the parent container of
  the widget typically calls the @fun{gtk:widget-size-allocate} function with an
  allocation, and that allocation is then adjusted, to handle margin and
  alignment for example, before assignment to the widget. The
  @fun{gtk:widget-allocation} function returns the adjusted allocation that was
  actually assigned to the widget. The adjusted allocation is guaranteed to be
  completely contained within the @fun{gtk:widget-size-allocate} allocation,
  however. So a @class{gtk:container} widget is guaranteed that its children
  stay inside the assigned bounds, but not that they have exactly the bounds the
  container assigned. There is no way to get the original allocation assigned by
  the @fun{gtk:widget-size-allocate} function, since it is not stored. If a
  container implementation needs that information it will have to track it
  itself.

  The allocation set should be the \"adjusted\" or actual allocation. If you
  are implementing a @class{gtk:container} widget, you want to use the
  @fun{gtk:widget-size-allocate} function instead of the
  @fun{gtk:widget-allocation} function. The
  @code{GtkWidgetClass::adjust_size_allocation} virtual method adjusts the
  allocation inside the @fun{gtk:widget-size-allocate} function to create an
  adjusted allocation.
  @begin[Notes]{dictionary}
    In the Lisp binding to GTK this function does not return an allocation
    of type @code{GtkAllocation}, but the type is @class{gdk:rectangle}. In the
    C implementation the type @code{GtkAllocation} is a synonym for the
    @class{gdk:rectangle} type.

    In most cases, it is more convenient to use the
    @fun{gtk:widget-allocated-width} and @fun{gtk:widget-allocated-height}
    functions to retrieve the allocated width and height of the widget.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-size-allocate}
  @see-function{gtk:widget-allocated-width}
  @see-function{gtk:widget-allocated-height}"
  (let ((allocation (gdk:rectangle-new)))
    (%widget-allocation widget allocation)
    allocation))

(export 'widget-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_baseline () -> widget-allocated-baseline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_baseline" widget-allocated-baseline)
    :int
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the baseline of the widget, or -1 if none.}
  @begin{short}
    Returns the baseline that has currently been allocated to the widget.
  @end{short}
  This function is intended to be used when implementing handlers for the
  \"draw\" function, and when allocating child widgets in \"size_allocate\".
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-allocated-baseline)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_clip ()
;;; gtk_widget_set_clip () -> widget-clip
;;; ----------------------------------------------------------------------------

(defun (setf widget-clip) (clip widget)
  (cffi:foreign-funcall "gtk_widget_set_clip"
                        (g:object widget) widget
                        (g:boxed gdk:rectangle) clip
                        :void)
  clip)

(cffi:defcfun ("gtk_widget_get_clip" %widget-clip) :void
  (widget (g:object widget))
  (clip (g:boxed gdk:rectangle)))

(defun widget-clip (widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-clip widget) => clip}
  @syntax{(setf (gtk:widget-clip widget) clip)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[clip]{a @class{gdk:rectangle} instance}
  @begin{short}
   The @fun{gtk:widget-clip} function retrieves the clip area of the widget.
  @end{short}
  The @setf{gtk:widget-clip} function sets the clip of the widget. The clip
  area is the area in which all of the drawing of the widget will happen. Other
  toolkits call it the bounding box. Historically, in GTK the clip area has
  been equal to the allocation retrieved via the @fun{gtk:widget-allocation}
  function.

  This must not be used directly, but from within a @code{size_allocate} method
  of the widget. It must be called after the @fun{gtk:widget-allocation}
  function, or after chaning up to the parent class, because that function
  resets the clip.

  The clip set should be the area that the widget draws on. If the widget is a
  @class{gtk:container} widget, the area must contain all clips of the
  children.

  If this function is not called by the widget during a \"size-allocate\"
  handler, the clip will be set to the allocation of the widget.
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-allocation}"
  (let ((clip (gdk:rectangle-new)))
    (%widget-clip widget clip)
    clip))

(export 'widget-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_has_window ()
;;; gtk_widget_set_has_window () -> widget-has-window
;;; ----------------------------------------------------------------------------

(defun (setf widget-has-window) (has-window widget)
  (cffi:foreign-funcall "gtk_widget_set_has_window"
                        (g:object widget) widget
                        :boolean has-window
                        :void)
  has-window)

(cffi:defcfun ("gtk_widget_get_has_window" widget-has-window) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-has-window widget) => has-window}
  @syntax{(setf (gtk:widget-has-window widget) has-window)}
  @argument[widget]{a @class{gtk:widget} instance}
  @argument[has-window]{a boolean whether or not @arg{widget} has a window}
  @begin{short}
    Accessor of the has window value of the @class{gtk:widget} object.
  @end{short}
  The @fun{gtk:widget-has-window} function determines whether the widget has a
  @class{gdk:window} object of its own. The @setf{gtk:widget-has-window}
  function specifies whether the widget has a @class{gdk:window} of its own.

  Note that all realized widgets have a non-@code{NULL} \"window\" pointer, the
  @fun{gtk:widget-window} function never returns a @code{NULL} window when a
  widget is realized, but for many of them it is actually the @class{gdk:window}
  object of one of its parent widgets. Widgets that do not create a window for
  themselves in \"realize\" must announce this by calling this function with
  the value @em{false}.

  This function should only be called by widget implementations, and they
  should call it in their @code{init()} function.
  @see-class{gtk:widget}
  @see-class{gdk:window}
  @see-function{gtk:widget-window}"
  (widget (g:object widget)))

(export 'widget-has-window)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_sensitive ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_sensitive" widget-is-sensitive) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is effectively sensitive.}
  @begin{short}
    Returns the widgets effective sensitivity, which means it is sensitive
    itself and also its parent widget is sensitive.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-sensitive}"
  (widget (g:object widget)))

(export 'widget-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_visible ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_visible" widget-is-visible) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget and all its parents are visible.}
  @begin{short}
    Determines whether the widget and all its parents are marked as visible.
  @end{short}
  This function does not check if the widget is obscured in any way. See also
  the @fun{gtk:widget-visible} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-visible}"
  (widget (g:object widget)))

(export 'widget-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state_flags ()
;;; gtk_widget_set_state_flags () -> widget-state-flags
;;; ----------------------------------------------------------------------------

(defun (setf widget-state-flags) (flags widget &optional (clear nil))
  (cffi:foreign-funcall "gtk_widget_set_state_flags"
                        (g:object widget) widget
                        state-flags flags
                        :boolean clear
                        :void)
  flags)

(cffi:defcfun ("gtk_widget_get_state_flags" widget-state-flags) state-flags
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-state-flags widget) => flags}
  @syntax{(setf (gtk:widget-state-flags widget clear) flags)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[flags]{a @symbol{gtk:state-flags} value with the state flags}
  @argument[clear]{an optional boolean whether to clear state before turning on
    flags}
  @begin{short}
    Accessor of the stage flags of the widget.
  @end{short}
  The @fun{gtk:widget-state-flags} function returns the widget state as a flag
  set. The @setf{gtk:widget-state-flags} function sets the widget state flags.
  This function is for use in widget implementations. Turns on flag values in
  the current widget state, insensitive, prelighted, etc.

  It is worth mentioning that any other state than @code{:insensitive}, will be
  propagated down to all non-internal children if the widget is a
  @class{gtk:container} widget, while @code{:insensitive} itself will be
  propagated down to all @class{gtk:container} children by different means than
  turning on the state flag down the hierarchy, both
  @fun{gtk:widget-state-flags} and @fun{gtk:widget-is-sensitive} functions will
  make use of these.
  @see-class{gtk:widget}
  @see-class{gtk:container}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-is-sensitive}"
  (widget (g:object widget)))

(export 'widget-state-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unset_state_flags ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unset_state_flags" widget-unset-state-flags) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[flags]{a @symbol{gtk:state-flags} value with state flags to turn
    off}
  @begin{short}
    This function is for use in widget implementations.
  @end{short}
  Turns off flag values for the current widget state, insensitive, prelighted,
  etc. See the @fun{gtk:widget-state-flags} function.
  @see-class{gtk:widget}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-state-flags}"
  (widget (g:object widget))
  (flags state-flags))

(export 'widget-unset-state-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_visible_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_has_visible_focus" widget-has-visible-focus) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget should display a focus rectangle.}
  @begin{short}
    Determines if the widget should show a visible indication that it has the
    global input focus.
  @end{short}
  This is a convenience function for use in @code{\"draw\"} handlers that takes
  into account whether focus indication should currently be shown in the
  toplevel window of the widget. See the @fun{gtk:window-focus-visible} function
  for more information about focus indication.

  To find out if the widget has the global input focus, use the
  @fun{gtk:widget-has-focus} function.
  @see-class{gtk:widget}
  @see-function{gtk:window-focus-visible}
  @see-function{gtk:widget-has-focus}"
  (widget (g:object widget)))

(export 'widget-has-visible-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_grab ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_has_grab" widget-has-grab) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget is the grab widgets stack.}
  @begin{short}
    Determines whether the widget is currently grabbing events, so it is the
    only widget receiving input events, keyboard and mouse.
  @end{short}
  See also the @fun{gtk:grab-add} function.
  @see-class{gtk:widget}
  @see-function{gtk:grab-add}"
  (widget (g:object widget)))

(export 'widget-has-grab)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_rc_style ()
;;;
;;; gboolean gtk_widget_has_rc_style (GtkWidget *widget);
;;;
;;; Warning
;;;
;;; gtk_widget_has_rc_style has been deprecated since version 3.0 and should not
;;; be used in newly written code. Use GtkStyleContext instead.
;;;
;;; Determines if the widget style has been looked up through the rc mechanism.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     TRUE if the widget has been looked up through the rc mechanism, FALSE
;;;     otherwise.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_drawable ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_drawable" widget-is-drawable) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is drawable, @em{false} otherwise.}
  @begin{short}
    Determines whether the widget can be drawn to.
  @end{short}
  A widget can be drawn to if it is mapped and visible.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-is-drawable)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_toplevel ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_toplevel" widget-is-toplevel) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is a toplevel, @em{false} otherwise.}
  @begin{short}
    Determines whether the widget is a toplevel widget.
  @end{short}
  Currently only @class{gtk:window}, @class{gtk:invisible}, and
  out-of-process @class{gtk:plug} widgets are toplevel widgets. Toplevel
  widgets have no parent widget.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gtk:invisible}
  @see-class{gtk:plug}"
  (widget (g:object widget)))

(export 'widget-is-toplevel)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_support_multidevice ()
;;; gtk_widget_set_support_multidevice () -> widget-support-multidevice
;;; ----------------------------------------------------------------------------

(defun (setf widget-support-multidevice) (support-multidevice widget)
  (cffi:foreign-funcall "gtk_widget_set_support_multidevice"
                        (g:object widget) widget
                        :boolean support-multidevice
                        :void)
  support-multidevice)

(cffi:defcfun ("gtk_widget_get_support_multidevice" widget-support-multidevice)
    :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-support-multidecice widget) => support-multidevice}
  @syntax{(setf (gtk:widget-support-multidevice widget) support-multidevice)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[support-multidevice]{@em{true} to support input from multiple
    devices}
  @begin{short}
    The @fun{gtk:widget-support-multidevice} function returns @em{true} if
    @arg{widget} is multidevice aware.
  @end{short}
  The @setf{gtk:widget-support-multidevice} function enables or disables
  multiple pointer awareness.

  If this setting is @em{true}, @arg{widget} will start receiving multiple,
  per device enter/leave events. Note that if custom @class{gdk:window}
  objects are created in \"realize\", the @fun{gdk:window-support-multidevice}
  function will have to be called manually on them.
  @see-class{gtk:widget}
  @see-function{gdk:window-support-multidevice}"
  (widget (g:object widget)))

(export 'widget-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_realized ()
;;; gtk_widget_set_realized () -> widget-realized
;;; ----------------------------------------------------------------------------

(defun (setf widget-realized) (realized widget)
  (cffi:foreign-funcall "gtk_widget_set_realized"
                        (g:object widget) widget
                        :boolean realized
                        :void)
  realized)

(cffi:defcfun ("gtk_widget_get_realized" widget-realized) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-realized widget) => realized}
  @syntax{(setf (gtk:widget-realized widget) realized)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[realized]{@em{true} to mark the widget as realized}
  @begin{short}
    The @fun{gtk:widget-realized} function determines whether the widget is
    realized.
  @end{short}
  The @setf{gtk:widget-realized} function marks the widget as being realized.

  This function should only ever be called in a derived \"realize\" or
  \"unrealize\" implementation of the widget.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-realized)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_mapped ()
;;; gtk_widget_set_mapped () -> widget-mapped
;;; ----------------------------------------------------------------------------

(defun (setf widget-mapped) (mapped widget)
  (cffi:foreign-funcall "gtk_widget_set_mapped"
                        (g:object widget) widget
                        :boolean mapped
                        :void)
  mapped)

(cffi:defcfun ("gtk_widget_get_mapped" widget-mapped) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @syntax{(gtk:widget-mapped widget) => mapped}
  @syntax{(setf (gtk:widget-mapped widget) mapped)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[mapped]{@em{true} to mark the widget as mapped}
  @begin{short}
    The @fun{gtk:widget-mapped} function determines whether the widget is
    mapped.
  @end{short}
  The @setf{gtk:widget-mapped} function marks the widget as being mapped.

  This function should only ever be called in a derived \"map\" or \"unmap\"
  implementation of the widget.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-mapped)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_requisition ()
;;;
;;; void gtk_widget_get_requisition (GtkWidget *widget,
;;;                                  GtkRequisition *requisition);
;;;
;;; Warning
;;;
;;; gtk_widget_get_requisition has been deprecated since version 3.0 and should
;;; not be used in newly written code. The GtkRequisition cache on the widget
;;; was removed, If you need to cache sizes across requests and allocations, add
;;; an explicit cache to the widget in question instead.
;;;
;;; Retrieves the widget's requisition.
;;;
;;; This function should only be used by widget implementations in order to
;;; figure whether the widget's requisition has actually changed after some
;;; internal state change (so that they can call gtk_widget_queue_resize()
;;; instead of gtk_widget_queue_draw()).
;;;
;;; Normally, gtk_widget_size_request() should be used.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; requisition :
;;;     a pointer to a GtkRequisition to copy to
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_device_is_shadowed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_device_is_shadowed" widget-device-is-shadowed)
    :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[device]{a @class{gdk:device} object}
  @return{@em{True} if there is an ongoing grab on @arg{device} by another
    widget than @arg{widget}.}
  @begin{short}
    Returns @em{true} if the device has been shadowed by a GTK device grab on
    another widget, so it would stop sending events to widget.
  @end{short}
  This may be used in the @code{\"grab-notify\"} signal to check for specific
  devices.
  See the @fun{gtk:device-grab-add} function.
  @see-class{gtk:widget}
  @see-class{gdk:device}
  @see-function{gtk:device-grab-add}"
  (widget (g:object widget))
  (device (g:object gdk:device)))

(export 'widget-device-is-shadowed)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_modifier_mask () -> widget-modifier-mask
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_modifier_mask" widget-modifier-mask)
    gdk:modifier-type
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[intent]{a value of the @symbol{gdk:modifier-intent} enumeration}
  @begin{short}
    Returns the modifier mask the windowing system backend of the widget uses
    for a particular purpose.
  @end{short}
  See the @fun{gdk:keymap-modifier-mask} function.
  @see-class{gtk:widget}
  @see-symbol{gdk:modifier-intent}
  @see-function{gdk:keymap-modifier-mask}"
  (widget (g:object widget))
  (intent gdk:modifier-intent))

(export 'widget-modifier-mask)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_insert_action_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_insert_action_group" widget-insert-action-group)
    :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[name]{a string with the prefix for actions in @arg{group}}
  @argument[group]{a @class{g:action-group} object}
  @begin{short}
    Inserts @arg{group} into @arg{widget}.
  @end{short}
  Children of @arg{widget} that implement the @class{gtk:actionable} interface
  can then be associated with actions in @arg{group} by setting their
  @code{action-name} to \"prefix.action-name\".
  @see-class{gtk:widget}
  @see-class{g:action-group}"
  (widget (g:object widget))
  (name :string)
  (group (g:object g:action-group)))

(export 'widget-insert-action-group)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_action_prefixes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_list_action_prefixes" widget-list-action-prefixes)
    g:strv-t
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The list of strings.}
  @begin{short}
    Retrieves a list of strings containing the prefixes of the
    @class{g:action-group} objects available to the widget .
  @end{short}
  @see-class{gtk:widget}
  @see-class{g:action-group}"
  (widget (g:object widget)))

(export 'widget-list-action-prefixes)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_action_group () -> widget-action-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_action_group" widget-action-group)
    (g:object g:action-group)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[prefix]{a string with the \"prefix\" of the action group}
  @return{The @class{g:action-group} object or @code{nil}.}
  @begin{short}
    Retrieves the action group that was registered using @arg{prefix}.
  @end{short}
  The resulting action group may have been registered to the widget or any
  widget in its ancestry.

  If no action group was found matching @arg{prefix}, then @code{nil} is
  returned.
  @see-class{gtk:widget}
  @see-class{g:action-group}"
  (widget (g:object widget))
  (prefix :string))

(export 'widget-action-group)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_path () -> widget-path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_path" widget-path) (g:boxed widget-path)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:widget-path} instance representing the widget.}
  @begin{short}
    Returns the widget path representing the widget.
  @end{short}
  If the widget is not connected to a toplevel widget, a partial path will be
  created.
  @see-class{gtk:widget}
  @see-class{gtk:widget-path}"
  (widget (g:object widget)))

(export 'widget-path)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style_context () -> widget-style-context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_style_context" widget-style-context)
    (g:object style-context)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:style-context} object.}
  @short{Returns the style context associated to the widget.}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}"
  (widget (g:object widget)))

(export 'widget-style-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_reset_style ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_reset_style" widget-reset-style) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Updates the style context of the widget and all descendents by updating its
    widget path.
  @end{short}
  Containers may want to use this on a child widget when reordering it in a way
  that a different style might apply to it. See also the
  @fun{gtk:container-path-for-child} function.
  @see-class{gtk:widget}
  @see-class{gtk:widget-path}
  @see-function{gtk:container-path-for-child}"
  (widget (g:object widget)))

(export 'widget-reset-style)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_get_css_name ()
;;; gtk_widget_class_set_css_name () -> widget-class-css-name
;;; ----------------------------------------------------------------------------

(defun (setf widget-class-css-name) (name gtype)
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:foreign-funcall "gtk_widget_class_set_css_name"
                            :pointer class
                            :string name
                            :void)
      (g:type-class-unref class))
    name))

(cffi:defcfun ("gtk_widget_class_get_css_name" %widget-class-css-name) :string
  (class :pointer))

(defun widget-class-css-name (gtype)
  #+liber-documentation
 "@version{2023-12-2}
  @syntax{(gtk:widget-class-css-name gtype) => name}
  @syntax{(setf (gtk:widget-class-css-name gtype) name)}
  @argument[gtype]{a @class{g:type-t} type ID with the widget class to set the
    CSS name on}
  @argument[name]{a string with the CSS name}
  @begin{short}
    Accessor of the CSS name of the widget class.
  @end{short}
  The @fun{gtk:widget-class-css-name} function gets the name used by this class
  for matching in CSS code. The @setf{gtk:widget-class-css-name} function sets
  the name. If this function is not called for a given class, the name of the
  parent class is used.
  @see-class{gtk:widget}
  @see-class{g:type-t}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-css-name class)
      (g:type-class-unref class))))

(export 'widget-class-css-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_height ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_height" %widget-preferred-height) :void
  (widget (g:object widget))
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun widget-preferred-height (widget)
 #+liber-documentation
 "@version{2023-6-18}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @arg{minimum-height} -- an integer with the minimum height, or @code{nil}
      @br{}
    @arg{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural height of the widget.
  @end{short}
  This call is specific to width-for-height requests.

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk:size-group} objects that have been applied. That is, the returned
  request is the one that should be used for layout, not necessarily the one
  returned by the widget itself.
  @see-class{gtk:widget}
  @see-class{gtk:size-group}
  @see-function{gtk:widget-preferred-width}
  @see-function{gtk:widget-preferred-size}"
  (cffi:with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%widget-preferred-height widget minimum-height natural-height)
    (values (cffi:mem-ref minimum-height :int)
            (cffi:mem-ref natural-height :int))))

(export 'widget-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_width" %widget-preferred-width) :void
  (widget (g:object widget))
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun widget-preferred-width (widget)
 #+liber-documentation
 "@version{2023-6-18}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}
      @br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural width of the widget.
  @end{short}
  This call is specific to height-for-width requests.

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk:size-group} objects that have been applied. That is, the returned
  request is the one that should be used for layout, not necessarily the one
  returned by the widget itself.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq widget (make-instance 'gtk:button :label \"Hello\"))
=> #<GTK:BUTTON {B1D0079@}>
(gtk:widget-preferred-width widget)
=> 49
=> 49
(setq widget (make-instance 'gtk:button :label \"Hello, more text\"))
=> #<GTK:BUTTON {B1D60E9@}>
(gtk:widget-preferred-width widget)
=> 123
=> 123
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:size-group}
  @see-function{gtk:widget-preferred-height}
  @see-function{gtk:widget-preferred-size}"
  (cffi:with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%widget-preferred-width widget minimum-width natural-width)
    (values (cffi:mem-ref minimum-width :int)
            (cffi:mem-ref natural-width :int))))

(export 'widget-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_height_for_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_height_for_width"
                %widget-preferred-height-for-width) :void
  (widget (g:object widget))
  (width :int)
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun widget-preferred-height-for-width (widget width)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[width]{an integer with the width which is available for allocation}
  @begin{return}
    @arg{minimum-height} -- an integer with the minimum height, or @code{nil}
      @br{}
    @arg{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural height of the widget if it would be given
    the specified @arg{width}.
  @end{short}

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk:size-group} objects that have been applied. That is, the returned
  request is the one that should be used for layout, not necessarily the one
  returned by the @arg{widget} itself.
  @see-class{gtk:widget}
  @see-class{gtk:size-group}
  @see-function{gtk:widget-preferred-width-for-height}"
  (cffi:with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%widget-preferred-height-for-width widget
                                        width
                                        minimum-height
                                        natural-height)
    (values (cffi:mem-ref minimum-height :int)
            (cffi:mem-ref natural-height :int))))

(export 'widget-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_width_for_height ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_width_for_height"
                %widget-preferred-width-for-height) :void
  (widget (g:object widget))
  (height :int)
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun widget-preferred-width-for-height (widget height)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[height]{an integer with the height which is available for
    allocation}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}
      @br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural width of the widget if it would be given
    the specified @arg{height}.
  @end{short}

  The returned request will be modified by the
  @code{GtkWidgetClass::adjust_size_request} virtual method and by any
  @class{gtk:size-group} objects that have been applied. That is, the returned
  request is the one that should be used for layout, not necessarily the one
  returned by the @arg{widget} itself.
  @see-class{gtk:widget}
  @see-class{gtk:size-group}
  @see-function{gtk:widget-preferred-height-for-width}"
  (cffi:with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%widget-preferred-width-for-height widget
                                        height
                                        minimum-width
                                        natural-width)
    (values (cffi:mem-ref minimum-width :int)
            (cffi:mem-ref natural-width :int))))

(export 'widget-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_height_and_baseline_for_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_height_and_baseline_for_width"
                %widget-preferred-height-and-baseline-for-width) :void
  (widget (g:object widget))
  (width :int)
  (min-height (:pointer :int))
  (nat-height (:pointer :int))
  (min-baseline (:pointer :int))
  (nat-baseline (:pointer :int)))

(defun widget-preferred-height-and-baseline-for-width (widget width)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[width]{an integer with the width which is available for allocation,
    or -1 if none}
  @begin{return}
    @arg{min-height} -- an integer with the minimum height, or @code{nil} @br{}
    @arg{nat-height} -- an integer with the natural height, or @code{nil} @br{}
    @arg{min-baseline} -- an integer with the baseline for the minimum height,
      or @code{nil} @br{}
    @arg{nat-baseline} -- an integer with the baseline for the natural height,
      or @code{nil}
  @end{return}
  @begin{short}
    Retrieves the minimum and natural height of the widget and the corresponding
    baselines if it would be given the specified width, or the default height
    if width is -1.
  @end{short}
  The baselines may be -1 which means that no baseline is requested for this
  widget.

  The returned request will be modified by the @code{adjust_size_request} and
  @code{adjust_baseline_request} virtual methods and by any
  @class{gtk:size-group} objects that have been applied. That is, the returned
  request is the one that should be used for layout, not necessarily the one
  returned by the widget itself.
  @see-class{gtk:widget}
  @see-class{gtk:size-group}"
  (cffi:with-foreign-objects ((min-height :int)
                              (nat-height :int)
                              (min-baseline :int)
                              (nat-baseline :int))
    (%widget-preferred-height-and-baseline-for-width widget
                                                     width
                                                     min-height
                                                     nat-height
                                                     min-baseline
                                                     nat-baseline)
    (values (cffi:mem-ref min-height :int)
            (cffi:mem-ref nat-height :int)
            (cffi:mem-ref min-baseline :int)
            (cffi:mem-ref nat-baseline :int))))

(export 'widget-preferred-height-and-baseline-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_request_mode () -> widget-request-mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_request_mode" widget-request-mode)
    size-request-mode
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @symbol{gtk:size-request-mode} value preferred by @arg{widget}.}
  @begin{short}
    Gets whether the widget prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @begin[Notes]{dictionary}
    The @class{gtk:bin} widget generally propagate the preference of its child,
    container widgets need to request something either in context of their
    children or in context of their allocation capabilities.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:bin}
  @see-symbol{gtk:size-request-mode}"
  (widget (g:object widget)))

(export 'widget-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_size () -> widget-preferred-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_size" %widget-preferred-size) :void
  (widget (g:object widget))
  (minium-size (g:boxed requisition))
  (natural-size (g:boxed requisition)))

(defun widget-preferred-size (widget)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @arg{minimum-size} -- a @class{gtk:requisition} instance with the minimum
      size, or @code{nil} @br{}
    @arg{natural-size} -- a @class{gtk:requisition} instance with the the
      natural size, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves the minimum and natural size of a widget, taking into account the
    preference for height-for-width management of the widget.
  @end{short}

  This is used to retrieve a suitable size by container widgets which do not
  impose any restrictions on the child placement. It can be used to deduce
  toplevel window and menu sizes as well as child widgets in free-form
  containers such as @class{gtk:layout} widget.
  @begin[Notes]{dictionary}
    Handle with care. Note that the natural height of a height-for-width widget
    will generally be a smaller size than the minimum height, since the required
    height for the natural width is generally smaller than the required height
    for the minimum width.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:requisition}"
 (let ((minimum-size (make-requisition))
       (natural-size (make-requisition)))
    (%widget-preferred-size widget minimum-size natural-size)
    (values minimum-size
            natural-size)))

(export 'widget-preferred-size)

;;; ----------------------------------------------------------------------------
;;; gtk_distribute_natural_allocation ()
;;;
;;; gint gtk_distribute_natural_allocation (gint extra_space,
;;;                                         guint n_requested_sizes,
;;;                                         GtkRequestedSize *sizes);
;;;
;;; Distributes extra_space to child sizes by bringing smaller children up to
;;; natural size first.
;;;
;;; The remaining space will be added to the minimum_size member of the
;;; GtkRequestedSize struct. If all sizes reach their natural size then the
;;; remaining space is returned.
;;;
;;; extra_space :
;;;     Extra space to redistribute among children after subtracting minimum
;;;     sizes and any child padding from the overall allocation
;;;
;;; n_requested_sizes :
;;;     Number of requests to fit into the allocation
;;;
;;; sizes :
;;;     An array of structs with a client pointer and a minimum/natural size in
;;;     the orientation of the allocation.
;;;
;;; Returns :
;;;     The remainder of extra_space after redistributing space to sizes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_valign_with_baseline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_valign_with_baseline"
                widget-valign-with-baseline) align
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The vertical @symbol{gtk:align} alignment of the widget.}
  @begin{short}
    Gets the value of the @slot[gtk:widget]{valign} property, including the
    @code{:baseline} value.
  @end{short}
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-valign}"
  (widget (g:object widget)))

(export 'widget-valign-with-baseline)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_compute_expand ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_compute_expand"
                widget-queue-compute-expand) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Mark the widget as needing to recompute its expand flags.
  @end{short}
  Call this function when setting legacy expand child properties on the child
  of a container. See the @fun{gtk:widget-compute-expand} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-compute-expand}"
  (widget (g:object widget)))

(export 'widget-queue-compute-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_expand ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_compute_expand" widget-compute-expand) :boolean
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[orientation]{a @symbol{gtk:orientation} value for the expand
    direction}
  @return{The boolean whether @arg{widget} tree rooted here should be expanded.}
  @begin{short}
    Computes whether a container should give this widget extra space when
    possible.
  @end{short}
  Containers should check this, rather than looking at the
  @slot[gtk:widget]{hexpand} or @slot[gkt-widget]{vexpand} properties.

  This function already checks whether the widget is visible, so visibility
  does not need to be checked separately. Non-visible widgets are not
  expanded.

  The computed expand value uses either the expand setting explicitly set on
  the widget itself, or, if none has been explicitly set, the widget may
  expand if some of its children do.
  @see-class{gtk:widget}
  @see-symbol{gtk:orientation}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand}"
  (widget (g:object widget))
  (orientation orientation))

(export 'widget-compute-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_init_template ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_init_template" widget-init-template) :void
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Creates and initializes child widgets defined in templates.
  @end{short}
  This function must be called in the instance initializer for any class which
  assigned itself a template using the @fun{gtk:widget-class-set-template}
  function.

  It is important to call this function in the instance initializer of a
  GtkWidget subclass and not in @code{GObject.constructed()} or
  @code{GObject.constructor()} for two reasons.

  One reason is that generally derived widgets will assume that parent class
  composite widgets have been created in their instance initializers.

  Another reason is that when calling the @fun{g:object-new} function on a
  widget with composite templates, it is important to build the composite
  widgets before the construct properties are set. Properties passed to
  the @fun{g:object-new} function should take precedence over properties set in
  the private template XML.
  @see-class{gtk:widget}
  @see-function{gtk:widget-class-set-template}
  @see-function{g:object-new}"
  (widget (g:object widget)))

(export 'widget-init-template)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_template ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_set_template" %widget-class-set-template) :void
  (class (:pointer (:struct g:type-class)))
  (template (g:boxed g:bytes)))

(defun widget-class-set-template (gtype template)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[gtype]{the @class{g:type-t} type ID of the widget class}
  @argument[template]{a string holding the @class{gtk:builder} XML}
  @begin{short}
    This should be called at class initialization time to specify the
    @class{gtk:builder} XML to be used to extend a widget.
  @end{short}
  For convenience, the @fun{gtk:widget-class-set-template-from-resource}
  function is also provided.

  Note that any class that installs templates must call the
  @fun{gtk:widget-init-template} function in the instance initializer of the
  widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-init-template}
  @see-function{gtk:widget-class-set-template-from-resource}"
  (let ((class (g:type-class-ref gtype)))
    (multiple-value-bind (data len)
        (cffi:foreign-string-alloc template)
      (unwind-protect
        (%widget-class-set-template class (g:bytes-new data len))
        (progn
          (g:type-class-unref class)
          (cffi:foreign-string-free data))))))

(export 'widget-class-set-template)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_template_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_set_template_from_resource"
                %widget-class-set-template-from-resource) :void
  (class (:pointer (:struct g:type-class)))
  (name :string))

(defun widget-class-set-template-from-resource (gtype name)
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[gtype]{the @class{g:type-t} type ID of the widget class}
  @argument[name]{a string with the name of the resource to load the template
    from}
  @begin{short}
    A convenience function to call the @fun{gtk:widget-class-set-template}
    function.
  @end{short}

  Note that any class that installs templates must call the
  @fun{gtk:widget-init-template} function in the instance initializer of the
  widget.
  @see-class{gtk:widget}
  @see-class{g:type-t}
  @see-function{gtk:widget-class-set-template}
  @see-function{gtk:widget-init-template}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-set-template-from-resource class name)
      (g:type-class-unref class))))

(export 'widget-class-set-template-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_template_child () -> widget-template-child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_template_child" widget-template-child) g:object
 #+liber-documentation
 "@version{#2023-3-8}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[gtype]{the @class{g:type-t} type ID to get a template child for}
  @argument[name]{a string with the \"ID\" of the child defined in the template
    XML}
  @return{The @class{g:object} instance built in the template XML with the ID
    name.}
  @begin{short}
    Fetch an object build from the template XML for @arg{gtype} in this widget
    instance.
  @end{short}
  This will only report children which were previously declared with the
  @fun{gtk:widget-class-bind-template-child-full} function or one of its
  variants.

  This function is only meant to be called for code which is private to the
  @arg{gtype} which declared the child and is meant for language bindings
  which cannot easily make use of the @class{g:object} structure offsets.
  @see-class{gtk:widget}
  @see-class{g:object}
  @see-class{g:type-t}
  @see-function{gtk:widget-class-bind-template-child-full}"
  (widget (g:object widget))
  (gtype g:type-t)
  (name :string))

(export 'widget-template-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child(widget_class, TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; instance structure.
;;;
;;; Parameters
;;;
;;; widget_class
;;;     a GtkWidgetClass
;;;
;;; TypeName
;;;     the type name of this widget
;;;
;;; member_name
;;;     name of the instance member in the instance struct for data_type
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;(cffi:defcfun ("gtk_widget_class_bind_template_child"
;                %widget-class-bind-template-child) :void
;  (class (:pointer (:struct g:type-class)))
;  (name :string)
;  (member :string))

;(defun widget-class-bind-template-child (class name member)
;  (let ((class (g:type-class-ref class)))
;    (unwind-protect
;      (%widget-class-bind-template-child class name member)
;      (g:type-class-unref class))))

;(export 'widget-class-bind-template-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_internal()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_internal(widget_class,
;;;                                               TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class , and also
;;; makes it available as an internal child in GtkBuilder, under the
;;; name member_name .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; instance structure.
;;;
;;; Parameters
;;;
;;; widget_class
;;;     a GtkWidgetClass
;;;
;;; TypeName
;;;     the type name, in CamelCase
;;;
;;; member_name
;;;     name of the instance member in the instance struct for data_type
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_private()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_private(widget_class,
;;;                                              TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; private data structure (it uses G_PRIVATE_OFFSET(), so the private struct
;;; must be added with G_ADD_PRIVATE()).
;;;
;;; Parameters
;;;
;;; widget_class
;;;     a GtkWidgetClass
;;;
;;; TypeName
;;;     the type name of this widget
;;;
;;; member_name
;;;     name of the instance private member in the private struct for data_type
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_internal_private()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_internal_private(widget_class,
;;;                                                       TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class , and also
;;; makes it available as an internal child in GtkBuilder, under the name
;;; member_name .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; private data structure.
;;;
;;; Parameters
;;;
;;; widget_class
;;;     a GtkWidgetClass
;;;
;;; TypeName
;;;     the type name, in CamelCase
;;;
;;; member_name
;;;     name of the instance private member on the private struct for data_type
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_full ()
;;;
;;; void
;;; gtk_widget_class_bind_template_child_full
;;;                               (GtkWidgetClass *widget_class,
;;;                                const gchar *name,
;;;                                gboolean internal_child,
;;;                                gssize struct_offset);
;;;
;;; Automatically assign an object declared in the class template XML to be set
;;; to a location on a freshly built instance’s private data, or alternatively
;;; accessible via gtk_widget_get_template_child().
;;;
;;; The struct can point either into the public instance, then you should use
;;; G_STRUCT_OFFSET(WidgetType, member) for struct_offset , or in the private
;;; struct, then you should use G_PRIVATE_OFFSET(WidgetType, member).
;;;
;;; An explicit strong reference will be held automatically for the duration of
;;; your instance’s life cycle, it will be released automatically when
;;; GObjectClass.dispose() runs on your instance and if a struct_offset that is
;;; != 0 is specified, then the automatic location in your instance public or
;;; private data will be set to NULL. You can however access an automated child
;;; pointer the first time your classes GObjectClass.dispose() runs, or
;;; alternatively in GtkWidgetClass.destroy().
;;;
;;; If internal_child is specified, GtkBuildableIface.get_internal_child() will
;;; be automatically implemented by the GtkWidget class so there is no need to
;;; implement it manually.
;;;
;;; The wrapper macros gtk_widget_class_bind_template_child(),
;;; gtk_widget_class_bind_template_child_internal(),
;;; gtk_widget_class_bind_template_child_private() and
;;; gtk_widget_class_bind_template_child_internal_private() might be more
;;; convenient to use.
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; Parameters
;;;
;;; widget_class
;;;     A GtkWidgetClass
;;;
;;; name
;;;     The "id" of the child defined in the template XML
;;;
;;; internal_child
;;;     Whether the child should be accessible as an "internal-child" when this
;;;     class is used in GtkBuilder XML
;;;
;;; struct_offset
;;;     The structure offset into the composite widget’s instance public or
;;;     private structure where the automated child pointer should be set, or
;;;     0 to not assign the pointer.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_callback()
;;;
;;; #define gtk_widget_class_bind_template_callback(widget_class, callback)
;;;
;;; Binds a callback function defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_callback_full() function.
;;;
;;; Parameters
;;;
;;; widget_class
;;;     a GtkWidgetClass
;;;
;;; callback
;;;     the callback symbol
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_callback_full ()
;;;
;;; void
;;; gtk_widget_class_bind_template_callback_full
;;;                               (GtkWidgetClass *widget_class,
;;;                                const gchar *callback_name,
;;;                                GCallback callback_symbol);
;;;
;;; Declares a callback_symbol to handle callback_name from the template XML
;;; defined for widget_type . See gtk_builder_add_callback_symbol().
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; Parameters
;;;
;;; widget_class
;;;     A GtkWidgetClass
;;;
;;; callback_name
;;;     The name of the callback as expected in the template XML
;;;
;;; callback_symbol
;;;     The callback symbol.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_connect_func ()
;;;
;;; void
;;; gtk_widget_class_set_connect_func (GtkWidgetClass *widget_class,
;;;                                   GtkBuilderConnectFunc connect_func,
;;;                                   gpointer connect_data,
;;;                                   GDestroyNotify connect_data_destroy);
;;;
;;; For use in language bindings, this will override the default
;;; GtkBuilderConnectFunc to be used when parsing GtkBuilder XML from this
;;; class’s template data.
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; Parameters
;;;
;;; widget_class
;;;     A GtkWidgetClass
;;;
;;; connect_func
;;;     The GtkBuilderConnectFunc to use when connecting signals in the class
;;;     template
;;;
;;; connect_data
;;;     The data to pass to connect_func
;;;
;;; connect_data_destroy
;;;     The GDestroyNotify to free connect_data , this will only be used at
;;;     class finalization time, when no classes of type widget_type are in use
;;;     anymore.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.widget.lisp -------------------------------------------
