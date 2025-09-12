;;; ----------------------------------------------------------------------------
;;; gtk3.accel-map.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
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
;;;
;;; Accelerator Maps
;;;
;;;     Loadable keyboard accelerator specifications
;;;
;;; Types and Values
;;;
;;;     GtkAccelMap
;;;
;;; Functions
;;;
;;;     gtk_accel_map_add_entry
;;;     gtk_accel_map_lookup_entry
;;;     gtk_accel_map_change_entry
;;;     gtk_accel_map_load
;;;     gtk_accel_map_save
;;;     gtk_accel_map_foreach                               not implemented
;;;     gtk_accel_map_load_fd                               not implemented
;;;     gtk_accel_map_save_fd                               not implemented
;;;     gtk_accel_map_load_scanner                          not implemented
;;;     gtk_accel_map_add_filter                            not implemented
;;;     gtk_accel_map_foreach_unfiltered                    not implemented
;;;     gtk_accel_map_get
;;;     gtk_accel_map_lock_path
;;;     gtk_accel_map_unlock_path
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAccelMap
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMap
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAccelMap" accel-map
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_map_get_type")
  nil)

#+liber-documentation
(setf (documentation 'accel-map 'type)
 "@version{2025-08-31}
  @begin{short}
    Accelerator maps are used to define runtime configurable accelerators.
  @end{short}
  Functions for manipulating them are usually used by higher level convenience
  mechanisms like the @class{gtk:ui-manager} object and are thus considered
  \"low-level\". You will want to use them if you are manually creating menus
  that should have user-configurable accelerators.

  An accelerator is uniquely defined by:
  @begin{itemize}
    @item{an accelerator path,}
    @item{an accelerator key, and}
    @item{accelerator modifiers.}
  @end{itemize}
  The accelerator path must consist of
  @code{\"<WINDOWTYPE>/Category1/Category2/.../Action\"}, where
  @code{WINDOWTYPE} should be a unique application specific identifier that
  corresponds to the kind of window the accelerator is being used in, for
  example @code{\"Gimp-Image\"}, @code{\"Abiword-Document\"} or
  @code{\"Gnumeric-Settings\"}. The @code{\"Category1/.../Action\"} portion is
  most appropriately chosen by the action the accelerator triggers, that is for
  accelerators on menu items, choose the item's menu path, for example
  @code{\"File/Save As\"}, @code{\"Image/View/Zoom\"} or
  @code{\"Edit/Select All\"}. So a full valid accelerator path may look like:
  @code{\"<Gimp-Toolbox>/File/Dialogs/Tool Options...\"}.

  All accelerators are stored inside one global @class{gtk:accel-map} object
  that can be obtained using the @fun{gtk:accel-map-get} function. See
  Monitoring changes for additional details.

  @subheading{Manipulating accelerators}
    New accelerators can be added using the @fun{gtk:accel-map-add-entry}
    function. To search for a specific accelerator, use the
    @fun{gtk:accel-map-lookup-entry} function. Modifications of existing
    accelerators should be done using the @fun{gtk:accel-map-change-entry}
    function.

    In order to avoid having some accelerators changed, they can be locked
    using the @fun{gtk:accel-map-lock-path} function. Unlocking is done using
    the @fun{gtk:accel-map-unlock-path} function.

  @subheading{Saving and loading accelerator maps}
    Accelerator maps can be saved to and loaded from some external resource.
    For simple saving and loading from file, the @fun{gtk:accel-map-save} and
    @fun{gtk:accel-map-load} functions are provided.

  @subheading{Monitoring changes}
    A @class{gtk:accel-map} object is only useful for monitoring changes of
    accelerators. By connecting to the @sig[gtk:accel-map]{changed} signal,
    one can monitor changes of all accelerators. It is also possible to monitor
    only a single accelerator path by using it as a detail of the
    @sig[gtk:accel-map]{changed} signal.
  @begin[Signal Details]{dictionary}
    @begin[accel-map::changed]{signal}
      @begin{pre}
lambda (object path key mods)    :has-details
      @end{pre}
      @begin[arg]{simple-table}
        @entry[object]{The global @class{gtk:accel-map} object.}
        @entry[path]{The string for the path of the accelerator that changed.}
        @entry[key]{The unsigned integer for the key value for the new
          accelerator.}
        @entry[mods]{The @sym{gdk:modifier-type} value for the modifier mask
          for the new accelerator.}
      @end{simple-table}
      Notifies of a change in the global accelerator map. The path is also used
      as the detail for the signal, so it is possible to connect to
      @sig[gtk:accel-map]{changed::accel-path}.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:ui-manager}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_add_entry" accel-map-add-entry) :void
 #+liber-documentation
 "@version{2025-07-07}
  @argument[path]{a string for the valid accelerator path}
  @argument[key]{an unsigned integer for the accelerator key}
  @argument[mods]{a @sym{gdk:modifier-type} value for the accelerator modifiers}
  @begin{short}
    Registers a new accelerator with the global accelerator map.
  @end{short}
  This function should only be called once per @arg{path} with the canonical
  @arg{key} and @arg{mods} for this path. To change the accelerator during
  runtime programatically, use the @fun{gtk:accel-map-change-entry} function.
  @see-class{gtk:accel-map}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:accel-map-change-entry}"
  (path :string)
  (key :uint)
  (mods gdk:modifier-type))

(export 'accel-map-add-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lookup_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_lookup_entry" %accel-map-lookup-entry) :boolean
  (path :string)
  (key (:pointer (:struct accel-key))))

(defun accel-map-lookup-entry (path)
 #+liber-documentation
 "@version{2025-07-07}
  @syntax{(gtk:accel-map-lookup-entry path) => key, mods, flags}
  @argument[path]{a string for the valid accelerator path}
  @argument[key]{an unsigned integer for the accelerator key}
  @argument[mods]{a @sym{gdk:modifier-type} value for the accelerator modifiers}
  @argument[flags]{an unsigned integer for the accelerator flags if @arg{path}
    is known, @code{nil} otherwise}
  @begin{short}
    Looks up the accelerator entry for @arg{path}.
  @end{short}
  @see-class{gtk:accel-map}
  @see-function{gtk:accel-map-add-entry}
  @see-function{gtk:accel-map-change-entry}"
  (cffi:with-foreign-object (key '(:struct accel-key))
    (when (%accel-map-lookup-entry path key)
      (values (cffi:foreign-slot-value key '(:struct accel-key) 'accel-key)
              (cffi:foreign-slot-value key '(:struct accel-key) 'accel-mods)
              (cffi:foreign-slot-value key '(:struct accel-key) 'accel-flags)))))

(export 'accel-map-lookup-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_change_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_change_entry" accel-map-change-entry) :boolean
 #+liber-documentation
 "@version{2025-07-07}
  @argument[path]{a string for the valid accelerator path}
  @argument[key]{an unsigned integer for the new accelerator key}
  @argument[mods]{a @sym{gdk:modifier-type} value for the new accelerator
    modifiers}
  @argument[replace]{@em{true} if other accelerators may be deleted upon
    conflicts}
  @begin{return}
    @em{True} if the accelerator could be changed, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Changes @arg{key} and @arg{mods} currently associated with @arg{path}.
  @end{short}
  Due to conflicts with other accelerators, a change may not always be
  possible, @arg{replace} indicates whether other accelerators may be deleted
  to resolve such conflicts. A change will only occur if all conflicts could be
  resolved, which might not be the case if conflicting accelerators are
  locked. Successful changes are indicated by a @em{true} return value.
  @see-class{gtk:accel-map}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:accel-map-add-entry}"
  (path :string)
  (key :uint)
  (mods gdk:modifier-type)
  (replace :boolean))

(export 'accel-map-change-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_load" %accel-map-load) :void
  (filename :string))

(defun accel-map-load (path)
 #+liber-documentation
 "@version{2025-06-19}
  @argument[path]{a pathname or namestring for a file containing accelerator
    specifications, in the GLib file name encoding}
  @begin{short}
    Parses a file previously saved with the @fun{gtk:accel-map-save} function
    for accelerator specifications, and propagates them accordingly.
  @end{short}
  @see-class{gtk:accel-map}
  @see-function{gtk:accel-map-save}"
  (%accel-map-load (namestring path)))

(export 'accel-map-load)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_save" %accel-map-save) :void
  (filename :string))

(defun accel-map-save (path)
 #+liber-documentation
 "@version{2025-06-19}
  @argument[path]{a pathname or namestring for the name of the file to contain
    accelerator specifications, in the GLib file name encoding}
  @begin{short}
    Saves current accelerator specifications, accelerator path, key and
    modifiers, to @arg{path}.
  @end{short}
  The file is written in a format suitable to be read back in by the
  @fun{gtk:accel-map-load} function.
  @see-class{gtk:accel-map}
  @see-function{gtk:accel-map-load}"
  (%accel-map-save (namestring path)))

(export 'accel-map-save)

;;; ----------------------------------------------------------------------------
;;; GtkAccelMapForeach                                      not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach                                   not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_fd                                   not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save_fd                                   not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_scanner                              not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_filter                                not implemented
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach_unfiltered                        not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_get" accel-map-get) (g:object accel-map)
 #+liber-documentation
 "@version{2025-07-14}
  @return{The global @class{gtk:accel-map} object.}
  @begin{short}
    Gets the singleton global @class{gtk:accel-map} object.
  @end{short}
  This object is useful only for notification of changes to the accelerator map
  via the @sig[gtk:accel-map]{changed} signal. It is not a parameter to the
  other accelerator map functions.
  @see-class{gtk:accel-map}")

(export 'accel-map-get)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lock_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_lock_path" accel-map-lock-path) :void
 #+liber-documentation
 "@version{#2025-06-19}
  @argument[path]{a string for the valid accelerator path}
  @begin{short}
    Locks the given accelerator path.
  @end{short}
  If the accelerator map does not yet contain an entry for @arg{path}, a new
  one is created.

  Locking an accelerator path prevents its accelerator from being changed
  during runtime. A locked accelerator path can be unlocked by the
  @fun{gtk:accel-map-unlock-path} function. Refer to the
  @fun{gtk:accel-map-change-entry} function for information about runtime
  accelerator changes.

  If called more than once, @arg{path} remains locked until the
  @fun{gtk:accel-map-unlock-path} function has been called an equivalent number
  of times.

  Note that locking of individual accelerator paths is independent from
  locking the @class{gtk:accel-group} object containing them. For runtime
  accelerator changes to be possible both the accelerator path and its
  @class{gtk:accel-group} object have to be unlocked.
  @see-class{gtk:accel-map}
  @see-class{gtk:accel-group}
  @see-function{gtk:accel-map-unlock-path}
  @see-function{gtk:accel-map-change-entry}"
  (path :string))

(export 'accel-map-lock-path)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_unlock_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_unlock_path" accel-map-unlock-path) :void
 #+liber-documentation
 "@version{#2025-06-19}
  @argument[path]{a string for the valid accelerator path}
  @begin{short}
    Undoes the last call to the @fun{gtk:accel-map-lock-path} function on this
    @arg{path}.
  @end{short}
  Refer to the @fun{gtk:accel-map-lock-path} function for information about
  accelerator path locking.
  @see-class{gtk:accel-map}
  @see-function{gtk:accel-map-lock-path}"
  (path :string))

(export 'accel-map-unlock-path)

;;; --- End of file gtk3.accel-map.lisp ----------------------------------------
