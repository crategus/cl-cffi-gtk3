;;; ----------------------------------------------------------------------------
;;; gtk3.accel-map.lisp
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
;;;     gtk_accel_map_foreach
;;;     gtk_accel_map_load_fd
;;;     gtk_accel_map_save_fd
;;;     gtk_accel_map_load_scanner
;;;     gtk_accel_map_add_filter
;;;     gtk_accel_map_foreach_unfiltered
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

(gobject:define-g-object-class "GtkAccelMap" accel-map
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_accel_map_get_type")
  nil)

#+liber-documentation
(setf (documentation 'accel-map 'type)
 "@version{2023-3-6}
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
  corresponds to the kind of window the accelerator is being used in, e.g.
  @code{\"Gimp-Image\"}, @code{\"Abiword-Document\"} or
  @code{\"Gnumeric-Settings\"}. The @code{\"Category1/.../Action\"} portion is
  most appropriately chosen by the action the accelerator triggers, i.e. for
  accelerators on menu items, choose the item's menu path, e.g.
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
    accelerators. By connecting to the @code{\"changed\"} signal, one can
    monitor changes of all accelerators. It is also possible to monitor only a
    single accelerator path by using it as a detail of the @code{\"changed\"}
    signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (object path key mods)    :has-details
      @end{pre}
      Notifies of a change in the global accelerator map. The path is also used
      as the detail for the signal, so it is possible to connect to
      \"changed::accel-path\".
    @begin[arg]{table}
      @entry[object]{The global @class{gtk:accel-map} object.}
      @entry[path]{A string with the path of the accelerator that changed.}
      @entry[key]{An unsigned integer with the key value for the new
        accelerator.}
      @entry[mods]{The @symbol{gdk:modifier-type} modifier mask for the new
        accelerator.}
    @end{table}
  @end{dictionary}
  @see-class{gtk:ui-manager}")

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_entry ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_add_entry" accel-map-add-entry) :void
 #+liber-documentation
 "@version{2023-3-6}
  @argument[path]{a string with the valid accelerator path}
  @argument[key]{an unsigned integer with the accelerator key}
  @argument[mods]{a @symbol{gdk:modifier-type} value with the accelerator
    modifiers}
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
;;; gtk_accel_map_lookup_entry ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_lookup_entry" %accel-map-lookup-entry) :boolean
  (path :string)
  (key (:pointer (:struct accel-key))))

(defun accel-map-lookup-entry (path)
 #+liber-documentation
 "@version{2023-3-6}
  @argument[path]{a string with the valid accelerator path}
  @begin{return}
    @code{key} -- an unsigned integer with the accelerator key @br{}
    @code{mods} -- a @symbol{gdk:modifier-type} value with the accelerator
      modifiers @br{}
    @code{flags} -- an unsigned integer with the accelerator flags if
      @arg{path} is known, @code{nil} otherwise
  @end{return}
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
;;; gtk_accel_map_change_entry ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_change_entry" accel-map-change-entry) :boolean
 #+liber-documentation
 "@version{2023-3-6}
  @argument[path]{a string with the valid accelerator path}
  @argument[key]{an unsigned integer with the new accelerator key}
  @argument[mods]{a @symbol{gdk:modifier-type} value with the new accelerator
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
;;; gtk_accel_map_load ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_load" %accel-map-load) :void
  (filename :string))

(defun accel-map-load (path)
 #+liber-documentation
 "@version{2023-3-6}
  @argument[path]{a pathname or namestring with a file containing accelerator
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
;;; gtk_accel_map_save ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_save" %accel-map-save) :void
  (filename :string))

(defun accel-map-save (path)
 #+liber-documentation
 "@version{2023-3-6}
  @argument[path]{a pathname or namestring with the name of the file to contain
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
;;; GtkAccelMapForeach ()
;;;
;;; void (*GtkAccelMapForeach) (gpointer data,
;;;                             const gchar *accel_path,
;;;                             guint accel_key,
;;;                             GdkModifierType accel_mods,
;;;                             gboolean changed);
;;;
;;; data :
;;;     User data passed to gtk_accel_map_foreach() or
;;;     gtk_accel_map_foreach_unfiltered()
;;;
;;; accel_path :
;;;     Accel path of the current accelerator
;;;
;;; accel_key :
;;;     Key of the current accelerator
;;;
;;; accel_mods :
;;;     Modifiers of the current accelerator
;;;
;;; changed :
;;;     Changed flag of the accelerator (if TRUE, accelerator has changed
;;;     during runtime and would need to be saved during an accelerator dump)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach ()
;;;
;;; void gtk_accel_map_foreach (gpointer data, GtkAccelMapForeach foreach_func);
;;;
;;; Loops over the entries in the accelerator map whose accel path does not
;;; match any of the filters added with gtk_accel_map_add_filter(), and execute
;;; foreach_func on each. The signature of foreach_func is that of
;;; GtkAccelMapForeach, the changed parameter indicates whether this accelerator
;;; was changed during runtime (thus, would need saving during an accelerator
;;; map dump).
;;;
;;; data :
;;;     data to be passed into foreach_func
;;;
;;; foreach_func :
;;;     function to be executed for each accel map entry which is not filtered
;;;     out
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_fd ()
;;;
;;; void gtk_accel_map_load_fd (gint fd);
;;;
;;; Filedescriptor variant of gtk_accel_map_load().
;;;
;;; Note that the file descriptor will not be closed by this function.
;;;
;;; fd :
;;;     a valid readable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_save_fd ()
;;;
;;; void gtk_accel_map_save_fd (gint fd);
;;;
;;; Filedescriptor variant of gtk_accel_map_save().
;;;
;;; Note that the file descriptor will not be closed by this function.
;;;
;;; fd :
;;;     a valid writable file descriptor
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_load_scanner ()
;;;
;;; void gtk_accel_map_load_scanner (GScanner *scanner);
;;;
;;; GScanner variant of gtk_accel_map_load().
;;;
;;; scanner :
;;;     a GScanner which has already been provided with an input file
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_add_filter ()
;;;
;;; void gtk_accel_map_add_filter (const gchar *filter_pattern);
;;;
;;; Adds a filter to the global list of accel path filters.
;;;
;;; Accel map entries whose accel path matches one of the filters are skipped
;;; by gtk_accel_map_foreach().
;;;
;;; This function is intended for GTK+ modules that create their own menus, but
;;; don't want them to be saved into the applications accelerator map dump.
;;;
;;; filter_pattern :
;;;     a pattern (see GPatternSpec)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_foreach_unfiltered ()
;;;
;;; void gtk_accel_map_foreach_unfiltered (gpointer data,
;;;                                        GtkAccelMapForeach foreach_func);
;;;
;;; Loops over all entries in the accelerator map, and execute foreach_func on
;;; each. The signature of foreach_func is that of GtkAccelMapForeach, the
;;; changed parameter indicates whether this accelerator was changed during
;;; runtime (thus, would need saving during an accelerator map dump).
;;;
;;; data :
;;;     data to be passed into foreach_func
;;;
;;; foreach_func :
;;;     function to be executed for each accel map entry
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_get ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_get" accel-map-get) (g:object accel-map)
 #+liber-documentation
 "@version{2023-3-6}
  @return{The global @class{gtk:accel-map} object.}
  @begin{short}
    Gets the singleton global @class{gtk:accel-map} object.
  @end{short}
  This object is useful only for notification of changes to the accelerator map
  via the @code{\"changed\"} signal. It is not a parameter to the other
  accelerator map functions.
  @see-class{gtk:accel-map}")

(export 'accel-map-get)

;;; ----------------------------------------------------------------------------
;;; gtk_accel_map_lock_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_lock_path" accel-map-lock-path) :void
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[path]{a string with the valid accelerator path}
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
;;; gtk_accel_map_unlock_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accel_map_unlock_path" accel-map-unlock-path) :void
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[path]{a string with the valid accelerator path}
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
