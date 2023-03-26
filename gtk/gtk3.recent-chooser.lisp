;;; ----------------------------------------------------------------------------
;;; gtk3.recent-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;;
;;; GtkRecentChooser
;;;
;;;     Interface implemented by widgets displaying recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentChooser
;;;     GtkRecentChooserError
;;;     GtkRecentSortType
;;;
;;;     GTK_RECENT_CHOOSER_ERROR
;;;
;;; Functions
;;;
;;;     gtk_recent_chooser_set_show_private
;;;     gtk_recent_chooser_get_show_private
;;;     gtk_recent_chooser_set_show_not_found
;;;     gtk_recent_chooser_get_show_not_found
;;;     gtk_recent_chooser_set_show_icons
;;;     gtk_recent_chooser_get_show_icons
;;;     gtk_recent_chooser_set_select_multiple
;;;     gtk_recent_chooser_get_select_multiple
;;;     gtk_recent_chooser_set_local_only
;;;     gtk_recent_chooser_get_local_only
;;;     gtk_recent_chooser_set_limit
;;;     gtk_recent_chooser_get_limit
;;;     gtk_recent_chooser_set_show_tips
;;;     gtk_recent_chooser_get_show_tips
;;;
;;;     gtk_recent_chooser_set_sort_type
;;;     gtk_recent_chooser_get_sort_type
;;;     gtk_recent_chooser_set_sort_func
;;;     gtk_recent_chooser_set_current_uri
;;;     gtk_recent_chooser_get_current_uri
;;;     gtk_recent_chooser_get_current_item
;;;     gtk_recent_chooser_select_uri
;;;     gtk_recent_chooser_unselect_uri
;;;     gtk_recent_chooser_select_all
;;;     gtk_recent_chooser_unselect_all
;;;     gtk_recent_chooser_get_items
;;;     gtk_recent_chooser_get_uris
;;;     gtk_recent_chooser_add_filter
;;;     gtk_recent_chooser_remove_filter
;;;     gtk_recent_chooser_list_filters
;;;     gtk_recent_chooser_set_filter                      Accessor
;;;     gtk_recent_chooser_get_filter                      Accessor
;;;
;;; Properties
;;;
;;;     filter
;;;     limit
;;;     local-only
;;;     recent-manager
;;;     select-multiple
;;;     show-icons
;;;     show-not-found
;;;     show-private
;;;     show-tips
;;;     sort-type
;;;
;;; Signals
;;;
;;;     item-activated
;;;     selection-changed
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkRecentChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_RECENT_CHOOSER_ERROR
;;;
;;; #define GTK_RECENT_CHOOSER_ERROR (gtk_recent_chooser_error_quark ())
;;;
;;; Used to get the GError quark for GtkRecentChooser errors.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentChooserError
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentChooserError" recent-chooser-error
  (:export t
   :type-initializer "gtk_recent_chooser_error_get_type")
  (:not-found 0)
  (:invalid-uri 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-chooser-error)
      "GEnum"
      (liber:symbol-documentation 'recent-chooser-error)
 "@version{#2023-3-24}
  @begin{short}
    These identify the various errors that can occur while calling
    @class{gtk:recent-chooser} functions.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRecentChooserError\" recent-chooser-error
  (:export t
   :type-initializer \"gtk_recent_chooser_error_get_type\")
  (:not-found 0)
  (:invalid-uri 1))
  @end{pre}
  @begin[code]{table}
    @entry[:not-found]{Indicates that a file does not exist.}
    @entry[:invalid-uri]{Indicates a malformed URI.}
  @end{table}
  @see-class{gtk:recent-chooser}")

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentSortType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRecentSortType" recent-sort-type
  (:export t
   :type-initializer "gtk_recent_sort_type_get_type")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-sort-type)
      "GEnum"
      (liber:symbol-documentation 'recent-sort-type)
 "@version{#2023-3-24}
  @begin{short}
    Used to specify the sorting method to be applyed to the recently used
    resource list.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRecentSortType\" recent-sort-type
  (:export t
   :type-initializer \"gtk_recent_sort_type_get_type\")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not sort the returned list of recently used resources.}
    @entry[:mru]{Sort the returned list with the most recently used items
      first.}
    @entry[:lru]{Sort the returned list with the least recently used items
      first.}
    @entry[:custom]{Sort the returned list using a custom sorting function
      passed using the @fun{gtk:recent-chooser-set-sort-func} function.}
  @end{table}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-set-sort-func}")

;;; ----------------------------------------------------------------------------
;;; GtkRecentChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkRecentChooser" recent-chooser
  (:export t
   :type-initializer "gtk_recent_chooser_get_type")
  ((filter
    recent-chooser-filter
    "filter" "GtkRecentFilter" t t)
   (limit
    recent-chooser-limit
    "limit" "gint" t t)
   (local-only
    recent-chooser-local-only
    "local-only" "gboolean" t t)
   (recent-manager
    recent-chooser-recent-manager
    "recent-manager" "GtkRecentManager" nil nil)
   (select-multiple
    recent-chooser-select-multiple
    "select-multiple" "gboolean" t t)
   (show-icons
    recent-chooser-show-icons
    "show-icons" "gboolean" t t)
   (show-not-found
    recent-chooser-show-not-found
    "show-not-found" "gboolean" t t)
   (show-private
    recent-chooser-show-private
    "show-private" "gboolean" t t)
   (show-tips
    recent-chooser-show-tips
    "show-tips" "gboolean" t t)
   (sort-type
    recent-chooser-sort-type
    "sort-type" "GtkRecentSortType" t t)))

#+liber-documentation
(setf (documentation 'recent-chooser 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @sym{gtk:recent-chooser} interface is an interface that can be
    implemented by widgets displaying the list of recently used files.
  @end{short}
  In GTK, the main objects that implement this interface are the
  @class{gtk:recent-chooser-widget}, @class{gtk:recent-chooser-dialog} and
  @class{gtk:recent-chooser-menu} widgets.
  @begin[Signal Details]{dictionary}
    @subheading{The \"item-activated\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the user \"activates\" a recent item in the
      recent chooser. This can happen by double-clicking on an item in the
      recently used resources list, or by pressing the @kbd{Enter} key.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk:recent-chooser} object which received the
          signal.}
      @end{table}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when there is a change in the set of selected
      recently used resources. This can happen when a user modifies the
      selection with the mouse or the keyboard, or when explicitely calling
      functions to change the selection.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk:recent-chooser} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:recent-chooser-filter}
  @see-slot{gtk:recent-chooser-limit}
  @see-slot{gtk:recent-chooser-local-only}
  @see-slot{gtk:recent-chooser-recent-manager}
  @see-slot{gtk:recent-chooser-select-multiple}
  @see-slot{gtk:recent-chooser-show-icons}
  @see-slot{gtk:recent-chooser-show-not-found}
  @see-slot{gtk:recent-chooser-show-private}
  @see-slot{gtk:recent-chooser-show-tips}
  @see-slot{gtk:recent-chooser-sort-type}
  @see-class{gtk:recent-chooser-widget}
  @see-class{gtk:recent-chooser-dialog}
  @see-class{gtk:recent-chooser-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- recent-chooser-filter --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'recent-chooser) t)
 "The @code{filter} property of type @class{gtk:recent-filter} (Read / Write)
  @br{}
  The filter to be used when displaying the recently used resources.")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-filter)
      "Accessor"
      (documentation 'recent-chooser-filter 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-filter objet) => filter}
  @syntax[]{(setf (gtk:recent-chooser-filter object) filter)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @begin{short}
    Accessor of the @slot[gtk:recent-chosser]{filter} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-filter} function gets the 
  @class{gtk:recent-filter} object currently used by @arg{chooser} to affect 
  the display of the recently used resources. The
  @sym{(setf gtk:recent-chooser-filter object) filter)} function sets the 
  filter.
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-filter}")

;;; --- recent-chooser-limit ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "limit" 'recent-chooser) t)
 "The @code{limit} property of type @code{:int} (Read / Write) @br{}
  The maximum number of recently used resources to be displayed, or -1 to
  display all items. By default, the @symbol{gtk:recent-files-limit} setting
  is respected. You can override that limit on a particular instance of the
  @sym{gtk:recent-chooser} widget by setting this property. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-limit)
      "Accessor"
      (documentation 'recent-chooser-limit 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-limit objet) => limit}
  @syntax[]{(setf (gtk:recent-chooser-limit object) limit)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[limit]{a positive integer, or -1 for all items}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{limit} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-limit} function gets the number of items returned
  by the @fun{gtk:recent-chooser-items} and @fun{gtk:recent-chooser-uris}
  functions. The @sym{(setf gtk:recent-chooser-limit)} function sets the number 
  of items that should be returned by the @fun{gtk:recent-chooser-items} and
  @fun{gtk:recent-chooser-uris} functions.
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-items}
  @see-function{gtk:recent-chooser-uris}")

;;; --- recent-chooser-local-only ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "local-only" 'recent-chooser) t)
 "The @code{local-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether this @sym{gtk:recent-chooser} should display only local (file:)
  resources. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-local-only)
      "Accessor"
      (documentation 'recent-chooser-local-only 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-local-only objet) => local-only}
  @syntax[]{(setf (gtk:recent-chooser-local-only object) local-only)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[local-only]{@em{true} if only local files can be shown}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{local-only} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-local-only} function gets whether only local 
  resources should be shown in the recently used resources selector. The
  @sym{(setf gtk:recent-chooser-local-only)} function sets whether only local 
  resources, that is resources using the file:// URI scheme, should be shown in 
  the recently used resources selector.

  If the @arg{local-only} argument is @em{true} (the default) then the shown
  resources are guaranteed to be accessible through the operating system native
  file system.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-recent-manager ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "recent-manager" 
                                               'recent-chooser) t)
 "The @code{recent-manager} property of type @class{gtk:recent-manager}
  (Write / Construct Only) @br{}
  The recent manager used by the recent chooser to display the list of recently
  used resources.")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-recent-manager)
      "Accessor"
      (documentation 'recent-chooser-recent-manager 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-recent-manager objet) => recent-manager}
  @syntax[]{(setf (gtk:recent-chooser-recent-manager object) recent-manager)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[recent-manager]{a @class{gtk:recent-manager} object}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{recent-manager} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The recent manager used by the recent chooser to display the list of recently
  used resources.
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-manager}")

;;; --- recent-chooser-select-multiple -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "select-multiple"
                                               'recent-chooser) t)
 "The @code{select-multiple} property of type @code{:boolean} (Read / Write)
  @br{}
  Allow the user to select multiple resources. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-select-multiple)
      "Accessor"
      (documentation 'recent-chooser-select-multiple 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-select-multiple objet) => select-multiple}
  @syntax[]{(setf (gtk:recent-chooser-select-multiple object) select-multiple)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[select-multiple]{@em{true} if @arg{chooser} can select more than
    one item}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{select-multiple} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-select-multiple} function gets whether 
  @arg{chooser} can select multiple items. The 
  @sym{(setf gtk:recent-choose-select-multiple)} function sets whether 
  @arg{chooser} can select multiple items.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-show-icons ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-icons" 'recent-chooser) t)
 "The @code{show-icons} property of type @code{:boolean} (Read / Write) @br{}
  Whether the recent chooser should display an icon near the item. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-show-icons)
      "Accessor"
      (documentation 'recent-chooser-show-icons 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-show-icons objet) => show-icons}
  @syntax[]{(setf (gtk:recent-chooser-show-icons object) show-icons)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[show-icons]{a boolean whether to show an icon near the resource}
  @begin{short}
    Accessor of the slot @slot[gtk:recent-chooser]{show-icons} of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-show-icons} function retrieves whether 
  @arg{chooser} should show an icon near the resource. The
  @sym{(setf gtk:recent-chooser-show-icons)} function sets whether 
  @arg{chooser} should show an icon near the resource when displaying it.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-show-not-found ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-not-found" 
                                               'recent-chooser) t)
 "The @code{show-not-found} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the recent chooser should display the recently used resources even if
  not present anymore. Setting this to @em{false} will perform a potentially
  expensive check on every local resource (every remote resource will always be
  displayed). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-show-not-found)
      "Accessor"
      (documentation 'recent-chooser-show-not-found 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-show-not-found objet) => show-not-found}
  @syntax[]{(setf (gtk:recent-chooser-show-not-found object) show-not-found)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[show-not-found]{a boolean whether to show the local items we did
    not find}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{show-not-found} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-show-not-found} function retrieves whether 
  @arg{chooser} should show the recently used resources that were not found. 
  The @sym{(setf gtk:recent-chooser-show-not-found)} function sets whether 
  @arg{chooser} should display the recently used resources that it did not 
  find. This only applies to local resources.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-show-private --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-private"
                                               'recent-chooser) t)
 "The @code{show-private} property of type @code{:boolean} (Read / Write) @br{}
  Whether the private items should be displayed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-show-private)
      "Accessor"
      (documentation 'recent-chooser-show-private 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-show-private objet) => show-private}
  @syntax[]{(setf (gtk:recent-chooser-show-private object) show-private)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[show-private]{@em{true} to show private items, @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{show-private} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-show-private} function returns whether 
  @arg{chooser} should display recently used resources registered as private.
  The @sym{(setf gtk:recent-chooser-show-private)} function sets whether to 
  show recently used resources marked registered as private.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-show-tips -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-tips" 'recent-chooser) t)
 "The @code{show-tips} property of type @code{:boolean} (Read / Write) @br{}
  Whether the recent chooser should display a tooltip containing the full path
  of the recently used resources. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-show-tips)
      "Accessor"
      (documentation 'recent-chooser-show-tips 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-show-tips objet) => show-tips}
  @syntax[]{(setf (gtk:recent-chooser-show-tips object) show-tips)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[show-tips]{@em{true} if tooltips should be shown}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{show-tips} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-show-tips} function gets whether @arg{chooser} 
  should display tooltips containing the full path of a recently user resource. 
  The @sym{(setf gtk:recent-chooser-show-tips)} function sets whether to show a 
  tooltips containing the full path of each recently used resource in a 
  @class{gtk:recent-chooser} widget.
  @see-class{gtk:recent-chooser}")

;;; --- recent-chooser-sort-type -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sort-type" 'recent-chooser) t)
 "The @code{sort-type} property of type @symbol{gtk:recent-sort-type}
  (Read / Write) @br{}
  Sorting order to be used when displaying the recently used resources. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-sort-type)
      "Accessor"
      (documentation 'recent-chooser-sort-type 'function)
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-sort-type objet) => sort-type}
  @syntax[]{(setf (gtk:recent-chooser-sort-type object) sort-type)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[sort-type]{sort order of type @see-symbol{gtk:recent-sort-type}
    that the chooser should use}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser]{sort-type} slot of the
    @class{gtk:recent-chooser} class.
  @end{short}
  The @sym{gtk:recent-chooser-sort-type} function gets the sort order that the 
  chooser should use. The @sym{(setf gtk:recent-chooser-sort-type)} function 
  changes the sorting order of the recently used resources list displayed by 
  chooser.
  @see-class{gtk:recent-chooser}
  @see-symbol{gtk:recent-sort-type}")

;;; ----------------------------------------------------------------------------
;;; GtkRecentSortFunc ()
;;; ----------------------------------------------------------------------------

(defcallback recent-sort-func :int
    ((a (g:boxed recent-info))
     (b (g:boxed recent-info))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) a b))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-sort-func)
      "Callback"
      (liber:symbol-documentation 'recent-sort-func)
 "@version{#2023-3-24}
  @begin{short}
    The callback function to set with the @fun{gtk:recent-chooser-set-sort-func}
    function.
  @end{short}
  @begin{pre}
lambda (item1 item2)
  @end{pre}
  @begin[code]{table}
    @entry[item1]{A @class{gtk:recent-info} instance.}
    @entry[item2]{A second @class{gtk:recent-info} instance.}
    @entry[Returns]{A positive integer if the first item comes before the
      second, zero if the two items are equal and a negative integer if the
      first item comes after the second.}
  @end{table}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-set-sort-func}")

(export 'recent-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_set_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_set_sort_func" %recent-chooser-set-sort-func) 
    :void
  (chooser (g:object recent-chooser))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun recent-chooser-set-sort-func (chooser func)
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[func]{a @symbol{gtk:recent-sort-func} comparison function}
  @begin{short}
    Sets the comparison function used when sorting to be @arg{func}.
  @end{short}
  If the chooser has the sort type set to the @code{:custom} value then the
  chooser will sort using this function.

  To the comparison function will be passed two @class{gtk:recent-info}
  instances. The @arg{func} callback function should return a positive integer
  if the first item comes before the second, zero if the two items are equal
  and a negative integer if the first item comes after the second.
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-info}
  @see-sybmol{gtk:recent-sort-func}"
  (%recent-chooser-set-sort-func
          chooser
          (cffi:callback recent-sort-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'recent-chooser-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_uri ()
;;; gtk_recent_chooser_set_current_uri () -> recent-chooser-current-uri
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_set_current_uri" %recent-chooser-set-current-uri) 
    :boolean
  (chooser (g:object recent-chooser))
  (uri :string)
  (err :pointer))

(defun (setf recent-chooser-current-uri) (uri chooser)
  (with-g-error (err)
    (%recent-chooser-set-current-uri chooser uri err)))

(defcfun ("gtk_recent_chooser_get_current_uri" recent-chooser-current-uri)
    :string
 #+liber-documentation
 "@version{#2023-3-24}
  @syntax[]{(gtk:recent-chooser-current-uri chooser) => uri}
  @syntax[]{(setf (gtk:recent-chooser-current-uri chooser) uri)}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[uri]{a string with the URI}
  @begin{short}
    Accessor of the URI currently selected by @arg{chooser}.
  @end{short}
  The @sym{gtk:recent-chooser-current-uri} function gets the URI currently 
  selected by @arg{chooser}. The @sym{(setf gtk:recent-chooser-current-uri)} 
  function sets @arg{uri} as the current URI for @arg{chooser}.
  @see-class{gtk:recent-chooser}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-current-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_current_item () -> recent-chooser-current-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_current_item" recent-chooser-current-item) 
    (g:boxed recent-info)
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @begin{return}
    A @class{gtk:recent-info} instance.
  @end{return}
  @begin{short}
    Gets the @class{gtk:recent-info} instance currently selected by
    @arg{chooser}.
  @end{short}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-info}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-current-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_select_uri" %recent-chooser-select-uri)
    :boolean
  (chooser (g:object recent-chooser))
  (uri :string)
  (err :pointer))

(defun recent-chooser-select-uri (chooser uri)
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[uri]{a string with the URI}
  @return{@em{True} if @arg{uri} was found.}
  @short{Selects @arg{uri} inside @arg{chooser}.}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-unselect-uri}"
  (with-g-error (err)
    (%recent-chooser-select-uri chooser uri err)))

(export 'recent-chooser-select-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_uri ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_unselect_uri" recent-chooser-unselect-uri) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[uri]{a string with the URI}
  @short{Unselects @arg{uri} inside @arg{chooser}.}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-select-uri}"
  (chooser (g:object recent-chooser))
  (uri :string))

(export 'recent-chooser-unselect-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_select_all" recent-chooser-select-all) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @begin{short}
    Selects all the items inside @arg{chooser}, if the chooser supports 
    multiple selection.
  @end{short}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-unselect-all}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_unselect_all" recent-chooser-unselect-all) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @short{Unselects all the items inside @arg{chooser}.}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-select-all}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_items () -> recent-chooser-items
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_items" recent-chooser-items)
    (g:list-t (g:boxed recent-info))
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @begin{return}
    A list of @class{gtk:recent-info} instances.
  @end{return}
  @begin{short}
    Gets the list of recently used resources in form of @class{gtk:recent-info}
    instances.
  @end{short}
  The return value of this function is affected by the
  @slot[gtk:recent-chooser]{sort-type} and @slot[gtk:recent-chooser]{limit}
  properties of @arg{chooser}.
  @see-class{gtk:recent-chooser}
  @see-symbol{gtk:recent-info}
  @see-function{gtk:recent-chooser-uris}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_get_uris () -> recent-chooser-uris
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_get_uris" %recent-chooser-uris) g:strv-t
  (chooser (g:object recent-chooser))
  (length :size))

(defun recent-chooser-uris (chooser)
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @return{A list of strings.}
  @begin{short}
    Gets the URIs of the recently used resources.
  @end{short}
  The return value of this function is affected by the
  @slot[gtk:recent-chooser]{sort-type} and @slot[gtk:recent-chooser]{limit}
  properties of @arg{chooser}.
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-chooser-items}"
  (with-foreign-object (length :size)
    (%recent-chooser-uris chooser length)))

(export 'recent-chooser-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_add_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_add_filter" recent-chooser-add-filter) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @begin{short}
    Adds @arg{filter} to the list of @class{gtk:recent-filter} objects held by
    @arg{chooser}.
  @end{short}
  If no previous filter objects were defined, this function will call the
  @fun{gtk:recent-chooser-filter} function.
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-filter}
  @see-function{gtk:recent-chooser-filter}
  @see-function{gtk:recent-chooser-remove-filter}"
  (chooser (g:object recent-chooser))
  (filter (g:object recent-filter)))

(export 'recent-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_remove_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_remove_filter" recent-chooser-remove-filter) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @begin{short}
    Removes @arg{filter} from the list of @class{gtk:recent-filter} objects
    held by @arg{chooser}.
  @end{short}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-filter}
  @see-function{gtk:recent-chooser-add-filter}"
  (chooser (g:object recent-chooser))
  (filter (g:object recent-filter)))

(export 'recent-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_list_filters ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_recent_chooser_list_filters" recent-chooser-list-filters)
    (g:slist-t (g:object recent-filter))
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[chooser]{a @class{gtk:recent-chooser} object}
  @return{A list of @class{gtk:recent-filter} objects.}
  @begin{short}
    Gets the @class{gtk:recent-filter} objects held by chooser.
  @end{short}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-filter}"
  (chooser (g:object recent-chooser)))

(export 'recent-chooser-list-filters)

;;; --- End of file gtk3.recent-chooser.lisp -----------------------------------
