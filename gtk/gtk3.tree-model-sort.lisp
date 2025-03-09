;;; ----------------------------------------------------------------------------
;;; gtk3.tree-model-sort.lisp
;;;
;;; The documentation in this file is taken from the GTK+ 3 Reference Manual
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
;;;
;;; GtkTreeModelSort
;;;
;;;     A GtkTreeModel which makes an underlying tree model sortable
;;;
;;; Types and Values
;;;
;;;     GtkTreeModelSort
;;;
;;; Accessors
;;;
;;;     gtk_tree_model_sort_get_model
;;;
;;; Functions
;;;
;;;     gtk_tree_model_sort_new_with_model
;;;     gtk_tree_model_sort_convert_child_path_to_path
;;;     gtk_tree_model_sort_convert_child_iter_to_iter
;;;     gtk_tree_model_sort_convert_path_to_child_path
;;;     gtk_tree_model_sort_convert_iter_to_child_iter
;;;     gtk_tree_model_sort_reset_default_sort_func
;;;     gtk_tree_model_sort_clear_cache
;;;     gtk_tree_model_sort_iter_is_valid
;;;
;;; Properties
;;;
;;;     model
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeModelSort
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeModelSort implements GtkTreeModel, GtkTreeSortable and
;;;     GtkTreeDragSource.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelSort
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeModelSort" tree-model-sort
  (:superclass g:object
   :export t
   :interfaces ("GtkTreeModel"
                "GtkTreeSortable"
                "GtkTreeDragSource")
   :type-initializer "gtk_tree_model_sort_get_type")
  ((model
    tree-model-sort-model
    "model" "GtkTreeModel" t t)))

;; TODO: Rework the examples

#+liber-documentation
(setf (documentation 'tree-model-sort 'type)
 "@version{2025-2-23}
  @begin{short}
    The @class{gtk:tree-model-sort} object is a model which implements the
    @class{gtk:tree-sortable} interface.
  @end{short}
  It does not hold any data itself, but rather is created with a child model
  and proxies its data. It has identical column types to this child model, and
  the changes in the child are propagated. The primary purpose of this model is
  to provide a way to sort a different model without modifying it. Note that
  the sort function used by the @class{gtk:tree-model-sort} object is not
  guaranteed to be stable.

  The use of this is best demonstrated through an example. In the following
  sample code we create two @class{gtk:tree-view} widgets each with a view of
  the same data. As the model is wrapped here by a @class{gtk:tree-model-sort}
  object, the two @class{gtk:tree-view} widgets can each sort their view of the
  data without affecting the other. By contrast, if we simply put the same
  model in each widget, then sorting the first would sort the second.

  @b{Example:} Using a @class{gtk:tree-model-sort} object
  @begin{pre}
(let* (;; Get the child model
       (child-model (gtk:my-model()))
       ;; Create the first tree view
       (sort-model1 (gtk:tree-model-sort-new-with-model child-model))
       (tree-view1 (gtk:tree-view-with-model sort-model1))
       ;; Create the second tree view
       (sort-model2 (gtk:tree-vmodel-sort-new-with-model child-model))
       (tree-view2 (gtk:tree-view-new-with-model sort-model2)))
  ;; Now we can sort the two models independently
  (setf (gtk:tree-sortable-sort-column-id sort-model1) col-1)
  (setf (gtk:tree-sortable-sort-column-id sort-model1) '(col1 :descending))
  ... )
  @end{pre}
  To demonstrate how to access the underlying child model from the sort model,
  the next example will be a callback for the @code{\"changed\"} signal of the
  @class{gtk:tree-selection} class. In this callback, we get a string from
  @code{COLUMN_1} of the model. We then modify the string, find the same
  selected row on the child model, and change the row there.

  @b{Example:} Accessing the child model in a selection changed callback
  @begin{pre}
(defun selection-changed (selection)
  (let* ((view (gtk:tree-selection-tree-view selection))
         ;; Get the current selected row and the model
         (sort-model (gtk:tree-view-model view))
         (sort-iter (gtk:tree-selection-selected selection))
         ;; Look up the current value on the selected row and get a new value
         (value (gtk:tree-model-value sort-model sort-iter col-1))
         (new-value (change-the-value value))
         ;; Get the child model and an iterator on the child model
         (model (gtk:tree-model-sort-model sort-model))
         (iter (gtk:tree-model-sort-convert-iter-to-child-iter sort-model
                                                               sort-iter)))
    ;; Change the value of the row in the child model
    (gtk:list-store-set-value model iter col-1 new-value)))
  @end{pre}
  @see-constructor{gtk:tree-model-sort-new-with-model}
  @see-slot{gtk:tree-model-sort-model}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-sortable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'tree-model-sort) t)
 "The @code{model} property of type @class{gtk:tree-model}
  (Read / Write / Construct) @br{}
  The model to sort.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-model-sort-model)
      "Accessor"
      (documentation 'tree-model-sort-model 'function)
 "@version{#2023-1-21}
  @syntax{(gtk:tree-model-sort-model object) => model}
  @argument[object]{a @class{gtk:tree-model-sort} object}
  @argument[model]{a @class{gtk:tree-model} child model being sorted}
  @begin{short}
    Accessor of the @slot[gtk:tree-model-sort]{model} slot of the
    @class{gtk:tree-model-sort} class.
  @end{short}
  The @fun{gtk:tree-model-sort-model} function returns the model the
  @class{gtk:tree-model-sort} object is sorting.
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-model-sort}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline tree-model-sort-new-with-model))

(defun tree-model-sort-new-with-model (model)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{A new @class{gtk:tree-model} object.}
  @begin{short}
    Creates a new tree model, with @arg{model} as the child model.
  @end{short}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-model-sort}"
  (make-instance 'tree-model-sort
                 :model model))

(export 'tree-model-sort-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_path_to_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_convert_child_path_to_path"
               tree-model-sort-convert-child-path-to-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @argument[path]{a @class{gtk:tree-path} instance to convert}
  @return{A @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path relative to @arg{model}.
  @end{short}
  That is, @arg{path} points to a path in the child model. The returned path
  will point to the same row in the sorted model. If @arg{path} is not a valid
  path on the child model, then @code{nil} is returned.
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:tree-path}"
  (model (g:object tree-model-sort))
  (path (g:boxed tree-path)))

(export 'tree-model-sort-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_iter_to_iter ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_convert_child_iter_to_iter"
               %tree-model-sort-convert-child-iter-to-iter) :boolean
  (model (g:object tree-model-sort))
  (sort-iter (g:boxed tree-iter))
  (child-iter (g:boxed tree-iter)))

(defun tree-model-sort-convert-child-iter-to-iter (model iter)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @argument[iter]{a valid @class{gtk:tree-iter} instance pointing to a
    row on the child model}
  @begin{return}
    A valid @class{gtk:tree-iter} iterator to a visible row in the sorted model,
    or @code{nil}.
  @end{return}
  @begin{short}
    Returns the iterator to the row in @arg{model} that corresponds to the row
    pointed at by @arg{iter}.
  @end{short}
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:tree-iter}"
  (let ((sort-iter (make-tree-iter)))
    (when (%tree-model-sort-convert-child-iter-to-iter model sort-iter iter)
      sort-iter)))

(export 'tree-model-sort-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_path_to_child_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_convert_path_to_child_path"
               tree-model-sort-convert-path-to-child-path)
    (g:boxed tree-path :return)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @argument[path]{a @class{gtk:tree-path} instance to convert}
  @return{A @class{gtk:tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path on the child model of @arg{model}.
  @end{short}
  That is, @arg{path} points to a location in @arg{model}. The returned path
  will point to the same location in the model not being sorted. If @arg{path}
  does not point to a location in the child model, @code{nil} is returned.
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:tree-path}"
  (model (g:object tree-model-sort))
  (path (g:boxed tree-path)))

(export 'tree-model-sort-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_iter_to_child_iter ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_convert_iter_to_child_iter"
               %tree-model-sort-convert-iter-to-child-iter) :void
  (model (g:object tree-model-sort))
  (child-iter (g:boxed tree-iter))
  (sorted-iter (g:boxed tree-iter)))

(defun tree-model-sort-convert-iter-to-child-iter (model iter)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @argument[iter]{a valid @class{gtk:tree-iter} iterator pointing to a
    row on @arg{model}}
  @begin{return}
    A @class{gtk:tree-iter} iterator.
  @end{return}
  @begin{short}
  Converts @arg{iter} to point to a row on @arg{model}.
  @end{short}
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:tree-iter}"
  (let ((child-iter (make-tree-iter)))
    (%tree-model-sort-convert-iter-to-child-iter model child-iter iter)
    child-iter))

(export 'tree-model-sort-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_reset_default_sort_func ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_reset_default_sort_func"
               tree-model-sort-reset-default-sort-func) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @begin{short}
    This resets the default sort function to be in the 'unsorted' state.
  @end{short}
  That is, it is in the same order as the child model. It will re-sort the
  model to be in the same order as the child model only if the
  @class{gtk:tree-model-sort} object is in 'unsorted' state.
  @see-class{gtk:tree-model-sort}"
  (model (g:object tree-model-sort)))

(export 'tree-model-sort-reset-default-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_clear_cache ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_clear_cache" tree-model-sort-clear-cache)
    :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @begin{short}
    This function should almost never be called. It clears the @arg{model} of
    any cached iterators that have not been reffed with the
    @fun{gtk:tree-model-ref-node} function.
  @end{short}
  This might be useful if the child model being sorted is static (and does not
  change often) and there has been a lot of unreffed access to nodes. As a side
  effect of this function, all unreffed iters will be invalid.
  @see-class{gtk:tree-model-sort}
  @see-function{gtk:tree-model-ref-node}"
  (model (g:object tree-model-sort)))

(export 'tree-model-sort-clear-cache)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tree_model_sort_iter_is_valid"
               tree-model-sort-iter-is-valid) :boolean
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[model]{a @class{gtk:tree-model-sort} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if @arg{iter} is
    invalid.}
  @begin{short}
    Checks if the given @arg{iter} is a valid iter for this
    @class{gtk:tree-model-sort} object.
  @end{short}
  @begin[Warning]{dictionary}
    This function is slow. Only use it for debugging and/or testing purposes.
  @end{dictionary}
  @see-class{gtk:tree-model-sort}
  @see-class{gtk:tree-iter}"
  (model (g:object tree-model-sort))
  (iter (g:boxed tree-iter)))

(export 'tree-model-sort-iter-is-valid)

;; --- End of file gtk3.tree-model-sort.lisp ----------------------------------
