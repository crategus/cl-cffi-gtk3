;;; ----------------------------------------------------------------------------
;;; gtk3.assistant.lisp
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
;;; GtkAssistant
;;;
;;;     A widget used to guide users through multi-step operations
;;;
;;; Types and Values
;;;
;;;     GtkAssistant
;;;     GtkAssistantPageType
;;;
;;; Functions
;;;
;;;     gtk_assistant_new
;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page
;;;
;;;     GtkAssistantPageFunc
;;;     gtk_assistant_set_forward_page_func
;;;
;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;     gtk_assistant_set_page_header_image                 Deprecated 3.2
;;;     gtk_assistant_get_page_header_image                 Deprecated 3.2
;;;     gtk_assistant_set_page_side_image                   Deprecated 3.2
;;;     gtk_assistant_get_page_side_image                   Deprecated 3.2
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_set_page_has_padding
;;;     gtk_assistant_get_page_has_padding
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page
;;;
;;; Properties
;;;
;;;     use-header-bar
;;;
;;; Child Properties
;;;
;;;     complete
;;;     has-padding
;;;     header-image
;;;     page-type
;;;     sidebar-image
;;;     title
;;;
;;; Style Properties
;;;
;;;     content-padding
;;;     header-padding
;;;
;;; Signals
;;;
;;;     apply
;;;     cancel
;;;     close
;;;     escape
;;;     prepare
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkAssistant
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAssistant implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkAssistantPageType" assistant-page-type
  (:export t
   :type-initializer "gtk_assistant_page_type_get_type")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))

#+liber-documentation
(setf (liber:alias-for-symbol 'assistant-page-type)
      "GEnum"
      (liber:symbol-documentation 'assistant-page-type)
 "@version{2025-06-24}
  @begin{declaration}
(gobject:define-genum \"GtkAssistantPageType\" gtk:assistant-page-type
  (:export t
   :type-initializer \"gtk_assistant_page_type_get_type\")
  (:content  0)
  (:intro    1)
  (:confirm  2)
  (:summary  3)
  (:progress 4)
  (:custom   5))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:content]{The page has regular contents. Both the Back and Forward
        buttons will be shown.}
      @entry[:intro]{The page contains an introduction to the assistant task.
        Only the Forward button will be shown if there is a next page.}
      @entry[:confirm]{The page lets the user confirm or deny the changes. The
        Back and Apply buttons will be shown.}
      @entry[:summary]{The page informs the user of the changes done. Only the
        Close button will be shown.}
      @entry[:progress]{Used for tasks that take a long time to complete, blocks
        the assistant until the page is marked as complete. Only the Back button
        will be shown.}
      @entry[:custom]{Used for when other page types are not appropriate. No
        buttons will be shown, and the application must add its own buttons
        through the @fun{gtk:assistant-add-action-widget} function.}
  @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration for determining the page role inside the
    @class{gtk:assistant} widget. It is used to handle buttons sensitivity and
    visibility.
  @end{short}

  Note that an assistant needs to end its page flow with a page of
  @val[gtk:assistant-page-type]{:confirm},
  @val[gtk:assistant-page-type]{:summary} or
  @val[gtk:assistant-page-type]{:progress} type to be correct. The Cancel
  button will only be shown if the page is not \"committed\". See the
  @fun{gtk:assistant-commit} function for details.
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-commit}
  @see-function{gtk:assistant-add-action-widget}")

;;; ----------------------------------------------------------------------------
;;; GtkAssistant
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAssistant" assistant
  (:superclass window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_assistant_get_type")
  ((use-header-bar
    assistant-use-header-bar
    "use-header-bar" "gint" t t)))

#+liber-documentation
(setf (documentation 'assistant 'type)
 "@version{2025-06-24}
  @begin{short}
    The @class{gtk:assistant} widget is used to represent a generally complex
    operation splitted in several steps, guiding the user through its pages and
    controlling the page flow to collect the necessary data.
  @end{short}

  @image[assistant]{Figure: GtkAssistant}

  The design of the @class{gtk:assistant} widget is that it controls what
  buttons to show and to make sensitive, based on what it knows about the page
  sequence and the type of each page, in addition to state information like the
  page completion and committed status.

  If you have a case that does not quite fit in an assistants way of handling
  buttons, you can use the @val[gtk:assistant-page-type]{:custom} page type of
  the @sym{gtk:assistant-page-type} enumeration and handle buttons yourself.
  @begin[GtkAssistant as GtkBuildable]{dictionary}
    The @class{gtk:assistant} implementation of the @class{gtk:buildable}
    interface exposes the action area as internal children with the
    name @code{\"action_area\"}. To add pages to an assistant in a
    @class{gtk:builder} object, simply add it as a @code{<child>} to the
    @class{gtk:assistant} widget and set its child properties as necessary.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:assistant} implementation has a single CSS node with the name
    @code{assistant}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[assistant:complete]{property}
      The @code{complete} child property of type @code{:boolean}
      (Read / Write) @br{}
      Setting to @em{true} marks a page as complete, that is, all the required
      fields are filled out. GTK uses this information to control the
      sensitivity of the navigation buttons. @br{}
      Default value: @em{false} @br{}
    @end{property}
    @begin[assistant:has-padding]{property}
      The @code{has-padding} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether the assistant adds padding around the page. @br{}
      Default value: @em{true}
    @end{property}
    @begin[assistant:header-image]{property}
      The @code{header-image} child property of type @class{gdk-pixbuf:pixbuf}
      (Read / Write) @br{}
      The image used to be displayed in the page header. @br{}
      @em{Warning:} The @code{header-image} child property has been deprecated
      since version 3.2 and should not be used in newly written code. Since
      GTK 3.2, a header is no longer shown. Add your header decoration to the
      page content instead.
    @end{property}
    @begin[assistant:page-type]{property}
      The @code{page-type} child property of type
      @sym{gtk:assistant-page-type} (Read / Write) @br{}
      The type of the assistant page. @br{}
      Default value: @val[gtk:assistant-page-type]{:content}
    @end{property}
    @begin[assistant:sidebar-image]{property}
      The @code{sidebar-image} child property of type
      @class{gdk-pixbuf:pixbuf} (Read / Write) @br{}
      The image used to be displayed in the sidebar. @br{}
      @em{Warning:} The @code{sidebar-image} child property has been
      deprecated since version 3.2 and should not be used in newly written
      code. Since GTK 3.2, the sidebar image is no longer shown.
    @end{property}
    @begin[assistant:title]{property}
      The @code{title} child property of type @code{:string} (Read / Write)
      @br{}
      The title of the page. @br{}
      Default value: @code{nil}
    @end{property}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[assistant:content-padding]{property}
      The @code{content-padding} style property of type @code{:int} (Read)
      @br{}
      The number of pixels around the content pages. @br{}
      @em{Warning:} The @code{content-padding} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. This style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1
    @end{property}
    @begin[assistant:header-padding]{property}
      The @code{header-padding} style property of type @code{:int} (Read)
      @br{}
      The number of pixels around the header. @br{}
      @em{Warning:} The @code{content-padding} has been deprecated since
      version 3.20 and should not be used in newly written code. This style
      property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 6
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[assistant::apply]{signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[assistant]{The @class{gtk:assistant} widget which received the
          signal.}
    @end{simple-table}
      The signal is emitted when the Apply button is clicked. The default
      behavior of the assistant is to switch to the page after the current page,
      unless the current page is the last one. A handler for the
      @sig[gtk:assistant]{apply} signal should carry out the actions for which
      the wizard has collected data. If the action takes a long time to
      complete, you might consider putting a
      @val[gtk:assistant-page-type]{:progress} page after the
      @val[gtk:assistant-page-type]{:confirm} page and handle this operation
      within the @sig[gtk:assistant]{prepare} signal of the progress page.
    @end{signal}
    @begin[assistant::cancel]{signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[assistant]{The @class{gtk:assistant} widget which received the
          signal.}
      @end{simple-table}
      The signal is emitted when the Cancel button is clicked.
    @end{signal}
    @begin[assistant::close]{signal}
      @begin{pre}
lambda (assistant)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[assistant]{The @class{gtk:assistant} widget which received the
          signal.}
      @end{simple-table}
      The signal is emitted either when the Close button of a summary page is
      clicked, or when the Apply button in the last page in the flow is clicked,
      which is the @val[gtk:assistant-page-type]{:confirm} page.
    @end{signal}
    @begin[assistant::escape]{signal}
      @begin{pre}
lambda (assistant)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[assistant]{The @class{gtk:assistant} widget which received the
          signal.}
      @end{simple-table}
      No documentation.
    @end{signal}
    @begin[assistant::prepare]{signal}
      @begin{pre}
lambda (assistant page)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[assistant]{The @class{gtk:assistant} widget which received the
          signal.}
      @entry[page]{The @class{gtk:widget} widget for the current page.}
      @end{simple-table}
      The signal is emitted when a new page is set as the assistants current
      page, before making the new page visible. A handler for this signal can
      do any preparations which are necessary before showing the page.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:assistant-new}
  @see-slot{gtk:assistant-use-header-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:assistant-use-header-bar -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-header-bar" 'assistant) t)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct) @br{}
  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'assistant-use-header-bar)
      "Accessor"
      (documentation 'assistant-use-header-bar 'function)
 "@version{2024-03-16}
  @syntax{(gtk:assistant-use-header-bar object) => setting}
  @syntax{(setf (gtk:assistant-use-header-bar object) setting)}
  @argument[object]{a @class{gtk:assistant} widget}
  @argument[setting]{@em{true} if the assistant uses a header bar}
  @begin{short}
    Accessor of the @slot[gtk:assistant]{use-header-bar} slot of the
    @class{gtk:assistant} class.
  @end{short}
  @em{True} if the assistant uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.
  @see-class{gtk:assistant}
  @see-class{gtk:header-bar}")

;;; ----------------------------------------------------------------------------
;;; Accessors of the Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk:assistant-child-complete -------------------------------------------

(define-child-property assistant-child-complete
                       "complete" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-complete)
      "Accessor"
      (documentation 'assistant-child-complete 'function)
 "@version{2025-06-26}
  @syntax{(gtk:assistant-child-complete container child) => complete}
  @syntax{(setf (gtk:assistant-child-complete container child) complete)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[complete]{a boolean whether the page is complete}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{complete} child property of the
    @class{gtk:assistant} class.
  @end{short}
  Setting the @prop[gtk:assistant]{complete} child property to @em{true} marks
  a page as complete, that is, all the required fields are filled out. GTK uses
  this information to control the sensitivity of the navigation buttons.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-page-complete}")

;;; --- gtk:assistant-child-has-padding ----------------------------------------

(define-child-property assistant-child-has-padding
                       "has-padding" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-has-padding)
      "Accessor"
      (documentation 'assistant-child-has-padding 'function)
 "@version{2025-06-26}
  @syntax{(gtk:assistant-child-has-padding container child) => setting}
  @syntax{(setf (gtk:assistant-child-has-padding container child) setting)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[setting]{a boolean whether the assistant adds padding around the
    page}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{has-padding} child property of the
    @class{gtk:assistant} class.
  @end{short}
  Whether the assistant adds padding around the page.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-page-has-padding}")

;;; --- gtk:assistant-child-header-image ---------------------------------------

;; not exported

(define-child-property assistant-child-header-image
                       "header-image" "GdkPixbuf" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-header-image)
      "Accessor"
      (documentation 'assistant-child-header-image 'function)
 "@version{2025-06-24}
  @syntax{(gtk:assistant-child-header-image container child) => image}
  @syntax{(setf (gtk:assistant-child-header-image container child) image)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[image]{a @class{gdk-pixbuf:pixbuf} image}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{header-image} child property of the
    @class{gtk:assistant} class.
  @end{short}
  The image used to be displayed in the page header.
  @begin[Warning]{dictionary}
    The @code{header-image} child property has been deprecated since version 3.2
    and should not be used in newly written code. Since GTK 3.2, a header is no
    longer shown. Add your header decoration to the page content instead.
  @end{dictionary}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:assistant-child-page-type ------------------------------------------

(define-child-property assistant-child-page-type
                       "page-type" "GtkAssistantPageType" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-page-type)
      "Accessor"
      (documentation 'assistant-child-page-type 'function)
 "@version{2025-06-24}
  @syntax{(gtk:assistant-child-page-type container child) => ptype}
  @syntax{(setf (gtk:assistant-child-page-type container child) ptype)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[ptype]{a value of the @sym{gtk:assistant-page-type} enumeration}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{page-type} child property of the
    @class{gtk:assistant} class.
  @end{short}
  The page type determines the page behavior in the assistant.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-symbol{gtk:assistant-page-type}
  @see-function{gtk:assistant-child-page-type}")

;;; --- gtk:assistant-child-sidebar-image --------------------------------------

;; not exported

(define-child-property assistant-child-sidebar-image
                       "sidebar-image" "GdkPixbuf" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-sidebar-image)
      "Accessor"
      (documentation 'assistant-child-sidebar-image 'function)
 "@version{2025-06-26}
  @syntax{(gtk:assistant-child-sidebar-image container child) => image}
  @syntax{(setf (gtk:assistant-child-sidebar-image container child) image)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[image]{a @class{gdk-pixbuf:pixbuf} image}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{sidebar-image} child property of the
    @class{gtk:assistant} class.
  @end{short}
  The image used to be displayed in the sidebar.
  @begin[Warning]{dictionary}
    The @prop[gtk:assistant]{sidebar-image} child property has been deprecated
    since version 3.2 and should not be used in newly written code. Since
    GTK 3.2, the sidebar image is no longer shown.
  @end{dictionary}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:assistant-child-title ----------------------------------------------

(define-child-property assistant-child-title
                       "title" "gchararray" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'assistant-child-title)
      "Accessor"
      (documentation 'assistant-child-title 'function)
 "@version{2025-06-26}
  @syntax{(gtk:assistant-child-title container child) => title}
  @syntax{(setf (gtk:assistant-child-title container child) title)}
  @argument[container]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} page of the assistant}
  @argument[title]{a string for the title of the page}
  @begin{short}
    Accessor of the @prop[gtk:assistant]{title} child property of the
    @class{gtk:assistant} class.
  @end{short}
  The title of the page.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_new
;;; ----------------------------------------------------------------------------

(declaim (inline assistant-new))

(defun assistant-new ()
 #+liber-documentation
 "@version{2024-03-16}
  @return{The @class{gtk:assistant} widget.}
  @short{Creates a new assistant.}
  @see-class{gtk:assistant}"
  (make-instance 'assistant))

(export 'assistant-new)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_current_page
;;; gtk_assistant_set_current_page
;;; ----------------------------------------------------------------------------

(defun (setf assistant-current-page) (index assistant)
  (cffi:foreign-funcall "gtk_assistant_set_current_page"
                        (g:object assistant) assistant
                        :int index
                        :void)
  index)

(cffi:defcfun ("gtk_assistant_get_current_page" assistant-current-page) :int
 #+liber-documentation
 "@version{2025-06-06}
  @syntax{(gtk:assistant-current-page assistant) => index}
  @syntax{(setf (gtk:assistant-current-page assistant) index)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer for the index of the page to switch to, starting
    from 0, if negative, the last page will be used, if greater than the number
    of pages in the assistant, nothing will be done}
  @begin{short}
    The @fun{gtk:assistant-current-page} function returns the page number of
    the current page in the assistant.
  @end{short}
  The @setf{gtk:assistant-current-page} function switches the page in the
  assistant to @arg{index}.

  Note that this will only be necessary in custom buttons, as the assistant
  flow can be set with the @fun{gtk:assistant-set-forward-page-func} function.
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-set-forward-page-func}"
  (assistant (g:object assistant)))

(export 'assistant-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_n_pages
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_get_n_pages" assistant-n-pages) :int
 #+liber-documentation
 "@version{#2024-03-16}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @return{The integer with the number of pages in @arg{assistant}.}
  @short{Returns the number of pages in the assistant.}
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_nth_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_get_nth_page" assistant-nth-page)
    (g:object widget)
 #+liber-documentation
 "@version{2025-06-06}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer for the index of a page in @arg{assistant},
    or -1 to get the last page}
  @begin{return}
    The @class{gtk:widget} child widget, or @code{nil} if the @arg{index}
    argument is out of bounds.
  @end{return}
  @begin{short}
    Returns the child widget contained in the assistant with the given page
    index.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant (g:object assistant))
  (index :int))

(export 'assistant-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_prepend_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_prepend_page" assistant-prepend-page) :int
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @return{The integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Prepends a page to the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-insert-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_append_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_append_page" assistant-append-page) :int
 #+liber-documentation
 "@version{2023-12-30}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @return{The integer with the index starting at 0 of the inserted page.}
  @begin{short}
    Appends a page to the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-insert-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget)))

(export 'assistant-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_insert_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_insert_page" assistant-insert-page) :int
 #+liber-documentation
 "@version{#2025-06-06}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of the assistant}
  @argument[position]{an integer for the index starting at 0 at which to
    insert @arg{page}, or -1 to append @arg{page} to the assistant}
  @return{The index starting from 0 of the inserted page.}
  @begin{short}
    Inserts a page in the assistant at a given position.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-remove-page}"
  (assistant (g:object assistant))
  (page (g:object widget))
  (position :int))

(export 'assistant-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_remove_page" assistant-remove-page) :void
 #+liber-documentation
 "@version{#2025-06-06}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[index]{an integer for the index of a page in the assistant, or -1
    to remove the last page}
  @begin{short}
    Removes the page with the given page index from the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-append-page}
  @see-function{gtk:assistant-prepend-page}
  @see-function{gtk:assistant-insert-page}"
  (assistant (g:object assistant))
  (index :int))

(export 'assistant-remove-page)

;;; ----------------------------------------------------------------------------
;;; GtkAssistantPageFunc
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation of the macro and compare the result with
;; similar code.

(gobject:define-cb-methods assistant-page-func :int ((current-page :int)))

#+liber-documentation
(setf (liber:alias-for-symbol 'assistant-page-func)
      "Callback"
      (liber:symbol-documentation 'assistant-page-func)
 "@version{#2025-06-06}
  @syntax{lambda (current) => result}
  @argument[current]{an integer for the page number used to calculate the next
    page}
  @argument[result]{an integer for the next page number}
  @begin{short}
    A callback function used by the @fun{gtk:assistant-set-forward-page-func}
    function to know which is the next page given a current one.
  @end{short}
  It is called both for computing the next page when the user presses the
  Forward button and for handling the behavior of the Last button.
  @see-class{gtk:assistant}
  @see-function{gtk:assistant-set-forward-page-func}")

(export 'assistant-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_forward_page_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_set_forward_page_func"
               %assistant-set-forward-page-func) :void
  (assistant (g:object assistant))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun assistant-set-forward-page-func (assistant func)
 #+liber-documentation
 "@version{#2025-04-24}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[func]{a @sym{gtk:assistant-page-func} page forwarding callback
    function, or @code{nil} to use the default one}
  @begin{short}
    Sets the page forwarding function to be @arg{func}.
  @end{short}
  This function will be used to determine what will be the next page when the
  user presses the Forward button. Setting @arg{func} to @code{nil} will make
  the assistant to use the default forward function, which just goes to the next
  visible page.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-func}"
  (if func
      (%assistant-set-forward-page-func
              assistant
              (cffi:callback assistant-page-func)
              (gobject:create-fn-ref assistant func)
              (cffi:callback assistant-page-func-destroy-notify))
      (%assistant-set-forward-page-func assistant
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'assistant-set-forward-page-func)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_type
;;; gtk_assistant_get_page_type
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-type) (ptype assistant page)
  (setf (assistant-child-page-type assistant page) ptype))

(defun assistant-page-type (assistant page)
 #+liber-documentation
 "@version{2025-06-24}
  @syntax{(gtk:assistant-page-type assistant page) => ptype}
  @syntax{(setf (gtk:assistant-page-type assistant page) ptype)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[ptype]{a value of the @sym{gtk:assistant-page-type} enumeration}
  @begin{short}
    The page type determines the page behavior in the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-symbol{gtk:assistant-page-type}"
  (assistant-child-page-type assistant page))

(export 'assistant-page-type)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_title
;;; gtk_assistant_get_page_title
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-title) (title assistant page)
  (setf (assistant-child-title assistant page) title))

(defun assistant-page-title (assistant page)
 #+liber-documentation
 "@version{2025-06-06}
  @syntax{(gtk:assistant-page-title assistant page) => title}
  @syntax{(setf (gtk:assistant-page-title assistant page) title)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[title]{a string for the new title for @arg{page}}
  @begin{short}
    The @fun{gtk:assistant-page-title} function gets the title for the page in
    the assistant.
  @end{short}
  The @setf{gtk:assistant-page-title} function sets a title. The title is
  displayed in the header area of the assistant when the page is the current
  page.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant-child-title assistant page))

(export 'assistant-page-title)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_header_image ()                  Deprecated 3.2
;;;
;;; Sets a header image for page.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_header_image ()                  Deprecated 3.2
;;;
;;; Gets the header image for page.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_side_image ()                    Deprecated 3.2
;;;
;;; Sets a side image for page.
;;;
;;; This image used to be displayed in the side area of the assistant when page
;;; is the current page.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_get_page_side_image ()                    Deprecated 3.2
;;;
;;; Gets the side image for page.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_complete
;;; gtk_assistant_get_page_complete
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-complete) (complete assistant page)
  (setf (assistant-child-complete assistant page) complete))

(defun assistant-page-complete (assistant page)
 #+liber-documentation
 "@version{2024-04-09}
  @syntax{(gtk:assistant-page-complete assistant page) => complete}
  @syntax{(setf (gtk:assistant-page-complete assistant page) complete)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[complete]{a boolean with the completeness status of the page}
  @begin{short}
    The @fun{gtk:assistant-page-complete} function gets whether the page is
    complete.
  @end{short}
  The @setf{gtk:assistant-page-complete} function sets whether the page contents
  are complete. This will make the assistant update the buttons state to be able
  to continue the task.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant-child-complete assistant page))

(export 'assistant-page-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_set_page_has_padding
;;; gtk_assistant_get_page_has_padding
;;; ----------------------------------------------------------------------------

(defun (setf assistant-page-has-padding) (setting assistant page)
  (setf (assistant-child-has-padding assistant page) setting))

(defun assistant-page-has-padding (assistant page)
 "@version{#2024-04-09}
  @syntax{(gtk:assistant-page-has-padding assistant page) => setting}
  @syntax{(setf (gtk:assistant-page-has-padding assistant page) setting)}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[page]{a @class{gtk:widget} page of @arg{assistant}}
  @argument[setting]{a boolean whether the page has padding}
  @begin{short}
    The @fun{gtk:assistant-page-has-padding} function gets whether the page has
    padding.
  @end{short}
  The @setf{gtk:assistant-page-has-padding} function sets whether the assistant
  is adding padding around the page.
  @see-class{gtk:assistant}
  @see-class{gtk:widget}"
  (assistant-child-has-padding assistant page))

(export 'assistant-page-has-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_add_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_add_action_widget" assistant-add-action-widget)
    :void
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Adds a child widget to the action area of the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-remove-action-widget}"
  (assistant (g:object assistant))
  (child (g:object widget)))

(export 'assistant-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_remove_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_remove_action_widget"
               assistant-remove-action-widget) :void
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Removes a child widget from the action area of the assistant.
  @end{short}
  @see-class{gtk:assistant}
  @see-class{gtk:widget}
  @see-function{gtk:assistant-add-action-widget}"
  (assistant (g:object assistant))
  (child (g:object widget)))

(export 'assistant-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_update_buttons_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_update_buttons_state"
               assistant-update-buttons-state) :void
 #+liber-documentation
 "@version{#2025-06-26}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Forces the assistant to recompute the buttons state.
  @end{short}
  GTK automatically takes care of this in most situations, for example, when
  the user goes to a different page, or when the visibility or completeness of
  a page changes. One situation where it can be necessary to call this function
  is when changing a value on the current page affects the future page flow of
  the assistant.
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-update-buttons-state)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_commit
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_commit" assistant-commit) :void
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Erases the visited page history so the Back button is not shown on the
    current page, and removes the Cancel button from subsequent pages.
  @end{short}
  Use this when the information provided up to the current page is hereafter
  deemed permanent and cannot be modified or undone. For example, showing a
  progress page to track a long running, unreversible operation after the user
  has clicked the Apply button on a confirmation page.
  @see-class{gtk:assistant}"
  (assistant (g:object assistant)))

(export 'assistant-commit)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_next_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_next_page" assistant-next-page) :void
 #+liber-documentation
 "@version{#2025-06-24}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Navigate to the next page.
  @end{short}
  It is a programming error to call this function when there is no next page.
  This function is for use when creating pages with the
  @val[gtk:assistant-page-type]{:custom} value of the
  @sym{gtk:assistant-page-type} enumeration.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-type}
  @see-function{gtk:assistant-previous-page}"
  (assistant (g:object assistant)))

(export 'assistant-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_assistant_previous_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_assistant_previous_page" assistant-previous-page) :void
 #+liber-documentation
 "@version{#2025-06-24}
  @argument[assistant]{a @class{gtk:assistant} widget}
  @begin{short}
    Navigate to the previous visited page.
  @end{short}
  It is a programming error to call this function when no previous page is
  available. This function is for use when creating pages with the
  @val[gtk:assistant-page-type]{:custom} value of the
  @sym{gtk:assistant-page-type} enumeration.
  @see-class{gtk:assistant}
  @see-symbol{gtk:assistant-page-type}
  @see-function{gtk:assistant-next-page}"
  (assistant (g:object assistant)))

(export 'assistant-previous-page)

;;; --- End of file gtk3.assistant.lisp ----------------------------------------
