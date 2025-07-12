(in-package :gtk-test)

(def-suite gtk-builder :in gtk-suite)
(in-suite gtk-builder)

(defvar *menus*
  "<interface>
    <menu id='app-menu'>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_New Window</attribute>
       <attribute name='action'>app.new</attribute>
       <attribute name='accel'>&lt;Primary&gt;n</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_About Bloatpad</attribute>
       <attribute name='action'>app.about</attribute>
      </item>
     </section>
     <section>
      <item>
       <attribute name='label' translatable='yes'>_Quit</attribute>
       <attribute name='action'>app.quit</attribute>
       <attribute name='accel'>&lt;Primary&gt;q</attribute>
      </item>
     </section>
     </menu>
    <menu id='menubar'>
     <submenu>
      <attribute name='label' translatable='yes'>_Edit</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Copy</attribute>
        <attribute name='action'>win.copy</attribute>
        <attribute name='accel'>&lt;Primary&gt;c</attribute>
       </item>
       <item>
        <attribute name='label' translatable='yes'>_Paste</attribute>
        <attribute name='action'>win.paste</attribute>
        <attribute name='accel'>&lt;Primary&gt;v</attribute>
       </item>
      </section>
     </submenu>
     <submenu>
      <attribute name='label' translatable='yes'>_View</attribute>
      <section>
       <item>
        <attribute name='label' translatable='yes'>_Fullscreen</attribute>
        <attribute name='action'>win.fullscreen</attribute>
        <attribute name='accel'>F11</attribute>
       </item>
      </section>
     </submenu>
    </menu>
   </interface>")

(defvar *dialog*
"<interface>
   <object class='GtkDialog' id='dialog1'>
     <child internal-child='vbox'>
       <object class='GtkVBox' id='vbox1'>
         <property name='border-width'>10</property>
         <child internal-child='action_area'>
           <object class='GtkHButtonBox' id='hbuttonbox1'>
             <property name='border-width'>20</property>
             <child>
               <object class='GtkButton' id='ok_button'>
                 <property name='label'>gtk-ok</property>
                 <property name='use-stock'>TRUE</property>
                 <signal name='clicked' handler='ok_button_clicked'/>
               </object>
             </child>
           </object>
         </child>
       </object>
     </child>
   </object>
 </interface>")

;;; --- Types and Values -------------------------------------------------------

;;;     GtkBuilderError

;;;     GtkBuilder

(test gtk-builder-class
  ;; Check type
  (is (g:type-is-object "GtkBuilder"))
  ;; Check registered name
  (is (eq 'gtk:builder
          (glib:symbol-for-gtype "GtkBuilder")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkBuilder")
          (g:gtype (cffi:foreign-funcall "gtk_builder_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkBuilder")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkBuilder")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkBuilder")))
  ;; Check class properties
  (is (equal '("translation-domain")
             (glib-test:list-properties "GtkBuilder")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkBuilder" GTK:BUILDER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_builder_get_type")
                       ((TRANSLATION-DOMAIN BUILDER-TRANSLATION-DOMAIN
                         "translation-domain" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkBuilder"))))

;;; ---  Properties ------------------------------------------------------------

(test gtk-builder-properties
  (glib-test:with-check-memory (builder)
    (is (typep (setf builder
                     (make-instance 'gtk:builder :from-string *dialog*))
               'gtk:builder))
    (is-false (gtk:builder-translation-domain builder))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_builder_new

(test gtk-builder-new
  (glib-test:with-check-memory (builder :strong 2)
    (let ((path (glib-sys:sys-path "test/resource/rtest-application.ui")))
      ;; gtk:builder-new is implemented with make-instance
      (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
      ;; Check Lisp extension for initializing gtk:builder
      (is (typep (setf builder
                       (make-instance 'gtk:builder :from-string *dialog*))
                 'gtk:builder))
      (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
      ;; Create builder from file
      (is (typep (setf builder
                       (make-instance 'gtk:builder
                                      :from-file (namestring path)))
                 'gtk:builder))
      (is (typep (gtk:builder-object builder "menubar") 'g:menu)))))

;;;     gtk_builder_new_from_file

(test gtk-builder-new-from-file
  (glib-test:with-check-memory (builder)
    (let ((path (glib-sys:sys-path "test/resource/rtest-application.ui")))
      (is (typep (setf builder
                       (gtk:builder-new-from-file path)) 'gtk:builder)))))

;;;     gtk_builder_new_from_resource

(test gtk-builder-new-from-resource
  (glib-test:with-check-memory (builder)
    (let* ((path (glib-sys:sys-path "test/resource/rtest-resource.gresource"))
           (resource (g:resource-load path)))
      (is-false (g:resources-register resource))
      (is (typep (setf builder
                       (gtk:builder-new-from-resource "/com/crategus/test/rtest-dialog.ui"))
                 'gtk:builder))
      (is-false (g:resources-unregister resource)))))

;;;     gtk_builder_new_from_string

(test gtk-builder-new-from-string
  (glib-test:with-check-memory (builder)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *menus*)) 'gtk:builder))))

;;;     gtk_builder_add_callback_symbol
;;;     gtk_builder_add_callback_symbols
;;;     gtk_builder_lookup_callback_symbol

;;;     gtk_builder_add_from_file

(test gtk-builder-add-from-file
  (glib-test:with-check-memory (builder)
    (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
    (is-true (gtk:builder-add-from-file builder
                 (glib-sys:sys-path "test/resource/rtest-application.ui")))))

;;;     gtk_builder_add_from_resource

(test gtk-builder-add-from-resource
  (glib-test:with-check-memory (builder)
    (let* ((filename (glib-sys:sys-path "test/resource/rtest-resource.gresource"))
           (resource (g:resource-load filename)))
    (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
    (is-false (g:resources-register resource))
    (is-true (gtk:builder-add-from-resource builder
                                            "/com/crategus/test/rtest-dialog.ui"))
    (is-false (g:resources-unregister resource)))))

;;;     gtk_builder_add_from_string

(test gtk-builder-add-from-string
  (glib-test:with-check-memory (builder)
    (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
    (is-true (gtk:builder-add-from-string builder *menus*))))

;;;     gtk_builder_add_objects_from_file

(test gtk-builder-add-objects-from-file.1
  (let ((builder (gtk:builder-new))
        (path (glib-sys:sys-path "test/resource/rtest-dialog.ui")))
    (is-true (gtk:builder-add-objects-from-file builder path "dialog1"))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (equal '(GTK:DIALOG GTK:BOX GTK:BUTTON-BOX GTK:BUTTON)
               (mapcar 'type-of (gtk:builder-objects builder))))))

(test gtk-builder-add-objects-from-file.2
  (glib-test:with-check-memory (builder :strong 2)
    (let ((path (glib-sys:sys-path "test/resource/rtest-dialog2.ui")))
      (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
      (is-true (gtk:builder-add-objects-from-file builder path
                                                          "button_cancel"
                                                          "button_ok"))
      (is (typep (gtk:builder-object builder "button_cancel") 'gtk:button))
      (is (typep (gtk:builder-object builder "button_ok") 'gtk:button))
      (is (equal '(GTK:BUTTON GTK:BUTTON)
                 (mapcar 'type-of (gtk:builder-objects builder)))))))

;;;     gtk_builder_add_objects_from_string

(test gtk-builder-add-objects-from-string
  (glib-test:with-check-memory (builder :strong 4)
    (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
    (is-true (gtk:builder-add-objects-from-string builder *dialog* "dialog1"))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (equal '(GTK:DIALOG GTK:BOX GTK:BUTTON-BOX GTK:BUTTON)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_extend_with_template

;;;     gtk_builder_object

(test gtk-builder-object
  (glib-test:with-check-memory (builder :strong 2)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *dialog*)) 'gtk:builder))
    (is (typep (gtk:builder-object builder "dialog1") 'gtk:dialog))
    (is (typep (gtk:builder-object builder "ok_button") 'gtk:button))))

;;;     gtk_builder_objects

(test gtk-builder-objects
  (glib-test:with-check-memory (builder :strong 6)
    (is (typep (setf builder (gtk:builder-new)) 'gtk:builder))
    (is (equal '() (gtk:builder-objects builder)))
    (is-true (gtk:builder-add-from-string builder *menus*))
    (is (equal '(g:menu g:menu)
               (mapcar 'type-of (gtk:builder-objects builder))))
    (is-true (gtk:builder-add-from-string builder *dialog*))
    (is (equal '(GTK:DIALOG G:MENU G:MENU GTK:BOX GTK:BUTTON-BOX GTK:BUTTON)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_builder_expose_object
;;;     gtk_builder_connect_signals
;;;     gtk_builder_connect_signals_full

;;;     gtk_builder_get_type_from_name

(test gtk-builder-type-from-name
  (glib-test:with-check-memory (builder)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *dialog*)) 'gtk:builder))
    (is (eq (g:gtype "GtkDialog")
            (gtk:builder-type-from-name builder "GtkDialog")))
    (is (eq (g:gtype "GtkButton")
            (gtk:builder-type-from-name builder "GtkButton")))))

;;;     gtk_builder_value_from_string
;;;     gtk_builder_value_from_string_type

;;; 2025-07-11
