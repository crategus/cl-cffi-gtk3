(in-package :gtk-test)

(def-suite gtk-buildable :in gtk-suite)
(in-suite gtk-buildable)

(defvar *dialog-gtk-buildable*
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

;;;     GtkBuildable

(test gtk-buildable-interface
  ;; Check type
  (is (g:type-is-interface "GtkBuildable"))
  ;; Check registered name
  (is (eq 'gtk:buildable
          (glib:symbol-for-gtype "GtkBuildable")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkBuildable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkBuildable" GTK:BUILDABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_buildable_get_type"))
             (gobject:get-gtype-definition "GtkBuildable"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_buildable_set_name
;;;     gtk_buildable_get_name

(test gtk-buildable-name.1
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:button)) 'gtk:button))
    (is-false (gtk:buildable-name button))
    (setf (gtk:buildable-name button) "button")
    (is (string= "button" (gtk:buildable-name button)))))

(test gtk-buildable-name.2
  (glib-test:with-check-memory (builder (dialog 2) (vbox 2) (hbuttonbox 2) :strong 3)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *dialog-gtk-buildable*))
               'gtk:builder))
    (is (typep (setf dialog
                     (gtk:builder-object builder "dialog1")) 'gtk:dialog))
    (is (typep (setf vbox (gtk:builder-object builder "vbox1")) 'gtk:box))
    (is (typep (setf hbuttonbox
                     (gtk:builder-object builder "hbuttonbox1"))
               'gtk:button-box))
    (is (string= "dialog1" (gtk:buildable-name dialog)))
    (is (string= "vbox1" (gtk:buildable-name vbox)))
    (is (string= "hbuttonbox1" (gtk:buildable-name hbuttonbox)))
    (is-false (gtk:widget-destroy dialog))))

;;;     gtk_buildable_add_child

(test gtk-buildable-add-child
  (glib-test:with-check-memory (builder (button-box 4) (button1 2) (label 2) :strong 6)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *dialog-gtk-buildable*))
               'gtk:builder))
    (is (typep (setf button-box
                     (gtk:builder-object builder "hbuttonbox1"))
               'gtk:button-box))
    (is (typep (setf button1 (make-instance 'gtk:button)) 'gtk:button))
    (is (typep (setf label (make-instance 'gtk:label)) 'gtk:label))
    (is (equal '(GTK:DIALOG GTK:BOX GTK:BUTTON-BOX GTK:BUTTON)
               (mapcar 'type-of (gtk:builder-objects builder))))
    (is (typep button-box 'gtk:button-box))
    (gtk:buildable-add-child button-box builder button1 (cffi:null-pointer))
    (gtk:buildable-add-child button-box builder label nil)
    (is (equal '(GTK:DIALOG GTK:BOX GTK:BUTTON-BOX GTK:BUTTON)
               (mapcar 'type-of (gtk:builder-objects builder))))))

;;;     gtk_buildable_set_buildable_property
;;;     gtk_buildable_construct_child
;;;     gtk_buildable_custom_tag_start
;;;     gtk_buildable_custom_tag_end
;;;     gtk_buildable_custom_finished
;;;     gtk_buildable_parser_finished

;;;     gtk_buildable_get_internal_child

(test gtk-buildable-internal-child
  (glib-test:with-check-memory (builder (dialog 2) :strong 2)
    (is (typep (setf builder
                     (gtk:builder-new-from-string *dialog-gtk-buildable*))
               'gtk:builder))
    (is (typep (setf dialog
                     (gtk:builder-object builder "dialog1")) 'gtk:dialog))
    (is (typep (gtk:buildable-internal-child dialog builder "action_area")
                'gtk:button-box))
    (is-false (gtk:widget-destroy dialog))))

;;; 2025-06-05
