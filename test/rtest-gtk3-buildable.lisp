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

(test buildable-interface
  ;; Type check
  (is (g:type-is-interface "GtkBuildable"))
  ;; Check the registered name
  (is (eq 'gtk:buildable
          (glib:symbol-for-gtype "GtkBuildable")))
  ;; Get the interface properties
  (is (equal '()
             (list-interface-properties "GtkBuildable")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkBuildable"
                                  GTK-BUILDABLE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_buildable_get_type"))
             (gobject:get-g-type-definition "GtkBuildable"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_buildable_set_name
;;;     gtk_buildable_get_name

(test buildable-name.1
  (let ((button (make-instance 'gtk:button)))
    (is-false (gtk:buildable-name button))
    (setf (gtk:buildable-name button) "button")
    (is (string= "button" (gtk:buildable-name button)))))

(test buildable-name.2
  (let* ((builder (gtk:builder-new-from-string *dialog-gtk-buildable*))
         (dialog (gtk:builder-object builder "dialog1"))
         (vbox (gtk:builder-object builder "vbox1"))
         (hbuttonbox (gtk:builder-object builder "hbuttonbox1")))
    (is (string= "dialog1" (gtk:buildable-name dialog)))
    (is (string= "vbox1" (gtk:buildable-name vbox)))
    (is (string= "hbuttonbox1" (gtk:buildable-name hbuttonbox)))))

;;;     gtk_buildable_add_child

(test buildable-add-child
  (let* ((builder (gtk:builder-new-from-string *dialog-gtk-buildable*))
         (button-box (gtk:builder-object builder "hbuttonbox1"))
         (button1 (make-instance 'gtk:button))
         (label (make-instance 'gtk:label)))
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

(test buildable-internal-child
  (let* ((builder (gtk:builder-new-from-string *dialog-gtk-buildable*))
         (dialog (gtk:builder-object builder "dialog1")))
    (is (typep (gtk:buildable-internal-child dialog builder "action_area")
                'gtk:button-box))))

;;; --- 2023-5-29 --------------------------------------------------------------
