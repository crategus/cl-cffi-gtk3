;;; ----------------------------------------------------------------------------
;;; gdk.properties.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
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
;;; Properties and Atoms
;;;
;;;     Functions to manipulate properties on windows
;;;
;;; Synopsis
;;;
;;;     GdkAtom
;;;
;;;     GDK_ATOM_TO_POINTER
;;;     GDK_POINTER_TO_ATOM
;;;     GDK_NONE
;;;
;;;     gdk_text_property_to_utf8_list_for_display
;;;     gdk_utf8_to_string_target
;;;     gdk_atom_intern
;;;     gdk_atom_intern_static_string
;;;     gdk_atom_name
;;;     gdk_property_get
;;;     gdk_property_change
;;;
;;;     GdkPropMode
;;;
;;;     gdk_property_delete
;;;
;;; Description
;;;
;;; Each window under X can have any number of associated properties attached
;;; to it. Properties are arbitrary chunks of data identified by atoms. (An
;;; atom is a numeric index into a string table on the X server. They are used
;;; to transfer strings efficiently between clients without having to transfer
;;; the entire string.) A property has an associated type, which is also
;;; identified using an atom.
;;;
;;; A property has an associated format, an integer describing how many bits
;;; are in each unit of data inside the property. It must be 8, 16, or 32. When
;;; data is transferred between the server and client, if they are of different
;;; endianesses it will be byteswapped as necessary according to the format of
;;; the property. Note that on the client side, properties of format 32 will be
;;; stored with one unit per long, even if a long integer has more than 32 bits
;;; on the platform. (This decision was apparently made for Xlib to maintain
;;; compatibility with programs that assumed longs were 32 bits, at the expense
;;; of programs that knew better.)
;;;
;;; The functions in this section are used to add, remove and change properties
;;; on windows, to convert atoms to and from strings and to manipulate some
;;; types of data commonly stored in X window properties.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAtom
;;; ----------------------------------------------------------------------------

#+nil
(defctype atom :pointer)

#+nil
(setf (liber:alias-for-symbol 'atom)
      "Type"
      (liber:symbol-documentation 'atom)
 "@version{#2021-03-24}
  @begin{short}
    An opaque type representing a string as an index into a table of strings on
    the X server.
  @end{short}

  An atom is a numeric index represented as a pointer into a string table on
  the X server. They are used to transfer strings efficiently between clients
  without having to transfer the entire string.

  Use the @fun{gdk:atom-intern} function to get the pointer for a string
  representing an atom and the @fun{gdk:atom-name} function to get the string
  for an atom.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:atom-intern \"CLIPBOARD\") => #.(SB-SYS:INT-SAP #X00000045)
(gdk:atom-name (make-pointer #x45)) => \"CLIPBOARD\"
    @end{pre}
  @end{dictionary}
  @see-function{gdk:atom-name}
  @see-function{gdk:atom-intern}")

;;; ----------------------------------------------------------------------------

;; Extension to replace a value of type GdkAtom with a string

(cffi:define-foreign-type atom-as-string-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser atom-as-string))

(defmethod cffi:translate-from-foreign (value (type atom-as-string-type))
  (%atom-name value))

(defmethod cffi:translate-to-foreign (value (type atom-as-string-type))
  (%atom-intern value nil))

#+liber-documentation
(setf (liber:alias-for-class 'atom-as-string)
      "Type"
      (documentation 'atom-as-string 'type)
 "@version{2024-03-23}
  @begin{short}
    The @class{gdk:atom-as-string} type represents the C @code{GAtom} type which
    represents a string as an index into a table of strings on the X server.
  @end{short}
  They are used to transfer strings efficiently between clients without having
  to transfer the entire string.
  @begin[Lisp implementation]{dictionary}
    In the Lisp implementation, a C @code{GAtom} is automatically converted to
    a Lisp string and vice versa. No other functions are implemented for this
    type.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(cffi:convert-to-foreign \"gboolean\" 'gdk:atom-as-string)
=> #.(SB-SYS:INT-SAP #X00000081)
(cffi:convert-from-foreign * 'gdk:atom-as-string)
=> \"gboolean\"
    @end{pre}
  @end{dictionary}")

(export 'atom-as-string)

;;; ----------------------------------------------------------------------------
;;; GDK_ATOM_TO_POINTER()
;;;
;;; #define GDK_ATOM_TO_POINTER(atom) (atom)
;;;
;;; Converts a GdkAtom into a pointer type.
;;;
;;; atom :
;;;     a GdkAtom.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_POINTER_TO_ATOM()
;;;
;;; #define GDK_POINTER_TO_ATOM(ptr) ((GdkAtom)(ptr))
;;;
;;; Extracts a GdkAtom from a pointer. The GdkAtom must have been stored in the
;;; pointer with GDK_ATOM_TO_POINTER().
;;;
;;; ptr :
;;;     a pointer containing a GdkAtom.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_NONE                                               not exported
;;; ----------------------------------------------------------------------------

#+nil
(defparameter +none+ "NONE" ; in sbcl defconstant does not work for a string
 #+liber-documentation
 "@version{#2025-08-31}
  @variable-value{\"NONE\"}
  A null value for @sym{gdk:atom}, used in a similar way as None in the Xlib
  API.")

#+nil
(setf (liber:alias-for-variable '+none+) "Constant")

;;; ----------------------------------------------------------------------------
;;; gdk_text_property_to_utf8_list_for_display ()
;;;
;;; gint gdk_text_property_to_utf8_list_for_display (GdkDisplay *display,
;;;                                                  GdkAtom encoding,
;;;                                                  gint format,
;;;                                                  const guchar *text,
;;;                                                  gint length,
;;;                                                  gchar ***list);
;;;
;;; Converts a text property in the given encoding to a list of UTF-8 strings.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; encoding :
;;;     an atom representing the encoding of the text
;;;
;;; format :
;;;     the format of the property
;;;
;;; text :
;;;     the text to convert
;;;
;;; length :
;;;     the length of text, in bytes
;;;
;;; list :
;;;     location to store the list of strings or NULL. The list should be freed
;;;     with g_strfreev()
;;;
;;; Returns :
;;;     the number of strings in the resulting list
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_utf8_to_string_target ()
;;;
;;; gchar * gdk_utf8_to_string_target (const gchar *str);
;;;
;;; Converts an UTF-8 string into the best possible representation as a STRING.
;;; The representation of characters not in STRING is not specified; it may be
;;; as pseudo-escape sequences \x{ABCD}, or it may be in some other form of
;;; approximation.
;;;
;;; str :
;;;     a UTF-8 string
;;;
;;; Returns :
;;;     the newly-allocated string, or NULL if the conversion failed. (It should
;;;     not fail for any properly formed UTF-8 string unless system limits like
;;;     memory or file descriptors are exceeded.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_atom_intern                                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_atom_intern" %atom-intern) :pointer
  (name :string)
  (only-if-exists :boolean))

(defun atom-intern (name &optional (only-if-exists nil))
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[name]{a string for the atom name}
  @argument[only-if-exists]{if @em{true}, GDK is allowed to not create a new
    atom, but just return a @code{cffi:null-pointer} if the requested atom does
    not already exists. Currently, the flag is ignored, since checking the
    existance of an atom is as expensive as creating it.}
  @return{The @sym{gdk:atom} pointer corresponding to @arg{name}.}
  @begin{short}
    Finds or creates an atom corresponding to a given string.
  @end{short}
  The optional @arg{only-if-exists} argument has the @em{false} default value.
  @see-symbol{gdk:atom}
  @see-function{gdk:atom-name}"
  (%atom-intern name only-if-exists))

;;; ----------------------------------------------------------------------------
;;; gdk_atom_intern_static_string ()
;;;
;;; GdkAtom gdk_atom_intern_static_string (const gchar *atom_name);
;;;
;;; Finds or creates an atom corresponding to a given string.
;;;
;;; Note that this function is identical to gdk_atom_intern() except that if a
;;; new GdkAtom is created the string itself is used rather than a copy. This
;;; saves memory, but can only be used if the string will always exist. It can
;;; be used with statically allocated strings in the main program, but not with
;;; statically allocated memory in dynamically loaded modules, if you expect to
;;; ever unload the module again (for example, do not use this function in GTK
;;; theme engines).
;;;
;;; atom_name :
;;;     a static string
;;;
;;; Returns :
;;;     the atom corresponding to atom_name
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_atom_name                                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_atom_name" %atom-name) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[atom]{a @sym{gdk:atom} pointer}
  @begin{return}
    The string containing the string corresponding to @arg{atom}.
  @end{return}
  @begin{short}
    Determines the string corresponding to the given atom.
  @end{short}
  @see-symbol{gdk:atom}
  @see-function{gdk:atom-intern}"
  (atom :pointer))

;;; ----------------------------------------------------------------------------
;;; gdk_property_get ()
;;;
;;; gboolean gdk_property_get (GdkWindow *window,
;;;                            GdkAtom property,
;;;                            GdkAtom type,
;;;                            gulong offset,
;;;                            gulong length,
;;;                            gint pdelete,
;;;                            GdkAtom *actual_property_type,
;;;                            gint *actual_format,
;;;                            gint *actual_length,
;;;                            guchar **data);
;;;
;;; Retrieves a portion of the contents of a property. If the property does not
;;; exist, then the function returns FALSE, and GDK_NONE will be stored in
;;; actual_property_type.
;;;
;;; Note
;;;
;;; The XGetWindowProperty() function that gdk_property_get() uses has a very
;;; confusing and complicated set of semantics. Unfortunately,
;;; gdk_property_get() makes the situation worse instead of better (the
;;; semantics should be considered undefined), and also prints warnings to
;;; stderr in cases where it should return a useful error to the program. You
;;; are advised to use XGetWindowProperty() directly until a replacement
;;; function for gdk_property_get() is provided.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; property :
;;;     the property to retrieve
;;;
;;; type :
;;;     the desired property type, or GDK_NONE, if any type of data is
;;;     acceptable. If this does not match the actual type, then actual_format
;;;     and actual_length will be filled in, a warning will be printed to stderr
;;;     and no data will be returned.
;;;
;;; offset :
;;;     the offset into the property at which to begin retrieving data, in 4
;;;     byte units.
;;;
;;; length :
;;;     the length of the data to retrieve in bytes. Data is considered to be
;;;     retrieved in 4 byte chunks, so length will be rounded up to the next
;;;     highest 4 byte boundary (so be careful not to pass a value that might
;;;     overflow when rounded up).
;;;
;;; pdelete :
;;;     if TRUE, delete the property after retrieving the data.
;;;
;;; actual_property_type :
;;;     location to store the actual type of the property
;;;
;;; actual_format :
;;;     location to store the actual return format of the data; either 8, 16 or
;;;     32 bits
;;;
;;; actual_length :
;;;     location to store the length of the retrieved data, in bytes. Data
;;;     returned in the 32 bit format is stored in a long variable, so the
;;;     actual number of 32 bit elements should be be calculated via
;;;     actual_length / sizeof(glong) to ensure portability to 64 bit systems.
;;;
;;; data :
;;;     location to store a pointer to the data. The retrieved data should be
;;;     freed with g_free() when you are finished using it
;;;
;;; Returns :
;;;     TRUE if data was successfully received and stored in data, otherwise
;;;     FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_property_change ()
;;;
;;; void gdk_property_change (GdkWindow *window,
;;;                           GdkAtom property,
;;;                           GdkAtom type,
;;;                           gint format,
;;;                           GdkPropMode mode,
;;;                           const guchar *data,
;;;                           gint nelements);
;;;
;;; Changes the contents of a property on a window.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; property :
;;;     the property to change
;;;
;;; type :
;;;     the new type for the property. If mode is GDK_PROP_MODE_PREPEND or
;;;     GDK_PROP_MODE_APPEND, then this must match the existing type or an error
;;;     will occur.
;;;
;;; format :
;;;     the new format for the property. If mode is GDK_PROP_MODE_PREPEND or
;;;     GDK_PROP_MODE_APPEND, then this must match the existing format or an
;;;     error will occur.
;;;
;;; mode :
;;;     a value describing how the new data is to be combined with the current
;;;     data.
;;;
;;; data :
;;;     the data (a guchar * gushort *, or gulong *, depending on format), cast
;;;     to a guchar *.
;;;
;;; nelements :
;;;     the number of elements of size determined by the format, contained in
;;;     data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkPropMode
;;;
;;; typedef enum {
;;;   GDK_PROP_MODE_REPLACE,
;;;   GDK_PROP_MODE_PREPEND,
;;;   GDK_PROP_MODE_APPEND
;;; } GdkPropMode;
;;;
;;; Describes how existing data is combined with new data when using
;;; gdk_property_change().
;;;
;;; GDK_PROP_MODE_REPLACE
;;;     the new data replaces the existing data.
;;;
;;; GDK_PROP_MODE_PREPEND
;;;     the new data is prepended to the existing data.
;;;
;;; GDK_PROP_MODE_APPEND
;;;     the new data is appended to the existing data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_property_delete ()
;;;
;;; void gdk_property_delete (GdkWindow *window, GdkAtom property);
;;;
;;; Deletes a property from a window.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; property :
;;;     the property to delete
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.properties.lisp ----------------------------------------
