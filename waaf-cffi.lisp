
#| 
Provide macros 

    WITH-ARRAY-AS-FOREIGN-POINTER

    WITH-ARRAYS-AS-FOREIGN-POINTERS
   
    SBDEFINE-ALIEN-ROUTINE 

    WITH-LISP-VALUES-AS-FOREIGN-POINTERS 

See MANUAL.txt 


This work is placed into the public domain, or released
under the LGPL if public domain does not exist in your
jurisdiciton.  See LICENSE.txt.


|# 

(defpackage waaf-cffi ;; WITH-ARRAY-AS-FOREIGN
  (:use :cl)
  (:nicknames :waaf)
  (:export
   ;; Macros to bind arrays to foreign pointers
   #:with-array-as-foreign-pointer
   #:with-arrays-as-foreign-pointers
   ;;
   ;; Macro to allow sbcl style function definition, with
   ;; automatic conversion of lisp values to pointers
   #:sbdefine-alien-routine
   ;;
   ;; Macro to put lisp values into foreign pointers, invoke foreign
   ;; code in the body, and then release the foreign memory
   #:with-lisp-values-as-foreign-pointers 

   ;; Various definitions of CFFI and lisp types.  Useful if users
   ;; want to make lisp arrays of types that match foreign types
   #:machine-pointer
   #:machine-int   #:machine-uint      #:machine-unsigned-int
   #:machine-char  #:machine-uchar     #:machine-unsigned-char
   #:machine-short #:machine-ushort    #:machine-unsigned-short
   #:machine-long  #:machine-ulong     #:machine-unsigned-long
   #:machine-llong #:machine-ullong    #:machine-unsigned-long-long
   #:machine-size_t ;; "almost always unsigned int" accordig to cffi manual
                    ;; but we make smarter (?) guess that it is same size as
                    ;; pointer 
   ;;
   ;; The CFFI foreign integer type that is the same as a :POINTER 
   #:+cffi-pointer-type+
   ;; Lisp types like (unsigned-byte 32) that can be placed into
   ;; corresponding CFFI slots in foreign memory
   #:+lisp-pointer-type+   ;; the lisp integer that is equivalent
			   ;; in size to a pointer
   #:+lisp-size_t-type+    ;; this is guessed to be same size as pointer
   #:+lisp-int-type+     #:+lisp-uint-type+     #:+lisp-unsigned-int-type+
   #:+lisp-char-type+    #:+lisp-uchar-type+    #:+lisp-unsigned-char-type+
   #:+lisp-short-type+   #:+lisp-ushort-type+   #:+lisp-unsigned-short-type+
   #:+lisp-long-type+    #:+lisp-ulong-type+    #:+lisp-unsigned-long-type+
   #:+lisp-llong-type+   #:+lisp-ullong-type+   #:+lisp-unsigned-long-long-type+
   ))

(in-package waaf)
 
;; ****** NOTE ****** The safety setting currently set below is not
;;   the fastest.  SAFETY=0 gives a 300% speedup for sbcl over the
;;   default value SAFETY=1.  SAFETY=0 should be OK, however, because
;;   risky generic arrays with unpredictable types automatically have
;;   their safety bumped up to 2 regardless of +SAFETY-SETTING+
(eval-when (:compile-toplevel)
  (defconstant +safety-setting+ 2) ;; FIXME - take this to safety=0 if we trust it
  (defconstant +speed-setting+  3))

 
;; Define various types useful both in this code and for users.  for
;; instance, MACHINE-POINTER is the integer size that represents a
;; pointer.  The only ones in this code are things like +LISP-INT-TYPE+
;; which is used to coerce lisp values so they can be put into foreign
;; memory of type :INT
(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (not (boundp '+lisp-int-type+)) ;; don't rebind defconstant; SBCL complains
    (progn

      (defconstant +lisp-int-type+ 
	(cond ((= 4 (cffi:foreign-type-size :int))
	       '(signed-byte 32))
	      ((= 8 (cffi:foreign-type-size :int))
	       '(signed-byte 64))
	      (t (error "Failed type lisp-int-type"))))
      
      (defconstant +lisp-uint-type+ 
	(cond ((= 4 (cffi:foreign-type-size :unsigned-int))
	       '(unsigned-byte 32))
	      ((= 8 (cffi:foreign-type-size :unsigned-int))
	       '(unsigned-byte 64))
	      (t (error "Failed type lisp-uint-type"))))


      (defconstant +lisp-pointer-type+  
	(cond ((= 4 (cffi:foreign-type-size :pointer))
	       '(unsigned-byte 32))
	      ((= 8 (cffi:foreign-type-size :pointer))
	       '(unsigned-byte 64))
	      (t (error "Failed type lisp-pointer-type"))))

     
      (defconstant +cffi-pointer-type+ 
	(cond ((= 4 (cffi:foreign-type-size :pointer))
	       :uint32)
	      ((= 8 (cffi:foreign-type-size :pointer))
	       :uint64)
	      (t (error "Failed type cffi-pointer-type"))))
      
      (defconstant +lisp-char-type+ 
	(cond ((= 1 (cffi:foreign-type-size :char))
	       '(signed-byte 8))
	      (t (error "Failed type lisp-char-type"))))
      (defconstant +lisp-uchar-type+ 
	(cond ((= 1 (cffi:foreign-type-size :uchar))
	       '(unsigned-byte 8))
	      (t (error "Failed type lisp-uchar-type")))) 
      
      (defconstant +lisp-short-type+
	(cond ((= 2 (cffi:foreign-type-size :short))
	       '(signed-byte 16))
	      (t (error "Failed type lisp-short-type"))))
      (defconstant +lisp-ushort-type+ 
	(cond ((= 2 (cffi:foreign-type-size :ushort))
	       '(unsigned-byte 16))
	      (t (error "Failed type lisp-ushort-type"))))

      
      (defconstant +lisp-long-type+ 
	(cond ((= 4 (cffi:foreign-type-size :long))
	       '(signed-byte 32))
	      ((= 8 (cffi:foreign-type-size :long))
	       '(signed-byte 64))
	      (t (error "Failed type lisp-long-type"))))
      (defconstant +lisp-ulong-type+ 
	(cond ((= 4 (cffi:foreign-type-size :ulong))
	       '(unsigned-byte 32))
	      ((= 8 (cffi:foreign-type-size :ulong))
	       '(unsigned-byte 64))
	      (t (error "Failed type lisp-ulong-type"))))
      ;;
      (defconstant +lisp-llong-type+ 
	(cond ((= 4 (cffi:foreign-type-size :llong))
	       '(signed-byte 32))
	      ((= 8 (cffi:foreign-type-size :llong))
	       '(signed-byte 64))
	      (t (error "Failed type lisp-llong-type"))))
      (defconstant +lisp-ullong-type+ 
	(cond ((= 4 (cffi:foreign-type-size :ullong))
	       '(unsigned-byte 32))
	      ((= 8 (cffi:foreign-type-size :ullong))
	       '(unsigned-byte 64))
	      (t (error "Failed type lisp-ullong-type"))))
      

      ;;
      )))


;; some additional synonyms - sometimes SBCL requires these in separate block, like here
(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (not (boundp '+lisp-unsigned-int-type+))
    (defconstant +lisp-unsigned-int-type+  +lisp-uint-type+)
    (defconstant +lisp-unsigned-long-type+ +lisp-ulong-type+)
    (defconstant +lisp-unsigned-long-long-type+ +lisp-ullong-type+)
    (defconstant +lisp-unsigned-short-type+ +lisp-ushort-type+)
    (defconstant +lisp-unsigned-char-type+ +lisp-uchar-type+)
    
    ;; this is a potentially hazardous guess.  The reasoning
    ;; is that a size_t will be the biggest integer type, which
    ;; will also be a pointer.
    (defconstant +lisp-size_t-type+ +lisp-pointer-type+)))
      

    
(eval-when (:load-toplevel :compile-toplevel :execute)
        ;; types to tell the user the Lisp equivalnt of various 
      ;; foreign ("machine") types.
      (deftype machine-size_t              ()  +lisp-pointer-type+) 
      (deftype machine-uint                ()     +lisp-uint-type+)
      (deftype machine-int                 ()      +lisp-int-type+)
      (deftype machine-pointer             ()  +lisp-pointer-type+)
      (deftype machine-char                ()     +lisp-char-type+)
      (deftype machine-uchar               ()    +lisp-uchar-type+)
      (deftype machine-unsigned-char       ()    +lisp-uchar-type+)
      (deftype machine-short               ()    +lisp-short-type+)
      (deftype machine-ushort              ()   +lisp-ushort-type+)
      (deftype machine-unsigned-short      ()   +lisp-ushort-type+)
      (deftype machine-long                ()     +lisp-long-type+)
      (deftype machine-ulong               ()    +lisp-ulong-type+)
      (deftype machine-ulong               ()    +lisp-ulong-type+)
      (deftype machine-llong               ()    +lisp-llong-type+)
      (deftype machine-ullong              ()   +lisp-ullong-type+)
      (deftype machine-unsigned-long-long  ()   +lisp-ullong-type+))
    
  
 

;; the type used in our loops for indexing arrays
(deftype indexnum ()
  `(integer 0  ,most-positive-fixnum))


 


(eval-when (:compile-toplevel :load-toplevel)
  ;;
  ;; given a FFI-TYPE, return a lisp type into which a lisp object
  ;; may be coerced to allow it to be inserted into a foreign memory
  ;; slot of that FFI-TYPE.  
  (defun lisp-coercion-type-for-ffi-type (ffi-type)
    (cond ((eq ffi-type :double) 'double-float) 
	  ((eq ffi-type :float)  'single-float)
	  ;;
	  ((eq ffi-type :int8)     '(signed-byte 8))
	  ((eq ffi-type :uint8)    '(unsigned-byte 8))
	  ((eq ffi-type :int16)    '(signed-byte 16))
	  ((eq ffi-type :uint16)   '(unsigned-byte 16))
	  ((eq ffi-type :int32)    '(signed-byte 32))
	  ((eq ffi-type :uint32)   '(unsigned-byte 32))
	  ((eq ffi-type :int64)    '(signed-byte 64))
	  ((eq ffi-type :uint64)   '(unsigned-byte 64))
	  ;;
	  ((eq ffi-type :pointer)         +lisp-pointer-type+) 
	  ((eq ffi-type :char)            +lisp-char-type+) 
	  ((eq ffi-type :unsigned-char)   +lisp-uchar-type+) 
	  ((eq ffi-type :uchar)           +lisp-uchar-type+) 
	  ((eq ffi-type :short)           +lisp-short-type+) 
	  ((eq ffi-type :unsigned-short)  +lisp-ushort-type+) 
	  ((eq ffi-type :ushort)          +lisp-ushort-type+) 
	  ((eq ffi-type :int)             +lisp-int-type+)
	  ((eq ffi-type :unsigned-int)    +lisp-uint-type+)
	  ((eq ffi-type :uint)            +lisp-uint-type+)
	  ((eq ffi-type :long)            +lisp-long-type+)
	  ((eq ffi-type :unsigned-long)   +lisp-ulong-type+)
	  ((eq ffi-type :ulong)           +lisp-ulong-type+)
	  ((eq ffi-type :long-long)       +lisp-llong-type+)
	  ((eq ffi-type :llong)           +lisp-llong-type+)
	  ((eq ffi-type :unsigned-long-long)  +lisp-ullong-type+)
	  ((eq ffi-type :ullong)            +lisp-ullong-type+)
	  ;;
	
	  (t
	   (error "Unable to figure out how to coerce lisp types into FFI-TYPE ~A" ffi-type)))))



 

(defmacro defun-make-foreign-memory-from-array 
    (fun-name lisp-array-type ffi-type &key complex)
  "Macro to make a function of one argument (an array) called FUN-NAME
that converts an array of LISP-ARRAY-TYPE to a foreign pointer of FFI-TYPE.

COMPLEX means that the values in the array are to be treated as complex numbers,
and split across two consecutive FFI numbers

Zero length arrays return a NULL pointer."
  (let* ((multiplier (if complex 2 1)) ;; need twice as much mem for complex
	 ;; what we use to coerce an array element to fit into the foreign memory
	 ;;   a good compiler (eg sbcl) will get rid of the coercion if it isn't needed
	 (coercion-type (lisp-coercion-type-for-ffi-type ffi-type))
	 ;; this lisp might not support all types, so we force it to
	 ;; tell us what the real type of the array is
	 (true-lisp-array-type 
	  (array-element-type (make-array 1 :element-type lisp-array-type)))
         ;; if all we have are general arrays, don't declare simple.
         ;; Hence type T will work for all arrays, including non-special
	 (array-decl-type (if (eq lisp-array-type t) 
			      '(array)
			      `(simple-array ,true-lisp-array-type)))
	 ;; be cautious about the safety setting in the inner loop in
	 ;; the case if generic arrays. It is of no benefit to compile
	 ;; generic arrays at safety 0, and there might be some
	 ;; coercions that fail, and we want them to be caught at runtime.
	 (array-is-generic  (or (eq true-lisp-array-type t)
				(eq lisp-array-type t)))
	 (inner-loop-safety-setting (if array-is-generic 
					(max 2 +safety-setting+)
					+safety-setting+)))
  
    `(progn
       ;;(declaim (inline ,fun-name)) ;; inline doesn't help much and is bulky
       (defun ,fun-name (array start end copy-to-foreign)
	 (declare (type ,array-decl-type array)
		  (type (or null indexnum) start end)
		  (optimize speed))
	 (let* ((narr (array-total-size array))
		(istart (or start 0))
		(iend (or end narr)) ;; 1 beyond final index accessed
		(nelem (progn
			 (when (or (> istart iend)
				   (> iend narr))
			   (error 
			    "Invalid START and END in array to FFI copy"))
			 (- iend istart)))
		(foreign-pointer 
		 (if (zerop nelem)
		     ;; if this is an empty array, return a NULL pointer
		     (cffi:null-pointer)
		     ;; otherwise allocate  memory
		   (cffi:foreign-alloc 
		    ,ffi-type 
		    :count (* ,multiplier nelem)))))
	   (declare (type indexnum nelem istart iend)
		    (optimize (safety ,inner-loop-safety-setting))
		    (optimize (speed ,+speed-setting+)))


	   ;; now do the array copy, if requested and if there is stuff to copy
	   (when (and copy-to-foreign (plusp nelem))
	     ;; this might be sped up further by incrementing the
	     ;; pointer rather than indexing it, but this would
	     ;; require consing a 2nd pointer because we can't restore
	     ;; the pointer's adddress if we increment it
	     ,(if (not complex)
		  `(loop 
		      for i of-type indexnum from istart below iend
		      for ii of-type indexnum from 0
		      for value of-type ,true-lisp-array-type 
			= (row-major-aref array i)
		      do 
			(setf (cffi:mem-aref foreign-pointer ,ffi-type ii) 
			      ,(if (not coercion-type) 
				   'value
				   `(coerce value (quote ,coercion-type)))))
		  `(loop
		      for i of-type indexnum from istart below iend
		      for ii of-type indexnum from 0 by 2
		      for value  of-type ,true-lisp-array-type 
			= (row-major-aref array i)
		      for real of-type ,coercion-type 
			= (coerce (realpart value) (quote ,coercion-type))
		      for imag of-type ,coercion-type 
			= (coerce (imagpart value) (quote ,coercion-type))
		      do 
			(setf (cffi:mem-aref foreign-pointer ,ffi-type ii) 
			      ,(if (not coercion-type) 
				   'real
				   `(coerce real (quote ,coercion-type))))
			(setf (cffi:mem-aref foreign-pointer ,ffi-type (1+ ii))
			      ,(if (not coercion-type) 
				   'imag
				   `(coerce imag (quote ,coercion-type)))))))
	   foreign-pointer)))))
     
  

(defmacro defun-copy-foreign-memory-to-array 
    (fun-name lisp-array-type ffi-type &key complex)
  "Macro to make a function 

   (FUN-NAME LISP-ARRAY FOREIGN-POINTER &OPTIONAL N-ELEMENTS)

that copies foreign elements of type FFI-TYPE to LISP-ARRAY of
LISP-ARRAY-TYPE, copying either the length of LISP-ARRAY or
N-ELEMENTS.

COMPLEX indicates whether adjacent elements of foreign memory are to
be treated as complex numbers, and packed into a single lisp element."
  (let* ((lisp-type-of-foreign (lisp-coercion-type-for-ffi-type ffi-type))
	 ;; this lisp might not support all types, so we force it to
	 ;; tell us what the real type of the array is
	 (true-lisp-array-type 
	  (array-element-type (make-array 1 :element-type lisp-array-type)))
	 (array-decl-type (if (eq true-lisp-array-type t) 
			      '(array)
  			      `(simple-array ,true-lisp-array-type)))
	 ;; be cautious about the safety setting in the inner loop in
	 ;; the case if generic arrays, it is of no benefit to
	 ;; compiling at safety 0, and there might be some coercions
	 ;; that fail, and we want it to be caught.
	 (array-is-generic  (or (eq true-lisp-array-type t)
				(eq lisp-array-type t)))
	 (inner-loop-safety-setting (if array-is-generic 
					(max 2 +safety-setting+)
					+safety-setting+)))
    ;;
    `(progn
       ;;(declaim (inline ,fun-name))
       (defun ,fun-name (array foreign-pointer start end)
	 (declare (type ,array-decl-type array)
		  (type (or null indexnum) start end)
		  (optimize speed))
	 (let* ((narr (array-total-size array))
		(istart (or start 0))
		(iend (or end narr)) ;; 1 beyond final element accessed
		(nelem (progn
			 (when (or (> istart iend)
				   (> iend narr))
			   (error 
			    "Invalid START and END in array to FFI copy"))
			 (- iend istart))))
	   (declare (type indexnum narr istart iend nelem)
		    (optimize (safety ,inner-loop-safety-setting))
		    (optimize (speed ,+speed-setting+)))
	   
	   
	   (when (and (plusp nelem) (plusp (cffi:pointer-address foreign-pointer)))
	     ;; this might be sped up further by incrementing the
	     ;; pointer rather than indexing it, but this would
	     ;; require consing a 2nd pointer because we can't restore
	     ;; the pointer's adddress if we increment it
	     ,(if (not complex)
		  `(loop 
		      for i of-type indexnum from istart below iend
		      for ii of-type indexnum from 0
		      for value of-type ,lisp-type-of-foreign 
			= (cffi:mem-aref foreign-pointer ,ffi-type ii) 
		      do 
			(setf (row-major-aref array i) 
			      ,(if (equalp true-lisp-array-type 
					   lisp-type-of-foreign)
		  		   'value
				   `(coerce value 
					    (quote ,true-lisp-array-type)))))
		  
		  `(loop
		      for i of-type indexnum from istart below iend
		      for ii of-type indexnum from 0 by 2
		      for value-r  of-type ,lisp-type-of-foreign = 
			(cffi:mem-aref foreign-pointer ,ffi-type ii) 
		      for value-i  of-type ,lisp-type-of-foreign = 
			(cffi:mem-aref foreign-pointer ,ffi-type (1+ ii))
		      for value = (coerce (complex value-r value-i) 
					  (list 'complex (quote ,true-lisp-array-type)))
		      do
			(setf (row-major-aref array i) value))))
	   array)))))
     
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel)
  ;; the set of covered transformations in the form of 
  ;; (LISP-TYPE FFI-TYPE COMPLEX-P)
  
  
  ;; hash where 
  ;;    KEY is   (LISP-TYPE FFI-TYPE COMPLEX)     
  ;;              eg  ((complex double-float) :float
  ;;    VAL is   (FUNC-TO-FFI  FUNC-FROM-FFI)   
  ;;              ie the funcs that copy to/from ffi space
  (defvar *lisp-to-ffi-function-pair-hash*
    (make-hash-table :test 'equal))


; helper func to turn a lisp-array-type and ffi-type into a basename for a 
;; transformation function


  (defun %make-function-prefix (lisp-array-type ffi-type complex)
    (let ((lisp-string (substitute
			#\- #\space
			(remove-if 
			 (lambda (c) (and (not (alphanumericp c))
					  (not (member c '(#\- #\space)))))
			 (format nil "~A~A" 
				 lisp-array-type
				 (if (and (eq lisp-array-type t) complex)
				     "C" ""))))))
      (format nil "~A<-->~A" lisp-string  ffi-type)))


;; generate the names for the transformation function, given the
;; types and whether it is a complex array
(defun %make-transform-func-names (lisp-array-type ffi-type complex)
 (let* ((func-prefix (%make-function-prefix 
		      lisp-array-type ffi-type complex))
	(l2f-func
	 (intern
	  (string-upcase 
	   (concatenate 
	    'string 
	    "%amap--" func-prefix "---lisp->ffi"))))
	(f2l-func
	 (intern 
	  (string-upcase 
	   (concatenate 
	    'string 
	    "%amap--" func-prefix "---ffi->lisp")))))
   ;; and put in hash
   (setf (gethash  (list lisp-array-type ffi-type complex)
		   *lisp-to-ffi-function-pair-hash*)
	 (list l2f-func f2l-func))
   (values l2f-func f2l-func)))
	


;; given a transformation set, DEFUN functions to do the forward
;; list->ffi and reverse ffi->lisp array copying
(defmacro add-lisp-to-ffi-function-pair (lisp-array-type ffi-type 
					 &optional complex)
  (multiple-value-bind (l2f-func f2l-func)
      (%make-transform-func-names lisp-array-type ffi-type complex)
    `(progn
       (defun-make-foreign-memory-from-array 
	   ,l2f-func
	 ,lisp-array-type ,ffi-type :complex ,complex)
       (defun-copy-foreign-memory-to-array  
	   ,f2l-func
	   ,lisp-array-type ,ffi-type :complex ,complex))))



) ;; end of eval-when


;; expand out into code to build all the functions.  
;;
;; Note that some of these mappings are duplicates, but we do that
;; rather than worrying about which CFFI types are synonyms.
;;
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *lisp-ffi-array-pairings*
    '((single-float           :float)
      (double-float           :double)
      (single-float           :double)
      (double-float           :float)
      ;;
      ;; We do not have :POINTER types because SBCL complains when
      ;; setting a :POINTER foreign slot to an int.  Instead, we
      ;; defined +CFFI-POINTER-TYPE+ to :UINT64 or :UINT32, and change
      ;; :POINTER to this at macroexpansion time.  This should work
      ;; though it is probably offically discouraged.

      ;;
      ((complex single-float) :float  t) ;; complex
      ((complex single-float) :double t) ;; complex
      ((complex double-float) :double t) ;; complex
      ((complex double-float) :float  t) ;; complex
	   
      ;;
      ((unsigned-byte 8)      :uint8)
      ((unsigned-byte 8)      :uchar)
      ((unsigned-byte 16)     :uint16)
      ((unsigned-byte 16)     :ushort)
	   
      ;;
      ((unsigned-byte 32)     :uint32)
      ((unsigned-byte 32)     :uint16)
      ((unsigned-byte 32)     :ushort)
      ((unsigned-byte 32)     :uint64)
      ((unsigned-byte 32)     :ulong)
      ;;
      ((unsigned-byte 64)     :uint64)
      ((unsigned-byte 64)     :uint32)
      ((unsigned-byte 64)     :ulong) 
      ((unsigned-byte 64)     :ullong)
      ;;
      ;;
      ((signed-byte 8)      :int8)
      ((signed-byte 8)      :char)
      ((signed-byte 16)     :int16)
      ((signed-byte 16)     :short)
      ;;
      ((signed-byte 32)     :int32)
      ((signed-byte 32)     :int16)
      ((signed-byte 32)     :short)
      ((signed-byte 32)     :int64)
      ((signed-byte 32)     :long)
      ;;
      ((signed-byte 64)     :int64)
      ((signed-byte 64)     :int32)
      ((signed-byte 64)     :long)
      ((signed-byte 64)     :llong)
      ;;
      ;; map general arrays to foreign types
      (t                      :short)
      (t                      :ushort)
      (t                      :char)
      (t                      :uchar)
      (t                      :long)
      (t                      :ulong)
      (t                      :llong)
      (t                      :ullong)
      (t                      :float)
      (t                      :double)
      (t                      :float  t) ;; complex
      (t                      :double t) ;; complex
      (t                      :uint8)
      (t                      :int8)
      (t                      :uint16)
      (t                      :int16)
      (t                      :uint32)
      (t                      :int32)
      (t                      :uint64)
      (t                      :int64))))


;; this macro creates a DEFUN for each pair of specialized array
;; copying functions, with pairings in *lisp-ffi-array-pairings*
(defmacro %build-all-array-copying-functions ()
    `(progn
       ,@(loop 
	    for func-desc in *lisp-ffi-array-pairings*
	    collect `(add-lisp-to-ffi-function-pair ,(first func-desc) 
						    ,(second func-desc)
						    ,(third func-desc)))))



;; expand macro to build all the specialized array copying routines
(eval-when (:compile-toplevel :load-toplevel)
  (%build-all-array-copying-functions))



;; this will put the functions into the hash at load time
(defun %initialize-hash-table ()
  (clrhash  *lisp-to-ffi-function-pair-hash*)
  (loop 
     for func-desc in *lisp-ffi-array-pairings*
     do (%make-transform-func-names ;; this also puts func pair into hash
	 (first func-desc)     ;; lisp type
	 (second func-desc)    ;; foreign type
	 (third func-desc))))  ;; complex?

;; build hash table containing the array copying functions
(eval-when (:load-toplevel)
  (%initialize-hash-table))



;; double check that all functions in the hash table are bound at load
;; time.  It should not happen, but there is a lot of fanciness in
;; building the function hash, so let's be safe.
(eval-when (:load-toplevel)
  (when (zerop (hash-table-count *lisp-to-ffi-function-pair-hash*))
    (error "There are ZERO items in *lisp-to-ffi-function-pair-hash*.  This should be full of array copying functions."))
  (loop 
     for pair being the hash-value of *lisp-to-ffi-function-pair-hash*
     for func1 = (first pair) and func2 = (second pair)
     do
       (when (not (fboundp func1))
		(error "Lisp -> FOREIGN array copying function ~A is not bound." func1))
       (when (not (fboundp func2))
		(error "FOREIGN -> Lisp copying function ~A is not bound." func2))))
       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; return a transformation pair good for strings - these are probably
;; not as fast as the specialized arrays, but it probably does not
;; matter because string ffi use is probably relatively lightweight.
;; But we need to do it this way to close on encoding.

(defun %amap--string<-->string---lisp->ffi (string start end encoding)
  (declare (type (or null indexnum) start end))
  (if end ;; cffi does not like NIL for END
      (cffi:foreign-string-alloc string :encoding encoding 
				 :start (or start 0) :end end)
      (cffi:foreign-string-alloc string :encoding encoding 
				 :start (or start 0))))
      

;; the CFFI string routines do null termination by default, so foreign strings
;; are null terminated.   This can never (?) hurt.
(defun %amap--string<-->string---ffi->lisp (string pointer start end encoding)
  (declare (type (or null indexnum) start end))
  (let* ((narr (length string))
	 (istart (or start 0))
	 (iend   (or end narr)) 
	 (nelem (progn
		  (when (or (> istart iend)
			    (> iend  narr))
		    (error 
		     "Invalid START=~A and END=~A in string to foreign string copy"
		     start end))
		  (- iend istart)))
	 (outstring (cffi:foreign-string-to-lisp 
		     pointer  
		     :offset 0 
		     :count  (1+ nelem) ;; allow one extra char for null termination
		     :encoding encoding)))
    (declare (type indexnum narr istart iend nelem))
    (loop 
       for i of-type indexnum  from istart below iend
       for ii of-type indexnum from 0
       do (setf (aref string i) (aref outstring ii)))))




  


(defun %get-transformation-function-pair-for-strings (lisp-array-type)
  (when (not (member lisp-array-type '(character char base-char t)))
    (error "WITH-ARRAYS-AS-FOREIGN-PONTERS helper function - you cannot have lisp-type=~A with a :STRING FFI TYPE.  The Lisp type must be char or T." 
	   lisp-array-type))
  (list
   '%amap--string<-->string---lisp->ffi
   '%amap--string<-->string---ffi->lisp))
       
       
	 					   
      
      
  
;; for certain clearly synonymous CFFI types we have only one function
;; pair, and translate the CFFI type into its synonym.
(defparameter *cffi-type-synonyms* 
  `((:unsigned-char  .  :uchar)
    (:unsigned-short .  :ushort)
    (:unsigned-int  .   :uint)
    (:long-long  .  :llong)
    (:unsigned-long-long  . :ullong)
    (:pointer . ,+cffi-pointer-type+))) ;; represent :POINTER as an integer type


;; so that we can call the macros with our special lisp types in addition
;; to standard lisp typea, we use a translation table.
(defparameter *lisp-type-synonyms*
  `((machine-pointer            . ,+lisp-pointer-type+)
    (machine-size_t             . ,+lisp-size_t-type+) 
    (machine-int                . ,+lisp-int-type+)
    (machine-uint               . ,+lisp-uint-type+)
    (machine-uint               . ,+lisp-unsigned-int-type+)
    (machine-char               . ,+lisp-char-type+)
    (machine-uchar              . ,+lisp-uchar-type+)
    (machine-unsigned-long      . ,+lisp-unsigned-char-type+)
    (machine-short              . ,+lisp-short-type+)
    (machine-ushort             . ,+lisp-ushort-type+)
    (machine-unsigned-short     . ,+lisp-unsigned-short-type+)
    (machine-long               . ,+lisp-long-type+)
    (machine-ulong              . ,+lisp-ulong-type+)
    (machine-unsigned-long      . ,+lisp-unsigned-long-type+)
    (machine-llong              . ,+lisp-llong-type+)
    (machine-ullong             . ,+lisp-ullong-type+)
    (machine-unsigned-long-long . ,+lisp-unsigned-long-long-type+)))


    

(defun %replace-longname-synonym (type)
  (or (cdr (assoc type *cffi-type-synonyms*))
      type))

(defun %replace-lisptype-synonym (type)
  (or (cdr (assoc type *lisp-type-synonyms*))
      type))

(defun %get-transformation-function-pair 
    (lisp-array-type ffi-type complex)
  (cond 
    ;; strings are special
    ((eq ffi-type :string)
     (%get-transformation-function-pair-for-strings  lisp-array-type))
    ;;
    (t
     (gethash (list (%replace-lisptype-synonym lisp-array-type)
		    (%replace-longname-synonym ffi-type)
		    complex) 
	      *lisp-to-ffi-function-pair-hash*))))





	      


(defmacro with-arrays-as-foreign-pointers (list-of-stuff-for-one-array
					   &body body)
  "Macro that evaluates code after copying lisp arrays into blocks of
freshly allocted foreign memory, the deallocates the memory inside an
unwind-protect.

This is a parallel version of WITH-ARRAY-AS-FOREIGN-POINTER:

  (WITH-ARRAYS-AS-FOREIGN-POINTERS 
     ((ARRAY-1 PTR-1 ...)
      (ARRAY-2 PTR-2 ...))
    (BODY))

is like

   (WITH-ARRAY-AS-FOREIGN-POINTER (ARRAY-1 PTR-1 ...)
        (WITH-ARRAY-AS-FOREIGN-POINTER (ARRAY-2 PTR-2 ...)
           (BODY)))

In practice, WITH-ARRAY-AS-FOREIGN-POINTER expands into
WITH-ARRAYS-AS-FOREIGN-POINTERS.

See WITH-ARRAY-AS-FOREIGN-POINTER  for documentation of arguments."
  (loop 
     with setq-list = nil
     with let-list = nil
     with free-list = nil 
     with copy-list = nil
     for thing in list-of-stuff-for-one-array
     for arrayname = (gensym "lisp-array-")
     for start-var = (gensym "start-")
     for end-var = (gensym "end-")
     for copy-to-foreign-var = (gensym "copy-to")
     for copy-from-foreign-var = (gensym "copy-from")
     do
     (destructuring-bind (array pointer-var ffi-type 
				&key 
				(copy-to-foreign t) ;; copy FROM Lisp TO FFI memory
				(copy-from-foreign t) ;; copy FROM FFI back TO Lisp
				;; bounds of array to copy [start,end)
				(start nil) (end nil) 
				(complex nil) 
				;; by default, the slow catch-all lisp
				;; array type
				(lisp-type t) 
				(encoding :ASCII)) ;; for strings only
	 thing
       ;;
       (when (not (symbolp pointer-var))
	 (error "WITH-ARRAYS-AS-FOREIGN-POINTERS: expected ~S to be a symbol 
to represent a foreign pointer in expression ~S, but it is of type ~A"
		pointer-var thing (type-of pointer-var)))
       ;;
       (when (not (member complex '(T NIL)))
	 (error "WITH-ARRAYS-AS-FOREIGN-POINTERS: COMPLEX=~S must be boolean T or NIL at macroexpansion time.  I are not permitted to be variable at runtime." complex))
		       
       ;;
       ;;
       ;; build the list of operations to bind filled foreign memory to
       ;; the user-specified variables
       (let* ((func-pair (or (%get-transformation-function-pair 
			      lisp-type ffi-type complex)
			     (error "Macro with-arrays-as-foreign-pointers: Cannot find a transformation function pair for LISP-TYPE=~S and FFI-TYPE=~S and COMPLEX=~A" 
				    lisp-type ffi-type complex)))
	      (l2f-func (first func-pair))
	      (f2l-func (second func-pair)))

	 ;;
	 ;; build the list of operations to LET all the internal variables
	 (setf let-list
	       (append let-list 
		       `((,arrayname ,array)
			 (,start-var ,start)
			 ,@(when (not (eq ffi-type :string))
				 `((,copy-to-foreign-var ,copy-to-foreign)))
			 (,copy-from-foreign-var ,copy-from-foreign)
			 (,end-var ,end)
			 (,pointer-var nil))))
	   
	 (setf setq-list
	       (append 
		setq-list
		(if (eq ffi-type :string)
		    ;; strings are special, using CFFI's built-ins - note that we can't disable
		    ;;   COPY-TO-FOREIGN for string type
		    `((setf ,pointer-var (,l2f-func ,arrayname ,start-var ,end-var ,encoding)))
		    `((setf ,pointer-var (,l2f-func ,arrayname ,start-var ,end-var 
						    ,copy-to-foreign-var))))))
	 ;; build the list of operations to free the allocated foreign pointers
	 (setf free-list
	       (if (eq ffi-type :string) ;; string freed specially
		   (append 
		    free-list
		    `((when ,pointer-var (cffi:foreign-string-free
					  ,pointer-var))))
		   (append 
		    free-list
		    `((when (and ,pointer-var
				 (not (zerop (cffi:pointer-address ,pointer-var))))
			(cffi:foreign-free ,pointer-var))))))
	 ;;
	 (when copy-from-foreign ;; only macroexpand copy-from-foreign if not NIL
	   (setf copy-list
		 (append 
		  copy-list
		  (list
		   `(when ,copy-from-foreign-var ;; do runtime check for copy-from-foreign
		      ,(if (eq ffi-type :string) ;; strings are special, using CFFI's built-ins
			   `(,f2l-func ,arrayname ,pointer-var ,start ,end ,encoding)
			   `(,f2l-func ,arrayname ,pointer-var ,start ,end)))))))))

     finally
     (return 
       `(let (,@let-list) 
	  (unwind-protect 
	       (progn ;; this is returned
		 ,@setq-list
		 (multiple-value-prog1 ;; return all values returned by body
		     (progn ,@body) 
		   ,@copy-list)        
		 )                     
	    (progn                     
	      ,@free-list))))))

			 

(defmacro with-array-as-foreign-pointer 
    ((array pointer-var ffi-type
	    &key (copy-to-foreign t) (copy-from-foreign t) (complex nil) (lisp-type t) 
	    (start nil) (end nil)
	    (encoding :ASCII))
     &body body)
"Macro that evaluates BODY code after copying one lisp array into a block
of foreign memory
   
   (WITH-ARRAY-AS-FOREIGN-POINTER
       (ARRAY POINTER-VAR CFFI-FOREIGN-TYPE  
        :COPY-TO-FOREIGN   T|NIL
        :COPY-FROM-FOREIGN T|NIL 
        :START NIL|index   
        :END   NIL|index
        :LISP-TYPE ELEMENT-TYPE-OF-LISP-ARRAY
        :COMPLEX   T|NIL
        :ENCODING  STRING-ENCODING)

    ;; body that uses block of foreign memory bound to pointer-var
    )



 ARRAY     is any Lisp array whose elements are to be passed to a foreign
           function in a block of foreign memory
 POINTER-VAR  is the name of a variable to which a pointer is to be bound,
           containing the contents of ARRAY
 CFFI-FOREIGN-TYPE is the type of the foreign memory, like :DOUBLE
 COPY-TO-FOREIGN   specifies if the Lisp array is to be copied to foreign memory
           before body is run.  It is T by default
 COPY-FROM-FOREIGN specifies if the foreign memory is copied back into the lisp array
           after body is run.  It is T by default.
 START and END is the range of elements to be copied into the foreign array,
           which will be of size (- END START); END is 1 beyond the last index 
           copied, like for the standard SUBSEQ function
 LISP-TYPE is  the element type of the Lisp array.  Examples of LISP-TYPE
           are FLOAT, DOUBLE, (UNSIGNED-BYTE 32), and T (the default). 
           If LISP-TYPE is not T, the array is  assumed SIMPLE.  More restrictive 
           types  (eg FLOAT and not T) are faster and cons less or not at all.
 COMPLEX   specifies whether the elements of the array are to be treated as 
           complex numbers, so that each one is unpacked into two foreign
           array slots, and the two slots are then combined when the
           COPY-FROM-FOREIGN operation copies from foreign space to the lisp array.
 ENCODING  used only when CFFI-FOREIGN-TYPE is :STRING, and we punt to
           CFFI's foreign string routines"

  `(with-arrays-as-foreign-pointers ((,array ,pointer-var ,ffi-type 
					     :copy-to-foreign ,copy-to-foreign
					     :copy-from-foreign ,copy-from-foreign
					     :start ,start :end ,end
					     :complex ,complex
					     :lisp-type ,lisp-type
					     :encoding ,encoding
					    ))
     ,@body))

					 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *null-pointer* (cffi:null-pointer))



;; types that are allowed for SBDEFINE-ALIEN-ROUTINE
(defparameter *sb-allowed-types*
  '(:pointer :string :void
    ;;
    :double :float :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64
    :char :unsigned-char :uchar :short :unsigned-short :ushort :int
    :unsigned-int :uint :long :unsigned-long :ulong :long-long :llong
    :unsigned-long-long :ullong))




(defun %sb-validate-type (type)
  (cond ((member type *sb-allowed-types*)
	 type)
	;; anything like (* foo) is accepted and converted to :POINTER
	((and (listp type) (eq (first type) '*))
	 :pointer)
	(t
	 (error "TYPE ~A is not one of the allowed types to use with SBDEFINE-ALIEN-ROUTINE" type))))
  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SBDEFINE-ALIEN-ROUTINE - a macro to allow sbcl style
;;      function definitions with :IN,:COPY, and :IN-OUT 
;;      parameters, allowing easy use of functions that take
;;      pointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro sbdefine-alien-routine    (name result-type &rest arguments)
"Macro to permit sbcl/cmucl style foreign function definition,
permitting the use of :COPY :IN :OUT and :IN-OUT arguments
that allow Lisp values to be turned into addresses passed as pointers.



Usage

    (SBDEFINE-ALIEN-ROUTINE 
       (FOREIGN-FUNCTION-NAME-STRING    
        LISP-FUNCTION-NAME-SYMBOL)        
	;;
       RETURN-TYPE-OF-FOREIGN-FUNCTION
       ;; 
       (ARGUMENT-NAME  ARGUMENT-TYPE  [:IN|:OUT|:COPY|:IN-OUT])
       ...)

RETURN-TYPE-OF-FOREIGN-FUNCTION and ARGUMENT-TYPE are CFFI types,
including :STRING and :POINTER.  Also, types of the form (* SOMETHING)
are recognized as :POINTER.

Each argument to the foreign function is one of

  :IN     (the default) - the argument is just passed to the foreign function
  :COPY   the argument is copied to foreign memory, and a POIINTER to the
          supplied value is passed to the foreign function
  :OUT    nothing is passed by the user, but a POINTER is allocated
          and the foreign function fills it, and the value is returned
  :IN-OUT a combination of :COPY and :OUT.  A pointer is allocated,
          the supplied user value is placed in foreign memory, and 
          it's address is passed to the foreign function. Then
          the value that the foreign function place in this same address
          is returned.

Arguments: the created function takes one argument for each :IN, :COPY,
           and :IN-OUT term, but not for :OUT terms.

Return values: the function returns its return value, plus one
               value for each :OUT and :IN-OUT.


Example:

    (SBDEFINE-ALIEN-ROUTINE 
       (\"ForeignFunc\"    ;; the foreign name
        lisp-func)         ;; the desired lisp name
       :INT ;; the return type for the function itself
       ;; 
       (N :INT) ;; this is :IN by default
       (X :FLOAT :COPY)   ;; a POINTER to X is passed to ForeignFunc
       (Y :DOUBLE :OUT)   ;; Y is not supplied, but a pointer 
                          ;;   is given to ForeignFunc for it to fill,
                          ;;   and this new value is returned.
       (Z :FLOAT :IN-OUT) ;; Z is supplied, given to ForeignFunc as a pointer,
                          ;; and is returned
       (S :STRING)       ;; S is passed as a string in usual CFFI manner
       (Q (* ANYTHING-GOES)) ;; Q is just a pointer
       (R :POINTER))         ;; R is a pointer too

 Now LISP-FUNC is called as

     (LISP-FUNC N X Z S Q R)

 and it returns

    (VALUES  RETURNED-INT   Y-FROM-FOREIGN  Z-FROM-FOREIGN)
"
  (let* ((result-type (%sb-validate-type result-type))
	 (lisp-args (loop for arg in arguments
		       when (or (= (length arg) 2)
				(member (third arg) '(:copy :in :in-out)))
		       collect arg))
	 (lisp-vars (mapcar 'first lisp-args))
	 (cffi-args (loop 
		       for arg in arguments
		       ;; the type is :POINTER if arg is of the form
		       ;;  (var type :in-out) or (var (* type))
		       for type = 
			 (%sb-validate-type
			  (if (or (member (third arg)
					  '(:copy :out :in-out))
				  ;; eg type is (* int)
				  (and (consp (second arg))
				       (eq (first (second arg)) '*)))
			      :pointer 
			      (second arg)))
		       collect (list (first arg) type)))
	 
	 ;; c argument names
	 (c-args (loop for arg in arguments
		    collect (gensym (string (first arg)))))
	 ;; 
	 ;; de-refs for pointers after value was modified by foreign code
	 (c-pointer-derefs
	  (loop for arg in arguments
	     for c-arg in c-args
	     for type = (%sb-validate-type (second arg))
	     when (member (third arg) '(:out :in-out)) ;; NOT :copy
	     collect `(cffi:mem-ref ,c-arg (quote ,type) 0)))
	 ;;
	 ;; name of foreign function
	 (c-name (if (consp name) (first name) name)) 
	 ;;
	 (lisp-name (if (listp name) ;; name of the function we build
			(second name)  ;; must be a symbol
			(intern  (string-upcase name))))
	 ;; 
	 (cffi-defcnum-name (intern
			     (string-upcase
			      (concatenate 'string "%defcfun-"
					   (string c-name))))))
    `(progn 
       ;;
       (declaim (inline  ,cffi-defcnum-name))
       ;;
       (cffi:defcfun (,c-name ,cffi-defcnum-name) 
	   ,result-type
	 ,@cffi-args)
       ;;
       (defun ,lisp-name (,@lisp-vars)
	 ,(loop
	     with result-sym = (gensym "result-")
	     with accum = `(let ((,result-sym (,cffi-defcnum-name ,@c-args)))
			     (values
			      ,result-sym
			      ,@c-pointer-derefs))
	     for arg in arguments
	     for argname = (first arg)
	     for type = (%sb-validate-type (second arg))
	     for is-plain-arg = (member (third arg) '(:in nil))
	     for is-pointer =  (member (third arg) '(:out :in-out :copy))
	     for coercion-type = (when is-pointer ;; only coerce for pointers
				   (lisp-coercion-type-for-ffi-type type))
	     for is-in-out-pointer =  (eq (third arg) :in-out)
	     for is-copy-pointer = (eq (third arg) ':copy)
	     for is-string = (eq type :string)
	     for c-arg in c-args
	     do
	       (cond 
		 ;; strings mimic SBCL behavior that NIL  is C NULL pointer
		 (is-string
		  (setf
		   accum
		   `(let ((,c-arg (or ,argname *null-pointer*)))
		      ,accum)))
		 ;; not a pointer, so just wrap in LET
		 (is-plain-arg
		  (setf
		   accum
		   `(let ((,c-arg ,argname))
		      ,accum)))
		 ;;
		 ;; 
		 ;; is a pointer, so do 'with-foreign-object' wrapping
		 (is-pointer
		  (setf 
		   accum
		   `(cffi:with-foreign-object (,c-arg (quote ,type))
		      ;; when :COPY or :IN-OUT set the array element of the temp
		      ;; foreign array to the value given
		      ,(when (or is-in-out-pointer is-copy-pointer)
			     `(setf (cffi:mem-ref ,c-arg (quote ,type) 0)
				    (coerce ,argname ',coercion-type)))
		      ,accum))))
	     finally (return accum))))))





 
(defmacro with-lisp-values-as-foreign-pointers (arguments &body body)
  "Macro that allocates individual units of foreign memory, places Lisp
values them, binds the pointer to a user supplied variable, executes
BODY, releases the memory, and returns (VALUES ....) for the new
values in the foreign pointers, which may have been modified by
foreign code in BODY.

Usage:
  
    (WITH-LISP-VALUES-AS-FOREIGN-POINTERS
       ((LISP-VALUE-1 POINTER-VAR-1 CFFI-TYPE-1
           :COPY-TO-FOREIGN   T|NIL 
	   :COPY-FROM-FOREIGN T|NIL)
        (LISP-VALUE-2 POINTER-VAR-2 CFFI-TYPE-2 ..)
        ...)
	   
       ;; body that uses pointers POINTER-VAR-1 POINTER-VAR-2
       )
        
    

Each variable to be bound is represented by

    (LISP-VALUE POINTER-VAR CFFI-TYPE  :COPY-TO-FOREIGN T :COPY-FROM-FOREIGN T)

where COPY-TO-FOREIGN and COPY-FROM keywords are optional and true by default.
COPY-TO-FOREIGN determines whether the Lisp value is placed in the allocated
foreign memory before invoking BODY, and COPY-FROM determines if the
value of the foreign memory slot after BODY runs is returned in the
macroexpanded form's VALUES."


  (loop 
     with setf-list = nil
     with let-list = nil
     with let-list-out = nil
     with free-list = nil 
     with values-list = nil  ;; what is returned
     for thing in arguments
     for valname = (gensym "lisp-val-")
     for valname-out = nil
     for coercion-type = nil
     do
       (destructuring-bind (val pointer-var ffi-type 
				&key 
				(copy-to-foreign t)    ;; copy FROM Lisp TO FFI memory
				(copy-from-foreign t)) ;; copy FROM FFI back TO Lisp
	   thing

	 ;;
	 (setf coercion-type
	       (lisp-coercion-type-for-ffi-type ffi-type)) ;; this throws error if it fails
	 ;;
	 (when (not (symbolp pointer-var))
	   (error "WITH-LISP-VALUES-AS-FOREIGN-POINTERS: expected ~S to be a symbol 
to represent a foreign pointer in expression ~S, but it is of type ~A"
		  pointer-var thing (type-of pointer-var)))
	 
	 	 ;;
	 ;; build the list of operations to LET all the internal variables

 	 (setf let-list
 	       (append let-list 
 		       `((,valname ,val)
 			 (,pointer-var nil))))


 	 ;; make the pointer
 	 (setf setf-list
 	       (append 
 		setf-list
 		(list
 		 `(setf ,pointer-var (cffi:foreign-alloc ,ffi-type  :count 1)))))

 	 (if copy-to-foreign
 	     (setf setf-list
 		   (append 
 		    setf-list
 		    (list
 		     `(setf (cffi:mem-ref ,pointer-var ,ffi-type)
 			    (coerce ,valname ',coercion-type))))))
	 
	 ;; build the list of operations to free the allocated foreign pointers
	 (setf free-list
	       (append 
		free-list
		`((when (and ,pointer-var
			     (not (zerop (cffi:pointer-address ,pointer-var))))
		    (cffi:foreign-free ,pointer-var)))))
	 ;; :COPY-FROM-FOREIGN is T by default
	 (when copy-from-foreign
	   (setf valname-out (gensym "lisp-val-out-"))
	   (setf let-list-out
		 (append let-list-out
			 `((,valname-out  (cffi:mem-ref ,pointer-var ,ffi-type)))))
	   (setf values-list (append values-list (list valname-out)))))
       
     finally
       (return 
	 `(let (,@let-list) 
	    (unwind-protect 
		 (progn 
		   ,@setf-list
		   ,@body ;; what BODY returns is ignored, because new values are returned
		   (let (,@let-list-out)
		     (values ,@values-list)) ;; values-list is the returned values
		   )
	      (progn ;; free inside UNWIND-PROTECT
		,@free-list))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; he who waafs wast, waafs best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
