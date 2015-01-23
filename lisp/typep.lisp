(defun %%typep (object type &optional (strict t))
  (declare (type ctype type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((instance) (%instancep object))
       ((funcallable-instance) (funcallable-instance-p object))
       ((extended-sequence) (extended-sequence-p object))
       ((nil) nil)))
    (numeric-type
     (and (numberp object)
          (let (;; I think this works because of an invariant of the
                ;; two components of a COMPLEX are always coerced to
                ;; be the same, e.g. (COMPLEX 1.0 3/2) => #C(1.0 1.5).
                ;; Dunno why that holds, though -- ANSI? Python
                ;; tradition? marsh faerie spirits? -- WHN 2001-10-27
                (num (if (complexp object)
                         (realpart object)
                         object)))
            (ecase (numeric-type-class type)
              (integer (integerp num))
              (rational (rationalp num))
              (float
               (ecase (numeric-type-format type)
                 (short-float (typep num 'short-float))
                 (single-float (typep num 'single-float))
                 (double-float (typep num 'double-float))
                 (long-float (typep num 'long-float))
                 ((nil) (floatp num))))
              ((nil) t)))
          (flet ((bound-test (val)
                   (let ((low (numeric-type-low type))
                         (high (numeric-type-high type)))
                     (and (cond ((null low) t)
                                ((listp low) (> val (car low)))
                                (t (>= val low)))
                          (cond ((null high) t)
                                ((listp high) (< val (car high)))
                                (t (<= val high)))))))
            (ecase (numeric-type-complexp type)
              ((nil) t)
              (:complex
               (and (complexp object)
                    (bound-test (realpart object))
                    (bound-test (imagpart object))))
              (:real
               (and (not (complexp object))
                    (bound-test object)))))))
    (array-type
     (and (arrayp object)
          (ecase (array-type-complexp type)
            ((t) (not (typep object 'simple-array)))
            ((nil) (typep object 'simple-array))
            ((:maybe) t))
          (or (eq (array-type-dimensions type) '*)
              (do ((want (array-type-dimensions type) (cdr want))
                   (got (array-dimensions object) (cdr got)))
                  ((and (null want) (null got)) t)
                (unless (and want got
                             (or (eq (car want) '*)
                                 (= (car want) (car got))))
                  (return nil))))
          (if (unknown-type-p (array-type-element-type type))
              ;; better to fail this way than to get bogosities like
              ;;   (TYPEP (MAKE-ARRAY 11) '(ARRAY SOME-UNDEFINED-TYPE)) => T
              (error "~@<unknown element type in array type: ~2I~_~S~:>"
                     (type-specifier type))
              t)
          (or (eq (array-type-specialized-element-type type) *wild-type*)
              (values (type= (array-type-specialized-element-type type)
                             (specifier-type (array-element-type
                                              object)))))))
    (member-type
     (when (member-type-member-p object type)
       t))
    (classoid
     #+sb-xc-host (ctypep object type)
     ;; It might be more efficient to check that OBJECT is either INSTANCEP
     ;; or FUNCALLABLE-INSTANCE-P before making this call.
     ;; But doing that would change the behavior if %%TYPEP were ever called
     ;; with a built-in classoid whose members are not instances.
     ;; e.g. (%%typep (find-fdefn 'car) (specifier-type 'fdefn))
     ;; I'm not sure if that can happen.
     #-sb-xc-host (classoid-typep (layout-of object) type object))
    (union-type
     (some (lambda (union-type-type) (%%typep object union-type-type strict))
           (union-type-types type)))
    (intersection-type
     (every (lambda (intersection-type-type)
              (%%typep object intersection-type-type strict))
            (intersection-type-types type)))
    (cons-type
     (and (consp object)
          (%%typep (car object) (cons-type-car-type type) strict)
          (%%typep (cdr object) (cons-type-cdr-type type) strict)))
    #!+sb-simd-pack
    (simd-pack-type
     (and (simd-pack-p object)
          (let* ((tag (%simd-pack-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-type-element-type type)))))))
    (character-set-type
     (and (characterp object)
         (let ((code (char-code object))
               (pairs (character-set-type-pairs type)))
           (dolist (pair pairs nil)
             (destructuring-bind (low . high) pair
               (when (<= low code high)
                 (return t)))))))
    (unknown-type
     ;; dunno how to do this ANSIly -- WHN 19990413
     #+sb-xc-host (error "stub: %%TYPEP UNKNOWN-TYPE in xcompilation host")
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
           (error "unknown type specifier: ~S"
                  (unknown-type-specifier reparse))
           (%%typep object reparse strict))))
    (negation-type
     (not (%%typep object (negation-type-type type) strict)))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
            (symbol (car hairy-spec)))
       (ecase symbol
         (and
          (every (lambda (spec) (%%typep object (specifier-type spec) strict))
                 (rest hairy-spec)))
         ;; Note: it should be safe to skip OR here, because union
         ;; types can always be represented as UNION-TYPE in general
         ;; or other CTYPEs in special cases; we never need to use
         ;; HAIRY-TYPE for them.
         (not
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (not (%%typep object (specifier-type (cadr hairy-spec)) strict)))
         (satisfies
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (values (funcall (symbol-function (cadr hairy-spec)) object))))))
    (alien-type-type
     (sb!alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (fun-type
     (if strict
         (error "Function types are not a legal argument to TYPEP:~%  ~S"
                (type-specifier type))
         (and (functionp object)
              (csubtypep (specifier-type (sb!impl::%fun-type object)) type))))))
