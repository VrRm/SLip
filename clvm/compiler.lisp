(defclass input-stream ()
  ((pos :accessor pos :initform 0)
   (line :accessor line :initform 0)
   (col :accessor col :initform 0)
   (text :accessor text :initarg :text))
  (:default-initargs text ""))

(defmethod len ((s input-stream))
  (length (text s)))

(defmethod peek ((s input-stream))
  (if (< (pos s) (len s))
      (aref (text s) (pos s))
      nil))

;; (setf s (make-instance
;; 	 'input-stream :text
;; 	 (let ((nl (string #\Newline)))
;; 	   (concatenate 'string "this" nl "is" nl  "an" nl "input" nl "stream"))))

(defmethod next ((s input-stream))
  (with-slots (pos line col text) s
    (if (< pos (len s))
	(let* ((ch (aref (text s) (pos s))))
	  (setf pos (1+ pos))
	  (if (eq ch #\Newline)
	      (progn
		(setf line (1+ line))
		(setf col 0))
	      (setf col (1+ col)))
	  ch)
	nil)))

(defclass lisp-reader ()
  ((stream :accessor stream)))

(defmethod read-while ((reader lisp-reader) reader-func)
  (with-slots (stream)
      (loop
	 while (let ((ch (peek stream)))
		     (and ch (funcall reader-func ch)))
	   (collect (next stream)))))


(defmethod skip-whitespace ((reader lisp-reader))
  (with-slots (stream)
      (read-while reader (lambda (ch)
			   (member ch '(#Tab #\Space #\Page #\Return
					#\Newline #\Linefeed))))))

(defmethod initialize-instance :after ((reader lisp-reader) &key code)
      (let ((stream (make-instance 'input-stream :text code)))

(defmethod skip ((reader lisp-reader) expected)
  (with-slots (stream)
      (unless (eq (next stream) expecting)
	(error "Expecting: ~S" (string expecting)))))

(defmacro str (&rest items)
  `(concatenate 'string ,@items))

(defmethod read-escaped ((reader lisp-reader) start end inces)
  (with-slots (stream) reader
    (skip reader start)
    (let ((escaped nil)
	  (new-string "")
	  (ch nil))
      (loop while (peek stream)
	   (setf ch (next stream))
	   (cond (escaped (setf new-string (str new-string (string ch))))
		 ((eq ch #\\)
		  (when inces (str new-string (string ch)))
		  (setf escaped t))
		 ((eq ch end) (loop-finish))
		 (t (setf new-string (str new-string (string ch))))))
      new-string)))

<<<<<<< HEAD
(defun read-string ()
  (read-escaped "\"" "\""))


(defun read-regexp ()
  (let ((str  (read-escaped "/" "/" true))
	(mods (string-downcase (read-while
				(lambda (ch)
				  (member (char-downcas ch)
					  '(#\y #\m #\g #\i)))))))
    (make-reg-exp str mods)))

(defun skip-comment ()
  (read-while (lambda (ch) (and ch (/= ch #\Newline)))))

(defun read-symbol ()
  (let ((str (read-while
	      (lambda (ch)
		(or (alphanumericp ch)
		    (member '(#\% #\$ #\| #\- #\: #\. #\+ #\* #\@ #\! #\? #\&
			      #\= #\< #\> #\[ #\] #\{ #\} #\/)))))))
    (if (and (> (length str) 0)
	     (test (reg-exp "/^-?[0-9]*\.?[0-9]*$/" str)
		   (let ((ret (read-from-string str)))
		     (if (numberp ret)
			 ret
			 (let ((str (string-upcase str))
			       (m (reg-exp-exec "/^(.*?)::?(.*)$/" str)))
			   (if m
			       (package-find-or-intern
				(or (lisp-package-get (aref m 1))
				    "KEYWORD")
				(aref m 2))
			       (if (lisp-package-get (lisp-package-intern (lisp-package-get "%")
									  "*PACKAGE*")
						     pak)
				   (lisp-package-find-or-intern pak str)
				   (lisp-symbol-get str)))))))))))

(defun read-char ()
  (let ((ch (concatenate 'string (next)
			 (read-while (lambda (ch)
				       (or (alphanumericp ch)
					   (member ch '(#\- #\_))))))
	  (if (= (length ch) 1)
	      (if (reg-exp-test "/^U[0-9a-f]{4}$/i" ch)
		  (setf ch (lisp-char-from-code (parse-int (aref ch 1)  16)))))))))

(defun read-sharp ()
  (skip #\#)
  (case  (peek()) {
	 (#\\ (next) (read-char))
;;	 (#\/ (read-regexp()
;;			  (#\( (new LispCons(LispSymbol.get("VECTOR") read-list())
;;	 (#\' next() (LispCons.fromArray([ LispSymbol.get("FUNCTION") read-token() ])
             (t
	      (error "Unsupported sharp syntax: #" + (peek)))))

(defun read-quasiquote ()
  (skip #\`)
  (skip-ws)
  (incr in-qq)
  )

(defun read-token ()
  (skip-ws
          (case (peek)
	    (nil skip-comment() continue out
		 (#\"  (read-string))
		 (#\( (read-list))
		 (#\# (read-sharp))
		 (#\` (read-quasiquote))
		 (#\, (read-comma))
		 (#\' (read-quote))))))


(defun read-list ()
  (let ((ret null)
	  (p (skip #\()))
    (loop
       (skip-ws)
       (case  (peek)
	 (#\) (break out))
       (null (break out))
       ("" (skip-comment) (continue out))))))


(defun find-var (name type env)
  for (var i = 0 i < env.length ++i) {
                        var frame = env[i]
                        for (var j = 0 j < frame.length ++j) {
                                var el = frame[j]
(defmethod read-string ((reader lisp-reader))
  (with-slots (stream) reader
    (read-escaped stream #\" #\")))

(defmethod read-regexp ((reader lisp-reader))
  (let ((str  (read_escaped reader #\" #\" t))
	(mods (string-downcase
	       (read-while
		(lambda (ch)
		  (member (char-downcase ch)
			  #'y #'m #'g #'i))))))
    (make-regexp str mods)))

(defmethod skip-comment ((reader lisp-reader))
  (with-slots (stream) reader
    (read-while (lambda (ch)(and ch (not (eq ch #\Newline)))))))

(defun read-symbol ((reader lisp-reader))
  (let ((str
	 (read-while
	  (lambda (ch)
	    (or (unicode-letter-p ch)
		(number-code-p ch)
		(member
		 '(#'% #'$ #'_ #'- #': #'. #'+ #'* #'@ #'! #'? #'& #'= #'< #'> #'[ #'] #'{ #'} #'/)))))))
    (if (and (> (length str) 0) (regexp  "/^-?[0-9]*\.?[0-9]*$/" str))
	(let ((var (parse-float str)))
	  (if (is-float str)
	      (parse-float str)
                (let ((str (up-case str))
		      (m (regexp "/^(.*?)::?(.*)$/" str)))
		  (if m
		      (let ((pak (or (lisp-package::get (aref m 1))
				     "KEYWORD")))
                        (find-or-intern pak (aref m 2)))
                (let ((pak  (intern (lisp-package::get "%") "*PACKAGE*")))
		  (if  (pak.value)
		       (find-or-intern str)
		       (lisp-symbol-get str))))))))))


(defun read-char ()
  (let ((ch (+ (next)
	       (read-while
		(lambda (ch)
		  (or (and (>= ch #'a) (<= ch #'z))
		      (and (>= ch #'A) (<= ch #'Z))
		      (and (>= ch #'0) (<= ch #'9))
		      (member ch '(#\- #\_))))))))
    (if (> (length ch) 1)
	(if (regexp::test "/^U[0-9a-f]{4}$/i" ch)
	    (setf ch (lisp-char-from-code (parse-int (substr ch) 16)))
	    (progn
                                ch = LispChar.fromName(ch);
                                if (ch == null)
                                        croak("Unknown character name: " + ch);
                        }
                        return ch;
                }
                return LispChar.get(ch);
        };


(function(LC){

        var cons = LC.cons
        , car = LC.car
        , cdr = LC.cdr
        , cadr = LC.cadr
        , caddr = LC.caddr
        , cadddr = LC.cadddr
        , cddr = LC.cddr
        , cdddr = LC.cdddr
        , length = LC.len
        , list = LC.fromArray;

        function find_var(name, type, env) {
                env = env.stuff;
                for (var i = 0; i < env.length; ++i) {
                        var frame = env[i];
                        for (var j = 0; j < frame.length; ++j) {
                                var el = frame[j];
                                if (el.name == name && el.type == type)
                                        ([ i j ]
                        }
                }
        )
					)

        (let ((LABEL-NUM 0

        var S-LAMBDA   = (LispSymbol.get "LAMBDA"))))
        var S-FN       = LispSymbol.get("%FN")
        var S-(= LispSymbol.get("FUNCTION")
        var S-IF       = LispSymbol.get("IF")
        var S-PROGN    = LispSymbol.get("PROGN")
        var S-QUOTE    = LispSymbol.get("QUOTE")
        var S-SET      = LispSymbol.get("SETQ")
        var S-T        = LispSymbol.get("T")
        var S-NIL      = LispSymbol.get("NIL")
        var S-CC       = LispSymbol.get("C/C")
        var S-NOT      = LispSymbol.get("NOT")
        var S-LET      = LispSymbol.get("LET")
        var S-LET$     = LispSymbol.get("LET*")
        var S-LABELS   = LispSymbol.get("LABELS")
        var S-FLET     = LispSymbol.get("FLET")
	)

        (let ((PAK-KEYWORD (LispPackage.get "KEYWORD"))))
	)


	(defun append ()
                (let ((ret []
                for  (var i = 0 i < arguments.length ++i) {)))
                        var el = arguments[i]
                        if (el.length > 0)
                                ret.push.apply(ret el)
                }
                (ret
        )
		)


		(defun gen-label ()
                (new LispSymbol
        )

        (let ((seq append


	 (defun gen ())))
                ([ slice(arguments) ]
        )
		)


		(defun nullp(x) {
                (x === S-NIL || x == null || (x instanceof Array && x.length == 0)
        )


		(defun arg-count(form min max) {
                if (max == null) max = min
                (let ((len (length cdr(form)))))
                if (len < min) throw new Error("Expecting at least " + min + " arguments")
                if (len > max) throw new Error("Expecting at most " + max + " arguments")
        )
		)


		(defun assert(cond error) {
                if (!cond) throw new Error(error)
        )


		(defun comp(x env VAL MORE) {
                if (nullp(x)) (comp-const(null VAL MORE)
                if (LispSymbol.is(x)) {
                        switch (x) {
                            case S-NIL: (comp-const(null VAL MORE)
                            case S-T: (comp-const(true VAL MORE)
                        }
                        if (x.pak === PAK-KEYWORD)
                                (comp-const(x VAL MORE)
                        (comp-var(x env VAL MORE)
                }
                else if (LispMachine.constantp(x)) {
                        (comp-const(x VAL MORE)
                }
                else switch (car(x)) {
                    case S-QUOTE:
                        arg-count(x 1)
                        (comp-const(cadr(x) VAL MORE)
                    case S-PROGN:
                        (comp-seq(cdr(x) env VAL MORE)
                    case S-SET:
                        arg-count(x 2)
                        assert(LispSymbol.is(cadr(x)) "Only symbols can be set")
                        (seq(comp(caddr(x) env true true),
                                   gen-set(cadr(x) env),
                                   VAL ? [] : gen("POP"),
                                   MORE ? [] : gen("RET"))
                    case S-IF:
                        arg-count(x 2 3)
                        (comp-if(cadr(x) caddr(x) cadddr(x) env VAL MORE)
                    case S-NOT:
                        arg-count(x 1)
                        (VAL ? seq(
                                comp(cadr(x) env true true),
                                gen("NOT"),
                                MORE ? [] : gen("RET")
                        ) : comp(cadr(x) env VAL MORE)
                    case S-CC:
                        arg-count(x 0)
                        (VAL ? seq(gen("CC")) : []
                    case S-LET:
                        (comp-let(cadr(x) cddr(x) env VAL MORE)
                    case S-LET$:
                        (comp-let$(cadr(x) cddr(x) env VAL MORE)
                    case S-LABELS:
                        (comp-flets(cadr(x) cddr(x) env true VAL MORE)
                    case S-FLET:
                        (comp-flets(cadr(x) cddr(x) env false VAL MORE)
                    case S-FN:
                        assert(LispSymbol.is(cadr(x)) "%FN requires a symbol name for the function")
                        (VAL ? seq(
                                comp-lambda(cadr(x) caddr(x) cdddr(x) env),
                                MORE ? [] : gen("RET")
                        ) : []
                    case S-LAMBDA:
                        (VAL ? seq(
                                comp-lambda(null cadr(x) cddr(x) env),
                                MORE ? [] : gen("RET")
                        ) : []
                    case S-FUNCTION:
                        arg-count(x 1)
                        assert(LispSymbol.is(cadr(x)) "(requires a symbol")
                        if (VAL) {
                                (let ((localfunc (find-var cadr(x) "func" env))))
                                if (localfunc) (gen("LVAR" localfunc[0] localfunc[1])
                                (gen("FGVAR" cadr(x))
                        }
                        ([]
                    default:
                        if (LispSymbol.is(car(x)) && car(x).macro())
                                (comp-macroexpand(car(x) cdr(x) env VAL MORE)
                        (comp-funcall(car(x) cdr(x) env VAL MORE)
                }
        )
			)


			(defun gen-set(name env) {
                if (!name.special()) {
                        (let ((p (find-var name "var" env))))
                        if (p) (gen("LSET" p[0] p[1])
                }
                (gen("GSET" name)
        )
		)


		(defun gen-var(name env) {
                if (!name.special()) {
                        (let ((pos (find-var name "var" env))))
                        if (pos) (gen("LVAR" pos[0] pos[1])
                }
                (gen("GVAR" name)
        )
		)


		(defun comp-const(x VAL MORE) {
                (VAL ? seq(
                        gen("CONST" x === S-NIL ? null : x === S-T ? true : x),
                        MORE ? [] : gen("RET")
                ) : []
        )


		(defun comp-var(x env VAL MORE) {
                (VAL ? seq(
                        gen-var(x env),
                        MORE ? [] : gen("RET")
                ) : []
        )


		(defun comp-seq(exps env VAL MORE) {
                if (nullp(exps)) (comp-const(null VAL MORE)
                if (nullp(cdr(exps))) (comp(car(exps) env VAL MORE)
                (seq(comp(car(exps) env false true),
                           comp-seq(cdr(exps) env VAL MORE))
        )


		(defun comp-list(exps env) {
                if (!nullp(exps)) (seq(
                        comp(car(exps) env true true),
                        comp-list(cdr(exps) env)
                )
                ([]
        )


		(defun comp-if(pred tthen telse env VAL MORE) {
                (let ((pcode (comp pred env true true))))
                var tcode = comp(tthen env VAL MORE)
                var ecode = comp(telse env VAL MORE)
                var l1 = gen-label() l2 = gen-label()
                (seq(
                        pcode,
                        gen("FJUMP" l1),
                        tcode,
                        MORE ? gen("JUMP" l2) : [],
                        [ l1 ],
                        ecode,
                        MORE ? [ l2 ] : []
                )
        )
		)


		(defun get-bindings(bindings flet) {
                (let ((names [] vals = [] specials = []
                (LispCons.forEach bindings function(el i dot))))
                        if (dot) throw new Error("Improper list in LET")
                        if (LC.is(el)) {
                                vals.push(flet ? cdr(el) : cadr(el))
                                el = car(el)
                        } else {
                                vals.push(S-NIL)
                        }
                        if (names.indexOf(el) >= 0)
                                throw new Error("Duplicate name in LET")
                        names.push(el)
                        if (el.special()) specials.push(i)
                })
                ({ names: names vals: vals specials: specials len: names.length )
        )
		)


		(defun comp-let(bindings body env VAL MORE) {
                if (nullp(bindings)) (comp-seq(body env VAL MORE)
                (let ((b (get-bindings bindings))))
                var ret = seq(
                        seq.apply(null b.vals.map (lambda (x)
                                (comp(x env true true)
                        })),
                        gen("LET" b.len),
                        b.specials.map (lambda (i)
                                (gen("BIND" b.names[i] i)[0]
                        }),
                        comp-seq(body env.extend("var" b.names) VAL true),
                        gen("UNFR" 1 b.specials.length 0),
                        MORE ? [] : gen("RET")
                )
                (ret
        )
		)


		(defun comp-let$(bindings body env VAL MORE) {
                if (nullp(bindings)) (comp-seq(body env VAL MORE)
                (let ((b (get-bindings bindings))))
                var newargs = []
                var ret = seq(
                        seq.apply(null b.vals.map (lambda (x i)
                                var name = b.names[i]
                                x = seq(
                                        comp(x env true true),
                                        i == 0 ? gen("FRAME") : [],
                                        gen("VAR"),
                                        name.special() ? gen("BIND" name i) : []
                                )
                                if (i == 0) {
                                        env = env.extend("var" newargs)
                                }
                                env.add(name "var")
                                (x
                        })),
                        comp-seq(body env VAL true),
                        gen("UNFR" 1 b.specials.length 0),
                        MORE ? [] : gen("RET")
                )
                (ret
        )
		)


		(defun comp-flets(bindings body env is-labels VAL MORE) {
                if (nullp(bindings)) (comp-seq(body env VAL MORE)
                (let ((b (get-bindings bindings true))))
                if (is-labels) env = env.extend("func" b.names)
                (seq(
                        is-labels ? gen("FRAME") : [],
                        seq.apply(null b.names.map (lambda (name i)
                                (comp-lambda(name car(b.vals[i]) cdr(b.vals[i]) env)
                        })),
                        is-labels ? [] : gen("FRAME"),
                        b.len > 1 ? gen("VARS" b.len) : gen("VAR"),
                        comp-seq(body is-labels ? env : env.extend("func" b.names) VAL true),
                        gen("UNFR" 1 0),
                        MORE ? [] : gen("RET")
                )
        )
		    )


		    (defun comp-funcall(f args env VAL MORE) {

			   (defun mkret(the-function) {
                        if (MORE) {
                                (let ((k (gen-label ))))
                                (seq(
                                        gen("SAVE" k),
                                        comp-list(args env),
                                        the-function,
                                        gen("CALL" length(args)),
                                        [ k ],
                                        VAL ? [] : gen("POP")
                                )
                        }
                        (seq(
                                comp-list(args env),
                                the-function,
                                gen("CALL" length(args))
                        )
                )
                if (LispSymbol.is(f)) {
                        var localfun = find-var(f "func" env)
                        if (localfun) (mkret(gen("LVAR" localfun[0] localfun[1]))
                        if (f.primitive()) {
                                if (!VAL && !f.getv("primitive-side-effects")) {
                                        (comp-seq(args env false MORE)
                                }
                                (seq(comp-list(args env),
                                           gen("PRIM" f length(args)),
                                           VAL ? [] : gen("POP"),
                                           MORE ? [] : gen("RET"))
                        }
                        if (!f.func()) console.warn("Undefined function" f)
                        (mkret(gen("FGVAR" f))
                }
                if (LC.is(f) && car(f) === S-LAMBDA && nullp(cadr(f))) {
                        assert(nullp(args) "Too many arguments")
                        (comp-seq(cddr(f) env VAL MORE)
                }
                (mkret(comp(f env true true))
        )
		)

(defun comp-lambda(name args body env) {
                if (LC.isList(args)) {
                        (let ((dot (LC.isDotted args))))
                        var a = LC.toArray(args)
                        if (dot) a.push([ a.pop() a.pop() ][0])
                        var dyn = []
                        for (var i = a.length --i >= 0) {
                                if (a[i].special())
                                        dyn.push([ "BIND" a[i] i ])
                        }
                        if (!dot) {
                                (gen("FN",
                                           seq(a.length > 0 ? gen("ARGS" a.length) : [],
                                               dyn,
                                               comp-seq(body a.length > 0 ? env.extend("var" a) : env true false)),
                                           name)
                        }
                        (gen("FN",
                                   seq(gen("ARG-" dot),
                                       dyn,
                                       comp-seq(body env.extend("var" a) true false)),
                                   name)
                } else {
                        (gen("FN",
                                   seq(gen("ARG-" 0),
                                       args.special() ? gen("BIND" args 0) : [],
                                       comp-seq(body env.extend("var" [ args ]) true false)),
                                   name)
                }
        )
			)

(defun comp-macroexpand(name args env VAL MORE) {
                (let ((m new (LispMachine ))))
                var ast = m.-call(name.macro() args)
                var ret = comp(ast env VAL MORE)
                (ret
        )
)
        this.lisp-compile = function(x) {
                (comp-seq(x new Environment() true true)
        )

        var Environment = DEFCLASS("Environment" null function(D P)
                P.INIT = function ()
                        this.stuff = []
                )
                P.extend = function(type val) {
                        var env = new Environment()
                        env.stuff = [ val.map (lambda (name)
                                ({ name: name type: type )
                        }) ].concat(this.stuff)
                        (env
                )
                P.add = function(name type) {
                        this.stuff[0].push({ name: name type: type })
                )
        })

})(LispCons)
