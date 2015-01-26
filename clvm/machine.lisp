(defclass lisp-cleanup
)
(defclass lisp-ret)

(defmethod run ((lr lisp-return))
  )

(defclass lisp-long-ret
)

(defmethod unwind ((llr lisp-long-return) m addr)
)

(defmethod run ((lr lisp-long-return))
  )


(defclass lisp-continuation
)

(defmethod run ((lc lisp-continuation))
  )

(defclass lisp-dynamic-binding
)

(defmethod run ((ldb lisp-dynamic-binding))
  )


(defclass lisp-catch
)

(defmethod run ((ldb lisp-dynamic-binding))
  )

(defclass lisp-machine
)

(defmethod init ((lm lisp-machine))
  )

(defmethod find-dynamic-variable ((lm lisp-machine) symbol)
  )

(defmethod get-global-value ((lm lisp-machine) symbol)
  )

(defmethod set-global-value ((lm lisp-machine) symbol value)
  )

(defmethod push-global-value ((lm lisp-machine) symbol value)
  )

(defmethod bind-value ((lm lisp-machine) symbol value)
  )

(defmethod push-stack ((lm lisp-machine) value)
  )

(defmethod pop-stack ((lm lisp-machine) value)
  )

(defmethod make-return ((lm lisp-machine) value)
  )


(defmethod unreturn ((lm lisp-machine) value)
  )

(defmethod make-contination ((lm lisp-machine))
  )

(defmethod uncontination ((lm lisp-machine))
  )

(defmethod top ((lm lisp-machine))
  )

(defmethod loop ((lm lisp-machine))
  )

(defmethod attomic-call ((lm lisp-machine) clojure args)
  )

(defmethod exec ((lm lisp-machine) clojure args)
  )

(defmethod low-level-call ((lm lisp-machine) clojure args)
  )

(defmethod call-next ((lm lisp-machine) clojure args)
  )

(defmethod set-clojure ((lm lisp-machine) clojure)
  )

(defmethod run ((lm lisp-machine) quota)
  )

var LispMachine = DEFCLASS("LispMachine", null, function(D, P){
        var OPS = {};

        D.stats = {
        };

        function inc_stat(name) {
                if (!D.stats[name]) D.stats[name] = 0;
                ++D.stats[name];
        };

        function max_stat(name, val) {
                if (!HOP(D.stats, name) || D.stats[name] < val)
                        D.stats[name] = val;
        };

        var optimize = (function(){
                function find_target(code, label) {
                        return code.indexOf(label);
                };
                function used_label(code, label) {
                        for (var i = code.length; --i >= 0;) {
                                var el = code[i];
                                if (!LispSymbol.is(el)) {
                                        if (el[1] === label)
                                                return true;
                                        if (el[0] === "FN" && used_label(el[1], label))
                                                return true;
                                }
                        }
                };
                function optimize1(code, i) {
                        var el = code[i];
                        if (LispSymbol.is(el)) {
                                if (!used_label(code, el)) {
                                        code.splice(i, 1);
                                        inc_stat("drop_label");
                                        return true;
                                }
                                return false;
                        }
                        switch (el[0]) {
                            case "VARS":
                                if (el[1] == 1) {
                                        code.splice(i, 1, [ "VAR" ]);
                                        inc_stat("vars");
                                        return true;
                                }
                                break;
                            case "JUMP":
                            case "TJUMP":
                            case "FJUMP":
                                for (var j = i + 1; j < code.length && LispSymbol.is(code[j]); ++j) {
                                        if (el[1] === code[j]) {
                                                if (el[0] == "JUMP") code.splice(i, 1);
                                                else code.splice(i, 1, [ "POP" ]);
                                                inc_stat("jumps");
                                                return true;
                                        }
                                }
                                break;
                            case "LVAR":
                            case "GVAR":
                                if (i < code.length - 1 && code[i+1][0] == "POP") {
                                        code.splice(i, 2);
                                        return true;
                                }
                                break;
                            case "PRIM":
                                if (el[1].pak === BASE_PACK) {
                                        if (/^C[AD]{1,4}R$/.test(el[1].name)) {
                                                inc_stat("primitives");
                                                code.splice(i, 1, [ el[1].name ]);
                                                return true;
                                        }
                                        switch (el[1].name) {
                                            case "CONS":
                                                inc_stat("primitives");
                                                code.splice(i, 1, [ "CONS" ]);
                                                return true;
                                            case "LIST":
                                                inc_stat("primitives");
                                                code.splice(i, 1, [ "LIST", el[2] ]);
                                                return true;
                                            case "LIST*":
                                                inc_stat("primitives");
                                                code.splice(i, 1, [ "LIST_", el[2] ]);
                                                return true;
                                        }
                                }
                        }
                        switch (el[0]) {
                            case "GSET":
                            case "GVAR":
                                if (i < code.length - 2 &&
                                    code[i+1][0] == "POP" &&
                                    code[i+2][0] == "GVAR" &&
                                    code[i+2][1] == el[1]) {
                                        code.splice(i + 1, 2);
                                        inc_stat("gvar");
                                        return true;
                                }
                                break;
                            case "LSET":
                            case "LVAR":
                                if (i < code.length - 2 &&
                                    code[i+1][0] == "POP" &&
                                    code[i+2][0] == "LVAR" &&
                                    code[i+2][1] == el[1] &&
                                    code[i+2][2] == el[2]) {
                                        code.splice(i + 1, 2);
                                        inc_stat("lvar");
                                        return true;
                                }
                                break;
                            case "SAVE":
                            case "FJUMP":
                            case "TJUMP":
                                // SAVE L1; ... L1: JUMP L2 --> SAVE L2
                                var idx = find_target(code, el[1]);
                                if (idx >= 0 && idx < code.length - 1 && code[idx + 1][0] == "JUMP") {
                                        el[1] = code[idx + 1][1];
                                        inc_stat("save_jump");
                                        return true;
                                }
                                break;
                            case "JUMP":
                                var idx = find_target(code, el[1]);
                                if (idx >= 0 && idx < code.length - 1 &&
                                    (code[idx + 1][0] == "JUMP" || code[idx + 1][0] == "RET")) {
                                        el[0] = code[idx + 1][0];
                                        el[1] = code[idx + 1][1];
                                        inc_stat("jumps");
                                        return true;
                                }
                            case "CALLJ":
                            case "RET":
                                for (var j = i; ++j < code.length;) {
                                        if (LispSymbol.is(code[j])) {
                                                break;
                                        }
                                }
                                if (j - i - 1 > 0) {
                                        code.splice(i + 1, j - i - 1);
                                        inc_stat("unreachable");
                                        return true;
                                }
                                break;
                            case "UNFR":
                                if (i < code.length - 1) {
                                        if (code[i+1][0] == "UNFR") {
                                                code[i][1] += code[i+1][1];
                                                code[i][2] += code[i+1][2];
                                                code.splice(i + 1, 1);
                                                inc_stat("join_unfr");
                                                return true;
                                        }
                                        if (code[i+1][0] == "RET") {
                                                code.splice(i, 1);
                                                return true;
                                        }
                                }
                                break;
                        }
                        if (i < code.length - 1) {
                                if ((el[0] == "CONST" && el[1] === null) || el[0] == "NIL") {
                                        switch (code[i+1][0]) {
                                            case "FJUMP":
                                                code.splice(i, 2, [ "JUMP", code[i+1][1] ]);
                                                inc_stat("const");
                                                return true;
                                            case "TJUMP":
                                                code.splice(i, 2);
                                                inc_stat("const");
                                                return true;
                                            case "NOT":
                                                inc_stat("const");
                                                code.splice(i, 2, [ "T" ]);
                                                return true;
                                        }
                                        if (el[0] == "CONST" && el[1] === null) {
                                                inc_stat("const");
                                                code.splice(i, 1, [ "NIL" ]);
                                                return true;
                                        }
                                }
                                if ((el[0] == "CONST" && constantp(el[1])) || el[0] == "T") {
                                        switch (code[i+1][0]) {
                                            case "FJUMP":
                                                code.splice(i, 2);
                                                inc_stat("const");
                                                return true;
                                            case "TJUMP":
                                                code.splice(i, 2, [ "JUMP", code[i+1][1] ]);
                                                inc_stat("const");
                                                return true;
                                            case "NOT":
                                                inc_stat("const");
                                                code.splice(i, 2, [ "NIL" ]);
                                                return true;
                                        }
                                        if (el[0] == "CONST" && el[1] === true) {
                                                inc_stat("const");
                                                code.splice(i, 1, [ "T" ]);
                                                return true;
                                        }
                                }
                        }
                        switch (el[0]) {
                            case "NIL":
                                if (i < code.length - 1) {
                                        if (code[i+1][0] == "CONS") {
                                                inc_stat("lists");
                                                code.splice(i, 2, [ "LIST", 1 ]);
                                                return true;
                                        }
                                }
                                break;
                            case "LIST":
                            case "LIST_":
                                if (i < code.length - 1) {
                                        if (code[i+1][0] == "CONS") {
                                                inc_stat("lists");
                                                code.splice(i, 2, [ el[0], el[1] + 1 ]);
                                                return true;
                                        }
                                        if (code[i+1][0] == "LIST_") {
                                                inc_stat("lists");
                                                code.splice(i, 2, [ el[0], el[1] + code[i+1][1] - 1 ]);
                                                return true;
                                        }
                                }
                                break;
                            case "CONS":
                                if (i < code.length - 1) {
                                        if (code[i+1][0] == "CONS") {
                                                inc_stat("lists");
                                                code.splice(i, 2, [ "LIST_", 3 ]);
                                                return true;
                                        }
                                }
                                break;
                        }
                };
                return function optimize(code) {
                        while (true) {
                                var changed = false;
                                for (var i = 0; i < code.length; ++i)
                                        if (optimize1(code, i)) changed = true;
                                if (!changed) break;
                        }
                };
        })();

        function constantp(x) {
                return x === true ||
                        x === null ||
                        typeof x == "number" ||
                        typeof x == "string" ||
                        x instanceof RegExp ||
                        LispChar.is(x) ||
                        LispSymbol.is(x);
        };

        function is_jump_instruction(op) {
                switch (op) {
                    case "JUMP":
                    case "TJUMP":
                    case "FJUMP":
                    case "LRET":
                    case "LJUMP":
                    case "UPOPEN":
                    case "SAVE":
                    case "CATCH":
                        return true;
                }
        };

        function assemble(code) {
                optimize(code);
                var ret = [];
                for (var i = 0; i < code.length; ++i) {
                        var el = code[i];
                        if (LispSymbol.is(el)) el.value = ret.length;
                        else ret.push(el);
                }
                for (var i = ret.length; --i >= 0;) {
                        var el = ret[i];
                        switch (el[0]) {
                            case "FN":
                                ret[i] = OPS.FN.make(assemble(el[1]), el[2]);
                                break;
                            default:
                                if (is_jump_instruction(el[0]))
                                        el[1] = el[1].value;
                                ret[i] = OPS[el[0]].make.apply(null, el.slice(1));
                        }
                }
                return ret;
        };

        function relocate(code, addr) {
                for (var i = code.length; --i >= 0;) {
                        var op = code[i];
                        if (is_jump_instruction(op._name))
                                op.addr += addr;
                }
                return code;
        };

        D.assemble = assemble;
        D.constantp = constantp;
        D.relocate = relocate;

        ////// <disassemble>

        var INDENT_LEVEL = 8;

        function indent(level) {
                return repeat_string(' ', level * INDENT_LEVEL);
        };

        D.disassemble = function(code) {
                var lab = 0;
                function disassemble(code, level) {
                        var labels = {};
                        code.forEach(function(op, i){
                                if (is_jump_instruction(op._name))
                                        if (!HOP(labels, op.addr))
                                                labels[op.addr] = "L" + (++lab);
                        });
                        return code.map(function(op, i){
                                var l = labels[i] || "";
                                if (l) l += ":";
                                var data;
                                var opcode = op._name;
                                switch (opcode) {
                                    case "FN":
                                        opcode = "λ:" + op.name;
                                        data = "\n" + disassemble(op.code, level + 1);
                                        break;
                                    case "PRIM":
                                        data = op.name + " " + op.nargs;
                                        break;
                                    case "CONST":
                                        data = LispMachine.dump(op.val);
                                        break;
                                    default:
                                        if (is_jump_instruction(opcode)) {
                                                data = labels[op.addr];
                                                break;
                                        }
                                        data = op._args.map(function(el){
                                                return pad_string(
                                                        LispMachine.serialize_const(op[el]),
                                                        8
                                                );
                                        }).join("");
                                }
                                var line = pad_string(l, INDENT_LEVEL)
                                        + indent(level)
                                        + pad_string(opcode, INDENT_LEVEL)
                                        + data;
                                return line;
                        }).join("\n");
                };
                return disassemble(code, 0);
        };

        ///// </disassemble>

        D.serialize = function(code, strip) {
                code = code.map(function(op){
                        return op._disp();
                }).join(",");
                return strip ? code : "[" + code + "]";
        };

        D.unserialize = function(code) {
                var names = [], values = [];
                for (var i in OPS) if (HOP(OPS, i)) {
                        var op = OPS[i];
                        names.push(i);
                        values.push(op.make);
                }
                names.push("s"); values.push(function(name, pak){
                        if (pak != null) {
                                pak = LispPackage.get(pak);
                                return LispSymbol.get(name, pak);
                        }
                        return new LispSymbol(name);
                });
                names.push("p"); values.push(function(name){
                        return LispPackage.get(name);
                });
                names.push("l"); values.push(function(){
                        return LispCons.fromArray(slice(arguments));
                });
                names.push("c"); values.push(function(char){
                        return LispChar.get(char);
                });
                names.push("DOT"); values.push(LispCons.DOT);
                var func = new Function("return function(" + names.join(",") + "){return[" + code + "]}")();
                code = func.apply(null, values);
                return code;
        };

        function serialize_const(val) {
                if (val === null || val === true) return val + "";
                if (LispSymbol.is(val) || LispPackage.is(val) || LispChar.is(val)) return val.serialize();
                if (val instanceof RegExp) return val.toString();
                if (LispCons.is(val)) return "l(" + LispCons.toArray(val).map(serialize_const).join(",") + ")";
                if (val instanceof Array) return "[" + val.map(serialize_const).join(",") + "]";
                if (typeof val == "string") return LispChar.sanitize(JSON.stringify(val));
                return val + "";
        };

        D.serialize_const = serialize_const;

        var OP = DEFCLASS("NOP", null, function(D, P){
                P._disp = function() {
                        var self = this;
                        return self._name + "(" + self._args.map(function(el){
                                return serialize_const(self[el]);
                        }).join(",") + ")";
                };
        });

        function defop(name, args, proto) {
                args = args ? args.split(" ") : [];
                var ctor = new Function(
                        "return function " + name + "(" + args.join(", ") + "){ " +
                                args.map(function(arg){
                                        return "this." + arg + " = " + arg;
                                }).join("; ") + "; this.INIT() };"
                )();
                ctor.prototype = new OP;
                ctor.make = new Function(
                        "OP",
                        "return function(" + args.join(",") + "){return new OP(" + args.join(",") + ")}"
                )(ctor);
                proto._name = name;
                proto._args = args;
                for (var i in proto) if (HOP(proto, i)) {
                        ctor.prototype[i] = proto[i];
                }
                return OPS[name] = ctor;
        };

        function frame(env, i) {
                while (i-- > 0) env = env.cdr;
                return env.car;
        };

        function rewind(env, i) {
                while (i-- > 0) env = env.cdr;
                return env;
        };

        var S_NIL = LispSymbol.get("NIL");
        var S_T = LispSymbol.get("T");
        function eq(a, b) {
                return (a === S_NIL && b === null) ||
                        (a === null && b === S_NIL) ||
                        (a === S_T && b === true) ||
                        (a === true && b === S_T) ||
                        a === b ? true : null;
        };

        [
                //// local vars namespace
                ["LVAR", "i j", {
                        run: function(m) {
                                //max_stat("lvar_frame", this.i);
                                //max_stat("lvar_index", this.j);
                                m.push(frame(m.env, this.i)[this.j]);
                        }
                }],
                ["LSET", "i j", {
                        run: function(m) {
                                frame(m.env, this.i)[this.j] = m.top();
                        }
                }],
                //// global/dynamic vars namespace
                ["GVAR", "name", {
                        run: function(m) {
                                m.push(m.gvar(this.name));
                        }
                }],
                ["GSET", "name", {
                        run: function(m) {
                                m.gset(this.name, m.top());
                        }
                }],
                ["BIND", "name i", {
                        run: function(m) {
                                m.bind(this.name, this.i);
                        }
                }],
                //// global functions namespace
                ["FGVAR", "name", {
                        run: function(m) {
                                var f = this.name.func();
                                if (!f) console.error("Undefined function", this.name);
                                m.push(f);
                        }
                }],
                ["FGSET", "name", {
                        run: function(m) {
                                this.name.setv("function", m.top());
                        }
                }],
                ////
                ["POP", 0, {
                        run: function(m) {
                                m.pop();
                        }
                }],
                ["CONST", "val", {
                        run: function(m) {
                                m.push(this.val);
                        }
                }],
                ["JUMP", "addr", {
                        run: function(m) {
                                m.pc = this.addr;
                        }
                }],
                ["TJUMP", "addr", {
                        run: function(m) {
                                if (m.pop() !== null) m.pc = this.addr;
                        }
                }],
                ["FJUMP", "addr", {
                        run: function(m) {
                                if (m.pop() === null) m.pc = this.addr;
                        }
                }],
                ["BLOCK", 0, {
                        run: function(m) {
                                // this is moderately tricky: we can't do
                                //   m.env = new LispCons([ new LispLongRet(m) ], m.env);
                                // I'll let you figure out why.
                                var frame = [];
                                m.env = new LispCons(frame, m.env);
                                frame[0] = new LispLongRet(m);
                        }
                }],
                ["LJUMP", "addr", {
                        run: function(m) {
                                m.pop().run(m, this.addr);
                        }
                }],
                ["LRET", "addr", {
                        run: function(m) {
                                var bret = m.pop(), val = m.pop();
                                bret.run(m, this.addr);
                                m.push(val);
                        }
                }],
                ["NOT", 0, {
                        run: function(m) {
                                m.push(m.pop() === null ? true : null);
                        }
                }],
                ["SETCC", 0, {
                        run: function(m) {
                                m.uncont(m.top());
                        }
                }],
                ["SAVE", "addr", {
                        run: function(m) {
                                m.push(m.mkret(this.addr));
                        }
                }],
                ["RET", 0, {
                        run: function(m) {
                                var noval = m.f.noval;
                                var val = m.pop();
                                m.unret(m.pop());
                                if (!noval) m.push(val);
                        }
                }],
                ["CALL", "count", {
                        run: function(m){
                                var closure = m.pop();
                                //if (m.trace) m.trace.push([ closure, m.stack.slice(-this.count) ]);
                                m.n_args = this.count;
                                m.code = closure.code;
                                m.env = closure.env;
                                m.pc = 0;
                                m.f = closure;
                        }
                }],
                ["EXEC", 0, {
                        run: function(m){
                                var closure = m.pop();
                                //if (m.trace) m.trace.push([ closure, [] ]);
                                m.push(m.mkret(m.pc));
                                m.n_args = 0;
                                m.code = closure.code;
                                m.env = closure.env;
                                m.pc = 0;
                                m.f = closure;
                        }
                }],
                /// <unwind-protect>
                ["UPOPEN", "addr", {
                        // push the cleanup code
                        run: function(m) {
                                var c = new LispCleanup(new LispLongRet(m), this.addr);
                                m.dynpush(c);
                        }
                }],
                ["UPEXIT", 0, {
                        // normal exit, we should run the cleanup code
                        // that's guaranteed to be at the top of denv
                        run: function(m){
                                // no need to run it, we're already in
                                // the right place.  just discard.
                                m.denv = m.denv.cdr;
                                m.after_cleanup = null;
                        }
                }],
                ["UPCLOSE", 0, {
                        // this instruction is closing the cleanup
                        // code of an unwind-protect.  If we got here
                        // as a result of a long jump, top of stack
                        // will contain a LispCleanup object that
                        // resumes the jump.
                        run: function(m){
                                if (m.after_cleanup) m.after_cleanup(m);
                        }
                }],
                /// </unwind-protect>
                /// <throw,catch>
                ["CATCH", "addr", {
                        run: function(m) {
                                var c = new LispCatch(m, this.addr, m.pop());
                                m.dynpush(c);
                        }
                }],
                ["THROW", 0, {
                        run: function(m) {
                                var val = m.pop();
                                var tag = m.pop();
                                var p = m.denv;
                                while (p) {
                                        var el = p.car;
                                        if (el instanceof LispCatch && eq(el.tag, tag)) {
                                                el.run(m, val);
                                                return;
                                        }
                                        p = p.cdr;
                                }
                                throw new LispPrimitiveError("CATCH tag not found " + LispMachine.dump(tag));
                        }
                }],
                /// </throw,catch>
                ["LET", "count", {
                        run: function(m){
                                var count = this.count;
                                var frame = new Array(count);
                                while (--count >= 0) frame[count] = m.pop();
                                m.env = new LispCons(frame, m.env);
                        }
                }],
                ["ARGS", "count", {
                        run: function(m){
                                var count = this.count;
                                if (count != m.n_args) {
                                        console.error(m.f);
                                        throw new LispPrimitiveError("Wrong number of arguments - expecting " + count + ", got " + m.n_args);
                                }
                                var frame = new Array(count);
                                while (--count >= 0) frame[count] = m.pop();
                                m.env = new LispCons(frame, m.env);
                        }
                }],
                ["ARG_", "count", {
                        run: function(m) {
                                var count = this.count;
                                var passed = m.n_args;
                                if (passed < count) throw new LispPrimitiveError("Insufficient number of arguments");
                                var p = null;
                                while (passed-- > count) p = new LispCons(m.pop(), p);
                                var frame = new Array(count + 1);
                                frame[count] = p;
                                while (--count >= 0) frame[count] = m.pop();
                                m.env = new LispCons(frame, m.env);
                        }
                }],
                ["FRAME", 0, {
                        run: function(m) {
                                m.env = new LispCons([], m.env);
                        }
                }],
                ["VAR", 0, {
                        run: function(m) {
                                m.env.car.push(m.pop());
                        }
                }],
                ["VARS", "count", {
                        run: function(m) {
                                var count = this.count, a = m.env.car, n = a.length;
                                while (--count >= 0) a[n + count] = m.pop();
                        }
                }],
                ["UNFR", "lex spec", {
                        run: function(m) {
                                if (this.lex) m.env = rewind(m.env, this.lex);
                                if (this.spec) m.denv = rewind(m.denv, this.spec);
                        }
                }],
                ["FN", "code name", {
                        run: function(m) {
                                m.push(new LispClosure(this.code, this.name, m.env));
                        },
                        _disp: function() {
                                return "FN(" + D.serialize(this.code) + (this.name ? "," + LispMachine.serialize_const(this.name) : "") + ")";
                        }
                }],
                ["PRIM", "name nargs", {
                        run: function(m) {
                                var n = this.nargs;
                                if (n == -1) n = m.n_args;
                                var ret = this.name.primitive()(m, n);
                                if (ret !== false) m.push(ret);
                        }
                }],
                ["NIL", 0, { run: function(m) { m.push(null) } }],
                ["T", 0, { run: function(m) { m.push(true) } }],
                ["CONS", 0, {
                        run: function(m) {
                                var b = m.pop(), a = m.pop();
                                m.push(new LispCons(a, b));
                        }
                }],
                ["LIST", "count", {
                        run: function(m) {
                                var p = null, n = this.count;
                                if (n == -1) n = m.n_args;
                                while (n-- > 0) p = new LispCons(m.pop(), p);
                                m.push(p);
                        }
                }],
                ["LIST_", "count", {
                        run: function(m) {
                                var p = m.pop(), n = this.count;
                                if (n == -1) n = m.n_args;
                                while (--n > 0) p = new LispCons(m.pop(), p);
                                m.push(p);
                        }
                }]

        ].map(function(_){ defop(_[0], _[1], _[2]) });

        defop("CC", 0, {
                run: function(cc){
                        return function(m) {
                                m.push(new LispClosure(cc, null, new LispCons([ m.mkcont() ])));
                        }
                }(assemble([
                        ["ARGS", 1],
                        ["LVAR", 1, 0],
                        ["SETCC"],
                        ["LVAR", 0, 0],
                        ["RET"]
                ]))
        });

        (function(i){
                for (i in LispCons) if (HOP(LispCons, i) && /^c[ad]+r$/.test(i)) {
                        defop(i.toUpperCase(), 0, {
                                run: new Function("f", "return function(m){ m.push(f(m.pop())) }")(LispCons[i])
                        });
                }
        })();

        var S_QUOTE = LispSymbol.get("QUOTE");

        P.dump = D.dump = function(thing) {
                if (thing === null) return "NIL";
                if (thing === true) return "T";
                if (typeof thing == "string") return JSON.stringify(LispChar.sanitize(thing));
                if (LispCons.is(thing)) {
                        if (LispCons.car(thing) === S_QUOTE && LispCons.len(thing) == 2)
                                return "'" + D.dump(LispCons.cadr(thing));
                        var ret = "(", first = true;
                        while (thing !== null) {
                                if (!first) ret += " ";
                                else first = false;
                                ret += D.dump(LispCons.car(thing));
                                thing = LispCons.cdr(thing);
                                if (!LispCons.isList(thing)) {
                                        ret += " . " + D.dump(thing);
                                        break;
                                }
                        }
                        return ret + ")";
                }
                if (LispType.is(thing)) return thing.print();
                return thing + "";
        };

});
