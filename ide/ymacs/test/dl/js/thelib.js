 /*
 * DynarchLIB version: 2.0
 *                                                                ____   _____
 * A lightweight JavaScript toolkit for professionals.            \  /_  /   /
 *                                                                 \  / /   /
 * Copyright (c) Dynarch.com 2005-2010.  All rights reserved.       \/ /_  /
 * See http://www.dynarchlib.com/licensing for licensing details.    \  / /
 *                                                                     / /
 *          this package built at 2012/07/17 11:24 GMT                 \/
 */
function $_YIELD(e) {
    this.timeout = e || 0
}
function $_RETURN(e) {
    this.args = e
}
function $YIELD(e) {
    throw new $_YIELD(e)
}
function $BREAK() {
    throw $_BREAK
}
function $CONTINUE() {
    throw $_CONTINUE
}
function $RETURN(e) {
    throw new $_RETURN(e)
}

function DEFINE_CLASS(name, base, definition, hidden) {
        D.name = name || "";
        if (hidden)
                D.hidden = true;
        if (base)
                D.inherits(base, name);
        function D(args) {
                if (args !== $__JSOOP) {
                        if (this === window)
                                return alert("FIXME: Constructor called without new in " + name);
                        var alist;
                        if (D.FIXARGS) {
                                if (arguments.length == 0) {
                                        args = {};
                                        alist = [ args ];
                                }
                                D.FIXARGS.apply(this, alist || arguments);
                        }
                        if (D.DEFAULT_ARGS)
                                D.setDefaults(this, args);
                        if (D.BEFORE_BASE)
                                D.BEFORE_BASE.apply(this, alist || arguments);
                        if (base)
                                base.apply(this, alist || arguments);
                        if (D.CONSTRUCT)
                                D.CONSTRUCT.apply(this, alist || arguments);
                }
        };
        if (name && !hidden)
                window[name] = D;
        var P = D.prototype;
        if (definition) {
                D.DEFINITION = definition;
                definition(D, P, DynarchDomUtils);
        }
        if (P.FINISH_OBJECT_DEF instanceof Function)
                P.FINISH_OBJECT_DEF();
        if (!P.$)
                P.$ = Object.curry2;
        return D;
};

function EXTEND_CLASS(e, t) {
    t(e, e.prototype, DynarchDomUtils)
}
function DEFINE_HIDDEN_CLASS(e, t, n) {
    return DEFINE_CLASS.call(this, e, t, n, !0)
}
function DEFINE_SINGLETON(e, t, n) {
    var r = DEFINE_HIDDEN_CLASS(e, t, n);
    return DlSingleton.register(e, r, !0), r
}
function DEFINE_EXCEPTION(e, t) {
    return DEFINE_CLASS(e, t || DlException)
}
function DlPoint(e, t) {
    typeof e == "object" ? (this.x = e.x, this.y = e.y) : (this.x = e, this.y = t)
}

function DlRect(e, t, n, r) {
    e instanceof DlRect ? this.setFromRect(e) : typeof e == "object" ? typeof t == "object" ? t instanceof DlPoint ? this.setFromPoints(e, t) : this.setFromValues(e.x, e.y, t.x, t.y) : this.setFromValues(e.x, e.y, n, r) : this.setFromValues(e, t, n, r)
}

function DlMenuBase() {
    function n(t, n, r) {
        var i = r ? r.widget : null;
        this._noClose || DlPopupMenu.clearAll(), e.applyHooks.delayed(1, e, "onSelect", [this.name, this, i])
    }
    var e, t;
    if (this._isMenuBase)
        return;
    this._isMenuBase = !0, this._items = [], e = this, t = this.appendWidget, this.appendWidget = function(e) {
        e instanceof DlMenuItem && (this._items.push(e), e.name != null && e.addEventListener("onSelect", n)), t.apply(this, Array.$(arguments))
    }, this.getItemByName = function(e) {
        return this._items.grep_first(function(t) {
            return t.name && t.name == e
        })
    }, this.getItemById = function(e) {
        return this._items.grep_first(function(t) {
            return t.__itemId && t.__itemId == e
        })
    }, this instanceof DlHbox ? this._popupAlign = {prefer: "Br",fallX1: "_r",fallX2: "_l",fallY1: "B_",fallY2: "T_"} : this._popupAlign = {prefer: "bR",fallX1: "_R",fallX2: "_L",fallY1: "b_",fallY2: "t_"}, this._getDlPopup = function() {
        var e, t = this.getParent(DlPopupMenu), n = 0;
        return t && (n = t._level + 1), e = DlPopupMenu.get(n), e.detachPopup(), t && e.attachToPopup(t), e
    }, this.getToplevelMenu = function() {
        var e = this;
        while (e.parentMenu)
            e = e.parentMenu;
        return e
    }
}
function DlType(e) {
    e && (this.name = e, DlType.TYPES[e] = this)
}
function DlConsole() {
    this._messages = [], DlConsole.INSTANCE = this
}
var CE_CACHE, $__JSOOP, $_BREAK, $_CONTINUE, DlElementCache, DlColor, DlKeyboard;
DlTEXTS = {goToday: "Go Today",_date_monthNames: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],_date_shortMonthNames: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],_date_dayNames: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],_date_shortDayNames: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"],_date_firstDayOfWeek: 1};
try {
    document.execCommand("BackgroundImageCache", !1, !0)
} catch (e) {
}
CE_CACHE = {HTML_ESCAPE_DIV: document.createElement("div"),HTML_ESCAPE_TEXT: document.createTextNode(""),CONTAINER: document.createElement("div")}, CE_CACHE.HTML_ESCAPE_DIV.appendChild(CE_CACHE.HTML_ESCAPE_TEXT), Array.$ = function(e, t) {
    var n, r, i;
    t == null && (t = 0);
    try {
        n = Array.prototype.slice.call(e, t)
    } catch (s) {
        n = Array(e.length - t);
        for (r = t, i = 0; r < e.length; ++r, ++i)
            n[i] = e[r]
    }
    return n
}, Object.merge = function(e, t) {
    var n;
    e == null && (e = {});
    if (t)
        for (n in t)
            e[n] = t[n];
    return e
}, Object.merge(Object, {mergeDefined: function(e, t) {
        var n;
        for (n in t)
            typeof t[n] != "undefined" && (e[n] = t[n]);
        return e
    },mergeUndefined: function(e, t) {
        var n;
        for (n in t)
            n in e || (e[n] = t[n]);
        return e
    },remove: function(e, t) {
        var n;
        for (n = t.length; --n >= 0; )
            delete e[t[n]]
    },isEmpty: function(e) {
        var t;
        for (t in e)
            return !1;
        return !0
    },makeCopy: function(e) {
        var t, n = {};
        for (t in e)
            n[t] = e[t];
        return n
    },makeDeepCopy: function(e) {
        var t, n, r;
        if (e instanceof Array) {
            t = [], n = e.length;
            while (--n >= 0)
                t[n] = Object.makeDeepCopy(e[n]);
            return t
        }
        if (e === null)
            return null;
        if (e instanceof Function)
            return e;
        if (e instanceof Date)
            return new Date(e);
        if (e instanceof Object) {
            r = {};
            for (n in e)
                r[n] = Object.makeDeepCopy(e[n]);
            return r
        }
        return e
    },makeShortcuts: function(e, t) {
        var n;
        for (n in t)
            e[n] = e[t[n]]
    },foreach: function(e, t, n) {
        var r;
        for (r in e)
            try {
                t.call(n, e[r], r)
            } catch (i) {
                if (i === $_BREAK)
                    break;
                if (i === $_CONTINUE)
                    continue;
                if (i instanceof $_RETURN)
                    return i.args;
                throw i
            }
    },map: function(e, t, n) {
        var r, i = [];
        for (r in e)
            if (e.hasOwnProperty(r))
                try {
                    i.push(t.call(n, e[r], r))
                } catch (s) {
                    if (s === $_BREAK)
                        break;
                    if (s === $_CONTINUE)
                        continue;
                    if (s instanceof $_RETURN)
                        return s.args;
                    throw s
                }
        return i
    },curry2: function(e) {
        return e instanceof Function || (e = this[e]), e.$A(this, Array.$(arguments, 1))
    },HOP: function(e, t) {
        return Object.prototype.hasOwnProperty.call(e, t)
    },clear: function(e) {
        var t;
        for (t in e)
            Object.HOP(e, t) && delete e[t]
    }}), Object.merge(Function, {INHERITANCE: {},getInheritanceGraph: function() {
        return Function.INHERITANCE
    },noop: function() {
    },identity: function(e) {
        return e
    },returnTrue: function() {
        return !0
    },returnFalse: function() {
        return !1
    },returnThis: function() {
        return this
    },invoke: function(e) {
        return e()
    }}), $__JSOOP = new Function.noop, Object.merge(Function.prototype, {$: Function.prototype.closure = function(e) {
        var t = Array.$(arguments, 1), n = this;
        return e == window.undefined ? function() {
            return n.apply(this, t.concat(Array.$(arguments)))
        } : function() {
            return n.apply(e, t.concat(Array.$(arguments)))
        }
    },$0: function(e) {
        var t = this, n = Array.$(arguments, 1);
        return function() {
            return t.apply(e, n)
        }
    },inherits: function(e, t) {
        var n = this.prototype = new e($__JSOOP);
        return n.constructor = this, this.BASE = e.prototype, Function.INHERITANCE[this.name = this._objectType = n._objectType = t || Dynarch.getFunctionName(this)] = Dynarch.getFunctionName(e), this.BASE
    },setDefaults: function(e, t, n) {
        return Dynarch.setDefaults.call(e, this.DEFAULT_ARGS, t, n)
    },$$: function(e) {
        return this.$.apply(this, e)
    },$A: function(e, t) {
        return this.$.apply(this, [e].concat(t))
    },$C: function() {
        var e = Array.$(arguments), t = this;
        return function() {
            return t.apply(null, e.concat(Array.$(arguments)))
        }
    },inverse: function() {
        var e = this;
        return function() {
            return !e.apply(this, arguments)
        }
    },clearingTimeout: function(e, t) {
        var n = null, r = this, i = Array.$(arguments, 2), s = function() {
            n && clearTimeout(n), n = setTimeout(r.$A(t == null ? this : t, i.concat(Array.$(arguments))), e)
        };
        return s.cancel = function() {
            clearTimeout(n)
        }, s.doItNow = function() {
            clearTimeout(n), r.apply(t, i.concat(Array.$(arguments)))
        }, s
    },rarify: function(e, t) {
        var n = this.$$(Array.$(arguments, 2)), r = this.clearingTimeout.apply(this, Array.$(arguments, 1)), i = e, s = null, o = function() {
            i = e
        };
        return function() {
            return s && clearTimeout(s), s = setTimeout(o, t), i-- > 0 ? n.apply(this, arguments) : r.apply(this, arguments)
        }
    },delayed: function(e) {
        var t = arguments.length > 1 ? this.$$(Array.$(arguments, 1)) : this;
        return setTimeout(t, e)
    },setInterval: function(e) {
        var t = arguments.length > 1 ? this.$$(Array.$(arguments, 1)) : this;
        return setTimeout(t, 0), setInterval(t, e)
    },inject: function(e) {
        return e == null && (e = this.OBJECT_EXTENSIONS), Object.merge(this.prototype, e), this
    },memoize: function() {
        var e = this, t = $__JSOOP;
        return function() {
            return t === $__JSOOP && (t = e.apply(this, arguments)), t
        }
    }}), Object.merge(Array, {hashKeys: function(e) {
        var t, n = [], r = 0;
        for (t in e)
            e.hasOwnProperty(t) && (n[r++] = t);
        return n
    },hashValues: function(e) {
        var t, n = [], r = 0;
        for (t in e)
            e.hasOwnProperty(t) && (n[r++] = e[t]);
        return n
    }}), $_BREAK = {}, $_CONTINUE = {}, Array.inject({map_hash: function(e, t, n) {
        return n || (n = {}), this.foreach(e instanceof Function ? function(r) {
            n[r] = e.call(t, r)
        } : function(t) {
            n[t] = e[t]
        }), n
    },accumulate: function(e, t) {
        var n;
        arguments.length < 2 && (t = 0);
        for (n = 0; n < this.length; ++n)
            t = e(this[n], t, n);
        return t
    },foreach: function(e, t) {
        var n, r;
        t == null && (t = this), n = 0, r = this.length;
        while (r-- > 0)
            try {
                e.call(t, this[n], n++)
            } catch (i) {
                if (i === $_BREAK)
                    break;
                if (i === $_CONTINUE)
                    continue;
                if (i instanceof $_RETURN)
                    return i.args;
                throw i
            }
    },r_foreach: function(e, t) {
        var n;
        t == null && (t = this);
        for (n = this.length; --n >= 0; )
            try {
                e.call(t, this[n], n)
            } catch (r) {
                if (r === $_BREAK)
                    break;
                if (r === $_CONTINUE)
                    continue;
                if (r instanceof $_RETURN)
                    return r.args;
                throw r
            }
    },assign_each: function(e, t) {
        return this.foreach(function(n, r) {
            this[r] = e.call(t, r, n)
        })
    },r_assign_each: function(e, t) {
        return this.r_foreach(function(n, r) {
            this[r] = e.call(t, r, n)
        })
    },toHash: function(e, t) {
        var n = {};
        return e instanceof Function ? this.foreach(function(r, i) {
            n[r] = e.call(t, r, i)
        }) : this.foreach(function(t, r) {
            n[t] = e != null ? e : r + 1
        }), n
    },toHash2: function() {
        var e = {}, t = 0;
        while (t < this.length)
            e[this[t++]] = this[t++];
        return e
    },toHash3: function(e, t) {
        var n = {};
        return e instanceof Function ? this.foreach(function(r, i) {
            var s = e.call(t != null ? t : r, r, i);
            n[s[0]] = s[1]
        }) : this.foreach(function(t) {
            n[t[e]] = t
        }), n
    },map: function(e, t) {
        var n, r, i = 0, s = this.length, o = [];
        if (e instanceof Function) {
            t == null && (t = this);
            while (s-- > 0)
                try {
                    o.push(e.call(t, this[i], i++))
                } catch (u) {
                    if (u === $_BREAK)
                        break;
                    if (u === $_CONTINUE)
                        continue;
                    if (u instanceof $_RETURN) {
                        o.push(u.args);
                        break
                    }
                    throw u
                }
        } else {
            n = Array.$(arguments, 1);
            while (s-- > 0)
                t = this[i], r = t[e], o[i++] = r instanceof Function ? r.apply(t, n) : r
        }
        return o
    },r_map: function(e, t) {
        var n, r, i = this.length, s = [];
        if (e instanceof Function) {
            t == null && (t = this);
            while (--i >= 0)
                try {
                    s.push(e.call(t, this[i], i))
                } catch (o) {
                    if (o === $_BREAK)
                        break;
                    if (o === $_CONTINUE)
                        continue;
                    if (o instanceof $_RETURN) {
                        s.push(o.args);
                        break
                    }
                    throw o
                }
        } else {
            r = Array.$(arguments, 1);
            while (--i >= 0)
                t = this[i], n = t[e], s[i] = n instanceof Function ? n.apply(t, r) : n
        }
        return s.reverse()
    },count: function(e, t) {
        var n = 0;
        return this.r_foreach(function(t, r) {
            e.call(this, t, r) && ++n
        }, t), n
    },keys_map: function(e) {
        return this.map(function(t) {
            return e[t]
        })
    },grep: function(e, t) {
        var n, r, i, s = 0, o = this.length, u = [];
        if (e instanceof RegExp)
            while (o-- > 0)
                n = this[s++], e.test(n) && u.push(n);
        else if (e instanceof Function) {
            t == null && (t = this);
            while (o-- > 0)
                n = this[s], e.call(t, n, s++) && u.push(n)
        } else {
            r = Array.$(arguments, 1);
            while (o-- > 0)
                t = this[s++], i = t[e], i instanceof Function ? i.apply(t, r) && u.push(t) : i && u.push(t)
        }
        return u
    },grep_last: function(e, t) {
        var n;
        t == null && (t = this.length - 1);
        while (t >= 0) {
            n = this[t--];
            if (e(n))
                return n
        }
        return null
    },grep_first: function(e, t) {
        var n;
        for (t = t || 0; t < this.length; ++t) {
            n = this[t];
            if (e(n))
                return n
        }
        return null
    },contains: function(e) {
        var t;
        for (t = this.length; --t >= 0; )
            if (this[t] === e)
                return !0;
        return !1
    },any: function(e, t) {
        var n, r, i;
        if (e instanceof Function) {
            t == null && (t = this);
            for (n = this.length; --n >= 0; )
                if (e.call(t, this[n], n))
                    return !0
        } else {
            r = Array.$(arguments, 1);
            for (n = this.length; --n >= 0; ) {
                t = this[n], i = t[e];
                if (i instanceof Function) {
                    if (i.apply(t, r))
                        return !0
                } else if (i)
                    return !0
            }
        }
        return !1
    },find: function(e) {
        var t;
        for (t = this.length; --t >= 0; )
            if (this[t] === e)
                return t;
        return -1
    },remove: function(e) {
        var t;
        for (t = this.length; --t >= 0; )
            this[t] === e && this.splice(t, 1);
        return this
    },pushUnique: function(e) {
        return this.find(e) < 0 ? (this.push(e), this.length) : null
    },peek: function(e) {
        if (this.length > 0)
            return this[this.length - 1 - (e != null ? Math.abs(e) : 0)]
    },min: function(e, t) {
        var n, r;
        if (this.length == 0)
            return null;
        if (arguments.length > 0) {
            n = e != null ? e.call(t, this[0], 0) : this[0];
            for (r = 1; r < this.length; ++r)
                n = Math.min(n, e != null ? e.call(t, this[r], r) : this[r]);
            return n
        }
        return Math.min.apply(Math, this)
    },minElement: function(e, t, n) {
        var r, i, s, o, u;
        if (this.length == 0)
            return null;
        r = 0, i = this[0], s = e.call(t, i), o = 0;
        while (++r < this.length)
            (u = e.call(t, this[r])) < s && (s = u, o = r, i = this[r]);
        return n && this.splice(o, 1), i
    },max: function(e, t) {
        var n, r;
        if (this.length == 0)
            return null;
        if (arguments.length > 0) {
            n = e != null ? e.call(t, this[0], 0) : this[0];
            for (r = 1; r < this.length; ++r)
                n = Math.max(n, e != null ? e.call(t, this[r], r) : this[r]);
            return n
        }
        return Math.max.apply(Math, this)
    },maxElement: function(e, t, n) {
        var r, i, s, o, u;
        if (this.length == 0)
            return null;
        r = 0, i = this[0], s = e.call(t, i), o = 0;
        while (++r < this.length)
            (u = e.call(t, this[r])) > s && (s = u, o = r, i = this[r]);
        return n && this.splice(o, 1), i
    },rotateIndex: function(e) {
        return Math.rotateLimit(e, 0, this.length - 1)
    },limitIndex: function(e) {
        return Math.limit(e, 0, this.length - 1)
    },nullLimitIndex: function(e) {
        return Math.nullLimit(e, 0, this.length - 1)
    },bytesToString: function() {
        var e, t = "", n = 0;
        while (n < this.length)
            e = this[n++], e & 240 ^ 240 ? e & 224 ^ 224 ? e & 192 ^ 192 || (e = (e & 31) << 6 | this[n++] & 63) : e = (e & 15) << 12 | (this[n++] & 63) << 6 | this[n++] & 63 : e = (e & 3) << 18 | (this[n++] & 63) << 12 | (this[n++] & 63) << 6 | this[n++] & 63, t += String.fromCharCode(e);
        return t
    },repeat: function(e) {
        var t;
        return e == 0 ? [] : e == 1 ? this : (t = this.repeat(e >> 1), t = t.concat(t), e & 1 && (t = t.concat(this)), t)
    },common_prefix: function() {
        var e, t, n, r;
        switch (this.length) {
            case 0:
                return "";
            case 1:
                return this[0];
            case 2:
                e = this[0], t = this[1], n = Math.min(e.length, t.length), r = 0;
                while (r < n && e.charAt(r) === t.charAt(r))
                    ++r;
                return e.substring(0, r);
            default:
                return [this[0], this.slice(1).common_prefix()].common_prefix()
        }
    },append: function(e) {
        this.push.apply(this, e)
    },prepend: function(e) {
        this.unshift.apply(this, e)
    },toXML: function() {
        var e, t, n, r = this[0];
        if (r == "~literal")
            return this.slice(1).flatJoin();
        e = "<" + r, t = 1, n = this[1], typeof n == "object" && (Object.foreach(n, function(t, n) {
            n.charAt(0) == "$" && (n = n.substr(1)), e += " " + n.htmlEscape() + '="', typeof t == "object" ? e += Object.map(t, function(e, t) {
                return t = t.replace(/([a-z]?)([A-Z])/g, function(e, t, n) {
                    return t + "-" + n.toLowerCase()
                }), t.htmlEscape() + ": " + e.htmlEscape()
            }).join("; ") : e += t.htmlEscape(), e += '"'
        }), ++t), e += ">";
        while (t < this.length)
            n = this[t++], n instanceof Array ? e += n.toXML() : e += (n + "").htmlEscape();
        return e + "</" + r + ">"
    },swap: function(e, t) {
        var n = this[e];
        return this[e] = this[t], this[t] = n, this
    }}), Number.inject({map: function(e, t) {
        return e + (t - e) * this
    },reduce: function(e, t) {
        return (this - e) / (t - e)
    },mapInt: function(e, t) {
        return Math.round(this.map(e, t))
    },reduceInt: function(e, t) {
        return Math.round((this - e) / (t - e))
    },bits1Array: function() {
        var e = this, t = [], n = 1, r = 0;
        while (e > 0)
            e & 1 && (t[r++] = n), n <<= 1, e >>= 1;
        return t
    },times: function(e, t) {
        var n = this, r = 0;
        while (--n >= 0)
            e.call(t, r++, n)
    },hex: function(e) {
        var t = this.toString(16).toUpperCase();
        if (e)
            while (t.length < e)
                t = "0" + t;
        return t
    },zeroPad: function(e, t) {
        var n = "" + Math.round(this);
        t == null && (t = "0");
        while (n.length < e)
            n = t + n;
        return n
    },formatTime: function() {
        var e, t = this, n = t / 60;
        return t %= 60, e = n / 60, n %= 60, [e, n, t].map("zeroPad", 2).join(":")
    },toDate: function(e, t, n, r) {
        return Date.intToDate(this, e, t, n, r)
    },limit: function(e, t) {
        return Math.limit(this, e, t)
    },rotateLimit: function(e, t) {
        return Math.rotateLimit(this, e, t)
    },nullLimit: function(e, t) {
        return Math.nullLimit(this, e, t)
    },i18n: function(e) {
        var t = this;
        return arguments.length > 1 ? e = Array.$(arguments) : e = e.trim().split(/\s*\|\s*/), e = t < e.length ? e[t] : e[e.length - 1], e.replace(/##?/g, function(e) {
            return e.length == 2 ? "#" : t
        })
    }}), Object.merge(Math, {nullLimit: function(e, t, n) {
        return e < t && (e = null), e > n && (e = null), e
    },limit: function(e, t, n) {
        return e < t && (e = t), e > n && (e = n), e + 0
    },rotateLimit: function(e, t, n) {
        return n++, e %= n - t, e < 0 ? e = n + e : e < t && (e = t - e), e + 0
    }}), Object.merge(Date, {_MD: [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],SECOND: 1e3,MINUTE: 6e4,HOUR: 36e5,DAY: 864e5,WEEK: 6048e5,_MN: DlTEXTS._date_monthNames,_SMN: DlTEXTS._date_shortMonthNames,_DN: DlTEXTS._date_dayNames,_SDN: DlTEXTS._date_shortDayNames,_FDOW: DlTEXTS._date_firstDayOfWeek,isWeekend: function(e) {
        return e == 0 || e == 6
    },parseMySQL: function(e, t) {
        var n = e.split(/\s+/), r = n[0].split(/-/), i = n[1].split(/:/), s = new Date(r[0], r[1] - 1, r[2], i[0] || null, i[1] || null, i[2] || null);
        return t && (s.setUTCMilliseconds(0), s.setUTCSeconds(i[2] || 0), s.setUTCMinutes(i[1] || 0), s.setUTCHours(i[0] || 0), s.setUTCDate(1), s.setUTCMonth(r[1] - 1), s.setUTCDate(r[2]), s.setUTCFullYear(r[0])), s
    },dateToInt: function(e) {
        return e instanceof Date ? 1e4 * e.getFullYear() + 100 * (e.getMonth() + 1) + e.getDate() : typeof e == "string" ? parseInt(e, 10) : e
    },intToDate: function(e, t, n, r, i) {
        var s, o;
        return e instanceof Date || (e = parseInt(e, 10), s = Math.floor(e / 1e4), e %= 1e4, o = Math.floor(e / 100), e %= 100, e = new Date(s, o - 1, e, t || 12, n || 0, r || 0, i || 0)), e
    },getMonthName: function(e, t) {
        var n = t ? Date._SMN : Date._MN;
        return n[e % 12]
    },getFirstDayOfWeek: function() {
        return Date._FDOW
    },getDayName: function(e, t) {
        var n = t ? Date._SDN : Date._DN;
        return n[e % 7]
    }}), Date.now || (Date.now = function() {
    return (new Date).getTime()
}), Date.inject({toInt: function() {
        return Date.dateToInt(this)
    },getMonthDays: function(e) {
        var t = this.getFullYear();
        return e == null && (e = this.getMonth()), 0 != t % 4 || 0 == t % 100 && 0 != t % 400 || e != 1 ? Date._MD[e] : 29
    },getDayOfYear: function() {
        var e = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0), t = new Date(this.getFullYear(), 0, 0, 0, 0, 0), n = e - t;
        return Math.floor(n / Date.DAY)
    },getWeekNumber: function() {
        var e, t = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0), n = t.getDay();
        return t.setDate(t.getDate() - (n + 6) % 7 + 3), e = t.valueOf(), t.setMonth(0), t.setDate(4), Math.round((e - t.valueOf()) / 6048e5) + 1
    },dateEqualsTo: function(e, t) {
        return this.getFullYear() == e.getFullYear() && this.getMonth() == e.getMonth() && (t || this.getDate() == e.getDate())
    },print: function(e) {
        var t, n, r, i, s, o, u = this.getMonth(), a = this.getDate(), f = this.getFullYear(), l = this.getWeekNumber(), c = this.getDay(), h = {}, p = this.getHours(), d = p >= 12, v = d ? p - 12 : p, m = this.getDayOfYear();
        return v == 0 && (v = 12), t = this.getMinutes(), n = this.getSeconds(), h["%a"] = Date.getDayName(c, !0), h["%A"] = Date.getDayName(c), h["%b"] = Date.getMonthName(u, !0), h["%B"] = Date.getMonthName(u), h["%C"] = 1 + Math.floor(f / 100), h["%d"] = a < 10 ? "0" + a : a, h["%e"] = a, h["%H"] = p < 10 ? "0" + p : p, h["%I"] = v < 10 ? "0" + v : v, h["%j"] = m < 100 ? m < 10 ? "00" + m : "0" + m : m, h["%k"] = p, h["%l"] = v, h["%m"] = u < 9 ? "0" + (1 + u) : 1 + u, h["%M"] = t < 10 ? "0" + t : t, h["%n"] = "\n", h["%p"] = d ? "PM" : "AM", h["%P"] = d ? "pm" : "am", h["%s"] = Math.floor(this.getTime() / 1e3), h["%S"] = n < 10 ? "0" + n : n, h["%t"] = "	", h["%U"] = h["%W"] = h["%V"] = l < 10 ? "0" + l : l, h["%u"] = c + 1, h["%w"] = c, h["%y"] = ("" + f).substr(2, 2), h["%Y"] = f, h["%%"] = "%", r = /%./g, e.replace(r, function(e) {
            return h[e] || e
        })
    }}), String.inject({breakable: function(e) {
        return e || (e = /([_.-])/g), this.replace(e, "$1<span class='BreakPoint'> </span>")
    },printf: function() {
        var e = Array.$(arguments), t = 0;
        return this.replace(/%[sdfo%]/g, function(n) {
            var r;
            n = n.charAt(1), r = e[t++];
            switch (n) {
                case "s":
                    return r + "";
                case "d":
                    return parseInt(r);
                case "f":
                    return parseFloat(r).toFixed(3);
                case "o":
                    return r;
                case "%":
                    return "%"
            }
            return "undefined"
        })
    },fixedWidth: function(e) {
        return String.buffer("<div style='width:", e, "'>", this, "</div>").get()
    },noWrap: function() {
        return this.replace(/\x20/g, "&nbsp;")
    },lastIndexOfRegexp: function(e, t) {
        var n, r = 0;
        e.lastIndex = 0, e.global = !0;
        while (n = e.exec(this)) {
            if (e.lastIndex >= t)
                break;
            r = e.lastIndex
        }
        return r
    },hashWords: function(e) {
        return this.trim().split(/\s+/).toHash(arguments.length > 0 ? e : !0)
    },arrayWords: function() {
        return this.trim().split(/\s+/)
    },trim: function(e, t) {
        var n = e ? this : this.replace(/^\s+/, "");
        return t || (n = n.replace(/\s+$/, "")), n
    },htmlEscapeFull: function() {
        return this.replace(/&/g, "&amp;").replace(/\x22/g, "&quot;").replace(/\x27/g, "&#x27;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\u00A0/g, "&#xa0;")
    },decodeJSON: function(e) {
        return DlJSON.decode(this, e)
    },makeLabel: function() {
        return this.replace(/\s+/g, "&nbsp;")
    },capitalizeString: function() {
        return this.charAt(0).toUpperCase() + this.substr(1)
    },htmlEmbed: function(e, t) {
        var n = ["<", e];
        return t != null && n.push(" class='", t, "'"), n.push(">", this, "</", e, ">"), n.join("")
    },repeat: function(e) {
        var t;
        return e == 0 ? "" : e == 1 ? "" + this : (t = this.repeat(e >> 1), t += t, e & 1 && (t += this), t)
    },hexToBytes: function(e) {
        var t = [], n = 0, r = this;
        e && (r = r.replace(/[^0-9a-f]/ig, "")), r.length & 1 && (r = "0" + r);
        while (n < r.length)
            t.push(parseInt(r.substr(n, 2), 16)), n++, n++;
        return t
    },toBytes: function() {
        var e, t = this.length, n = 0, r = 0, i = [];
        while (--t >= 0)
            e = this.charCodeAt(r++), e < 128 ? i[n++] = e : e < 2048 ? (i[n++] = 192 | e >>> 6 & 31, i[n++] = 128 | e & 63) : e < 65536 ? (i[n++] = 224 | e >>> 12 & 15, i[n++] = 128 | e >>> 6 & 63, i[n++] = 128 | e & 63) : e < 1114112 && (i[n++] = 240 | e >>> 18 & 3, i[n++] = 128 | e >>> 12 & 63, i[n++] = 128 | e >>> 6 & 63, i[n++] = 128 | e & 63);
        return i
    }}), function() {
    function f(e, t) {
        return t ? e instanceof Function ? function(t, n) {
            return e(n, t)
        } : function(e, t) {
            return t < e ? -1 : t > e ? 1 : 0
        } : e instanceof Function ? e : function(e, t) {
            return e < t ? -1 : e > t ? 1 : 0
        }
    }
    function l(e) {
        e instanceof Array ? e.foreach(l, this) : e instanceof Function ? (e = e(), e != null && e != 0 && l.call(this, e)) : this.push(e)
    }
    function c(e) {
        return e instanceof Array ? e.accumulate(function(e, t) {
            return t + c(e)
        }, "") : e instanceof Function ? c(e()) : e === !1 || e == null ? "" : e + ""
    }
    var e, t, n, r, i, s, o, u, a = navigator.userAgent;
    is_opera = /opera/i.test(a), is_ie = /msie/i.test(a) && !is_opera && !/mac_powerpc/i.test(a), is_ie5 = is_ie && /msie 5\.[^5]/i.test(a), is_ie6 = is_ie && /msie 6/i.test(a), is_ie7 = is_ie && /msie 7/i.test(a), is_ie8 = is_ie && /msie 8/i.test(a), ie_box_model = is_ie && document.compatMode && document.compatMode == "BackCompat", is_mac_ie = /msie.*mac/i.test(a), is_khtml = /Konqueror|Safari|KHTML/i.test(a), is_safari = /Safari/i.test(a), is_safari3 = is_safari && /Version\/3/i.test(a), is_konqueror = is_khtml && !is_safari3, is_gecko = /gecko/i.test(a) && !is_khtml && !is_opera && !is_ie, is_chrome = /Chrome/i.test(a), is_w3 = !is_ie, is_macintosh = /Macintosh/i.test(a), is_gecko && /rv:\s*([0-9.]+)/.test(a) && (gecko_version = parseFloat(RegExp.$1)), e = Array.prototype, t = Date.prototype, n = String.prototype, r = Number.prototype, e.mergeSort = function(e, t) {
        function r(e, t) {
            var r = [], i = 0, s = 0, o = 0;
            while (i < e.length && s < t.length)
                n(e[i], t[s]) > 0 ? r[o++] = t[s++] : r[o++] = e[i++];
            return i < e.length && r.push.apply(r, e.slice(i)), s < t.length && r.push.apply(r, t.slice(s)), r
        }
        function i(e) {
            var t, n, s;
            return e.length > 1 ? (t = Math.floor(e.length / 2), n = e.slice(0, t), s = e.slice(t), n = i(n), s = i(s), r(n, s)) : e
        }
        var n;
        return this.length < 2 ? Array.$(this) : (n = f(e, t), i(this))
    }, e.qsort = function(e, t) {
        function o(e, t) {
            var u = e, a = t, f = !1;
            if (u < a) {
                do
                    n(r[u], r[a]) > 0 && (i = r[u], r[u] = r[a], r[a] = i, f = !f, s = !0), f ? --a : ++u;
                while (u < a);
                o(e, u - 1), o(u + 1, t)
            }
        }
        var n, r, i, s;
        if (this.length < 2)
            return;
        return n = f(e, t), r = this, s = !1, o(0, this.length - 1), s
    }, e.x = e.repeat, e.flatJoin = function() {
        return c(this)
    }, e.flatten = function() {
        var e = [];
        return l.call(e, this), e
    }, i = r.$1K = 1024, s = r.$1M = i * 1024, o = r.$1G = s * 1024, u = r.$1T = o * 1024, r.formatBytes = function(e) {
        var t, n, r = this;
        return r < i ? t = "B" : r < s ? (r /= i, t = "K") : r < o ? (r /= s, t = "M") : r < u && (r /= o, t = "G"), n = Math.round(r), e && r != n ? r.toFixed(e) + t : n + t
    }, n.qw = n.arrayWords, n.bold = n.htmlEmbed.$(window.undefined, "b"), n.x = n.repeat
}(), Object.merge(String, {firstNonEmpty: function() {
        var e, t;
        for (e = 0; e < arguments.length; ++e) {
            t = arguments[e];
            if (/\S/.test(t))
                return t
        }
    },template: function() {
        var format = String.buffer.apply(this, arguments).get();
        return function(props) {
            return format.replace(/(.?)\$(\{.*?\}|[a-zA-Z0-9_]+)/g, function(s, p1, p2) {
                return p1.charAt(0) == "\\" ? s.substr(1) : (p2.charAt(0) == "{" && (p2 = p2.substr(1, p2.length - 2)), eval("p2 = props." + p2), p1 + p2)
            })
        }
    },buffer: is_ie || is_khtml ? function() {
        var e = [], t = 0, n = function() {
            for (var r = 0; r < arguments.length; ++r)
                e[t++] = arguments[r];
            return n
        };
        return n.get = function() {
            return e = [e.join("")], t = 1, e[0]
        }, arguments.length > 0 && n.apply(this, arguments), n
    } : function() {
        var e = "", t = function() {
            return e = e.concat.apply(e, arguments), t
        };
        return arguments.length > 0 && t.apply(this, arguments), t.get = function() {
            return e
        }, t
    }}), String.prototype.htmlEscape = is_gecko ? function() {
    return this.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\u00A0/g, "&#xa0;")
} : function() {
    return CE_CACHE.HTML_ESCAPE_TEXT.data = this, CE_CACHE.HTML_ESCAPE_DIV.innerHTML
}, window.Dynarch = {dblClickTimeout: 400,setDefaults: function(e, t, n) {
        var r, i, s;
        t || (t = {});
        for (r in e)
            if (n || !(r in this))
                s = e[r], s instanceof Array ? s[0] != null ? (i = s[0], i in t ? i = t[i] : i = s[1]) : i = s[1] : i = s, this[r] = i
    },__IDS: {},ID: function(e) {
        var t = Dynarch.__IDS;
        return e == null && (e = "generic"), e in t || (t[e] = 0), "dynarch-" + e + "-" + ++t[e]
    },getFunctionName: function(e) {
        return e.name != null ? e.name : /function\s+(\$?[a-z0-9_]+)\(/i.test(e + "") ? RegExp.$1 : "UNKNOWN_FUNCTION"
    },EXPORT: function(e, t) {
        var n = String.buffer("var D=window.", e, "=", e, ",P=", e, ".prototype;");
        return t && n(DynarchDomUtils.importCommonVars()), n.get()
    },getBaseURL: function() {
        var e, t, n, r = window.Dynarch_Base_Url;
        if (!r) {
            e = document.getElementsByTagName("script"), t = 0;
            while (n = e[t++])
                if (n.className == "DynarchLIB") {
                    r = n.src;
                    if (/^(.*)\x2fjs\x2f/.test(r)) {
                        Dynarch_Base_Url = r = RegExp.$1;
                        break
                    }
                }
        }
        return r
    },getFileURL: function(e) {
        return Dynarch.getBaseURL() + "/" + e
    },firebugRunning: function() {
        return window.console && window.console.firebug
    },evalClean: function(code) {
        return Dynarch.firebugRunning() ? Function("return (" + code + ");")() : eval("(" + code + ")")
    }}, window.DynarchDomUtils = {ID: Dynarch.ID,related: function(e, t) {
        var n, r;
        is_ie ? (r = t.type, r == "mouseover" ? n = t.fromElement : r == "mouseout" && (n = t.toElement)) : n = t.relatedTarget;
        if (is_gecko && n)
            try {
                n.parentNode
            } catch (i) {
                try {
                    n = t.parentNode
                } catch (i) {
                    n = t.target
                }
            }
        try {
            for (; n; n = n.parentNode)
                if (n === e)
                    return !0
        } catch (i) {
            return !0
        }
        return !1
    },getScrollbarSize: function(e) {
        return {x: e.offsetWidth - e.clientWidth,y: e.offsetHeight - e.clientHeight}
    },addEvent: function(e, t, n) {
        var r;
        if (typeof t == "string")
            e.addEventListener ? e.addEventListener(t, n, !1) : e.attachEvent ? e.attachEvent("on" + t, n) : e["on" + t] = n;
        else if (t instanceof Array)
            DynarchDomUtils.addEvents(e, t, n);
        else
            for (r in t)
                DynarchDomUtils.addEvent(e, r, t[r])
    },addEvents: function(e, t, n) {
        var r;
        for (r = t.length; --r >= 0; )
            DynarchDomUtils.addEvent(e, t[r], n)
    },removeEvent: function(e, t, n) {
        var r;
        if (typeof t == "string")
            e.removeEventListener ? e.removeEventListener(t, n, !1) : e.detachEvent ? e.detachEvent("on" + t, n) : e["on" + t] = "";
        else if (t instanceof Array)
            DynarchDomUtils.removeEvents(e, t, n);
        else
            for (r in t)
                DynarchDomUtils.removeEvent(e, r, t[r])
    },removeEvents: function(e, t, n) {
        var r;
        for (r = t.length; --r >= 0; )
            DynarchDomUtils.removeEvent(e, t[r], n)
    },condEvent: function(e) {
        return e = e ? DynarchDomUtils.addEvent : DynarchDomUtils.removeEvent, e.apply(DynarchDomUtils, Array.$(arguments, 1))
    },condEvents: function(e) {
        return e = e ? DynarchDomUtils.addEvents : DynarchDomUtils.removeEvents, e.apply(DynarchDomUtils, Array.$(arguments, 1))
    },stopEvent: function(e) {
        return is_ie ? (e.cancelBubble = !0, e.returnValue = !1) : (e.preventDefault(), e.stopPropagation()), !1
    },addLoadHandler: function(e, t) {
        is_ie ? e.onreadystatechange = function() {
            if (e.readyState == 4) {
                try {
                    e.onreadystatechange = null
                } catch (n) {
                }
                t()
            }
        } : DynarchDomUtils.addEvent(e, "load", t)
    },callHandler: function(e, t) {
        if (e[t] instanceof Function)
            return e[t].call(e);
        if (typeof e[t] == "string")
            return Dynarch.evalClean(e[t])
    },setStyleProperty: function(e, t, n) {
        switch (t) {
            case "float":
                t = "styleFloat";
                break;
            default:
                t = t.toLowerCase().replace(/-([a-z])/g, function(e, t) {
                    return t.toUpperCase()
                })
        }
        e.style[t] = n
    },setOpacity: function(e, t) {
        if (t != null)
            return t == "" && t != 0 ? is_ie ? e.style.filter = "" : e.style.opacity = "" : is_ie ? e.style.filter = "alpha(opacity=" + Math.round(t * 100) + ")" : e.style.opacity = t, t;
        if (!is_ie)
            return parseFloat(e.style.opacity);
        if (/alpha\(opacity=([0-9.])+\)/.test(e.style.opacity))
            return parseFloat(RegExp.$1)
    },getClosestParentByTagName: function(e, t) {
        t = t.toLowerCase();
        while (e && e.tagName && e.tagName.toLowerCase() != t)
            e = e.parentNode;
        return e
    },isInside: function(e, t) {
        try {
            while (e) {
                if (e === t)
                    return !0;
                e = e.parentNode
            }
        } catch (n) {
        }
        return !1
    },getWindowSize: function() {
        var e, t;
        return is_gecko ? document.documentElement.clientWidth ? {x: document.documentElement.clientWidth,y: document.documentElement.clientHeight} : {x: window.innerWidth,y: window.innerHeight} : is_opera ? {x: window.innerWidth,y: window.innerHeight} : is_ie ? !document.compatMode || document.compatMode == "BackCompat" ? {x: document.body.clientWidth,y: document.body.clientHeight} : {x: document.documentElement.clientWidth,y: document.documentElement.clientHeight} : (e = document.createElement("div"), t = e.style, t.position = "absolute", t.bottom = t.right = "0px", document.body.appendChild(e), t = {x: e.offsetLeft,y: e.offsetTop}, document.body.removeChild(e), t)
    },getPos: function(e) {
        var t, n, r, i, s, o, u;
        if (e.getBoundingClientRect)
            return t = e.getBoundingClientRect(), {x: t.left - document.documentElement.clientLeft,y: t.top - document.documentElement.clientTop};
        if (document.getBoxObjectFor) {
            t = e.ownerDocument.getBoxObjectFor(e), n = {x: t.x,y: t.y};
            while (e.parentNode && e.parentNode !== document.body)
                e = e.parentNode, n.x -= e.scrollLeft, n.y -= e.scrollTop;
            return n
        }
        return /^body$/i.test(e.tagName) ? {x: 0,y: 0} : (r = 0, i = 0, s = /^div$/i.test(e.tagName), s && e.scrollLeft && (r = e.scrollLeft), s && e.scrollTop && (i = e.scrollTop), o = {x: e.offsetLeft - r,y: e.offsetTop - i}, e.offsetParent && (u = DynarchDomUtils.getPos(e.offsetParent), o.x += u.x, o.y += u.y), o)
    },getBRPos: function(e) {
        var t = DynarchDomUtils.getPos(e), n = DynarchDomUtils.getOuterSize(e);
        return t.x += n.x - 1, t.y += n.y - 1, t
    },setPos: function(e, t, n) {
        typeof t == "number" && (t += "px"), typeof n == "number" && (n += "px"), t != null && (e.style.left = t), n != null && (e.style.top = n)
    },createElement: function(e, t, n, r, i) {
        var s, o = CE_CACHE[e] || (CE_CACHE[e] = document.createElement(e));
        o = o.cloneNode(!1);
        if (t)
            for (s in t)
                is_ie ? DynarchDomUtils.setStyleProperty(o, s, t[s]) : o.style.setProperty(s, t[s], "");
        if (n)
            for (s in n)
                o[s] = n[s];
        return r && (typeof i == "number" && (i = r.childNodes[i]), i || (i = null), r.insertBefore(o, i)), o
    },setUnselectable: function(e, t) {
        var n;
        t == null && (t = !0), is_ie ? (t = t ? "on" : "off", n = Array.$(e.getElementsByTagName("*")), n.push(e), n.foreach(function(e) {
            e.setAttribute("unselectable", t)
        })) : (t = t ? "none" : "normal", e.style.MozUserSelect = t, e.style.WebkitUserSelect = t, e.style.userSelect = t)
    },addClass: function(e, t, n) {
        DynarchDomUtils.delClass(e, n, t)
    },delClass: function(e, t, n) {
        var r, i, s, o;
        if (e) {
            r = e.className, t instanceof RegExp && (r = r.replace(t, " "), t = null);
            if (n || t) {
                i = r.split(/\s+/), s = i.length, o = {}, t && (o[t] = 1), n && (o[n] = 1);
                while (--s >= 0)
                    i[s] in o && i.splice(s, 1);
                n && i.push(n), r = i.join(" ")
            }
            e.className = r
        }
    },condClass: function(e, t, n, r) {
        DynarchDomUtils[t ? "addClass" : "delClass"](e, n, r)
    },hasClass: function(e, t) {
        return e.className.split(" ").contains(t)
    },elementIsVisible: function(e) {
        return !!e.offsetWidth && e.style.visibility != "hidden"
    },ie_getBackgroundColor: function(e) {
        var t = document.body.createTextRange();
        return t.moveToElementText(e), "#" + parseInt(t.queryCommandValue("BackColor")).hex(6)
    },getStyle: function(e, t) {
        var n = null;
        return window.getComputedStyle ? n = document.defaultView.getComputedStyle(e, "").getPropertyValue(t) : e.currentStyle && (t = t.replace(/-[a-z]/g, function(e) {
            return e.charAt(1).toUpperCase()
        }), t == "backgroundColor" ? n = ie_getBackgroundColor(e) : n = e.currentStyle[t]), n
    },getStylePX: function(e, t) {
        var n = parseInt(DynarchDomUtils.getStyle(e, t), 10);
        return isNaN(n) && (n = 0), n
    },getBorder: function(e) {
        return {x: e.offsetWidth - e.clientWidth,y: e.offsetHeight - e.clientHeight}
    },getPadding: function(e) {
        var t = DynarchDomUtils.getStylePX, n = t(e, "padding-left") + t(e, "padding-right"), r = t(e, "padding-top") + t(e, "padding-bottom");
        return {x: n,y: r}
    },getPaddingAndBorder: function(e) {
        var t = 0, n = 0, r = DynarchDomUtils.getStylePX;
        return t += r(e, "border-left-width"), t += r(e, "border-right-width"), n += r(e, "border-top-width"), n += r(e, "border-bottom-width"), t += r(e, "padding-left"), t += r(e, "padding-right"), n += r(e, "padding-top"), n += r(e, "padding-bottom"), {x: t,y: n}
    },getSelectionRange: function(e) {
        var t, n, r, i, s;
        return is_ie ? (r = document.selection.createRange(), i = r.compareEndPoints("StartToEnd", r) == 0, i || r.collapse(!0), s = r.getBookmark(), t = s.charCodeAt(2) - 2, r = document.selection.createRange(), i = r.compareEndPoints("StartToEnd", r) == 0, i || r.collapse(!1), s = r.getBookmark(), n = s.charCodeAt(2) - 2) : (t = e.selectionStart, n = e.selectionEnd), {start: t,end: n}
    },setSelectionRange: function(e, t, n) {
        var r, i;
        n == null && (n = t), t > n && (r = t, t = n, n = r), typeof t == "object" && (n = t.end, t = t.start), is_ie ? (i = e.createTextRange(), i.collapse(!0), i.moveStart("character", t), i.moveEnd("character", n - t), i.select()) : e.setSelectionRange(t, n)
    },setOuterSize: function(e, t, n) {
        var r = DynarchDomUtils.getPaddingAndBorder(e);
        t != null && r.x != NaN && (t -= r.x), n != null && r.y != NaN && (n -= r.y), DynarchDomUtils.setInnerSize(e, t, n)
    },setInnerSize: function(e, t, n) {
        try {
            typeof t == "number" && t != NaN && (t = Math.abs(t) + "px"), typeof n == "number" && n != NaN && (n = Math.abs(n) + "px"), t != null && t != NaN && (!is_ie || t > 0) && (e.style.width = t), n != null && n != NaN && (!is_ie || n > 0) && (e.style.height = n)
        } catch (r) {
        }
    },getOuterSize: function(e) {
        return {x: e.offsetWidth,y: e.offsetHeight}
    },getInnerSize: function(e) {
        var t = DynarchDomUtils.getOuterSize(e), n = DynarchDomUtils.getPaddingAndBorder(e);
        return t.x -= n.x, t.y -= n.y, t
    },importCommonVars: function() {
        return ["var DOM=DynarchDomUtils", "AC=DOM.addClass", "DC=DOM.delClass", "CC=DOM.condClass", "CE=DOM.createElement", "ID=Dynarch.ID"].join(",")
    },trash: function(e, t) {
        e && (t = e.parentNode) && t.removeChild(e)
    },strip: function(e) {
        var t = e.parentNode;
        while (e.firstChild)
            t.insertBefore(e.firstChild, e);
        DynarchDomUtils.trash(e)
    },createFromHtml: function(e) {
        var t = CE_CACHE.CONTAINER;
        return t.innerHTML = e, t.firstChild
    },swapNodes: function(e, t) {
        var n = e.parentNode, r = e.nextSibling;
        t.parentNode.replaceChild(e, t), n.insertBefore(t, r)
    },scrollIntoView: function(e) {
        var t, n, r, i, s, o, u = e.parentNode;
        while (u && (u.scrollHeight == u.clientHeight && u.scrollWidth == u.clientWidth || /table|tbody/i.test(u.tagName)))
            u = u.parentNode;
        if (u && u !== document.body) {
            t = [], n = u;
            while (n)
                t.push(n), n = n.parentNode;
            r = 0, i = 0, n = e;
            while (n && n != u) {
                r += n.offsetTop, i += n.offsetLeft, n = n.offsetParent;
                if (t.contains(n)) {
                    n != u && (r -= u.offsetTop, i -= u.offsetLeft);
                    break
                }
            }
            s = r + e.offsetHeight, o = i + e.offsetWidth, r < u.scrollTop && (u.scrollTop = r), r > u.scrollTop && s > u.scrollTop + u.clientHeight && (u.scrollTop = s - u.clientHeight), i < u.scrollLeft && (u.scrollLeft = i), i > u.scrollLeft && o > u.scrollLeft + u.clientWidth && (u.scrollLeft = o - u.clientWidth)
        }
    },flash: function(e, t, n) {
        var r;
        n || (n = 3), r = setInterval(function() {
            e.style.visibility = n & 1 ? "hidden" : "", --n, n < 0 && clearInterval
            (r)
        }, t || 150)
    },walk: function(e, t) {
        var n;
        if (!t(e))
            for (n = e.firstChild; n; n = n.nextSibling)
                n.nodeType == 1 && DynarchDomUtils.walk(n, t)
    },setDocumentTitle: function(e) {
        document.title = e
    },setZoom: function(e, t) {
        t = "scale(" + t + ")", e = e.style, DynarchDomUtils.forAllStandards("transform", function(n) {
            e.setProperty(n, t, "")
        })
    },forAllStandards: function(e, t) {
        ["-moz-", "-webkit-", "-o-", "-ms-", ""].foreach(function(n) {
            t(n + e)
        })
    },CE_CACHE: CE_CACHE}, DEFINE_CLASS("DlException", null, function(e, t) {
    e.CONSTRUCT = function(e, t) {
        this.error = this.constructor.name, e || (e = "*** no error message given ***"), this.message = this.constructor.name + ": " + e, t != null && (this.code = t)
    }, t.toString = function() {
        var e = this.message;
        return this.code && (e += " / code: " + this.code), e
    }
}).stopEventBubbling = function() {
    throw new DlExStopEventBubbling
}, DEFINE_EXCEPTION("DlExInvalidOperation"), DEFINE_EXCEPTION("DlExAbstractBaseClass"), DEFINE_EXCEPTION("DlExStopEventProcessing"), DEFINE_EXCEPTION("DlExStopFrameEvent"), DEFINE_EXCEPTION("DlExStopEventBubbling"), DEFINE_EXCEPTION("DlDataException"), DEFINE_EXCEPTION("DlSecurityException"), DEFINE_CLASS("DlEventProxy", null, function(e, t) {
    function n(e, t) {
        return e.applyHooks(t, Array.$(arguments, 2))
    }
    e.CONSTRUCT = function() {
        this.__eventHooks = {}, this.__disHooks = {}, this.registerEvents(this.DEFAULT_EVENTS), this.addEventListener("onDestroy", this.__onDestroy)
    }, t.DEFAULT_EVENTS = ["onDestroy"], t.FINISH_OBJECT_DEF = function() {
        var e = this.constructor.DEFAULT_EVENTS;
        e && (this.DEFAULT_EVENTS = this.DEFAULT_EVENTS.concat(e))
    }, t.__onDestroy = function() {
        ["__eventHooks", "__disHooks"].foreach(function(e) {
            var t, n;
            for (t in this[e])
                n = this[e][t], n && n.foreach(function(e, t) {
                    this[t] = null
                }), this[e][t] = null;
            this[e] = null
        }, this)
    }, t.__getEventHooks = function(e, t) {
        var n;
        if (!this.__eventHooks)
            throw new DlExStopEventBubbling;
        return n = this.__eventHooks[e.toLowerCase()], n ? (t && (n = n.slice(0)), n) : []
    }, t.registerEvents = function(e) {
        var t, n = this.__eventHooks, r = 0;
        while (t = e[r++])
            t = t.toLowerCase(), n[t] || (n[t] = [])
    }, t.condEventListener = function(e) {
        return e = e ? this.addEventListener : this.removeEventListener, e.apply(this, Array.$(arguments, 1))
    }, t.addEventListener = function(e, t, n, r) {
        var i, s, o;
        if (e instanceof Array) {
            i = 0;
            while (s = e[i++])
                this.addEventListener(s, t, n, r)
        } else if (typeof e == "object")
            for (i in e)
                this.addEventListener(i, e[i], t, n);
        else
            o = this.__getEventHooks(e), o.remove(t), n == null && e.toLowerCase() == "ondestroy" && (n = !0), n ? o.unshift(t) : o.push(t), r && r.addEventListener("onDestroy", this.removeEventListener.$(this, e, t));
        return this
    }, t.listenOnce = function(e, t, n) {
        var r;
        return n == null && (n = 1), r = function() {
            --n == 0 && this.removeEventListener(e, r), t.apply(this, arguments)
        }, this.addEventListener(e, r)
    }, t.connectEvents = function(e, t, r) {
        var i;
        typeof t == "string" ? (r = t, t = this) : r || (r = e);
        if (e instanceof Array)
            for (i = 0; i < e.length; ++i)
                this.connectEvents(e[i], t, r[i]);
        else
            this.addEventListener(e, n.$(null, t, r));
        return this
    }, t.removeEventListener = function(e, t) {
        var n, r = 0;
        if (e instanceof Array)
            while (n = e[r++])
                this.removeEventListener(n, t);
        else if (typeof e == "object")
            for (r in e)
                this.removeEventListener(r, e[r]);
        else
            this.__getEventHooks(e).remove(t);
        return this
    }, t.removeAllListeners = function(e) {
        var t;
        if (e instanceof Array)
            e.foreach(this.removeAllListeners, this);
        else if (typeof e == "object")
            for (t in e)
                this.removeAllListeners(t);
        else
            this.__getEventHooks(e).length = 0;
        return this
    }, t.disableHooks = function(e) {
        return e instanceof Array ? e.r_foreach(this.disableHooks, this) : (e = e.toLowerCase(), this.__disHooks[e] = this.__eventHooks[e], this.__eventHooks[e] = []), this
    }, t.enableHooks = function(e) {
        return e instanceof Array ? e.r_foreach(this.enableHooks, this) : (e = e.toLowerCase(), this.__eventHooks[e] = this.__disHooks[e], this.__disHooks[e] = null), this
    }, t.callHooks = function(e) {
        var t = arguments.length > 1 ? Array.$(arguments, 1) : [];
        return this.applyHooks(e, t)
    }, t.hasHooks = function(e) {
        var t = this.__eventHooks[e.toLowerCase()];
        return t && t.length > 0
    }, t.withHooks = function(e, t) {
        this.addEventListener.apply(this, e);
        try {
            return t()
        }finally {
            this.removeEventListener.apply(this, e)
        }
    }, t.withDisabledHooks = function(e, t) {
        this.disableHooks.apply(this, e);
        try {
            return t()
        }finally {
            this.enableHooks.apply(this, e)
        }
    }, t.applyHooks = function(e, t) {
        var n, r, i = [], s = 0;
        try {
            n = this.__getEventHooks(e, !0);
            while (r = n[s++])
                i.push(r.apply(this, t))
        } catch (o) {
            if (!(o instanceof DlExStopEventProcessing))
                throw o
        }
        return i
    }, t.debug_countHooks = function() {
        var e, t = {};
        for (e in this.__eventHooks)
            t[e] = this.__eventHooks[e].length;
        return t
    }, t.invoke = function(e) {
        var t = Array.$(arguments, 1);
        return function() {
            this[e].apply(this, t.concat(Array.$(arguments)))
        }.$(this)
    }, t.destroy = function() {
        this.destroyed || (this.destroying = !0, this.callHooks("onDestroy"), this.__eventHooks = null, this.destroying = !1, this.destroyed = !0)
    }
}), DEFINE_CLASS("DlEvent", null, function(e, t, n) {
    function f(e, t, r, i) {
        var s, o = t.getObject();
        switch (t.type) {
            case "click":
                break;
            case "mousedown":
                e._ev_mouseDown = !0, e.applyHooks(t.dl_type, [t]);
                break;
            case "mouseup":
                s = e._ev_mouseDown, e._ev_mouseDown = !1, e.applyHooks(t.dl_type, [t]), s && e._ev_mouseInside && t.button === 0 && (t = new DlEvent(i), t.dl_type = "onClick", this.push([e, t, r, i]));
                break;
            case "mouseover":
            case "mouseout":
                !r || !n.related(r, i) ? (e === o && (e._ev_mouseInside = t.type == "mouseover"), e.applyHooks(t.dl_type, [t])) : (t.dl_type = t.type == "mouseover" ? "onMouseOver" : "onMouseOut", e.applyHooks(t.dl_type, [t]));
                break;
            case "dblclick":
                if ((is_ie || is_opera) && !e.hasHooks("onDblClick")) {
                    t = new DlEvent(i), t.type = "click", e.applyHooks(t.dl_type = "onClick", [t]);
                    break
                }
                ;
            default:
                e.applyHooks(t.dl_type, [t])
        }
        i && t.domStop && n.stopEvent(i)
    }
    function l() {
        this === r && (r = null)
    }
    function c() {
        o.r_foreach(Function.invoke)
    }
    var r, i, s, o, u = n.createElement, a = {mouseover: "onMouseEnter",mouseout: "onMouseLeave",mousedown: "onMouseDown",mouseup: "onMouseUp",mousemove: "onMouseMove",click: "onClick",dblclick: "onDblClick",keydown: "onKeyDown",keyup: "onKeyUp",keypress: "onKeyPress",contextmenu: "onContextMenu",focus: "on_DOM_Focus",blur: "on_DOM_Blur",mousewheel: "onMouseWheel",DOMMouseScroll: "onMouseWheel",textInput: "onTextInput",paste: "onPaste",copy: "onCopy",cut: "onCut"};
    e.CONSTRUCT = function(t) {
        var n;
        this.type = t.type, this.dl_type = a[this.type] || this.type, this.ctrlKey = t.ctrlKey, this.which = t.which, this.keyIdentifier = t.keyIdentifier, is_macintosh ? (this.altGrKey = t.altKey, this.altKey = t.metaKey) : this.altKey = t.altKey, this.shiftKey = t.shiftKey, this.button = t.button, this.focusedWidget = e.focusedWidget();
        if (is_ie)
            switch (t.button) {
                case 0:
                    this.button = null;
                    break;
                case 1:
                    this.button = 0;
                    break;
                case 2:
                    this.button = 2;
                    break;
                case 4:
                    this.button = 1
            }
        this.type.indexOf("key") == 0 && (this.keyCode = t.keyCode, this.charCode = "which" in t ? t.which : is_ie || is_opera ? t.keyCode : t.charCode, this.keyStr = String.fromCharCode(this.charCode)), this.dl_type == "onMouseWheel" && (t.wheelDelta ? n = t.wheelDelta / 120 : t.detail && (n = -t.detail / 3), this.wheelDelta = n), this.pos = {x: t.clientX,y: t.clientY}, this.relPos = this.pos;
        if (is_ie) {
            this.target = t.srcElement;
            switch (this.type) {
                case "mouseover":
                    this.relatedTarget = t.fromElement;
                    break;
                case "mouseout":
                    this.relatedTarget = t.toElement
            }
        } else
            try {
                this.target = t.target, this.target.nodeType == 3 && (this.target = this.target.parentNode);
                if (this.type == "mouseout" || this.type == "mouseover")
                    this.relatedTarget = t.relatedTarget, this.relatedTarget && this.relatedTarget.nodeType == 3 && (this.relatedTarget = this.relatedTarget.parentNode)
            } catch (r) {
                this.relatedTarget = t.explicitOriginalTarget
            }
    }, t.computePos = function(e) {
        var t = e ? e instanceof DlWidget ? e.getElement() : e : document.body, r = this.elPos = t ? n.getPos(t) : {x: 0,y: 0};
        return this.relPos = {x: this.pos.x - r.x,y: this.pos.y - r.y,elPos: r}
    }, t.getObject = function(e) {
        var t = this.target, n = this.object;
        if (!n) {
            try {
                while (t && !DlWidget.getFromElement(t))
                    t = t.parentNode;
                n = t ? DlWidget.getFromElement(t) : null
            } catch (r) {
                n = null
            }
            this.object = n
        }
        return e && (n = n.getParent(e)), n
    }, t.getParentElement = function(e, t) {
        var n;
        t && t instanceof DlWidget && (t = t.getElement());
        if (t && n === t)
            return null;
        n = this.target;
        try {
            while (n && n.tagName.toLowerCase() != e) {
                n = n.parentNode;
                if (t && n === t)
                    return null
            }
        } catch (r) {
            n = null
        }
        return n
    }, e.stopEvent = n.stopEvent, r = null, e.fakeBlur = function() {
        var e, t;
        if (is_safari && r.blur)
            return r.blur();
        e = n.CE_CACHE.FAKE_FOCUS, e || (e = n.CE_CACHE.FAKE_FOCUS = u("a", null, {href: "#",innerHTML: "test",className: "DYNARCH-FAKE-FOCUS"}, document.body)), e.focus(), is_ie && (t = document.body.createTextRange(), t.moveStart("character", 0), t.collapse(!0), t.select()), e.blur(), window.status = ""
    }, e.focusedWidget = function(t) {
        var n;
        if (arguments.length > 0 && r !== t) {
            r && !r.destroyed && (r._focusable == 2 ? t._focusable < 2 && e.fakeBlur() : r.blur(), r.removeEventListener("onDestroy", l)), r = t;
            if (t) {
                t.addEventListener("onDestroy", l), n = t.parent;
                while (n)
                    n._focusedWidget = t, n = n.parent
            }
        }
        return r
    }, e.checkDisabled = function(e) {
        while (e) {
            if (e.disabled())
                return !0;
            e = e.parent
        }
        return !1
    }, t.destroy = function() {
        this.object = this.target = this.relatedTarget = null
    }, t.stopDomEvent = function() {
        e.stopEvent(e.latestDomEvent)
    }, i = e.GLOBAL_CAPTURES = {}, s = "keydown keyup keypress".hashWords(), e._genericEventHandler = function(t, n) {
        var o, u, a, l, c, h;
        t || (t = window.event), a = t instanceof e ? t : new e(t), n && (t = n);
        if (a._failed) {
            e.stopEvent(t);
            return
        }
        e.latestEvent = a, e.latestDomEvent = t, a.pos.x && a.dl_type != "onMouseWheel" && (e.latestMouseEvent = a, a.dl_type == "onMouseDown" && (e.latestMouseDownEvent = a));
        try {
            l = i[a.dl_type];
            if (l)
                for (c = l.length; --c >= 0; )
                    l[c](a);
            a.type in s && r ? o = r.getElement() : o = a.target, h = [], c = 0;
            while (o) {
                u = DlWidget.getFromElement(o);
                if (u) {
                    e.checkDisabled(u) || (h[c++] = [u, a, o, t]);
                    if (u.__noPropEvents && u.__noPropEvents.test(a.dl_type))
                        break
                }
                o = o.parentNode
            }
            for (c = 0; c < h.length; ++c)
                f.apply(h, h[c])
        } catch (p) {
            if (!(p instanceof DlExStopEventBubbling))
                throw p;
            e.stopEvent(t)
        }
        a.destroy()
    }, o = [], e._unloadHandler = c, e.captureGlobals = function(t) {
        var n;
        for (n in t)
            e.captureGlobal(n, t[n])
    }, e.releaseGlobals = function(t) {
        var n;
        for (n in t)
            e.releaseGlobal(n, t[n])
    }, e.captureGlobal = function(e, t) {
        var n = i[e];
        n || (n = i[e] = []), n.push(t)
    }, e.releaseGlobal = function(e, t) {
        var n = i[e];
        n && n.remove(t)
    }, e.atUnload = function(e) {
        o.push(e)
    }, n.addEvents(document, ["contextmenu", "click", "dblclick", "mousedown", "mouseup", "mouseover", "mouseout", "mousemove", is_gecko ? "DOMMouseScroll" : "mousewheel", "keydown", "keyup", "keypress", "paste", "copy", "cut"], e._genericEventHandler), n.addEvent(window, "unload", c)
}), DEFINE_CLASS("DlWidget", DlEventProxy, function(e, t, n) {
    function p() {
        return i || (i = new DlTooltip({})), i
    }
    function d() {
        var t;
        this.__tooltipActive && p().hide(), this.__contextMenu instanceof e && this.__contextMenu.destroy();
        if (this.parent)
            try {
                this.parent.removeWidget(this)
            } catch (i) {
            }
        t = this.getElement(), t && (t._dynarch_object = null, t._dynarch_focusable = null), this._element = null, n.trash(t), r[this.id] && (r[this.id] = null, delete r[this.id]), t = null, this.__refNodes.r_foreach(function(e, t) {
            this.__refNodes[t] = null, this[e] = null, delete this[e]
        }, this), this.__refNodes = null, this.userData = null, this.__propsUserData = null
    }
    function v() {
        this.__tooltip && this._popupTooltip()
    }
    function m() {
        p().hide()
    }
    function C(e) {
        var t, n;
        p().cancel(), this._focusable && !e._justFocusedWidget && (e._justFocusedWidget = this, this._focusable < 2 && this.focus()), e.button == 0 && (t = this._dragArgs, t && !o && t.startOK(this, e) && (n = e.getObject(), n && n.applyHooks("onMouseLeave", [e]), o = !0, t.source = this, t.captures = {onMouseMove: g.$(this, t),onMouseUp: b.$(this, t),onMouseOver: w.$(this, t),onMouseOut: E.$(this, t),onMouseEnter: S.$(this, t),onMouseLeave: x.$(this, t),onContextMenu: N.$(this, t),onKeyPress: T.$(this, t)}, t.startPos = e.pos, t.startElPos = this.getPos(), DlEvent.captureGlobals(t.captures)))
    }
    function k(e) {
        var t, n = this.__contextMenu;
        typeof n == "function" && (n = n.call(this, e)), n && (t = this._getContextMenuPopup(), t.popup({timeout: 0,content: n,anchor: n.contextMenuAnchor || this.getElement(),align: n.contextMenuAlign || "mouse",widget: this,onPopup: n.contextMenuOnPopup || null,onHide: n.contextMenuOnHide || null,isContext: !0}), DlException.stopEventBubbling())
    }
    var r, i, s, o, u, a = n.createElement, f = n.addClass, l = n.delClass, c = n.condClass, h = Dynarch.ID;
    e.FIXARGS = function(e) {
        e.focusable == null && e.tabIndex && (e.focusable = !0)
    }, e.CONSTRUCT = function() {
        this.__propsUserData = {}, this.__refNodes = [];
        if (!(this._parent == null || this._parent instanceof DlContainer))
            throw new DlException("Parent must be an instance of DlContainer");
        this.id = h(this._objectType || "DlWidget"), r[this.id] = this, this.initDOM()
    }, e.DEFAULT_ARGS = {userData: ["data", null],_parent: ["parent", null],_fillParent: ["fillParent", null],_tagName: ["tagName", "div"],_dragArgs: ["drag", null],_element: ["element", null],_focusable: ["focusable", !1],_tabIndex: ["tabIndex", 0],_accessKey: ["accessKey", null],__appendArgs: ["appendArgs", window.undefined],__addClassName: ["className", ""],__disabled: ["disabled", !1],__tooltip: ["tooltip", null],__contextMenu: ["contextMenu", null],__tooltipTimeout: ["tooltipTimeout", 650],__refCnt: ["_refCnt", 0],__noPropEvents: ["dontBubbleEvents", null]}, e.DEFAULT_EVENTS = ["onMouseEnter", "onMouseLeave", "onMouseMove", "onMouseDown", "onMouseUp", "onMouseOver", "onMouseOut", "onMouseWheel", "onClick", "onDblClick", "onDisabled", "onDisplay", "onFocus", "on_DOM_Focus", "onBlur", "on_DOM_Blur", "onKeyDown", "onKeyUp", "onKeyPress", "onResize", "onContextMenu", "onTooltipShow", "onTooltipHide"], r = e.WIDGETS = {}, e.getById = function(e) {
        return r[e]
    }, e.getFromElement = function(e) {
        return e._dynarch_object
    }, e.getFromUpElement = function(e) {
        while (e && !e._dynarch_object)
            e = e.parentNode;
        return e && e._dynarch_object
    }, i = null, e.getTooltip = p, t.getWidgetId = function() {
        return this.id
    }, t._className = [], t.FINISH_OBJECT_DEF = function() {
        e.BASE.FINISH_OBJECT_DEF.call(this), this._className = this._className.concat([this._objectType])
    }, s = null, t.getResizeRect = e.getResizeRect = function() {
        return s || (s = a("div", {display: "none"}, {className: "Dl-ResizeRect",innerHTML: "&nbsp;"}, document.body)), s
    }, e.debug_countHooks = function() {
        var e = {};
        return Array.hashKeys(DlWidget.WIDGETS).foreach(function(t) {
            e[t] = DlWidget.WIDGETS[t].debug_countHooks()
        }), e
    }, t.destroy = function() {
        this.unref() <= 0 && e.BASE.destroy.call(this)
    }, t.__onTooltipShow = function() {
        this.__tooltipActive = !0, this.callHooks("onTooltipShow")
    }, t.__onTooltipHide = function() {
        this.__tooltipActive = !1, this.callHooks("onTooltipHide")
    }, t._popupTooltip = function() {
        p().popup({timeout: this.__tooltipTimeout,content: this.__tooltip,anchor: this.getElement(),align: "mouse",onPopup: this.__onTooltipShow,onHide: this.__onTooltipHide,widget: this})
    };
    function g(e, t) {
        var n;
        !e.dragging && (Math.abs(t.pos.x - e.startPos.x) >= e.delta || Math.abs(t.pos.y - e.startPos.y) >= e.delta) && (e.dragging = !0, e.makeElementCopy(this, t), e.applyHooks("onStartDrag", [this, t]), this.addClass(e.draggingClass)), e.dragging && (n = e.elementCopy, n && (n.style.left = t.pos.x + 5 + "px", n.style.top = t.pos.y + 5 + "px"), e.moving(this, t), DlException.stopEventBubbling())
    }
    function y(e, t, n) {
        this.delClass(e.draggingClass), DlEvent.releaseGlobals(e.captures), e.captures = null, o = !1, n || e.doDrop(this, t), e.reset(n)
    }
    function b(e, t) {
        var n;
        t.button == 0 && (n = e.dragging && e.canDrop, y.call(this, e, t, !n))
    }
    function w() {
        DlException.stopEventBubbling()
    }
    function E() {
        DlException.stopEventBubbling()
    }
    function S(e, t) {
        var n, r = t.getObject(), i = !1, s = r;
        while (s) {
            if (s === this) {
                i = !0;
                break
            }
            s = s.parent
        }
        n = e.dropOK(this, t, r, i), DlException.stopEventBubbling()
    }
    function x() {
        DlException.stopEventBubbling()
    }
    function T(e, t) {
        t.keyCode == DlKeyboard.ESCAPE && y.call(this, e, t, !0), DlException.stopEventBubbling()
    }
    function N() {
        DlException.stopEventBubbling()
    }
    o = !1, t.setData = function(e, t) {
        arguments.length == 1 ? delete this.__propsUserData[e] : this.__propsUserData[e] = t
    }, t.getData = function(e) {
        return this.__propsUserData[e]
    }, t._getDlPopup = function() {
        var e = this.getParent(DlPopup) || 0;
        return e && (e = e._level + 1), DlPopupMenu.get(e)
    }, t._getContextMenuPopup = t._getDlPopup, u = {onDestroy: d,onMouseEnter: v,onMouseLeave: m,onMouseDown: C,onContextMenu: k}, t._setListeners = function() {
        this.addEventListener(u), this.addEventListener(is_ie || is_khtml ? "onKeyDown" : "onKeyPress", this._handle_focusKeys)
    }, t._handle_focusKeys = function() {
    }, t._check_accessKey = function(e) {
        return this._accessKey && DlKeyboard.checkKey(e, this._accessKey)
    }, t._handle_accessKey = function() {
        this.focus()
    }, t._setFocusedStyle = function(e) {
        this.condClass(e, this._className.peek() + "-focus")
    }, t.focus = function() {
        this._focusable ? (DlEvent.focusedWidget(this), this._setFocusedStyle(!0), this.callHooks("onFocus"), this instanceof DlEntry || this.scrollIntoView()) : this.parent && this.parent.focus()
    }, t.blur = function() {
        this._focusable && (this.destroyed || (this._setFocusedStyle(!1), this.callHooks("onBlur")))
    }, t.focusInside = function() {
        var e = DlEvent.focusedWidget();
        while (e) {
            if (e == this)
                break;
            e = e.parent
        }
        return !!e
    }, t._createElement = function(e) {
        var t, r, i = this._element;
        i ? this.__alreadyInDom = !0 : (t = this.constructor, r = t.__joinedClassName || this._className.join(" "), t.__joinedClassName || (t.__joinedClassName = r), this.__addClassName && (r += " " + this.__addClassName), e ? (i = n.createFromHtml(e), i.className = r) : i = a(this._tagName, null, {className: r}), this._focusable && (i._dynarch_focusable = !0), this._element = i), i._dynarch_object = this
    }, t.getElement = function() {
        return this._element
    }, t.getParentNode = function() {
        return this._element.parentNode
    }, t.getDOMChildren = function() {
        return Array.$(this.getContentElement().childNodes)
    }, t.getContentElement = function() {
        return this.getElement()
    }, t.setStyle = function(e, t) {
        var n, r = this.getElement().style;
        if (arguments.length > 1)
            r[e] = t;
        else
            for (n in e)
                this.setStyle(n, e[n])
    }, t.setContent = function(t) {
        var n = this.getContentElement();
        while (n.firstChild)
            n.removeChild(n.lastChild);
        if (typeof t == "string")
            n.innerHTML = t;
        else {
            if (t instanceof Function)
                return this.setContent(t.call(this));
            t instanceof e ? this.appendWidget(t, this.__appendArgs) : t instanceof Array ? n.innerHTML = t.join("") : t != null && n.appendChild(t)
        }
        return t != null
    }, t.ref = function() {
        return this.__refCnt++
    }, t.unref = function() {
        return --this.__refCnt
    }, t.refCnt = function() {
        return this.__refCnt
    }, t.setContextMenu = function(t) {
        this.__contextMenu instanceof e && this.__contextMenu.destroy(), t instanceof e && t.ref(), this.__contextMenu = t
    }, t.setTooltip = function(e) {
        this.__tooltip = e
    }, t.initDOM = function() {
        return this._setListeners(), this._createElement(), this._parent && (this._parent.appendWidget(this, this.__appendArgs), this._parent = null), this.__disabled && this.disabled(!0, !0), this.__onTooltipShow = this.__onTooltipShow.$(this), this.__onTooltipHide = this.__onTooltipHide.$(this), this
    }, t.setUnselectable = function(e, t) {
        e == null && (e = this.getElement()), n.setUnselectable(e, t)
    }, t.disabled = function(e, t) {
        return e != null && (t || e != this.__disabled) && (this.__disabled = e, this.condClass(e, "DlWidget-disabled"), this.condClass(e, this._className.peek() + "-disabled"), this.applyHooks("onDisabled", [e])), this.__disabled
    }, t.enabled = function(e, t) {
        return e != null && this.disabled(!e, t), !this.__disabled
    }, t.getParent = function(e, t) {
        var n;
        if (e == null)
            return this.parent;
        n = this, t && (n = this.parent);
        while (n && !(n instanceof e))
            n = n.parent;
        return n
    }, t.findParent = function(e, t) {
        var n, r, i = this;
        t && (i = this.parent);
        if (e instanceof Function)
            while (i && !e(i))
                i = i.parent;
        else {
            r = Array.$(arguments, 2);
            while (i) {
                n = i[e];
                if (n) {
                    if (!(n instanceof Function))
                        break;
                    if (n.apply(i, r))
                        break
                }
                i = i.parent
            }
        }
        return i
    }, t.getPos = function() {
        return n.getPos(this.getElement())
    }, t.getBRPos = function() {
        return n.getBRPos(this.getElement())
    }, t.getOffsetPos = function() {
        var e = this.getElement();
        return {x: e.offsetLeft,y: e.offsetTop}
    }, t.setPos = function(e, t) {
        var n = this.getElement();
        e != null && typeof e == "object" && (t = e.y, e = e.x), e != null && (n.style.left = e + "px"), t != null && (n.style.top = t + "px")
    }, t.setSize = t.setOuterSize = function(e) {
        n.setOuterSize(this.getElement(), e.x, e.y), this.callHooks("onResize")
    }, t.setInnerSize = function(e) {
        n.setInnerSize(this.getContentElement(), e.x, e.y), this.callHooks("onResize")
    }, t.getSize = t.getOuterSize = function() {
        return n.getOuterSize(this.getElement())
    }, t.getInnerSize = function() {
        return n.getInnerSize(this.getContentElement())
    }, t.display = function(e) {
        var t = this.getElement().style;
        return e != null ? (t.display = e ? "" : "none", this.applyHooks("onDisplay", [e, t.display, "display"]), e) : t.display != "none"
    }, t.visibility = function(e) {
        var t = this.getElement().style;
        return e != null ? (t.visibility = e ? "" : "hidden", this.applyHooks("onDisplay", [e, t.visibility, "visibility"]), e) : t.visible != "hidden"
    }, t.opacity = function(e) {
        return n.setOpacity(this.getElement(), e)
    }, t.position = function(e) {
        var t = this.getElement().style, n = t.position;
        return e != null && (t.position = e), n
    }, t.setIconClass = function(e) {
        var t = this.getContentElement();
        c(t, e != null, this.__withIconClass || this._className.peek() + "-withIcon"), this.iconClass && l(t, this.iconClass), e && f(t, e), this.iconClass = e
    }, t.addClass = function(e, t) {
        f(this.getElement(), e, t)
    }, t.delClass = function(e, t) {
        l(this.getElement(), e, t)
    }, t.condClass = function(e, t, n) {
        return c(this.getElement(), e, t, n), e
    }, t.zIndex = function(e) {
        var t = this.getElement();
        return e != null ? (t.style.zIndex = e, e) : t.style.zIndex ? parseInt(t.style.zIndex, 10) : 0
    }, t.refNode = function(e, t) {
        return this[e] = t, this.__refNodes.remove(e), t != null && this.__refNodes.push(e), t
    }, t.debug_highlight = function(e) {
        this.getElement().style.backgroundColor = e || "yellow"
    }, t.getQuickPopup = function() {
        var e = this.getParent(DlPopup) || 0;
        return e && (e = e._level + 1), DlDialogPopup.get(e)
    }, t.quickPopup = function(e) {
        var t = this.getQuickPopup();
        e = Object.makeCopy(e), Object.mergeUndefined(e, {anchor: this.getElement(),align: {prefer: "CC"}}), t.popup(e)
    }, t.getScroll = function() {
        var e = this.getElement();
        return {x: e.scrollLeft,y: e.scrollTop}
    }, t.scrollIntoView = function() {
        n.scrollIntoView(this.getElement())
    }, t.flash = function(e, t) {
        n.flash(this.getElement(), e, t)
    }, DlEvent.atUnload(function() {
        var e, t, n;
        do {
            window.DL_CLOSING = !0, e = !1;
            for (t in r) {
                e = !0, n = r[t];
                try {
                    r[t] = null, delete r[t], n.destroy()
                } catch (i) {
                }
                break
            }
        } while (e);
        r = null
    })
}), DEFINE_CLASS("DlContainer", DlWidget, function(e, t) {
    function r(e, t) {
        return e = e ? Array.$(e.getElement().getElementsByTagName("*")) : [], Array.$(this.getElement().getElementsByTagName("*")).grep(t ? "_dynarch_object" : "_dynarch_focusable").grep(DynarchDomUtils.elementIsVisible).grep(e.contains.$(e).inverse()).map(DlWidget.getFromElement).grep("enabled").mergeSort(function(e, t) {
            return e._tabIndex - t._tabIndex
        })
    }
    function i(e, t) {
        var n = r.call(this, e), i = n.find(e);
        i = n.rotateIndex(i + t);
        if (i != null)
            return n[i]
    }
    var n;
    e.BEFORE_BASE = function() {
        this._widgets = []
    }, e.DEFAULT_ARGS = {_scrollBars: ["scroll", !1],__noParentKeyBindings: ["noParentKB", !1]}, t._createElement = function() {
        e.BASE._createElement.apply(this, arguments), this._scrollBars && this.setStyle("overflow", "auto")
    }, t.appendWidget = function(e) {
        e.parent && e.parent.removeWidget(e), this._widgets.push(e), e.parent = this, e.__alreadyInDom || this._appendWidgetElement.apply(this, arguments), delete e.__alreadyInDom
    }, t._appendWidgetElement = function(e, t) {
        var n, r = e.getElement();
        if (typeof t == "number") {
            n = this.getContentElement();
            try {
                t = n.childNodes[t], n.insertBefore(r, t)
            } catch (i) {
                n.appendChild(r)
            }
        } else
            t == null ? t = this.getContentElement() : typeof t == "string" && (t = document.getElementById(t)), r.parentNode !== t && t.appendChild(r)
    }, t.removeWidget = function(e) {
        e.parent === this && (this._removeWidgetElement(e), this._widgets.remove(e), e.parent = null)
    }, t._removeWidgetElement = function(e) {
        var t;
        this._widgets.contains(e) && (t = e.getElement(), t.parentNode && t.parentNode.removeChild(t))
    }, t.destroyChildWidgets = function() {
        var t, n, r = Array.$(this._widgets);
        for (t = 0; t < r.length; ++t)
            r[t] instanceof e && r.push.apply(r, r[t]._widgets);
        return r.r_foreach(function(e) {
            try {
                e.destroy()
            } catch (t) {
            }
        }), n = this.getContentElement(), n && (n.innerHTML = ""), n
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener("onDestroy", this.destroyChildWidgets), this.addEventListener("onResize", this.__doLayout)
    }, t.disabled = function(t, n) {
        var r = e.BASE.disabled.call(this, t, n);
        return t != null && this._widgets.r_foreach(function(e) {
            e.disabled(t, n)
        }), r
    }, t.children = function(e) {
        return e != null ? this._widgets[e] : this._widgets
    }, t.__doLayout = function() {
        var e = this.children().grep_first(function(e) {
            return e._fillParent
        });
        e && e.setSize(this.getInnerSize())
    }, t.getNextFocusWidget = function(e) {
        return i.call(this, e, 1)
    }, t.getPrevFocusWidget = function(e) {
        return i.call(this, e, -1)
    }, t.getFirstFocusWidget = function() {
        return this.getNextFocusWidget(null)
    }, t.getLastFocusWidget = function() {
        return this.getPrevFocusWidget(null)
    }, t._handleKeybinding = function(e, t) {
        var n;
        if (e.altKey || e.ctrlKey)
            n = r.call(this, t, !0), n.foreach(function(t) {
                if (t._check_accessKey(e))
                    throw t._handle_accessKey(e), e.domStop = !0, new DlExStopEventBubbling
            }), this.parent && !this.__noParentKeyBindings && this.parent._handleKeybinding(e, this)
    }, e.getHiddenContainer = function() {
        return n || (n = new this({className: "DlContainer-Hidden"}), document.body.appendChild(n.getElement())), n
    }
}), DEFINE_CLASS("DlRadioGroup", DlEventProxy, function(e, t) {
    function r() {
        n[this.id] && (this._buttons = null, this._buttonsById = null, this._buttonsByValue = null, this._history = null, delete n[this.id])
    }
    function i(e) {
        var t;
        if (e != null) {
            this._changed = !0;
            if (e.checked()) {
                if (this._maxChecked != null)
                    while (this._history.length >= this._maxChecked)
                        t = this._history[0], t.checked(!1, !0), this._history.splice(0, 1);
                this._history.push(e)
            } else {
                if (this._minChecked != null && this._history.length <= this._minChecked)
                    throw e.checked(!0, !0), new DlExStopEventProcessing;
                this._history.remove(e)
            }
        }
    }
    var n;
    e.CONSTRUCT = function(e) {
        e != null && (this._maxChecked = 1, this._minChecked = null, this.id = e, this.reset(), this.addEventListener("onDestroy", r))
    }, n = {}, e.DEFAULT_EVENTS = ["onChange"], e.getById = e.get = function(e) {
        var t;
        return e || (e = Dynarch.ID("group")), t = n[e], t || (t = n[e] = new this(e)), t
    }, t.reset = function() {
        this._buttons && this._buttons.r_foreach(function(e) {
            e.__group = e.__groupId = null
        }), this._changed = !1, this._buttons = [], this._buttonsById = {}, this._buttonsByValue = {}, this._history = [], this.removeAllListeners("onChange"), this.addEventListener("onChange", i)
    }, t.changed = function(e) {
        var t = this._changed;
        return e != null && (this._changed = e), t
    }, t.getSelected = function() {
        return this._history
    }, t.getButtons = function() {
        return this._buttons
    }, t.getNextButton = function(e) {
        var t, n;
        e == null && (e = this.getSelected()[0]), t = this._buttons, n = t.nullLimitIndex(t.find(e) + 1);
        if (n != null)
            return t[n]
    }, t.getPrevButton = function(e) {
        var t, n;
        e == null && (e = this.getSelected()[0]), t = this._buttons, n = t.nullLimitIndex(t.find(e) - 1);
        if (n != null)
            return t[n]
    }, t.getValue = function() {
        return this._history.map("value")
    }, t.setValue = function(e, t) {
        var n, r = this._buttonsByValue;
        e instanceof Array || (e = [e]), e = e.toHash(!0), this._history = [];
        for (n in r)
            r[n].checked(e[n], !0), e[n] && this._history.push(r[n]);
        t && this.callHooks("onChange")
    }, t.getByValue = function(e) {
        return this._buttonsByValue[e]
    }, t.addWidget = function(e, t) {
        var n;
        this._buttonsById[e.id] || (t == null && (t = this._buttons.length), this._buttonsById[e.id] = e, this._buttons.splice(t, 0, e), e.checked() && this._history.push(e), n = e.value(), typeof n != "undefined" && (this._buttonsByValue[n] = e), e.addEventListener("onDestroy", this.removeWidget.$(this, e)))
    }, t.removeWidget = function(e) {
        var t;
        this._buttonsById[e.id] && (this._changed = !0, delete this._buttonsById[e.id], t = e.value(), typeof t != "undefined" && delete this._buttonsByValue[e.value()], this._buttons.remove(e), this._history.length != this._history.remove(e).length && this.callHooks("onChange"))
    }, t.minChecked = function(e) {
        return arguments.length > 0 && (this._minChecked = e), this._minChecked
    }, t.maxChecked = function(e) {
        return arguments.length > 0 && (this._maxChecked = e), this._maxChecked
    }, t.checkAll = function(e, t) {
        e == null && (e = !0), t == null && (t = !1), this._buttons.foreach(function(n) {
            n.checked(e, !t)
        }), this._history = e ? Array.$(this._buttons) : []
    }, t.unCheckAll = function() {
        this._history.r_foreach(function(e) {
            e.checked(!1)
        })
    }
}), DEFINE_CLASS("DlAbstractButton", DlWidget, function(e, t) {
    var n = ["onMouseEnter", "onMouseLeave", "onMouseDown", "onMouseUp", "onUpdateLabel", "onClick", "onCheck", "onChange", "onDisabled"], r = {STANDARD: 1,TWOSTATE: 2};
    e.DEFAULT_ARGS = {_label: ["label", ""],_classes: ["classes", {}],_checked: ["checked", !1],__groupId: ["group", null],_btnType: ["type", r.STANDARD],_value: ["value", window.undefined],_noCapture: ["noCapture", !1],_alwaysCheck: ["alwaysCheck", !1]}, e.CONSTRUCT = function(e) {
        var t, n = this.__groupId;
        n != null && (typeof n == "object" ? (t = n, this.__groupId = t.id) : t = DlRadioGroup.get(n), this.__group = t, t.addWidget(this, typeof e.appendArgs == "number" ? e.appendArgs : null)), this._noCapture || (this._btnpressCapture = {onMouseMove: DlException.stopEventBubbling,onMouseUp: this._cap_onMouseUp.$(this),onMouseOver: DlException.stopEventBubbling,onMouseOut: DlException.stopEventBubbling,onMouseEnter: this._cap_onMouseEnter.$(this),onMouseLeave: this._cap_onMouseLeave.$(this)})
    }, e.DEFAULT_EVENTS = ["onCheck", "onUncheck", "onChange", "onUpdateLabel"], t._cap_onMouseUp = function(e) {
        var t = e.getObject();
        DlEvent.releaseGlobals(this._btnpressCapture), this.applyHooks("onMouseUp", [e]), this._ev_mouseInside || this.applyHooks("onMouseLeave", [e]), t !== this && (t && t.applyHooks("onMouseEnter", [e]), DlException.stopEventBubbling())
    }, t._cap_onMouseEnter = function(e) {
        var t = e.getObject();
        t === this && this.addClass(this._classes.active), t && (t._ev_mouseInside = !0), DlException.stopEventBubbling()
    }, t._cap_onMouseLeave = function(e) {
        var t = e.getObject();
        t === this && this.delClass(this._classes.active), t && (t._ev_mouseInside = !1), DlException.stopEventBubbling()
    }, e.TYPE = r, t._onMouseEnter = function() {
        this.addClass(this._classes.hover)
    }, t._onMouseLeave = function() {
        this.delClass(this._classes.hover), this.delClass(this._classes.active)
    }, t._onMouseDown = function(e) {
        e.button === 0 && (this._ev_mouseInside = !0, this.addClass(this._classes.hover), this.addClass(this._classes.active), this._noCapture || (DlEvent.captureGlobals(this._btnpressCapture), e.domStop = !0))
    }, t._onMouseUp = function() {
        this.delClass(this._classes.active)
    }, t._onUpdateLabel = function() {
        this.condClass(!this._label || !/\S/.test(this._label), this._classes.empty)
    }, t._onClick = function() {
        this._btnType == r.TWOSTATE && (this._alwaysCheck ? this.checked(!0) : this.toggle())
    }, t.keyClicked = function(e) {
        this.addClass(this._classes.active), function() {
            this.delClass(this._classes.hover), this.delClass(this._classes.active), this.applyHooks("onClick", [e])
        }.delayed(90, this), e && (e.domStop = !0, DlException.stopEventBubbling())
    }, t._handle_focusKeys = function(t) {
        var n, r, i = t.keyCode;
        i == DlKeyboard.ENTER || t.charCode == DlKeyboard.SPACE ? this.keyClicked(t) : !this._customMoveKeys && this.__group && i in DlKeyboard.KEYS_MOVE && (n = i in DlKeyboard.KEYS_MOVE_PREV, r = n ? this.__group.getPrevButton(this) : this.__group.getNextButton(this), r && (r.focus(), t.shiftKey && (this.checked(!0), r.checked(!0)), t.domStop = !0, DlException.stopEventBubbling())), e.BASE._handle_focusKeys.call(this, t)
    }, t._handle_accessKey = function(e) {
        this.focus(), this.keyClicked(e)
    }, t.disabled = function(t, n) {
        return t != null && t && (this.delClass(this._classes.hover), this.delClass(this._classes.active)), e.BASE.disabled.call(this, t, n)
    }, t._onChange = function() {
        this.__group != null && this.__group.applyHooks("onChange", [this])
    }, t._onCheck = Function.noop, t._onDisabled = function(e) {
        this.condClass(e, this._classes.disabled), e && this._capture && (DlEvent.releaseCapture(this._capture), this._capture = null)
    }, t._createElement = function() {
        e.BASE._createElement.call(this), this._createLabelElement(), this.label(this._label, !0), this._updateState(), this.setUnselectable()
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), n.r_foreach(function(e) {
            this.addEventListener(e, this["_" + e])
        }, this)
    }, t._createLabelElement = Function.noop, t.label = function(e, t) {
        if (t || arguments.length > 0 && e !== this._label)
            this._label = e, e && (e = "<div class='DlButton-Label'>" + this._label + "</div>"), this.setContent(e), this.applyHooks("onUpdateLabel", [this._label]);
        return this._label
    }, t.setLabel = t.label, t.getLabel = t.label, t.group = function() {
        return this.__group
    }, t._checkTwoState = function(e) {
        var t = this._btnType != r.TWOSTATE;
        if (t && !e)
            throw new DlExInvalidOperation("This operation is only available for a TWOSTATE button");
        return !t
    }, t._updateState = function() {
        var e;
        this._checkTwoState(!0) && (e = this._classes, this.condClass(this._checked, e.checked, e.unchecked))
    }, t.checked = function(e, t) {
        var n;
        return this._checkTwoState(), arguments.length > 0 && (e = !!e, n = !t && this._checked !== e, this._checked = e, this._updateState(), n && (this.callHooks("onChange"), this.callHooks(e ? "onCheck" : "onUncheck"))), this._checked
    }, t.toggle = function(e) {
        this._checkTwoState(), this.checked(!this._checked, e)
    }, t.value = function(e) {
        var t = this._value;
        return arguments.length > 0 && (this._value = e), t
    }, t.setValue = t.value, t.getValue = t.value
}), DEFINE_CLASS("DlAnimation", DlEventProxy, function(e, t) {
    function c() {
        this.t = this.i / this.length;
        try {
            this.applyHooks("onUpdate", [this.t]), ++this.i > this.length && this.stop(!0)
        } catch (e) {
            throw this.stop(), e
        }
    }
    var n, r, i, s, o, u, a, f, l;
    e.DEFAULT_EVENTS = ["onStart", "onStop", "onPause", "onUpdate"], e.CONSTRUCT = function(e, t) {
        this.addEventListener("onDestroy", this.stop.$(this)), e != null && (this.length = e), t != null && (this._speed = 1e3 / t), this._update = c.$(this)
    }, t.start = function(e, t, n) {
        this.stop(), e != null && (this.length = e), t != null && (this._speed = 1e3 / t), n != null && (n instanceof Function || (n = DlAnimation.easing[n]), this.func = n), this.t = 0, this.i = 0, this.callHooks("onStart"), this._timer = setInterval(this._update, this._speed)
    }, t.running = function() {
        return this._timer
    }, t.stop = function(e) {
        this._timer && (clearInterval(this._timer), this._timer = null, this.applyHooks("onStop", [e]))
    }, t.getPos = function(e) {
        return e == null && (e = this.func), e.call(this, this.t)
    }, n = Math.PI, r = Math.abs, i = Math
    .asin, s = Math.pow, o = Math.sin, u = Math.cos, a = Math.exp, f = Math.round, l = e.easing = {elastic_b: function(e) {
            return 1 - u(-e * 5.5 * n) / s(2, 7 * e)
        },elastic_b_custom: function(e, t, r) {
            return e += .5, 1 - u(-r * e * n) / s(2, t * r)
        },magnetic: function(e) {
            return 1 - u(e * e * e * 10.5 * n) / a(4 * e)
        },accel_b: function(e) {
            return e = 1 - e, 1 - e * e * e
        },accel_a: function(e) {
            return e * e * e
        },accel_ab: function(e) {
            return e = 1 - e, 1 - o(e * e * e * n / 2)
        },bounce_b: function(e) {
            return e < 1 / 2.75 ? 7.5625 * e * e : e < 2 / 2.75 ? 7.5625 * (e -= 1.5 / 2.75) * e + .75 : e < 2.5 / 2.75 ? 7.5625 * (e -= 2.25 / 2.75) * e + .9375 : 7.5625 * (e -= 2.625 / 2.75) * e + .984375
        },shake: function(e) {
            return e < .5 ? -u(e * 11 * n) * e * e : (e = 1 - e, u(e * 11 * n) * e * e)
        }}
}), DEFINE_CLASS("DlBox", DlContainer, function(e, t, n) {
    e.DEFAULT_ARGS = {_borderSpacing: ["borderSpacing", 0],_align: ["align", null],_tagName: ["tagName", "table"]}, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getElement(), t.cellSpacing = this._borderSpacing, t.cellPadding = 0, this._align && (t.align = this._align), this.refNode("_tbody", n.createElement("tbody", null, null, t))
    }, t.getTableElement = t.getElement, t._appendWidgetElement = function(e, t) {
        t == null ? this.createCellElement().appendChild(e.getElement()) : t.appendChild(e.getElement())
    }, t.destroyChildWidgets = function() {
        var e = Array.$(this._widgets);
        e.r_foreach(function(e) {
            try {
                e.destroy()
            } catch (t) {
            }
        })
    }, t.__addSep = function(e, t, r) {
        var i;
        return r || (r = this.createCellElement()), r.separator = !0, i = this._objectType + "-" + e, t && (i += " " + t), r.className = i, r.innerHTML = "<div class='" + i + "'>&nbsp;</div>", n.setUnselectable(r), r
    }, t.addSeparator = function(e, t) {
        return this.__addSep("separator", e, t)
    }, t.addSpace = function(e, t) {
        return this.__addSep("spacer", e, t)
    }
}), DEFINE_CLASS("DlButton", DlAbstractButton, function(e, t, n) {
    e.CONSTRUCT = function() {
        this.setIconClass(this._iconClass), this._iconClass = null
    }, e.TYPE = DlAbstractButton.TYPE, e.DEFAULT_ARGS = {_classes: ["classes", {active: "DlButton-active",hover: "DlButton-hover",checked: "DlButton-1",unchecked: "DlButton-0",empty: "DlButton-empty",disabled: "DlButton-disabled"}],_iconClass: ["iconClass", null]}, t.__withIconClass = "DlButton-withIcon", t._createElement = function() {
        e.BASE._createElement.call(this), this.addClass("DlWidget-3D")
    }, t._createLabelElement = function() {
        this.getElement().innerHTML = "<div class='DlButton-inner'><div></div></div>"
    }, t.getContentElement = function() {
        return this.getElement().firstChild.firstChild
    }, t.setSize = t.setOuterSize = function(e) {
        var t = n.getPaddingAndBorder(this.getElement());
        e.x != null && (e.x -= t.x), e.y != null && (e.y -= t.y), t = n.getPaddingAndBorder(this.getElement().firstChild), e.x != null && (e.x -= t.x), e.y != null && (e.y -= t.y), n.setOuterSize(this.getContentElement(), e.x, e.y)
    }
}), DEFINE_CLASS("DlHbox", DlBox, function(e, t, n) {
    var r = n.createElement;
    t._createElement = function() {
        e.BASE._createElement.call(this), this.refNode("_row", r("tr", null, null, this._tbody))
    }, t.createCellElement = function(e) {
        var t = r("td", null, {className: "cell"});
        return e != null ? this._row.insertBefore(t, e) : this._row.appendChild(t), t
    }, t._removeWidgetElement = function(e) {
        var t;
        this._widgets.contains(e) && (t = e.getElement(), t.parentNode.parentNode.removeChild(t.parentNode))
    }, t.addFiller = function() {
        var e = this.createCellElement();
        e.className += " DlHbox-filler", this.addClass("DlHbox-hasFiller")
    }, t.setAlign = function(e, t) {
        var n = this.getElement();
        switch (e) {
            case "left":
                n.style.marginLeft = "0", n.style.marginRight = "auto";
                break;
            case "center":
                n.style.marginLeft = "auto", n.style.marginRight = "auto";
                break;
            case "right":
                n.style.marginLeft = "auto", n.style.marginRight = "0";
                break;
            default:
                n.style.marginLeft = e != null ? e : "auto", n.style.marginRight = t != null ? t : "auto"
        }
    }, t.setEqualWidths = function(e) {
        var t = this.children().max(function(e) {
            return e.getSize().x
        });
        e && (t += e), this.children().r_foreach(function(e) {
            e.setSize({x: t})
        })
    }
}), DlPoint.prototype = {clone: function() {
        return new DlPoint(this.x, this.y)
    },normalize: function(e) {
        var t;
        return this.x > e.x && (t = this.x, this.x = e.x, e.x = t), this.y > e.y && (t = this.y, this.y = e.y, e.y = t), this
    },distanceTo: function(e) {
        var t = Math.abs(e.x - this.x), n = Math.abs(e.y - this.y);
        return Math.sqrt(t * t + n * n)
    }}, DlRect.prototype = {setFromRect: function(e) {
        return this.x = e.x, this.y = e.y, this.w = e.w, this.h = e.h, this
    },setFromPoints: function(e, t) {
        return e = e.clone().normalize(t = t.clone()), this.x = e.x, this.y = e.y, this.w = t.x - e.x + 1, this.h = t.y - e.y + 1, this
    },setFromValues: function(e, t, n, r) {
        return this.x = e, this.y = t, this.w = n, this.h = r, this
    },getTL: function() {
        return new DlPoint(this.x, this.y)
    },getBR: function() {
        return new DlPoint(this.x + this.w - 1, this.y + this.h - 1)
    },getPoints: function() {
        return [getTL(), getBR()]
    },height: function(e) {
        return e != null && (this.h = e), this.h
    },width: function(e) {
        return e != null && (this.w = e), this.w
    },containsPoint: function(e) {
        return this.x <= e.x && this.x + this.w > e.x && this.y <= e.y && this.y + this.h > e.y
    },intersect: function(e) {
        var t = null, n = Math, r = n.max(this.x, e.x), i = n.max(this.y, e.y), s = n.min(this.x + this.w, e.x + e.w) - r, o = n.min(this.y + this.h, e.y + e.h) - i;
        return s > 0 && o > 0 && (t = new DlRect(r, i, s, o)), t
    },area: function() {
        return this.w * this.h
    },makeDiv: function(e, t) {
        var n;
        return t || (t = "#000"), e == null && (e = 0), n = {position: "absolute",left: this.x + "px",top: this.y + "px",width: this.w - e * 2 + "px",height: this.h - e * 2 + "px",overflow: "hidden",lineHeight: "1px",fontSize: "1px",border: e + "px solid " + t}, n = DynarchDomUtils.createElement("div", n, {innerHTML: "&nbsp;"}), n
    },positionDiv: function(e) {
        e.style.left = this.x + "px", e.style.top = this.y + "px", e.style.height = this.h + "px", e.style.width = this.w + "px"
    },toString: function() {
        return this.w + "x" + this.h + "@" + this.x + "," + this.y
    }}, DEFINE_CLASS("DlPopup", DlContainer, function(e, t, n) {
    function h(e) {
        this.getScrollDiv().scrollTop += this._scrollStep * e, this._scrollSetArrowState()
    }
    function p(e, t) {
        e._scrollStep = e._oscroll.step1, e._scrollTimer = setInterval(h.$(e, t), e._oscroll.speed), s(this, "DlPopup-scroll-hover"), u(this, t > 0, "DlPopup-scrollDown-hover", "DlPopup-scrollUp-hover")
    }
    function d() {
        a[this._objectType][this._level] = this;
        if (!this._autolink || this._level == 0)
            l[this.id] = this
    }
    function v() {
        var e = a[this._objectType], t = e[this._level + 1];
        t && t.hide(), e.splice(this._level, 1), l[this.id] && delete l[this.id]
    }
    function m(e) {
        var t, n;
        this.args.scrollVisible && (t = this.getScrollDiv(), n = t.scrollTop, e.wheelDelta < 0 ? n += 20 : (n -= 20, n < 0 && (n = 0)), t.scrollTop = n, e.domStop = !0, DlException.stopEventBubbling())
    }
    function g(t) {
        var n = t.getObject(), r = {};
        while (n && !(n instanceof e))
            n.currentPopup && (r[n.currentPopup.id] = !0), n = n.parent;
        if (n)
            while (n != null)
                r[n.id] = !0, n = n._parentPopup;
        e.clearAllPopups(r)
    }
    function y(e) {
        e.onBeforePopup && e.onBeforePopup.call(this, e), this._timer = null;
        if (!this.setContent(e.content))
            return;
        e.onPopup && e.onPopup.call(this, e), this.applyHooks("onPopup", [e]), this.showAt(e.anchor, e.align || this._align, e.pos, e.shift, e.fluid), this._prevFocus = DlEvent.focusedWidget(), this.focus()
    }
    function b() {
        if (this.visible) {
            this.args && this.args.onHide && this.args.onHide.call(this, this.args);
            if (this._focusable && this._prevFocus)
                try {
                    this._prevFocus.focus()
                } catch (e) {
                }
            this.args = null, this._timer = null, this.callHooks("onHide"), this.display(!1), this.visible = !1
        }
    }
    var r, i = n.createElement, s = n.addClass, o = n.delClass, u = n.condClass, a = {}, f = {}, l = {}, c = /DlPopup-scroll(Up|Down)?-hover/g;
    e.BEFORE_BASE = function() {
        this._hasScrolling = !1, this.visible = !1
    }, e.DEFAULT_EVENTS = ["onPopup", "onHide"], e.DEFAULT_ARGS = {_level: ["level", 0],_autolink: ["autolink", !0],_oscroll: ["scroll", {step1: 5,step2: 10,speed: 40}],_align: ["align", null],_zIndex: ["zIndex", 1e3],_focusable: ["focusable", !0]}, t.FINISH_OBJECT_DEF = function() {
        e.BASE.FINISH_OBJECT_DEF.call(this), this.constructor.get = e.get, this.constructor.clearAll = e.clearAll, a[this._objectType] = [], f[this._objectType] = {}
    }, e.get = function(e, t) {
        var n, r, i = this.prototype._objectType, s = !1, o = a[i].length;
        if (e == null || e >= o)
            e = o, s = !0;
        return n = f[i], f[i] || (n = f[i] = {}), r = n[e], r || (t ? r = null : r = n[e] = new this({level: e})), r.detachPopup(), s && e > 0 && r.attachToPopup(a[i].peek()), r
    }, e.clearAll = function(e) {
        var t = a[this.prototype._objectType];
        t = t[e || 0], t && t.hide()
    }, e.clearAllPopups = function(e) {
        var t;
        for (t in l)
            (!e || !e[t]) && l[t].hide()
    }, t._createElement = function() {
        var t, n = this._parent;
        this._parent = null, e.BASE._createElement.call(this), t = this.getElement(), this.parent = n, this.display(!1), document.body.appendChild(t), is_gecko && (t = i("div", null, {className: "Gecko-Bug-302380"}, t)), this.refNode("_contentEl", i("div", null, {className: "DlPopup-scrollArea"}, t)), this.zIndex(this._zIndex)
    }, t.getContentElement = function() {
        return this._contentEl
    }, t.getScrollDiv = t.getContentElement, t._scrollSetArrowState = function() {
        var e = this.getScrollDiv(), t = this._scroll_el(0), n = this._scroll_el(1), r = e.scrollTop == 0, i = e.scrollTop + e.offsetHeight == e.scrollHeight;
        (r || i) && this._scrollStopHandler(), u(t, r, "DlPopup-scrollUp-disabled"), u(n, i, "DlPopup-scrollDown-disabled")
    }, t._scrollStopHandler = function() {
        this._scrollTimer && (clearInterval(this._scrollTimer), this._scrollTimer = null, this._scrollSetArrowState()), o(this._scroll_el(0), c), o(this._scroll_el(1), c)
    }, t._scrollDoubleSpeed = function(e) {
        return this._scrollStep = this._oscroll[e ? "step2" : "step1"], !1
    }, t._scroll_setup = function() {
        var e, t, n;
        this._hasScrolling || (this._hasScrolling = !0, e = this.getElement(), t = i("div", null, {className: "DlPopup-scrollUp"}, e, 0), n = i("div", null, {className: "DlPopup-scrollDown"}, e), t.onmouseover = p.$(null, this, -1), n.onmouseover = p.$(null, this, 1), t.onmouseout = n.onmouseout = this._scrollStopHandler.$(this), t.onmousedown = n.onmousedown = this._scrollDoubleSpeed.$(this, !0), t.onmouseup = n.onmouseup = this._scrollDoubleSpeed.$(this, !1), this.refNode("_scrollEl0", t), this.refNode("_scrollEl1", n), this.getScrollDiv().onscroll = this._scrollSetArrowState.$(this)), this._scroll_visibile(!0)
    }, t._scroll_el = function(e) {
        return this["_scrollEl" + e]
    }, t._scroll_visibile = function(e) {
        this._hasScrolling && (is_gecko && (this.getScrollDiv().parentNode.style.overflow = e ? "auto" : ""), e = e ? "" : "none", this._scroll_el(0).style.display = e, this._scroll_el(1).style.display = e, this.args.scrollVisible = !e)
    }, r = !1, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onPopup: d,onMouseWheel: m,onHide: v}), r || (r = !0, DlEvent.captureGlobal("onMouseDown", g))
    }, t.popup = function(e) {
        this.args = e, this.cancel(), e.timeout ? this._timer = y.$(this, e).delayed(e.timeout) : y.call(this, e)
    }, t.hide = function(e) {
        this.cancel(), e ? this._timer = b.$(this).delayed(e) : b.call(this)
    }, t.cancel = function() {
        this._timer && (clearTimeout(this._timer), this._timer = null)
    }, t.correctPos = Function.noop, t.reposition = function() {
        this.visible && this.showAt(this.args.anchor, this.args.align || this._align, this.args.pos, this.args.shift, this.args.fluid)
    }, t.showAt = function(e, t, r, i, s) {
        var o, u, a, f, l, c, h, p, d, v, m = this.getScrollDiv();
        t || (t = this._align), t == "mouse" ? (r == null && (r = Object.makeCopy(DlEvent.latestMouseEvent.pos)), o = r, this._mouseDiff && (o.x += this._mouseDiff.x, o.y += this._mouseDiff.y), t = {prefer: "__",fallX1: "_R",fallX2: "_L",fallY1: "B_",fallY2: "T_"}) : (o = n.getPos(e), i && (i.x && (o.x += i.x), i.y && (o.y += i.y))), a = n.getOuterSize(e), u = Object.makeCopy(o), this.visibility(!1), m.style.height = "", this._scroll_visibile(!1), this.setPos(-3e4, -3e4), this.display(!0), is_ie && (this.getElement().style.width = ""), f = this._bestPosition(t, u, a), l = f.height(), c = this.getScrollDiv().offsetHeight, h = this.getElement().offsetHeight - c, u = f.getTL(), l < c && (s ? this.children(0).setSize({y: l}) : (is_ie && (this.getElement().style.width = m.offsetWidth + "px"), this._scroll_setup(), p = this._scroll_el(0).offsetHeight, d = this._scroll_el(1).offsetHeight, m.style.height = l - p - d - h + "px", this._scrollSetArrowState(), m.scrollTop = 0)), this.correctPos(u), this.setPos(u.x, u.y), this._parentPopup && (v = this._parentPopup.zIndex() + 1, this.zIndex(v)), this.visibility(!0), this.visible = !0
    }, t._bestPosition = function(e, t, r) {
        var i, s, o, u, a, f, l, c = n.getWindowSize(), h = new DlRect(0, 0, c.x, c.y), p = new DlPoint(t);
        return c = this.getSize(), this._doAlign(e.prefer, p, r), i = (new DlRect(p, c)).intersect(h), s = this.checkXPos(p, h), o = this.checkYPos(p, h), s == 0 && o == 0 ? i : (s != 0 && (p.x = t.x, this._doAlign(e.fallX1, p, r), u = (new DlRect(p, c)).intersect(h), p.x = t.x, this._doAlign(e.fallX2, p, r), a = (new DlRect(p, c)).intersect(h), u && a ? p = u.area() > a.area() ? u.getTL() : a.getTL() : u ? p = u.getTL() : a && (p = a.getTL()), this.args.fallX = !0), o != 0 && (p.y = t.y, this._doAlign(e.fallY1, p, r), f = (new DlRect(p, c)).intersect(h), p.y = t.y, this._doAlign(e.fallY2, p, r), l = (new DlRect(p, c)).intersect(h), f && l ? p = f.area() > l.area() ? f.getTL() : l.getTL() : f ? p = f.getTL() : l && (p = l.getTL()), this.args.fallY = !0), (new DlRect(p, c)).intersect(h))
    }, t._doAlign = function(e, t, n) {
        var r = this.getSize(), i = e.substr(0, 1), s = "";
        e.length > 1 && (s = e.substr(1, 1));
        switch (i) {
            case "T":
                t.y -= r.y, this._mouseDiff && this.args.align == "mouse" && (t.y -= 2 * this._mouseDiff.y);
                break;
            case "B":
                t.y += n.y, this._mouseDiff && this.args.align == "mouse" && (t.y += 2 * this._mouseDiff.y);
                break;
            case "C":
            case "c":
                t.y += (n.y - r.y) / 2;
                break;
            case "t":
                t.y += n.y - r.y;
                break;
            case "b":
        }
        switch (s) {
            case "L":
                t.x -= r.x, this._mouseDiff && this.args.align == "mouse" && (t.x -= 2 * this._mouseDiff.x);
                break;
            case "R":
                t.x += n.x, this._mouseDiff && this.args.align == "mouse" && (t.x += 2 * this._mouseDiff.x);
                break;
            case "C":
            case "c":
                t.x += (n.x - r.x) / 2;
                break;
            case "l":
                t.x += n.x - r.x;
                break;
            case "r":
        }
    }, t.checkXPos = function(e, t) {
        var n, r;
        return e.x < t.x ? e.x - t.x : (n = this.getSize(), r = e.x + n.x - t.x - t.w, r > 0 ? r : 0)
    }, t.checkYPos = function(e, t) {
        var n, r;
        return e.y < t.y ? e.y - t.y : (n = this.getSize(), r = e.y + n.y - t.y - t.h, r > 0 ? r : 0)
    }, t.attachToPopup = function(e) {
        this._parentPopup = e, e._childPopup = this
    }, t.detachPopup = function() {
        this._parentPopup && (this._parentPopup._childPopup = null), this._parentPopup = null
    }, t.getToplevelPopup = function() {
        var e = this;
        while (e._parentPopup)
            e = e._parentPopup;
        return e
    }, t._handle_focusKeys = function(e) {
        var t;
        !e.altKey && !e.ctrlKey && (e.keyCode == DlKeyboard.ESCAPE ? this.hide() : e.keyCode == DlKeyboard.TAB && (t = e.focusedWidget, t = e.shiftKey ? this.getPrevFocusWidget(t) : this.getNextFocusWidget(t), t && t.focus(), e.domStop = !0, DlException.stopEventBubbling())), this._handleKeybinding(e)
    }
}), DEFINE_CLASS("DlVbox", DlBox, function(e, t, n) {
    var r = n.createElement;
    t.createCellElement = function() {
        return r("td", null, {className: "cell"}, r("tr", null, {className: "row"}, this._tbody))
    }, t._removeWidgetElement = function(e) {
        var t;
        this._widgets.contains(e) && (t = e.getElement(), t.parentNode.parentNode.parentNode.removeChild(t.parentNode.parentNode))
    }
}), DEFINE_CLASS("DlPopupMenu", DlPopup, function(e, t) {
    function n() {
        var e;
        this.cancel(), e = this.args
    }
    e.CONSTRUCT = function() {
        this._mouseDiff = {x: 2,y: 1}
    }, e.onBeforePopup = function(e) {
        var t;
        e.isContext ? e.widget.activateSubmenu && e.widget.activateSubmenu(!0) : (e.item.currentPopup = this, e.item._popupVisible = !0, e.menu._popupVisible = !0, e.item.activateSubmenu(!0)), t = e.content, t instanceof Function && (e.content = t = t.call(this)), t.parentMenu = e.isContext ? e.widget : e.menu, t instanceof DlWidget && t.hasHooks("onPopup") && t.applyHooks("onPopup", [e])
    }, e.onHide = function(e, t) {
        var n;
        e.isContext ? e.widget.activateSubmenu && e.widget.activateSubmenu(!1) : (e.item.activateSubmenu(!1), e.item.currentPopup = null, e.item._popupVisible = !1, e.menu._popupVisible = !1, n = e.content, n instanceof DlWidget && n.hasHooks("onHide") && n.applyHooks("onHide", [e]), n.parentMenu = null), t && t.call(this, e)
    }, t.popup = function(t) {
        t.onBeforePopup || (t.onBeforePopup = e.onBeforePopup), t.onHide ? t.onHide = e.onHide.$(this, t, t.onHide) : t.onHide = e.onHide, e.BASE.popup.call(this, t)
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onMouseEnter: n})
    }, t.correctPos = function(e) {
        var t, n, r = this.args;
        try {
            !r.isContext && !r.scrollVisible && (t = r.menu, t && t instanceof DlVMenu && (n = r.content._widgets[0].getPos().y - r.content.parent.getElement().offsetTop, Math.abs(n) < 50 && (r.fallY ? e.y += n : e.y -= n)))
        } catch (i) {
        }
    }
}), DEFINE_CLASS("DlHMenu", DlHbox, function(e) {
    e.CONSTRUCT = DlMenuBase, e.DEFAULT_EVENTS = ["onSelect", "onPopup"]
}), DEFINE_CLASS("DlVMenu", DlVbox, function(e) {
    e.CONSTRUCT = DlMenuBase, e.DEFAULT_EVENTS = ["onSelect", "onPopup"]
}), DEFINE_CLASS("DlMenuItem", DlContainer, function(e, t, n) {
    function r() {
        var e;
        this.scrollIntoView(), this.addClass("DlMenuItem-hover", "DlMenuItem-active"), e = this._inBaseMenu();
        if (!e || this.parentMenu._popupVisible)
            this._menu ? this._popupMenu(e ? 0 : 250) : e && this._getDlPopup().hide(100)
    }
    function i() {
        var e;
        this.delClass("DlMenuItem-hover"), this.delClass("DlMenuItem-active"), e = this._inBaseMenu(), e || this._getDlPopup().hide(100)
    }
    function s() {
        var e;
        this.delClass("DlMenuItem-active"), this.hasHooks("onSelect") && (e = DlPopupMenu.get(0).args, this._noClose || DlPopupMenu.clearAll(), this.applyHooks.delayed(1, this, "onSelect", [this.name, this, e]))
    }
    function o() {
        this.addClass("DlMenuItem-active"), this._popupMenu(0), DlException.stopEventBubbling()
    }
    function u() {
        this._menu instanceof DlWidget && (this._menu.destroy(), this._menu = null)
    }
    e.CONSTRUCT = function() {
        this.parentMenu || (this.parentMenu = this.parent)
    }, e.DEFAULT_EVENTS = ["onSelect"], e.DEFAULT_ARGS = {label: ["label", "DlMenuItem"],_iconClass: ["iconClass", null],_noClose: ["noClose", !1],parentMenu: ["menu", null],name: ["name", null],__itemId: ["id", null]}, t._inBaseMenu = function() {
        return !this.parentMenu.parentMenu
    }, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getElement(), t.innerHTML = '<div class="div1"><div class="div2"></div></div>', this.setIconClass(this._iconClass), this._iconClass = null, this.setUnselectable(), this.label && this.setContent(this.label)
    }, t.getContentElement = function() {
        return this.getElement().firstChild.firstChild
    }, t._getDlPopup = function() {
        return this.parentMenu._getDlPopup()
    }, t._popupMenu = function(e) {
        var t, n;
        this._menu && !this._popupVisible && (t = this.parentMenu, n = this._getDlPopup(), n.visible && n.hide(), n.popup({timeout: e,content: this.getMenu(),anchor: this.getElement(),align: t._popupAlign,item: this,menu: this.parentMenu,onPopup: this._onPopup,onHide: this._onHide}))
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onMouseEnter: r,onMouseLeave: i,onMouseDown: o,onMouseUp: s,onDestroy: u})
    }, t.getMenu = function() {
        return this._menu
    }, t.setMenu = function(e, t, r) {
        this._menu instanceof DlWidget && this._menu.destroy(), e instanceof DlWidget && e.ref(), this._menu = e, this._onPopup = t, this._onHide = r, n.condClass(this.getElement().firstChild, e, "DlMenuItem-withPopup")
    }, t.activateSubmenu = function(e) {
        this.condClass(e, "DlMenuItem-popped")
    }
}), DEFINE_CLASS("DlButtonMenu", DlHbox, function(e, t) {
    e.CONSTRUCT = DlMenuBase, e.DEFAULT_EVENTS = ["onSelect", "onPopup", "onHide", "onClick"], e.DEFAULT_ARGS = {label: ["label", null],_iconClass: ["iconClass", null],_shiftMenu: ["shiftMenu", null],_connected: ["connected", !1]}, t.ALIGN = {prefer: "Br",fallX1: "_r",fallX2: "_l",fallY1: "B_",fallY2: "T_"}, t.activateSubmenu = function(e) {
        this._mainBtn.condClass(e, "DlButton-1"), this._menuBtn.condClass(e, "DlButton-1")
    }, t._createElement = function() {
        e.BASE._createElement.call(this), this._mainBtn = new DlButton({parent: this,focusable: !1,label: this.label,className: "LabelPart",noCapture: this._connected,iconClass: this._iconClass}), this._mainBtn.connectEvents("onClick", this), this._menuBtn = new DlButton({parent: this,focusable: !1,label: "&nbsp;",className: "MenuArrow",noCapture: !0}), this._menuBtn.getElement().parentNode.style.width = "3px", this._menuBtn.getContentElement().className = "MenuArrow-inner", this._connected && this._mainBtn.connectEvents("onMouseDown", this._menuBtn), this._mainBtn.connectEvents(["onMouseEnter", "onMouseLeave"], this._menuBtn), this._menuBtn.addEventListener("onMouseDown", this._do_popupMenu.$(this)), this.addEventListener("onDestroy", this.setMenu.$(this, null))
    }, t._do_popupMenu = function(e) {
        var t;
        this._popupVisible || (t = this._getContextMenuPopup(), t.popup({timeout: 0,content: this.getMenu(),align: this.ALIGN,anchor: this.getTableElement(),isContext: !0,widget: this,shift: this._shiftMenu,onHide: this.callHooks.$(this, "onHide")}), e instanceof DlEvent && (e._justFocusedWidget = t), this.callHooks("onPopup"))
    }, t.getMenu = function() {
        return this._menu
    }, t.getButton = function() {
        return this._mainBtn
    }, t.getArrow = function() {
        return this._menuBtn
    }, t.setMenu = function(e) {
        this._menu instanceof DlWidget && this._menu.destroy(), e instanceof DlWidget && e.ref(), this._menu = e
    }
}), DlElementCache = {get: function(e) {
        return this[e].cloneNode(!0)
    }}, function() {
    var e = DynarchDomUtils.createElement, t = Dynarch.ID, n = DlElementCache;
    (function() {
        var t = document.createDocumentFragment();
        e("td", null, null, e("tr", null, null, e("tbody", null, null, t))), n.TBODY_RC = t
    })(), function() {
        var t, r = e("tr"), i = e("td", null, null, r);
        6..times(function() {
            r.appendChild(i.cloneNode(!0))
        }), n.CAL_HEAD = e("thead"), n.CAL_HEAD.appendChild(r.cloneNode(!0)), t = n.CAL_BODY = e("tbody"), 6..times(function() {
            t.appendChild(r.cloneNode(!0))
        })
    }(), n.DRAGGING_LINE = e("div", null, {className: "DlResizeBar-DraggingLine"})
}(), DEFINE_CLASS("DlCalendar", DlWidget, function(e, t, n) {
    function a() {
        var e;
        return r || (r = new DlVMenu({}), r.setStyle("textAlign", "center"), new DlMenuItem({parent: r,label: DlTEXTS.goToday,name: "today",noClose: !0}), r.addSeparator(), e = new DlSpinner({parent: r,size: 4}), e.addEventListener("onChange", function() {
            e.validationError || r.calendar.setYear(e.getValue())
        }), e.getElement().align = "center", r.addSeparator(), 12..times(function(e) {
            new DlMenuItem({parent: r,label: Date.getMonthName(e),name: e,noClose: !0})
        }), r.addEventListener("onPopup", function(t) {
            this.calendar = t.widget, e.setValue(this.calendar.date.getFullYear()), e.focus.$(e).delayed(5)
        }), r.addEventListener("onSelect", function(t) {
            t == "today" ? this.calendar.setToday() : (this.calendar.setMonth(t), this.calendar.setYear(e.getValue())), r.getParent(DlPopup).hide()
        })), r
    }
    function f(e) {
        var t;
        this._clearTimer(), t = e.getParentElement("td", this);
        if (t) {
            this._currentHover && (o(this._currentHover, "hover"), o(this._currentHover, "rolling"), this._currentHover = null, DlWidget.getTooltip().hide());
            if (t._navType != null && this._navDisabled)
                return;
            if (t._otherMonth && this._omDisabled)
                return;
            if (t._firstDay != null && this.fixedFirstDay || t._week != null)
                return;
            if (t.disabled)
                return;
            s(t, "hover"), this._currentHover = t, this.__tooltip && this._popupTooltip()
        }
    }
    function l() {
        this._clearTimer(), this._currentHover && (o(this._currentHover, "hover"), o(this._currentHover, "rolling"), this._currentHover = null)
    }
    function c(e) {
        var t, n, r;
        f.call(this, e), t = e.getParentElement("td", this);
        if (!t)
            return;
        if (!(e.button == 0 || t._navType == null && !t._otherMonth))
            return;
        if (t._otherMonth && this._omDisabled || t.disabled)
            return;
        t._navType != null && e.dl_type == "onMouseDown" ? this._navDisabled || this._navCellClicked(t, t._navType != 0 ? 350 : 0, e) : t._year != null && e.dl_type == "onMouseUp" ? (n = this.date, n.setFullYear(t._year, t._month, t._iday), r = this._selectedDate, this._selectedDate = t._iday, t._otherMonth ? (this.init(), this.applyHooks("onSelect", [!1, !0, !1, n])) : r != this._selectedDate ? this._selectCell(t, !0) : this.applyHooks("onSelect", [!1, !1, !0, n])) : t._firstDay != null && !this.fixedFirstDay && e.button == 0 && e.dl_type == "onMouseDown" && (this.firstDay = t._firstDay, this._displayCalendar())
    }
    function h() {
        var e, t = this.args.widget, n = t._currentHover;
        return n && (e = n._info), n = t._cal_tooltip, n ? n.call(t, e) : e ? e.tooltip : null
    }
    var r, i = n.createElement, s = n.addClass, o = n.delClass, u = n.condClass;
    e.FIXARGS = function(e) {
        e.tagName = "table", this._dayNamesOn = -1, this._selectedDate = e.date && e.selected ? e.date.getDate() : 0
    }, e.CONSTRUCT = function() {
        this._noinit || this.init()
    }, e.DEFAULT_EVENTS = ["onSelect", "onChange", "onRendered"], e.DEFAULT_ARGS = {firstDay: ["firstDay", Date.getFirstDayOfWeek()],fixedFirstDay: ["fixedFirstDay", !0],_weekNumbers: ["weekNumbers", !1],date: ["date", null],selected: ["selected", !0],_navigation: ["navigation", 2],_navDisabled: ["navDisabled", !1],_omDisabled: ["omDisabled", !1],_noinit: ["noinit", !1],_withMenu: ["withMenu", !1],_disableHandler: ["disableHandler", Function.returnFalse],_cal_tooltip: ["dayTooltip", null],_infoDates: ["infoDates", null],__tooltip: ["tooltip", h]}, t._createElement = function() {
        var t, n, r, s, o, u, a, f, l, c;
        e.BASE._createElement.call(this), o = this.getElement(), u = DlElementCache.get("CAL_BODY"), o.cellSpacing = o.cellPadding = o.border = 0, o.appendChild(DlElementCache.get("CAL_HEAD")), o.appendChild(u);
        if (this._weekNumbers) {
            t = o.getElementsByTagName("tr");
            for (n = t.length; --n >= 0; )
                r = t[n], s = i("td", null, {className: "DlCalendar-WeekNumber"}), r.insertBefore(s, r.firstChild)
        }
        r = i("tr"), s = i("td", null, null, r), r.className = "DlCalendar-Navigation", this._navigation == 0 ? (s.colSpan = this.getNCols(), this._makeNavPart(s, 0)) : (a = i("td", null, null, r), f = i("td", null, null, r), this._navigation == 1 ? (a.colSpan = this.getNCols() - 2, this._makeNavPart(a, 0, s, -1, f, 1)) : this._navigation == 2 && (l = i("td", null, null, r), c = i("td", null, null, r), f.colSpan = this.getNCols() - 4, this._makeNavPart(f, 0, s, -2, a, -1, l, 1, c, 2))), n = o.rows[0], n.parentNode.insertBefore(r, n), this.setUnselectable(), this._withMenu && this._createQuickNavMenu()
    }, r = null, t._createQuickNavMenu = function() {
        this.setContextMenu(a)
    }, t._makeNavPart = function() {
        var e, t, n;
        for (n = 0; n < arguments.length; ++n) {
            e = arguments[n++], t = arguments[n], e._navType = t;
            switch (t) {
                case -2:
                    e.className = "PrevYear";
                    break;
                case -1:
                    e.className = "PrevMonth";
                    break;
                case 0:
                    e.className = "Month", this.refNode("_monthTD", e);
                    break;
                case 1:
                    e.className = "NextMonth";
                    break;
                case 2:
                    e.className = "NextYear"
            }
        }
    }, t.getNCols = function() {
        return this._weekNumbers ? 8 : 7
    }, t.getTableElement = function() {
        return this.getElement()
    }, t._displayDayNames = function() {
        var e, t = new Date, n = t.getDay(), r = this.getTableElement().getElementsByTagName("tr")[1], i = this._weekNumbers ? 1 : 0, s = this.firstDay;
        r.className = "DlCalendar-DayNames";
        while (e = r.cells[i++])
            e._firstDay = s % 7, u(e, s == n, "Today"), e.innerHTML = Date.getDayName(s++, !0), u(e, Date.isWeekend(e._firstDay), "WeekEnd");
        this._dayNamesOn = this.firstDay, this._weekNumbers && (e = r.cells[0], e.innerHTML = "w", e._week = -1, e.className = "WeekNumber")
    }, t._displayCalendar = function() {
        var e, t, n, r, i, s, o, u, a, f, l, c, h, p, d, v, m, g, y = new Date, b = y.getFullYear(), w = y.getMonth(), E = y.getDate();
        this._selectedTD = null, this._dayNamesOn != this.firstDay && this._displayDayNames(), e = new Date(this.date), e.setHours(12), t = e.getMonth(), n = e.getDate(), r = e.getFullYear(), i = e.getMonthDays(), this._monthTD.innerHTML = String.buffer("<b>", Date.getMonthName(t, this._navigation == 2), "</b> ", r).get(), e.setDate(1), s = (e.getDay() - this.firstDay) % 7, s < 0 && (s += 7), e.setDate(-s), e.setDate(e.getDate() + 1), o = this.getTableElement().rows[2], u = this._cells = [], a = this._displayedInterval = {};
        for (l = 0; l < 6; ++l, o = o.nextSibling) {
            o.className = "Dates", c = o.firstChild, this._weekNumbers && (c.className = "WeekNumber", c.innerHTML = c._week = e.getWeekNumber(), c = c.nextSibling);
            for (p = 0; p < 7; ++p, c = c.nextSibling, e.setDate(h + 1))
                d = e.getDay(), v = [], c._iday = h = e.getDate(), c._month = e.getMonth(), c._year = e.getFullYear(), c._info = null, f = {y: c._year,m: c._month,d: c._iday}, this._infoDates && (m = f.y + "-" + (1 + f.m).zeroPad(2) + "-" + f.d.zeroPad(2), g = this._infoDates[m], g && (c._info = g, v.push(g.className || "DlCalendar-infoDay"))), a.start || (a.start = f), (c._otherMonth = t != f.m) ? (v.push("OtherMonth"), u[h + (h > 15 ? 100 : 200)] = c) : (t == w && h == E && b == f.y && v.push("Today"), this._selectedDate == h && (this._selectCell(c), v.push("Selected")), u[h] = c), (d == 0 || d == 6) && v.push("WeekEnd"), c.disabled = this._disableHandler(e, v, c), c.innerHTML = this.getDayHTML(h), c.className = v.join(" ")
        }
        a.end = f, this.applyHooks("onRendered", [this])
    }, t.getDayHTML = Function.identity, t.getDateCell = function(e) {
        return this._cells[e]
    }, t.getDisplayedInterval = function() {
        return this._displayedInterval
    }, t.selectDate = function(e, t) {
        e instanceof Date && (e.dateEqualsTo(this.date, !0) || (this.date = new Date(e), this.init()), e = e.getDate()), this._selectCell(this.getDateCell(e), !t)
    }, t.clearSelection = function() {
        this._selectedDate = null, this._initialized && this._displayCalendar()
    }, t.setYear = function(e) {
        e != this.date.getFullYear() && (this.date.setFullYear(e), this.init())
    }, t.setMonth = function(e) {
        e != this.date.getMonth() && (this.date.setMonth(e), this.init())
    }, t.setToday = function() {
        var e = new Date;
        this._selectedDate = 0, this.date = e, this.init()
    }, t._navCellClicked = function(e, t, n) {
        var r, i;
        s(e, "rolling"), this._selectedDate = 0, r = this.date, e._navType != 0 && r.setDate(1);
        switch (e._navType) {
            case 0:
                if (this._withMenu)
                    this.applyHooks("onContextMenu", [n]);
                else {
                    i = new Date;
                    if (r.dateEqualsTo(i, !0))
                        return;
                    this.date = i
                }
                break;
            case -2:
                r.setFullYear(r.getFullYear() - 1);
                break;
            case -1:
                r.setMonth(r.getMonth() - 1);
                break;
            case 1:
                r.setMonth(r.getMonth() + 1);
                break;
            case 2:
                r.setFullYear(r.getFullYear() + 1)
        }
        this.init(), this.applyHooks("onChange", [e._navType]), this.applyHooks("onSelect", [!0, e._navType, null, r]), t && e._navType != 0 && (++this._timerStep, this._timer = setTimeout(this._navCellClicked.$(this, e, this._timerStep > 4 ? 50 : 100), t))
    }, t._clearTimer = function() {
        this._timer && clearTimeout(this._timer), this._timer = null, this._timerStep = 0
    }, t._selectCell = function(e, t) {
        this._selectedDate = e._iday, this._selectedTD && (o(this._selectedTD, "Selected"), o(this._selectedTD.parentNode, "Selected")), this._selectedTD = e, s(e, "Selected"), s(e.parentNode, "Selected"), o(e, "hover"), t && this.applyHooks("onSelect", [!1, !1, !1, this.date])
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onMouseOver: f,onMouseLeave: l,onMouseUp: c,onMouseDown: c})
    }, t.setInfoDates = function(e) {
        this._infoDates = e, this._initialized && this._displayCalendar()
    }, t.init = function() {
        this.date || (this.date = new Date), this._displayCalendar(), this._initialized = !0
    }, t.setSize = t.setOuterSize = function(t) {
        e.BASE.setOuterSize.call(this, {x: t.x != null ? t.x + 2 : null,y: t.y})
    }
}), DEFINE_CLASS("DlButtonCalendar", DlButtonMenu, function(e, t) {
    function n(e, t) {
        t || (this.getButton().setContent(e.date.print(this.dateFormat)), DlPopup.clearAllPopups(), this.date = new Date(e.date), this.applyHooks("onSelect", [this.date]))
    }
    function r(e) {
        var t;
        this.date instanceof Date && (t = this.getCalendar(), t.date.dateEqualsTo(this.date) || (t.date = new Date(this.date), t._selectedDate = this.date.getDate(), t.init())), this.getArrow().applyHooks("onMouseDown", [e])
    }
    e.DEFAULT_ARGS = {dateFormat: ["dateFormat", "%Y/%m/%d"],_calendarArgs: ["calendar", {}],_iconClass: ["iconClass", "IconCalendar"],date: ["date", "Select date..."]}, e.DEFAULT_EVENTS = ["onSelect", "onCalendarRendered"], t.getCalendar = function() {
        var e = this._calendar;
        return e || (this._calendarArgs.noinit = !0, e = this._calendar = new DlCalendar(this._calendarArgs), this.addEventListener("onDestroy", e.destroy.$(e)), e.addEventListener("onSelect", n.$(this, e)), e.connectEvents("onRendered", this, "onCalendarRendered"), this.date instanceof Date && (e.date = new Date(this.date)), e.init()), this._calendar
    }, t.initDOM = function() {
        var t;
        e.BASE.initDOM.call(this), t = this.getButton(), this.date instanceof Date ? t.label(this.date.print(this.dateFormat)) : t.label(this.date), this.setMenu(this.getCalendar.$(this)), t.addEventListener("onClick", r.$(this))
    }, t.getValue = function() {
        return this.date instanceof Date ? this.date : null
    }
}), DEFINE_CLASS("DlButtonColorPicker", DlButtonMenu, function(e, t, n) {
    function r(e, t, n, r, i) {
        var s = e.getColorElement();
        s.style.backgroundColor = r, e.hsv = Array.$(n), e._updateValues(), DlPopup.clearAllPopups(), e.applyHooks("onSelect", [t, n, r, i])
    }
    function i(e, t, n, r) {
        e.getColorElement().style.backgroundColor = r
    }
    function s(e, t) {
        var n, r;
        e.hsv && (n = [t, e.hsv[1], e.hsv[2]], r = DlColor.RGB2color(DlColor.HSV2RGB(n)), e.getColorElement().style.backgroundColor = r, e.hsv = n, e._updateValues())
    }
    function o(e) {
        var t = e.getColorElement();
        t.style.backgroundColor = e.rgb ? DlColor.RGB2color(e.rgb) : ""
    }
    function u() {
        this._cp.addEventListener(this._events_cp), this.hsv && this._cp.setHSV(this.hsv)
    }
    function a() {
        this._cp.removeEventListener(this._events_cp)
    }
    e.CONSTRUCT = function() {
        this._events_cp = {onSelect: r.$(null, this),onHueChange: s.$(null, this),onHover: i.$(null, this),onHoverOut: o.$(null, this)}, this._updateValues()
    }, e.DEFAULT_ARGS = {rgb: ["rgb", null],hsv: ["hsv", null],color: ["color", null]}, t._updateValues = function() {
        this.hsv ? (this.rgb = DlColor.HSV2RGB(this.hsv), this.color = DlColor.RGB2color(this.rgb)) : this.rgb ? (this.hsv = DlColor.RGB2HSV(this.rgb), this.color = DlColor.RGB2color(this.rgb)) : this.color && (this.rgb = DlColor.color2RGB(this.color), this.hsv = DlColor.RGB2HSV(this.rgb))
    }, t.setColorPicker = function(e) {
        this._cp = e, this.setMenu(e), this.hsv && (e.setHSV(this.hsv), r.call(e, this, this.rgb, this.hsv, DlColor.RGB2color(this.rgb), DlColor.RGBrightness(this.rgb)))
    }, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getButton().setContent(["<table cellspacing='0' cellpadding='0'><tr><td>", "<div unselectable='on' class='ColorPart'>&nbsp;</div>", "</td><td></td></tr></table>"].join("")), this.setLabel(this.label)
    }, t.setLabel = function(e) {
        var t = this.getLabelElement();
        t.innerHTML = e || "", n.condClass(t, e, "Label", "EmptyLabel")
    }, t.getColorElement = function() {
        return this.getButton().getContentElement().firstChild.rows[0].cells[0].firstChild
    }, t.getLabelElement = function() {
        return this.getButton().getContentElement().firstChild.rows[0].cells[1]
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onPopup: u,onHide: a})
    }
}), DEFINE_CLASS("DlCanvas", DlContainer, function(e, t) {
    function o(e) {
        return function(t) {
            var n = t.computePos(this), r = this.getContext(), i = [n.x, n.y, r, this, t];
            return e.call(this, t, n, r, i)
        }
    }
    function u(e) {
        function t(t, n, r, i, s) {
            var o;
            e.dragging = !0, e.ctx = r, e.cw = i, o = e.getPos(), e._dragDiff = {x: o.x - t,y: o.y - n}, DlEvent.captureGlobals(e._dragHandlers), s.domStop = !0
        }
        function n() {
            DlEvent.releaseGlobals(e._dragHandlers), e.dragging = !1, e.cw.refresh(), e.cw = null, e.ctx = null, e._dragPos = null
        }
        function r(t) {
            var n = t.computePos(e.cw);
            n = {x: n.x + e._dragDiff.x,y: n.y + e._dragDiff.y}, e.setPos(n.x, n.y), e.applyHooks("onMove", [n]), e.cw.refresh(), s()
        }
        e.registerEvents(["onMove"]), e._dragHandlers = {onMouseMove: r,onMouseUp: n,onMouseOver: s
            ,onMouseOut: s,onMouseEnter: s,onMouseLeave: s}, e.addEventListener({onMouseDown: t})
    }
    function a(e) {
        function r() {
            s(e, "TL", function() {
                return [e.left(), e.top()]
            }), s(e, "T", function() {
                return [e.hcenter(), e.top()]
            }), s(e, "TR", function() {
                return [e.right(), e.top()]
            }), s(e, "L", function() {
                return [e.left(), e.vcenter()]
            }), s(e, "R", function() {
                return [e.right(), e.vcenter()]
            }), s(e, "BL", function() {
                return [e.left(), e.bottom()]
            }), s(e, "B", function() {
                return [e.hcenter(), e.bottom()]
            }), s(e, "BR", function() {
                return [e.right(), e.bottom()]
            })
        }
        function i() {
            Object.foreach(e._handles, function(e) {
                e.update()
            })
        }
        function s(e, r, i) {
            var s = i(), o = new n(s[0], s[1]);
            return o.update = function() {
                var e = i();
                this.setPos(e[0], e[1])
            }, e._handles[r] = o, o.addEventListener("onMove", t[r]), o
        }
        var t;
        e._handles = {}, u(e), e.addEventListener({onActivate: function(e) {
                e ? r() : (Array.hashKeys(this._handles).map("destroy"), this._handles = {})
            },onMove: function() {
                i()
            }}), e.handles = function() {
            return Array.hashValues(this._handles)
        }, e.activable = function() {
            return !0
        }, t = {TL: function(t) {
                e.left(t.x), e.top(t.y), i()
            },T: function(t) {
                e.top(t.y), i()
            },TR: function(t) {
                e.right(t.x), e.top(t.y), i()
            },L: function(t) {
                e.left(t.x), i()
            },R: function(t) {
                e.right(t.x), i()
            },BL: function(t) {
                e.left(t.x), e.bottom(t.y), i()
            },B: function(t) {
                e.bottom(t.y), i()
            },BR: function(t) {
                e.right(t.x), e.bottom(t.y), i()
            }}
    }
    function f(e, t) {
        return t.zIndex() - e.zIndex()
    }
    var n, r = 1e5, i = "onMouseMove onMouseDown onMouseUp onMouseEnter onMouseLeave onClick", s = DlException.stopEventBubbling;
    e.DEFAULT_ARGS = {width: ["width", 100],height: ["height", 100]}, e.CONSTRUCT = function() {
        this._elements = [], this._activeEl = null, this._noUpdates = 0
    }, t.setMouseListeners = function() {
        i.qw().foreach(function(e) {
            this[e] instanceof Function && this.addEventListener(e, this[e])
        }, this)
    }, t._createElement = function() {
        e.BASE._createElement.apply(this, arguments), this.setContent("<canvas width='" + this.width + "' height='" + this.height + "'></canvas>")
    }, t.getCanvas = function() {
        return this.getContentElement().firstChild
    }, t.getContext = function() {
        return this._context || this.refNode("_context", this.getCanvas().getContext("2d"))
    }, t.withContext = function(e) {
        e(this.getContext())
    }, t.withSavedContext = function(e) {
        this.getContext().save();
        try {
            return e(this.getContext())
        }finally {
            this.getContext().restore()
        }
    }, t.setSize = function(e) {
        this.getCanvas().width = e.x, this.getCanvas().height = e.y, this.refresh(), this.callHooks("onResize")
    }, t.add = function(e) {
        this._elements.push(e), this.refresh()
    }, t.clear = function() {
        var e = this.getCanvas();
        this.getContext().clearRect(0, 0, e.width, e.height)
    }, t.refresh = function() {
        this._noUpdates == 0 && (this.clear(), this.getSortedElements().reverse().foreach(this.renderElement, this))
    }, t.getSortedElements = function() {
        var e = this._elements.mergeSort(f);
        return this._activeEl && e.unshift.apply(e, this._activeEl.handles()), e
    }, t.renderElement = function(e) {
        var t = this.getContext();
        t.save(), e.render(t, this), t.restore()
    }, t.withNoUpdates = function(e) {
        ++this._noUpdates;
        try {
            return e()
        }finally {
            --this._noUpdates
        }
    }, t.onMouseMove = o(function(e, t, n, r) {
        this.getSortedElements().foreach(function(e) {
            e.pointInside(t, n) ? (e.__mouseInside || (e.__mouseInside = !0, e.applyHooks("onMouseEnter", r)), e.applyHooks("onMouseMove", r)) : e.__mouseInside && (e.__mouseInside = !1, e.applyHooks("onMouseLeave", r))
        }, this)
    }), t.onMouseLeave = o(function(e, t, n, r) {
        this.getSortedElements().foreach(function(e) {
            e.__mouseInside && (e.__mouseInside = !1, e.applyHooks("onMouseLeave", r))
        }, this)
    }), t.onMouseDown = o(function(t, n, r, i) {
        var s = !1;
        this.getSortedElements().foreach(function(t) {
            t.pointInside(n, r) && (t instanceof e.Handle ? s = !0 : !s && t.activable() && (t !== this._activeEl && (this._activeEl && this._activeEl.applyHooks("onActivate", [!1]), t.applyHooks("onActivate", [!0]), this._activeEl = t), s = !0), t.applyHooks("onMouseDown", i), $BREAK())
        }, this), s ? this.refresh() : this._activeEl && (this._activeEl.applyHooks("onActivate", [!1]), this._activeEl = null, this.refresh())
    }), t.onMouseUp = o(function(e, t, n, r) {
        this.getSortedElements().foreach(function(e) {
            e.pointInside(t, n) && e.applyHooks("onMouseUp", r)
        }, this)
    }), t.onClick = o(function(e, t, n, r) {
        this.getSortedElements().foreach(function(e) {
            e.pointInside(t, n) && e.applyHooks("onClick", r)
        }, this)
    }), e.make_movable = u, e.make_resizable = a, e.Element = DEFINE_CLASS(null, DlEventProxy, function(e, t) {
        e.CONSTRUCT = function() {
            this._zIndex = 0
        }, e.DEFAULT_EVENTS = (i + " onActivate").qw(), t.pointInside = function(e, t) {
            return t.save(), this.setMyPath(t), t.restore(), t.isPointInPath(e.x, e.y)
        }, t.handles = function() {
            return []
        }, t.activable = function() {
            return !1
        }, t.setClipPath = function(e) {
            this.setMyPath(e)
        }, t.zIndex = function(e) {
            return e != null && (this._zIndex = e), this._zIndex
        }
    }), e.Rect = DEFINE_CLASS(null, e.Element, function(e, t) {
        e.CONSTRUCT = function(e, t, n, r) {
            this._p1 = new DlPoint(e, t), this._p2 = new DlPoint(e + n, t + r), this.normalize()
        }, t.normalize = function() {
            this._p1.normalize(this._p2)
        }, t.rectangle = function() {
            return new DlRect(this._p1, this._p2)
        }, t.left = function(e) {
            return e != null && (this._p1.x = e, this.normalize()), this._p1.x
        }, t.top = function(e) {
            return e != null && (this._p1.y = e, this.normalize()), this._p1.y
        }, t.right = function(e) {
            return e != null && (this._p2.x = e, this.normalize()), this._p2.x
        }, t.bottom = function(e) {
            return e != null && (this._p2.y = e, this.normalize()), this._p2.y
        }, t.hcenter = function() {
            return (this.left() + this.right()) / 2
        }, t.vcenter = function() {
            return (this.top() + this.bottom()) / 2
        }, t.width = function() {
            return Math.abs(this._p2.x - this._p1.x)
        }, t.height = function() {
            return Math.abs(this._p2.y - this._p1.y)
        }, t.getPos = function() {
            return this._p1
        }, t.setPos = function(e, t) {
            var n, r;
            e != null && (n = e - this._p1.x, this._p1.x = e, this._p2.x += n), t != null && (r = t - this._p1.y, this._p1.y = t, this._p2.y += r)
        }, t.setMyPath = function(e) {
            var t, n, r, i;
            e.beginPath(), e.translate(this.hcenter(), this.vcenter()), t = this.width(), n = this.height(), r = t / 2, i = n / 2, e.rect(-r, -i, t, n), e.closePath()
        }, t.render = function(e) {
            e.fillStyle = "#ffffff", e.strokeStyle = "#000000", this.setMyPath(e), e.fill(), e.stroke()
        }
    }), e.Ellipse = DEFINE_CLASS(null, e.Rect, function(e, t) {
        function n(e, t, n, r, i) {
            var s = .5522848, o = r / 2 * s, u = i / 2 * s, a = t + r, f = n + i, l = t + r / 2, c = n + i / 2;
            e.moveTo(t, c), e.bezierCurveTo(t, c - u, l - o, n, l, n), e.bezierCurveTo(l + o, n, a, c - u, a, c), e.bezierCurveTo(a, c + u, l + o, f, l, f), e.bezierCurveTo(l - o, f, t, c + u, t, c)
        }
        t.setMyPath = function(e) {
            var t, r, i, s;
            e.beginPath(), e.translate(this.hcenter(), this.vcenter()), t = this.width(), r = this.height(), i = t / 2, s = r / 2, n(e, -i, -s, t, r), e.closePath()
        }
    }), n = e.Handle = DEFINE_CLASS(null, e.Element, function(e, t) {
        var n = "rgba(0, 0, 0, 0.5)", i = "#5500ff", s = "rgba(255, 0, 0, 0.5)";
        e.CONSTRUCT = function(e, t, n) {
            var r = this;
            u(r), r._point = new DlPoint(e, t), r._size = n || 4, r.addEventListener({onMouseEnter: function(e, t, n, o) {
                    o.withSavedContext(function(e) {
                        e.strokeStyle = i, e.fillStyle = s, r.setMyPath(e), e.fill(), e.stroke()
                    })
                },onMouseLeave: function(e, t, n, i) {
                    i.withSavedContext(function(e) {
                        r.setClipPath(e), e.clip(), i.refresh()
                    })
                }})
        }, t.setMyPath = function(e) {
            e.beginPath(), e.arc(this._point.x, this._point.y, this._size, 0, 2 * Math.PI, !0), e.closePath()
        }, t.setClipPath = function(e) {
            e.beginPath(), e.rect(this._point.x - this._size - 1, this._point.y - this._size - 1, this._size * 2 + 2, this._size * 2 + 2), e.closePath()
        }, t.render = function(e) {
            e.fillStyle = this.dragging ? i : n, this.setMyPath(e), e.fill()
        }, t.zIndex = function() {
            return r
        }, t.setPos = function(e, t) {
            e != null && (this._point.x = e), t != null && (this._point.y = t)
        }, t.getPos = function() {
            return this._point
        }
    })
}), DEFINE_CLASS("DlCheckbox", DlAbstractButton, function(e) {
    e.DEFAULT_ARGS = {_classes: ["classes", {active: "DlCheckbox-active",hover: "DlCheckbox-hover",checked: "DlCheckbox-1",unchecked: "DlCheckbox-0",empty: "DlCheckbox-empty",disabled: "DlCheckbox-disabled"}]}, e.FIXARGS = function(e) {
        e.type = DlButton.TYPE.TWOSTATE
    }
}), DlColor = {RGB2HSV: function(e) {
        var t = e[0], n = e[1], r = e[2], i, s, o, u, a, f;
        return i = Math.min(t, n, r), s = Math.max(t, n, r), f = s, o = s - i, s != 0 ? (a = o / s, t == s ? u = (n - r) / o : n == s ? u = 2 + (r - t) / o : u = 4 + (t - n) / o, u *= 60, u < 0 && (u += 360)) : (a = 0, u = -1), [u, a, f]
    },HSV2RGB: function(e) {
        var t = e[0], n = e[1], r = e[2], i, s, o, u, a, f, l, c;
        if (n == 0)
            s = o = u = r;
        else {
            t /= 60, i = Math.floor(t), a = t - i, f = r * (1 - n), l = r * (1 - n * a), c = r * (1 - n * (1 - a));
            switch (i) {
                case 0:
                    s = r, o = c, u = f;
                    break;
                case 1:
                    s = l, o = r, u = f;
                    break;
                case 2:
                    s = f, o = r, u = c;
                    break;
                case 3:
                    s = f, o = l, u = r;
                    break;
                case 4:
                    s = c, o = f, u = r;
                    break;
                default:
                    s = r, o = f, u = l
            }
        }
        return [s, o, u]
    },RGB2bytes: function(e) {
        var t = Array(3);
        return t[0] = Math.round(e[0] * 255), t[1] = Math.round(e[1] * 255), t[2] = Math.round(e[2] * 255), t
    },RGB2color: function(e) {
        return String.buffer("rgb(", e[0] * 100, "%,", e[1] * 100, "%,", e[2] * 100, "%)").get()
    },RGB2hex: function(e) {
        return e = DlColor.RGB2bytes(e), e[0].hex(2) + e[1].hex(2) + e[2].hex(2)
    },color2RGB: function(e) {
        var t = 0, n = 0, r = 0;
        if (!/^#/.test(e))
            throw new DlException("Can't parse color: " + e);
        return e.length == 4 && (e = e.replace(/([a-f0-9])/ig, "$1$1")), t = parseInt(e.substr(1, 2), 16) / 255, n = parseInt(e.substr(3, 2), 16) / 255, r = parseInt(e.substr(5, 2), 16) / 255, [t, n, r]
    },brighter: function(e) {
        var t = Array.$(e);
        return t[1] -= .5, t[1] < 0 && (t[1] = 0), t
    },darker: function(e) {
        var t = Array.$(e);
        return t[2] -= .5, t[2] < 0 && (t[2] = 0), t
    },RGBrightness: function(e) {
        return (e[0] * 299 + e[1] * 587 + e[2] * 114) / 1e3
    }}, DEFINE_CLASS("DlColorPickerHSV", DlWidget, function(e, t, n) {
    function o(e) {
        var t = e.target;
        try {
            while (t && t.tagName.toLowerCase() != "td")
                t = t.parentNode
        } catch (n) {
            t = null
        }
        return t
    }
    function u(e) {
        var t = o(e);
        if (!t)
            return;
        throw t.rgb && this.applyHooks("onSelect", [t.rgb, t.hsv, t.style.backgroundColor, DlColor.RGBrightness(t.rgb)]), new DlExStopEventBubbling
    }
    function a(e) {
        var t, n = o(e);
        if (!n)
            return;
        throw t = n.getAttribute("hueCell"), t && (e.computePos(this), this._refresh(e), DlEvent.captureGlobals(this._dragHandlers)), new DlExStopEventBubbling
    }
    function f(e) {
        var t, n;
        this._currentHover && (i(this._currentHover, "hover1"), i(this._currentHover, "hover2")), t = o(e), t && (t.rgb ? (this._currentHover = t, n = DlColor.RGBrightness(t.rgb), s(t, n > .6, "hover2", "hover1"), this.applyHooks("onHover", [t.rgb, t.hsv, t.style.backgroundColor, n])) : this._currentHover && (this.callHooks("onHoverOut"), this._currentHover = null))
    }
    function l() {
        var e = this._currentHover;
        e && (i(e, "hover1"), i(e, "hover2"), this.callHooks("onHoverOut")), this._currentHover = null
    }
    function c() {
        throw DlEvent.releaseGlobals(this._dragHandlers), new DlExStopEventBubbling
    }
    function h(e) {
        var t = e.computePos(this), n = t.y - 2;
        throw n < 0 ? n = 0 : n > 119 && (n = 119), this.getHueBarElement().style.top = n + "px", this.__cphsvTimeout && clearTimeout(this.__cphsvTimeout), this.__cphsvTimeout = this._refresh.$(this, e).delayed(5), new DlExStopEventBubbling
    }
    var r, i = n.delClass, s = n.condClass;
    e.DEFAULT_EVENTS = ["onSelect", "onHover", "onHoverOut", "onHueChange"], r = String.buffer("<table cellspacing='1' cellpadding='0' border='0'>", "<tbody>", "<tr>", "<td></td>".repeat(11), "<td rowspan='11' class='DlColorPickerHSV-Sep'></td>", "<td rowspan='11' class='DlColorPickerHSV-HSV' hueCell='1'>", "<div class='DlColorPickerHSV-HSV-bar'></div>", "</td>", "</tr>", ("<tr>" + "<td></td>".repeat(11) + "</tr>").repeat(10), "</tbody></table>").get(), t.getHueBarElement = function() {
        return this.getElement().rows[0].cells[12].firstChild
    }, t._createElement = function() {
        e.BASE._createElement.call(this, r), this.setUnselectable()
    }, t.initDOM = function() {
        e.BASE.initDOM.call(this), this.addEventListener({onMouseUp: u,onMouseDown: a,onMouseOver: f,onMouseLeave: l}), this._dragHandlers = {onMouseMove: h.$(this),onMouseUp: c.$(this),onMouseOver: DlException.stopEventBubbling,onMouseOut: DlException.stopEventBubbling,onMouseEnter: DlException.stopEventBubbling,onMouseLeave: DlException.stopEventBubbling}, this._redraw(360)
    }, t._refresh = function(e) {
        var t = Math.limit(e.relPos.y - 2, 0, 119), n = Math.round((1 - t / 120) * 360);
        n = this._redraw(n), this.applyHooks("onHueChange", [n]), this.__cphsvTimeout = null
    }, t.setHSV = function(e) {
        this._redraw(e[0])
    }, t._redraw = function(e) {
        var t, n, r, i, s = this.getHueBarElement(), o = this.getElement(), u = o.rows, a = u.length - 1, f = u[0].cells.length - 3;
        s.style.top = 120 - e / 3 + "px", e == 360 && (e = 0);
        for (t = a; t >= 0; --t) {
            i = u[t].cells;
            for (r = f; r >= 0; --r)
                n = i[r], n.hsv = [e, 1 - t / a, r / f], n.rgb = DlColor.HSV2RGB(n.hsv), n.style.backgroundColor = DlColor.RGB2color(n.rgb)
        }
        return e
    }
}), DEFINE_CLASS("DlEntry", DlContainer, function(e, t, n) {
    function i() {
        this.addClass("DlEntry-Focus"), this._focused = !0, e.BASE.focus.call(this), this._isEmpty && (this.getInputElement().value = "", this.delClass("DlEntry-empty"), this._isEmpty = !1)
    }
    function s() {
        this.delClass("DlEntry-Focus"), this._focused = !1, e.BASE.blur.call(this), this.__setEmpty()
    }
    function o() {
        this.destroyed || (this.__setEmpty(), this.callHooks("onChange"))
    }
    function u() {
        this.validate()
    }
    function a(e) {
        this._isEmpty = !1, e.keyCode == DlKeyboard.ENTER ? this.applyHooks("onKey-ENTER", [e]) : e.keyCode == DlKeyboard.ESCAPE && this.applyHooks("onKey-ESCAPE", [e])
    }
    var r = n.createElement;
    e.FIXARGS = function(e) {
        e.tagName = "table", this._isTextArea = e.type == "textarea"
    }, e.DEFAULT_EVENTS = ["onChange", "onKey-ENTER", "onKey-ESCAPE", "onValidationError", "onValidation", "onPaste", "onCopy", "onCut"], e.DEFAULT_ARGS = {_domType: ["type", "text"],_value: ["value", null],_size: ["size", null],_rows: ["rows", null],_readonly: ["readonly", !1],_emptyText: ["emptyText", ""],_emptyValue: ["emptyValue", ""],_width: ["width", null],_name: ["name", null],_validators: ["validators", []],_allowEmpty: ["allowEmpty", null],_focusable: ["focusable", 2],_maxlen: ["maxlength", null],_noSelect: ["noSelect", !1],_trim: ["trim", !1],_noWrap: ["noWrap", !1]}, t.validate = function(e) {
        var t, n, r, i;
        e == null && (e = this.getValue(!0));
        if (this._allowEmpty != null && !/\S/.test(e))
            return this.condClass(!this._allowEmpty, "DlEntry-ValidationError"), this.applyHooks("onValidation", [!this._allowEmpty]), this._allowEmpty;
        t = this._validators, i = !1;
        for (n = 0; n < t.length; ++n) {
            r = t[n];
            if (!r.ok(e)) {
                i = r.getError() || !0;
                break
            }
        }
        return r && !i && this.setValue(r.getLastVal(), !0), this.validationError = i, !this._noSelect && this._focused && !this.readonly() && this._domType != "textarea" && this.select(), this.condClass(i, "DlEntry-ValidationError"), this.applyHooks("onValidation", [i]), i && (this.setInvalidTooltip(i.message), this.applyHooks("onValidationError", [i])), !i
    }, t.setInvalidTooltip = function(e) {
        this._invalidTooltip.setTooltip(e)
    }, t.timerFocus = function(e) {
        return this.focus.clearingTimeout(e || 10, this)
    }, t.select = function() {
        try {
            is_gecko ? this.setSelectionRange(0, this.getValue(!0).length) : this.getInputElement().select()
        } catch (e) {
        }
    }, t.focus = function() {
        this.getInputElement().focus(), !this._noSelect && !this.readonly() && this._domType != "textarea" && this.select()
    }, t.blur = function() {
        this.getInputElement().blur()
    }, t.__setEmpty = function(e) {
        return e == null && (e = this.getInputElement().value), this._isEmpty = this.__checkEmpty(e), this._isEmpty ? this._focused ? this.getInputElement().value = e : (this.addClass("DlEntry-empty"), this.getInputElement().value = "") : this.delClass("DlEntry-empty"), this._isEmpty
    }, t.__checkEmpty = function(e) {
        return e == null && (e = this.getInputElement().value), e === ""
    }, t._createElement = function() {
        var t, n;
        e.BASE._createElement.apply(this, arguments), t = this.getElement(), t.appendChild(DlElementCache.get("TBODY_RC")), t.cellSpacing = t.cellPadding = t.border = 0, t = t.rows[0].cells[0], t.className = "DlEntry-cell", n = this._isTextArea ? document.createElement("textarea") : n = document.createElement("input"), n.id = this.id + "-input", n.setAttribute("autocomplete", "off", 1), this._noWrap && n.setAttribute("wrap", "off"), this._isTextArea && this._rows && (n.rows = this._rows), this._maxlen != null && n.setAttribute("maxlength", this._maxlen);
        switch (this._domType) {
            case "password":
            case "file":
            case "hidden":
                n.type = this._domType
        }
        is_gecko && gecko_version < 1.9 && !this._no_gecko_bug && (t = r("div", null, {className: "Gecko-Bug-226933"}, t)), t = r("div", {position: "relative",overflow: "hidden"}, null, t), this._emptyText && r("label", null, {htmlFor: this.id + "-input",className: "DlEntry-emptyText",innerHTML: this._emptyText.htmlEscape()}, t), t.appendChild(n), this.refNode("_invalidTooltip", new DlWidget({className: "DlEntry-invalidIcon",parent: this,appendArgs: t}))
    }, t.getInputElement = function() {
        return this.getElement().getElementsByTagName(this._isTextArea ? "textarea" : "input")[0]
    }, t.getContentElement = t.getInputElement, t.setIfEmpty = function(e, t) {
        this._isEmpty && e && this.setValue(e, t)
    }, t.isEmpty = function() {
        return this.__checkEmpty()
    }, t.setValue = function(e, t) {
        var n;
        this.__setEmpty(e) || (this._maxlen != null && (e = (e + "").substr(0, this._maxlen)), n = this.getInputElement(), n.value = e, n.defaultValue = e), t || this.callHooks("onChange")
    }, t.isDirty = function() {
        var e = this.getInputElement();
        return e.value != e.defaultValue
    }, t.clear = function(e) {
        return this.setValue("", e), this
    }, t.getValue = function(e) {
        var t = !e && this.isEmpty() ? this._emptyValue : this.getInputElement().value;
        return this._trim && typeof t == "string" && (t = t.trim()), t
    }, t.getSelectionRange = function() {
        return n.getSelectionRange(this.getInputElement())
    }, t.setSelectionRange = function(e, t) {
        n.setSelectionRange(this.getInputElement(), e, t)
    }, t.moveEOF = function() {
        var e = this.getValue(!0).length;
        this.setSelectionRange(e, e)
    }, t.moveBOF = function() {
        this.setSelectionRange(0, 0)
    }, t.collapse = function(e) {
        var t = this.getSelectionRange();
        t = e ? t.start : t.end, this.setSelectionRange(t, t)
    }, t.insertReplacingSelection = function(e, t) {
        var n = this.getSelectionRange(), r = this.getValue();
        this.setValue(r.substr(0, n.start) + e + r.substr(n.end)), this.setSelectionRange(n.start, t ? n.start + e.length : n.start)
    }, t.initDOM = function() {
        var t;
        e.BASE.initDOM.call(this), t = this.getInputElement(), n.addEvent(t, {focus: this._on_element_focus = i.$(this),blur: this._on_element_blur = s.$(this),change: this._on_element_change = o.clearingTimeout(10, this)}), this.addEventListener({onChange: u,onKeyPress: a}), this._value != null ? this.setValue(this._value, !0) : this.clear(!0), this._width != null ? t.style.width = this._width : this._size != null && this.setSize({x: this._size * 9 + 7 - this._size}), this._name != null && (t.name = this._name), this.readonly(this._readonly)
    }, t.readonly = function(e) {
        var t = this.getInputElement();
        return e != null && (t.readOnly = e, e ? t.setAttribute("readonly", !0, 1) : t.removeAttribute("readonly"), this.condClass(e, "DlEntry-Readonly")), t.getAttribute("readonly")
    }, t.disabled = function(t, n) {
        var r = e.BASE.disabled.call(this, t, n);
        return t != null && (this.getInputElement().disabled = !!t), r
    }, t.setSize = t.setOuterSize = function(e) {
        var t = this.getInputElement(), r = e.x, i = e.y, s = n.getPaddingAndBorder(this.getElement()), o = n.getPaddingAndBorder(t), u = this._btn ? this._btn.getSize().x : 0;
        u ? n.setOuterSize(t, e.x - s.x - o.x - u + 2) : (r != null && (r -= o.x + 4), i != null && (i -= o.y + 4), this._domType != "textarea" && (i = null), n.setInnerSize(t, r, i), r != null && (r += 8, n.setInnerSize(this.getElement(), r)))
    }, t._makeButton = function(e, t, n, i) {
        var s;
        return !i && !n && (n = "DlEntry-dropDownBtn", i = {hover: "DlEntry-dropDownBtn-hover",active: "DlEntry-dropDownBtn-active"}), s = r("td", null, null, this.getElement().rows[0]), this._btn = new DlAbstractButton({parent: this,appendArgs: s,label: e,iconClass: t,className: n,classes: i})
    }
}), DEFINE_CLASS("DlCompletionEntry", DlEntry, function(e, t, n) {
    function o() {
        return r || (r = DlCompletionPopup.get()), r
    }
    function u() {
        return i && (s = null, i.destroy()), i = new DlVMenu({})
    }
    function a() {
        return i && i.parent.visible
    }
    function f(e, t) {
        var n = i.children().find(this);
        n != s && s != null && i.children(s).callHooks("onMouseLeave"), s = n, (e._electric || !t) && e._applyCompletion(this.userData)
    }
    function l(e) {
        e._hideMenu(), e._applyCompletion(this.userData, !0), e.applyHooks("onSelect", [this.userData]), e.focus.delayed(0, e)
    }
    function c(e, t) {
        this.__origData = {value: this.getValue(),range: this.getSelectionRange()}, this.__forced = t, this.addClass("DlCompletionEntry-busy"), this.applyHooks("onCompletion", [this.getSelectionRange(), e, t])
    }
    function h(e) {
        var t, n, r;
        if (!a())
            return !1;
        t = s;
        switch (e.keyCode) {
            case DlKeyboard.ARROW_UP:
                s == null && (s = 0), s = i.children().rotateIndex(--s);
                break;
            case DlKeyboard.ARROW_DOWN:
            case DlKeyboard.TAB:
                s == null && (s = -1), s = i.children().rotateIndex(++s);
                break;
            case DlKeyboard.ENTER:
                s != null && (this.collapse(!1), i.children(s).callHooks("onSelect")), DlException.stopEventBubbling();
            case DlKeyboard.ESCAPE:
                this._hideMenu(), r = this.__origData, r && (this.setValue(r.value), this.setSelectionRange(r.range)), DlException.stopEventBubbling()
        }
        t != s ? (t != null && (n = i.children(t), n.callHooks("onMouseLeave")), n = i.children(s), n.callHooks("onMouseEnter"), DlException.stopEventBubbling()) : this._hideMenu()
    }
    function p(e) {
        if (is_ie)
            return h.call(this, e)
    }
    function d() {
        this.cancelCompletion()
    }
    var r, i, s;
    e.DEFAULT_EVENTS = ["onCompletion", "onSelect"], e.DEFAULT_ARGS = {__timeout: ["timeout", 500],_shiftMenu: ["shift", null],__smart: ["smart", !0],__noTab: ["noTab", !1],_noSelect: ["noSelect", !0],_sizeToFit: ["sizeToFit", !1],_electric: ["electric", !0]}, t.ALIGN = {prefer: "Br",fallX1: "_r",fallX2: "_L",fallY1: "B_",fallY2: "T_"}, t._setListeners = function() {
        this.__on_itemHover = f.$(null, this), this.__on_itemSelect = l.$(null, this), e.BASE._setListeners.call(this), this.addEventListener({onKeyDown: p,onBlur: d,onDestroy: this.__clearTimer})
    }, t._hideMenu = function() {
        o().hide(50), this.__clearTimer()
    }, t.__clearTimer = function() {
        this.__timer && clearTimeout(this.__timer), this.__timer = null
    }, r = null, i = null, s = null, t._applyCompletion = function(e, t) {
        var n, r, i, s;
        if (!e.nomodify || t) {
            n = this.getSelectionRange(), r = e.completion || e.label, t && e.after && (r += e.after), i = this.getValue(), s = e.start != null ? e.start : n.start, i = i.substr(0, s) + r + i.substr(n.end), this.setValue(i), n.end = s + r.length, e.rstart != null && (n.start = e.rstart);
            if (e.noselect || t)
                n.start = n.end;
            this.setSelectionRange(n)
        }
    }, t._on_menuHide = function() {
        i && (i.destroy(), i = null, s = null)
    }, t.completionReady = function(e) {
        var t, r, i;
        (this.__timer || this.__forced) && e != null && e.length > 0 && (this.__smart && e.length == 1 ? (this._applyCompletion(e[0], !0), this.applyHooks("onSelect", [e[0]])) : (t = u(), r = null, e.foreach(function(e) {
            var n = new DlMenuItem({parent: t,label: e.label,data: e});
            n.addEventListener({onSelect: this.__on_itemSelect,onMouseEnter: this.__on_itemHover}), e.selected && (r = n)
        }, this), o().popup({timeout: 0,content: t,align: this.ALIGN,anchor: this.getElement(),isContext: !0,widget: this,onHide: this._on_menuHide.$(this),shift: this._shiftMenu}), this._sizeToFit && (i = this.getSize().x, t.getSize().x < i && t.setSize({x: i - n.getPaddingAndBorder(o().getElement()).x})), r && r.callHooks("onMouseEnter"))), this.cancelCompletion()
    }, t.cancelCompletion = function() {
        this.delClass("DlCompletionEntry-busy"), this.__clearTimer(), this.__forced = null
    }, t._handle_focusKeys = function(t) {
        if (!DlKeyboard.KEYS_CONTROL[t.keyCode])
            this._hideMenu(), this.__timeout != null && (this.__timer = c.delayed(this.__timeout, this, t, !1));
        else if (!is_ie) {
            if (!(!this.__noTab && t.keyCode == DlKeyboard.TAB && !a() && !this.isEmpty()))
                return h.call(this, t);
            c.call(this, t, !0), t.domStop = !0, DlException.stopEventBubbling()
        }
        return e.BASE._handle_focusKeys.call(this, t)
    }, t.completeFromWords = function(t, n) {
        return e.completeFromWords.call(e, this, t, n)
    }, e.completeFromWords = function(e, t, n) {
        return n == null && (n = {}), n.sep == null && (n.sep = /\s+/g), function(r) {
            var i, s = [], o = e.getValue(), u = o.lastIndexOfRegexp(n.sep, r.start);
            o = o.substring(u, r.start);
            if (o)
                for (i = 0; i < t.length; ++i)
                    t[i].indexOf(o) == 0 && s.push({label: t[i],noselect: n.noselect,after: n.addSep,start: u});
            s.length > 0 ? e.completionReady(s) : e.cancelCompletion()
        }
    }
}), DEFINE_CLASS("DlCompletionPopup", DlPopup, function(e) {
    e.FIXARGS = function(e) {
        e.zIndex = 1e3, e.focusable = !1
    }
}), DEFINE_CLASS("DlComboBox", DlCompletionEntry, function(e, t) {
    function n(e) {
        e.button == 0 && (this._forcePopup(), DlException.stopEventBubbling())
    }
    e.DEFAULT_ARGS = {_noSelect: ["noSelect", !1],__smart: ["smart", !1],__noTab: ["noTab", !0],_options: ["options", null],_sizeToFit: ["sizeToFit", !0],_electric: ["electric", !1]}, t._createElement = function() {
        e.BASE._createElement.apply(this, arguments), this._makeButton(null, null, "DlComboBox-dropDownBtn", {hover: "DlComboBox-dropDownBtn-hover"}).addEventListener("onMouseDown", n.$(this)), this.addEventListener("onCompletion", this.doCompletion)
    }, t._on_menuHide = function() {
        e.BASE._on_menuHide.call(this), this._btn.delClass("DlComboBox-dropDownBtn-active")
    }, t._forcePopup = function() {
        this._btn.addClass("DlComboBox-dropDownBtn-active"), this.__forced = !0, this.doCompletion(null), this.focus.delayed(0, this)
    }, t.doCompletion = function(e) {
        var t, n = "", r = [];
        if (e) {
            n = this.getValue().trim().toLowerCase();
            if (!n)
                return this.cancelCompletion()
        }
        t = this._options;
        if (t instanceof Function) {
            t = t.apply(this, arguments);
            if (t == null)
                return
        }
        t.foreach(function(e) {
            e.toLowerCase().indexOf(n) == 0 && r.push({label: e.htmlEscape(),start: 0,completion: e})
        }), r.length > 0 ? this.completionReady(r) : this.cancelCompletion()
    }
}), DEFINE_CLASS("DlDrag", DlEventProxy, function(e, t, n) {
    e.DEFAULT_EVENTS = ["onDrop", "onStartDrag"], e.DEFAULT_ARGS = {delta: ["delta", 3],dragging: ["_dragging", !1],draggingClass: ["draggingClass", "DlWidget-dragging"],_animArgs: ["animation", {length: 30,fps: 50}],cursor: ["cursor", {noDrop: "CURSOR-NO-DROP",okDrop: "CURSOR-DROP"}]}, t.dropOK = function(e, t, n) {
        return this.target = n, this.canDrop = !0
    }, t._handleDrop = function(e, t, n) {
        this.applyHooks("onDrop", [e, t, n])
    }, t.doDrop = function() {
        throw new DlExAbstractBaseClass
    }, t.startOK = function() {
        return !0
    }, t.moving = function() {
    }, t.reset = function(e) {
        var t, r, i, s = this.elementCopy;
        s && s.parentNode && (e && this._animArgs ? (t = new DlAnimation(this._animArgs.length, this._animArgs.fps), r = this.startElPos || this.startPos, i = n.getPos(s), t.addEventListener({onUpdate: function() {
                var e = this.getPos();
                s.style.left = e.mapInt(i.x, r.x) + "px", s.style.top = e.mapInt(i.y, r.y) + "px", n.setOpacity(s, this.t.map(1, .2))
            },onStop: function() {
                n.trash(s), s = null
            }}), t.start(null, null, "accel_ab")) : s.parentNode.removeChild(s)), this.dragging = !1, this.canDrop = !1, this.target = null, this.elementCopy = null, this.startPos = null, this.source = null
    }, t.makeElementCopy = function(e, t) {
        var r = this.elementCopy;
        return r || (r = this.elementCopy = e.getElement().cloneNode(!0), n.addClass(r, "DlWidget-dragged-clone"), r.style.top = t.pos.y + "px", r.style.left = t.pos.x + "px", document.body.appendChild(r), r.style.width = r.offsetWidth + "px"), r
    }
}), DEFINE_CLASS("DlDragTreeItem", DlDrag, function(e, t) {
    function u(e) {
        return /DlTree-IconWidth/.test(e.target.className)
    }
    var n, r, i, s, o;
    e.DEFAULT_ARGS = {_noReparent: ["noReparent", !1]}, n = "DlTreeItem-dropTarget", r = "DlTreeItem-dropTarget-upper", i = "DlTreeItem-dropTarget-lower", s = /DlTreeItem-dropTarget[^\s]*/g, o = /DlTreeItem-dropTarget-[^\s]*/g, t.startOK = function(e, t) {
        return !u(t)
    }, t.dropOK = function(e, t, r, i) {
        var o;
        while (r && !(r instanceof DlTreeItem))
            r = r.parent;
        return o = !i && r, o && (o = !this._noReparent || e.parent === r.parent), this.target = o ? r : null, this.canDrop = !!o, this.oldTarget && this.oldTarget !== this.target && this.oldTarget.delClass(s), o && this.target.addClass(n), this.oldTarget = this.target, o
    }, t.doDrop = function(e, t) {
        var n;
        this._noReparent || u(t) ? (n = this.target.getIndex(), this.dropBefore || ++n, this.target.parent.appendWidget(e, n), this._handleDrop(e, this.target, this.dropBefore ? "before" : "after")) : this.target.getSubtreeWidget() !== e.parent && (this.target.addSubItem(e), this._handleDrop(e, this.target))
    }, t.moving = function(e, t) {
        var n, s, a, f = this.target;
        this.canDrop && f && (this._noReparent || u(t)) ? (n = t.computePos(f), s = f.getDivElement().offsetHeight / 2, a = n.y <= s, f.condClass(a, r, i), this.dropBefore = a) : f && (this.dropBefore = null, f.delClass(o))
    }, t.reset = function() {
        this.target && this.target.delClass(s), this.oldTarget && this.oldTarget.delClass(s), e.BASE.reset.apply(this, arguments), this.oldTarget = null
    }
}), DEFINE_CLASS("DlLayout", DlContainer, function(e, t, n) {
    e.DEFAULT_ARGS = {_outerSpace: ["outerSpace", 0],_fillParent: ["fillParent", !0]}, e.setFill = function(e, t) {
        var n = e._dllayout_args.fill;
        e._dllayout_args.fill = t, t != n && e.parent.doLayout()
    }, e.getArgs = function(e) {
        return e._dllayout_args
    }, t._appendWidgetElement = function(t, r) {
        var i;
        if (r == null)
            return e.BASE._appendWidgetElement.apply(this, arguments);
        i = n.createElement("div", null, {className: "DlLayout-positioned"}, this.getElement()), r.zIndex && (i.style.zIndex = r.zIndex), r.overflow && (i.style.overflow = r.overflow), i.appendChild(t.getElement()), t._dllayout_args = r
    }, t._removeWidgetElement = function(e) {
        var t, n;
        this._widgets.contains(e) && (t = e.getElement(), n = t.parentNode, n && (n.parentNode.removeChild(n), n.removeChild(t)))
    }, t.packWidget = function(e, t) {
        this.appendWidget(e, t)
    }, t.replaceWidget = function(e, t) {
        var n, r, i = this._widgets.find(e);
        i >= 0 && (t.parent && t.parent.removeWidget(t), this._widgets.splice(i, 1, t), t._dllayout_args = e._dllayout_args, e._dllayout_args = null, n = e.getElement(), r = n.parentNode, r.insertBefore(t.getElement(), n), r.removeChild(n), t.parent = this, e.parent = null)
    }, t.doLayout = function() {
        function b() {
            typeof y == "number" ? e = t = n = r = y : y instanceof Array ? (r = y[0], t = y[1], n = y[2], e = y[3]) : (r = y.top || 0, t = y.right || 0, n = y.bottom || 0, e = y.left || 0)
        }
        var e, t, n, r, i, s, o, u, a, f, l, c, h, p, d, v, m, g = this.getInnerSize(), y = this._outerSpace;
        b(), i = this._widgets, s = Array(i.length), o = {};
        for (u = 0; u < i.length; ++u) {
            a = i[u], f = a._dllayout_args;
            if (!f || !a.display())
                continue;
            l = a.getElement().parentNode, c = f.before = f.before || 0, h = f.after = f.after || 0, p = f.fill, f.resetSize && (a.getElement().style.height = "", a.getElement().style.width = ""), d = a.getOuterSize();
            switch (f.pos) {
                case "top":
                    r += c, p == null && (p = d.y), s[u] = {sy: p}, typeof p == "number" && (r += p), r += h;
                    break;
                case "right":
                    t += c, p == null && (p = d.x), s[u] = {sx: p}, typeof p == "number" && (t += p), t += h;
                    break;
                case "bottom":
                    n += c, p == null && (p = d.y), s[u] = {sy: p}, typeof p == "number" && (n += p), n += h;
                    break;
                case "left":
                    e += c, p == null && (p = d.x), s[u] = {sx: p}, typeof p == "number" && (e += p), e += h
            }
            s[u].w = a, s[u].args = f, s[u].div = l
        }
        v = g.x - e - t, m = g.y - r - n, b(), s.foreach(function(i) {
            function u() {
                var e = r, t = g.y - r - n, u = {x: i.sx};
                switch (s.valign) {
                    case "top":
                        break;
                    case "center":
                        e += (t - o.getOuterSize().y) / 2;
                        break;
                    case "bottom":
                        e += t - o.getOuterSize().y;
                    default:
                        u.y = t
                }
                i.div.style.top = e + "px", o.setSize(u)
            }
            var s, o;
            i || $CONTINUE(), s = i.args, o = i.w;
            if (!o.display())
                return;
            switch (s.pos) {
                case "top":
                case "bottom":
                    typeof i.sy != "number" && (i.sy == "*" ? i.sy = m : /%/.test(i.sy) && (i.sy = Math.floor(parseFloat(i.sy) * m / 100)), s.min != null && i.sy < s.min && (i.sy = s.min), s.max != null && i.sy > s.max && (i.sy = s.max), m -= i.sy);
                    break;
                case "left":
                case "right":
                    typeof i.sx != "number" && (i.sx == "*" ? i.sx = v : /%/.test(i.sx) && (i.sx = Math.floor(parseFloat(i.sx) * v / 100)), s.min != null && i.sx < s.min && (i.sx = s.min), s.max != null && i.sx > s.max && (i.sx = s.max), v -= i.sx)
            }
            switch (s.pos) {
                case "top":
                    r += s.before, i.div.style.left = e + "px", i.div.style.top = r + "px", o.setSize({x: g.x - e - t,y: i.sy}), r += i.sy + s.after;
                    break;
                case "bottom":
                    n += s.before, i.div.style.left = e + "px", i.div.style.top = g.y - n - i.sy + "px", o.setSize({x: g.x - e - t,y: i.sy}), n += i.sy + s.after;
                    break;
                case "left":
                    e += s.before, i.div.style.left = e + "px", u(), e += i.sx + s.after;
                    break;
                case "right":
                    t += s.before, i.div.style.left = g.x - t - i.sx + "px", u(), t += i.sx + s.after
            }
        })
    }, t.__doLayout = function() {
        this.doLayout()
    }, t.sizeToFit = function() {
        var e, t, n, r, i = this._widgets, s = 0, o = 0;
        for (e = 0; e < i.length; ++e) {
            t = i[e], n = t._dllayout_args, r = t.getOuterSize();
            switch (n.pos) {
                case "top":
                case "bottom":
                    s += r.y, r.x > o && (o = r.x);
                    break;
                case "left":
                case "right":
                    o += r.x, r.h > s && (s = r.h)
            }
        }
        this.setOuterSize({x: o,y: s})
    }
}), DEFINE_CLASS("DlResizeBar", DlWidget, function(e, t, n) {
    function i(e) {
        var t, r, i, s = DlElementCache.DRAGGING_LINE, o = this.getPos();
        this._dragPos = this.isHoriz() ? o.y : o.x, t = this.isHoriz() ? e.pos.y : e.pos.x, this._mposDiff = t - this._dragPos, r = this.getSize(), s.style.top = o.y + "px", s.style.left = o.x + "px", s.style.width = r.x + "px", s.style.height = r.y + "px", this._widget && (i = this._widget instanceof DlWidget ? this._widget.getSize() : n.getOuterSize(this._widget), this._dragSize = this.isHoriz() ? i.y : i.x), document.body.appendChild(s), this._setResizeCaptures(!0), DlException.stopEventBubbling()
    }
    function s(e) {
        this._setResizeCaptures(!1), this._doResize(e), document.body.removeChild(DlElementCache.DRAGGING_LINE), this.callHooks("onStop")
    }
    function o(e) {
        var t, n, r, i, s, o, u = DlElementCache.DRAGGING_LINE, a = this.isHoriz() ? e.pos.y : e.pos.x;
        a -= this._mposDiff, t = this._invert * (a - this._dragPos), n = this._min, r = this._max, i = this._widget;
        if (i) {
            s = DlLayout.getArgs(i), s && (n == null && (n = s.min), r == null && (r = s.max));
            if (n != null || r != null)
                o = this._dragSize + t;
            n != null && o < n ? a += this._invert * (n - o) : r != null && o > r && (a += this._invert * (r - o))
        }
        this.isHoriz() ? u.style.top = a + "px" : u.style.left = a + "px", this._cont && this._doResize(e)
    }
    var r;
    e.DEFAULT_EVENTS = ["onResizing", "onStop"], r = n.condClass, e.FIXARGS = function(e) {
        e.invert = e.invert ? -1 : 1
    }, e.DEFAULT_ARGS = {_isHoriz: ["horiz", null],_widget: ["widget", null],_invert: ["invert", !1],_min: ["min", null],_max: ["max", null],_cont: ["continuous", !1],_keepPrc: ["keepPercent", !1]}, e.getDragBar = function() {
        return DlElementCache.DRAGGING_LINE
    }, t.initDOM = function() {
        e.BASE.initDOM.call(this), this.condClass(this.isHoriz(), "DlResizeBar-Horizontal", "DlResizeBar-Vertical"), this.setUnselectable(null, !0)
    }, t.isHoriz = function() {
        var e;
        return this._isHoriz == null && (e = DlLayout.getArgs(this), e && (this._isHoriz = /top|bottom/.test(e.pos))), this._isHoriz
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this._resizeHandlers = {onMouseMove: o.$(this),onMouseUp: s.$(this),onMouseOver: DlException.stopEventBubbling,onMouseOut: DlException.stopEventBubbling,onMouseEnter: DlException.stopEventBubbling,onMouseLeave: DlException.stopEventBubbling}, this.addEventListener("onMouseDown", i)
    }, t._setResizeCaptures = function(e) {
        var t;
        (e ? DlEvent.captureGlobals : DlEvent.releaseGlobals)(this._resizeHandlers), t = DlDialog.activateEventStopper(e), r(t, e, this.isHoriz() ? "CURSOR-RESIZE-S" : "CURSOR-RESIZE-E")
    }, t._doResize = function() {
        var e, t, r, i, s, o = this.isHoriz(), u = n.getPos(DlElementCache.DRAGGING_LINE);
        u = o ? u.y : u.x, e = this._invert * (u - this._dragPos), t = this._widget, t && (r = this._dragSize, t instanceof DlWidget ? (i = DlLayout.getArgs(t), i ? (i = i.fill, s = /%$/.test(i), s && !this._keepPrc || i == null || typeof i == "number" ? DlLayout.setFill(t, r + e) : s && (i = parseFloat(i), DlLayout.setFill(t, i *
        (r + e) / r + "%"))) : this._isHoriz ? t.setSize({y: r + e}) : t.setSize({x: r + e})) : this._isHoriz ? n.setOuterSize(t, null, r + e) : n.setOuterSize(t, r + e, null), this.callHooks("onResizing", t))
    }
}), DEFINE_CLASS("DlWM", DlContainer, function(e, t, n) {
    function r(e) {
        e.on_dlgShow(this)
    }
    function i(e) {
        e.on_dlgHide(this)
    }
    function s(e, t) {
        return e.x < t.x ? -1 : e.x > t.x ? 1 : 0
    }
    function o(e, t) {
        return e.y < t.y ? -1 : e.y > t.y ? 1 : 0
    }
    function u(e, t) {
        var n = e.length, r = Math.floor(t.x / n), i = 0;
        e.mergeSort(s).foreach(function(e, s) {
            e.y = 0, e.x = i, e.h = t.y, s == n - 1 ? e.w = t.x - i : e.w = r, i += r
        })
    }
    function a(e, t) {
        var n = e.length, r = Math.floor(t.y / n), i = 0;
        e.mergeSort(o).foreach(function(e, s) {
            e.x = 0, e.y = i, e.w = t.x, s == n - 1 ? e.h = t.y - i : e.h = r, i += r
        })
    }
    t.getInnerSize = t.getOuterSize = t.getSize = function() {
        return this.parent ? this.parent.getInnerSize() : n.getWindowSize()
    }, t.initDOM = function() {
        var t;
        e.BASE.initDOM.apply(this, arguments), this.getElement().innerHTML = "<div class='DlWM-modalStopper'></div>", this.dialogsVisible = [], this.modalsVisible = 0, this._manageEvents = {onShow: r.$(null, this),onHide: i.$(null, this)}, t = this.on_parentResize.$(this), this.parent ? this.parent.addEventListener("onResize", t) : n.addEvent(window, "resize", t)
    }, t.getModalStopperElement = function() {
        return this.getElement().childNodes[0]
    }, t.activatePrev = function() {
        var e = this.dialogsVisible;
        e.length > 1 && (e.peek().deactivate(), e.unshift(e.pop()), top = e.pop(), top.activate())
    }, t.activateNext = function() {
        var e = this.dialogsVisible;
        e.length > 1 && e[0].activate()
    }, t.getActiveDialog = function() {
        return this.dialogsVisible.peek()
    }, t.updateZIndex = function() {
        this.dialogsVisible.r_foreach(function(e, t) {
            e.zIndex((e.__modal ? 900 : 500) + t)
        })
    }, t.getVisibleDialogs = function() {
        return this.dialogsVisible
    }, t.getAllDialogs = function() {
        return this.children().grep(function(e) {
            return e instanceof DlDialog
        })
    }, t.appendWidget = function(t) {
        e.BASE.appendWidget.apply(this, arguments), t instanceof DlDialog && this.manage(t)
    }, t.removeWidget = function(t) {
        e.BASE.removeWidget.apply(this, arguments), t instanceof DlDialog && this.unmanage(t)
    }, t.manage = function(e) {
        e.addEventListener(this._manageEvents)
    }, t.unmanage = function(e) {
        e.removeEventListener(this._manageEvents)
    }, t.on_dlgShow = function(e) {
        e.__modal && this.modalsVisible++, this.condClass(this.modalsVisible > 0, "DlWM-hasModals")
    }, t.on_dlgHide = function(e) {
        e.__modal && this.modalsVisible--, this.condClass(this.modalsVisible > 0, "DlWM-hasModals"), this.dialogsVisible.length == 0 && this.parent && this.parent.focus()
    }, t.on_parentResize = function() {
        this.dialogsVisible.foreach(function(e) {
            e.__maximized && e.__doMaximize()
        }), this.callHooks("onResize")
    }, t.rearrange = function(e) {
        var t = this.dialogsVisible.map(function(e) {
            var t = e.getOffsetPos(), n = e.getOuterSize();
            return {dlg: e,x: t.x,y: t.y,w: n.x,h: n.y}
        });
        e(t, this.getInnerSize()), t.foreach(function(e) {
            e.dlg.setPos(e.x, e.y), e.dlg.setSize({x: e.w,y: e.h})
        })
    }, t.tileHoriz = function() {
        this.rearrange(u)
    }, t.tileVert = function() {
        this.rearrange(a)
    }, DlContainer.prototype._makeWindowManager = function() {
        return this.__wm || (this.__wm = new DlWM({parent: this})).addEventListener("onDestroy", function() {
            this.__wm = null
        }.$(this)), this.__wm
    }
}), DEFINE_CLASS("DlDialog", DlContainer, function(e, t, n) {
    function c() {
        var e = n.CE_CACHE["DlDialog.EVENT_STOPPER"];
        return e || (e = n.CE_CACHE["DlDialog.EVENT_STOPPER"] = l("div", null, {className: "DYNARCH-EVENT-STOPPER"}, document.body)), e.style.visibility = "", e
    }
    function h() {
        var e = n.CE_CACHE["DlDialog.EVENT_STOPPER"];
        return e && (e.className = "DYNARCH-EVENT-STOPPER", e.style.visibility = "hidden"), e
    }
    function p(e) {
        var t, r, i;
        if (!this.dragging && !this.__maximized) {
            DlPopup.clearAllPopups(), this.activate(), this.dragging = !0, e || (e = window.event), t = e instanceof DlEvent ? e : new DlEvent(e), this.addClass("DlDialog-Dragging"), this._dragPos = t.computePos(this), this._setDragCaptures(!0), u(c(), "CURSOR-DRAGGING"), this.__moveDelay != null && (r = this.getResizeRect(), u(r, "Dl-ResizeRect-moving"), i = this.getOuterSize(), n.setPos(r, t.elPos.x, t.elPos.y), n.setOuterSize(r, i.x, i.y), r.style.display = "");
            if (t !== e)
                return n.stopEvent(e)
        }
    }
    function d(e) {
        e.ctrlKey && e.shiftKey && (e.button == 0 && this._dragHandlers ? p.call(this, e) : e.button == 2 && e.dl_type == "onContextMenu" && this._resizable && (w.call(this, e), o()))
    }
    function v(e) {
        var t, n;
        this.dragging && (t = this.getResizeRect(), this.dragging = !1, this.delClass("DlDialog-Dragging"), this._setDragCaptures(!1), this.__moveDelay != null && (e ? (n = m.call(this, e), this.__doDrag.doItNow(n.x, n.y)) : this.__doDrag.cancel()), a(t, "Dl-ResizeRect-moving"), t.style.display = "none", h())
    }
    function m(e) {
        var t, n, r, i, s = this.parent;
        return e.computePos(s.getContentElement()), t = e.relPos.x - this._dragPos.x, n = e.relPos.y - this._dragPos.y, r = this.getOuterSize(), i = s.getInnerSize(), t < 0 ? t = 0 : t + r.x > i.x && (t = i.x - r.x), n < 0 ? n = 0 : n + r.y > i.y && (n = i.y - r.y), {x: t,y: n}
    }
    function g(e) {
        var t = e.x, r = e.y;
        e = n.getPos(this.parent.getContentElement()), t += e.x, r += e.y, n.setPos(this.getResizeRect(), t, r)
    }
    function y(e, t) {
        this.setPos(e, t), this.__oldDlgPos = this.getOffsetPos()
    }
    function b(e) {
        var t = m.call(this, e);
        this.__moveDelay != null && g.call(this, t), this.__doDrag(t.x, t.y), o()
    }
    function w(e) {
        var t, r, i, s;
        this.resizing || (this.resizing = !0, e || (e = window.event), t = e instanceof DlEvent ? e : new DlEvent(e), this._dragPos = t.computePos(this), r = this.getOuterSize(), this._dragPos.x -= r.x, this._dragPos.y -= r.y, i = this.getPos(), s = this.getResizeRect(), n.setPos(s, i.x, i.y), n.setOuterSize(s, r.x, r.y), s.style.display = "", this.addClass("DlDialog-Resizing"), this._setResizeCaptures(!0), u(c(), "CURSOR-DRAGGING"), S.call(this, t, !0), t !== e && n.stopEvent(e))
    }
    function E() {
        var t, r;
        this.resizing && (this.disableHooks("onResize"), this.getElement().style.overflow = "hidden", t = this.getResizeRect(), r = n.getOuterSize(t), n.setPos(t, 0, 0), t.style.display = "none", this.delClass("DlDialog-Resizing"), this.setOuterSize({x: r.x - 2,y: r.y - 2}), is_gecko && e.BASE.setOuterSize.call(this, {x: "auto",y: "auto"}), this.resizing = !1, this._setResizeCaptures(!1), this.getElement().style.overflow = "", h(), this.enableHooks("onResize"), this.callHooks("onResize"))
    }
    function S(e, t) {
        var r, i;
        this.resizing && (r = this.getResizeRect(), i = n.getPos(r), i.x = e.pos.x - this._dragPos.x - i.x - 2, i.x < 100 && (i.x = 100), i.y = e.pos.y - this._dragPos.y - i.y - 2, i.y < 100 && (i.y = 100), this._resizable === 1 && (i.y = null), this._resizable === 2 && (i.x = null), n.setInnerSize(r, i.x, i.y), t || o())
    }
    function x(e) {
        var t, n = DlSystem();
        e ? (this.callHooks("onShow"), this.activate(), this.setModal(this.__modal, !0), n.applyHooks("on-dialog-show", [this]), this.__maximized && this.__doMaximize.delayed(1, this)) : (t = this.parent.getVisibleDialogs(), t.remove(this), this.callHooks("onHide"), this.deactivate(), n.applyHooks("on-dialog-hide", [this]), t.length >= 1 && t.peek().activate())
    }
    function T(e) {
        var t;
        if (e.shiftKey || e.altKey)
            t = this.__dlgOpacity, t == null && (t = 100), e.wheelDelta > 0 ? t += .05 : t -= .05, t = this.__dlgOpacity = t.limit(.1, 1), this.opacity(t), o()
    }
    function N(e, t) {
        var n = e.getRelElement().childNodes;
        return n[n.length - t]
    }
    var r, i, s, o = DlException.stopEventBubbling, u = n.addClass, a = n.delClass, f = n.condClass, l = n.createElement;
    e.DEFAULT_EVENTS = ["onShow", "onHide", "onActivate", "onQuitBtn"], e.DEFAULT_ARGS = {_title: ["title", "DlDialog"],_noEmptyTitle: ["noEmptyTitle", !0],_fixed: ["fixed", !1],_resizable: ["resizable", !1],_focusable: ["focusable", !0],_iconClass: ["iconClass", null],_focusedWidget: ["focusDefault", null],__noShadows: ["noShadows", !1],__quitBtn: ["quitBtn", !1],__maxBtn: ["maxBtn", !0],__modal: ["modal", !1],__moveDelay: ["moveDelay", null]}, e.FIXARGS = function(t) {
        t.parent || (t.parent = e.getTopWM()), t.parent instanceof DlWM || (t.parent instanceof DlDialog && (t.noShadows = !0), t.parent = t.parent._makeWindowManager())
    }, e.CONSTRUCT = function() {
        this.__doDrag = this.__moveDelay != null ? y.clearingTimeout(this.__moveDelay, this) : y.$(this), this.active = !1
    }, e.getTopWM = function() {
        return r || (r = new DlWM({className: "DlTopWindowManager"}), document.body.appendChild(r.getElement())), r
    }, i = "<div class='DlDialog-Rel'><div class='DlDialog-WindowButtons'></div><div class='DlDialog-Title'><div></div></div><div class='DlDialog-Content'></div></div>", s = {x: -3e4,y: -3e4}, t._setDragCaptures = function(e) {
        DlEvent[e ? "captureGlobals" : "releaseGlobals"](this._dragHandlers)
    }, t._setResizeCaptures = function(e) {
        DlEvent[e ? "captureGlobals" : "releaseGlobals"](this._resizeHandlers)
    }, e.activateEventStopper = function(e) {
        return e ? c() : h()
    }, t.setOuterSize = t.setSize = function(e) {
        e = Object.makeCopy(e), e.y != null && (e.y -= this.getTitleElement().offsetHeight), this.setInnerSize(e)
    }, t.hide = function() {
        this.display() && n.elementIsVisible(this.getElement()) && (this.__oldDlgPos = this.getOffsetPos(), this.display(!1), this.setPos(s))
    }, t.show = function(e) {
        this.__wasDisplayed || this.setStyle({visibility: ""}), !this.display() || !this.__wasDisplayed ? (this.__oldDlgPos ? this.setPos(this.__oldDlgPos) : e && this.centerOnParent(), this.display(!0)) : this.activate(), this.__wasDisplayed = !0
    }, t.activate = function() {
        var e = this.parent.getVisibleDialogs(), t = e.peek();
        this.active || (t && t.active && t.deactivate(!0), this.addClass("DlDialog-Active"), e.remove(this), e.push(this), this.parent.updateZIndex(), this.active = !0, this.focus(), this._focusedWidget && !this._focusedWidget.destroyed && this._focusedWidget.focus(), this.applyHooks("onActivate", [!0]))
    }, t.deactivate = function() {
        this.active && (this.delClass("DlDialog-Active"), this.active = !1, this.blur(), this.applyHooks("onActivate", [!1]))
    }, t._createElement = function() {
        var t, n, r;
        e.BASE._createElement.call(this), this.setPos(s), this.setStyle({visibility: "hidden"}), this.getElement().innerHTML = i, t = this.getRelElement(), this.__noShadows && (this.__noShadows = !0, u(t, "DlDialog-noShadows")), this.title(this._title), this.setUnselectable(this.getTitleElement()), n = this.__quitBtn, n && (r = this.__quitBtn = new DlAbstractButton({parent: this,className: "DlDialog-QuitBtn",appendArgs: this.getButtonsElement(),classes: {hover: "DlDialog-QuitBtn-hover",active: "DlDialog-QuitBtn-active"}}), n == "destroy" ? n = this.destroy.$(this) : n == "hide" && (n = this.hide.$(this)), n instanceof Function ? r.addEventListener("onClick", n) : r.connectEvents("onClick", this, "onQuitBtn")), this._resizable && this.makeResizable(), this.setIconClass(this._iconClass), this._fixed || this.makeDraggable(), this.addEventListener({onMouseDown: this.activate,onMouseWheel: T,onDisplay: x,onDestroy: this.hide})
    }, t.setIconClass = function(e) {
        var t = this.getTitleElement().firstChild;
        f(t, e != null, "DlDialog-Title-withIcon"), this.iconClass && a(t, this.iconClass), e && u(t, e), this.iconClass = e
    }, t.getState = function() {
        var e = this.__maximized && this.__maximizeSavePos;
        return e ? e = Object.makeDeepCopy(e) : e = {pos: this.getOffsetPos(),size: this.getOuterSize()}, e.max = !!this.__maximized, e
    }, t.maximize = function(e) {
        var t, n;
        e == null && (e = this.__maxBtn.checked()), this.__maximized = e, e && (t = this.getOffsetPos(), n = this.getOuterSize(), this.__maximizeSavePos = {pos: t,size: n}), this.condClass(e, "DlDialog-Maximized"), e ? this.__doMaximize() : (t = this.__maximizeSavePos, n = t.size, t = t.pos, this.setOuterSize({x: n.x,y: n.y}), this.setPos(t.x, t.y)), this.__maxBtn.checked(e, !0), this._focusedWidget && !this._focusedWidget.destroyed && this._focusedWidget.focus()
    }, t.__doMaximize = function() {
        var e;
        this.setPos(0, 0), e = this.parent.getInnerSize(), this.setOuterSize({x: e.x,y: e.y})
    }, t.setModal = function(e, t) {
        t || (this.__modal = e), this.display() && this.parent.updateZIndex()
    }, t.modal = function() {
        return this.__modal
    }, t.makeResizable = function() {
        var e, t;
        this._resizeHandlers || (this.getContentElement().style.overflow = "hidden", e = this.getRelElement(), t = l("div", null, {className: "ResizeHandle"}, null), e.insertBefore(t, e.firstChild), this._resizeHandlers = {onMouseMove: S.$(this),onMouseUp: E.$(this),onMouseOver: o,onMouseOut: o,onMouseEnter: o,onMouseLeave: o}, n.addEvent(t, "mousedown", w.$(this)), this.resizing = !1, this.__maxBtn && (this.__maxBtn = new DlAbstractButton({parent: this,className: "DlDialog-MaximizeBtn",appendArgs: this.getButtonsElement(),type: DlAbstractButton.TYPE.TWOSTATE,classes: {hover: "DlDialog-MaximizeBtn-hover",active: "DlDialog-MaximizeBtn-active",checked: "DlDialog-MaximizeBtn-1"}}), this.__maxBtn.addEventListener("onChange", this.maximize.$0(this, null))))
    }, t.makeDraggable = function(e) {
        e || (e = this.getTitleElement(), e.style.cursor = "default", this.addEventListener(["onMouseDown", "onContextMenu"], d)), this._dragHandlers || (this._dragHandlers = {onMouseMove: b.$(this),onMouseUp: v.$(this),onMouseOver: o,onMouseOut: o,onMouseEnter: o,onMouseLeave: o}, this.dragging = !1), n.addEvent(e, "mousedown", p.$(this))
    }, t.title = function(e) {
        return e != null && (e instanceof Array && (e = e.join("")), this._title = e, this.getTitleElement().firstChild.innerHTML = e, this._noEmptyTitle && (this.getTitleElement().style.display = /\S/.test(e) ? "" : "none")), this._title
    }, t._handle_focusKeys = function(e) {
        var t;
        !e.altKey && !e.ctrlKey && (e.keyCode == DlKeyboard.ESCAPE ? !this.dragging && this.__quitBtn ? this.__quitBtn.keyClicked(e) : this.dragging && v.call(this) : e.keyCode == DlKeyboard.TAB && (t = e.focusedWidget, t = e.shiftKey ? this.getPrevFocusWidget(t) : this.getNextFocusWidget(t), t && t.focus(), e.domStop = !0, o())), this._handleKeybinding(e)
    }, t.getRelElement = function() {
        return this.getElement().firstChild
    }, t.getContentElement = function() {
        return N(this, 1)
    }, t.getTitleElement = function() {
        return N(this, 2)
    }, t.getButtonsElement = function() {
        return N(this, 3)
    }, t.centerOnParent = function() {
        var e = this.getOuterSize(), t = this.parent.getOuterSize();
        this.setPos((t.x - e.x) / 2, (t.y - e.y) / 2)
    }, t.getWM = function() {
        return this.parent
    }, DlWidget.prototype.getParentDialog = function() {
        var t = this.parent;
        while (t && !(t instanceof e))
            t = t.parent;
        return t
    }
}), DEFINE_CLASS("DlDialogPopup", DlPopup, function(e) {
    e.FIXARGS = function(e) {
        e.autolink = !1, e.zIndex = 5e3
    }
}), DEFINE_CLASS("DlRecord", DlEventProxy, function(e, t) {
    e.DEFAULT_EVENTS = ["onChange"], e.DEFAULT_ARGS = {_data: ["data", null],_set: ["recordSet", null]}, t.id = function() {
        return this._data.id
    }, t.get = function(e) {
        return this._data[e]
    }, t.set = function(e, t, n) {
        var r, i = this._data[e];
        this._data[e] = t, n || (r = [this, e, t, i], this.applyHooks("onChange", r), this._set && this._set.applyHooks("onChange", r))
    }, t.compareTo = function(e, t) {
        var n = this.get(t), r = e.get(t);
        return n < r ? -1 : n == r ? 0 : 1
    }
}), DEFINE_CLASS("DlRecordCache", DlEventProxy, function(e, t) {
    e.DEFAULT_EVENTS = ["onChange", "onInsert", "onBeforeDelete", "onDelete", "onRefresh"], e.DEFAULT_ARGS = {_data: ["data", null]}, e.CONSTRUCT = function() {
        this._init()
    }, t.get = function(e) {
        return this._data[e]
    }, t.hasRecord = function(e) {
        return this.get(e)
    }, t.getRecords = function(e, t, n) {
        t.call(n, e.map(this.get, this))
    }, t.getAllIds = function() {
        return Array.hashKeys(this._data)
    }, t.getAllRecords = function() {
        return Array.hashValues(this._data)
    }, t.getHash = function() {
        return this._data
    }, t.formatHTML = function(e, t, n, r) {
        var i = r ? r.format(e, this) : null;
        i == null && (i = (e.get(t) + "").htmlEscape()), n(i)
    }, t.getRecClass = function() {
    }, t.getCellClass = function() {
    }, t.getInsertPos = function() {
    }, t.insert = function(e, t) {
        t == null && (t = this.getInsertPos(e)), this._data[e.id()] = e, e._set = this, this.applyHooks("onInsert", [e, t])
    }, t.remove = function(e) {
        this.applyHooks("onBeforeDelete", [this.get(e)]), e instanceof Array ? e.foreach(function(e) {
            delete this._data[e]
        }, this) : delete this._data[e], this.applyHooks("onDelete", [e])
    }, t.sort = function(e, t, n, r, i, s) {
        var o;
        t == n && r !== null ? o = e.reverse() : o = e.mergeSort(function(e, n) {
            return e = this.get(e), n = this.get(n), e.compareTo(n, t)
        }.$(this), r), i.call(s, o)
    }, t._init = function() {
        var e;
        this._data ? (e = {}, this._data.foreach(function(t) {
            e[t.id()] = t, t._set = this
        }, this), this._data = e) : this._data = {}
    }
}), DEFINE_CLASS("DlDataGridHeadLabel", DlButton, function(e, t, n) {
    function s(e) {
        var t = e._resizeHandle;
        return t || (t = e._resizeHandle = new DlWidget({parent: e,className: "DlDataGrid-resizeHandle"}), t.display(!1), t.grid = e.parent, t.addEventListener({onMouseLeave: t.display.$(t, !1),onMouseDown: o}), t._resizeCaptures = {onMouseMove: u.$(t),onMouseUp: a.$(t),onMouseOver: i,onMouseOut: i,onMouseEnter: i,onMouseLeave: i}), t
    }
    function o(e) {
        var t, r, s;
        this.dragging = !0, t = DlResizeBar.getDragBar(), r = t.style, this.sl = this.grid.getBodyDiv().scrollLeft, r.left = this.pos - this.sl - 1 + "px", r.height = "100%", r.width = this.getElement().offsetWidth - 4 + "px", r.top = "0px", this.grid.getElement().appendChild(t), s = DlDialog.activateEventStopper(!0), n.addClass(s, "CURSOR-RESIZE-E"), DlEvent.captureGlobals(this._resizeCaptures), this.origW = this.grid.getColWidth(this.col), this.origM = e.pos.x, this.col._button.addClass("DlDataGridHeadLabel-resizing"), i()
    }
    function u(e) {
        var t, n = DlResizeBar.getDragBar(), s = this.pos + e.pos.x - this.origM - 1, o = s - this.pos, u = this.origW + o;
        u < r && (s += r - u, u = r), s -= this.sl, n.style.left = s + "px", this.width = u, this.diff = o, is_ie || (t = this.col._button.getElement(), t.style.width = t.parentNode.style.width = t.parentNode.parentNode.style.width = u + "px"), i()
    }
    function a(e) {
        var t, r;
        this.dragging = !1, this.grid.getElement().removeChild(DlResizeBar.getDragBar()), t = DlDialog.activateEventStopper(!1), n.delClass(t, "CURSOR-RESIZE-E"), DlEvent.releaseGlobals(this._resizeCaptures), this.width && (this.grid.setColWidth(this.col, this.width), this.grid._computeColPos()), this.col._button.delClass("DlDataGridHeadLabel-resizing"), is_ie || (r = this.col._button.getElement(), r.style.width = r.parentNode.style.width = r.parentNode.parentNode.style.width = ""), this.width = this.diff = this.found = this.col = null, this.parent.callHooks("onMouseMove", e), i()
    }
    var r, i;
    e.FIXARGS = function(e) {
        "contextMenu" in e || (e.contextMenu = this._getContextMenu)
    }, e.CONSTRUCT = function() {
        var e;
        this.isSortable() || (e = this._classes = Object.makeCopy(this._classes), e.active = e.hover = null)
    }, r = 20, t.setWidth = function(e) {
        this.setOuterSize({x: e})
    }, t.isSortable = function() {
        return this.col.isSortable()
    }, t._onClick = function() {
        this.isSortable() && this.parent._onHeadClick(this.col, this)
    }, t._getContextMenu = function() {
        var e, t = this._dgContextMenu, n = this.parent;
        return t ? (e = t.buttons, n._cols.foreach(function(t, n) {
            e[n].checked(t.isVisible(), !0)
        })) : (this._dgContextMenu = t = new DlVMenu({}), e = t.buttons = [], n._cols.foreach(function(n, r) {
            var i;
            n.getMenuLabel() && (i = e[r] = new DlCheckbox({parent: t,label: n.getMenuLabel(),checked: n.isVisible()}), i.addEventListener("onChange", function() {
                n.setVisible(this.checked())
            }))
        })), t
    }, e.DEFAULT_ARGS = {col: ["col", null],_focusable: ["focusable", !1],_noCapture: ["noCapture", !0],_classes: ["classes", {active: "DlDataGridHeadLabel-active",hover: "DlDataGridHeadLabel-hover",checked: "DlDataGridHeadLabel-1",unchecked: "DlDataGridHeadLabel-0",empty: "DlDataGridHeadLabel-empty",disabled: "DlDataGridHeadLabel-disabled"}]}, i = DlException.stopEventBubbling, e._on_headMouseMove = function(e) {
        var t, n, r, i, o;
        if (!this._colPos || this.dragging)
            return;
        t = this.parent, n = t.getBodyDiv().scrollLeft, r = e.computePos(this).x + n, this._colPos.foreach(function(e) {
            Math.abs(r - e.pos) < 4 && (e.col.isResizable() && (i = e), $BREAK())
        }), o = s(this), i ? (i.col !== o.col && (o.found = i, o.col = i.col, o.pos = i.pos, o.setPos(i.pos - n)), o.display(!0)) : (o.display(!1), o.found = o.col = null)
    }, e._on_headMouseLeave = function() {
        s(this).display(!1)
    }
}), DEFINE_CLASS("DlGridCol", DlEventProxy, function(e, t) {
    var n;
    e.DEFAULT_EVENTS = ["onChange", "onVisibility"], e.DEFAULT_ARGS = {_field_id: ["id", null],_width: ["width", null],_fill: ["fill", null],_style: ["style", null],_label: ["label", null],_menuLabel: ["menuLabel", null],_tooltip: ["tooltip", null],_iconClass: ["iconClass", null],_isSortable: ["sortable", !0],_isResizable: ["resizable", !0],_cssRule: ["cssRule", null],_isVisible: ["visible", !0],_format: ["format", null]}, n = {}, t.id = function() {
        return this._field_id
    }, t.getWidth = function() {
        return this._width
    }, t.getFill = function() {
        return this._fill
    }, t.getLabel = function() {
        return this._label
    }, t.getMenuLabel = function() {
        return this._menuLabel || this._label
    }, t.getTooltip = function() {
        return this._tooltip
    }, t.getIconClass = function() {
        return this._iconClass
    }, t.getStyle = function(e, t) {
        return (this._style || n)[e] || t
    }, t.isSortable = function() {
        return this._isSortable
    }, t.isResizable = function() {
        return this._isResizable
    }, t.isVisible = function() {
        return this._isVisible
    }, t.setVisible = function(e) {
        this._isVisible = e, this.applyHooks("onVisibility", [e])
    }, t.sort = function() {
    }, t.format = function(e, t) {
        if (this._format)
            return this._format(e, t, this.id())
    }
}), DEFINE_CLASS("DlGridDragCol", DlDrag, function(e, t, n) {
    function i() {
        var e = r;
        return e || (e = r = n.createElement("div", {display: "none"}, {className: "DlDataGrid-drop-col"}, document.body)), e
    }
    var r;
    e.CONSTRUCT = function() {
        this.addEventListener("onStartDrag", function(e) {
            this.grid_pos = e.parent.getPos(), e._onMouseLeave()
        })
    }, t.startOK = function() {
        return !0
    }, t.dropOK = function(e, t, n, r) {
        return !r && n instanceof DlDataGridHeadLabel && e.parent === n.parent ? (this.target = n, this.canDrop = !0) : this.canDrop = !1
    }, t.doDrop = function(e) {
        e.parent.reorderColumn(e.col, this.target.col, !this.dropBefore)
    }, t.moving = function(e, t) {
        var n, r, s, o, u, a = this.target, f = i(), l = f.style;
        this.canDrop && a ? (n = t.computePos(a), r = a.getElement(), s = r.offsetWidth, o = n.x <= s / 2, u = a.col.index, o && u--, u < 0 ? u = 0 : u = e.parent._headCont._colPos[u].pos - e.parent.getBodyDiv().scrollLeft, l.display = "block", l.left = u + this.grid_pos.x + "px", l.top = n.elPos.y + "px", this.dropBefore = o) : a && (this.dropBefore = null, l.display = "none")
    }, t.reset = function() {
        r && (r.style.display = "none"), e.BASE.reset.apply(this, arguments)
    }
}), DEFINE_CLASS("DlSelectionModel", DlEventProxy, function(e, t) {
    e.DEFAULT_EVENTS = ["onChange", "onReset"], e.CONSTRUCT = function() {
        this.sel || (this.sel = {})
    }, e.DEFAULT_ARGS = {multiple: ["multiple", !0],sel: ["sel", null]}, t.reset = function(e, t) {
        var n = this.sel;
        this.sel = e.toHash(!0), t || this.applyHooks("onReset", [n, this.sel])
    }, t.clear = function(e) {
        this.reset([], e)
    }, t.get = function() {
        return this.sel
    }, t.getArray = function() {
        return Array.hashKeys(this.sel)
    }, t.getFirst = function() {
        var e;
        for (e in this.sel)
            return e
    }, t.isSelected = function(e) {
        return this.sel[e]
    }, t.size = function() {
        var e, t = 0;
        for (e in this.sel)
            t++;
        return t
    }, t.filter = function(e) {
        var t, n = [];
        for (t in this.sel)
            t in e || n.push(t);
        this.unselect(n)
    }, t.select = function(e, t) {
        var n, r = this.sel, i = null;
        return e instanceof Array ? (n = {}, e.foreach(function(e) {
            r[e] || (r[e] = n[e] = !0, i = !0)
        }, this), !t && i != null && this.applyHooks("onReset", [{}, n])) : r[e] || (r[e] = !0, t || this.applyHooks("onChange", [e, !0]), i = !0), i
    }, t.unselect = function(e, t) {
        var n, r = this.sel, i = null;
        return e instanceof Array ? (n = {}, e.foreach(function(e) {
            r[e] && (delete r[e], n[e] = !0, i = !1)
        }, this), !t && i != null && this.applyHooks("onReset", [n, {}])) : r[e] && (delete r[e], t || this.applyHooks("onChange", [e, !1]), i = !1), i
    }, t.toggle = function(e, t) {
        return this.sel[e] ? this.unselect(e, t) : this.select(e, t)
    }
}), DEFINE_CLASS("DlDataGrid", DlContainer, function(e, t, n) {
    function a(e) {
        var t, n, r, i, s, o = e.target;
        try {
            while (o && o.tagName) {
                s = o.tagName.toLowerCase();
                if (s == "div" && (r = o.getAttribute("recid")) != null) {
                    t = o;
                    break
                }
                !i && s == "td" && (n = o, i = o.getAttribute("colid")), o = o.parentNode
            }
        } catch (u) {
        }
        return t ? {row: t,col: n,id: r,col_id: i} : null
    }
    function f() {
        var e, t, r, i, s, o, u, a, f;
        if (this._processing_scroll)
            return;
        this._processing_scroll = !0, e = this.getBodyDiv(), t = e.scrollTop, this.getHeaderTable().style.marginLeft = -e.scrollLeft + "px";
        if (this._vScroll && this._records && t != this._oldScroll) {
            r = this.getRowsContainer(), i = this._records.array, s = this._info_display, o = this.getBoundRecords(), u = o.first, a = o.last, o = o.h;
            if (u < s.first_row_index || a > s.last_row_index)
                a < s.first_row_index || u > s.last_row_index ? (f = i.slice(u, u + this._rpp), this._display_ids(f, o * u)) : u < s.first_row_index ? (f = i.slice(u, s.first_row_index), this.__scrollConts++, this.__cont = function(e) {
                    var t, i, a, f, l, c = String.buffer("<div>"), p = e.length;
                    e.foreach(this._fetchRowHTML.$(this, c)), c("</div>"), t = c.get(), i = n.createFromHtml(t);
                    try {
                        f = document.createRange(), l = r.childNodes, f.selectNodeContents(i), a = f.extractContents(), f.detach(), f = document.createRange(), f.setStartBefore(l[l.length - p]), f.setEndAfter(l[l.length - 1]), f.deleteContents()
                    } catch (d) {
                        a || (a = document.createDocumentFragment());
                        while (i.firstChild)
                            r.removeChild(r.lastChild), a.appendChild(i.firstChild)
                    }
                    r.insertBefore(a, r.firstChild), this._setVScroll(o * u), s.first_row_index = u, s.last_row_index -= p
                }, this._fetch_data(f, -1, function(e) {
                    this.__scrollConts--, this.__scrollConts == 0 && this.__cont(e)
                })) : a > s.last_row_index && (f = i.slice(s.last_row_index + 1, a + 1), this.__scrollConts++, this.__cont = function(e) {
                    var t, i, u, f, l = String.buffer("<div>"), c = e.length;
                    e.foreach(this._fetchRowHTML.$(this, l)), l("</div>"), t = l.get(), i = n.createFromHtml(t);
                    try {
                        f = document.createRange(), f.selectNodeContents(i), u = f.extractContents(), f.detach(), f = document.createRange(), f.setStartBefore(r.firstChild), f.setEndBefore(r.childNodes[c]), f.deleteContents()
                    } catch (p) {
                        u || (u = document.createDocumentFragment());
                        while (i.firstChild)
                            r.removeChild(r.firstChild), u.appendChild(i.firstChild)
                    }
                    r.appendChild(u), this._setVScroll(this._getVSScrollDiv().offsetHeight + o * c), s.first_row_index += c, s.last_row_index = a
                }, this._fetch_data(f, 1, function(e) {
                    this.__scrollConts--, this.__scrollConts == 0 && this.__cont(e)
                }));
            this._oldScroll = t
        }
        this.callHooks("onBodyScroll"), this._processing_scroll = !1
    }
    var r, i, s = n.addClass, o = n.delClass, u = n.condClass;
    e.DEFAULT_EVENTS = ["onBodyDblClick", "onBodyScroll", "onRowClick", "onRowDblClick", "onResetIds"], e.CONSTRUCT = function() {
        this.__scrollConts = 0
    }, r = DlException.stopEventBubbling, e.DEFAULT_ARGS = {_records: ["records", null],_selection: ["selection", null],_data: ["data", null],_page: ["page", 0],_rpp: ["rpp", 60],_minReqRows: ["minReq", null],_threshold: ["threshold", null],_vScroll: ["virtualScrolling", !0],_cols: ["cols", null],_headType: ["headType", DlDataGridHeadLabel],_focusable: ["focusable", !0],_rtClickKeepSel: ["rightClickKeepsSel", !1],_noReselect: ["noReselect", !1],_rarify: ["rarifyScroll", null]}, i = String.buffer("<div class='DlDataGrid-Headers'>", "<table class='DlDataGrid-rowTable' cellspacing='0' cellpadding='0'>", "<tbody><tr></tr></tbody>", "</table></div>", "<div class='DlDataGrid-Body'>", "<div class='DlDataGrid-VSHeight'>", "<div class='DlDataGrid-VSHeight-before'></div>", "<div class='DlDataGrid-RowsCont'></div>", "</div>", "</div>").get(), t.getHeaderDiv = function() {
        return this.getElement().firstChild
    }, t.getHeaderTable = function() {
        return this.getHeaderDiv().firstChild
    }, t.getHeaderRow = function() {
        return this.getHeaderDiv().firstChild.rows[0]
    }, t.getBodyDiv = function() {
        return this.getElement().childNodes[1]
    }, t._getVSHeightDiv = function() {
        return this.getBodyDiv().firstChild
    }, t._getVSScrollDiv = function() {
        return this._getVSHeightDiv().firstChild
    }, t.getRowsContainer = function() {
        return this._getVSHeightDiv().childNodes[1]
    }, t.resetIDS = function(e) {
        var t = {}, n = this._selection;
        e.foreach(function(e, n) {
            t[e] = n
        }), this._records = {array: e,id_to_pos: t}, n.filter(t), n.getArray().length == 0 && (n._last = null), this.callHooks("onResetIds")
    }, t._fetch_data = function(e, t, n) {
        var r, i, s, o, u, a, f, l = this._minReqRows, c = e.length, h = this._data;
        if (l != null && l > c) {
            r = e.slice(0), i = this._records.array, o = this._threshold || Math.ceil(this._rpp / 2);
            if (t <= 0) {
                u = this._records.id_to_pos[e[0]], a = o;
                while (a-- > 0)
                    if (!h.hasRecord(i[--u]))
                        break;
                if (a > 0)
                    for (s = u; s >= 0 && r.length < l; s--)
                        f = i[s], h.hasRecord(f) || r.push(f)
            }
            if (t >= 0) {
                u = this._records.id_to_pos[e.peek()], a = o;
                while (a-- > 0)
                    if (!h.hasRecord(i[++u]))
                        break;
                if (a > 0)
                    for (s = u; s < i.length && r.length < l; s++)
                        f = i[s], h.hasRecord(f) || r.push(f)
            }
            h.getRecords(r, function(e) {
                n.call(this, e.slice(0, c))
            }, this)
        } else
            h.getRecords(e, n, this)
    }, t._display_ids = function(e, t) {
        this._info_display = {length: e.length,first_row_index: this._records.id_to_pos[e[0]],last_row_index: this._records.id_to_pos[e.peek()]}, this._fetch_data(e, 0, function(e) {
            var n = String.buffer();
            e.foreach(this._fetchRowHTML.$(this, n)), this.getRowsContainer().innerHTML = n.get(), t != null && this._setVScroll(t), this._resetVSHeight(), e.length > 1 ? this.scrollToRecord(e[1].id()) : this._setVScroll(this.getBodyDiv().scrollTop = 0), this.getBoundRecords()
        })
    }, t.displayPage = function(e) {
        var t, n;
        e == null && (e = 0), this._page = e, t = this._records.array, this._rpp && (n = e * this._rpp, t = t.slice(n, n + this._rpp)), this._display_ids(t)
    }, t._resetVSHeight = function() {
        this._vScroll && this._info_display && (h = Math.floor(this.getRowsContainer().offsetHeight * this.getNRecords() / this._info_display.length), this._getVSHeightDiv().style.height = isNaN(h) ? "" : h + "px")
    }, t._setVScroll = function(e) {
        var t = this._getVSScrollDiv();
        e ? (t.style.height = e + "px", t.style.display = "block") : t.style.display = "none"
    }, t.initWidths = function() {
        var e, t, n, r, i, s, o = {};
        this._cols.foreach(function(e) {
            o[e.id()] = this.getColWidth(e)
        }, this), e = this.getRowsContainer();
        for (t = e.firstChild; t; t = t.nextSibling) {
            n = t.firstChild.rows[0].cells;
            for (r = n.length; --r >= 0; )
                i = n[r], s = i.getAttribute("colid"), o[s] = Math.max(o[s] || 0, i.offsetWidth)
        }
        this._cols.foreach(function(e) {
            this.setColWidth(e, o[e.id()])
        }, this)
    }, t.resetColumns = function(e) {
        var t, n = this.getHeaderRow(), r = n.cells, i = [];
        this._cols = e.map(function(e, t) {
            var n = this._colsById[e.id];
            return i.push(n._cell), n.index = t, n._width = e.width, n._isVisible = e.visible, n
        }, this), t = document.createDocumentFragment(), i.foreach(function(e) {
            t.appendChild(e)
        }), n.appendChild(t), this.refreshDisplay(), this._cols.foreach(function(e) {
            this.setColVisible(e, e.isVisible()), this.setColWidth(e, e.getWidth())
        }, this)
    }, t.reorderColumn = function(e, t, n) {
        var e, t, r, i, s, o = e.index, u = t.index;
        n && u++, r = this._cols, r.splice(o, 1), r.splice(o < u ? u - 1 : u, 0, e);
        for (i = 0; i < r.length; ++i)
            r[i].index = i;
        for (i = this.getRowsContainer().firstChild; i; i = i.nextSibling)
            s = i.firstChild.rows[0].cells, e = s[o], t = s[u], e.parentNode.insertBefore(e, t || null);
        s = this.getHeaderRow().cells, e = s[o], t = s[u], e.parentNode.insertBefore(e, t || null), this._computeColPos()
    }, t.getNRecords = function() {
        return this._records ? this._records.array.length : 0
    }, t.getNPages = function() {
        return this._rpp ? Math.ceil(this.getNRecords() / this._rpp) : 1
    }, t.rec_isSelected = function(e) {
        return this._selection.isSelected(e.id())
    }, t._computeColPos = function() {
        var e = -1;
        this._headCont._colPos = this._cols.map(function(t) {
            return e += this.getColWidth(t), {pos: e,col: t}
        }, this)
    }, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), this.getElement().id = this.id, this._ss = new DlStyleSheet, this._cssPrefix = "#" + this.id, this.setContent(i), this._initHeaders(), this.getBodyDiv().onscroll = this._rarify ? f.rarify(this._rarify.calls, this._rarify.timeout, this) : f.$(this), t = this._headCont = new DlContainer({parent: this,element: this.getHeaderDiv()}), t.addEventListener({onMouseMove: DlDataGridHeadLabel._on_headMouseMove,onMouseLeave: DlDataGridHeadLabel._on_headMouseLeave,onMouseEnter: this._computeColPos.$(this)}), this._bodyCont = new DlContainer({parent: this,element: this.getBodyDiv(),drag: this._dragArgs}), this._dragArgs = null, "onMouseOver onMouseOut onMouseDown onMouseUp onMouseLeave onDblClick".qw().foreach(function(e) {
            this.addEventListener(e, this["_body_" + e])
        }, this), this._cacheEvents = {onChange: this._data_onChange.$(this),onInsert: this._data_onInsert.$(this),onDelete: this._data_onDelete.$(this),onRefresh: this._data_onRefresh.$(this)}, this.setCache(this._data), this.addEventListener("onDestroy", this._onDestroy), this._records && this.resetIDS(this._records), this._sel_events = {onChange: this._sel_onChange.$(this),onReset: this._sel_onReset.$(this)}, this._selection || (this._selection = new DlSelectionModel({})), this.setSelectionModel(this._selection)
    }, t._onDestroy = function() {
        this._ss.destroy(), this.setCache(null)
    }, t.setCache = function(e) {
        this._data && this._data.removeEventListener(this._cacheEvents), this._data = e, e && e.addEventListener(this._cacheEvents)
    }, t._data_onChange = function(e) {
        var t, r, i = this.getRowElement(e.id());
        i && (t = String.buffer(), this._fetchRowHTML(t, e), t = t.get(), is_ie ? i.outerHTML = t : (r = n.createFromHtml(t), n.trash(i.parentNode.replaceChild(r, i))))
    }, t._data_onInsert = function(e, t) {
        var n = this._records.array;
        t == null && (t = n.length), n.splice(t, 0, e.id()), this.resetIDS(n), this.refreshDisplay()
    }, t._data_onDelete = function(e) {
        var t = this._records.array;
        e instanceof Array ? e.foreach(function(e) {
            this.remove(e)
        }, t) : t.remove(e), this.resetIDS(t), this.refreshDisplay()
    }, t._data_onRefresh = function() {
    }, t._recompDynamicWidths = function() {
        var e = this.getBodyDiv().clientWidth, t = [];
        this._cols.foreach(function(n) {
            n.getFill() == null ? e -= this.getColWidth(n) : t.push(n)
        }, this), e -= 1, t.foreach(function(t) {
            this.setColWidth(t, e * t.getFill())
        }, this)
    }, t._initHeaders = function() {
        this._colsById = {}, this._cols.foreach(function(e, t) {
            var n, r, i, s, o, u;
            e instanceof DlGridCol || (e = this._cols[t] = new DlGridCol(e)), e.addEventListener("onVisibility", this.setColVisible.$(this, e)), e.index = t, this._colsById[e.id()] = e, n = "DlDataGrid-col-" + e.id(), r = this._cssPrefix + " ." + n, r = r + "," + r + " .DlDataGrid-cellData", i = [], s = e.getWidth(), typeof s == "number" && i.push("width:" + s + "px"), i = i.join(";"), e._cssRule = this._ss.insertRule(r, i), e.isVisible() || this._ss.modifyRule(e._cssRule, {display: "none"}), o = e._cell = document.createElement("td"), o.innerHTML = "<div class='DlDataGrid-cellData'></div>", o.className = n, this.getHeaderRow().appendChild(o), u = this._makeHeadLabel({parent: this,appendArgs: o.firstChild,iconClass: e.getIconClass(),label: e.getLabel(),col: e,className: "DlGrid-align-" + e.getStyle("textAlign", "left"),tooltip: e.getTooltip.$(e),drag: this._getDragObject()}), e._button = u
        }, this)
    }, t.findRowFromEvent = function(e) {
        return a(e)
    }, t._sel_onChange = function(e, t) {
        var n = this.getRowElement(e);
        n && u(n, t, "DlDataGridRow-selected")
    }, t._sel_onReset = function(e, t) {
        var n, r;
        for (n in e)
            t[n] || (r = this.getRowElement(n), r && o(r, "DlDataGridRow-selected"));
        for (n in t)
            e[n] || (r = this.getRowElement(n), r && s(r, "DlDataGridRow-selected"))
    }, t.setSelectionModel = function(e) {
        this._selection && this._selection.removeEventListener(this._sel_events), this._selection = e, e.addEventListener(this._sel_events)
    }, t._body_onDblClick = function(e) {
        var t;
        this.callHooks
        ("onBodyDblClick"), t = a(e), t && this.callHooks("onRowDblClick", t)
    }, t._body_onMouseOver = function(e) {
        var t = a(e);
        t && this.__tooltip instanceof Function && (this._tooltipRow = t, DlWidget.getTooltip().popup({timeout: this.__tooltipTimeout,content: this.__tooltip.$(this, t),anchor: this.getElement(),align: "mouse",onPopup: this.__onTooltipShow,onHide: this.__onTooltipHide,widget: this}))
    }, t._body_onMouseOut = function(e) {
        var t = a(e);
        t && (DlWidget.getTooltip().hide(), this._tooltipRow = null)
    }, t._body_onMouseLeave = function() {
    }, t.__handleSelectClick = function(e, t) {
        var n, r, i, s = this._selection, o = this._records;
        if (s.multiple)
            t.button == 2 ? this._rtClickKeepSel || (t.ctrlKey ? (this.callHooks("onRowClick", e, t, {rtc: !0,ctrl: !0,type: "select",ids: [e.id]}), s.select([e.id])) : s.isSelected(e.id) || (this.callHooks("onRowClick", e, t, {rtc: !0,type: "reset",ids: [e.id]}), s.reset([e.id]))) : t.ctrlKey ? (this.callHooks("onRowClick", e, t, {ctrl: !0,type: "toggle",ids: [e.id]}), s.toggle(e.id), s._last = e.id) : t.shiftKey ? s._last != null ? (n = o.id_to_pos[s._last], r = o.id_to_pos[e.id], i = o.array.slice(Math.min(n, r), Math.max(n, r) + 1), this.callHooks("onRowClick", e, t, {shift: !0,type: "reset",ids: i}), s.reset(i)) : (this.callHooks("onRowClick", e, t, {shift: !0,type: "toggle",ids: [e.id]}), s.toggle(e.id), s._last = e.id) : (this.callHooks("onRowClick", e, t, {type: "reset",ids: [e.id]}), s.reset([e.id]), s._last = e.id);
        else if (!this._noReselect || !s.isSelected(e.id))
            this.callHooks("onRowClick", e, t, {type: "reset",ids: [e.id]}), s.reset([e.id]), s._last = e.id
    }, t._body_onMouseUp = function(e) {
        var t = a(e), n = this.__handleOnMouseUp;
        t && n && t.id == n.id && this.__handleSelectClick(t, e)
    }, t._body_onMouseDown = function(e) {
        var t = a(e), n = this._selection;
        t && (this.__handleOnMouseUp = !n.isSelected(t.id) || !this._bodyCont._dragArgs || e.ctrlKey || e.shiftKey ? null : t, this.__handleOnMouseUp || this.__handleSelectClick(t, e), e.button != 2 && r())
    }, t.scrollToRecord = function(e, t) {
        var n = this.getRowsContainer(), r = Math.floor(n.offsetHeight / n.childNodes.length), i = r * this._records.id_to_pos[e || this._selection._last], s = this.getBodyDiv(), o = s.scrollTop, u = s.clientHeight;
        if (t == null)
            i < o ? (s.scrollTop = i, this._setVScroll(r * this._info_display.first_row_index)) : i + r > o + u && (s.scrollTop = i + r - u, this._setVScroll(r * this._info_display.first_row_index));
        else
            switch (t) {
                case "top":
                    s.scrollTop = i;
                    break;
                case "bottom":
                    s.scrollTop = i + r - u;
                    break;
                case "center":
                    s.scrollTop = (2 * i + r - u) / 2
            }
    }, t.scrollHome = function() {
        this.getBodyDiv().scrollTop = 0
    }, t.scrollEnd = function() {
        this.getBodyDiv().scrollTop = this._getVSHeightDiv().offsetHeight
    }, t.scrollPage = function(e) {
        var t = this.getBodyDiv();
        t.scrollTop += e * t.clientHeight - 20
    }, t._handle_focusKeys = function(t) {
        var n, i, s, o = this._selection, u = t.keyCode, a = t.charCode, f = this._records;
        switch (u) {
            case DlKeyboard.ARROW_DOWN:
                n = -1, o._last != null && (n = f.id_to_pos[o._last]), t.shiftKey && o.multiple ? (i = f.array.slice(n, n + 2), o.select(i), o._last = i.peek()) : (n = f.array.limitIndex(n + 1), s = f.array[n], o.reset([s]), o._last = s), this.scrollToRecord(), r();
                break;
            case DlKeyboard.ARROW_UP:
                n = f.array.length, o._last != null && (n = f.id_to_pos[o._last]), t.shiftKey && o.multiple ? (i = f.array.slice(n - 1, n), o.select(i), o._last = i.peek()) : (n = f.array.limitIndex(n - 1), s = f.array[n], o.reset([s]), o._last = s), this.scrollToRecord(), r();
                break;
            case DlKeyboard.HOME:
                this.scrollHome(), r();
                break;
            case DlKeyboard.END:
                this.scrollEnd(), r();
                break;
            case DlKeyboard.PAGE_UP:
                this.scrollPage(-1), r();
                break;
            case DlKeyboard.PAGE_DOWN:
                this.scrollPage(1), r()
        }
        e.BASE._handle_focusKeys.call(this, t)
    }, t._makeHeadLabel = function(e) {
        return new this._headType(e)
    }, t._onHeadClick = function(e) {
        var t, n;
        e.isSortable() && (e = e.id(), t = this.__sortCol || null, n = null, e == t && (n = !0, this.__sortRev && (n = !n)), this.__sortRev = n, this.sort(this._records.array, e, t, n, this._handleSort.$(this, e, n)))
    }, t.sort = function() {
        this._data.sort.apply(this._data, arguments)
    }, t._handleSort = function(e, t, n) {
        this.resetIDS(n), this.refreshDisplay(), this.setSortColumn(e, t)
    }, t.setSortColumn = function(e, t) {
        var n = this.__sortCol;
        n && (n = this._colsById[n], n._button.delClass(/DlDataGridHeadLabel-sort-[^\s]+/g)), this.__sortCol = e, e && this._colsById[e]._button.condClass(t, "DlDataGridHeadLabel-sort-down", "DlDataGridHeadLabel-sort-up")
    }, t.getSortColumn = function() {
        return this.__sortCol
    }, t.getSortReverse = function() {
        return this.__sortRev
    }, t.getCol = function(e) {
        return e instanceof DlGridCol || (e = this._colsById[e]), e
    }, t.getRec = function(e) {
        return e instanceof DlRecord || (e = this._data.get(e)), e
    }, t.setColWidth = function(e, t) {
        e = this.getCol(e), e._width = t, this._ss.modifyRule(e._cssRule, {width: t + "px"})
    }, t.setColVisible = function(e, t) {
        e = this.getCol(e), this._ss.modifyRule(e._cssRule, {display: t ? "" : "none"}), e._isVisible = !!t
    }, t.getColWidth = function(e) {
        return e = this.getCol(e), this.getHeaderRow().cells[e.index].offsetWidth
    }, t._getDragObject = function() {
        return this.__drag || (this.__drag = new DlGridDragCol({})), this.__drag
    }, t._fetchRowContentHTML = function(e, t) {
        var n, r, i, s, o, u, a;
        e("<table class='DlDataGrid-rowTable' cellspacing='0' cellpadding='0'><tr>"), n = this._cols, r = n.length, i = this._data;
        for (a = 0; a < r; ++a)
            s = n[a], o = s.id(), e("<td colid='", o, "' class='DlDataGrid-col-", o), u = i.getCellClass(t, s.id()), u && e(" ", u), e("'>"), is_ie && e("<div class='DlDataGrid-cellData'>"), i.formatHTML(t, s.id(), e, s), is_ie && e("</div>"), e("</td>");
        e("</tr></table>")
    }, t._fetchRowHTML = function(e, t) {
        var n = "DlDataGrid-row", r = this._data.getRecClass(t);
        r && (n += " " + r), this.rec_isSelected(t) && (n += " DlDataGridRow-selected"), e("<div id='", this.id, ":", t.id(), "' class='", n, "' recid='", t.id(), "'>"), this._fetchRowContentHTML(e, t), e("</div>")
    }, t.getRowElement = function(e) {
        return document.getElementById(this.id + ":" + e)
    }, t.refreshDisplay = function() {
        var e, t, n, r, i, s, o, u, a;
        this._oldScroll = null, e = this.getBodyDiv(), t = e.scrollTop, n = this.getRowsContainer(), r = this._records.array, i = this._info_display, this._rpp && r.length < this._rpp && this.displayPage(0), this._rpp && (s = Math.floor(n.offsetHeight / n.childNodes.length), o = Math.ceil(t / s) - 1, o < 0 && (o = 0), u = Math.floor((t + e.clientHeight) / s), u >= r.length && (u = r.length - 1), a = r.slice(o, o + this._rpp), this._display_ids(a, s * o))
    }, t.__doLayout = function() {
        var e = this.getInnerSize(), t = this.getBodyDiv(), r = this.getHeaderDiv();
        n.setOuterSize(t, e.x, e.y - r.offsetHeight), n.setOuterSize(r, e.x, null), this._resetVSHeight(), this._records && this._records.array.length > 0 && (this._oldScroll = null, f.call(this)), this._recompDynamicWidths()
    }, t.getBoundRecords = function() {
        var e, t, n, r = this.getBodyDiv(), i = r.scrollTop, s = this.getRowsContainer(), o = this._records.array, u = s.offsetHeight;
        return u == 0 ? this.__boundRecords : (e = Math.floor(u / s.childNodes.length), t = Math.ceil(i / e) - 1, t < 0 && (t = 0), n = Math.floor((i + r.clientHeight) / e), n >= o.length && (n = o.length - 1), this.__boundRecords = {first: t,last: n,count: n - t + 1,h: e})
    }
}), DEFINE_CLASS("DlDragDataGrid", DlDrag, function(e, t) {
    t.startOK = function(e, t) {
        var n = e.parent, r = !1, i = t.target;
        while (i && i != e.getElement()) {
            if (i == n._getVSHeightDiv()) {
                r = !0;
                break
            }
            i = i.parentNode
        }
        return r && n._selection.getArray().length > 0 ? this.grid = n : r = !1, r
    }, t.reset = function() {
        this.grid = null, e.BASE.reset.apply(this, arguments)
    }
}), DEFINE_CLASS("DlDesktop", DlContainer, function(e, t) {
    var n, r;
    e.DEFAULT_ARGS = {_bounds: ["bounds", new DlRect(50, 30, 800, 600)]}, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getElement(), this._bounds.positionDiv(t), document.body.appendChild(t)
    }, is_ie && (n = Dynarch.ID("IEsux"), r = function() {
        var e = document.getElementById(n);
        e || (e = document.createElement("div"), e.style.position = "absolute", e.style.right = e.style.bottom = e.style.width = e.style.height = "0px", e.style.zIndex = "-100", document.body.appendChild(e)), this.setSize({x: e.offsetLeft,y: e.offsetTop + e.offsetHeight})
    }), t.fullScreen = function() {
        var e, t = this.getElement().style;
        t.top = "0px", t.left = "0px", t.width = "100%", t.height = "100%", is_ie ? e = r.$(this) : e = this.callHooks.$(this, "onResize"), DynarchDomUtils.addEvent(window, "resize", e.clearingTimeout(25))
    }
}), DEFINE_CLASS("DlTable", DlContainer, function(e, t, n) {
    var r = n.createElement;
    e.FIXARGS = function(e) {
        e.tagName = "table", this._colSpan = 0
    }, e.DEFAULT_ARGS = {__cellSpacing: ["cellSpacing", null],__cellPadding: ["cellPadding", null],__align: ["align", null]}, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getElement(), this.__cellPadding != null && (t.cellPadding = this.__cellPadding), this.__cellSpacing != null && (t.cellSpacing = this.__cellSpacing), this.__align != null && (t.align = this.__align), r("tbody", null, null, t)
    }, t.getContentElement = function() {
        return this.getElement().firstChild
    }, t.addRow = function() {
        return new DlTableRow({parent: this})
    }, t.getRow = function(e) {
        return this.children(e)
    }, t.addCell = function(e, t, n) {
        var r, i, s = new DlTableCell({parent: e});
        return t != null && s.addClass("DlAlign-" + t), n != null && (r = s.getElement().style, r.verticalAlign = n), i = s.getElement().cellIndex + 1, i > this._colSpan && (this._colSpan = i), s
    }, t.getColSpan = function() {
        return this._colSpan
    }, t.setColSpan = function(e) {
        this._colSpan = e
    }, t.addSeparator = function(e) {
        e == null && (e = this.getColSpan()), r("div", null, {innerHTML: "&nbsp;"}, r("td", null, {colSpan: e}, r("tr", null, {className: "DlTable-RowSeparator"}, this.getContentElement())))
    }
}), DEFINE_CLASS("DlTableRow", DlContainer, function(e) {
    e.DEFAULT_ARGS = {_tagName: ["tagName", "tr"]}
}), DEFINE_CLASS("DlTableCell", DlContainer, function(e) {
    e.DEFAULT_ARGS = {_tagName: ["tagName", "td"]}
}), DEFINE_CLASS("DlFieldGrid", DlTable, function(e, t) {
    e.CONSTRUCT = function() {
        this.__fields = {}
    }, t.addField = function(e, t, n) {
        var r, i, s, o, u, a, f = e.widget || new DlEntry(e), l = e.label;
        return t || (t = {}), l && (l instanceof DlWidget ? l instanceof DlLabel && l.setWidget(f) : l = new DlLabel({label: e.label.makeLabel(),widget: f})), r = this.addRow(), i = this.addCell(r, "right", e.valign), i.addClass("DlFieldGrid-labelCell"), e.valign == "top" && l instanceof DlLabel && (i.getElement().style.paddingTop = e.vtop || "4px"), l && i.appendWidget(l), t.middleText && (s = this.addCell(r), s.setContent(t.middleText)), o = this.addCell(r), o.appendWidget(f), u = e.id || e.name, u != null && (this.__fields[u] = f.getWidgetId(), delete e.id), t && (a = o.getElement(), t.colSpan && (a.colSpan = t.colSpan), t.rowSpan && (a.rowSpan = t.rowSpan)), n && (n.row = r, n.c1 = i, n.c2 = o, n.label = l, n.entry = f), f
    }, t.getField = function(e) {
        return e ? DlWidget.getById(this.__fields[e]) : this.__fields
    }, t.setField = function(e, t) {
        this.__fields[e] = t.getWidgetId()
    }, t.getValue = function() {
        var e, t, n, r, i = {};
        for (e in this.__fields)
            t = this.getField(e), n = t.getFormValue || t.getValue, n instanceof Function && (t instanceof DlAbstractButton && t._checkTwoState(!0) ? (r = n.call(t), typeof r == "boolean" ? i[e] = r : r == null ? i[e] = t.checked() : t.checked() && (i[e] = r)) : i[e] = n.call(t));
        return i
    }, t.getValues = t.getValue, t.setValue = function(e) {
        var t, n, r, i;
        for (t in e)
            n = this.getField(t), r = e[t], n && (i = n.setFormValue || n.setValue, i instanceof Function && (n instanceof DlAbstractButton && n._checkTwoState(!0) ? n.checked(typeof r == "string" ? r != "0" : !!r) : i.call(n, r)))
    }, t.setValues = t.setValue
}), DEFINE_CLASS("DlFieldset", DlContainer, function(e, t, n) {
    e.DEFAULT_ARGS = {_label: ["label", "DlFieldset"]}, t._createElement = function() {
        e.BASE._createElement.call(this), this.getElement().innerHTML = ["<span class='DlFieldset-label'>", this._label, "</span>", "<div class='DlFieldset-content'></div>"].join("")
    }, t.getContentElement = function() {
        return this.getElement().childNodes[1]
    }, t.getLabelElement = function() {
        return this.getElement().firstChild
    }, t.setOuterSize = t.setSize = function(e) {
        var t = n.getPos(this.getLabelElement()), r = n.getPos(this.getContentElement()), i = r.y - t.y;
        n.setOuterSize(this.getElement(), e.x, e.y - i), e = n.getInnerSize(this.getElement()), n.setOuterSize(this.getContentElement(), e.x, e.y), this.callHooks("onResize")
    }
}), function() {
    var e = {}, t = {};
    window.DlSingleton = {get: function(n, r) {
            return t[n] || !r && (t[n] = new e[n])
        },register: function(t, n, r) {
            e[t] = n, r && (window[t] = this.get.$C(t))
        }}
}(), DEFINE_SINGLETON("DlFlashUtils", DlEventProxy, function(e, t) {
    var n, r, i, s, o;
    e.DEFAULT_EVENTS = ["onLoad", "onStorageStatus"], n = is_ie ? String.template('<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="http://fpdownload.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=8,0,0,0" width="215" height="138" id="DlFlashUtils-MOVIE" align="middle">', '<param name="allowScriptAccess" value="always" />', '<param name="movie" value="$url" />', '<param name="quality" value="high" />', "</object>") : String.template('<embed id="DlFlashUtils-MOVIE" src="$url" quality="high" bgcolor="#ffffff" width="215" height="138" ', 'allowScriptAccess="always" ', 'type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/go/getflashplayer" />'), r = null, window.DlFlashUtils_init = function() {
        DlFlashUtils().callHooks("onLoad")
    }, t.init = function() {
        var e;
        r || (e = n({url: Dynarch.getFileURL("swf/flash.swf")}), document.write("<div style='position: absolute; z-index: 31000; left: -256px; top: 50%; margin-left: -108px; margin-top: -69px; width: 216px; height: 138px;'>" + e + "</div>"), r = document.getElementById("DlFlashUtils-MOVIE"))
    }, t.getObject = function() {
        return r
    }, t.display = function(e) {
        r.parentNode.style.left = e ? "50%" : "-256px"
    };
    function u(e) {
        var t = null;
        try {
            t = e.GetVariable("$version")
        } catch (n) {
        }
        return t
    }
    i = {"ShockwaveFlash.ShockwaveFlash.7": u,"ShockwaveFlash.ShockwaveFlash.6": function(e) {
            var t = "Win 6,0,21";
            try {
                e.AllowScriptAccess = "always", t = u(e)
            } catch (n) {
            }
            return t
        },"ShockwaveFlash.ShockwaveFlash": u}, t.isSupported = function() {
        var e, t, n, r = navigator.plugins;
        if (r && r.length) {
            r = r["Shockwave Flash"];
            if (r && r.description && /^Shockwave Flash\s+([^\s]+)/i.test(r.description))
                return parseFloat(RegExp.$1) >= 8
        } else if (is_ie)
            for (e in i)
                try {
                    t = new ActiveXObject(e);
                    if (t) {
                        n = i[e](t);
                        if (n != null)
                            return n = n.split(/\s+/)[1], parseFloat(n) >= 8
                    }
                } catch (s) {
                }
        return !1
    }, t.loadPolicyFile = function(e) {
        return this.getObject().DlSocket_loadPolicyFile(e)
    }, s = t.decodeString = function(e) {
        return e.replace(/%22/g, '"').replace(/%5c/g, "\\").replace(/%26/g, "&").replace(/%25/g, "%")
    }, o = t.decodeObject = function(e) {
        var t, n;
        if (e instanceof Array)
            for (t = e.length; --t >= 0; )
                e[t] = o(e[t]);
        else if (typeof e == "object") {
            if (e == null)
                return e;
            n = {};
            for (t in e)
                n[s(t)] = o(e[t]);
            e = n
        } else
            typeof e == "string" && (e = s(e));
        return e
    }
}), DlFlashStore = {set: function(e, t) {
        DlFlashUtils().getObject().DlStorage_set(e, t)
    },get: function(e) {
        return DlFlashUtils().decodeObject(DlFlashUtils().getObject().DlStorage_get(e))
    },getAllKeys: function() {
        return DlFlashUtils().decodeObject(DlFlashUtils().getObject().DlStorage_getAllKeys())
    },remove: function(e) {
        DlFlashUtils().getObject().DlStorage_remove(e)
    },clear: function() {
        DlFlashUtils().getObject().DlStorage_clear()
    },flush: function(e) {
        var t = DlFlashUtils().getObject().DlStorage_flush();
        return e && t == "pending" && DlFlashUtils().display(!0), t
    },_onStatus: function(e) {
        DlFlashUtils().display(!1), DlFlashUtils().applyHooks("onStorageStatus", [e])
    }}, DEFINE_CLASS("DlGridLayout", DlLayout, function(e, t, n) {
    var r, i = n.createElement, s = n.addClass;
    e.DEFAULT_ARGS = {__layout: ["layout", null],__layoutHTML: ["layoutHTML", null],__cellSpacing: ["cellSpacing", 0],__cellPadding: ["cellPadding", 1]}, r = ["width", "padding", "paddingLeft", "paddingRight", "paddingTop", "paddingBottom", "verticalAlign", "textAlign", "whiteSpace"], t._createElement = function() {
        var t, n;
        e.BASE._createElement.call(this), this.__layoutHTML ? (this.setContent(this.__layoutHTML), t = this.getElement().getElementsByTagName("table")[0]) : (t = i("table", null, {cellSpacing: this.__cellSpacing,cellPadding: this.__cellPadding,className: "DlGridLayout-table"}), i("tbody", null, null, t), n = this.__layout, n.foreach(function(e) {
            var n = e.props, i = t.insertRow(-1);
            n && n.minHeight && (s(i, "DlGridLayout-tr-minHeight"), i.minHeight = !0), e.cells.foreach(function(e, t) {
                var o = i.insertCell(-1);
                t == 0 && n && n.height && (o.style.height = n.height), e && (e.colSpan && (o.colSpan = e.colSpan), e.className && (o.className = e.className), e.rowSpan && (o.rowSpan = e.rowSpan), e.minWidth && s(o, "DlGridLayout-td-minWidth"), e.minHeight && (o.minHeight = !0, s(o, "DlGridLayout-td-minHeight")), r.r_foreach(function(t) {
                    var n = e[t];
                    n != null && (this[t] = n)
                }, o.style))
            })
        }), this.getElement().appendChild(t)), this.refNode("__table", t)
    }, t._appendWidgetElement = function(t, n) {
        var r;
        n.inCell ? (r = this.getCellElement(n.row, n.col), r.appendChild(t.getElement()), t._dllayout_args = n) : e.BASE._appendWidgetElement.call(this, t, n)
    }, t._removeWidgetElement = function(t) {
        var n;
        this._widgets.contains(t) && (t._dllayout_args.inCell ? (n = t.getElement(), n.parentNode.removeChild(n)) : e.BASE._removeWidgetElement.call(this, t))
    }, t.getTableElement = function() {
        return this.__table
    }, t.getCellElement = function(e, t) {
        return this.getTableElement().rows[e].cells[t]
    }, t.doLayout = function() {
        var e = this.children();
        2..times(function(t) {
            e.foreach(function(e) {
                var n, r, i, s, o, u, a, f = e._dllayout_args;
                f.inCell || (n = this.getCellElement(f.row, f.col), r = n.offsetLeft, i = n.offsetTop, s = n.offsetWidth, o = n.offsetHeight, t == 0 ? (u = e.getOuterSize(), (n.minHeight || n.parentNode.minHeight) && o < u.y && (n.style.height = u.y + "px")) : (a = e.getElement().parentNode.style, a.left = r + "px", a.top = i + "px", e.setOuterSize({x: s,y: o})))
            }, this)
        }, this)
    }, t.showWidgets = function(e) {
        arguments.length == 0 && (e = !0), this.children().r_foreach(function(t) {
            t.display(e)
        })
    }
}), DlHtmlUtils = {_blockTags: "body form textarea fieldset ul ol dl dd dt li div blockquote p h1 h2 h3 h4 h5 h6 quote pre table thead tbody tfoot tr td iframe address".hashWords(),_quickTags: "br hr input link meta img".hashWords(),_headingTags: "h1 h2 h3 h4 h5 h6".hashWords(),_descTags: "p blockquote td div li".hashWords(),isBlockElement: function(e) {
        return e && e.nodeType == 1 && e.tagName.toLowerCase() in DlHtmlUtils._blockTags
    },needsClosingTag: function(e) {
        return e && e.nodeType == 1 && !(e.tagName.toLowerCase() in DlHtmlUtils._quickTags)
    },htmlEncode: function(e) {
        return (e + "").replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\x22/g, "&quot;").replace(/\u00A0/g, "&#xa0;")
    },getHTML: function(e, t, n) {
        function m(e, t) {
            var r, i, g, y, b, w, E;
            switch (e.nodeType) {
                case 11:
                    t = !1;
                case 1:
                    i = e.tagName.toLowerCase();
                    if (e.className == "DynarchLIB-REMOVE-ME")
                        break;
                    if (t) {
                        g = !e.hasChildNodes() && !u(e);
                        if (i == "br") {
                            if (e.previousSibling && !e.nextSibling)
                                break;
                            if (v) {
                                s[o++] = "\n";
                                break
                            }
                        }
                        n && (l == null && i in d ? (l = f(e), h = 0) : c == null && i in p && (c = f(e))), s[o++] = "<", s[o++] = i, y = e.attributes;
                        for (r = 0; r < y.length; ++r) {
                            b = y.item(r);
                            if (!b.specified)
                                continue;
                            w = b.nodeName.toLowerCase();
                            if (/^_moz|^_msh/.test(w))
                                continue;
                            w != "style" ? typeof e[b.nodeName] != "undefined" && w != "href" && w != "src" && !/^on/.test(w) ? E = e[b.nodeName] : E = b.nodeValue : E = e.style.cssText;
                            if (/(_moz|^$)/.test(E))
                                continue;
                            s[o++] = " " + w + '="' + a(E) + '"'
                        }
                        s[o++] = g ? " />" : ">"
                    }
                    i == "pre" && ++v;
                    for (r = e.firstChild; r; r = r.nextSibling)
                        m(r, !0);
                    i == "pre" && --v, t && !g && (s[o++] = "</" + i + ">"), h === 0 && (h = o);
                    break;
                case 3:
                    /^(script|style)$/i.test(e.parentNode.tagName) ? e.data.indexOf("/*<![CDATA[*/") != 0 ? (s[o++] = "/*<![CDATA[*/", s[o++] = e.data, s[o++] = "/*]]>*/") : s[o++] = e.data : s[o++] = e.data.htmlEscape();
                    break;
                case 4:
                case 8:
                    s[o++] = "<!--", s[o++] = e.data, s[o++] = "-->"
            }
        }
        var r, i = DlHtmlUtils, s = [], o = 0, u = i.needsClosingTag, a = i.htmlEncode, f = i.getInnerText, l = null, c = null, h = null, p = i._descTags, d = i._headingTags, v = 0;
        return m(e, t), r = s.join(""), n && (r = {title: l,description: c,content: r,contentButTitle: ""}, h && (r.contentButTitle = s.slice(h).join(""))), r
    },getInnerText: function(e) {
        if (e.innerText != null)
            return e.innerText;
        if (e.textContent != null)
            return e.textContent
    },getText: function(e) {
        var t, n, r, i, s = e.cloneNode(!0), o = s.getElementsByTagName("*");
        for (t = o.length; --t >= 0; )
            n = o[t], DlHtmlUtils.isBlockElement(n) && (r = n.ownerDocument.createTextNode(" "), n.insertBefore(r, n.firstChild), n.appendChild(r.cloneNode(!0)));
        return i = DlHtmlUtils.getInnerText(s), DynarchDomUtils.trash(s), i
    },_can_t_DeleteFull_tags: "td".hashWords(),canDeleteFullNode: function(e) {
        return !(e.toLowerCase() in DlHtmlUtils._can_t_DeleteFull_tags)
    },_can_t_DeleteContent_tags: "ul ol dd table tr img br hr".hashWords(),canDeleteContent: function(e) {
        return !(e.toLowerCase() in DlHtmlUtils._can_t_DeleteContent_tags)
    },_can_t_StripNode_tags: "ul ol li dd dt dl img br hr table tr td object applet iframe form textarea".hashWords(),canStripNode: function(e) {
        return !(e.toLowerCase() in DlHtmlUtils._can_t_StripNode_tags)
    }}, DEFINE_CLASS("DlIconListView", DlContainer), DEFINE_CLASS("DlIconListItem", DlAbstractButton, function(e, t) {
    var n;
    e.DEFAULT_ARGS = {__itemSize: ["itemSize", {x: 100,y: null}],__itemSpacing: ["itemSpacing", 0],__spaceEvenly: ["spaceEvenly", !1],__iconSize: ["iconSize", {x: 40,y: 40}],__iconAbove: ["iconAbove", !0],_btnType: ["type", DlAbstractButton.TYPE.TWOSTATE],_tagName: ["tagName", "table"],_classes: ["classes", {active: "DlIconListItem-active",hover: "DlIconListItem-hover",checked: "DlIconListItem-1",unchecked: "DlIconListItem-0",empty: "DlIconListItem-empty",disabled: "DlIconListItem-disabled"}],_iconClass: ["iconClass", null]}, n = ["DlIconListItem-iconCell", "DlIconListItem-labelCell"], t._createElement = function() {
        var e;
        DlWidget.prototype._createElement.call(this), e = this.getElement(), e.cellSpacing = e.cellPadding = 0, this.__spaceEvenly ? e.style.margin = this.__itemSpacing + "px" : e.style.marginRight = e.style.marginBottom = this.__itemSpacing + "px", e.insertRow(-1).insertCell(-1), e.insertRow(-1).insertCell(-1), e.align = "left", this.setIconAbove(this.__iconAbove, !0), this.setIconClass(this._iconClass), this.label(this._label, !0), this.setIconSize(this.__iconSize), this._updateState()
    }, t.setIconClass = function(e) {
        this.getIconCell().className = n[0] + " " + e
    }, t.getIconCell = function() {
        return this.getElement().rows[this.__iconAbove ? 0 : 1].cells[0]
    }, t.getLabelCell = function() {
        return this.getElement().rows[this.__iconAbove ? 1 : 0].cells[0]
    }, t.setIconSize = function(e) {
        DynarchDomUtils.setInnerSize(this.getIconCell(), e.x, e.y), this.__iconSize = e
    }, t.getIconSize = function() {
        return this.__iconSize
    }, t.setIconAbove = function(e, t) {
        var r = this.getElement().rows;
        t ? (r[0].cells[0].className = n[e ? 0 : 1], r[1].cells[0].className = n[e ? 1 : 0]) : e !== this.__iconAbove && r[1].parentNode.insertBefore(r[1], r[0]), this.__iconAbove = e
    }, t.label = function(e, t) {
        return e != null && (t || e !== this._label) && (this._label = e, this.getLabelCell().innerHTML = String.buffer("<div class='DlIconListItem-labelDiv' style='width:", this.__itemSize.x, "px'>", e, "</div>").get(), this.applyHooks("onUpdateLabel", [this._label])), this._label
    }
}), DlJSON = {RE_strings: /(\x22(\\.|[^\x22\\])*\x22|\x27(\\.|[^\x27\\])*\x27)/g,RE_forbid: /([\n;()+=\x2f*-])/g,encode: function(e) {
        var t, n;
        if (e == null)
            t = "null";
        else if (e.dynarchlib_toJSON)
            t = e.dynarchlib_toJSON();
        else if (e instanceof Array)
            t = "[" + e.map(DlJSON.encode).join(",") + "]";
        else if (e instanceof Date)
            t = DlJSON.encode(e.toUTCString());
        else if (typeof e == "object") {
            t = [];
            for (n in e)
                t.push(DlJSON.encode(n) + ":" + DlJSON.encode(e[n]));
            t = "{" + t.join(",") + "}"
        } else
            typeof e == "string" ? t = '"' + e.replace(/\x5c/g, "\\\\").replace(/\r?\n/g, "\\n").replace(/\t/g, "\\t").replace(/\x22/g, '\\"') + '"' : t = e + "";
        return t
    },encodeIndented: function(e, t) {
        function r(e) {
            return ++n, e = e(), --n, e
        }
        function i(e) {
            return " ".repeat(n * t) + e
        }
        var n;
        return t == null && (t = 2), n = 0, function s(e) {
            var t;
            return e == null ? t = "null" : e.dynarchlib_toJSON ? t = e.dynarchlib_toJSON() : e instanceof Array ? t = "[ " + e.map(s).join(", ") + " ]" : e instanceof Date ? t = s(e.toUTCString()) : typeof e == "object" ? (t = r(function() {
                var t, n = [];
                for (t in e)
                    n.push(s(t) + " : " + s(e[t]));
                return n.map(i).join(",\n") + "\n"
            }), t = "{\n" + t + i("}")) : typeof e == "string" ? t = '"' + e.replace(/\x5c/g, "\\\\").replace(/\r?\n/g, "\\n").replace(/\t/g, "\\t").replace(/\x22/g, '\\"') + '"' : t = e + "", t
        }(e)
    },decode: function(e, t) {
        var n;
        if (!t) {
            e = e.trim(), n = e.replace(DlJSON.RE_strings, "");
            if (DlJSON.RE_forbid.test(n))
                throw new DlSecurityException("Character " + RegExp.$1 + " not allowed in JSON input!")
        }
        try {
            return Dynarch.evalClean(e)
        } catch (r) {
            throw new DlDataException("Malformed data in JSON input: " + r)
        }
    },domToObject: function(e) {
        var t, n, r, i = {}, s = String.buffer();
        for (t = e.firstChild; t; t = t.nextSibling)
            t.nodeType == 1 ? (n = DlJSON.domToObject(t), r = t.nodeName, r in i ? (i[r] instanceof Array || (i[r] = [i[r]]), i[r].push(n)) : i[r] = n) : t.nodeType == 3 && s(t.nodeValue);
        return i.$text = s.get(), i
    }}, DlKeyboard = {BACKSPACE: 8,TAB: 9,ENTER: 13,ESCAPE: 27,SPACE: 32,DASH: 45,PAGE_UP: 33,PAGE_DOWN: 34,END: 35,HOME: 36,ARROW_LEFT: 37,ARROW_UP: 38,ARROW_RIGHT: 39,ARROW_DOWN: 40,INSERT: 45,DELETE: 46,F1: 112,F2: 113,F3: 114,F4: 115,F5: 116,F6: 117,F7: 118,F8: 119,F9: 120,F10: 121,F11: 122,F12: 123,parseKey: function(e) {
        var t = {}, n;
        e = e.toUpperCase();
        if (n = /^([a-z]+)\s+\x27(.)\x27$/i.exec(e))
            t[n[1]] = !0, t.key = n[2];
        else if (n = /^([a-z]+)-([a-z]+)\s+\x27(.)\x27$/i.exec(e))
            t[n[1]] = t[n[2]] = !0, t.key = n[3];
        else if (n = /^([a-z]+)-([a-z]+)-([a-z]+)\s+\x27(.)\x27$/i.exec(e))
            t[n[1]] = t[n[2]] = t[n[3]] = !0, t.key = n[4];
        return t
    },checkKey: function(e, t) {
        return typeof t == "string" && (t = DlKeyboard.parseKey(t)), (!t.CTRL && !e.ctrlKey || t.CTRL && e.ctrlKey) && (!t.ALT && !e.altKey || t.ALT && e.altKey) && (!t.SHIFT && !e.shiftKey || t.SHIFT && e.shiftKey) && e.keyStr.toUpperCase() == t.key.toUpperCase()
    }}, DlKeyboard.KEYS_CONTROL = ["BACKSPACE", "TAB", "DELETE", "ESCAPE", "ENTER", "PAGE_UP", "PAGE_DOWN", "END", "HOME", "ARROW_LEFT", "ARROW_UP", "ARROW_RIGHT", "ARROW_DOWN"].keys_map(DlKeyboard).toHash(!0), DlKeyboard.KEYS_MOVE = ["ARROW_LEFT", "ARROW_UP", "ARROW_RIGHT", "ARROW_DOWN"].keys_map(DlKeyboard).toHash(!0), DlKeyboard.KEYS_MOVE_PREV = ["ARROW_LEFT", "ARROW_UP"].keys_map(DlKeyboard).toHash(!0), DEFINE_CLASS("DlLabel", DlAbstractButton, function(e, t) {
    e.DEFAULT_ARGS = {_activateWidget: ["widget", null],_tagName: ["tagName", "span"]}, t._onMouseDown = function(e) {
        var t = this._activateWidget;
        t && (t.focus(), e.domStop = !0, DlException.stopEventBubbling())
    }, t.setWidget = function(e) {
        this._activateWidget = e
    }, t.getWidget = function() {
        return this._activateWidget
    }, t._handle_accessKey = function(e) {
        this._onMouseDown(e)
    }
}), DEFINE_CLASS("DlLiteTree", DlContainer, function(e, t, n) {
    var r, i;
    e.DEFAULT_EVENTS = "onItemMouseDown onItemDblClick".qw(), e.DEFAULT_ARGS = {items: ["items", null],sort: ["sort", Function.identity],_opt_toggleSelection: ["toggleSelection", !1],_focusable: ["focusable", !0]}, e.FIXARGS = function(e) {
        Object.mergeUndefined(e, {fillParent: !0})
    }, e.CONSTRUCT = function() {
        this.addEventListener({onMouseDown: this._onMouseDown,onDblClick: this._onDblClick})
    }, t.reset = function(e) {
        this.top_items = this.sort(e), this.setContent(this._buildHTML(this.top_items, 0)), this._selection && this._selection.filter(this._itemsById)
    }, t.setSelectionModel = function(e) {
        this._selection ? this._selection.removeEventListener(this._selListeners) : this._selListeners || (this._selListeners = {onChange: this.$("_on_selChange"),onReset: this.$("_on_selReset")}), this._selection = e, e.addEventListener(this._selListeners)
    }, t.isSelected = function(e) {
        return this._selection && this._selection.isSelected(e)
    }, t.refreshItems = function(e) {
        e.foreach(function(e) {
            var t, n, r, i = this._getItemElement(e);
            i && (t = ["item-label"], n = this._itemsById[e], this.isSelected(e) && t.push("selected"), n.addClassNames(t), i.className = t.join(" "), r = String.buffer("<span class='expander'></span>"), n.formatHTML(r), i.innerHTML = r.get())
        }, this)
    }, t.getItemById = function(e) {
        return this._itemsById[e]
    }, t._buildHTML = function(e, t) {
        var n;
        return e.length == 0 ? "" : (t == null && (t = 0), t == 0 && (this._itemsById = {}), n = String.buffer("<ul>"), e.foreach(function(e) {
            var r, i, s = e.children(), o = s.length > 0;
            n("<li>"), r = ["item-label"], i = e.id(), e.addClassNames(r), this.isSelected(i) && r.push("selected"), o && r.push("expanded"), n("<div id='", this._makeId(i), "' lite-tree-item='", i, "' class='", r.join(" "), "'><span class='expander'></span>"), e.formatHTML(n), n("</div>", this._buildHTML(s, t + 1), "</li>"), this._itemsById[e.id()] = e
        }, this), n("</ul>"), n.get())
    }, t._makeId = function(e) {
        return this.id + ":" + e
    }, t._findItemFromEvent = function(e) {
        var t, n = {}, r = e.target;
        while (r && r.nodeType == 1) {
            t = r.getAttribute("lite-tree-item");
            if (t != null)
                return n.el = r, n.id = t, n.item = this._itemsById[t], n;
            r.className == "expander" && (n.expander = r), r = r.parentNode
        }
    }, t.scrollToRecord = function(e) {
        n.scrollIntoView(this._getItemElement(e))
    }, t._getItemElement = function(e) {
        return document.getElementById(this._makeId(e))
    }, t.__handleSelectClick = function(e, t, r) {
        var i, s, o = this._selection, u = this._itemsById[e.id], a = [u, e, t];
        if (r) {
            o && !o.isSelected(e.id) && this.canSelectItem(u) && o.reset([e.id]), this.applyHooks("onItemDblClick", a);
            return
        }
        !o || e.expander || !this.canSelectItem(e.item) ? (i = e.el.nextSibling, i && (s = n.hasClass(i, "hidden"), n.condClass(i, !s, "hidden"), n.condClass(e.el, s, "expanded", "collapsed")), this.applyHooks("onItemMouseDown", a)) : o && this.canSelectItem(e.item) && (o.multiple ? t.ctrlKey ? o.toggle(e.id) : o.reset([e.id]) : this._opt_toggleSelection && o.isSelected(e.id) ? o.toggle(e.id) : o.reset([e.id]), this.applyHooks("onItemMouseDown", a))
    }, t.canSelectItem = function(e) {
        return e.isSelectable()
    }, r = (new Date).getTime(), i = null, t._onMouseDown = function(e) {
        var t = this._findItemFromEvent(e), n = (new Date).getTime();
        n - r < Dynarch.dblClickTimeout && t && i && t.id == i.id ? this.__handleSelectClick(t, e, !0) : t && (r = n, this.__handleSelectClick(t, e, !1)), i = t
    }, t._on_selChange = function(e, t) {
        n.condClass(this._getItemElement(e), t, "selected")
    }, t._on_selReset = function(e, t) {
        Object.foreach(e, function(e, t) {
            n.delClass(this._getItemElement(t), "selected")
        }, this), Object.foreach(t, function(e, t) {
            n.addClass(this._getItemElement(t), "selected")
        }, this)
    }, e.Item = DEFINE_HIDDEN_CLASS(null, DlEventProxy, function(e, t) {
        e.DEFAULT_ARGS = {_name: ["name", null],_id: ["id", null],_children: ["children", null]}, e.CONSTRUCT = function() {
            this._children == null && (this._children = [])
        }, t.formatHTML = function(e) {
            e((this._name + "").htmlEscape())
        }, t.addClassNames = Function.noop, t.id = function() {
            return this._id
        }, t.children = function() {
            return this._children
        }, t.isSelectable = Function.returnTrue
    })
}), DEFINE_CLASS("DlMacBarIcon", DlAbstractButton, function(e, t) {
    function r() {
        this.__anim.destroy()
    }
    function i() {
        this.__anim.sw = this.__currentWidth, this.__anim.sh = this.__currentHeight
    }
    function s() {
        var e, t = this.__anim, n = this.getImgElement(), r = t.getPos(), i = t.getPos(function(e) {
            return 1 - Math.cos(e * 2.5 * Math.PI) / Math.exp(5 * e)
        });
        n.width = this.__currentWidth = r.mapInt(t.sw, t.ew), n.height = this.__currentHeight = i.mapInt(t.sh, t.eh), t = this.__align, e = this.__minWidth - this.__currentWidth, t.center && (e /= 2);
        if (t.left || t.center)
            n.style.marginRight = e + "px";
        if (t.right || t.center)
            n.style.marginLeft = e + "px";
        e = this.__minHeight - this.__currentHeight, t.middle && (e /= 2);
        if (t.top || t.middle)
            n.style.marginBottom = e + "px";
        if (t.bottom || t.middle)
            n.style.marginTop = e + "px"
    }
    var n;
    e.BEFORE_BASE = function() {
        this.__currentWidth = this.__minWidth, this.__currentHeight = this.__minHeight, this.__align = this.__align.split(/\s+/).toHash()
    }, n = {active: "DlMacBarIcon-active",hover: "DlMacBarIcon-hover",checked: "DlMacBarIcon-1",unchecked: "DlMacBarIcon-0",empty: "DlMacBarIcon-empty",disabled: "DlMacBarIcon-disabled"}, e.DEFAULT_ARGS = {_classes: ["classes", n],__image: ["img", null],__minWidth: ["minWidth", 32],__minHeight: ["minHeight", 32],__maxWidth: ["maxWidth", 64],__maxHeight: ["maxHeight", 64],__align: ["align", "bottom"],__tooltipTimeout: ["tooltipTimeout", 900]}, t._createElement = function() {
        DlWidget.prototype._createElement.call(this), this.setContent(String.buffer("<img src='", this.__image, "' width='", this.__minWidth, "' height='", this.__minHeight, " ' />").get())
    }, t.getImgElement = function() {
        return this.getElement().firstChild
    }, t.flash = function(e) {
        this._onMouseEnter(), this._onMouseLeave.delayed(e || 100, this)
    }, t.initDOM = function() {
        e.BASE.initDOM.call(this), this.addEventListener({onDestroy: r}), this.__anim = new DlAnimation(25, 40), this.__anim.addEventListener({onUpdate: s.$(this),onStart: i.$(this)})
    }, t._onMouseEnter = function() {
        var t;
        e.BASE._onMouseEnter.apply(this, arguments), t = this.__anim, t.ew = this.__maxWidth, t.eh = this.__maxHeight, t.start(30, 50, DlAnimation.easing.elastic_b)
    }, t._onMouseLeave = function() {
        var t;
        e.BASE._onMouseLeave.apply(this, arguments), t = this.__anim, t.ew = this.__minWidth, t.eh = this.__minHeight, t.start(50, 50, DlAnimation.easing.accel_b)
    }
}), DEFINE_CLASS("DlNotebook", DlContainer, function(e, t) {
    var n = ["onChange"];
    t._createElement = function() {
        e.BASE._createElement.call(this), this.getElement().innerHTML = "<div class='TabContent-inner'></div>"
    }, t.appendWidget = function(t, n) {
        var r, i;
        t.registerEvents(["onNotebookShow"]), e.BASE.appendWidget.call(this, t), r = t.getElement(), i = this.getContentElement(), n != null ? n = this.__widgetsPosition : this.__widgetsPosition = n, n == null && (n = this.__widgetsPosition = DynarchDomUtils.getPadding(i).x / 2), r.style.position = "absolute", r.style.visibility = "hidden", r.style.left = r.style.top = n + "px", i.appendChild(r), this._panes.push(t)
    }, t.initDOM = function() {
        this._panes = [], this._currentPane = null, this.registerEvents(n), e.BASE.initDOM.call(this)
    }, t.getPane = function(e) {
        return this._panes[e]
    }, t.getAllPanes = function() {
        return this._panes
    }, t.getCurrentPane = function() {
        return this.getPane(this._currentPane)
    }, t.getCurrentPaneIndex = function() {
        return this._currentPane
    }, t.length = function() {
        return this._panes.length
    }, t.showPane = function(e) {
        var t, n = this._currentPane;
        return n != null && (this.getPane(n).visibility(!1), this.getPane(n).setPos({x: -3e4,y: -3e4})), this._currentPane = e, t = this.getPane(e), t._dl_notebook_has_size || (t.setSize(this.getInnerSize()), t._dl_notebook_has_size = !0), t.setStyle({left: "",top: ""}), t.visibility(!0), e !== n && this.applyHooks("onChange", [e, n]), t.callHooks("onNotebookShow"), this
    }, t.firstPane = function() {
        this.showPane(0)
    }, t.lastPane = function() {
        this.showPane(this.length() - 1)
    }, t.nextPane = function() {
        var e = this._currentPane;
        return e == null ? e = 0 : ++e, e >= this._panes.length && (e = 0), this.showPane(e)
    }, t.prevPane = function() {
        var e = this._currentPane;
        return e == null ? e = this._panes.length - 1 : --e, e < 0 && (e = this._panes.length - 1), this
        .showPane(e)
    }, t.isFirstPane = function() {
        return this._currentPane == 0
    }, t.isLastPane = function() {
        return this._currentPane == this._panes.length - 1
    }, t.getContentElement = function() {
        return this.getElement().firstChild
    }, t.setSize = t.setOuterSize = function(t) {
        var n, r;
        e.BASE.setOuterSize.call(this, t), n = this.getElement(), t = DynarchDomUtils.getInnerSize(n), DynarchDomUtils.setOuterSize(this.getContentElement(), t.x, t.y), n.style.width = n.style.height = "", t = DynarchDomUtils.getInnerSize(this.getContentElement()), this._currentPane == null && this.showPane(0), r = this.getCurrentPane(), this._panes.foreach(function(e) {
            e._dl_notebook_has_size = !1
        }), r.setSize(t), r._dl_notebook_has_size = !0
    }, t.setIdealSize = function() {
        var e = {x: 0,y: 0};
        this._panes.r_foreach(function(t) {
            var n = t.getOuterSize();
            n.x > e.x && (e.x = n.x), n.y > e.y && (e.y = n.y)
        }), this.setInnerSize(e)
    }
}), DEFINE_CLASS("DlProgressBar", DlWidget, function(e, t) {
    var n = DynarchDomUtils.createElement;
    e.DEFAULT_ARGS = {__progress_minVal: ["min", 0],__progress_maxVal: ["max", 100],__progress_val: ["val", 0],__label: ["label", null]}, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = this.getElement(), n("div", null, {className: "DlProgressBar-fill"}, t), n("div", null, {className: "DlProgressBar-label",innerHTML: "&nbsp;"}, t), this.setLabel(this.__label), this.setValue(this.__progress_val)
    }, t._getLabelElement = function() {
        return this.getElement().lastChild
    }, t._getFillElement = function() {
        return this.getElement().firstChild
    }, t.getValue = function() {
        return this.__progress_val
    }, t.getMaxVal = function() {
        return this.__progress_maxVal
    }, t.getMinVal = function() {
        return this.__progress_minVal
    }, t.setValue = function(e) {
        var t, n, r;
        this.__progress_val = e, e > this.__progress_maxVal && (e = this.__progress_maxVal), t = this.__progress_maxVal - this.__progress_minVal, n = e - this.__progress_minVal, r = 100 * n / t, !isNaN(r) && r >= 0 && (this._getFillElement().style.width = r + "%", this._updateLabel(r))
    }, t.setLabel = function(e) {
        this.__label = e, this._updateLabel()
    }, t._updateLabel = function(e) {
        var t = this.__label;
        e == null && (e = 0), t != null && (typeof t == "function" ? t = t(this, e, this.__progress_val) : t = t.replace(/%d/g, Math.round(e)).replace(/%f/g, e.toFixed(2)).replace(/%v/g, this.__progress_val), /\S/.test(t) || (t = "&nbsp;"), this._getLabelElement().innerHTML = t)
    }, t.reset = function(e, t, n, r) {
        n == null && (n = e), this.__progress_minVal = e, this.__progress_maxVal = t, arguments.length > 3 && (this.__label = r), this.setValue(n)
    }
}), DEFINE_CLASS("DlRadioButton", DlCheckbox, function(e, t) {
    e.FIXARGS = function(e) {
        e.alwaysCheck = !0
    }, e.DEFAULT_ARGS = {_groupId: ["group", 0],_classes: ["classes", {active: "DlRadioButton-active",hover: "DlRadioButton-hover",checked: "DlRadioButton-1",unchecked: "DlRadioButton-0",empty: "DlRadioButton-empty",disabled: "DlRadioButton-disabled"}]}, t.FINISH_OBJECT_DEF = function() {
        e.BASE.FINISH_OBJECT_DEF.call(this), this._className.remove("DlCheckbox")
    }
}), DEFINE_CLASS("DlRadioSelect", DlButtonMenu, function(e, t) {
    function n(e) {
        this.value(e.userData), DlPopup.clearAllPopups(), e._onMouseLeave()
    }
    e.DEFAULT_ARGS = {_options: ["options", []],_value: ["value", null],_connected: ["connected", !0]}, e.DEFAULT_EVENTS = ["onChange"], e.CONSTRUCT = function() {
        this._radioGroup = DlRadioGroup.get(), this._options.length && this.setOptions(this._options), this.value(this._value, !0), this.addEventListener("onDestroy", function() {
            this._radioGroup.reset()
        })
    }, t.value = function(e, t, n) {
        var r = this._value;
        if (t || typeof e != "undefined" && e !== r)
            this._value = e, this._updateLabel(), n || this.applyHooks("onChange", [r, e]);
        return r
    }, t.getValue = function() {
        return this.value()
    }, t.setValue = t.value, t._updateLabel = function() {
        var e, t, n = null, r = this._options;
        for (e = r.length; --e >= 0; ) {
            t = r[e];
            if (t == null)
                continue;
            this._value == t.value ? (this.getButton().label(t.label), t.widget.checked(!0, !0)) : t.widget.checked(!1, !0)
        }
    }, t.setOptions = function(e) {
        var t, r, i, s, o = this._radioGroup;
        o.reset(), o.addEventListener("onChange", n.$(this)), t = new DlVMenu({className: "DlSelect-menu"}), r = {parent: t,group: o,noCapture: !0}, e.foreach(function(e) {
            var n;
            e == null ? t.addSeparator() : (r.label = e.label, r.data = r.value = e.value, r.className = e.className, n = e.widget = new DlRadioButton(r))
        }, this), i = t.getElement(), i.style.position = "absolute", t.zIndex(-100), document.body.appendChild(i), s = t.getOuterSize().x, document.body.removeChild(i), t.zIndex(""), i.style.position = "", function() {
            this.getButton().setOuterSize({x: s - this.getArrow().getOuterSize().x})
        }.$(this).delayed(10), this.setMenu(t), this._options = e
    }, t.addOption = function(e, t) {
        var n;
        return t == null && (t = this._options.length), n = e.widget = new DlRadioButton({parent: this._menu,group: this._radioGroup,noCapture: !0,label: e.label,data: e.value,value: e.value,className: e.className}), this._options.splice(t, 0, e), n
    }
}), DlRegexp = {EMAIL: /^([a-zA-Z0-9_\.\-])+\@(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,4})+$/,MIME_WEB_IMAGE: /^image\x2f.*(png|jpe?g|gif|tiff?)/i,UNICODE_LETTER: "\\u0041-\\u005A\\u0061-\\u007A\\u00AA\\u00B5\\u00BA\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02C1\\u02C6-\\u02D1\\u02E0-\\u02E4\\u02EC\\u02EE\\u0370-\\u0374\\u0376\\u0377\\u037A-\\u037D\\u0386\\u0388-\\u038A\\u038C\\u038E-\\u03A1\\u03A3-\\u03F5\\u03F7-\\u0481\\u048A-\\u0523\\u0531-\\u0556\\u0559\\u0561-\\u0587\\u05D0-\\u05EA\\u05F0-\\u05F2\\u0621-\\u064A\\u066E\\u066F\\u0671-\\u06D3\\u06D5\\u06E5\\u06E6\\u06EE\\u06EF\\u06FA-\\u06FC\\u06FF\\u0710\\u0712-\\u072F\\u074D-\\u07A5\\u07B1\\u07CA-\\u07EA\\u07F4\\u07F5\\u07FA\\u0904-\\u0939\\u093D\\u0950\\u0958-\\u0961\\u0971\\u0972\\u097B-\\u097F\\u0985-\\u098C\\u098F\\u0990\\u0993-\\u09A8\\u09AA-\\u09B0\\u09B2\\u09B6-\\u09B9\\u09BD\\u09CE\\u09DC\\u09DD\\u09DF-\\u09E1\\u09F0\\u09F1\\u0A05-\\u0A0A\\u0A0F\\u0A10\\u0A13-\\u0A28\\u0A2A-\\u0A30\\u0A32\\u0A33\\u0A35\\u0A36\\u0A38\\u0A39\\u0A59-\\u0A5C\\u0A5E\\u0A72-\\u0A74\\u0A85-\\u0A8D\\u0A8F-\\u0A91\\u0A93-\\u0AA8\\u0AAA-\\u0AB0\\u0AB2\\u0AB3\\u0AB5-\\u0AB9\\u0ABD\\u0AD0\\u0AE0\\u0AE1\\u0B05-\\u0B0C\\u0B0F\\u0B10\\u0B13-\\u0B28\\u0B2A-\\u0B30\\u0B32\\u0B33\\u0B35-\\u0B39\\u0B3D\\u0B5C\\u0B5D\\u0B5F-\\u0B61\\u0B71\\u0B83\\u0B85-\\u0B8A\\u0B8E-\\u0B90\\u0B92-\\u0B95\\u0B99\\u0B9A\\u0B9C\\u0B9E\\u0B9F\\u0BA3\\u0BA4\\u0BA8-\\u0BAA\\u0BAE-\\u0BB9\\u0BD0\\u0C05-\\u0C0C\\u0C0E-\\u0C10\\u0C12-\\u0C28\\u0C2A-\\u0C33\\u0C35-\\u0C39\\u0C3D\\u0C58\\u0C59\\u0C60\\u0C61\\u0C85-\\u0C8C\\u0C8E-\\u0C90\\u0C92-\\u0CA8\\u0CAA-\\u0CB3\\u0CB5-\\u0CB9\\u0CBD\\u0CDE\\u0CE0\\u0CE1\\u0D05-\\u0D0C\\u0D0E-\\u0D10\\u0D12-\\u0D28\\u0D2A-\\u0D39\\u0D3D\\u0D60\\u0D61\\u0D7A-\\u0D7F\\u0D85-\\u0D96\\u0D9A-\\u0DB1\\u0DB3-\\u0DBB\\u0DBD\\u0DC0-\\u0DC6\\u0E01-\\u0E30\\u0E32\\u0E33\\u0E40-\\u0E46\\u0E81\\u0E82\\u0E84\\u0E87\\u0E88\\u0E8A\\u0E8D\\u0E94-\\u0E97\\u0E99-\\u0E9F\\u0EA1-\\u0EA3\\u0EA5\\u0EA7\\u0EAA\\u0EAB\\u0EAD-\\u0EB0\\u0EB2\\u0EB3\\u0EBD\\u0EC0-\\u0EC4\\u0EC6\\u0EDC\\u0EDD\\u0F00\\u0F40-\\u0F47\\u0F49-\\u0F6C\\u0F88-\\u0F8B\\u1000-\\u102A\\u103F\\u1050-\\u1055\\u105A-\\u105D\\u1061\\u1065\\u1066\\u106E-\\u1070\\u1075-\\u1081\\u108E\\u10A0-\\u10C5\\u10D0-\\u10FA\\u10FC\\u1100-\\u1159\\u115F-\\u11A2\\u11A8-\\u11F9\\u1200-\\u1248\\u124A-\\u124D\\u1250-\\u1256\\u1258\\u125A-\\u125D\\u1260-\\u1288\\u128A-\\u128D\\u1290-\\u12B0\\u12B2-\\u12B5\\u12B8-\\u12BE\\u12C0\\u12C2-\\u12C5\\u12C8-\\u12D6\\u12D8-\\u1310\\u1312-\\u1315\\u1318-\\u135A\\u1380-\\u138F\\u13A0-\\u13F4\\u1401-\\u166C\\u166F-\\u1676\\u1681-\\u169A\\u16A0-\\u16EA\\u1700-\\u170C\\u170E-\\u1711\\u1720-\\u1731\\u1740-\\u1751\\u1760-\\u176C\\u176E-\\u1770\\u1780-\\u17B3\\u17D7\\u17DC\\u1820-\\u1877\\u1880-\\u18A8\\u18AA\\u1900-\\u191C\\u1950-\\u196D\\u1970-\\u1974\\u1980-\\u19A9\\u19C1-\\u19C7\\u1A00-\\u1A16\\u1B05-\\u1B33\\u1B45-\\u1B4B\\u1B83-\\u1BA0\\u1BAE\\u1BAF\\u1C00-\\u1C23\\u1C4D-\\u1C4F\\u1C5A-\\u1C7D\\u1D00-\\u1DBF\\u1E00-\\u1F15\\u1F18-\\u1F1D\\u1F20-\\u1F45\\u1F48-\\u1F4D\\u1F50-\\u1F57\\u1F59\\u1F5B\\u1F5D\\u1F5F-\\u1F7D\\u1F80-\\u1FB4\\u1FB6-\\u1FBC\\u1FBE\\u1FC2-\\u1FC4\\u1FC6-\\u1FCC\\u1FD0-\\u1FD3\\u1FD6-\\u1FDB\\u1FE0-\\u1FEC\\u1FF2-\\u1FF4\\u1FF6-\\u1FFC\\u2071\\u207F\\u2090-\\u2094\\u2102\\u2107\\u210A-\\u2113\\u2115\\u2119-\\u211D\\u2124\\u2126\\u2128\\u212A-\\u212D\\u212F-\\u2139\\u213C-\\u213F\\u2145-\\u2149\\u214E\\u2183\\u2184\\u2C00-\\u2C2E\\u2C30-\\u2C5E\\u2C60-\\u2C6F\\u2C71-\\u2C7D\\u2C80-\\u2CE4\\u2D00-\\u2D25\\u2D30-\\u2D65\\u2D6F\\u2D80-\\u2D96\\u2DA0-\\u2DA6\\u2DA8-\\u2DAE\\u2DB0-\\u2DB6\\u2DB8-\\u2DBE\\u2DC0-\\u2DC6\\u2DC8-\\u2DCE\\u2DD0-\\u2DD6\\u2DD8-\\u2DDE\\u2E2F\\u3005\\u3006\\u3031-\\u3035\\u303B\\u303C\\u3041-\\u3096\\u309D-\\u309F\\u30A1-\\u30FA\\u30FC-\\u30FF\\u3105-\\u312D\\u3131-\\u318E\\u31A0-\\u31B7\\u31F0-\\u31FF\\u3400\\u4DB5\\u4E00\\u9FC3\\uA000-\\uA48C\\uA500-\\uA60C\\uA610-\\uA61F\\uA62A\\uA62B\\uA640-\\uA65F\\uA662-\\uA66E\\uA67F-\\uA697\\uA717-\\uA71F\\uA722-\\uA788\\uA78B\\uA78C\\uA7FB-\\uA801\\uA803-\\uA805\\uA807-\\uA80A\\uA80C-\\uA822\\uA840-\\uA873\\uA882-\\uA8B3\\uA90A-\\uA925\\uA930-\\uA946\\uAA00-\\uAA28\\uAA40-\\uAA42\\uAA44-\\uAA4B\\uAC00\\uD7A3\\uF900-\\uFA2D\\uFA30-\\uFA6A\\uFA70-\\uFAD9\\uFB00-\\uFB06\\uFB13-\\uFB17\\uFB1D\\uFB1F-\\uFB28\\uFB2A-\\uFB36\\uFB38-\\uFB3C\\uFB3E\\uFB40\\uFB41\\uFB43\\uFB44\\uFB46-\\uFBB1\\uFBD3-\\uFD3D\\uFD50-\\uFD8F\\uFD92-\\uFDC7\\uFDF0-\\uFDFB\\uFE70-\\uFE74\\uFE76-\\uFEFC\\uFF21-\\uFF3A\\uFF41-\\uFF5A\\uFF66-\\uFFBE\\uFFC2-\\uFFC7\\uFFCA-\\uFFCF\\uFFD2-\\uFFD7\\uFFDA-\\uFFDC"}, DEFINE_SINGLETON("DlSystem", DlEventProxy, function(e) {
    e.DEFAULT_EVENTS = ["on-dialog-create", "on-dialog-show", "on-dialog-hide", "on-dialog-minimize", "on-dialog-restore", "on-rpc-start", "on-rpc-stop", "on-rpc-timeout"]
}), DEFINE_CLASS("DlRPC", DlEventProxy, function(e, t) {
    function n(e) {
        var t;
        if (e.readyState == 4) {
            delete e.onreadystatechange, this._request = null, this._timeoutID && (clearTimeout(this._timeoutID), this._timeoutID = null);
            try {
                t = {success: e.status == 200,status: e.status,statusText: e.statusText,timeout: !1,xml: e.responseXML,text: e.responseText}
            } catch (n) {
            }
            DlSystem().applyHooks("on-rpc-stop", [this, t, e]), this.applyHooks("onStop", [this, t, e]), this.callback && this.callback(t)
        }
    }
    function r(e) {
        this._request = null, e.abort(), DlSystem().applyHooks("on-rpc-timeout", [this, e]), this.applyHooks("onTimeout", [this, e]), this.callback && this.callback({success: !1,timeout: !0})
    }
    e.CONSTRUCT = function() {
        this.method == null && (this.method = this.data != null ? "POST" : "GET"), this._timeoutID = 0
    }, e.DEFAULT_EVENTS = "onStart onStop onTimeout onUploadProgress onUploadDone onUploadError onUploadAbort".qw(), e.DEFAULT_ARGS = {url: ["url", null],args: ["args", null],callback: ["callback", null],method: ["method", null],data: ["data", null],timeout: ["timeout", null]}, t.abort = function() {
        this._request.abort()
    }, t.call = function(e) {
        var t, r, i, s, o, u;
        e != null && Object.merge(this, e), r = !1;
        if (window.XMLHttpRequest)
            t = new XMLHttpRequest;
        else {
            if (!window.ActiveXObject)
                throw "Browser does not support XMLHttpRequest";
            t = new ActiveXObject("Microsoft.XMLHTTP")
        }
        this._request = t, t.onreadystatechange = n.$(this, t), t.upload && (t.upload.addEventListener("progress", this.$("callHooks", "onUploadProgress"), !1), t.upload.addEventListener("load", this.$("callHooks", "onUploadDone"), !1), t.upload.addEventListener("error", this.$("callHooks", "onUploadError"), !1), t.upload.addEventListener("abort", this.$("callHooks", "onUploadAbort"), !1)), s = this.args;
        if (s) {
            r = [];
            for (i in s)
                r.push(escape(i) + "=" + escape(s[i]));
            r.length == 0 ? r = !1 : r = r.join("&")
        }
        o = this.url;
        switch (this.method) {
            case "POST":
                u = this.data, r && u && (o += "?" + r), t.open("POST", o, !0), u ? (typeof u != "string" && (u = DlJSON.encode(u), this.data = u, t.setRequestHeader("Content-Type", "text/javascript; charset=UTF-8")), this._start(u)) : (t.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"), this._start(r));
                break;
            case "GET":
                r && (o += "?" + r), t.open("GET", o, !0), this._start(null)
        }
    }, t._start = function(e) {
        this.timeout ? this._timeoutID = r.delayed(this.timeout, this, this._request) : this._timeoutID = 0, DlSystem().applyHooks("on-rpc-start", [this]), this.applyHooks("onStart", [this]), this._request.send(e)
    }
}), DEFINE_CLASS("DlRteFrame", DlWidget, function(e, t, n) {
    function h(e) {
        var t, r, i;
        e || (e = this.getIframeWin().event), t = new DlEvent(e), t.type == "oncontextmenu" && n.stopEvent(e), t.origTarget = t.target, r = t.origPos = t.pos, i = n.getPos(this.getIframeElement()), t.pos = {x: r.x + i.x,y: r.y + i.y}, t.target = this.getElement();
        try {
            DlEvent._genericEventHandler(t, e)
        } catch (s) {
            s instanceof DlExStopFrameEvent && n.stopEvent(e)
        }
        /onMouseDown|onMouseUp|onKey/.test(t.dl_type) && this.callUpdateHooks(t, e)
    }
    function p(e) {
        var t = this.getIframeDoc();
        this.__hasFrameEvents = !0, n.addEvents(t, f, this.__eventProxy), this.__rte_onFocus = v.$(this), this.__rte_onBlur = m.$(this), is_ie ? (t = this.getIframeElement(), t.onfocus = this.__rte_onFocus) : n.addEvent(t, "focus", this.__rte_onFocus), t.onblur = this.__rte_onBlur, e && e.call(this), this.__pendingHTML && (this.getIframeBody().innerHTML = this.__pendingHTML, this._onSetHTML(), this.moveBOF(), this.__pendingHTML = null)
    }
    function d() {
        var e = this.getIframeDoc();
        n.removeEvents(e, f, this.__eventProxy), is_ie ? (e = this.getIframeElement(), delete e.onfocus, e.onfocus = null) : n.removeEvent(e, "focus", this.__rte_onFocus), delete e.onblur, e.onblur = null
    }
    function v() {
        s(this.getIframeDoc().documentElement, "DlRteFrame-Focused"), e.BASE.focus.call(this)
    }
    function m() {
        o(this.getIframeDoc().documentElement, "DlRteFrame-Focused"), e.BASE.blur.call(this, !0)
    }
    var r, i = n.createElement, s = n.addClass, o = n.delClass, u = n.condClass, a = n.ID, f = ["mouseover", "mouseout", "mousemove", "mousedown", "mouseup", "click", "keydown", "keyup", "keypress", "contextmenu"], l = is_gecko ? "<br type='_moz' />" : "", c = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><html class="DlRteFrame-HTML"><head><title>DynarchLIB Rich Text Editor</title></head><body class="DlRteFrame-Body"><p>' + l + "</p></body></html>";
    e.BEFORE_BASE = function() {
        this.__eventProxy = h.$(this), this.callUpdateHooks = this.callUpdateHooks.clearingTimeout(40, this)
    }, e.CONSTRUCT = function() {
        this.__sections && this.setSections(this.__sections)
    }, e.DEFAULT_EVENTS = ["onUpdate", "onSectionChange"], e.DEFAULT_ARGS = {__paragraphsMode: ["useParagraphs", !0],__sections: ["sections", null],_focusable: ["focusable", 2],_tabChar: ["tabChar", "    "]}, t.COMMANDS = {backcolor: {id: is_ie ? "backcolor" : "hilitecolor"},forecolor: {id: "forecolor"},bold: {id: "bold",key: "CTRL 'B'"},italic: {id: "italic",key: "CTRL 'I'"},underline: {id: "underline",key: "CTRL 'U'"},strike: {id: "strikethrough",key: "CTRL '-'"},subscript: {id: "subscript"},superscript: {id: "superscript"},removeformat: {id: "removeformat",key: "ALT-CTRL '0'"},justifyleft: {id: "justifyleft",key: "ALT-CTRL 'l'"},justifyright: {id: "justifyright",key: "ALT-CTRL 'r'"},justifycenter: {id: "justifycenter",key: "ALT-CTRL 'e'"},justifyfull: {id: "justifyfull",key: "ALT-CTRL 'j'"},orderedlist: {id: "insertorderedlist",key: "ALT-CTRL 'o'"},unorderedlist: {id: "insertunorderedlist",key: "ALT-CTRL-SHIFT 'o'"},unorderedlist1: {id: "insertunorderedlist",key: "ALT-CTRL 'u'"},indent: {id: "indent",key: "CTRL '.'"},outdent: {id: "outdent",key: "CTRL ','"},undo: {id: "undo"},redo: {id: "redo"},"<hr>": {id: "inserthorizontalrule",key: "CTRL ' '"},"<h1>": {id: "formatblock",key: "CTRL '1'",arg: "h1"},"<h2>": {id: "formatblock",key: "CTRL '2'",arg: "h2"},"<h3>": {id: "formatblock",key: "CTRL '3'",arg: "h3"},"<h4>": {id: "formatblock",key: "CTRL '4'",arg: "h4"},"<h5>": {id: "formatblock",key: "CTRL '5'",arg: "h5"},"<h6>": {id: "formatblock",key: "CTRL '6'",arg: "h6"},"<p>": {id: "formatblock",key: "CTRL 'm'",arg: "p"},"<pre>": {id: "formatblock",key: "CTRL-ALT 'n'",arg: "pre"},"<address>": {id: "formatblock",arg: "pre"},"<blockquote>": {id: "formatblock",key: "CTRL 'q'",arg: "blockquote"},_nextSection: {id: "_nextSection",key: "CTRL ']'"},_prevSection: {id: "_prevSection",key: "CTRL '['"},showHtml: {id: "showHtml",key: "CTRL-ALT-SHIFT 'h'"}}, r = "h1 h2 h3 h4 h5 h6 p pre address blockquote".hashWords(), t.SEMANTIC_TAGS = {bold: !0,italic: !0,strike: !0,subscript: !0,superscript: !0,indent: !0,underline: !0,outdent: !0,strikethrough: !0,strike: !0}, t.callUpdateHooks = function(e, t) {
        this.destroyed || this.applyHooks("onUpdate", [e, t])
    }, t.execCommand = function(e, t) {
        var n, r, i, s, o, u, a, f, l, c, h;
        if (this.readonly())
            return;
        this.focus(), r = this.getIframeDoc(), is_gecko && r.execCommand("styleWithCSS", !1, !(e in this.SEMANTIC_TAGS)), this.COMMANDS[e] && (e = this.COMMANDS[e], typeof t == "undefined" && (t = e.arg || ""), e = e.id);
        if (e == "formatblock" && is_ie) {
            i = this.getAncestorsHash();
            if (i.pre) {
                if (t == "pre")
                    return;
                s = this.getSelection(), o = this.getRange(s), u = this.createRange(), u.moveToElementText(i.pre), o.setEndPoint("EndToEnd", u), o.select(), f = String.buffer("<", t, ">dummy</", t, ">").get(), o.compareEndPoints("StartToStart", u) == 0 ? (i.pre.insertAdjacentHTML("beforeBegin", f), a = i.pre.previousSibling) : (l = o.htmlText, c = o.text, o.moveStart("character", -1), o.text.charAt(0) != c.charAt(0) && o.moveStart("character", 1), i.pre.insertAdjacentHTML("afterEnd", "<br />"), o.pasteHTML(""), i.pre.parentNode.removeChild(i.pre.nextSibling), i.pre.insertAdjacentHTML("afterEnd", f), a = i.pre.nextSibling, /\S/.test(c) && (h = i.pre.cloneNode(!0), h.innerHTML = l, a.parentNode.insertBefore(h, a.nextSibling))), o.moveToElementText(a), o.collapse(), o.select(), a.innerHTML = "";
                return
            }
            t = "<" + t + ">"
        }
        switch (e) {
            case "showHtml":
                try {
                    alert(this.getHTML())
                } catch (p) {
                    alert("ERROR: " + p)
                }
                break;
            case "_nextSection":
                this.nextSection();
                break;
            case "_prevSection":
                this.prevSection();
                break;
            default:
                n = r.execCommand(e, !1, t)
        }
        return this.focus(), this.callUpdateHooks(), n
    }, t.queryCommandState = function(e) {
        if (!this.readonly()) {
            this.COMMANDS[e] && (e = this.COMMANDS[e].id);
            try {
                return this.getIframeDoc().queryCommandState(e)
            } catch (t) {
            }
        }
    }, t.queryCommandValue = function(e) {
        var t, n, i;
        if (!this.readonly()) {
            this.COMMANDS[e] && (e = this.COMMANDS[e].id);
            if (!is_gecko && /^formatblock$/i.test(e)) {
                t = this.getAllAncestors();
                for (n = 0; n < t.length; ++n) {
                    i = t[n].tagName.toLowerCase();
                    if (i in r)
                        return i
                }
            }
            try {
                return this.getIframeDoc().queryCommandValue(e)
            } catch (s) {
            }
        }
    }, t.getInnerHTML = function() {
        return this.getIframeBody().innerHTML
    }, t.getHTML = function(e, t) {
        var n, r;
        return !is_ie && !t && (this.collapse(!0), n = this.getIframeDoc().createElement("span"), n.id = "DYNARCHLIB_RTEFRAME_CARET", this.insertNode(n)), r = DlHtmlUtils.getHTML(this.getIframeBody(), !1, e), !is_ie && !t && this.deleteNode(n), r
    }, t.getInnerText = function() {
        return DlHtmlUtils.getInnerText(this.getIframeBody())
    }, t.getText = function() {
        return DlHtmlUtils.getText(this.getIframeBody())
    }, t.setSections = function(e) {
        var t;
        this.__sections = e, t = this.__sectionsHash = {}, e.foreach(function(e, n) {
            e.index = n, t[e.name] = e;
            if (e.current || n == 0)
                this.__currentSection = n, this.setHTML(e.content)
        }.$(this))
    }, t.setSectionContent = function(e, t) {
        var n = this.getSection(e, !0);
        n.content = t, n.index == this.__currentSection && this.setHTML(t)
    }, t.getSections = function() {
        var e = this.getHTML(!0);
        return Object.merge(this.getCurrentSection(), e), this.__sectionsHash
    }, t.getSection = function(e, t) {
        var n, r = this.__sectionsHash[e];
        return r.index == this.__currentSection && !t && (n = this.getHTML(!0), Object.merge(r, n)), r
    }, t.getCurrentSection = function() {
        return this.__sections[this.__currentSection]
    }, t._setCurrentSection = function(e) {
        var t, n = this.getCurrentSection();
        n !== e && (t = this.getHTML(!0), Object.merge(n, t), this.__currentSection = e.index, this.setHTML(e.content), this.applyHooks("onSectionChange", [n, e]))
    }, t.setCurrentSection = function(e) {
        var t = this.__sectionsHash[e];
        this._setCurrentSection(t)
    }, t.setCurrentSectionIndex = function(e) {
        var t;
        e != this.__currentSection && (t = this.__sections[e], this._setCurrentSection(t))
    }, t.prevSection = function() {
        this.setCurrentSectionIndex(this.__sections.rotateIndex(this.__currentSection - 1))
    }, t.nextSection = function() {
        this.setCurrentSectionIndex(this.__sections.rotateIndex(this.__currentSection + 1))
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener(is_ie ? "onKeyDown" : "onKeyPress", function(e) {
            this._onKeypress(e)
        }), this.addEventListener({onDestroy: d})
    }, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = i("iframe", {display: "block"}, {frameBorder: 0,marginHeight: 0,marginWidth: 0,src: is_ie ? "javascript:'';" : "about:blank"}, this.getElement()), this.__hasFrameEvents = !1, this.updateKeymap()
    }, t.updateKeymap = function() {
        var e, t, n = this.__rte_keymap = [];
        for (e in this.COMMANDS)
            t = this.COMMANDS[e], t.key && n.push([DlKeyboard.parseKey(t.key), e])
    }, t.setOuterSize = t.setSize = function(e) {
        var t = n.getBorder(this.getElement()), r = n.getBorder(this.getContentElement());
        this.setInnerSize({x: e.x - t.x - r.x,y: e.y - t.y - r.y})
    }, t.getIframeElement = function() {
        return this.getElement().firstChild
    }, t.getContentElement = t.getIframeElement, t.getIframeWin = function() {
        return this.getIframeElement().contentWindow
    }, t.getIframeDoc = function() {
        return this.getIframeWin().document
    }, t.getIframeBody = function() {
        return this.getIframeDoc().body
    }, t.initDesignMode = function(e) {
        var t = this.getIframeDoc();
        t.open(), t.write(c), t.close(), t.designMode = "on", this.__hasFrameEvents || p.delayed(5, this, e)
    }, t.readonly = function(e) {
        var t = this.getIframeDoc();
        return arguments.length > 0 && (t.designMode = e ? "off" : "on", u(t.documentElement, e, "DlRteFrame-ReadOnly")), t.designMode == "off"
    }, t.setHTML = function(e) {
        e instanceof Array && (e = e.join("")), e = e.trim();
        if (this.__hasFrameEvents) {
            is_ie && (e = e.replace(/(<pre[^>]*>)((.|\n)+?)(<\x2fpre>)/gi, function(e, t, n, r, i) {
                return n = n.replace(/\x20/g, " ").replace(/\t/g, " ".repeat(4)), t + n + i
            })), this.getIframeBody().innerHTML = e, this._onSetHTML();
            try {
                this.moveBOF()
            } catch (t) {
            }
            this.callUpdateHooks()
        } else
            this.__pendingHTML = e
    }, t._onSetHTML = function() {
        var e, t, n, r = this.getIframeDoc().getElementsByTagName("pre");
        for (e = r.length; --e >= 0; )
            t = r[e], t.innerHTML = t.innerHTML.replace(/\n/g, "<br>");
        is_ie || (n = this.getIframeDoc().getElementById("DYNARCHLIB_RTEFRAME_CARET"), n && function(e) {
            try {
                this.selectNodeContents(e), this.collapse(!0), this.deleteNode(e)
            } catch (t) {
            }
        }.delayed(10, this, n))
    }, t.clear = function() {
        this.setHTML("")
    }, t.focus = function() {
        this.getIframeWin().focus(), e.BASE.focus.call(this)
    }, t.loadStyle = function(e) {
        var t, n, r = this.getIframeDoc(), i = e.replace(/\x2f/g, "_");
        r.getElementById(i) || (t = r.getElementsByTagName("head")[0], n = r.createElement("link"), n.type = "text/css", n.rel = "stylesheet", n.href = e, n.id = i, t.appendChild(n), n.disabled = !0, n.disabled = !1)
    }, t.createRange = function() {
        return is_ie ? this.getIframeBody().createTextRange() : this.getIframeDoc().createRange()
    }, t.getSelection = function() {
        return is_ie ? this.getIframeDoc().selection : this.getIframeWin().getSelection()
    }, t.getRange = function(e) {
        return e == null && (e = this.getSelection()), is_ie ? e.createRange() : e.getRangeAt(0)
    }, t.getParentElement = function() {
        var e, t = this.getSelection(), n = this.getRange(t);
        if (is_ie)
            switch (t.type) {
                case "Text":
                case "None":
                    return n.parentElement();
                case "Control":
                    return n.item(0);
                default:
                    return null
            }
        else
            try {
                e = n.commonAncestorContainer, !n.collapsed && n.startContainer == n.endContainer && n.startOffset - n.endOffset <= 1 && n.startContainer.hasChildNodes() && (e = n.startContainer.childNodes[n.startOffset]);
                while (e.nodeType == 3)
                    e = e.parentNode;
                return e
            } catch (r) {
                return null
            }
    }, t.getAllAncestors = function() {
        var e, t, n = this.getParentElement();
        n && n.nodeType == 1 && (n = this.getParentElement()), e = this.getIframeBody(), t = [];
        while (n && n !== e && n.nodeType == 1)
            t.push(n), n = n.parentNode;
        return t.push(e), t
    }, t.getAncestorsHash = function() {
        var e, t = this.getAllAncestors(), n = {__all: t};
        return t.foreach(function(t) {
            e = t.tagName.toLowerCase(), n[e] || (n[e] = t)
        }), n
    }, t.getSelectedText = function() {
        var e = this.getRange();
        return is_ie ? e.text : e + ""
    }, t.selectRange = function(e) {
        var t;
        is_ie ? e.select() : (t = this.getSelection(), t.removeAllRanges(), t.addRange(e))
    }, t.isCollapsed = function() {
        var e = this.getRange();
        return is_w3 ? e.collapsed : e.compareEndPoints("StartToEnd", e) == 0
    }, t.collapse = function(e) {
        var t = this.getSelection(), n = this.getRange(t);
        is_w3 && t.removeAllRanges(), n.collapse(!!e), this.selectRange(n)
    }, t.insertNode = function(e, t) {
        var n, r, i = this.getSelection(), s = this.getRange(i);
        return is_w3 ? (s.deleteContents(), s.insertNode(e), t && (i.removeAllRanges(), s.selectNode(e), i.addRange(s))) : (n = a("rteframe"), s.pasteHTML(["<span id='", n, "'></span>"].join("")), r = this.getIframeDoc().getElementById(n), r.parentNode.insertBefore(e, r), r.parentNode.removeChild(r), t && e.nodeType != 3 && (s.moveToElementText(e), s.select())), e
    }, t.insertText = function(e, t) {
        var n = this.getIframeDoc().createTextNode(e);
        return this.insertNode(n, t)
    }, t.selectNodeContents = function(e) {
        var t = this.createRange();
        is_w3 ? t.selectNodeContents(e) : t.moveToElementText(e), this.selectRange(t)
    }, t.selectNode = function(e) {
        var t;
        is_w3 ? (t = this.createRange(), t.selectNode(e), this.selectRange(t)) : this.selectNodeContents(e)
    }, t.createLink = function(e, t) {
        var n, r, i, s;
        t && this.insertText(t, !0), n = "javascript:" + a("link"), this.execCommand("createlink", n), r = this.getIframeDoc().getElementsByTagName("a");
        for (s = r.length; --s >= 0; )
            if (r[s].href == n) {
                i = r[s];
                break
            }
        return i.href = e, i
    }, t.createAnchor = function(e) {
        var t = this.getSelectedText() ? null : "Anchor", n = this.createLink("#", t);
        return n.removeAttribute("href"), s(n, t ? "DlAnchor-Empty" : "DlAnchor"), n.setAttribute("name", e), t && (n.innerHTML = ""), n
    }, t.getAnchors = function() {
        var e, t = this.getIframeDoc().getElementsByTagName("a"), n = t.length, r = [];
        while (--n >= 0)
            e = t[n], e.name && r.unshift(e);
        return r
    }, t.unlink = function() {
        this.execCommand("unlink")
    }, t.getSelectedImage = function() {
        var e = this.getParentElement();
        return e && !/^img$/i.test(e.tagName) && (e = null), e
    }, t.insertImage = function(e) {
        var t, n, r, i = this.getSelectedImage();
        if (!i) {
            t = "javascript:" + a("img"), this.execCommand("insertimage", t), n = this.getIframeDoc().getElementsByTagName("img");
            for (r = n.length; --r >= 0; )
                if (n[r].src == t) {
                    i = n[r];
                    break
                }
        }
        return i.src = e.url, e.width && (i.width = e.width), e.height && (i.height = e.height), e.align && (i.align = e.align), e.alt && (i.alt = e.alt), e.marginLeft && (i.style.marginLeft = e.marginLeft), e.marginTop && (i.style.marginTop = e.marginTop), e.marginRight && (i.style.marginRight = e.marginRight), e.marginBottom && (i.style.marginBottom = e.marginBottom), i
    }, t.moveBOF = function(e) {
        var t, n, r = this.getIframeBody(), i = e ? r.lastChild : r.firstChild;
        if (!i)
            return;
        is_w3 ? (n = this.getRange(t = this.getSelection()), t.removeAllRanges(), i.nodeType == 1 ? n.selectNodeContents(i) : n.selectNode(i), n.collapse(!e), t.addRange(n)) : (n = r.createTextRange(), n.collapse(!e), n.select())
    }, t.moveEOF = function(e) {
        return this.moveBOF(!e)
    }, t.setParagraphsMode = function(e) {
        this.__paragraphsMode = e
    }, t.addBlockClass = function(e, t, r) {
        var i = this.getAncestorsHash()[e];
        if (i) {
            if (n.hasClass(i, t)) {
                if (r)
                    return n.delClass(i, t), !1
            } else
                n.addClass(i, t);
            return !0
        }
        return null
    }, t.canDeleteFullNode = function(e) {
        return DlHtmlUtils.canDeleteFullNode(e.tagName)
    }, t.canDeleteContent = function(e) {
        return DlHtmlUtils.canDeleteContent(e.tagName)
    }, t.canStripNode = function(e) {
        return DlHtmlUtils.canStripNode(e.tagName)
    }, t.deleteNodeContents = function(e) {
        e.innerHTML = DlHtmlUtils.isBlockElement(e) ? l : ""
    }, t.deleteNode = function(e) {
        e.parentNode.removeChild(e)
    }, t.stripNode = function(e) {
        var t = e.ownerDocument.createDocumentFragment();
        while (e.firstChild)
            t.appendChild(e.firstChild);
        e.parentNode.insertBefore(t, e), e.parentNode.removeChild(e), this.callUpdateHooks()
    }, t._onKeypress = function(e) {
        var t, n, r, i = e.keyCode;
        this.__rte_keymap.r_foreach(function(t) {
            if (DlKeyboard.checkKey(e, t[0]))
                throw this.execCommand(t[1]), new DlExStopFrameEvent
        }, this);
        if (i == DlKeyboard.TAB) {
            t = this.queryCommandValue("formatblock");
            if (t == "pre")
                throw this.insertText(this._tabChar, !0), this.collapse(!1), new DlExStopFrameEvent
        } else if (i == DlKeyboard.ENTER && is_ie && !e.shiftKey) {
            t = this.queryCommandValue("formatblock");
            if (t == "pre")
                throw n = this.getSelection(), r = this.getRange(n), r.pasteHTML("<br/><div class='DynarchLIB-REMOVE-ME'></div>"), new DlExStopFrameEvent
        }
    }
}), DEFINE_CLASS("DlSocket", DlEventProxy, function(e, t) {
    function r() {
        return DlFlashUtils().getObject()
    }
    function i() {
        r().DlSocket_destroy(this.id), delete n[this.id]
    }
    var n;
    e.DEFAULT_EVENTS = ["onConnect", "onRelease", "onData"], e.DEFAULT_ARGS = {_host: ["host", null],_port: ["port", null],_json: ["json", !1]}, e.FIXARGS = function(e) {
        e.host || (e.host = document.domain)
    }, e.CONSTRUCT = function() {
        this.addEventListener("onDestroy", i), DlEvent.atUnload(this.destroy.$(this))
    }, n = {}, t.send = function(e) {
        r().DlSocket_send(this.id, e)
    }, t.sendJSON = function(e) {
        r().DlSocket_send(this.id, DlJSON.encode(e))
    }, t.connect = function() {
        this.id = r().DlSocket_connect(this._host, this._port), n[this.id] = this
    }, t.reconnect = function() {
        r().DlSocket_reconnect(this.id)
    }, t.disconnect = function() {
        r().DlSocket_disconnect(this.id)
    }, window.DlSocket_onConnect = function(e, t) {
        n[e].applyHooks("onConnect", [t])
    }, window.DlSocket_onData = function(e, t) {
        var r;
        t = DlFlashUtils.decodeString(t), r = n[e], r._json && (t = DlJSON.decode(t)), r.applyHooks("onData", [t])
    }, window.DlSocket_onDisconnect = function(e, t) {
        n[e].applyHooks("onRelease", [t])
    }
}), DEFINE_CLASS("DlSound", DlEventProxy, function(e, t) {
    function r() {
        return DlFlashUtils().getObject()
    }
    function i() {
        delete n[this.id]
    }
    function s(e) {
        this.__fileLoaded = e, e && this.__shouldPlay && (this.play.apply(this, this.__shouldPlay), this.__shouldPlay = null)
    }
    var n = {};
    e.DEFAULT_EVENTS = ["onLoad", "onComplete"], e.DEFAULT_ARGS = {_volume: ["volume", null],_pan: ["pan", null],_url: ["url", null],_stream: ["stream", !1]}, e.CONSTRUCT = function() {
        this.addEventListener({onDestroy: i,onLoad: s}), this.id = r().DlSound_create(), this._volume != null && this.setVolume(this._volume), this._pan != null && this.setPan(this._pan), n[this.id] = this
    }, t.load = function(e, t) {
        e == null && (e = this._url), t == null && (t = this._stream), this.__fileLoaded = !1, this.__loadCalled = !0, r().DlSound_load(this.id, this._url = e, this._stream = t)
    }, t.play = function(e, t) {
        this.__fileLoaded ? r().DlSound_play(this.id, e, t) : this.__loadCalled || (this.__shouldPlay = [e, t], this.load())
    }, t.stop = function() {
        r().DlSound_stop(this.id)
    }, t.getBytesLoaded = function() {
        return r().DlSound_getBytesLoaded(this.id)
    }, t.getBytesTotal = function() {
        return r().DlSound_getBytesTotal(this.id)
    }, t.getDuration = function() {
        return r().DlSound_getDuration(this.id)
    }, t.getPosition = function() {
        return r().DlSound_getPosition(this.id)
    }, t.setPan = function(e) {
        r().DlSound_setPan(this.id, this._pan = e)
    }, t.setVolume = function(e) {
        r().DlSound_setVolume(this.id, this._volume = e)
    }, t.getPan = function() {
        return this._pan
    }, t.getVolume = function() {
        return this._volume
    }, t.getURL = function() {
        return this._url
    }, window.DlSound_onLoad = function(e, t) {
        n[e].applyHooks("onLoad", [t])
    }, window.DlSound_onSoundComplete = function(e) {
        n[e].callHooks("onComplete")
    }
}), DEFINE_CLASS("DlSpinner", DlEntry, function(e, t, n) {
    function i() {
        this.select()
    }
    function s() {
        this._clearTimer()
    }
    function o(e) {
        throw this._spinnerUpdateVal(e.wheelDelta > 0), new DlExStopEventBubbling
    }
    function u(e) {
        switch (e.keyCode) {
            case DlKeyboard.ARROW_DOWN:
                throw l.call(this, {_direction: !1}), new DlExStopEventBubbling;
            case DlKeyboard.ARROW_UP:
                throw l.call(this, {_direction: !0}), new DlExStopEventBubbling
        }
    }
    function a() {
        this._clearTimer()
    }
    function f() {
        var e = this.getValue(), t = e == this._maxVal, n = e == this._minVal;
        this._buttonUp.disabled(t || !!this.readonly()), this._buttonDown.disabled(n || !!this.readonly()), (t || n) && this._clearTimer()
    }
    function l(e) {
        throw this._spinnerUpdateVal(e._direction), (this._timerState = Array.$(this.intervals)).r_foreach(function(e) {
            e.step *= this.step
        }, this), this._timerPos = 0, this._startTimer(e._direction, 250), new DlExStopEventBubbling
    }
    function c() {
        this._clearTimer()
    }
    var r = n.createElement;
    e.FIXARGS = function(e) {
        e.validators = [new DlValidator(DlValidator.Number, e.minVal, e.maxVal, e.integer, e.decimals)], e.type = "text"
    }, e.CONSTRUCT = function() {
        this._timer = null, this._timerStep = null, this._timerState = null, this._timerPos = null
    }, e.DEFAULT_EVENTS = ["onSpin"], e.DEFAULT_ARGS = {_step: ["step", 1],_size: ["size", 4],_value: ["value", 0],_minVal: ["minVal", null],_maxVal: ["maxVal", null],_decimals: ["decimals", null],_integer: ["integer", !1]}, t.intervals = [{pos: 1,step: 1,speed: 125}, {pos: 10,step: 1,speed: 70}, {pos: 20,step: 1,speed: 35}, {pos: 50,step: 1,speed: 20}, {pos: 100,step: 1,speed: 10}, {pos: 200,step: 2,speed: 10}], t._createElement = function() {
        var t, n, i, s, o;
        this._no_gecko_bug = !0, e.BASE._createElement.call(this), t = this.getElement(), n = t.rows[0].cells[0], n.rowSpan = 2, n = n.parentNode, i = r("tr", null, null, n.parentNode), s = r("td", null, {className: "DlSpinner-Button DlSpinner-Button-Up"}, n), o = r("td", null, {className: "DlSpinner-Button DlSpinner-Button-Down"}, i), this._buttonUp = new DlButton({parent: this,appendArgs: s}), this._buttonDown = new DlButton({parent: this,appendArgs: o}), this._btn = this._buttonUp
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onFocus: i,onBlur: s,onMouseWheel: o,onKeyDown: u,onKeyUp: a,onChange: f})
    }, t.initDOM = function() {
        e.BASE.initDOM.call(this), this._setupSpinnerBtn(this._buttonUp, !0), this._setupSpinnerBtn(this._buttonDown, !1)
    }, t.readonly = function(t) {
        return t != null && (this._buttonUp.disabled(t), this._buttonDown.disabled(t)), e.BASE.readonly.apply(this, arguments)
    }, t.getFormValue = function() {
        var e = this.getValue(), t = parseFloat(e);
        return isNaN(t) ? e : t
    }, t._spinnerUpdateVal = function(e) {
        var t, n, r, i;
        this._readonly || (t = new Number(this.getValue()), n = this._timerStep || this._step, t = e ? t + n : t - n, this._minVal != null && t < this._minVal && (t = this._minVal), this._maxVal != null && t > this._maxVal && (t = this._maxVal), this.setValue(t), this.applyHooks("onSpin", [t]), this.focus(), this.select(), this._timer && (r = ++this._timerPos, this._timerState.length && r == this._timerState[0].pos && (i = this._timerState.shift(), this._clearTimer(!0), this._timerStep = i.step, this._startTimer(e, i.speed))))
    }, t._clearTimer = function(e) {
        this._timer && clearInterval
        (this._timer), e || (this._timerState = null, this._timerStep = null, this._timerPos = null), this._timer = null
    }, t._startTimer = function(e, t) {
        this._timer && clearInterval(this._timer), this._timer = setInterval(this._spinnerUpdateVal.$(this, e), t)
    }, t._setupSpinnerBtn = function(e, t) {
        var n;
        e._direction = t, n = c.$(this, e), e.addEventListener({onMouseDown: l.$(this, e),onMouseUp: n})
    }, t.setMinVal = function(e, t) {
        var n;
        this._minVal = e, n = this.getFormValue(), n != null && !isNaN(n) && n < e && this.setValue(e, t)
    }, t.setMaxVal = function(e, t) {
        var n;
        this._maxVal = e, n = this.getFormValue(), n != null && !isNaN(n) && n > e && this.setValue(e, t)
    }
}), DEFINE_CLASS("DlStyleSheet", DlEventProxy, function(e, t, n) {
    function r() {
        n.trash(this._el), this._s = null, this._el = null
    }
    e.CONSTRUCT = function() {
        this._init()
    }, t.insertRule = function(e, t, r) {
        var i, s, o, u, a = this._s;
        r == null && (r = this.getRules().length);
        if (typeof t == "object") {
            i = [];
            for (s in t)
                i.push(s + ":" + t[s]);
            t = i.join(";")
        } else
            t instanceof Array && (t = t.join(";"));
        if (is_ie) {
            e = e.split(/\s*,\s*/);
            if (e.length != 1) {
                o = n.ID(), u = this._ier[o] = [];
                for (s = 0; s < e.length; ++s)
                    a.addRule(e[s], t, r + s), u.push(this.getRules()[r + s]);
                return o
            }
            a.addRule(e, t, r)
        } else
            a.insertRule(e + "{" + t + "}", r);
        return this.getRules()[r]
    }, t.deleteRule = function(e) {
        var t, n;
        if (is_ie && typeof e == "string")
            this._ier[e].foreach(this.deleteRule.$(this)), delete this._ier[e];
        else {
            t = this.getRules();
            for (n = t.length; --n >= 0; )
                if (t[n] === e)
                    return is_ie ? this._s.removeRule(n) : this._s.deleteRule(n), n
        }
    }, t.modifyRule = function(e, t) {
        var n;
        if (is_ie && typeof e == "string")
            this._ier[e].foreach(function(e) {
                this.modifyRule(e, t)
            }, this);
        else
            for (n in t)
                e.style[n] = t[n]
    }, t.refresh = function() {
        var e = this.disabled();
        this.disabled(!e), this.disabled(e)
    }, t.getRules = function() {
        return is_ie ? this._s.rules : this._s.cssRules
    }, t.disabled = function(e) {
        var t = is_ie ? this._s : this._el;
        return e != null && (t.disabled = e), !!t.disabled
    }, t._init = function() {
        is_ie && (this._ier = {}), this._el = n.createElement("style", null, {type: "text/css"}, document.getElementsByTagName("head")[0]), this._s = document.styleSheets[document.styleSheets.length - 1], this.addEventListener("onDestroy", r)
    }
}), DEFINE_CLASS("DlTabs", DlContainer, function(e, t) {
    function n(e, t, n) {
        var r = t != null ? this._panes[t] : null;
        r && r._tab.checked(!0), e.applyHooks("onChange", [t, n])
    }
    function r(e) {
        e.checked() && this._tabContent.showPane(e.userData)
    }
    function i() {
        throw this.checked(!0), new DlExStopEventProcessing
    }
    e.DEFAULT_EVENTS = ["onChange"], e.DEFAULT_ARGS = {_tabPos: ["tabPos", "top"]}, t.addTab = function(e, t, n) {
        return this._tabContent.appendWidget(e, n), e._tab = new DlButton({label: t,parent: this._tabBar,group: this._tabGroup,type: DlButton.TYPE.TWOSTATE,data: this._tabContent.length() - 1}), e._tab.addEventListener("onClick", i, !0), e.addEventListener("onDestroy", e._tab.destroy.$(e._tab)), e
    }, t.addTab2 = function(e) {
        var t = this.addTab(e.widget, e.title, e.pos);
        return e.iconClass && t._tab.setIconClass(e.iconClass), t
    }, t.getTabBar = function() {
        return this._tabBar
    }, t.getNotebook = function() {
        return this._tabContent
    }, t.getTabButton = function(e) {
        return this.getNotebook().getPane(e)._tab
    }, t.getTabContent = t.getNotebook, t.initDOM = function() {
        e.BASE.initDOM.call(this), this._tabGroup = DlRadioGroup.get(this.id), this._tabBar = new DlHbox({className: "TabBar"}), this._tabContent = new DlNotebook({className: "TabContent"});
        switch (this._tabPos) {
            case "top":
            case "left":
                this.appendWidget(this._tabBar), this.appendWidget(this._tabContent);
                break;
            case "bottom":
            case "right":
                this.appendWidget(this._tabContent), this.appendWidget(this._tabBar)
        }
        this._tabContent.addEventListener("onChange", n.$(this._tabContent, this)), this._tabGroup.addEventListener("onChange", r.$(this)), this.addClass("DlTabs-" + this._tabPos)
    }, t.setTabPos = function(e) {
        var t, n = this._tabBar.getElement(), r = this._tabContent.getElement();
        n.parentNode && n.parentNode.removeChild(n), t = e == "top" || e == "left" ? t = r : null, r.parentNode.insertBefore(n, t), this.addClass("DlTabs-" + e, "DlTabs-" + this._tabPos), this._tabPos = e
    }, t.setTabAlign = function(e) {
        return this._tabBar.setAlign(e)
    }, t.setOuterSize = t.setSize = function(t) {
        var n;
        e.BASE.setSize.call(this, t), t = this.getInnerSize(), n = this._tabBar.getSize();
        switch (this._tabPos) {
            case "top":
            case "bottom":
                t.y -= n.y;
                break;
            case "left":
            case "right":
                t.x -= n.x
        }
        this._tabContent.setSize(t)
    }, t.showPane = function(e) {
        return this._tabContent.showPane(e)
    }, t.nextPane = function() {
        return this._tabContent.nextPane()
    }, t.prevPane = function() {
        return this._tabContent.prevPane()
    }, t.isFirstPane = function() {
        return this._tabContent.isFirstPane()
    }, t.isLastPane = function() {
        return this._tabContent.isLastPane()
    }, t.getCurrentPane = function() {
        return this._tabContent.getCurrentPane()
    }, t.getCurrentPaneIndex = function() {
        return this._tabContent.getCurrentPaneIndex()
    }, t._handle_focusKeys = function(e) {
        var t;
        e.shiftKey ? e.keyCode == DlKeyboard.PAGE_UP ? (this.prevPane(), this.getCurrentPane()._tab.focus(), DlException.stopEventBubbling()) : e.keyCode == DlKeyboard.PAGE_DOWN && (this.nextPane(), this.getCurrentPane()._tab.focus(), DlException.stopEventBubbling()) : e.keyCode == DlKeyboard.TAB && this._tabBar.focusInside() && (t = this.getCurrentPane().getFirstFocusWidget(), t && (t.focus(), DlException.stopEventBubbling()))
    }
}), DlTextUtils = function() {
    function u(t) {
        function d() {
            return this.scrollLeft = o.x, this.scrollTop = o.y, n.stopEvent(t)
        }
        var i, o, u, a, f, l, c, h, p;
        t || (t = window.event), i = n.getSelectionRange(this), o = {x: this.scrollLeft,y: this.scrollTop};
        if (t.altKey && t.charCode == 113)
            return u = e.fillText(this.value, 72, i.start), this.value = u.text, n.setSelectionRange(this, u.pos, u.pos), d.call(this);
        if (t.ctrlKey && t.keyCode in s)
            return a = t.keyCode == r.ARROW_UP, f = e.getParagraph(this.value, a ? i.start : i.end), l = (a ? f.start - 1 : f.end + 1).limit(0, this.length), n.setSelectionRange(this, t.shiftKey ? a ? i.end : i.start : l, l), n.stopEvent(t);
        if (t.altKey && t.keyCode == r.ENTER)
            return c = this.value, f = e.getParagraph(c, i.start), h = e.getFillPrefix(f.text), p = h[0], typeof p == "function" ? p = p(h) : p = p[0], c = c.substr(0, f.end) + "\n\n" + p + c.substr(f.end), this.value = c, n.setSelectionRange(this, f.end + 2 + p.length), d.call(this)
    }
    function a(e, t, n) {
        var r, i, s = -1;
        t.lastIndex = 0, t.global = !0, i = -1;
        while (r = t.exec(e)) {
            if (t.lastIndex >= n)
                break;
            s = t.lastIndex;
            if (s == i)
                throw "Repeated! " + s;
            i = s
        }
        return s
    }
    function f(e, t, n) {
        var r;
        return t.lastIndex = n, t.global = !0, r = t.exec(e), r ? r.index : null
    }
    var e, t, n = DynarchDomUtils, r = DlKeyboard, i = [/^(\s*[-*]+\s+)/, function(e) {
            return [e, " ".x(e[0].length), e[0].length]
        }, /^(\s*)([0-9]+)(\.\s+)/, function(e) {
            return [function() {
                    var t = parseInt(e[2], 10) + 1;
                    return e[1] + t + e[3]
                }, " ".x(e[0].length), e[0].length]
        }, /^(\s*)([a-z])(\)\s+)/i, function(e) {
            return [function() {
                    var t = String.fromCharCode(e[2].charCodeAt(0) + 1);
                    return e[1] + t + e[3]
                }, " ".x(e[0].length), e[0].length]
        }, /^\s*([>|]\s*)*/, function(e) {
            return [e, e[0], e[0].length, /\n\s*([>|]\s*)*/g, "\n"]
        }, /^\s+/, function(e) {
            return [e, e[0], e[0].length]
        }], s = [r.ARROW_UP, r.ARROW_DOWN].toHash(!0), o = String.fromCharCode(0);
    return t = /\n([>|\s]*\n)+/g, e = {getParagraph: function(e, n) {
            var r = a(e, t, n + 1), i = f(e, t, n);
            return r == -1 && (r = 0), i == null && (i = e.length), {start: r,end: i,text: e.substring(r, i)}
        },getFillPrefix: function(e) {
            var t, n, r, s = 0;
            e = e.replace(/\x00/g, "");
            while (s < i.length) {
                t = i[s++], n = i[s++], t.lastIndex = 0;
                if (r = t.exec(e))
                    return n(r)
            }
        },fillParagraph: function(t, n, r) {
            var i, s, u, a, f, l, c, h, p;
            t = t.substr(0, r) + o + t.substr(r), i = e.getFillPrefix(t), s = i[1], u = i[2], a = t.substr(0, u), t = t.substr(u), i[3] && (t = t.replace(i[3], function() {
                return i[4] || ""
            })), t = t.replace(/\n/g, " ").replace(/([^.?!])\s\s+/g, "$1 "), f = RegExp("(.{0," + (n - s.length) + "})(\\s+|$)", "g"), c = [], h = 0;
            while (l = f.exec(t)) {
                f.index != h ? p = t.substring(h, f.lastIndex) : p = l[1], h = f.lastIndex;
                if (!/\S/.test(p))
                    break;
                c.push(p.trim(!0))
            }
            return t = a + c.join("\n" + s), r = t.indexOf(o), r >= 0 && (t = t.substr(0, r) + t.substr(r + 1)), {text: t,pos: r}
        },fillText: function(t, n, r) {
            var i = e.getParagraph(t, r), s = t.substr(0, i.start), o = t.substr(i.end), u = r - i.start, a = e.fillParagraph(i.text, n, u);
            return {text: s + a.text + o,pos: i.start + a.pos}
        },emacsipateTextarea: function(e) {
            n.addEvent(e, is_ie ? "keydown" : "keypress", u)
        }}
}(), DEFINE_CLASS("DlTooltip", DlPopup, function(e) {
    e.FIXARGS = function(e) {
        e.zIndex = 2e3, e.focusable = !1, this._mouseDiff = {x: 8,y: 12}
    }
}), DEFINE_CLASS("DlTree", DlContainer, function(e, t, n) {
    e.CONSTRUCT = function() {
        this.__treeItems = []
    }, t.getItem = function(e) {
        return this.__treeItems[e]
    }, t.getItems = function() {
        return this.__treeItems
    }, t.appendWidget = function(t, n) {
        t instanceof DlTreeItem && t.parent === this && n > t.getIndex() && --n, e.BASE.appendWidget.call(this, t, n)
    }, t.removeWidget = function(t) {
        var n, r;
        e.BASE.removeWidget.call(this, t), t instanceof DlTreeItem && (n = this.__treeItems.find(t), this.__treeItems.splice(n, 1), r = this.__treeItems.length, r == 0 ? this.parent instanceof DlTreeItem && this.destroy() : (n == 0 && this.__treeItems[n]._setFirstLast(!0, null), n == r && this.__treeItems[n - 1]._setFirstLast(null, !0)))
    }, t._appendWidgetElement = function(e, t) {
        var n, r, i = this.__treeItems, s = this.getContentElement();
        if (t == null)
            e instanceof DlTreeItem && (n = i.peek(), n ? n._setFirstLast(null, !1) : e._setFirstLast(!0, null), i.push(e), e._setFirstLast(null, !0)), s.appendChild(e.getElement());
        else {
            if (t == i.length)
                return this._appendWidgetElement(e, null);
            r = i[t], r && r._setFirstLast(!1, t == i.length - 1), e._setFirstLast(t == 0, !1), i.splice(t, 0, e), s.insertBefore(e.getElement(), s.childNodes[t])
        }
    }, t.addSeparator = function(e) {
        n.createElement("div", null, {className: e || "DlTree-separator",innerHTML: "&nbsp;"}, this.getElement())
    }
}), DEFINE_CLASS("DlTreeItem", DlContainer, function(e, t, n) {
    function a(e) {
        var t = e.target;
        try {
            while (t && t.tagName.toLowerCase() != "td")
                t = t.parentNode
        } catch (n) {
            t = null
        }
        return t
    }
    function f(e) {
        var t = a(e);
        if (t && /DlTreeItem-(Expander|Icon)/.test(t.className))
            throw this.toggle(), new DlExStopEventBubbling
    }
    function l() {
        var e = this.getSubtreeDiv();
        window.DL_CLOSING || n.trash(e), n.removeEvent(this.getDivElement(), "mousedown", this.__onLabelMouseDown)
    }
    var r, i = n.createElement, s = n.addClass, o = n.delClass, u = n.condClass;
    e.CONSTRUCT = function() {
        this.setIconClass(this.__iconClass), this.__iconClass = null
    }, e.DEFAULT_ARGS = {__label: ["label", null],__iconClass: ["iconClass", null],__itemClass: ["itemClassName", null]}, e.DEFAULT_EVENTS = ["onExpand", "onCollapse", "onLabelMouseDown"], r = "<div class='DlTreeItem-div'><table cellspacing='0' cellpadding='0' class='DlTreeItem-Table'><tbody><tr><td class='DlTreeItem-Expander'><div class='DlTree-IconWidth'>&nbsp;</div></td><td></td><td class='DlTreeItem-Label'></td></tr></tbody></table></div><div class='DlTreeItem-Subtree'></div>", t._setFirstLast = function(e, t) {
        e != null && (this.condClass(e, "DlTreeItem-First"), u(this.getTableElement(), e, "DlTreeItem-First")), t != null && (this.condClass(t, "DlTreeItem-Last"), u(this.getTableElement(), t, "DlTreeItem-Last"))
    }, t._setListeners = function() {
        e.BASE._setListeners.call(this), this.addEventListener({onMouseDown: f,onDestroy: l})
    }, t._createElement = function() {
        e.BASE._createElement.call(this), this.getElement().innerHTML = r, this.__label && this.setContent(this.__label), this.setUnselectable(), this.__onLabelMouseDown = this._onLabelMouseDown.$(this), n.addEvent(this.getDivElement(), "mousedown", this.__onLabelMouseDown), this.__itemClass && s(this.getDivElement(), this.__itemClass)
    }, t._onLabelMouseDown = function() {
        this.callHooks("onLabelMouseDown")
    }, t.getDivElement = function() {
        return this.getElement().firstChild
    }, t.getTableElement = function() {
        return this.getElement().firstChild.firstChild
    }, t.getExpanderElement = function() {
        return this.getTableElement().rows[0].cells[0]
    }, t.getIconElement = function() {
        return this.getTableElement().rows[0].cells[1]
    }, t.getContentElement = function() {
        return this.getTableElement().rows[0].cells[2]
    }, t.getSubtreeDiv = function() {
        return this.getElement().childNodes[1]
    }, t.getSubtreeWidget = function() {
        return this._subtree
    }, t.getIndex = function() {
        return this.parent.__treeItems.find(this)
    }, t.getParentItem = function() {
        return this.parent.parent
    }, t.addSubItem = function(e, t) {
        var n = this.getSubtreeWidget();
        !n && !this._tree && (n = new DlTree({}), this.setTree(n), this.expand()), n.appendWidget(e, t)
    }, t.setTree = function(e, t, n) {
        this._tree && typeof this._tree != "function" && this.removeWidget(this._tree), this._tree = e, e != null && (typeof e != "function" ? this.appendWidget(e, !0) : t == null && (t = !1), t ? this.expand(t) : (this.getSubtreeDiv().style.display = "none", this.updateExpanderState())), n == null ? (this._subtreeNeverExpires = !0, this._subtreeExpires = null) : (this._subtreeNeverExpires = !1, this._subtreeExpires = (new Date).getTime() + n), this.condClass(e, "DlTreeItem-hasSubtree"), this.updateExpanderState()
    }, t.isExpanded = function() {
        return this.getSubtreeDiv().style.display !== "none"
    }, t.toggle = function() {
        this.expand(!this.isExpanded())
    }, t.getPath = function() {
        var e = [], t = this.getParentItem();
        while (t instanceof DlTreeItem)
            e.push(t), t = t.getParentItem();
        return e
    }, t.expandParents = function(e) {
        var t = this.getParentItem();
        while (t instanceof DlTreeItem)
            t.expand(e), t = t.getParentItem()
    }, t.expand = function(e, t) {
        function i() {
            n.getSubtreeDiv().style.display = e ? "block" : "none", n.updateExpanderState(), t || n.callHooks(e ? "onExpand" : "onCollapse")
        }
        function s(e, t) {
            var r = n._tree;
            if (n._subtree)
                try {
                    n._subtree.destroy()
                } catch (s) {
                }
            n._tree = r, t == null ? (n._subtreeNeverExpires = !0, n._subtreeExpires = null) : (n._subtreeNeverExpires = !1, n._subtreeExpires = (new Date).getTime() + t), n.appendWidget(e, !0), i()
        }
        var n, r;
        e == null && (e = !0), n = this, e !== this.isExpanded() && (e && typeof this._tree == "function" ? this._subtree ? this._subtreeNeverExpires ? i() : (r = (new Date).getTime(), this._subtreeExpires && r <= this._subtreeExpires ? i() : this._tree(s, this)) : this._tree(s, this) : i())
    }, t.setIconClass = function(e) {
        var t = this.getIconElement();
        u(t, e != null, "DlTreeItem-Icon"), this.iconClass && (t.innerHTML = "", o(t, this.iconClass)), e && (t.innerHTML = "<div class='DlTree-IconWidth'>&nbsp;</div>", s(t, e)), this.iconClass = e
    }, t.updateExpanderState = function() {
        var e, t = this.getExpanderElement().firstChild;
        this._tree ? (e = this.isExpanded(), u(t, e, "DlTreeItem-Arrow-Expanded", "DlTreeItem-Arrow-Collapsed"), u(this.getTableElement(), e, "DlTreeItem-Table-Expanded", "DlTreeItem-Table-Collapsed")) : (o(t, "DlTreeItem-Arrow-Expanded"), o(t, "DlTreeItem-Arrow-Collapsed"), this.delClass("DlTreeItem-hasSubtree"))
    }, t._appendWidgetElement = function(e, t) {
        var n, r = e.getElement();
        e instanceof DlTreeItem ? this.addSubItem(e, t) : (n = t || e instanceof DlTree ? this.getSubtreeDiv() : this.getContentElement(), t && (this._subtree = e, s(r, "DlTree-withLines"), this.addClass("DlTreeItem-hasSubtree")), n.appendChild(r))
    }, t._removeWidgetElement = function(t) {
        e.BASE._removeWidgetElement.call(this, t), this.getSubtreeDiv().firstChild || (this._tree = null, this._subtree = null), this.updateExpanderState()
    }, t._setFocusedStyle = function(e) {
        u(this.getDivElement(), e, "DlTreeItem-div-focus")
    }
}), DlType.TYPES = {}, DlType.prototype = {getDisplayValue: function(e) {
        return e
    },compare: function() {
        throw "No comparator for type: " + this.name
    }}, DEFINE_CLASS("DlUploadEntry", DlWidget, function(e, t, n) {
    function r(e) {
        e.applyHooks("onChange", [this, this.name, this.value])
    }
    function i(e) {
        is_ie && (e = this.ownerDocument.parentWindow.event), this.firstChild.style.right = 30 - e.clientX + "px"
    }
    e.DEFAULT_EVENTS = ["onUploadStart", "onUploadEnd", "onChange"], e.DEFAULT_ARGS = {_url: ["url", null],_files: ["files", ["file"]],_params: ["params", null]}, e.BEFORE_BASE = function() {
        this._files instanceof Array || (this._files = [this._files])
    }, t._createElement = function() {
        var t;
        e.BASE._createElement.call(this), t = n.createElement("iframe", null, {frameBorder: 0,marginHeight: 0,marginWidth: 0,allowTransparency: !0,src: is_ie ? "javascript:'';" : "about:blank"}, this.getElement()), this.refNode("_iframe", t)
    }, t.init = function() {
        var e, t, n, s, o = String.buffer("<html style='margin: 0; padding: 0; overflow: hidden; height: 100%;'>", "<head>", "<link type='text/css' rel='stylesheet' href='", Dynarch.getFileURL("css/uploadentry.css"), "' />", "</head>", "<body>", "<form action='", this._url, "' method='POST' encoding='multipart/form-data'>", "<input type='hidden' name='_uploaderID' value='", this.getWidgetId(), "' />"), u = this._params;
        if (u) {
            u instanceof Array && (u = u.toHash(""));
            for (e in u)
                o("<input type='hidden' name='", e, "' value='", u[e], "' />")
        }
        this._files.foreach(function(e) {
            o("<label class='upload'><input type='file' name='", e, "' /></label>")
        }), o("</form></body></html>"), t = this._iframe.contentWindow, n = t.document, n.open(), n.write(o.get()), n.close(), this.refNode("_win", t), this.refNode("_doc", n), this.refNode("_form", n.getElementsByTagName("form")[0]), this._form.method = "POST", this._form.encoding = "multipart/form-data", s = r.$(null, this), this._files.foreach(function(e) {
            var t = this._form.elements.namedItem(e);
            t.onchange = s, t.parentNode.onmousemove = i
        }, this)
    }, t.setParam = function(e, t) {
        var n, r;
        if (typeof e == "string")
            n = this.getField(e), n || (n = this._doc.createElement("input"), n.type = "hidden", n.name = e, this._form.appendChild(n)), n.value = t;
        else
            for (r in e)
                this.setParam(r, e[r])
    }, t.getParam = function(e) {
        var t = this.getField(e);
        return t && t.value
    }, t.getField = function(e) {
        return this._form.elements.namedItem(e)
    }, t.submit = function() {
        this.callHooks("onUploadStart"), this._form.submit()
    }, e.finishUpload = function(e) {
        e instanceof DlUploadEntry || (e = DlWidget.getById(e));
        if (!e)
            throw "No such uploader: " + e;
        e.init(), e.applyHooks("onUploadEnd", Array.$(arguments, 1))
    }
}), DEFINE_EXCEPTION("DlValidatorException"), DlValidatorException.MISMATCH = 1, DlValidatorException.TOO_SMALL = 2, DlValidatorException.TOO_BIG = 3, DEFINE_CLASS("DlValidator", null, function(e, t) {
    function n(e) {
        function n(t) {
            return t.foreach(function(t, n) {
                t.toLowerCase().indexOf(e) == 0 && $RETURN(n)
            })
        }
        var t;
        return e = e.toLowerCase(), t = n(DlTEXTS._date_shortMonthNames) || n(DlTEXTS._date_monthNames), t != null && t++, t
    }
    e.CONSTRUCT = function(t) {
        t && (typeof t == "string" && (t = e[t]), this._callback = t, this._args = arguments.length > 1 ? Array.$(arguments, 1) : null)
    }, t.ok = function(e) {
        var t, n;
        if (typeof this._lastData != "undefined" && this._lastData === e)
            return !0;
        try {
            return t = [e].concat(this._args || Array.$(arguments, 1)), n = this._callback.apply(this, t), this._lastData = e, this._lastValue = n, !0
        } catch (r) {
            if (r instanceof DlValidatorException)
                return this._error = r, !1;
            throw r
        }
    }, t.getLastVal = function() {
        return this._lastValue
    }, t.getLastData = function() {
        return this._lastData
    }, t.getError = function() {
        return this._error
    }, e.Number = function(e, t, n, r, i) {
        var s;
        e = e.replace(/\s/g, ""), s = new Number(e);
        if (isNaN(s))
            throw new DlValidatorException("Value must be numeric", DlValidatorException.MISMATCH);
        if (r && s != Math.round(s))
            throw new DlValidatorException("Value must be an integer", DlValidatorException.MISMATCH);
        if (t != null && s < t)
            throw new DlValidatorException("Value must be bigger than " + t, DlValidatorException.TOO_SMALL);
        if (n != null && s > n)
            throw new DlValidatorException("Value must be smaller than " + n, DlValidatorException.TOO_BIG);
        return i && (s = s.toFixed(i)), s
    }, e.Email = function(e) {
        e = e.trim();
        if (!/^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])$/i.test(e))
            throw new DlValidatorException("That doesn't look like an email address", DlValidatorException.MISMATCH);
        return e
    }, e.URL = function(e, t) {
        t || (t = {}), e = e.trim();
        if (!/^(https?|ftps?):\x2f\x2f/.test(e)) {
            if (/^([a-z0-9_-]+\.)+[a-z]+$/i.test(e))
                return /^www\./.test(e) || (e = "www." + e), "http://" + e + "/";
            throw new DlValidatorException("Value must be an absolute URL", DlValidatorException.MISMATCH)
        }
        return e
    }, e.Date = function(e, t, r) {
        var i, s, o, u, a, f, l, c, h, p;
        if (!/\S/.test(e))
            return "";
        t || (t = "%Y-%m-%d"), e = e.replace(/^\s+/, "").replace(/\s+$/, ""), i = new Date, s = null, o = null, u = null, a = null, f = null, l = null, c = e.match(/([0-9]{1,2}):([0-9]{1,2})(:[0-9]{1,2})?\s*(am|pm)?/i), c && (a = parseInt(c[1], 10), f = parseInt(c[2], 10), l = c[3] ? parseInt(c[3].substr(1), 10) : 0, e = e.substring(0, c.index) + e.substr(c.index + c[0].length), c[4] && (c[4].toLowerCase() == "pm" && a < 12 ? a += 12 : c[4].toLowerCase() == "am" && a >= 12 && (a -= 12))), h = e.split(/\W+/), p = [], h.foreach(function(e) {
            /^[0-9]{4}$/.test(e) ? (s = parseInt(e, 10), !o && !u && r == null && (r = !0)) : /^[0-9]{1,2}$/.test(e) ? (e = parseInt(e, 10), e < 60 ? e < 0 || e > 12 ? e >= 1 && e <= 31 && (u = e) : p.push(e) : s = e) : o = n(e)
        }), p.length < 2 ? p.length == 1 && (u ? o || (o = p.shift()) : u = p.shift()) : r ? (o || (o = p.shift()), u || (u = p.shift())) : (u || (u = p.shift()), o || (o = p.shift())), s || (s = p.length > 0 ? p.shift() : i.getFullYear()), s < 30 ? s += 2e3 : s < 99 && (s += 1900), o || (o = i.getMonth() + 1);
        if (!(s && o && u))
            throw this._date = null, new DlValidatorException("Can't figure out this date", DlValidatorException.MISMATCH);
        return this._date = new Date(s, o - 1, u, a, f, l), this._date.print(t)
    }
}), DlConsole.prototype = {log: function(e) {
        e = e.printf.apply(e, Array.$(arguments, 1)), this._addMsg({str: e})
    },line: function() {
        this._addMsg({str: "&nbsp;",cls: "sep"})
    },CC: function(e, t) {
        var n = this.win.document.createElement("div");
        n.className = t || "msg", n.innerHTML = e, this.win.document.body.appendChild(n), this.win.scrollTo(0, n.offsetTop + n.offsetHeight), this._last && DynarchDomUtils.delClass(this._last, "current"), DynarchDomUtils.addClass(n, "current"), this._last = n
    },_addMsg: function(e) {
        this._init(), this.win ? this.CC(e.str, e.cls) : this._messages.push(e)
    },_init: function() {
        this.win || window.open(Dynarch.getFileURL("html/dlconsole.html"), "DlConsole", "height=400,width=600,menubar=0,toolbar=0,scrollbars=1")
    },_loaded: function(e) {
        this._last = null, this.win = e, this.log("<b>DynarchLIB Console</b><br />Initialized at %s", new Date), this.line(), this._messages.foreach(this._addMsg, this)
    },protect: function(name) {
        var func = eval(name), f = function() {
            try {
                var e = [];
                for (var t = 0; t < arguments.length; ++t)
                    e.push(arguments[t]);
                console.log(name + " [" + e.join(", ") + "]"), func.apply(this, arguments)
            } catch (n) {
                throw alert("Exception in " + name + "\n" + n), n
            }
        };
        eval(name + " = f")
    }}, window.dlconsole || (window.dlconsole = new DlConsole);
