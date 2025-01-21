(() => {
  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };
  var composeFlipped = function(dictSemigroupoid) {
    var compose1 = compose(dictSemigroupoid);
    return function(f) {
      return function(g) {
        return compose1(g)(f);
      };
    };
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map112(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidRight = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(x) {
      return map112($$const(x));
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applyFirst = function(dictApply) {
    var apply1 = apply(dictApply);
    var map25 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map25($$const)(a))(b);
      };
    };
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map25 = map(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply1(map25($$const(identity2))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map25 = map(dictApply.Functor0());
    return function(f) {
      return function(a) {
        return function(b) {
          return apply1(map25(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var when = function(dictApplicative) {
    var pure12 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure12(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply5 = apply(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply5(pure12(f))(a);
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };
  var unsafeSet = function(label) {
    return function(value) {
      return function(rec) {
        var copy = {};
        for (var key2 in rec) {
          if ({}.hasOwnProperty.call(rec, key2)) {
            copy[key2] = rec[key2];
          }
        }
        copy[label] = value;
        return copy;
      };
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupRecordNil = {
    appendRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    }
  };
  var semigroupArray = {
    append: concatArray
  };
  var appendRecord = function(dict) {
    return dict.appendRecord;
  };
  var semigroupRecord = function() {
    return function(dictSemigroupRecord) {
      return {
        append: appendRecord(dictSemigroupRecord)($$Proxy.value)
      };
    };
  };
  var append = function(dict) {
    return dict.append;
  };
  var semigroupFn = function(dictSemigroup) {
    var append15 = append(dictSemigroup);
    return {
      append: function(f) {
        return function(g) {
          return function(x) {
            return append15(f(x))(g(x));
          };
        };
      }
    };
  };
  var semigroupRecordCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(dictSemigroupRecord) {
        var appendRecord1 = appendRecord(dictSemigroupRecord);
        return function(dictSemigroup) {
          var append15 = append(dictSemigroup);
          return {
            appendRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = appendRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var insert6 = unsafeSet(key2);
                  var get5 = unsafeGet(key2);
                  return insert6(append15(get5(ra))(get5(rb)))(tail2);
                };
              };
            }
          };
        };
      };
    };
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var join = function(dictBind) {
    var bind1 = bind(dictBind);
    return function(m) {
      return bind1(m)(identity3);
    };
  };

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eqRowNil = {
    eqRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return true;
        };
      };
    }
  };
  var eqRecord = function(dict) {
    return dict.eqRecord;
  };
  var eqRec = function() {
    return function(dictEqRecord) {
      return {
        eq: eqRecord(dictEqRecord)($$Proxy.value)
      };
    };
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var eqRowCons = function(dictEqRecord) {
    var eqRecord1 = eqRecord(dictEqRecord);
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictEq) {
          var eq32 = eq(dictEq);
          return {
            eqRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = eqRecord1($$Proxy.value)(ra)(rb);
                  var key2 = reflectSymbol2($$Proxy.value);
                  var get5 = unsafeGet(key2);
                  return eq32(get5(ra))(get5(rb)) && tail2;
                };
              };
            }
          };
        };
      };
    };
  };
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq32(x)(y))(false);
      };
    };
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showCharImpl = function(c) {
    var code = c.charCodeAt(0);
    if (code < 32 || code === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq8) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq8 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var one = function(dict) {
    return dict.one;
  };
  var mul = function(dict) {
    return dict.mul;
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a) {
      return sub1(zero2)(a);
    };
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var greaterThan = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare3(a1)(a2);
        if (v instanceof GT) {
          return true;
        }
        ;
        return false;
      };
    };
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare3(a1)(a2);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var lessThan = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare3(a1)(a2);
        if (v instanceof LT) {
          return true;
        }
        ;
        return false;
      };
    };
  };
  var signum = function(dictOrd) {
    var lessThan1 = lessThan(dictOrd);
    var greaterThan1 = greaterThan(dictOrd);
    return function(dictRing) {
      var Semiring0 = dictRing.Semiring0();
      var zero2 = zero(Semiring0);
      var negate1 = negate(dictRing);
      var one2 = one(Semiring0);
      return function(x) {
        var $89 = lessThan1(x)(zero2);
        if ($89) {
          return negate1(one2);
        }
        ;
        var $90 = greaterThan1(x)(zero2);
        if ($90) {
          return one2;
        }
        ;
        return x;
      };
    };
  };
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var abs = function(dictOrd) {
    var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
    return function(dictRing) {
      var zero2 = zero(dictRing.Semiring0());
      var negate1 = negate(dictRing);
      return function(x) {
        var $99 = greaterThanOrEq1(x)(zero2);
        if ($99) {
          return x;
        }
        ;
        return negate1(x);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Maybe/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe$prime = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v(unit);
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 250, column 1 - line 250, column 62): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity4);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Data.String.Common/foreign.js
  var toLower = function(s) {
    return s.toLowerCase();
  };
  var toUpper = function(s) {
    return s.toUpperCase();
  };

  // output/Data.Abc/index.js
  var show2 = /* @__PURE__ */ show(showInt);
  var Volta = /* @__PURE__ */ function() {
    function Volta2(value0) {
      this.value0 = value0;
    }
    ;
    Volta2.create = function(value0) {
      return new Volta2(value0);
    };
    return Volta2;
  }();
  var VoltaRange = /* @__PURE__ */ function() {
    function VoltaRange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    VoltaRange2.create = function(value0) {
      return function(value1) {
        return new VoltaRange2(value0, value1);
      };
    };
    return VoltaRange2;
  }();
  var Thin = /* @__PURE__ */ function() {
    function Thin2() {
    }
    ;
    Thin2.value = new Thin2();
    return Thin2;
  }();
  var ThinThin = /* @__PURE__ */ function() {
    function ThinThin2() {
    }
    ;
    ThinThin2.value = new ThinThin2();
    return ThinThin2;
  }();
  var ThinThick = /* @__PURE__ */ function() {
    function ThinThick2() {
    }
    ;
    ThinThick2.value = new ThinThick2();
    return ThinThick2;
  }();
  var ThickThin = /* @__PURE__ */ function() {
    function ThickThin2() {
    }
    ;
    ThickThin2.value = new ThickThin2();
    return ThickThin2;
  }();
  var Invisible = /* @__PURE__ */ function() {
    function Invisible2() {
    }
    ;
    Invisible2.value = new Invisible2();
    return Invisible2;
  }();
  var A = /* @__PURE__ */ function() {
    function A2() {
    }
    ;
    A2.value = new A2();
    return A2;
  }();
  var B = /* @__PURE__ */ function() {
    function B2() {
    }
    ;
    B2.value = new B2();
    return B2;
  }();
  var C = /* @__PURE__ */ function() {
    function C2() {
    }
    ;
    C2.value = new C2();
    return C2;
  }();
  var D = /* @__PURE__ */ function() {
    function D2() {
    }
    ;
    D2.value = new D2();
    return D2;
  }();
  var E = /* @__PURE__ */ function() {
    function E2() {
    }
    ;
    E2.value = new E2();
    return E2;
  }();
  var F = /* @__PURE__ */ function() {
    function F2() {
    }
    ;
    F2.value = new F2();
    return F2;
  }();
  var G = /* @__PURE__ */ function() {
    function G2() {
    }
    ;
    G2.value = new G2();
    return G2;
  }();
  var Major = /* @__PURE__ */ function() {
    function Major2() {
    }
    ;
    Major2.value = new Major2();
    return Major2;
  }();
  var Minor = /* @__PURE__ */ function() {
    function Minor2() {
    }
    ;
    Minor2.value = new Minor2();
    return Minor2;
  }();
  var Ionian = /* @__PURE__ */ function() {
    function Ionian2() {
    }
    ;
    Ionian2.value = new Ionian2();
    return Ionian2;
  }();
  var Dorian = /* @__PURE__ */ function() {
    function Dorian2() {
    }
    ;
    Dorian2.value = new Dorian2();
    return Dorian2;
  }();
  var Phrygian = /* @__PURE__ */ function() {
    function Phrygian2() {
    }
    ;
    Phrygian2.value = new Phrygian2();
    return Phrygian2;
  }();
  var Lydian = /* @__PURE__ */ function() {
    function Lydian2() {
    }
    ;
    Lydian2.value = new Lydian2();
    return Lydian2;
  }();
  var Mixolydian = /* @__PURE__ */ function() {
    function Mixolydian2() {
    }
    ;
    Mixolydian2.value = new Mixolydian2();
    return Mixolydian2;
  }();
  var Aeolian = /* @__PURE__ */ function() {
    function Aeolian2() {
    }
    ;
    Aeolian2.value = new Aeolian2();
    return Aeolian2;
  }();
  var Locrian = /* @__PURE__ */ function() {
    function Locrian2() {
    }
    ;
    Locrian2.value = new Locrian2();
    return Locrian2;
  }();
  var LeftArrow = /* @__PURE__ */ function() {
    function LeftArrow2(value0) {
      this.value0 = value0;
    }
    ;
    LeftArrow2.create = function(value0) {
      return new LeftArrow2(value0);
    };
    return LeftArrow2;
  }();
  var RightArrow = /* @__PURE__ */ function() {
    function RightArrow2(value0) {
      this.value0 = value0;
    }
    ;
    RightArrow2.create = function(value0) {
      return new RightArrow2(value0);
    };
    return RightArrow2;
  }();
  var AboveNextSymbol = /* @__PURE__ */ function() {
    function AboveNextSymbol2() {
    }
    ;
    AboveNextSymbol2.value = new AboveNextSymbol2();
    return AboveNextSymbol2;
  }();
  var BelowNextSymbol = /* @__PURE__ */ function() {
    function BelowNextSymbol2() {
    }
    ;
    BelowNextSymbol2.value = new BelowNextSymbol2();
    return BelowNextSymbol2;
  }();
  var LeftOfNextSymbol = /* @__PURE__ */ function() {
    function LeftOfNextSymbol2() {
    }
    ;
    LeftOfNextSymbol2.value = new LeftOfNextSymbol2();
    return LeftOfNextSymbol2;
  }();
  var RightOfNextSymbol = /* @__PURE__ */ function() {
    function RightOfNextSymbol2() {
    }
    ;
    RightOfNextSymbol2.value = new RightOfNextSymbol2();
    return RightOfNextSymbol2;
  }();
  var Discretional = /* @__PURE__ */ function() {
    function Discretional2() {
    }
    ;
    Discretional2.value = new Discretional2();
    return Discretional2;
  }();
  var Sharp = /* @__PURE__ */ function() {
    function Sharp2() {
    }
    ;
    Sharp2.value = new Sharp2();
    return Sharp2;
  }();
  var Flat = /* @__PURE__ */ function() {
    function Flat2() {
    }
    ;
    Flat2.value = new Flat2();
    return Flat2;
  }();
  var DoubleSharp = /* @__PURE__ */ function() {
    function DoubleSharp2() {
    }
    ;
    DoubleSharp2.value = new DoubleSharp2();
    return DoubleSharp2;
  }();
  var DoubleFlat = /* @__PURE__ */ function() {
    function DoubleFlat2() {
    }
    ;
    DoubleFlat2.value = new DoubleFlat2();
    return DoubleFlat2;
  }();
  var Natural = /* @__PURE__ */ function() {
    function Natural2() {
    }
    ;
    Natural2.value = new Natural2();
    return Natural2;
  }();
  var Implicit = /* @__PURE__ */ function() {
    function Implicit2() {
    }
    ;
    Implicit2.value = new Implicit2();
    return Implicit2;
  }();
  var Pitch = /* @__PURE__ */ function() {
    function Pitch2(value0) {
      this.value0 = value0;
    }
    ;
    Pitch2.create = function(value0) {
      return new Pitch2(value0);
    };
    return Pitch2;
  }();
  var Area = /* @__PURE__ */ function() {
    function Area2(value0) {
      this.value0 = value0;
    }
    ;
    Area2.create = function(value0) {
      return new Area2(value0);
    };
    return Area2;
  }();
  var Book = /* @__PURE__ */ function() {
    function Book2(value0) {
      this.value0 = value0;
    }
    ;
    Book2.create = function(value0) {
      return new Book2(value0);
    };
    return Book2;
  }();
  var Composer = /* @__PURE__ */ function() {
    function Composer2(value0) {
      this.value0 = value0;
    }
    ;
    Composer2.create = function(value0) {
      return new Composer2(value0);
    };
    return Composer2;
  }();
  var Discography = /* @__PURE__ */ function() {
    function Discography2(value0) {
      this.value0 = value0;
    }
    ;
    Discography2.create = function(value0) {
      return new Discography2(value0);
    };
    return Discography2;
  }();
  var FileUrl = /* @__PURE__ */ function() {
    function FileUrl2(value0) {
      this.value0 = value0;
    }
    ;
    FileUrl2.create = function(value0) {
      return new FileUrl2(value0);
    };
    return FileUrl2;
  }();
  var Group = /* @__PURE__ */ function() {
    function Group2(value0) {
      this.value0 = value0;
    }
    ;
    Group2.create = function(value0) {
      return new Group2(value0);
    };
    return Group2;
  }();
  var History = /* @__PURE__ */ function() {
    function History2(value0) {
      this.value0 = value0;
    }
    ;
    History2.create = function(value0) {
      return new History2(value0);
    };
    return History2;
  }();
  var Instruction = /* @__PURE__ */ function() {
    function Instruction2(value0) {
      this.value0 = value0;
    }
    ;
    Instruction2.create = function(value0) {
      return new Instruction2(value0);
    };
    return Instruction2;
  }();
  var Key = /* @__PURE__ */ function() {
    function Key2(value0) {
      this.value0 = value0;
    }
    ;
    Key2.create = function(value0) {
      return new Key2(value0);
    };
    return Key2;
  }();
  var UnitNoteLength = /* @__PURE__ */ function() {
    function UnitNoteLength2(value0) {
      this.value0 = value0;
    }
    ;
    UnitNoteLength2.create = function(value0) {
      return new UnitNoteLength2(value0);
    };
    return UnitNoteLength2;
  }();
  var Meter = /* @__PURE__ */ function() {
    function Meter2(value0) {
      this.value0 = value0;
    }
    ;
    Meter2.create = function(value0) {
      return new Meter2(value0);
    };
    return Meter2;
  }();
  var Macro = /* @__PURE__ */ function() {
    function Macro2(value0) {
      this.value0 = value0;
    }
    ;
    Macro2.create = function(value0) {
      return new Macro2(value0);
    };
    return Macro2;
  }();
  var Notes = /* @__PURE__ */ function() {
    function Notes2(value0) {
      this.value0 = value0;
    }
    ;
    Notes2.create = function(value0) {
      return new Notes2(value0);
    };
    return Notes2;
  }();
  var Origin = /* @__PURE__ */ function() {
    function Origin2(value0) {
      this.value0 = value0;
    }
    ;
    Origin2.create = function(value0) {
      return new Origin2(value0);
    };
    return Origin2;
  }();
  var Parts = /* @__PURE__ */ function() {
    function Parts2(value0) {
      this.value0 = value0;
    }
    ;
    Parts2.create = function(value0) {
      return new Parts2(value0);
    };
    return Parts2;
  }();
  var Tempo = /* @__PURE__ */ function() {
    function Tempo2(value0) {
      this.value0 = value0;
    }
    ;
    Tempo2.create = function(value0) {
      return new Tempo2(value0);
    };
    return Tempo2;
  }();
  var Rhythm = /* @__PURE__ */ function() {
    function Rhythm2(value0) {
      this.value0 = value0;
    }
    ;
    Rhythm2.create = function(value0) {
      return new Rhythm2(value0);
    };
    return Rhythm2;
  }();
  var Remark = /* @__PURE__ */ function() {
    function Remark2(value0) {
      this.value0 = value0;
    }
    ;
    Remark2.create = function(value0) {
      return new Remark2(value0);
    };
    return Remark2;
  }();
  var Source = /* @__PURE__ */ function() {
    function Source2(value0) {
      this.value0 = value0;
    }
    ;
    Source2.create = function(value0) {
      return new Source2(value0);
    };
    return Source2;
  }();
  var SymbolLine = /* @__PURE__ */ function() {
    function SymbolLine2(value0) {
      this.value0 = value0;
    }
    ;
    SymbolLine2.create = function(value0) {
      return new SymbolLine2(value0);
    };
    return SymbolLine2;
  }();
  var Title = /* @__PURE__ */ function() {
    function Title2(value0) {
      this.value0 = value0;
    }
    ;
    Title2.create = function(value0) {
      return new Title2(value0);
    };
    return Title2;
  }();
  var UserDefined = /* @__PURE__ */ function() {
    function UserDefined2(value0) {
      this.value0 = value0;
    }
    ;
    UserDefined2.create = function(value0) {
      return new UserDefined2(value0);
    };
    return UserDefined2;
  }();
  var Voice = /* @__PURE__ */ function() {
    function Voice2(value0) {
      this.value0 = value0;
    }
    ;
    Voice2.create = function(value0) {
      return new Voice2(value0);
    };
    return Voice2;
  }();
  var WordsAfter = /* @__PURE__ */ function() {
    function WordsAfter2(value0) {
      this.value0 = value0;
    }
    ;
    WordsAfter2.create = function(value0) {
      return new WordsAfter2(value0);
    };
    return WordsAfter2;
  }();
  var WordsAligned = /* @__PURE__ */ function() {
    function WordsAligned2(value0) {
      this.value0 = value0;
    }
    ;
    WordsAligned2.create = function(value0) {
      return new WordsAligned2(value0);
    };
    return WordsAligned2;
  }();
  var ReferenceNumber = /* @__PURE__ */ function() {
    function ReferenceNumber2(value0) {
      this.value0 = value0;
    }
    ;
    ReferenceNumber2.create = function(value0) {
      return new ReferenceNumber2(value0);
    };
    return ReferenceNumber2;
  }();
  var Transcription = /* @__PURE__ */ function() {
    function Transcription2(value0) {
      this.value0 = value0;
    }
    ;
    Transcription2.create = function(value0) {
      return new Transcription2(value0);
    };
    return Transcription2;
  }();
  var FieldContinuation = /* @__PURE__ */ function() {
    function FieldContinuation2(value0) {
      this.value0 = value0;
    }
    ;
    FieldContinuation2.create = function(value0) {
      return new FieldContinuation2(value0);
    };
    return FieldContinuation2;
  }();
  var Comment = /* @__PURE__ */ function() {
    function Comment2(value0) {
      this.value0 = value0;
    }
    ;
    Comment2.create = function(value0) {
      return new Comment2(value0);
    };
    return Comment2;
  }();
  var UnsupportedHeader = /* @__PURE__ */ function() {
    function UnsupportedHeader2() {
    }
    ;
    UnsupportedHeader2.value = new UnsupportedHeader2();
    return UnsupportedHeader2;
  }();
  var Note = /* @__PURE__ */ function() {
    function Note2(value0) {
      this.value0 = value0;
    }
    ;
    Note2.create = function(value0) {
      return new Note2(value0);
    };
    return Note2;
  }();
  var BrokenRhythmPair = /* @__PURE__ */ function() {
    function BrokenRhythmPair2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    BrokenRhythmPair2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new BrokenRhythmPair2(value0, value1, value2);
        };
      };
    };
    return BrokenRhythmPair2;
  }();
  var Rest = /* @__PURE__ */ function() {
    function Rest2(value0) {
      this.value0 = value0;
    }
    ;
    Rest2.create = function(value0) {
      return new Rest2(value0);
    };
    return Rest2;
  }();
  var Tuplet = /* @__PURE__ */ function() {
    function Tuplet2(value0) {
      this.value0 = value0;
    }
    ;
    Tuplet2.create = function(value0) {
      return new Tuplet2(value0);
    };
    return Tuplet2;
  }();
  var DecoratedSpace = /* @__PURE__ */ function() {
    function DecoratedSpace2(value0) {
      this.value0 = value0;
    }
    ;
    DecoratedSpace2.create = function(value0) {
      return new DecoratedSpace2(value0);
    };
    return DecoratedSpace2;
  }();
  var Annotation = /* @__PURE__ */ function() {
    function Annotation2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Annotation2.create = function(value0) {
      return function(value1) {
        return new Annotation2(value0, value1);
      };
    };
    return Annotation2;
  }();
  var ChordSymbol = /* @__PURE__ */ function() {
    function ChordSymbol2(value0) {
      this.value0 = value0;
    }
    ;
    ChordSymbol2.create = function(value0) {
      return new ChordSymbol2(value0);
    };
    return ChordSymbol2;
  }();
  var Chord = /* @__PURE__ */ function() {
    function Chord2(value0) {
      this.value0 = value0;
    }
    ;
    Chord2.create = function(value0) {
      return new Chord2(value0);
    };
    return Chord2;
  }();
  var Inline = /* @__PURE__ */ function() {
    function Inline2(value0) {
      this.value0 = value0;
    }
    ;
    Inline2.create = function(value0) {
      return new Inline2(value0);
    };
    return Inline2;
  }();
  var Spacer = /* @__PURE__ */ function() {
    function Spacer2(value0) {
      this.value0 = value0;
    }
    ;
    Spacer2.create = function(value0) {
      return new Spacer2(value0);
    };
    return Spacer2;
  }();
  var Ignore = /* @__PURE__ */ function() {
    function Ignore2() {
    }
    ;
    Ignore2.value = new Ignore2();
    return Ignore2;
  }();
  var Continuation = /* @__PURE__ */ function() {
    function Continuation2(value0) {
      this.value0 = value0;
    }
    ;
    Continuation2.create = function(value0) {
      return new Continuation2(value0);
    };
    return Continuation2;
  }();
  var Score = /* @__PURE__ */ function() {
    function Score2(value0) {
      this.value0 = value0;
    }
    ;
    Score2.create = function(value0) {
      return new Score2(value0);
    };
    return Score2;
  }();
  var BodyInfo = /* @__PURE__ */ function() {
    function BodyInfo2(value0) {
      this.value0 = value0;
    }
    ;
    BodyInfo2.create = function(value0) {
      return new BodyInfo2(value0);
    };
    return BodyInfo2;
  }();
  var showVolta = {
    show: function(v) {
      if (v instanceof Volta) {
        return show2(v.value0);
      }
      ;
      if (v instanceof VoltaRange) {
        return show2(v.value0) + ("-" + show2(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Data.Abc (line 203, column 1 - line 205, column 58): " + [v.constructor.name]);
    }
  };
  var showPitchClass = {
    show: function(v) {
      if (v instanceof A) {
        return "A";
      }
      ;
      if (v instanceof B) {
        return "B";
      }
      ;
      if (v instanceof C) {
        return "C";
      }
      ;
      if (v instanceof D) {
        return "D";
      }
      ;
      if (v instanceof E) {
        return "E";
      }
      ;
      if (v instanceof F) {
        return "F";
      }
      ;
      if (v instanceof G) {
        return "G";
      }
      ;
      throw new Error("Failed pattern match at Data.Abc (line 278, column 1 - line 285, column 15): " + [v.constructor.name]);
    }
  };
  var middlecOctave = 5;
  var eqThickness = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Thin && y instanceof Thin) {
          return true;
        }
        ;
        if (x instanceof ThinThin && y instanceof ThinThin) {
          return true;
        }
        ;
        if (x instanceof ThinThick && y instanceof ThinThick) {
          return true;
        }
        ;
        if (x instanceof ThickThin && y instanceof ThickThin) {
          return true;
        }
        ;
        if (x instanceof Invisible && y instanceof Invisible) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eqPitchCLass = {
    eq: function(x) {
      return function(y) {
        if (x instanceof A && y instanceof A) {
          return true;
        }
        ;
        if (x instanceof B && y instanceof B) {
          return true;
        }
        ;
        if (x instanceof C && y instanceof C) {
          return true;
        }
        ;
        if (x instanceof D && y instanceof D) {
          return true;
        }
        ;
        if (x instanceof E && y instanceof E) {
          return true;
        }
        ;
        if (x instanceof F && y instanceof F) {
          return true;
        }
        ;
        if (x instanceof G && y instanceof G) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordPitchCLass = {
    compare: function(x) {
      return function(y) {
        if (x instanceof A && y instanceof A) {
          return EQ.value;
        }
        ;
        if (x instanceof A) {
          return LT.value;
        }
        ;
        if (y instanceof A) {
          return GT.value;
        }
        ;
        if (x instanceof B && y instanceof B) {
          return EQ.value;
        }
        ;
        if (x instanceof B) {
          return LT.value;
        }
        ;
        if (y instanceof B) {
          return GT.value;
        }
        ;
        if (x instanceof C && y instanceof C) {
          return EQ.value;
        }
        ;
        if (x instanceof C) {
          return LT.value;
        }
        ;
        if (y instanceof C) {
          return GT.value;
        }
        ;
        if (x instanceof D && y instanceof D) {
          return EQ.value;
        }
        ;
        if (x instanceof D) {
          return LT.value;
        }
        ;
        if (y instanceof D) {
          return GT.value;
        }
        ;
        if (x instanceof E && y instanceof E) {
          return EQ.value;
        }
        ;
        if (x instanceof E) {
          return LT.value;
        }
        ;
        if (y instanceof E) {
          return GT.value;
        }
        ;
        if (x instanceof F && y instanceof F) {
          return EQ.value;
        }
        ;
        if (x instanceof F) {
          return LT.value;
        }
        ;
        if (y instanceof F) {
          return GT.value;
        }
        ;
        if (x instanceof G && y instanceof G) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Abc (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqPitchCLass;
    }
  };
  var eqMode = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Major && y instanceof Major) {
          return true;
        }
        ;
        if (x instanceof Minor && y instanceof Minor) {
          return true;
        }
        ;
        if (x instanceof Ionian && y instanceof Ionian) {
          return true;
        }
        ;
        if (x instanceof Dorian && y instanceof Dorian) {
          return true;
        }
        ;
        if (x instanceof Phrygian && y instanceof Phrygian) {
          return true;
        }
        ;
        if (x instanceof Lydian && y instanceof Lydian) {
          return true;
        }
        ;
        if (x instanceof Mixolydian && y instanceof Mixolydian) {
          return true;
        }
        ;
        if (x instanceof Aeolian && y instanceof Aeolian) {
          return true;
        }
        ;
        if (x instanceof Locrian && y instanceof Locrian) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eqAccidental = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Sharp && y instanceof Sharp) {
          return true;
        }
        ;
        if (x instanceof Flat && y instanceof Flat) {
          return true;
        }
        ;
        if (x instanceof DoubleSharp && y instanceof DoubleSharp) {
          return true;
        }
        ;
        if (x instanceof DoubleFlat && y instanceof DoubleFlat) {
          return true;
        }
        ;
        if (x instanceof Natural && y instanceof Natural) {
          return true;
        }
        ;
        if (x instanceof Implicit && y instanceof Implicit) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var enumPitchClass = {
    succ: function(v) {
      if (v instanceof C) {
        return new Just(D.value);
      }
      ;
      if (v instanceof D) {
        return new Just(E.value);
      }
      ;
      if (v instanceof E) {
        return new Just(F.value);
      }
      ;
      if (v instanceof F) {
        return new Just(G.value);
      }
      ;
      if (v instanceof G) {
        return new Just(A.value);
      }
      ;
      if (v instanceof A) {
        return new Just(B.value);
      }
      ;
      if (v instanceof B) {
        return new Just(C.value);
      }
      ;
      throw new Error("Failed pattern match at Data.Abc (line 290, column 1 - line 305, column 18): " + [v.constructor.name]);
    },
    pred: function(v) {
      if (v instanceof C) {
        return new Just(B.value);
      }
      ;
      if (v instanceof D) {
        return new Just(C.value);
      }
      ;
      if (v instanceof E) {
        return new Just(D.value);
      }
      ;
      if (v instanceof F) {
        return new Just(E.value);
      }
      ;
      if (v instanceof G) {
        return new Just(F.value);
      }
      ;
      if (v instanceof A) {
        return new Just(G.value);
      }
      ;
      if (v instanceof B) {
        return new Just(A.value);
      }
      ;
      throw new Error("Failed pattern match at Data.Abc (line 290, column 1 - line 305, column 18): " + [v.constructor.name]);
    },
    Ord0: function() {
      return ordPitchCLass;
    }
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a) {
      return function(f) {
        return f(a);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();
  var monadEither = {
    Applicative0: function() {
      return applicativeEither;
    },
    Bind1: function() {
      return bindEither;
    }
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var gcd = function(dictEq) {
    var eq8 = eq(dictEq);
    return function(dictEuclideanRing) {
      var zero2 = zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0());
      var mod1 = mod(dictEuclideanRing);
      return function(a) {
        return function(b) {
          var $24 = eq8(b)(zero2);
          if ($24) {
            return a;
          }
          ;
          return gcd(dictEq)(dictEuclideanRing)(b)(mod1(a)(b));
        };
      };
    };
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };
  var monoidFn = function(dictMonoid) {
    var mempty12 = mempty(dictMonoid);
    var semigroupFn2 = semigroupFn(dictMonoid.Semigroup0());
    return {
      mempty: function(v) {
        return mempty12;
      },
      Semigroup0: function() {
        return semigroupFn2;
      }
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Const/index.js
  var Const = function(x) {
    return x;
  };
  var functorConst = {
    map: function(f) {
      return function(m) {
        return m;
      };
    }
  };
  var applyConst = function(dictSemigroup) {
    var append15 = append(dictSemigroup);
    return {
      apply: function(v) {
        return function(v1) {
          return append15(v)(v1);
        };
      },
      Functor0: function() {
        return functorConst;
      }
    };
  };
  var applicativeConst = function(dictMonoid) {
    var mempty7 = mempty(dictMonoid);
    var applyConst1 = applyConst(dictMonoid.Semigroup0());
    return {
      pure: function(v) {
        return mempty7;
      },
      Apply0: function() {
        return applyConst1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var under = function() {
    return function() {
      return function(v) {
        return coerce2;
      };
    };
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };

  // output/Data.Lens.Internal.Forget/index.js
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var Forget = function(x) {
    return x;
  };
  var profunctorForget = {
    dimap: function(f) {
      return function(v) {
        return function(v1) {
          return function($36) {
            return v1(f($36));
          };
        };
      };
    }
  };
  var strongForget = {
    first: function(v) {
      return function($37) {
        return v(fst($37));
      };
    },
    second: function(v) {
      return function($38) {
        return v(snd($38));
      };
    },
    Profunctor0: function() {
      return profunctorForget;
    }
  };
  var choiceForget = function(dictMonoid) {
    var mempty7 = mempty(monoidFn(dictMonoid));
    return {
      left: function(v) {
        return either(v)(mempty7);
      },
      right: function(v) {
        return either(mempty7)(v);
      },
      Profunctor0: function() {
        return profunctorForget;
      }
    };
  };
  var wanderForget = function(dictMonoid) {
    var applicativeConst2 = applicativeConst(dictMonoid);
    var choiceForget1 = choiceForget(dictMonoid);
    return {
      wander: function(f) {
        return function(v) {
          return alaF2(Const)(f(applicativeConst2))(v);
        };
      },
      Strong0: function() {
        return strongForget;
      },
      Choice1: function() {
        return choiceForget1;
      }
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.Profunctor/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };
  var dimap = function(dict) {
    return dict.dimap;
  };
  var rmap = function(dictProfunctor) {
    var dimap1 = dimap(dictProfunctor);
    return function(b2c) {
      return dimap1(identity5)(b2c);
    };
  };

  // output/Data.Profunctor.Choice/index.js
  var right = function(dict) {
    return dict.right;
  };

  // output/Data.Profunctor.Strong/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var strongFn = {
    first: function(a2b) {
      return function(v) {
        return new Tuple(a2b(v.value0), v.value1);
      };
    },
    second: /* @__PURE__ */ map(functorTuple),
    Profunctor0: function() {
      return profunctorFn;
    }
  };
  var second = function(dict) {
    return dict.second;
  };
  var first = function(dict) {
    return dict.first;
  };
  var splitStrong = function(dictCategory) {
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    return function(dictStrong) {
      var first1 = first(dictStrong);
      var second1 = second(dictStrong);
      return function(l) {
        return function(r) {
          return composeFlipped2(first1(l))(second1(r));
        };
      };
    };
  };
  var fanout = function(dictCategory) {
    var identity1 = identity(dictCategory);
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    var splitStrong1 = splitStrong(dictCategory);
    return function(dictStrong) {
      var dimap2 = dimap(dictStrong.Profunctor0());
      var splitStrong2 = splitStrong1(dictStrong);
      return function(l) {
        return function(r) {
          var split3 = dimap2(identity6)(function(a) {
            return new Tuple(a, a);
          })(identity1);
          return composeFlipped2(split3)(splitStrong2(l)(r));
        };
      };
    };
  };

  // output/Data.Lens.Internal.Wander/index.js
  var wander = function(dict) {
    return dict.wander;
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply5) {
      return function(map25) {
        return function(pure12) {
          return function(f) {
            return function(array) {
              function go(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure12([]);
                  case 1:
                    return map25(array1)(f(array[bot]));
                  case 2:
                    return apply5(map25(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply5(apply5(map25(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply5(map25(concat2)(go(bot, pivot)))(go(pivot, top3));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Data.Maybe.First/index.js
  var First = function(x) {
    return x;
  };
  var semigroupFirst = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v;
        }
        ;
        return v1;
      };
    }
  };
  var monoidFirst = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupFirst;
      }
    };
  }();

  // output/Data.Monoid.Conj/index.js
  var Conj = function(x) {
    return x;
  };
  var semigroupConj = function(dictHeytingAlgebra) {
    var conj2 = conj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return conj2(v)(v1);
        };
      }
    };
  };
  var monoidConj = function(dictHeytingAlgebra) {
    var semigroupConj1 = semigroupConj(dictHeytingAlgebra);
    return {
      mempty: tt(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupConj1;
      }
    };
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x) {
    return x;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Data.Foldable/index.js
  var alaF3 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond4 = applySecond(dictApplicative.Apply0());
    var pure12 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond4(f($454));
        })(pure12(unit));
      };
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append6 = append(dictMonoid.Semigroup0());
      var mempty7 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append6(f(x))(acc);
          };
        })(mempty7);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var foldM = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonad) {
      var bind8 = bind(dictMonad.Bind1());
      var pure12 = pure(dictMonad.Applicative0());
      return function(f) {
        return function(b0) {
          return foldl22(function(b) {
            return function(a) {
              return bind8(b)(flip(f)(a));
            };
          })(pure12(b0));
        };
      };
    };
  };
  var any = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF3(Disj)(foldMap22(monoidDisj(dictHeytingAlgebra)));
    };
  };
  var elem = function(dictFoldable) {
    var any1 = any(dictFoldable)(heytingAlgebraBoolean);
    return function(dictEq) {
      var $462 = eq(dictEq);
      return function($463) {
        return any1($462($463));
      };
    };
  };
  var all = function(dictFoldable) {
    var foldMap22 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF3(Conj)(foldMap22(monoidConj(dictHeytingAlgebra)));
    };
  };

  // output/Data.Maybe.Last/index.js
  var Last = function(x) {
    return x;
  };
  var semigroupLast = {
    append: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return v1;
        }
        ;
        if (v1 instanceof Nothing) {
          return v;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe.Last (line 54, column 1 - line 56, column 36): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var monoidLast = /* @__PURE__ */ function() {
    return {
      mempty: Nothing.value,
      Semigroup0: function() {
        return semigroupLast;
      }
    };
  }();

  // output/Data.Traversable/index.js
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity7);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Lens.Prism/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var prism = function(to) {
    return function(fro) {
      return function(dictChoice) {
        var Profunctor0 = dictChoice.Profunctor0();
        var dimap2 = dimap(Profunctor0);
        var right2 = right(dictChoice);
        var rmap2 = rmap(Profunctor0);
        return function(pab) {
          return dimap2(fro)(either(identity8)(identity8))(right2(rmap2(to)(pab)));
        };
      };
    };
  };
  var prism$prime = function(to) {
    return function(fro) {
      return function(dictChoice) {
        return prism(to)(function(s) {
          return maybe(new Left(s))(Right.create)(fro(s));
        })(dictChoice);
      };
    };
  };

  // output/Data.Lens.Lens/index.js
  var lens$prime = function(to) {
    return function(dictStrong) {
      var dimap2 = dimap(dictStrong.Profunctor0());
      var first2 = first(dictStrong);
      return function(pab) {
        return dimap2(to)(function(v) {
          return v.value1(v.value0);
        })(first2(pab));
      };
    };
  };
  var lens = function(get5) {
    return function(set3) {
      return function(dictStrong) {
        return lens$prime(function(s) {
          return new Tuple(get5(s), function(b) {
            return set3(s)(b);
          });
        })(dictStrong);
      };
    };
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a) {
      return function(b) {
        return fn(a, b);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return fn(a, b, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
  var runFn5 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(e) {
              return fn(a, b, c, d, e);
            };
          };
        };
      };
    };
  };

  // output/Record/index.js
  var set = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(b) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(b)(r);
            };
          };
        };
      };
    };
  };
  var get = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };

  // output/Data.Lens.Record/index.js
  var prop = function(dictIsSymbol) {
    var get5 = get(dictIsSymbol)();
    var set3 = set(dictIsSymbol)()();
    return function() {
      return function() {
        return function(l) {
          return function(dictStrong) {
            return lens(get5(l))(flip(set3(l)))(dictStrong);
          };
        };
      };
    };
  };

  // output/Data.Abc.Optics/index.js
  var prop2 = /* @__PURE__ */ prop({
    reflectSymbol: function() {
      return "properties";
    }
  })()();
  var prop8 = /* @__PURE__ */ prop({
    reflectSymbol: function() {
      return "headers";
    }
  })()();
  var _properties = function(dictStrong) {
    return prop2($$Proxy.value)(dictStrong);
  };
  var _headers = function(dictStrong) {
    return prop8($$Proxy.value)(dictStrong);
  };
  var _Voice = function(dictChoice) {
    return prism$prime(Voice.create)(function(v) {
      if (v instanceof Voice) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    })(dictChoice);
  };
  var _UnitNoteLength = function(dictChoice) {
    return prism$prime(UnitNoteLength.create)(function(v) {
      if (v instanceof UnitNoteLength) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    })(dictChoice);
  };
  var _Tempo = function(dictChoice) {
    return prism$prime(Tempo.create)(function(v) {
      if (v instanceof Tempo) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    })(dictChoice);
  };
  var _ModifiedKeySignature = function(dictChoice) {
    return prism$prime(Key.create)(function(v) {
      if (v instanceof Key) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    })(dictChoice);
  };
  var _Meter = function(dictChoice) {
    return prism$prime(Meter.create)(function(v) {
      if (v instanceof Meter) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    })(dictChoice);
  };

  // output/Data.Array/foreign.js
  var rangeImpl = function(start, end) {
    var step2 = start > end ? -1 : 1;
    var result = new Array(step2 * (end - start) + 1);
    var i = start, n = 0;
    while (i !== end) {
      result[n++] = i;
      i += step2;
    }
    result[n] = i;
    return result;
  };
  var replicateFill = function(count, value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
  var replicatePolyfill = function(count, value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head6) {
      return function(tail2) {
        return new Cons3(head6, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr4, xs) {
      return listToArray(foldr4(curryCons)(emptyList)(xs));
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var indexImpl = function(just, nothing, xs, i) {
    return i < 0 || i >= xs.length ? nothing : just(xs[i]);
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (f(xs[i]))
        return just(i);
    }
    return nothing;
  };
  var _updateAt = function(just, nothing, i, a, l) {
    if (i < 0 || i >= l.length)
      return nothing;
    var l1 = l.slice();
    l1[i] = a;
    return just(l1);
  };
  var reverse = function(l) {
    return l.slice().reverse();
  };
  var concat = function(xss) {
    if (xss.length <= 1e4) {
      return Array.prototype.concat.apply([], xss);
    }
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
      var xs = xss[i];
      for (var j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var sortByImpl = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3, fromOrdering, xs) {
      var out;
      if (xs.length < 2)
        return xs;
      out = xs.slice(0);
      mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  }();
  var sliceImpl = function(s, e, l) {
    return l.slice(s, e);
  };
  var zipWithImpl = function(f, xs, ys) {
    var l = xs.length < ys.length ? xs.length : ys.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(xs[i])(ys[i]);
    }
    return result;
  };
  var unsafeIndexImpl = function(xs, n) {
    return xs[n];
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind8 = bind(dictMonad.Bind1());
    var pure12 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind8(f)(function(f$prime) {
          return bind8(a)(function(a$prime) {
            return pure12(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go(f($85));
    };
  };
  var monadRecEither = {
    tailRecM: function(f) {
      return function(a0) {
        var g = function(v) {
          if (v instanceof Left) {
            return new Done(new Left(v.value0));
          }
          ;
          if (v instanceof Right && v.value0 instanceof Loop) {
            return new Loop(f(v.value0.value0));
          }
          ;
          if (v instanceof Right && v.value0 instanceof Done) {
            return new Done(new Right(v.value0.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 145, column 7 - line 145, column 33): " + [v.constructor.name]);
        };
        return tailRec(g)(f(a0));
      };
    },
    Monad0: function() {
      return monadEither;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read2 = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl2 = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write2 = function(a) {
    return function(ref) {
      return function() {
        return ref.value = a;
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var modify$prime = modifyImpl2;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var functorST = {
    map: map_
  };

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  var pushAllImpl = function(as, xs) {
    return xs.push.apply(xs, as);
  };
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  function copyImpl(xs) {
    return xs.slice();
  }
  var thawImpl = copyImpl;
  var sortByImpl2 = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i = from3;
      j = mid;
      k = from3;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3, fromOrdering, xs) {
      if (xs.length < 2)
        return xs;
      mergeFromTo(compare3, fromOrdering, xs, xs.slice(0), 0, xs.length);
      return xs;
    };
  }();

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
  var withArray = function(f) {
    return function(xs) {
      return function __do2() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = function(a) {
    return runSTFn2(pushAllImpl)([a]);
  };

  // output/Data.Array.ST.Iterator/index.js
  var map4 = /* @__PURE__ */ map(functorST);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var $$void2 = /* @__PURE__ */ $$void(functorST);
  var Iterator = /* @__PURE__ */ function() {
    function Iterator2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Iterator2.create = function(value0) {
      return function(value1) {
        return new Iterator2(value0, value1);
      };
    };
    return Iterator2;
  }();
  var peek = function(v) {
    return function __do2() {
      var i = read2(v.value1)();
      return v.value0(i);
    };
  };
  var next = function(v) {
    return function __do2() {
      var i = read2(v.value1)();
      modify(function(v1) {
        return v1 + 1 | 0;
      })(v.value1)();
      return v.value0(i);
    };
  };
  var pushWhile = function(p) {
    return function(iter) {
      return function(array) {
        return function __do2() {
          var $$break = newSTRef(false)();
          while (map4(not2)(read2($$break))()) {
            (function __do3() {
              var mx = peek(iter)();
              if (mx instanceof Just && p(mx.value0)) {
                push(mx.value0)(array)();
                return $$void2(next(iter))();
              }
              ;
              return $$void2(write2(true)($$break))();
            })();
          }
          ;
          return {};
        };
      };
    };
  };
  var iterator = function(f) {
    return map4(Iterator.create(f))(newSTRef(0));
  };
  var iterate = function(iter) {
    return function(f) {
      return function __do2() {
        var $$break = newSTRef(false)();
        while (map4(not2)(read2($$break))()) {
          (function __do3() {
            var mx = next(iter)();
            if (mx instanceof Just) {
              return f(mx.value0)();
            }
            ;
            if (mx instanceof Nothing) {
              return $$void2(write2(true)($$break))();
            }
            ;
            throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): " + [mx.constructor.name]);
          })();
        }
        ;
        return {};
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust7) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value = b;
              while (true) {
                var maybe2 = f(value);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust7(maybe2);
                result.push(fst2(tuple));
                value = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust7) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value = b;
              while (true) {
                var tuple = f(value);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value = fromJust7(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Semigroup.Foldable/index.js
  var JoinWith = function(x) {
    return x;
  };
  var semigroupJoinWith = function(dictSemigroup) {
    var append6 = append(dictSemigroup);
    return {
      append: function(v) {
        return function(v1) {
          return function(j) {
            return append6(v(j))(append6(j)(v1(j)));
          };
        };
      }
    };
  };
  var joinee = function(v) {
    return v;
  };
  var foldMap1 = function(dict) {
    return dict.foldMap1;
  };
  var intercalateMap = function(dictFoldable1) {
    var foldMap11 = foldMap1(dictFoldable1);
    return function(dictSemigroup) {
      var foldMap12 = foldMap11(semigroupJoinWith(dictSemigroup));
      return function(j) {
        return function(f) {
          return function(foldable) {
            return joinee(foldMap12(function($171) {
              return JoinWith($$const(f($171)));
            })(foldable))(j);
          };
        };
      };
    };
  };

  // output/Data.Semigroup.Traversable/index.js
  var traverse1 = function(dict) {
    return dict.traverse1;
  };
  var sequence1 = function(dict) {
    return dict.sequence1;
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldr1 = function(dict) {
    return dict.unfoldr1;
  };
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };
  var replicate1 = function(dictUnfoldable1) {
    var unfoldr11 = unfoldr1(dictUnfoldable1);
    return function(n) {
      return function(v) {
        var step2 = function(i) {
          if (i <= 0) {
            return new Tuple(v, Nothing.value);
          }
          ;
          if (otherwise) {
            return new Tuple(v, new Just(i - 1 | 0));
          }
          ;
          throw new Error("Failed pattern match at Data.Unfoldable1 (line 68, column 5 - line 68, column 39): " + [i.constructor.name]);
        };
        return unfoldr11(step2)(n - 1 | 0);
      };
    };
  };
  var replicate1A = function(dictApply) {
    return function(dictUnfoldable1) {
      var replicate11 = replicate1(dictUnfoldable1);
      return function(dictTraversable1) {
        var sequence12 = sequence1(dictTraversable1)(dictApply);
        return function(n) {
          return function(m) {
            return sequence12(replicate11(n)(m));
          };
        };
      };
    };
  };

  // output/Data.Unfoldable/index.js
  var map5 = /* @__PURE__ */ map(functorMaybe);
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var replicate = function(dictUnfoldable) {
    var unfoldr12 = unfoldr(dictUnfoldable);
    return function(n) {
      return function(v) {
        var step2 = function(i) {
          var $17 = i <= 0;
          if ($17) {
            return Nothing.value;
          }
          ;
          return new Just(new Tuple(v, i - 1 | 0));
        };
        return unfoldr12(step2)(n);
      };
    };
  };
  var fromMaybe2 = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(b) {
      return map5(flip(Tuple.create)(Nothing.value))(b);
    });
  };

  // output/Data.Array/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var updateAt = /* @__PURE__ */ function() {
    return runFn5(_updateAt)(Just.create)(Nothing.value);
  }();
  var unsafeIndex = function() {
    return runFn2(unsafeIndexImpl);
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
  var toUnfoldable = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(xs) {
      var len = length(xs);
      var f = function(i) {
        if (i < len) {
          return new Just(new Tuple(unsafeIndex1(xs)(i), i + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 163, column 3 - line 165, column 26): " + [i.constructor.name]);
      };
      return unfoldr3(f)(0);
    };
  };
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
    };
  };
  var slice = /* @__PURE__ */ runFn3(sliceImpl);
  var take = function(n) {
    return function(xs) {
      var $152 = n < 1;
      if ($152) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var singleton2 = function(a) {
    return [a];
  };
  var replicate2 = /* @__PURE__ */ runFn2(replicateImpl);
  var range2 = /* @__PURE__ */ runFn2(rangeImpl);
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index = /* @__PURE__ */ function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  }();
  var last = function(xs) {
    return index(xs)(length(xs) - 1 | 0);
  };
  var modifyAt = function(i) {
    return function(f) {
      return function(xs) {
        var go = function(x) {
          return updateAt(i)(f(x))(xs);
        };
        return maybe(Nothing.value)(go)(index(xs)(i));
      };
    };
  };
  var head = function(xs) {
    return index(xs)(0);
  };
  var groupBy = function(op) {
    return function(xs) {
      return function __do2() {
        var result = newSTArray();
        var iter = iterator(function(v) {
          return index(xs)(v);
        })();
        iterate(iter)(function(x) {
          return $$void3(function __do3() {
            var sub1 = newSTArray();
            push(x)(sub1)();
            pushWhile(op(x))(iter)(sub1)();
            var grp = unsafeFreeze(sub1)();
            return push(grp)(result)();
          });
        })();
        return unsafeFreeze(result)();
      }();
    };
  };
  var fromFoldable = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var elemIndex = function(dictEq) {
    var eq23 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq23(v)(x);
      });
    };
  };
  var drop = function(n) {
    return function(xs) {
      var $173 = n < 1;
      if ($173) {
        return xs;
      }
      ;
      return slice(n)(length(xs))(xs);
    };
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $189 = maybe([])(singleton2);
      return function($190) {
        return $189(f($190));
      };
    }());
  };
  var catMaybes = /* @__PURE__ */ mapMaybe(/* @__PURE__ */ identity(categoryFn));

  // output/Data.NonEmpty/index.js
  var map6 = /* @__PURE__ */ map(functorTuple);
  var map1 = /* @__PURE__ */ map(functorMaybe);
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var unfoldable1NonEmpty = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return {
      unfoldr1: function(f) {
        return function(b) {
          return uncurry(NonEmpty.create)(map6(unfoldr3(map1(f)))(f(b)));
        };
      }
    };
  };
  var singleton3 = function(dictPlus) {
    var empty5 = empty(dictPlus);
    return function(a) {
      return new NonEmpty(a, empty5);
    };
  };
  var functorNonEmpty = function(dictFunctor) {
    var map25 = map(dictFunctor);
    return {
      map: function(f) {
        return function(m) {
          return new NonEmpty(f(m.value0), map25(f)(m.value1));
        };
      }
    };
  };
  var foldableNonEmpty = function(dictFoldable) {
    var foldMap3 = foldMap(dictFoldable);
    var foldl11 = foldl(dictFoldable);
    var foldr4 = foldr(dictFoldable);
    return {
      foldMap: function(dictMonoid) {
        var append15 = append(dictMonoid.Semigroup0());
        var foldMap12 = foldMap3(dictMonoid);
        return function(f) {
          return function(v) {
            return append15(f(v.value0))(foldMap12(f)(v.value1));
          };
        };
      },
      foldl: function(f) {
        return function(b) {
          return function(v) {
            return foldl11(f)(f(b)(v.value0))(v.value1);
          };
        };
      },
      foldr: function(f) {
        return function(b) {
          return function(v) {
            return f(v.value0)(foldr4(f)(b)(v.value1));
          };
        };
      }
    };
  };
  var traversableNonEmpty = function(dictTraversable) {
    var sequence3 = sequence(dictTraversable);
    var traverse3 = traverse(dictTraversable);
    var functorNonEmpty1 = functorNonEmpty(dictTraversable.Functor0());
    var foldableNonEmpty1 = foldableNonEmpty(dictTraversable.Foldable1());
    return {
      sequence: function(dictApplicative) {
        var Apply0 = dictApplicative.Apply0();
        var apply5 = apply(Apply0);
        var map25 = map(Apply0.Functor0());
        var sequence12 = sequence3(dictApplicative);
        return function(v) {
          return apply5(map25(NonEmpty.create)(v.value0))(sequence12(v.value1));
        };
      },
      traverse: function(dictApplicative) {
        var Apply0 = dictApplicative.Apply0();
        var apply5 = apply(Apply0);
        var map25 = map(Apply0.Functor0());
        var traverse12 = traverse3(dictApplicative);
        return function(f) {
          return function(v) {
            return apply5(map25(NonEmpty.create)(f(v.value0)))(traverse12(f)(v.value1));
          };
        };
      },
      Functor0: function() {
        return functorNonEmpty1;
      },
      Foldable1: function() {
        return foldableNonEmpty1;
      }
    };
  };
  var foldable1NonEmpty = function(dictFoldable) {
    var foldl11 = foldl(dictFoldable);
    var foldr4 = foldr(dictFoldable);
    var foldableNonEmpty1 = foldableNonEmpty(dictFoldable);
    return {
      foldMap1: function(dictSemigroup) {
        var append15 = append(dictSemigroup);
        return function(f) {
          return function(v) {
            return foldl11(function(s) {
              return function(a1) {
                return append15(s)(f(a1));
              };
            })(f(v.value0))(v.value1);
          };
        };
      },
      foldr1: function(f) {
        return function(v) {
          return maybe(v.value0)(f(v.value0))(foldr4(function(a1) {
            var $250 = maybe(a1)(f(a1));
            return function($251) {
              return Just.create($250($251));
            };
          })(Nothing.value)(v.value1));
        };
      },
      foldl1: function(f) {
        return function(v) {
          return foldl11(f)(v.value0)(v.value1);
        };
      },
      Foldable0: function() {
        return foldableNonEmpty1;
      }
    };
  };

  // output/Data.List.Types/index.js
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var nelCons = function(a) {
    return function(v) {
      return new NonEmpty(a, new Cons(v.value0, v.value1));
    };
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map7 = /* @__PURE__ */ map(functorList);
  var functorNonEmptyList = /* @__PURE__ */ functorNonEmpty(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev = function() {
          var go = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b);
        return function($285) {
          return $284(rev($285));
        };
      };
    },
    foldl: function(f) {
      var go = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty7 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty7);
      };
    }
  };
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var foldableNonEmptyList = /* @__PURE__ */ foldableNonEmpty(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var traversableList = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      var map112 = map(Apply0.Functor0());
      var lift22 = lift2(Apply0);
      var pure12 = pure(dictApplicative);
      return function(f) {
        var $301 = map112(foldl3(flip(Cons.create))(Nil.value));
        var $302 = foldl3(function(acc) {
          var $304 = lift22(flip(Cons.create))(acc);
          return function($305) {
            return $304(f($305));
          };
        })(pure12(Nil.value));
        return function($303) {
          return $301($302($303));
        };
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableList)(dictApplicative)(identity9);
    },
    Functor0: function() {
      return functorList;
    },
    Foldable1: function() {
      return foldableList;
    }
  };
  var traversableNonEmptyList = /* @__PURE__ */ traversableNonEmpty(traversableList);
  var unfoldable1List = {
    unfoldr1: function(f) {
      return function(b) {
        var go = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source3, memo) {
              var v = f(source3);
              if (v.value1 instanceof Just) {
                $tco_var_source = v.value1.value0;
                $copy_memo = new Cons(v.value0, memo);
                return;
              }
              ;
              if (v.value1 instanceof Nothing) {
                $tco_done = true;
                return foldl3(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go(b)(Nil.value);
      };
    }
  };
  var unfoldableList = {
    unfoldr: function(f) {
      return function(b) {
        var go = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source3, memo) {
              var v = f(source3);
              if (v instanceof Nothing) {
                $tco_done = true;
                return foldl3(flip(Cons.create))(Nil.value)(memo);
              }
              ;
              if (v instanceof Just) {
                $tco_var_source = v.value0.value1;
                $copy_memo = new Cons(v.value0.value0, memo);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go(b)(Nil.value);
      };
    },
    Unfoldable10: function() {
      return unfoldable1List;
    }
  };
  var unfoldable1NonEmptyList = /* @__PURE__ */ unfoldable1NonEmpty(unfoldableList);
  var foldable1NonEmptyList = /* @__PURE__ */ foldable1NonEmpty(foldableList);
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map7(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var apply2 = /* @__PURE__ */ apply(applyList);
  var applyNonEmptyList = {
    apply: function(v) {
      return function(v1) {
        return new NonEmpty(v.value0(v1.value0), append1(apply2(v.value1)(new Cons(v1.value0, Nil.value)))(apply2(new Cons(v.value0, v.value1))(v1.value1)));
      };
    },
    Functor0: function() {
      return functorNonEmptyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();
  var applicativeNonEmptyList = {
    pure: /* @__PURE__ */ function() {
      var $315 = singleton3(plusList);
      return function($316) {
        return NonEmptyList($315($316));
      };
    }(),
    Apply0: function() {
      return applyNonEmptyList;
    }
  };
  var pure2 = /* @__PURE__ */ pure(applicativeNonEmptyList);
  var traversable1NonEmptyList = {
    traverse1: function(dictApply) {
      var Functor0 = dictApply.Functor0();
      var mapFlipped2 = mapFlipped(Functor0);
      var lift22 = lift2(dictApply);
      var map112 = map(Functor0);
      return function(f) {
        return function(v) {
          return mapFlipped2(foldl3(function(acc) {
            var $317 = lift22(flip(nelCons))(acc);
            return function($318) {
              return $317(f($318));
            };
          })(map112(pure2)(f(v.value0)))(v.value1))(function(v1) {
            return foldl3(flip(nelCons))(pure2(v1.value0))(v1.value1);
          });
        };
      };
    },
    sequence1: function(dictApply) {
      return traverse1(traversable1NonEmptyList)(dictApply)(identity9);
    },
    Foldable10: function() {
      return foldable1NonEmptyList;
    },
    Traversable1: function() {
      return traversableNonEmptyList;
    }
  };

  // output/Data.Lens.Fold/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var foldMapOf = /* @__PURE__ */ under()()(Forget);
  var lastOf = function(p) {
    var $130 = foldMapOf(p)(function($132) {
      return Last(Just.create($132));
    });
    return function($131) {
      return unwrap2($130($131));
    };
  };
  var firstOf = function(p) {
    var $145 = foldMapOf(p)(function($147) {
      return First(Just.create($147));
    });
    return function($146) {
      return unwrap2($145($146));
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var put = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(s) {
      return state1(function(v) {
        return new Tuple(unit, s);
      });
    };
  };
  var get2 = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Control.Monad.State.Trans/index.js
  var functorStateT = function(dictFunctor) {
    var map25 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map25(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var evalStateT = function(dictFunctor) {
    var map25 = map(dictFunctor);
    return function(v) {
      return function(s) {
        return map25(fst)(v(s));
      };
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind8 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind8(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure12 = pure(dictMonad.Applicative0());
    return {
      pure: function(a) {
        return function(s) {
          return pure12(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var monadStateStateT = function(dictMonad) {
    var pure12 = pure(dictMonad.Applicative0());
    var monadStateT1 = monadStateT(dictMonad);
    return {
      state: function(f) {
        return function($200) {
          return pure12(f($200));
        };
      },
      Monad0: function() {
        return monadStateT1;
      }
    };
  };

  // output/Control.Monad.State/index.js
  var evalState = function(v) {
    return function(s) {
      var v1 = v(s);
      return v1.value0;
    };
  };

  // output/Data.Lens.Traversal/index.js
  var traversed = function(dictTraversable) {
    var traverse3 = traverse(dictTraversable);
    return function(dictWander) {
      return wander(dictWander)(function(dictApplicative) {
        return traverse3(dictApplicative);
      });
    };
  };

  // output/Data.Ratio/index.js
  var Ratio = /* @__PURE__ */ function() {
    function Ratio2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ratio2.create = function(value0) {
      return function(value1) {
        return new Ratio2(value0, value1);
      };
    };
    return Ratio2;
  }();
  var reduce = function(dictOrd) {
    var gcd2 = gcd(dictOrd.Eq0());
    var signum2 = signum(dictOrd);
    var abs3 = abs(dictOrd);
    return function(dictEuclideanRing) {
      var gcd1 = gcd2(dictEuclideanRing);
      var div4 = div(dictEuclideanRing);
      var Ring0 = dictEuclideanRing.CommutativeRing0().Ring0();
      var mul7 = mul(Ring0.Semiring0());
      var signum1 = signum2(Ring0);
      var abs1 = abs3(Ring0);
      return function(n) {
        return function(d) {
          var g = gcd1(n)(d);
          var d$prime = div4(d)(g);
          return new Ratio(mul7(div4(n)(g))(signum1(d$prime)), abs1(d$prime));
        };
      };
    };
  };
  var semiringRatio = function(dictOrd) {
    var reduce1 = reduce(dictOrd);
    return function(dictEuclideanRing) {
      var Semiring0 = dictEuclideanRing.CommutativeRing0().Ring0().Semiring0();
      var one2 = one(Semiring0);
      var reduce22 = reduce1(dictEuclideanRing);
      var mul7 = mul(Semiring0);
      var add3 = add(Semiring0);
      return {
        one: new Ratio(one2, one2),
        mul: function(v) {
          return function(v1) {
            return reduce22(mul7(v.value0)(v1.value0))(mul7(v.value1)(v1.value1));
          };
        },
        zero: new Ratio(zero(Semiring0), one2),
        add: function(v) {
          return function(v1) {
            return reduce22(add3(mul7(v.value0)(v1.value1))(mul7(v.value1)(v1.value0)))(mul7(v.value1)(v1.value1));
          };
        }
      };
    };
  };
  var ringRatio = function(dictOrd) {
    var reduce1 = reduce(dictOrd);
    var semiringRatio1 = semiringRatio(dictOrd);
    return function(dictEuclideanRing) {
      var reduce22 = reduce1(dictEuclideanRing);
      var Ring0 = dictEuclideanRing.CommutativeRing0().Ring0();
      var sub3 = sub(Ring0);
      var mul7 = mul(Ring0.Semiring0());
      var semiringRatio2 = semiringRatio1(dictEuclideanRing);
      return {
        sub: function(v) {
          return function(v1) {
            return reduce22(sub3(mul7(v.value0)(v1.value1))(mul7(v.value1)(v1.value0)))(mul7(v.value1)(v1.value1));
          };
        },
        Semiring0: function() {
          return semiringRatio2;
        }
      };
    };
  };
  var numerator = function(v) {
    return v.value0;
  };
  var eqRatio = function(dictEq) {
    var eq8 = eq(dictEq);
    return {
      eq: function(v) {
        return function(v1) {
          return eq8(v.value0)(v1.value0) && eq8(v.value1)(v1.value1);
        };
      }
    };
  };
  var ordRatio = function(dictOrd) {
    var ringRatio1 = ringRatio(dictOrd);
    var Eq0 = dictOrd.Eq0();
    var eq8 = eq(Eq0);
    var greaterThan3 = greaterThan(dictOrd);
    var eqRatio1 = eqRatio(Eq0);
    return function(dictEuclideanRing) {
      var sub3 = sub(ringRatio1(dictEuclideanRing));
      var zero2 = zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0());
      return {
        compare: function(x) {
          return function(y) {
            var v = sub3(x)(y);
            var $130 = eq8(v.value0)(zero2);
            if ($130) {
              return EQ.value;
            }
            ;
            var v1 = greaterThan3(v.value1)(zero2);
            var v2 = greaterThan3(v.value0)(zero2);
            if (v2 && v1) {
              return GT.value;
            }
            ;
            if (!v2 && !v1) {
              return GT.value;
            }
            ;
            return LT.value;
          };
        },
        Eq0: function() {
          return eqRatio1;
        }
      };
    };
  };
  var denominator = function(v) {
    return v.value1;
  };
  var commutativeRingRatio = function(dictOrd) {
    var ringRatio1 = ringRatio(dictOrd);
    return function(dictEuclideanRing) {
      var ringRatio2 = ringRatio1(dictEuclideanRing);
      return {
        Ring0: function() {
          return ringRatio2;
        }
      };
    };
  };
  var euclideanRingRatio = function(dictOrd) {
    var reduce1 = reduce(dictOrd);
    var semiringRatio1 = semiringRatio(dictOrd);
    var commutativeRingRatio1 = commutativeRingRatio(dictOrd);
    return function(dictEuclideanRing) {
      var reduce22 = reduce1(dictEuclideanRing);
      var mul7 = mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0());
      var zero2 = zero(semiringRatio1(dictEuclideanRing));
      var commutativeRingRatio2 = commutativeRingRatio1(dictEuclideanRing);
      return {
        degree: function(v) {
          return 1;
        },
        div: function(v) {
          return function(v1) {
            return reduce22(mul7(v.value0)(v1.value1))(mul7(v.value1)(v1.value0));
          };
        },
        mod: function(v) {
          return function(v1) {
            return zero2;
          };
        },
        CommutativeRing0: function() {
          return commutativeRingRatio2;
        }
      };
    };
  };

  // output/JS.BigInt/foreign.js
  var fromInt = (n) => BigInt(n);
  var toNumber = (n) => Number(n);
  var biAdd = (x) => (y) => x + y;
  var biMul = (x) => (y) => x * y;
  var biSub = (x) => (y) => x - y;
  var biMod = (x) => (y) => {
    if (y === 0n)
      return 0n;
    const yy = y < 0n ? -y : y;
    return (x % yy + yy) % yy;
  };
  var biDiv = (x) => (y) => {
    if (y === 0n)
      return 0n;
    return (x - biMod(x)(y)) / y;
  };
  var biDegree = (x) => {
    const xx = x < 0n ? -x : x;
    return BigInt.asIntN(32, xx > 2147483647n ? 2147483647n : xx);
  };
  var biZero = 0n;
  var biOne = 1n;
  var biEquals = (x) => (y) => x == y;
  var biCompare = (x) => (y) => {
    if (x === y)
      return 0;
    else if (x > y)
      return 1;
    else
      return -1;
  };
  var toString = (x) => x.toString();

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber2 = function(n) {
    return n;
  };
  var fromStringAsImpl = function(just) {
    return function(nothing) {
      return function(radix) {
        var digits;
        if (radix < 11) {
          digits = "[0-" + (radix - 1).toString() + "]";
        } else if (radix === 11) {
          digits = "[0-9a]";
        } else {
          digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
        }
        var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
        return function(s) {
          if (pattern.test(s)) {
            var i = parseInt(s, radix);
            return (i | 0) === i ? just(i) : nothing;
          } else {
            return nothing;
          }
        };
      };
    };
  };
  var pow = function(x) {
    return function(y) {
      return Math.pow(x, y) | 0;
    };
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var round = Math.round;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromStringAs = /* @__PURE__ */ function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  }();
  var fromString = /* @__PURE__ */ fromStringAs(10);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber2(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber2(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var round2 = function($37) {
    return unsafeClamp(round($37));
  };

  // output/JS.BigInt/index.js
  var showBigInt = {
    show: toString
  };
  var semiringBigInt = {
    add: biAdd,
    zero: biZero,
    mul: biMul,
    one: biOne
  };
  var ringBigInt = {
    sub: biSub,
    Semiring0: function() {
      return semiringBigInt;
    }
  };
  var eqBigInt = {
    eq: biEquals
  };
  var ordBigInt = {
    compare: function(x) {
      return function(y) {
        var v = biCompare(x)(y);
        if (v === 1) {
          return GT.value;
        }
        ;
        if (v === 0) {
          return EQ.value;
        }
        ;
        return LT.value;
      };
    },
    Eq0: function() {
      return eqBigInt;
    }
  };
  var commutativeRingBigInt = {
    Ring0: function() {
      return ringBigInt;
    }
  };
  var euclideanRingBigInt = {
    degree: biDegree,
    div: biDiv,
    mod: biMod,
    CommutativeRing0: function() {
      return commutativeRingBigInt;
    }
  };
  var toInt = function($19) {
    return fromNumber(toNumber($19));
  };

  // output/Data.Rational/index.js
  var reduce2 = /* @__PURE__ */ reduce(ordBigInt)(euclideanRingBigInt);
  var toRationalInt = {
    toRational: function(a) {
      return function(b) {
        return reduce2(fromInt(a))(fromInt(b));
      };
    }
  };
  var semiringRational = /* @__PURE__ */ semiringRatio(ordBigInt)(euclideanRingBigInt);
  var ringRational = /* @__PURE__ */ ringRatio(ordBigInt)(euclideanRingBigInt);
  var ordRational = /* @__PURE__ */ ordRatio(ordBigInt)(euclideanRingBigInt);
  var euclideanRingRational = /* @__PURE__ */ euclideanRingRatio(ordBigInt)(euclideanRingBigInt);
  var eqRational = /* @__PURE__ */ eqRatio(eqBigInt);
  var toRational = function(dict) {
    return dict.toRational;
  };
  var toNumber3 = function(v) {
    return toNumber(numerator(v)) / toNumber(denominator(v));
  };
  var numerator2 = function(v) {
    return numerator(v);
  };
  var fromInt2 = function(i) {
    return reduce2(fromInt(i))(fromInt(1));
  };
  var denominator2 = function(v) {
    return denominator(v);
  };

  // output/Data.Abc.Meter/index.js
  var join2 = /* @__PURE__ */ join(bindMaybe);
  var _headers2 = /* @__PURE__ */ _headers(strongForget);
  var traversed2 = /* @__PURE__ */ traversed(traversableList)(/* @__PURE__ */ wanderForget(monoidFirst));
  var _Meter2 = /* @__PURE__ */ _Meter(/* @__PURE__ */ choiceForget(monoidFirst));
  var getMeter = function(tune) {
    return join2(firstOf(function($9) {
      return _headers2(traversed2(_Meter2($9)));
    })(tune));
  };
  var cutTime = {
    numerator: 2,
    denominator: 2
  };
  var commonTime = {
    numerator: 4,
    denominator: 4
  };
  var getDefaultedMeter = function(tune) {
    return fromMaybe(commonTime)(getMeter(tune));
  };

  // output/Data.List/index.js
  var map8 = /* @__PURE__ */ map(functorMaybe);
  var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
  var foldl4 = /* @__PURE__ */ foldl(foldableList);
  var uncons = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable2 = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map8(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons(xs));
    });
  };
  var reverse2 = /* @__PURE__ */ function() {
    var go = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go(Nil.value);
  }();
  var $$null2 = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var manyRec = function(dictMonadRec) {
    var bind1 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt5 = alt(Alt0);
      var map112 = map(Alt0.Functor0());
      var pure12 = pure(dictAlternative.Applicative0());
      return function(p) {
        var go = function(acc) {
          return bind1(alt5(map112(Loop.create)(p))(pure12(new Done(unit))))(function(aa) {
            return pure12(bimap2(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse2(acc);
            })(aa));
          });
        };
        return tailRecM5(go)(Nil.value);
      };
    };
  };
  var length2 = /* @__PURE__ */ foldl4(function(acc) {
    return function(v) {
      return acc + 1 | 0;
    };
  })(0);
  var filter2 = function(p) {
    var go = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return reverse2(v);
          }
          ;
          if (v1 instanceof Cons) {
            if (p(v1.value0)) {
              $tco_var_v = new Cons(v1.value0, v);
              $copy_v1 = v1.value1;
              return;
            }
            ;
            if (otherwise) {
              $tco_var_v = v;
              $copy_v1 = v1.value1;
              return;
            }
            ;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 390, column 3 - line 390, column 27): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go(Nil.value);
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.List.NonEmpty/index.js
  var map9 = /* @__PURE__ */ map(functorMaybe);
  var wrappedOperation = function(name2) {
    return function(f) {
      return function(v) {
        var v1 = f(new Cons(v.value0, v.value1));
        if (v1 instanceof Cons) {
          return new NonEmpty(v1.value0, v1.value1);
        }
        ;
        if (v1 instanceof Nil) {
          return unsafeCrashWith("Impossible: empty list in NonEmptyList " + name2);
        }
        ;
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 92, column 3 - line 94, column 81): " + [v1.constructor.name]);
      };
    };
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var toUnfoldable3 = function(dictUnfoldable) {
    var $196 = unfoldr(dictUnfoldable)(function(xs) {
      return map9(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons(xs));
    });
    return function($197) {
      return $196(toList($197));
    };
  };
  var singleton4 = /* @__PURE__ */ function() {
    var $200 = singleton3(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var reverse3 = /* @__PURE__ */ wrappedOperation("reverse")(reverse2);
  var length3 = function(v) {
    return 1 + length2(v.value1) | 0;
  };
  var head2 = function(v) {
    return v.value0;
  };
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Two2(value0, value1, value2, value3);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return new Three2(value0, value1, value2, value3, value4, value5, value6);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoLeft2(value0, value1, value2);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoRight2(value0, value1, value2);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeLeft2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeMiddle2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeRight2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new KickUp2(value0, value1, value2, value3);
          };
        };
      };
    };
    return KickUp2;
  }();
  var singleton5 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var toUnfoldable4 = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(m) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof Leaf) {
              $copy_v = v.value1;
              return;
            }
            ;
            if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), v.value1));
            }
            ;
            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), new Cons(v.value0.value3, v.value1)));
            }
            ;
            if (v.value0 instanceof Two) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton5(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, v.value1)));
              return;
            }
            ;
            if (v.value0 instanceof Three) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton5(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, new Cons(singleton5(v.value0.value4)(v.value0.value5), new Cons(v.value0.value6, v.value1)))));
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 624, column 18 - line 633, column 71): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 623, column 3 - line 623, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return unfoldr3(go)(new Cons(m, Nil.value));
    };
  };
  var lookup = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare3(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare3(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare3(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare3(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare3(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max4 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max4.key, max4.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = compare3(k)(m.value4);
              var v3 = compare3(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max4 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max4.key, max4.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max4 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max4.key, max4.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable3 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var $$delete = function(dictOrd) {
    var pop1 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop1(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup1 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert1 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup1(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert1(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };
  var update = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          return alter1(maybe(Nothing.value)(f))(k)(m);
        };
      };
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i = 0; i < n; ++i) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var succ = function(dict) {
    return dict.succ;
  };
  var pred = function(dict) {
    return dict.pred;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low) {
      return function(high) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom22);
            if ($140) {
              return low;
            }
            ;
            return high;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a) {
    return a.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton6 = function(c) {
    return c;
  };
  var _charAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(s) {
          return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
        };
      };
    };
  };
  var length4 = function(s) {
    return s.length;
  };
  var drop3 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var charAt2 = /* @__PURE__ */ function() {
    return _charAt(Just.create)(Nothing.value);
  }();

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy2 = function(name2, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map10 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons2 = function(s) {
    var v = length4(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop3(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop3(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map10(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons2(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length4(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length5 = function($74) {
    return length(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons2(v1);
      if (v2 instanceof Just) {
        return singleton7(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take4 = /* @__PURE__ */ _take(takeFallback);
  var splitAt2 = function(i) {
    return function(s) {
      var before = take4(i)(s);
      return {
        before,
        after: drop3(length4(before))(s)
      };
    };
  };
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var drop4 = function(n) {
    return function(s) {
      return drop3(length4(take4(n)(s)))(s);
    };
  };
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum2($77));
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy2("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Data.String.Utils/foreign.js
  function endsWithImpl(searchString, s) {
    return s.endsWith(searchString);
  }
  function includesImpl(searchString, str) {
    return str.includes(searchString);
  }
  function startsWithImpl(searchString, s) {
    return s.startsWith(searchString);
  }

  // output/Data.String.Regex/foreign.js
  var regexImpl = function(left2) {
    return function(right2) {
      return function(s1) {
        return function(s2) {
          try {
            return right2(new RegExp(s1, s2));
          } catch (e) {
            return left2(e.message);
          }
        };
      };
    };
  };
  var _match = function(just) {
    return function(nothing) {
      return function(r) {
        return function(s) {
          var m = s.match(r);
          if (m == null || m.length === 0) {
            return nothing;
          } else {
            for (var i = 0; i < m.length; i++) {
              m[i] = m[i] == null ? nothing : just(m[i]);
            }
            return just(m);
          }
        };
      };
    };
  };

  // output/Data.String.Regex.Flags/index.js
  var noFlags = {
    global: false,
    ignoreCase: false,
    multiline: false,
    dotAll: false,
    sticky: false,
    unicode: false
  };

  // output/Data.String.Regex/index.js
  var renderFlags = function(v) {
    return function() {
      if (v.global) {
        return "g";
      }
      ;
      return "";
    }() + (function() {
      if (v.ignoreCase) {
        return "i";
      }
      ;
      return "";
    }() + (function() {
      if (v.multiline) {
        return "m";
      }
      ;
      return "";
    }() + (function() {
      if (v.dotAll) {
        return "s";
      }
      ;
      return "";
    }() + (function() {
      if (v.sticky) {
        return "y";
      }
      ;
      return "";
    }() + function() {
      if (v.unicode) {
        return "u";
      }
      ;
      return "";
    }()))));
  };
  var regex = function(s) {
    return function(f) {
      return regexImpl(Left.create)(Right.create)(s)(renderFlags(f));
    };
  };
  var match = /* @__PURE__ */ function() {
    return _match(Just.create)(Nothing.value);
  }();

  // output/Data.String.Utils/index.js
  var startsWith = function(searchString) {
    return function(s) {
      return startsWithImpl(searchString, s);
    };
  };
  var includes = function(searchString) {
    return function(s) {
      return includesImpl(searchString, s);
    };
  };
  var endsWith = function(searchString) {
    return function(s) {
      return endsWithImpl(searchString, s);
    };
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    };
    function finalCell(head6) {
      return new ConsCell(head6, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply5, map25, f) {
      var buildFrom = function(x, ys) {
        return apply5(map25(consList)(f(x)))(ys);
      };
      var go = function(acc, currentLen, xs) {
        if (currentLen === 0) {
          return acc;
        } else {
          var last4 = xs[currentLen - 1];
          return new Cont(function() {
            var built = go(buildFrom(last4, acc), currentLen - 1, xs);
            return built;
          });
        }
      };
      return function(array) {
        var acc = map25(finalCell)(f(array[array.length - 1]));
        var result = go(acc, array.length - 1, array);
        while (result instanceof Cont) {
          result = result.fn();
        }
        return map25(listToArray)(result);
      };
    };
  }();

  // output/Data.Array.NonEmpty.Internal/index.js
  var foldableNonEmptyArray = foldableArray;

  // output/Data.Array.NonEmpty/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var toArray = function(v) {
    return v;
  };
  var adaptMaybe = function(f) {
    return function($126) {
      return fromJust4(f(toArray($126)));
    };
  };
  var head4 = /* @__PURE__ */ adaptMaybe(head);
  var last3 = /* @__PURE__ */ adaptMaybe(last);
  var adaptAny = function(f) {
    return function($128) {
      return f(toArray($128));
    };
  };
  var length6 = /* @__PURE__ */ adaptAny(length);

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

  // output/StringParser.Parser/index.js
  var map11 = /* @__PURE__ */ map(functorEither);
  var bind2 = /* @__PURE__ */ bind(bindEither);
  var pure3 = /* @__PURE__ */ pure(applicativeEither);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEither);
  var unParser = function(v) {
    return v;
  };
  var runParser = function(v) {
    return function(s) {
      return map11(function(v1) {
        return v1.result;
      })(v({
        substring: s,
        position: 0
      }));
    };
  };
  var functorParser = {
    map: function(f) {
      return function(v) {
        var $69 = map11(function(v1) {
          return {
            result: f(v1.result),
            suffix: v1.suffix
          };
        });
        return function($70) {
          return $69(v($70));
        };
      };
    }
  };
  var fail = function(error2) {
    return function(v) {
      return new Left({
        pos: v.position,
        error: error2
      });
    };
  };
  var applyParser = {
    apply: function(v) {
      return function(v1) {
        return function(s) {
          return bind2(v(s))(function(v2) {
            return bind2(v1(v2.suffix))(function(v3) {
              return pure3({
                result: v2.result(v3.result),
                suffix: v3.suffix
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParser;
    }
  };
  var bindParser = {
    bind: function(v) {
      return function(f) {
        return function(s) {
          return bind2(v(s))(function(v1) {
            return unParser(f(v1.result))(v1.suffix);
          });
        };
      };
    },
    Apply0: function() {
      return applyParser;
    }
  };
  var applicativeParser = {
    pure: function(a) {
      return function(s) {
        return new Right({
          result: a,
          suffix: s
        });
      };
    },
    Apply0: function() {
      return applyParser;
    }
  };
  var monadParser = {
    Applicative0: function() {
      return applicativeParser;
    },
    Bind1: function() {
      return bindParser;
    }
  };
  var monadRecParser = {
    tailRecM: function(f) {
      return function(a) {
        var split3 = function(v) {
          if (v.result instanceof Loop) {
            return new Loop({
              state: v.result.value0,
              str: v.suffix
            });
          }
          ;
          if (v.result instanceof Done) {
            return new Done({
              result: v.result.value0,
              suffix: v.suffix
            });
          }
          ;
          throw new Error("Failed pattern match at StringParser.Parser (line 87, column 5 - line 87, column 68): " + [v.constructor.name]);
        };
        return function(str) {
          return tailRecM3(function(st) {
            return map11(split3)(unParser(f(st.state))(st.str));
          })({
            state: a,
            str
          });
        };
      };
    },
    Monad0: function() {
      return monadParser;
    }
  };
  var altParser = {
    alt: function(v) {
      return function(v1) {
        return function(s) {
          var v2 = v(s);
          if (v2 instanceof Left) {
            if (s.position === v2.value0.pos) {
              return v1(s);
            }
            ;
            if (otherwise) {
              return new Left({
                error: v2.value0.error,
                pos: v2.value0.pos
              });
            }
            ;
          }
          ;
          return v2;
        };
      };
    },
    Functor0: function() {
      return functorParser;
    }
  };
  var plusParser = {
    empty: /* @__PURE__ */ fail("No alternative"),
    Alt0: function() {
      return altParser;
    }
  };
  var alternativeParser = {
    Applicative0: function() {
      return applicativeParser;
    },
    Plus1: function() {
      return plusParser;
    }
  };

  // output/StringParser.Combinators/index.js
  var alt2 = /* @__PURE__ */ alt(altParser);
  var bind3 = /* @__PURE__ */ bind(bindParser);
  var pure4 = /* @__PURE__ */ pure(applicativeParser);
  var map12 = /* @__PURE__ */ map(functorParser);
  var applyFirst2 = /* @__PURE__ */ applyFirst(applyParser);
  var applySecond2 = /* @__PURE__ */ applySecond(applyParser);
  var apply3 = /* @__PURE__ */ apply(applyParser);
  var tailRecM4 = /* @__PURE__ */ tailRecM(monadRecParser);
  var pure1 = /* @__PURE__ */ pure(applicativeNonEmptyList);
  var withError = function(p) {
    return function(msg) {
      return alt2(p)(fail(msg));
    };
  };
  var $$try = function(v) {
    return function(s) {
      var v1 = v(s);
      if (v1 instanceof Left) {
        return new Left({
          pos: s.position,
          error: v1.value0.error
        });
      }
      ;
      return v1;
    };
  };
  var optional = function(p) {
    return alt2(bind3(p)(function(v) {
      return pure4(unit);
    }))(pure4(unit));
  };
  var option = function(a) {
    return function(p) {
      return alt2(p)(pure4(a));
    };
  };
  var optionMaybe = function(p) {
    return option(Nothing.value)(map12(Just.create)(p));
  };
  var cons$prime = function(h) {
    return function(t) {
      return new NonEmpty(h, t);
    };
  };
  var choice = function(dictFoldable) {
    return foldl(dictFoldable)(alt2)(fail("Nothing to parse"));
  };
  var between = function(open) {
    return function(close) {
      return function(p) {
        return applyFirst2(applySecond2(open)(p))(close);
      };
    };
  };
  var assertConsume = function(v) {
    return function(s) {
      var v1 = v(s);
      if (v1 instanceof Right) {
        var $34 = s.position < v1.value0.suffix.position;
        if ($34) {
          return new Right(v1.value0);
        }
        ;
        return new Left({
          pos: s.position,
          error: "Consumed no input."
        });
      }
      ;
      return v1;
    };
  };
  var many = /* @__PURE__ */ function() {
    var $37 = manyRec(monadRecParser)(alternativeParser);
    return function($38) {
      return $37(assertConsume($38));
    };
  }();
  var many1 = function(p) {
    return apply3(map12(cons$prime)(p))(many(p));
  };
  var sepBy1 = function(p) {
    return function(sep) {
      return bind3(p)(function(a) {
        return bind3(many(applySecond2(sep)(p)))(function(as) {
          return pure4(cons$prime(a)(as));
        });
      });
    };
  };
  var sepBy = function(p) {
    return function(sep) {
      return alt2(map12(toList)(sepBy1(p)(sep)))(pure4(Nil.value));
    };
  };
  var many1Till = function(p) {
    return function(end) {
      var ending = function(acc) {
        return bind3(end)(function() {
          return pure4(new Done(reverse3(acc)));
        });
      };
      var $$continue = function(acc) {
        return bind3(assertConsume(p))(function(c) {
          return pure4(new Loop(cons2(c)(acc)));
        });
      };
      var inner = function(acc) {
        return alt2(ending(acc))($$continue(acc));
      };
      return bind3(p)(function(x) {
        return tailRecM4(inner)(pure1(x));
      });
    };
  };
  var manyTill = function(p) {
    return function(end) {
      return alt2(applySecond2(end)(pure4(Nil.value)))(map12(toList)(many1Till(p)(end)));
    };
  };

  // output/StringParser.CodeUnits/index.js
  var anyChar = function(v) {
    var v1 = charAt2(0)(v.substring);
    if (v1 instanceof Just) {
      return new Right({
        result: v1.value0,
        suffix: {
          substring: drop3(1)(v.substring),
          position: v.position + 1 | 0
        }
      });
    }
    ;
    if (v1 instanceof Nothing) {
      return new Left({
        pos: v.position,
        error: "Unexpected EOF"
      });
    }
    ;
    throw new Error("Failed pattern match at StringParser.CodeUnits (line 50, column 3 - line 52, column 63): " + [v1.constructor.name]);
  };

  // output/StringParser.CodePoints/index.js
  var bind4 = /* @__PURE__ */ bind(bindParser);
  var elem3 = /* @__PURE__ */ elem(foldableArray)(eqInt);
  var pure5 = /* @__PURE__ */ pure(applicativeParser);
  var show3 = /* @__PURE__ */ show(showChar);
  var map13 = /* @__PURE__ */ map(functorMaybe);
  var alt3 = /* @__PURE__ */ alt(altParser);
  var show22 = /* @__PURE__ */ show(showInt);
  var upperCaseChar = /* @__PURE__ */ $$try(/* @__PURE__ */ bind4(anyChar)(function(c) {
    var $42 = elem3(toCharCode2(c))(range2(65)(90));
    if ($42) {
      return pure5(c);
    }
    ;
    return fail("Expected an upper case character but found " + show3(c));
  }));
  var string = function(pattern) {
    return function(v) {
      var length7 = length5(pattern);
      var v1 = splitAt2(length7)(v.substring);
      var $45 = v1.before === pattern;
      if ($45) {
        return new Right({
          result: pattern,
          suffix: {
            substring: v1.after,
            position: v.position + length7 | 0
          }
        });
      }
      ;
      return new Left({
        pos: v.position,
        error: "Expected '" + (pattern + "'.")
      });
    };
  };
  var regex2 = function(pat) {
    var pattern = "^(" + (pat + ")");
    var matchRegex = function(r) {
      return function(v2) {
        var v1 = map13(head4)(match(r)(v2.substring));
        if (v1 instanceof Just && v1.value0 instanceof Just) {
          return new Right({
            result: v1.value0.value0,
            suffix: {
              substring: drop4(length5(v1.value0.value0))(v2.substring),
              position: v2.position + length5(v1.value0.value0) | 0
            }
          });
        }
        ;
        return new Left({
          pos: v2.position,
          error: "no match"
        });
      };
    };
    var v = regex(pattern)(noFlags);
    if (v instanceof Left) {
      return fail("StringParser.String.regex': illegal regex " + pat);
    }
    ;
    if (v instanceof Right) {
      return matchRegex(v.value0);
    }
    ;
    throw new Error("Failed pattern match at StringParser.CodePoints (line 158, column 3 - line 162, column 19): " + [v.constructor.name]);
  };
  var lowerCaseChar = /* @__PURE__ */ $$try(/* @__PURE__ */ bind4(anyChar)(function(c) {
    var $59 = elem3(toCharCode2(c))(range2(97)(122));
    if ($59) {
      return pure5(c);
    }
    ;
    return fail("Expected a lower case character but found " + show3(c));
  }));
  var eof = function(s) {
    if (0 < length5(s.substring)) {
      return new Left({
        pos: s.position,
        error: "Expected EOF"
      });
    }
    ;
    return new Right({
      result: unit,
      suffix: s
    });
  };
  var anyLetter = /* @__PURE__ */ alt3(lowerCaseChar)(/* @__PURE__ */ withError(upperCaseChar)("Expected a letter"));
  var anyDigit = /* @__PURE__ */ $$try(/* @__PURE__ */ bind4(anyChar)(function(c) {
    var $63 = c >= "0" && c <= "9";
    if ($63) {
      return pure5(c);
    }
    ;
    return fail("Character " + (show3(c) + " is not a digit"));
  }));
  var anyCodePoint = function(v) {
    var v1 = uncons2(v.substring);
    if (v1 instanceof Nothing) {
      return new Left({
        pos: v.position,
        error: "Unexpected EOF"
      });
    }
    ;
    if (v1 instanceof Just) {
      return new Right({
        result: v1.value0.head,
        suffix: {
          substring: v1.value0.tail,
          position: v.position + 1 | 0
        }
      });
    }
    ;
    throw new Error("Failed pattern match at StringParser.CodePoints (line 72, column 3 - line 74, column 103): " + [v1.constructor.name]);
  };
  var anyChar2 = /* @__PURE__ */ function() {
    var notAChar = function(cc) {
      return fail("Code point " + (show22(cc) + " is not a character"));
    };
    return bind4(mapFlipped(functorParser)(anyCodePoint)(fromEnum(boundedEnumCodePoint)))(function(cc) {
      var v = fromCharCode3(cc);
      if (v instanceof Just) {
        var $73 = cc > 65535;
        if ($73) {
          return notAChar(cc);
        }
        ;
        return pure5(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return notAChar(cc);
      }
      ;
      throw new Error("Failed pattern match at StringParser.CodePoints (line 57, column 3 - line 65, column 27): " + [v.constructor.name]);
    });
  }();
  var satisfy = function(f) {
    return $$try(bind4(anyChar2)(function(c) {
      var $75 = f(c);
      if ($75) {
        return pure5(c);
      }
      ;
      return fail("Character " + (show3(c) + " did not satisfy predicate"));
    }));
  };
  var $$char = function(c) {
    return withError(satisfy(function(v) {
      return v === c;
    }))("Could not match character " + show3(c));
  };
  var alphaNum = /* @__PURE__ */ alt3(anyLetter)(/* @__PURE__ */ withError(anyDigit)("Expected a letter or a number"));

  // output/Data.Abc.Parser/index.js
  var map14 = /* @__PURE__ */ map(functorParser);
  var applySecond3 = /* @__PURE__ */ applySecond(applyParser);
  var eq3 = /* @__PURE__ */ eq(eqChar);
  var alt4 = /* @__PURE__ */ alt(altParser);
  var applyFirst3 = /* @__PURE__ */ applyFirst(applyParser);
  var voidRight2 = /* @__PURE__ */ voidRight(functorParser);
  var map15 = /* @__PURE__ */ map(functorMaybe);
  var choice2 = /* @__PURE__ */ choice(foldableArray);
  var replicate1A2 = /* @__PURE__ */ replicate1A(applyParser)(unfoldable1NonEmptyList)(traversable1NonEmptyList);
  var apply4 = /* @__PURE__ */ apply(applyParser);
  var toRational2 = /* @__PURE__ */ toRational(toRationalInt);
  var map22 = /* @__PURE__ */ map(functorFn);
  var bind5 = /* @__PURE__ */ bind(bindParser);
  var max3 = /* @__PURE__ */ max(ordInt);
  var pure6 = /* @__PURE__ */ pure(applicativeParser);
  var TempoDesignation = /* @__PURE__ */ function() {
    function TempoDesignation2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TempoDesignation2.create = function(value0) {
      return function(value1) {
        return new TempoDesignation2(value0, value1);
      };
    };
    return TempoDesignation2;
  }();
  var tupletLength = /* @__PURE__ */ regex2("[2-9]");
  var tup = /* @__PURE__ */ map14(/* @__PURE__ */ join(bindMaybe))(/* @__PURE__ */ optionMaybe(/* @__PURE__ */ applySecond3(/* @__PURE__ */ $$char(":"))(/* @__PURE__ */ optionMaybe(tupletLength))));
  var toTupletInt = function(s) {
    return fromMaybe(3)(fromString(s));
  };
  var strToEol = /* @__PURE__ */ regex2("[^\r\n%]*");
  var space = /* @__PURE__ */ $$char(" ");
  var shortDecoration = /* @__PURE__ */ withError(/* @__PURE__ */ regex2("[\\.~HLMOPSTuv]"))("short decoration");
  var sharpOrFlat = /* @__PURE__ */ map14(function(x) {
    var $59 = x === "#";
    if ($59) {
      return Sharp.value;
    }
    ;
    return Flat.value;
  })(/* @__PURE__ */ alt4(/* @__PURE__ */ $$char("#"))(/* @__PURE__ */ $$char("b")));
  var scoreSpace = /* @__PURE__ */ alt4(/* @__PURE__ */ $$char("	"))(space);
  var spacer = /* @__PURE__ */ function() {
    return withError(map14(Spacer.create)(map14(length3)(many1(scoreSpace))))("space");
  }();
  var whiteSpace = /* @__PURE__ */ map14(/* @__PURE__ */ foldMap(foldableList)(monoidString)(function($82) {
    return singleton7(codePointFromChar($82));
  }))(/* @__PURE__ */ many(scoreSpace));
  var unsupportedHeaderCode = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ regex2("[a-qt-vx-zEJ]:"))(whiteSpace);
  var unsupportedHeader = /* @__PURE__ */ function() {
    return withError(applyFirst3(voidRight2(UnsupportedHeader.value)(unsupportedHeaderCode))(strToEol))("unsupported header");
  }();
  var scientificPitchNotation = function(pc) {
    return function(oct) {
      var $60 = includes(pc)("ABCDEFG");
      if ($60) {
        return middlecOctave + oct | 0;
      }
      ;
      return (middlecOctave + 1 | 0) + oct | 0;
    };
  };
  var rightBracket = /* @__PURE__ */ $$char(")");
  var rightSlurBrackets = /* @__PURE__ */ withError(/* @__PURE__ */ map14(length2)(/* @__PURE__ */ many(rightBracket)))("right slurs");
  var repeatMarkers = /* @__PURE__ */ map14(length2)(/* @__PURE__ */ many(/* @__PURE__ */ $$char(":")));
  var pitch = /* @__PURE__ */ regex2("[A-Ga-g]");
  var phrygian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Phrygian.value)(whiteSpace))(regex2("[P|p][H|h][R|r][A-Za-z]*"));
  }();
  var octaveShift = function(s) {
    var up = length(filter(eq3("'"))(toCharArray(s)));
    var down = length(filter(eq3(","))(toCharArray(s)));
    return up - down | 0;
  };
  var nometer = /* @__PURE__ */ function() {
    return voidRight2(Nothing.value)(string("none"));
  }();
  var newline = /* @__PURE__ */ withError(/* @__PURE__ */ satisfy(/* @__PURE__ */ eq3("\n")))("expected newline");
  var moveOctave = /* @__PURE__ */ map14(octaveShift)(/* @__PURE__ */ regex2("[',]*"));
  var mixolydian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Mixolydian.value)(whiteSpace))(regex2("[M|m][I|i][X|x][A-Za-z]*"));
  }();
  var minor = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Minor.value)(whiteSpace))(regex2("[M|m][A-Za-z]*"));
  }();
  var maybeTie = /* @__PURE__ */ withError(/* @__PURE__ */ map14(/* @__PURE__ */ map15(function(v) {
    return "-";
  }))(/* @__PURE__ */ optionMaybe(/* @__PURE__ */ regex2(" *-"))))("tie");
  var major = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Major.value)(whiteSpace))(regex2("[M|m][A|a][J|j][A-Za-z]*"));
  }();
  var lydian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Lydian.value)(whiteSpace))(regex2("[L|l][Y|y][D|d][A-Za-z]*"));
  }();
  var lookupPitch = function(p) {
    var v = toUpper(p);
    if (v === "A") {
      return A.value;
    }
    ;
    if (v === "B") {
      return B.value;
    }
    ;
    if (v === "C") {
      return C.value;
    }
    ;
    if (v === "D") {
      return D.value;
    }
    ;
    if (v === "E") {
      return E.value;
    }
    ;
    if (v === "F") {
      return F.value;
    }
    ;
    if (v === "G") {
      return G.value;
    }
    ;
    return C.value;
  };
  var longDecoration = /* @__PURE__ */ withError(/* @__PURE__ */ between(/* @__PURE__ */ $$char("!"))(/* @__PURE__ */ $$char("!"))(/* @__PURE__ */ regex2("[^\r\n!]+")))("long decoration");
  var locrian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Locrian.value)(whiteSpace))(regex2("[L|l][O|o][C|c][A-Za-z]*"));
  }();
  var literalQuotedString = function(retainQuotes) {
    var quotedString = withError(applyFirst3(applySecond3(string('"'))(regex2('(\\\\"|[^"\n])*')))(string('"')))("quoted string");
    if (retainQuotes) {
      return map14(function(s) {
        return '"' + (s + '"');
      })(quotedString);
    }
    ;
    return quotedString;
  };
  var spacedQuotedString = /* @__PURE__ */ $$try(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ applySecond3(whiteSpace)(/* @__PURE__ */ literalQuotedString(true)))(whiteSpace));
  var leftBracket = /* @__PURE__ */ $$char("(");
  var leftSlurBrackets = /* @__PURE__ */ withError(/* @__PURE__ */ map14(length2)(/* @__PURE__ */ many(leftBracket)))("left slurs");
  var tupletBrackets = /* @__PURE__ */ withError(/* @__PURE__ */ map14(length3)(/* @__PURE__ */ many1(leftBracket)))("tuplet + slurs");
  var keyName = /* @__PURE__ */ regex2("[A-G]");
  var ionian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Ionian.value)(whiteSpace))(regex2("[I|i][O|o][N|n][A-Za-z]*"));
  }();
  var inlineInfo = function(isInline) {
    var pattern = function() {
      if (isInline) {
        return "[^\r\n\\[\\]]*";
      }
      ;
      return "[^\r\n]*";
    }();
    return regex2(pattern);
  };
  var ignore = /* @__PURE__ */ function() {
    return withError(voidRight2(Ignore.value)(regex2("[#@;`\\*\\?]+")))("ignored character");
  }();
  var headerCode = function(c) {
    var pattern = fromCharArray([c, ":"]);
    return applyFirst3(string(pattern))(whiteSpace);
  };
  var history = /* @__PURE__ */ function() {
    return withError(map14(History.create)(applySecond3(headerCode("H"))(strToEol)))("H header");
  }();
  var instruction = function(isInline) {
    return withError(map14(Instruction.create)(applySecond3(headerCode("I"))(inlineInfo(isInline))))("I header");
  };
  var macro = function(isInline) {
    return withError(map14(Macro.create)(applySecond3(headerCode("m"))(inlineInfo(isInline))))("m header");
  };
  var notes = function(isInline) {
    return withError(map14(Notes.create)(applySecond3(headerCode("N"))(inlineInfo(isInline))))("N header");
  };
  var origin = /* @__PURE__ */ function() {
    return withError(map14(Origin.create)(applySecond3(headerCode("O"))(strToEol)))("O header");
  }();
  var parts = function(isInline) {
    return withError(map14(Parts.create)(applySecond3(headerCode("P"))(inlineInfo(isInline))))("P header");
  };
  var remark = function(isInline) {
    return withError(map14(Remark.create)(applySecond3(headerCode("r"))(inlineInfo(isInline))))("r header");
  };
  var rhythm = function(isInline) {
    return withError(map14(Rhythm.create)(applySecond3(headerCode("R"))(inlineInfo(isInline))))("R header");
  };
  var source2 = /* @__PURE__ */ function() {
    return withError(map14(Source.create)(applySecond3(headerCode("S"))(strToEol)))("S header");
  }();
  var symbolLine = function(isInline) {
    return withError(map14(SymbolLine.create)(applySecond3(headerCode("s"))(inlineInfo(isInline))))("s header");
  };
  var title = function(isInline) {
    return withError(map14(Title.create)(applySecond3(headerCode("T"))(inlineInfo(isInline))))("T header");
  };
  var transcription = /* @__PURE__ */ function() {
    return withError(map14(Transcription.create)(applySecond3(headerCode("Z"))(strToEol)))("Z header");
  }();
  var userDefined = function(isInline) {
    return withError(map14(UserDefined.create)(applySecond3(headerCode("U"))(inlineInfo(isInline))))("U header");
  };
  var wordsAfter = function(isInline) {
    return withError(map14(WordsAfter.create)(applySecond3(headerCode("W"))(inlineInfo(isInline))))("W header");
  };
  var wordsAligned = function(isInline) {
    return withError(map14(WordsAligned.create)(applySecond3(headerCode("w"))(inlineInfo(isInline))))("w header");
  };
  var tuneBodyOnlyInfo = function(isInline) {
    return withError(choice2([symbolLine(isInline), wordsAligned(isInline)]))("tune body only info");
  };
  var group3 = /* @__PURE__ */ function() {
    return withError(map14(Group.create)(applySecond3(headerCode("G"))(strToEol)))("G header");
  }();
  var fileUrl = /* @__PURE__ */ function() {
    return withError(map14(FileUrl.create)(applySecond3(headerCode("F"))(strToEol)))("F header");
  }();
  var fieldContinuation = /* @__PURE__ */ function() {
    return withError(map14(FieldContinuation.create)(applySecond3(headerCode("+"))(strToEol)))("field continuation");
  }();
  var dorian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Dorian.value)(whiteSpace))(regex2("[D|d][O|o][R|r][A-Za-z]*"));
  }();
  var discography = /* @__PURE__ */ function() {
    return withError(map14(Discography.create)(applySecond3(headerCode("D"))(strToEol)))("D header");
  }();
  var degenerateDoubleColon = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2({
      endRepeats: 1,
      thickness: Thin.value,
      startRepeats: 1,
      iteration: Nothing.value
    })($$char(":")))($$char(":"));
  }();
  var decoration = /* @__PURE__ */ withError(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ alt4(shortDecoration)(longDecoration))(whiteSpace))("decoration");
  var decorations = /* @__PURE__ */ many(decoration);
  var decoratedSpace = /* @__PURE__ */ function() {
    return applyFirst3(map14(DecoratedSpace.create)(decorations))($$char("y"));
  }();
  var cutTime2 = /* @__PURE__ */ function() {
    return voidRight2(new Just(cutTime))(string("C|"));
  }();
  var crlf = /* @__PURE__ */ withError(/* @__PURE__ */ voidRight2("\n")(/* @__PURE__ */ regex2("!?\r(\n)?")))("expected crlf");
  var counted = function(num) {
    return function(parser) {
      return replicate1A2(num)(parser);
    };
  };
  var composer = /* @__PURE__ */ function() {
    return withError(map14(Composer.create)(applySecond3(headerCode("C"))(strToEol)))("C header");
  }();
  var commonTime2 = /* @__PURE__ */ function() {
    return voidRight2(new Just(commonTime))($$char("C"));
  }();
  var commentStrToEol = /* @__PURE__ */ regex2("[^\r\n]*");
  var comment = /* @__PURE__ */ applySecond3(/* @__PURE__ */ $$char("%"))(commentStrToEol);
  var commentLine = /* @__PURE__ */ function() {
    return withError(map14(Comment.create)(comment))("comment line");
  }();
  var eol = /* @__PURE__ */ alt4(/* @__PURE__ */ applySecond3(/* @__PURE__ */ optional(comment))(crlf))(newline);
  var continuation = /* @__PURE__ */ function() {
    return withError(applyFirst3(apply4(voidRight2(Continuation.create)($$char("\\")))(regex2("[^\r\n]*")))(eol))("continuation");
  }();
  var chordSymbol = /* @__PURE__ */ withError(/* @__PURE__ */ map14(function($83) {
    return ChordSymbol.create(function(v) {
      return {
        name: v,
        duration: Nothing.value
      };
    }($83));
  })(/* @__PURE__ */ literalQuotedString(false)))("chord symbol");
  var buildTupletSignature = function(ps) {
    return function(mq) {
      return function(mr) {
        var p = toTupletInt(ps);
        var qdefault = function() {
          if (p === 2) {
            return 3;
          }
          ;
          if (p === 3) {
            return 2;
          }
          ;
          if (p === 4) {
            return 3;
          }
          ;
          if (p === 6) {
            return 2;
          }
          ;
          if (p === 8) {
            return 3;
          }
          ;
          return 2;
        }();
        var q = fromMaybe(qdefault)(map15(toTupletInt)(mq));
        var r = fromMaybe(p)(map15(toTupletInt)(mr));
        return {
          p,
          q,
          r
        };
      };
    };
  };
  var tupletSignature = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ map14(buildTupletSignature)(tupletLength))(tup))(tup))(whiteSpace);
  var buildTempoSignature3 = function(bpm) {
    var noteLengths = singleton4(toRational2(1)(4));
    return {
      noteLengths,
      bpm,
      marking: Nothing.value
    };
  };
  var buildTempoSignature = function(marking) {
    return function(td) {
      return {
        noteLengths: td.value0,
        bpm: td.value1,
        marking
      };
    };
  };
  var buildTempoSignature2 = function(marking) {
    return function(td) {
      return buildTempoSignature(new Just(marking))(td);
    };
  };
  var buildRationalFromSlashList = function(xs) {
    var f = function(i) {
      return toRational2(1)(pow(2)(i));
    };
    return f(length3(xs));
  };
  var manySlashes = /* @__PURE__ */ map14(buildRationalFromSlashList)(/* @__PURE__ */ apply4(/* @__PURE__ */ map14(cons2)(/* @__PURE__ */ $$char("/")))(/* @__PURE__ */ many1(/* @__PURE__ */ $$char("/"))));
  var buildBrokenOperator = function(s) {
    var $68 = startsWith("<")(s);
    if ($68) {
      return new LeftArrow(length5(s));
    }
    ;
    return new RightArrow(length5(s));
  };
  var brokenRhythmOperator = /* @__PURE__ */ regex2("(<+|>+)");
  var degenerateBrokenRhythmOperator = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ applySecond3(/* @__PURE__ */ optional(leftBracket))(brokenRhythmOperator))(/* @__PURE__ */ optional(rightBracket));
  var brokenRhythmTie = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ map14(buildBrokenOperator)(degenerateBrokenRhythmOperator))(whiteSpace);
  var book = /* @__PURE__ */ function() {
    return withError(map14(Book.create)(applySecond3(headerCode("B"))(strToEol)))("B Header");
  }();
  var barlineThickness = /* @__PURE__ */ function() {
    return choice2([voidRight2(ThickThin.value)(string("[|")), voidRight2(ThinThick.value)(string("|]")), voidRight2(ThickThin.value)(string("]|")), voidRight2(ThinThin.value)(string("||")), voidRight2(Thin.value)(string("|"))]);
  }();
  var area = /* @__PURE__ */ function() {
    return withError(map14(Area.create)(applySecond3(headerCode("A"))(strToEol)))("A header");
  }();
  var anyInt = /* @__PURE__ */ regex2("(0|[1-9][0-9]*)");
  var $$int = /* @__PURE__ */ withError(/* @__PURE__ */ map14(/* @__PURE__ */ map22(/* @__PURE__ */ fromMaybe(1))(fromString))(anyInt))("expected a positive integer");
  var anyRat = /* @__PURE__ */ apply4(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ map14(toRational2)(/* @__PURE__ */ option(1)($$int)))(/* @__PURE__ */ $$char("/")))(/* @__PURE__ */ option(2)($$int));
  var degenerateTempo = /* @__PURE__ */ map14(buildTempoSignature3)($$int);
  var integralAsRational = /* @__PURE__ */ map14(fromInt2)($$int);
  var noteDur = /* @__PURE__ */ choice2([/* @__PURE__ */ $$try(manySlashes), /* @__PURE__ */ $$try(anyRat), integralAsRational]);
  var rational = /* @__PURE__ */ apply4(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ map14(toRational2)($$int))(/* @__PURE__ */ $$char("/")))($$int);
  var headerRational = /* @__PURE__ */ applyFirst3(rational)(whiteSpace);
  var noteDuration = /* @__PURE__ */ applyFirst3(rational)(whiteSpace);
  var unitNoteLength = /* @__PURE__ */ function() {
    return withError(map14(UnitNoteLength.create)(applySecond3(headerCode("L"))(noteDuration)))("L header");
  }();
  var referenceNumber = /* @__PURE__ */ function() {
    return withError(applyFirst3(map14(ReferenceNumber.create)(applySecond3(headerCode("X"))(optionMaybe($$int))))(whiteSpace))("x header");
  }();
  var tuneInfo = /* @__PURE__ */ withError(/* @__PURE__ */ choice2([area, book, composer, discography, fileUrl, group3, history, origin, source2, referenceNumber, transcription, unsupportedHeader]))("tune info");
  var tempoDesignation = /* @__PURE__ */ function() {
    return apply4(applyFirst3(map14(TempoDesignation.create)(many1(headerRational)))($$char("=")))($$int);
  }();
  var prefixedTempoDesignation = /* @__PURE__ */ apply4(/* @__PURE__ */ map14(buildTempoSignature2)(spacedQuotedString))(tempoDesignation);
  var suffixedTempoDesignation = /* @__PURE__ */ apply4(/* @__PURE__ */ map14(/* @__PURE__ */ flip(buildTempoSignature2))(tempoDesignation))(spacedQuotedString);
  var unlabelledTempoDesignation = /* @__PURE__ */ function() {
    return map14(buildTempoSignature(Nothing.value))(tempoDesignation);
  }();
  var tempoSignature = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ choice2([/* @__PURE__ */ $$try(suffixedTempoDesignation), /* @__PURE__ */ $$try(unlabelledTempoDesignation), degenerateTempo, prefixedTempoDesignation]))(whiteSpace);
  var tempo = /* @__PURE__ */ function() {
    return withError(map14(Tempo.create)(applySecond3(headerCode("Q"))(tempoSignature)))("Q header");
  }();
  var timeSignature = /* @__PURE__ */ function() {
    return map14(Just.create)(applyFirst3(apply4(applyFirst3(map14(function(v) {
      return function(v1) {
        return {
          numerator: v,
          denominator: v1
        };
      };
    })($$int))($$char("/")))($$int))(whiteSpace));
  }();
  var meterDefinition = /* @__PURE__ */ choice2([cutTime2, commonTime2, timeSignature, nometer]);
  var meter = /* @__PURE__ */ function() {
    return withError(map14(Meter.create)(applySecond3(headerCode("M"))(meterDefinition)))("M header");
  }();
  var anyDigit2 = /* @__PURE__ */ regex2("([0-9])");
  var digit = /* @__PURE__ */ withError(/* @__PURE__ */ map14(/* @__PURE__ */ map22(/* @__PURE__ */ fromMaybe(1))(fromString))(anyDigit2))("expected a digit");
  var simpleVolta = /* @__PURE__ */ function() {
    return withError(map14(Volta.create)(digit))("simple volta");
  }();
  var voltaRange = /* @__PURE__ */ function() {
    return withError(apply4(map14(VoltaRange.create)(digit))(applySecond3($$char("-"))(digit)))("volta range");
  }();
  var volta = /* @__PURE__ */ alt4(/* @__PURE__ */ $$try(voltaRange))(simpleVolta);
  var repeatSection = /* @__PURE__ */ sepBy1(volta)(/* @__PURE__ */ $$char(","));
  var degenerateBarVolta = /* @__PURE__ */ function() {
    return map14(function(v) {
      return {
        endRepeats: 0,
        thickness: Thin.value,
        startRepeats: 0,
        iteration: v
      };
    })(map14(Just.create)(applySecond3(applySecond3(whiteSpace)($$char("[")))(repeatSection)));
  }();
  var normalBarline = /* @__PURE__ */ withError(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ map14(function(v) {
    return function(v1) {
      return function(v2) {
        return function(v3) {
          return {
            endRepeats: v,
            thickness: v1,
            startRepeats: v2,
            iteration: v3
          };
        };
      };
    };
  })(repeatMarkers))(barlineThickness))(repeatMarkers))(/* @__PURE__ */ optionMaybe(repeatSection)))("bartype");
  var barline = /* @__PURE__ */ choice2([/* @__PURE__ */ $$try(normalBarline), degenerateDoubleColon, degenerateBarVolta]);
  var annotationString = /* @__PURE__ */ withError(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ applySecond3(/* @__PURE__ */ string('"'))(/* @__PURE__ */ regex2('[\\^\\>\\<-@](\\\\"|[^"\n])*')))(/* @__PURE__ */ string('"')))("annotation");
  var annotation = /* @__PURE__ */ function() {
    var buildAnnotation = function(s) {
      var placement = function() {
        var v = charAt2(0)(s);
        if (v instanceof Just && v.value0 === "^") {
          return AboveNextSymbol.value;
        }
        ;
        if (v instanceof Just && v.value0 === "_") {
          return BelowNextSymbol.value;
        }
        ;
        if (v instanceof Just && v.value0 === "<") {
          return LeftOfNextSymbol.value;
        }
        ;
        if (v instanceof Just && v.value0 === ">") {
          return RightOfNextSymbol.value;
        }
        ;
        return Discretional.value;
      }();
      return new Annotation(placement, drop4(1)(s));
    };
    return withError(map14(buildAnnotation)(annotationString))("annotation");
  }();
  var alphaNumPlusString = /* @__PURE__ */ map14(/* @__PURE__ */ function() {
    var $84 = fromFoldable(foldableList);
    return function($85) {
      return fromCharArray($84(toList($85)));
    };
  }())(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ many1(/* @__PURE__ */ alt4(alphaNum)(/* @__PURE__ */ alt4(/* @__PURE__ */ $$char("-"))(/* @__PURE__ */ alt4(/* @__PURE__ */ $$char("+"))(/* @__PURE__ */ $$char("_"))))))(whiteSpace));
  var kvPair = /* @__PURE__ */ function() {
    return apply4(map14(Tuple.create)(alphaNumPlusString))(applySecond3($$char("="))(alt4(spacedQuotedString)(alphaNumPlusString)));
  }();
  var amorphousProperties = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ map14(/* @__PURE__ */ fromFoldable3(ordString)(foldableList))(/* @__PURE__ */ many(kvPair)))(whiteSpace);
  var voice = /* @__PURE__ */ function() {
    return map14(Voice.create)(withError(apply4(apply4(voidRight2(function(v) {
      return function(v1) {
        return {
          id: v,
          properties: v1
        };
      };
    })(headerCode("V")))(alphaNumPlusString))(amorphousProperties))("V header"));
  }();
  var aeolian = /* @__PURE__ */ function() {
    return applyFirst3(voidRight2(Aeolian.value)(whiteSpace))(regex2("[A|a][E|e][O|o][A-Za-z]*"));
  }();
  var mode = /* @__PURE__ */ choice2([/* @__PURE__ */ $$try(major), ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian, minor]);
  var keySignature = /* @__PURE__ */ function() {
    var buildKeySignature = function(pStr) {
      return function(ma) {
        return function(mm) {
          return {
            pitchClass: lookupPitch(pStr),
            accidental: ma,
            mode: fromMaybe(Major.value)(mm)
          };
        };
      };
    };
    return apply4(applyFirst3(apply4(map14(buildKeySignature)(keyName))(option(Natural.value)(sharpOrFlat)))(whiteSpace))(optionMaybe(mode));
  }();
  var accidental = /* @__PURE__ */ function() {
    var buildAccidental = function(s) {
      if (s === "^^") {
        return DoubleSharp.value;
      }
      ;
      if (s === "__") {
        return DoubleFlat.value;
      }
      ;
      if (s === "^") {
        return Sharp.value;
      }
      ;
      if (s === "_") {
        return Flat.value;
      }
      ;
      return Natural.value;
    };
    return map14(buildAccidental)(choice2([string("^^"), string("__"), string("^"), string("_"), string("=")]));
  }();
  var keyAccidental = /* @__PURE__ */ function() {
    var buildPitch = function(a) {
      return function(pitchStr) {
        return new Pitch({
          pitchClass: lookupPitch(pitchStr),
          accidental: a
        });
      };
    };
    return apply4(map14(buildPitch)(accidental))(pitch);
  }();
  var keyAccidentals = /* @__PURE__ */ applySecond3(whiteSpace)(/* @__PURE__ */ sepBy(keyAccidental)(space));
  var key = /* @__PURE__ */ function() {
    return map14(Key.create)(withError(apply4(apply4(apply4(voidRight2(function(v) {
      return function(v1) {
        return function(v2) {
          return {
            keySignature: v,
            modifications: v1,
            properties: v2
          };
        };
      };
    })(headerCode("K")))(keySignature))(keyAccidentals))(amorphousProperties))("K header"));
  }();
  var anywhereInfo = function(isInline) {
    return withError(choice2([instruction(isInline), key, unitNoteLength, meter, macro(isInline), notes(isInline), parts(isInline), tempo, rhythm(isInline), remark(isInline), title(isInline), userDefined(isInline), voice, wordsAfter(isInline), fieldContinuation, commentLine]))("anywhere info");
  };
  var informationField = function(isInline) {
    return withError(choice2([anywhereInfo(isInline), tuneInfo]))("header");
  };
  var header = /* @__PURE__ */ applyFirst3(/* @__PURE__ */ informationField(false))(eol);
  var headers = /* @__PURE__ */ withError(/* @__PURE__ */ many(header))("headers");
  var tuneBodyInfo = function(isInline) {
    return withError(choice2([tuneBodyOnlyInfo(isInline), anywhereInfo(isInline)]))("tune body info");
  };
  var inline = /* @__PURE__ */ function() {
    return withError(map14(Inline.create)(between($$char("["))($$char("]"))(tuneBodyInfo(true))))("inline header");
  }();
  var tuneBodyHeader = /* @__PURE__ */ function() {
    return withError(applyFirst3(map14(BodyInfo.create)(tuneBodyInfo(true)))(eol))("tune body header");
  }();
  var maybeAccidental = /* @__PURE__ */ optionMaybe(accidental);
  var acciaccatura = /* @__PURE__ */ map14(function(v) {
    return true;
  })(/* @__PURE__ */ optionMaybe(/* @__PURE__ */ $$char("/")));
  var abcRest = /* @__PURE__ */ withError(/* @__PURE__ */ map14(function(v) {
    return {
      duration: v
    };
  })(/* @__PURE__ */ map14(/* @__PURE__ */ fromMaybe(/* @__PURE__ */ fromInt2(1)))(/* @__PURE__ */ applySecond3(/* @__PURE__ */ regex2("[XxZz]"))(/* @__PURE__ */ optionMaybe(noteDur)))))("abcRest");
  var rest = /* @__PURE__ */ function() {
    return withError(map14(Rest.create)(abcRest))("rest");
  }();
  var abcNote = /* @__PURE__ */ function() {
    var buildNote = function(macc) {
      return function(pitchStr) {
        return function(octave) {
          return function(ml) {
            return function(mt) {
              var tied = function() {
                if (mt instanceof Just) {
                  return true;
                }
                ;
                return false;
              }();
              var spn = scientificPitchNotation(pitchStr)(octave);
              var pc = lookupPitch(toUpper(pitchStr));
              var l = fromMaybe(toRational2(1)(1))(ml);
              var acc = function() {
                if (macc instanceof Nothing) {
                  return Implicit.value;
                }
                ;
                if (macc instanceof Just) {
                  return macc.value0;
                }
                ;
                throw new Error("Failed pattern match at Data.Abc.Parser (line 307, column 9 - line 309, column 22): " + [macc.constructor.name]);
              }();
              return {
                pitchClass: pc,
                accidental: acc,
                octave: spn,
                duration: l,
                tied
              };
            };
          };
        };
      };
    };
    return withError(apply4(apply4(apply4(apply4(map14(buildNote)(maybeAccidental))(pitch))(moveOctave))(optionMaybe(noteDur)))(maybeTie))("ABC note");
  }();
  var grace = /* @__PURE__ */ apply4(/* @__PURE__ */ map14(function(v) {
    return function(v1) {
      return {
        isAcciaccatura: v,
        notes: v1
      };
    };
  })(acciaccatura))(/* @__PURE__ */ many1(abcNote));
  var graceBracket = /* @__PURE__ */ withError(/* @__PURE__ */ applyFirst3(/* @__PURE__ */ between(/* @__PURE__ */ $$char("{"))(/* @__PURE__ */ $$char("}"))(grace))(whiteSpace))("grace bracket");
  var graceableNote = /* @__PURE__ */ withError(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ map14(function(v) {
    return function(v1) {
      return function(v2) {
        return function(v3) {
          return function(v4) {
            return {
              maybeGrace: v,
              leftSlurs: v1,
              decorations: v2,
              abcNote: v3,
              rightSlurs: v4
            };
          };
        };
      };
    };
  })(/* @__PURE__ */ optionMaybe(graceBracket)))(leftSlurBrackets))(decorations))(abcNote))(rightSlurBrackets))("graceable note");
  var note = /* @__PURE__ */ function() {
    return map14(Note.create)(graceableNote);
  }();
  var restOrNote = /* @__PURE__ */ function() {
    return alt4(map14(Left.create)(abcRest))(map14(Right.create)(graceableNote));
  }();
  var brokenRhythmPair = /* @__PURE__ */ function() {
    return withError(apply4(apply4(map14(BrokenRhythmPair.create)(restOrNote))(brokenRhythmTie))(restOrNote))("broken rhythm pair");
  }();
  var tuplet = /* @__PURE__ */ bind5(/* @__PURE__ */ optionMaybe(graceBracket))(function(maybeGrace) {
    return bind5(tupletBrackets)(function(leftBracketCount) {
      var leftSlurs = max3(0)(leftBracketCount - 1 | 0);
      return bind5(tupletSignature)(function(signature) {
        return bind5(counted(signature.r)(applySecond3(whiteSpace)(restOrNote)))(function(restsOrNotes) {
          return pure6(new Tuplet({
            maybeGrace,
            leftSlurs,
            signature,
            restsOrNotes
          }));
        });
      });
    });
  });
  var abcChord = /* @__PURE__ */ function() {
    var buildChord = function(leftSlurs) {
      return function(decs) {
        return function(ns) {
          return function(ml) {
            return function(rightSlurs) {
              var l = fromMaybe(fromInt2(1))(ml);
              return {
                leftSlurs,
                decorations: decs,
                notes: ns,
                duration: l,
                rightSlurs
              };
            };
          };
        };
      };
    };
    return withError(apply4(apply4(apply4(apply4(map14(buildChord)(leftSlurBrackets))(decorations))(between($$char("["))($$char("]"))(many1(applyFirst3(abcNote)(whiteSpace)))))(optionMaybe(noteDur)))(rightSlurBrackets))("ABC chord");
  }();
  var chord = /* @__PURE__ */ function() {
    return withError(map14(Chord.create)(abcChord))("chord");
  }();
  var scoreItem = /* @__PURE__ */ withError(/* @__PURE__ */ choice2([/* @__PURE__ */ $$try(chord), /* @__PURE__ */ $$try(inline), continuation, /* @__PURE__ */ $$try(decoratedSpace), ignore, spacer, /* @__PURE__ */ $$try(annotation), chordSymbol, /* @__PURE__ */ $$try(tuplet), /* @__PURE__ */ $$try(brokenRhythmPair), rest, /* @__PURE__ */ $$try(note)]))("score item");
  var bar = /* @__PURE__ */ withError(/* @__PURE__ */ apply4(/* @__PURE__ */ apply4(/* @__PURE__ */ map14(function(v) {
    return function(v1) {
      return function(v2) {
        return {
          decorations: v,
          startLine: v1,
          music: v2
        };
      };
    };
  })(decorations))(barline))(/* @__PURE__ */ many(scoreItem)))("bar");
  var fullyBarredLine = /* @__PURE__ */ withError(/* @__PURE__ */ manyTill(bar)(eol))("fully barred line");
  var introBar = /* @__PURE__ */ function() {
    var invisibleBarType = {
      endRepeats: 0,
      thickness: Invisible.value,
      startRepeats: 0,
      iteration: Nothing.value
    };
    return withError(map14(function(v) {
      return {
        decorations: Nil.value,
        startLine: invisibleBarType,
        music: v
      };
    })(many(scoreItem)))("intro bar");
  }();
  var introLine = /* @__PURE__ */ function() {
    return withError(apply4(map14(Cons.create)(introBar))(manyTill(bar)(eol)))("intro line");
  }();
  var score = /* @__PURE__ */ function() {
    return withError(map14(Score.create)(alt4(introLine)(fullyBarredLine)))("score");
  }();
  var body = /* @__PURE__ */ function() {
    return apply4(map14(Cons.create)(score))(manyTill(alt4($$try(tuneBodyHeader))(score))(eof));
  }();
  var abc = /* @__PURE__ */ apply4(/* @__PURE__ */ map14(function(v) {
    return function(v1) {
      return {
        headers: v,
        body: v1
      };
    };
  })(headers))(body);
  var parse = function(s) {
    return runParser(abc)(s);
  };

  // output/Examples.Bugs.Texts/index.js
  var voltaBrackets = /* @__PURE__ */ function() {
    return "X:1\r\nT: volta brackets\r\nM: 2/4\r\nL: 1/16\r\nK: D\r\nB2BG BGBG |1 B2A2 A4 :|2 B2A2 A3c |[M: 3/4] e2gf d8 |\r\n";
  }();
  var threeTwoBeaming = /* @__PURE__ */ function() {
    return "X:1\r\nT: three-two beaming\r\nM: 3/2\r\nL: 1/4\r\nK: D\r\na g/a/ b a/g/ a g/a/|\r\n";
  }();
  var spacing1 = /* @__PURE__ */ function() {
    return "X:1\r\nT: spacing 1\r\nM: 3/4\r\nL: 1/8\r\nQ: 1/4=70\r\nK: Gm\r\nG | C6 |\r\n";
  }();
  var minimLayout = /* @__PURE__ */ function() {
    return "X:1\r\nT: minim layout\r\nM: 3/4\r\nL: 1/8\r\nK: C\r\n| c4 de | ed c4 |\r\n";
  }();
  var horizontalLayout = /* @__PURE__ */ function() {
    return "x: 1\r\nT: horizontal layout\r\nM: 3/4\r\nL: 1/16\r\nK: AMinor\r\ne2>a2 a4>e4 | gfed e4>B4 | cdcB A2B2 c2d2 | e4 e8 |\r\n";
  }();
  var crossBeat16th = /* @__PURE__ */ function() {
    return "X:1\r\nT: beam 16th with small tuplet crossing the beat\r\nM: 3/4\r\nL: 1/16\r\nK: C\r\nABCDE | A2B2c (3cde (3cde ABc |\r\n";
  }();

  // output/VexFlow.Abc.Beat/index.js
  var div3 = /* @__PURE__ */ div(euclideanRingRational);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toRational3 = /* @__PURE__ */ toRational(toRationalInt);
  var exactBeatNumber = function(phraseDur) {
    return function(beatDur) {
      return function(noteIndex) {
        var beats = div3(phraseDur)(beatDur);
        var $5 = noteIndex === 0;
        if ($5) {
          return Nothing.value;
        }
        ;
        var v = toString(denominator2(beats));
        if (v === "1") {
          return new Just({
            beatNumber: fromJust5(toInt(numerator2(beats))),
            noteIndex
          });
        }
        ;
        return Nothing.value;
      };
    };
  };
  var beatDuration = function(ts) {
    if (ts.numerator === 3 && ts.denominator === 2) {
      return toRational3(1)(2);
    }
    ;
    if (ts.numerator === 6 && ts.denominator === 8) {
      return toRational3(3)(8);
    }
    ;
    if (ts.numerator === 9 && ts.denominator === 8) {
      return toRational3(3)(8);
    }
    ;
    if (ts.numerator === 12 && ts.denominator === 8) {
      return toRational3(3)(8);
    }
    ;
    return toRational3(1)(ts.denominator);
  };

  // output/VexFlow.Abc.ContextChange/index.js
  var Treble = /* @__PURE__ */ function() {
    function Treble2() {
    }
    ;
    Treble2.value = new Treble2();
    return Treble2;
  }();
  var Alto = /* @__PURE__ */ function() {
    function Alto2() {
    }
    ;
    Alto2.value = new Alto2();
    return Alto2;
  }();
  var Tenor = /* @__PURE__ */ function() {
    function Tenor2() {
    }
    ;
    Tenor2.value = new Tenor2();
    return Tenor2;
  }();
  var Bass = /* @__PURE__ */ function() {
    function Bass2() {
    }
    ;
    Bass2.value = new Bass2();
    return Bass2;
  }();
  var MeterChange = /* @__PURE__ */ function() {
    function MeterChange2(value0) {
      this.value0 = value0;
    }
    ;
    MeterChange2.create = function(value0) {
      return new MeterChange2(value0);
    };
    return MeterChange2;
  }();
  var KeyChange = /* @__PURE__ */ function() {
    function KeyChange2(value0) {
      this.value0 = value0;
    }
    ;
    KeyChange2.create = function(value0) {
      return new KeyChange2(value0);
    };
    return KeyChange2;
  }();
  var UnitNoteChange = /* @__PURE__ */ function() {
    function UnitNoteChange2(value0) {
      this.value0 = value0;
    }
    ;
    UnitNoteChange2.create = function(value0) {
      return new UnitNoteChange2(value0);
    };
    return UnitNoteChange2;
  }();
  var ClefChange = /* @__PURE__ */ function() {
    function ClefChange2(value0) {
      this.value0 = value0;
    }
    ;
    ClefChange2.create = function(value0) {
      return new ClefChange2(value0);
    };
    return ClefChange2;
  }();
  var showClef = {
    show: function(v) {
      if (v instanceof Treble) {
        return "treble";
      }
      ;
      if (v instanceof Alto) {
        return "alto";
      }
      ;
      if (v instanceof Tenor) {
        return "tenor";
      }
      ;
      if (v instanceof Bass) {
        return "bass";
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.ContextChange (line 19, column 1 - line 23, column 21): " + [v.constructor.name]);
    }
  };

  // output/Data.Abc.KeySignature/index.js
  var fromJust6 = /* @__PURE__ */ fromJust();
  var succ2 = /* @__PURE__ */ succ(enumPitchClass);
  var append12 = /* @__PURE__ */ append(semigroupArray);
  var eq4 = /* @__PURE__ */ eq(eqPitchCLass);
  var eq12 = /* @__PURE__ */ eq(eqAccidental);
  var pred2 = /* @__PURE__ */ pred(enumPitchClass);
  var eq22 = /* @__PURE__ */ eq(eqMode);
  var _headers3 = /* @__PURE__ */ _headers(strongForget);
  var traversed3 = /* @__PURE__ */ traversed(traversableList)(/* @__PURE__ */ wanderForget(monoidFirst));
  var _ModifiedKeySignature2 = /* @__PURE__ */ _ModifiedKeySignature(/* @__PURE__ */ choiceForget(monoidFirst));
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var map16 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable(unfoldableList);
  var White = /* @__PURE__ */ function() {
    function White2(value0) {
      this.value0 = value0;
    }
    ;
    White2.create = function(value0) {
      return new White2(value0);
    };
    return White2;
  }();
  var Black = /* @__PURE__ */ function() {
    function Black2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Black2.create = function(value0) {
      return function(value1) {
        return new Black2(value0, value1);
      };
    };
    return Black2;
  }();
  var successor = function(pc) {
    return fromJust6(succ2(pc));
  };
  var rotate = function(n) {
    return function(xs) {
      return append12(drop(n)(xs))(take(n)(xs));
    };
  };
  var predecessor = function(pc) {
    return fromJust6(pred2(pc));
  };
  var pianoOctave = /* @__PURE__ */ function() {
    return [new White(C.value), new Black(C.value, D.value), new White(D.value), new Black(D.value, E.value), new White(E.value), new White(F.value), new Black(F.value, G.value), new White(G.value), new Black(G.value, A.value), new White(A.value), new Black(A.value, B.value), new White(B.value)];
  }();
  var pianoKeyToPitch = function(isFlatCtx) {
    return function(pianoKey) {
      var convertPianoKey = function(v) {
        return function(v1) {
          if (v1 instanceof White) {
            return new Pitch({
              pitchClass: v1.value0,
              accidental: Natural.value
            });
          }
          ;
          if (v1 instanceof Black) {
            if (v) {
              return new Pitch({
                pitchClass: v1.value1,
                accidental: Flat.value
              });
            }
            ;
            return new Pitch({
              pitchClass: v1.value0,
              accidental: Sharp.value
            });
          }
          ;
          throw new Error("Failed pattern match at Data.Abc.KeySignature (line 440, column 5 - line 440, column 52): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return convertPianoKey(isFlatCtx)(pianoKey);
    };
  };
  var notesInChromaticScale = 12;
  var isFSharp = function(ks) {
    return eq4(ks.pitchClass)(F.value) && (eq12(ks.accidental)(Sharp.value) && (eq22(ks.mode)(Major.value) || eq22(ks.mode)(Ionian.value)));
  };
  var getKeySig = function(tune) {
    return firstOf(function($166) {
      return _headers3(traversed3(_ModifiedKeySignature2($166)));
    })(tune);
  };
  var fSharpScale = /* @__PURE__ */ function() {
    return new Cons(new Pitch({
      pitchClass: F.value,
      accidental: Sharp.value
    }), new Cons(new Pitch({
      pitchClass: G.value,
      accidental: Sharp.value
    }), new Cons(new Pitch({
      pitchClass: A.value,
      accidental: Sharp.value
    }), new Cons(new Pitch({
      pitchClass: B.value,
      accidental: Natural.value
    }), new Cons(new Pitch({
      pitchClass: C.value,
      accidental: Sharp.value
    }), new Cons(new Pitch({
      pitchClass: D.value,
      accidental: Sharp.value
    }), new Cons(new Pitch({
      pitchClass: E.value,
      accidental: Sharp.value
    }), Nil.value)))))));
  }();
  var fSharpKeySet = /* @__PURE__ */ filter2(function(v) {
    return eq12(v.value0.accidental)(Sharp.value);
  })(fSharpScale);
  var eqPianoKey = {
    eq: function(x) {
      return function(y) {
        if (x instanceof White && y instanceof White) {
          return eq4(x.value0)(y.value0);
        }
        ;
        if (x instanceof Black && y instanceof Black) {
          return eq4(x.value0)(y.value0) && eq4(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqPianoKey);
  var distanceFromMajor = function(mode2) {
    if (mode2 instanceof Dorian) {
      return 10;
    }
    ;
    if (mode2 instanceof Phrygian) {
      return 8;
    }
    ;
    if (mode2 instanceof Lydian) {
      return 7;
    }
    ;
    if (mode2 instanceof Mixolydian) {
      return 5;
    }
    ;
    if (mode2 instanceof Aeolian) {
      return 3;
    }
    ;
    if (mode2 instanceof Minor) {
      return 3;
    }
    ;
    if (mode2 instanceof Locrian) {
      return 1;
    }
    ;
    if (mode2 instanceof Major) {
      return 0;
    }
    ;
    if (mode2 instanceof Ionian) {
      return 0;
    }
    ;
    throw new Error("Failed pattern match at Data.Abc.KeySignature (line 420, column 3 - line 429, column 16): " + [mode2.constructor.name]);
  };
  var distanceFromC = function(keySig) {
    return fromMaybe(0)(elemIndex2(keySig)(pianoOctave));
  };
  var diatonicScaleOffsets = [0, 2, 4, 5, 7, 9, 11];
  var pianoKeyScale = function(keySig) {
    return function(mode2) {
      var shift = mod3(distanceFromC(keySig) + distanceFromMajor(mode2) | 0)(notesInChromaticScale);
      var scale = rotate(shift)(pianoOctave);
      var tonic = fromMaybe(new White(C.value))(head(scale));
      var lookup1 = function(key2) {
        return fromMaybe(new White(C.value))(index(scale)(key2));
      };
      var keys3 = map16(lookup1)(diatonicScaleOffsets);
      return new Tuple(tonic, keys3);
    };
  };
  var defaultKey = /* @__PURE__ */ function() {
    return {
      keySignature: {
        pitchClass: C.value,
        accidental: Natural.value,
        mode: Major.value
      },
      modifications: Nil.value,
      properties: empty2
    };
  }();
  var buildPianoKey = function(v) {
    if (v.value0.accidental instanceof Flat) {
      return new Black(predecessor(v.value0.pitchClass), v.value0.pitchClass);
    }
    ;
    if (v.value0.accidental instanceof Sharp) {
      return new Black(v.value0.pitchClass, successor(v.value0.pitchClass));
    }
    ;
    return new White(v.value0.pitchClass);
  };
  var normaliseModalKey = function(ks) {
    var pianoKeySignature = buildPianoKey(new Pitch({
      pitchClass: ks.pitchClass,
      accidental: ks.accidental
    }));
    var v = pianoKeyScale(pianoKeySignature)(ks.mode);
    var isFlatCtx = eq12(ks.accidental)(Flat.value);
    var v1 = pianoKeyToPitch(isFlatCtx)(v.value0);
    return {
      pitchClass: v1.value0.pitchClass,
      accidental: v1.value0.accidental,
      mode: Major.value
    };
  };
  var blackKeySet = function(keySig) {
    return function(mode2) {
      var v = pianoKeyScale(keySig)(mode2);
      var isBlackKey = function(v1) {
        if (v1 instanceof White) {
          return false;
        }
        ;
        if (v1 instanceof Black) {
          return true;
        }
        ;
        throw new Error("Failed pattern match at Data.Abc.KeySignature (line 385, column 5 - line 385, column 38): " + [v1.constructor.name]);
      };
      return new Tuple(v.value0, filter(isBlackKey)(v.value1));
    };
  };
  var keySet = function(ks) {
    var pianoKeySignature = buildPianoKey(new Pitch({
      pitchClass: ks.pitchClass,
      accidental: ks.accidental
    }));
    var v = blackKeySet(pianoKeySignature)(ks.mode);
    var isFlatCtx = function() {
      if (v.value0 instanceof White && v.value0.value0 instanceof F) {
        return true;
      }
      ;
      if (v.value0 instanceof White) {
        return false;
      }
      ;
      return true;
    }();
    var basicKeySet = toUnfoldable5(map16(pianoKeyToPitch(isFlatCtx))(v.value1));
    var $155 = isFSharp(ks);
    if ($155) {
      return fSharpKeySet;
    }
    ;
    if (v.value0 instanceof Black && (v.value0.value0 instanceof F && v.value0.value1 instanceof G)) {
      return new Cons(new Pitch({
        pitchClass: C.value,
        accidental: Flat.value
      }), basicKeySet);
    }
    ;
    return basicKeySet;
  };

  // output/VexFlow.Abc.TickableContext/index.js
  var add1 = /* @__PURE__ */ add(semiringRational);
  var map17 = /* @__PURE__ */ map(functorMaybe);
  var foldl5 = /* @__PURE__ */ foldl(foldableArray);
  var mul2 = /* @__PURE__ */ mul(semiringRational);
  var toRational4 = /* @__PURE__ */ toRational(toRationalInt);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var TickableContext = /* @__PURE__ */ function() {
    function TickableContext2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TickableContext2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TickableContext2(value0, value1, value2);
        };
      };
    };
    return TickableContext2;
  }();
  var tickableSemigroupCtx = {
    append: function(v) {
      return function(v1) {
        return new TickableContext(v.value0 + v1.value0 | 0, v.value1 + v1.value1 | 0, add1(v.value2)(v1.value2));
      };
    }
  };
  var tickableMonoidCtx = /* @__PURE__ */ function() {
    return {
      mempty: new TickableContext(0, 0, fromInt2(0)),
      Semigroup0: function() {
        return tickableSemigroupCtx;
      }
    };
  }();
  var mempty2 = /* @__PURE__ */ mempty(tickableMonoidCtx);
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList)(tickableMonoidCtx);
  var tickableCountWidth = function(hasClef) {
    return function(n) {
      if (hasClef) {
        return toNumber2(n);
      }
      ;
      if (n === 1) {
        return 1.9;
      }
      ;
      if (n === 2) {
        return 2.7;
      }
      ;
      return toNumber2(n);
    };
  };
  var keySignatureWidth = function(keySignature3) {
    var v = length2(keySet(keySignature3));
    if (v === 0) {
      return 0;
    }
    ;
    if (v === 1) {
      return 1;
    }
    ;
    if (v === 2) {
      return 1;
    }
    ;
    if (v === 3) {
      return 1.5;
    }
    ;
    return 2;
  };
  var graceLength = function(maybeGraceNote) {
    return fromMaybe(0)(map17(function(g) {
      return length3(g.notes);
    })(maybeGraceNote));
  };
  var getRorNsGraceLength = function(rOrNs) {
    var f = function(acc) {
      return function(rOrN) {
        if (rOrN instanceof Left) {
          return 0 + acc | 0;
        }
        ;
        if (rOrN instanceof Right) {
          return graceLength(rOrN.value0.maybeGrace) + acc | 0;
        }
        ;
        throw new Error("Failed pattern match at VexFlow.Abc.TickableContext (line 115, column 7 - line 119, column 53): " + [rOrN.constructor.name]);
      };
    };
    return foldl5(f)(0)(rOrNs);
  };
  var getRorNsDuration = function(rOrNs) {
    var f = function(acc) {
      return function(rOrN) {
        if (rOrN instanceof Left) {
          return add1(rOrN.value0.duration)(acc);
        }
        ;
        if (rOrN instanceof Right) {
          return add1(rOrN.value0.abcNote.duration)(acc);
        }
        ;
        throw new Error("Failed pattern match at VexFlow.Abc.TickableContext (line 105, column 7 - line 107, column 68): " + [rOrN.constructor.name]);
      };
    };
    return foldl5(f)(fromInt2(0))(rOrNs);
  };
  var getTickableContext = function(m) {
    if (m instanceof Note) {
      return new TickableContext(1, graceLength(m.value0.maybeGrace), m.value0.abcNote.duration);
    }
    ;
    if (m instanceof Rest) {
      return new TickableContext(1, 0, m.value0.duration);
    }
    ;
    if (m instanceof Chord) {
      var abcNote2 = head2(m.value0.notes);
      var duration = mul2(m.value0.duration)(abcNote2.duration);
      return new TickableContext(1, 0, duration);
    }
    ;
    if (m instanceof Tuplet) {
      var reduction = toRational4(m.value0.signature.q)(m.value0.signature.p);
      var graceNoteLength = getRorNsGraceLength(toUnfoldable6(m.value0.restsOrNotes));
      var duration = mul2(reduction)(getRorNsDuration(toUnfoldable6(m.value0.restsOrNotes)));
      return new TickableContext(m.value0.signature.r, graceNoteLength, duration);
    }
    ;
    return mempty2;
  };
  var estimateBarWidth = function(hasClef) {
    return function(hasTimeSig) {
      return function(maybeKeySig) {
        return function(pixelsPerItem) {
          return function(abcBar) {
            var v = foldMap2(getTickableContext)(abcBar.music);
            var timeSigSpace = function() {
              if (hasTimeSig) {
                return 1;
              }
              ;
              return 0;
            }();
            var keySigSpace = maybe(0)(keySignatureWidth)(maybeKeySig);
            var clefSpace = function() {
              if (hasClef) {
                return 1.3;
              }
              ;
              return 0;
            }();
            return round2((clefSpace + timeSigSpace + keySigSpace + tickableCountWidth(hasClef)(v.value0) + 0.5 * toNumber2(v.value1)) * pixelsPerItem);
          };
        };
      };
    };
  };
  var defaultNoteSeparation = 32;

  // output/VexFlow.ApiBindings/foreign.js
  var wrapper = function() {
    var VF = null;
    return {
      initializeCanvas: function(config2) {
        VF = Vex.Flow;
        var renderer;
        if (config2.isSVG) {
          renderer = new VF.Renderer(config2.parentElementId, VF.Renderer.Backends.SVG);
        } else {
          renderer = new VF.Renderer(config2.parentElementId, VF.Renderer.Backends.CANVAS);
        }
        renderer.resize(config2.width, config2.height);
        var context = renderer.getContext();
        context.scale(config2.scale, config2.scale);
        return renderer;
      },
      resizeCanvas: function(renderer, config2) {
        renderer.resize(config2.width, config2.height);
        var context = renderer.getContext();
        context.scale(config2.scale, config2.scale);
        return renderer;
      },
      clearCanvas: function(renderer) {
        var context = renderer.getContext();
        context.clear();
      },
      makeStave: function(staveConfig2, clef, keySignature3) {
        var staveOptions = new Object();
        staveOptions.right_bar = staveConfig2.hasRightBar;
        staveOptions.fill_style = staveConfig2.lineColour;
        var stave = new VF.Stave(staveConfig2.x, staveConfig2.y, staveConfig2.width, staveOptions);
        if (staveConfig2.hasDoubleRightBar) {
          stave.setEndBarType(VF.Barline.type.DOUBLE);
        }
        if (staveConfig2.barNo == 0) {
          wrapper.addKeySignature(stave, keySignature3, clef);
        }
        return stave;
      },
      getStaveWidth: function(stave) {
        return stave.getWidth();
      },
      renderStave: function(renderer, stave) {
        var context = renderer.getContext();
        stave.setContext(context).draw();
      },
      addTimeSignature: function(stave, timeSignature2) {
        var meter2 = timeSignature2.numerator + "/" + timeSignature2.denominator;
        stave.setTimeSignature(meter2);
      },
      displayVolta: function(stave, volta2) {
        var voltaType;
        switch (volta2.voltaType) {
          case 2:
            voltaType = VF.Volta.type.BEGIN;
            break;
          case 3:
            voltaType = VF.Volta.type.MID;
            break;
          case 4:
            voltaType = VF.Volta.type.END;
            break;
          case 5:
            voltaType = VF.Volta.type.BEGIN_END;
            break;
          default:
            voltaType = VF.Volta.type.NONE;
        }
        stave.setVoltaType(voltaType, volta2.iteration, 25);
      },
      renderText: function(renderer, title2, font, x, y) {
        var context = renderer.getContext();
        context.setFont(font);
        context.fillText(title2, x, y);
      },
      addKeySignature: function(stave, keySignature3, clef) {
        if (clef) {
          stave.addClef(clef);
        }
        stave.setKeySignature(keySignature3);
      },
      addTempoSignature: function(stave, tempo2) {
        stave.setTempo(tempo2, 0);
      },
      /* draw the contents of the bar, using explicit beaming for the notes */
      renderBarContents: function(renderer, stave, beamSpecs, vexCurves2, musicSpec) {
        var context = renderer.getContext();
        var notes2 = musicSpec.noteSpecs.map(wrapper.makeStaveNote);
        var tuplets = musicSpec.tuplets.map(wrapper.makeTupletLayout(notes2));
        var ties = musicSpec.ties.map(wrapper.makeTie(notes2));
        var beams = beamSpecs.map(wrapper.makeBeam(notes2));
        var curves = vexCurves2.map(wrapper.makeCurve(notes2));
        wrapper.addRepetitions(stave, musicSpec.repetitions);
        wrapper.formatAndDrawNotes(context, stave, notes2);
        ties.forEach(function(t) {
          t.setContext(context).draw();
        });
        beams.forEach(function(b) {
          b.setContext(context).draw();
        });
        tuplets.forEach(function(tuplet3) {
          tuplet3.setContext(context).draw();
        });
        curves.forEach(function(c) {
          c.setContext(context).draw();
        });
      },
      displayBarBeginRepeat: function(stave, message2) {
        stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
        if (message2) {
          stave.setText(
            message2,
            VF.Modifier.Position.ABOVE,
            { shift_y: 5, justification: VF.TextNote.Justification.LEFT }
          );
        }
      },
      displayBarEndRepeat: function(stave) {
        stave.setEndBarType(VF.Barline.type.REPEAT_END);
      },
      displayBarBothRepeat: function(stave) {
        stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
        stave.setEndBarType(VF.Barline.type.REPEAT_END);
      },
      /*  This formatting appears to be sufficient for our needs.  It attempts to emulate
          the formatting we used successfully with VexFlow 1.2.89.  This went horribly wrong
          for our purposes with VexFlow 3.  The magic softmaxFactor seems to put things right.
      */
      formatAndDrawNotes: function(context, stave, notes2) {
        const voice2 = new VF.Voice().setMode(VF.Voice.Mode.SOFT);
        voice2.addTickables(notes2);
        new VF.Formatter({ softmaxFactor: 5 }).joinVoices([voice2]).format([voice2]).formatToStave([voice2], stave);
        voice2.draw(context, stave);
      },
      // make a stave note (n.b. this can represent a single note or a chord)
      makeStaveNote: function(noteSpec) {
        var sn = new VF.StaveNote(noteSpec.vexNote);
        wrapper.addAccidentals(sn, noteSpec.accidentals);
        wrapper.addDots(sn, noteSpec.dotCount);
        wrapper.addOrnaments(sn, noteSpec.ornaments);
        wrapper.addArticulations(sn, noteSpec.articulations);
        if (noteSpec.chordSymbol) {
          wrapper.addChordSymbol(sn, noteSpec.chordSymbol);
        }
        if (noteSpec.graceKeys.length > 0) {
          var graceNotes = noteSpec.graceKeys.map(wrapper.makeGraceNote);
          wrapper.addGraceAccidentals(graceNotes, noteSpec.graceAccidentals);
          var graceNoteGroup = new VF.GraceNoteGroup(graceNotes, true);
          sn.addModifier(graceNoteGroup.beamNotes(), 0);
        }
        return sn;
      },
      makeGraceNote: function(graceKey) {
        var note2 = { keys: [graceKey], duration: "8" };
        return new Vex.Flow.GraceNote(note2);
      },
      // make a tuplet layout
      makeTupletLayout: function(notes2) {
        return function(vexTuplet) {
          return new Vex.Flow.Tuplet(notes2.slice(vexTuplet.startPos, vexTuplet.endPos), {
            num_notes: vexTuplet.p,
            notes_occupied: vexTuplet.q,
            location: VF.Tuplet.LOCATION_BOTTOM
          });
        };
      },
      // make a beam between the specified notes
      makeBeam: function(notes2) {
        return function(beamSpec) {
          return new Vex.Flow.Beam(notes2.slice(beamSpec[0], beamSpec[1]), true);
        };
      },
      // tie a note to its successor
      makeTie: function(notes2) {
        return function(noteIndex) {
          return new VF.StaveTie({
            first_note: notes2[noteIndex],
            last_note: notes2[noteIndex + 1],
            first_indices: [0],
            last_indices: [0]
          });
        };
      },
      // make a slur represented by a curve
      makeCurve: function(notes2) {
        return function(vexCurve) {
          var controlPoints = [{ x: 0, y: 5 }, { x: 0, y: 5 }];
          if (vexCurve.to - vexCurve.from > 1) {
            controlPoints = [{ x: 0, y: 10 }, { x: 0, y: 10 }];
          }
          return new VF.Curve(
            notes2[vexCurve.from],
            notes2[vexCurve.to],
            {
              thickness: 2,
              cps: controlPoints
            }
          );
        };
      },
      // add the accidental(s) to the staveNote
      addAccidentals: function(staveNote, accidentals) {
        accidentals.forEach(function(accidentalString, index4) {
          if (accidentalString) {
            staveNote.addModifier(new VF.Accidental(accidentalString), index4);
          }
        });
      },
      // add any accidentals to the grace notes
      addGraceAccidentals: function(graceNotes, accidentals) {
        accidentals.forEach(function(accidentalString, index4) {
          if (accidentalString) {
            graceNotes[index4].addModifier(new VF.Accidental(accidentalString), 0);
          }
        });
      },
      // add the dottedness to the staveNote
      addDots: function(staveNote, dotCount) {
        if (dotCount == 2) {
          VF.Dot.buildAndAttach([staveNote], { all: true });
          VF.Dot.buildAndAttach([staveNote], { all: true });
        } else if (dotCount == 1) {
          VF.Dot.buildAndAttach([staveNote], { all: true });
        }
      },
      // add the ornament(s) to the staveNote
      addOrnaments: function(staveNote, ornaments2) {
        ornaments2.forEach(function(ornament, index4) {
          staveNote.addModifier(new VF.Ornament(ornament), 0);
        });
      },
      // add a chord symbol above the note where it is to take effect 
      addChordSymbol: function(staveNote, chordSymbol2) {
        var chord3 = new VF.ChordSymbol().addGlyphOrText(chordSymbol2);
        staveNote.addModifier(chord3, 0);
      },
      // add the articulation(s) to the staveNote
      addArticulations: function(staveNote, articulations2) {
        articulations2.forEach(function(articulation, index4) {
          staveNote.addModifier(new VF.Articulation(articulation).setPosition(4), 0);
        });
      },
      // add the repetitions to the stave
      addRepetitions: function(stave, repetitions) {
        repetitions.forEach(function(repetition, index4) {
          stave.setRepetitionType(repetition, 25);
        });
      }
    };
  }();
  var initialiseCanvasImpl = wrapper.initializeCanvas;
  var resizeCanvasImpl = wrapper.resizeCanvas;
  var clearCanvasImpl = wrapper.clearCanvas;
  var makeStaveImpl = wrapper.makeStave;
  var renderStaveImpl = wrapper.renderStave;
  var getStaveWidthImpl = wrapper.getStaveWidth;
  var renderTextImpl = wrapper.renderText;
  var displayBarBeginRepeatImpl = wrapper.displayBarBeginRepeat;
  var displayBarEndRepeatImpl = wrapper.displayBarEndRepeat;
  var displayBarBothRepeatImpl = wrapper.displayBarBothRepeat;
  var renderBarContentsImpl = wrapper.renderBarContents;
  var displayVoltaImpl = wrapper.displayVolta;
  var addTimeSignatureImpl = wrapper.addTimeSignature;
  var addKeySignatureImpl = wrapper.addKeySignature;
  var addTempoSignatureImpl = wrapper.addTempoSignature;

  // output/Effect.Uncurried/foreign.js
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runEffectFn2 = function runEffectFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };
  var runEffectFn3 = function runEffectFn32(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function() {
            return fn(a, b, c);
          };
        };
      };
    };
  };
  var runEffectFn5 = function runEffectFn52(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(e) {
              return function() {
                return fn(a, b, c, d, e);
              };
            };
          };
        };
      };
    };
  };

  // output/Data.Abc.Canonical/index.js
  var keySignatureAccidental = function(a) {
    if (a instanceof Sharp) {
      return "#";
    }
    ;
    if (a instanceof Flat) {
      return "b";
    }
    ;
    return "";
  };

  // output/VexFlow.Abc.Repetition/index.js
  var fromFoldable4 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var lookup2 = /* @__PURE__ */ lookup(ordString);
  var buildRepetition = function(decoration2) {
    var repetitionMap = fromFoldable4([new Tuple("coda", 2), new Tuple("segno", 4), new Tuple("D.C.", 6), new Tuple("dacoda", 7), new Tuple("dacapo", 8), new Tuple("D.S.", 9), new Tuple("fine", 12)]);
    return fromMaybe(1)(lookup2(decoration2)(repetitionMap));
  };

  // output/VexFlow.Abc.Slur/index.js
  var foldl6 = /* @__PURE__ */ foldl(foldableArray);
  var LeftBracket = /* @__PURE__ */ function() {
    function LeftBracket2(value0) {
      this.value0 = value0;
    }
    ;
    LeftBracket2.create = function(value0) {
      return new LeftBracket2(value0);
    };
    return LeftBracket2;
  }();
  var RightBracket = /* @__PURE__ */ function() {
    function RightBracket2(value0) {
      this.value0 = value0;
    }
    ;
    RightBracket2.create = function(value0) {
      return new RightBracket2(value0);
    };
    return RightBracket2;
  }();
  var SlurStack = /* @__PURE__ */ function() {
    function SlurStack2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SlurStack2.create = function(value0) {
      return function(value1) {
        return new SlurStack2(value0, value1);
      };
    };
    return SlurStack2;
  }();
  var pop2 = function(v) {
    if (v.value0 instanceof Nil) {
      return new Tuple(Nothing.value, new SlurStack(Nil.value, v.value1));
    }
    ;
    if (v.value0 instanceof Cons) {
      return new Tuple(new Just(v.value0.value0), new SlurStack(v.value0.value1, v.value1));
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.Slur (line 78, column 1 - line 78, column 56): " + [v.constructor.name]);
  };
  var push2 = function(slurStack) {
    return function($$new2) {
      if ($$new2 instanceof RightBracket) {
        var v = pop2(slurStack);
        if (v.value0 instanceof Just && v.value0.value0 instanceof LeftBracket) {
          return new SlurStack(v.value1.value0, cons({
            from: v.value0.value0.value0,
            to: $$new2.value0
          })(v.value1.value1));
        }
        ;
        if (v.value0 instanceof Just) {
          return new SlurStack(new Cons($$new2, new Cons(v.value0.value0, v.value1.value0)), v.value1.value1);
        }
        ;
        if (v.value0 instanceof Nothing) {
          return new SlurStack(new Cons($$new2, v.value1.value0), v.value1.value1);
        }
        ;
        throw new Error("Failed pattern match at VexFlow.Abc.Slur (line 64, column 9 - line 70, column 43): " + [v.value0.constructor.name]);
      }
      ;
      return new SlurStack(new Cons($$new2, slurStack.value0), slurStack.value1);
    };
  };
  var empty3 = /* @__PURE__ */ function() {
    return new SlurStack(Nil.value, []);
  }();
  var vexCurves = function(brackets) {
    var v = foldl6(push2)(empty3)(brackets);
    return reverse(v.value1);
  };

  // output/Data.Abc.UnitNote/index.js
  var _headers4 = /* @__PURE__ */ _headers(strongForget);
  var traversed4 = /* @__PURE__ */ traversed(traversableList)(/* @__PURE__ */ wanderForget(monoidFirst));
  var _UnitNoteLength2 = /* @__PURE__ */ _UnitNoteLength(/* @__PURE__ */ choiceForget(monoidFirst));
  var getUnitNoteLength = function(tune) {
    return firstOf(function($14) {
      return _headers4(traversed4(_UnitNoteLength2($14)));
    })(tune);
  };

  // output/Data.Abc.Tempo/index.js
  var _headers5 = /* @__PURE__ */ _headers(strongForget);
  var traversed5 = /* @__PURE__ */ traversed(traversableList);
  var traversed1 = /* @__PURE__ */ traversed5(/* @__PURE__ */ wanderForget(monoidFirst));
  var _Tempo2 = /* @__PURE__ */ _Tempo(/* @__PURE__ */ choiceForget(monoidFirst));
  var getTempoSig = function(tune) {
    return firstOf(function($32) {
      return _headers5(traversed1(_Tempo2($32)));
    })(tune);
  };

  // output/Data.Lens.AffineTraversal/index.js
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var fanout2 = /* @__PURE__ */ fanout(categoryFn)(strongFn);
  var affineTraversal$prime = function(to) {
    return function(dictStrong) {
      var second2 = second(dictStrong);
      return function(dictChoice) {
        var dimap2 = dimap(dictChoice.Profunctor0());
        var right2 = right(dictChoice);
        return function(pab) {
          return dimap2(to)(function(v) {
            return either(identity10)(v.value0)(v.value1);
          })(second2(right2(pab)));
        };
      };
    };
  };
  var affineTraversal = function(set3) {
    return function(pre) {
      return function(dictStrong) {
        return function(dictChoice) {
          return affineTraversal$prime(fanout2(set3)(pre))(dictStrong)(dictChoice);
        };
      };
    };
  };

  // output/Foreign.Object/foreign.js
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Lens.Index/index.js
  var indexMap = function(dictOrd) {
    var update3 = update(dictOrd);
    var lookup6 = lookup(dictOrd);
    return {
      ix: function(k) {
        return function(dictStrong) {
          return function(dictChoice) {
            var set3 = function(s) {
              return function(b) {
                return update3(function(v) {
                  return new Just(b);
                })(k)(s);
              };
            };
            var pre = function(s) {
              return maybe(new Left(s))(Right.create)(lookup6(k)(s));
            };
            return affineTraversal(set3)(pre)(dictStrong)(dictChoice);
          };
        };
      }
    };
  };

  // output/Data.Lens.At/index.js
  var atMap = function(dictOrd) {
    var lookup6 = lookup(dictOrd);
    var $$delete5 = $$delete(dictOrd);
    var insert6 = insert(dictOrd);
    var indexMap2 = indexMap(dictOrd);
    return {
      at: function(k) {
        return function(dictStrong) {
          return lens(lookup6(k))(function(m) {
            return maybe$prime(function(v) {
              return $$delete5(k)(m);
            })(function(v) {
              return insert6(k)(v)(m);
            });
          })(dictStrong);
        };
      },
      Index0: function() {
        return indexMap2;
      }
    };
  };
  var at = function(dict) {
    return dict.at;
  };

  // output/VexFlow.Types/index.js
  var append3 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupRecord()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "beatMarkers";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "chordSymbols";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "contextChanges";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "noteSpecs";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "repetitions";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "slurBrackets";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "tickableContext";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "ties";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "tuplets";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "typesettingSpaces";
    }
  })()(semigroupRecordNil)(semigroupArray))(semigroupArray))(semigroupArray))(tickableSemigroupCtx))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray)));
  var mempty3 = /* @__PURE__ */ mempty(monoidArray);
  var NoTitle = /* @__PURE__ */ function() {
    function NoTitle2() {
    }
    ;
    NoTitle2.value = new NoTitle2();
    return NoTitle2;
  }();
  var TitlePlusOrigin = /* @__PURE__ */ function() {
    function TitlePlusOrigin2() {
    }
    ;
    TitlePlusOrigin2.value = new TitlePlusOrigin2();
    return TitlePlusOrigin2;
  }();
  var Single = /* @__PURE__ */ function() {
    function Single2() {
    }
    ;
    Single2.value = new Single2();
    return Single2;
  }();
  var Double = /* @__PURE__ */ function() {
    function Double2() {
    }
    ;
    Double2.value = new Double2();
    return Double2;
  }();
  var NoLine = /* @__PURE__ */ function() {
    function NoLine2() {
    }
    ;
    NoLine2.value = new NoLine2();
    return NoLine2;
  }();
  var Empty = /* @__PURE__ */ function() {
    function Empty2() {
    }
    ;
    Empty2.value = new Empty2();
    return Empty2;
  }();
  var Partial = /* @__PURE__ */ function() {
    function Partial2() {
    }
    ;
    Partial2.value = new Partial2();
    return Partial2;
  }();
  var Full = /* @__PURE__ */ function() {
    function Full2() {
    }
    ;
    Full2.value = new Full2();
    return Full2;
  }();
  var OverFull = /* @__PURE__ */ function() {
    function OverFull2() {
    }
    ;
    OverFull2.value = new OverFull2();
    return OverFull2;
  }();
  var titleDepth = 60;
  var staveSeparation = 110;
  var staveIndentation = 10;
  var musicSpecSemigroup = {
    append: function(v) {
      return function(v1) {
        return append3(v)(v1);
      };
    }
  };
  var musicSpecMonoid = {
    mempty: {
      noteSpecs: mempty3,
      tuplets: mempty3,
      ties: mempty3,
      tickableContext: /* @__PURE__ */ mempty(tickableMonoidCtx),
      contextChanges: mempty3,
      slurBrackets: mempty3,
      beatMarkers: mempty3,
      repetitions: mempty3,
      typesettingSpaces: mempty3,
      chordSymbols: mempty3
    },
    Semigroup0: function() {
      return musicSpecSemigroup;
    }
  };
  var eqLineThickness = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Single && y instanceof Single) {
          return true;
        }
        ;
        if (x instanceof Double && y instanceof Double) {
          return true;
        }
        ;
        if (x instanceof NoLine && y instanceof NoLine) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var defaultConfig = /* @__PURE__ */ function() {
    return {
      parentElementId: "canvas",
      width: 1600,
      height: 800,
      scale: 0.8,
      isSVG: true,
      titling: TitlePlusOrigin.value,
      noteSeparation: defaultNoteSeparation,
      showChordSymbols: false
    };
  }();

  // output/VexFlow.Abc.Utils/index.js
  var mul3 = /* @__PURE__ */ mul(semiringRational);
  var show4 = /* @__PURE__ */ show(showBigInt);
  var _headers6 = /* @__PURE__ */ _headers(strongForget);
  var traversed6 = /* @__PURE__ */ traversed(traversableList);
  var map18 = /* @__PURE__ */ map(functorMaybe);
  var toRational5 = /* @__PURE__ */ toRational(toRationalInt);
  var eq5 = /* @__PURE__ */ eq(eqRational);
  var greaterThan2 = /* @__PURE__ */ greaterThan(ordRational);
  var foldl7 = /* @__PURE__ */ foldl(foldableNonEmptyList);
  var add12 = /* @__PURE__ */ add(semiringRational);
  var foldl12 = /* @__PURE__ */ foldl(foldableArray);
  var at2 = /* @__PURE__ */ at(/* @__PURE__ */ atMap(ordString));
  var join3 = /* @__PURE__ */ join(bindMaybe);
  var traversed22 = /* @__PURE__ */ traversed6(/* @__PURE__ */ wanderForget(monoidLast));
  var _Voice2 = /* @__PURE__ */ _Voice(/* @__PURE__ */ choiceForget(monoidLast));
  var _properties2 = /* @__PURE__ */ _properties(strongForget);
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var updateAbcContext = function(abcContext) {
    return function(change) {
      if (change instanceof MeterChange) {
        return {
          keySignature: abcContext.keySignature,
          mTempo: abcContext.mTempo,
          unitNoteLength: abcContext.unitNoteLength,
          clef: abcContext.clef,
          staveNo: abcContext.staveNo,
          accumulatedStaveWidth: abcContext.accumulatedStaveWidth,
          isMidVolta: abcContext.isMidVolta,
          maxWidth: abcContext.maxWidth,
          pendingRepeatBegin: abcContext.pendingRepeatBegin,
          noteSeparation: abcContext.noteSeparation,
          showChordSymbols: abcContext.showChordSymbols,
          timeSignature: change.value0,
          isNewTimeSignature: true,
          beatDuration: beatDuration({
            numerator: change.value0.numerator,
            denominator: change.value0.denominator
          })
        };
      }
      ;
      if (change instanceof KeyChange) {
        return {
          timeSignature: abcContext.timeSignature,
          mTempo: abcContext.mTempo,
          unitNoteLength: abcContext.unitNoteLength,
          clef: abcContext.clef,
          staveNo: abcContext.staveNo,
          accumulatedStaveWidth: abcContext.accumulatedStaveWidth,
          isMidVolta: abcContext.isMidVolta,
          maxWidth: abcContext.maxWidth,
          pendingRepeatBegin: abcContext.pendingRepeatBegin,
          beatDuration: abcContext.beatDuration,
          noteSeparation: abcContext.noteSeparation,
          showChordSymbols: abcContext.showChordSymbols,
          keySignature: change.value0.keySignature,
          isNewTimeSignature: false
        };
      }
      ;
      if (change instanceof UnitNoteChange) {
        return {
          timeSignature: abcContext.timeSignature,
          keySignature: abcContext.keySignature,
          mTempo: abcContext.mTempo,
          clef: abcContext.clef,
          staveNo: abcContext.staveNo,
          accumulatedStaveWidth: abcContext.accumulatedStaveWidth,
          isMidVolta: abcContext.isMidVolta,
          maxWidth: abcContext.maxWidth,
          pendingRepeatBegin: abcContext.pendingRepeatBegin,
          beatDuration: abcContext.beatDuration,
          noteSeparation: abcContext.noteSeparation,
          showChordSymbols: abcContext.showChordSymbols,
          unitNoteLength: change.value0,
          isNewTimeSignature: false
        };
      }
      ;
      if (change instanceof ClefChange) {
        return {
          timeSignature: abcContext.timeSignature,
          keySignature: abcContext.keySignature,
          mTempo: abcContext.mTempo,
          unitNoteLength: abcContext.unitNoteLength,
          staveNo: abcContext.staveNo,
          accumulatedStaveWidth: abcContext.accumulatedStaveWidth,
          isMidVolta: abcContext.isMidVolta,
          maxWidth: abcContext.maxWidth,
          pendingRepeatBegin: abcContext.pendingRepeatBegin,
          beatDuration: abcContext.beatDuration,
          noteSeparation: abcContext.noteSeparation,
          showChordSymbols: abcContext.showChordSymbols,
          clef: change.value0,
          isNewTimeSignature: false
        };
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Utils (line 161, column 3 - line 185, column 10): " + [change.constructor.name]);
    };
  };
  var noteTicks = function(unitNoteLength2) {
    return function(d) {
      return round2(toNumber3(mul3(mul3(unitNoteLength2)(d))(fromInt2(128))));
    };
  };
  var vexDuration = function(unitNoteLength2) {
    return function(d) {
      var v = noteTicks(unitNoteLength2)(d);
      if (v === 128) {
        return new Right({
          vexDurString: "w",
          dots: 0
        });
      }
      ;
      if (v === 112) {
        return new Right({
          vexDurString: "h",
          dots: 2
        });
      }
      ;
      if (v === 96) {
        return new Right({
          vexDurString: "h",
          dots: 1
        });
      }
      ;
      if (v === 64) {
        return new Right({
          vexDurString: "h",
          dots: 0
        });
      }
      ;
      if (v === 56) {
        return new Right({
          vexDurString: "q",
          dots: 2
        });
      }
      ;
      if (v === 48) {
        return new Right({
          vexDurString: "q",
          dots: 1
        });
      }
      ;
      if (v === 32) {
        return new Right({
          vexDurString: "q",
          dots: 0
        });
      }
      ;
      if (v === 28) {
        return new Right({
          vexDurString: "8",
          dots: 2
        });
      }
      ;
      if (v === 24) {
        return new Right({
          vexDurString: "8",
          dots: 1
        });
      }
      ;
      if (v === 16) {
        return new Right({
          vexDurString: "8",
          dots: 0
        });
      }
      ;
      if (v === 14) {
        return new Right({
          vexDurString: "16",
          dots: 2
        });
      }
      ;
      if (v === 12) {
        return new Right({
          vexDurString: "16",
          dots: 1
        });
      }
      ;
      if (v === 8) {
        return new Right({
          vexDurString: "16",
          dots: 0
        });
      }
      ;
      if (v === 7) {
        return new Right({
          vexDurString: "32",
          dots: 2
        });
      }
      ;
      if (v === 6) {
        return new Right({
          vexDurString: "32",
          dots: 1
        });
      }
      ;
      if (v === 4) {
        return new Right({
          vexDurString: "32",
          dots: 0
        });
      }
      ;
      if (v === 3) {
        return new Right({
          vexDurString: "64",
          dots: 1
        });
      }
      ;
      if (v === 2) {
        return new Right({
          vexDurString: "64",
          dots: 0
        });
      }
      ;
      return new Left("too long or too dotted duration: " + (show4(numerator2(d)) + ("/" + show4(denominator2(d)))));
    };
  };
  var noteDotCount = function(ctx) {
    return function(abcNote2) {
      var v = vexDuration(ctx.unitNoteLength)(abcNote2.duration);
      if (v instanceof Right) {
        return v.value0.dots;
      }
      ;
      return 0;
    };
  };
  var nextStaveNo = function(v) {
    if (v instanceof Nothing) {
      return new Just(0);
    }
    ;
    if (v instanceof Just) {
      return new Just(v.value0 + 1 | 0);
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.Utils (line 195, column 1 - line 195, column 38): " + [v.constructor.name]);
  };
  var isEmptyMusicSpec = function(v) {
    return $$null(v.noteSpecs) && $$null(v.repetitions);
  };
  var getBarFill = function(timeSignature2) {
    return function(unitNoteLength2) {
      return function(v) {
        var signatureDuration = toRational5(timeSignature2.numerator)(timeSignature2.denominator);
        var barDuration = mul3(unitNoteLength2)(v.value2);
        var $61 = eq5(barDuration)(fromInt2(0));
        if ($61) {
          return Empty.value;
        }
        ;
        var $62 = eq5(barDuration)(signatureDuration);
        if ($62) {
          return Full.value;
        }
        ;
        var $63 = greaterThan2(barDuration)(signatureDuration);
        if ($63) {
          return OverFull.value;
        }
        ;
        return Partial.value;
      };
    };
  };
  var compoundVexDuration = function(vexDur) {
    var dStr = fromCharArray(replicate2(vexDur.dots)("d"));
    return vexDur.vexDurString + dStr;
  };
  var cMajor = defaultKey;
  var buildTempo = function(bpm) {
    return function(d) {
      var v = vexDuration(fromInt2(1))(d);
      if (v instanceof Right) {
        return new Right({
          duration: v.value0.vexDurString,
          dots: v.value0.dots,
          bpm
        });
      }
      ;
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Utils (line 102, column 3 - line 106, column 15): " + [v.constructor.name]);
    };
  };
  var tempoMarking = function(tempoSig) {
    var tempoNoteLength = foldl7(add12)(fromInt2(0))(tempoSig.noteLengths);
    return hush(buildTempo(tempoSig.bpm)(tempoNoteLength));
  };
  var applyContextChanges = function(abcContext) {
    return function(eSpec) {
      if (eSpec instanceof Right) {
        return foldl12(updateAbcContext)(abcContext)(eSpec.value0.contextChanges);
      }
      ;
      return abcContext;
    };
  };
  var _clef = function(dictStrong) {
    return at2("clef")(dictStrong);
  };
  var _clef1 = /* @__PURE__ */ _clef(strongForget);
  var getVoiceClef = function(tune) {
    var f = function(s) {
      if (s === "Alto") {
        return Alto.value;
      }
      ;
      if (s === "alto") {
        return Alto.value;
      }
      ;
      if (s === "Tenor") {
        return Tenor.value;
      }
      ;
      if (s === "tenor") {
        return Tenor.value;
      }
      ;
      if (s === "Bass") {
        return Bass.value;
      }
      ;
      if (s === "bass") {
        return Bass.value;
      }
      ;
      return Treble.value;
    };
    var clefString = join3(lastOf(function($77) {
      return _headers6(traversed22(_Voice2(_properties2(_clef1($77)))));
    })(tune));
    return map18(f)(clefString);
  };
  var initialAbcContext = function(tune) {
    return function(config2) {
      var unitNoteLength2 = fromMaybe(toRational5(1)(8))(getUnitNoteLength(tune));
      var timeSignature2 = getDefaultedMeter(tune);
      var modifiedKeySignature = fromMaybe(cMajor)(map18(identity11)(getKeySig(tune)));
      var mTempo = maybe(Nothing.value)(tempoMarking)(getTempoSig(tune));
      var clef = fromMaybe(Treble.value)(getVoiceClef(tune));
      var $74 = $$null2(modifiedKeySignature.modifications);
      if ($74) {
        return new Right({
          timeSignature: timeSignature2,
          keySignature: modifiedKeySignature.keySignature,
          mTempo,
          unitNoteLength: unitNoteLength2,
          clef,
          staveNo: Nothing.value,
          accumulatedStaveWidth: staveIndentation,
          isMidVolta: false,
          isNewTimeSignature: true,
          maxWidth: round2(toNumber2(config2.width - staveIndentation | 0) / config2.scale),
          pendingRepeatBegin: false,
          beatDuration: beatDuration(timeSignature2),
          noteSeparation: config2.noteSeparation,
          showChordSymbols: config2.showChordSymbols
        });
      }
      ;
      return new Left("modifications to standard key signatures are not supported");
    };
  };

  // output/VexFlow.Abc.Translate/index.js
  var foldl8 = /* @__PURE__ */ foldl(foldableList);
  var show5 = /* @__PURE__ */ show(showPitchClass);
  var lookup4 = /* @__PURE__ */ lookup(ordString);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var replicate3 = /* @__PURE__ */ replicate(unfoldableArray);
  var map19 = /* @__PURE__ */ map(functorEither);
  var fromMaybe3 = /* @__PURE__ */ fromMaybe2(unfoldableArray);
  var mempty4 = /* @__PURE__ */ mempty(musicSpecMonoid);
  var map110 = /* @__PURE__ */ map(functorArray);
  var fromFoldable5 = /* @__PURE__ */ fromFoldable(foldableList);
  var toUnfoldable7 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var show1 = /* @__PURE__ */ show(showInt);
  var show23 = /* @__PURE__ */ show(showClef);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeEither);
  var mul4 = /* @__PURE__ */ mul(semiringRational);
  var mempty1 = /* @__PURE__ */ mempty(monoidArray);
  var ornaments = function(decorations2) {
    var f = function(acc) {
      return function(decoration2) {
        if (decoration2 === "T") {
          return cons("tr")(acc);
        }
        ;
        if (decoration2 === "trill") {
          return cons("tr")(acc);
        }
        ;
        if (decoration2 === "turn") {
          return cons("turn")(acc);
        }
        ;
        if (decoration2 === "P") {
          return cons("upmordent")(acc);
        }
        ;
        if (decoration2 === "uppermordent") {
          return cons("upmordent")(acc);
        }
        ;
        if (decoration2 === "M") {
          return cons("mordent")(acc);
        }
        ;
        if (decoration2 === "lowermordent") {
          return cons("mordent")(acc);
        }
        ;
        return acc;
      };
    };
    return foldl8(f)([])(decorations2);
  };
  var keySignature2 = function(ks) {
    var newks = function() {
      if (ks.mode instanceof Major) {
        return ks;
      }
      ;
      if (ks.mode instanceof Minor) {
        return ks;
      }
      ;
      return normaliseModalKey(ks);
    }();
    var modeStr = function() {
      if (newks.mode instanceof Minor) {
        return "m";
      }
      ;
      return "";
    }();
    return show5(newks.pitchClass) + (keySignatureAccidental(newks.accidental) + modeStr);
  };
  var headerChange = function(h) {
    if (h instanceof Key) {
      return [new KeyChange(h.value0)];
    }
    ;
    if (h instanceof UnitNoteLength) {
      return [new UnitNoteChange(h.value0)];
    }
    ;
    if (h instanceof Meter) {
      if (h.value0 instanceof Just) {
        return [new MeterChange(h.value0.value0)];
      }
      ;
      return [];
    }
    ;
    if (h instanceof Voice) {
      var v = lookup4("clef")(h.value0.properties);
      if (v instanceof Just && v.value0 === "Bass") {
        return [new ClefChange(Bass.value)];
      }
      ;
      if (v instanceof Just && v.value0 === "bass") {
        return [new ClefChange(Bass.value)];
      }
      ;
      if (v instanceof Just && v.value0 === "Tenor") {
        return [new ClefChange(Tenor.value)];
      }
      ;
      if (v instanceof Just && v.value0 === "tenor") {
        return [new ClefChange(Tenor.value)];
      }
      ;
      if (v instanceof Just && v.value0 === "Alto") {
        return [new ClefChange(Alto.value)];
      }
      ;
      if (v instanceof Just && v.value0 === "alto") {
        return [new ClefChange(Alto.value)];
      }
      ;
      return [new ClefChange(Treble.value)];
    }
    ;
    return [];
  };
  var buildSlurBrackets = function(noteIndex) {
    return function(startCount) {
      return function(endCount) {
        return append13(replicate3(startCount)(new LeftBracket(noteIndex)))(replicate3(endCount)(new RightBracket(noteIndex)));
      };
    };
  };
  var buildTupletPrefaceSlurs = function(noteIndex) {
    return function(startCount) {
      return buildSlurBrackets(noteIndex)(startCount)(0);
    };
  };
  var buildMusicSpecFromN = function(tCtx) {
    return function(noteIndex) {
      return function(mBeatMarker) {
        return function(isTied) {
          return function(slurStartCount) {
            return function(slurEndCount) {
              return function(ens) {
                return map19(function(ns) {
                  return {
                    noteSpecs: [ns],
                    tuplets: [],
                    ties: function() {
                      if (isTied) {
                        return [noteIndex];
                      }
                      ;
                      return [];
                    }(),
                    tickableContext: tCtx,
                    contextChanges: [],
                    slurBrackets: buildSlurBrackets(noteIndex)(slurStartCount)(slurEndCount),
                    beatMarkers: fromMaybe3(mBeatMarker),
                    repetitions: [],
                    typesettingSpaces: [],
                    chordSymbols: []
                  };
                })(ens);
              };
            };
          };
        };
      };
    };
  };
  var buildMusicSpecFromDecorations = function(decorations2) {
    return function(noteIndex) {
      var repetitions = map110(buildRepetition)(fromFoldable5(decorations2));
      return {
        noteSpecs: mempty4.noteSpecs,
        tuplets: mempty4.tuplets,
        ties: mempty4.ties,
        tickableContext: mempty4.tickableContext,
        contextChanges: mempty4.contextChanges,
        slurBrackets: mempty4.slurBrackets,
        beatMarkers: mempty4.beatMarkers,
        chordSymbols: mempty4.chordSymbols,
        repetitions,
        typesettingSpaces: [noteIndex]
      };
    };
  };
  var buildMusicSpecFromContextChange = function(contextChanges) {
    return {
      noteSpecs: mempty4.noteSpecs,
      tuplets: mempty4.tuplets,
      ties: mempty4.ties,
      tickableContext: mempty4.tickableContext,
      slurBrackets: mempty4.slurBrackets,
      beatMarkers: mempty4.beatMarkers,
      repetitions: mempty4.repetitions,
      typesettingSpaces: mempty4.typesettingSpaces,
      chordSymbols: mempty4.chordSymbols,
      contextChanges
    };
  };
  var buildMusicSpecFromChordSymbol = function(symbol) {
    return function(noteIndex) {
      return {
        noteSpecs: mempty4.noteSpecs,
        tuplets: mempty4.tuplets,
        ties: mempty4.ties,
        tickableContext: mempty4.tickableContext,
        contextChanges: mempty4.contextChanges,
        slurBrackets: mempty4.slurBrackets,
        beatMarkers: mempty4.beatMarkers,
        repetitions: mempty4.repetitions,
        typesettingSpaces: mempty4.typesettingSpaces,
        chordSymbols: [{
          name: symbol.name,
          noteIndex
        }]
      };
    };
  };
  var buildInterTupletSlurs = function(noteIndex) {
    return function(tupletNotes) {
      var f = function(pos) {
        return function(rOrN) {
          if (rOrN instanceof Left) {
            return [];
          }
          ;
          if (rOrN instanceof Right) {
            return buildSlurBrackets(noteIndex + pos | 0)(rOrN.value0.leftSlurs)(rOrN.value0.rightSlurs);
          }
          ;
          throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 529, column 7 - line 532, column 79): " + [rOrN.constructor.name]);
        };
      };
      return concat(mapWithIndex2(f)(toUnfoldable7(tupletNotes)));
    };
  };
  var buildTupletSlurs = function(noteIndex) {
    return function(prefaceSlurCount) {
      return function(tupletNotes) {
        return append13(buildTupletPrefaceSlurs(noteIndex)(prefaceSlurCount))(buildInterTupletSlurs(noteIndex)(tupletNotes));
      };
    };
  };
  var articulations = function(artics) {
    var f = function(acc) {
      return function(decoration2) {
        if (decoration2 === ".") {
          return cons("a.")(acc);
        }
        ;
        if (decoration2 === "upbow") {
          return cons("a|")(acc);
        }
        ;
        if (decoration2 === "u") {
          return cons("a|")(acc);
        }
        ;
        if (decoration2 === "downbow") {
          return cons("am")(acc);
        }
        ;
        if (decoration2 === "v") {
          return cons("am")(acc);
        }
        ;
        if (decoration2 === "L") {
          return cons("a>")(acc);
        }
        ;
        if (decoration2 === "accent") {
          return cons("a>")(acc);
        }
        ;
        if (decoration2 === "emphasis") {
          return cons("a>")(acc);
        }
        ;
        if (decoration2 === "H") {
          return cons("a@a")(acc);
        }
        ;
        if (decoration2 === "fermata") {
          return cons("a@a")(acc);
        }
        ;
        if (decoration2 === "tenuto") {
          return cons("a-")(acc);
        }
        ;
        return acc;
      };
    };
    return foldl8(f)([])(artics);
  };
  var accidental2 = function(v) {
    if (v instanceof Sharp) {
      return "#";
    }
    ;
    if (v instanceof Flat) {
      return "b";
    }
    ;
    if (v instanceof DoubleSharp) {
      return "##";
    }
    ;
    if (v instanceof DoubleFlat) {
      return "bb";
    }
    ;
    if (v instanceof Natural) {
      return "n";
    }
    ;
    if (v instanceof Implicit) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 61, column 1 - line 61, column 35): " + [v.constructor.name]);
  };
  var noteAccidental = function(abcNote2) {
    return accidental2(abcNote2.accidental);
  };
  var pitch2 = function(pc) {
    return function(acc) {
      return function(oct) {
        return toLower(show5(pc)) + (accidental2(acc) + ("/" + show1(oct)));
      };
    };
  };
  var notePitch = function(abcNote2) {
    return pitch2(abcNote2.pitchClass)(abcNote2.accidental)(abcNote2.octave - 1 | 0);
  };
  var chord2 = function(context) {
    return function(abcChord2) {
      var representativeNote = head2(abcChord2.notes);
      var keys3 = map110(notePitch)(toUnfoldable7(abcChord2.notes));
      var dotCount = noteDotCount(context)(representativeNote);
      var eVexDur = vexDuration(context.unitNoteLength)(representativeNote.duration);
      var accidentals = map110(noteAccidental)(toUnfoldable7(abcChord2.notes));
      if (eVexDur instanceof Right) {
        var vexNote = {
          clef: show23(context.clef),
          keys: keys3,
          duration: compoundVexDuration(eVexDur.value0),
          auto_stem: true
        };
        return new Right({
          vexNote,
          accidentals,
          dotCount,
          graceKeys: [],
          graceAccidentals: [],
          ornaments: [],
          articulations: [],
          noteTicks: noteTicks(context.unitNoteLength)(representativeNote.duration),
          chordSymbol: ""
        });
      }
      ;
      if (eVexDur instanceof Left) {
        return new Left(eVexDur.value0);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 263, column 5 - line 284, column 23): " + [eVexDur.constructor.name]);
    };
  };
  var graceableNote2 = function(context) {
    return function(gn) {
      var key2 = notePitch(gn.abcNote);
      var graceNotes = maybe([])(function(grace2) {
        return toUnfoldable7(grace2.notes);
      })(gn.maybeGrace);
      var graceKeys = map110(notePitch)(graceNotes);
      var graceAccidentals = map110(noteAccidental)(graceNotes);
      var eVexDur = vexDuration(context.unitNoteLength)(gn.abcNote.duration);
      if (eVexDur instanceof Right) {
        var vexNote = {
          clef: show23(context.clef),
          keys: [key2],
          duration: compoundVexDuration(eVexDur.value0),
          auto_stem: true
        };
        return new Right({
          vexNote,
          accidentals: [accidental2(gn.abcNote.accidental)],
          dotCount: eVexDur.value0.dots,
          graceKeys,
          graceAccidentals,
          ornaments: ornaments(gn.decorations),
          articulations: articulations(gn.decorations),
          noteTicks: noteTicks(context.unitNoteLength)(gn.abcNote.duration),
          chordSymbol: ""
        });
      }
      ;
      if (eVexDur instanceof Left) {
        return new Left(eVexDur.value0);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 181, column 5 - line 202, column 23): " + [eVexDur.constructor.name]);
    };
  };
  var restPitch = function(v) {
    if (v instanceof Bass) {
      return pitch2(D.value)(Implicit.value)(3);
    }
    ;
    if (v instanceof Tenor) {
      return pitch2(A.value)(Implicit.value)(3);
    }
    ;
    if (v instanceof Alto) {
      return pitch2(C.value)(Implicit.value)(4);
    }
    ;
    return pitch2(B.value)(Implicit.value)(4);
  };
  var rest2 = function(context) {
    return function(abcRest2) {
      var key2 = restPitch(context.clef);
      var eVexDur = vexDuration(context.unitNoteLength)(abcRest2.duration);
      if (eVexDur instanceof Right) {
        var vexNote = {
          clef: show23(context.clef),
          keys: [key2],
          duration: compoundVexDuration(eVexDur.value0) + "r",
          auto_stem: true
        };
        return new Right({
          vexNote,
          accidentals: [],
          dotCount: eVexDur.value0.dots,
          graceKeys: [],
          graceAccidentals: [],
          ornaments: [],
          articulations: [],
          noteTicks: noteTicks(context.unitNoteLength)(abcRest2.duration),
          chordSymbol: ""
        });
      }
      ;
      if (eVexDur instanceof Left) {
        return new Left(eVexDur.value0);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 214, column 5 - line 235, column 23): " + [eVexDur.constructor.name]);
    };
  };
  var restOrNote2 = function(context) {
    return function(rOrn) {
      if (rOrn instanceof Left) {
        return rest2(context)(rOrn.value0);
      }
      ;
      if (rOrn instanceof Right) {
        return graceableNote2(context)(rOrn.value0);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 316, column 3 - line 320, column 31): " + [rOrn.constructor.name]);
    };
  };
  var tuplet2 = function(context) {
    return function(startOffset) {
      return function(signature) {
        return function(rns) {
          var vexTuplet = {
            p: signature.p,
            q: signature.q,
            startPos: startOffset,
            endPos: startOffset + length(rns) | 0
          };
          var isTied = function() {
            var v = last(rns);
            if (v instanceof Just && v.value0 instanceof Right) {
              return v.value0.value0.abcNote.tied;
            }
            ;
            return false;
          }();
          var enoteSpecs = sequence2(map110(restOrNote2(context))(rns));
          if (enoteSpecs instanceof Right) {
            return new Right({
              vexTuplet,
              noteSpecs: enoteSpecs.value0,
              tied: isTied
            });
          }
          ;
          if (enoteSpecs instanceof Left) {
            return new Left(enoteSpecs.value0);
          }
          ;
          throw new Error("Failed pattern match at VexFlow.Abc.Translate (line 304, column 5 - line 312, column 15): " + [enoteSpecs.constructor.name]);
        };
      };
    };
  };
  var music = function(context) {
    return function(tickablePosition) {
      return function(noteIndex) {
        return function(phraseDuration) {
          return function(m) {
            var tickableContext = getTickableContext(m);
            var barFraction = mul4(phraseDuration)(context.unitNoteLength);
            var mBeatMarker = exactBeatNumber(barFraction)(context.beatDuration)(noteIndex);
            if (m instanceof Note) {
              return buildMusicSpecFromN(tickableContext)(noteIndex)(mBeatMarker)(m.value0.abcNote.tied)(m.value0.leftSlurs)(m.value0.rightSlurs)(graceableNote2(context)(m.value0));
            }
            ;
            if (m instanceof Rest) {
              return buildMusicSpecFromN(tickableContext)(noteIndex)(mBeatMarker)(false)(0)(0)(rest2(context)(m.value0));
            }
            ;
            if (m instanceof Chord) {
              return buildMusicSpecFromN(tickableContext)(noteIndex)(mBeatMarker)(false)(m.value0.leftSlurs)(m.value0.rightSlurs)(chord2(context)(m.value0));
            }
            ;
            if (m instanceof Tuplet) {
              var eRes = tuplet2(context)(tickablePosition)(m.value0.signature)(toUnfoldable7(m.value0.restsOrNotes));
              return map19(function(tupletSpec) {
                return {
                  noteSpecs: tupletSpec.noteSpecs,
                  tuplets: [tupletSpec.vexTuplet],
                  ties: function() {
                    if (tupletSpec.tied) {
                      return [(noteIndex + length(tupletSpec.noteSpecs) | 0) - 1 | 0];
                    }
                    ;
                    return [];
                  }(),
                  tickableContext,
                  contextChanges: mempty1,
                  slurBrackets: buildTupletSlurs(noteIndex)(m.value0.leftSlurs)(m.value0.restsOrNotes),
                  beatMarkers: fromMaybe3(mBeatMarker),
                  repetitions: mempty1,
                  typesettingSpaces: mempty1,
                  chordSymbols: mempty1
                };
              })(eRes);
            }
            ;
            if (m instanceof ChordSymbol) {
              if (context.showChordSymbols) {
                return new Right(buildMusicSpecFromChordSymbol(m.value0)(noteIndex));
              }
              ;
              return new Right(mempty4);
            }
            ;
            if (m instanceof Inline) {
              return new Right(buildMusicSpecFromContextChange(headerChange(m.value0)));
            }
            ;
            if (m instanceof DecoratedSpace) {
              return new Right(buildMusicSpecFromDecorations(m.value0)(noteIndex));
            }
            ;
            return new Right(mempty4);
          };
        };
      };
    };
  };

  // output/VexFlow.ApiBindings/index.js
  var pure7 = /* @__PURE__ */ pure(applicativeEffect);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var show6 = /* @__PURE__ */ show(showInt);
  var renderText = /* @__PURE__ */ runEffectFn5(renderTextImpl);
  var renderStave = /* @__PURE__ */ runEffectFn2(renderStaveImpl);
  var renderBarContents = /* @__PURE__ */ runEffectFn5(renderBarContentsImpl);
  var makeStave = /* @__PURE__ */ runEffectFn3(makeStaveImpl);
  var newStave = function(staveCnfg) {
    return function(clefString) {
      return function(ks) {
        return makeStave(staveCnfg)(clefString)(keySignature2(ks));
      };
    };
  };
  var initialiseCanvas = /* @__PURE__ */ runEffectFn1(initialiseCanvasImpl);
  var displayVolta = /* @__PURE__ */ runEffectFn2(displayVoltaImpl);
  var processVolta = function(staveBar) {
    return function(mVolta) {
      if (mVolta instanceof Just) {
        return displayVolta(staveBar)(mVolta.value0);
      }
      ;
      return pure7(unit);
    };
  };
  var displayBarEndRepeat = /* @__PURE__ */ runEffectFn1(displayBarEndRepeatImpl);
  var processBarEndRepeat = function(staveBar) {
    return function(isRepeat) {
      return when2(isRepeat)(displayBarEndRepeat(staveBar));
    };
  };
  var displayBarBeginRepeat = /* @__PURE__ */ runEffectFn2(displayBarBeginRepeatImpl);
  var processBarBeginRepeat = function(staveBar) {
    return function(barLine) {
      if (barLine.startRepeats === 0) {
        return pure7(unit);
      }
      ;
      if (barLine.startRepeats === 1) {
        return displayBarBeginRepeat(staveBar)("");
      }
      ;
      return displayBarBeginRepeat(staveBar)("play " + (show6(barLine.startRepeats + 1 | 0) + " times"));
    };
  };
  var addTimeSignature = /* @__PURE__ */ runEffectFn2(addTimeSignatureImpl);
  var addTempoSignature = /* @__PURE__ */ runEffectFn2(addTempoSignatureImpl);
  var addTempoMarking = function(stave) {
    return function(mTempo) {
      return maybe(pure7(unit))(addTempoSignature(stave))(mTempo);
    };
  };
  var addKeySignature = /* @__PURE__ */ runEffectFn2(addKeySignatureImpl);
  var displayContextChange = function(staveBar) {
    return function(contextChange) {
      if (contextChange instanceof MeterChange) {
        return addTimeSignature(staveBar)(contextChange.value0);
      }
      ;
      if (contextChange instanceof KeyChange) {
        return addKeySignature(staveBar)(keySignature2(contextChange.value0.keySignature));
      }
      ;
      if (contextChange instanceof UnitNoteChange) {
        return pure7(unit);
      }
      ;
      if (contextChange instanceof ClefChange) {
        return pure7(unit);
      }
      ;
      throw new Error("Failed pattern match at VexFlow.ApiBindings (line 102, column 3 - line 114, column 16): " + [contextChange.constructor.name]);
    };
  };

  // output/Data.Abc.Utils/index.js
  var toRational6 = /* @__PURE__ */ toRational(toRationalInt);
  var all4 = /* @__PURE__ */ all(foldableList)(heytingAlgebraBoolean);
  var isEmptyStave = function(bars2) {
    var isEmptyBar = function(bar3) {
      var f = function(music$prime) {
        if (music$prime instanceof Spacer) {
          return true;
        }
        ;
        if (music$prime instanceof Ignore) {
          return true;
        }
        ;
        if (music$prime instanceof Continuation) {
          return true;
        }
        ;
        return false;
      };
      return all4(f)(bar3.music) || $$null2(bar3.music);
    };
    return all4(isEmptyBar)(bars2);
  };
  var dotFactor = function(i) {
    if (i === 1) {
      return toRational6(1)(2);
    }
    ;
    if (i === 2) {
      return toRational6(3)(4);
    }
    ;
    if (i === 3) {
      return toRational6(7)(8);
    }
    ;
    return toRational6(0)(1);
  };

  // output/Data.Abc.Normaliser/index.js
  var map20 = /* @__PURE__ */ map(functorNonEmptyList);
  var mul5 = /* @__PURE__ */ mul(semiringRational);
  var toRational7 = /* @__PURE__ */ toRational(toRationalInt);
  var add2 = /* @__PURE__ */ add(semiringRational);
  var sub2 = /* @__PURE__ */ sub(ringRational);
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var map111 = /* @__PURE__ */ map(functorList);
  var normaliseChord = function(abcChord2) {
    var v = toNumber3(abcChord2.duration);
    if (v === 1) {
      return abcChord2;
    }
    ;
    var notes2 = map20(function(n) {
      return {
        accidental: n.accidental,
        octave: n.octave,
        pitchClass: n.pitchClass,
        tied: n.tied,
        duration: mul5(n.duration)(abcChord2.duration)
      };
    })(abcChord2.notes);
    return {
      leftSlurs: abcChord2.leftSlurs,
      decorations: abcChord2.decorations,
      notes: notes2,
      duration: toRational7(1)(1),
      rightSlurs: abcChord2.rightSlurs
    };
  };
  var normaliseBrokenRhythm = function(broken) {
    return function(rorNa) {
      return function(rorNb) {
        var factorb = function() {
          if (broken instanceof LeftArrow) {
            return add2(fromInt2(1))(dotFactor(broken.value0));
          }
          ;
          if (broken instanceof RightArrow) {
            return sub2(fromInt2(1))(dotFactor(broken.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Abc.Normaliser (line 72, column 7 - line 76, column 38): " + [broken.constructor.name]);
        }();
        var musicb = function() {
          if (rorNb instanceof Left) {
            return new Rest({
              duration: mul5(rorNb.value0.duration)(factorb)
            });
          }
          ;
          if (rorNb instanceof Right) {
            var newAbcNote = {
              accidental: rorNb.value0.abcNote.accidental,
              octave: rorNb.value0.abcNote.octave,
              pitchClass: rorNb.value0.abcNote.pitchClass,
              tied: rorNb.value0.abcNote.tied,
              duration: mul5(rorNb.value0.abcNote.duration)(factorb)
            };
            return new Note({
              maybeGrace: rorNb.value0.maybeGrace,
              leftSlurs: rorNb.value0.leftSlurs,
              decorations: rorNb.value0.decorations,
              rightSlurs: rorNb.value0.rightSlurs,
              abcNote: newAbcNote
            });
          }
          ;
          throw new Error("Failed pattern match at Data.Abc.Normaliser (line 87, column 7 - line 94, column 45): " + [rorNb.constructor.name]);
        }();
        var factora = function() {
          if (broken instanceof LeftArrow) {
            return sub2(fromInt2(1))(dotFactor(broken.value0));
          }
          ;
          if (broken instanceof RightArrow) {
            return add2(fromInt2(1))(dotFactor(broken.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Abc.Normaliser (line 66, column 7 - line 70, column 38): " + [broken.constructor.name]);
        }();
        var musica = function() {
          if (rorNa instanceof Left) {
            return new Rest({
              duration: mul5(rorNa.value0.duration)(factora)
            });
          }
          ;
          if (rorNa instanceof Right) {
            var newAbcNote = {
              accidental: rorNa.value0.abcNote.accidental,
              octave: rorNa.value0.abcNote.octave,
              pitchClass: rorNa.value0.abcNote.pitchClass,
              tied: rorNa.value0.abcNote.tied,
              duration: mul5(rorNa.value0.abcNote.duration)(factora)
            };
            return new Note({
              maybeGrace: rorNa.value0.maybeGrace,
              leftSlurs: rorNa.value0.leftSlurs,
              decorations: rorNa.value0.decorations,
              rightSlurs: rorNa.value0.rightSlurs,
              abcNote: newAbcNote
            });
          }
          ;
          throw new Error("Failed pattern match at Data.Abc.Normaliser (line 78, column 7 - line 85, column 45): " + [rorNa.constructor.name]);
        }();
        return new Tuple(musica, musicb);
      };
    };
  };
  var normaliseMusic = function(next2) {
    return function(acc) {
      if (next2 instanceof BrokenRhythmPair) {
        var v = normaliseBrokenRhythm(next2.value1)(next2.value0)(next2.value2);
        return new Cons(v.value0, new Cons(v.value1, acc));
      }
      ;
      if (next2 instanceof Chord) {
        return new Cons(new Chord(normaliseChord(next2.value0)), acc);
      }
      ;
      return new Cons(next2, acc);
    };
  };
  var normaliseBar = function(bar3) {
    var newMusic = foldr3(normaliseMusic)(Nil.value)(bar3.music);
    return {
      decorations: bar3.decorations,
      startLine: bar3.startLine,
      music: newMusic
    };
  };
  var normaliseBarList = /* @__PURE__ */ map111(normaliseBar);
  var normaliseBodyPart = function(bp) {
    if (bp instanceof Score) {
      return new Score(normaliseBarList(bp.value0));
    }
    ;
    return bp;
  };
  var normaliseTuneBody = /* @__PURE__ */ map111(normaliseBodyPart);
  var normalise = function(t) {
    return {
      headers: t.headers,
      body: normaliseTuneBody(t.body)
    };
  };

  // output/VexFlow.Abc.Volta/index.js
  var notEq2 = /* @__PURE__ */ notEq(eqThickness);
  var intercalateMap2 = /* @__PURE__ */ intercalateMap(foldable1NonEmptyList)(semigroupString);
  var show7 = /* @__PURE__ */ show(showVolta);
  var isEndVolta = function(barLine) {
    if (barLine.iteration instanceof Nothing) {
      return notEq2(barLine.thickness)(Thin.value) && notEq2(barLine.thickness)(Invisible.value) || (barLine.endRepeats + barLine.startRepeats | 0) > 0;
    }
    ;
    if (barLine.iteration instanceof Just) {
      return true;
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.Volta (line 105, column 3 - line 110, column 11): " + [barLine.iteration.constructor.name]);
  };
  var isMidVolta = function(barLine) {
    return function(current) {
      var $11 = isJust(barLine.iteration);
      if ($11) {
        return true;
      }
      ;
      var $12 = isEndVolta(barLine);
      if ($12) {
        return false;
      }
      ;
      return current;
    };
  };
  var startVolta = function(barLine) {
    return function(isCurrentlyMidVolta) {
      if (barLine.iteration instanceof Nothing) {
        if (isCurrentlyMidVolta) {
          var $15 = isEndVolta(barLine);
          if ($15) {
            return Nothing.value;
          }
          ;
          return new Just({
            voltaType: 3,
            iteration: ""
          });
        }
        ;
        return Nothing.value;
      }
      ;
      if (barLine.iteration instanceof Just) {
        return new Just({
          voltaType: 2,
          iteration: intercalateMap2(",")(show7)(barLine.iteration.value0)
        });
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.Volta (line 44, column 3 - line 60, column 10): " + [barLine.iteration.constructor.name]);
    };
  };
  var completeVolta = function(mvolta) {
    if (mvolta instanceof Nothing) {
      return Nothing.value;
    }
    ;
    if (mvolta instanceof Just) {
      var newVoltaType = function() {
        if (mvolta.value0.voltaType === 2) {
          return 5;
        }
        ;
        if (mvolta.value0.voltaType === 3) {
          return 4;
        }
        ;
        if (mvolta.value0.voltaType === 5) {
          return 4;
        }
        ;
        return mvolta.value0.voltaType;
      }();
      return new Just({
        iteration: mvolta.value0.iteration,
        voltaType: newVoltaType
      });
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.Volta (line 67, column 3 - line 84, column 50): " + [mvolta.constructor.name]);
  };

  // output/VexFlow.Abc.BarEnd/index.js
  var mempty5 = /* @__PURE__ */ mempty(musicSpecMonoid);
  var bind6 = /* @__PURE__ */ bind(/* @__PURE__ */ bindStateT(monadIdentity));
  var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadIdentity);
  var get3 = /* @__PURE__ */ get2(monadStateStateT2);
  var put2 = /* @__PURE__ */ put(monadStateStateT2);
  var pure8 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadIdentity));
  var staveWidth = function(bs) {
    return maybe(0)(function(b) {
      return b.xOffset + b.width | 0;
    })(last(bs));
  };
  var staveEndsWithRepeatBegin = function(bs) {
    var isBeginVolta = function(b) {
      return b.startLine.startRepeats > 0;
    };
    return maybe(false)(isBeginVolta)(last(bs));
  };
  var simpleBarLine = /* @__PURE__ */ function() {
    return {
      endRepeats: 0,
      thickness: Thin.value,
      startRepeats: 0,
      iteration: Nothing.value
    };
  }();
  var redundantBar = function(barSpec) {
    return isEmptyMusicSpec(barSpec.musicSpec) && barSpec.barNumber !== 0;
  };
  var fillStaveLine = function(maxWidth) {
    return function(bs) {
      var v = last(bs);
      if (v instanceof Just) {
        var currentWidth = v.value0.xOffset + v.value0.width | 0;
        var $22 = currentWidth <= maxWidth;
        if ($22) {
          var completionBar = {
            fill: v.value0.fill,
            timeSignature: v.value0.timeSignature,
            barNumber: v.value0.barNumber + 1 | 0,
            width: maxWidth - currentWidth | 0,
            xOffset: currentWidth,
            startLine: simpleBarLine,
            endLineThickness: NoLine.value,
            endLineRepeat: false,
            volta: Nothing.value,
            beamSpecs: [],
            curves: [],
            musicSpec: mempty5
          };
          return snoc(bs)(completionBar);
        }
        ;
        return bs;
      }
      ;
      if (v instanceof Nothing) {
        return bs;
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.BarEnd (line 84, column 3 - line 108, column 9): " + [v.constructor.name]);
    };
  };
  var barlineThickness2 = function(barLine) {
    if (barLine.thickness instanceof Thin) {
      return Single.value;
    }
    ;
    if (barLine.thickness instanceof Invisible) {
      return NoLine.value;
    }
    ;
    return Double.value;
  };
  var shiftBarEnd = function(acc) {
    return function(barSpec) {
      return bind6(get3)(function(lastBarType) {
        var newVolta = function() {
          var $25 = isEndVolta(lastBarType);
          if ($25) {
            return completeVolta(barSpec.volta);
          }
          ;
          return barSpec.volta;
        }();
        var lastLineThickness = barlineThickness2(lastBarType);
        var isLastBarEndRepeat = lastBarType.endRepeats > 0;
        var newBarSpec = {
          barNumber: barSpec.barNumber,
          beamSpecs: barSpec.beamSpecs,
          curves: barSpec.curves,
          fill: barSpec.fill,
          musicSpec: barSpec.musicSpec,
          startLine: barSpec.startLine,
          timeSignature: barSpec.timeSignature,
          width: barSpec.width,
          xOffset: barSpec.xOffset,
          endLineRepeat: isLastBarEndRepeat,
          endLineThickness: lastLineThickness,
          volta: newVolta
        };
        return bind6(put2(barSpec.startLine))(function() {
          var $26 = redundantBar(barSpec);
          if ($26) {
            return pure8(acc);
          }
          ;
          return pure8(cons(newBarSpec)(acc));
        });
      });
    };
  };
  var shiftBarEnds = /* @__PURE__ */ foldM(foldableArray)(/* @__PURE__ */ monadStateT(monadIdentity))(shiftBarEnd)(/* @__PURE__ */ mempty(monoidArray));
  var repositionBarEndRepeats = function(bs) {
    return evalState(shiftBarEnds(reverse(bs)))(simpleBarLine);
  };

  // output/Control.Monad.Except.Trans/index.js
  var map21 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var withExceptT = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return function(f) {
      return function(v) {
        var mapLeft = function(v1) {
          return function(v2) {
            if (v2 instanceof Right) {
              return new Right(v2.value0);
            }
            ;
            if (v2 instanceof Left) {
              return new Left(v1(v2.value0));
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 42, column 3 - line 42, column 32): " + [v1.constructor.name, v2.constructor.name]);
          };
        };
        return map112(mapLeft(f))(v);
      };
    };
  };
  var runExceptT = function(v) {
    return v;
  };
  var monadTransExceptT = {
    lift: function(dictMonad) {
      var bind8 = bind(dictMonad.Bind1());
      var pure12 = pure(dictMonad.Applicative0());
      return function(m) {
        return bind8(m)(function(a) {
          return pure12(new Right(a));
        });
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransExceptT);
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map112 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map112(map21(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind8 = bind(dictMonad.Bind1());
    var pure12 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind8(v)(either(function($187) {
            return pure12(Left.create($187));
          })(function(a) {
            var v1 = k(a);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadStateExceptT = function(dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var lift1 = lift3(Monad0);
    var state2 = state(dictMonadState);
    var monadExceptT1 = monadExceptT(Monad0);
    return {
      state: function(f) {
        return lift1(state2(f));
      },
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };

  // output/VexFlow.Abc.Beam/index.js
  var lookup5 = /* @__PURE__ */ lookup(ordInt);
  var map23 = /* @__PURE__ */ map(functorArray);
  var elem4 = /* @__PURE__ */ elem(foldableArray)(eqInt);
  var insert5 = /* @__PURE__ */ insert(ordInt);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var foldl9 = /* @__PURE__ */ foldl(foldableArray);
  var eq6 = /* @__PURE__ */ eq(/* @__PURE__ */ eqRec()(/* @__PURE__ */ eqRowCons(/* @__PURE__ */ eqRowCons(eqRowNil)()({
    reflectSymbol: function() {
      return "numerator";
    }
  })(eqInt))()({
    reflectSymbol: function() {
      return "denominator";
    }
  })(eqInt)));
  var toUnfoldable8 = /* @__PURE__ */ toUnfoldable4(unfoldableArray);
  var Beamable = /* @__PURE__ */ function() {
    function Beamable2() {
    }
    ;
    Beamable2.value = new Beamable2();
    return Beamable2;
  }();
  var Unbeamable = /* @__PURE__ */ function() {
    function Unbeamable2() {
    }
    ;
    Unbeamable2.value = new Unbeamable2();
    return Unbeamable2;
  }();
  var StartOnly = /* @__PURE__ */ function() {
    function StartOnly2() {
    }
    ;
    StartOnly2.value = new StartOnly2();
    return StartOnly2;
  }();
  var quarterNoteTicks = 32;
  var lookupRanges = function(idx) {
    return function(bm) {
      return fromMaybe([])(lookup5(idx)(bm));
    };
  };
  var eqBeamability = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Beamable && y instanceof Beamable) {
          return true;
        }
        ;
        if (x instanceof Unbeamable && y instanceof Unbeamable) {
          return true;
        }
        ;
        if (x instanceof StartOnly && y instanceof StartOnly) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var notEq1 = /* @__PURE__ */ notEq(eqBeamability);
  var eq13 = /* @__PURE__ */ eq(eqBeamability);
  var groupBeamableNotes = function(bns) {
    var f = function(a) {
      return function(b) {
        return notEq1(a.beamability)(Unbeamable.value) && eq13(b.beamability)(Beamable.value);
      };
    };
    return filter(function(g) {
      return length6(g) > 1;
    })(groupBy(f)(bns));
  };
  var getBeamRanges = function(bns) {
    var createBeamRange = function(bg) {
      var start = head4(bg);
      var end = last3(bg);
      return {
        start: start.noteIndex,
        end: end.noteIndex + 1 | 0
      };
    };
    return map23(createBeamRange)(groupBeamableNotes(bns));
  };
  var beamableNote = function(typesettingSpaces) {
    return function(offset) {
      return function(idx) {
        return function(noteSpec) {
          var noteIndex = offset + idx | 0;
          var beamability = function() {
            var $39 = noteSpec.noteTicks >= quarterNoteTicks || endsWith("r")(noteSpec.vexNote.duration);
            if ($39) {
              return Unbeamable.value;
            }
            ;
            var $40 = elem4(noteIndex)(typesettingSpaces);
            if ($40) {
              return StartOnly.value;
            }
            ;
            return Beamable.value;
          }();
          return {
            noteIndex,
            beamability
          };
        };
      };
    };
  };
  var beamFunc = function(noteSpecs) {
    return function(typesettingSpaces) {
      return function(acc) {
        return function(beatMarker) {
          var notesInBeat = slice(acc.beatMarker.noteIndex)(beatMarker.noteIndex)(noteSpecs);
          var beamables = mapWithIndex2(beamableNote(typesettingSpaces)(acc.beatMarker.noteIndex))(notesInBeat);
          var beamRanges = getBeamRanges(beamables);
          return {
            beatMarker,
            beams: insert5(beatMarker.beatNumber)(beamRanges)(acc.beams)
          };
        };
      };
    };
  };
  var anUncoalesceableRange = function(r1) {
    return function(r2) {
      return (r1.end - r1.start | 0) !== 2 || (r2.end - r2.start | 0) !== 2;
    };
  };
  var coalesce = function(v) {
    return function(v1) {
      return function(v2) {
        if (v.length === 1 && v1.length === 1) {
          var $44 = elem4(v1[0].start)(v2) || anUncoalesceableRange(v[0])(v1[0]);
          if ($44) {
            return append4([v[0]])([v1[0]]);
          }
          ;
          return [{
            start: v[0].start,
            end: v1[0].end
          }];
        }
        ;
        return append4(v)(v1);
      };
    };
  };
  var optimiseCommonTimeBeaming = function(bm) {
    return function(typesettingSpaces) {
      return append4(coalesce(lookupRanges(1)(bm))(lookupRanges(2)(bm))(typesettingSpaces))(coalesce(lookupRanges(3)(bm))(lookupRanges(4)(bm))(typesettingSpaces));
    };
  };
  var calculateStandardBeams = function(timeSignature2) {
    return function(noteSpecs) {
      return function(beatMarkers) {
        return function(typesettingSpaces) {
          var initialBM = {
            beatNumber: 0,
            noteIndex: 0
          };
          var result = foldl9(beamFunc(noteSpecs)(typesettingSpaces))({
            beatMarker: initialBM,
            beams: empty2
          })(beatMarkers);
          var $47 = eq6(commonTime)(timeSignature2);
          if ($47) {
            return optimiseCommonTimeBeaming(result.beams)(typesettingSpaces);
          }
          ;
          return concat(map23(snd)(toUnfoldable8(result.beams)));
        };
      };
    };
  };
  var calculateBeams = function(timeSignature2) {
    return function(noteSpecs) {
      return function(beatMarkers) {
        return function(typesettingSpaces) {
          return map23(function(r) {
            return [r.start, r.end];
          })(calculateStandardBeams(timeSignature2)(noteSpecs)(beatMarkers)(typesettingSpaces));
        };
      };
    };
  };

  // output/VexFlow.Abc.ChordSymbol/index.js
  var attachChordSymbol = function(noteSpecs) {
    return function(chordSymbol2) {
      var v = modifyAt(chordSymbol2.noteIndex)(function(ns) {
        return {
          accidentals: ns.accidentals,
          articulations: ns.articulations,
          dotCount: ns.dotCount,
          graceAccidentals: ns.graceAccidentals,
          graceKeys: ns.graceKeys,
          noteTicks: ns.noteTicks,
          ornaments: ns.ornaments,
          vexNote: ns.vexNote,
          chordSymbol: chordSymbol2.name
        };
      })(noteSpecs);
      if (v instanceof Nothing) {
        return noteSpecs;
      }
      ;
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Abc.ChordSymbol (line 21, column 3 - line 23, column 30): " + [v.constructor.name]);
    };
  };
  var attachChordSymbols = function(v) {
    var newNoteSpecs = foldl2(attachChordSymbol)(v.noteSpecs)(v.chordSymbols);
    return {
      tuplets: v.tuplets,
      ties: v.ties,
      tickableContext: v.tickableContext,
      contextChanges: v.contextChanges,
      slurBrackets: v.slurBrackets,
      beatMarkers: v.beatMarkers,
      repetitions: v.repetitions,
      typesettingSpaces: v.typesettingSpaces,
      chordSymbols: v.chordSymbols,
      noteSpecs: newNoteSpecs
    };
  };

  // output/VexFlow.Abc.TranslateStateful/index.js
  var toUnfoldable9 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var monadStateT2 = /* @__PURE__ */ monadStateT(monadIdentity);
  var bind7 = /* @__PURE__ */ bind(/* @__PURE__ */ bindExceptT(monadStateT2));
  var monadStateExceptT2 = /* @__PURE__ */ monadStateExceptT(/* @__PURE__ */ monadStateStateT(monadIdentity));
  var get4 = /* @__PURE__ */ get2(monadStateExceptT2);
  var put3 = /* @__PURE__ */ put(monadStateExceptT2);
  var throwError2 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowExceptT(monadStateT2));
  var applicativeExceptT2 = /* @__PURE__ */ applicativeExceptT(monadStateT2);
  var pure9 = /* @__PURE__ */ pure(applicativeExceptT2);
  var append5 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupRecord()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "beatMarkers";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "chordSymbols";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "contextChanges";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "noteSpecs";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "repetitions";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "slurBrackets";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "tickableContext";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "ties";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "tuplets";
    }
  })()(/* @__PURE__ */ semigroupRecordCons({
    reflectSymbol: function() {
      return "typesettingSpaces";
    }
  })()(semigroupRecordNil)(semigroupArray))(semigroupArray))(semigroupArray))(tickableSemigroupCtx))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray))(semigroupArray)));
  var mempty6 = /* @__PURE__ */ mempty(musicSpecMonoid);
  var map24 = /* @__PURE__ */ map(functorArray);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable(foldableList);
  var foldM4 = /* @__PURE__ */ foldM(foldableArray)(/* @__PURE__ */ monadExceptT(monadStateT2));
  var mul6 = /* @__PURE__ */ mul(semiringRational);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var fromMaybe4 = /* @__PURE__ */ fromMaybe2(unfoldableArray);
  var withExceptT2 = /* @__PURE__ */ withExceptT(/* @__PURE__ */ functorStateT(functorIdentity));
  var show8 = /* @__PURE__ */ show(showInt);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeExceptT2);
  var show12 = /* @__PURE__ */ show(showClef);
  var foldl10 = /* @__PURE__ */ foldl(foldableArray);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var evalStateT2 = /* @__PURE__ */ evalStateT(functorIdentity);
  var coerce3 = /* @__PURE__ */ coerce();
  var zipBars = function(bs) {
    var intArray = range2(0)(length2(bs));
    var barArray = toUnfoldable9(bs);
    return zip(intArray)(barArray);
  };
  var music2 = function(tickablePosition) {
    return function(noteIndex) {
      return function(phraseDuration) {
        return function(m) {
          return bind7(get4)(function(abcContext) {
            var spec = music(abcContext)(tickablePosition)(noteIndex)(phraseDuration)(m);
            var newContext = applyContextChanges(abcContext)(spec);
            return bind7(put3(newContext))(function() {
              return either(throwError2)(pure9)(spec);
            });
          });
        };
      };
    };
  };
  var modifiedStartLine = function(isPendingRepeatbegin) {
    return function(barLine) {
      if (isPendingRepeatbegin) {
        return {
          endRepeats: barLine.endRepeats,
          thickness: barLine.thickness,
          iteration: barLine.iteration,
          startRepeats: 1
        };
      }
      ;
      return barLine;
    };
  };
  var foldMusicsFunction = function(eacc) {
    return function(m) {
      var noteIndex = length(eacc.noteSpecs);
      return bind7(music2(eacc.tickableContext.value0)(noteIndex)(eacc.tickableContext.value2)(m))(function(v) {
        return pure9(append5(eacc)(v));
      });
    };
  };
  var foldOverMusics = function(barDecorations) {
    var repetitions = map24(buildRepetition)(fromFoldable6(barDecorations));
    var initialSpec = {
      beatMarkers: mempty6.beatMarkers,
      chordSymbols: mempty6.chordSymbols,
      contextChanges: mempty6.contextChanges,
      noteSpecs: mempty6.noteSpecs,
      slurBrackets: mempty6.slurBrackets,
      tickableContext: mempty6.tickableContext,
      ties: mempty6.ties,
      tuplets: mempty6.tuplets,
      typesettingSpaces: mempty6.typesettingSpaces,
      repetitions
    };
    return foldM4(foldMusicsFunction)(initialSpec);
  };
  var addFinalBeatMarker = function(abcContext) {
    return function(v) {
      var barDuration = mul6(v.tickableContext.value2)(abcContext.unitNoteLength);
      var mBeatMarker = exactBeatNumber(barDuration)(abcContext.beatDuration)(v.tickableContext.value0);
      var $112 = length(v.beatMarkers) === 0 && isNothing(mBeatMarker);
      if ($112) {
        return {
          noteSpecs: v.noteSpecs,
          tuplets: v.tuplets,
          ties: v.ties,
          tickableContext: v.tickableContext,
          contextChanges: v.contextChanges,
          slurBrackets: v.slurBrackets,
          repetitions: v.repetitions,
          typesettingSpaces: v.typesettingSpaces,
          chordSymbols: v.chordSymbols,
          beatMarkers: [{
            beatNumber: 1,
            noteIndex: length(v.noteSpecs)
          }]
        };
      }
      ;
      return {
        noteSpecs: v.noteSpecs,
        tuplets: v.tuplets,
        ties: v.ties,
        tickableContext: v.tickableContext,
        contextChanges: v.contextChanges,
        slurBrackets: v.slurBrackets,
        repetitions: v.repetitions,
        typesettingSpaces: v.typesettingSpaces,
        chordSymbols: v.chordSymbols,
        beatMarkers: append14(v.beatMarkers)(fromMaybe4(mBeatMarker))
      };
    };
  };
  var bar2 = function(barNumber) {
    return function(abcBar) {
      return bind7(foldOverMusics(abcBar.decorations)(toUnfoldable9(abcBar.music)))(function(musicSpec0) {
        return bind7(get4)(function(abcContext) {
          var musicSpec1 = attachChordSymbols(musicSpec0);
          var musicSpec = addFinalBeatMarker(abcContext)(musicSpec1);
          var volta2 = function() {
            var $117 = barNumber === 0 && isEmptyMusicSpec(musicSpec);
            if ($117) {
              return Nothing.value;
            }
            ;
            return startVolta(abcBar.startLine)(abcContext.isMidVolta);
          }();
          var newIsMidVolta = isMidVolta(abcBar.startLine)(abcContext.isMidVolta);
          var displayedKeySig = function() {
            var $118 = barNumber === 0;
            if ($118) {
              return new Just(abcContext.keySignature);
            }
            ;
            return Nothing.value;
          }();
          var width = estimateBarWidth(barNumber === 0)(abcContext.isNewTimeSignature)(displayedKeySig)(abcContext.noteSeparation)(abcBar);
          var barSpec = {
            barNumber,
            width,
            xOffset: abcContext.accumulatedStaveWidth,
            startLine: modifiedStartLine(abcContext.pendingRepeatBegin)(abcBar.startLine),
            endLineThickness: Single.value,
            endLineRepeat: false,
            fill: getBarFill(abcContext.timeSignature)(abcContext.unitNoteLength)(musicSpec.tickableContext),
            volta: volta2,
            timeSignature: abcContext.timeSignature,
            beamSpecs: calculateBeams(abcContext.timeSignature)(musicSpec.noteSpecs)(musicSpec.beatMarkers)(musicSpec.typesettingSpaces),
            curves: vexCurves(musicSpec.slurBrackets),
            musicSpec
          };
          var newWidth = abcContext.accumulatedStaveWidth + barSpec.width | 0;
          var newAbcContext = {
            beatDuration: abcContext.beatDuration,
            clef: abcContext.clef,
            keySignature: abcContext.keySignature,
            mTempo: abcContext.mTempo,
            maxWidth: abcContext.maxWidth,
            noteSeparation: abcContext.noteSeparation,
            showChordSymbols: abcContext.showChordSymbols,
            staveNo: abcContext.staveNo,
            timeSignature: abcContext.timeSignature,
            unitNoteLength: abcContext.unitNoteLength,
            accumulatedStaveWidth: newWidth,
            isMidVolta: newIsMidVolta,
            isNewTimeSignature: false,
            pendingRepeatBegin: false
          };
          return bind7(put3(newAbcContext))(function() {
            return withExceptT2(function(err) {
              return err + (": bar " + show8(barNumber));
            })(pure9(barSpec));
          });
        });
      });
    };
  };
  var bars = function(bs) {
    var tupleArray = zipBars(bs);
    return traverse2(function(v) {
      return bar2(v.value0)(v.value1);
    })(tupleArray);
  };
  var bodyPart = function(bp) {
    if (bp instanceof Score) {
      var $123 = isEmptyStave(bp.value0);
      if ($123) {
        return pure9(Nothing.value);
      }
      ;
      return bind7(get4)(function(abcContext) {
        var mStaveNo = nextStaveNo(abcContext.staveNo);
        var staveNo = fromMaybe(0)(mStaveNo);
        return bind7(put3(function() {
          var $124 = {};
          for (var $125 in abcContext) {
            if ({}.hasOwnProperty.call(abcContext, $125)) {
              $124[$125] = abcContext[$125];
            }
            ;
          }
          ;
          $124.staveNo = mStaveNo;
          $124.accumulatedStaveWidth = staveIndentation;
          return $124;
        }()))(function() {
          return bind7(bars(bp.value0))(function(staveBars) {
            return bind7(get4)(function(abcContext$prime) {
              var pendingRepeatBegin = staveEndsWithRepeatBegin(staveBars);
              var normalisedStaveBars = repositionBarEndRepeats(staveBars);
              var filledStaveLine = fillStaveLine(abcContext.maxWidth)(normalisedStaveBars);
              var clefString = show12(abcContext$prime.clef);
              var accumulatedStaveWidth = staveWidth(normalisedStaveBars);
              return bind7(put3(function() {
                var $127 = {};
                for (var $128 in abcContext$prime) {
                  if ({}.hasOwnProperty.call(abcContext$prime, $128)) {
                    $127[$128] = abcContext$prime[$128];
                  }
                  ;
                }
                ;
                $127.pendingRepeatBegin = pendingRepeatBegin;
                return $127;
              }()))(function() {
                return pure9(new Just({
                  staveNo,
                  staveWidth: accumulatedStaveWidth,
                  clefString,
                  keySignature: abcContext.keySignature,
                  isNewTimeSignature: abcContext.isNewTimeSignature,
                  mTempo: abcContext.mTempo,
                  barSpecs: filledStaveLine
                }));
              });
            });
          });
        });
      });
    }
    ;
    if (bp instanceof BodyInfo) {
      return bind7(get4)(function(abcContext) {
        var contextChanges = headerChange(bp.value0);
        var newAbcContext = foldl10(updateAbcContext)(abcContext)(contextChanges);
        return bind7(put3(newAbcContext))(function() {
          return pure9(Nothing.value);
        });
      });
    }
    ;
    throw new Error("Failed pattern match at VexFlow.Abc.TranslateStateful (line 89, column 3 - line 141, column 21): " + [bp.constructor.name]);
  };
  var tuneBody = function(bodyParts) {
    return bind7(traverse2(bodyPart)(toUnfoldable9(bodyParts)))(function(mStaveSpecs) {
      var score2 = catMaybes(mStaveSpecs);
      var v = length(score2);
      if (v === 0) {
        return throwError2("The score is empty");
      }
      ;
      return pure9(coerce3(score2));
    });
  };
  var runTuneBody = function(abcContext) {
    return function(bps) {
      return unwrap3(evalStateT2(runExceptT(tuneBody(bps)))(abcContext));
    };
  };

  // output/VexFlow.Score/index.js
  var notEq3 = /* @__PURE__ */ notEq(eqLineThickness);
  var eq7 = /* @__PURE__ */ eq(eqLineThickness);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var traverse_22 = /* @__PURE__ */ traverse_2(foldableNonEmptyArray);
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var staveConfig = function(staveNo) {
    return function(isTitled) {
      return function(barSpec) {
        var titleVerticalDepth = function() {
          if (isTitled) {
            return titleDepth;
          }
          ;
          return 0;
        }();
        return {
          x: barSpec.xOffset,
          y: (staveSeparation * staveNo | 0) + titleVerticalDepth | 0,
          width: barSpec.width,
          barNo: barSpec.barNumber,
          lineColour: "#1a1a1a",
          hasRightBar: notEq3(barSpec.endLineThickness)(NoLine.value),
          hasDoubleRightBar: eq7(barSpec.endLineThickness)(Double.value)
        };
      };
    };
  };
  var displayBarSpec = function(renderer) {
    return function(staveSpec) {
      return function(isTitled) {
        return function(barSpec) {
          var staveBarConfig = staveConfig(staveSpec.staveNo)(isTitled)(barSpec);
          return function __do2() {
            var staveBar = newStave(staveBarConfig)(staveSpec.clefString)(staveSpec.keySignature)();
            traverse_1(displayContextChange(staveBar))(barSpec.musicSpec.contextChanges)();
            when3(barSpec.barNumber === 0 && staveSpec.isNewTimeSignature)(addTimeSignature(staveBar)(barSpec.timeSignature))();
            when3(barSpec.barNumber === 0 && staveSpec.staveNo === 0)(addTempoMarking(staveBar)(staveSpec.mTempo))();
            processBarBeginRepeat(staveBar)(barSpec.startLine)();
            processBarEndRepeat(staveBar)(barSpec.endLineRepeat)();
            processVolta(staveBar)(barSpec.volta)();
            when3(!$$null(barSpec.musicSpec.noteSpecs))(renderBarContents(renderer)(staveBar)(barSpec.beamSpecs)(barSpec.curves)(barSpec.musicSpec))();
            return renderStave(renderer)(staveBar)();
          };
        };
      };
    };
  };
  var displayStaveSpec = function(renderer) {
    return function(isTitled) {
      return function(staveSpec) {
        return traverse_1(displayBarSpec(renderer)(staveSpec)(isTitled))(staveSpec.barSpecs);
      };
    };
  };
  var renderUntitledScore = function(renderer) {
    return function(eStaveSpecs) {
      if (eStaveSpecs instanceof Right) {
        return function __do2() {
          traverse_22(displayStaveSpec(renderer)(false))(eStaveSpecs.value0)();
          return Nothing.value;
        };
      }
      ;
      if (eStaveSpecs instanceof Left) {
        return pure10(new Just("error in producing score: " + eStaveSpecs.value0));
      }
      ;
      throw new Error("Failed pattern match at VexFlow.Score (line 187, column 3 - line 192, column 56): " + [eStaveSpecs.constructor.name]);
    };
  };
  var createScoreAtStave = function(staveNo) {
    return function(config2) {
      return function(abcTune) {
        var normalisedTune = normalise(abcTune);
        var v = initialAbcContext(normalisedTune)(config2);
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return runTuneBody({
            timeSignature: v.value0.timeSignature,
            keySignature: v.value0.keySignature,
            mTempo: v.value0.mTempo,
            unitNoteLength: v.value0.unitNoteLength,
            clef: v.value0.clef,
            accumulatedStaveWidth: v.value0.accumulatedStaveWidth,
            isMidVolta: v.value0.isMidVolta,
            isNewTimeSignature: v.value0.isNewTimeSignature,
            maxWidth: v.value0.maxWidth,
            pendingRepeatBegin: v.value0.pendingRepeatBegin,
            beatDuration: v.value0.beatDuration,
            noteSeparation: v.value0.noteSeparation,
            showChordSymbols: v.value0.showChordSymbols,
            staveNo: new Just(staveNo)
          })(normalisedTune.body);
        }
        ;
        throw new Error("Failed pattern match at VexFlow.Score (line 158, column 5 - line 162, column 80): " + [v.constructor.name]);
      };
    };
  };
  var renderTuneAtStave = function(staveNo) {
    return function(config2) {
      return function(renderer) {
        return function(abcTune) {
          return renderUntitledScore(renderer)(createScoreAtStave(staveNo)(config2)(abcTune));
        };
      };
    };
  };

  // output/Examples.Bugs.Main/index.js
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var canvasWidth = 1200;
  var canvasHeight = 1600;
  var config = /* @__PURE__ */ function() {
    return {
      parentElementId: defaultConfig.parentElementId,
      scale: defaultConfig.scale,
      isSVG: defaultConfig.isSVG,
      noteSeparation: defaultConfig.noteSeparation,
      showChordSymbols: defaultConfig.showChordSymbols,
      width: canvasWidth,
      height: canvasHeight,
      titling: NoTitle.value
    };
  }();
  var displayAtStave = function(renderer) {
    return function(text) {
      return function(staveNo) {
        var v = parse(text);
        if (v instanceof Right) {
          return renderTuneAtStave(staveNo)(config)(renderer)(v.value0);
        }
        ;
        return pure11(new Just("ABC failed to parse"));
      };
    };
  };
  var main = function __do() {
    var renderer = initialiseCanvas(config)();
    renderText(renderer)("Bugs!")(" 25pt Arial")(80)(80)();
    displayAtStave(renderer)(crossBeat16th)(0)();
    displayAtStave(renderer)(minimLayout)(1)();
    displayAtStave(renderer)(horizontalLayout)(2)();
    displayAtStave(renderer)(voltaBrackets)(3)();
    displayAtStave(renderer)(threeTwoBeaming)(4)();
    displayAtStave(renderer)(spacing1)(5)();
    return unit;
  };

  // <stdin>
  main();
})();
