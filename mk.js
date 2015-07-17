import { Map, Iterable } from "immutable";

const runThunk = (v) => typeof v === "function" ? runThunk(v()) : v;

const nil = "Nil";

class Var {
  constructor(c) { this.var = c; }
  equals(o) { return this.var === o.var; }
  hashCode() { return this.var; }
  toString() { return "?" + this.var; }
}
const mkvar = (c) => new Var(c);
const isvar = (c) => c instanceof Var;

class Stream {
  constructor(a, d) { this.a = a; this.d = d; }
  *[Symbol.iterator]() {
    yield this.car();
    const next = this.cdr();
    if (next !== nil) yield* next;
  }
  car() { return runThunk(this.a); }
  cdr() { return runThunk(this.d); }
  toArray() { return toJSArray(this); }
  toString() {
    return properList(this) ? "[" + this.toArray().join(", ") + "]" :
      "(" + this.car() + " . " +
      this.cdr() + ")";
  }
}
const cons = (car, cdr) => new Stream(car, cdr);
const listp = (s) => s instanceof Stream;
const list = (vs) => vs.length ? cons(vs[0], list(vs.slice(1))) : nil;
const properList = (l) => {
  if (l === nil) { return true; }
  if (!listp(l.cdr()) && l.cdr() !== nil) { return false; }
  return properList(l.cdr());
};
const toJSArray = (l) => l === nil ? [] : [l.car()].concat(toJSArray(l.cdr()));

const walk = (u, s) => isvar(u) && s.has(u) ? walk(s.get(u), s) : u;

const emptystate = [Map(), 0];

const mzero = nil;
const unit = (x) => cons(x, nil);

const unify = (u, v, s) => {
  if (s === nil) { return nil; }
  if (u instanceof Array) { u = list(u); }
  if (v instanceof Array) { v = list(v); }
  u = walk(u, s); v = walk(v, s);
  if (isvar(u) && isvar(v) && u.equals(v)) { return s; }
  if (isvar(u)) { return s.set(u, v); }
  if (isvar(v)) { return s.set(v, u); }
  if (listp(u) && listp(v)) {
    return unify(u.cdr(), v.cdr(), unify(u.car(), v.car(), s));
  }
  if (u === v) { return s; }
  return nil;
};

const eq = (u, v) => ([s, c]) => {
  const sp = unify(u, v, s);
  return (sp === nil) ? mzero : unit([sp, c]);
};

const callFresh = (f) => ([s, c]) => f(mkvar(c))([s, c + 1]);

const mplus = (s1, s2) => {
  s1 = runThunk(s1);
  if (s1 === nil) { return s2; }
  return cons(s1.car(), () => mplus(s2, s1.cdr()));
};

const bind = (s, g) => {
  if (s === nil) { return mzero; }
  if (typeof s === "function") { return bind(s(), g); }
  return mplus(g(s.car()), () => bind(s.cdr(), g));
};

const disj = (g1, g2) => (sc) => mplus(g1(sc), g2(sc));
const conj = (g1, g2) => (sc) => bind(g1(sc), g2);

const walk1 = (v, s) => {
  const c = walk(v, s);
  if (listp(c)) { return cons(walk1(c.car(), s), walk1(c.cdr(), s)); }
  return c;
};
const reify1st = ([s, c]) => walk1(mkvar(0), s);

const callGoal = (g) => g(emptystate);
const pull = (n, g) => {
  const r = runThunk(callGoal(g));
  return r === nil ? Iterable() : Iterable(r[Symbol.iterator]()).take(n).map(reify1st);
};

const appendo = (l, r, out) => disj(
  conj(eq(l, nil), eq(r, out)),
  callFresh((a) =>
  callFresh((d) =>
  callFresh((res) =>
            conj(eq(cons(a, d), l),
                 conj(eq(cons(a, res), out),
                      (sc) => () => appendo(d, r, res)(sc))))))
);

// const fives = (x) => disj(eq(x, 5), (sc) => () => fives(x)(sc));
// const sixes = (x) => disj(eq(x, 6), (sc) => () => sixes(x)(sc));
// const fivesAndSixes = callFresh((x) => disj(fives(x), sixes(x)));
// console.log(pull(10, fivesAndSixes));

// const wat = callFresh((q) =>
//             callFresh((a) =>
//             callFresh((b) =>
//                       conj(eq(q, [a, b]),
//                            appendo(a, b, [1, 2, 3, 4, 5])))));
// console.log(pull(10, wat));
