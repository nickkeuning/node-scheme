export const tokenize = (program: string) =>
  program.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);

type Atom = string | number | boolean | null;
type Lambda = {
  params: string[];
  body: Exp;
  env: Env;
};
type Exp = Atom | Lambda | Exp[];

const atom = (token: string): Atom => {
  const int = parseInt(token);
  if (!isNaN(int)) return int;
  const float = parseFloat(token);
  if (!isNaN(float)) return float;
  if (token.toLowerCase() === "true") return true;
  if (token.toLowerCase() === "false") return false;
  if (token.toLowerCase() === "nil") return null;
  return token;
};

type Stream<T> = { peek: () => T; pop: () => T };
export const getStream = <T>(arr: T[]): Stream<T> => {
  let idx = 0;
  const peek = () => arr[idx];
  const pop = () => arr[idx++];
  return { peek, pop };
};

export const parse = (tokens: Stream<string>): Exp => {
  switch (tokens.peek()) {
    case undefined:
      throw "unexpected EOF";
    case "(":
      const list = [];
      tokens.pop();
      while (tokens.peek() !== ")") list.push(parse(tokens));
      tokens.pop();
      return list;
    case ")":
      throw "unexpected )";
    default:
      return atom(tokens.pop());
  }
};

type Env = {
  define: (key: string, value: any) => void;
  find: (key: string) => any;
  set: (key: string, value: any) => void;
  extend: (names: string[], values: any[]) => Env;
};

const makeEnv = (
  map: Record<string, any>,
  pGet: ((key: string) => any) | undefined = undefined,
  pSet: ((key: string, value: any) => any) | undefined = undefined
) => {
  const define = (key: string, value: any) => (map[key] = value);
  const find = (key: string) => map[key] ?? pGet?.(key);
  const set = (key: string, value: any) =>
    map[key] !== undefined ? (map[key] = value) : pSet?.(key, value);
  const extend = (names: string[], values: any[]) =>
    makeEnv(Object.fromEntries(names.map((n, i) => [n, values[i]])), find, set);
  return { find, define, set, extend };
};

const globalEnv = {
  "+": (k: any, a: number, b: number) => k(a + b),
  "*": (k: any, a: number, b: number) => k(a * b),
  "-": (k: any, a: number, b: number) => k(a - b),
  "=": (k: any, a: any, b: any) => k(a === b),
  log: (k: any, msg: any) => k(console.log(msg)),
};

export const evalCps = (ex: Exp, env: Env, k: any): any => {
  if (typeof ex === "string") {
    return k(env.find(ex));
  }
  if (!Array.isArray(ex)) {
    return k(ex);
  }
  switch (ex[0]) {
    case "letrec": {
      const [_, bindings, ...bodyExs] = ex as any;
      const defs = bindings.map(([name]: any) => ["define", name, null]);
      const sets = bindings.map(([name, valEx]: any) => ["set!", name, valEx]);
      return evalCps([["lambda", [], ...defs, ...sets, ...bodyExs]], env, k);
    }
    case "let*": {
      const [_, bindings, ...bodyExprs] = ex as any;
      const toLet = (bindings: any): any =>
        bindings.length < 2
          ? ["let", bindings, ...bodyExprs]
          : ["let", bindings.slice(0, 1), toLet(bindings.slice(1))];
      return evalCps(toLet(bindings), env, k);
    }
    case "let": {
      const [_, bindings, ...body] = ex as any;
      const names = bindings.map(([name]: any) => name);
      const values = bindings.map(([_, value]: any) => value);
      return evalCps([["lambda", names, ...body], ...values], env, k);
    }
    case "define": {
      const [_, name, valueExpr] = ex;
      return evalCps(valueExpr, env, (value: any) =>
        k(env.define(name as string, value))
      );
    }
    case "set!": {
      const [_, name, valueExpr] = ex;
      return evalCps(valueExpr, env, (value: any) =>
        k(env.set(name as string, value))
      );
    }
    case "if": {
      const [_, test, consequence, alternate] = ex;
      return evalCps(test, env, (result: any) =>
        evalCps(result ? consequence : alternate, env, k)
      );
    }
    case "call/cc": {
      const [_, lambda] = ex as any;
      const cc = (_: any, ret: any) => k(ret);
      return evalCps([lambda, cc] as any, env, k);
    }
    case "lambda": {
      const [_, params, ...body] = ex as any;
      return k((callback: any, ...args: any) =>
        evalCps(["do", ...body], env.extend(params, args), callback)
      );
    }
    case "do": {
      const [_, ...exprs] = ex as any;
      const loop = ([h, ...t]: any[]): any =>
        t.length > 0 ? evalCps(h, env, () => loop(t)) : evalCps(h, env, k);
      return loop(exprs);
    }
    default: {
      const [rawProc, ...rawArgs] = ex;
      return evalCps(rawProc, env, (proc: any) => {
        if (proc === undefined) {
          throw `"${ex[0]}" could not be found`;
        }
        if (typeof proc !== "function") {
          throw `"${proc}" is not callable`;
        }
        const loop = ([rawArg, ...rest]: any[], args: any[]) =>
          rawArg === undefined
            ? () => proc(k, ...args)
            : evalCps(rawArg, env, (arg: any) => loop(rest, [...args, arg]));
        return loop(rawArgs, []);
      });
    }
  }
};

const trampoline = (f: any, ...args: any) => {
  let result = f(...args);
  while (typeof result === "function") result = result();
  return result;
};

const evaluate = (program: string, env = makeEnv(globalEnv)) =>
  trampoline(
    evalCps,
    parse(getStream(tokenize(program))),
    env,
    (res: any) => res
  );

const programs = [
  ["((lambda (fun) (fun 2)) log)", undefined],
  ["((lambda (fun) (fun 2)) (lambda (num) (+ num 1)))", 3],
  ["((lambda () 1))", 1],
  ["(((lambda () 1)))", 'Error: "1" is not callable'],
  ["nil", null],
  ["2", 2],
  ["(+ 1 1)", 2],
  ["(+ (+ 1 2) (+ 3 4))", 10],
  ["((lambda (a b) (+ a b)) 1 2)", 3],
  ["((lambda (a b) b) 1 2)", 2],
  ["(do (define a 1) a)", 1],
  ["(do (define a 1) ((lambda (ignored) (define a 2)) true) a)", 1],
  ["(do (define a 1) (set! a 2) a)", 2],
  [
    `(do
      (define sum-to-n
        (lambda (n)
          (define sum-to-n-inner
            (lambda (n acc)
              (if (= n 0)
                acc
                (sum-to-n-inner (- n 1) (+ n acc)))))
          (sum-to-n-inner n 0)
        )
      )
      (sum-to-n 100)
    )`,
    5050,
  ],
  ["(if false 1 2)", 2],
  ["(if true 1 2)", 1],
  ["(do (define add (lambda (a b) (+ a b))) (add 1 2) (add 3 4))", 7],
  ["(do (log 1) 1)", 1],
  [
    `(do
      (define a 1)
      (define fun
        (lambda (ignored)
          (log 1)
          (set! a 2)
        )
      )
      (fun true)
      a
    )`,
    2,
  ],
  [
    `(let ((x (+ 1 2)) (y 4))
      (- x y)
    )`,
    -1,
  ],
  [
    `(let* ((x 3) (y (* x 5)))
      (- x y)
    )`,
    -12,
  ],
  [
    `(let* ((x (+ 1 2)) (y 4) (z (+ x y)))
      (-(- x y)z)
    )`,
    -8,
  ],
  [
    `(let* (
      (x (do (log 1) (+ 1 2)))
      (y (do (log 2) 4))
      (z (do (log 3) (+ x y))))
        (- (- x y) z)
    )`,
    -8,
  ],
  [
    `(let* (
      (add +)
      (double (lambda (x) (log 1) (log x) (add x x)))
      (quad (lambda (x) (log 2) (log x) (double (double x)))))
        (quad 4)
    )`,
    16,
  ],
  [
    `(do
      (define sum-to-n
        (lambda (n)
          (define sum-to-n-inner
            (lambda (n acc)
              (if (= n 0)
                acc
                (sum-to-n-inner (- n 1) (+ n acc)))))
          (sum-to-n-inner n 0)
        )
      )
      (sum-to-n 10000)
    )`,
    50005000,
  ],
  [
    `(do
      (define fib
        (lambda (n)
          (if (= n 0)
              0
              (if (= n 1)
                  1
                  (+ (fib (- n 1)) (fib (- n 2)))))))
      (fib 10)
    )`,
    55,
  ],
  [
    `(letrec (
      (sum-to-n-inner
        (lambda (n acc)
          (if (= n 0)
            acc
            (sum-to-n-inner (- n 1) (+ n acc)))))
      (sum-to-n 
        (lambda (n)
          (sum-to-n-inner n 0))))
        (sum-to-n 10000)
    )`,
    50005000,
  ],
  [
    `(letrec (
      (zero? 
        (lambda (n)
          (= n 0)))
      (dec
        (lambda (n)
          (- n 1)))
      (is-even?
        (lambda (n)
          (if (zero? n) true (is-odd? (dec n)))))
      (is-odd?
        (lambda (n)
          (if (zero? n) false (is-even? (dec n))))))
        (is-odd? 111111))`,
    true,
  ],
  [
    `(let (
      (f
        (lambda (return)
          (return 2)
          3)))
      (log (f (lambda (x) x)))
      (call/cc f)
    )`,
    2,
  ],
  // [
  //   `(let* (
  //     (yin
  //       ((lambda (cc) (log 1) cc) (call/cc (lambda (c) c))))
  //     (yang
  //       ((lambda (cc) (log 2) cc) (call/cc (lambda (c) c))))
  //     )
  //   (yin yang))`,
  //   undefined,
  // ],
] as const;

programs.slice().forEach(([program, expectation], i) => {
  let result = null;
  let logged = [] as any;
  try {
    result = evaluate(
      program,
      makeEnv({ ...globalEnv, log: (k: any, msg: any) => k(logged.push(msg)) })
    );
  } catch (e) {
    result = `Error: ${e}`;
  } finally {
    const passed = result === expectation;

    console.log(
      logged.length > 0 ? { test: i, passed, logged } : { test: i, passed }
    );
    if (!passed) console.log({ program, expectation, result });
  }
});
