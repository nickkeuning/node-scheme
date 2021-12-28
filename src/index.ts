export const tokenize = (program: string) =>
  program.replace(/\(/g, " ( ").replace(/\)/g, " ) ").trim().split(/\s+/);

type Atom = string | number | boolean;
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
): Env => {
  const define = (key: string, value: any) => (map[key] = value);
  const find = (key: string) => map[key] ?? pGet?.(key);
  const set = (key: string, value: any) =>
    map[key] !== undefined ? (map[key] = value) : pSet?.(key, value);
  const extend = (names: string[], values: any[]) =>
    makeEnv(Object.fromEntries(names.map((n, i) => [n, values[i]])), find, set);
  return { find, define, set, extend };
};

const globalEnv = {
  "+": (a: number, b: number) => a + b,
  "*": (a: number, b: number) => a * b,
  "-": (a: number, b: number) => a - b,
  "=": (a: any, b: any) => a === b,
  log: console.log,
};

const makeLambda = (params: string[], body: Exp, env: Env) => ({
  params,
  body,
  env,
});

export const evaluate = (ex: Exp, env: Env, k: any): Exp | undefined => {
  if (typeof ex === "string") {
    k(env.find(ex));
    return;
  }
  if (!Array.isArray(ex)) {
    k(ex);
    return;
  }
  switch (ex[0]) {
    case "define": {
      const [_, name, value] = ex;
      evaluate(value, env, (val: any) => {
        env.define(name as string, val);
        k();
      });
      return;
    }
    case "set!": {
      const [_, name, value] = ex;
      evaluate(value, env, (val: any) => {
        env.set(name as string, val);
        k();
      });
      return;
    }
    case "let*": {
      const [_, bindings, ...body] = ex as any;
      const toLet = (bindings: any): any =>
        bindings.length < 2
          ? ["let", bindings, ...body]
          : ["let", bindings.slice(0, 1), toLet(bindings.slice(1))];
      evaluate(toLet(bindings), env, k);
      return;
    }
    case "let": {
      const [_, bindings, ...body] = ex as any;
      const names = bindings.map(([name]: any) => name);
      const values = bindings.map(([_, value]: any) => value);
      evaluate([["lambda", names, ...body], ...values], env, k);
      return;
    }
    case "lambda": {
      const [_, params, ...body] = ex;
      k(makeLambda(params as string[], ["do", ...body], env));
      return;
    }
    case "if": {
      const [_, test, consequence, alternate] = ex;
      evaluate(test, env, (result: any) => {
        evaluate(result ? consequence : alternate, env, k);
      });
      return;
    }
    case "do": {
      for (const exp of ex.slice(1, -1)) evaluate(exp, env, () => {});
      evaluate(ex.slice(-1)[0], env, k);
      return;
    }
    default: {
      evaluate(ex[0], env, (proc: any) => {
        if (proc === undefined) throw `"${ex[0]}" is not a function`;
        const loop = (rawArgs: any[], args: any[]) => {
          if (rawArgs.length > 0) {
            evaluate(rawArgs.shift(), env, (arg: any) => {
              args.push(arg);
              loop(rawArgs, args);
            });
          } else {
            if (typeof proc === "function") {
              k(proc(...args));
            } else {
              evaluate(proc.body, proc.env.extend(proc.params, args), k);
            }
          }
        };
        loop(ex.slice(1), []);
      });
      return;
    }
  }
};

const programs = [
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
  [
    "(lambda (a b) (+ a b))",
    ({ body: [doSymbol, body] }: any) => {
      return (
        doSymbol === "do" &&
        body[0] === "+" &&
        body[1] === "a" &&
        body[2] === "b"
      );
    },
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
  // [
  //   `(do
  //     (define fib
  //       (lambda (n)
  //         (if (= n 0)
  //             0
  //             (if (= n 1)
  //                 1
  //                 (+ (fib (- n 1)) (fib (- n 2)))))))
  //     (fib 10)
  //   )`,
  //   55,
  // ],
  // [
  //   `(do
  //     (define sum-to-n
  //       (lambda (n)
  //         (define sum-to-n-inner
  //           (lambda (n acc)
  //             (if (= n 0)
  //               acc
  //               (sum-to-n-inner (- n 1) (+ n acc)))))
  //         (sum-to-n-inner n 0)
  //       )
  //     )
  //     (sum-to-n 100000)
  //   )`,
  //   5000050000,
  // ],
] as const;

programs.forEach(([program, expectation]) => {
  const parsed = parse(getStream(tokenize(program)));
  evaluate(parsed, makeEnv({ ...globalEnv, log: () => {} }), (result: any) => {
    const passed =
      typeof expectation === "function"
        ? expectation(result)
        : result === expectation;

    console.log({ passed });
    if (!passed) console.log({ program, expectation, result });
  });
});
