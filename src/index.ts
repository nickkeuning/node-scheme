export const tokenize = (program: string) =>
  program.replace(/\(/g, "( ").replace(/\)/g, " )").trim().split(/\s+/);

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

const globalEnv = makeEnv({
  "+": (a: number, b: number) => a + b,
  "-": (a: number, b: number) => a - b,
  "=": (a: any, b: any) => a === b,
  log: console.log,
});

const makeLambda = (params: string[], body: Exp, env: Env) => ({
  params,
  body,
  env,
});

export const evaluate = (ex: Exp, env = globalEnv): Exp | undefined => {
  while (true) {
    if (typeof ex === "string") return env.find(ex);
    if (!Array.isArray(ex)) return ex;
    switch (ex[0]) {
      case "define": {
        const [_, name, value] = ex;
        env.define(name as string, evaluate(value, env));
        return;
      }
      case "set!": {
        const [_, name, value] = ex;
        env.set(name as string, evaluate(value, env));
        return;
      }
      case "lambda": {
        const [_, params, ...body] = ex;
        return makeLambda(params as string[], ["do", ...body], env);
      }
      case "let": {
        const [_, bindings, ...body] = ex as any;
        const names = bindings.map(([name]: any) => name);
        const values = bindings.map(([_, value]: any) => value);
        ex = [["lambda", names, ...body], ...values];
        continue;
      }
      case "let*": {
        const [_, bindings, ...body] = ex as any;
        const toLet = (bindings: any): any =>
          bindings.length < 2
            ? ["let", bindings, ...body]
            : ["let", bindings.slice(0, 1), toLet(bindings.slice(1))];
        ex = toLet(bindings);
        continue;
      }
      case "if": {
        const [_, test, consequence, alternate] = ex;
        ex = evaluate(test, env) ? consequence : alternate;
        continue;
      }
      case "do": {
        for (const exp of ex.slice(1, -1)) evaluate(exp, env);
        ex = ex.slice(-1)[0];
        continue;
      }
      default: {
        const [proc, ...args] = ex.map((exp) => evaluate(exp, env)) as any;
        if (typeof proc === "function") {
          return proc(...args);
        } else {
          ex = proc.body;
          env = proc.env.extend(proc.params, args);
        }
      }
    }
  }
};

const programs = [
  ["(+ (+ 1 2) (+ 3 4))", 10],
  ["(if false 1 2)", 2],
  ["(if true 1 2)", 1],
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
  ["(do (define add (lambda (a b) (+ a b))) (add 1 2) (add 3 4))", 7],
  ["((lambda (a b) (+ a b)) 1 2)", 3],
  ["(do (define a 1) (set! a 2) a)", 2],
  ["(do (log 1) 1)", 1],
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
    `(let ((x (+ 1 2)) (y 4))
      (- x y)
    )`,
    -1,
  ],
  [
    `(let* ((x 3) (y x))
      (- x y)
    )`,
    0,
  ],
  [
    `(let* ((x (+ 1 2)) (y 4))
      (- x y)
    )`,
    -1,
  ],
] as const;

programs.forEach(([program, expectation]) => {
  const result = evaluate(parse(getStream(tokenize(program))));
  const passed =
    typeof expectation === "function"
      ? expectation(result)
      : result === expectation;
  console.log({ passed });
  if (!passed) console.log({ program, expectation, result });
});
