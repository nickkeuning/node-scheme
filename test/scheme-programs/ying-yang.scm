
; original
(let* (
  (yin
    ((lambda (cc) (display 1) cc) (call/cc (lambda (c) c))))
  (yang
    ((lambda (cc) (display 2) cc) (call/cc (lambda (c) c))))
  )
(yin yang))

; desugar let* to let
(let (
  (yin
    ((lambda (cc) (display 1) cc) (call/cc (lambda (c) c)))))
  (let (
    (yang
      ((lambda (cc) (display 2) cc) (call/cc (lambda (c) c)))))
      (yin yang)
  )
)

; desugar let to lambda + begin
((lambda (yin)
  ((lambda (yang)
    (yin yang)
  ) ((lambda (cc) (begin (display 2) cc)) (call/cc (lambda (c) c))))
) ((lambda (cc) (begin (display 1) cc)) (call/cc (lambda (c) c))))
