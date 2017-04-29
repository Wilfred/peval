(require 'ert)
(require 'peval)

(ert-deftest peval--cond ()
  ;; Simplify some arms
  (should
   (equal
    (peval--simplify
     '(cond
       (a (+ b 1))
       (c d)
       (e (foo)))
     '((b . 3) (c . 5)))
    (list 'partial
          '(cond
            ;; We don't know if this clause will be evaluated, but we can
            ;; simplify its body.
            (a 4)
            ;; We know this clause is true, so we can discard later arms.
            (5 d)))))
  ;; Simplify cond entirely.
  (should
   (equal
    (peval--simplify
     '(cond
       (a 1)
       (b c)
       (e (foo)))
     '((a . nil) (b . t)))
    (list 'partial 'c)))
  ;; cond clause without body.
  (should
   (equal
    (peval--simplify
     '(cond
       (a)
       (b))
     '((a . nil) (b . 123)))
    (list 'value 123)))
  ;; No clauses evaluate to t.
  (should
   (equal
    (peval--simplify
     '(cond
       (a (foo))
       (b (bar)))
     '((a . nil) (b . nil)))
    (list 'value nil)))
  ;; Single unknown clause could just be a when.
  (should
   (equal
    (peval--simplify
     '(cond
       (a (foo) (x))
       (b (bar) (x))
       (c (baz) (x)))
     '((a . nil) (c . nil)))
    (list 'partial '(when b (baz) (x)))))
  ;; Single unknown clause without a body.
  (should
   (equal
    (peval--simplify
     '(cond
       (a (foo) (x))
       (b)
       (c (baz) (x)))
     '((a . nil) (c . nil)))
    (list 'partial 'b))))

(ert-deftest peval--progn ()
  (should
   (equal
    (peval--simplify '(progn 1) nil)
    (list 'value 1)))
  (should
   (equal
    (peval--simplify '(progn nil y) nil)
    (list 'partial 'y)))
  (should
   (equal
    (peval--simplify '(progn x nil y) nil)
    (list 'partial '(progn x y)))))

(ert-deftest peval--when ()
  (should
   (equal
    (peval--simplify '(when nil x y) nil)
    (list 'value nil)))
  (should
   (equal
    (peval--simplify '(when t x y) nil)
    (list 'partial '(progn x y))))
  (should
   (equal
    (peval--simplify '(when x y) '((y . 1)))
    (list 'partial '(when x 1)))))

(ert-deftest peval--unless ()
  (should
   (equal
    (peval--simplify '(unless nil x y) nil)
    (list 'partial '(progn x y))))
  (should
   (equal
    (peval--simplify '(unless t x y) nil)
    (list 'value nil)))
  (should
   (equal
    (peval--simplify '(unless x y) '((y . 1)))
    (list 'partial '(unless x 1)))))

(ert-deftest peval--known-fn-call ()
  "If we know we're calling a function, we should simplify its
arguments."
  ;; Function with side effects.
  (should
   (equal
    (peval--simplify '(message x) '((x . 1)))
    (list 'partial '(message 1))))
  (should
   (equal
    (peval--simplify '(+ x 2) '((x . 3)))
    (list 'value 5))))

(ert-deftest peval--unknown-fn-call ()
  "If we don't recognise the symbol, do nothing."
  (should
   (equal
    (peval--simplify '(foo x) '((x . 1)))
    (list 'partial '(foo x)))))

(ert-deftest peval--let ()
  "Unimplemented, but ensure we don't crash."
  (should
   (equal
    (peval--simplify '(let ((x 1) (y 2)) x) nil)
    (list 'partial '(let ((x 1) (y 2)) x)))))

(ert-deftest peval--setq ()
  "Ensure we only evaluate the second argument."
  (should
   (equal
    (peval--simplify '(setq x y) '((x . 1) (y . 2)))
    (list 'partial '(setq x 2)))))

(ert-deftest peval--or ()
  ;; Remove nil values.
  (should
   (equal
    (peval--simplify '(or nil x y z) '((x . nil)))
    (list 'partial '(or y z))))
  ;; Simplify a single value.
  (should
   (equal
    (peval--simplify '(or x y) '((x . nil)))
    (list 'partial 'y)))
  ;; Stop on first truthy value.
  (should
   (equal
    (peval--simplify '(or x y) '((x . "foo")))
    (list 'value "foo")))
  ;; All falsy values
  (should
   (equal
    (peval--simplify '(or x y) '((x . nil) (y . nil)))
    (list 'value nil))))

(ert-deftest peval--bool ()
  (should
   (equal
    (peval--simplify 'nil nil)
    (list 'value nil)))
  (should
   (equal
    (peval--simplify 't nil)
    (list 'value t))))

(ert-deftest peval--number ()
  (should
   (equal
    (peval--simplify 123 nil)
    (list 'value 123))))

(ert-deftest peval--keyword ()
  (should
   (equal
    (peval--simplify :foo nil)
    (list 'value :foo))))

(ert-deftest peval--string ()
  (should
   (equal
    (peval--simplify "foo" nil)
    (list 'value "foo"))))

(ert-deftest peval--symbol ()
  (should
   (equal
    (peval--simplify 'x nil)
    (list 'partial 'x)))
  (should
   (equal
    (peval--simplify 'x '((x . 42)))
    (list 'value 42))))

(ert-deftest peval--quoted-symbol ()
  (should
   (equal
    (peval--simplify ''x nil)
    (list 'value 'x))))

(ert-deftest peval--if-by-condition ()
  "We should discard then THEN or the ELSE case
if we can evaluate the condition."
  (should
   (equal
    (peval--simplify '(if x y z) nil)
    (list 'partial '(if x y z))))
  (should
   (equal
    (peval--simplify '(if x y z) '((x . 42)))
    (list 'partial 'y))))

(ert-deftest peval--if-body ()
  "We should always simplify the THEN and ELSE."
  (should
   (equal
    (peval--simplify '(if x y z) '((y . 42) (z . 41)))
    (list 'partial '(if x 42 41)))))

(ert-deftest peval--if-condition ()
  "We should always simplify the COND."
  (should
   (equal
    (peval--simplify '(if (if t x a) y z) '((y . 42) (z . 41)))
    (list 'partial '(if x 42 41)))))

(ert-deftest peval--if-without-else ()
  (should
   (equal
    (peval--simplify '(if t y) nil)
    (list 'partial 'y)))
  (should
   (equal
    (peval--simplify '(if nil y) nil)
    (list 'value nil))))
