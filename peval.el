;;; peval.el --- Partial evaluation of elisp forms   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Partially evaluate (and simplify) elisp forms.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defun peval--source (fn-symbol)
  "Get the source of function named FN-SYMBOL as an s-expression."
  (pcase-let ((`(,buf . ,pos) (find-function-noselect fn-symbol t)))
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (read buf)))))

(defvar peval-bindings
  '((arg . nil)
    (expr .  (eq kind (ly-raw quote choice)))))

(defun peval (sym)
  "Insert simplified source."
  (interactive
   (list (elisp-refs--completing-read-symbol "Function: " #'functionp)))
  (let* ((buf (get-buffer-create (format "*peval: %s*" sym)))
         (src (peval--source sym))
         (fn-name (cl-second src))
         (fn-args (cl-third src))
         (fn-body `(progn ,@(-slice src 3)))
         (simple-body
          (cl-second (peval--simplify fn-body peval-bindings)))
         (simple-body
          (if (eq (car simple-body) 'progn)
              (cdr simple-body)
            (list simple-body)))
         (simple-fn `(defun ,fn-name ,fn-args ,@simple-body)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format ";; Function: %s\n" sym))
        (cl-prettyprint simple-fn)
        (goto-char (point-min))
        (emacs-lisp-mode)
        (setq buffer-read-only t)))
    (switch-to-buffer buf)))

(defun peval--simplify-progn-body (forms bindings)
  "Simplify all the forms in FORMS using partial application.
If any form evaluates to a simple value, discard it unless
it is the final form."
  (let (simplified-exprs current)
    ;; Evaluate every expression in the progn body.
    (dolist (form forms)
      (setq current (peval--simplify form bindings))
      ;; If we evaluated the expression to a value, just throw it
      ;; away.
      (pcase current
        (`(partial ,subform)
         (push (list 'partial subform) simplified-exprs))))
    ;; If the last expression was a value, we still need to return
    ;; it.
    (pcase current
      (`(value ,value)
       (push (list 'value value) simplified-exprs)))
    (pcase (nreverse simplified-exprs)
      (`(,expr) expr)
      (`,exprs
       (list 'partial `(progn ,@(mapcar #'cl-second exprs)))))))

(defun peval--values-p (forms)
  "Do all FORMS represent values?"
  (--all-p (eq (car it) 'value) forms))

(defun peval--simplify (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
Loops are not executed and side-effecting functions are not run.

Returns a list ('value VALUE) if we could simplify the entire
FORM to an expression, or a list ('partial NEW-FORM) if some
parts of FORM could not be simplified."
  (pcase form
    ;; nil and t evaluate to themselves.
    (`nil (list 'value nil))
    (`t (list 'value t))
    ;; Literal keywords, strings and numbers evaluate to themselves.
    ((pred keywordp)
     (list 'value form))
    ((pred stringp)
     (list 'value form))
    ((pred numberp)
     (list 'value form))
    ;; We can evaluate a symbol if it is present in BINDINGS.
    ((pred symbolp)
     (if (assoc form bindings)
         (list 'value (alist-get form bindings))
       (list 'partial form)))

    (`(if ,cond ,then)
     (peval--simplify `(if ,cond ,then nil) bindings))
    (`(if ,cond ,then ,else)
     (setq cond (peval--simplify cond bindings))
     (setq then (peval--simplify then bindings))
     (setq else (peval--simplify else bindings))
     (pcase cond
       ;; If we can evaluate the if condition, then simplify to just the
       ;; THEN or the ELSE.
       (`(value ,value)
        (if value then
          else))
       ;; Otherwise, return an if where we have simplified as much as
       ;; we can.
       (`(partial ,_)
        (list 'partial
              `(if ,(cl-second cond) ,(cl-second then) ,(cl-second else))))))

    ;; Remove pointless values in progn, e.g.
    ;; (progn nil (foo) (bar)) -> (progn (foo) (bar))
    (`(progn . ,exprs)
     (peval--simplify-progn-body exprs bindings))
    
    (`(when ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (pcase cond
       (`(value ,value)
        (if value
            body
          (list 'value nil)))
       (`(partial ,form)
        (list 'partial
              `(when ,form
                 ;; body looks like (progn BODY...), so strip the progn.
                 ,@(cdr body))))))
    (`(unless ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (pcase cond
       (`(value ,value)
        (if value
            (list 'value nil)
          body))
       (`(partial ,_)
        (pcase body
          (`(value ,value)
           (list 'partial `(unless ,(cl-second cond) ,value)))
          (`(partial ,form)
           (list 'partial `(unless ,(cl-second cond)
                             ;; form looks like (progn BODY...), so strip the progn.
                             ,@(cdr form))))))))
    ;; TODO: backquote.
    (`(quote ,sym)
     (list 'value sym))
    
    ;; TODO: update `bindings' after setq.
    (`(setq ,sym ,val)
     (setq val (peval--simplify val bindings))
     (list 'partial `(setq ,sym ,(cl-second val))))

    (`(or . ,exprs)
     (let (simple-exprs
           current)
       (cl-block result
         (dolist (expr exprs)
           (setq current (peval--simplify expr bindings))
           (pcase current
             (`(value ,value)
              (when value
                ;; If the first value is truthy, we can simplify.
                ;; (or 123 x y) => 123
                (if (null simple-exprs)
                    (cl-return-from result (list 'value value))
                  ;; Otherwise, we will need to build up a list of
                  ;; arguments to `or'.
                  (push value simple-exprs))))
             ;; If we couldn't fully evaluate it, we need to preserve it.
             (`(partial ,expr)
              (push expr simple-exprs))))
         (pcase (nreverse simple-exprs)
           (`() (list 'value nil))
           (`(,expr) (list 'partial expr))
           (`,exprs (list 'partial `(or ,@exprs)))))))

    (`(cond . ,clauses)
     (let (simple-clauses)
       (cl-block result
         (dolist (clause clauses)
           (pcase clause
             (`(,condition)
              (error "todo"))
             (`(,condition . ,body)
              (message "simplified condit: %s" (peval--simplify condition bindings))
              (pcase (peval--simplify condition bindings)
                (`(value ,value)
                 (when value
                   (message "body start: %s" body)
                   (setq body (peval--simplify-progn-body
                               body bindings))
                   (message "body end: %s" body)
                   ;; If the first clause is truthy, we can simplify.
                   ;; (cond (nil 1) (t 123) (x y)) => 123
                   (if (null simple-clauses)
                       (cl-return-from result body)
                     ;; Otherwise, simplify this clause, and terminate
                     ;; this loop.
                     ;; (cond (x y) (t 123) (a b)) => (cond (x y) (t 123))
                     (progn
                       (push `(,value ,@(cl-second body)) simple-clauses)
                       (cl-return)))))  ; break from dolist.
                (`(partial ,form)
                 (setq body (peval--simplify-progn-body
                             body bindings))
                 (push `(,form ,@(cl-second body)) simple-clauses))))))
         (message "simple clauses: %s" (reverse simple-clauses))
         (pcase (nreverse simple-clauses)
           (`() (list 'value nil))
           (`(,clause)
            (list 'partial `(when ,clause)))
           (`,clauses
            (list 'partial `(cond ,@clauses)))))))

    ;; Function call.
    ((and `(,fn . ,args) (guard (functionp fn)))
     (setq args (--map (peval--simplify it bindings) args))
     ;; If it's a pure function, and we could evaluate all the
     ;; arguments, call it.
     (if (and
          (peval--values-p args)
          (get fn 'side-effect-free))
         (progn
           (list 'value (apply fn (mapcar #'cl-second args))))
       (list 'partial `(,fn ,@(mapcar #'cl-second args)))))
    (`(,fn . ,args)
     ;; Either a function we don't know about, or a macro. We can't
     ;; simplify because we don't know which arguments are evaluated.
     (list 'partial form))

    (_ (error "Don't know how to simplify: %s" form))))

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

(peval--simplify
 '(cond
   (a (+ b 1))
   (c d))
 nil)

(peval--simplify
 '(cond
   (a (+ b 1))
   (c d)
   (e (foo)))
 '((b . 3) (c . 4)))

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

(provide 'peval)
;;; peval.el ends here

