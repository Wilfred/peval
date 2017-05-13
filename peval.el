;;; peval.el --- Partial evaluation of elisp forms   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Version: 0.1

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

(defmacro peval--if-value (sexp value-body &optional partial-body)
  "Evaluate SEXP, and execute VALUE-BODY or PARTIAL-BODY
depending on the car of SEXP.

Within VALUE-BODY, `it-value' is bound, whereas in PARTIAL-BODY
`it-form' is bound.

This is intended to be used within `peval', as it always returns
a list ('value 123) or a list ('partial '(+ 122 x))."
  (declare (indent 2) (debug t))
  `(-let [(sym value-or-form) ,sexp]
     (if (eq sym 'value)
         (let ((it-value value-or-form))
           ,value-body)
       (cl-assert (eq sym 'partial))
       (let ((it-form value-or-form))
         ,partial-body))))

(defun peval-live ()
  (interactive)
  (let* ((buf (get-buffer-create "*peval*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert ";; Specify function arguments\n"
                "(-slice '(2 3 4 5) _ 3)\n"
                "\n"
                ";; Simplified function")
        (peval--live-update)
        (emacs-lisp-mode)))
    (switch-to-buffer buf)))

(defconst peval-placeholder
  (make-symbol "peval-placeholder")
  "A unique symbol representing _ in forms given.")

;; TODO: Handle &optional
(defun peval--pretty-args (source-args bindings-given)
  "Attach :? to all the arguments in the source where we haven't
been given a placeholder."
  (-map
   (-lambda ((arg . binding))
     (if (eq binding peval-placeholder)
         (read (format "%s:?" arg))
       arg))
   (-zip source-args bindings-given)))

(defun peval--live-update ()
  (interactive)
  (let (form-given sym-given raw-bindings-given bindings-given)
    (save-excursion
      ;; Get the form entered by the user.
      (goto-char (point-min))
      (setq form-given (read (current-buffer)))
      (setq sym-given (car form-given))
      (setq raw-bindings-given (cdr form-given))

      ;; Go to the next comment and erase the previous simplified
      ;; form.
      (search-forward ";;")
      (forward-line)
      (delete-region (point) (point-max))

      ;; TODO: ensure SYM-GIVEN is a defined function.

      ;; Evaluate the bindings specified by the user. This enables us
      ;; to convert '(foo tab-width) => '(foo 4)
      (dolist (form raw-bindings-given)
        (push 
         (if (eq form '_)
             peval-placeholder
           (eval form))
         bindings-given))
      (setq bindings-given (nreverse bindings-given))

      ;; Get the source and partially evaluate it with respect to the
      ;; arguments given.
      (let* ((src (peval--source sym-given))
             (fn-name (cl-second src))
             (fn-args (cl-third src))
             (fn-body `(progn ,@(-slice src 3)))
             (simple-body
              (cl-second (peval--simplify fn-body peval-bindings)))
             (simple-body
              (if (eq (car simple-body) 'progn)
                  (cdr simple-body)
                (list simple-body)))
             (simple-fn `(defun ,fn-name ,(peval--pretty-args fn-args bindings-given)
                           ,@simple-body)))
        (cl-prettyprint simple-fn))
      
      )

    
    ))

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
      (peval--if-value current
          nil
        (push (list 'partial it-form) simplified-exprs)))
    ;; If the last expression was a value, we still need to return
    ;; it.
    (peval--if-value current
        (push (list 'value it-value) simplified-exprs))
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
    (`(if ,cond ,then . ,else)
     (setq cond (peval--simplify cond bindings))
     (setq then (peval--simplify then bindings))
     (setq else (peval--simplify-progn-body else bindings))
     (peval--if-value cond
         ;; If we can evaluate the if condition, then simplify to just the
         ;; THEN or the ELSE.
         (if it-value then else)
       ;; Otherwise, return an if where we have simplified as much as
       ;; we can.
       (pcase else
         ;; a progn can be added by `peval--simplify-progn-body', so
         ;; (if _ _ (progn x y)) => (if _ _ x y)
         (`(partial (progn . ,else))
          (list 'partial
                `(if ,(cl-second cond)
                     ,(cl-second then)
                   ,@else)))
         (_
          (list 'partial
                `(if ,(cl-second cond)
                     ,(cl-second then)
                   ,(cl-second else)))))))

    ;; Remove pointless values in progn, e.g.
    ;; (progn nil (foo) (bar)) -> (progn (foo) (bar))
    (`(progn . ,exprs)
     (peval--simplify-progn-body exprs bindings))
    
    (`(when ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (peval--if-value cond
         (if it-value
             body
           (list 'value nil))
       (list 'partial
             `(when ,it-form
                ;; body looks like (progn BODY...), so strip the progn.
                ,@(cdr body)))))
    
    (`(unless ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (peval--if-value cond
         ;; If we could evaluate the condition, just return the
         ;; simplified body.
         (if it-value
             (list 'value nil)
           body)
       ;; Otherwise, preserve the condition form but simplify the
       ;; body.
       (peval--if-value body
           (list 'partial `(unless ,(cl-second cond) ,it-value))
         (list 'partial `(unless ,(cl-second cond)
                           ;; form looks like (progn BODY...), so strip the progn.
                           ,@(cdr it-form))))))
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
           (peval--if-value current
               (when it-value
                 ;; If the first value is truthy, we can simplify.
                 ;; (or 123 x y) => 123
                 (if (null simple-exprs)
                     (cl-return-from result (list 'value it-value))
                   ;; Otherwise, we will need to build up a list of
                   ;; arguments to `or'.
                   (push it-value simple-exprs)))
             ;; If we couldn't fully evaluate it, we need to preserve it.
             (push it-form simple-exprs)))
         (pcase (nreverse simple-exprs)
           (`() (list 'value nil))
           (`(,expr) (list 'partial expr))
           (`,exprs (list 'partial `(or ,@exprs)))))))

    (`(cond . ,clauses)
     (let (simple-clauses)
       (cl-block result
         (cl-block nil                  ; dolist is not advised in ert-runner
           (dolist (clause clauses)
             (pcase clause
               (`(,condition)
                (peval--if-value (peval--simplify condition bindings)
                    (when it-value
                      ;; If the first clause is truthy, we can simplify.
                      ;; (cond (nil 1) (123) (x y)) => 123
                      (if (null simple-clauses)
                          (cl-return-from result (list 'value it-value))
                        ;; Otherwise, simplify this clause, and terminate
                        ;; this loop, because we will never execute later clauses.
                        ;; (cond (x y) (123) (a b)) => (cond (x y) (123))
                        (progn
                          (push (list it-value) simple-clauses)
                          ;; break from dolist
                          (cl-return))))
                  (push (list it-form) simple-clauses)))
               (`(,condition . ,body)
                (setq body (peval--simplify-progn-body body bindings))
                (peval--if-value (peval--simplify condition bindings)
                    (when it-value
                      ;; If the first clause is truthy, we can simplify.
                      ;; (cond (nil 1) (t 123) (x y)) => 123
                      (if (null simple-clauses)
                          (cl-return-from result body)
                        ;; Otherwise, simplify this clause, and terminate
                        ;; this loop, because we will never execute later clauses.
                        ;; (cond (x y) (t 123) (a b)) => (cond (x y) (t 123))
                        (progn
                          (push `(,it-value ,(cl-second body)) simple-clauses)
                          ;; break from dolist
                          (cl-return))))
                  (push `(,it-form ,(cl-second body)) simple-clauses))))))
         (pcase (nreverse simple-clauses)
           ;; We simplified away all the clauses, so this is just nil.
           (`() (list 'value nil))
           ;; We simplifed to a single clause without a body.
           ;; (cond (a)) => a
           (`((,condition))
            (list 'partial condition))
           ;; We simplified to a single clause with a body.
           ;; (cond (a b c)) => (when a b c)
           (`((,condition (progn . ,progn-body)))
            (list 'partial `(when ,condition ,@progn-body)))
           ;; Return a cond of the clauses that we couldn't simplify.
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

(provide 'peval)
;;; peval.el ends here

