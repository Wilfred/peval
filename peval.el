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

;; Backport of
;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=dc79aa10f117dea1204634626a5f96a21722807f
(when (< emacs-major-version 26)
  (put 'keywordp 'side-effect-free 'error-free))

(defun peval--source (sym)
  "Return the source code of SYM as an s-expression."
  (-if-let ((buf . start-pos) (peval--definition sym))
      (with-current-buffer buf
        (save-excursion
          (goto-char start-pos)
          (read (current-buffer))))
    ;; Could not find source -- probably defined interactively, or via
    ;; a macro, or file has changed, or a primitive.
    (indirect-function sym)))

(defun peval--definition (sym)
  "Return a pair (BUF . POS) where SYM is defined."
  (let (buf-and-pos)
    (ignore-errors
      (setq buf-and-pos
            (find-function-noselect sym)))
    (if buf-and-pos
        buf-and-pos
      ;; If it's defined interactively, it may have an edebug property
      ;; that tells us where it's defined.
      (-when-let (marker (get sym 'edebug))
        (cons (marker-buffer marker)
              (marker-position marker))))))

(define-derived-mode peval-mode emacs-lisp-mode "Partial Eval"
  "Major mode for partially evaluating elisp functions.")

(defun peval ()
  (interactive)
  (let* ((buf (get-buffer-create "*peval*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert ";; Specify function arguments\n"
                "(-slice '(2 3 4 5) _ 3)\n"
                "\n"
                ";; Simplified function (press C-c C-c to update):")
        (peval-update)
        (peval-mode)))
    (switch-to-buffer buf)))

(define-key peval-mode-map (kbd "C-c C-c") #'peval-update)

(defconst peval-placeholder
  (make-symbol "peval-placeholder")
  "A unique symbol representing _ in forms given.")

(defun peval--zip-bindings (bindings-given raw-func-args)
  "Given BINDINGS-GIVEN of the form (1 2 3) and RAW-FUNC-ARGS
of the form (x y &optional z), return a list of zipped pairs."
  (let ((bindings-i 0)
        (args-i 0)
        (required-args t)
        result)
    ;; All the bindings we have been given for arguments.
    (while (< bindings-i (length bindings-given))
      (let ((binding (nth bindings-i bindings-given))
            (arg (nth args-i raw-func-args)))
        (cond
         ((eq arg '&optional)
          (setq required-args nil)
          (cl-incf args-i))
         ((eq arg '&rest)
          (error "todo"))
         ;; Skip placeholders.
         ((eq binding peval-placeholder)
          (cl-incf bindings-i)
          (cl-incf args-i))
         ;; Match up an argument with the binding given.
         (t
          (push (cons arg binding) result)
          (cl-incf bindings-i)
          (cl-incf args-i)))))
    ;; All the remaining optional arguments.
    (while (< args-i (length raw-func-args))
      (let ((arg (nth args-i raw-func-args)))
        (cond
         ((not required-args)
          (push (cons arg nil) result))
         ((eq arg '&optional)
          (setq required-args nil)))
        (cl-incf args-i)))
    (nreverse result)))

(defun peval-update ()
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
      ;; to convert (foo tab-width) => (foo 4)
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
              (peval-result-value
               (peval--simplify
                fn-body
                (peval--zip-bindings bindings-given fn-args))))
             (simple-fn `(defun ,fn-name ,fn-args
                           ,simple-body)))
        (cl-prettyprint simple-fn)))))

(defun peval--simplify-progn-body (forms bindings)
  "Simplify all the forms in FORMS using partial application.
If any form evaluates to a simple value, discard it unless
it is the final form."
  (let (simplified-exprs current)
    ;; Evaluate every expression in the progn body.
    (dolist (form forms)
      (setq current (peval--simplify form bindings))
      (setq bindings (peval-result-bindings current))
      ;; If we evaluated the expression to a value, just throw it
      ;; away.
      (unless (peval-result-evaluated-p current)
        (push current simplified-exprs)))
    ;; If the last expression was a value, we still need to return
    ;; it.
    (when (and current (peval-result-evaluated-p current))
      (push current simplified-exprs))
    
    (setq simplified-exprs (nreverse simplified-exprs))
    (if (= (length simplified-exprs) 1)
        (car simplified-exprs)
      (make-peval-result
       :evaluated-p nil
       :value `(progn
                 ,@(mapcar #'peval-result-value simplified-exprs))
       :bindings bindings))))

(defun peval--progn-body-safe (form)
  "Strip the leading 'progn in FORM, if present.
Always returns a list.

'(progn x y) => '(x y)
'(a b) => '((a b))
1 => '(1)"
  (cond
   ((eq (car-safe form) 'progn)
    (cdr form))
   (t
    (list form))))

(defun peval--simplify-let (exprs let-bindings bindings)
  (let ((bindings-inside bindings)
        unknown-bindings
        simple-body)
    ;; TODO: this assumes `let*' semantics, handle `let' too.
    (dolist (let-binding let-bindings)
      (pcase let-binding
        (`(,sym ,form)
         ;; Evaluate form, and add it to the known bindings if we can
         ;; fully evaluate it.
         (let* ((val (peval--simplify form bindings)))
           ;; TODO: Handle unknown bindings properly, adding it to
           ;; BINDINGS with a sentinel value.
           (if (peval-result-evaluated-p val)
               (setq bindings-inside
                     (peval--set-variable sym (peval-result-value val)
                                          bindings-inside))
             (push
              (list sym (peval-result-value val))
              unknown-bindings))))
        (`,sym
         ;; (let (x) ...) is equivalent to (let ((x nil)) ...).
         (setq bindings-inside
               (peval--set-variable sym nil bindings-inside)))))
    (setq unknown-bindings (nreverse unknown-bindings))

    ;; TODO: propagate assignments to free variables, e.g.
    ;; (let (x) (setq y 1))
    (setq simple-body (peval--simplify-progn-body exprs bindings-inside))

    (if (peval-result-evaluated-p simple-body)
        (make-peval-result
         :evaluated-p t
         :value (peval-result-value simple-body)
         :bindings bindings)

      ;; (let () x) => x
      ;; TODO: is it safe to discard bindings if the let body contains
      ;; unknown macros?
      (if (null unknown-bindings)
          (make-peval-result
           :evaluated-p nil
           :value (peval-result-value simple-body)
           :bindings bindings)
        ;; a progn can be added by `peval--simplify-progn-body', so
        ;; (let _ (progn x y)) => (let _ x y)
        (make-peval-result
         :evaluated-p nil
         :value
         `(let ,unknown-bindings
            ,@(peval--progn-body-safe (peval-result-value simple-body)))
         :bindings bindings)))))

(cl-defstruct peval-result
  "Structure that represents the result of partially evaluating
an s-expression.

Slots:

`evaluated-p'
    Whether we were able to fully evaluate the form given.

`value'
    The result of evaluating the form. This may be the original form,
    a simplified version of that form, or a simple value.

`bindings'
    Variables whose value is known after evaluating the form."
  
  evaluated-p value bindings)

(defun peval--set-variable (symbol value bindings)
  "Return a new BINDINGS list with SYMBOL set to VALUE.
Does not modify BINDINGS."
  ;; todo: if symbol isn't handle current scope vs global scope.
  (cons (cons symbol value) bindings))

;; TODO: ensure we propagate bindings (we should never call
;; peval--simplify without updating bindings afterwards).
(defun peval--simplify-list (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
FORM must be a cons cell."
  (pcase form
    (`(if ,cond ,then)
     (peval--simplify `(if ,cond ,then nil) bindings))
    (`(if ,cond ,then . ,else)
     (setq cond (peval--simplify cond bindings))
     (setq then (peval--simplify then bindings))
     (setq else (peval--simplify-progn-body else bindings))
     (cond
      ;; If we can evaluate the if condition, then simplify to just the
      ;; THEN or the ELSE.
      ((peval-result-evaluated-p cond)
       (if (peval-result-value cond) then else))
      ;; `peval--simplify-progn-body' may have added a progn, so
      ;; simplify: (if _ _ (progn x y)) => (if _ _ x y)
      ((not (peval-result-evaluated-p else))
       (make-peval-result
        :evaluated-p nil
        :value `(if ,(peval-result-value cond)
                    ,(peval-result-value then)
                  ,@(peval--progn-body-safe (peval-result-value else)))
        :bindings bindings))
      ;; Otherwise, return an if where we have simplified as much as
      ;; we can.
      (t
       (make-peval-result
        :evaluated-p nil
        :value `(if ,(peval-result-value cond)
                    ,(peval-result-value then)
                  ,(peval-result-value else))
        :bindings bindings))))

    ;; Discard (declare ...) forms.
    (`(declare . ,_)
     (make-peval-result
      :evaluated-p t :value nil
      :bindings bindings))

    ;; Remove pointless values in progn, e.g.
    ;; (progn nil (foo) (bar)) -> (progn (foo) (bar))
    (`(progn . ,exprs)
     (peval--simplify-progn-body exprs bindings))

    (`(let ,let-bindings . ,exprs)
     (peval--simplify-let exprs let-bindings bindings))
    
    (`(when ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (cond
      ;; (when t _) => _
      ((and
        (peval-result-evaluated-p cond)
        (peval-result-value cond))
       body)
      ;; (when nil _) => nil
      ((peval-result-evaluated-p cond)
       (make-peval-result
        :evaluated-p t :value nil
        :bindings bindings))
      ;; If we've fully evaluated the body, but not the condition.
      ((peval-result-evaluated-p body)
       (make-peval-result
        :evaluated-p nil
        :value `(when ,(peval-result-value cond)
                  ,(peval-result-value body))
        :bindings bindings))
      ;; Partially evaluated form.
      (t
       (make-peval-result
        :evaluated-p nil
        :value `(when ,(peval-result-value cond)
                  ;; body looks like (progn BODY...), so strip the progn.
                  ,@(peval--progn-body-safe (peval-result-value body)))
        :bindings bindings))))
    
    (`(unless ,cond . ,body)
     (setq cond (peval--simplify cond bindings))
     (setq body (peval--simplify-progn-body body bindings))
     (cond
      ;; (unless nil _) => _
      ((and
        (peval-result-evaluated-p cond)
        (not (peval-result-value cond)))
       body)
      ;; (unless t _) => nil
      ((peval-result-evaluated-p cond)
       (make-peval-result
        :evaluated-p t :value nil
        :bindings bindings))
      ;; Partially evaluated form.
      (t
       (make-peval-result
        :evaluated-p nil
        :value `(unless ,(peval-result-value cond)
                  ;; body looks like (progn BODY...), so strip the progn.
                  ,@(peval--progn-body-safe (peval-result-value body)))
        :bindings bindings))))
    
    ;; TODO: backquote.
    (`(quote ,sym)
     (make-peval-result
      :evaluated-p t :value sym
      :bindings bindings))
    
    ;; TODO: (setq x _ y _)
    ;; TODO: consider aliasing of mutable values (e.g. two variables
    ;; pointing to the same list).
    (`(setq ,sym ,val)
     (setq val (peval--simplify val bindings))
     (if (peval-result-evaluated-p val)
         (make-peval-result
          :evaluated-p t
          :value (peval-result-value val)
          :bindings (peval--set-variable sym (peval-result-value val)
                                         bindings))
       ;; TODO: record that value of symbol is no longer known, even
       ;; if it was before.
       (make-peval-result
        :evaluated-p nil
        :value `(setq ,sym ,(peval-result-value val))
        :bindings bindings)))

    (`(or . ,exprs)
     (let (simple-exprs
           current)
       (cl-block nil                  ; dolist is not advised in `ert-runner'
         (dolist (expr exprs)
           (setq current (peval--simplify expr bindings))
           (cond
            ;; If a value is truthy, we can simplify.
            ;; (or x t y) => (or x t)
            ((and
              (peval-result-evaluated-p current)
              (peval-result-value current))
             (push current simple-exprs)
             (cl-return))              ; break from dolist
            ;; If the current value is evaluated and falsy, discard
            ;; it.
            ((peval-result-evaluated-p current))
            ;; Otherwise, we will need to build up a list of
            ;; unevaluated arguments to `or'.
            (t
             (push current simple-exprs)))))

       (setq simple-exprs (nreverse simple-exprs))
       (cond
        ;; (or) => nil
        ((null simple-exprs)
         (make-peval-result
          :evaluated-p t :value nil
          :bindings bindings))
        ;; (or _) => _
        ((= (length simple-exprs) 1)
         (car simple-exprs))
        ;; Partially evaluated or.
        (t
         (make-peval-result
          :evaluated-p nil
          :value `(or ,@(mapcar #'peval-result-value simple-exprs))
          :bindings bindings)))))

    (`(cond . ,clauses)
     (let (simple-clauses)
       (cl-block nil           ; dolist is not advised in ert-runner
         (dolist (clause clauses)
           (pcase clause
             (`(,condition)
              (setq condition (peval--simplify condition bindings))

              (cond
               ;; If this clause is falsy, discard it.
               ;; (cond (nil) (y z)) => (cond (y z))
               ((and
                 (peval-result-evaluated-p condition)
                 (not (peval-result-value condition))))
               ;; If this clause is truthy, we can simplify.
               ;; (cond (x y) (123) (a b)) => (cond (x y) (123))
               ((peval-result-evaluated-p condition)
                (push (list condition) simple-clauses)
                ;; Break from dolist, because we will never execute
                ;; later clauses.
                (cl-return))
               ;; Otherwise, build up the list of simplified clauses.
               (t
                (push (list condition) simple-clauses))))
             
             (`(,condition . ,body)
              (setq condition (peval--simplify condition bindings))

              (cond
               ;; If this clause is falsy, discard it.
               ;; (cond (nil x) (y z)) => (cond (y z))
               ((and
                 (peval-result-evaluated-p condition)
                 (not (peval-result-value condition))))
               ;; If this clause is truthy, we can simplify
               ;; (cond (x y) (t z) (a b)) => (cond (x y) (t z))
               ((peval-result-evaluated-p condition)
                (push (list condition
                            (peval--simplify-progn-body body bindings))
                      simple-clauses)
                ;; Break from dolist, because we will never execute
                ;; later clauses.
                (cl-return))
               ;; Otherwise, build up the list of simplified clauses.
               (t
                (push (list condition
                            (peval--simplify-progn-body body bindings))
                      simple-clauses)))))))

       (pcase (nreverse simple-clauses)
         ;; We simplified away all the clauses, so this is just nil.
         ;; (cond) => nil
         (`()
          (make-peval-result
           :evaluated-p t :value nil
           :bindings bindings))
         ;; We simplifed to a single clause without a body.
         ;; (cond (a)) => a
         (`((,condition))
          condition)
         ;; We simplified to a single clause with a body.
         (`((,condition ,body))
          (cond
           ;; (cond (t x)) => x
           ((and
             (peval-result-evaluated-p condition)
             (peval-result-value condition))
            body)
           ;; (cond (nil x)) => nil
           ((peval-result-evaluated-p condition)
            (make-peval-result
             :evaluated-p nil
             :value nil
             :bindings bindings))
           ;; (cond (a b c)) => (when a b c)
           (t
            (make-peval-result
             :evaluated-p nil
             :value `(when ,(peval-result-value condition)
                       ,@(peval--progn-body-safe (peval-result-value body)))
             :bindings bindings))))
         ;; Return a cond of the clauses that we couldn't simplify.
         (`,clauses
          (make-peval-result
           :evaluated-p nil
           :value (let ((clause-forms
                         (--map
                          (pcase it
                            (`(,condition)
                             (list (peval-result-value condition)))
                            (`(,condition ,body)
                             (cons (peval-result-value condition)
                                   (peval--progn-body-safe (peval-result-value body)))))
                          clauses)))
                    `(cond ,@clause-forms))
           :bindings bindings)))))

    ;; Function call.
    ((and `(,fn . ,args) (guard (functionp fn)))
     (setq args (--map (peval--simplify it bindings) args))
     ;; If it's a pure function, and we could evaluate all the
     ;; arguments, call it.
     (if (and
          (--all-p (peval-result-evaluated-p it) args)
          (get fn 'side-effect-free))
         (make-peval-result
          :evaluated-p t
          :value (apply fn (mapcar #'peval-result-value args))
          :bindings bindings)
       (make-peval-result
        :evaluated-p nil
        :value `(,fn ,@(mapcar #'peval-result-value args))
        :bindings bindings)))
    (`(,fn . ,args)
     ;; Either a function we don't know about, or a macro. We can't
     ;; simplify because we don't know which arguments are evaluated.
     (make-peval-result
      :evaluated-p nil
      :value form
      :bindings bindings))

    (_ (error "Don't know how to simplify list: %s" form))))

(defun peval--simplify-atom (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application."
  (cond
   ;; Symbols that we don't look up BINDINGS.
   ((eq form nil)
    (make-peval-result
     :evaluated-p t :value nil
     :bindings bindings))
   ((eq form t)
    (make-peval-result
     :evaluated-p t :value t
     :bindings bindings))

   ;; Keywords (which are symbols) evaluate to themselves.
   ((keywordp form)
    (make-peval-result
     :evaluated-p t :value form
     :bindings bindings))
   
   ;; We can evaluate a symbol if it is present in BINDINGS.
   ((symbolp form)
    (if (assoc form bindings)
        (make-peval-result
         :evaluated-p t :value (alist-get form bindings)
         :bindings bindings)
      (make-peval-result
       :evaluated-p nil :value form
       :bindings bindings)))
   ;; Other atoms (strings, keywords, integers, characters) just
   ;; evaluate to themselves.
   (t
    (make-peval-result
     :evaluated-p t :value form
     :bindings bindings))))

(defun peval--simplify (form bindings)
  "Simplify FORM in the context of BINDINGS using partial application.
Loops are not executed and side-effecting functions are not run.

Returns a `peval-value' struct."
  (cond
   ((atom form)
    (peval--simplify-atom form bindings))
   (t
    (peval--simplify-list form bindings))))

(provide 'peval)
;;; peval.el ends here

