# Partial Evaluator for Emacs Lisp

Partially evaluate elisp forms. Handy for debugging, and [inspired by
a discussion on Reddit](https://www.reddit.com/r/emacs/comments/60tl6o/tips_on_reading_dense_emacs_lisp_code/dfa92hg/) and
[this talk on Program Slicing](https://www.youtube.com/watch?v=dSqLt8BgbRQ).

## Limitations

* Assumes lexical scope.

* Assumes that if a macro call site does not contain a symbol, than
  that symbol is not modified (TODO: example).
  
* Does not macro expand, because the goal is to simplify
  (e.g. expanding `cl-loop` will generally produce larger
  s-expressions).

* TODO: what about global variables modified by functions?

* TODO: what about setting variables using `set`?
