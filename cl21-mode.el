(defun cl21-keyword-regexp (keywords)
  (concat "(\\("
          (regexp-opt (mapcar #'symbol-name keywords))
          "\\)[ \n]"))

(defvar cl21-keywords
  '(if-let
    once-only
    unwind-protect-case
    when-let
    with-gensyms
    xor
    doeach
    let1
    until
    while
    while-let
    lm
    defsyntax
    with-sequence-iterator))

(define-derived-mode
  cl21-mode lisp-mode "CL21"
  "Major mode for CL21"
  (font-lock-add-keywords nil
                          `((,(cl21-keyword-regexp cl21-keywords)
                             1
                             'font-lock-keyword-face))))
