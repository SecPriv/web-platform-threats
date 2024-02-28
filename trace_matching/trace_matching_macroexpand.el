#!/usr/bin/env -S emacs --script
;; -*- lexical-binding: nil  -*-

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'macroexp)

(defvar macro-funs! nil)

(cl-defmacro define-list-macro (name function)
  `(defmacro ,name (captures bind &rest body)
     (cl-destructuring-bind (list var type) bind
       (let ((new-fun (gensym (intern (format "!%s_%s_%s_" (symbol-name (quote ,name)) (symbol-name list) (symbol-name var)))))
             (parameter (gensym)))
         (cl-multiple-value-bind (return-type fnbody) (funcall ,function new-fun (cl-coerce captures 'list) type parameter var body)
           (push
            `(define-fun-rec ,new-fun ((,parameter (List ,type)) ,@(cl-coerce captures 'list)) ,return-type
               ,fnbody)
            macro-funs!)
           `(,new-fun ,list ,@(mapcar #'car (cl-coerce captures 'list))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS

(define-list-macro all!
  (lambda (name captures type parameter var body)
    (let ((rest (gensym)))
      (cl-values
       'Bool
       `(match ,parameter
               ((nil true)
                ((insert ,var ,rest)
                 (and ,@body
                      (,new-fun ,rest ,@(mapcar #'car captures))))))))))

(define-list-macro any!
  (lambda (name captures type parameter var body)
    (let ((rest (gensym)))
      (cl-values
       'Bool
       `(match ,parameter
               ((nil false)
                ((insert ,var ,rest)
                 (or (and ,@body)
                     (,new-fun ,rest ,@(mapcar #'car captures ))))))))))
  
(define-list-macro map!
  (lambda (name captures type parameter var body)
    (let ((rest (gensym)))
      (cl-assert (eql (length body) 1))
      (cl-values
       `(List ,type)
       `(match ,parameter
               ((nil (as nil (List ,type)))
                ((insert ,var ,rest) (insert ,@body (,name ,rest ,@captures)))))))))
  
(define-list-macro filter!
  (lambda (name captures type parameter var body)
    (let ((rest (gensym)))
      (cl-assert (eql (length body) 1))
      (cl-values
       `(List ,type)
       `(match ,parameter
               ((nil nil)
                ((insert ,var ,rest)
                 (ite ,@body (insert ,var (,new-fun ,rest ,@(mapcar #'car captures)))
                      (,new-fun ,rest ,@(mapcar #'car captures))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPAND
;;
;; Expand and print all macros found in stdin

(defconst smtlib-macro-env (list #'all! #'any!))

(cl-defun expand-form (form &optional (env smtlib-macro-env))
  (let ((macro-funs! nil))
    (let ((out (macroexpand-all form env)))
      (append macro-funs! (list out)))))


(defun slurp-stdin ()
  (condition-case nil (while t (insert (read-from-minibuffer "") "\n"))
    (error)))

(defun print-fix (object)
  (let ((str (with-output-to-string (pp object) (princ "\n"))))
    (princ (replace-regexp-in-string "\\\\." "." str))))

(with-temp-buffer
  (slurp-stdin)
  (goto-char (point-min))
  (let ((done nil))
    (while (not done)
      (condition-case test
          (let* ((expr (read (current-buffer)))
                 (expanded (expand-form expr)))
            (mapc #'print-fix expanded))
        (end-of-file (setf done t))))))
