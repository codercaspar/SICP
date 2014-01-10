(load "evaluator")

(defun ambeval (exp env succeed fail)
  (funcall (analyze. exp) env succeed fail))

(defun analyze. (exp)
  (cond ((self-evaluating? exp)
          (analyze-self-evaluating exp))
        ((quoted? exp) 
          (analyze-quoted exp))
        ((variable? exp) 
          (analyze-variable exp))
        ((assignment? exp) 
          (analyze-assignment exp))
        ((definition? exp) 
          (analyze-definition exp))
        ((if? exp) 
          (analyze-if exp))
        ((lambda? exp) 
          (analyze-lambda exp))
        ((let? exp)
          (analyze. (let->combination exp)))
        ((begin? exp) 
          (analyze-sequence (begin-actions exp)))
        ((cond? exp) 
          (analyze. (cond->if exp)))
        ((amb? exp)
          (analyze-amb exp))
        ((application? exp) 
          (analyze-application exp))
        (t
          (error "Unknown expression in EVAL: " exp))))

; amb operator
;
(defun amb? (exp) (tagged-list? exp 'amb))
(defun amb-choices (exp) (cdr exp))

(defun analyze-amb (exp)
  (let ((cprocs (mapcar #'analyze. (amb-choices exp))))
    (lambda (env succeed fail)
      (labels (
          (try-next (choices)
            (if (null choices)
              (funcall fail)
              (funcall (car choices)
                env
                succeed
                (lambda ()
                  (try-next (cdr choices)))))))
        (try-next cprocs)))))

(defun analyze-self-evaluating (exp)
  (lambda (env succeed fail)
    (funcall succeed exp fail)))

(defun analyze-quoted (exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (funcall succeed qval fail))))

(defun analyze-variable (exp)
  (lambda (env succeed fail)
    (funcall succeed (lookup-variable-value exp env) fail)))

(defun analyze-lambda (exp)
  (let ((vars (lambda-parameteres exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (funcall succeed (make-procedure vars bproc env) fail))))

(defun analyze-if (exp)
  (let ((pproc (analyze. (if-predicate exp)))
        (cproc (analyze. (if-consequent exp)))
        (aproc (analyze. (if-alternative exp))))
    (lambda (env succeed fail)
      (funcall pproc 
        env
        ;; success continuation for evaluating the
        ;; predicate to obtain pred-value
        (lambda (pred-value fail2)
          (if (true? pred-value)
            (funcall cproc env succeed fail2)
            (funcall aproc env succeed fail2)))
        ;; failure continuation for evaluating the
        ;; predicate
        fail))))

(defun analyze-sequence (exps)
  (labels (
      (sequentially (proc1 proc2)
        (lambda (env succeed fail)
          (funcall proc1
            env
            ;; success continuation for calling proc1
            (lambda (proc1-value fail2)
              (funcall proc2 env succeed fail2))
            ;; failure continuation for calling proc1
            fail)))
      (sloop (first-proc rest-procs)
        (if (null rest-procs)
          first-proc
          (sloop
            (sequentially first-proc (car rest-procs))
            (cdr rest-procs)))))
    (let ((procs (mapcar #'analyze. exps)))
      (if (null procs)
        (error "Empty sequence in ANALYZE-SEQUENCE"))
      (sloop (car procs) (cdr procs)))))

(defun analyze-definition (exp)
  (let ((var (definition-variable exp))
        (vproc (analyze. (definition-value exp))))
    (lambda (env succeed fail)
      (funcall vproc
        env
        (lambda (val fail2)
          (define-variable! var val env)
          (funcall succeed 'ok fail2))
        fail))))

(defun analyze-assignment (exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze. (assignment-value exp))))
    (lambda (env succeed fail)
      (funcall vproc 
        env
        (lambda (val fail2)      
          (let ((old-value
                  (lookup-variable-value var env)))
            (set-variable-value! var val env)
            (funcall succeed 
              'ok
              (lambda ()         
                (set-variable-value! 
                  var 
                  old-value
                  env)
                (funcall fail2)))))
        fail))))

(defun analyze-application (exp)
  (let ((fproc (analyze. (operator exp)))
        (aprocs (mapcar #'analyze. (operands exp))))
    (lambda (env succeed fail)
      (funcall fproc
        env
        (lambda (proc fail2)
          (get-args
            aprocs
            env
            (lambda (args fail3)
              (execute-application
                proc args succeed fail3))
            fail2))
        fail))))

(defun get-args (aprocs env succeed fail)
  (if (null aprocs)
    (funcall succeed '() fail)
    (funcall (car aprocs)
      env
      (lambda (arg fail2)
        (get-args 
          (cdr aprocs)
          env
          (lambda (args fail3)
            (funcall succeed (cons arg args) fail3))
          fail2))
      fail)))

(defun execute-application (proc args succeed fail)
  (cond ((primitive-procedure? proc)
          (funcall succeed
            (apply-primitive-procedure proc args)
            fail))
        ((compound-procedure? proc)
         (funcall (procedure-body proc)
          (extend-environment 
            (procedure-parameters proc)
            args
            (procedure-env proc))
          succeed
          fail))
        (t
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION ~a"
          proc))))

(deflex input-prompt ";;; Amb-Eval input:")
(deflex output-prompt ";;; Amb-Eval value:")

(defun user-print (object)
  (if (compound-procedure? object)
    (format t "~a~%" 
      (list 
        'compound-procedure
        (procedure-parameters object)
        (procedure-body object)
        '<env>))
    (format t "~a~%" object)))

(defun driver-loop ()
  (labels (
      (internal-loop (try-again)
        (format t "~%~%~a~%" input-prompt)
        (let ((input (read)))
          (if (eql input 'try-again)
            (funcall try-again)
            (progn
              (format t "~%;;; Starting a new problem ")
              (ambeval
                input
                the-global-environment
                (lambda (val next-alternative)
                  (format t "~%~a~%" output-prompt)
                  (user-print val)
                  (internal-loop next-alternative))
                (lambda ()
                  (format t "~%~a~%" ";;; there are no more values of")
                  (user-print input)
                  (driver-loop))))))))
    (internal-loop
      (lambda ()
        (format t "~%;;; there is no current problem")
        (driver-loop)))))
        
(defun interpret (exp)
  (ambeval 
    exp 
    the-global-environment
    (lambda (val fail)
      val)
    (lambda ()
      (format t "FAIL~%"))))

; Adding some useful functions

(interpret
  '(define (require p)
    (if (not p) (amb))))

(interpret
  '(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))))

; for interaction in a lisp console
;
(defun zz ()
  (load "evaluator_amb")
  (driver-loop))





