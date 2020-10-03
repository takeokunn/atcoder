;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      anaphoric macros      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
     ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args))))))
               args)))

(defmacro amap (form list)
  `(mapcar #'(lambda (it) ,form) ,list))

(defmacro aevery (form list)
  `(every #'(lambda (it) ,form) ,list))

(defmacro areduce (form list &key initial-value)
  `(reduce #'(lambda (it-accum it) ,form) ,list :initial-value ,initial-value))

;;;;;;;;;;;;;;;;;;;;;;
;;      string      ;;
;;;;;;;;;;;;;;;;;;;;;;
(defmacro string+ (&rest str)
  `(concatenate 'string ,@str))

(defmacro every-string= (c sequence)
  `(aevery (string= it ,c) ,sequence))

;;;;;;;;;;;;;;;;;;;;
;;      char      ;;
;;;;;;;;;;;;;;;;;;;;
(defmacro char-1+ (char)
  `(code-char (1+ (char-code ,char))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;      something      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nested-loops (vars values &rest body)
  (if vars
      `(loop :for ,(first vars) :from ,(car (first values)) :to ,(cdr (first values))
             :do (nested-loops ,(rest vars) ,(rest values) ,@body))
      `(progn ,@body)))

(defmacro nested-nth (idxs my-lst)
  `(if ,idxs (nth (car (reverse ,idxs)) (nested-nth (cdr (reverse ,idxs)) ,my-lst)) ,my-lst))


(defun get-even-list (lst)
  (loop :with return-lst = '()
        :for index :from 0 :below (length lst)
        :unless (zerop (mod index 2))
          :do (setf return-lst (append return-lst (list (nth index lst))))
        :finally (return return-lst)))
