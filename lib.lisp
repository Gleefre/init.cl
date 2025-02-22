(in-package #:sky-user)

;;; alexandria / serapeum / ... subset

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body)))

(defmacro defenum ((&key (start 0) (step 1))
                   &rest identifiers)
  (declare (type integer start step))
  `(progn
     ,@(loop for value = start then (+ start step)
             for id in identifiers
             for (sym . docstring) = (ensure-list id)
             collect `(defconstant ,sym ,value ,@docstring))))

(defun clamp (x low high)
  (min (max x low) high))

(define-modify-macro clampf (low high) clamp)
(define-modify-macro multf (&rest numbers) *)

(defun compose (f g)
  (lambda (&rest arguments)
	(funcall f (apply g arguments))))

;;; Sequences (?)

(defun sort-as (vec1 vec2 predicate &key (key #'identity))
  (map 'vector
       #'car
       (sort (map 'vector #'cons vec1 vec2)
             predicate
             :key (compose key #'cdr))))

;;; Stuff

(defun variable-type (symbol)
  (check-type symbol symbol)
  (eval
   `(let ((,symbol :lexical))
      (flet ((result () ,symbol))
        (let ((,symbol :special))
          (declare (ignorable ,symbol))
          (result))))))

(defun special-variable-p (symbol)
  (eq (variable-type symbol) :special))

(declaim (inline place (setf place)))

(defun place (getter setter)
  (declare (ignore setter))
  (funcall getter))

(defun (setf place) (value getter setter)
  (declare (ignore getter))
  (funcall setter value))

(macrolet ((def (nname name)
             `(defun ,nname (obj1 obj2)
                (not (,name obj1 obj2)))))
  (def neq     eq)
  (def neql    eql)
  (def nequal  equal)
  (def nequalp equalp))

(macrolet ((def (vname name clause)
             `(defun ,vname (&rest objects)
                (loop with one = (car objects)
                      for another in (cdr objects)
                      ,clause (,name one another)))))
  (def eq*      eq      always)
  (def eql*     eql     always)
  (def equal*   equal   always)
  (def equalp*  equalp  always)
  (def neq*     neq     thereis)
  (def neql*    neql    thereis)
  (def nequal*  nequal  thereis)
  (def nequalp* nequalp thereis))

;;; Tricky macros

(defmacro tmlet (definitions &body body &environment env)
  `(macrolet (,@(loop for (name lambda-list . body) in definitions
                      for transfer = (gensym (concatenate 'string "%" (symbol-name name)))
                      append `((,transfer (&rest args &environment env)
                                          (funcall ,(macro-function name env)
                                                   `(,',name ,@args)
                                                   env))
                               (,name ,lambda-list
                                      (let ((,name ',transfer))
                                        ,@body)))))
     ,@body))

(defmacro tmlet* (definitions &body body)
  (if (null definitions)
      `(locally ,@body)
      `(tmlet (,(car definitions))
         (tmlet* (,@(cdr definitions))
           ,@body))))

(eval-always
  (defmacro at-compile-time ((&optional env) &body body)
    (let ((capture (gensym)))
      `(macrolet ((,capture (,@(when env `(&environment ,env)))
                    ,@body))
         (,capture)))))

;;; compile-time-let

(defun signal-compile-time-value-error (name)
  (error "There is no COMPILE-TIME-VALUE assosiated with name ~S" name))

(defun (setf signal-compile-time-value-error) (value name)
  (error "Attempting to set non-existing COMPILE-TIME-VALUE with name ~S to ~S" name value))

(defmacro compile-time-value (name &optional (default nil defaultp))
  (unless defaultp
    (warn "COMPILE-TIME-VALUE used outside of COMPILE-TIME-LET or COMPILE-TIME-VALUE-LET."))
  (if defaultp
      default
      `(signal-compile-time-value-error ',name)))

(defmacro compile-time-value-let (bindings &body body)
  (let ((specs (loop for (name value) in (mapcar #'ensure-list bindings)
                     collect (list name (gensym "%PROXY") value))))
    ;; Proxy is a compile-time function (a local macro) that computes the compile-time
    ;; value for the variable and then returns a pair of closures that act as a
    ;; PLACE. This additional step is needed to ensure that VALUE is evaluated in the
    ;; correct environment.
    `(macrolet (,@(loop for (nil %proxy value) in specs
                        for %var = (gensym "%VAR")
                        collect `(,%proxy ()
                                   (let ((,%var ,value))
                                     (values (lambda () ,%var)
                                             (lambda (v) (setf ,%var v)))))))
       (at-compile-time (env)
         ;; %PROXY gets called here, and the resulting place is inlined.
         ;;
         ;; There are two things to consider:
         ;;  - COMPILE-TIME-VALUE-LET can be expanded multiple times;
         ;;  - COMPILE-TIME-VALUE can be expanded multiple times.
         ;;
         ;; All expansion of COMPILE-TIME-VALUE forms inside the body which are not
         ;; shadowed will always use the same definition of COMPILE-TIME-VALUE (latest
         ;; one?), and thus the same pair of functions as the place no matter how many
         ;; times COPMILE-TIME-VALUE form is macroexpanded.
         ;;
         ;; That means that the value form can be evaluated multiple times, but the place
         ;; referenced by COMPILE-TIME-VALUE will always be the same.
         `(tmlet ((compile-time-value (name &rest args)
                    (case name
                      ,@(loop for (name %proxy) in ',specs
                              collect `(,name '(place ,@(multiple-value-list
                                                         (funcall (macro-function %proxy env)
                                                          `(,%proxy)
                                                          env)))))
                      (t `(,compile-time-value ,name ,@args)))))
            ,@',body)))))

(defmacro compile-time-let (bindings &body body)
  `(compile-time-value-let ,bindings
     (symbol-macrolet (,@(loop for (var) in bindings
                               collect `(,var (compile-time-value ,var))))
       ,@body)))

;;; G! / CG!

(defun cg! (name)
  (declare (ignore name))
  (error "CG! called outside of WITH-G!"))

(defmacro g! (name)
  `(cg! ',name))

(defmacro with-g! (&body body)
  (let ((gensyms (gensym (string 'gensyms))))
    `(let ((,gensyms (make-hash-table :test 'equal)))
       (flet ((cg! (name)
                (or (gethash name ,gensyms)
                    (setf (gethash name ,gensyms)
                          (gensym (with-standard-io-syntax
                                    (princ-to-string name)))))))
         ,@body))))

(defmacro defmacro! (name lambda-list &body body)
  `(defmacro ,name ,lambda-list
     (with-g! ,@body)))

(macrolet ((def (m! m)
             `(defmacro ,m! (definitions &body body)
                `(,',m (,@(loop for (name lambda-list . body) in definitions
                                collect `(,name ,lambda-list (with-g! ,@body))))
                       ,@body))))
  (def macrolet! macrolet)
  (def tmlet! tmlet)
  (def tmlet*! tmlet*))

#-slow
(at-compile-time ()
  `(progn
     ,@(loop for m in '(macrolet! tmlet tmlet* tmlet! tmlet*!)
             collect `(ti:define-indentation ,m (as flet)))))

;;; Computed go

(deftype go-tag ()
  '(or symbol integer))

(defun go-tag-p (object)
  (typep object 'go-tag))

(defmacro cgo (clabel)
  `(at-compile-time ()
     `(ecase ,',clabel
        ,@(loop for tag in (compile-time-value %tags ())
                collect `((,tag) (go ,tag))))))

(defmacro ctagbody (&rest statements)
  (let ((tags (remove-if-not #'go-tag-p statements)))
    `(compile-time-let ((%tags (union ',tags (compile-time-value %tags ()))))
       (tagbody ,@statements))))

#-slow
(ti:define-indentation ctagbody (as tagbody))

;;; trivial loops

(defmacro! dowhile (cond &body body)
  `(block nil
     (tagbody
      ,(g! start)
        ,@body
        (when ,cond
          (go ,(g! start))))))

(defmacro! dountil (cond &body body)
  `(block nil
     (tagbody
      ,(g! start)
        ,@body
        (unless ,cond
          (go ,(g! start))))))

(defmacro! while (cond &body body)
  `(block nil
     (tagbody
        ,(g! loop)
        (unless ,cond
          (go ,(g! end)))
        ,@body
        (go ,(g! loop))
        ,(g! end))))

(defmacro! until (cond &body body)
  `(block nil
     (tagbody
        ,(g! loop)
        (when ,cond
          (go ,(g! end)))
        ,@body
        (go ,(g! loop))
        ,(g! end))))

;;; WITH-LOCALS

;; environments
(defun make-env (parent-env)
  (cons (make-hash-table :test 'eql) parent-env))

(defun lookup-env (env name)
  (if (null env)
      (error "Undefined LOCAL named ~S" name)
      (multiple-value-bind (value success) (gethash name (car env))
        (if success
            value
            (lookup-env (cdr env) name)))))

(defun top-table-env (env name)
  (labels ((rec (env)
             (and (consp env)
                  (if (nth-value 1 (gethash name (car env)))
                      (car env)
                      (rec (cdr env))))))
    (or (rec env) (car env))))

(defun set-env (env name value)
  (setf (gethash name (top-table-env env name)) value))

(defmacro get-locals-env () nil)

(defmacro with-new-env ((&optional (var (gensym "env"))) &body body)
  `(let ((,var (make-env (get-locals-env))))
     (macrolet ((get-locals-env () ',var))
       ,@body)))

;; main stuff
(defmacro! with-locals ((&optional scope-name) &body body)
  `(with-new-env (,(g! env))
     (tmlet ((local (name &optional (scope-name nil scope-name-p))
               (if scope-name-p
                   (if (neq scope-name ',scope-name)
                       `(,local ,name ,scope-name)
                       `(place (lambda () (lookup-env ,',(g! env) ',name ',scope-name))))
                   (with-g!
                     `(place (lambda () (lookup-env ,',(g! env) ',name))
                             (lambda (,(g! value)) (set-env ,',(g! env) ',name ,(g! value))))))))
       ,@body)))

(defmacro local (name &optional scope-name)
  (warn "LOCAL used outside of WITH-LOCALS (~S in scope ~S)" name scope-name)
  `(error "Undefined LOCAL named ~S" ',name))

(defmacro deflocal (&rest scope-name-and-value)
  (destructuring-bind (value name &optional (scope nil scopep))
      (reverse scope-name-and-value)
    `(setf (local ,name ,@(when scopep `(,scope))) ,value)))

;; TODO: LOCALS-LET

;;; Math

(defun factors (n)
  (sort (remove-duplicates (loop for i from 1
                                 for n/i = (/ n i)
                                 while (<= i n/i)
                                 if (integerp n/i)
                                 collect i
                                 and collect n/i))
        #'<))

(defun vp (n p)
  (loop for deg from 0
        while (zerop (mod n p))
        do (setf n (/ n p))
        finally (return (values deg n))))

(defun factorize (num)
  (loop for i from 2
        for n = num then new-n
        for (deg new-n) = (multiple-value-list (vp n i))
        while (> n 1)
        when (plusp deg)
        collect (cons i deg)))

(defun radians (degrees)
  (* pi (/ degrees 180)))

(defun degrees (radians)
  (* 180 (/ radians pi)))

(declaim (inline sq))
(defun sq (x) (* x x))

(defun sqrt* (x)
  (flet ((isqrt-p (x)
           (and (integerp x)
                (not (minusp x))
                (let ((y (isqrt x)))
                  (when (= x (sq y))
                    y)))))
    (typecase x
      (integer (or (isqrt-p x) (sqrt x)))
      (rational (let ((n (isqrt-p (numerator x)))
                      (m (isqrt-p (denominator x))))
                  (if (and n m)
                      (/ n m)
                      (sqrt x))))
      (t (sqrt x)))))

(defun solve-sq (a b c)
  (let ((d-sqrt (sqrt* (- (* b b) (* 4 a c)))))
    (values (/ (+ (- b) d-sqrt) 2 a)
            (/ (- (- b) d-sqrt) 2 a))))

(defun solve-sq* (a b c)
  (let ((d-sqrt `(sqrt ,(- (* b b) (* 4 a c)))))
    (values `(/ (+ ,(- b) ,d-sqrt) ,(* 2 a))
            `(/ (- ,(- b) ,d-sqrt) ,(* 2 a)))))

(defun digits (n &optional (base 10))
  (check-type n (integer 0 *))
  (let ((result))
    (dowhile (plusp n)
      (multiple-value-bind (q r)
          (floor n base)
        (setf n q)
        (push r result)))
    result))

(defun primep (num)
  (and
   (> num 1)
   (loop for i from 2 while (<= (sq i) num)
         never (zerop (mod num i)))))

;;; ranges

(defun <=< (min x max)
  (and (<= min x) (< x max)))

(defun <<= (min x max)
  (and (< min x) (< x max)))

;;; Random stuff

(declaim (inline random-between random-chance))

(defun random-between (a b)
  (+ a (random (- b a))))

(defun random-chance (q)
  (cond ((<= 0 q 1)
         (etypecase q
           (rational (< (random (denominator q))
                        (numerator q)))
           (float (< (random 1d0) q))))
        ((< 1 q) t)
        (t nil)))

;;; Weird stuff

(defun python-list (list)
  (format t "[~{~s~^, ~}]~%" list))

;;; Permutations

(defun permutation-sign (p &aux (n (length p)) (s (make-array n :initial-contents p)))
  (loop with sign = 1
        for i below n
        do (loop for j = (aref s i)
                 until (= i j)
                 do (rotatef (aref s i) (aref s j))
                    (multf sign -1))
        finally (return sign)))

;;; Polynoms

(defstruct (polynom (:constructor polynom (&rest coeffs
                                           &aux (coefficients
                                                 (coerce (reverse (member-if-not #'zerop (reverse coeffs)))
                                                         'vector)))))
  coefficients)

(defun polynom-degree (p)
  (max 0 (1- (length (polynom-coefficients p)))))

(defun polynom-coefficient (p k)
  (if (< k (length (polynom-coefficients p)))
      (aref (polynom-coefficients p) k)
      0))

(defun coeff->string (coeff power beginp &aux (plusp (plusp coeff)) (onep (and (= 1 (abs coeff)) (not (zerop power)))))
  (cond ((zerop coeff) "")
        ((and beginp onep)
         (format nil "~:[-~;~]~[~;x~:;x^~:*~A~]" plusp power))
        (beginp
         (format nil "~A~[~; x~:; x^~:*~A~]" coeff power))
        (t
         (format nil "~:[-~;+~]~:[ ~A~;~*~]~[~; x~:; x^~:*~A~]" plusp onep (abs coeff) power))))

(defun coeffs->string (coeffs)
  (loop for power from 0
        for coeff across coeffs
        for str = (coeff->string coeff power (null strs))
        unless (string= "" str) collect str into strs
        finally (return (if strs (format nil "~{~A~^ ~}" strs) "0"))))

(defmethod print-object ((polynom polynom) stream)
  (format stream "#<P ~A>" (coeffs->string (polynom-coefficients polynom))))

(defun polynom+2 (p1 p2)
  (apply #'polynom (loop for x below (max (length (polynom-coefficients p1))
                                          (length (polynom-coefficients p2)))
                         collect (+ (polynom-coefficient p1 x)
                                    (polynom-coefficient p2 x)))))

(defun polynom+ (&rest ps)
  (reduce #'polynom+2 ps :initial-value (polynom)))

(defun polynom-2 (p1 p2)
  (apply #'polynom (loop for x to (max (polynom-degree p1) (polynom-degree p2))
                         collect (- (polynom-coefficient p1 x)
                                    (polynom-coefficient p2 x)))))

(defun polynom- (p &rest ps)
  (polynom-2 p (apply #'polynom+ ps)))

(defun polynom*2 (p1 p2)
  (apply #'polynom (loop for x to (+ (polynom-degree p1) (polynom-degree p2))
                         collect (loop for y to x
                                       sum (* (polynom-coefficient p1 y)
                                              (polynom-coefficient p2 (- x y)))))))

(defun polynom* (&rest ps)
  (reduce #'polynom*2 ps :initial-value (polynom 1)))


(defun mat->cmat (mat &aux (n (array-dimension mat 0)))
  (make-array (list n n)
              :initial-contents (loop for x below n
                                      collect (loop for y below n
                                                    collect (polynom (aref mat x y) (if (= x y) -1 0))))))

(defun pmat-det (mat &aux (n (array-dimension mat 0)))
  (let ((result (polynom)))
    (a:map-permutations
     (lambda (s)
       (let ((summand (apply #'polynom*
                             (polynom (permutation-sign s))
                             (loop for i from 0 for j in s
                                   collect (aref mat i j)))))
         (setf result (polynom+ result summand))))
     (a:iota n))
    result))

;;; Graphs

(defun pn? (n x y)
  (declare (ignorable n))
  (or (= x (1+ y))
      (= y (1+ x))))

(defun cn? (n x y)
  (or (= x (mod (1+ y) n))
      (= y (mod (1+ x) n))))

(defun kn? (n x y)
  (declare (ignorable n))
  (not (= x y)))

(defun mat-pred (n pred)
  (make-array (list n n) :initial-contents (loop for x below n
                                                 collect (loop for y below n
                                                               collect (if (funcall pred n x y) 1 0)))))

(defun pn-mat (n)
  (mat-pred n #'pn?))

(defun cn-mat (n)
  (mat-pred n #'cn?))

(defun kn-mat (n)
  (mat-pred n #'kn?))

(defun mat-polynom (mat)
  (pmat-det (mat->cmat mat)))
