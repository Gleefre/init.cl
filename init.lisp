(defpackage #:sky-user
  (:use #:cl))

(in-package #:sky-user)

;;; Initialize random-state
(setf *random-state* (make-random-state T))

;;; Load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; disable Ultralisp by default
(ql-dist:disable (ql-dist:dist "ultralisp"))

;; Don't load lots of libraries on slow implementations
#+(or ecl acl clisp clasp abcl cmucl) (pushnew :slow *features*)

;; Local projects
(push #P"/home/grolter/sky-emp/" ql:*local-project-directories*)
(push #P"/home/grolter/good-root/projects/lisp/" ql:*local-project-directories*)
(push #P"/home/grolter/good-root/learn-materials/lisp/" ql:*local-project-directories*)

;; Load asdf-contrib for ABCL, setup path to .jar files
#+abcl (require :abcl-contrib)
#+abcl (require :abcl-asdf)
#+abcl (abcl-asdf:add-directory-jars-to-class-path "~/Public/abcl/jars/" NIL)

;;; NIH: trivial-macroexpand-all
#+(or sbcl cmucl ccl allegro ecl abcl clisp lispworks cormanlisp mkcl)
(pushnew :macroexpand-all *features*)

#+mkcl (require :walker)
#+macroexpand-all
(defun macroexpand-all (form &optional env)
  (declare (ignorable env))
  (values (#+sbcl sb-walker:macroexpand-all
           #+cmucl walker:macroexpand-all
           #+ccl ccl:macroexpand-all
           #+allegro excl::walk-form
           #+ecl walker:macroexpand-all
           #+abcl ext:macroexpand-all
           #+clisp ext:expand-form
           #+lispworks walker:walk-form
           #+cormanlisp ccl:macroexpand-all
           #+mkcl walker:macroexpand-all
           form
           #-clisp env)
          t
          (or #-clisp t)))

;;; Allegro supports PLNs
#+allegro (pushnew :package-local-nicknames *features*)

;;; Creates a global nickname when PLNs are not supported
(defmacro nick (&rest nick-package &key &allow-other-keys)
  `(progn
     #+package-local-nicknames
     ,@(loop for (nick package) on nick-package by #'cddr
             collect `(#.(car (find-all-symbols "REMOVE-PACKAGE-LOCAL-NICKNAME")) ',nick)
             collect `(#.(car (find-all-symbols "ADD-PACKAGE-LOCAL-NICKNAME")) ',nick ',package))
     #-package-local-nicknames
     (warn "PLNs are not supported, global nicknames are used.")
     #-package-local-nicknames
     ,@(loop for (nick package) on nick-package by #'cddr
             collect `(let ((package (find-package ',nick)))
                        (unless (and package
                                     (or (eq (package-name package) ',nick)
                                         (eq (package-name package) ',package)))
                          (when package
                            (warn "Removing global nickname ~A for ~A" ',nick package)
                            (rename-package package
                                            (package-name package)
                                            (remove ',nick (copy-list (package-nicknames package)))))
                          (rename-package ',package
                                          ',package
                                          (list* ',nick (package-nicknames (find-package ',package)))))))))

;;; Load various libraries, add nicknames
#-slow
(progn
  (ql:quickload (list :alexandria #-mkcl :serapeum  ; doesn't work on MKCL
                      :stopclock :named-readtables
                      :trivial-indent
                      :cffi)
                :silent t)
  #-mkcl (nick #:s   #:serapeum)
  (nick #:sc  #:stopclock
        #:a   #:alexandria
        #:nr  #:named-readtables
        #:ti  #:trivial-indent))

;;; symbol-links

#+(and (or sbcl lispworks ccl cmucl) (not symbol-links))
(use-package (ql:quickload :symbol-links :silent t))

#+(and (or sbcl lispworks ccl cmucl) (not symbol-links))
(progn
  (enable-hack!)
  (pushnew :symbol-links *features*))

#+symbol-links
(progn
  (link λ lambda)
  #-slow
  (named-readtables:defreadtable :symbol-links
    (:merge :standard)
    (:dispatch-macro-char #\# #\@
                          (lambda (stream char arg)
                            (declare (ignore char arg))
                            (without-symbol-links
                             (read stream t nil t))))))

;;; loop-continue

#+(or sbcl cmucl ccl allegro clasp abcl ecl)
(ql:quickload :loop-continue/enable :silent t)

;;; Define shortcuts as functions named by keywords
;;; Doesn't work on lispworks

#-lispworks
(progn
  (defun :test (s) (asdf:test-system s))
  (defun :sl (n &optional (force t)) (asdf:load-system n :force force))
  (defun :ql (n) (ql:quickload n))
  (defmacro :mac (form &environment env)
    `(let ((*print-case* :downcase))
       (pprint ',(let ((*gensym-counter* 0))
                   (macroexpand form env)))))
  #-(or slow (not package-local-nicknames))
  (defmacro :maca (form &environment env)
    `(let ((*print-case* :downcase))
       (pprint ',(let ((*gensym-counter* 0))
                   (macroexpand-all form env)))))
  (defmacro :mac1 (form &environment env)
    `(let ((*print-case* :downcase))
       (pprint ',(let ((*gensym-counter* 0))
                   (macroexpand-1 form env)))))
  (defun :quit () (uiop:quit)))

;;; Load a small library
(load (merge-pathnames ".lib.lisp" (user-homedir-pathname)))
