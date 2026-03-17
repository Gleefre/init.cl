(defpackage #:sky-user
  (:use #:cl))

(in-package #:sky-user)

;;; Initialize random-state
(setf *random-state* (make-random-state T))

;;; Load a machine-specific init file
(load (merge-pathnames ".local-init.lisp" (user-homedir-pathname)))

;; Not used when set in the local init file
(defvar *quicklisp-path* "quicklisp/setup.lisp")
(defvar *local-projects* ())
#+abcl (defvar *abcl-jars* nil)

;;; Load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames *quicklisp-path* (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(dolist (path *local-projects*)
  (push (merge-pathnames path (user-homedir-pathname))
        ql:*local-project-directories*))

;; always load alexandria
(ql:quickload :alexandria :silent t)

;; quicklisp over https via drakma
;; adapted from https://semelz.de/posts/quicklisp-with-https.html

#+(or sbcl ccl)  ;; doesn't work on cmucl, pulls way too many dependencies in general
(ql:quickload :drakma :silent t)

#+(or sbcl ccl)
(defun ql-https-fetch/drakma (url file &key (follow-redirects t) quietly
                                            (if-exists :rename-and-delete)
                                            (maximum-redirects ql-http:*maximum-redirects*))
  "Custom scheme-function for https protocol (using drakma)."
  (setf url (ql-http::merge-urls url ql-http:*default-url-defaults*))
  (setf file (merge-pathnames file))
  (when (and (equal (ql-http:scheme url) "http")
             (null (ql-http:port url)))
    ;; redirect http urls to https, unless a port is specified
    (setf (ql-http:scheme url) "https"))
  (multiple-value-bind (response code)
      (handler-bind ((drakma:drakma-error
		               (lambda (c) ; This is as fine-grained as Drakma gets, sadly.
			             (when (search "but redirection limit has been exceeded." (princ-to-string c))
			               (error 'ql-http:too-many-redirects
                                  :url url :redirect-count maximum-redirects)))))
	    (drakma:http-request (ql-http::urlstring url)
			                 :force-binary t
			                 :redirect (if follow-redirects maximum-redirects nil)
			                 :proxy (if ql-http:*proxy-url*
					                    (let ((p (ql-http:url ql-http:*proxy-url*)))
					                      (list (ql-http:hostname p)
						                        (or (ql-http:port p) 80)))
					                    nil)
			                 :user-agent (concatenate 'string
                                                      (ql-http::user-agent-string)
                                                      " (drakma)")))
    (when (/= code 200)
      (error 'ql-http:unexpected-http-status :url url :status-code code))
    (unless quietly
      (format *trace-output* "~&; Fetched (via HTTPS) ~A~%" url))
    (alexandria:write-byte-vector-into-file response file :if-exists if-exists)
    ;; The first return value is supposed to be the response header,
    ;; but since is unused, we can leave it NIL here.
    (values nil (and file (probe-file file)))))

#+(or sbcl ccl)
(setf (alexandria:assoc-value ql-http:*fetch-scheme-functions* "https" :test 'equal)
      #'ql-https-fetch/drakma
      (alexandria:assoc-value ql-http:*fetch-scheme-functions* "http" :test 'equal)
      #'ql-https-fetch/drakma)

;; Load asdf-contrib for ABCL, setup path to .jar files
#+abcl (require :abcl-contrib)
#+abcl (require :abcl-asdf)
#+abcl (when *abcl-jars* (abcl-asdf:add-directory-jars-to-class-path *abcl-jars* NIL))

;; disable Ultralisp by default
(let ((ultralisp (ql-dist:dist "ultralisp")))
  (when ultralisp
    (ql-dist:disable ultralisp)))

;; Don't load lots of libraries on slow implementations
#+(or ecl acl clisp clasp abcl cmucl mkcl) (pushnew :slow *features*)

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
  (ql:quickload (list #-mkcl :serapeum  ; doesn't work on MKCL
                      :stopclock :named-readtables
                      :trivial-indent
                      :trivial-gray-streams
                      :cffi
                      :com.inuoe.jzon)
                :silent t)
  (dolist (*package* (list *package* (find-package '#:cl-user)))
    #-mkcl
    (nick #:s    #:serapeum)
    (nick #:sc   #:stopclock
          #:a    #:alexandria
          #:nr   #:named-readtables
          #:ti   #:trivial-indent
          #:tgs  #:trivial-gray-streams
          #:jzon #:com.inuoe.jzon)))

;;; Make JZON understand plists & alists (like in old jzon versions)
#-slow
(progn
  (defparameter *jzon-detect-alists* T)
  (defparameter *jzon-detect-plists* NIL)

  (defun jzon-alistp (value)
    (loop for x in value
          always (and (consp x)
                      (or (characterp (car x))
                          (symbolp (car x))
                          (stringp (car x))))))

  (defun jzon-plistp (value)
    (loop for (k . rest) on value by #'cddr
          always (and (not (null rest))
                      (or (characterp k)
                          (symbolp k)
                          (stringp k))))))

#-slow
(defmethod jzon:write-value :around ((writer jzon:writer) (value cons))
  (cond ((and *jzon-detect-alists* (jzon-alistp value))
         (jzon:with-object writer
           (let ((replacer (slot-value writer 'jzon::%replacer)))
             (if replacer
                 (loop for (key . x) in value do
                   (multiple-value-call
                       (lambda (write-p &optional (new-value nil value-changed-p))
                         (when write-p
                           (jzon:write-key writer key)
                           (jzon:write-value writer (if value-changed-p new-value x))))
                     (funcall replacer key x)))
                 (loop for (key . x) in value do
                   (jzon:write-key writer key)
                   (jzon:write-value writer x))))))
        ((and *jzon-detect-plists* (jzon-plistp value))
         (jzon:with-object writer
           (let ((replacer (slot-value writer 'jzon::%replacer)))
             (if replacer
                 (loop for (key x) on value by #'cddr do
                   (multiple-value-call
                       (lambda (write-p &optional (new-value nil value-changed-p))
                         (when write-p
                           (jzon:write-key writer key)
                           (jzon:write-value writer (if value-changed-p new-value x))))
                     (funcall replacer key x)))
                 (loop for (key x) on value by #'cddr do
                   (jzon:write-key writer key)
                   (jzon:write-value writer x))))))
        (t (call-next-method))))

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
  (link mvb multiple-value-bind)
  (link quit :quit)
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
  (defun :ql (n &rest args) (apply #'ql:quickload n args))
  (defmacro :mac (form &environment env)
    `(let ((*print-case* :downcase))
       (pprint ',(let ((*gensym-counter* 0))
                   (macroexpand form env)))))
  #+macroexpand-all
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

;;; harmony -- graceful quit
(defun maybe-stop-harmony ()
  (let ((server (find-package "ORG.SHIRAKUMO.FRAF.HARMONY")))
    (when (and server
               (setf server (find-symbol "*SERVER*" server))
               (setf server (symbol-value server))
               (funcall (find-symbol "STARTED-P" "ORG.SHIRAKUMO.FRAF.HARMONY") server))
      (funcall (find-symbol "STOP" "ORG.SHIRAKUMO.FRAF.HARMONY") server))))

#+sbcl (pushnew 'maybe-stop-harmony sb-ext:*exit-hooks*)
