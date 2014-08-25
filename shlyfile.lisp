(in-package :cl-user)

(ql:quickload '(:quickdist :split-sequence :cl-ppcre :cl-fad))

(defpackage cl21.update-dist
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:export :release-new-tags
           :release-tag)
  (:documentation "Provides Shelly tasks for releasing a git project with QUICKDIST.

Usage:
  $ shly release-new-tags     # Release all git tags
  $ shly release-tag 0.1.0    # Release a specific git tag

You can specify the output dist directory by a shell env variable QLDIST_OUTPUT_DIR."))
(in-package :cl21.update-dist)

(defparameter *shlyfile-pathname* (or *load-pathname* *compile-file-pathname*))

(defparameter *project-name*
  (car (last (pathname-directory *shlyfile-pathname*))))
(defparameter *projects-dir* (fad:pathname-parent-directory
                              (make-pathname
                               :directory (pathname-directory *shlyfile-pathname*))))
(defparameter *project-dir*
  (make-pathname
   :directory (append (pathname-directory *projects-dir*) (list *project-name*))))
(defparameter *qldist-base-url* "http://dists.cl21.org/")
(defparameter *qldist-output-dir* (or (asdf::truenamize (asdf::getenv "QLDIST_OUTPUT_DIR"))
                                      (merge-pathnames "dists/"
                                                       (fad:pathname-parent-directory *projects-dir*))))

(defun shell-command (program)
  (format *debug-io* "~&~A~%" program)
  (with-output-to-string (s)
    (sb-ext:run-program "/bin/sh" `("-c" ,program)
                        :output s
                        :wait t)))

(defun remote-tags ()
  (let ((res
          (shell-command (format nil "git --git-dir ~A.git ls-remote --tags origin"
                                 *project-dir*))))
    (loop for tag in (split-sequence #\Newline res :remove-empty-subseqs t)
          for match = (nth-value 1 (ppcre:scan-to-strings "^(.+)\\s+refs/tags/(.+?)(?:\\^{})?$" tag))
          while match
          collect (cons (aref match 1)
                        (aref match 0)))))

(defun tag-commit-hash (tagname)
  (let ((res
          (shell-command (format nil "git --git-dir ~A.git show-ref refs/tags/~A | awk '{ print $1 }'"
                                 *project-dir*
                                 tagname))))
    (if (zerop (length res))
        nil
        (subseq res 0 (1- (length res))))))

(defun released-versions ()
  (let ((versions-file (merge-pathnames (format nil "~A-versions.txt" *project-name*)
                                        *qldist-output-dir*)))
    (when (probe-file versions-file)
      (with-open-file (in versions-file :direction :input :if-does-not-exist nil)
        (loop for line = (read-line in nil nil)
              while line
              collect (subseq line 0 (position #\Space line)))))))

(defun new-tags ()
  (let ((remote (remote-tags))
        (released (released-versions)))
    (loop for (key . val) in remote
          unless (find key released :test #'string=)
            collect (cons key val))))

(defun release-tag (tag)
  "Releases the specified TAG."
  (let ((commit (tag-commit-hash tag)))
    (unless commit
      (error "Tag ~A does not exist." tag)
      (return-from release-tag))

    (format t "~&Releasing ~A...~%" tag)
    (shell-command (format nil "git --git-dir ~A.git checkout ~A"
                           *project-dir*
                           commit))
    (quickdist:quickdist :name *project-name*
                         :version tag
                         :base-url *qldist-base-url*
                         :projects-dir *projects-dir*
                         :dists-dir *qldist-output-dir*)
    (with-open-file (out (merge-pathnames (format nil "~A-versions.txt" *project-name*)
                                          *qldist-output-dir*)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~&~A ~A~A/~A/distinfo.txt~%"
              tag
              *qldist-base-url*
              *project-name*
              tag))
    (format t "~&Done.~%")))

(defun release-new-tags ()
  "Releases new tags which hasn't released yet."
  (let ((tags (new-tags)))
    (unless tags
      (format t "~&No new tags. Exiting.~%")
      (return-from release-new-tags))

    (format t "~&~D new tag~:*~[s~;~;s~].~%" (length tags))
    (shell-command (format nil "git --git-dir ~A.git pull"
                           *project-dir*))
    (loop for (tag) in tags
          do (release-tag tag)))
  T)

(do-external-symbols (symb :cl21.update-dist)
  (setf (symbol-function (intern (string symb) :cl-user))
        (symbol-function symb)))
