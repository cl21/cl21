(in-package :cl21-user)
(defpackage cl21.process
  (:use :cl21)
  (:export :process
           :processp
           :process-pid
           :process-command
           :process-status
           :process-exit-code
           :process-input
           :process-output
           :process-error
           :run-process
           :process-alive-p
           :process-wait
           :process-send-signal
           :process-kill
           :process-terminate
           :process-stop
           :process-continue)
  (:documentation "cl21.process allows you to run a new processes with implementation independent ways.
Currently, SBCL and Clozure CL on UNIX are supported now."))
(in-package :cl21.process)

(defconstant +sigkill+ 9)
(defconstant +sigterm+ 15)
(defconstant +sigstop+ 17)
(defconstant +sigcont+ 19)

(defparameter *shell-path* "/bin/sh")


;;
;; process

(defstruct (process (:copier nil)
                    (:predicate processp))
  pid
  command
  (%status nil :type keyword)
  (%exit-code nil :type (or null fixnum))
  input
  output
  error
  %raw-object)

(defmethod print-object ((object process) stream)
  (format stream "#<~A ~{~A~^ ~} (~A) ~A>"
          (class-name (class-of object))
          (process-command object)
          (process-pid object)
          (if (process-alive-p object)
              (process-%status object)
              (format nil "~A ~S"
                      (process-%status object)
                      (process-%exit-code object)))))

(defun process-status (process)
  (when (process-alive-p process)
    (process-wait process))
  (process-%status process))

(defun process-exit-code (process)
  (or (process-%exit-code process)
      (progn
        (when (process-alive-p process)
          (process-wait process))
        (process-%exit-code process))))


;;
;; Functions

(defun run-process (command &key input (output *standard-output*) (error *error-output*) (wait nil) shell)
  "Creates a new process and runs a shell command in that process.

Example:
  (run-process '(\"ls\" \"-l\") :wait t)"

  (let ((command (if shell
                     command
                     `(,*shell-path* "-c" ,(format nil "~{~A~^ ~}" command)))))
    #+sbcl
    (handler-case
        (let* ((sb-proc
                 (sb-ext:run-program (car command) (cdr command)
                                     :wait wait
                                     :input input
                                     :output output
                                     :error error
                                     :search t))
               (process (make-process
                         :pid (sb-ext:process-pid sb-proc)
                         :command command
                         :%status (sb-ext:process-status sb-proc)
                         :%exit-code (sb-ext:process-exit-code sb-proc)
                         :input input
                         :output output
                         :error error
                         :%raw-object sb-proc)))
          (setf (sb-ext:process-status-hook sb-proc)
                (lambda (sb-proc)
                  (setf (process-%status process)
                        (sb-ext:process-status sb-proc))
                  (setf (process-%exit-code process)
                        (sb-ext:process-exit-code sb-proc))))
          process)
      (simple-error (e)
        (cond
          ((null error))
          ((eq error t) (princ e))
          (T  (princ e error)))
        (make-process
         :command command
         :%status :exited
         :%exit-code (if (string= (second (simple-condition-format-arguments e))
                                  "No such file or directory")
                         127
                         nil)
         :input input
         :output output
         :error error)))
    #+ccl
    (let ((process
            (ccl:run-program (car command) (cdr command)
                             :wait wait
                             :input input
                             :output output
                             :error error)))
      (multiple-value-bind (status exit-code)
          (ccl:external-process-status process)
        (make-process
         :pid (ccl:external-process-id process)
         :command command
         :%status status
         :%exit-code exit-code
         :input input
         :output output
         :error error
         :%raw-object process)))))

(defun process-alive-p (process)
  "Returns T if the status of PROCESS is :RUNNING or :STOPPED."
  (not
   (null
    (find (process-%status process)
          '(:running :stopped)
          :test #'eq))))

(defun process-wait (process &optional check-for-stopped)
  "Waits for PROCESS to complete."
  (case (process-%status process)
    (:running)
    (:stopped (when check-for-stopped
                (return-from process-wait (eql (process-%exit-code process) 0))))
    (otherwise
     (return-from process-wait (eql (process-%exit-code process) 0))))

  #+sbcl
  (multiple-value-bind (pid status exit-code)
      (sb-impl::waitpid (process-pid process))
    (when pid
      (setf (process-%status process) status)
      (setf (process-%exit-code process) exit-code)))
  #+ccl
  (let ((raw (process-%raw-object process)))
    (ccl::external-process-wait raw)
    (setf (process-%status process) (ccl:external-process-status raw))
    (setf (process-%exit-code process) (ccl::external-process-%exit-code raw)))
  (eql (process-%exit-code process) 0))

(defun process-send-signal (process signal)
  "Sends a signal SIGNAL to PROCESS."
  #+sbcl
  (multiple-value-bind (okay errno)
      (sb-unix:unix-kill (process-pid process) signal)
    (cond
      ((not okay)
       (values nil errno))
      ((= signal +sigcont+)
       (setf (process-%status process) :running)
       (setf (process-%exit-code process) nil)
       t)
      (t t)))
  #+ccl
  (ccl:signal-external-process (process-%raw-object process) signal))

(defun process-kill (process)
  "Sends a signal SIGKILL to PROCESS."
  (process-send-signal process +sigkill+))

(defun process-terminate (process)
  "Sends a signal SIGTERM to PROCESS."
  (process-send-signal process +sigterm+))

(defun process-stop (process)
  "Sends a signal SIGSTOP to PROCESS."
  (process-send-signal process +sigstop+))

(defun process-continue (process)
  "Sends a signal SIGCONT to PROCESS."
  (process-send-signal process +sigcont+))
