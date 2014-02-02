(in-package :cl-user)
(defpackage cl21.core.stream
  (:use :cl)
  (:import-from :trivial-gray-streams
                :stream-write-string
                :stream-start-line-p
                :stream-terpri
                :stream-listen
                :stream-read-char-no-hang
                :trivial-gray-stream-mixin
                :stream-read-line
                :stream-line-column
                :stream-read-sequence
                :fundamental-character-output-stream
                :stream-write-char
                :fundamental-stream
                :fundamental-character-stream
                :stream-unread-char
                :fundamental-character-input-stream
                :stream-read-char
                :fundamental-binary-stream
                :fundamental-binary-output-stream
                :stream-finish-output
                :stream-advance-to-column
                :stream-fresh-line
                :fundamental-output-stream
                :stream-write-byte
                :stream-force-output
                :stream-read-byte
                :stream-peek-char
                :stream-clear-input
                :fundamental-input-stream
                :fundamental-binary-input-stream
                :stream-clear-output
                :stream-write-sequence
                :stream-file-position)
  (:import-from :alexandria
                :copy-stream
                :read-file-into-string
                :write-string-into-file
                :read-file-into-byte-vector
                :write-byte-vector-into-file)
  (:export :make-null-input-stream
           :make-null-output-stream
           :make-null-stream

           :stream
           :broadcast-stream
           :concatenated-stream
           :echo-stream
           :file-stream
           :string-stream
           :synonym-stream
           :two-way-stream
           :input-stream-p
           :output-stream-p
           :interactive-stream-p
           :open-stream-p
           :stream-element-type
           :streamp
           :read-byte
           :write-byte
           :peek-char
           :read-char
           :read-char-no-hang
           :terpri
           :fresh-line
           :unread-char
           :write-char
           :read-line
           :write-string
           :write-line
           :read-sequence
           :write-sequence
           :file-length
           :file-position
           :file-string-length
           :open
           :stream-external-format
           :with-open-file
           :close
           :with-open-stream
           :listen
           :clear-input
           :finish-output
           :force-output
           :clear-output
           :y-or-n-p
           :yes-or-no-p
           :make-synonym-stream
           :synonym-stream-symbol
           :broadcast-stream-streams
           :make-broadcast-stream
           :make-two-way-stream
           :two-way-stream-input-stream
           :two-way-stream-output-stream
           :echo-stream-input-stream
           :echo-stream-output-stream
           :make-echo-stream
           :concatenated-stream-streams
           :make-concatenated-stream
           :get-output-stream-string
           :make-string-input-stream
           :make-string-output-stream
           :with-input-from-string
           :with-output-to-string
           :*debug-io*
           :*error-output*
           :*query-io*
           :*standard-input*
           :*standard-output*
           :*trace-output*
           :*terminal-io*
           :stream-error
           :stream-error-stream
           :end-of-file

           :copy-stream
           :read-file-into-string
           :write-string-into-file
           :read-file-into-byte-vector
           :write-byte-vector-into-file

           ;; trivial-gray-streams
           :stream-write-string
           :stream-start-line-p
           :stream-terpri
           :stream-listen
           :stream-read-char-no-hang
           :trivial-gray-stream-mixin
           :stream-read-line
           :stream-line-column
           :stream-read-sequence
           :fundamental-character-output-stream
           :stream-write-char
           :fundamental-stream
           :fundamental-character-stream
           :stream-unread-char
           :fundamental-character-input-stream
           :stream-read-char
           :fundamental-binary-stream
           :fundamental-binary-output-stream
           :stream-finish-output
           :stream-advance-to-column
           :stream-fresh-line
           :fundamental-output-stream
           :stream-write-byte
           :stream-force-output
           :stream-read-byte
           :stream-peek-char
           :stream-clear-input
           :fundamental-input-stream
           :fundamental-binary-input-stream
           :stream-clear-output
           :stream-write-sequence
           :stream-file-position))
(in-package :cl21.core.stream)

(defun make-null-input-stream ()
  "Make a null input stream."
  (make-concatenated-stream))

(defun make-null-output-stream ()
  "Make a null output stream."
  (make-broadcast-stream))

(defun make-null-stream ()
  "Make a null input/output stream. Acts like the UNIX /dev/null."
  (make-two-way-stream
   (make-null-input-stream)
   (make-null-output-stream)))
