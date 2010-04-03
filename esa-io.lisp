;;; -*- Mode: Lisp; Package: ESA-IO -*-

;;;  (c) copyright 2006 by
;;;           Robert Strandh (strandh@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :esa-io)

(defgeneric buffers (application-frame)
  (:documentation "Return a list of all the buffers of the application"))

(defgeneric frame-current-buffer (application-frame)
  (:documentation "Return the current buffer of APPLICATION-FRAME"))

(defun current-buffer (&optional (frame *application-frame*))
  "Return the current buffer of `frame'. This function merely
calls `frame-current-buffer' with `frame' as argument."
  (frame-current-buffer frame))

(defgeneric frame-find-file (application-frame file-path)
  (:documentation "If a buffer with the file-path already exists,
return it, else if a file with the right name exists, return a
fresh buffer created from the file, else return a new empty
buffer having the associated file name."))
(defgeneric frame-find-file-read-only (application-frame file-path))
(defgeneric frame-set-visited-file-name (application-frame filepath buffer))
(defgeneric frame-save-buffer (application-frame buffer))
(defgeneric frame-write-buffer (application-frame filepath buffer))

(defun find-file (file-path)
  (frame-find-file *application-frame* file-path))
(defun find-file-read-only (file-path)
  (frame-find-file-read-only *application-frame* file-path))
(defun set-visited-file-name (filepath buffer)
  (frame-set-visited-file-name *application-frame* filepath buffer))
(defun save-buffer (buffer)
  (frame-save-buffer *application-frame* buffer))
(defun write-buffer (filepath buffer)
  (frame-write-buffer *application-frame* filepath buffer))

(make-command-table 'esa-io-table :errorp nil)

(defun filename-completer (so-far mode)
  (flet ((remove-trail (s)
           (subseq s 0 (let ((pos (position #\/ s :from-end t)))
                         (if pos (1+ pos) 0)))))
    (let* ((directory-prefix
            (if (and (plusp (length so-far)) (eql (aref so-far 0) #\/))
                ""
                (namestring #+sbcl *default-pathname-defaults*
                            #+cmu (ext:default-directory)
                            #-(or sbcl cmu) *default-pathname-defaults*)))
           (full-so-far (concatenate 'string directory-prefix so-far))
           (pathnames
            (loop with length = (length full-so-far)
                  and wildcard = (concatenate 'string (remove-trail so-far) "*.*")
                  for path in
                  #+(or sbcl cmu lispworks) (directory wildcard)
                  #+openmcl (directory wildcard :directories t)
                  #+allegro (directory wildcard :directories-are-files nil)
                  #+cormanlisp (nconc (directory wildcard)
                                      (cl::directory-subdirs dirname))
                  #-(or sbcl cmu lispworks openmcl allegro cormanlisp)
                    (directory wildcard)
                  when (let ((mismatch (mismatch (namestring path) full-so-far)))
                         (or (null mismatch) (= mismatch length)))
                    collect path))
           (strings (mapcar #'namestring pathnames))
           (first-string (car strings))
           (length-common-prefix nil)
           (completed-string nil)
           (full-completed-string nil))
      (unless (null pathnames)
        (setf length-common-prefix
              (loop with length = (length first-string)
                    for string in (cdr strings)
                    do (setf length (min length (or (mismatch string first-string) length)))
                    finally (return length))))
      (unless (null pathnames)
        (setf completed-string
              (subseq first-string (length directory-prefix)
                      (if (null (cdr pathnames)) nil length-common-prefix)))
        (setf full-completed-string
              (concatenate 'string directory-prefix completed-string)))
      (case mode
        ((:complete-limited :complete-maximal)
         (cond ((null pathnames)
                (values so-far nil nil 0 nil))
               ((null (cdr pathnames))
                (values completed-string t (car pathnames) 1 nil))
               (t
                (values completed-string nil nil (length pathnames) nil))))
        (:complete
         (cond ((null pathnames)
                (values so-far t so-far 1 nil))
               ((null (cdr pathnames))
                (values completed-string t (car pathnames) 1 nil))
               ((find full-completed-string strings :test #'string-equal)
                (let ((pos (position full-completed-string strings :test #'string-equal)))
                  (values completed-string
                          t (elt pathnames pos) (length pathnames) nil)))
               (t
                (values completed-string nil nil (length pathnames) nil))))
        (:possibilities
         (values nil nil nil (length pathnames)
                 (loop with length = (length directory-prefix)
                       for name in pathnames
                       collect (list (subseq (namestring name) length nil)
                                     name))))))))

(define-presentation-method present (object (type pathname)
                                            stream (view textual-view) &key)
  (princ (namestring object) stream))

(define-presentation-method accept ((type pathname) stream (view textual-view)
                                    &key (default nil defaultp) (default-type type))
  (multiple-value-bind (pathname success string)
      (complete-input stream
                      #'filename-completer
                      :allow-any-input t)
    (cond (success
           (values pathname type))
          ((and (zerop (length string))
                defaultp)
           (values default default-type))
          (t (values string 'string)))))

;;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
        (type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
         (or (null type) (eql type :unspecific)))))

(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
                   "." (pathname-type pathname))))

(defmethod frame-find-file (application-frame filepath)
  (cond ((null filepath)
         (display-message "No file name given.")
         (beep))
        ((directory-pathname-p filepath)
         (display-message "~A is a directory name." filepath)
         (beep))
        (t
         (or (find filepath (buffers *application-frame*)
                   :key #'filepath :test #'equal)
             (let ((buffer (if (probe-file filepath)
                               (with-open-file (stream filepath :direction :input)
                                 (make-buffer-from-stream stream))
                               (make-new-buffer))))
               (setf (filepath buffer) filepath
                     (name buffer) (filepath-filename filepath)
                     (needs-saving buffer) nil)
               buffer)))))

(defun directory-of-current-buffer ()
  "Extract the directory part of the filepath to the file in the current buffer.
   If the current buffer does not have a filepath, the path to
   the user's home directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (filepath (current-buffer))
        (user-homedir-pathname)))))

(define-command (com-find-file :name t :command-table esa-io-table) 
    ((filepath 'pathname
               :prompt "Find File: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :default-type 'pathname
               :insert-default t))
  "Prompt for a filename then edit that file.
If a buffer is already visiting that file, switch to that
buffer. Does not create a file if the filename given does not
name an existing file."
  (find-file filepath))

(set-key `(com-find-file ,*unsupplied-argument-marker*)
         'esa-io-table '((#\x :control) (#\f :control)))

(defmethod frame-find-file-read-only (application-frame filepath)
  (cond ((null filepath)
         (display-message "No file name given.")
         (beep))
        ((directory-pathname-p filepath)
         (display-message "~A is a directory name." filepath)
         (beep))
        (t
         (or (find filepath (buffers *application-frame*)
                   :key #'filepath :test #'equal)
             (if (probe-file filepath)
                 (with-open-file (stream filepath :direction :input)
                   (let ((buffer (make-buffer-from-stream stream)))
                     (setf (filepath buffer) filepath
                           (name buffer) (filepath-filename filepath)
                           (read-only-p buffer) t
                           (needs-saving buffer) nil)))
                 (progn
                   (display-message "No such file: ~A" filepath)
                   (beep)
                   nil))))))

(define-command (com-find-file-read-only :name t :command-table esa-io-table)
    ((filepath 'pathname
               :prompt "Find File read-only: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :default-type 'pathname
               :insert-default t))
  "Prompt for a filename then open that file readonly.
If a buffer is already visiting that file, switch to that
buffer. If the filename given does not name an existing file,
signal an error."
  (find-file-read-only filepath))

(set-key `(com-find-file-read-only ,*unsupplied-argument-marker*)
         'esa-io-table '((#\x :control) (#\r :control)))

(define-command (com-read-only :name t :command-table esa-io-table)
    ()
  "Toggle the readonly status of the current buffer.
When a buffer is readonly, attempts to change the contents of the
buffer signal an error."
  (let ((buffer (current-buffer)))
    (setf (read-only-p buffer) (not (read-only-p buffer)))))

(set-key 'com-read-only 'esa-io-table '((#\x :control) (#\q :control)))

(defmethod frame-set-visited-file-name (application-frame filepath buffer)
  (setf (filepath buffer) filepath
        (name buffer) (filepath-filename filepath)
        (needs-saving buffer) t))

(define-command (com-set-visited-file-name :name t :command-table esa-io-table)
    ((filename 'pathname :prompt "New filename: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :insert-default t
               :default-type 'pathname
               :insert-default t))
    "Prompt for a new filename for the current buffer.
The next time the buffer is saved it will be saved to a file with
that filename."
  (set-visited-file-name filename (current-buffer)))

(defun extract-version-number (pathname)
  "Extracts the emacs-style version-number from a pathname."
  (let* ((type (pathname-type pathname))
	 (length (length type)))
    (when (and (> length 2) (char= (char type (1- length)) #\~))
      (let ((tilde (position #\~ type :from-end t :end (- length 2))))
	(when tilde
	  (parse-integer type :start (1+ tilde) :junk-allowed t))))))

(defun version-number (pathname)
  "Return the number of the highest versioned backup of PATHNAME
or 0 if there is no versioned backup. Looks for name.type~X~,
returns highest X."
  (let* ((wildpath (merge-pathnames (make-pathname :type :wild) pathname))
	 (possibilities (directory wildpath)))
    (loop for possibility in possibilities
	  for version = (extract-version-number possibility) 
	  if (numberp version)
	    maximize version into max
	  finally (return max))))

(defun check-file-times (buffer filepath question answer)
  "Return NIL if filepath newer than buffer and user doesn't want
to overwrite."
  (let ((f-w-d (file-write-date filepath))
	(f-w-t (file-write-time buffer)))
    (if (and f-w-d f-w-t (> f-w-d f-w-t))
	(if (accept 'boolean
		    :prompt (format nil "File has changed on disk. ~a anyway?"
				    question))
	    t
	    (progn (display-message "~a not ~a" filepath answer)
		   nil))
	t)))

(defmethod frame-save-buffer (application-frame buffer)
  (let ((filepath (or (filepath buffer)
                      (accept 'pathname :prompt "Save Buffer to File"))))
    (cond
      ((directory-pathname-p filepath)
       (display-message "~A is a directory." filepath)
       (beep))
      (t
       (unless (check-file-times buffer filepath "Overwrite" "written")
	 (return-from frame-save-buffer))
       (when (and (probe-file filepath) (not (file-saved-p buffer)))
         (let ((backup-name (pathname-name filepath))
               (backup-type (format nil "~A~~~D~~"
				    (pathname-type filepath)
				    (1+ (version-number filepath)))))
           (rename-file filepath (make-pathname :name backup-name
                                                :type backup-type))))
       (with-open-file (stream filepath :direction :output :if-exists :supersede)
         (save-buffer-to-stream buffer stream))
       (setf (filepath buffer) filepath
             (file-write-time buffer) (file-write-date filepath)
             (name buffer) (filepath-filename filepath))
       (display-message "Wrote: ~a" (filepath buffer))
       (setf (needs-saving buffer) nil)))))

(define-command (com-save-buffer :name t :command-table esa-io-table) ()
    "Write the contents of the buffer to a file.
If there is filename associated with the buffer, write to that
file, replacing its contents. If not, prompt for a filename."
  (let ((buffer (current-buffer)))
    (if (or (null (filepath buffer))
            (needs-saving buffer))
        (save-buffer buffer)
        (display-message "No changes need to be saved from ~a" (name buffer)))))

(set-key 'com-save-buffer 'esa-io-table '((#\x :control) (#\s :control)))

(defmethod frame-write-buffer (application-frame filepath buffer)
  (cond
    ((directory-pathname-p filepath)
     (display-message "~A is a directory name." filepath))
    (t
     (with-open-file (stream filepath :direction :output :if-exists :supersede)
       (save-buffer-to-stream buffer stream))
     (setf (filepath buffer) filepath
           (name buffer) (filepath-filename filepath)
           (needs-saving buffer) nil)
     (display-message "Wrote: ~a" (filepath buffer)))))

(define-command (com-write-buffer :name t :command-table esa-io-table) 
    ((filepath 'pathname :prompt "Write Buffer to File: " :prompt-mode :raw
               :default (directory-of-current-buffer) :insert-default t
               :default-type 'pathname))
    "Prompt for a filename and write the current buffer to it.
Changes the file visted by the buffer to the given file."
  (let ((buffer (current-buffer)))
    (write-buffer buffer filepath)))

(set-key `(com-write-buffer ,*unsupplied-argument-marker*)
         'esa-io-table '((#\x :control) (#\w :control)))

