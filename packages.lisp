(defpackage :esa
  (:use :clim-lisp :clim)
  (:export #:minibuffer-pane #:display-message
           #:with-minibuffer-stream
           #:esa-pane-mixin #:previous-command
           #:info-pane #:master-pane
           #:esa-frame-mixin #:windows #:recordingp #:executingp
           #:*numeric-argument-p* #:*current-gesture*
           #:esa-top-level #:simple-command-loop
           #:global-esa-table #:keyboard-macro-table
           #:help-table
	   #:help-stream
           #:set-key
           #:find-applicable-command-table
           #:esa-command-parser
           #:esa-partial-command-parser))

(defpackage :esa-buffer
  (:use :clim-lisp :clim :esa)
  (:export #:frame-make-buffer-from-stream #:make-buffer-from-stream
           #:frame-save-buffer-to-stream #:save-buffer-to-stream
           #:filepath #:name #:needs-saving #:file-write-time #:file-saved-p
           #:esa-buffer-mixin
           #:frame-make-new-buffer #:make-new-buffer
           #:read-only-p))

(defpackage :esa-io
  (:use :clim-lisp :clim :esa :esa-buffer)
  (:export #:buffers #:frame-current-buffer #:current-buffer
           #:frame-find-file #:find-file
           #:frame-find-file-read-only #:find-file-read-only
           #:frame-set-visited-filename #:set-visited-filename
           #:frame-save-buffer #:save-buffer
           #:frame-write-buffer #:write-buffer
           #:esa-io-table))

#-mcclim
(defpackage :clim-extensions
  (:use :clim-lisp :clim)
  (:export
   #:+blue-violet+
   #:+dark-blue+
   #:+dark-green+
   #:+dark-violet+
   #:+gray50+
   #:+gray85+
   #:+maroon+
   #:+purple+))