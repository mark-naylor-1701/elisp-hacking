;;; misc-utils.el --- General functions that currently don't fit anywhere else

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.3"))
;; Keywords: foo bar baz
;; URL: http://example.com/jrhacker/superfrobnicate
;; date:  2021-Mar-02
;; ------------------------------------------------------------------------------

;;(require 'cl-lib)
;;(require 'dash)

(defun dired-get-explicit-marked-files (&optional localp arg filter)
  "Only gets the files that have been marked.
`dired-get-marked-files' will return the file sitting under the
cursor, even if there no mark character tagging it in the dired
buffer. Passes the parameters other than
DISTINGUISHED-ONE-MARKED; that one will always be true."
  (let* ((marked-files (dired-get-marked-files localp arg filter t))
         (marked-length (length marked-files)))
    (cond
      ((> marked-length 2) marked-files)
      ((= marked-length 2) (if (eq t (first marked-files))
                               (rest marked-files)
                             marked-files)))))

;; buffer and directory infomaton ----------------------------------------------

(cl-defun buffers-with-files (&optional (buffers (buffer-list)))
  "Given a list of buffers, returns just the ones that have a
file associated with them. If `buffers' is nil, use the current
live buffers."
  (when (listp buffers)
    (->> buffers
         (-filter #'bufferp)
         (-filter #'buffer-file-name))))


(cl-defun buffer-names-containing (fragment &optional (buffers (buffer-list)))
  "Returns a list of buffers whose names contain a string
fragment. If `buffers' is nil, use the current live buffers."
  (let* ((has-fragment? (-partial #'s-contains? fragment))
         (name-contains? (lambda (buffer) (funcall has-fragment? (buffer-name buffer)))))
    (->> buffers
         (-filter #'bufferp)
         (-filter name-contains?))))

(defun buffer-or-name? (buffer-or-name)
  "Is `buffer-or-name' either a buffer or a string? If
`buffer-or-name' is a string and there is no buffer with that
name, return nil. If `buffer-or-name' is a buffer or is the name
of a buffer, return t."
  (or (bufferp buffer-or-name)
      (when (stringp buffer-or-name)
        (-> buffer-or-name get-buffer null not))))

(defun buffer-directory (buffer)
  "If the `buffer' is a buffer, return its directory as a string, nil otherwise."
  (when (bufferp  buffer)
    (with-current-buffer buffer default-directory)))

(cl-defun rename-specified-buffer (&optional buffer-or-name new-name)
  "Check that `buffer-or-name' identifies a live buffer and that
`new-name' is a string that does *not* identify a live buffer.
Either returns the renamed buffer or nil."
  (interactive "bSelect buffer to rename: \nsNew name for the buffer: ")
  (when (and (buffer-or-name? buffer-or-name)
             (stringp new-name)
             (> (length new-name) 0)
             (not (buffer-or-name? new-name)))
    (if-let* ((target (get-buffer buffer-or-name))
              (old-buffer (current-buffer))
              ((not (eq target old-buffer))))
        ;; The two buffers are different; switch contexts and rename.
        (progn
          (switch-to-buffer target)
          (rename-buffer new-name)
          (switch-to-buffer old-buffer)
          target)
      ;; Renaming the current buffer
      (get-buffer (rename-buffer new-name)))))

(cl-defun swap-buffer-names (&optional buffer-or-name-1 buffer-or-name-2)
  "Given two live buffers or their names, give buffer 1 the name of buffer 2 and buffer 2 the name of buffer 1. Returns Returns buffer 1 upon success, nil otherwise."
  (interactive "bSelecty buffer #1: \nbSelect buffer #2: ")
  (when-let* ((b1 (and (buffer-or-name? buffer-or-name-1) (get-buffer buffer-or-name-1)))
              (b2 (and (buffer-or-name? buffer-or-name-2) (get-buffer buffer-or-name-2)))
              (b1-name (buffer-name b1))
              (b2-name (buffer-name b2)))
    (rename-specified-buffer b1 (generate-new-buffer-name "temp-buffer"))
    (rename-specified-buffer b2 b1-name)
    (rename-specified-buffer b1 b2-name)))

;; -----------------------------------------------------------------------------

(provide 'misc-utils)

;; BSD 3-Clause License

;; Copyright © 2021, Mark W. Naylor
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; misc-utils.el ends here
