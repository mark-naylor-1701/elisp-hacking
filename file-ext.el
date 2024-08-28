;;; file-ext.el --- Provide tighter control over find file functions. -*- lexical-binding: t -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "29.0"))

;; file:  file-ext.el
;; date:  2024-May-12

;;; Commentary:
;;  Basic find-file can be unpleasant to use. This module provides dynamic
;;  "wrapping" of find-file and find-file-read-only. It is sensitive to whenther
;;  helm exists and is enabled or if ido is enable.

;;; Code

(defun fxt-helm-find-files-read-only ()
  "See `helm-find-files'; but opens the file/buffer in read only mode."
  (interactive)
  (when (and (functionp 'helm-find-files)
             (call-interactively #'helm-find-files))
    (let ((name (buffer-file-name (current-buffer))))
      (if (file-exists-p name)
          (progn
            (read-only-mode 1)
            (current-buffer))
        (kill-buffer)
        (message "File %s does not exist." name)))))

(defun fxt-select-find-file ()
  "Determine which find file function to use."
  (cond
   ((functionp 'helm-find-files) #'helm-find-files)

   ((functionp 'ido-find-file) #'ido-find-file)

   (t #'find-file)))

(defun fxt-select-find-file-read-only ()
  "Determine which find file read only function to use."
  (cond
   ((functionp 'fxt-helm-find-files-read-only) #'fxt-helm-find-files-read-only)

   ((functionp 'ido-find-file-read-only) #'ido-find-file-read-only)

   ;; Default base-line function.
   (t #'find-file-read-only)))

(defun fxt-find-file ()
  "Replacement for basic `find-file'."
  (interactive)
  (call-interactively (fxt-select-find-file)))

(defun fxt-find-file-read-only ()
  "Replacement for basic `find-file-read-only'."
  (interactive)
  (call-interactively (fxt-select-find-file-read-only)))

;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2024, Mark W. Naylor
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

;; https://opensource.org/licenses/BSD-3-Clause

(provide 'file-ext)

;;; file-ext.el ends here
