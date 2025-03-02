;;; frame-tab-extension.el --- Provides equivalent kill-buffer-and-window functions for frames and tabs. -*- lexical-binding: t; read-symbol-shorthands: (("fte-" . "frame-tab-")) -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "29.0") (dash "2.19.1")
;; Keywords: frame tab buffer kill
;; URL: https://github.com/mark-naylor-1701/elisp-hacking.git/frame-tab-extension.el


;;; Commentary:
;;  After discovering C-x 4 0, `kill-buffer-and-window', I thought it would be
;;  convenient to have something similar for frames and tabs. Use case not quite
;;  the same, so prefixes to commands will address the different situations.
;;  Suggested bindings:
;;      (define-key global-map (kbd "C-x 5 9") #'kill-buffer-and-frame)
;;      (define-key global-map (kbd "C-x t 9") #'kill-buffer-and-tab)


;;; Code:

;; Requires
(require 'dash)


;; Forward declarations
(defvar fte--fn-label-alist)


;;  Public functions

(defun fte-kill-buffer-and-frame (p)
  "Kill the buffer and then the frame. See
`frame-tab--close-buffer-and-container' for explantion of how prefixes affect
  execution of the command."
  (interactive "p")
  (fte--close-buffer-and-container p #'fte--close-buffer-and-frame))

(defun fte-kill-buffer-and-tab (p)
  "Kill the buffer and then the tab. See
`frame-tab--close-buffer-and-container' for explantion of how prefixes affect
  execution of the command."
  (interactive "p")
  (fte--close-buffer-and-container p #'fte--close-buffer-and-tab))


;; Private functions

(defun fte--close-buffer-and-container (p close-fn)
  "Generic handler for either frames or tabs. `close-fn' handles
closing the container that holds the buffer. Currently, this
should be either `frame-tab--close-buffer-and-frame'or
`frame-tab--close-buffer-and-tab'. `p' is the prefix given to the
command.

1: The default (no explicit prefix). Posts a message if all the
windows in the container do not point to the same buffer.
`kill-buffer' is subject to normal verification rules.

C-u: Asks the user if the active buffer should be killed and the container
closed. If yes, `kill-buffer' is subject to normal verification rules.

C-u C-u: Force save and kill of the current buffer; close the container.

C-u C-u C-u: Force kill of the current buffer without saving; close the
container."
  (let ((buffers (mapcar #'window-buffer (window-list nil 'never)))
        (container (fte--closer-label close-fn)))
    (cond
     ((= p 1)
      (if (fte--all-eq-p buffers)
          (funcall close-fn)
        (message "Windows do not display the same active buffer #<buffer %s>."
                 (cl-first buffers))))

     ((= p 4)
      (when (yes-or-no-p
             (format "Kill #<buffer %s> and close %s? "
                     (cl-first buffers) container))
        (funcall close-fn)))

     ((= p 16)
      (progn
        (save-buffer)
        (funcall close-fn)))

     ((= p 64)
      (progn
        (set-buffer-modified-p nil)
        (funcall close-fn))))))

(defun fte--all-eq-p (seq)
  "Are all the remaing sequence elements `eq' the first?"
  (-every-p (lambda (x) (eq x (cl-first seq))) (cl-rest seq)))

(defun fte--close-buffer-and-frame ()
  "If the current buffer can be killed, do so, then kill the frame."
  (when (kill-buffer)
    (delete-frame)))

(defun fte--close-buffer-and-tab ()
  "If the current buffer can be killed, do so, then kill the tab."
  (when (kill-buffer)
    (tab-bar-close-tab)))

(defun fte--closer-label (close-fn)
  "Given a close function, return the associated label. With no
matches, return \"container\"."
  (or (alist-get close-fn fte--fn-label-alist)
      "container"))


;;; Private values
(defvar fte--fn-label-alist
  (list
   (cons #'frame-tab--close-buffer-and-frame "frame")
   (cons #'frame-tab--close-buffer-and-tab   "tab"))
  "Links a function to an associated label fragment.")


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

(provide 'frame-tab-extension)

;;; frame-tab-extension.el ends here
