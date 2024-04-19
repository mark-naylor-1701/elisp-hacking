;;; better-paging.el ---

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (cl-lib "1.0") (info) (locate))
;; Keywords: navigation
;; file:  better-paging.el
;; date:  2021-Feb-10

(require 'cl-lib)
(require 'info)
(require 'locate)

(defun page-up (&optional move-point-fn)
  (interactive)
  (when (> (cl-first (page--count-lines-page)) (line-number-bottom-window))
    (let ((recenter-positions '(bottom)))
      (move-to-window-line-top-bottom))
    (funcall (or move-point-fn #'backward-paragraph))
    (let ((recenter-positions '(top)))
      (recenter-top-bottom))
    (point)))

(defun page-down (&optional move-point-fn)
  (interactive)
  (when (> (line-number-top-window) (bp--top-line))
    (let ((recenter-positions '(top)))
      (move-to-window-line-top-bottom))
    (funcall (or move-point-fn #'forward-paragraph))
    (let ((recenter-positions '(bottom)))
      (recenter-top-bottom))
    (point)))

(defun info-page-up ()
  "Like `page-up', except that it will jump to the next info node
when at the bottom of the current page."
  (interactive)
  (if (not (page-up))
      (Info-scroll-up)))

(defun info-page-down ()
  "Like `page-down', except that it will jump to the previous info node
when at the top of the current page."
  (interactive)
  (if (not (page-down))
      (Info-scroll-down)))

(defun prior-erc-tag ()
  (search-backward-regexp "^<" nil t))

(defun next-erc-tag ()
  (search-forward-regexp "^<" nil t))

(defun erc-page-down ()
  (interactive)
  (page-down #'next-erc-tag))

(defun erc-page-up ()
  (interactive)
  (page-up #'prior-erc-tag))

(defun line-number-window-relative (number-or-symbol)
  (save-excursion
    (let* ((recenter-positions (list number-or-symbol)))
      (move-to-window-line-top-bottom)
      (locate-current-line-number))))

(defun line-number-bottom-window ()
  (line-number-window-relative 'bottom))

(defun line-number-top-window ()
  (line-number-window-relative 'top))

(defun displayed-lines ()
  (- (line-number-bottom-window) (line-number-top-window) -1))

;; Hack because M-r, `move-to-window-line-top-bottom', jumps to the /second/
;; line, not the first.
(defun bp--top-line ()
  (cond
   ((string= major-mode "Info-mode") 2)
   (t 1)))

(provide 'better-paging)
;; ------------------------------------------------------------------------------
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


;;; better-paging.el ends here
