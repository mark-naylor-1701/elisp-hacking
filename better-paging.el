;;; better-paging.el --- Enhancements to scroll-up-command and scroll-down-command. -*- lexical-binding: t -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (cl-lib "1.0") (info) (locate))
;; Keywords: navigation
;; file:  better-paging.el
;; date:  2021-Feb-10


;;; Commentary:

;;  The scroll up/down commands move the point and recenter text in the window
;;  based upon the first/last lines in the window. Often, it makes more sense to
;;  use somthing else as the anchor point for moving the page to the top/bottom
;;  of the window.

;;; Code:

;; Requires

(require 'cl-lib)
(require 'info)
(require 'locate)


;; Values

(defvar-local bp--top '(top))
(defvar-local bp--bottom '(bottom))


;; Functions

(defun page-up (&optional move-point-fn)
  "Move the page up.

This looks like the move window down in GUI editors. The default place to move
the point is `backward-paragraph'. MOVE-POINT-FN overrides the point movement."
  (interactive)
  (when (> (count-lines (point-min) (point-max)) (line-number-bottom-window))
    (let ((recenter-positions bp--bottom))
      (move-to-window-line-top-bottom))
    (funcall (or move-point-fn #'backward-paragraph))
    (let ((recenter-positions bp--top))
      (recenter-top-bottom))
    (point)))

(defun page-down (&optional move-point-fn)
  "Move the page up.

This looks like the move window up in GUI editors. The default place to move the
point is `forward-paragraph'. MOVE-POINT-FN overrides the point movement."
  (interactive)
  (when (> (line-number-top-window) (bp--top-line))
    (let ((recenter-positions bp--top))
      (move-to-window-line-top-bottom))
    (funcall (or move-point-fn #'forward-paragraph))
    (let ((recenter-positions bp--bottom))
      (recenter-top-bottom))
    (point)))

(defun info-page-up ()
  "This is similar to `page-up'.

The difference is that it will jump to the next info node when at
the bottom of the current page."
  (interactive)
  (unless (page-up)
    (Info-scroll-up)))

(defun info-page-down ()
  "This is similar to  `page-down'.

The difference is that it will jump to the previous info node
when at the top of the current page."
  (interactive)
  (unless (page-down)
    (Info-scroll-down)))

(defun prior-erc-tag ()
  "Move the point to the previous instance of a nick indicator in
the buffer."
  (search-backward-regexp "^<" nil t))

(defun next-erc-tag ()
  "Move the point to the next instance of a nick indicator or the
line for erc input."
  (search-forward-regexp "^\\(<\\|erc>\\)" nil t))

(defun erc-page-down ()
  "Smarter page up function, for use in an `erc-mode' buffer."
  (interactive)
  (let (found)
    (save-restriction
      ;; -----------------------------------------------------------------------
      ;; The lines in this block need to be left intact. The code following
      ;; depends upon the side effects. That includes the calling sequence of
      ;; the actual parameters to `narrow-to-region'.
      ;; -----------------------------------------------------------------------
      (let ((recenter-positions '(-2)))
        (move-to-window-line-top-bottom))
      (move-end-of-line nil)
      (narrow-to-region (point) (goto-window-top))
      ;; -----------------------------------------------------------------------
      (setq found (next-erc-tag)))
    (if found
        (let ((recenter-positions bp--bottom))
          (recenter-top-bottom))
      (page-down #'prior-erc-tag))))

(defun erc-page-up ()
  "Smarter page down function, for use in an `erc-mode' buffer."
  (interactive)
  (let (found)
    (save-restriction
     (let ((recenter-positions '(1)))
       (move-to-window-line-top-bottom))
     (narrow-to-region (point) (goto-window-bottom))
     (setq found (prior-erc-tag)))
    (if found
        (let ((recenter-positions bp--top))
          (recenter-top-bottom))
      (page-up #'next-erc-tag))))

;TODO: Move following functions into a new library package.

;; Tag for new library package
(defun line-number-window-relative (number-or-symbol)
  "Return the point to NUMBER-OR-SYMBOL.

NUMBER-OR-SYMBOL can be an integer or one of the symbols used by
`recenter-positions'. Restores the point before exiting the
function."
  (save-excursion
    (let* ((recenter-positions (list number-or-symbol)))
      (move-to-window-line-top-bottom)
      (locate-current-line-number))))

;; Tag for new library package
(defun line-number-bottom-window ()
  "Return the point of the last line, relative to the current window."
  (line-number-window-relative 'bottom))

;; Tag for new library package
(defun line-number-top-window ()
  "Return the point of the first line, relative to the current
window."
  (line-number-window-relative 'top))

;; Tag for new library package
(defun displayed-lines ()
  "Calculate the number of full lines diplayed in the current
window."
  (- (line-number-bottom-window) (line-number-top-window) -1))

;; Tag for new library package
(defun goto-window-top ()
  "Move to the first char in the window and return the new point."
  (let ((recenter-positions bp--top))
    (move-to-window-line-top-bottom))
  (point))

;; Tag for new library package
(defun goto-window-bottom ()
  "Move to the last char in the window and return the new point."
  (let ((recenter-positions bp--bottom))
    (move-to-window-line-top-bottom))
  (move-end-of-line nil)
  (point))

;; Tag for new library package
(defun narrow-to-window ()
  "Narrow to the fully displayed lines in the current window."
  (narrow-to-region (goto-window-top) (goto-window-bottom)))

(defun bp--window-region ()
  "Return the region of the displayed lines in the current window."
  (save-excursion
    (cons
     (goto-window-top)
     (goto-window-bottom))))

(defun bp--top-line ()
  "This is a hack for `Info-mode'.

In this mode, `move-to-window-line-top-bottom' jumps to line
number two, not the actual top line in the window."
  (cond
   ((string= major-mode "Info-mode") 2)
   (t 0)))

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
