;;; better-paging.el --- Enhancements to scroll-up-command and scroll-down-command. -*- lexical-binding: t -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (window-ext "0.9") (cl-lib "1.0") (info) (locate))
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

(require 'window-ext)


;; Functions
;; Public

(defun page-up (&optional move-point-fn)
  "Move the page up.

This looks like the move window down in GUI editors. The default place to move
the point is `backward-paragraph'. MOVE-POINT-FN overrides the point movement."
  (interactive)
  (when (> (count-lines (point-min) (point-max)) (window-ext-line-number-bottom-window))
    (let ((top-point (window-ext-goto-window-top)))
      (window-ext-move-to-window-line-bottom)
      (funcall (or move-point-fn #'backward-paragraph))
      (window-ext-recenter-top)
      ;; This section handles the case when a paragraph is larger than the window.
      (let ((point (point)))
        (if (< top-point point)
            ;; Paragraph smaller than window. Keep buffer state and return the
            ;; point.
            point
          ;; Paragraph larger than window. Go to original point and scroll up.
          (progn
            (goto-char top-point)
            (window-ext-recenter-top)
            (scroll-up-command)
            (point)))))))

(defun page-down (&optional move-point-fn)
  "Move the page up.

This looks like the move window up in GUI editors. The default place to move the
point is `forward-paragraph'. MOVE-POINT-FN overrides the point movement."
  (interactive)
  (when (> (window-ext-line-number-top-window) (bp--top-line))
    (let ((bottom-point (window-ext-goto-window-bottom)))
      (window-ext-move-to-window-line-top)
      (funcall (or move-point-fn #'forward-paragraph))
      (window-ext-recenter-bottom)
      ;; This section handles the case when a paragraph is larger than the window.
      (let ((point (point)))
        (if (< point bottom-point)
            ;; Paragraph smaller than window. Keep buffer state and return the
            ;; point.
            point
          ;; Paragraph larger than window. Go to original point and scroll down.
          (progn
            (goto-char bottom-point)
            (window-ext-recenter-bottom)
            (scroll-down-command)
            (point)))))))

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
      (move-to-window-line-top-bottom -2)
      (move-end-of-line nil)
      (narrow-to-region (point) (window-ext-goto-window-top))
      ;; -----------------------------------------------------------------------
      (setq found (next-erc-tag)))
    (if found
        (window-ext-recenter-bottom)
      (page-down #'prior-erc-tag))))

(defun erc-page-up ()
  "Smarter page down function, for use in an `erc-mode' buffer."
  (interactive)
  (let (found)
    (save-restriction
      (move-to-window-line-top-bottom 1)
      (narrow-to-region (point) (window-ext-goto-window-bottom))
      (setq found (prior-erc-tag)))
    (if found
        (window-ext-recenter-top)
      (page-up #'next-erc-tag))))

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
