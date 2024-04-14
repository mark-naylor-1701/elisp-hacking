;;; better-paging.el ---

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0"))
;; Keywords: navigation
;; file:  better-paging.el
;; date:  2021-Feb-10

(defun page-scroll (new-window-line anchor-fn point-compare-fn
                    point-at-line-number scroll-fn)
  (let* ((start-point (point))
         (_ (move-to-window-line new-window-line))
         (_ (funcall anchor-fn))
        (new-point (point)))
    (if (funcall point-compare-fn start-point new-point)
        (progn
          (recenter point-at-line-number)
          new-point)
      (progn
        (goto-char start-point)
        (condition-case erc
         (funcall scroll-fn)
         (error (bp-scroll-erc-handler erc)))))))

(defun bp-scroll-erc-handler (erc)
  (let ((msg (car erc)))
    (if (not
         (or (string= msg "end-of-buffer")
             (string= msg "beginning-of-buffer")))
        (error msg))))

(defun page-up (&optional anchor-fn)
  (interactive)
  (page-scroll
   -1
   (or anchor-fn #'backward-paragraph)
   #'<
   0
   #'scroll-up-command))

(defun page-down (&optional anchor-fn)
  (interactive)
  (page-scroll
   1
   (or anchor-fn #'forward-paragraph)
   #'>
   -1
   #'scroll-down-command))

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
