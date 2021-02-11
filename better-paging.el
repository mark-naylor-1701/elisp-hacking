;;; better-paging.el ---

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0"))
;; Keywords: navigation
;; file:  better-paging.el
;; date:  2021-Feb-10


(defun page-scroll (jump-line anchor-fn compare-fn recenter-point scroll-fn)
  (let ((start-point (point)))
    (move-to-window-line jump-line)
    (funcall anchor-fn)
    (if (funcall compare-fn start-point (point))
        (recenter recenter-point)
      (progn
        (goto-char start-point)
        (funcall scroll-fn)))))

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
