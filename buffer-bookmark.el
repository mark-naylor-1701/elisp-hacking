;;; buffer-bookmark.el --- Provide single instance bookmarks tied to buffers.

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (map "2.1"))

;; file:  buffer-bookmark.el
;; date:  2021-Feb-02

(require 'map)
(require 'subr-x)
(require 'better-paging)

(defvar buffer-bookmarks (make-hash-table) "Data structure for tracking bookmarks.")

(defun bbm-set (key value)
  (when (and (bufferp key)
             (integerp value))
    (map-put! buffer-bookmarks key value)))

(defun bbm-get (key)
  (when (bufferp key)
    (map-elt buffer-bookmarks key)))

(defun buffer-bookmark-set ()
  "Bookmarks the curent location of the current buffer."
  (interactive)
  (bbm-set (current-buffer) (point)))

(defun buffer-bookmark-jump ()
  "If there is a mark set for the current buffer, jump to it."
  (interactive)
  (when-let ((new-point (bbm-get (current-buffer))))
    (goto-char new-point)))

(defun optional-buffer-bookmark-set ()
  "Only set a buffer bookmark if one does not aleady exist."
  (unless (bbm-get (current-buffer))
    (bbm-set buffer-bookmarks (current-buffer))))

(defun buffer-bookmark-remove ()
  "A clean-up function. When a buffer is killed, its bookmark entry
is no longer needed. Meant to be used with kill-buffer-hook"
  (when (boundp 'buffer-bookmarks)
    (map-delete buffer-bookmarks (current-buffer))))

(defun page-up-with-bookmark (&optional page-fn)
  (interactive)
  (let ((page-up (or page-fn #'page-up)))
    (funcall page-up)
    (buffer-bookmark-set)))

(defun page-down-with-bookmark (&optional page-fn)
  (interactive)
  (let ((page-down (or page-fn #'page-down)))
    (funcall page-down)
    (buffer-bookmark-set)))

(defun erc-page-up-with-bookmark ()
  (interactive)
  (page-up-with-bookmark #'erc-page-up))

(defun erc-page-down-with-bookmark ()
  (interactive)
  (page-down-with-bookmark #'erc-page-down))



(provide 'buffer-bookmark)
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


;;; buffer-bookmark.el ends here
