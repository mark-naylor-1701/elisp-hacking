;; -*- lexical-binding: t; read-symbol-shorthands: (("fte-" . "frame-tab-")) -*-
;; author: Mark W. Naylor
;; file:  frame-tab-extension.el
;; date:  2024-Sep-29

(defun fte-kill-buffer-and-frame (p)
  (interactive "p")
  (let ((buffers (mapcar #'window-buffer (window-list nil 'never))))
    (cond
     ((= p 1)
      (if (fte--all-eq-p buffers)
          (fte--close-buffer-and-frame)
        (message "Windows do not display the same buffer.")))

     ((= p 4)
      (when (yes-or-no-p
             (format "Kill #<buffer %s> and close frame? "
                     (first buffers)))
        (fte--close-buffer-and-frame)))

     ((= p 16)
      (progn
        (save-buffer)
        (fte--close-buffer-and-frame)))

     ((= p 64)
      (progn
        (set-buffer-modified-p nil)
        (fte--close-buffer-and-frame))))))

(defun fte--all-eq-p (seq)
  "Are all the remaing sequence elements `eq' the first?"
  (-every-p (lambda (x) (eq x (first seq))) (rest seq)))

(defun fte--close-buffer-and-frame ()
  "If the current buffer can be killed, do so, then kill the frame."
  (when (kill-buffer)
    (delete-frame)))

(defun fte--close-buffer-and-tab ()
  "If the current tab can be killed, do so, then kill the frame."
  (when (kill-buffer)
    (tab-bar-close-tab)))

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
