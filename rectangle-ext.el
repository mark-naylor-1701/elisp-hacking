;; author: Mark W. Naylor
;; file:  rectangle-ext.el
;; date:  2024-May-27

(require 'org-macs)

(defun rectext-sort-rectangle (prefix)
  "Sort the text of a rectangle.

Has the effect of sorting in place."
  (interactive "p")
  (when-let (((region-active-p))
             (anchor (region-beginning)))
    (kill-rectangle anchor (region-end))
    (with-temp-buffer
      (yank-rectangle)
      (let ((start (point-min))
            (stop  (point-max)))
        (sort-lines (= prefix 4) start stop)
        (kill-rectangle start stop)))
    (goto-char anchor)
    (yank-rectangle)))

(defun rectext-set-anchor ()
  "Set the target location where the rectangle will move."
  (interactive)
  (setq-local rectext--anchor (point-marker)))

(defun rectext-clear-anchor ()
  "Reset the anchor to no location."
  (interactive)
  (setq-local rectext--anchor nil))

(defun rect-text-clear-rect (origin width height)
  "Do stuff"
  nil)

(defun rectext-move-rectangle (p)
  "Kill the marked rectangle, yank it at the anchor."
  (interactive "P")
  ;; Change when into when-let. Assign each s-exp of the and form to _. Add
  ;; local values to capture the active region and the rectangle dimensions. One
  ;; for the active region and one for the dimensions of the rectangle.
  (when-let* (((and (boundp 'rectext--anchor) rectext--anchor))
              ((region-active-p))
              (start  (region-beginning))
              (end    (region-end))
              (dims   (rectangle-dimensions start end))
              (width  (car dims))
              (height (cdr dims)))
    (kill-rectangle start end)
    (goto-char rectext--anchor)
    ;; Here is the logic for clearing out the target rectangle, based upon the
    ;; prefix P.
    (unless (= p 4)
      rectext-clear-anchor start width height)
    (yank-rectangle)
    (rectext-clear-anchor)))

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
