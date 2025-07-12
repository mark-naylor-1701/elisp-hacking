;;; window-ext.el --- Enhancement to emacs core.  -*- lexical-binding: t -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "30.0") (locate))
;; Keywords: foo bar baz
;; URL: http://example.com/jrhacker/superfrobnicate

;;; Commentary:
;;  Extentions of the window move line and recenter functions.

;;; Code

;; Requires

(require 'locate)


;; Values

(defvar-local we--top '(top))
(defvar-local we--bottom '(bottom))



;; Functions

(defun we-move-to-window-line-bottom ()
  "Set the cursor on the first line of the current window."
  (let ((recenter-positions we--bottom))
    (move-to-window-line-top-bottom)))

(defun we-move-to-window-line-top ()
  "Set the cursor on the last line of the current window."
  (let ((recenter-positions we--top))
    (move-to-window-line-top-bottom)))

(defun we-recenter-bottom ()
  "Move the current line to the bottom of the window."
  (let ((recenter-positions we--bottom))
    (recenter-top-bottom)))

(defun we-recenter-top ()
  "Move the current line to the top of the window."
  (let ((recenter-positions we--top))
    (recenter-top-bottom)))

(defun we-goto-window-bottom ()
  "Move to the last char in the window and return the current point."
  (we-move-to-window-line-bottom)
  (move-end-of-line nil)
  (point))

(defun we-goto-window-top ()
  "Move to the first char in the window and return the new point."
  (we-move-to-window-line-top)
  (point))

(defun we-line-number-window-relative (number-or-symbol)
  "Return the line numbeber of NUMBER-OR-SYMBOL.

NUMBER-OR-SYMBOL can be an integer or one of the symbols used by
`recenter-positions'. Restores the point before exiting the function. If an
number, it will be relative to the top line if positive and relative to the
bottom line if negative. If number exceeds the lines in the buffer, returns the
corresponding top or bottom line of the buffer."
  (save-excursion
    (let ((recenter-positions (list number-or-symbol)))
      (move-to-window-line-top-bottom)
      (locate-current-line-number))))

(defun we-line-number-bottom-window ()
  "Return the point of the last line, relative to the current window."
  (we-line-number-window-relative 'bottom))

(defun we-line-number-top-window ()
  "Return the point of the first line, relative to the current
window."
  (we-line-number-window-relative 'top))

(defun we-displayed-lines ()
  "Calculate the number of full lines diplayed in the current window."
  (- (we-line-number-bottom-window) (we-line-number-top-window) -1))

(defun we-narrow-to-window ()
  "Narrow to the fully displayed lines in the current window."
  (narrow-to-region (we-goto-window-top) (we-goto-window-bottom)))

(provide 'window-ext)
;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2025, Mark W. Naylor
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
;; Local Variables:
;; read-symbol-shorthands: (("we-" . "window-ext-"))
;; End:

;;; window-ext.el ends here
