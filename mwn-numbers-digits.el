;;; -*- lexical-binding: t -*-
;; author: Mark W. Naylor
;; file:  mwn-numbers-digits.el
;; date:  2026-Sep-22


;;; Commentary:

;;  Provides conversions from a number to the digits that represent it. For
;;  bases over 10, the digits are the capital letter string.
;;  Ex.: A -> 10, B -> 11, C -> 12, ..., Z -> 35

(require 'dash)


;;; Code:

;; Public functions

(cl-defun mwn-number->digits (n &optional (base 10))
  "Given `N', an integer, return a list of digits representing that number, in
base `BASE'."
  (let ((digits
         (named-let recur ((accum ()) (n n))
           (cond
            ((zerop n) (or accum '(0)))
            (t (recur (cons (mod n base) accum) (/ n base)))))))
    (if (<= base 10)
        digits
      (mapcar #'mwn--value->digit digits))))

(cl-defun mwn-digits->number (ns &optional (base 10))
  "Given a list of digits `NS' in base `BASE', construct the number they represent."
  (-reduce
   (lambda (x y) (+ y (* base x)))
   (mapcar
    #'mwn--digit->value
    (mapcar
     (lambda (x)
       (if (stringp x)
           (upcase x)
         x))
     ns))))


;; Private variabels

(defvar mwn--alphas
  (mapcar
   #'char-to-string
   (range ?A (1+ ?Z)))
  "The letters ?A to ?Z")

(defvar mwn--values
  (range 10 36)
  "Equivalent values for letters a through z")

(defvar mwn--alphas-x-values
  (-zip mwn--alphas mwn--values)
  "Match letters to values.")

(defvar mwn--values-x-alphas
  (-zip mwn--values mwn--alphas)
  "Match values to letters.")

;; Private functions

(defun mwn--digit->value (str-or-num)
  "Given a number or string, return its value"
  ;; (alist-get str-or-num mwn--syms-x-values str-or-num)
  (cond
   ((stringp str-or-num) (alist-get str-or-num mwn--alphas-x-values nil nil #'equal))
   ((numberp str-or-num) str-or-num)))

(defun mwn--value->digit (num)
  "Given a number, return its symbol."
  (alist-get num mwn--values-x-alphas num ))

;; ------------------------- begin scratch -------------------------------------
;; ------------------------- end scratch   -------------------------------------


;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2026, Mark W. Naylor
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

(provide 'mwn-numbers-digits)

;;; mwn-numbers-digits.el ends here
