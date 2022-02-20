;; author: Mark W. Naylor
;; file:  unbinding.el
;; date:  2022-Feb-18

;; Private variable definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar unbinding--optional-whitespace "[[:space:]]*")
(defvar unbinding--required-whitespace "[[:space:]]+")

(defun unbinding--build-re (s)
  "Helper function. Used to contruct search regular expressions."
  (concat
   "("
   unbinding--optional-whitespace
   s
   unbinding--required-whitespace))

(defvar unbinding--defvar-re
  (unbinding--build-re "defvar")
  "Expression used to locate variable definitions.")

(defvar unbinding--defun-re
  (unbinding--build-re "defun")
  "Expression used to locate functon definitions.")

;; Private function definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unbinding--s-expr (regexp)
  "Return a single instance, if any, of an s-expression, which is prefixed by `regexp'."
  (when (search-forward-regexp regexp nil t)
    (let* ((start (point)))
      (forward-sexp)
      (buffer-substring-no-properties start (point)))))

(defun unbinding--s-expressions (regexp)
 "Return a collection of s-expressions, using the `regexp' to
  locate the string prefix of an individual s-expression."
 (cl-labels
     ((s-expressions
       (acc)
       (if-let (s-exp (unbinding--s-expr regexp))
           (s-expressions (cons s-exp acc))
         acc)))

   (save-excursion
     (goto-char 1)
     (s-expressions ()))))

(defvar unbinding--defvars
  (-partial #'unbinding--s-expressions
            unbinding--defvar-re)
  "Return all the s-expressions that repesent variable definitions.")

(defvar unbinding--defuns
  (-partial #'unbinding--s-expressions
            unbinding--defun-re)
  "Return all the s-expressions that repesent function definitions.")

(defun unbinding--ubound-fn (sym)
  "Return the proper unbounding function, depending on whether
  `sym' is a function or a variable. sym should be a symbol."
  (if (functionp sym)
      #'fmakunbound
    #'makunbound))

(defun unbinding--ubound-item (s)
  "Given an s-exp name `s', make it nil if it has been defined."
  (when-let ((sym (intern-soft s)))
    (funcall (unbinding--ubound-fn sym) sym)))

;; Public function definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2022, Mark W. Naylor
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