;;; obarray-ext.el --- Helper functions to extend obarray built-ins -*- lexical-binding: t; -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "30.0"))
;; Keywords: obarray
;; URL: https://github.com/mark-naylor-1701/elisp-hacking/obarray-ext.el
;; date:  2025-Jun-07


;;; Commentary:
;;



;;; Code:
;; Public

(cl-defun oba-list (&optional pred (o obarray))
  "Convert an obarray to a list.
`PRED' is a filter function for selecting items from `O'. If nil, all
items are selected.
`O' is an optional obarray object. If not specified, uses the default
elisp obarray."
  (when (obarrayp o)
    (if pred
        (cl-loop for sym being symbols of o
                 when (funcall pred sym) collect sym)
      (cl-loop for sym being symbols of o collect sym))))

(cl-defun oba-variables (&optional (o obarray))
  "Retrieve the variables from an obarray.
Unless `O' is supplied, use the default elisp obarray."
  (oba-list #'boundp o))

(cl-defun oba-functions (&optional (o obarray))
  "Retrieve the functions from an obarray.
Unless `O' is supplied, use the default elisp obarray."
  (oba-list #'fboundp o))

(cl-defun oba-commands (&optional (o obarray))
  "Retrieve the commands from an obarray.
Unless `O' is supplied, use the default elisp obarray."
  (oba-list #'commandp o))


;; Private

(defun oba--unboundp (sym)
  "Return non-nil if the symbol `SYM' has neither a function binding nor a
value binding."
  (when (symbolp sym)
    (not (or (boundp sym)
             (fboundp sym)))))

(provide 'obarray-ext)

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


;;; obarray-ext.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("oba-" . "obarray-ext-"))
;; End:
