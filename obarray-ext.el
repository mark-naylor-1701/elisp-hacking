;;;  -*- lexical-binding: t; -*-
;;; obarray-ext.el --- Helper functions to extend obarray built-ins

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "30.0"))
;; Keywords: obarray
;; URL: http://example.com/jrhacker/superfrobnicate
;; date:  2025-Jun-07


;;; Commentary:
;;

;;; Code:

(cl-defun oba-obarray-list (&optional pred (o obarray))
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

(cl-defun oba-obarray-variables (&optional (o obarray))
  "Retrieves the variables from an obarray. Unless `o' is supplied, use the
default elisp obarray."
  (oba-obarray-list #'boundp o))

(cl-defun oba-obarray-functions (&optional (o obarray))
  "Retrieves the functions from an obarray. Unless `o' is supplied, use the
default elisp obarray."
  (oba-obarray-list #'fboundp o))

(cl-defun oba-obarray-commands (&optional (o obarray))
  "Retrieves the commands from an obarray. Unless `o' is supplied, use the
default elisp obarray."
  (oba-obarray-list #'commandp o))

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
