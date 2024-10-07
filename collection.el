;;; collection.el --- Checks for composite and atomic data types. -*- lexical-binding: t;  -*-

;; Copyright (©) 2024 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "29.0") (dash "2.19.1"))
;; Keywords:
;; URL:
;; File:  collection.el
;; Date:  2024-Jun-09


;;; Commentary:
;;  Provide functions that help determine if a value contains a single value or
;;  multiple values. Data types like array, string, and hash table are
;;  considerred to be multiple/composite even if they contain zero items or one
;;  item.


;;; Code:

;; Requires

(require 'dash)


;; Values

(defvar collection--base-test-fns
  (list #'consp #'arrayp #'hash-table-p #'recordp))
;; composite if any of the base tests pass.
(defvar collection-composite-fn
      (apply #'-orfn collection--base-test-fns))
;; atomic if none of the base tests pass.
(defvar collection-atomic-fn
      (apply #'-andfn (mapcar #'-not collection--base-test-fns )))


;; Functions

;; Public

(defun collection-compositep (x)
  "Is X a data type that contains (or could contain) multiple items?"
  (funcall collection-composite-fn x))


(defun collection-atomicp (x)
  "Is X a data type that contains a singular item?"
  (funcall collection-atomic-fn x))

(provide 'collection)


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


;;; collection.el ends here
