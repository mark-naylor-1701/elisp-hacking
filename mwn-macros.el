;;; mwn-macros.el --- General macros.  -*- lexical-binding: t; -*-

;; Copyright (©) 2026 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "31.0"))
;; Keywords: foo bar baz
;; URL: http://example.com/jrhacker/superfrobnicate
;; file:  mwn-macros.el
;; date:  2026-Nov-14

;;; Commentary:
;;  Macros that were originally in .emacs.

;;; Code:

(defmacro nor (&rest args)
  "Equivalent to an integrated circuit nor gate. Takes multiple inputs. Lazy
evaluation of the args expressions."
  `(not (or ,@args)))

(defmacro nand (&rest args)
  "Equivalent to an integrated circuit nand gate. Takes multiple inputs. Lazy
evaluation of the args expressions."
  `(not (and ,@args)))

(defmacro fsetq (name definition &optional docstring)
  "Set NAME’s function definition to DEFINITION, and return DEFINITION.
If the resulting chain of function definitions would contain a loop,
signal a ‘cyclic-function-indirection’ error. If DOCSTRING is not nil,
set the function's documentation."
  `(prog1
     (fset ',name ,definition)
     (when ,docstring
       (put ',name 'function-documentation ,docstring))))

(defmacro until (test &rest body)
  "If TEST yields nil, eval BODY and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on until
TEST' returns non-nil."
  `(while (not ,test)
     (progn ,@body)))

(defmacro if-not (cond then &rest else)
  "If COND yields nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s. THEN
must be one expression, but ELSE... can be zero or more expressions. If
COND yields nil, and there are no ELSE’s, the value is nil.
"
  `(if (not ,cond)
       ,then
     ,@else))

(defmacro do-while (test &rest body)
  "Implement the DO-WHILE loop, exits when TEST returns nil. BODY executes at
least once."
  `(while
       (progn
         ,@body
         ,test)))

(defmacro do-until (test &rest body)
  "Implement the DO-WHILE loop, exits when TEST returns non-nil. BODY executes at
least once."
  `(while
       (progn
         ,@body
         (not ,test))))

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


;;; mwn-macros.el ends here


;;; mwn-macros.el ends here

(provide 'mwn-macros)
