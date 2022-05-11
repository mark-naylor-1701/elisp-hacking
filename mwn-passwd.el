;;; mwn-passwd.el --- Password wrapper

;; Copyright (©) 2022 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0"))
;; Keywords: security password
;; URL: http://example.com/jrhacker/superfrobnicate
;; file:  mwn-passwd.el
;; date:  2022-May-09

;;; Commentary:
;;; Gets a password. Provides options for masking entry,
;;; confirming correcy entry when masked, and setting a supplied
;;; variable with the password value.

;;(require 'cl-lib)

(cl-defun mwn-passwd-read (&key (prompt "Enter password: ")
                                (mask t)
                                (confirm t)
                                (var-sym))
  (let* (password)
    (cond
     (mask (setq password (read-passwd prompt confirm)))
     (t (setq password (read-string prompt))))
    (when (and var-sym (symbolp var-sym))
      (set var-sym password))
    password))

(provide 'mwn-passwd)
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


;;; mwn-passwd.el ends here
