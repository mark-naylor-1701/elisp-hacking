;;; restore-mode-map.el --- Fix keymaps for major (or minor) modes. -*- lexical-binding: t -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "30.0") (obarray-ext "0.9") (s "20220902.1511")
;; Keywords: mode map keybinding
;; URL: https://github.com/mark-naylor-1701/elisp-hacking/restore-mode-map.el
;; date:  2025-Jun-07

;;; Commentary:
;;



;;; Requires:
(require 'obarray-ext)


;;; Code:

(defun rmm-restore (arg)
  "docstring"
  (interactive "P")
  (rmm--restore
   (cond
    (arg (rmm--select-mode-map))
    (t (concat (symbol-name major-mode) "-map")))))



;; Private

(defun rmm--restore (mode-map-name-str)
  "When `MODE-NAME-STR' matches a major or minor mode, reset its key map to the
original definition."
  (when-let* ((mode-sym (intern-soft mode-map-name-str)))
    (when (boundp mode-sym)
      (describe-variable mode-sym)
      (switch-to-buffer "*Help*")
      (help-view-source)
      ;;(forward-sexp)
      ;;(eval-last-sexp)
      (forward-char)
      (eval-defun nil)
      (kill-buffer-and-window)
      ;;(switch-to-buffer "*Help*")
      (select-window (next-window))
      (help-go-back)
      (delete-window))))

(defun rmm--mode-map-name-p (str)
  "Determine if `STR' has a name that matches a mode map pattern."
  (s-ends-with? "-mode-map" str))


(defun rmm--mode-map-symbol-p (sym)
  "Determine if `SYM' matches a mode map variable pattern."
  (rmm--mode-map-name-p (symbol-name sym)))

(defun rmm--select-mode-map ()
  "Generate a list of mode map variables, prompt the user to select one."
  (completing-read
   "Select a variable: "
   (obarray-ext-variables)
   #'rmm--mode-map-symbol-p))

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

(provide 'restore-mode-map)

;; Local Variables:
;; read-symbol-shorthands: (("rmm-" . "restore-mode-map-"))
;; End:

;;; restore-mode-map.el ends here
