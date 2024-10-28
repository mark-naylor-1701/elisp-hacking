;;; other-window-extension.el --- Combination of other window and split window. -*- lexical-binding: t; read-symbol-shorthands: (("owe-" . "other-window-extension-")) -*-


;; author: Mark W. Naylor
;; file:  other-window-extension.el
;; date:  2024-Oct-17


;;; Commentary:
;;  Inspired by the *other-window* functions. Probabably will not implement the
;;  full set. TBD dertermined based upon lessons learned during incremental development.


;; Similiar to (switch-to-buffer-other-window BUFFER-OR-NAME &optional NORECORD)
(defun owe-buffer-below (buffer-or-name &optional norecord)
  "Create a window below the current one, make it active, switch to
the selected buffer."
  (interactive (list (read-buffer "Select a buffer for the new window: ")))
  (select-window (split-window-below current-prefix-arg))
  (switch-to-buffer buffer-or-name))

;; Similiar to (switch-to-buffer-other-window BUFFER-OR-NAME &optional NORECORD)
(defun owe-buffer-right (buffer-or-name &optional norecord)
  "Create a window to the right of the current one, make it active,
switch to the selected buffer."
  (interactive (list (read-buffer "Select a buffer for the new window: ")))
  (select-window (split-window-right current-prefix-arg))
  (switch-to-buffer buffer-or-name))

;; Similiar to (find-file-other-window FILENAME &optional WILDCARDS)
(defun owe-file-below (filename &optional wildcards)
  "Create a window below the current one, make it active, switch to
the selected file."
  (interactive (list (read-file-name "Select file for the new window: ")))
  (select-window (split-window-below current-prefix-arg))
  (find-file filename))

;; Similiar to (find-file-other-window FILENAME &optional WILDCARDS)
(defun owe-file-right (filename &optional wildcards)
  "Create a window below the current one, make it active, switch to
the selected file."
  (interactive (list (read-file-name "Select file for the new window: ")))
  (select-window (split-window-right current-prefix-arg))
  (find-file filename))

;; Similiar to (find-file-read-only-other-window FILENAME &optional WILDCARDS)
(defun owe-file-read-only-below (filename &optional wildcards)
  "Create a window below the current one, make it active, switch to
the selected file in read-only mode."
  (interactive
   (list (read-file-name "Select file for the new window: " nil nil t)))
  (select-window (split-window-below current-prefix-arg))
  (find-file-read-only filename))

;; Similiar to (find-file-read-only-other-window FILENAME &optional WILDCARDS)
(defun owe-file-read-only-right (filename &optional wildcards)
  "Create a window to the right of the current one, make it active,
switch to the selected file in read-only mode."
  (interactive
   (list (read-file-name "Select file for the new window: " nil nil t)))
  (select-window (split-window-right current-prefix-arg))
  (find-file-read-only filename))

;; Similiar to (other-window-prefix)
(defun owe-prefix-below (cmd)
  "Create a window below the current one and run a selected comand in it."
  (interactive (list (read-command "Select command to run in the new window: ")))
  (select-window (split-window-below current-prefix-arg))
  (call-interactively cmd))

;; Similiar to (other-window-prefix)
(defun owe-prefix-right (cmd)
  "Create a window right the current one and run a selected comand in it."
  (interactive (list (read-command "Select command to run in the new window: ")))
  (select-window (split-window-right current-prefix-arg))
  (call-interactively cmd))

;; Similiar to (dired-other-window DIRNAME &optional SWITCHES)
(defun owe-directory-below (dirname &optional switches)
  "Create a window below the current one and open the selected directory in
dired mode."
  (interactive (list (read-directory-name "Select a directory for the new window:")))
  (select-window (split-window-below current-prefix-arg))
  (dired dirname))

;; Similiar to (dired-other-window DIRNAME &optional SWITCHES)
(defun owe-directory-right (dirname &optional switches)
  "Create a window below the current one and open the selected directory in
dired mode."
  (interactive (list (read-directory-name "Select a directory for the new window:")))
  (select-window (split-window-right current-prefix-arg))
  (dired dirname))


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
(provide 'other-window-extension)

;;; other-window-extension.el ends here
