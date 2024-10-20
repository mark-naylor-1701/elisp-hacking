;;; other-window-extension.el --- Combination of other window and split window. -*- lexical-binding: t; read-symbol-shorthands: (("owe-" . "other-window-extension-")) -*-


;; author: Mark W. Naylor
;; file:  other-window-extension.el
;; date:  2024-Oct-17


;;; Commentary:
;;  Inspired by the *other-window* functions. Probabably will not implement the
;;  full set. TBD dertermined baesed upon lessons learned during incremental development.

;; This form defines buffer local functions that should be kept out of the
;; global namespace. Lexical binding makes them available to public functions.
(cl-labels
    ((-buffer-name (buffer)
       (interactive "BSelect a buffer for the new window:")
       buffer)

     (-file-name (file)
       (interactive "FSelect a file for the new window:")
       file)

     (-file-name-read-only (file-ro)
       (interactive "fSelect a read-only file for the new window:")
       file-ro)

     (-command (cmd)
       (interactive "CSelect a command to run in the new window:")
       cmd)

     (-directory (dir)
       (interactive "DSelect a directory for dired in the new window:")
       dir))

  (defun owe-buffer-below (lines)
    "Create a window below the current one, make it active, switch to
the selected buffer."
    (interactive "P")
    (when-let (buffer (call-interactively #'-buffer-name))
      (select-window (split-window-below lines))
      (switch-to-buffer buffer)))

  (defun owe-buffer-right (cols)
    "Create a window to the right of the current one, make it active,
switch to the selected buffer."
    (interactive "P")
    (when-let (buffer (call-interactively #'-buffer-name))
      (select-window (split-window-right cols))
      (switch-to-buffer buffer)))

  (defun owe-file-below (lines)
    "Create a window below the current one, make it active, switch to
the selected file."
    (interactive "P")
    (when-let (file (call-interactively #'-file-name))
      (select-window (split-window-below lines))
      (find-file file)))

  (defun owe-file-right (cols)
    "Create a window to the right of the current one, make it active,
switch to the selected file."
    (interactive "P")
    (when-let (file (call-interactively #'-file-name))
      (select-window (split-window-right cols))
      (find-file file)))

  (defun owe-file-read-only-below (lines)
    "Create a window below the current one, make it active, switch to
the selected file in read-only mode."
    (interactive "P")
    (when-let (file (call-interactively #'-file-name-read-only))
      (select-window (split-window-below lines))
      (find-file-read-only file)))

  (defun owe-file-read-only-right (cols)
    "Create a window to the right of the current one, make it active,
switch to the selected file in read-only mode."
    (interactive "P")
    (when-let (file (call-interactively #'-file-name-read-only))
      (select-window (split-window-right cols))
      (find-file-read-only file)))

  (defun owe-prefix-below (lines)
    "Create a window below the current one and run a selected comand in it."
    (interactive "P")
    (when-let ((cmd (call-interactively #'-command)))
      (select-window (split-window-below lines))
      (call-interactively cmd)))

  (defun owe-prefix-right (cols)
    "Create a window to the right of the current one and run a selected comand in it."
    (interactive "P")
    (when-let ((cmd (call-interactively #'-command)))
      (select-window (split-window-right cols))
      (call-interactively cmd)))

  (defun owe-directory-below (lines)
    "Create a window below the current one and open the selected directory in
dired mode."
    (interactive "P")
    (when-let ((dir (call-interactively #'-directory)))
      (select-window (split-window-below lines))
      (dired dir)))

  (defun owe-directory-right (cols)
    "Create a window below the current one and open the selected directory in
dired mode."
    (interactive "P")
    (when-let ((dir (call-interactively #'-directory)))
      (select-window (split-window-right cols))
      (dired dir)))

  )

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
