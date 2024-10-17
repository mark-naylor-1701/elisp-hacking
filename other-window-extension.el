;;; other-window-extension.el --- Combination of other window and split window. -*- lexical-binding: t; read-symbol-shorthands: (("owe-" . "other-window-extension-")) -*-

;;; Commentary:
;;  Inspired by the *other-window* functions. Probabably will not implement the
;;  full set.


;; Key              Binding
;; C-x 4 C-f		find-file-other-window
;; C-x 4 C-j		dired-jump-other-window
;; C-x 4 C-o		display-buffer
;; C-x 4 .			xref-find-definitions-other-window
;; C-x 4 0			kill-buffer-and-window
;; C-x 4 1			same-window-prefix
;; C-x 4 4			other-window-prefix
;; C-x 4 a			add-change-log-entry-other-window
;; C-x 4 b			switch-to-buffer-other-window
;; C-x 4 c			clone-indirect-buffer-other-window
;; C-x 4 d			dired-other-window
;; C-x 4 f			find-file-other-window
;; C-x 4 m			compose-mail-other-window
;; C-x 4 p			project-other-window-command
;; C-x 4 r			find-file-read-only-other-window


;; author: Mark W. Naylor
;; file:  other-window-extension.el
;; date:  2024-Oct-17
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
