;;; mode-keybindings.el --- Stripped down version of `describe-mode'  -*- lexical-binding: t; -*-

;; Copyright (©) 2022 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (dash "20210826.1149") (s "20210616.619"))
;; author: Mark W. Naylor
;; file:  mode-keybindings.el
;; date:  2022-Jan-30

(require 'dash)
(require 's)


(defvar mode-keybindings--form-feed ""
  "The Help mode uses the form feed control character to separate
  the sections.")

(defvar mode-keybindings--mode-line-re
  "^\\(.+\\) mode.*fined in"
  "First part of the string that identifies the mode for the
*Help* buffer.")

(defvar mode-keybindings--binding-re
  "key             binding"
  "Fragment that will be in any minor mode block that has been
  assigned hot keys to commands.")

(defvar mode-keybindings--help-buffer-name
  "*Help*"
  "Default name name for emacs help buffers.")

(defvar mode-keybindings--newline
  (char-to-string 10)
  "Newline string.")

(defvar mode-keybindings--minor-section-separator
  (concat mode-keybindings--newline
          "----------------------------------------"
          mode-keybindings--newline
          mode-keybindings--newline)
  "Place this string between the minor mode sections.")

;; -----------------------------------------------------------------------------

(defun mode-keybindings--regex-matches (re s &optional start)
  "Given a regular expression and a string, Return a sequeces of
matches, including groups, if any."
  (cl-labels
      ((regex-matches
        (acc index)
        (let ((this-match (match-string index s)))
          (cond
           (this-match (regex-matches (cons this-match acc) (1+ index)))
           (t (reverse acc))))))

    (when (string-match re s start)
      (regex-matches '() 0))))

(defun mode-keybindings--help-text ()
  "Grab all text from an active Help buffer."
  (when (member mode-keybindings--help-buffer-name (buffer-names))
   (with-current-buffer mode-keybindings--help-buffer-name
     (buffer-substring-no-properties 1 (point-max)))))

(defun mode-keybindings--mode-sections (full-text)
  "Separate the text sections."
  (when full-text
    ;TODO: replace w/ string-split.
    (s-split mode-keybindings--form-feed full-text)))

(defun mode-keybindings--minor-mode-sections (sections)
  "Return the minor mode sections."
  (->> sections
      rest
      ;TODO: replace w/ cl-remove-if-not.
      (-filter (-partial
                #'mode-keybindings--regex-matches
                mode-keybindings--binding-re))))

(defun mode-keybindings--mode-line? (s)
  "Does the string match the pattern for a mode identifying line
  in a help mode buffer?"
  (mode-keybindings--regex-matches mode-keybindings--mode-line-re s))

(defun mode-keybindings--clean-header (lines)
  "From major mode lines, remove all that precede the mode
identifier line."
  (->> lines
       ;TODO: replace -drop-while w/ seq-drop-while
       (-drop-while
        ;TODO: replace -not w/ complement
        (-not #'mode-keybindings--mode-line?))))

(defun mode-keybindings--major-mode-section (sections)
  "Return the first block, describing the major mode, as a
sequence of lines."
  (when sections
    (-> sections
        first
        ;TODO: replace w/ string-trim.
        s-trim
        ;TODO: replace w/ custom function/lambda.
        s-lines
        mode-keybindings--clean-header)))

(defun mode-keybindings--key-binding-text ()
  (describe-mode)
  (let* ((text (mode-keybindings--help-text))
         (sections  (mode-keybindings--mode-sections text))
         (major (mode-keybindings--major-mode-section sections))
         (minors (mode-keybindings--minor-mode-sections sections)))
    ;TODO: replace s-join w/ string-join.
    (s-join mode-keybindings--minor-section-separator
            (cons (s-join mode-keybindings--newline major)
                  ;TODO: replace s-trim w/ string-trim.
                  (mapcar #'s-trim minors)))))

;; Begin glue code section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun nike ()
;;   "Just do it!!!"
;;   (interactive)

;;   (key-binding-text)

;;   )




;; End glue code section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------------------------------------
;; Command function section.


(provide 'mode-keybindings)
;; -----------------------------------------------------------------------------
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

;;; mode-keybindings.el ends here
