;; author: Mark W. Naylor
;; file:  text-decoration.el
;; date:  2020-Dec-21

;; Diacritical application

(require 'subr-x)

(defvar short-strike "̵")
(defvar long-strike "̶")
(defvar low-underline "̲")
(defvar low-double-underline "̳")
(defvar bold-char "*")
(defvar italics-char "/")
(defvar underline-char "_")

(defun ascii-decorate (decoration)
  "Surrounds an active region with the supplied string."
  (when (use-region-p)
    (let* ((region (list (region-end) (region-beginning))))
      (mapc (lambda (pt) (goto-char pt) (insert decoration)) region)
      (goto-char (+ 2 (car region))))))

(defun bold-ascii ()
  "Makes the active region bold."
  (interactive)
  (ascii-decorate bold-char))

(defun italics-ascii ()
  "Makes the active region italics."
  (interactive)
  (ascii-decorate italics-char))

(defun underline-ascii ()
  "Makes the active region underlined."
  (interactive)
  (ascii-decorate underline-char))

(defun bold-italics-ascii ()
  "Makes the active region bold-italics."
  (interactive)
  (bold-ascii)
  (italics-ascii))

(defalias 'borg 'bold-ascii)
(defalias 'itrg 'italics-ascii)
(defalias 'ulrg 'underline-ascii)
(defalias 'birg 'bold-italics-ascii)

(defun apply-diacritical (a-string mark)
  (concat
    (s-join mark
            (butlast
             (rest
              (s-split "" a-string))))
    mark))

(defun strikethrough-string (a-string)
  (apply-diacritical a-string long-strike))

(defun strikethrough-string-short (a-string)
  (apply-diacritical a-string short-strike))

(defun underline-string (a-string)
  (apply-diacritical a-string low-underline))

(defun double-underline-string (a-string)
  (apply-diacritical a-string low-double-underline))

(defun decorate-region (converter)
  "docstring"
  (when-let ((_ (region-active-p))
             (start (region-beginning))
             (end (region-end))
             (src (funcall converter (buffer-substring start end))))
    (delete-region start end)
    (insert src)))

(defun strikethrough-region ()
  (interactive)
  (decorate-region #'strikethrough-string))

(defun short-strikethrough-region ()
  (interactive)
  (decorate-region #'short-strikethrough-region))


(defun underline-region ()
  (interactive)
  (decorate-region #'underline-string))

(defun double-underline-region ()
  (interactive)
  (decorate-region #'double-underline-string))

(defun bold-region ()
  "docstring"
  (interactive)
  (decorate-region #'bold-string))

(defun italic-region ()
  "docstring"
  (interactive)
  (decorate-region #'italic-string))

(defun bold-italic-region ()
  "docstring"
  (interactive)
  (decorate-region #'bold-italic-string))


;; Apply affects: bold, italic, bold-italic
(defvar bold-A ?𝗔 "Base for bold character embellishments. All san-serif.")
(defvar italic-A ?𝘈 "Base for italic character embellishments. All san-serif.")
(defvar bold-italic-A ?𝘼 "Base for bold+italic character embellishments. All san-serif.")

(defvar bold-a ?𝗮 "Base for bold character embellishments. All san-serif.")
(defvar italic-a ?𝘢 "Base for italic character embellishments. All san-serif.")
(defvar bold-italic-a ?𝙖 "Base for bold+italic character embellishments. All san-serif.")

(defvar bold-0 ?𝟬 "Base for bold character embellishments. All san-serif.")
(defvar italic-0 ?0 "Base for italic character embellishments. All san-serif.")
(defvar bold-italic-0 ?0 "Base for bold+italic character embellishments. All san-serif.")

(defun c-numeric? (char)
  (<= ?0 char ?9))

(defun c-lowercase? (char)
  (<= ?a char ?z))

(defun c-uppercase? (char)
  (<= ?A char ?Z))

(defun bold-shift-upper-letter (a-char)
  (+ a-char (- bold-A ?A)))

(defun bold-shift-lower-letter (a-char)
  (+ a-char (- bold-a ?a)))

(defun bold-shift-numeral (a-char)
  (+ a-char (- bold-0 ?0)))

(defun italic-shift-upper-letter (a-char)
  (+ a-char (- italic-A ?A)))

(defun italic-shift-lower-letter (a-char)
  (+ a-char (- italic-a ?a)))

(defun italic-shift-numeral (a-char)
  (+ a-char (- italic-0 ?0)))

(defun bold-italic-shift-upper-letter (a-char)
  (+ a-char (- bold-italic-A ?A)))

(defun bold-italic-shift-lower-letter (a-char)
  (+ a-char (- bold-italic-a ?a)))

(defun bold-italic-shift-numeral (a-char)
  (+ a-char (- bold-italic-0 ?0)))

(defun bold-italic-char (a-char)
  (cond
   ((c-lowercase? a-char) (bold-italic-shift-lower-letter a-char))
   ((c-uppercase? a-char) (bold-italic-shift-upper-letter a-char))
   ((c-numeric? a-char) (bold-italic-shift-numeral a-char))
   (t a-char)))

(defun shift-char (a-char lower-shift-fn upper-shift-fn numeral-shift-fn)
  (cond
   ((c-lowercase? a-char) (funcall lower-shift-fn a-char))
   ((c-uppercase? a-char) (funcall upper-shift-fn a-char))
   ((c-numeric? a-char) (funcall numeral-shift-fn a-char))
   (t a-char)))

(defun bold-char (a-char)
  (shift-char a-char
              #'bold-shift-lower-letter
              #'bold-shift-upper-letter
              #'bold-shift-numeral))

(defun italic-char (a-char)
  (shift-char a-char
              #'italic-shift-lower-letter
              #'italic-shift-upper-letter
              #'italic-shift-numeral))

(defun bold-italic-char (a-char)
  (shift-char a-char
              #'bold-italic-shift-lower-letter
              #'bold-italic-shift-upper-letter
              #'bold-italic-shift-numeral))



(defun apply-affect (a-string shift-fn)
  (concat (mapcar shift-fn a-string)))

(defun bold-string (a-string)
   (apply-affect a-string #'bold-char))

(defun italic-string (a-string)
   (apply-affect a-string #'italic-char))

(defun bold-italic-string (a-string)
   (apply-affect a-string #'bold-italic-char))


;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2020, Mark W. Naylor
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
