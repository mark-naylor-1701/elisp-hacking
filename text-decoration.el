;;; -*- lexical-binding: t; -*-

;;; text-decoration.el --- Various data and function definitions to
;;; allow fancy applications to strings, such as bold, italics,
;;; conversion of lowercase letters to/from small-caps, etc.

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (names "20180321.1155") (dash "2.18.0"))
;; file:  text-decoration.el
;; date:  2020-Dec-21

;;; Commentary:

;; Old requirements
;; (dash-functional "20201215.40") (a "20201203.1927")

;; Diacritical application

(require 'names)
(require 'dash)
(require 's)

;; Begin namespace definition
(define-namespace text-decoration:

(defvar short-strike "̵")
(defvar long-strike "̶")
(defvar low-underline "̲")
(defvar low-double-underline "̳")
(defvar bold-char "*")
(defvar italics-char "/")
(defvar underline "_")

(defun ascii-decorate (decoration)
  "Surrounds an active region with the supplied string."
  (when (use-region-p)
    (let* ((region (list (region-end) (region-beginning))))
      (mapc (lambda (pt) (goto-char pt) (insert decoration)) region)
      (goto-char (+ 2 (car region))))))

;; Decide where to put aliases ;;;;;;;;;
;; Maybe make package comments as suggested mappings.
(defalias 'borg 'bold-ascii)
(defalias 'itrg 'italics-ascii)
(defalias 'ulrg 'underline-ascii)
(defalias 'birg 'bold-italics-ascii)

(defun apply-diacritical (str mark)
  "Add the ASCII `mark' to the beginning and end of `str'."
  (concat mark str mark))

(defun ascii-strikethrough-string (str)
  "Convert `str' to ASCII strikethrough."
  (apply-diacritical str long-strike))

(defun ascii-strikethrough-string-short (str)
  "Convert `str' to ASCII strikethrough."
  (apply-diacritical str short-strike))

(defun ascii-underline-string (str)
  "Convert `str' to ASCII underline."
  (apply-diacritical str low-underline))

(defun double-underline-string (str)
  "Convert `str' to ASCII double underline."
  (apply-diacritical str low-double-underline))

(defun decorate-region (converter)
  "Apply the supplied `converter' function to the string in the active buffer."
  (when-let ((_ (region-active-p))
             (start (region-beginning))
             (end (region-end))
             (src (funcall converter (buffer-substring start end))))
    (delete-region start end)
    (insert src)))

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

(defvar strikethrough-code #x0336)
(defvar underline-code #x0332)

(defun whitespace? (char)
  (when (integerp char)
    (string-match "[[:blank:]]+" (string char))))

(defvar not-whitespace? (-compose #'not #'whitespace?))

(defun c-numeric? (char)
  "Is the character numeric?"
  (<= ?0 char ?9))

(defun c-lowercase? (char)
  "Is the `char' lowercase?"
  (<= ?a char ?z))

(defun c-uppercase? (char)
  "Is the `char' uppercase?"
  (<= ?A char ?Z))

(defun bold-upper-letter (char)
  "Convert `char' to bold uppercase."
  (+ char (- bold-A ?A)))

(defun bold-lower-letter (char)
  "Convert `char' to bold lowercase."
  (+ char (- bold-a ?a)))

(defun bold-numeral (char)
  "Convert `char' to bold numeral."
  (+ char (- bold-0 ?0)))

(defun italic-upper-letter (char)
  "Convert `char' to italic uppercase."
  (+ char (- italic-A ?A)))

(defun italic-lower-letter (char)
  "Convert `char' to italic lowercase."
  (+ char (- italic-a ?a)))

(defun italic-numeral (char)
  "Convert `char' to italic numeral."
  (+ char (- italic-0 ?0)))

(defun bold-italic-upper-letter (char)
  "Convert `char' to bold italic uppercase."
  (+ char (- bold-italic-A ?A)))

(defun bold-italic-lower-letter (char)
  "Convert `char' to bold italic lowercase."
  (+ char (- bold-italic-a ?a)))

(defun bold-italic-numeral (char)
  "Convert `char' to bold italic numeral."
  (+ char (- bold-italic-0 ?0)))

(defun alter-char (code char)
  "Append a Unicode character to a character if not a whitespace.
Return result as a string."
  (if (funcall not-whitespace? char)
      (string char code)
    (string char)))

(defvar strikethrough-char (-partial #'alter-char strikethrough-code))
(defvar underline-char (-partial #'alter-char underline-code))

(defun strikethrough-string (str)
  "Make all non whitespace character Unicode strikethrough charaters."
  (apply #'concat (mapcar strikethrough-char str)))

(defun underline-string (str)
  "Make all non whitespace character Unicode underline charaters."
  (apply #'concat (mapcar underline-char str)))

(defun convert-char (char lower-fn upper-fn numeral-fn)
  "Apply the appropriate conversion function to the `char'."
  (cond
   ((c-lowercase? char) (funcall lower-fn char))
   ((c-uppercase? char) (funcall upper-fn char))
   ((c-numeric? char) (funcall numeral-fn char))
   (t char)))

(defun bold-char (char)
  "Convert `char' to bold."
  (convert-char char
                #'bold-lower-letter
                #'bold-upper-letter
                #'bold-numeral))

(defun italic-char (char)
  "Convert `char' to italic."
  (convert-char char
                #'italic-lower-letter
                #'italic-upper-letter
                #'italic-numeral))

(defun bold-italic-char (char)
  "Convert `char' to bold italic."
  (convert-char char
                #'bold-italic-lower-letter
                #'bold-italic-upper-letter
                #'bold-italic-numeral))

(defun apply-affect (str converter)
  "Convert all the characters in `str' by the `converter' function."
  (concat (mapcar converter str)))

(defun bold-string (str)
  "Convert `str' to bold characters."
  (apply-affect str #'bold-char))

(defun italic-string (str)
  "Convert `str' to italic characters."
  (apply-affect str #'italic-char))

(defun bold-italic-string (str)
  "Convert `str' to bold italic characters."
  (apply-affect str #'bold-italic-char))

;; Section for handling conversion to and from small capital letters
;; -----------------------------------------------------------------
(defvar small-caps (list '(?a . ?ᴀ) '(?b . ?ʙ) '(?c . ?ᴄ) '(?d . ?ᴅ) '(?e . ?ᴇ) '(?f . ?ꜰ)
                         '(?g . ?ɢ) '(?h . ?ʜ) '(?i . ?ɪ) '(?j . ?ᴊ) '(?k . ?ᴋ) '(?l . ?ʟ)
                         '(?m . ?ᴍ) '(?n . ?ɴ) '(?o . ?ᴏ) '(?p . ?ᴘ) '(?q . ?q) '(?r . ?ʀ)
                         '(?s . ?ꜱ) '(?t . ?ᴛ) '(?u . ?ᴜ) '(?v . ?ᴠ) '(?w . ?ᴡ) '(?x . ?x)
                         '(?y . ?ʏ) '(?z . ?ᴢ))

  "The data structue that drives the whole small capital letter
conversion system. It's a mapping with unique domain and range,
so lookups on values work as well as as for keys.")

(defun lower-case<->small-caps (char converter)
  "Convert a character `char' to/from a small capital letter. Action
based upon the supplied `converter' function.
The fuction is of the form (f character look-up-association-list)"
  ;; If an appropriate match for `char' is not found, return `char' instead.
  (or (funcall converter char small-caps) char))

;; Functions to be fed to lower-case<->small-caps. Encapsulates action taken upon a single
;; character. Second function selectes the correct dotted pair from the list. The first
;; function selects the correct element from the pair.

(defvar char-to-smallcaps (-compose #'cdr #'assoc)
  "The key is the first element of the dotted pair, the lowercase
letter. The match is the second element of the pair")

(defvar char-to-lowercase (-compose #'car #'rassq)
  "The key is the second element of the dotted pair, the small
capital letter. The match is the first element of the pair")

(defun string->small-caps (str)
  "Convert the lowercase letters in the string to small capital
letters."
  (concat
   (mapcar #'(lambda (char)
               (lower-case<->small-caps char char-to-smallcaps) )
           (string-to-list str))))

(defun string->no-small-caps (str)
  "Convert the small capital letters in the string to lowercase
letters."
  (concat
   (mapcar #'(lambda (char)
               (lower-case<->small-caps char char-to-lowercase) )
           (string-to-list str))))


(defun region-convert (string-converter)
  "If a region is active, change all its letters using the
`string-converter' function."
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (src (funcall string-converter (buffer-substring start end))))
      (delete-region start end)
      (goto-char start)
      (insert src)
      ;; Since `insert' returns nil, force to t when successfully
      ;; reaching this point in the conversion process.
      t)))

(defvar vowels
  '((?a . ?α) (?A . ?Α)
    (?e . ?ε) (?E . ?Ε)
    (?i . ?ι) (?I . ?Ι)
    (?o . ?ο) (?O . ?Ο)
    (?u . ?υ) (?U . ?Υ))
  "Convertion table of Latin vowels to Greek.")

(defun greekify-char (char)
  "Return a Greek chararter if `char' is a Latin vowel, `char' otherwise."
  ;; (assoc-default char vowels nil char)
  (or
   (cdr (assoc char vowels))
   char))

(defun ungreekify-char (char)
  "Return a Latin chararter if `char' is a Greek vowel, `char' otherwise."
  (or (->> vowels (rassoc char) car)
      char))

(defun greekify-string (str)
  "Convert all Greek vowels to Latin vowels in a string."
  (concat (mapcar #'greekify-char str)))

(defun ungreekify-string (str)
  "Convert all Latin vowels to Greek vowels in a string."
  (concat (mapcar #'ungreekify-char str)))

(defun split-join (str split-str join-str)
  "Split a string with one character, join it with another."
  (s-join join-str
          (s-split split-str str t)))

(defun unspreadify-string (str)
  "Remove asterisks from a spread string."
  (split-join str "*" ""))

(defun spreadify-string (str)
  "Spread a plain string apart with asteriks."
  (split-join str "" "*"))

(defun bol-point ()
  "Return the point of the beginning of the current line."
  (save-excursion
    (move-beginning-of-line nil)
    (point)))

(defun eol-point ()
  "Return the point of the end of the current line."
  (save-excursion
    (move-end-of-line nil)
    (point)))

(defun find-greek-vowel ()
  "Move the point to the next Greek vowel, if any."
  (search-forward-regexp "[αΑεΕιΙοΟυΥ]" nil t))

(defun forward-blank-punct ()
  "Move the point the next character that is either whitespace or
a punctuation character, if any. Otherwise, move to end of the
line."
  (or
   (search-forward-regexp "[[:blank:][:punct:]]" (eol-point) t)
   (move-end-of-line nil)))

(defun backward-blank-punct ()
  "Move the point the previous character that is either
whitespace or a punctuation character, if any. Otherwise, move to
beginning of the line."
  (or
   (search-backward-regexp "[[:blank:][:punct:]]" (bol-point) t)
   (move-beginning-of-line nil)))

) ;; End of the namespace definition.

;; Public Area ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bold-ascii ()
  "Makes the active region bold."
  (interactive)
  (text-decoration:ascii-decorate text-decoration:bold-char))

(defun italics-ascii ()
  "Makes the active region italics."
  (interactive)
  (text-decoration:ascii-decorate text-decoration:italics-char))

(defun underline-ascii ()
  "Makes the active region underlined."
  (interactive)
  (text-decoration:ascii-decorate text-decoration:underline))

(defun bold-italics-ascii ()
  "Makes the active region bold-italics."
  (interactive)
  (bold-ascii)
  (italics-ascii))

(defun strikethrough-region ()
  "Makes the active region strikethrough."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:strikethrough-string))

(defun short-strikethrough-region ()
  "Makes the active region short strikethrough."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:ascii-strikethrough-string-short))

(defun underline-region ()
  "Makes the active region underlined."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:underline-string))

(defun double-underline-region ()
  "Makes the active region double underlined."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:double-underline-string))

(defun bold-region ()
  "Makes the active region bold."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:bold-string))

(defun italic-region ()
  "Makes the active region italics."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:italic-string))

(defun bold-italic-region ()
  "Makes the active region bold italics."
  (interactive)
  (text-decoration:decorate-region #'text-decoration:bold-italic-string))

(defun region-smallcaps ()
  "Turn all lowercase letters in a selected region to small capitals."
  (interactive)
  (text-decoration:region-convert #'text-decoration:string->small-caps))

(defun region-nosmallcaps ()
  "Turn all lowercase letters in a selected region to small capitals."
  (interactive)
  (text-decoration:region-convert #'text-decoration:string->no-small-caps))

(defun greekify-region ()
  "Convert all Latin vowels in the selected region to Greek vowels."
  (interactive)
  (text-decoration:region-convert #'text-decoration:greekify-string))

(defun ungreekify-region ()
  "Convert all Greek vowels in the selected region to Latin vowels."
  (interactive)
  (text-decoration:region-convert #'text-decoration:ungreekify-string))

(defun ungreekify-buffer ()
  "Convert all Greek vowels in the buffer to Latin vowels."
  (interactive)
  (save-excursion
    (set-mark 1)
    (goto-char (point-max))
    (ungreekify-region)))

(defun mark-word-greek-vowel ()
  "Search forward for the next Greek vowel. If found, make the
word that contains it the active region. Word boundaries
determined by whitespace, punctuation, beginning of line, or end
of line."
  (when (text-decoration:find-greek-vowel)
    (text-decoration:backward-blank-punct)
    (set-mark (point))
    (text-decoration:forward-blank-punct)
    (region-bounds)))

(defun string-regions (searcher)
  "Generate a stack of regions that meet the criteria defined by
`searcher'. `searcher' is a function that locates a string, marks
it as the current region, and returns its region bounds. Return
matches `region-bounds'. The stack will have the last region in
the buffer as its head."
  (cl-labels
      ((-string-regions
        (acc)
        (cond
         ((when-let ((bounds (funcall searcher))) (-string-regions (cons (car bounds) acc))))
         (:else acc))))
    (-string-regions ())))

(defun greekified-regions ()
  "Return a list of all regions the have words with Greek vowels."
  (string-regions #'mark-word-greek-vowel))

(provide 'text-decoration)
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


;;; text-decoration.el ends here
