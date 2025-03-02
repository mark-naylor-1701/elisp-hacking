;;; crossword-merge-util.el --- Functions to manange crossword puzzle merging and print tracking.  -*- lexical-binding: t; -*-

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (dash "20201218.1237") (s "20180406.808"))
;; Keywords:
;; date:  2021-Feb-25

(require 'cl)                           ; Built in, ergo not in Package-Requires
(require 'dash)
(require 's)


(defvar cw/combined "cw.pdf" "The combined crossword pdf file.")
(defvar cw/printed "printed.txt" "List of crosswords that have been printed.")

(defun cw/contains (str)
  (s-contains? cw/combined str))

(defun cw/bases (xs)
  (-flatten
   (mapcar
    #'(lambda (x) (last (s-split "/" x)))
    xs)))

(defun cw/sources (xs)
  "Get the stripped down crossword files (sorted). Target file
name removed if in the collection."
  (sort (cl-remove-if #'cw/contains (cw/bases xs)) #'string-lessp))

(defun cw/build-command (xs)
  "Given a collection of file names, create the concatenation command string."
  (when (and xs (listp xs))
    (cons "pdfunite"
          (concat (s-join " " (cw/sources xs))
                  " "
                  cw/combined))))

(defun cw/log-print (xs)
  "Add the pdf files in the collection to the print log."
  (let ((src (cw/sources xs))
        (src-buffer  (current-buffer))
        (working (find-file cw/printed)))
    (goto-char (point-max))
    (dolist (item src)
      (insert item)
      (newline))
    (save-buffer working)
    (kill-buffer working)
    (switch-to-buffer src-buffer)))

(provide 'crossword-merge-util)

;; ------------------------------------------------------------------------------
;; BSD 3-Clause License

;; Copyright © 2021, Mark W. Naylor
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


;;; crossword-merge-util.el ends here
