;;; word-hunter.el --- Utility for finding and replacing strings.

;; Copyright (©) 2021 Mark W. Naylor

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "26.0") (names "20180321.1155"))
;; Keywords: foo bar baz
;; URL: http://example.com/jrhacker/superfrobnicate
;; author: Mark W. Naylor
;; file:  word-hunter.el
;; date:  2021-Jun-10

(require 'names)

(define-namespace word-hunter:

(defun wrap-word-bounds (target)
  "Turn a string or regexp into a regexp that matches beginning and end word boundaries."
  (concat "\\<" target "\\>"))

(defun find-and-mark (target)
  "If a word matches the regexp, set it to the active region."
  (when (search-forward-regexp (wrap-word-bounds target) nil t)
    (set-mark-command nil)
    (search-backward-regexp "\\<")
    t))

(defun transform-from-point (modifier target &optional max-transforms)
  "Apply `MODIFIER' to all (or at most `MAX-TRANSFORMS')
  instances of `TARGET' starting from the current point in the
  bubuffer. `MODIFIER' is a function that changes the active
  region. Returns the count of strings changed."

  (cl-labels
      ((-transform (count region-selected?)
         (cond
          ((or (null region-selected?)
               (and (not (null max-transforms))
                    (>= count max-transforms)))
           count)
          (t (progn
               (funcall modifier)
               (-transform (1+ count) (find-and-mark target))))
          )))
    (-transform 0 (find-and-mark target)))))

(defun transform-in-buffer (modifier target &optional max-transforms)
  "Go to beginning of buffer and apply `TRANSFORM-FROM-POINT.'"
  (save-excursion
    (goto-char (point-min))
    (word-hunter:transform-from-point modifier target max-transforms)))

(provide 'word-hunter)
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


;;; word-hunter.el ends here
