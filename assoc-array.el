;;; assoc-array.el --- Print associative arrays. -*- lexical-binding: t; -*-

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "29.0") (collection "0.9"))
;; Keywords:
;; URL:
;; Date:  2024-Jun-04
;; File:  assoc-array.el

;;; Commentary:
;;  For a very large associative array type value (alist, plist, or hash-table),
;;  Emacs will not display all the keys and values. This package supplies the
;;  functions to open a buffer and dump the contents in a more readable form.


;;; Code:

;; Requires
(require 'dash)
(require 'collection)


;; Functions

(defun asca-list->alist (xs)
  "Convert an ordinary list into an alist.

Takes pairs from the original list XS and construcst cons cells
from them. If the list has an odd number of elements, the last
one will have a nil value."
  (let ((acc ()))
    (while xs
      (setq acc
            (append
             (list (cons (car xs) (cadr xs)))
             acc))
      (setq xs (cddr xs)))
    (reverse acc)))

(defun asca-hash-table->alist (ht)
  "Convert a hash table HT to an alist.

Uses key/value pairs to construct the cons cells."
  (cl-mapcar #'cons (hash-table-keys ht) (hash-table-values ht)))

(cl-defun asca-dump-alist (alist &optional buffer-or-name)
  "Print out the key value pairs.

ALIST is the structure to be printed. Results go into a buffer. B is the
  optional buffer, defaults to *dump* if one is not supplied."
  (let ((buffer (get-buffer-create (or buffer-or-name "*dump*"))))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (save-excursion
     (while alist
       (princ (caar alist) buffer)
       (princ " :: " buffer)
       (princ (cdar alist) buffer)
       (princ "\n" buffer)
       (setq alist (cdr alist))))))

(defun asca-alistp (xs)
  "Determine if XS is a list of lists."
  (and (consp xs)
       (cl-every #'consp xs)))

(defun asca-dump-associative-array (aa)
  "Prints out the keys and their values of an associative array AA."
  (cond
   ((hash-table-p aa)
    (asca-dump-alist (asca-hash-table->alist aa) "*dump hash-table*"))

   ((asca-alistp aa)
    (asca-dump-alist aa "*dump alist*"))

   ((plistp aa)
    (asca-dump-alist (asca-list->alist aa) "*dump plist*"))))

(defun asca-pure-plist-p (xs)
  "Return t if XS is a plist and all the keys are atomic, nil otherwise."
  (and (plistp xs)
       (cl-every #'collection-atomicp
                 (map-keys xs))))

(defun asca-pure-alist-p (xs)
  "Return t if XS is a list and all the contained items are lists,
nil otherwise."
  (and (consp xs)
       (cl-every #'consp xs)))

(defun asca-pure-hash-table-p (xs)
  "Return t if XS is a hash-table and all the keys are atomic, nil otherwise."
  (and (hash-table-p xs)
       (cl-every #'collection-atomicp
                 (map-keys xs))))

;;; get functions
;; (gethash KEY TABLE &optional DFLT)
;; (plist-get PLIST PROP &optional PREDICATE)            ; PREDICATE defaults to eq
;; (alist-get KEY ALIST &optional DEFAULT REMOVE TESTFN) ; TESTFN default to eq
;; (plist-member PLIST PROP &optional PREDICATE)         ;

;;; put functions
;; (puthash KEY VALUE TABLE)
;; (add-to-list LIST-VAR ELEMENT &optional APPEND COMPARE-FN)
;; (plist-put PLIST PROP VAL &optional PREDICATE)

;;; remove functions
;; (remhash KEY TABLE)

;;; Function API -- these may need to be macros.
;; (defun asca-get (assoc-array key &optional default predicate) <BODY>)
;; (defun asca-put (assoc-array key value &optional predicate) <BODY>)
;; (defun asca-rem (assoc-array key &optional predicate) <BODY>

(cl-defun asca-rem (assoc-array key &optional (predicate #'equal))
  (when-let ((remove-fnc
              (cond
               ((hash-table-p assoc-array) #'asca--hash-table-rem)
               ((asca-alistp assoc-array) #'asca--alist-rem)
               ((plistp assoc-array) #'asca--plist-rem))))
    (funcall remove-fnc assoc-array key predicate)))

;; Private

(cl-defun asca--plist-rem (plist key &optional (pred #'equal))
  (if-let* (((member key (map-keys plist)))
            (idx (-find-index (lambda (x) (funcall pred x key)) plist))
            ((delete key plist))
            (next-item (nth idx plist)))
      (delete next-item plist)))

(cl-defun asca--alist-rem (alist key &optional (pred 'equal))
  "Remove the KEY value pair from ALIST, if found.

ALIST must be a place that can be modified. This is a destructive
function."
  (cl-delete-if
   (lambda (x)
     (funcall pred key (car x)))
   alist))

(cl-defun asca--hash-table-rem (hash-table key &optional (_ nil))
  "Remove the KEY value pair from HASH-TABLE, if found.

HASH-TABLE must be a place that can be modified. This is a destructive function.
  The third parameter is disposable. It exists for API compatibility."
  (remhash key hash-table))


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

(provide 'assoc-array)

;; Local Variables:
;; read-symbol-shorthands: (("asca-" . "assoc-array-"))
;; End:

;;; assoc-array.el ends here
