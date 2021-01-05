;;; lein-project.el --- Provides assistance in creating Clojure test modules/defns

;; Author: Mark W. Naylor
;; file:  lein-project.el
;; date:  2020-Dec-29

;; Version: 0.9
;; Package-Requires: ((clojure-mode "20201126.1558") (clj-refactor "20200831.1244"))
;; Keywords: languages, clojure, lisp, cider
;; License: 3 Clause BSD

;; Commentary:
;; Values and functions neeed to create test modules and functions from Clojure source files.

;;; Code:

(defvar defn-re "^[^;]*([ \t]*defn[^-]" "Identifier for the defn sexp.")
(defvar deftest-re "^[^;]*([ \t]*deftest" "Identifier for the deftest sexp.")

(defvar ns-frag1 "(ns %s" "Start of the ns sexp.")
(defvar ns-frag2 "(:require [clojure.test :refer :all]" "Add the clojure.test requirement.")
(defvar ns-frag3 "[%s :refer :all]))" "Bind in definitions to be tested, close the ns sexp.")

(defvar defn-frag1 "(deftest %s []" "Start of the deftest sexp.")
(defvar defn-frag2 "\"Test for defn `%s'.\"" "Document string for the test")
(defvar defn-frag3 "(testing" "Beginning of a test hierarchy.")
(defvar defn-frag4 "\"FIXME, I fail.\"" "Message reported upon failure.")
(defvar defn-frag5 "(is (= 0 1))))" "Default failing test, close the deftest sexpr.")

(defvar test-suffix "-test" "String fragment that is part of Clojure deftest names.")

;;  clojure-expected-ns-function
(defun namespace-id ()
  "Returns the namespace identifier, nil on failure."
  (or (clojure-find-ns) (clojure-expected-ns)))


(defun callable-id (regexp &optional get-first?)
  "Returns a Clojure callable identifier, nil on failure. The
kind of identifier will be determined by the supplied regular
expression."
  ;; If the defn sexp has been commented out, make a recursive call to
  ;; find the next one.
  (when (and (booleanp get-first?) get-first?)
    (goto-char 1))
  (when  (search-forward-regexp regexp nil t)
    (if (non-comment-line?)
        (substring-no-properties (s-trim (cljr--extract-sexp)))
      (callable-id))))


(defun defn-id (&optional get-first?)
  "Returns a Clojure defn identifier, nil on failure."
  ;; If the defn sexp has been commented out, make a recursive call to
  ;; find the next one.
  (callable-id defn-re get-first?))


(defun deftest-id (&optional get-first?)
  "Returns a Clojure deftest identifier, nil on failure. deftest
is part of the clojure.test namespace."
  ;; If the deftest sexp has been commented out, make a recursive call to
  ;; find the next one.
  (callable-id deftest-re get-first?))


;; Keep this for the edge case where a defn is defined twice.
(defun uniques (xs)
  "Return only the unique items of a collection."
  (cl-labels (
    (_uniques (acc xs)
     ;; Implement with inner tail recursive version.
      (let*
          ((head (first xs))
           (tail (cdr xs)))
        (cond
         ((null xs) acc)
         ((member head acc) (_uniques acc tail))
         (:else (_uniques (append acc (list head)) tail))))))

    (_uniques () xs)))


(defun callable-ids (locator)
  "Returns a collection of all the callable identifiers. Possibly
empty. The `locator' function determines the kind of definition
to be found."
  (save-excursion
    (when-let (id (funcall locator t))
      (cl-labels
          ((_callable-ids ()
            ;; Recursive definition hides implementation.
             (let* ((id (funcall locator)))
               (cond
                ((null id) ())
                (:else (cons id (_callable-ids)))))))

        (uniques (cons id (_callable-ids)))))))


(defun defn-ids ()
  "Return a collection, possible empty, of defn identifiers."
  (callable-ids #'defn-id))


(defun deftest-ids ()
  "Return a collection, possible empty, of deftest identifiers."
  (callable-ids #'deftest-id))


(defun test-name (identifier)
  "Append `test-suffix' to the identifer, for use in the test module."
  (concat identifier test-suffix))


(defun define-ns (ns-name)
  "Return a collection of string fragments that comprise a
Clojure namespace definition."
  (list
    (format ns-frag1 (test-name ns-name))
    ns-frag2
    (format ns-frag3 ns-name)))


(defun define-defn (name)
  "Return a collection of string fragments that comprise a
Clojure defn definition."
  (list
   nil
   nil
   (format defn-frag1 (test-name name))
   (format defn-frag2 name)
   defn-frag3
   defn-frag4
   defn-frag5))


(defun insert-sexp (position sexp-frags)
  "Move the point to the position. Insert the sexp fragment. Format/indent appropriately."
  (let* ((head (first sexp-frags))
         (tail (rest sexp-frags)))
    (save-excursion
      (goto-char position)
      (insert (or head ""))
      (mapc (lambda (x) (newline) (insert (or x "")) (indent-for-tab-command))
            tail))))


(defun needs-tests (functions tests)
  "Return a collection of test names that are not currently
  defined in test module."
  (let* ((base-defns (mapcar #'(lambda (x) (string-remove-suffix test-suffix x)) tests)))
    (-filter #'(lambda (x) (not (member x base-defns))) functions)))


(defun insert-defn (xs)
  "Explicitly defined helper function, used to avoid problems
debugging the top level calling function when it was implemented
with a lambda. Insertions take place at the end of the buffer."
  (insert-sexp (point-max) xs))


(defun lein-test-file ()
  "Create/open Clojure test module. Works in the Leiningen
project environment."
  (interactive)
  (let*  ((current-file (buffer-file-name))
          (test-dir (s-replace "src" "test" default-directory))
          (test-file (s-replace
                      ".clj"
                      "_test.clj"
                      (s-replace "src" "test" current-file)))
          (new-namespace-id (namespace-id))
          (new-defn-ids (defn-ids))
          )

    (when (not (file-exists-p test-dir))
      (make-directory test-dir t))

    (if (file-exists-p test-file)

        ;; TODO: move the "THEN" portion to a separate function.
        ;; Improves readability; allows testing the fragment separate
        ;; from the top level function.
        (progn
          (find-file test-file)
          (save-excursion
            ;; Add the namespace definition if there isn't one already.
            (unless (clojure-find-ns)
              (first-content-line)
              (newline)
              (insert-sexp (point) (define-ns new-namespace-id))
              (newline))

            ;; Add in any missing deftests.
            (let ((existing-deftests (deftest-ids)))
              (mapc #'insert-defn
                    (mapcar #'define-defn
                            (needs-tests new-defn-ids existing-deftests))))))

      ;; TODO: Move the else portion to a separate function. Improves
      ;; readability; allows testing the fragment separate from the top
      ;; level function.

      ;; Shadow to keep the clr-refactor package from adding a
      ;; namepace definition to the new file.
      (let* ((cljr-add-ns-to-blank-clj-files nil))
        (find-file test-file)

        ;; Create the namespace for the new test module.
        (insert-sexp (point-min) (define-ns new-namespace-id))
        (move-beginning-of-line 1)
        (newline 2)

        ;; lambda in place of the explicitly defined `insert-defn'
        ;; caused problems when instrumenting with `edebug-defun'.
        (mapc #'insert-defn
              (mapcar #'define-defn new-defn-ids))

        (mwn/new-source-header)))) )

(provide 'lein-project)



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

;;; lein-project.el ends here
