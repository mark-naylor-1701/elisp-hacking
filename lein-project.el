;;; lein-project --- Provides assistance in creating Clojure test modules/defns
;; author: Mark W. Naylor
;; file:  lein-project.el
;; date:  2020-Dec-29

;;; Code:


;; (ns org.mark-naylor-1701.grid)


;; (defn foo
;;   "I don't do a whole lot."
;;   ([]
;;    (println "Hello, World!"))
;;   ([x]
;;    (println "Hello, World!" x))
;;   ([x & args]
;;    (println "Hello, World!" x  args)))

;; (defn bar [])

;; (defn foobar
;; [])

;; (defn baz[])

;; (defn bar [])

;; (defn baz	[])


;; (defvar namespace-re "([:space:]*ns")
;; (defvar defn-re "([:space:]*defn ")
;; (defvar remainder-re "[[:space:])[]")

(defvar namespace-re "([\t ]*ns[\t ]+.")
(defvar defn-re "([\t ]*defn[\t ]+.")
(defvar remainder-re "[\t )[]")


(setq namespace-re "([:space:]*ns\\>")
(setq defn-re "([:space:]*defn[\t $]")
(setq remainder-re "[[:space:])[]")


;; (search-forward-regexp namespace-re nil t)
;; (search-forward-regexp defn-re nil t)


(defun identifier (re)
  "Finds the defining key word in a Clojure s-exp, starting with
  the current point. Returns either the identifier or nil."
  (when (search-forward-regexp re nil t)
    (forward-word)
    (backward-word)
    (first
     (s-split
      remainder-re
      (buffer-substring-no-properties
       (point)
       (line-end-position))))))


;;  clojure-expected-ns-function
(defun namespace-id ()
  "Returns the namespace identifier, nil on failure."
  (save-excursion
    (goto-char 1)
    (identifier namespace-re)))

(defun defn-id (&rest args)
  "Returns a defn identifier, nil on failure."
  (when (and (booleanp (first args)) (first args))
    (goto-char 1))
  (identifier defn-re))


(defun uniques (xs)
  "Return only the unique items of a collection."
  (cl-labels (
              (uniques (acc xs)
                       (let*
                           ((head (first xs))
                            (tail (cdr xs)))
                         (cond
                          ((null xs) acc)
                          ((member head acc) (uniques acc tail))
                          (:else (uniques (append acc (list head)) tail))))))

    (uniques () xs)))

(defun defn-ids ()
  "Returns a collection of all the defn identifiers. Possibly empty."
  (save-excursion
    (when-let (id (defn-id t))
      (list id)

      (cl-labels
          ((defn-ids ()
             (let* ((id (defn-id)))
               (cond
                ((null id) ())
                (:else (cons id (defn-ids)))))))

        (uniques (cons id (defn-ids)))))))


(defun test-name (identifier)
  "Append \"-test\" to the identifer, for use in the test module."
  (concat identifier "-test"))

;; Probably going to drop this one.
(defun test-names (identifiers)
  "Convert a collection of identifiers to their companion test names."
  (mapcar #'test-name identifiers))


;; (ns org.mark-naylor-1701.grid-test
;;   (:require [clojure.test :refer :all]
;;             [org.mark-naylor-1701.grid :refer :all]))

(setq ns-frag1 "(ns %s")
(setq ns-frag2 "(:require [clojure.test :refer :all]")
(setq ns-frag3 "[%s :refer :all]))")

(defun define-ns (ns-name)
  "docstring"

  (list
    (format ns-frag1 (test-name ns-name))
    ns-frag2
    (format ns-frag3 ns-name)))


;; (defun insert-namespace (ns-name)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (insert (define-ns ns-name))
;;     (indent-for-tab-command)
;;     (newline 2)))



(setq defn-frag1 "(deftest %s []")
(setq defn-frag2 "\"Test for defn `%s'.\"")
(setq defn-frag3 "(testing")
(setq defn-frag4 "\"FIXME, I fail.\"")
(setq defn-frag5 "(is (= 0 1))))")


(defun define-defn (defn-name)
  (list
   nil
   nil
   (format defn-frag1 (test-name defn-name))
   (format defn-frag2 defn-name)
   defn-frag3
   defn-frag4
   defn-frag5))

(defun insert-sexp (pos defn-frags)
  (let* ((head (first defn-frags))
         (tail (rest defn-frags)))
    (save-excursion
      (goto-char pos)
      (insert (or head ""))
      (mapc (lambda (x) (newline) (insert (or x "")) (indent-for-tab-command))
            tail)
      )
    ))

(defn-ids)




(defun insert-defn (defn-name)
  (save-excursion
    (goto-char (point-max))
    (newline 2)
    (let* ((start (point)))
      (insert (define-defn defn-name))
      (push-mark)
      (goto-char start)
      (indent-for-tab-command)
      (pop-mark))))

(defun insert-ns (ns-name)
  "docstring"
  (insert-sexp (point-min) (define-ns  ns-name))
  )
;;------------------------------------------------------------------------------

(defun insert-defn (xs)
  "docstring"
  (insert-sexp (point-max) xs)
  )

(defun lein-test-file ()
  "Create/open Clojure test module. Works in the Leiningen
project environment."
  (interactive)
  (let*  ((this-file (buffer-file-name))
          (test-dir (s-replace "src" "test" default-directory))
          (test-file (s-replace
                      "."
                      "_test."
                      (s-replace "src" "test" this-file)))
          (new-namespace-id (or (namespace-id) (clojure-find-ns) (clojure-expected-ns)))
          (new-defn-ids (defn-ids))
          )

    (when (not (file-exists-p test-dir))
      (make-directory test-dir t))

    (if (file-exists-p test-file)
        (find-file-read-only test-file)
      ;; Shadow to keep the clr-refactor package from adding a
      ;; namepace definition to the new file.
      (let* ((cljr-add-ns-to-blank-clj-files nil))

        (find-file test-file)
        ;;(delete-buffer)

        (insert-sexp (point-min) (define-ns new-namespace-id))

        (mapc #'insert-defn
              (mapcar #'define-defn new-defn-ids))

        (mwn/new-source-header)
        (newline)

        )


      )))








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
