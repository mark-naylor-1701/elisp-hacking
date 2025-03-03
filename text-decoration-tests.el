;; -*- lexical-binding: t; -*-

;; author: Mark W. Naylor
;; file:  text-decoration-texts.el
;; date:  2021-Jul-15

(ert-deftest ungreekify-char-test ()
  (should (equal (text-decoration:ungreekify-char ?ι) ?i))
  (should (equal (text-decoration:ungreekify-char ?L) ?L)))

(ert-deftest ungreekify-string-test ()
  (should (equal (text-decoration:ungreekify-string "αpplε") "apple"))
  (should (equal (text-decoration:ungreekify-string "pie") "pie")))


;; (defun fixture (body)
;;   (unwind-protect
;;       (progn
;;         (setup)
;;         (funcall body)
;;         (teardown))))

(defvar greek-text "Nοw ιs thε tιmε fοr αll gοοd mεn tο cοmε tο thε αιd οf thειr cουntry.")
(defvar latin-text "Now is the time for all good men to come to the aid of their country.")
(defvar test-buffer-name "my-test-buffer")

(defun create-test-buffer (buffer-text)
  (switch-to-buffer test-buffer-name)
  (insert buffer-text)
  (goto-char (point-min)))

(defun kill-test-buffer ()
  (kill-buffer test-buffer-name))

(defun ungreekify-buffer-fixture (body)
  (unwind-protect
      (progn
        (create-test-buffer greek-text)
        (funcall body)
        (kill-test-buffer))
    ()))

(ert-deftest ungreekify-buffer-test ()
  (ungreekify-buffer-fixture
   (lambda ()
     (let* ((_ (ungreekify-buffer))
            (actual (buffer-substring (point-min) (point-max))))
       (should (equal actual latin-text))))))

(defvar wood-text "How much wood would a woodchuck chuck if a woodchuck could chuck wood?")
(defvar wood-count 4)

(defun string-regions-fixture (body)
  (unwind-protect
      (progn
        (create-test-buffer wood-text)
        (funcall body)
        (kill-test-buffer))
    ()))

(defun find-wood ()
  (when (search-forward "wood" nil t)
    (backward-word)
    (set-mark (point))
    (forward-word)
    (region-bounds)))

(ert-deftest string-regions-test ()
  (string-regions-fixture
   (lambda ()
     (should (equal (length (string-regions #'find-wood)) wood-count)))))


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
