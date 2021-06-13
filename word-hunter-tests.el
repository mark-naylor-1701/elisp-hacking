;; author: Mark W. Naylor
;; file:  word-hunter-tests.el
;; date:  2021-Jun-10

;; wrap-word-bounds test section ;;;;;;;
(ert-deftest wrap-word-bounds-test ()
  (should (equal
           (wrap-word-bounds "test")
           "\\<test\\>")))

;; find-and-mark test section ;;;;;;;;;;
(defvar matching-text
  "This buffer has target word\nmoney\nmoney\nmoney\nmoney\n")

(defvar non-matching-text
  "This buffer does not have the target word\n\n ")

(defvar test-buffer-name "--testing-buffer")

(defun create-test-buffer (buffer-text)
  (switch-to-buffer test-buffer-name)
  (insert buffer-text)
  (goto-char (point-min)))

(defun create-match-buffer ()
  (create-test-buffer matching-text))

(defun create-non-match-buffer ()
  (create-test-buffer non-matching-text))

(defun kill-test-buffer ()
  (kill-buffer test-buffer-name))

(defun matching-fixture (body)
  (unwind-protect
      (progn
        (create-match-buffer)
        (funcall body)
        (kill-test-buffer))))

(defun non-matching-fixture (body)
  (unwind-protect
      (progn
        (create-non-match-buffer)
        (funcall body)
        (kill-test-buffer))))

(ert-deftest find-and-mark-test ()
  (matching-fixture
   (lambda ()
     (should (equal
              (find-and-mark "money")
              t))))
  (matching-fixture
   (lambda ()
     (should (equal
              (find-and-mark "bu..er")
              t))))
  (non-matching-fixture
   (lambda ()
     (should (equal
              (find-and-mark "money")
              nil)))))

(ert-deftest transform-from-point-test ()
  (matching-fixture
   (lambda ()
     (should (equal
              (transform-from-point #'greekify-region "money")
              4))))
  (matching-fixture
   (lambda ()
     (should (equal
              (transform-from-point #'greekify-region "money" 2)
              2))))
  (non-matching-fixture
   (lambda ()
     (should (equal
              (transform-from-point #'greekify-region "money")
              0)))))
