;;; orgox-test.el --- Tests for orgox

(defun extract-date (buffer-name)
  (if (string-match-p "^20[[:digit:]]\\{6\\}$" (file-name-sans-extension buffer-name))
      (concat
       (substring buffer-name 0 4)
       "-"
       (substring buffer-name 4 6)
       "-"
       (substring buffer-name 6 8))
    "Unable to extract date"))

(ert-deftest test-extract-date()
  (should (string= (extract-date "20250111.org") "2025-01-11"))

  ;; the buffer name without extension should consist of 8 digits: if you pass
  ;; anything else, it will return a string that indicates an error
  (should (string= (extract-date "abcdefgh.org") "Unable to extract date"))
  (should (string= (extract-date "*Messages*") "Unable to extract date")))

;;; orgox-test.el ends here
