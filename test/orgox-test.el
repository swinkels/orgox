;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'orgox)

(defvar test-directory (file-name-concat default-directory "test"))

(ert-deftest test-extract-date()
  (should (string= (extract-date "20250111.org") "2025-01-11"))

  ;; the buffer name without extension should consist of 8 digits: if you pass
  ;; anything else, it will return a string that indicates an error
  (should (string= (extract-date "abcdefgh.org") "Unable to extract date"))
  (should (string= (extract-date "*Messages*") "Unable to extract date")))

(ert-deftest test-orgox-current-buffer-to-ox-hugo()
  (let ((org-file (file-name-concat test-directory "20250119.org"))
        (expected-ox-hugo-org-file (file-name-concat test-directory "20250119.ox-hugo.org"))
        (ox-hugo-org-file (file-name-concat temporary-file-directory "20250119.ox-hugo.org")))

    ;; delete any pre-existing output file - this is a very poor-mans solution
    ;; to a teardown phase
    (when (file-exists-p ox-hugo-org-file)
      (delete-file ox-hugo-org-file))

    (with-current-buffer (find-file-noselect org-file)
      (let ((orgox-content-org-directory temporary-file-directory))
        (orgox-current-buffer-to-ox-hugo)))

    (should (file-readable-p ox-hugo-org-file))
    (should (string= (f-read expected-ox-hugo-org-file) (f-read ox-hugo-org-file)))))

;;; orgox-test.el ends here
