;;; orgox-test.el --- Tests for orgox

(require 'f)
(require 'orgox)

(defvar test-directory (file-name-concat default-directory "test"))

(defun verify-error-string (buffer-name error-string)
  (let ((expected-error-string
         (format "Unable to extract date elements from \"%s\"" buffer-name)))
    (should (string= expected-error-string error-string))))

(ert-deftest test-extract-date()
  (should (string= (extract-date "20250111.org") "2025-01-11"))

  ;; the buffer name without extension should consist of 8 digits: if you pass
  ;; anything else, it will signal an error

  (dolist (buffer-name (list "abcdefgh.org" "*Messages*"))
    (let ((err  (should-error (extract-date buffer-name) :type 'error)))
      (verify-error-string buffer-name (error-message-string err)))))

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
