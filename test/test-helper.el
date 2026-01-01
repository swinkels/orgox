;;; test-helper.el --- Support code to test Orgox

(defmacro with-tmp-dir (var &rest body)
  "Create a temporary directory for execution of BODY.
This macro deletes the temporary when BODY has executed, even when it
signalled an error.  The path to the temporary directory is assigned to
VAR."
  ;; for examples on how to use this macro, have a look at the unit tests in
  ;; this file
  `(let* ((,var (f-join temporary-file-directory (make-temp-name (format "%s-" (symbol-name ',var))))))

     ;; safeguard that we will be using a new directory
     (unless (not (f-exists-p ,var))
       (error (format "Unable to create temporary directory %s: directory already exists" ,var)))

     (f-mkdir-full-path ,var)

     (unwind-protect
         ,@body
       (progn

         ;; safeguard that we will be deleting a temporary directory
         (unless (f-descendant-of-p ,var temporary-file-directory)
           (error (format
                   "Unable to delete temporary directory %s: directory is not a subdirectory of %s"
                   ,var temporary-file-directory)))

         (f-delete ,var t)))))

(defun get-buffer-content (buffer)
  "Return the string with the content of BUFFER without text properties."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

;; tests for support code

(ert-deftest test-delete-tmp-dir-after-successful-completion-of-body()
  ;; The temporary directory that macro `with-tmp-dir' creates, exists at the
  ;; start of the execution of its body. When that execution ended successfully,
  ;; the macro deletes that directory.
  (let ((created-tmp-dir nil))
    (with-tmp-dir orgox-tmp-dir
                  (progn
                    (should (f-directory-p orgox-tmp-dir))
                    (should (f-descendant-of-p orgox-tmp-dir temporary-file-directory))
                    (should (string-prefix-p "orgox-tmp-dir" (f-filename orgox-tmp-dir)))
                    (setq created-tmp-dir orgox-tmp-dir)))
    (should-not (f-exists-p created-tmp-dir))))

(ert-deftest test-deletes-tmp-dir-when-body-aborts-with-error()
  ;; When the body to macro `with-tmp-dir' signals an error and aborts its
  ;; execution, it deletes the temporary directory.
  (let ((err (should-error (with-tmp-dir orgox-tmp-dir (error orgox-tmp-dir)))))
    (let ((tmp-dir (error-message-string err)))
      (should (f-descendant-of-p tmp-dir temporary-file-directory))
      (should-not (f-exists-p tmp-dir)))))
