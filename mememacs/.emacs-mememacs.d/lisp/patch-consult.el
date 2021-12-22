;;; redefine form consult.el
;; I want to add " " to the initial input here

(defun consult-completion-in-region (start end collection &optional predicate)
  "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer"
  (cl-letf* ((config (alist-get #'consult-completion-in-region consult--read-config))
             ;; Overwrite both the local and global value of `completion-styles', such that the
             ;; `completing-read' minibuffer sees the overwritten value in any case. This is
             ;; necessary if `completion-styles' is buffer-local.
             ;; NOTE: The completion-styles will be overwritten for recursive editing sessions!
             (cs (or (plist-get config :completion-styles) completion-styles))
             (completion-styles cs)
             ((default-value 'completion-styles) cs)
             (prompt (or (plist-get config :prompt) "Completion: "))
             (require-match (plist-get config :require-match))
             (preview-key (if (plist-member config :preview-key)
                              (plist-get config :preview-key)
                            consult-preview-key))
             (initial (buffer-substring-no-properties start end))
             (metadata (completion-metadata initial collection predicate))
             (threshold (or (plist-get config :cycle-threshold) (completion--cycle-threshold metadata)))
             (all (completion-all-completions initial collection predicate (length initial)))
             ;; Provide `:annotation-function' if `:company-docsig' is specified
             (completion-extra-properties
              (if-let (fun (and (not (plist-get completion-extra-properties :annotation-function))
                                (plist-get completion-extra-properties :company-docsig)))
                  `(:annotation-function
                    ,(lambda (cand)
                       (concat (propertize " " 'display '(space :align-to center))
                               (funcall fun cand)))
                    ,@completion-extra-properties)
                completion-extra-properties)))
    ;; error if `threshold' is t or the improper list `all' is too short
    (if (and threshold
	     (or (not (consp (ignore-errors (nthcdr threshold all))))
		 (and completion-cycling completion-all-sorted-completions)))
        (completion--in-region start end collection predicate)
      (let* ((limit (car (completion-boundaries initial collection predicate "")))
             (category (completion-metadata-get metadata 'category))
             (buffer (current-buffer))
             (completion
              (cond
               ((atom all) nil)
               ((and (consp all) (atom (cdr all)))
                (concat (substring initial 0 limit) (car all)))
               (t (car
                   (consult--with-preview
                       preview-key
                       ;; preview state
                       (consult--insertion-preview start end)
                       ;; transformation function
                       (if (eq category 'file)
                           (cond
                            ;; Transform absolute file names
                            ((file-name-absolute-p initial)
                             (lambda (_inp cand)
                               (substitute-in-file-name cand)))
                            ;; Ensure that ./ prefix is kept for the shell (#356)
                            ((string-match-p "\\`\\.\\.?/" initial)
                             (lambda (_inp cand)
                               (setq cand (file-relative-name (substitute-in-file-name cand)))
                               (if (string-match-p "\\`\\.\\.?/" cand) cand (concat "./" cand))))
                            ;; Simplify relative file names
                            (t
                             (lambda (_inp cand)
                               (file-relative-name (substitute-in-file-name cand)))))
                         (lambda (_inp cand) cand))
                       ;; candidate function
                       (apply-partially #'run-hook-with-args-until-success
                                        'consult--completion-candidate-hook)
                     (let ((enable-recursive-minibuffers t))
                       (if (eq category 'file)
                           ;; We use read-file-name, since many completion UIs make it nicer to
                           ;; navigate the file system this way; and we insert the initial text
                           ;; directly into the minibuffer to allow the user's completion
                           ;; styles to expand it as appropriate (particularly useful for the
                           ;; partial-completion and initials styles, which allow for very
                           ;; condensed path specification).
                           (consult--minibuffer-with-setup-hook
                               (lambda () (insert initial))
                             (read-file-name prompt nil initial require-match nil predicate))
                         (completing-read prompt
                                          ;; Evaluate completion table in the original buffer.
                                          ;; This is a reasonable thing to do and required
                                          ;; by some completion tables in particular by lsp-mode.
                                          ;; See https://github.com/minad/vertico/issues/61.
                                          (if (functionp collection)
                                              (lambda (&rest args)
                                                (with-current-buffer buffer
                                                  (apply collection args)))
                                            collection)
                                          predicate require-match (concat initial " "))))))))))
        (if completion
            (progn
              (delete-region start end)
              (insert (substring-no-properties completion))
              (when-let (exit (plist-get completion-extra-properties :exit-function))
                (funcall exit completion
                         ;; If completion is finished and cannot be further completed,
                         ;; return 'finished. Otherwise return 'exact.
                         (if (eq (try-completion completion collection predicate) t)
                             'finished 'exact)))
              t)
          (message "No completion")
          nil)))))




(provide 'patch-consult)
