(defun tiddlywiki-parse-tid-file ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((prop-list nil)
          (content nil)
          (in-header t))
      (while in-header
        (if (looking-at "^\\([^:]+\\):[[:space:]]+?\\(.+\\)$")
            ;; in header
            (setq prop-list (plist-put prop-list (intern (match-string 1))
                                       (match-string 2)))
          ;; end read header
          (setq in-header nil))
        (forward-line))
      (setq prop-list (plist-put prop-list :header-end (point)))
      (setq prop-list (plist-put prop-list :nheader (- (line-number-at-pos) 2)))
      (setq prop-list (plist-put prop-list :content (buffer-substring (point) (point-max))))
      prop-list)))

(defun tiddlywiki-narrow-file ()
  (interactive)
  (let ((info (tiddlywiki-parse-tid-file)))
    (goto-line (+ (plist-get info :nheader) 2))
    (narrow-to-region (point) (point-max))
    (let ((ftype (plist-get info 'type)))
      (cond ((string= ftype "text/org")
             (message "orgmode")
             (org-mode))
            (t
             (message "nothing"))))))

(defun tiddlywiki-widen-file ()
  (interactive)
  (widen)
  (text-mode)
  (recenter-top-bottom))

(defun tiddlywiki-set-header-read-only ()
  (let ((info (tiddlywiki-parse-tid-file))
        (modified (buffer-modified-p)))
    (add-text-properties (point-min) (plist-get info :header-end)
                         '(read-only t face warning))
    (set-buffer-modified-p modified)))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Read-Only-Buffers.html#Read-Only-Buffers
(defun tiddlywiki-unset-header-read-only ()
  (let ((info (tiddlywiki-parse-tid-file))
        (cur-inhibit-read-only inhibit-read-only)
        (modified (buffer-modified-p)))
    (setq inhibit-read-only t)
    (remove-text-properties (point-min) (plist-get info :header-end) '(read-only t face warning))
    (setq inhibit-read-only cur-inhibit-read-only)
    (set-buffer-modified-p modified)))

(defun tiddlywiki-org-mode-hook ()
  (tiddlywiki-set-header-read-only))

(define-derived-mode tiddlywiki-org-mode
  org-mode "TiddlyWiki-org"
  "TiddlyWiki+org interaction mode"
  (progn
    (tiddlywiki-set-header-read-only)
    (tiddlywiki-narrow-file)
    ))

(defun tiddlywiki-update-modified-time ()
  (when (string= "tid" (file-name-extension (buffer-file-name)))
    (tiddlywiki-unset-header-read-only)
    (save-excursion
      (beginning-of-buffer)
      (search-forward "modified: ")
      (kill-line)
      (insert (format-time-string "%Y%m%d%H%M%S%3N")))
    (tiddlywiki-set-header-read-only)))

(add-hook 'org-mode-hook
          (lambda () 
            (add-hook 'before-save-hook 'tiddlywiki-update-modified-time nil 'make-it-local)))

(add-to-list 'auto-mode-alist '("\\.org.tid\\'" . tiddlywiki-org-mode))

