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
