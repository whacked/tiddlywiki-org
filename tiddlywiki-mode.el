;; TODO
;; method for creating new tid file directly from emacs = generate .tid and add header stuff

(defvar tiddlywiki-org-mode-mimetype "text/org")

(defun tiddlywiki-timestamp ()
  (format-time-string "%Y%m%d%H%M%S%3N"))

(defun tiddlywiki-org-mode-tiddler-preamble (title)
  "returns a default preamble section for org-mode content type"
  (format "created: %s
modified: %s
tags: 
title: %s
type: %s"
          (tiddlywiki-timestamp)
          (tiddlywiki-timestamp)
          title
          tiddlywiki-org-mode-mimetype))

(defun tiddlywiki-parse-tid-file ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((prop-list nil)
          (content nil)
          (in-header t))
      (while in-header
        (if (looking-at "^\\([^\s]+\\):[[:space:]]+?\\(.+\\)$")
            ;; in header
            (setq prop-list (plist-put prop-list (intern (match-string 1))
                                       (match-string 2)))
          ;; end read header
          (setq in-header nil))
        (forward-line))
      (setq prop-list (plist-put prop-list :header-end-point (point)))
      (setq prop-list (plist-put prop-list :header-line-count (- (line-number-at-pos) 2)))
      (setq prop-list (plist-put prop-list :content (buffer-substring (point) (point-max))))
      prop-list)))

(defun tiddlywiki-narrow-file ()
  (interactive)
  (let ((info (tiddlywiki-parse-tid-file)))
    
    (if (> 1 (plist-get info :header-line-count))
        ;; HACK WARNING
        ;; *** if we assume wrong here, may clobber the file.***
        ;; there is no preamble/metadata section. Assume this is a new file,
        ;; and create the preamble.
        (progn
          (message "No preamble found, creating one...")
          (insert (tiddlywiki-org-mode-tiddler-preamble (file-name-base (buffer-name)))
                  "\n\n")
          ;; re-set the content type
          (setq info (plist-put info 'type tiddlywiki-org-mode-mimetype)))

      ;; assume existing tiddler with correct header information
      (goto-line (+ (plist-get info :header-line-count) 2)))
    
    (narrow-to-region (point) (point-max))

    ;; edit mode dispatch
    (let ((ftype (plist-get info 'type)))
      (cond ((string= ftype tiddlywiki-org-mode-mimetype)
             (message "org-mode file")
             (org-mode))
            ((string= ftype "text/x-markdown")
             (message "markdown file")
             (markdown-mode))
            ((string= ftype "application/javascript")
             (message "javascript source")
             (js2-mode))
            ((string= ftype "application/js")
             (message "executable javascript")
             (js2-mode))
            ((string= ftype "application/json")
             (message "json source")
             (javascript-mode))
            ((string= ftype "text/css")
             (message "css source")
             (css-mode))
            ((string= ftype "text/html")
             (message "html source")
             (html-mode))
            ((string= ftype "text/vnd.tiddlywiki")
             (message "tiddlywiki WikiText")
             ;; FIXME fixme1
             (markdown-mode))
            (t
             (message (concat "unhandled mode: " type)))))))

(defun tiddlywiki-widen-file ()
  (interactive)
  (widen)
  (text-mode)
  (recenter-top-bottom))

(defun tiddlywiki-set-header-read-only ()
  (interactive)
  (let ((info (tiddlywiki-parse-tid-file))
        (modified (buffer-modified-p)))
    ;; prevent front of first header char still being editable
    (put-text-property 1 2 'front-sticky t)
    (put-text-property 1 2 'rear-nonsticky nil)
    ;; prevent front of first non-header being uneditable
    (put-text-property (1- (plist-get info :header-end-point))
                       (plist-get info :header-end-point)
                       'rear-nonsticky t)
    (add-text-properties (point-min) (plist-get info :header-end-point)
                         '(read-only t face warning))
    (set-buffer-modified-p modified)))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Read-Only-Buffers.html#Read-Only-Buffers
(defun tiddlywiki-unset-header-read-only ()
  (interactive)
  (let ((info (tiddlywiki-parse-tid-file))
        (cur-inhibit-read-only inhibit-read-only)
        (modified (buffer-modified-p)))
    (setq inhibit-read-only t)
    (remove-text-properties (point-min) (plist-get info :header-end-point) '(read-only t face warning))
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
;; (add-to-list 'auto-mode-alist '("\\.org.tid\\'" . tiddlywiki-org-mode))

(defun tiddlywiki-update-modified-time ()
  (when (string= "tid" (file-name-extension (buffer-file-name)))
    (tiddlywiki-unset-header-read-only)
    (save-excursion
      (beginning-of-buffer)
      (search-forward "modified: ")
      (kill-line)
      (insert (tiddlywiki-timestamp)))
    (tiddlywiki-set-header-read-only)))

(add-hook 'before-save-hook 'tiddlywiki-update-modified-time)
;; (add-hook 'org-mode-hook
;;           (lambda () 
;;             (add-hook 'before-save-hook 'tiddlywiki-update-modified-time nil 'make-it-local)))

(define-derived-mode tiddlywiki-mode
  fundamental-mode "TiddlyWiki"
  "TiddlyWiki interaction mode"
  (progn
    (tiddlywiki-set-header-read-only)
    (tiddlywiki-narrow-file)
    ;; TODO: look into reapplying narrow file after auto revert
    ;; ref http://www.gnu.org/software/emacs/manual/html_node/elisp/Reverting.html
    (auto-revert-mode)))
(add-to-list 'auto-mode-alist '("\\.tid\\'" . tiddlywiki-mode))



