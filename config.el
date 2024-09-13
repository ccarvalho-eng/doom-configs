;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; Basic Configuration

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;;;; Project Management
(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Projects")
        projectile-indexing-method 'hybrid
        projectile-enable-caching t))

;;;; UI Configuration

;; Theme
(setq doom-theme 'doom-solarized-dark)

;; Font
(setq doom-font (font-spec :size 11))

;; Italic comments and documentation
(custom-set-faces!
  `(font-lock-comment-face :slant italic)
  `(font-lock-doc-face :slant italic))

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Default frame size
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 70))

;;;; LSP Configuration

(use-package! lsp-mode
  :ensure t
  :hook (elixir-ts-mode . lsp)
  :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nextls" "--stdio"))
                    :multi-root t
                    :activation-fn (lsp-activate-on "elixir")
                    :server-id 'next-ls))
  ;; Ignore directories for file watchers
  (dolist (match '("[/\\\\].direnv$"
                   "[/\\\\]node_modules$"
                   "[/\\\\]deps"
                   "[/\\\\]priv"
                   "[/\\\\]build"
                   "[/\\\\]_build"))
    (add-to-list 'lsp-file-watch-ignored match)))

(use-package! lsp-ui
  :commands (lsp-ui-mode lsp-ui-imenu)
  :config
  (setq lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-webkit nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-kind-position 'left
        lsp-ui-sideline-code-actions-prefix "💡"
        lsp-ui-imenu-enable t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-kind-tooltip t))

(use-package! lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;;;; Elixir Configuration

(use-package! exunit)

;; Custom Elixir functions
(defun elixir-append-dbg()
  (interactive)
  (evil-append-line nil)
  (insert " |> dbg()")
  (evil-normal-state))

(defun elixir-mix-credo ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix credo")))

(defun elixir-mix-dialyzer ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix dialyzer")))

(defun elixir-mix-deps-compile ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix deps.compile")))

(defun elixir-mix-deps-get ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix deps.get")))

(defun elixir-mix-ecto-create ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.create")))

(defun elixir-mix-ecto-migrate ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.migrate")))

(defun elixir-mix-ecto-rollback ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix ecto.rollback")))

;; Elixir keybindings
(map! :mode elixir-ts-mode
      :leader
      :desc "Sort Lines" :nve "l" #'sort-lines
      :desc "iMenu" :nve "c/" #'lsp-ui-imenu
      :desc "dbg/1" :nve "cI" #'elixir-append-dbg
      :desc "mix credo" :nve "mc" #'elixir-mix-credo
      :desc "mix dialyzer" :nve "mdy" #'elixir-mix-dialyzer
      :desc "mix deps.compile" :nve "mDc" #'elixir-mix-deps-compile
      :desc "mix deps.get" :nve "mDg" #'elixir-mix-deps-get
      :desc "mix ecto.create" :nve "meC" #'elixir-mix-ecto-create
      :desc "mix ecto.migrate" :nve "meM" #'elixir-mix-ecto-migrate
      :desc "mix ecto.rollback" :nve "meR" #'elixir-mix-ecto-rollback)

;;;; Org Mode Configuration

;; Define directory paths
(defvar my-org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org-notes/"
  "The directory where I store my org files.")
(defvar my-journal-directory (concat my-org-directory "journal/")
  "The directory where I store my journal files.")
(defvar my-templates-directory (concat my-org-directory "templates/")
  "The directory where I store my templates.")

;; Set org-related directories
(setq org-directory my-org-directory)
(setq org-default-notes-file my-org-directory)
(setq org-roam-directory (concat my-org-directory "notes"))

;; Journal configuration
(after! org-journal
  (setq org-journal-dir my-journal-directory
        org-journal-date-format "%a %e %b, %Y"))

;; Agenda configuration
(setq org-agenda-include-diary t)

(defun find-org-files-recursively (directory)
  "Find all .org files recursively within DIRECTORY."
  (directory-files-recursively directory "\\.org$"))

(defun update-org-agenda-files ()
  "Update org-agenda-files with all .org files in org and journal directories."
  (interactive)
  (setq org-agenda-files
        (append
         (find-org-files-recursively my-org-directory))))

;; Initialize org-agenda-files
(update-org-agenda-files)

;; Automatically update org-agenda-files every 5 minutes
(run-with-timer 0 300 'update-org-agenda-files)

;; Journal functions
(defun org-journal-refresh-agenda-list ()
  "Refresh the org-agenda-files list and reload the agenda."
  (interactive)
  (update-org-agenda-files)
  (org-agenda-redo-all))

(defun org-journal-load-template (period)
  "Load the template for the given PERIOD."
  (let ((template-file (expand-file-name (concat (symbol-name period) ".org") my-templates-directory)))
    (when (file-exists-p template-file)
      (insert-file-contents template-file)
      (goto-char (point-max)))))

(after! org-journal
  (setq org-journal-dir my-journal-directory
        org-journal-date-format "%a %e %b, %Y"  ; Default format for daily entries
        org-journal-file-format "%Y-%m-%d.org"  ; Default format for daily entries
        org-journal-file-type 'daily))  ; Default to daily entries

(defun org-journal-create-entry (period)
  "Create a new journal entry for the given PERIOD."
  (let* ((config (cdr (assoc period
                             '((daily . ((dir . "daily/%Y/%Y-%m")
                                         (format . "%Y-%m-%d.org")
                                         (date-format . "%a %e %b, %Y")))
                               (weekly . ((dir . "weekly/%Y/%Y-%m")
                                          (format . "%Y-W%V.org")
                                          (date-format . "Week %V, %Y")))
                               (monthly . ((dir . "monthly/%Y")
                                           (format . "%Y-%m.org")
                                           (date-format . "%B %Y")))
                               (quarterly . ((dir . "quarterly/%Y")
                                             (format . "%Y-Q%q.org")
                                             (date-format . "Q%q %Y")))
                               (yearly . ((dir . "yearly")
                                          (format . "%Y.org")
                                          (date-format . "%Y")))))))
         (date (org-read-date nil nil ""))
         (time (org-time-string-to-time date))
         (dir (concat my-journal-directory
                      (format-time-string (alist-get 'dir config) time))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (let ((org-journal-dir dir)
          (org-journal-file-format (alist-get 'format config))
          (org-journal-date-format (alist-get 'date-format config))
          (org-journal-file-type period))
      (org-journal-new-entry t time)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* " nil t)
          (forward-line)
          (insert ":PROPERTIES:\n"
                  ":CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]" time) "\n"
                  ":+TAGS: " (symbol-name period) "\n"
                  ":END:\n\n")
          (let ((start-point (point)))
            (org-journal-load-template period)
            (goto-char start-point)
            (while (not (eobp))
              (cond
               ((looking-at "^\\* ")
                (replace-match "** "))
               ((looking-at "^\\*\\* ")
                (replace-match "*** "))
               ((looking-at "^\\*\\*\\* ")
                (replace-match "**** ")))
              (forward-line 1)))))
      (org-journal-refresh-agenda-list))))

;; Add a hook to refresh agenda files after saving an org file
(add-hook 'after-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-journal-refresh-agenda-list))))

;; Keybinding to manually refresh agenda files
(map! :leader
      :desc "Refresh org agenda files" "o a r" #'org-journal-refresh-agenda-list)

(defun org-journal-daily-entry ()
  "Create a new daily journal entry."
  (interactive)
  (org-journal-create-entry 'daily))

(defun org-journal-weekly-entry ()
  "Create a new weekly journal entry."
  (interactive)
  (org-journal-create-entry 'weekly))

(defun org-journal-monthly-entry ()
  "Create a new monthly journal entry."
  (interactive)
  (org-journal-create-entry 'monthly))

(defun org-journal-quarterly-entry ()
  "Create a new quarterly journal entry."
  (interactive)
  (org-journal-create-entry 'quarterly))

(defun org-journal-yearly-entry ()
  "Create a new yearly journal entry."
  (interactive)
  (org-journal-create-entry 'yearly))

;; Journal keybindings
(map! :leader
      :desc "Daily journal entry" "n j d" #'org-journal-daily-entry
      :desc "Weekly journal entry" "n j w" #'org-journal-weekly-entry
      :desc "Monthly journal entry" "n j m" #'org-journal-monthly-entry
      :desc "Quarterly journal entry" "n j q" #'org-journal-quarterly-entry
      :desc "Yearly journal entry" "n j y" #'org-journal-yearly-entry)

;; Org UI customization
(custom-set-faces!
  '(org-level-1 :height 1.0)
  '(org-level-2 :height 0.95)
  '(org-level-3 :height 0.9))

;; Org clock and logging configuration
(setq org-clock-into-drawer "LOGBOOK"
      org-log-done 'time
      org-log-into-drawer t
      org-log-reschedule 'note
      org-log-redeadline 'note
      org-log-state-notes-into-drawer t)

;;;; Text Wrapping

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(global-visual-line-mode 1)

;;;; MacOS Configuration

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;; Additional Packages

(use-package! imenu-list
  :commands imenu-list
  :config
  (setq imenu-list-position 'left))

(use-package! imenu-anywhere
  :commands imenu-anywhere)

(use-package! lsp-origami
  :after lsp-mode
  :config
  (setq lsp-enable-folding t)
  (add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable))

;;;; Miscellaneous

;; Multiple cursors keybindings
(map! :leader
      (:prefix "m"
       :desc "Edit lines" "l" #'mc/edit-lines
       :desc "Add cursor to all" "a" #'mc/mark-all-like-this
       :desc "Mark all in region" "r" #'mc/mark-all-in-region))

;; Org-roam-ui keybinding
(global-set-key (kbd "C-c r u") 'org-roam-ui-open)

;;; Company (autocompletion)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

;;; Flycheck (syntax checking)
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-idle-change-delay 0.1))

;;; Which-key (key binding hints)
(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05))

;;; Dired (file management)
(after! dired
  (setq dired-dwim-target t))

;;; Rainbow delimiters (colorize nested parentheses)
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Language-specific configurations

;; JavaScript/TypeScript
(after! typescript-mode
  (setq typescript-indent-level 2))

;; Web-mode
(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; YAML
(use-package! yaml-mode
  :mode "\\.ya?ml\\'")

;;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;; Miscellaneous
(setq auto-save-timeout 15
      make-backup-files nil
      create-lockfiles nil)

(global-auto-revert-mode t)
(delete-selection-mode t)
(global-subword-mode t)

;; Export org table to CSV
(defun my/org-table-export-csv ()
  (interactive)
  (org-table-export (read-file-name "Export table to CSV file: ") "orgtbl-to-csv"))

(map! :map org-mode-map
      :leader
      :desc "Export table to CSV"
      "m T c" #'my/org-table-export-csv)

;; Copilot configuration
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
  (add-to-list 'copilot-indentation-alist '(org-mode . 2))
  (add-to-list 'copilot-indentation-alist '(text-mode . 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode . 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2)))

