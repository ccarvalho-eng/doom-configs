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

;; Font
(setq doom-font (font-spec :size 11))

;; Italic comments and documentation
(custom-set-faces!
  `(font-lock-comment-face :slant italic)
  `(font-lock-doc-face :slant italic))

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

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

;; Set org-related directories
(setq org-directory my-org-directory)
(setq org-default-notes-file my-org-directory)

;; Journal configuration
(after! org-journal
  (setq org-journal-dir my-journal-directory
        org-journal-date-format "%a %e %b, %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'daily))

;; Agenda configuration
(setq org-agenda-include-diary t)

;; Set up org-agenda-files to include both org directory and journal directory
(setq org-agenda-files (list org-directory my-journal-directory))

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

;; LilyPond configuration
(defun run-lilypond-on-current-file ()
  "Run LilyPond on the current file and export it as a PDF."
  (interactive)
  (let* ((file (buffer-file-name))
         (output-dir (file-name-directory file))
         (base-name (file-name-base file))
         (output-file (expand-file-name (concat base-name ".pdf") output-dir))
         (command (format "lilypond -o \"%s\" \"%s\"" (file-name-sans-extension output-file) file))
         (compilation-buffer-name-function
          (lambda (mode) "*LilyPond Output*")))
    (compile command)
    (message "LilyPond command executed: %s" command)
    (unless (get-buffer "*LilyPond Output*")
      (pop-to-buffer "*LilyPond Output*"))))

(map! :leader
      :desc "Run LilyPond on current file"
      "t l" #'run-lilypond-on-current-file)
