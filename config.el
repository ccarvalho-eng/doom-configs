;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; Basic Configuration

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;;;; UI Configuration

;; Italic comments and documentation
(custom-set-faces!
  `(font-lock-comment-face :slant italic)
  `(font-lock-doc-face :slant italic))

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Default frame size
(add-to-list 'default-frame-alist '(width . 185))
(add-to-list 'default-frame-alist '(height . 65))

;;;; Editor Configuration

;; Text Wrapping
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(global-visual-line-mode 1)

;; Misc editor settings
(setq auto-save-timeout 15
      make-backup-files nil
      create-lockfiles nil)

(global-auto-revert-mode t)
(delete-selection-mode t)
(global-subword-mode t)

;;;; Project Management

(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Projects")
        projectile-indexing-method 'hybrid
        projectile-enable-caching t))

;;;; LSP Configuration

(use-package! lsp-mode
  :ensure t
  :hook ((elixir-ts-mode . lsp)
         (web-mode . lsp)
         (typescript-mode . lsp))
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

(defun elixir-mix-command (command)
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (concat "mix " command))))

(defun elixir-mix-credo () (interactive) (elixir-mix-command "credo"))
(defun elixir-mix-dialyzer () (interactive) (elixir-mix-command "dialyzer"))
(defun elixir-mix-deps-compile () (interactive) (elixir-mix-command "deps.compile"))
(defun elixir-mix-deps-get () (interactive) (elixir-mix-command "deps.get"))
(defun elixir-mix-ecto-create () (interactive) (elixir-mix-command "ecto.create"))
(defun elixir-mix-ecto-migrate () (interactive) (elixir-mix-command "ecto.migrate"))
(defun elixir-mix-ecto-rollback () (interactive) (elixir-mix-command "ecto.rollback"))

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
(setq org-directory my-org-directory
      org-default-notes-file (concat org-directory "notes.org"))

;; Journal configuration
(use-package! org-journal
  :config
  (setq org-journal-dir my-journal-directory
        org-journal-date-format "%a, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'daily))

;; Agenda configuration
(setq org-agenda-include-diary t
      org-agenda-start-on-weekday nil  ; Start agenda on current day
      org-agenda-span 14               ; Show two weeks in agenda
      org-agenda-start-day "-1d")      ; Start agenda from yesterday

;; Function to find .org files recursively
(defun find-org-files-recursively (directory)
  "Find all .org files recursively within DIRECTORY."
  (directory-files-recursively directory "\\.org$"))

;; Function to update org-agenda-files
(defun update-org-agenda-files ()
  "Update org-agenda-files with all .org files in org and journal directories."
  (interactive)
  (setq org-agenda-files
        (append
         (find-org-files-recursively my-org-directory)
         (find-org-files-recursively my-journal-directory))))

;; Initialize org-agenda-files
(update-org-agenda-files)

;; Org-habit configuration
(after! org (add-to-list 'org-modules 'org-habit))
(setq org-habit-graph-column 50         ;; Adjust column for graph display
      org-habit-preceding-days 21       ;; Number of days before today to show in graph
      org-habit-following-days 7        ;; Number of future days to show in graph
      org-habit-show-all-today t)       ;; Ensure all habits scheduled for today are shown

(map! :map org-agenda-mode-map
      :leader
      :desc "Toggle habits" "h t" #'org-habit-stats-view-habit-at-point
      :desc "Toggle habits in agenda" "h T" #'org-habit-stats-view-habit-at-point-agenda)

;; Org UI customization
(custom-set-faces!
  '(org-level-1 :height 1.2 :weight bold)
  '(org-level-2 :height 1.1 :weight semi-bold)
  '(org-level-3 :height 1.05 :weight normal))

;; Org clock and logging configuration
(setq org-clock-into-drawer "LOGBOOK"
      org-log-done 'time
      org-log-into-drawer t
      org-log-reschedule 'note
      org-log-redeadline 'note
      org-log-state-notes-into-drawer t)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree org-default-notes-file)
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Org babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

;; Org export settings
(setq org-export-with-toc t
      org-export-with-author t
      org-export-with-creator t
      org-export-with-email t)

;; Enable org-indent-mode and visual-line-mode by default
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Export org table to CSV
(defun my/org-table-export-csv ()
  (interactive)
  (org-table-export (read-file-name "Export table to CSV file: ") "orgtbl-to-csv"))

(map! :map org-mode-map
      :leader
      :desc "Export table to CSV"
      "m T c" #'my/org-table-export-csv)

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
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package! yaml-mode
  :mode "\\.ya?ml\\'")

;;;; Keybindings

;; Multiple cursors keybindings
(map! :leader
      (:prefix "m"
       :desc "Edit lines" "l" #'mc/edit-lines
       :desc "Add cursor to all" "a" #'mc/mark-all-like-this
       :desc "Mark all in region" "r" #'mc/mark-all-in-region))

;; Org-roam-ui keybinding
(map! :leader
      :desc "Open Org Roam UI"
      "n r u" #'org-roam-ui-open)

;;;; Language-specific configurations

;;;; Gleam Configuration
(use-package! gleam-ts-mode
  :config
  ;; setup formatter to be used by `SPC c f`
  (after! apheleia
    (setf (alist-get 'gleam-ts-mode apheleia-mode-alist) 'gleam)
    (setf (alist-get 'gleam apheleia-formatters) '("gleam" "format" "--stdin"))))

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    ;; compile the treesit grammar file the first time
    (gleam-ts-install-grammar)))

;; JavaScript/TypeScript
(after! typescript-mode
  (setq typescript-indent-level 2))

;; Web-mode
(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;;;; Completion and Syntax Checking

;; Company (autocompletion)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

;; Flycheck (syntax checking)
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-idle-change-delay 0.1))

;; Which-key (key binding hints)
(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05))

;;;; File Management

;; Dired (file management)
(after! dired
  (setq dired-dwim-target t))

;;;; Performance tweaks

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;;; AI Assistance

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
  (setq copilot-indent-offset-alist
        '((prog-mode . 2)
          (org-mode . 2)
          (text-mode . 2)
          (closure-mode . 2)
          (emacs-lisp-mode . 2))))

;;;; LilyPond Configuration

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

;;;; GitHub Integration
(use-package! github-browse-file
  :config
  (defun browse-file-at-line ()
    "Browse the current file on GitHub including the current line."
    (interactive)
    (let ((github-browse-file-show-line-at-point t))
      (github-browse-file))))

(map! :leader
      :desc "Browse file at current line on GitHub"
      "g h" #'browse-file-at-line)

