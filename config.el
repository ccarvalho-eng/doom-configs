;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

(use-package! projectile
  :config
  (setq projectile-project-search-path '("~/Projects")))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'frappe)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Emacs Configuration for LSP and Related Packages

;;; Package: lsp-mode
;; Configuration for Language Server Protocol support in Emacs
(use-package! lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l") ; Set prefix for lsp-command-keymap
  :config
  ;; Elixir LSP
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     (expand-file-name
                      "~/.elixir-ls/release/language_server.sh"))
                    :major-modes '(elixir-mode)
                    :priority -1
                    :server-id 'elixir-ls
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (let ((config `(:elixirLS
                                                        (:mixEnv "dev"
                                                         :dialyzerEnabled
                                                         :json-false))))
                                          (lsp--set-configuration config)))))))
;; lsp-treemacs integration
(use-package! lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; ;;; Package: lsp-ui
;; ;; Enhancements for lsp-mode with additional UI features
(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  ;; Configure UI options for lsp-ui
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
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix))

;;; Package: lsp-elixir
;; Elixir-specific LSP support
(use-package! lsp-elixir
  :defer t
  :hook (elixir-mode . lsp))

;; Folding configuration
(setq lsp-enable-folding t)
(use-package! lsp-origami)
(add-hook! 'lsp-after-open-hook #'lsp-origami-try-enable)

;;; Package: lsp-treemacs
;; Integrates LSP with Treemacs for displaying errors and symbols
(use-package! lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  ;; Enable sync mode for Treemacs with LSP
  (lsp-treemacs-sync-mode 1))

;;; LSP Mode - Additional Configuration
(after! lsp-mode
  ;; Add directories to be ignored by LSP file watchers
  (dolist (match '("[/\\\\].direnv$"
                   "[/\\\\]node_modules$"
                   "[/\\\\]deps"
                   "[/\\\\]priv"
                   "[/\\\\]build"
                   "[/\\\\]_build"))
    (add-to-list 'lsp-file-watch-ignored match)))

;;; Package: exunit
;; Emacs support for Elixir ExUnit
(use-package! exunit)

;;; UI Configuration
;; Set default font
(setq doom-font (font-spec :family "MonacoB2" :size 11))

;;; Custom Face Settings
;; Customize org-mode heading sizes
(custom-set-faces!
  '(org-level-1 :height 1.0)
  '(org-level-2 :height 0.95)
  '(org-level-3 :height 0.9))

;; Set faces for comments and documentation to italic
(custom-set-faces!
  `(font-lock-comment-face :slant italic)
  `(font-lock-doc-face :slant italic))

;;; Default Frame Size
;; Configure default frame dimensions
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 60))

;; Custom functions
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
(map! :mode elixir-mode
      :leader
      :desc "Sort Lines" :nve "l" #'sort-lines
      :desc "iMenu" :nve "c/" #'lsp-ui-imenu
      :desc "dbg/0" :nve "cI" #'elixir-append-dbg
      :desc "mix credo" :nve "mc" #'elixir-mix-credo
      :desc "mix dialyzer" :nve "mdy" #'elixir-mix-dialyzer
      :desc "mix deps.compile" :nve "mDc" #'elixir-mix-deps-compile
      :desc "mix deps.get" :nve "mDg" #'elixir-mix-deps-get
      :desc "mix ecto.create" :nve "meC" #'elixir-mix-ecto-create
      :desc "mix ecto.migrate" :nve "meM" #'elixir-mix-ecto-migrate
      :desc "mix ecto.rollback" :nve "meR" #'elixir-mix-ecto-rollback)

;; Multiple cursors keybindings
(map! :leader
      (:prefix "m"
       :desc "Edit lines" "l" #'mc/edit-lines
       :desc "Add cursor to all" "a" #'mc/mark-all-like-this
       :desc "Mark all in region" "r" #'mc/mark-all-in-region))

;;; Org configuration
;; Define a variable to store the directory path for org-mode files
(defvar my-org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org-notes/"
  "The directory where I store my org files.")

;; Define a variable to store the directory path for the journal files
(defvar my-journal-directory (concat my-org-directory "journal/")
  "The directory where I store my journal files.")

;; Define a variable to store the directory path for the templates
(defvar my-templates-directory (concat my-org-directory "templates/")
  "The directory where I store my templates.")

;; Set the date format for org-journal entries
(setq org-journal-date-format "%a %e %b, %Y")

;; Set the default org directory
(setq org-directory my-org-directory)

;; Set the org-journal directory to the journal directory
(after! org-journal
  (setq org-journal-dir my-journal-directory))

;; Set the default notes file to the journal directory
(setq org-default-notes-file my-org-directory)

;; Define a function to recursively find org files in a directory
(defun find-org-files-recursively (directory)
  "Find all .org files recursively within DIRECTORY."
  (directory-files-recursively directory "\\.org$"))

;; Set the agenda files to include both org and journal directories and subdirectories
(setq org-agenda-files
      (append
       (find-org-files-recursively my-org-directory)))

(setq org-roam-directory (concat my-org-directory "train-of-thought"))

;; Include org agenda diary
(setq org-agenda-include-diary t)

;; Update org-agenda-files with new journal entries
(defun org-journal-update-agenda-files ()
  "Update `org-agenda-files` to include all org files in the journal directory."
  (setq org-agenda-files
        (append
         (find-org-files-recursively my-org-directory))))

;; Reload the org agenda buffer
(defun org-journal-reload-agenda ()
  "Reload the org agenda buffer to reflect any changes."
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo))))

;; Create a daily journal entry and update agenda
(defun org-journal-daily-entry ()
  "Create a new daily journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "daily/"))
  (setq org-journal-date-format "%a %e %b, %Y")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-file-type 'daily)
  (org-journal-new-entry nil)
  (org-journal-load-template "daily")
  (org-journal-update-agenda-files)
  (org-journal-reload-agenda))

;; Create a weekly journal entry and update agenda
(defun org-journal-weekly-entry ()
  "Create a new weekly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "weekly/"))
  (setq org-journal-date-format "Week %V, %Y")
  (setq org-journal-file-format "%Y-W%V.org")
  (setq org-journal-file-type 'weekly)
  (org-journal-new-entry nil)
  (org-journal-load-template "weekly")
  (org-journal-update-agenda-files)
  (org-journal-reload-agenda))

;; Create a monthly journal entry and update agenda
(defun org-journal-monthly-entry ()
  "Create a new monthly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "monthly/"))
  (setq org-journal-date-format "%B %Y")
  (setq org-journal-file-format "%Y-%m.org")
  (setq org-journal-file-type 'monthly)
  (org-journal-new-entry nil)
  (org-journal-load-template "monthly")
  (org-journal-update-agenda-files)
  (org-journal-reload-agenda))

;; Create a quarterly journal entry and update agenda
(defun org-journal-quarterly-entry ()
  "Create a new quarterly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "quarterly/"))
  (setq org-journal-date-format "Quarter %q, %Y")
  (setq org-journal-file-format "%Y-Q%q.org")
  (setq org-journal-file-type 'quarterly)
  (org-journal-new-entry nil)
  (org-journal-load-template "quarterly")
  (org-journal-update-agenda-files)
  (org-journal-reload-agenda))

;; Create a yearly journal entry and update agenda
(defun org-journal-yearly-entry ()
  "Create a new yearly journal entry."
  (interactive)
  (setq org-journal-dir (concat my-journal-directory "yearly/"))
  (setq org-journal-date-format "%Y")
  (setq org-journal-file-format "%Y.org")
  (setq org-journal-file-type 'yearly)
  (org-journal-new-entry nil)
  (org-journal-load-template "yearly")
  (org-journal-update-agenda-files)
  (org-journal-reload-agenda))

;; Load a template for the journal entry of the given type
(defun org-journal-load-template (type)
  "Load a template for the journal entry of TYPE."
  (let ((template-file (concat my-templates-directory type ".org")))
    (when (file-exists-p template-file)
      (insert-file-contents template-file))))

;; Keybindings for journal entries
(map! :leader
      :desc "Daily journal entry" "n j d" #'org-journal-daily-entry
      :desc "Weekly journal entry" "n j w" #'org-journal-weekly-entry
      :desc "Monthly journal entry" "n j m" #'org-journal-monthly-entry
      :desc "Quarterly journal entry" "n j q" #'org-journal-quarterly-entry
      :desc "Yearly journal entry" "n j y" #'org-journal-yearly-entry)

(global-set-key (kbd "C-c r u") 'org-roam-ui-open)

;;; Text wrapping
;; Set fill-column to a reasonable value
(setq-default fill-column 80)

;; Enable auto-fill-mode to automatically wrap text at `fill-column`
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Enable visual line mode for visual line wrapping
(global-visual-line-mode 1)

;; MacOS config
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
