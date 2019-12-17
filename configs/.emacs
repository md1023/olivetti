;; list the repositories containing them
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/") ;; too old packages
                         ))


;; list the packages you want
(setq package-list '(
                     ag
                     all-the-icons-dired
                     all-the-icons-gnus
                     atom-one-dark-theme
                     avy
                     bash-completion
                     company-anaconda
                     csv-mode
                     diff-hl
                     dired-sidebar
                     docker-tramp
                     dockerfile-mode
                     doom-modeline
                     elcord
                     fic-mode
                     flycheck
                     git
                     git-gutter-fringe
                     highlight-symbol
                     hlinum
                     js2-mode
                     lua-mode
                     magit
                     monky
                     multi-web-mode
                     rainbow-delimiters
                     yaml-mode
))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; write customize options in separate file
(setq custom-file "~/.emacs.d/custom.el")

(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; Don't open journal at start
;; (setq initial-buffer-choice "~/Documents/journal.org")
(setq initial-buffer-choice nil)

;; Don't show welcome screen.
(setq inhibit-startup-screen t)

;; mercurial path on MacOS
(add-to-list 'exec-path "/usr/local/bin")

;; silver searcher location
(setq ag-executable "/usr/bin/ag")

;; theme
(load-theme 'atom-one-dark t)

;; Don't display watch
;; (defvar display-time-format "%Y.%m.%d %H:%M")
;; (defvar display-time-default-load-average nil)
;; (display-time)

;; jump to word beginning/end
(require 'misc)
(define-key global-map [remap right-word] 'forward-to-word)
(define-key global-map [remap left-word] 'backward-word)

;; change font here
(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-12"))

(setq frame-title-format "%b-%p")

;; no tab indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; scroll bindings
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

;; disable menus
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; show line and columns numbers
(setq line-number-mode 1)
(setq column-number-mode 1)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 1)
(setq doom-modeline-minor-modes 1)
(setq doom-modeline-vcs-max-length 30)

;; cursor type
(setq-default cursor-type '(hbar . 2))
;; (setq x-stretch-cursor 1)
(blink-cursor-mode 1)

;; insert middle-click at cursor, not pointer
(setq mouse-yank-at-point t)

;; disable word wrapping
;; use toggle-truncate-lines to override
(set-default 'truncate-lines 1)

(require 'linum)
;; Linum mode from recent 26 versions
(global-display-line-numbers-mode)

;; highlight current line number in the fringe
(require 'hlinum)
(hlinum-activate)

;; fix linum face width
(eval-after-load "hlinum"
  '(set-face-attribute 'linum-highlight-face nil :height 90))
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 90))

;; highlight fringe in folders under version control
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; move around buffers with cursor keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(show-paren-mode 1)

(require 'fic-mode)
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0)
;; TODO move add-hooks elsewhere
;; face in comments for TODO highlights
(dolist
    (h '(fic-mode
         rainbow-delimiters-mode
         highlight-symbol-mode
         highlight-symbol-nav-mode))
  (add-hook 'prog-mode-hook h))

;; pythonic-activate /path/to/venv - change anaconda interpreter
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook '(lambda () (
                                         local-set-key
                                         (kbd "C-c C-t")
                                         'test-function-at-point
                                        )
))
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; disable suspend
(global-set-key (kbd "C-z") nil)

;; set command key instead of ctrl
(setq mac-command-modifier 'meta)
(setq mac-right-command-modifier 'super)

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; override capitalize first letter with copy and scroll with paste
(global-set-key (kbd "M-c") 'ns-copy-including-secondary)
(global-set-key (kbd "M-v") 'yank)

;; org mode
(setf org-replace-disputed-keys 1
      org-todo-keywords '("TODO" "WORKING" "FAIL" "COMPLETE")
      org-todo-keyword-faces '(
                               ("WORKING" . '(:foreground "#FFD000" :weight bold))
                               ("FAIL" . '(:foreground "#EE1010" :weight bold))
                               ))

;; buffer and frames focus behaviour
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window
      ido-decorations '(" { " " }" "  " "  ..." " [ " " ]  " " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")
      ido-enable-dot-prefix 1
      ido-enable-last-directory-history 1
      ido-enable-record-commands 1
      ido-enable-flex-matching 1
      ibuffer-shrink-to-minimum-size 1)

;; discord chat
;; (require 'elcord)
;; (elcord-mode)

(defun simno-dired-mode-setup ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1)
  (local-set-key (kbd "TAB") 'dired-subtree-cycle)
  (font-lock-mode 0)
  (dired-omit-mode 1)
)

(setq dired-sidebar-no-delete-other-windows t)

;; https://stackoverflow.com/questions/1292936/line-wrapping-within-emacs-compilation-buffer
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil)
  (setq compilation-scroll-output 'first-error)
)
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun test-function-at-point ()
  "Test function at point"
  (interactive)
  (compile (let ((container "um-api")) (concat
          ;; "/usr/local/bin/docker exec -it uauth_server_1 pytest --disable-warnings -svvv "
          ;; "dockerexec um-api pytest --disable-warnings -svvv "
          (format "dockerexec %s pytest --disable-warnings -svvv " container)
          (car
           (reverse
            (split-string (buffer-file-name) (format "u-auth/%s/" container))
           )
          )
          (if (which-function)
              (replace-regexp-in-string
               " (def)"
               ""
               (concat "::"
                       (combine-and-quote-strings
                        (split-string (which-function) "\\.")
                        "::"
                       )
               )
              )
          )
  )))
)

(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(defvar ansi-color-names-vector
  ["#222222" "#dca3a3" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#c0bed1" "#93b3a3" "#cccccc"])

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq dired-listing-switches "-lap --group-directories-first")

(require 'dired-x)
(setq dired-omit-files "^*.pyc$\\|\\.+/$\\|__pycache__/$\\|.pytest_cache$\\|.orig\\|\\./$")  ;; use \\|^urls.py$ to append other file

(add-hook 'dired-mode-hook 'simno-dired-mode-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq
 backup-by-copying t
 backup-directory-alist
 `((".*" . , "~/.emacs.d/tmp"))
 auto-save-file-name-transforms
 `((".*" , "~/.emacs.d/tmp" t)))

;; VCS
(global-git-gutter-mode t)
(setq git-gutter:modified-sign "±")
(setq git-gutter:handled-backends '(git hg))

(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;; jump to any symbol
(global-set-key (kbd "M-s") 'avy-goto-char-timer)
;; (global-set-key (kbd "M-S-s") 'avy-goto-char)

;; flash on error
(setq visible-bell t)
