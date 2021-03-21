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
                     birds-of-paradise-plus-theme
                     avy
                     bash-completion
                     company-anaconda
                     csv-mode
                     diff-hl
                     dired-sidebar
                     docker-tramp
                     dockerfile-mode
                     doom-modeline
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
                     org-journal
                     yaml-mode
                     xterm-color
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

;; write customize options in separate file, otherwise will append to this file
(setq custom-file "~/.emacs.d/custom.el")

(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; Don't open journal at start
;; (setq initial-buffer-choice "~/Documents/journal.org")
(setq initial-buffer-choice nil)
(require 'org-journal)
;; (setq org-journal-dir "~/Documents/org/")  ;; doesn't work on Emacs 26.3
(setq org-journal-file-format "%m.%B.journal.org")
(setq org-journal-date-format "%B %d, %A, week %V")
(setq org-journal-file-type 'monthly)

;; highlight jira task
(font-lock-add-keywords 'org-mode '(
   ("\\(IT\_DEV\-[0-9]+\\)" (0 font-lock-type-face))
   ("\\(IT\-[0-9]+\\)" (0 font-lock-type-face))
))


;; Don't show welcome screen.
(setq inhibit-startup-screen t)

;; mercurial path on MacOS
(add-to-list 'exec-path "/usr/local/bin")

;; silver searcher location
;; misses .gitignore settings when run from here, runs slower
;; (setq ag-executable "/usr/bin/ag")
(setq ag-reuse-window nil)

;; theme
;; (load-theme 'atom-one-dark t)
(load-theme 'birds-of-paradise-plus t)

;; Don't display watch
;; (defvar display-time-format "%Y.%m.%d %H:%M")
;; (defvar display-time-default-load-average nil)
;; (display-time)

;; jump to word beginning/end
(require 'misc)
(define-key global-map [remap right-word] 'forward-to-word)
(define-key global-map [remap left-word] 'backward-word)

;; change font here
;; (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-12"))
(add-to-list 'default-frame-alist '(font . "Consolas-9"))

(setq frame-title-format "%b-%p")

;; no tab indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

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

;; override org-mode functions whereas S-<cursor> has no effect
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

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
;; (define-key python-mode-map (kbd "<C-tab>") 'hs-toggle-hiding)
;; (define-key python-mode-map (kbd "C-c TAB") 'hs-show-all)
;; (define-key python-mode-map (kbd "C-c <backtab>") 'hs-hide-all)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; (require 'dap-python)
;; (dap-register-debug-template "My App"
;;   (list :type "python"
;;         :args "-i"
;;         :cwd nil
;;         :env '(("DEBUG" . "1"))
;;         :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
;;         :request "launch"
;;         :name "Cheapstake2"))

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


;; https://stackoverflow.com/questions/1292936/line-wrapping-within-emacs-compilation-buffer
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil) ;; automatically becomes buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil)
  (setq compilation-scroll-output 'first-error)
)
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun test-function-at-point-kc ()
  "
  cd /home/mnikolaev/Documents/it/dev/keycloak/tests/; \
    source ./venv/bin/activate; \
    (export $(xargs < .env.local); \
       pytest --disable-warnings -svvv \
         api/test_oidc_token.py::test__oidc_token__direct_grant_public_client__login \
    )
  "
  (interactive)
  (compile (let
               ((test-path-bits
                 (split-string
                  (car
                   (reverse
                    (split-string (buffer-file-name) "keycloak/")))
                  "/"))
                (project-path
                  (car
                    (split-string (buffer-file-name) "/keycloak"))
                  ))
             (concat
               (format
                "cd %s/keycloak/%s/; "
                project-path
                (car test-path-bits)
                )
               "source ./venv/bin/activate; "
               (format
                "(export $(xargs < .env.local); pytest --disable-warnings -svvv %s"
                (string-join (cdr test-path-bits) "/")
                )
               (if (which-function)
                  (replace-regexp-in-string
                   " (def)"
                   ""
                   (concat "::"
                           (combine-and-quote-strings
                            (split-string (which-function) "\\.")
                            "::"))))
               ")"
              )
             )
           )
)

(defun test-function-at-point ()
  "
  cd /home/mnikolaev/Documents/it/dev/keycloak/tests/; \
    source ./venv/bin/activate; \
    (export $(xargs < .env.local); \
       pytest --disable-warnings -svvv \
         api/test_oidc_token.py::test__oidc_token__direct_grant_public_client__login \
    )
  "
  (interactive)
  (compile (let
               ((test-path-bits
                 (split-string
                  (car
                   (reverse
                    (split-string (buffer-file-name) "keycloak/")))
                  "/"))
                (project-path
                  (car
                    (split-string (buffer-file-name) "/keycloak"))
                  ))
             (concat
               (format
                "cd %s/keycloak/%s/; "
                project-path
                (car test-path-bits)
                )
               "source ./venv/bin/activate; "
               (format
                "(export $(xargs < .env.local); pytest --disable-warnings -svvv %s"
                (string-join (cdr test-path-bits) "/")
                )
               (if (which-function)
                  (replace-regexp-in-string
                   " (def)"
                   ""
                   (concat "::"
                           (combine-and-quote-strings
                            (split-string (which-function) "\\.")
                            "::"))))
               ")"
             )
           )
  )
)

(defun test-function-at-point-u-auth ()
  "Test function at point.
    |-----------|----------------|---------------------------------|
    |  path     |      car       |             cdr                 |
    |-----------|----------------|---------------------------------|
    | ../u-auth | credentials-api| tests/test_login.py             |
    | ../u-auth | tests          | ui/requests/test_login_logout.py|
    |-----------|----------------|---------------------------------|"
  (interactive)
  (compile (let
               ((path
                 (split-string
                  (car
                   (reverse
                    (split-string
                     (buffer-file-name)
                     "u-auth/"
                     )))
                  "/")))
             (concat
              (format
               "dockerexec %s pytest --disable-warnings -svvv %s"
               (car path)
               (string-join (cdr path) "/"))
              (if (which-function)
                  (replace-regexp-in-string
                   " (def)"
                   ""
                   (concat "::"
                           (combine-and-quote-strings
                            (split-string (which-function) "\\.")
                            "::")))))
           )
  )
)

(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; https://stackoverflow.com/a/63711498/379159
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (if (eq major-mode 'ag-mode)
      (funcall f proc string)
    (funcall f proc (xterm-color-filter string))))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; prepare .zshrc aliases
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(setq dired-listing-switches "-lap --group-directories-first")

(require 'dired-x)
(setq dired-omit-files "^*.pyc$\\|\\.+/$\\|__pycache__/$\\|.pytest_cache$\\|.orig\\|\\./$")  ;; use \\|^urls.py$ to append other file

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-sidebar-no-delete-other-windows t)

(setq
 backup-by-copying t
 backup-directory-alist
 `((".*" . , "~/.emacs.d/tmp"))
 auto-save-file-name-transforms
 `((".*" , "~/.emacs.d/tmp" t)))

;; VCS
(global-git-gutter-mode t)
(setq git-gutter:modified-sign "±")
(setq git-gutter:handled-backends '(git))

(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;; jump to any symbol
(global-set-key (kbd "M-s") 'avy-goto-char-timer)
;; (global-set-key (kbd "M-S-s") 'avy-goto-char)

;; flash on error
(setq visible-bell t)
