(defconst HOME (getenv "HOME") "User's home directory")

(add-to-list 'load-path (concat HOME "/emacs"))
(add-to-list 'load-path (concat HOME "/emacs/flycheck"))
(add-to-list 'load-path (concat HOME "/emacs/magit"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-jabber")
(add-to-list 'load-path (concat HOME "/emacs/pep8"))

(when (>= emacs-major-version 24)
  (setq package-archives 
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("marmalade" . "http://marmalade-repo.org/packages/")
	  ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

;; display watch
(setq display-time-format "%Y.%m.%d %H:%M")
(display-time)

;; jump to word beginning/end
(require 'misc)
(define-key global-map [remap right-word] 'forward-to-word)
(define-key global-map [remap left-word] 'backward-word)

;; jump around text
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; open files via ssh: C-x C-f /sudo:root@jenkins:/
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as\\'" . ecmascript-mode))

;; MozRepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(require 'column-marker)

;; change font here
(add-to-list 'default-frame-alist '(font . "Consolas-9"))

;; autocompletion
(autoload 'tern-mode "tern.el" nil t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq python-python-command "python2.7"
      calendar-week-start-day 0
      inhibit-startup-message 1
      global-font-lock-mode 1
      current-language-environment "Cyrillic-UTF8")

(put 'unit-test 'safe-local-variable
     '(lambda (val) 1))

;; smooth scrolling
(setq redisplay-dont-pause 1
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  auto-window-vscroll 0
)

;; scroll bindings
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

;; disable menus
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; cursor type
(setq-default cursor-type '(hbar . 2))
;; (setq x-stretch-cursor 1)
(blink-cursor-mode 1)

;; disable word wrapping
;; use toggle-truncate-lines to override
(set-default 'truncate-lines 1)

(require 'magit)
(defalias 'ms 'magit-status)

(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq git-gutter:modified-sign "M")
;; (fringe-helper-define 'git-gutter-fr:modified nil
;;   ".X...X."
;;   ".XX.XX."
;;   ".X.X.X."
;;   "X..X..X"
;;   "X..X..X"
;;   "X.....X"
;;   "X.....X")
(set-face-foreground 'git-gutter-fr:modified "#d080d0")
(set-face-foreground 'git-gutter:modified "#d080d0")

(require 'org-install)
(require 'hl-line+)
(require 'highlight)
(require 'highlight-indentation)
(require 'ecmascript-mode)
(require 'minimap)
(require 'php-mode)

;; colorize emacs, check zenburn overrides
(require 'zenburn-theme)

;;(set-fringe-mode 4) ;; half-width fringe line
(show-paren-mode 1)
(set-face-attribute 'show-paren-match-face 0 :weight 'bold)
(set-face-background 'show-paren-match-face "#5F7F5F")
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'linum)
(global-linum-mode)

(require 'ido)
;; buffer and frames focus behaviour
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window
      ido-decorations '(" { " " }" "  " "  ..." " [ " " ]  " " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")
      ido-enable-dot-prefix 1
      ido-enable-last-directory-history 1
      ido-enable-record-commands 1
      ido-enable-flex-matching 1
      ibuffer-shrink-to-minimum-size 1)

;; move around buffers with cursor keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; disable suspend
(global-set-key (kbd "C-z") nil)

;; hs
(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)
(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))
(hideshowvis-symbols)

(load-library "hideshow")
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)

;; russian hotkeys layout fixer
(define-key function-key-map [?\C-ч] [?\C-x])
(define-key function-key-map [?\M-ч] [?\M-x])
(define-key function-key-map [?\C-ы] [?\C-s])
(define-key function-key-map [?\C-к] [?\C-r])
(define-key function-key-map [?\C-п] [?\C-g])
(define-key function-key-map [?\C-ц] [?\C-w])
(define-key function-key-map [?\C-н] [?\C-y])
(define-key function-key-map [?\C-д] [?\C-l])
(define-key function-key-map [?\C-а] [?\C-f])
(define-key function-key-map [?\C-л] [?\C-k])
(define-key function-key-map [?\C-в] [?\C-d])
(define-key function-key-map [?\C-р] [?\C-h])
(define-key function-key-map [?и] [?b])
(define-key function-key-map [?\C-ж] [?\C-\;])

;; lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." 1)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; org mode
(print "MY ORG LOADED")
(setf org-replace-disputed-keys 1
      org-todo-keywords '("TODO" "WORKING" "FAIL" "COMPLETE")
      org-todo-keyword-faces '(
                               ("WORKING" . '(:foreground "#FFD000" :weight bold))
                               ("FAIL" . '(:foreground "#EE1010" :weight bold))
                               ))

;; tests
(require 'nose)

;; database
(require 'sql)
(defun erase-sql-buffer ()
  (interactive)
  (when sql-buffer
    (save-excursion
      (set-buffer sql-buffer)
      (erase-buffer))))
(require 'term)

(defun erase-terminal-buffer ()
  (interactive)
  (when "*terminal*"
    (save-excursion
      (set-buffer "*terminal*")
      (erase-buffer))))

(defun erase-sql-buffer-and-exec ()
  (interactive)
  (save-buffer)
  (erase-sql-buffer)
  (sql-send-string (format "\\i %s" (buffer-file-name))))
(define-key sql-mode-map "\C-c\C-w" 'erase-sql-buffer-and-exec)

(defun mydb-cms1 ()
  (interactive)
  (message "Login...")
  (setf sql-database "cms1"
        sql-server ""
        sql-user ""
        sql-postgres-options '("-P" "pager=off"))
  (sql-postgres)
  ;; All done.
  (sql-set-sqli-buffer-generally)
  (message "Login...done")
  (pop-to-buffer "*SQL*"))

;; flycheck
(require 's)
(require 'dash)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ack-grep
(require 'ack)

;; irc settings
(require 'erc)
(defun office-chat ()
  "Connect to IRC and Jabber at igrade.ru"
  (interactive)
  (let ((password (read-passwd "Enter password: ")))
  (erc-tls :server "irc.igrade.ru" :port 6667
  	     :nick "m-nikolaev" :full-name "Maxim Nikolaev"
  	     :password password)
  (setq erc-autojoin-channels-alist '(("igrade.ru" "#it" "#notify" "#flood")))
  (jabber-connect "m-nikolaev@igrade.ru" "igrade.ru" "" nil password
		  "igrade.ru" 5223 'ssl)))

;;jabber settings
(require 'jabber-autoloads)
(setq jabber-account-list '(("m-nikolaev@igrade.ru"
			     (:network-server . "igrade.ru")
			     (:port . 5223)
			     (:connection-type . ssl)))
      ;; jabber-alert-message-hooks '(jabber-message-echo jabber-message-display jabber-message-scroll)
      jabber-auto-reconnect 1
      jabber-vcard-avatars-retrieve nil
      jabber-vcard-avatars-publish nil
      jabber-show-offline-contacts nil
      jabber-roster-show-bindings nil
      jabber-rare-time-format "%e %b %Y %H:00"
      jabber-connection-ssl-program 'gnutls
      jabber-history-enabled 1
      jabber-history-size-limit 8192
      jabber-roster-line-format " %c %-25n %u %-8s  %S"
      ;; jabber-roster-line-format "%a %c %-25n %u %-8s  %S"
      ;; jabber-roster-sort-functions (quote (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group))
      jabber-use-global-history 0)

;; show line and columns numbers
(setq line-number-mode 1)
(setq column-number-mode 1)

;; color behave feature files
(setq feature-default-language "ru")
(setq feature-default-i18n-file (concat HOME "emacs/i18n.yml"))
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-blue ((((background dark)) (:background "LightBlue3" :foreground "black"))))
 '(hi-green ((((min-colors 88) (background dark)) (:background "OliveDrab3" :foreground "black"))))
 '(hi-yellow ((((min-colors 88) (background dark)) (:background "yellow3" :foreground "black")))))
(put 'narrow-to-region 'disabled nil)

;; default layout for C-\ switch
(setq default-input-method "russian-computer")
