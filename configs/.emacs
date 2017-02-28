;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("2882cf41c12276b5875879a71cc670d1468653e342586075a48ed68cfed15bea" "1fab355c4c92964546ab511838e3f9f5437f4e68d9d1d073ab8e36e51b26ca6a" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "35fc36f6bcd5acfc0ca68a0120b78c472337dc92746c81c763c9274d9e7d8afb" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "100d6bde8ef749efd2984f24db31434d90348d9aaf718f94231208e95fae37a2" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "bac3f5378bc938e96315059cd0488d6ef7a365bae73dac2ff6698960df90552d" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(git-gutter:handled-backends (quote (hg git)))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (nose multi-web-mode quasi-monochrome-theme jenkins jenkins-watch flymake-gjshint flymake-json flymake-php flymake-python-pyflakes flymake-shell php-mode flex-autopair rainbow-delimiters magit golden-ratio ahg bash-completion fic-mode python-mode git git-gutter git-gutter+ git-gutter-fringe git-gutter-fringe+ hgrc-mode hideshow-org hideshowvis js2-mode ag highlight-symbol hlinum ensime flycheck monky org zenburn-theme)))
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter "python3")
 '(url-proxy-services
   (quote
    (("http" . "192.168.200.105:8088")
     ("https" . "192.168.200.105:8088")))))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; .emacs --- simno's Emacs config

;; THEME
(defconst -HOME (getenv "HOME") "User's home directory")
(add-to-list 'load-path (concat -HOME "/Documents/olivetti/configs/emacs"))
(load-theme 'atom-one-dark t)

(load-theme 'atom-one-dark t)
;; GENERAL

(defconst -HOME (getenv "HOME") "User's home director.")

;; display watch
(defvar display-time-format "%Y.%m.%d %H:%M")
(defvar display-time-default-load-average nil)
(display-time)

;; jump to word beginning/end
(require 'misc)
(define-key global-map [remap right-word] 'forward-to-word)
(define-key global-map [remap left-word] 'backward-word)

;; change font here
(add-to-list 'default-frame-alist '(font . "Anonymous Pro-14"))

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

;; cursor type
(setq-default cursor-type '(hbar . 2))
;; (setq x-stretch-cursor 1)
(blink-cursor-mode 1)

;; disable word wrapping
;; use toggle-truncate-lines to override
(set-default 'truncate-lines 1)

(require 'linum)
(global-linum-mode)

(require 'hlinum)
(hlinum-activate)

;; move around buffers with cursor keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(show-paren-mode 1)

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0)
;; TODO move add-hooks elsewhere
(dolist (h '(highlight-symbol-mode highlight-symbol-nav-mode)) (add-hook 'prog-mode-hook h))

;; disable suspend
(global-set-key (kbd "C-z") nil)

;; face in comments for TODO highlights
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; python
(require 'nose)

;; org mode
(print "MY ORG LOADED")
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

(defun simno-dired-mode-setup ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'simno-dired-mode-setup)

;; VCS

(global-git-gutter-mode t)
(setq git-gutter:modified-sign "M")
(setq git-gutter:handled-backends '(git hg))
;; (set-face-foreground 'git-gutter-fr:modified "#d080d0")
(set-face-foreground 'git-gutter:modified "#d080d0")
