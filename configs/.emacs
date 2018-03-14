;; list the repositories containing them
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))


;; list the packages you want
(setq package-list '(
                     ag
                     all-the-icons
                     all-the-icons-dired
                     all-the-icons-gnus
                     anaconda-mode
                     atom-one-dark-theme
                     avy
                     bash-completion
                     csv-mode
                     company-anaconda
                     diff-hl
                     dired-sidebar
                     dired-subtree
                     dockerfile-mode
                     fic-mode
                     flex-autopair
                     flycheck
                     flymake-gjshint
                     flymake-json
                     flymake-python-pyflakes
                     flymake-shell
                     git
                     git-gutter
                     git-gutter+
                     git-gutter-fringe
                     git-gutter-fringe+
                     hgrc-mode
                     highlight-symbol
                     hlinum
                     js2-mode
                     magit
                     monky
                     multi-web-mode
                     nose
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

;; theme
(load-theme 'atom-one-dark t)

;; display watch
(defvar display-time-format "%Y.%m.%d %H:%M")
(defvar display-time-default-load-average nil)
(display-time)

;; jump to word beginning/end
(require 'misc)
(define-key global-map [remap right-word] 'forward-to-word)
(define-key global-map [remap left-word] 'backward-word)

;; change font here
(add-to-list 'default-frame-alist '(font . "Consolas-9"))

;; no tab indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; scroll bindings
(global-set-key (kbd "M-S-<down>") 'scroll-up-line)
(global-set-key (kbd "M-S-<up>") 'scroll-down-line)

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

;; insert middle-click at cursor, not pointer
(setq mouse-yank-at-point t)

;; disable word wrapping
;; use toggle-truncate-lines to override
(set-default 'truncate-lines 1)

(require 'linum)
(global-linum-mode)

;; highlight current line number in the fringe
(require 'hlinum)
(hlinum-activate)

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

;; disable suspend
(global-set-key (kbd "C-z") nil)

;; python
(require 'nose)

(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "flake8")

;; find \
;;     /usr/lib/python3/dist-packages/django \
;;     /usr/local/lib/python3.6/dist-packages/rest_framework \
;;     ~/Documents/mdwh \
;;     -type f -name '*.py' | xargs etags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (shell-command
   (
    format "find %s -type f -name '*.py' | xargs etags -o %s/TAGS"
           (directory-file-name dir-name)
           (directory-file-name dir-name)
           )
   )
  )

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
  (dired-hide-details-mode 1)
  (local-set-key (kbd "TAB") 'dired-subtree-cycle)
  (font-lock-mode 0)
)

(require 'dired-x)
(setq dired-omit-files "^*.pyc$")  ;; use \\|^urls.py$ to append other file

(setq dired-listing-switches "-al --group-directories-first")

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
(setq git-gutter:modified-sign "M")
(setq git-gutter:handled-backends '(git hg))
;; (set-face-foreground 'git-gutter-fr:modified "#d080d0")
(set-face-foreground 'git-gutter:modified "#d080d0")

(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

;; jump to any symbol
(global-set-key (kbd "M-s") 'avy-goto-char-timer)
;; (global-set-key (kbd "M-S-s") 'avy-goto-char)

;; https://stackoverflow.com/a/65992/379159
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; flash on error
(setq visible-bell t)
