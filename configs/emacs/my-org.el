;; org mode
(print "MY ORG LOADED")
(setf org-replace-disputed-keys t
      org-todo-keywords '("TODO" "WORKING" "COMPLETE")
      org-todo-keyword-faces '(("WORKING" . '(:foreground "#FFD000" :weight bold))))

(provide 'my-org)
