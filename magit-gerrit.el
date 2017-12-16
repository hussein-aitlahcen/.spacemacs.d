(require 'magit)

(defun magit-push-to-gerrit ()
  (interactive)
  (magit-git-command-topdir "git push origin HEAD:refs/for/master"))

(magit-define-popup-action 'magit-push-popup
  ?m
  "Push to gerrit"
  'magit-push-to-gerrit)
