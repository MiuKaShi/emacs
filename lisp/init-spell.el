;;; init-spell.el --- Spell checker -*- lexical-binding: t -*-
;; z= `ispell-word'

;;; Commentary:
;;

;;; Code:

(use-package ispell
  :ensure nil
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i p" . ispell-comment-or-string-at-point)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i b" . ispell-buffer)
         ("C-c i r" . ispell-region))
  :config
  ;; MacOS is broken
  (when (eq system-type 'darwin)
    (setenv "DICTIONARY" "en_US"))

  ;; no spell checking for org special blocks
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  :custom
  (ispell-really-hunspell t)
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  (ispell-following-word t)
  (ispell-personal-dictionary (locate-user-emacs-file "hunspell_dict.txt")))

;; Spell check on-the-fly
(use-package flyspell
  :ensure nil
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))

  (dolist (mode '(
                  ;;org-mode-hook
                  mu4e-compose-mode-hook))
    (add-hook mode (lambda () (flyspell-mode 1))))

  (setq flyspell-issue-welcome-flag nil
				flyspell-use-meta-tab t
        flyspell-issue-message-flag nil))

(provide 'init-spell)
