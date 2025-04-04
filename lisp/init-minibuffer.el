;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-sort-function nil))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic substring))
                                   (project-file (styles basic substring))))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               ;; Prefixes: "re-re" ~ "recode-region",
                               ;;                     "query-replace-regex"
                               ;; orderless-prefixes
                               ;;
                               ;; Initialisms: "abc" ~ "alfred-batman-catwoman"
                               ;; NOTE: Initialism regexps are really slow under
                               ;;       the hood. You probably don't want them
                               ;; orderless-initialism
                               ;;
                               ;; Flex:
                               ;; characters in the search string must appear in the candidate
                               ;; string, but not necessarily consecutively,  i.e, "abc" really
                               ;; means "a.*b.*c", so it would match "anybody can."
                               ;; orderless-flex
                               )))


(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect)))

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap jump-to-register]       . consult-register-load)
         ([remap point-to-register]      . consult-register-store))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

	(setq history-delete-duplicates t)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
	:after (embark consult)
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))


(provide 'init-minibuffer)
