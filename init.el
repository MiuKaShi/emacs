;;; Increase how much is read from processes in a single chunk (default is 4kb).

;;; Commentary:
;;

;;; Code:

;; `lsp-mode' benefits from that.
(setq read-process-output-max (* 4 1024 1024))

(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; 安装 `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
	(setq use-package-verbose nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; 安装 `quelpa'
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; 将lisp目录放到加载路径的前面以加快启动速度
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))

;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
	(require 'init-base)
	(require 'init-utils)
	(require 'init-ui)
	(require 'init-tools)
	(require 'init-evil)
	(require 'init-lsp)
	(require 'init-git)
	(require 'init-dev)
	(require 'init-dired)
	(require 'init-minibuffer)
	(require 'init-keymaps)
	;; standalone apps
	(require 'init-org)
	(require 'init-text)
	(require 'init-shell)
	(require 'init-spell)
)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
