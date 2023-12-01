;;; init-lua.el --- Lua -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode))

(provide 'init-lua)
