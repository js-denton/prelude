;;; prelude-core.el --- Emacs Prelude: Core Prelude functions.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the general-purpose functions and
;; commands added by Prelude.  Some modules define additional module-specific
;; functions and commands.
;;
;; Note that many of the original core Prelude commands were extracted to the
;; crux package (Prelude installs it automatically).  Prelude's auto-save
;; functionality was extracted to the super-save package.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-to-list 'auto-mode-alist '("\\.xi\\'" . auto-fill-mode))

;; Включаем поддержку magit при выборе проекта, иначе что бы активировать
;; данную возможность необходимо ввести `C-x M-g`.
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Включаем минорный режим дополнения vertico
(use-package vertico
  :init
  (vertico-mode))

;; Включаем минорный режим дополнения marginalia
;; Благодаря этому режиму в дополнении можно увидеть описание
(use-package marginalia
  :init
  (marginalia-mode))

;; Включаем механизм позволяющий искать все возможные варианты
;; совпадений для дополнения.
;; Например, написав txt после C-x C-f, то отобразятся все файлы
;; содержащие txt (расширение, имя, ...).
;; Без orderless дополнение работает только на полное совпадение
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Включаем поддержку автодополнения в буфере (popup окно)
(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; Включаем поддержку наследования для вложений
;; Пример можно увидеть здесь: https://orgmode.org/manual/Attachment-options.html#index-org_002dattach_002duse_002dinheritance
(use-package org-attach
  :after org
  :custom
  (org-attach-use-inheritance t))

;; Включаем возможность угадать каталог назначения
;; Если открыто два буфера Dired то будет проще копировать/премещать файлы между ними
(setq dired-dwim-target t)
(provide 'prelude-core)
;;; prelude-core.el ends here
