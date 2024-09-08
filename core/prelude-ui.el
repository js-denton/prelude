;;; prelude-ui.el --- Emacs Prelude: UI optimizations and tweaks.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Базовая модификация пользовательского интерфейса.

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

;; Отключаем toolbar панель.
;; Она занимает много места и может не работать в терминальном режиме.
;; Изменить параметры можно через:
;; Option -> Show/Hide -> Tool Bar
(tool-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
;; (blink-cursor-mode 1)

;; Отключение звукового сигнала
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Beeping.html
;; https://www.emacswiki.org/emacs/AlarmBell
;; (setq ring-bell-function 'ignore)

;; Отключение приветсвенного экрана
(setq inhibit-startup-screen t)

;; ;; nice scrolling
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

;; Включаем режим отображения номера строк глобально
;; Изменить можно через:
;; Option -> Show/Hide -> Line Numbers for All Lines
(global-display-line-numbers-mode t)
;; Режим нумерации относительно текущей строки - так удобно "прыгать"
(setq display-line-numbers-type 'relative)

;; Для Mode Line отключаем отображение текущей строки/колонки и размер файла
;; Изменить можно через:
;; Option -> Show/Hide -> Line Numbers in Mode Line
(setq cloumn-number-mode nil)
(setq line-number-mode nil)
(setq size-indication-mode nil)

;; https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?h=emacs-29&id=ef8838c3a5f041769f72758b831eb3fa7a130fb9#n493
;; show line numbers at the beginning of each line
;; (unless prelude-minimalistic-ui
;;   ;; there's a built-in linum-mode, but we're using
;;   ;; display-line-numbers-mode or nlinum-mode,
;;   ;; as it's supposedly faster
;;   (if (fboundp 'global-display-line-numbers-mode)
;;       (global-display-line-numbers-mode)
;;     (global-nlinum-mode t)))

;; Включение возможности короткого ответа y/n вместо yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; use zenburn as the default theme
;; (when prelude-theme
;;   (load-theme prelude-theme t))

;; show available keybindings after you start typing
;; add to hook when running as a daemon as a workaround for a
;; which-key bug
;; https://github.com/justbur/emacs-which-key/issues/306
;; (if (daemonp)
;;     (add-hook 'server-after-make-frame-hook 'which-key-mode)
;;   (which-key-mode +1))

(provide 'prelude-ui)
;;; prelude-ui.el ends here
