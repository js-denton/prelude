;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
                                        ;(package-initialize)

;; Определение имя пользователя для Приветствия
;; TODO проверить на Windows
(defvar prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
;; Определяем минимальну версию Emacs на котором тестировался модуль
(defvar prelude-emacs-required "29.4")

;; Приветствие
(message "[Prelude] Prelude включается... Будьте терпеливее, Мастер %s!" prelude-user)

;; Emacs не тестировался и не проверялся на версии ниже 29.4 поэтому что-то может не работать.
(when (version< emacs-version prelude-emacs-required)
  (warn "[Prelude] Prelude тестировалс на версии %s, но у тебя %s" prelude-emacs-required emacs-version))

;; Always load newest byte code
;; Не понятно зачем всё время загружать новый байт код?
;; Видимо для тех случаев когда произошло обновление Prelude что бы код обновился.
;; С такой опцией Prelude получается всегда дольше запускается в отличие от Spacemacs (не точно)
;; Может быть нужно посмотреть в сторону нативной компиляции (https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html)
;; ???
;; (setq load-prefer-newer t)

;; Определяем структуру каталогов
(defvar prelude-dir (file-name-directory load-file-name)
  "Корневой каталог для Prelude относительной файла init.el.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "Каталог базовой функциональности Prelude.")
;; (defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
;;   "Каталог встроеных модулей Prelude.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "Каталог для персональных настроек.

;; Users of Emacs Prelude are encouraged to keep their personal configuration
;; changes in this directory.  All Emacs Lisp files there are loaded automatically
;; by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "Каталог персональных настроек которые должны быть загружено до Prelude.")
;; (defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
;;   "Каталог для расширений недоступных из репозиторий.")
(defvar prelude-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Какталог для автоматического сохранения файла/истории изменений.")
;; (defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-personal-dir)
;;   "This file contains a list of modules that will be loaded by Prelude.")

;; Дальше идут функции которые объявлены в файлах Prelude
;; Создание каталога для хранения истории модификации файла так как Emacs из коробки после закрытия сессии
;; её больше не имеет.
(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

;; Устанавливем путь загрузки модулей (наверное)
;; TODO
;; (defun prelude-add-subfolders-to-load-path (parent-dir)
;;   "Add all level PARENT-DIR subdirs to the `load-path'."
;;   (dolist (f (directory-files parent-dir))
;;     (let ((name (expand-file-name f parent-dir)))
;;       (when (and (file-directory-p name)
;;                  (not (string-prefix-p "." f)))
;;         (add-to-list 'load-path name)
;;         (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
;; (add-to-list 'load-path prelude-modules-dir)
;; (add-to-list 'load-path prelude-vendor-dir)
;; (prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; Увеличивание размера буфера для сборщика мусора.
;; Пока не понятно как он влияет на работу Emacs (https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html)
;; TODO
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; (setq gc-cons-threshold 50000000)

;; Загрузка настроек до загрузки Prelude из `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "[Prelude] Загрузка персональных пред настроек из %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Prelude] Загрузка базовых модулей Prelude...")

;; Загрузка пакетов/модулей
;; ???
;; TODO
;; load the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
;; (require 'prelude-mode)
;; (require 'prelude-editor)
;; (require 'prelude-global-keybindings)

;; Настройки для macOS
;; Не тестировались
;; TODO
;; (when (eq system-type 'darwin)
;;   (require 'prelude-macos))

;; Настройки для GNU/Linux
;; Не тестировались
;; TODO
;; (when (eq system-type 'gnu/linux)
;;   (require 'prelude-linux))

;; Настройки для WSL
;; Не тестировались
;; TODO
;; (when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
;;   (require 'prelude-wsl))

;; Настройки для Windows
;; Не тестировались
;; TODO
;; (when (eq system-type 'windows-nt)
;;   (require 'prelude-windows))

;; (message "[Prelude] Loading Prelude's additional modules...")

;; Настройка модулей ??? Что-то уже много настроек и приготовлений
;; TODO
;; the modules
;; (if (file-exists-p prelude-modules-file)
;;     (load prelude-modules-file)
;;   (message "[Prelude] Missing personal modules file %s" prelude-modules-file)
;;   (message "[Prelude] Falling back to the bundled example file sample/prelude-modules.el")
;;   (message "[Prelude] You should copy this file to your personal configuration folder and tweak it to your liking")
;;   (load (expand-file-name "sample/prelude-modules.el" prelude-dir)))

;; Устанавливаем путь для пользовательских насторек.
;; Теперь они будут храниться в каталоге `prelude-personal-dir`
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; Загружаем персональные настройки
(when (file-exists-p prelude-personal-dir)
  (message "[Prelude] Загружаем пресональные настройки из %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$")))
;; Здесь прежде чем загружать персональные настойки из списка файлов удаляются те, кторые есть
;; в `prelude-modules-file` - пока этот модуль отключен.
  ;; (mapc 'load (delete
  ;;              prelude-modules-file
  ;;              (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Prelude] Prelude готов исполнить твои приказания, Мастер %s!" prelude-user)

;; Не понятно что происходит
;; TODO
;; (prelude-eval-after-init
;;  ;; greet the use with some useful tip
;;  (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
