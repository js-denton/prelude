;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;
;; Copyright © 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.  This module also adds a couple of package.el extensions
;; and provides functionality for auto-installing major modes on demand.

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
;; Инициализация straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'vertico)
(straight-use-package 'marginalia) ;; аннотации к vertico
(straight-use-package
 '(magit :type git
	 :files ("lisp/magit*.el"
		 "lisp/git-*.el"
		 "docs/magit.texi"
		 "docs/AUTHORS.md"
		 "LICENSE" "magit-pkg.el"
		 (:exclude "lisp/magit-section.el")
		 "magit-pkg.el")
	 :host github
	 :repo "magit/magit"))
(straight-use-package 'orderless) ;; расширенный стиль дополнения
(straight-use-package 'corfu) ;; автодобавление в буфере

(defun prelude-check-installed (package)
  "Возвращает t если PACKAGE установлен, в противном случае nil.

Функция заточена под straight.el и не использует package.el.
Так как у straight нет готового метода \"из коробки\", то что бы
определить установлен пакет в системе или нет прибегаем к поиску
PACKGAE из таблицы straight--recipe-cache и если находим, то
можно считать что пакет уже установлен.

Альтернативным способом является поиск на файловой системе."
  (plist-get (gethash (symbol-name package) straight--recipe-cache) :package))

;; (require 'cl-lib)
;; (require 'package)

;; ;;;; Package setup and additional utility functions

;; ;; accessing a package repo over https on Windows is a no go, so we
;; ;; fallback to http there
;; (if (and (>= emacs-major-version 27) (>= emacs-minor-version 1))
;;   (if (eq system-type 'windows-nt)
;;       (add-to-list 'package-archives
;;                    '("melpa" . "http://melpa.org/packages/") t)
;;     (add-to-list 'package-archives
;;                  '("melpa" . "https://melpa.org/packages/") t)))

;; ;; load the pinned packages
;; (let ((prelude-pinned-packages-file (expand-file-name "prelude-pinned-packages.el" prelude-dir)))
;;   (if (file-exists-p prelude-pinned-packages-file)
;;       (load prelude-pinned-packages-file)))

;; ;; set package-user-dir to be relative to Prelude install path
;; (setq package-user-dir (expand-file-name "elpa" prelude-dir))
;; (package-initialize)

;; ;; install & enable use-package
;; (unless (package-installed-p 'use-package)
;;   ;; emacs 26.1 can't install package use-package if not do refresh
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-verbose t)

;; (defvar prelude-packages
;;   '(ace-window
;;     ag
;;     avy
;;     anzu
;;     browse-kill-ring
;;     crux
;;     discover-my-major
;;     diff-hl
;;     diminish
;;     easy-kill
;;     editorconfig
;;     epl
;;     expand-region
;;     flycheck
;;     gist
;;     git-timemachine
;;     git-modes
;;     guru-mode
;;     hl-todo
;;     imenu-anywhere
;;     projectile
;;     magit
;;     move-text
;;     nlinum
;;     operate-on-number
;;     smartrep
;;     super-save
;;     undo-tree
;;     volatile-highlights
;;     which-key
;;     zenburn-theme
;;     zop-to-char)
;;   "A list of packages to ensure are installed at launch.")

;; (defun prelude-packages-installed-p ()
;;   "Check if all packages in `prelude-packages' are installed."
;;   (cl-every #'package-installed-p prelude-packages))

;; (defun prelude-require-package (package)
;;   "Install PACKAGE unless already installed."
;;   (unless (memq package prelude-packages)
;;     (add-to-list 'prelude-packages package))
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; (defun prelude-require-packages (packages)
;;   "Ensure PACKAGES are installed.
;; Missing packages are installed automatically."
;;   (mapc #'prelude-require-package packages))

;; (defun prelude-install-packages ()
;;   "Install all packages listed in `prelude-packages'."
;;   (unless (prelude-packages-installed-p)
;;     ;; check for new packages (package versions)
;;     (message "%s" "Emacs Prelude is now refreshing its package database...")
;;     (package-refresh-contents)
;;     (message "%s" " done.")
;;     ;; install the missing packages
;;     (prelude-require-packages prelude-packages)))

;; ;; run package installation
;; (prelude-install-packages)

;; (defun prelude-list-foreign-packages ()
;;   "Browse third-party packages not bundled with Prelude.

;; Behaves similarly to `package-list-packages', but shows only the packages that
;; are installed and are not in `prelude-packages'.  Useful for
;; removing unwanted packages."
;;   (interactive)
;;   (package-show-package-list
;;    (cl-set-difference package-activated-list prelude-packages)))

(defvar prelude-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

(defmacro prelude-auto-install (extension package mode)
  "Установка по требованию - когда файл с EXTENSION будет открыт, то
выполнится автоматическая установка PACKAGE (при условии что его
не было). В конце файл открывается в режиме MODE.

Такое поведение удаётся достич благодаря auto-mode-alist.
То есть, для определённого расширения устанавливается режим
работы и функция по установке необходимого расширения. Функция
срабатывает именно в тот момент когда файл с расширением начнёт
открываться, поэтому добавлена дополнительная проверка наличия
пакета, что бы избежать случайной повторной установки."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (prelude-check-installed ',package)
                                   (straight-use-package ',package))
                                 (,mode)))))

;; Формируется список расширений и модулей которые ещё не установлены
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (prelude-check-installed package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(provide 'prelude-packages)

;;; prelude-packages.el ends here
