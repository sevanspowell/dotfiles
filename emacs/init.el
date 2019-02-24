(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

;; Package manager setup
(require 'package)

;; Don't load any packages on startup
(setq package-enable-at-startup nil)

(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Bootstrap `diminish`
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))
(eval-when-compile
  (require 'diminish))

;; Save custom variables to custom.el
(setq custom-file (expand-file-name "custom.el"
				    user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Load packages
;; (load-file (expand-file-name "conf/packages.el"
;; 			     user-emacs-directory))

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" default)))
 '(package-selected-packages
   (quote
    (diminish projectile base16-theme nix-mode haskell-mode haskell tramp-mode evil-magit magit doom-themes evil-surround which-key evil counsel swiper avy general use-package)))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
