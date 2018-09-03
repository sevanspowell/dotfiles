;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     yaml
     react
     haskell
     csv
     shaders
     octave
     asm
     helm
     erc
     python
     javascript
     html
     auto-completion
     emacs-lisp
     git
     lua
     markdown
     nixos
     (org :variables
          org-babel-load-languages '((emacs-lisp . t)
                                     (haskell . t)
                                     (latex . t))
          )
     syntax-checking
     version-control
     pandoc
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     swift
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "samdelion")))
     php
     finance
     semantic
     gtags
     purescript
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      grayscale-theme
                                      direnv
                                      dracula-theme
                                      color-theme-solarized
                                      seoul256-theme
                                      tao-theme
                                      gotham-theme
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    org-bullets
                                    fill-column-indicator
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         grayscale
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq web-mode-enable-current-column-highlight t)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq tramp-verbose 10)

  ;; term
  (set-face-attribute 'term-color-black nil :foreground "#2e2e2e" :background "#474747")
  (set-face-attribute 'term-color-red nil :foreground "#dca3a3" :background "#474747")
  (set-face-attribute 'term-color-green nil :foreground "#8fb28f" :background "#474747")
  (set-face-attribute 'term-color-yellow nil :foreground "#f0dfaf" :background "#474747")
  (set-face-attribute 'term-color-blue nil :foreground "#94bff3" :background "#474747")
  (set-face-attribute 'term-color-magenta nil :foreground "#ec93d3" :background "#474747")
  (set-face-attribute 'term-color-cyan nil :foreground "#93e0e3" :background "#474747")
  (set-face-attribute 'term-color-white nil :foreground "#b6b6b6" :background "#474747")
  '(term-default-fg-color ((t (:inherit term-color-white))))
  '(term-default-bg-color ((t (:inherit term-color-black))))

  ;; ansi-term colors
  (setq ansi-term-color-vector
    [term term-color-black term-color-red term-color-green term-color-yellow
      term-color-blue term-color-magenta term-color-cyan term-color-white])


  (setq-default js2-basic-offset 2
                js-indent-level 2)
  (setq json-reformat:indent-width 2)
  (setq psc-ide-use-npm-bin t)

  (local-set-key (kbd "RET") 'newline)

  (setq-default indent-tabs-mode nil)

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  (autoload 'org-invoice-report "org-invoice")
  (autoload 'org-dblock-write:invoice "org-invoice")

  ;; Change powerline separator
  (setq powerline-default-separator 'bar)

  ;; Minimal fringe
  (set-fringe-mode '(1 . 1))

  ;; Set fill column indicator
  (setq fill-column 80)
  (setq fill-column-indicator 1)

  ;; C-customizations
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)

  ;; Add /usr/local/bin to PATH
  (setq exec-path (append exec-path '("/usr/local/bin")))

  ;; Format code according to clang-format using TAB key
  (add-hook 'c++-mode-hook 'clang-format-bindings)
  (defun clang-format-bindings ()
    (define-key c++-mode-map [tab] 'clang-format-buffer))
  (add-hook 'c-mode-hook 'clang-format-bindings-c)
  (defun clang-format-bindings-c ()
    (define-key c-mode-map [tab] 'clang-format-buffer))
  (add-hook 'objc-mode-hook 'clang-format-bindings-objc)
  (defun clang-format-bindings-objc ()
    (define-key objc-mode-map [tab] 'clang-format-buffer))

  ;; Org-mode customizations from Harry Schwartz's configuration and
  ;; http://doc.norang.ca/org-mode.html
  ;; Use syntax highlighting in source blocks while editing
  (setq org-src-fontify-natively t)
  ;; Make TAB act as if it were issued in a buffer of the language's major mode.
  (setq org-src-tab-acts-natively t)
  ;; When editing a code snippet, use the current window rather than popping open a new one.
  (setq org-src-window-setup 'current-window)
  ;; Turn off org-indent-mode
  ;; (with-eval-after-load 'org (setq org-startup-indented nil))

  ;; (setq org-directory "~/org")

  ;; (defun org-file-path (filename)
  ;;   "Return the absolute address of an org file, given its relative name."
  ;;   (concat (file-name-as-directory org-directory) filename))

  ;; (setq org-inbox-file (org-file-path "inbox.org"))
  ;; (setq org-default-notes-file org-inbox-file)
  ;; (setq org-agenda-files (list "~/org"
                               ;; ))

  ;; (load-library "find-lisp")
  ;; (setq org-agenda-files
  ;;       (find-lisp-find-files "~/org" "\.org$"))

  ;; ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  ;; (setq org-refile-targets (quote ((nil :maxlevel . 9)
  ;;                                  (org-agenda-files :maxlevel . 9))))
  ;; (setq org-clock-out-remove-zero-time-clocks t)
  ;; (setq org-archive-mark-done nil)
  ;; ;; Archive based on month
  ;; (setq org-archive-location (concat "~/org/archive/" (format-time-string "%Y-%m" (current-time)) ".org_archive::"))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        (quote (("t" "Todo" entry (file org-inbox-file)
                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("n" "Note" entry (file org-inbox-file)
                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("m" "Meeting" entry (file org-inbox-file)
                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file org-inbox-file)
                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file org-inbox-file)
                 "* TODO %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                )))
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))
  (setq org-columns-default-format "%40ITEM(Task) %17Effort(Effort){:} %10CLOCKSUM")
  (setq org-tags-column -77)

  ;; Enable habit tracking
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)

  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)

  (setq org-enforce-todo-dependencies t)

  (setq org-todo-keywords
        (quote ((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold))))

  ;; Don't enable this because it breaks access to emacs from some devices
  (setq org-startup-with-inline-images nil)

  ;; Projects
  (setq org-html-head-include-default-style nil)

  (setq org-publish-project-alist
      ;; org-notes - org files are transformed to html
      ;; org-static - static files are just copied
      ;; org - both
      (quote (("org-notes"
              :base-directory "~/code/web/sevanspowell/"
              :base-extension "org"
              :publishing-directory "~/code/web/sevanspowell/docs"
              :recursive t
              :publishing-function org-html-publish-to-html
              :headline-levels 4
              :auto-preamble t
              :section-numbers nil
              :exclude "org-templates\\|docs"
              )
              ("org-static"
              :base-directory "~/code/web/sevanspowell/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
              :publishing-directory "~/code/web/sevanspowell/docs"
              :recursive t
              :publishing-function org-publish-attachment
              :section-numbers nil
              ;; :auto-sitemap t                  ; Generate sitemap.org automagically...
              ;; :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
              ;; :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
              )
              ("org" :components ("org-notes" "org-static"))

              ("remote-org-notes"
              :base-directory "~/code/web/sevanspowell/"
              :base-extension "org"
              :publishing-directory "~/code/web/sevanspowell/docs"
              :recursive t
              :publishing-function org-html-publish-to-html
              :headline-levels 4
              :auto-preamble t
              :section-numbers nil
              :exclude "org-templates\\|docs"
              )
              ("remote-org-static"
              :base-directory "~/code/web/sevanspowell/"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
              :publishing-directory "~/code/web/sevanspowell/docs"
              :recursive t
              :publishing-function org-publish-attachment
              :section-numbers nil
              ;; :auto-sitemap t                  ; Generate sitemap.org automagically...
              ;; :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
              ;; :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
              )
              ("remote-org" :components ("remote-org-notes" "remote-org-static"))
              )))

  (setq org-html-postamble-format '("en" "<p class=\"author\">Author: %a (%e)</p>\n<p class=\"date\">Date: %d</p>\n<p class=\"creator\">%c</p>\n"))

  (evil-leader/set-key-for-mode 'org-mode ";" 'org-columns)
  (evil-leader/set-key-for-mode 'objc-mode "ga" 'projectile-find-other-file)
  (evil-leader/set-key-for-mode 'purescript-mode "gs" 'purescript-pursuit)
  ;; End org-mode customizations

  (spacemacs/declare-prefix "o" "other")
  (spacemacs/set-leader-keys "ol" 'sort-lines)
  (spacemacs/set-leader-keys "of" 'fill-region)

  ;; Mode hooks
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))

  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
  )

  (defun my-web-mode-hook ()
    (electric-indent-mode -1)
    )

  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
  ;; Above is deprecated since Org 9.1 use:
  (setq org-duration-format (quote h:mm))
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :scope subtree :narrow 40))

  ;; Jordon Biondo
  ;; https://emacs.stackexchange.com/questions/2710/switching-between-window-layouts
  ;; Window stacks
  (defvar winstack-stack '()
    "A Stack holding window configurations.
Use `winstack-push' and
`winstack-pop' to modify it.")

  (defun winstack-push()
    "Push the current window configuration onto `winstack-stack'."
    (interactive)
    (if (and (window-configuration-p (first winstack-stack))
              (compare-window-configurations (first winstack-stack) (current-window-configuration)))
        (message "Current config already pushed")
      (progn (push (current-window-configuration) winstack-stack)
              (message (concat "pushed " (number-to-string
                                          (length (window-list (selected-frame)))) " frame config")))))

  (defun winstack-pop()
    "Pop the last window configuration off `winstack-stack' and apply it."
    (interactive)
    (if (first winstack-stack)
        (progn (set-window-configuration (pop winstack-stack))
                (message "popped"))
      (message "End of window stack")))


  (evil-leader/set-key "w'" 'winstack-pop)
  (evil-leader/set-key "w\"" 'winstack-push)

  ;; derived from rails-project:root from
  ;; https://github.com/remvee/emacs-rails
  (defun git-root ()
    "Return GIT_ROOT if this file is a part of a git repo,
  else return nil"
    (let ((curdir default-directory)
          (max 10)
          (found nil))
      (while (and (not found) (> max 0))
        (progn
          (if (file-directory-p (concat curdir ".git"))
              (progn
                (setq found t))
            (progn
              (setq curdir (concat curdir "../"))
              (setq max (- max 1))))))
      (if found (expand-file-name curdir))))

  (evil-leader/set-key "ps" 'git-grep)

  ;; (spacemacs|define-custom-layout "work"
  ;;  :binding "w"
  ;;  :body
  ;;  (find-file "~/org/projects/mylearning/_inbox.org")
  ;;  (split-window-below)
  ;;  (find-file "~/org/work.org")
  ;;  )

  (add-hook 'org-mode-hook (lambda () (linum-mode 0)))

  ;; Multiple cursors keybindings
  (evil-leader/set-key ".l" 'mc/edit-lines)
  (evil-leader/set-key ".n" 'mc/mark-next-like-this)
  (evil-leader/set-key ".N" 'mc/mark-previous-like-this)
  (evil-leader/set-key ".*" 'mc/mark-all-like-this)

  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium")

  (direnv-mode)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (grayscale-theme-theme yapfify yaml-mode xterm-color x86-lookup ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tao-theme tagedit swift-mode stickyfunc-enhance srefactor spaceline smeargle slim-mode shell-pop seoul256-theme scss-mode sass-mode restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode psci psc-ide popwin pip-requirements phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el paradox pandoc-mode ox-pandoc orgit org-projectile org-present org-pomodoro org-mime org-download open-junk-file nix-mode neotree nasm-mode multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode ledger-mode json-mode js2-refactor js-doc intero indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-nixos-options helm-mode-manager helm-make helm-hoogle helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets grayscale-theme gotham-theme google-translate golden-ratio gnuplot glsl-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags fuzzy flycheck-pos-tip flycheck-ledger flycheck-haskell flx-ido fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav dumb-jump drupal-mode dracula-theme disaster direnv diminish diff-hl define-word cython-mode csv-mode company-web company-tern company-statistics company-nixos-options company-ghci company-ghc company-cabal company-c-headers company-anaconda column-enforce-mode color-theme-solarized coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
