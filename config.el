(setq user-full-name "Tibor Pilz"
      user-mail-address "tibor@pilz.berlin")

(defun is-mac ()
  (string-equal system-type "darwin"))

(defun is-linux ()
  (string-equal system-type "gnu/linux"))

(defun is-workstation ()
  (string-equal (system-name) "archyMcArchstation"))

(setq font-scale-factor (if (is-workstation) 1.1 1.0))

(defun scale-font (size)
  (round (* size font-scale-factor)))

(when (is-mac) (setq doom-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 14))) nil)
;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 16) :weight 'light)
;;       doom-big-font (font-spec :family "FiraCode Nerd Font" :size (scale-font 24))
;;       doom-variable-pitch-font (font-spec :family "Open Sans" :size (scale-font 16))
;;       doom-serif-font (font-spec :family "FreeSerif" :weight 'light))

(setq display-line-numbers-type 'relative)

(setq tab-width 2)

(setq org-directory "~/org/")
(setq org-agenda-files (list org-directory))

(setq org-use-property-inheritance t)
(setq org-log-done 'time) ; Log time when task completes
(setq org-list-allow-alphabetical t)       ; a, A, a) A) list bullets)
(setq org-catch-invisible-edits 'smart) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{})


(setq org-return-follows-link 1)
(setq calendar-week-start-day 1) ;; start on monday
(setq org-agenda-include-diary t)

(add-hook 'org-mode-hook #'+org-pretty-mode)

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-fontify-quote-and-verse-blocks t)

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
         (?E . 'all-the-icons-blue)))

(use-package! ob-http
  :commands org-babel-execute:http)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noeweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name fie)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh" "ditaa"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(use-package! ox-gfm :after ox)

(setq org-export-headline-levels 5)

;(require 'ox-extra)
;(ox-extras-activate '(ignore-headlines))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(setq org-highlight-latex-and-related '(native script entities))

;(use-package! org-re-reveal)

(setq org-roam-directory "~/org")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive    )
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(setq org-roam-ui-open-on-start nil)

;; (use-package! org-gcal
;;   :config
;;   (setq org-gcal-client-id "CLIENT_ID"
;;         org-gcal-client-secret "CLIENT_SECRET"
;;         org-gcal-fetch-file-alit '(("tbrpilz@googlemail.com" . "~/org/schedule.org"))))

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(evil-define-command evil-buffer-org-new (count file)
  "creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "new empty ORG buffer" "o" #'evil-buffer-org-new))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(defadvice! org-edit-latex-env-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

(add-hook! markdown-mode (auto-fill-mode -1))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(setq projectile-project-search-path '(("~/Code/" . 1)))

(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package! svelte-mode
    :mode "\\.svelte\\'")

(with-eval-after-load 'web-mode
  (setq web-mode-script-padding 0))

(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(setq typescript-indent-level 2)

(use-package! nix-mode
  :mode "\\.nix\\'")

(add-hook! python-mode
  (advice-add 'python-pytest-file :before
              (lambda (&rest args)
                (setq-local python-pytest-executable
                            (executable-find "pytest")))))

(use-package! polymode)
(use-package! poly-markdown)

(setq company-idle-delay
      (lambda () (if (company-in-string-or-comment) nil 0.01)))

(setq company-minimum-prefix-length 1)

(setq company-selection-wrap-around t)

(setq company-backends '((company-capf company-dabbrev-code)))

(setq company-format-margin-function #'company-vscode-dark-icons-margin)

(setq copilot-node-executable
      (replace-regexp-in-string "\n" "" (shell-command-to-string ". $XDG_CONFIG_HOME/zsh/.zshrc; nvm which 16")))

(use-package! copilot
  :bind (("<backtab>" . 'copilot-accept-completion)))

(setq dap-python-debugger 'debugpy)

;;;###autoload
(defun +debugger/clear ()
  "Clear the debugger configuration from the doom-store."
  (interactive)
  (doom-store-rem (doom-project-root) "+debugger"))

(setq debugger-start-copy (symbol-function '+debugger/start))

;;;###autoload
(defun +debugger/repeat (arg)
  "Start the debugger."
  (interactive)
  (funcall debugger-start-copy arg))

;;;###autoload
(defun +debugger/start (arg)
  "Launch a debugger session.
Launches the last used debugger, if one exists. Otherwise, you will be prompted
for what debugger to use. If the prefix ARG is set, prompt anyway."
  (interactive "P")
  (message arg)
  (+debugger--set-config (+debugger-completing-read))
  (+debugger/start-last))

(defun get-window-with-file-buffer ()
  "Get the window with a file buffer."
  (seq-find (lambda (window)
              (buffer-file-name (window-buffer window)))
            (window-list)))

(defun reset-file-window-buffer ()
  "Reset the file window's buffer."
  (let ((window (get-window-with-file-buffer)))
    (when window
      (set-window-buffer window (window-buffer window)))))

(defun add-reset-file-window-buffer-hook (&rest args)
  "Add the reset-file-window-buffer function to the window-configuration-change-hook."
  (add-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(defun remove-reset-file-window-buffer-hook (&rest args)
    "Remove the reset-file-window-buffer function from the window-configuration-change-hook."
    (remove-hook 'window-configuration-change-hook 'reset-file-window-buffer))

(add-hook 'dap-mode-hook 'add-reset-file-window-buffer-hook)

(map! :leader
      (:prefix-map ("d" . "debugger")
       :desc "Debug" "d" #'dap-debug
       :desc "Next" "n" #'dap-next
       :desc "Step in" "i" #'dap-step-in
       :desc "Step out" "o" #'dap-step-out
       :desc "Continue" "c" #'dap-continue
       :desc "Restart" "r" #'dap-restart-frame
       :desc "Disconnect" "D" #'dap-disconnect
       :desc "Evaluate" "e" #'dap-eval
       :desc "Add Expression" "a" #'dap-ui-expressions-add
       (:prefix ("b" . "breakpoints")
        :desc "Toggle" "t" #'dap-breakpoint-toggle
        :desc "Add" "a" #'dap-breakpoint-add
        :desc "Delete" "d" #'dap-breakpoint-delete
        :desc "Set condition" "c" #'dap-breakpoint-condition
        :desc "Set log message" "m" #'dap-breakpoint-log-message
        :desc "Set hit condition" "h" #'dap-breakpoint-hit-condition)))

(setq flycheck-syntax-automatically '(save-mode-enable))

(setq lsp-completion-provider :capf)

(setq lsp-completion-show-detail nil)

(setq lsp-completion-show-kind t)

(map! :leader
      (:prefix ("c" . "code")
       :desc "Glance at documentation" "g" #'lsp-ui-doc-glance))

(setq lsp-lens-enable t)

(setq lsp-headerline-breadcrub-enable t)

(setq lsp-eldock-enable-hover nil)

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)

(setq doom-theme 'doom-nord-aurora)

;; (add-to-list 'load-path "~/Code/doom-nano-testing")
;; (require 'load-nano)
;; (setq doom-themes-treemacs-theme "doom-atom")

;; (use-package! nano-modeline
;;   :config
;;   (nano-modeline-mode 1))

(setq doom-modeline-vcs-max-length 50)

(setq doom-modeline-hud t)

(defun wjb/posframe-arghandler (buffer-or-name arg-name value)
  (let ((info '(:internal-border-width 2 :width 500 :height 48)))
    (or (plist-get info arg-name) value)))
(setq posframe-arghandler #'wjb/posframe-arghandler)

;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

(use-package! all-the-icons-ivy-rich
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode +1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.8))

(setq ivy-posframe-width 80)

(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(setq vertico-count-format nil)

(setq vertico-posframe-width 200)

(setq read-process-output-max (* 1024 1024)) ;; 1mb
