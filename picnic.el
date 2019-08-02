(require 'transient)

(defconst picnic/tabulated-list-format
  [("Name" 50 t)
   ("Ready" 10 t)
   ("Status" 20 t)
   ("Restarts" 10 t)
   ("Age" 15 t)]
  "List format.")

(defconst picnic/env-staging "staging")
(defconst picnic/env-production "production")

(defun picnic/kubectl (env)
  "Utility function to prefix the kubectl command for ENV."
  (cond
    ((equal env picnic/env-production) "pkubectl")
    (t "skubectl")))

(defun picnic/buffer-name ()
  "Return picnic buffer name."
  (concat "*picnic/kube*"))

(defvar picnic/log-tail-n "100"
  "Number of lines to tail.")

(defun picnic/create-or-pop-to-buffer (name)
  (unless (get-buffer name)
      (get-buffer-create name))
  (pop-to-buffer name))

(defconst picnic/status-colors
  '(("Running" . "green")
    ("Error" . "red")
    ("Completed" . "yellow")
    ("CrashLoopBackOff" . "red")
    ("Terminating" . "blue"))
  "Associative list of status to color.")

(defun picnic/propertize-status (status)
  "Return the status in proper font color. STATUS is the pod status string."
  (let ((pair (cdr (assoc status picnic/status-colors))))
    (if pair
        (propertize status 'font-lock-face `(:foreground ,pair))
      status)))

(defun picnic/list-entries (env)
  "Create the entries for the service list."
  (let ((temp (list)))
    (with-temp-buffer
      (insert (shell-command-to-string (concat (picnic/kubectl env) " get pods --no-headers=true")))
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-z0-9\-]+\\) +\\([0-9]+/[0-9]+\\) +\\(\\w+\\) +\\([0-9]+\\) +\\([0-9a-z]+\\)$" (point-max) t)
        (setq temp (append temp (list (list (match-string 1) (vector (match-string 1) (match-string 2) (picnic/propertize-status (match-string 3)) (match-string 4) (match-string 5)))))))
      )
    temp))


(defun picnic/exec (env buffer-name async args)
  "Utility function to run commands in the proper context and namespace.
\"BUFFER-NAME\" is the buffer-name. Default to *picnic-command*.
ASYNC is a bool. If true will run async.
ARGS is a ist of arguments."
  (when (equal buffer-name "")
    (setq buffer-name "*picnic-command*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
    (apply #'start-process buffer-name buffer-name (picnic/kubectl env) args)
    (apply #'call-process (picnic/kubectl env) nil buffer-name nil args))
  (pop-to-buffer buffer-name))

(defun picnic/default-tail-arg (args)
  "Ugly function to make sure that there is at least the default tail.
ARGS is the arg list from transient."
  (if (car (remove nil (mapcar (lambda (x)
                                 (string-prefix-p "--tail=" x)) args)))
      args
    (append args (list (concat "--tail=" picnic/log-tail-n)))))

(defun picnic/get-pod-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry) 0))

(defun picnic/get-containers (env pod-name)
  "List the containers in a pod.
POD-NAME is the name of the pod."
  (split-string
   (shell-command-to-string
     (format "%s get pod %s -o jsonpath='{.spec.containers[*].name}'" (picnic/kubectl env) pod-name)) " "))

(define-infix-argument picnic/log-pod-popup:--tail ()
  :description "Tail"
  :class 'transient-option
  :shortarg "-n"
  :argument "--tail=")

(define-transient-command picnic/log-pod-popup ()
  "Kubel Log Menu"
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-p" "Previous" "-p")
   (picnic/log-pod-popup:--tail)]
  ["Actions"
   ("l" "Tail pod logs" kubel-get-pod-logs)])

(defun picnic/get-pod-logs (env &optional args)
  "Get the last N logs of the pod under the cursor.
ARGS is the arguments list from transient."
  (interactive
    (list (transient-args 'picnic/log-pod-popup)))
  (let* ((pod (picnic/get-pod-under-cursor))
          (containers (picnic/get-containers env pod))
          (container (if (equal (length containers) 1)
                       (car containers)
                       (completing-read "Select container: " containers)))
          (buffer-name (format "*%s - logs - %s - %s*" (picnic/kubectl env) pod container))
          (async nil))
    (when (member "-f" args)
      (setq async t))
    (picnic/exec env buffer-name async
      (append '("logs") (picnic/default-tail-arg args) (list pod container)))))

(define-derived-mode picnic-kube-mode tabulated-list-mode "Picnic Kube"
  "Special mode for picnic buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq major-mode 'picnic-kube-mode)
  (hl-line-mode 1)
  (run-mode-hooks 'picnic-mode-hook))

(defun picnic/get-pods (env)
  (picnic/create-or-pop-to-buffer (picnic/buffer-name))
  (setq tabulated-list-format picnic/tabulated-list-format)
  (setq tabulated-list-entries (lambda () (picnic/list-entries env)))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (picnic-kube-mode))

(defun picnic/todo ()
  (interactive)
  (message "TODO"))

(defun pye/run-in-term (buffer-name cmd &optional args)
  "Runs foo in a `term' buffer."
  (let* ((switches (split-string-and-unquote args))
         (termbuf (apply 'make-term buffer-name cmd nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

(defun picnic/diff ()
  (interactive)
  (let
    ((default-directory (projectile-project-root))
      (picnic/diff-buffer-name "*picnic/diff*"))
    (setenv "EDITOR" "emacs -Q")
    (pye/run-in-term picnic/diff-buffer-name "make" "diff")))

(defun picnic/make (command &optional args)
  (let
    ((default-directory (projectile-project-root))
      (picnic/land-buffer-name "*picnic*"))
    (picnic/create-or-pop-to-buffer picnic/land-buffer-name)
    (start-process "land" picnic/land-buffer-name "make" command)))

(defun picnic/land ()
  (interactive)
  (picnic/make "land"))

(defun picnic/release ()
  (interactive)
  (picnic/make "release"))

(defun picnic/get-staging-pods ()
  (interactive)
  (picnic/get-pods picnic/env-staging))

(defun picnic/get-production-pods ()
  (interactive)
  (picnic/get-pods picnic/env-production))

(define-transient-command picnic ()
  "Picnic🍏 🍎 🍐 🍊 🍋 🍌 🍉 🍇 🍓 🍈 🍒 🍑 🍍 🍅 🍆 🥑"
  ["Dev"
    ("d" "Diff" picnic/diff)
    ("l" "Land" picnic/make)
    ("r" "Release" picnic/release)]
  ["Kube"
    ("p p" "Prod Pods" picnic/get-production-pods)
    ("s p" "Staging Pods" picnic/get-staging-pods)])

(provide 'picnic)
;;; picnic.el ends here