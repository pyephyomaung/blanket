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

(defun picnic/upsert-ansi-term-buffer (buffer-name)
  "Initialize term in a `term' buffer."
  (ansi-term "bash" buffer-name))

(defun picnic/upsert-eshell-buffer (buffer-name)
  "Initialize eshell buffer."
  (let* ((eshell-buffer-exists (member buffer-name
                                 (mapcar (lambda (buf) (buffer-name buf))
                                   (buffer-list)))))
    (if eshell-buffer-exists
      (pop-to-buffer buffer-name)
      (progn
        (eshell 99)
        (rename-buffer buffer-name)))))

;;;;;;;;;;;;;;;;
;; Kubernetes ;;
;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun picnic/diff ()
  (interactive)
  (let
    ((default-directory (projectile-project-root))
      (picnic/diff-buffer-name "picnic/diff"))
    (setenv "EDITOR" "emacs -Q")
    (picnic/dev-run-in-terminal (projectile-project-root) "make diff")))

(defun picnic/make (command &optional args)
  (interactive)
  (let
    ((default-directory (projectile-project-root))
      (picnic/land-buffer-name "picnic"))
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

(defun picnic/dev-select-migration-file ()
  (interactive)
  (file-name-nondirectory
    (read-file-name "Select migration: " (concat (projectile-project-root) "packages/models/db/sequelize_migrations"))))

(defun picnic/dev-run-in-terminal (working-directory command)
  (interactive)
  (let ((buffer-name "*eshell*<picnic/dev-run-in-terminal>")
        (default-directory working-directory))
    (picnic/upsert-eshell-buffer buffer-name)
    (with-current-buffer buffer-name
      (eshell-return-to-prompt)
      (insert command))))

(defun picnic/dev-run-in-app (working-directory command)
  (interactive)
  (picnic/dev-run-in-terminal
    default-directory
    (format "docker exec -it -w %s picnic_picnichealth-app_1 %s" working-directory command)))

(defun picnic/dev-exec-to-app ()
  (interactive)
  (picnic/dev-run-in-terminal
    default-directory
    "docker exec -it picnic_picnichealth-app_1 bash"))

;;;;;;;;;;;;;;;
;; Migration ;;
;;;;;;;;;;;;;;;

(defun picnic/dev-migration-create ()
  (interactive)
  (let ((name (read-string "Migration name (use kebab-case): ")))
    (picnic/dev-run-in-app
      "/picnic/packages/models"
      (format "bin/sequelize migration:create --name %s" name))))

(defun picnic/dev-migration-up ()
  "Run migration."
  (interactive)
  (picnic/dev-run-in-app "/picnic/packages/models" "bin/sequelize db:migrate"))

(define-transient-command picnic/dev-migration-down ()
  "Undo database migration on dev env."
  (interactive)
  (let ((migration-name (picnic/dev-select-migration-file)))
    (picnic/dev-run-in-app
    "/picnic/packages/models"
    (format "bin/sequelize db:migrate:undo --name %s" migration-name))))

(define-transient-command picnic/dev-migration ()
  "Migration on dev env."
  [:description "Arguments"
   ("-n" "Undo by name" ("-n" "--name"))]
  [:description "Migration"
    ("c" "Create" picnic/dev-migration-create)
   ("r" "Run" picnic/dev-migration-up)
   ("u" "Undo" picnic/dev-migration-down)]
  (interactive)
  (transient-setup 'picnic/dev-migration nil nil))

(defun picnic/dev-migration-arguments ()
  (transient-args 'picnic/dev-migration))


;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;

(defun picnic/dev-testing-arguments ()
  (transient-args 'picnic/dev-testing))

(define-suffix-command picnic/dev-test-app (args &optional is-frontend)
  "Test app."
  (interactive (list (picnic/dev-testing-arguments)))
  (let* ((picnic-root (projectile-project-root))
         (test-file (replace-regexp-in-string
                      ".*\/packages"
                      "packages"
                      (read-file-name "Select test file: "
                        (cond
                          ((string-match picnic-root (buffer-file-name)) (buffer-file-name))
                          (t (concat picnic-root "packages/app/test")))))))
    (picnic/dev-run-in-terminal
      (projectile-project-root)
      (concat
        "docker run --rm"
        (format " --env-file %ssecrets/local.env" picnic-root)
        " --env NODE_ENV=test"
        " --env USER"
        " --env DEV_LOCAL_IP=host.docker.internal"
        (format " -v %s:/picnic" picnic-root)
        " -v /picnic/node_modules"
        " -v /picnic/packages/app/node_modules"
        " -v /picnic/packages/libs/node_modules"
        " -v /picnic/packages/mission-design/node_modules"
        " -v /picnic/packages/models/node_modules"
        " picnic_picnichealth-app"
        (cond
          ((eq is-frontend t)
            (format " sh -c 'cd /picnic/packages/app && /picnic/node_modules/.bin/jest %s'" test-file))
          (t
            (format " sh -c 'cd /picnic && make test-js-app TEST_FILES=%s'" test-file)))))))

(defun picnic/dev-test-app-frontend (args)
  (interactive (list (picnic/dev-testing-arguments)))
  (picnic/dev-test-app args t))


(define-suffix-command picnic/dev-test-export-dataset (args)
  "Test export_dataset."
  (interactive (list (picnic/dev-testing-arguments)))
  (picnic/dev-run-in-terminal
    (concat (projectile-project-root) "python/picnic/export_dataset")
    "../../bin/docker-test export_dataset picnichealth/export-dataset mount"))

(define-suffix-command picnic/dev-test-labelling (args)
  "Test labelling."
  (interactive (list (picnic/dev-testing-arguments)))
  (picnic/dev-run-in-terminal
    (concat (projectile-project-root) "python/picnic/labelling")
    "../../bin/docker-test labelling picnichealth/labelling mount"))

(define-suffix-command picnic/dev-test-export-dataset-tools (args)
  "Test export-dataset-tools."
  (interactive (list (picnic/dev-testing-arguments)))
  (let ((picnic-root (projectile-project-root)))
    (picnic/dev-run-in-terminal
      (projectile-project-root)
      (concat
        "docker run --rm"
        (format " --env-file %ssecrets/local.env" picnic-root)
        " --env PYTHON_ENV=test"
        (format " -v %s/python/picnic/export_dataset:/picnic/export_dataset" picnic-root)
        (format " -v %s/python/picnic/config:/picnic/config" picnic-root)
        (format " -v %s/python/picnic/enums:/picnic/enums" picnic-root)
        (format " -v %s/python/picnic/db_models:/picnic/db_models" picnic-root)
        (format " -v %s/python/picnic/utils:/picnic/utils" picnic-root)
        (format " -v %s/python/picnic/jobs:/picnic/jobs" picnic-root)
        (format " -v %s/packages/export-dataset-tools:/picnic/export_dataset_tools" picnic-root)
        (format " -v %s/packages/libs:/picnic/libs" picnic-root)
        " -v /picnic/export_dataset_tools/node_modules"
        " -v /picnic/libs/node_modules"
        " picnichealth/export-dataset"
        " sh -c 'cd /picnic/export_dataset_tools && make test'"))))

;;;;;;;;;;;;;;;
;; Main menu ;;
;;;;;;;;;;;;;;;

(define-transient-command picnic ()
  ["Development"
    ["Shipping"
      ("d" "Diff" picnic/diff)
      ("l" "Land" picnic/make)
      ("r" "Release" picnic/release)]
    ["Migration"
      ("m c" "Create" picnic/dev-migration-create)
      ("m r" "Run" picnic/dev-migration-up)
      ("m u" "Undo" picnic/dev-migration-down)]
    ["Testing"
      ("t a" "app" picnic/dev-test-app)
      ("t f" "app/frontend" picnic/dev-test-app-frontend)
      ("t l" "labelling" picnic/dev-test-labelling)
      ("t e" "export-dataset" picnic/dev-test-export-dataset)
      ("t t" "export-dataset-tools" picnic/dev-test-export-dataset-tools)]
    [("x" "Exec to app" picnic/dev-exec-to-app)]]
  ["Staging"
    ("s p" "Staging Pods" picnic/get-staging-pods)]
  ["Production"
    ("p p" "Prod Pods" picnic/get-production-pods)])

(provide 'picnic)
;;; picnic.el ends here



(defun test-transient ()
  (interactive)
  (transient-setup 'test-transient))
