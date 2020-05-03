(require 'transient)
(require 'json)

(defconst blanket/env-staging "staging")
(defconst blanket/env-production "production")
(defconst blanket/shell-buffer-name "blanket/shell")

(defconst blanket/shell-banner-message
  "\n.______    __    ______ .__   __.  __    ______\n|   _  \\  |  |  /      ||  \\ |  | |  |  /      |\n|  |_)  | |  | |  ,----'|   \\|  | |  | |  ,----'\n|   ___/  |  | |  |     |  . `  | |  | |  |\n|  |      |  | |  `----.|  |\\   | |  | |  `----.\n| _|      |__|  \\______||__| \\__| |__|  \\______|\n\n"

  "Picnic banner. Tip: use string-edit")

(defun blanket/create-or-pop-to-buffer (name)
  "Create or find existing buffer by matching NAME."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer name))

(defun blanket/upsert-eshell-buffer (buffer-name)
  "Initialize eshell buffer."
  (let ((eshell-buffer-exists (member buffer-name
                                (mapcar (lambda (buf) (buffer-name buf))
                                  (buffer-list))))
        (eshell-banner-message blanket/shell-banner-message))
    (if eshell-buffer-exists
      (pop-to-buffer buffer-name)
      (progn
        (eshell 99)
        (rename-buffer buffer-name)))))

(defun blanket/repo-root ()
  "Get picnic project root directory"
  (vc-call-backend 'git 'root default-directory))

;;;;;;;;;;;;;;;;
;; Kubernetes ;;
;;;;;;;;;;;;;;;;

(defface blanket/container-image-face
  '((t :foreground "SteelBlue"
       :weight bold
      :underline t))
  "Face for docker image name")

(defun blanket/kubectl (env)
  "Utility function to prefix the kubectl command for ENV."
  (cond
    ((equal env blanket/env-production) "pkubectl")
    (t "skubectl")))

(defconst blanket/status-colors
  '(("Running" . "green")
    ("Error" . "red")
    ("Completed" . "yellow")
    ("CrashLoopBackOff" . "red")
    ("Terminating" . "blue"))
  "Associative list of status to color.")

(defconst blanket/kube-pod-list-format
  [("Name" 50 t)
   ("Ready" 10 t)
   ("Status" 20 t)
   ("Restarts" 10 t)
   ("Age" 15 t)]
  "Pod list format.")

(defconst blanket/kube-service-list-format
  [("Name" 50 t)
   ("Type" 15 t)
   ("External-IP" 15 t)
   ("Port" 20 t)
   ("Age" 15 t)]
  "Service list format.")

(defvar blanket/log-tail-n "100"
  "Number of lines to tail.")

(defun blanket/propertize-status (status)
  "Return the status in proper font color. STATUS is the pod status string."
  (let ((pair (cdr (assoc status blanket/status-colors))))
    (if pair
        (propertize status 'font-lock-face `(:foreground ,pair))
      status)))

(defun blanket/list-pod-entries (env)
  "Create the entries for the pod list."
  (let ((temp
          (list
            (list
              "env"
              (vector
                (propertize (format "%s @ %s" env (current-time-string)) 'font-lock-face `(:foreground "black" :background "white")) "" "" "" "" ""))))
         (column-regex "^\\([a-z0-9\-]+\\) +\\([0-9]+/[0-9]+\\) +\\(\\w+\\) +\\([0-9]+\\) +\\([0-9a-z]+\\)$"))
    (with-temp-buffer
      (insert (shell-command-to-string (concat (blanket/kubectl env) " get pods --no-headers=true")))
      (goto-char (point-min))
      (while (re-search-forward column-regex (point-max) t)
        (setq temp
          (append
            temp
            (list
              (list
                (match-string 1)
                (vector
                  (match-string 1)
                  (match-string 2)
                  (blanket/propertize-status (match-string 3))
                  (match-string 4)
                  (match-string 5))))))))
    temp))


(defun blanket/list-service-entries (env)
  "Create the entries for the service list."
  (let ((temp
          (list
            (list
              "env"
              (vector
                (propertize env 'font-lock-face `(:foreground "black" :background "white")) "" "" "" "" ""))))
         (column-regex "^\\([a-z0-9-]+\\) +\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\)$"))
    (with-temp-buffer
      (insert (shell-command-to-string (concat (blanket/kubectl env) " get services --no-headers=true")))
      (goto-char (point-min))
      (while (re-search-forward column-regex (point-max) t)
        (setq temp
          (append
            temp
            (list
              (list
                (match-string 1)
                (vector
                  (match-string 1)
                  (match-string 2)
                  (match-string 4)
                  (match-string 5)
                  (match-string 6))))))))
    temp))


(defun blanket/exec (env buffer-name async args)
  "Utility function to run commands in the proper context and namespace.
\"BUFFER-NAME\" is the buffer-name. Default to *blanket-command*.
ASYNC is a bool. If true will run async.
ARGS is a ist of arguments."
  (message args)
  (when (equal buffer-name "")
    (setq buffer-name "*blanket-command*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
    (apply #'start-process buffer-name buffer-name (blanket/kubectl env) args)
    (apply #'call-process (blanket/kubectl env) nil buffer-name nil args))
  (pop-to-buffer buffer-name))

(defun blanket/default-tail-arg (args)
  "Ugly function to make sure that there is at least the default tail.
ARGS is the arg list from transient."
  (if (car (remove nil (mapcar (lambda (x)
                                 (string-prefix-p "--tail=" x)) args)))
      args
    (append args (list (concat "--tail=" blanket/log-tail-n)))))

(defun blanket/get-pod-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry (point)) 0))

(defun blanket/get-containers (env pod-name)
  "List the containers in a pod.
POD-NAME is the name of the pod."
  (split-string
   (shell-command-to-string
     (format "%s get pod %s -o jsonpath='{.spec.containers[*].name}'" (blanket/kubectl env) pod-name)) " "))

(define-infix-argument blanket/log-pod-popup:--tail ()
  :description "Tail"
  :class 'transient-option
  :shortarg "-n"
  :argument "--tail=")

(define-transient-command blanket/log-pod-popup ()
  "Log Menu"
  [:description "Arguments"
    ("-f" "Follow" "-f")
    ("-p" "Previous" "-p")
    (blanket/log-pod-popup:--tail)]
  [:description "Actions"
    ("l" "Tail pod logs" blanket/get-pod-logs)]
  (interactive)
  (transient-setup 'blanket/log-pod-popup nil nil))

(defun blanket/get-pod-logs (env &optional args)
  "Get the last N logs of the pod under the cursor.
ARGS is the arguments list from transient."
  (interactive
    (list (transient-args 'blanket/log-pod-popup)))
  (let* ((pod (blanket/get-pod-under-cursor))
         (containers (blanket/get-containers env pod))
         (container (if (equal (length containers) 1)
                      (car containers)
                      (completing-read "Select container: " containers)))
         (buffer-name (format "*%s - logs - %s - %s*" (blanket/kubectl env) pod container))
         (async nil))
    (when (member "-f" args)
      (setq async t))
    (blanket/exec env buffer-name async
      (append '("logs") (blanket/default-tail-arg args) (list pod "-c" container)))))

(defun blanket/get-pods (env)
  "Get a list of pods from kubernetes cluster corresponding to ENV."
  (blanket/create-or-pop-to-buffer "blanket/kube-pods")
  (setq tabulated-list-format blanket/kube-pod-list-format)
  (setq tabulated-list-entries (blanket/list-pod-entries env))
  (setq tabulated-list-revert-hook (lambda () (setq tabulated-list-entries (blanket/list-pod-entries blanket-kube-env))))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (blanket-kube-pod-mode)
  (with-current-buffer "blanket/kube-pods"
    (make-local-variable 'blanket-kube-env)
    (setq blanket-kube-env env)))

(defun blanket/get-services (env)
  "Get a list of services from kubernetes cluster corresponding to ENV."
  (blanket/create-or-pop-to-buffer "blanket/kube-services")
  (setq tabulated-list-format blanket/kube-service-list-format)
  (setq tabulated-list-entries (lambda () (blanket/list-service-entries env)))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (blanket-kube-service-mode)
  (with-current-buffer "blanket/kube-services"
    (make-local-variable 'blanket-kube-env)
    (setq blanket-kube-env env)))

(defun blanket/describe-pod-at-point ()
  (interactive)
  (let* ((pod-name (blanket/get-pod-under-cursor))
         (env blanket-kube-env)
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (pod-json
           (json-read-from-string
             (shell-command-to-string (concat (blanket/kubectl env) " get pod " pod-name " -o json"))))
         (pod-metadata (gethash "metadata" pod-json))
         (pod-spec (gethash "spec" pod-json))
         (pod-containers (gethash "containers" pod-spec)))
    (blanket/create-or-pop-to-buffer "blanket/kube-pod")
    (insert (format "%s.%s" (gethash "namespace" pod-metadata) (gethash "name" pod-metadata)))
    (newline)
    (newline)
    (insert "Containers:")
    (dolist (container pod-containers)
      (newline)
      (insert (format "%s -> %s"
                (gethash "name" container)
                (propertize (gethash "image" container) 'face 'blanket/container-image-face))))
    (read-only-mode)
    (goto-char (point-min))))

;; mode for pod list
(define-derived-mode blanket-kube-pod-mode tabulated-list-mode "Blanket Pods"
  "Special mode for blanket pod buffers."
  (buffer-disable-undo)
  (setq show-trailing-whitespace nil))

(defvar blanket-kube-pod-mode-map (make-sparse-keymap) "Keymap for blanket-kube-pod-mode")
(define-key blanket-kube-pod-mode-map (kbd "q") 'kill-current-buffer)
(define-key blanket-kube-pod-mode-map (kbd "l") 'blanket/log-pod-popup)
(define-key blanket-kube-pod-mode-map (kbd "RET") 'blanket/describe-pod-at-point)

;; mode for service list
(define-derived-mode blanket-kube-service-mode tabulated-list-mode "Blanket Services")
(defvar blanket-kube-service-mode-map (make-sparse-keymap) "Keymap for blanket-kube-service-mode")
(define-key blanket-kube-service-mode-map (kbd "q") 'kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun blanket/make (command &optional args)
  (interactive)
  (let
    ((blanket/shell-buffer-name "blanket/shipping"))
    (setenv "EDITOR" "emacs -Q")
    (blanket/dev-run-in-terminal
      (blanket/repo-root)
      (format "make %s" command))))

(defun blanket/show-recent-diff-tag ()
  "Extract most recent phabricator git tag within branch."
  (interactive)
  (let ((default-directory (blanket/repo-root)))
    (blanket/dev-run-in-terminal
      (blanket/repo-root)
      "git fetch -t && git describe --tags --abbrev=0 --match=phabricator/diff/\\*")))

(defun blanket/diff ()
  (interactive)
  (blanket/make "diff"))

(defun blanket/land ()
  (interactive)
  (blanket/make "land"))

(defun blanket/release ()
  (interactive)
  (blanket/make "release"))

(defun blanket/get-staging-pods ()
  (interactive)
  (blanket/get-pods blanket/env-staging))

(defun blanket/get-staging-services ()
  (interactive)
  (blanket/get-services blanket/env-staging))

(defun blanket/get-production-pods ()
  (interactive)
  (blanket/get-pods blanket/env-production))

(defun blanket/get-production-services ()
  (interactive)
  (blanket/get-services blanket/env-production))

(defun blanket/dev-select-migration-file ()
  (interactive)
  (file-name-nondirectory
    (read-file-name "Select migration: "
      (concat (blanket/repo-root) "packages/models/db/sequelize_migrations"))))

(defun blanket/dev-run-in-terminal (working-directory command)
  (interactive)
  (let ((buffer-name blanket/shell-buffer-name))
    (blanket/upsert-eshell-buffer buffer-name)
    (with-current-buffer buffer-name
      (eshell/cd working-directory)
      (eshell-return-to-prompt)
      (insert command)
      ;; (eshell-send-input)
      )))


(defun blanket/dev-run-in-app (env working-directory command)
  (interactive)
  (let ((app-container (string-trim-right (shell-command-to-string "docker ps -f name=picnic_picnichealth-app_1 -q"))))
    (print "enouhonehu'")
    (print (length app-container))
    (cond
      ((> (length app-container) 0)
        (blanket/dev-run-in-terminal
          (blanket/repo-root)
          (concat
            "docker exec -it picnic_picnichealth-app_1"
            (format " sh -c 'cd %s && %s'" working-directory command))))
      (t
        (blanket/dev-run-in-terminal
          (blanket/repo-root)
          (concat
            "docker run --rm"
            (format " --env-file %ssecrets/local.env" (blanket/repo-root))
            (format " --env NODE_ENV=%s" env)
            " --env USER"
            " --env DEV_LOCAL_IP=host.docker.internal"
            (format " -v %s:/picnic" (blanket/repo-root))
            " -v /picnic/node_modules"
            " -v /picnic/packages/app/node_modules"
            " -v /picnic/packages/libs/node_modules"
            " -v /picnic/packages/mission-design/node_modules"
            " -v /picnic/packages/models/node_modules"
            " picnic_picnichealth-app"
            (format " sh -c 'cd %s && %s'" working-directory command)))))))

(defun blanket/dev-exec-to-app ()
  (interactive)
  (blanket/dev-run-in-terminal
    default-directory
    "docker exec -it picnic_picnichealth-app_1 bash"))

;;;;;;;;;;;;;;;
;; Migration ;;
;;;;;;;;;;;;;;;

(defun blanket/dev-migration-create ()
  (interactive)
  (let ((name (read-string "Migration name (use kebab-case): ")))
    (blanket/dev-run-in-app
      "development"
      "/picnic/packages/models"
      (format "bin/sequelize migration:create --name %s" name))))

(defun blanket/dev-migration-up ()
  "Run migration."
  (interactive)
  (blanket/dev-run-in-app "development" "/picnic/packages/models" "bin/sequelize db:migrate"))

(define-transient-command blanket/dev-migration-down ()
  "Undo database migration on dev env."
  (interactive)
  (let ((migration-name (blanket/dev-select-migration-file)))
    (blanket/dev-run-in-app
      "development"
      "/picnic/packages/models"
      (format "bin/sequelize db:migrate:undo --name %s" migration-name))))

(define-transient-command blanket/dev-migration ()
  "Migration on dev env."
  [:description "Arguments"
    ("-n" "Undo by name" ("-n" "--name"))]
  [:description "Migration"
    ("c" "Create" blanket/dev-migration-create)
    ("r" "Run" blanket/dev-migration-up)
    ("u" "Undo" blanket/dev-migration-down)]
  (interactive)
  (transient-setup 'blanket/dev-migration nil nil))

(defun blanket/dev-migration-arguments ()
  (transient-args 'blanket/dev-migration))


;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;

(defun blanket/dev-test-app (&optional is-frontend)
  "Test app."
  (interactive)
  (let* ((blanket-root (blanket/repo-root))
         (test-file (replace-regexp-in-string
                      ".*\/packages"
                      "packages"
                      (read-file-name "Select test file: "
                        (cond
                          ((string-match blanket-root (buffer-file-name)) (buffer-file-name))
                          (t (concat blanket-root "packages/app/test")))))))
    (blanket/dev-run-in-terminal
      (blanket/repo-root)
      (concat
        "docker run --rm"
        (format " --env-file %ssecrets/local.env" blanket-root)
        " --env NODE_ENV=test"
        " --env USER"
        " --env DEV_LOCAL_IP=host.docker.internal"
        (format " -v %s:/picnic" blanket-root)
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

(defun blanket/dev-test-app-frontend ()
  (interactive)
  (blanket/dev-test-app t))

(defun blanket/dev-test-python-module (module)
  "Test a python module"
  (interactive)
  (blanket/dev-run-in-terminal
    (concat (blanket/repo-root) (format "python/picnic/%s" module))
    (format "../../bin/docker-test %s picnichealth/%s mount " module (string-inflection-kebab-case-function module))))

(defun blanket/dev-test-python-models ()
  (interactive)
  (blanket/dev-test-python-module "db_models"))

(defun blanket/dev-test-python-export-dataset ()
  (interactive)
  (blanket/dev-test-python-module "export_dataset"))

(defun blanket/dev-test-python-labelling ()
  (interactive)
  (blanket/dev-test-python-module "labelling"))

(defun blanket/dev-test-python-trialing ()
  (interactive)
  (blanket/dev-test-python-module "trialing"))

(defun blanket/dev-test-python-ui-action-logger ()
  (interactive)
  (blanket/dev-run-in-terminal
    (concat (blanket/repo-root) "python/picnic/ui_action_logger")
    (concat
      "docker run --rm"
      (format " --env-file %ssecrets/local.env" (blanket/repo-root))
      " --env PYTHON_ENV=test"
      " --env VERBOSE=0"
      " --env USER"
      (format " -v %s/python/picnic/db_models:/picnic/db_models" (blanket/repo-root))
      (format " -v %s/python/picnic/ui_action_logger:/picnic/ui_action_logger" (blanket/repo-root))
        " picnichealth/ui-action-logger/test"
        " sh -c 'pytest -q /picnic/'")))

(defun blanket/dev-test-export-dataset-tools ()
  "Test export-dataset-tools."
  (interactive)
  (let ((blanket-root (blanket/repo-root)))
    (blanket/dev-run-in-terminal
      (blanket/repo-root)
      (concat
        "docker run --rm"
        (format " --env-file %ssecrets/local.env" blanket-root)
        " --env PYTHON_ENV=test"
        (format " -v %spython/picnic/export_dataset:/picnic/export_dataset" blanket-root)
        (format " -v %spython/picnic/config:/picnic/config" blanket-root)
        (format " -v %spython/picnic/enums:/picnic/enums" blanket-root)
        (format " -v %spython/picnic/db_models:/picnic/db_models" blanket-root)
        (format " -v %spython/picnic/utils:/picnic/utils" blanket-root)
        (format " -v %spython/picnic/jobs:/picnic/jobs" blanket-root)
        (format " -v %spackages/export-dataset-tools:/picnic/export_dataset_tools" blanket-root)
        (format " -v %spackages/libs:/picnic/libs" blanket-root)
        " -v /picnic/export_dataset_tools/node_modules"
        " -v /picnic/libs/node_modules"
        " picnichealth/export-dataset"
        " sh -c 'cd /picnic/export_dataset_tools && make test'"))))

;;;;;;;;;;;;;;;
;; Main menu ;;
;;;;;;;;;;;;;;;

(define-transient-command blanket ()
  ["Development"
    ["Shipping"
      ("d" "Diff" blanket/diff)
      ("l" "Land" blanket/land)
      ("r" "Release" blanket/release)]
    ["Migration"
      ("m c" "Create" blanket/dev-migration-create)
      ("m r" "Run" blanket/dev-migration-up)
      ("m u" "Undo" blanket/dev-migration-down)]
    ["Testing javascript"
      ("t a" "app" blanket/dev-test-app)
      ("t f" "app/frontend" blanket/dev-test-app-frontend)
      ("t x" "export-dataset-tools" blanket/dev-test-export-dataset-tools)]
    ["Testing python"
      ("t m" "models" blanket/dev-test-python-models)
      ("t e" "export-dataset" blanket/dev-test-python-export-dataset)
      ("t l" "labelling" blanket/dev-test-python-labelling)
      ("t t" "trialing" blanket/dev-test-python-trialing)
      ("t u" "ui-action-logger" blanket/dev-test-python-ui-action-logger)]
    [("x" "Exec to app" blanket/dev-exec-to-app)
     ("y" "Show recent diff tag" blanket/show-recent-diff-tag)]]
  ["Staging"
    ("s p" "Pods" blanket/get-staging-pods)
    ("s s" "Services" blanket/get-staging-services)]
  ["Production"
    ("p p" "Pods" blanket/get-production-pods)
    ("p s" "Services" blanket/get-production-services)])

(provide 'blanket)
;;; blanket.el ends here
