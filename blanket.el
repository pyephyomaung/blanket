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

;; load gitlab
(load (concat (file-name-directory (or load-file-name buffer-file-name)) "gitlab.el"))

;;;;;;;;;;;;;;;
;; Main menu ;;
;;;;;;;;;;;;;;;

(define-transient-command blanket ()
  [
    "Development"
    [
      "Shipping"
      ("g" "Gitlab issues" blanket/gitlab-show-issues)
    ]
    [
      "Migration"
      ("m c" "Create" blanket/dev-migration-create)
      ("m r" "Run" blanket/dev-migration-up)
      ("m u" "Undo" blanket/dev-migration-down)
    ]
    [
      "Testing javascript"
      ("t a" "app" blanket/dev-test-app)
      ("t f" "app/frontend" blanket/dev-test-app-frontend)
      ("t x" "export-dataset-tools" blanket/dev-test-export-dataset-tools)
    ]
    [
      "Testing python"
      ("t m" "models" blanket/dev-test-python-models)
      ("t e" "export-dataset" blanket/dev-test-python-export-dataset)
      ("t l" "labelling" blanket/dev-test-python-labelling)
      ("t t" "trialing" blanket/dev-test-python-trialing)
      ("t u" "ui-action-logger" blanket/dev-test-python-ui-action-logger)
    ]
  ]
)

(provide 'blanket)
;;; blanket.el ends here
