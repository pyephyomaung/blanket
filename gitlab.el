(defvar blanket/gitlab-root "https://gitlab.picnichealth.com")
(defvar blanket/gitlab-gql-endpoint (concat blanket/gitlab-root "/api/graphql"))
(defvar blanket/gitlab-gql-token (getenv "GITLAB_TOKEN"))
(defvar blanket/gitlab-username (getenv "GITLAB_USERNAME"))
(defvar blanket/gitlab-default-project-fullpath "team/picnic")

(setq blanket/gitlab-issue-query "
query GetIssues($project: ID!, $assignee: String!) {
  project(fullPath: $project) {
    name,
  	issues (assigneeUsername: $assignee, state: opened) {
      nodes {
        iid,
        title,
        state,
        dueDate,
        webUrl,
        description,
        createdAt,
        labels {
          edges {
            node {
              id,
              title
            }
          }
        },
        createdAt,
        updatedAt
      }
    }
  }
}
")

(defun blanket/gitlab-gql-request (query &optional variables)
  ;; Query Gitlab api
  ;; Test:
  ;; (with-current-buffer (switch-to-buffer "output")
  ;;   (insert
  ;;     (json-encode
  ;;       (blanket/gitlab-gql-request
  ;;           blanket/gitlab-issue-query
  ;;           (list
  ;;             (cons "project" blanket/gitlab-default-project-fullpath)
  ;;             (cons "assignee" blanket/gitlab-username)))))
  ;;     (json-pretty-print-buffer))
  (let* ((url-request-method "POST")
          (url-request-extra-headers
            (list
              (cons "Content-Type" "application/json")
              (cons "Authorization" (format "Bearer %s" blanket/gitlab-gql-token))))
          (url-request-data
            (json-encode
              (list
                (cons "query" query)
                (cons "variables" (and variables (json-encode variables))))))
          (buffer (url-retrieve-synchronously blanket/gitlab-gql-endpoint)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (let ((json-object-type 'hash-table))
        (json-read)))))

(defun blanket/gitlab-fetch-issues ()
  "Return list of issues"
  (let*
    ((response
       (blanket/gitlab-gql-request
         blanket/gitlab-issue-query
         (list
           (cons "project" blanket/gitlab-default-project-fullpath)
           (cons "assignee" blanket/gitlab-username))))
      (data (gethash "data" response))
      (project (gethash "project" data))
      (issues (gethash "nodes" (gethash "issues" project))))
    ;; issues is a vector so convert to list and return
    (cl-map 'list 'identity issues)))

(defun blanket/gitlab-issues-to-org-raw-string ()
  "Generate org elements as strin for Gitlab issues."
  (let*
    ((issues (blanket/gitlab-fetch-issues)))
    (mapconcat
      (lambda (issue)
        (let*
          (
            (id (gethash "iid" issue))
            (title (gethash "title" issue))
            (state (gethash "state" issue))
            (link (gethash "webUrl" issue))
            (description (gethash "description" issue)))
          (concat
            "* TODO " (format "[[%s][%s]] " link id) title "\n"
            "  :PROPERTIES:\n"
            "  :ID: " id "\n"
            "  :LINK: " link "\n"
            "  :STATE: " state "\n"
            "  :END:\n"
            "  " description
            ))
        )
      ;; convert vector to list
      issues
      "\n")))

(defun blanket/gitlab-issue-to-org-element (issue)
  (let*
    ((id (gethash "iid" issue))
      (title (gethash "title" issue))
      (state (gethash "state" issue))
      (link (blanket/gitlab-issue-link id))
      (description (gethash "description" issue))
      (label-titles
        (cl-map
          'list
          (lambda (edge) (gethash "title" (gethash "node" edge)))
          (gethash "edges" (gethash "labels" issue))))
      (priority
        (cond
          ((seq-some (lambda (x) (string-prefix-p "P0" x)) label-titles) "[#A] ")
          ((seq-some (lambda (x) (string-prefix-p "P1" x)) label-titles) "[#A] ")
          ((seq-some (lambda (x) (string-prefix-p "P3" x)) label-titles) "[#C] ")
          (t ""))))
    (list
      (list
        'headline
        (list
          :level 1
          :title (format "%s[[%s][%s]] %s" priority link id title)
          :todo-keyword "TODO")
        (list
          'src-block
          (list
            :language "markdown"
            :value description)))
      )))

(defun blanket/gitlab-issue-to-org-document ()
  "Generate org elements as strin for Gitlab issues."
  (let*
    ((issues (blanket/gitlab-fetch-issues)))
    (org-element-interpret-data
      (append
        (list
          '(keyword (:key "TITLE" :value "Gitlab issues")))
        (mapcar 'blanket/gitlab-issue-to-org-element issues)))))

(defun blanket/gitlab-show-issues ()
  "Show Gitlab issues"
  (interactive)
  (with-current-buffer (switch-to-buffer "blanket/gitlab")
    (erase-buffer)
    (insert (blanket/gitlab-issue-to-org-document))
    (org-mode)))
