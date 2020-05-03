(defvar blanket/gitlab-gql-endpoint "https://gitlab.picnichealth.com/api/graphql")
(defvar blanket/gitlab-gql-token (getenv "GITLAB_TOKEN"))
(defvar blanket/gitlab-username (getenv "GITLAB_USERNAME"))
(defvar blanket/gitlab-default-project-fullpath "team/picnic")

(defvar blanket/gitlab-issue-query "
query GetIssues($project: ID!, $assignee: String!) {
  project(fullPath: $project) {
    name,
  	issues (assigneeUsername: $assignee, state: opened) {
      nodes {
        iid,
        title,
        state,
        dueDate,
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

(defun blanket/gql-request (query &optional variables)
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
  (let*
    ((response (blanket/gql-request
                   blanket/gitlab-issue-query
                   (list
                     (cons "project" blanket/gitlab-default-project-fullpath)
                     (cons "assignee" blanket/gitlab-username))))
      (data (gethash "data" response))
      (project (gethash "project" data))
      (issues (gethash "nodes" (gethash "issues" project))))
    (with-current-buffer (generate-new-buffer "output")
      (insert (json-encode issues))
      (json-pretty-print-buffer))
    ))
(blanket/gitlab-fetch-issues)
