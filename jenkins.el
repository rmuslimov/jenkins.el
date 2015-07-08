;;; jenkins.el --- small iteraction library for jenkins

;;; Commentary:

;; To proper installation, please define variables as it shown below:
;;
;; (setq jenkins-api-token "<api token can be found on user's configure page>")
;; (setq jenkins-hostname "<jenkins url>")
;; (setq jenkins-username "<your user name>")
;; (setq jenkins-viewname "<viewname>")

;;; Code:

(require 'dash)

(defconst jenkins-buffer-name
  "*jenkins-status*"
  "Name of jenkins buffer.")

(defconst jenkins-list-format
  [("#" 3 f :pad-right 2 :right-align t :col-source jenkins--render-indicator)
   ("Name" 35 t :col-source :name)
   ("Last success" 20 f :col-source :last-success)
   ("Last failed" 20 f :col-source :last-failed)]
  "Columns format.")

(defvar jenkins-api-token nil)
(defvar jenkins-hostname nil)
(defvar jenkins-username nil)
(defvar jenkins-viewname nil)

(defvar *jenkins-jobs-list* nil)

(defun jenkins-jobs-view-url (hostname viewname)
  "Jenkins url for get list of jobs in queue and their summaries"
  (format "%sview/%s/api/json?depth=2&tree=name,jobs[name,lastSuccessfulBuild[result,timestamp,duration,id],lastFailedBuild[result,timestamp,duration,id],lastBuild[result]]"
          hostname viewname))

(defun jenkins--setup-variables ()
  "Ask from user required variables if they not defined yet"
  (unless jenkins-hostname
    (setq jenkins-hostname (read-from-minibuffer "Jenkins hostname: ")))
  (unless jenkins-viewname
    (setq jenkins-viewname (read-from-minibuffer "Jenkins viewname: ")))
  (unless jenkins-username
    (setq jenkins-username (read-from-minibuffer "Jenkins username: ")))
  (unless jenkins-api-token
    (setq jenkins-api-token (read-from-minibuffer "Jenkins API Token: "))))

;; models

(defun jenkins--make-job (name result last-success last-failed)
  "Define regular jenkins job here."
  (list :name name
        :result result
        :last-success last-success
        :last-failed last-failed))

(defun jenkins--render-indicator (job)
  "Special indicator on main jenkins window."
  (let ((result (plist-get job :result)))
    (if (equal result "SUCCESS")
        (propertize "●" 'font-lock-face 'success)
      (propertize "●" 'font-lock-face 'error))))

(defun jenkins--convert-jobs-to-tabulated-format ()
  "Use global jenkins-jobs-list prepare data from table"
  (--map
   (list
    (plist-get it :name)
    (apply 'vector (-map
     (lambda (column)
       (let* ((args (nthcdr 3 column))
              (col-source (plist-get args :col-source)))
         (if (functionp col-source)
             (funcall col-source it)
           (plist-get it col-source))))
     jenkins-list-format)))
   *jenkins-jobs-list*
   ))

;;; actions

(defun jenkins:enter-job (&optional jobindex)
  (interactive)
  (message "Enter"))

(defun jenkins:restart-job (&optional jobindex)
  "Build jenkins job"
  (interactive)
  (let (index (tabulated-list-get-id))
    index))

(defun jenkins--parse-time-from (time-since timeitems)
  "Return list of text representations of time from event"
  (let* ((timeitem (car timeitems))
         (extracted-time (mod time-since (cdr timeitem)))
         (rest-time (/ (- time-since extracted-time) (cdr timeitem)))
         )
    (if (cdr timeitems)
        (apply 'list
               (list extracted-time (car timeitem))
               (jenkins--parse-time-from rest-time (cdr timeitems)))
      (list (list time-since (car timeitem)))
      )))

(defun jenkins--time-since-to-text (timestamp)
  "Returns beatiful string presenting time since event"
  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins--parse-time-from seconds-since timeitems))
         )
    (mapconcat
     (lambda (values) (apply 'format "%d%s" values))
     (-take 3 (reverse (--filter (not (= (car it) 0)) time-pairs)))
     ":")))

(defun jenkins--refresh-jobs-list ()
  "Force loading reloading jobs from jenkins and return them formatter for table"
  (jenkins-get-jobs-list)
  (jenkins--convert-jobs-to-tabulated-format))

(defun jenkins--parse-jobs-json (data)
  "Parse gotten data from jenkins, build entries for view"
  (let ((jobs (cdr (assoc 'jobs data))))
    (--map
     (apply 'jenkins--make-job
      (cdr (assoc 'name it))
      (cdr (assoc 'result (assoc 'lastBuild it)))
      (-map
       (lambda (x)
         (let ((val (cdr (assoc 'timestamp (assoc x it)))))
           (if val (jenkins--time-since-to-text (/ val 1000)) "")))
       (list 'lastSuccessfulBuild 'lastFailedBuild)))
     jobs)))

(defun jenkins-get-jobs-list ()
  "Get list of jobs from jenkins server"
  (let* ((url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Authorization" .
             ,(concat "Basic " (base64-encode-string (concat jenkins-username ":" jenkins-api-token))))))
         (response (with-current-buffer
                       (url-retrieve-synchronously (jenkins-jobs-view-url jenkins-hostname jenkins-viewname))
                     (goto-char (point-min))
                     (re-search-forward "^$")
                     (delete-region (point) (point-min))
                     (buffer-string)))
         (raw-data (json-read-from-string response))
         (data (jenkins--parse-jobs-json raw-data))
         )
    (setq *jenkins-jobs-list* data)))

;; helpers

(defun jenkins:visit-jenkins-web-page ()
  "Open jenkins web page using predefined variables."
  (interactive)
  (unless jenkins-hostname
    (setq jenkins-hostname (read-from-minibuffer "Jenkins hostname: ")))
  (browse-url jenkins-hostname))

(defvar jenkins-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'jenkins:restart-job)
    (define-key map (kbd "RET") 'jenkins:enter-job)
    map)
  "Jenkins status mode keymap.")

;; emacs major mode funcs and variables

(define-derived-mode jenkins-mode tabulated-list-mode "Jenkins"
  "Special mode for jenkins status buffer"
  (setq truncate-lines t)
  (kill-all-local-variables)
  (setq mode-name "Jenkins")
  (setq major-mode 'jenkins-mode)
  (use-local-map jenkins-mode-map)
  (hl-line-mode 1)
  (setq tabulated-list-format jenkins-list-format)
  (setq tabulated-list-entries 'jenkins--refresh-jobs-list)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun jenkins ()
  "Initialize jenkins buffer."
  (interactive)
  (jenkins--setup-variables)
  (switch-to-buffer-other-window jenkins-buffer-name)
  (erase-buffer)
  (setq buffer-read-only t)
  (jenkins-mode)
)

(provide 'jenkins)
;;; jenkins ends here
