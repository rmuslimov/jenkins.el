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
  [("#" 3 f :pad-right 2 :right-align t)
   ("Name" 35 t)
   ("Last success" 20 f)
   ("Last failed" 20 f)]
  "Columns format.")

(defvar jenkins-api-token nil)
(defvar jenkins-hostname nil)
(defvar jenkins-username nil)
(defvar jenkins-viewname nil)

(defvar *jenkins-jobs-list* nil)

(defvar jenkins-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'jenkins:restart-job)
    ;; (define-key map (kbd "g") 'butler-refresh)
    map)
  "Jenkins status mode keymap.")

(defun jenkins:restart-job (&optional jobindex)
  "Build jenkins job"
  (interactive)
  (let (index (tabulated-list-get-id))
    index))

(defun jenkins:setup-variables ()
  "Ask from user required variables if they not defined yet"
  (unless jenkins-hostname
    (setq jenkins-hostname (read-from-minibuffer "Jenkins hostname: ")))
  (unless jenkins-viewname
    (setq jenkins-viewname (read-from-minibuffer "Jenkins viewname: ")))
  (unless jenkins-username
    (setq jenkins-username (read-from-minibuffer "Jenkins username: ")))
  (unless jenkins-api-token
    (setq jenkins-api-token (read-from-minibuffer "Jenkins API Token: "))))

(defun jenkins:-parse-time-from (time-since timeitems)
  "Return list of text representations of time from event"
  (let* ((timeitem (car timeitems))
         (extracted-time (mod time-since (cdr timeitem)))
         (rest-time (/ (- time-since extracted-time) (cdr timeitem)))
         )
    (if (cdr timeitems)
        (apply 'list
               (list extracted-time (car timeitem))
               (jenkins:-parse-time-from rest-time (cdr timeitems)))
      (list (list time-since (car timeitem)))
      )))

(defun jenkins:time-since-to-text (timestamp)
  "Returns beatiful string presenting time since event"
  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins:-parse-time-from seconds-since timeitems))
         )
    (mapconcat
     (lambda (values) (apply 'format "%d%s" values))
     (reverse (--filter (not (= (car it) 0)) time-pairs))
     ":")))

(defun jenkins:view-url (hostname viewname)
  "Jenkins url for get list of jobs in queue and their summaries"
  (format "%sview/%s/api/json?depth=2&tree=name,jobs[name,lastSuccessfulBuild[result,timestamp,duration,id],lastFailedBuild[result,timestamp,duration,id],lastBuild[result]]"
          hostname viewname))

(define-derived-mode jenkins-mode tabulated-list-mode "Jenkins"
  "Special mode for jenkins status buffer"
  (setq truncate-lines t)
  (kill-all-local-variables)
  (setq mode-name "Jenkins")
  (setq major-mode 'jenkins-mode)
  (use-local-map jenkins-mode-map)
  (hl-line-mode 1)
  (setq tabulated-list-format jenkins-list-format)
  (setq tabulated-list-entries 'jenkins:get-jobs-list)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun jenkins:process-data (data)
  "Parse gotten data from jenkins, build entries for view"
  (let ((jobs (cdr (assoc 'jobs data))))
    (-map
     (lambda (item)
       (list
        (cdr (assoc 'name item))
        (apply 'vector
               ;; status
               (if (equal (cdr (assoc 'result (assoc 'lastBuild item))) "SUCCESS")
                   (propertize "●" 'font-lock-face 'success)
                 (propertize "●" 'font-lock-face 'error))
               ;; name
               (cdr (assoc 'name item))
               ;; last build times
               (-map
                (lambda (x)
                  (let ((val (cdr (assoc 'timestamp (assoc x item)))))
                    (if val (jenkins:time-since-to-text (/ val 1000)) "")))
                (list 'lastSuccessfulBuild 'lastFailedBuild)))))
                jobs)))

(defun jenkins:get-jobs-list ()
  "Get list of jobs from jenkins server"
  (let* ((url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Authorization" .
             ,(concat "Basic " (base64-encode-string (concat jenkins-username ":" jenkins-api-token))))))
         (response (with-current-buffer
                       (url-retrieve-synchronously (jenkins:view-url jenkins-hostname jenkins-viewname))
                     (goto-char (point-min))
                     (re-search-forward "^$")
                     (delete-region (point) (point-min))
                     (buffer-string)))
         (raw-data (json-read-from-string response))
         (data (jenkins:process-data raw-data))
         )
    (progn
      (setq *jenkins-jobs-list* data)
      data)
    ))

(defun jenkins:visit-jenkins-web-page ()
  "Open jenkins web page using predefined variables."
  (interactive)
  (unless jenkins-hostname
    (setq jenkins-hostname (read-from-minibuffer "Jenkins hostname: ")))
  (browse-url jenkins-hostname))

(defun jenkins:render ()
  "Write all information about jenkins jobs."
  (erase-buffer)
  (setq buffer-read-only t)
  (jenkins-mode))

(defun jenkins ()
  "Initialize jenkins buffer."
  (interactive)
  (jenkins:setup-variables)
  (switch-to-buffer-other-window jenkins-buffer-name)
  (jenkins:render))

(provide 'jenkins)
;;; jenkins ends here
