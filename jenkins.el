;; jenkins.el --- small iteraction library for jenkins

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
   ("Name" 35 t :col-source jenkins--render-name)
   ("Last success" 20 f :col-source :last-success)
   ("Last failed" 20 f :col-source :last-failed)]
  "List of columns for main jenkins jobs screen.")

(defvar jenkins-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'jenkins--call-build-job-from-main-screen)
    (define-key map (kbd "r") 'jenkins-restart-job)
    (define-key map (kbd "RET") 'jenkins-enter-job)
    map)
  "Jenkins main screen status mode keymap.")

(defvar jenkins-jobs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'jenkins-job-details-toggle)
    (define-key keymap (kbd "b") 'jenkins--call-build-job-from-job-screen)
    keymap)
  "Jenkins jobs status mode keymap.")

;; Setup this varialbles get proper working jenkins.el
(defvar jenkins-api-token nil)
(defvar jenkins-hostname nil)
(defvar jenkins-username nil)
(defvar jenkins-viewname nil)

(defvar *jenkins-jobs-list*
  nil
  "Data retrieved from jenkins for main jenkins screen.")

(defun jenkins--render-name (item)
  "Render jobname for main jenkins jobs screen."
  (let ((jobname (plist-get item :name))
        (progress (plist-get item :progress)))
    (if progress
        (format "%s %s"
                (propertize (format "%s%%" progress) 'font-lock-face 'warning)
                jobname)
      (format "%s" jobname))))

(defun jenkins-jobs-view-url (hostname viewname)
  "Jenkins url for get list of jobs in queue and their summaries."
  (format (concat
           "%sview/%s/api/json?depth=2&tree=name,jobs[name,"
           "lastSuccessfulBuild[result,timestamp,duration,id],"
           "lastFailedBuild[result,timestamp,duration,id],"
           "lastBuild[result,executor[progress]],"
           "lastCompletedBuild[result]]"
           )
          hostname viewname))

(defun jenkins-job-url (hostname jobname)
  "Job url in jenkins"
  (format (concat
           "%sjob/%s/"
           "api/json?depth=1&tree=builds"
           "[id,timestamp,result,url,building,"
           "culprits[fullName]]")
          hostname jobname))

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

(defun jenkins--make-job (name result progress last-success last-failed)
  "Define regular jenkins job here."
  (list :name name
        :result result
        :progress progress
        :last-success last-success
        :last-failed last-failed))

(defun jenkins--get-proper-face-for-result (result)
  "Simple function returning proper 'face for jenkins result."
  (let ((facemap (list '("SUCCESS" . 'success)
                       '("FAILURE" . 'error)
                       '("ABORTED" . 'warning))))
    (cdr (assoc result facemap))))

(defun jenkins--render-indicator (job)
  "Special indicator on main jenkins window."
  (propertize
   "●" 'font-lock-face
   (jenkins--get-proper-face-for-result
    (plist-get job :result))))

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
   (mapcar 'cdr *jenkins-jobs-list*)
   ))

;;; actions

(defun jenkins-enter-job (&optional jobindex)
  "Open each job detalization page"
  (interactive)
  (let ((jobindex (or jobindex (tabulated-list-get-id))))
    (jenkins-job-view jobindex)))

(defun jenkins-restart-job (&optional jobindex)
  "Build jenkins job"
  (interactive)
  (let (index (tabulated-list-get-id))
    index))

(defun jenkins--time-since-to-text (timestamp)
  "Returns beatiful string presenting time since event"
  (defun jenkins--parse-time-from (time-since timeitems)
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

  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins--parse-time-from seconds-since timeitems))
         )
    (mapconcat
     (lambda (values) (apply 'format "%d%s" values))
     (-take 3 (reverse (--filter (not (= (car it) 0)) time-pairs)))
     ":")
    ))

(defun jenkins--refresh-jobs-list ()
  "Force loading reloading jobs from jenkins and return them formatter for table"
  (jenkins-get-jobs-list)
  (jenkins--convert-jobs-to-tabulated-format))

(defun jenkins--retrieve-page-as-json (url)
  "Shortcut for jenkins api to return valid json"
  (let* ((url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Authorization" .
             ,(concat
               "Basic "
               (base64-encode-string
                (concat jenkins-username ":" jenkins-api-token)))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (json-read-from-string (buffer-string)))
    ))

(defun jenkins--extract-time-of-build (x buildname)
  "Helper defun to render timstamps"
  (let ((val (cdr (assoc 'timestamp (assoc buildname x)))))
    (if val (jenkins--time-since-to-text (/ val 1000)) "")))

(defun jenkins-get-jobs-list ()
  "Get list of jobs from jenkins server"
  (setq
   *jenkins-jobs-list*
   (let* ((jobs-url (jenkins-jobs-view-url jenkins-hostname jenkins-viewname))
          (raw-data (jenkins--retrieve-page-as-json jobs-url))
          (jobs (cdr (assoc 'jobs raw-data))))
     (--map
      (apply 'list (cdr (assoc 'name it))
             (jenkins--make-job
              (cdr (assoc 'name it))
              (cdr (assoc 'result (assoc 'lastCompletedBuild it)))
              (cdr (assoc 'progress (assoc 'executor (assoc 'lastBuild it))))
              (jenkins--extract-time-of-build it 'lastSuccessfulBuild)
              (jenkins--extract-time-of-build it 'lastFailedBuild)))
      jobs)
     )
   ))

(defun jenkins-get-job-details (jobname)
  "Make to certain job call"

  (defun convert-item (item)
    "Converting to item."
    (defun retrieve (attr item)
      (cdr (assoc attr (cdr item))))

    (list
     (string-to-number (retrieve 'id item))
     :author (let ((culprits (cdar item)))
               (if (> (length culprits) 0)
                 (cdar (aref culprits 0)) "---"))
     :url (retrieve 'url item)
     :timestring (jenkins--time-since-to-text (/ (retrieve 'timestamp item) 1000))
     :building (equal (retrieve 'building item) :json-true)
     :result (retrieve 'result item)
     ))

  (defun vector-take (N vec)
    (--map
     (aref vec it)
     (number-sequence 0 (1- (min  N (length vec))))
     ))

  (let* (
         (job-url (jenkins-job-url jenkins-hostname jobname))
         (raw-data (jenkins--retrieve-page-as-json job-url))
         (builds (-map 'convert-item (vector-take 25 (cdar raw-data))))
         (latestSuccessful
          (caar (--filter (equal (plist-get (cdr it) :result) "SUCCESS") builds)))
         (latestFailed
          (caar (--filter (equal (plist-get (cdr it) :result) "FAILURE") builds)))
         )
    (list :name jobname
          :builds builds
          :latestSuccessful latestSuccessful
          :latestFailed latestFailed
          )
    ))

;; helpers
(defun jenkins:visit-jenkins-web-page ()
  "Open jenkins web page using predefined variables."
  (interactive)
  (unless jenkins-hostname
    (setq jenkins-hostname (read-from-minibuffer "Jenkins hostname: ")))
  (browse-url jenkins-hostname))

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

(define-derived-mode jenkins-job-view-mode special-mode "jenkins-job"
  "Mode for viewing jenkins job details"
  ;; (view-mode 1)
  (font-lock-mode 1)

  ;; buffer defaults
  (setq-local local-jobname jobname)
  ;; (setq-local local-jobs-shown nil)
  (setq major-mode 'jenkins-job-view-mode)
  (use-local-map jenkins-jobs-mode-map)
  )

(defun jenkins-job-render (jobname)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((job (cdr (assoc jobname *jenkins-jobs-list*))))
    (insert
     (jenkins-job-details-screen jobname)
     ))
  (setq buffer-read-only t))

(defun jenkins-job-view (jobname)
  "Open job details screen."
  (interactive)
  (setq local-jobs-shown nil)
  (let ((details-buffer-name (format "*%s details*" jobname)))
    (switch-to-buffer details-buffer-name)
    (jenkins-job-render jobname)
    (jenkins-job-view-mode)
    ))

(defun jenkins-job-details-toggle ()
  (interactive)
  (setq-local local-jobs-shown (not local-jobs-shown))
  (jenkins-job-render local-jobname)
  (goto-line 4)
  )

(defun jenkins-job-call-build (jobname)
  "Call building job in jenkins."
  (interactive)
  (message (format "Building %s job started!" jobname)))

(defun jenkins--call-build-job-from-main-screen ()
  "Build job from main screen."
  (interactive)
  (jenkins-job-call-build (tabulated-list-get-id)))

(defun jenkins--call-build-job-from-job-screen ()
  "Call building job from job details in jenkins."
  (interactive)
  (jenkins-job-call-build local-jobname))

(defun jenkins-job-details-screen (jobname)
  "Jenkins job detailization screen, JOBNAME."
  (let* ((job-details (jenkins-get-job-details jobname))
         (jobname (plist-get job-details :name))
         (builds (plist-get job-details :builds))
         (latest (cdar builds))
         (latest-result (plist-get latest :result))
         (latestSuccessful
          (cdr (assoc (plist-get job-details :latestSuccessful) builds)))
         )
    (concat
     (format "Job name:\t%s\n" jobname)
     "Status:\t\t"
     (propertize
      (format "%s\n\n" latest-result)
      'face (jenkins--get-proper-face-for-result latest-result))
     (propertize
      (concat
       (format
        "Latest %s builds: "
        (length builds))
       (propertize ";; (press 1 to toggle)\n" 'font-lock-face 'italic)
       (if local-jobs-shown
           (apply 'concat
                  (--map
                   (propertize
                    (format "- Job #%s, %s %s\n"
                            (car it)
                            (plist-get (cdr it) :author)
                            (plist-get (cdr it) :timestring)
                            )
                    'face
                    (jenkins--get-proper-face-for-result
                     (plist-get (cdr it) :result)
                     ))
                   builds)))))
     "\nBuild now! "
     (propertize ";; (press b to Build)\n" 'font-lock-face 'italic)
     ))
  )

(defun jenkins ()
  "Initialize jenkins buffer."
  (interactive)
  (jenkins--setup-variables)
  (switch-to-buffer-other-window jenkins-buffer-name)
  (erase-buffer)
  (setq buffer-read-only t)
  (jenkins-mode))

(provide 'jenkins)
;;; jenkins ends here
