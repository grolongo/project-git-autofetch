;;; project-git-autofetch.el --- automatically fetch git repositories  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; project-git-autofetch is an adaptation of projectile-git-autofetch for
;; the builtin package project.el. Thus, it is mostly identical.
;; If anything, all credit goes to Andreas Müller.
;; https://github.com/andrmuel/projectile-git-autofetch

;; project-git-autofetch can be used to periodically fetch git
;; repositories. Depending on the value of
;; project-git-autofetch-projects, all open projects or all projects
;; known to project.el are fetched.

;;; Code:

(require 'project)

(defgroup project-git-autofetch nil
  "Automatically fetch git repositories."
  :group 'tools)

;;;###autoload
(define-minor-mode project-git-autofetch-mode
  "Fetch git repositories periodically."
  :init-value nil
  :group 'project-git-autofetch
  :require 'project-git-autofetch
  :global t
  :lighter " git-af"
  (if project-git-autofetch-mode
      (project-git-autofetch-setup)
      (project-git-autofetch-stop)))

(defcustom project-git-autofetch-projects 'open
  "Which projects to auto-fetch.

Selection of projects that should be automatically fetched."
  :package-version '(project-git-autofetch . "0.1.0")
  :group 'project-git-autofetch
  :safe (lambda (val) (memq val '(current open all)))
  :type '(choice (const open    :tag "All open projects.")
                 (const all     :tag "All known projects.")
                 (const nil     :tag "Nothing.")))

(defcustom project-git-autofetch-initial-delay 10
  "Initial delay in seconds before fetching."
  :package-version '(project-git-autofetch . "0.1.0")
  :group 'project-git-autofetch
  :type 'integer)

(defcustom project-git-autofetch-interval 300
  "Auto-fetch interval in seconds."
  :package-version '(project-git-autofetch . "0.1.0")
  :group 'project-git-autofetch
  :type 'integer)

(defcustom project-git-autofetch-timeout nil
  "Timeout in seconds for git processes or nil to disable."
  :package-version '(project-git-autofetch . "0.1.1")
  :group 'project-git-autofetch
  :type 'integer)

(defcustom project-git-autofetch-ping-host nil
  "Host to ping on order to check for Internet connectivity or nil to disable."
  :package-version '(project-git-autofetch . "0.1.2")
  :group 'project-git-autofetch
  :type 'string)

(defcustom project-git-autofetch-fetch-args '("--no-progress")
  "Additional arguments for git fetch."
  :package-version '(project-git-autofetch . "0.1.2")
  :group 'project-git-autofetch
  :type '(repeat string))

(defcustom project-git-autofetch-process-filter nil
  "Optional filter for fetch process."
  :package-version '(project-git-autofetch . "0.1.2")
  :group 'project-git-autofetch
  :type '(choice function (const nil)))

(defcustom project-git-autofetch-after-fetch-hook nil
  "Hooks to run after fetching a repository.
Note: runs in the git fetch buffer, so you can use project.el
functions like `project-root` to determine project
parameters."
  :group 'project-git-autofetch
  :type 'hook)

(defcustom project-git-autofetch-after-successful-fetch-hook nil
  "Hooks to run after sucessfully fetching a repository.
In contrast to `project-git-autofetch-after-fetch-hook`, these
hooks only run when new git objects were fetched.
Note: runs in the git fetch buffer, so you can use project.el
functions like `project-root` to determine project
parameters."
  :group 'project-git-autofetch
  :type 'hook)

(defun project-git-autofetch-sentinel (process _)
  "Handle the state of PROCESS."
  (unless (process-live-p process)
    (let ((buffer (process-buffer process))
          (default-directory (process-get process 'project-project)))
      (with-current-buffer buffer
        (run-hooks 'project-git-autofetch-after-fetch-hook)
        (when (> (buffer-size) 0)
          (run-hooks 'project-git-autofetch-after-successful-fetch-hook)))
      (delete-process process)
      (kill-buffer buffer))))

(defun project-git-autofetch-run ()
  "Fetch all repositories."
  (if project-git-autofetch-ping-host
      (make-process :name "project-git-autofetch-ping"
                    :buffer "*project-git-autofetch-ping"
                    :command `("ping" "-c" "1" "-W" "3" ,project-git-autofetch-ping-host)
                    :sentinel 'project-git-autofetch--ping-sentinel
                    :noquery t)
    (project-git-autofetch--work)))

(defun project-git-autofetch--ping-sentinel (process event)
  "Sentinel function for PROCESS to check ping success given EVENT."
  (when (string= "finished\n" event)
    (project-git-autofetch--work))
  (let ((buffer (process-buffer process)))
    (when (not (get-buffer-process buffer))
      (delete-process process)
      (kill-buffer buffer))))

(defun project-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (delete-dups
   (delq nil
         (mapcar (lambda (buffer)
                   (with-current-buffer buffer
                     (when-let ((project (project-current)))
                       (when (string-prefix-p (file-truename (project-root project))
                                              (file-truename default-directory))
                         (abbreviate-file-name (project-root project))))))
                 (buffer-list)))))

(defun project-git-autofetch--work ()
  "Worker function to fetch all repositories."
  (let ((projects (cond
                   ((eq project-git-autofetch-projects 'open)
                    (project-open-projects))
                   ((eq project-git-autofetch-projects 'all)
                    (project-known-project-roots))
                   (t nil))))
    (dolist (project projects)
      (let ((default-directory project))
        (when (and (file-directory-p ".git")
                   (car (ignore-errors
                          (process-lines "git" "config" "--get" "remote.origin.url"))))
          (let* ((buffer (generate-new-buffer " *git-fetch"))
                 (process
                  (apply #'start-process "git-fetch" buffer "git" "fetch" project-git-autofetch-fetch-args)))
            (process-put process 'project-project project)
            (when project-git-autofetch-process-filter
              (set-process-filter process project-git-autofetch-process-filter))
            (set-process-query-on-exit-flag process nil)
            (set-process-sentinel process #'project-git-autofetch-sentinel)
            (when project-git-autofetch-timeout
              (add-timeout project-git-autofetch-timeout 'project-git-autofetch-timeout-handler process))))))))

(defun project-git-autofetch-timeout-handler (process)
  "Timeout handler to kill slow or blocked PROCESS."
  (delete-process process))

(defvar project-git-autofetch-timer nil
  "Timer object for git fetches.")

(defun project-git-autofetch-setup ()
  "Set up timers to periodically fetch repositories."
  (interactive)
  (unless (timerp project-git-autofetch-timer)
    (setq project-git-autofetch-timer
          (run-with-timer
           project-git-autofetch-initial-delay
           project-git-autofetch-interval
           'project-git-autofetch-run))))

(defun project-git-autofetch-stop ()
  "Stop auto fetch timers."
  (interactive)
  (cancel-timer project-git-autofetch-timer)
  (setq project-git-autofetch-timer nil))

(provide 'project-git-autofetch)
;;; project-git-autofetch.el ends here
