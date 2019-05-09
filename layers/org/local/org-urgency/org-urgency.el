(defun org-urgency/switch-to-agenda-buffer ()
  "Switch to the *Org Agenda* buffer."
  (interactive)
  (switch-to-buffer "*Org Agenda*"))

(defvar org-urgency/per-priority-scores
  '(("A" . 6.0)
    ("B" . 3.9)
    ("C" . 1.8))
  "The scores for each priority.")

(defvar org-urgency/deadline-coefficient 12.0
  "The coefficient for the deadline property.")

(defvar org-urgency/activity-coefficient 4.0
  "The coefficient for active tasks.")

(defvar org-urgency/age-coefficient 2.0
  "The coefficient for the age of tasks.")

(defvar org-urgency/max-age 365
  "The maximum age of a task in days.")

(defvar org-urgency/blocking-coefficient 2.0
  "The coefficient for blocking tasks.")

(defvar org-urgency/default-tag-score 1.0
  "The default score for a tag attached to a task.")

(defvar org-urgency/per-tag-scores
  '(("next" . 15.0))
  "The scores for specific tags attached to a task.")

(defvar org-urgency/parents-property-name
  "PARENTS"
  "The name of the property used to list the parents of the entry.")

(defvar org-urgency/children-property-name
  "CHILDREN"
  "The name of the property used to list the children of the entry.")

(defun org-urgency/priority-score (position)
  "Extract the priority of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((priority (org-entry-get position "PRIORITY"))
         (kv       (assoc priority org-urgency/per-priority-scores)))
    (if (null kv)
        0.0
      (cdr kv))))

(defun org-urgency/detail-priority-score (position)
  "Detail how the priority score is computed for the TODO entry at the given POSITION."
  (let* ((priority (org-entry-get position "PRIORITY")))
    (if (null priority)
        (message "| priority     | %12f | %2.9f | %10f |" 1 (org-urgency/priority-score position) priority (org-urgency/priority-score position))
      (message "| priority     | %12f | %2.6f (%s) | %10f |" 1 (org-urgency/priority-score position) priority (org-urgency/priority-score position)))))

(defun org-urgency/scaled-deadline (position)
  "Extract the deadline of the TODO entry at the given POSITION and scale it.

Map a range of 21 days to the range [0.2, 1.0]:

            Past          Present                                      Future
            Overdue       Due                                          Due

Distance    7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 days

        <-- 1.0                         linear                         0.2 -->
            capped                                                  capped

References:
- src/Task.cpp, Task::urgency_due().
"
  (let* ((deadline (org-entry-get position "DEADLINE")))
    (if (or (null deadline)
          (string-empty-p deadline))
        0.0
      (let* ((distance (* -1.0 (org-time-stamp-to-now deadline))))
        ;; Distance is positive if deadline is in the past (i.e., the task is overdue).
        (cond
         ((>= distance 7.0)   1.0)                                           ;; Is overdue for more than one week so highest urgency.
         ((>= distance -14.0) (+ (/ (* (+ distance 14.0) 0.8) 21.0) 0.2))
         (t                   0.2)))) ))                                       ;; Still have more than 14 days to complete the task so lowest urgency.

(defun org-urgency/deadline-score (position)
  "Extract the deadline of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((scaled-deadline (org-urgency/scaled-deadline position)))
    (* org-urgency/deadline-coefficient scaled-deadline)))

(defun org-urgency/detail-deadline-score (position)
  "Detail how the deadline score is computed for the TODO entry at the given POSITION."
  (let* ((scaled-deadline (org-urgency/scaled-deadline position)))
    (message "| deadline     | %12f | %12f | %10f |" org-urgency/deadline-coefficient scaled-deadline (org-urgency/deadline-score position))))

;; XXX Need to find the first keyword of the TODO sequence in case it's not
;; TODO.
(defun org-urgency/is-active (position)
  "Tell if the TODO entry at the given POSITION is active."
  (let* ((state (org-entry-get position "TODO")))
    (and (not (string= "TODO" state)))))

(defun org-urgency/activity-score (position)
  "Extract the current state of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((is-active (org-urgency/is-active position))
         (score (if is-active
                    org-urgency/activity-coefficient
                  0.0)))
    score))

(defun org-urgency/detail-activity-score (position)
  "Detail how the activity score is computed for the TODO entry at the given POSITION."
  (let* ((is-active (org-urgency/is-active position)))
    (message "| activity     | %12f | %12f | %10f |"
             org-urgency/activity-coefficient
             (if is-active 1.0 0.0)
             (org-urgency/activity-score position))))

(defun org-urgency/age-score (position)
  "Extract the age of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((timestamp (org-entry-get position "TIMESTAMP")))
    (if (null timestamp)
        0.0
      (let* ((age   (* -1.0 (org-time-stamp-to-now timestamp) ))
             (score (if (> age org-urgency/max-age)
                        org-urgency/age-coefficient
                      (* org-urgency/age-coefficient (/ age org-urgency/max-age) ))))
        score))))

(defun org-urgency/detail-age-score (position)
  "Detail how the age score is computed for the TODO entry at the given POSITION."
  (let* ((timestamp  (org-entry-get position "TIMESTAMP"))
         (age        (if (null timestamp)
                         0.0
                       (* -1.0 (org-time-stamp-to-now timestamp))))
         (scaled-age (if (> age org-urgency/max-age)
                         1.0
                       (/ age org-urgency/max-age))))
    (message "| age          | %12f | %7.3f (%dd) | %10f |"
             org-urgency/age-coefficient
             scaled-age
             age
             (org-urgency/age-score position))))

(defun org-urgency/tag-score (tag)
  "Return the score of the given TAG."
  (let* ((kv (assoc tag org-urgency/per-tag-scores)))
    (if (null kv)
        org-urgency/default-tag-score
      (cdr kv))))

(defun org-urgency/get-entry-tags (pom)
  "Return the list of all the tags bound to the entry at POM. Include inherited tags."
  (let* ((tags (org-entry-get pom "ALLTAGS" t)))
    (if (null tags)
        (list)
      (split-string tags ":" t))))

(defun org-urgency/tags-score (pom)
  "Extract the tags attached to the TODO entry at the given POSITION and return their aggregated scores."
  (let* ((tags (org-urgency/get-entry-tags pom))
         (per-tag-scores (mapcar 'org-urgency/tag-score tags))
         (score (apply '+ per-tag-scores))
         )
    score))

(defun org-urgency/parents-ids (pom)
  "Return the IDs of the parents of the entry at the given POM (position or marker)."
  (let* ((ids (org-entry-get pom org-urgency/parents-property-name)))
    (if (null ids)
        (list)
      (split-string ids " "))))

(defun org-urgency/children-ids (pom)
  "Return the IDs of the children of the entry at the given POM (position or marker)."
  (let* ((ids (org-entry-get pom org-urgency/children-property-name)))
    (if (null ids)
        (list)
      (split-string ids " "))))

(defun org-urgency/is-not-done-p (id)
  "Tell if the entry with the given ID is a TODO entry that is not done. Will return nil if the entry is not a TODO entry."
  (save-excursion ;; XXX save-excursion don't seems to work as expected.
    (org-id-goto id) ;; XXX Was org-id-open but is not autoload'ed. "save-excursion: Symbol’s function definition is void: org-id-open"
    (and (org-entry-is-todo-p)
         (not (org-entry-is-done-p)))))

(defun org-urgency/blocking-score (position)
  "List the TODO entries blocked by the one at the given POSITION and return the corresponding score."
  (let* ((parents (org-urgency/parents-ids position))
         (score (apply '+ (mapcar (lambda (id)
                                    (if (org-urgency/is-not-done-p id)
                                        org-urgency/blocking-coefficient
                                      0.0))
                                  parents))))
    score))

;; (let* ((scaled-deadline (org-urgency/scaled-deadline position)))
;;   (* org-urgency/deadline-coefficient scaled-deadline)))

(defun org-urgency/detail-urgency-computation ()
  "Describe how the urgency of the TODO entry at the current point position is computed."
  (interactive)
  (let* ((pom             (point-marker))
         ;; (priority        (org-entry-get pom "PRIORITY"))
         ;; (scaled-deadline (org-urgency/scaled-deadline pom))
         (urgency         (org-urgency/urgency pom))
         )
    ;;                                                 Coefficient                       Value                                       Score
    (message "Urgency at %s:" pom)
    (message "| %12s | %12s | %12s | %10s |" "Property" "Coefficient" "Value" "Score")
    (message "|--------------+--------------+--------------+------------|")
    (org-urgency/detail-priority-score pom)
    (org-urgency/detail-deadline-score pom)
    (org-urgency/detail-activity-score pom)
    (org-urgency/detail-age-score      pom)
    ;; (org-urgency/detail-tags-score     pom)
    (message "|--------------+--------------+--------------+------------|")
    (message "| urgency      |              |              | %10f |" urgency)
    ))

(defun org-urgency/urgency (&optional pom)
  "Return the urgency of the TODO entry at the given POM (position or marker). Default to the current point position."
  (interactive)
  (let* ((pom (if (null pom) (point-marker) pom)))
    (let* ((priority (org-urgency/priority-score pom))
           (deadline (org-urgency/deadline-score pom))
           (activity (org-urgency/activity-score pom))
           (age      (org-urgency/age-score      pom))
           (tags     (org-urgency/tags-score     pom))
           (blocking (org-urgency/blocking-score pom))
           (urgency  (+ priority deadline activity age tags blocking)))
      urgency)))

(defun org-urgency/cmp-urgencies (a b)
  "Compare two TODO entries, A and B, by computing their urgency.

If A is before B, return -1. If A is after B, return 1. If they are equal return nil."
  (let* ((a-position (get-text-property 0 'org-marker a))
         (b-position (get-text-property 0 'org-marker b))
         (a-urgency  (org-urgency/urgency a-position))
         (b-urgency  (org-urgency/urgency b-position)))
    (progn
      (cond ((= a-urgency b-urgency) nil)
            ((> a-urgency b-urgency) -1)
            ((< a-urgency b-urgency) 1)))))

(defun org-urgency/show-tasks-by-urgency ()
  "Display TODO entries sorted by urgency."
  (interactive)
  (org-agenda nil "u")
  (org-urgency/switch-to-agenda-buffer)) ;; FIXME Explicitly switching to the *Org Agenda* buffer shouldn't be necessary.

(provide 'org-urgency)
