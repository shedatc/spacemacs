(defun sheda-org/switch-to-brain-buffer ()
  "Switch to the *org-brain* buffer."
  (interactive)
  (switch-to-buffer "*org-brain*"))

(defvar sheda-org/deadline-coefficient 12.0
  "The coefficient for the deadline property.")

(defvar sheda-org/activity-coefficient 4.0
  "The coefficient for active tasks.")

(defvar sheda-org/age-coefficient 2.0
  "The coefficient for the age of tasks.")

(defvar sheda-org/max-age 365
  "The maximum age of a task in days.")

(defvar sheda-org/default-tag-score 1.0
  "The default score for a tag attached to a task.")

(defvar sheda-org/per-tag-scores
  '(("next" . 15.0))
  "The scores for specific tags attached to a task.")

(defun sheda-org/priority-score (position)
  "Extract the priority of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((priority (org-entry-get position "PRIORITY"))
         (score (cond
                 ((string= "A" priority) 6.0)
                 ((string= "B" priority) 3.9)
                 ((string= "C" priority) 1.8)
                 (t                      0.0))))
    (message "\tPriority:    raw-value='%s' score=%f" priority score)
    score))

(defun sheda-org/scaled-deadline (position)
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
        (message "\tDeadline:    raw-value='%s'" deadline)
        (cond
         ((>= distance 7.0)   1.0)                                           ;; Is overdue for more than one week so highest urgency.
         ((>= distance -14.0) (+ (/ (* (+ distance 14.0) 0.8) 21.0) 0.2))
         (t                   0.2)))) ))                                       ;; Still have more than 14 days to complete the task so lowest urgency.

(defun sheda-org/deadline-score (position)
  "Extract the deadline of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((scaled-deadline (sheda-org/scaled-deadline position))
         (score (* sheda-org/deadline-coefficient scaled-deadline)))
    (message "\tDeadline:    scaled=%f coefficient=%f score=%f" scaled-deadline sheda-org/deadline-coefficient score)
    score))

;; XXX Need to find the first keyword of the TODO sequence of type.
(defun sheda-org/activity-score (position)
  "Extract the current state of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((state (org-entry-get position "TODO"))
         (is-active (and (not (string= "TODO" state))
                         (not (member state org-not-done-keywords))))
         (score (if is-active
                    sheda-org/activity-coefficient
                  0.0)))
    (message "\tActivity:    state='%s' is-active=%s coefficient=%f score=%f"
             state is-active sheda-org/age-coefficient score)
    score))

(defun sheda-org/age-score (position)
  "Extract the age of the TODO entry at the given POSITION and return its corresponding score."
  (let* ((timestamp (org-entry-get position "TIMESTAMP")))
    (if (null timestamp)
        0.0
      (let* ((age   (* -1.0 (org-time-stamp-to-now timestamp) ))
             (score (if (> age sheda-org/max-age)
                        sheda-org/age-coefficient
                      (* sheda-org/age-coefficient (/ age sheda-org/max-age) ))))
        (message "\tAge:         timestamp=%s age=%s max-age=%f coefficient=%f score=%f"
                 timestamp age sheda-org/max-age sheda-org/age-coefficient score)
        score))))

(defun sheda-org/tag-score (tag)
  "Return the score of the given TAG."
  (let* ((kv (assoc tag sheda-org/per-tag-scores)))
    (if (null kv)
        sheda-org/default-tag-score
      (cdr kv))))

(defun sheda-org/tags-score (position)
  "Extract the tags attached to the TODO entry at the given POSITION and return their aggregated scores."
  (let* ((tags       (org-entry-get position "TAGS"))
         (per-tag-scores (mapcar 'sheda-org/tag-score tags))
         ;; (tags-count (length tags))
         ;; (score      (* tags-count sheda-org/tags-coefficient))
         (score (apply '+ per-tag-scores))
         )
    (message "\tTags:        tags='%s' score=%f" tags score)
    score))

(defun sheda-org/children-ids (&optional pom property)
  "Return the ID of each children of the TODO entry at the given position or marker (POM)."
  (interactive)
  (let* ((pom (if (null pom) (point-marker) pom))
         (children (org-entry-get pom
                                  (if (null property)
                                      "BRAIN_CHILDREN"
                                    property))))
    (message "Children at %s: %s" pom children)
    (if (null children)
        nil
      (split-string children " "))))

(defun sheda-org/blocking-score (position)
  "List the TODO entries blocked by the one at the given POSITION and return the corresponding score."
  0.0)

(defun sheda-org/urgency (position)
  "Return the urgency of the TODO entry at the given POSITION."
  (message "Position: %s" position)
  (let* ((priority (sheda-org/priority-score position))
         (deadline (sheda-org/deadline-score position))
         (activity (sheda-org/activity-score position))
         (age      (sheda-org/age-score      position))
         (tags     (sheda-org/tags-score     position))
         (blocking (sheda-org/blocking-score position))
         (urgency  (+ priority deadline activity age tags blocking)))
    (message "Urgency: %f" urgency)
    urgency))

(defun sheda-org/cmp-urgencies (a b)
  "Compare two TODO entries, A and B, by computing their urgency.

If A is before B, return -1. If A is after B, return 1. If they are equal return nil."
  (let* ((a-position (get-text-property 0 'org-marker a))
         (b-position (get-text-property 0 'org-marker b))
         (a-urgency  (sheda-org/urgency a-position))
         (b-urgency  (sheda-org/urgency b-position)))
    (progn
      (message "A: marker=%s urgency=%f" a a-urgency)
      (message "B: marker=%s urgency=%f" b b-urgency)
      (cond ((= a-urgency b-urgency) nil)
            ((> a-urgency b-urgency) -1)
            ((< a-urgency b-urgency) 1)))))
