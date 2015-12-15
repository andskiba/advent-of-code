;;; reindeer-olympics --- Day 14 of Advent of Code

;;; Commentary:

;;; Code:

(require 'seq)

(defun run-olympics (input seconds)
  "Run the reindeer Olympics on INPUT running SECONDS."
  (let ((reindeers (get-set input)))
    (dotimes (time seconds)
      (dolist (reindeer reindeers)
        (update-reindeer reindeer time)))
    (apply #'max (seq-map #'distance reindeers))))

(defun run-olympics-p2 (input seconds)
  "Run the reindeer Olympics on INPUT running SECONDS."
  (let ((reindeers (get-set input)))
    (dotimes (time seconds)
      (dolist (reindeer reindeers)
        (update-reindeer reindeer time))
      (award-points reindeers))
    (apply #'max (seq-map #'points reindeers))))

(defun get-set (input)
  "Generate state of the Olympics from INPUT."
  (let ((lines (split-string input "\n")))
    (seq-map #'make-reindeer lines)))

(defun make-reindeer (line)
  "Create reindeer from LINE."
  (string-match
   "^\\([a-zA-Z]+\\) can fly \\([0-9]+\\) km/s for \\([0-9]+\\) seconds, but then must rest for \\([0-9]+\\) seconds\.$"
   line)
  (vector  (match-string 1 line)
           (string-to-number (match-string 2 line))
           (string-to-number (match-string 3 line))
           (string-to-number (match-string 4 line))
           0
           0))

(defun is-moving (reindeer time)
  "See if REINDEER is moving in TIME second."
  (> (stamina reindeer)
     (mod time
          (+ (stamina reindeer)
             (rest-time reindeer)))))

(defun name (reindeer)
  "Name of REINDEER."
  (aref reindeer 0))

(defun speed (reindeer)
  "Speed of a REINDEER."
  (aref reindeer 1))

(defun stamina (reindeer)
  "How long REINDEER can run."
  (aref reindeer 2))

(defun rest-time (reindeer)
  "How long REINDEER needs to rest."
  (aref reindeer 3))

(defun distance (reindeer &optional val)
  "Distance traveled by REINDEER, optionally set distance to VAL."
  (when (not (null val))
    (aset reindeer 4 val))
  (aref reindeer 4))

(defun points (reindeer)
  "Points REINDEER gathered."
  (aref reindeer 5))

(defun add-point (reindeer)
  "Give point to REINDEER."
  (aset reindeer 5 (+ 1 (aref reindeer 5))))

(defun update-reindeer (reindeer time)
  "Update REINDEER state given TIME."
  (when (is-moving reindeer time)
    (distance reindeer (+ (distance reindeer) (speed reindeer)))))

(defun award-points (reindeers)
  "Award a point to the REINDEERS in the lead."
  (let* ((max-distance (apply #'max (seq-map #'distance reindeers)))
         (leaders (seq-filter (lambda (r)
                                (= (distance r) max-distance))
                              reindeers)))
    (dolist (leader leaders)
      (add-point leader))))

;;; Tests:

(ert-deftest test-part-1 ()
  (should
   (equal (run-olympics
           (concat "Comet can fly 14 km/s for 10 seconds, "
                   "but then must rest for 127 seconds.\n"
                   "Dancer can fly 16 km/s for 11 seconds, "
                   "but then must rest for 162 seconds.") 1000)
          1120)))

(ert-deftest test-part-2 ()
  (should
   (equal (run-olympics-p2
           (concat "Comet can fly 14 km/s for 10 seconds, "
                   "but then must rest for 127 seconds.\n"
                   "Dancer can fly 16 km/s for 11 seconds, "
                   "but then must rest for 162 seconds.") 1000)
          689)))

(provide 'reindeer-olympics)

;;; reindeer-olympics ends here
