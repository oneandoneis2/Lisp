(defparameter *player-heath* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. You suck."))
  (when (monsters-dead)
    (princ "You have killed all the monsters! Win!")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead) ;; Why is this here?? We check monsters-dead a few lines earlier!
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list (lambda (m)
                 (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-heath* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30)
  )

(defun player-dead ()
  (<= *player-heath* 0))

(defun show-player ()
  (fresh-line)
  (prince "You are a valiant knight with a health of ")
  (prince *player-health*)
  (prince ", an agility of ")
  (prince *player-agility*)
  (prince ", and strength of ")
  (prince *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
      (random-monster)
      m)))
