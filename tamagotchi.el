;;; tamagotchi.el --- tamagotchi on emacs -*- lexical-binding: t -*-

;;; Commentary:
;; idk what to put here but :3

;;; Code:

(require 'button)

(defvar alive-dead nil)
(defvar tama-energy 0)
(defvar tama-hunger 0)
(defvar tama-happieness 0)
(defvar tama-stage 'egg)
(defvar tama-decay-timer nil)
(defvar tama-hatch-timer nil)
(defvar tama-pet-type nil)
(defvar tama-last-action (make-hash-table :test 'equal))
(defvar care-cooldowns
  '(("feed"  . 300)
    ("hug"   . 120)
    ("clean" . 60)
    ("sleep" . 900)))
(defvar tama-sprites
  '((egg  . "
  ,''`.
 /     \
:       :
:       :
 `.___,' ")

    (dog . "
  __      _
o'')}____//
 `_/      )
 (_(_/-(_/")

    (cat . "
 |\__/,|   (`\
 |_ _  |.--.) )
 ( T   )     /
(((^_(((/(((_/")

    (bear . "
     (()__(()
     /       \
    ( /    \  \
     \ o o    /
     (_()_)__/ \
    / _,==.____ \
   (   |--|      )
   /\_.|__|'-.__/\_
  / (        /     \
  \  \      (      /
   )  '._____)    /
(((____.--(((____/")))

(defun tamagotchi ()
  "For start game menu."
  (interactive)
  (message "tamagotchi start :3")
  (let ((buf (get-buffer-create "*tamagotchi*")))
    (switch-to-buffer buf)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n
████████  █████  ███    ███  █████   ██████   ██████  ████████  ██████ ██   ██ ██ 
   ██    ██   ██ ████  ████ ██   ██ ██       ██    ██    ██    ██      ██   ██ ██ 
   ██    ███████ ██ ████ ██ ███████ ██   ███ ██    ██    ██    ██      ███████ ██ 
   ██    ██   ██ ██  ██  ██ ██   ██ ██    ██ ██    ██    ██    ██      ██   ██ ██ 
   ██    ██   ██ ██      ██ ██   ██  ██████   ██████     ██     ██████ ██   ██ ██")
      (insert "\n\n+_______________________________________________________________________________+\n\n\n")
      (insert-button "Continue"
		     'action 'continue)
      (center-line)
      (insert "\n")
      (insert-button "New Game"
		     'action 'new-game)
      (center-line)
      (insert "\n\n\n+_______________________________________________________________________________+\n\n")
      (center-line)
      (insert "M-x tamagotchi - open start menu\nM-x tamastatus - to check your tamagotchi status\nM-x tamacare   - to care you tamagotchi :3\n")
      (insert "You can use hug, feed, clean, sleep")
      (insert "\n+_______________________________________________________________________________+\n")
      (let ((save-path "~/.emacs.d/tamagotchi/save.el"))
        (if (file-exists-p save-path)	    
            (progn
              (load save-path)
              (insert (format "\nStatus: %s | Hunger: %d | Happiness: %d | Energy: %d"		      
                      (if alive-dead "Alive" "Dead")
                      tama-hunger
                      tama-happieness
		      tama-energy))
	      (center-line))
          (insert "\nNo save file found D:")
	  (center-line))))))

(defun new-game (_button)
  "New game using BUTTON."
  (setq tama-hunger 20)
  (setq tama-happieness 0)
  (setq alive-dead t)
  (setq tama-energy 50)
  (tama-start-decay)
  (tama-start-hatch-timer)
  (tama-draw)
  (with-temp-file "~/.emacs.d/tamagotchi/save.el"
    (insert (format "(setq alive-dead %S)\n" alive-dead))
    (insert (format "(setq tama-energy %d)\n" tama-energy))
    (insert (format "(setq tama-hunger %d)\n" tama-hunger))
    (insert (format "(setq tama-happieness %d)\n" tama-happieness))))

(defun continue (_button)
  "Open save files using BUTTON."
  (let ((save-file "~/.emacs.d/tamagotchi/save.el"))
    (if (file-exists-p save-file)
        (progn
          (load save-file)
          (if (eq tama-stage 'egg)
              (tama-start-hatch-timer)
            (tama-start-decay))
          (tama-draw)))))

(defun minmax ()
  "Mix-max status."
  (setq tama-energy    (max 0 (min 100 tama-energy)))
  (setq tama-hunger    (max 0 (min 100 tama-hunger)))
  (setq tama-happieness (max 0 (min 100 tama-happieness))))

(defun tama-decay ()
  "Decrease stats over time."
  (when alive-dead
    (setq tama-hunger    (- tama-hunger 2))
    (setq tama-happieness (- tama-happieness 1))
    (setq tama-energy    (- tama-energy 1))
    (minmax)
    (when (= tama-hunger 0)
      (setq alive-dead nil)
      (message "Your Tamagotchi died of hunger... D:")
      (tamasave))
    (tamasave)))

(defun tama-start-decay ()
  "Start the decay timer."
  (tama-stop-decay)
  (setq tama-decay-timer
        (run-at-time 60 60 #'tama-decay)))

(defun tama-stop-decay ()
  "Stop the decay timer."
  (when (timerp tama-decay-timer)
    (cancel-timer tama-decay-timer)
    (setq tama-decay-timer nil)))

(defun tama-draw ()
  "Draw the tamagotchi based on current stage."
  (let ((inhibit-read-only t))
    (if (switch-to-buffer "*tamagotchi*")
	(get-buffer-create "*tamagotchi*"))
    (erase-buffer)
    (insert "\n\n\n\n\n")
    (if (eq tama-stage 'egg)
        (insert (cdr (assq 'egg tama-sprites)))
      (let ((sprite (cdr (assq tama-pet-type tama-sprites))))
        (insert (or sprite "???")))))
  (insert (format "\n\n  [ %s ]" (if (eq tama-stage 'egg)
                                     "Egg..."
                                   (symbol-name tama-pet-type)))))

(defun tama-hatch ()
  "Hatch the egg into a random pet."
  (setq tama-stage 'pet)
  (setq tama-pet-type (nth (random 3) '(dog cat bear)))
  (tama-start-decay)
  (tamasave)
  (tama-draw)
  (message "Your egg hatched into a %s! :D" tama-pet-type))

(defun tama-start-hatch-timer ()
  "Start the hatch timer."
  (when (timerp tama-hatch-timer)
    (cancel-timer tama-hatch-timer)
    (setq tama-hatch-timer nil))
  (setq tama-hatch-timer
        (run-at-time 300 nil #'tama-hatch)))

(defun tamasave ()
  "Save."
  (let ((save-dir "~/.emacs.d/tamagotchi/save.el"))
    (unless (file-exists-p save-dir)
      (make-directory save-dir t))
    (with-temp-file (concat save-dir "save.el")
      (insert (format "(setq alive-dead %S)\n" alive-dead))
      (insert (format "(setq tama-stage '%S)\n" tama-stage))
      (insert (format "(setq tama-pet-type '%S)\n" tama-pet-type))
      (insert (format "(setq tama-hunger %d)\n" tama-hunger))
      (insert (format "(setq tama-happieness %d)\n" tama-happieness))
      (insert (format "(setq tama-energy %d)\n" tama-energy)))))

(defun tamastatus ()
  "Check tamagotchi status."
  (interactive)
  (let ((save-path "~/.emacs.d/tamagotchi/save.el"))
    (if (file-exists-p save-path)
        (progn
          (load save-path)
          (message "Status: %s | Hunger: %d | Happiness: %d | Energy: %d"
                   (if alive-dead "Alive" "Dead")
                   tama-hunger
                   tama-happieness
		   tama-energy))
      (message "No save file found D:"))))

(defun tama-on-cooldown (care)
  "Return remaining cooldown seconds for CARE, or nil if ready."
  (let* ((last (gethash care tama-last-action 0))
         (cooldown (or (cdr (assoc care care-cooldowns)) 0))
         (remaining (- (+ last cooldown) (float-time))))
    (if (> remaining 0) remaining nil)))

(defun tama-use-action (care)
  "Record that CARE was just used."
  (puthash care (float-time) tama-last-action))

(defun tamacare (care)
  "Care your tamagotchi with CARE."
  (interactive "stamacare: ")
  (let* ((action (downcase care))
         (remaining (tama-on-cooldown action)))
    (if remaining
        (message "'%s' is on cooldown for %.0f more seconds..." action remaining)
      (pcase action
        ("feed"
	 (if (= tama-hunger 100)
	     (message "Your Tamagotchi is too full...")
         (setq tama-hunger (+ tama-hunger 20))
         (tama-use-action action)
         (message "You fed your Tamagotchi!")))
        ("hug"
         (if (< tama-energy 1)
             (message "Your Tamagotchi is too tired to hug...")
           (setq tama-happieness (+ tama-happieness 10))
           (setq tama-energy (- tama-energy 1))
           (tama-use-action action)
           (message "You hug your Tamagotchi!")))
        ("clean"
         (if (< tama-energy 5)
             (message "Your Tamagotchi is too tired to clean...")
           (setq tama-happieness (+ tama-happieness 5))
           (setq tama-energy (- tama-energy 5))
           (tama-use-action action)
           (message "You cleaned up Your Tamagotchi")))
        ("sleep"
         (if (< tama-hunger 20)
             (message "Your Tamagotchi is too hungry to sleep...")
           (setq tama-hunger (- tama-hunger 20))
           (setq tama-energy (+ tama-energy 40))
           (tama-use-action action)
           (message "Tamagotchi is now going to sleep...")))
        (_
         (message "Your Tamagotchi doesn't understand '%s'..." action)))
      (minmax)
      (tamasave))))

(provide 'tamagotchi)
;;; tamagotchi.el ends here
