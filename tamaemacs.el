;;; tamaemacs.el --- tamaemacs -*- lexical-binding: t -*-

;; Package-Version: 0.2

;;; Commentary:
;; idk what to put here but :3

;;; Code:

(require 'button)
(require 'cl-lib)

;;; vars
(defvar alive-dead nil)
(defvar tama-energy 0)
(defvar tama-hunger 0)
(defvar tama-happieness 0)
(defvar tama-stage 'egg)
(defvar tama-decay-timer nil)
(defvar tama-hatch-timer nil)
(defvar tama-pet-type nil)
(defvar tama-last-action (make-hash-table :test 'equal))
(defvar tama-decay-last nil)
(defvar tama-hatch-start nil)
(defvar care-cooldowns
  '(("feed"  . 300)
    ("hug"   . 120)
    ("clean" . 60)
    ("sleep" . 900)))
(defvar tama-pets
  '((egg . "
  ,''`.
 /     \\
:       :
:       :
 `.___,' ")
    (dog . "
  __      _
o'')}____//
 `_/      )
 (_(_/-(_/")
    (cat . "
 |\\__/,|   (`\\
 |_ _  |.--.) )
 ( T   )     /
(((^_(((/(((_/")
    (bear . "
     (()__(()
     /       \\
    ( /    \\  \\
     \\ o o    /
     (_()_)__/ \\
    / _,==.____ \\
   (   |--|      )
   /\\_.|__|'-.__/\\_
  / (        /     \\
  \\  \\      (      /
   )  '._____)    /
(((____.--(((____/")))
(defvar tama-garden
  '((carrot . 0)
    (tomato . 0)
    (corn   . 0))
  "Garden plots.")

(defvar tama-plant-timers '()
  "Alist of (crop . timer) for growing plants.")

(defvar tama-inventory '()
  "Alist of (ingredient . count).")

(defvar tama-recipes
  '((stew    . ((carrot . 3) (tomato . 3) (corn . 3)))
    (salad   . ((carrot . 1) (tomato . 2)))
    (popcorn . ((corn . 4))))
  "Recipes: (meal . ((ingredient . amount) ...)).")

(defvar tama-meal-effects
  '((stew    . ((tama-hunger . 40) (tama-happieness . 10) (tama-energy . 5)))
    (salad   . ((tama-hunger . 20) (tama-happieness . 5)))
    (popcorn . ((tama-hunger . 15) (tama-happieness . 15))))
  "Effects of each meal on stats.")

(defvar tama-grow-times
  '((carrot . 240)
    (tomato . 180)
    (corn   . 120))
  "Grow time in seconds for each crop.")

(defvar tama-foods
  '((kibble  . ((tama-hunger . 5) (tama-happieness . -5)))
    (snack   . ((tama-hunger . 10) (tama-happieness . -10)))
    (water   . ((tama-hunger . 5)  (tama-energy . 5) (tama-happieness . -5))))
  "Basic foods available without cooking.")

;;; save / load
(defun tamasave ()
  "Save."
  (let* ((save-dir "~/.emacs.d/tamaemacs/")
         (save-file (concat save-dir "save.el")))
    (unless (file-exists-p save-dir)
      (make-directory save-dir t))
    (with-temp-file save-file
      (insert (format "(setq alive-dead %S)\n" alive-dead))
      (insert (format "(setq tama-stage '%S)\n" tama-stage))
      (insert (format "(setq tama-pet-type '%S)\n" tama-pet-type))
      (insert (format "(setq tama-hunger %d)\n" tama-hunger))
      (insert (format "(setq tama-happieness %d)\n" tama-happieness))
      (insert (format "(setq tama-energy %d)\n" tama-energy))
      (insert (format "(setq tama-garden '%S)\n" tama-garden))
      (insert (format "(setq tama-inventory '%S)\n" tama-inventory))
      (when tama-decay-last
        (insert (format "(setq tama-decay-last %f)\n" tama-decay-last)))
      (when tama-hatch-start
        (insert (format "(setq tama-hatch-start %f)\n" tama-hatch-start)))
      (maphash (lambda (action timestamp)
                 (insert (format "(puthash %S %f tama-last-action)\n"
                                 action timestamp)))
               tama-last-action))))

;;; drawing
(defun tama-draw ()
  "Draw the tamaemacs based on current stage."
  (with-current-buffer (get-buffer-create "*tamaemacs*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n\n\n\n\n")
      (if (eq tama-stage 'egg)
          (insert (cdr (assq 'egg tama-pets)))
        (insert (or (cdr (assq tama-pet-type tama-pets)) "???")))
      (insert (format "\n\n  [ %s ]"
                      (if (eq tama-stage 'egg)
                          "Egg..."
                        (symbol-name tama-pet-type)))))
      (local-set-key (kbd "q") #'quit-window)
    (switch-to-buffer "*tamaemacs*")))

;;; decay
(defun minmax ()
  "Clamp all stats to 0-100."
  (setq tama-energy     (max 0 (min 100 tama-energy)))
  (setq tama-hunger     (max 0 (min 100 tama-hunger)))
  (setq tama-happieness (max 0 (min 100 tama-happieness))))

(defun tama-decay ()
  "Decrease stats over time."
  (when alive-dead
    (setq tama-decay-last (float-time))
    (setq tama-hunger     (- tama-hunger 2))
    (setq tama-happieness (- tama-happieness 1))
    (setq tama-energy     (- tama-energy 1))
    (minmax)
    (when (= tama-hunger 0)
      (setq alive-dead nil)
      (message "Your Tamaemacs died of hunger... D:"))
    (tamasave)))

(defun tama-start-decay ()
  "Start the decay timer."
  (tama-stop-decay)
  (setq tama-decay-timer (run-at-time 60 60 #'tama-decay)))

(defun tama-stop-decay ()
  "Stop the decay timer."
  (when (timerp tama-decay-timer)
    (cancel-timer tama-decay-timer)
    (setq tama-decay-timer nil)))

;;; hatching
(defun tama-hatch ()
  "Hatch the egg into a random pet."
  (setq tama-stage 'pet)
  (setq tama-pet-type (nth (random 3) '(dog cat bear)))
  (setq tama-hatch-start nil)
  (tama-start-decay)
  (tamasave)
  (tama-draw)
  (message "Your egg hatched into a %s! :D" tama-pet-type))

(defun tama-start-hatch-timer ()
  "Start the hatch timer."
  (when (timerp tama-hatch-timer)
    (cancel-timer tama-hatch-timer)
    (setq tama-hatch-timer nil))
  (setq tama-hatch-timer (run-at-time 1800 nil #'tama-hatch)))

;;; game functions
(defun tamaemacs ()
  "For start game menu."
  (interactive)
  (let ((buf (get-buffer-create "*tamaemacs*")))
    (switch-to-buffer buf)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n
████████  █████  ███    ███  █████  ███████ ███    ███  █████   ██████ ███████ 
   ██    ██   ██ ████  ████ ██   ██ ██      ████  ████ ██   ██ ██      ██      
   ██    ███████ ██ ████ ██ ███████ █████   ██ ████ ██ ███████ ██      ███████ 
   ██    ██   ██ ██  ██  ██ ██   ██ ██      ██  ██  ██ ██   ██ ██           ██ 
   ██    ██   ██ ██      ██ ██   ██ ███████ ██      ██ ██   ██  ██████ ███████")
      (insert "\n\n+_______________________________________________________________________________+\n\n\n")
      (insert-button "Continue" 'action 'continue)
      (center-line)
      (insert "\n")
      (insert-button "New Game" 'action 'new-game)
      (center-line)
      (local-set-key (kbd "q") #'quit-window)
      (insert "\n\n\n+_______________________________________________________________________________+\n\n")
      (insert "M-x tamaemacs  - open start menu")
      (insert "\nM-x tamacare   - to care you tamaemacs :3")
      (insert "\nM-x tamastatus - to check your tamaemacs status")
      (insert "\nM-x tamacook   - to cook for your tamaemacs")
      (insert "\nM-x tamafarm   - to plant crop for cooking")
      (insert "\n\n+_______________________________________________________________________________+\n")
      (let ((save-path "~/.emacs.d/tamaemacs/save.el"))
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
  (setq tama-stage 'egg)
  (setq tama-pet-type nil)
  (setq tama-hatch-start (float-time))
  (setq tama-decay-last nil)
  (setq tama-garden '((carrot . 0)
                      (tomato . 0)
                      (corn   . 0)))
  (setq tama-inventory '())
  (clrhash tama-last-action)
  (tama-start-hatch-timer)
  (tamasave)
  (tama-draw))

(defun continue (_button)
  "Open save files using BUTTON."
  (let ((save-file "~/.emacs.d/tamaemacs/save.el"))
    (if (file-exists-p save-file)
        (progn
          (load save-file)
          (cond
           ((eq tama-stage 'egg)
            (let* ((now (float-time))
                   (elapsed (- now tama-hatch-start))
                   (remaining (- 1800 elapsed)))
              (if (<= remaining 0)
                  (tama-hatch)
                (setq tama-hatch-timer
                      (run-at-time remaining nil #'tama-hatch)))))
           (alive-dead
            (when tama-decay-last
              (let* ((now (float-time))
                     (missed (floor (/ (- now tama-decay-last) 60))))
                (dotimes (_ missed)
                  (setq tama-hunger     (- tama-hunger 2))
                  (setq tama-happieness (- tama-happieness 1))
                  (setq tama-energy     (- tama-energy 1)))
                (minmax)
                (when (= tama-hunger 0)
                  (setq alive-dead nil)
                  (message "Your Tamaemacs died while you were away... D:"))))
            (tama-start-decay)))
          (tama-draw)))))

;;; status
(defun tamastatus ()
  "Check tamaemacs status."
  (interactive)
  (let ((save-path "~/.emacs.d/tamaemacs/save.el"))
    (if (file-exists-p save-path)
        (progn
          (load save-path)
          (message "Status: %s | Hunger: %d | Happiness: %d | Energy: %d"
                   (if alive-dead "Alive" "Dead")
                   tama-hunger
                   tama-happieness
                   tama-energy))
      (message "No save file found D:"))))

;;; feed
(defun tama-feed-menu ()
  "Show a food selection buffer."
  (let ((buf (get-buffer-create "*tama-feed*")))
    (switch-to-buffer buf)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n  === FEED YOUR PET ===\n\n")

      (insert "  -- Basic --\n\n")
      (dolist (food tama-foods)
        (let* ((name (car food))
               (effects (cdr food)))
          (insert "  ")
          (insert-button (format "%-10s" name)
                         'action (lambda (_b) (tama-eat-basic name))
                         'food name)
          (insert "  ")
          (dolist (eff effects)
            (let* ((stat (car eff))
                   (amount (cdr eff))
                   (label (pcase stat
                            ('tama-hunger    "hunger")
                            ('tama-happieness "happy")
                            ('tama-energy    "energy")
                            (_ (symbol-name stat)))))
              (insert (format "%s %s%d  "
                              label
                              (if (> amount 0) "+" "")
                              amount))))
          (insert "\n")))

      (insert "\n  -- Cooked Meals --\n\n")
      (let ((has-meals nil))
        (dolist (meal tama-meal-effects)
          (let* ((name (car meal))
                 (count (tama-inv-get name)))
            (when (> count 0)
              (setq has-meals t)
              (insert "  ")
              (insert-button (format "%-10s (x%d)" name count)
                             'action (lambda (_b) (tama-eat-meal name))
                             'meal name)
              (insert "  ")
              (dolist (eff (cdr meal))
                (let* ((stat (car eff))
                       (amount (cdr eff))
                       (label (pcase stat
                                ('tama-hunger    "hunger")
                                ('tama-happieness "happy")
                                ('tama-energy    "energy")
                                (_ (symbol-name stat)))))
                  (insert (format "%s +%d  " label amount))))
              (insert "\n"))))
        (unless has-meals
          (insert "  (no cooked meals - use M-x tamacook!)\n")))

      (insert "\n  [q] quit\n")
      (local-set-key (kbd "q") #'quit-window))))

(defun tama-eat-basic (food)
  "Eat a basic FOOD item."
  (if (= tama-hunger 100)
      (message "Your Tamaemacs is too full!")
    (let ((effects (cdr (assq food tama-foods))))
      (tama-use-action "feed")
      (dolist (eff effects)
        (set (car eff) (+ (symbol-value (car eff)) (cdr eff))))
      (minmax)
      (tamasave)
      (quit-window)
      (message "Your Tamaemacs ate some %s! :3" food))))

(defun tama-eat-meal (meal)
  "Eat a cooked MEAL from inventory."
  (if (= tama-hunger 100)
      (message "Your Tamaemacs is too full!")
    (if (= (tama-inv-get meal) 0)
        (message "You don't have any %s!" meal)
      (tama-use-action "feed")
      (tama-inv-remove meal 1)
      (let ((effects (cdr (assq meal tama-meal-effects))))
        (dolist (eff effects)
          (set (car eff) (+ (symbol-value (car eff)) (cdr eff))))
        (minmax)
        (tamasave)
        (quit-window)
        (message "Your Tamaemacs ate %s! Yummy! :D" meal)))))

;;; care
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
  "Care your tamaemacs with CARE."
  (interactive "stamacare: ")
  (when (eq tama-stage 'egg)
    (user-error "Your Tamaemacs hasn't hatched yet!"))
  (let* ((action (downcase care))
         (remaining (tama-on-cooldown action)))
    (if remaining
        (message "'%s' is on cooldown for %.0f more seconds..." action remaining)
      (pcase action
        ("feed"
	 (tama-feed-menu))
        ("hug"
         (if (< tama-energy 1)
             (message "Your Tamaemacs is too tired to hug...")
           (setq tama-happieness (+ tama-happieness 10))
           (setq tama-energy (- tama-energy 1))
           (tama-use-action action)
           (message "You hug your Tamaemacs!")))
        ("clean"
         (if (< tama-energy 5)
             (message "Your Tamaemacs is too tired to clean...")
           (setq tama-happieness (+ tama-happieness 5))
           (setq tama-energy (- tama-energy 5))
           (tama-use-action action)
           (message "You cleaned up Your Tamaemacs")))
        ("sleep"
         (if (< tama-hunger 20)
             (message "Your Tamaemacs is too hungry to sleep...")
           (setq tama-hunger (- tama-hunger 20))
           (setq tama-energy (+ tama-energy 40))
           (tama-use-action action)
           (message "Tamaemacs is now going to sleep...")))
        (_
         (message "Your Tamaemacs doesn't understand '%s'..." action)))
      (minmax)
      (tamasave))))

;;; inventory
(defun tama-inv-get (item)
  "Get count of ITEM in inventory."
  (or (cdr (assq item tama-inventory)) 0))

(defun tama-inv-add (item amount)
  "Add AMOUNT of ITEM to inventory."
  (let ((existing (assq item tama-inventory)))
    (if existing
        (setcdr existing (+ (cdr existing) amount))
      (push (cons item amount) tama-inventory))))

(defun tama-inv-remove (item amount)
  "Remove AMOUNT of ITEM from inventory.  Return t if successful."
  (let ((existing (assq item tama-inventory)))
    (if (and existing (>= (cdr existing) amount))
        (progn
          (setcdr existing (- (cdr existing) amount))
          t)
      nil)))

;;; farming
(defun tama-crop-ready (crop)
  "Mark CROP as ready to harvest."
  (let ((plot (assq crop tama-garden)))
    (when plot
      (setcdr plot 2)
      (tamasave)
      (message "Your %s is ready to harvest! Use M-x tamafarm to harvest." crop))))

(defun tamafarm ()
  "Open the farming interface."
  (interactive)
  (let ((buf (get-buffer-create "*tama-farm*")))
    (switch-to-buffer buf)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n  === GARDEN ===\n\n")
      (dolist (plot tama-garden)
        (let* ((crop (car plot))
               (state (cdr plot))
               (label (pcase state
                        (0 "[ empty  ]")
                        (1 "[ growing]")
                        (2 "[ READY! ]"))))
          (insert (format "  %-10s %s  " crop label))
          (pcase state
            (0 (insert-button "Plant"
                              'action (lambda (_b) (tama-plant crop))
                              'crop crop))
            (1 (insert "..."))
            (2 (insert-button "Harvest"
                              'action (lambda (_b) (tama-harvest crop))
                              'crop crop)))
          (insert "\n")))
      (insert "\n  === INVENTORY ===\n\n")
      (if tama-inventory
          (dolist (item tama-inventory)
	    (when (> (cdr item) 0)
            (insert (format "  %-10s x%d\n" (car item) (cdr item))))
        (insert "  (empty)\n"))
      (insert "\n  [q] quit    [r] refresh\n")
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "r") #'tamafarm)))))

(defun tama-plant (crop)
  "Plant CROP in the garden."
  (let ((plot (assq crop tama-garden)))
    (if (not plot)
        (message "Unknown crop: %s" crop)
      (if (/= (cdr plot) 0)
          (message "This plot is already in use!")
        (setcdr plot 1)
        (let* ((grow-time (or (cdr (assq crop tama-grow-times)) 120))
               (timer (run-at-time grow-time nil
                                   (lambda () (tama-crop-ready crop)))))
          (push (cons crop timer) tama-plant-timers))
        (tamasave)
        (tamafarm)
        (message "Planted %s! Come back in %d seconds."
                 crop
                 (or (cdr (assq crop tama-grow-times)) 120))))))

(defun tama-harvest (crop)
  "Harvest CROP from the garden."
  (let ((plot (assq crop tama-garden)))
    (if (and plot (= (cdr plot) 2))
        (progn
          (setcdr plot 0)
          (tama-inv-add crop 1)
          (tamasave)
          (tamafarm)
          (message "Harvested 1 %s!" crop))
      (message "Nothing to harvest here yet..."))))

;;; cooking
(defun tamacook ()
  "Open the cooking interface."
  (interactive)
  (when (eq tama-stage 'egg)
    (user-error "Your Tamaemacs hasn't hatched yet!"))
  (let ((buf (get-buffer-create "*tama-cook*")))
    (switch-to-buffer buf)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n  === KITCHEN ===\n\n")
      (insert "  INVENTORY:\n")
      (if tama-inventory
          (dolist (item tama-inventory)
            (when (> (cdr item) 0)
              (insert (format "    %-10s x%d\n" (car item) (cdr item)))))
        (insert "    (empty - go farm something!)\n"))
      (insert "\n  RECIPES:\n\n")
      (dolist (recipe tama-recipes)
        (let* ((meal (car recipe))
               (ingredients (cdr recipe))
               (can-cook (cl-every (lambda (ing)
                                     (>= (tama-inv-get (car ing)) (cdr ing)))
                                   ingredients)))
          (insert (format "  %-10s needs: " meal))
          (dolist (ing ingredients)
            (insert (format "%s x%d  " (car ing) (cdr ing))))
          (insert "  ")
          (if can-cook
              (insert-button "Cook!"
                             'action (lambda (_b) (tama-cook-meal meal))
                             'meal meal)
            (insert "(need more ingredients)"))
          (insert "\n")))
      (insert "\n  [q] quit    [r] refresh\n")
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "r") #'tamacook))))

(defun tama-cook-meal (meal)
  "Cook MEAL and store in inventory."
  (let* ((recipe (cdr (assq meal tama-recipes)))
         (can-cook (cl-every (lambda (ing)
                               (>= (tama-inv-get (car ing)) (cdr ing)))
                             recipe)))
    (if (not can-cook)
        (message "You don't have enough ingredients!")
      (dolist (ing recipe)
        (tama-inv-remove (car ing) (cdr ing)))
      (tama-inv-add meal 1)
      (minmax)
      (tamasave)
      (tamacook)
      (message "You cooked %s! Feed it with M-x tamacare > feed :D" meal))))

(add-hook 'kill-emacs-hook #'tama-stop-decay)

(provide 'tamaemacs)
;;; tamaemacs.el ends here
