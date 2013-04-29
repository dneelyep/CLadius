;; CLadius - a Common Lisp clone/improvement/future MMO(?) version of the 
;;           classic Gradius games.
;; ======================================================================
;; game.lisp - Primary game logic.
;; ======================================================================
;; TODO Convert ships to use a single coords object?
(defstruct ship
  x
  y
  x-vel
  y-vel
  image
  power-ups
  power-up-element)
;;  weapon)

(defstruct bullet
  image
  coords
  vel-x)

;; TODO Would it make more sense to have a generic Entity, with coords, velocity, and an image?
(defstruct power-up
  coords
  x-vel
  y-vel
  image)

(defparameter *bullet-list* (vector) "A list of all the bullets currently floating on/off the screen.")

(defparameter *power-up-list* (vector) "A list of all the power-ups currently floating on/off the screen.")

(defun start-game ()
  "Start a new game of CLadius."
  (sdl:with-init (sdl:sdl-init-audio)
    (sdl:window 800 600 :title-caption "Cladius! The game.")
    (setf (sdl:frame-rate) 60)
    ;; TODO Ideally there should not be a delay of 1 millisecond here. I want it to be
    ;;      0 millisecond delay.
    (sdl:enable-key-repeat 30 30)	; Enable key repeating, so we can hold keys to move.
    
    (sdl-mixer:open-audio)
    (let ((sound (sdl-mixer:load-music (concatenate 'string *path-prefix* "audio/transition.mp3")))
	  (player-ship (make-ship :x 300 :y 300 :x-vel 15 :y-vel 15 :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Ship.png") :image-type :png) :power-ups '((speedup . 0) (missile . 0) (double . 0) (laser . 0) (option . 0) (? . 0) (! . 0)) :power-up-element nil))
	  ;; TODO Use a circularly linked list to represent the current power-up-element in the list
	  ;;      of power-ups the player has.
	  (power-up-counter 0))

      (sdl-mixer:play-music sound :loop nil)

      ;; TODO Eventually, we can just make a vector of all possible weapon types or similar.
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event ()
			 (loop for each-key in (sdl:keys-down-p) do
			    ;; FIXME Convert these to use window size values.
			    ;; TODO Convert these to ANDed conds. Maybe also a case instead. Simpler.
			      (case each-key
				(:sdl-key-up
				 (if (> (ship-y player-ship) 5)
				     (decf (ship-y player-ship) (ship-y-vel player-ship))))

				(:sdl-key-down
				 (if (< (ship-y player-ship) 560)
				     (incf (ship-y player-ship) (ship-y-vel player-ship))))

				(:sdl-key-left
				 (if (> (ship-x player-ship) 10)
				     (decf (ship-x player-ship) (ship-x-vel player-ship))))

				(:sdl-key-right
				 (if (< (ship-x player-ship) 690)
				     (incf (ship-x player-ship) (ship-x-vel player-ship))))
				
				(:sdl-key-space
				 ;; FIXME This is not making sound.
				 ;; TODO Consider moving this into the shoot function.
				 (let ((bullet-sound (sdl-mixer:load-music (concatenate 'string *path-prefix* "audio/bullet.mp3"))))
				   (sdl-mixer:play-music bullet-sound :loop nil)
				   (sdl-mixer:free bullet-sound))
				 (shoot player-ship))

				;; TODO Fix this to also work with right control.
				(:sdl-key-lctrl
				 (use-power-up player-ship))
				    
				(:sdl-key-q
				 (sdl-mixer:free sound)
				 (sdl-mixer:close-audio)
				 (sdl:push-quit-event)))))
	(:idle ()
	       ;; TODO Could I use a similar idiom here rather than putting logic in :key-pressed?
	       ;; Change the color of the box if the left mouse button is depressed
	       ;; (when (sdl:mouse-left-p)
	       ;; 	 (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))

	       ;; Clear the display each game loop
	       (sdl:clear-display sdl:*black*)

	       (if (>= power-up-counter 100)
		   (progn
		     (add-power-up)
		     (setf power-up-counter 0)))
	       
	       ;; Draw all entities on the screen.
	       (draw-entities player-ship)

	       (incf power-up-counter)
	       ;; Redraw the display
	       (sdl:update-display))))))

(defun shoot (ship)
  "Have the provided ship fire its weapon."
  ;; Add an extra bullet onto the bullet list.
  (setf *bullet-list*
	(concatenate 'vector *bullet-list* (vector (make-bullet :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Standard-Bullet.png") :image-type :png ) :coords (sdl:point :x (+ 10 (ship-x ship)) :y (ship-y ship)) :vel-x 3)))))

(defun use-power-up (ship)
  "Have the provided ship use the currently-selected power up."
  ;; LEFTOFFHERE Implementing these power-up options.
  (case (ship-power-up-element ship)
    (:speedup 
     (incf (ship-x-vel ship) 2)
     (incf (ship-y-vel ship) 2))
    ;; (:missile (setf (ship-power-up-element ship) :double))
    ;; (:double  (setf (ship-power-up-element ship) :laser))
    ;; (:laser   (setf (ship-power-up-element ship) :option))
    ;; (:option  (setf (ship-power-up-element ship) :?))
    ;; (:?       (setf (ship-power-up-element ship) :!))
  (setf (ship-power-up-element ship) nil)))
    ;; (:!       (setf (ship-power-up-element ship) :speedup))))

(defun draw-entities (player-ship)
  "Draw all ships, bullets, powerups, and the power up bar on the screen."
  ;; Draw the player's ship at its current location.
  (sdl:draw-surface-at
   (ship-image player-ship)
   (sdl:point :x (ship-x player-ship) :y (ship-y player-ship)))
  (draw-bullets)
  (draw-power-ups player-ship)
  (draw-power-up-bar player-ship))

(defun draw-power-up-bar (ship)
  "Draw a bar at the bottom of the screen that represents the provided
ship's current power up levels."
  ;; TODO Clean up this ugliness. Use better data structures, loop, etc.
  ;; TODO Replace values with constants for screen edge.
  (sdl:initialise-default-font)
  (sdl:draw-string-shaded "SPEEDUP" (sdl:point :x 10  :y 570) sdl:*white* sdl:*blue*);;100 20 :color sdl:*white*)
  (sdl:draw-string-shaded "MISSILE" (sdl:point :x 130 :y 570) sdl:*white* sdl:*blue*)
  (sdl:draw-string-shaded "DOUBLE"  (sdl:point :x 250 :y 570) sdl:*white* sdl:*blue*)
  (sdl:draw-string-shaded "LASER"   (sdl:point :x 370 :y 570) sdl:*white* sdl:*blue*)
  (sdl:draw-string-shaded "OPTION"  (sdl:point :x 490 :y 570) sdl:*white* sdl:*blue*)
  (sdl:draw-string-shaded "?"       (sdl:point :x 610 :y 570) sdl:*white* sdl:*blue*)
  (sdl:draw-string-shaded "!"       (sdl:point :x 730 :y 570) sdl:*white* sdl:*blue*)

  ;; TODO Increase the font size of these strings.
  ;; TODO Make the width of each of the bars uniform, and make the bars stretch to exactly the width of the screen.
  (case (ship-power-up-element ship)
    (:speedup (sdl:draw-string-shaded "SPEEDUP" (sdl:point :x 10  :y 570) sdl:*white* sdl:*red*))
    (:missile (sdl:draw-string-shaded "MISSILE" (sdl:point :x 130 :y 570) sdl:*white* sdl:*red*))
    (:double  (sdl:draw-string-shaded "DOUBLE"  (sdl:point :x 250 :y 570) sdl:*white* sdl:*red*))
    (:laser   (sdl:draw-string-shaded "LASER"   (sdl:point :x 370 :y 570) sdl:*white* sdl:*red*))
    (:option  (sdl:draw-string-shaded "OPTION"  (sdl:point :x 490 :y 570) sdl:*white* sdl:*red*))
    (:?       (sdl:draw-string-shaded "?"       (sdl:point :x 610 :y 570) sdl:*white* sdl:*red*))
    (:!       (sdl:draw-string-shaded "!"       (sdl:point :x 730 :y 570) sdl:*white* sdl:*red*))))
	      
(defun draw-bullets ()
  "Draw all of the bullets in *bullet-list* on screen."
  (if (not (equal *bullet-list* #()))
      (loop for each-bullet across *bullet-list* do
	   (progn
	     (sdl:draw-surface-at (bullet-image each-bullet) (bullet-coords each-bullet))
	     ;; Delete each bullet outside the screen bounds, that is not nil. 
	     ;; Delete all bullets that have flown off screen, to keep us from wasting
	     ;; processing time/memory.
	     ;; TODO Replace these with constants that represent the edges of the screen.
	     (if (or (< (sdl:y (bullet-coords each-bullet)) 0)
		     (> (sdl:y (bullet-coords each-bullet)) 600)
		     (< (sdl:x (bullet-coords each-bullet)) 0)
		     (> (sdl:x (bullet-coords each-bullet)) 800))
		 (setf *bullet-list* (delete each-bullet *bullet-list*))
		 (sdl:set-point-* (bullet-coords each-bullet) :x (+ (sdl:x (bullet-coords each-bullet)) (bullet-vel-x each-bullet))))))))

(defun add-power-up ()
  "Add a new power-up to a random location on the screen."
  ;; TODO Again, replace hard-coded values with constants.
  (setf *power-up-list*
	(concatenate 'vector *power-up-list* (vector (make-power-up :coords (sdl:point :x 775 :y (random 575)) :x-vel -3 :y-vel 0 :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Power-Up.png") :image-type :png))))))

(defun kill-player ()
  "Destroy the player's ship."
  (format t "Player killed!~%"))

(defun cycle-power-up (ship)
  "Give the provided ship an extra power-up, cycling the power up that they are currently able to activate."
  ;; TODO Convert ship power-ups to a circular linked list. Much more natural, simple way of doing this.
  ;; TODO Make sure that this setf triggers a change in what gets drawn on the screen.
  (case (ship-power-up-element ship)
    ('nil     (setf (ship-power-up-element ship) :speedup))
    (:speedup (setf (ship-power-up-element ship) :missile))
    (:missile (setf (ship-power-up-element ship) :double))
    (:double  (setf (ship-power-up-element ship) :laser))
    (:laser   (setf (ship-power-up-element ship) :option))
    (:option  (setf (ship-power-up-element ship) :?))
    (:?       (setf (ship-power-up-element ship) :!))
    (:!       (setf (ship-power-up-element ship) :speedup))))

(defun draw-power-ups (ship)
  "Draw to the screen every power up in the *power-up-list*, checking if each power
   up collides with the provided ship."
  (if (not (equal *power-up-list* #()))
      (loop for each-power-up across *power-up-list* do
	   (progn
	     ;; (format t "Power up: (~a, ~a)~%" (sdl:x (power-up-coords each-power-up)) (sdl:y (power-up-coords each-power-up)))
	     ;; (format t "Ship: (~a, ~a)~%" (ship-x ship) (ship-y ship))

	     (if (collides-with-p ship each-power-up)
		 (progn 
		   (setf *power-up-list* (delete each-power-up *power-up-list*))
		   (cycle-power-up ship)
		   ;; TODO We shouldn't kill the player here - we kill the player when (s)he hits a bullet.
		   (kill-player))
		 (progn
		   (sdl:draw-surface-at (power-up-image each-power-up) (power-up-coords each-power-up))
		   ;; Delete each power ups outside the screen bounds, that is not nil. 
		   ;; TODO Replace these with constants that represent the edges of the screen.
		   ;; TODO Replace this duplicated stuff in both methods with a collect-garbage method 
		   ;;      that cleans up any entity outside of these bounds.
		   (if (or (< (sdl:y (power-up-coords each-power-up)) 0)
			   (> (sdl:y (power-up-coords each-power-up)) 600)
			   (< (sdl:x (power-up-coords each-power-up)) 0)
			   (> (sdl:x (power-up-coords each-power-up)) 800))
		       (setf *power-up-list* (delete each-power-up *power-up-list*))
		       (sdl:set-point-* (power-up-coords each-power-up) :x (+ (sdl:x (power-up-coords each-power-up)) (power-up-x-vel each-power-up))))))))))


;; TODO Start making use of M-x slime-documentation to lookup defun docs rather than hyperspec. Use C-c C-d d
;;      Also, start making use of M-p/M-n in the minibuffer and elsewhere.
;;      Also, start using the SLIME inspector. Extremely cool. C-c I over a variable/object.
;;      Also, start using C-c M-k to compile the current file in slime, rather than M-x slime-eval-buffer
(defun collides-with-p (ship object)
  "Check whether or not the provided ship and object collide with each other."
  ;; HACK In this case, I'm only testing if any of the four corners of the object
  ;;      are within the bounds of the ship. For this to be truly correct, I'd
  ;;      need to test every single point on the provided object, not just corners.
  (let ((object-x-min (sdl:x (power-up-image object)))
	(object-x-max (+ (sdl:x (power-up-image object)) (sdl:width (power-up-image object))))
	(object-y-min (sdl:y (power-up-image object)))
	(object-y-max (+ (sdl:y (power-up-image object)) (sdl:height (power-up-image object))))

	;; TODO Note that these values assume that increasing y is going downwards.
	(ship-x-min (sdl:x (ship-image ship)))
	(ship-x-max (+ (sdl:x (ship-image ship)) (sdl:width (ship-image ship))))
	(ship-y-min (sdl:y (ship-image ship)))
	(ship-y-max (+ (sdl:y (ship-image ship)) (sdl:height (ship-image ship)))))

    (if (or (and (>= object-x-min ship-x-min)
		 (<= object-x-min ship-x-max)
		 (>= object-y-min ship-y-min)
		 (<= object-y-min ship-y-max))

	    (and (>= object-x-min ship-x-min)
		 (<= object-x-min ship-x-max)
		 (>= object-y-max ship-y-min)
		 (<= object-y-max ship-y-max))

	    (and (>= object-x-max ship-x-min)
		 (<= object-x-max ship-x-max)
		 (>= object-y-min ship-y-min)
		 (<= object-y-min ship-y-max))

	    (and (>= object-x-max ship-x-min)
		 (<= object-x-max ship-x-max)
		 (>= object-y-max ship-y-min)
		 (<= object-y-max ship-y-max)))
	t
	nil)))
