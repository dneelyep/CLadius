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
  image)
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

(defparameter *bullet-list* (vector))

(defparameter *power-up-list* (vector))

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
	  (player-ship (make-ship :x 300 :y 300 :x-vel 15 :y-vel 15 :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Ship.png") :image-type :png)))
	  (power-up-counter 0))

      (sdl-mixer:play-music sound :loop nil)

      ;; TODO Eventually, we can just make a vector of all possible weapon types or similar.
      (sdl:with-events ()
      	(:quit-event () t)
      	(:key-down-event ()
      	  (loop for each-key in (sdl:keys-down-p) do
      	     ;; FIXME Convert these to use window size values.
      	     ;; TODO Convert these to ANDed conds. Maybe also a case instead. Simpler.
      	     (cond ((equal each-key :sdl-key-up)
      		    (if (> (ship-y player-ship) 5)
      			(decf (ship-y player-ship) (ship-y-vel player-ship))))
		   
      		   ((equal each-key :sdl-key-down)
      		    (if (< (ship-y player-ship) 560)
      			(incf (ship-y player-ship) (ship-y-vel player-ship))))
		   
      		   ;; ==============
      		   ;; LEFTOFFHERE Convert conditionals to this cleaner form.
      		   ;; (if (and (equal each-key :sdl-key-left)
      		   ;; 	  (> (ship-x player-ship) 10))
      		   ;; ==============
			   
      		   ((equal each-key :sdl-key-left)
      		    (if (> (ship-x player-ship) 10)
      			(decf (ship-x player-ship) (ship-x-vel player-ship))))
		   
      		   ((equal each-key :sdl-key-right)
      		    (if (< (ship-x player-ship) 690)
      			(incf (ship-x player-ship) (ship-x-vel player-ship))))
		     
      		   ((equal each-key :sdl-key-space)
      		    ;; FIXME This is not making sound.
      		    ;; TODO Consider moving this into the shoot function.
      		    (let ((bullet-sound (sdl-mixer:load-music (concatenate 'string *path-prefix* "audio/bullet.mp3"))))
      		      (sdl-mixer:play-music bullet-sound :loop nil)
      		      (sdl-mixer:free bullet-sound))
      		    (shoot player-ship))
		   
      		   ((equal each-key :sdl-key-q)
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

      	       (if (>= power-up-counter 300)
      		   (progn
      		     (add-power-up)
      		     (setf power-up-counter 0)))
	       
      	       ;; Draw all entities on the screen.
      	       (draw-entities player-ship)

      	       (incf power-up-counter)
	       ;; Redraw the display
	       (sdl:update-display))))))

(defun shoot (ship)
  "Have the provided ship fire their weapon."
  ;; Add an extra bullet onto the bullet list.
  (setf *bullet-list*
	(concatenate 'vector *bullet-list* (vector (make-bullet :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Standard-Bullet.png") :image-type :png ) :coords (sdl:point :x (+ 10 (ship-x ship)) :y (ship-y ship)) :vel-x 3)))))

(defun draw-entities (player-ship)
  "Draw all ships, bullets, and powerups on the screen."
  ;; Draw the player's ship at its current location.
  (sdl:draw-surface-at
   (ship-image player-ship)
   (sdl:point :x (ship-x player-ship) :y (ship-y player-ship)))
  (draw-bullets)
  (draw-power-ups player-ship))

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
	     (if (or (< (elt (multiple-value-list (sdl:point-* (bullet-coords each-bullet))) 1) 0)
	     	     (> (elt (multiple-value-list (sdl:point-* (bullet-coords each-bullet))) 1) 600)
	     	     (< (elt (multiple-value-list (sdl:point-* (bullet-coords each-bullet))) 0) 0)
	     	     (> (elt (multiple-value-list (sdl:point-* (bullet-coords each-bullet))) 0) 800))
		 (setf *bullet-list* (delete each-bullet *bullet-list*))
		 (sdl:set-point-* (bullet-coords each-bullet) :x (+ (elt (bullet-coords each-bullet) 0) (bullet-vel-x each-bullet))))))))

(defun add-power-up ()
  "Add a new power-up to a random location on the screen."
  ;; TODO Again, replace hard-coded values with constants.
  (setf *power-up-list*
	(concatenate 'vector *power-up-list* (vector (make-power-up :coords (sdl:point :x 775 :y (random 575)) :x-vel -3 :y-vel 0 :image (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Power-Up.png") :image-type :png))))))

(defun draw-power-ups (ship)
  "Draw to the screen every power up in the *power-up-list*, checking if each power
   up collides with the provided ship."
  (if (not (equal *power-up-list* #()))
      (loop for each-power-up across *power-up-list* do
	   (progn
	     ;; (format t "Power up: (~a, ~a)~%" (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 0) (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 1))
	     ;; (format t "Ship: (~a, ~a)~%" (ship-x ship) (ship-y ship))

	     (collides-with-p ship each-power-up)
;;	     (if (  [the surface to be drawn collides with player ship])
;;		 ()
		 (progn
		   (sdl:draw-surface-at (power-up-image each-power-up) (power-up-coords each-power-up))
		   ;; Delete each power ups outside the screen bounds, that is not nil. 
		   ;; TODO Replace these with constants that represent the edges of the screen.
		   ;; TODO Replace this call in both methods with a collect-garbage method that cleans
		   ;;      up any entity outside of these bounds.
		   (if (or (< (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 1) 0)
			   (> (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 1) 600)
			   (< (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 0) 0)
			   (> (elt (multiple-value-list (sdl:point-* (power-up-coords each-power-up))) 0) 800))
		       (setf *power-up-list* (delete each-power-up *power-up-list*))
		       (sdl:set-point-* (power-up-coords each-power-up) :x (+ (elt (power-up-coords each-power-up) 0) (power-up-x-vel each-power-up)))))))))


;; TODO Start making use of M-x slime-documentation to lookup defun docs rather than hyperspec. Use C-c C-d d
;;      Also, start making use of M-p/M-n in the minibuffer and elsewhere.
;;      Also, start using the SLIME inspector. Extremely cool. C-c I over a variable/object.
;;      Also, start using C-c M-k to compile the current file in slime, rather than M-x slime-eval-buffer

;; TODO Convert this macro to a function.
(defun rect-x (rect-object)
  (elt (multiple-value-list (sdl:rectangle-* rect-object)) 0))

(defun rect-y (rect-object)
  (elt (multiple-value-list (sdl:rectangle-* rect-object)) 1))

;;Returns the X, Y, WIDTH and HEIGHT coordinates of the object as a spread. The RESULT is (VALUES X Y WIDTH HEIGHT) 

(defun collides-with-p (ship object)
  "Check whether or not the provided ship and object collide with each other."
  ;; HACK In this case, I'm only testing if any of the four corners of the object
  ;;      are within the bounds of the ship. For this to be truly correct, I'd
  ;;      need to test every single point on the provided object, not just corners.

  ;; TODO Review this code. It's a mess - there's logic in here, having a hard time teasing
  ;;      it out, though. Tired.
  (let ((object-x-min (rect-x (power-up-image object)))
	(object-x-max (+ (rect-x (power-up-image object)) (elt (multiple-value-list (sdl:rectangle-* (power-up-image object))) 2)))
	(object-y-min (rect-y (power-up-image object)))
	(object-y-max (+ (rect-y (power-up-image object)) (elt (multiple-value-list (sdl:rectangle-* (power-up-image object))) 3)))

    	;; TODO Note that these values assume that increasing y is going downwards.
    	(ship-top-left     (sdl:point :x (rect-x (ship-image ship)) :y (rect-y (ship-image ship))))
    	(ship-top-right    (sdl:point :x (+ (rect-x (ship-image ship)) (elt (multiple-value-list (sdl:rectangle-* (ship-image ship))) 2)) :y (rect-y (ship-image ship))))
    	(ship-bottom-left  (sdl:point :x (rect-x (ship-image ship)) :y (+ (rect-y (ship-image ship)) (elt (multiple-value-list (sdl:rectangle-* (ship-image ship))) 3))))
    	(ship-bottom-right (sdl:point :x (+ (rect-x (ship-image ship)) (elt (multiple-value-list (sdl:rectangle-* (ship-image ship))) 2)) :y (+ (rect-y (ship-image ship)) (elt (multiple-value-list (sdl:rectangle-* (ship-image ship))) 3)))))

    (format t "object-x-min: ~a~%" object-x-min)
    (format t "object-x-max: ~a~%" object-x-max)
    (format t "object-y-min: ~a~%" object-y-min)
    (format t "object-y-max: ~a~%~%" object-y-max)

    (format t "ship-top-left: ~a~%" ship-top-left)
    (format t "ship-top-right: ~a~%" ship-top-right)
    (format t "ship-bottom-left: ~a~%" ship-bottom-left)
    (format t "ship-bottom-right: ~a~%" ship-bottom-right)

    (format t "(elt ship-top-left 0): ~a~%" (elt ship-top-left 0))

    (format t "~a~%" 
	    (and (>= object-x-min (elt ship-top-left 0))
		 (<= object-x-max (elt ship-top-left 0))
		 (>= object-y-min (elt ship-top-left 1))
		 (<= object-y-max (elt ship-top-left 1)))) ; top-left     point within ship

    (sdl:draw-box-* object-x-min object-y-min 15 15 :color sdl:*white*)
    (sdl:draw-box-* (elt ship-top-left 0) (elt ship-top-left 1) 15 15 :color sdl:*red*)
    (sdl:draw-box-* (elt ship-bottom-right 0) (elt ship-bottom-right 1) 15 15 :color sdl:*red*)

    ;; TODO USE (sdl:width) and (sdl:height) !!!!!

    (format t "~%~%")

    (format t "~a~%" (and (>= object-x-min (elt ship-top-right 0))
	 (<= object-x-max (elt ship-top-right 0))
	 (>= object-y-min (elt ship-top-right 1))
	 (>= object-y-max (elt ship-top-right 1)))) ; top-right    point within ship

    (format t "~a~%" (and (>= object-x-min (elt ship-bottom-left 0))
	 (<= object-x-max (elt ship-bottom-left 0))
	 (>= object-y-min (elt ship-bottom-left 1))
	 (>= object-y-max (elt ship-bottom-left 1)))) ; bottom-left  point within ship

    (format t "~a~%"  (and (>= object-x-min (elt ship-bottom-right 0))
	 (<= object-x-max (elt ship-bottom-right 0))
	 (>= object-y-min (elt ship-bottom-right 1))
	 (>= object-y-max (elt ship-bottom-right 1))))


    ;; TODO I need to be checking the 4 points of the object in relation to the ship here.
    ;; (format t "~a~%" (>= object-x-min (elt ship-top-left 0)))
    ;; (format t "~a~%" (<= object-x-min (+ (elt ship-top-left 0) (sdl:width (ship-image ship)))))
    ;; (format t "~a~%" (>= object-y-min (elt ship-top-left 1)))
    ;; (format t "~a~%" (<= object-y-min (+ (elt ship-top-left 1) (sdl:height (ship-image ship)))))

    ;; LEFTOFFHERE: These conditionals should be: object-x-min, object-y-min represents top-left of object. Etc.
    ;;              Check if any of the 4 points of the object fall within the bounds of the ship. See the
    ;;              format right above here.

    (if (or (and (>= object-x-min (elt ship-top-left 0))
    		 (<= object-x-max (elt ship-top-left 0))
    		 (>= object-y-min (elt ship-top-left 1))
    		 (<= object-y-max (elt ship-top-left 1)))      ; top-left     point of the object

    	    (and (>= object-x-min (elt ship-top-right 0))
    		 (<= object-x-max (elt ship-top-right 0))
    		 (>= object-y-min (elt ship-top-right 1))
    		 (>= object-y-max (elt ship-top-right 1)))     ; top-right    point of the object

    	    (and (>= object-x-min (elt ship-bottom-left 0))
    		 (<= object-x-max (elt ship-bottom-left 0))
    		 (>= object-y-min (elt ship-bottom-left 1))
    		 (>= object-y-max (elt ship-bottom-left 1)))   ; bottom-left  point of the object

    	    (and (>= object-x-min (elt ship-bottom-right 0))
    		 (<= object-x-max (elt ship-bottom-right 0))
    		 (>= object-y-min (elt ship-bottom-right 1))
    		 (>= object-y-max (elt ship-bottom-right 1)))) ; bottom-right point of the object
    	  (format t "COLLISION!"))))
