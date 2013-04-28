;; CLadius - a Common Lisp clone/improvement/future MMO(?) version of the 
;;           classic Gradius games.
;; ======================================================================
;; cladius.lisp - entry-point into the program.
;; ======================================================================

(defun init-project ()
  "Load all required dependencies for this project."
  (ql:quickload "lispbuilder-sdl")
  (ql:quickload "lispbuilder-sdl-image")
  (ql:quickload "lispbuilder-sdl-mixer"))

(defparameter *path-prefix* "/home/daniel/Programming/Common_Lisp/games/Gradius_clone/")

(defstruct menu-item
  x
  y
  title
  image
  rollover)

;; TODO Would it be more clean to make this a vector of images rather than just keywords?
(defparameter *menu-options*
  (vector 
   (make-menu-item :x 400 :y 100 :title 'start 
     :image (concatenate 'string *path-prefix* "img/bin/Menu-Start.png")   :rollover (concatenate 'string *path-prefix* "img/bin/Menu-Start-Rollover.png"))
   (make-menu-item :x 400 :y 300 :title 'options 
     :image (concatenate 'string *path-prefix* "img/bin/Menu-Options.png") :rollover (concatenate 'string *path-prefix* "img/bin/Menu-Options-Rollover.png"))
   (make-menu-item :x 400 :y 500 :title 'quit 
     :image (concatenate 'string *path-prefix* "img/bin/Menu-Quit.png")    :rollover (concatenate 'string *path-prefix* "img/bin/Menu-Quit-Rollover.png"))))

(defvar current-menu-item (elt *menu-options* 0))

(defun cladius ()
  "Open up and start the Cladius game."
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "Cladius! A Gradius clone in Common Lisp.")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
	(format t "~a~%" key)
	(format t "~a~%" (position current-menu-item *menu-options*))

	(case key
	  (:sdl-key-up
	   (case (menu-item-title current-menu-item)
	     (start   (setf current-menu-item (elt *menu-options* 2)))
	     (options (setf current-menu-item (elt *menu-options* 0)))
	     (quit    (setf current-menu-item (elt *menu-options* 1))))
	   (format t "Up!~%"))
	   
	  (:sdl-key-down
	   (case (menu-item-title current-menu-item)
	     (start   (setf current-menu-item (elt *menu-options* 1)))
	     (options (setf current-menu-item (elt *menu-options* 2)))
	     (quit    (setf current-menu-item (elt *menu-options* 0))))
	   (format t "Down!~%"))

	  (:sdl-key-return
	   (go-to current-menu-item))))

      (:idle ()
       ;; Clear the display each game loop
       (sdl:clear-display sdl:*black*)

       (display-menu-buttons current-menu-item)

       (sdl:draw-surface-at (sdl-image:load-image (concatenate 'string *path-prefix* "img/bin/Ship.png") :image-type :png) (sdl:point :x 30 :y (menu-item-y current-menu-item)))

       ;; Redraw the display
       (sdl:update-display)))))

(defun display-menu-buttons (selected-button)
  "Display a main menu with a few buttons to let the player start, quit, or set options.
   Highlight the selected-button."
  ;; Draw un-highlighted buttons.
  (loop for button across *menu-options* do
       (sdl:draw-surface-at (sdl-image:load-image (menu-item-image button) :image-type :png)
			    (sdl:point :x 400 :y (menu-item-y button))))

  ;; Highlight the currently selected button.
  (sdl:draw-surface-at (sdl-image:load-image (menu-item-rollover selected-button) :image-type :png) (sdl:point :x 400 :y (menu-item-y selected-button))))

;; TODO Eventually we'll need to add in a character/ship select and customization screen.
(defun go-to (menu-button)
  "Go to a next state depending on the menu button pressed. Options are starting a new game,
   changing options, or quitting."
  (case (menu-item-title menu-button)
    (start   (start-game)
	     (sdl:push-quit-event))
    (options (format t "Options.~%"))
    (quit    (sdl:push-quit-event))))

