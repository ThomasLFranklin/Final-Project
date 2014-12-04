;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Infiniano) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; Electronic Keyboard Program
;   By Thomas Franklin, Nish Dara, Blain Weeks, Devon Grove

; Required Packages
(require "songs/Songs.rkt")
(require rsound)
(require rsound/piano-tones)
(require 2htdp/image)
(require 2htdp/universe)

; Useful functions and definitions
(define ps (make-pstream))
(define (both a b) b)
(define (s seconds)
  (* 44100 seconds))
(check-expect (s 2) 88200)
(check-expect (s 0) 0)

; Program Features Wishlist and Bug Fixes:
;  Metronome feature
;  Demo songs feature-get the songs to stop!
;  Recording feature
;  Preset Beats

; initial state of the keyList element of the WorldState
(define INITIAL_KEYBOARD (list "plholder" #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

; a met is a structure of 2 elements
; on? is the t/f state of the metronome, determining whether it is playing or not
; bpm is the beats per minute at which the metronome is playing
(define-struct met (on? bpm current-time))
(define INITIAL_MET (make-met #f 100 0))

; a world is a worldState structure of 6 elements
; keyList refers to a list corresponding to the state of all 24 keys, whether they are turned "on" (t) or "off" (f)
; inst refers to the instrument (represented by a positive integer) the note will be played with
; oct refers to the range (represented by -1, 0, or 1) of the tones of the playble notes on the keyboard
; vol refers to the volume multiplier (represented by a number between 0 and 1) of the notes
; met refers to whether or not the metronome is on and what rate it is ticking at
; mode is a string that refers to the current state of the program
; demo-mode is the current song being played, represented as a list of notes. If the list is empty, no song is playing.
(define-struct world (keyList inst oct vol met mode demo-mode))
(define INITIAL_STATE (make-world INITIAL_KEYBOARD 1 0 1 INITIAL_MET "title screen" empty))

;Instuments
(define trumpet (rs-scale .5 (rs-read "sounds/Trumpet.wav")))
(define flute (rs-scale .5 (rs-read "sounds/Pan Flute.wav")))
(define sax (rs-scale .5 (rs-read "sounds/Sax.wav")))
(define strings (rs-scale .5 (rs-read "sounds/Synth Strings.wav")))
(define musicbox (rs-scale .5 (rs-read "sounds/Music Box.wav")))
(define synth (rs-scale .5 (rs-read "sounds/Synth Pad.wav")))
(define harp (rs-scale .5 (rs-read "sounds/Harp.wav")))


; The worldstate is a structure

(define (main w)
(big-bang w
          [to-draw graphics]
          ;NOTE: DO NOT CHANGE THE INTERVAL FOR THE ON-TICK FUNCTION OR THE DEMO SONGS WILL NOT WORK
          [on-tick tock 1/20]
          [on-key key-handler]
          [on-release reset]
          [on-mouse mousehandler]))

;Changes the nth number of the given list to the specified value by returning a new list with the nth value changed
(define (list-change list index value)
  (if (null? list)
    list
    (cons
      (if (zero? index)
        value
        (car list))
      (list-change (cdr list) (- index 1) value))))

; Keyhandler functions

(define keymap
  (map 
   list
   (string->list "q2w3er5t6y7uzsxdcvgbhnjm")
   (range 48 72 1)))

(define chars
  (map string (string->list "q2w3er5t6y7uzsxdcvgbhnjm")))

(define key-chars
  (map string (string->list "qwertyuzxcvbnm23567sdghj")))

(define keymap2
  (map 
   list
   (string->list "qwertyuzxcvbnm23567sdghj")
   (range 1 25 1)))


(check-expect (second (assq #\m keymap)) 71)

;; is this key one of the keyboard keys
(define (is-keyboard-key? k)
  (member k chars))

;; given a keyboard key, find the appropirate midi note number
(define (lookup-key k)
  (second (assoc (first (string->list k)) keymap)))
(define (lookup-key2 k)
  (second (assoc (first (string->list k)) keymap2)))

(check-expect (lookup-key "m") 71)

; Function for when the program is in "play" mode
; Changes the given changes an element in the keybooleen struct to true when an alpha-numeric key is pressed or
; Changes the given world-oct when either the "up" or "down" key is pressed
; WorldState keyEvent -> WorldState (plays pstream)
(define (play+light-up-keys w key)
  (both
   (cond 
     [(is-keyboard-key? key) 
      (play-note (+ (lookup-key key) (* (world-oct w) 24)) w)]
     [else w])
   (cond
     [(is-keyboard-key? key) 
      (make-world (list-change (world-keyList w) (lookup-key2 key) #t) (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "up") (if (< (world-oct w) 1) (make-world (world-keyList w) (world-inst w) (+ (world-oct w) 1) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w)) w)]
     [(key=? key "down") (if (> (world-oct w) -1) (make-world (world-keyList w) (world-inst w) (- (world-oct w) 1) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w)) w)]
     [else w])))

; Keyhandler function
; Passes key event and worldstate through to different functions based on the world-mode of the world
(define (key-handler w key)
  (cond
    [(string=? (world-mode w) "play") (play+light-up-keys w key)]
    [else w]))
    
; Helper function for the on-key function
; Plays a note
; worldstate number -> sound
(define (play-note num w)
  (cond
    [(= (world-inst w) 1) (pstream-play ps (rs-scale (world-vol w) (piano-tone num)))]
    [(= (world-inst w) 2) (pstream-play ps (rs-scale (world-vol w) (clip trumpet (s (* 2 (- num 24))) (s (* 2 (- num 23.25))))))]
    [(= (world-inst w) 3) (pstream-play ps (rs-scale (world-vol w) (clip flute (s (* 2 (- num 24))) (s (* 2 (- num 23.25))))))]
    [(= (world-inst w) 4) (pstream-play ps (rs-scale (world-vol w) (clip sax (s (* 2 (- num 24))) (* 2 (s (- num 23.25))))))]
    [(= (world-inst w) 5) (pstream-play ps (rs-scale (world-vol w) (clip harp (s (* 2 (- num 24))) (s (* 2 (- num 23.25))))))]
    [(= (world-inst w) 6) (pstream-play ps (rs-scale (world-vol w) (clip musicbox (s (* 2 (- num 24))) (s ( * 2 (- num 23.25))))))]
    [(= (world-inst w) 7) (pstream-play ps (rs-scale (world-vol w) (clip strings (s (* 2 (- num 24))) (s (* 2(- num 23.25))))))]
    [(= (world-inst w) 8) (pstream-play ps (rs-scale (world-vol w) (clip synth (s (* 2 (- num 24))) (s (* 2 (- num 23.25))))))]
    [else w]))

; Functions for the on-release handler

; Function for when the program is in "play" mode
; Changes the coresponding part of the keyList struct to false when that key is released
; worldstare keyevent -> worldstate
(define (reset w key)
  (cond
    [(is-keyboard-key? key) 
      (make-world (list-change (world-keyList w) (lookup-key2 key) #f) (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
    [else w]))

; Key-release handler function
; Passes the worldstate and key-release event to a helper function based on the world-mode of the world
(define (key-release-handler w key)
  (cond
    [(string=? (world-mode w) "play") (reset w key)]
    [else w]))

; Functions for the mousehandler

; Function for when the program mode is "title screen"
; worldstate mouse-event -> worldstate
(define (mousehandler-title w x y me)
  (cond
    [(mouse=? "button-up" me) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "waiver" (world-demo-mode w))]
    [else w]))

; Function for when the program mode is "waiver"
; worldkstate mouse-event -> worldstate
(define (mousehandler-waiver w x y me)
  (cond
    [(mouse=? "button-up" me) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))]
    [else w]))

; Function for when the program mode is "main menu"
; worldstate mouse-event -> worldstate
(define (mousehandler-menu w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x 500) (< x 700) (> y 225) (< y 275)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "play" (world-demo-mode w))]
                                  [(and (> x 500) (< x 700) (> y 325) (< y 375)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "demo" (world-demo-mode w))]
                                  [(and (> x 500) (< x 700) (> y 425) (< y 475)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "record" (world-demo-mode w))]
                                  [(and (> x 500) (< x 700) (> y 525) (< y 575)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "instructions" (world-demo-mode w))]
                                  [else w])]
    [else w]))

;Function for when the program mode is "record"
(define (mousehandler-record w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x 1025) (< x 1125) (> y 25) (< y 75)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))]
                                  [else w])]
    [else w]))

; Function for when the program mode is "instructions"
(define (mousehandler-instructions w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                   [(and (> x 1025) (< x 1125) (> y 25) (< y 75)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))]
                                   [else w])]
    [else w]))
                                   
; Function for when the program mode is "play"
; worldstate mouse-event -> worldstate
(define (mousehandler-play w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x (* len 9/64)) (< x (+ (* len 9/64) 8)) (> y (- (* wid 4/5) 4)) (< y (+ (* wid 4/5) 4))) (make-world (world-keyList w) 1 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (* len 9/64)) (< x (+ (* len 9/64) 8)) (> y (+ (* wid 4/5) 22)) (< y (+ (* wid 4/5) 30))) (make-world (world-keyList w) 2 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (* len 9/64)) (< x (+ (* len 9/64) 8)) (> y (+ (* wid 4/5) 48)) (< y (+ (* wid 4/5) 56))) (make-world (world-keyList w) 3 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (* len 9/64)) (< x (+ (* len 9/64) 8)) (> y (+ (* wid 4/5) 74)) (< y (+ (* wid 4/5) 82))) (make-world (world-keyList w) 4 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (- (- (* len 19/64) 12) 4)) (< x (+ (- (* len 19/64) 12) 4)) (> y (- (* wid 4/5) 4)) (< y (+ (* wid 4/5) 4))) (make-world (world-keyList w) 5 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (- (- (* len 19/64) 12) 4)) (< x (+ (- (* len 19/64) 12) 4)) (> y (+ (* wid 4/5) 22)) (< y (+ (* wid 4/5) 30))) (make-world (world-keyList w) 6 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (- (- (* len 19/64) 12) 4)) (< x (+ (- (* len 19/64) 12) 4)) (> y (+ (* wid 4/5) 48)) (< y (+ (* wid 4/5) 56))) (make-world (world-keyList w) 7 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x (- (- (* len 19/64) 12) 4)) (< x (+ (- (* len 19/64) 12) 4)) (> y (+ (* wid 4/5) 74)) (< y (+ (* wid 4/5) 82))) (make-world (world-keyList w) 8 (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
                                  [(and (> x 1025) (< x 1125) (> y 25) (< y 75)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))]
                                  [(and (> x (- (/ len 2) 50)) (< x (- (/ len 2) 30)) (> y (- (* wid 5/6) 105)) (< y (- (* wid 5/6) 85))) (if (= 0 (world-vol w)) (make-world (world-keyList w) (world-inst w) (world-oct w) 1 (world-met w) (world-mode w) (world-demo-mode w)) (make-world (world-keyList w) (world-inst w) (world-oct w) 0 (world-met w) (world-mode w) (world-demo-mode w)))]
                                  [(and (< x (+ (/ len 2) 50)) (> x (+ (/ len 2) 30)) (> y (- (* wid 5/6) 105)) (< y (- (* wid 5/6) 85))) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (make-met #t (met-bpm (world-met w)) (met-current-time (world-met w))) (world-mode w) (world-demo-mode w))]
                                  [else w])]
    [(mouse=? "drag" me) (cond
                           [(and (> x (- (/ len 2) 50)) (< x (- (/ len 2) 30)) (> y (- (* wid 5/6) 75)) (< y (+ (* wid 5/6) 75))) (make-world (world-keyList w) (world-inst w) (world-oct w) (- 1 (/ (- y (- (* wid 5/6) 75)) 150)) (world-met w) (world-mode w) (world-demo-mode w))]
                           [(and (> x (+ (/ len 2) 30)) (< x (+ (/ len 2) 50)) (> y (- (* wid 5/6) 75)) (< y (+ (* wid 5/6) 75))) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (make-met (met-on?(world-met w)) (- 100 (/ (- y (- (* wid 5/6) 75)) 150)) (met-current-time (world-met w))) (world-mode w) (world-demo-mode w))]
                           [else w])]
    [else w]))


; Function for when the program mode is "demo"
; worldstate mouse-event -> worldstate
; NOTE: The code for the songs is defined in the "Songs.rkt" file located in the songs folder
(define (mousehandler-demo w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x 200) (< x 400) (> y 325) (< y 375)) (start-playing-song w lights)]
                                  [(and (> x 200) (< x 400) (> y 425) (< y 475)) (start-playing-song w radioactive)]
                                  [(and (> x 200) (< x 400) (> y 525) (< y 575)) (start-playing-song w sail)]
                                  [(and (> x 800) (< x 1000) (> y 325) (< y 375)) (start-playing-song w summertime-sadness)]
                                  [(and (> x 800) (< x 1000) (> y 425) (< y 475)) (start-playing-song w wonderwall)]
                                  [(and (> x 800) (< x 1000) (> y 525) (< y 575)) (start-playing-song w clocks)]
                                  [(and (> x 550) (< x 650) (> y 350) (< y 550)) (make-world (world-keyList w) 
                                                                                             (world-inst w) 
                                                                                             (world-oct w) 
                                                                                             (world-vol w) 
                                                                                             (world-met w) 
                                                                                             (world-mode w) 
                                                                                             empty)]
                                  [(and (> x 1025) (< x 1125) (> y 25) (< y 75)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))])]
    [else w]))




; Functions for playing songs in demo mode
; These are all helper functions to make a list of notes playble in our program

; Extracts the list of notes from the world struct, then passes it into the play-song function
(define (extract-list w)
  (cond
    [(empty? (world-demo-mode w)) w]
    [(cons? (world-demo-mode w)) (play-song (world-demo-mode w) w)]
    ))

; Passes each individual note from the list of notes into the play-sound function
(define (play-song lon w)
  (cond
    [(empty? lon) w]
    [(cons? lon) (both (play-sound (first lon) w) (play-song (rest lon) w))]))

; Plays a single note from the list
(define (play-sound n w)
  (local [(define sound1 (piano-tone (note-note-num n)))]
    (cond
      [(and (> (note-time n) (- (pstream-current-frame ps) 4000)) (< (note-time n) (+ (pstream-current-frame ps) 4000))) (both (pstream-queue ps 
                                                                                                                                     (clip sound1 0 (min (rs-frames sound1)
                                                                                                                                                         (note-duration n)))
                                                                                                                                     (note-time n))
                                                                                                                      w)]
      [else w])))

; Function used to determine the correct time in the pstream to start playing the song, since the pstream starts automatically when the program starts
; This prevents the song from starting half way through when you click to start it
(define (start-playing-song w lon) 
  (cond
    [(empty? lon) empty]
    [(cons? lon) (make-world (world-keyList w) 
                             (world-inst w) 
                             (world-oct w) 
                             (world-vol w) 
                             (world-met w) 
                             (world-mode w) 
                             (start-playing-song-helper lon (pstream-current-frame ps)))]))
(define (start-playing-song-helper lon t) 
  (cond
    [(empty? lon) empty]
    [(cons? lon) (cons (make-note (note-note-num (first lon)) 
                                  (+ (s 2) t (note-time (first lon))) 
                                  (note-duration (first lon))) 
                       (start-playing-song-helper (rest lon) t))]))


; Mousehandler function
; Passes the worldstate, x position, y position, and mouse event to a helper function based on the world-mode of the world
(define (mousehandler w x y me)
  (cond
    [(string=? (world-mode w) "title screen") (mousehandler-title w x y me)]
    [(string=? (world-mode w) "waiver") (mousehandler-waiver w x y me)]
    [(string=? (world-mode w) "main menu") (mousehandler-menu w x y me)]
    [(string=? (world-mode w) "play") (mousehandler-play w x y me)]
    [(string=? (world-mode w) "demo") (mousehandler-demo w x y me)]
    [(string=? (world-mode w) "instructions") (mousehandler-instructions w x y me)]
    [(string=? (world-mode w) "record") (mousehandler-record w x y me)]
    ))
  


; On-tick functions
; Just like all the other functions, the on-tick function is determined by the mode of the program.

; Metronome functions
; On-tick function for demo mode
; Passes the list of notes to the play-song function to be played.
(define (on-tick-for-demo-songs w) (extract-list w))

; Main on-tick function
(define (tock w)
  (cond
    [(string=? (world-mode w) "play") (check-metronome w)]
    [(string=? (world-mode w) "demo") (on-tick-for-demo-songs w)]
    [else w]
    ))

;;Check Metronome Function
(define (check-metronome w)
  (cond
    [(met-on? (world-met w)) (if (>= (met-current-time (world-met w)) (/ 1 (+ 1 (/ (met-bpm (world-met w)) 50)))) 
                                  (both (pstream-play ps ding) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (make-met (met-on? (world-met w)) (met-bpm (world-met w)) 0) (world-mode w) (world-demo-mode w))) 
                                  (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (make-met (met-on? (world-met w)) (met-bpm (world-met w)) (+ 1/20 (met-current-time (world-met w)))) (world-mode w) (world-demo-mode w)))]
    [else w]))


; Functions for the graphical interface
; The Grphical Interface is based on the mode element of the world struct. World-mode can be 
; - "title screen"
; - "menu"
; - "play"
; - "demo"
; - more coming later
; Constant Definitions for all modes
(define box-color (make-color 40 150 250))
(define len 1200)
(define wid 650)


; Functions for the "title screen" mode
; "title screen" is the openning mode of the program, showing the name of the program and the creators
(define (title-screen w)
  (place-images (list (text/font "Infiniano" 100 "white" "Palatino Linotype" 'default 'italic 'normal #f)
                      (text "\"The Infinite Piano\"" 30 "white" )
                      (text "Click Anywhere to Start" 20 "white" )
                      (bitmap/file "graphics/background.jpg")
                      )
                (list (make-posn (/ len 2) (/ wid 4))
                      (make-posn (/ len 2) 250)
                      (make-posn (/ len 2) (* wid 2/3))
                      (make-posn (/ len 2) (/ wid 2))
                      )
                (rectangle len wid "solid" box-color)))

; Functions for the "waiver" mode
; "waiver" is the terms and conditions of the program and release of liability
(define (waiver w)
  (place-images (list
                 (text "Click to Accept Terms and Conditions and Advance to Main Menu" 22 "white")
                 (bitmap/file "Infiniano-waiver.jpg")
                 (bitmap/file "graphics/background.jpg")
                 )
                (list
                 (make-posn (/ len 2) 625)
                 (make-posn (/ len 2) (/ wid 2))
                 (make-posn (/ len 2) (/ wid 2))
                 )
                (rectangle len wid "solid" box-color)))
                

; Functions for the "main menu"
; "main menu" mode is, well, the main menu for the program
(define (menu w)
  (place-images (list (text/font "Main Menu" 80 "white" "Palatino Linotype" 'default 'italic 'normal #f)
                      (text "Select a Mode to Begin" 20 "white" )
                      (text "Freeplay" 20 "white" )
                      (text "Demo Songs" 20 "white" )
                      (text "Record Mode" 20 "white" )
                      (text "Instructions" 20 "white" )
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (bitmap/file "graphics/background.jpg")
                      )
                (list (make-posn (/ len 2) (/ wid 6))
                      (make-posn (/ len 2) 170)
                      (make-posn (/ len 2) 250)
                      (make-posn (/ len 2) 350)
                      (make-posn (/ len 2) 450)
                      (make-posn (/ len 2) 550)
                      (make-posn (/ len 2) 250)
                      (make-posn (/ len 2) 350)
                      (make-posn (/ len 2) 450)
                      (make-posn (/ len 2) 550)
                      (make-posn (/ len 2) (/ wid 2))
                      )
                (rectangle len wid "solid" box-color)))

; Functions for the "intstructions mode
; Instructions displays directions on how to use the program
(define (instructions w)
  (place-images (list
                 (text "Menu" 30 "white")
                 (rectangle 100 50 "solid" box-color)
                 (text "Play Mode" 30 "white")
                 (text "Use keys on the keyboard to play the respective keys on the piano" 22 "white")
                 (text "Click on boxes next to the instrument to change the instrument played" 22 "white")
                 (text "Drag the volume slider to change volume" 22 "white")
                 (text "Drag the metronome slider to change rate of metronome" 22 "white")
                 (text "Use the up and down arrow keys to change octaves" 22 "white")
                 (text "This is a placeholder to be replaced" 22 "white")
                 (text "Did you know if a giraffe drank coffee, it would be cold before it got to its stomach" 22 "white")
                 (text "Of course you didn't, because all you think about is yourself" 22 "white")
                 (text "What has 6 wheels and flies??? A garbage truck hahaha" 22 "white")
                 (text "I'm running out of things for placeholders" 22 "white")
                 (text "DO IT FOR THE RACKETS" 22 "white")
                 (rectangle 900 550 "solid" box-color)
                 (bitmap/file "graphics/background.jpg"))
                (list
                 (make-posn 1075 50)
                 (make-posn 1075 50)
                 (make-posn (/ len 2) (* wid 2/26))
                 (make-posn (/ len 2) (* wid 4/26))
                 (make-posn (/ len 2) (* wid 6/26))
                 (make-posn (/ len 2) (* wid 8/26))
                 (make-posn (/ len 2) (* wid 10/26))
                 (make-posn (/ len 2) (* wid 12/26))
                 (make-posn (/ len 2) (* wid 14/26))
                 (make-posn (/ len 2) (* wid 16/26))
                 (make-posn (/ len 2) (* wid 18/26))
                 (make-posn (/ len 2) (* wid 20/26)) 
                 (make-posn (/ len 2) (* wid 22/26))
                 (make-posn (/ len 2) (* wid 24/26))
                 (make-posn (/ len 2) (* wid 29/52))
                 (make-posn (/ len 2) (/ wid 2)))
                (rectangle len wid "solid" box-color)))

; Functions for the "record" mode
; Record allows the user to record a series of notes being played
(define (record w)
  (place-images (list
                 (text "Menu" 30 "white")
                 (rectangle 100 50 "solid" box-color)
                 (bitmap/file "graphics/background.jpg"))
                (list
                 (make-posn 1075 50)
                 (make-posn 1075 50)
                 (make-posn (/ len 2) (/ wid 2)))
                (rectangle len wid "solid" box-color)))
                

; Functions for the "play" mode
; "play" is the main mode for the program, allowing users to play different notes on the keyboard 

;Constnats for "play" mode
(define wkeylen 75)
(define wkeywid 310)
(define wkey-y-pos 270)
(define bkeylen 48)
(define bkeywid 170)
(define bkey-y-pos 200)

; Color change functions
; Change the color of the on screen keyboard when a key is held or released
; worldstate -> image
(define (drawkey n world)
  (if (and (list-ref (world-keyList world) n) #t)
      (rectangle wkeylen wkeywid "solid" "yellow")
      (rectangle wkeylen wkeywid "solid" "white")))

(define (bk1 w) (if (and (list-ref (world-keyList w) 15) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk2 w) (if (and (list-ref (world-keyList w) 16) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk3 w) (if (and (list-ref (world-keyList w) 17) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk4 w) (if (and (list-ref (world-keyList w) 18) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk5 w) (if (and (list-ref (world-keyList w) 19) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk6 w) (if (and (list-ref (world-keyList w) 20) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk7 w) (if (and (list-ref (world-keyList w) 21) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk8 w) (if (and (list-ref (world-keyList w) 22) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk9 w) (if (and (list-ref (world-keyList w) 23) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk10 w) (if (and (list-ref (world-keyList w) 24) #t) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))

; Functions for the placement of the keys (white, black, and outlines)
; Places the keys in the correct positions in the world
(define (key-outlines w) (place-image (beside (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")
                                              (rectangle wkeylen wkeywid "outline" "black")) (/ len 2) wkey-y-pos (rectangle len wid "outline" box-color)))
;; place a list of things beside each other
(define (list-beside l)
  (foldr beside INVISIBLE-RECTANGLE l))

;; a rectangle that's invisible (used as base case for list-beside)
(define INVISIBLE-RECTANGLE (rectangle 0 0 "solid" "black"))

(define (wkfun n w) (drawkey n w))

(define (white-keys w) 
  (place-image (list-beside
                (map (lambda (n)
                       (wkfun n w))
                     (range 1 15 1)))
               (/ len 2)
               wkey-y-pos
               (rectangle len wid "outline" box-color)))

(define (black-keys w) (place-images 
                        (list 
                         (text "2" 18 box-color)
                         (text "3" 18 box-color)
                         (text "5" 18 box-color)
                         (text "6" 18 box-color)
                         (text "7" 18 box-color)
                         (text "S" 18 box-color)
                         (text "D" 18 box-color)
                         (text "G" 18 box-color)
                         (text "H" 18 box-color)
                         (text "J" 18 box-color)
                         (bk1 w)
                         (bk2 w)
                         (bk3 w)
                         (bk4 w)
                         (bk5 w)
                         (bk6 w)
                         (bk7 w)
                         (bk8 w)
                         (bk9 w)
                         (bk10 w))
                        (list 
                         (make-posn (- (* len 2/16) 8) bkey-y-pos)
                         (make-posn (+ (* len 3/16) 8) bkey-y-pos)
                         (make-posn (- (* len 5/16) 8) bkey-y-pos)
                         (make-posn (* len 6/16) bkey-y-pos)
                         (make-posn (+ (* len 7/16) 8) bkey-y-pos)
                         (make-posn (- (* len 9/16) 8) bkey-y-pos)
                         (make-posn (+ (* len 10/16) 8) bkey-y-pos)
                         (make-posn (- (* len 12/16) 8) bkey-y-pos)
                         (make-posn (* len 13/16) bkey-y-pos)
                         (make-posn (+ (* len 14/16) 8) bkey-y-pos)
                         (make-posn (- (* len 2/16) 8) bkey-y-pos)
                         (make-posn (+ (* len 3/16) 8) bkey-y-pos)
                         (make-posn (- (* len 5/16) 8) bkey-y-pos)
                         (make-posn (* len 6/16) bkey-y-pos)
                         (make-posn (+ (* len 7/16) 8) bkey-y-pos)
                         (make-posn (- (* len 9/16) 8) bkey-y-pos)
                         (make-posn (+ (* len 10/16) 8) bkey-y-pos)
                         (make-posn (- (* len 12/16) 8) bkey-y-pos)
                         (make-posn (* len 13/16) bkey-y-pos)
                         (make-posn (+ (* len 14/16) 8) bkey-y-pos))
                      (rectangle len wid "outline" box-color)))

; Functions for text and extras

; Text for keyboard range selector
(define len1 (* 6 wkeylen))
(define wid1 150)
(define box-text1 (rectangle len1 wid1 "solid" box-color))

(define (text1 w) (place-images
                  (list
                   (text/font "Range of Notes" 20 "white" "Palatino Linotype" 'default 'normal 'normal #t)
                   (text "Press the up or down arrow keys to change the range of the notes." 10 "white")
                   (text "Current Note Range:" 14 "white")
                   (bitmap/file "graphics/keyboard.jpg")
                   (rectangle 130 15 "solid" (if (= (world-oct w) -1) "yellow" box-color))
                   (rectangle 130 15 "solid" (if (= (world-oct w) 0) "yellow" box-color))
                   (rectangle 130 15 "solid" (if (= (world-oct w) 1) "yellow" box-color)))
                  (list
                   (make-posn (/ len1 2) 14)
                   (make-posn (/ len1 2) 40)
                   (make-posn (/ len1 2) 55)
                   (make-posn (/ len1 2) 110)
                   (make-posn 95 77)
                   (make-posn 225 77)
                   (make-posn 355 77))
                  box-text1))

; Text for the instrument selector
(define len2 (* 6 wkeylen))
(define wid2 150)
(define box-text2 (rectangle len2 wid2 "solid" box-color))

(define (text2 w) (place-image
                   (text/font "Instrument Selector" 20 "white" "Palatino Linotype" 'default 'normal 'normal #t) (/ len2 2) 14
                   box-text2))

(define inst1text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Piano" 14 "white")))

(define inst2text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Trumpet" 14 "white")))

(define inst3text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Pan Flute" 14 "white")))

(define inst4text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Alto Sax" 14 "white")))

(define inst5text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Harp" 14 "white")))

(define inst6text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Music Box" 14 "white")))

(define inst7text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Synth Strings" 14 "white")))

(define inst8text (beside
                        (rectangle 8 8 "outline" "black")
                        (rectangle 8 8 "solid" box-color)
                        (text "Synth Pad" 14 "white")))

; Function for the volume slider
; Draws a slider that changes the volume
; worldstate->worldstate
(define (volume-slider w) (place-image
                    (add-line
                     (rectangle 2 150 "solid" "white") -10 (* 150 (- 1 (world-vol w)) ) 10 (* 150 (- 1 (world-vol w))) (make-pen box-color 10 "solid" "round" "round")) (- (/ len 2) 40) (* wid 5/6) (rectangle len wid "outline" box-color)))

; Function for the metronome slider
; Draws a slider that changes the volume
; worldstate->worldstate
(define (metronome-slider w) (place-image
                    (add-line
                     (rectangle 2 150 "solid" "white") -10 (* 150 (- 100 (met-bpm (world-met w))) ) 10 (* 150 (- 100 (met-bpm (world-met w)))) (make-pen box-color 10 "solid" "round" "round")) (+ (/ len 2) 40) (* wid 5/6) (rectangle len wid "outline" box-color)))


; Main renedering for the "play" mode
(define (key-board w) (place-images
                       (list
                        (text/font "Infiniano" 60 "white" "Palatino Linotype" 'default 'italic 'normal #f)
                        (text "Volume" 16 "white")
                        (text "Metronome" 16 "white")
                        (volume-slider w)
                        (metronome-slider w)
                        (text "Q" 18 box-color)
                        (text "W" 18 box-color)
                        (text "E" 18 box-color)
                        (text "R" 18 box-color)
                        (text "T" 18 box-color)
                        (text "Y" 18 box-color)
                        (text "U" 18 box-color)
                        (text "Z" 18 box-color)
                        (text "X" 18 box-color)
                        (text "C" 18 box-color)
                        (text "V" 18 box-color)
                        (text "B" 18 box-color)
                        (text "N" 18 box-color)
                        (text "M" 18 box-color)
                        inst1text
                        inst2text
                        inst3text
                        inst4text
                        inst5text
                        inst6text
                        inst7text
                        inst8text
                        (rectangle 8 8 "solid" (if (= (world-inst w) 1) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 2) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 3) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 4) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 5) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 6) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 7) "yellow" box-color))
                        (rectangle 8 8 "solid" (if (= (world-inst w) 8) "yellow" box-color))
                        (text2 w)
                        (text1 w)
                        (black-keys w)
                        (key-outlines w)
                        (white-keys w)
                        (text "Menu" 30 "white")
                        (rectangle 100 50 "solid" box-color)
                        (bitmap/file "graphics/background.jpg"))
                       (list
                        (make-posn (/ len 2) 55)
                        (make-posn (- (/ len 2) 40) (- (* wid 5/6) 100))
                        (make-posn (+ (/ len 2) 40) (- (* wid 5/6) 100))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (* len 3/32) (/ wid 2))
                        (make-posn (* len 5/32) (/ wid 2))
                        (make-posn (* len 7/32) (/ wid 2))
                        (make-posn (* len 9/32) (/ wid 2))
                        (make-posn (* len 11/32) (/ wid 2))
                        (make-posn (* len 13/32) (/ wid 2))
                        (make-posn (* len 15/32) (/ wid 2))
                        (make-posn (* len 17/32) (/ wid 2))
                        (make-posn (* len 19/32) (/ wid 2))
                        (make-posn (* len 21/32) (/ wid 2))
                        (make-posn (* len 23/32) (/ wid 2))
                        (make-posn (* len 25/32) (/ wid 2))
                        (make-posn (* len 27/32) (/ wid 2))
                        (make-posn (* len 29/32) (/ wid 2))
                        (make-posn (+ (* len 9/64) (/ (image-width inst1text) 2)) (* wid 4/5))
                        (make-posn (+ (* len 9/64) (/ (image-width inst2text) 2)) (+ (* wid 4/5) 26))
                        (make-posn (+ (* len 9/64) (/ (image-width inst3text) 2)) (+ (* wid 4/5) 52))
                        (make-posn (+ (* len 9/64) (/ (image-width inst4text) 2)) (+ (* wid 4/5) 78))
                        (make-posn (+ (* len 19/64) (- (/ (image-width inst5text) 2) 16)) (* wid 4/5))
                        (make-posn (+ (* len 19/64) (- (/ (image-width inst6text) 2) 16)) (+ (* wid 4/5) 26))
                        (make-posn (+ (* len 19/64) (- (/ (image-width inst7text) 2) 16)) (+ (* wid 4/5) 52))
                        (make-posn (+ (* len 19/64) (- (/ (image-width inst8text) 2) 16)) (+ (* wid 4/5) 78))
                        (make-posn (+ (* len 9/64) 4) (* wid 4/5))
                        (make-posn (+ (* len 9/64) 4) (+ (* wid 4/5) 26))
                        (make-posn (+ (* len 9/64) 4) (+ (* wid 4/5) 52))
                        (make-posn (+ (* len 9/64) 4) (+ (* wid 4/5) 78))
                        (make-posn (- (* len 19/64) 12) (* wid 4/5))
                        (make-posn (- (* len 19/64) 12) (+ (* wid 4/5) 26))
                        (make-posn (- (* len 19/64) 12) (+ (* wid 4/5) 52))
                        (make-posn (- (* len 19/64) 12) (+ (* wid 4/5) 78))
                        (make-posn (* len 1/4) (* wid 5/6))
                        (make-posn (* len 3/4) (* wid 5/6))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn 1075 50)
                        (make-posn 1075 50)
                        (make-posn (/ len 2) (/ wid 2)))
                       (rectangle len wid "solid" box-color)))

; Functions for "demo" mode
; "demo" mode allows users to play the selection of preset songs
(define (demo w)
  (place-images (list (text/font "Demo Mode" 80 "white" "Palatino Linotype" 'default 'italic 'normal #f)
                      (text "Select a Song to Start it Playing" 20 "white" )
                      (text "Lights" 20 "white" )
                      (text "Radioactive" 20 "white" )
                      (text "Sail" 20 "white" )
                      (text "Summertime Sadness" 20 "white" )
                      (text "Wonderwall" 20 "white" )
                      (text "Clocks" 20 "white" )
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (text "Menu" 30 "white")
                      (rectangle 100 50 "solid" box-color)
                      (text "Stop" 35 "white")
                      (rectangle 100 100 "solid" "red")
                      (bitmap/file "graphics/background.jpg")
                      )
                (list (make-posn (/ len 2) (/ wid 6))
                      (make-posn (/ len 2) 170)
                      (make-posn (/ len 4) 350)
                      (make-posn (/ len 4) 450)
                      (make-posn (/ len 4) 550)
                      (make-posn (* len 3/4) 350)
                      (make-posn (* len 3/4) 450)
                      (make-posn (* len 3/4) 550)
                      (make-posn (/ len 4) 350)
                      (make-posn (/ len 4) 450)
                      (make-posn (/ len 4) 550)
                      (make-posn (* len 3/4) 350)
                      (make-posn (* len 3/4) 450)
                      (make-posn (* len 3/4) 550)
                      (make-posn 1075 50)
                      (make-posn 1075 50)
                      (make-posn (/ len 2) 450)
                      (make-posn (/ len 2) 450)
                      (make-posn (/ len 2) (/ wid 2))
                      )
                (rectangle len wid "solid" box-color)))


; Main function for the graphical interface of the program
; Draws the graphical interface of the program, based on the world-mode field of the worldstate
(define (graphics w)
  (cond
    [(string=? (world-mode w) "title screen") (title-screen w)]
    [(string=? (world-mode w) "waiver") (waiver w)]
    [(string=? (world-mode w) "main menu") (menu w)]
    [(string=? (world-mode w) "play") (key-board w)]
    [(string=? (world-mode w) "demo") (demo w)]
    [(string=? (world-mode w) "instructions") (instructions w)]
    [(string=? (world-mode w) "record") (record w)]
    ))
 
; Main function that runs the program
(main INITIAL_STATE)