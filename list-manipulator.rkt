;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Infiniano) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
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

; Program Features Wishlist and Bug Fixes:
;  Metronome feature
;  Demo songs feature-get the songs to stop!
;  Recording feature


; a keyList is a structure of 24 elements
; wk(n) is the t/f state of the nth white key from left of the keyboard where n is an integer between 1 and 14
; bk(n) is the t/f state of the nth black key from left of the keyboard where n is an integer between 1 and 10
(define-struct keyList (wk1 wk2 wk3 wk4 wk5 wk6 wk7 wk8 wk9 wk10 wk11 wk12 wk13 wk14 bk1 bk2 bk3 bk4 bk5 bk6 bk7 bk8 bk9 bk10))
; the initial keyboard is a list-of-booleans mapped to each key
(define INITIAL_KEYBOARD (list "plholder" #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

; a met is a structure of 2 elements
; met? is the t/f state of the metronome, determining whether it is playing or not
; bpm is the beats per minute at which the metronome is playing
(define-struct met (met? bpm))
(define INITIAL_MET (make-met #f 100))

; a demo-mode is a structure of 2 elements
; playing? is the t/f state determining whether a song is playing or not
; song is a list of notes to be played
(define-struct demo-mode (playing? song))
(define INITIAL_DEMO-MODE (make-demo-mode #f empty))

; a world is a worldState structure of 6 elements
; keyList refers to the current keyList state of the program
; inst refers to the instrument (represented by a positive integer) the note will be played with
; oct refers to the range (represented by -1, 0, or 1) of the tones of the playble notes on the keyboard
; vol refers to the volume multiplier (represented by a number between 0 and 1) of the notes
; met refers to whether or not the metronome is on and what rate it is ticking at
; mode is a string that refers to the current state of the program
(define-struct world (keyList inst oct vol met mode demo-mode))
(define INITIAL_STATE (make-world INITIAL_KEYBOARD 1 0 1 INITIAL_MET "title screen" INITIAL_DEMO-MODE))

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
          ;[on-tick check-metronome]
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

; Function for when the program is in "play" mode
; Changes the given changes an element in the keybooleen struct to true when an alpha-numeric key is pressed or
; Changes the given world-oct when either the "up" or "down" key is pressed
; WorldState keyEvent -> WorldState (plays pstream)
(define (play+light-up-keys w key)
  (both
   (cond
    [(key=? key "q") (play-note (+ 48 (* (world-oct w) 24)) w)]
    [(key=? key "2") (play-note (+ 49 (* (world-oct w) 24)) w)]
    [(key=? key "w") (play-note (+ 50 (* (world-oct w) 24)) w)]
    [(key=? key "3") (play-note (+ 51 (* (world-oct w) 24)) w)]
    [(key=? key "e") (play-note (+ 52 (* (world-oct w) 24)) w)]
    [(key=? key "r") (play-note (+ 53 (* (world-oct w) 24)) w)]
    [(key=? key "5") (play-note (+ 54 (* (world-oct w) 24)) w)]
    [(key=? key "t") (play-note (+ 55 (* (world-oct w) 24)) w)]
    [(key=? key "6") (play-note (+ 56 (* (world-oct w) 24)) w)]
    [(key=? key "y") (play-note (+ 57 (* (world-oct w) 24)) w)]
    [(key=? key "7") (play-note (+ 58 (* (world-oct w) 24)) w)]
    [(key=? key "u") (play-note (+ 59 (* (world-oct w) 24)) w)]
    [(key=? key "z") (play-note (+ 60 (* (world-oct w) 24)) w)]
    [(key=? key "s") (play-note (+ 61 (* (world-oct w) 24)) w)]
    [(key=? key "x") (play-note (+ 62 (* (world-oct w) 24)) w)]
    [(key=? key "d") (play-note (+ 63 (* (world-oct w) 24)) w)]
    [(key=? key "c") (play-note (+ 64 (* (world-oct w) 24)) w)]
    [(key=? key "v") (play-note (+ 65 (* (world-oct w) 24)) w)]
    [(key=? key "g") (play-note (+ 66 (* (world-oct w) 24)) w)]
    [(key=? key "b") (play-note (+ 67 (* (world-oct w) 24)) w)]
    [(key=? key "h") (play-note (+ 68 (* (world-oct w) 24)) w)]
    [(key=? key "n") (play-note (+ 69 (* (world-oct w) 24)) w)]
    [(key=? key "j") (play-note (+ 70 (* (world-oct w) 24)) w)]
    [(key=? key "m") (play-note (+ 71 (* (world-oct w) 24)) w)]
    [else w])
   (cond
     [(key=? key "q") (make-world (list-change (world-keyList w) 1 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "2") (make-world (list-change (world-keyList w) 15 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "w") (make-world (list-change (world-keyList w) 2 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "3") (make-world (list-change (world-keyList w) 16 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "e") (make-world (list-change (world-keyList w) 3 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "r") (make-world (list-change (world-keyList w) 4 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "5") (make-world (list-change (world-keyList w) 17 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "t") (make-world (list-change (world-keyList w) 5 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "6") (make-world (list-change (world-keyList w) 18 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "y") (make-world (list-change (world-keyList w) 6 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "7") (make-world (list-change (world-keyList w) 19 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "u") (make-world (list-change (world-keyList w) 7 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "z") (make-world (list-change (world-keyList w) 8 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "s") (make-world (list-change (world-keyList w) 20 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "x") (make-world (list-change (world-keyList w) 9 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "d") (make-world (list-change (world-keyList w) 21 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "c") (make-world (list-change (world-keyList w) 10 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "v") (make-world (list-change (world-keyList w) 11 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "g") (make-world (list-change (world-keyList w) 22 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "b") (make-world (list-change (world-keyList w) 12 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "h") (make-world (list-change (world-keyList w) 23 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "n") (make-world (list-change (world-keyList w) 13 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "j") (make-world (list-change (world-keyList w) 24 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "m") (make-world (list-change (world-keyList w) 14 #t)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
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
     [(key=? key "q") (make-world (list-change (world-keyList w) 1 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "2") (make-world (list-change (world-keyList w) 15 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "w") (make-world (list-change (world-keyList w) 2 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "3") (make-world (list-change (world-keyList w) 16 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "e") (make-world (list-change (world-keyList w) 3 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "r") (make-world (list-change (world-keyList w) 4 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "5") (make-world (list-change (world-keyList w) 17 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "t") (make-world (list-change (world-keyList w) 5 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "6") (make-world (list-change (world-keyList w) 18 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "y") (make-world (list-change (world-keyList w) 6 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "7") (make-world (list-change (world-keyList w) 19 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "u") (make-world (list-change (world-keyList w) 7 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "z") (make-world (list-change (world-keyList w) 8 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "s") (make-world (list-change (world-keyList w) 20 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "x") (make-world (list-change (world-keyList w) 9 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "d") (make-world (list-change (world-keyList w) 21 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "c") (make-world (list-change (world-keyList w) 10 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "v") (make-world (list-change (world-keyList w) 11 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "g") (make-world (list-change (world-keyList w) 22 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "b") (make-world (list-change (world-keyList w) 12 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "h") (make-world (list-change (world-keyList w) 23 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "n") (make-world (list-change (world-keyList w) 13 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "j") (make-world (list-change (world-keyList w) 24 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [(key=? key "m") (make-world (list-change (world-keyList w) 14 #f)
                                  (world-inst w) (world-oct w) (world-vol w) (world-met w) (world-mode w) (world-demo-mode w))]
     [else w]
  ))

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
    [(mouse=? "button-up" me) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))]
    [else w]))


; Function for when the program mode is "main menu"
; worldstate mouse-event -> worldstate
(define (mousehandler-menu w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x 500) (< x 700) (> y 225) (< y 275)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "play" (world-demo-mode w))]
                                  [(and (> x 500) (< x 700) (> y 325) (< y 375)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "demo" (world-demo-mode w))])]
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
                                  [else w])]
    [(mouse=? "drag" me) (cond
                           [(and (> x (- (/ len 2) 50)) (< x (- (/ len 2) 30)) (> y (- (* wid 5/6) 75)) (< y (+ (* wid 5/6) 75))) (make-world (world-keyList w) (world-inst w) (world-oct w) (- 1 (/ (- y (- (* wid 5/6) 75)) 150)) (world-met w) (world-mode w) (world-demo-mode w))]
                           [(and (> x (+ (/ len 2) 30)) (< x (+ (/ len 2) 50)) (> y (- (* wid 5/6) 75)) (< y (+ (* wid 5/6) 75))) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (make-met (met-met? (world-met w)) (- 100 (/ (- y (- (* wid 5/6) 75)) 150))) (world-mode w) (world-demo-mode w))]
                           [else w])]
    [else w]))


; Function for when the program mode is "demo"
; worldstate mouse-event -> worldstate
(define (mousehandler-demo w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x 200) (< x 400) (> y 325) (< y 375)) (both (play-song (start-playing-song lights)) w)]
                                  [(and (> x 200) (< x 400) (> y 425) (< y 475)) (both (play-song (start-playing-song radioactive)) w)]
                                  [(and (> x 200) (< x 400) (> y 525) (< y 575)) (both (play-song (start-playing-song sail)) w)]
                                  [(and (> x 800) (< x 1000) (> y 325) (< y 375)) (both (play-song (start-playing-song summertime-sadness)) w)]
                                  [(and (> x 800) (< x 1000) (> y 425) (< y 475)) (both (play-song (start-playing-song wonderwall)) w)]
                                  [(and (> x 800) (< x 1000) (> y 525) (< y 575)) (both (both (stop) ps) w)]
                                  [(and (> x 1025) (< x 1125) (> y 25) (< y 75)) (make-world (world-keyList w) (world-inst w) (world-oct w) (world-vol w) (world-met w) "main menu" (world-demo-mode w))])]
    [else w]))

; Mousehandler function
; Passes the worldstate, x position, y position, and mouse event to a helper function based on the world-mode of the world
(define (mousehandler w x y me)
  (cond
    [(string=? (world-mode w) "title screen") (mousehandler-title w x y me)]
    [(string=? (world-mode w) "main menu") (mousehandler-menu w x y me)]
    [(string=? (world-mode w) "play") (mousehandler-play w x y me)]
    [(string=? (world-mode w) "demo") (mousehandler-demo w x y me)]
    ))
  
; Metronome functions
; Checks whether the metronome is running or not
#;(define (check-metronome w)
  (cond [(world-met w) (play-metronome (met-bpm (world-met w)))]
        [else w]))

; Makes a metronome when it is supposed to be playing.
#;(define (play-metronome bpm)
  ())

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
(define (wk1 w) (if (and (list-ref (world-keyList w) 1) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk2 w) (if (and (list-ref (world-keyList w) 2) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk3 w) (if (and (list-ref (world-keyList w) 3) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk4 w) (if (and (list-ref (world-keyList w) 4) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk5 w) (if (and (list-ref (world-keyList w) 5) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk6 w) (if (and (list-ref (world-keyList w) 6) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk7 w) (if (and (list-ref (world-keyList w) 7) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk8 w) (if (and (list-ref (world-keyList w) 8) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk9 w) (if (and (list-ref (world-keyList w) 9) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk10 w) (if (and (list-ref (world-keyList w) 10) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk11 w) (if (and (list-ref (world-keyList w) 11) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk12 w) (if (and (list-ref (world-keyList w) 12) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk13 w) (if (and (list-ref (world-keyList w) 13) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk14 w) (if (and (list-ref (world-keyList w) 14) #t) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
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
(define (white-keys w) (place-image (beside (wk1 w)
                                            (wk2 w)
                                            (wk3 w)
                                            (wk4 w)
                                            (wk5 w)
                                            (wk6 w)
                                            (wk7 w)
                                            (wk8 w)
                                            (wk9 w)
                                            (wk10 w)
                                            (wk11 w)
                                            (wk12 w)
                                            (wk13 w)
                                            (wk14 w)) (/ len 2) wkey-y-pos (rectangle len wid "outline" box-color)))
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
                        ;(text "0" 15 "white")
                        ;(text "100" 15 "white")
                        ;(text "50" 15 "white")
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
                        ;(make-posn 100 135)
                        ;(make-posn 1100 135)
                        ;(make-posn 600 135)
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
                      (text "Place Holder" 20 "white" )
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (rectangle 200 50 "solid" box-color)
                      (text "Menu" 30 "white")
                      (rectangle 100 50 "solid" box-color)
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
                      (make-posn (/ len 2) (/ wid 2))
                      )
                (rectangle len wid "solid" box-color)))


; Main function for the graphical interface of the program
; Draws the graphical interface of the program, based on the world-mode field of the worldstate
(define (graphics w)
  (cond
    [(string=? (world-mode w) "title screen") (title-screen w)]
    [(string=? (world-mode w) "main menu") (menu w)]
    [(string=? (world-mode w) "play") (key-board w)]
    [(string=? (world-mode w) "demo") (demo w)]
    ))
 
; Main function that runs the program
(main INITIAL_STATE)
