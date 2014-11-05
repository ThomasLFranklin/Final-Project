;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Prototype) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; Electronic Keyboard Program-PROTOTYPE
;   By Thomas Franklin, Nish Dara, Blain Weeks, Devon Grove
; **Created for presentation use ONLY. Don't add features to this file.**

; Required Packages
(require rsound)
(require rsound/piano-tones)
(require 2htdp/image)
(require 2htdp/universe)

; Useful functions and definitions
(define ps (make-pstream))
(define (both a b) b)
(define (s seconds)
  (* 44100 seconds))

; Worldstate is a structure of 4 elements
; note-num refers to the MIDI note number
; inst refers to the instrument (represented by a positive integer) the note will be played with
; oct refers to the range (represented by -1, 0, or 1) of the tones of the playble notes on the keyboard
; vol refers to the volume multiplier (represented by a number between 0 and 1) of the notes
; Initial world is (make-world 0 1 0 1) meaning no note played, piano selected, mid range octave, at full volume
(define-struct world (note-num inst oct vol))
(define w (make-world 0 1 0 1))
  
  
; Changes the given world-note-num to a MIDI note number when an alpha-numeric key is pressed or
; Changes the given world-not-oct when either the "up" or "down" key is pressed
; Worldstate key-event -> Worldstate
(define (change-worldstate w key)
  (cond
    [(key=? key "q") (both (play-note (+ 48 (* (world-oct w) 24)) w) (make-world 48 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "2") (both (play-note (+ 49 (* (world-oct w) 24)) w) (make-world 49 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "w") (both (play-note (+ 50 (* (world-oct w) 24)) w) (make-world 50 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "3") (both (play-note (+ 51 (* (world-oct w) 24)) w) (make-world 51 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "e") (both (play-note (+ 52 (* (world-oct w) 24)) w) (make-world 52 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "r") (both (play-note (+ 53 (* (world-oct w) 24)) w) (make-world 53 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "5") (both (play-note (+ 54 (* (world-oct w) 24)) w) (make-world 54 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "t") (both (play-note (+ 55 (* (world-oct w) 24)) w) (make-world 55 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "6") (both (play-note (+ 56 (* (world-oct w) 24)) w) (make-world 56 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "y") (both (play-note (+ 57 (* (world-oct w) 24)) w) (make-world 57 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "7") (both (play-note (+ 58 (* (world-oct w) 24)) w) (make-world 58 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "u") (both (play-note (+ 59 (* (world-oct w) 24)) w) (make-world 59 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "z") (both (play-note (+ 60 (* (world-oct w) 24)) w) (make-world 60 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "s") (both (play-note (+ 61 (* (world-oct w) 24)) w) (make-world 61 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "x") (both (play-note (+ 62 (* (world-oct w) 24)) w) (make-world 62 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "d") (both (play-note (+ 63 (* (world-oct w) 24)) w) (make-world 63 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "c") (both (play-note (+ 64 (* (world-oct w) 24)) w) (make-world 64 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "v") (both (play-note (+ 65 (* (world-oct w) 24)) w) (make-world 65 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "g") (both (play-note (+ 66 (* (world-oct w) 24)) w) (make-world 66 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "b") (both (play-note (+ 67 (* (world-oct w) 24)) w) (make-world 67 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "h") (both (play-note (+ 68 (* (world-oct w) 24)) w) (make-world 68 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "n") (both (play-note (+ 69 (* (world-oct w) 24)) w) (make-world 69 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "j") (both (play-note (+ 70 (* (world-oct w) 24)) w) (make-world 70 (world-inst w) (world-oct w) (world-vol w)))]
    [(key=? key "m") (both (play-note (+ 71 (* (world-oct w) 24)) w) (make-world 71 (world-inst w) (world-oct w) (world-vol w)))]
    [else w]))
  
  
; Plays a note
; worldstate number -> sound
(define (play-note num w)
  (cond
    [(= (world-inst w) 1) (pstream-play ps (rs-scale (world-vol w) (piano-tone num)))]
    [else w]))



; Produces an image of a keyboard 
; Numerical Constants
(define background (make-color 43 147 208))
(define len 1600)
(define wid 1000)
(define wkeylen 100)
(define wkeywid 450)
(define wkey-y-pos 450)
(define bkeylen 66)
(define bkeywid 260)
(define bkey-y-pos 355)

; Functions for the keys (white, black, and outlines)
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
                                              (rectangle wkeylen wkeywid "outline" "black")) (/ len 2) wkey-y-pos (rectangle len wid "outline" background)))
(define (white-keys w) (place-image (beside (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")
                                            (rectangle wkeylen wkeywid "solid" "white")) (/ len 2) wkey-y-pos (rectangle len wid "outline" background)))
(define (black-keys w) (place-image 
                        (rectangle bkeylen bkeywid "solid" "black") (+ (* len 3/32) 17 22) bkey-y-pos
                        (place-image 
                         (rectangle bkeylen bkeywid "solid" "black") (- (* len 7/32) 17 22) bkey-y-pos
                         (place-image 
                          (rectangle bkeylen bkeywid "solid" "black") (+ (* len 9/32) 17 22) bkey-y-pos
                          (place-image 
                           (rectangle bkeylen bkeywid "solid" "black") (+ (* len 11/32) 17 33) bkey-y-pos
                           (place-image 
                            (rectangle bkeylen bkeywid "solid" "black") (- (* len 15/32) 17 22) bkey-y-pos
                            (place-image
                             (rectangle bkeylen bkeywid "solid" "black") (+ (* len 17/32) 17 22) bkey-y-pos
                             (place-image 
                              (rectangle bkeylen bkeywid "solid" "black") (- (* len 21/32) 17 22) bkey-y-pos
                              (place-image 
                               (rectangle bkeylen bkeywid "solid" "black") (+ (* len 23/32) 17 22) bkey-y-pos
                               (place-image 
                                (rectangle bkeylen bkeywid "solid" "black") (+ (* len 25/32) 17 33) bkey-y-pos
                                (place-image 
                                 (rectangle bkeylen bkeywid "solid" "black") (- (* len 29/32) 17 22) bkey-y-pos
                                 (rectangle len wid "outline" background))))))))))))

; Functions for text and extras
; Takes in a world and produces text based on the current range of the keyboard
(define len1 600)
(define wid1 225)
(define box-text1 (rectangle len1 wid1 "solid" (make-color 40 150 250)))

(define(text1 w) (place-image
                  (text/font "Range of Notes" 30 "red" "Palatino Linotype" 'default 'normal 'normal #t) (/ len1 2) 24 
                  (place-image
                   (text "Press the up or down arrow keys to change the range of the notes." 20 "red") (/ len1 2) 66 
                   (place-image
                    (text "Current Note Range (in MIDI note numbers):" 24 "red") (/ len1 2) (/ wid1 2)
                    (place-image
                     (text (string-append (number->string (+ 48 (* (world-oct w) 24))) "-" (number->string (+ 71 (* (world-oct w) 24)))) 50 "red") (/ len1 2) (* wid1 3/4) box-text1)))))

; Text for the instrument selector
(define (text2 w) (place-image
                   (text "Instrument Selector" 30 "red") (/ len1 2) 24
                   box-text1))

(define inst1text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Piano" 24 "red")))

(define inst2text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Trumpet" 24 "red")))

(define inst3text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Pan Flute" 24 "red")))

(define inst4text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Alto Sax" 24 "red")))

(define inst5text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Harp" 24 "red")))

(define inst6text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Music Box" 24 "red")))

(define inst7text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Synth Strings" 24 "red")))

(define inst8text (beside
                        (rectangle 16 16 "outline" "black")
                        (rectangle 17 16 "solid" (make-color 40 150 250))
                        (text "Synth Pad" 24 "red")))

; Function for the volume slider
; Draws a slider that changes the volume

(define (slider w) (place-image
                    (add-line
                     (rectangle 1000 5 "solid" "black") (* 1000 (world-vol w)) -10 (* 1000 (world-vol w)) 15 (make-pen "red" 10 "solid" "round" "round")) (/ len 2) 190 (rectangle len wid "outline" background)))

; Main renedering function
(define (key-board w) (place-image
                       (text "Volume" 20 "black") (/ len 2) 130
                       (place-image
                        (text "0" 15 "black") 300 160
                        (place-image
                         (text "100" 15 "black") 1300 160
                         (place-image
                          (text "50" 15 "black") 800 160
                          (place-image
                           (slider w) (/ len 2) (/ wid 2)
                           (place-image
                            inst1text (+ (* len 3/32) (- (/ (image-width inst1text) 2) 8)) (* wid 4/5)
                            (place-image
                             inst2text (+ (* len 3/32) (- (/ (image-width inst2text) 2) 8)) (+ (* wid 4/5) 32)
                             (place-image
                              inst3text (+ (* len 3/32) (- (/ (image-width inst3text) 2) 8)) (+ (* wid 4/5) 64)
                              (place-image
                               inst4text (+ (* len 3/32) (- (/ (image-width inst4text) 2) 8)) (+ (* wid 4/5) 96)
                               (place-image
                                inst5text (+ (* len 1/4) (- (/ (image-width inst5text) 2) 8)) (* wid 4/5)
                                (place-image
                                 inst6text (+ (* len 1/4) (- (/ (image-width inst6text) 2) 8)) (+ (* wid 4/5) 32)
                                 (place-image
                                  inst7text (+ (* len 1/4) (- (/ (image-width inst7text) 2) 8)) (+ (* wid 4/5) 64)
                                  (place-image
                                   inst8text (+ (* len 1/4) (- (/ (image-width inst8text) 2) 8)) (+ (* wid 4/5) 96)
                                   (place-image
                                    (rectangle 16 16 "solid" (if (= (world-inst w) 1) "yellow" (make-color 40 150 250))) (* len 3/32) (* wid 4/5)
                                    (place-image
                                     (rectangle 16 16 "solid" (if (= (world-inst w) 2) "yellow" (make-color 40 150 250))) (* len 3/32) (+ (* wid 4/5) 32)
                                     (place-image
                                      (rectangle 16 16 "solid" (if (= (world-inst w) 3) "yellow" (make-color 40 150 250))) (* len 3/32) (+ (* wid 4/5) 64)
                                      (place-image
                                       (rectangle 16 16 "solid" (if (= (world-inst w) 4) "yellow" (make-color 40 150 250))) (* len 3/32) (+ (* wid 4/5) 96)
                                       (place-image
                                        (rectangle 16 16 "solid" (if (= (world-inst w) 5) "yellow" (make-color 40 150 250))) (* len 1/4) (* wid 4/5)
                                        (place-image
                                         (rectangle 16 16 "solid" (if (= (world-inst w) 6) "yellow" (make-color 40 150 250))) (* len 1/4) (+ (* wid 4/5) 32)
                                         (place-image
                                          (rectangle 16 16 "solid" (if (= (world-inst w) 7) "yellow" (make-color 40 150 250))) (* len 1/4) (+ (* wid 4/5) 64)
                                          (place-image
                                           (rectangle 16 16 "solid" (if (= (world-inst w) 8) "yellow" (make-color 40 150 250))) (* len 1/4) (+ (* wid 4/5) 96)
                                           (place-image
                                            (text2 w) (* len 1/4) (* wid 5/6)
                                            (place-image
                                             (text1 w) (* len 3/4) (* wid 5/6)
                                             (place-image 
                                              (black-keys w) (/ len 2) (/ wid 2)
                                              (place-image 
                                               (key-outlines w) (/ len 2) (/ wid 2)
                                               (place-image 
                                                (white-keys w) (/ len 2) (/ wid 2) 
                                                (rectangle len wid "solid" background))))))))))))))))))))))))))))
  
  
  
; The worldstate is a number

(big-bang w
          [to-draw key-board]
          [on-key change-worldstate]
          )