;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Prototype) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; Electronic Keyboard Program



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


;Instuments
(define trumpet (rs-scale .5 (rs-read "Trumpet.wav")))
(define flute (rs-scale .5 (rs-read "Pan Flute.wav")))
(define sax (rs-scale .5 (rs-read "Sax.wav")))
(define strings (rs-scale .5 (rs-read "Synth Strings.wav")))
(define musicbox (rs-scale .5 (rs-read "Music Box.wav")))
(define synth (rs-scale .5 (rs-read "Synth Pad.wav")))
(define harp (rs-scale .5 (rs-read "Harp.wav")))

  
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
    [(key=? key "up") (if (< (world-oct w) 1) (make-world (world-note-num w) (world-inst w) (+ (world-oct w) 1) (world-vol w)) w)]
    [(key=? key "down") (if (> (world-oct w) -1) (make-world (world-note-num w) (world-inst w) (- (world-oct w) 1) (world-vol w)) w)]
    [else w]))


; Changes the instrument
; worldstate mouse event -> worldstate
(define (mousehandler w x y me)
  (cond
    [(mouse=? "button-down" me) (cond
                                  [(and (> x (- (* len 3/32) 8)) (< x (+ (* len 3/32) 8)) (> y (- (* wid 4/5) 8)) (< y (+ (* wid 4/5) 8))) (make-world (world-note-num w) 1 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 3/32) 8)) (< x (+ (* len 3/32) 8)) (> y (+ (* wid 4/5) 24)) (< y (+ (* wid 4/5) 40))) (make-world (world-note-num w) 2 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 3/32) 8)) (< x (+ (* len 3/32) 8)) (> y (+ (* wid 4/5) 56)) (< y (+ (* wid 4/5) 72))) (make-world (world-note-num w) 3 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 3/32) 8)) (< x (+ (* len 3/32) 8)) (> y (+ (* wid 4/5) 88)) (< y (+ (* wid 4/5) 104))) (make-world (world-note-num w) 4 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 1/4) 8)) (< x (+ (* len 1/4) 8)) (> y (- (* wid 4/5) 8)) (< y (+ (* wid 4/5) 8))) (make-world (world-note-num w) 5 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 1/4) 8)) (< x (+ (* len 1/4) 8)) (> y (+ (* wid 4/5) 24)) (< y (+ (* wid 4/5) 40))) (make-world (world-note-num w) 6 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 1/4) 8)) (< x (+ (* len 1/4) 8)) (> y (+ (* wid 4/5) 56)) (< y (+ (* wid 4/5) 72))) (make-world (world-note-num w) 7 (world-oct w) (world-vol w))]
                                  [(and (> x (- (* len 1/4) 8)) (< x (+ (* len 1/4) 8)) (> y (+ (* wid 4/5) 88)) (< y (+ (* wid 4/5) 104))) (make-world (world-note-num w) 8 (world-oct w) (world-vol w))]
                                  [else w])]
    [(mouse=? "drag" me) (cond
                           [(and (> x 100) (< x 1100) (> y 148) (< y 182)) (make-world (world-note-num w) (world-inst w) (world-oct w) (/ (- x 100) 1000))]
                           [else w])]
    [else w])) 
  
  
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



; Produces an image of a keyboard 
; Numerical Constants
(define background (make-color 43 147 208))
(define len 1200)
(define wid 750)
(define wkeylen 75)
(define wkeywid 310)
(define wkey-y-pos 340)
(define bkeylen 48)
(define bkeywid 170)
(define bkey-y-pos 270)

; Color change functions
(define (wk1 w) (if (= (world-note-num w) 48) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk2 w) (if (= (world-note-num w) 50) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk3 w) (if (= (world-note-num w) 52) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk4 w) (if (= (world-note-num w) 53) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk5 w) (if (= (world-note-num w) 55) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk6 w) (if (= (world-note-num w) 57) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk7 w) (if (= (world-note-num w) 59) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk8 w) (if (= (world-note-num w) 60) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk9 w) (if (= (world-note-num w) 62) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk10 w) (if (= (world-note-num w) 64) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk11 w) (if (= (world-note-num w) 65) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk12 w) (if (= (world-note-num w) 67) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk13 w) (if (= (world-note-num w) 69) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (wk14 w) (if (= (world-note-num w) 71) (rectangle wkeylen wkeywid "solid" "yellow") (rectangle wkeylen wkeywid "solid" "white")))
(define (bk1 w) (if (= (world-note-num w) 49) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk2 w) (if (= (world-note-num w) 51) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk3 w) (if (= (world-note-num w) 54) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk4 w) (if (= (world-note-num w) 56) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk5 w) (if (= (world-note-num w) 58) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk6 w) (if (= (world-note-num w) 61) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk7 w) (if (= (world-note-num w) 63) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk8 w) (if (= (world-note-num w) 66) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk9 w) (if (= (world-note-num w) 68) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))
(define (bk10 w) (if (= (world-note-num w) 70) (rectangle bkeylen bkeywid "solid" "yellow") (rectangle bkeylen bkeywid "solid" "black")))

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
                                            (wk14 w)) (/ len 2) wkey-y-pos (rectangle len wid "outline" background)))
(define (black-keys w) (place-images 
                        (list (bk1 w)
                              (bk2 w)
                              (bk3 w)
                              (bk4 w)
                              (bk5 w)
                              (bk6 w)
                              (bk7 w)
                              (bk8 w)
                              (bk9 w)
                              (bk10 w))
                        (list (make-posn (- (* len 2/16) 8) bkey-y-pos)
                              (make-posn (+ (* len 3/16) 8) bkey-y-pos)
                              (make-posn (- (* len 5/16) 8) bkey-y-pos)
                              (make-posn (* len 6/16) bkey-y-pos)
                              (make-posn (+ (* len 7/16) 8) bkey-y-pos)
                              (make-posn (- (* len 9/16) 8) bkey-y-pos)
                              (make-posn (+ (* len 10/16) 8) bkey-y-pos)
                              (make-posn (- (* len 12/16) 8) bkey-y-pos)
                              (make-posn (* len 13/16) bkey-y-pos)
                              (make-posn (+ (* len 14/16) 8) bkey-y-pos))
                      (rectangle len wid "outline" background)))

; Functions for text and extras
; Takes in a world and produces text based on the current range of the keyboard
(define len1 (* 6 wkeylen))
(define wid1 200)
(define box-text1 (rectangle len1 wid1 "solid" (make-color 40 150 250)))

(define(text1 w) (place-images
                  (list
                   (text/font "Range of Notes" 30 "red" "Palatino Linotype" 'default 'normal 'normal #t)
                   (text "Press the up or down arrow keys to change the range of the notes." 15 "red")
                   (text "Current Note Range (in MIDI note numbers):" 20 "red")
                   (text (string-append (number->string (+ 48 (* (world-oct w) 24))) "-" (number->string (+ 71 (* (world-oct w) 24)))) 50 "red"))
                  (list
                   (make-posn (/ len1 2) 24)
                   (make-posn (/ len1 2) 66 )
                   (make-posn (/ len1 2) 90)
                   (make-posn (/ len1 2) 145))
                  box-text1))

; Text for the instrument selector
(define (text2 w) (place-image
                   (text/font "Instrument Selector" 30 "red" "Palatino Linotype" 'default 'normal 'normal #t) (/ len1 2) 24
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
                     (rectangle 1000 5 "solid" "black") (* 1000 (world-vol w)) -10 (* 1000 (world-vol w)) 15 (make-pen "red" 10 "solid" "round" "round")) (/ len 2) 160 (rectangle len wid "outline" background)))

; Main renedering function
(define (key-board w) (place-images
                       (list
                        (text/font "Infiniano" 60 "red" "Palatino Linotype" 'default 'italic 'normal #f)
                        (text "Volume" 20 "black")
                        (text "0" 15 "black")
                        (text "100" 15 "black")
                        (text "50" 15 "black")
                        (slider w)
                        inst1text
                        inst2text
                        inst3text
                        inst4text
                        inst5text
                        inst6text
                        inst7text
                        inst8text
                        (rectangle 16 16 "solid" (if (= (world-inst w) 1) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 2) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 3) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 4) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 5) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 6) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 7) "yellow" (make-color 40 150 250)))
                        (rectangle 16 16 "solid" (if (= (world-inst w) 8) "yellow" (make-color 40 150 250)))
                        (text2 w)
                        (text1 w)
                        (black-keys w)
                        (key-outlines w)
                        (white-keys w))
                       (list
                        (make-posn (/ len 2) 55)
                        (make-posn (/ len 2) 110)
                        (make-posn 100 135)
                        (make-posn 1100 135)
                        (make-posn 600 135)
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (+ (* len 3/32) (- (/ (image-width inst1text) 2) 8)) (* wid 4/5))
                        (make-posn (+ (* len 3/32) (- (/ (image-width inst2text) 2) 8)) (+ (* wid 4/5) 32))
                        (make-posn (+ (* len 3/32) (- (/ (image-width inst3text) 2) 8)) (+ (* wid 4/5) 64))
                        (make-posn (+ (* len 3/32) (- (/ (image-width inst4text) 2) 8)) (+ (* wid 4/5) 96))
                        (make-posn (+ (* len 1/4) (- (/ (image-width inst5text) 2) 8)) (* wid 4/5))
                        (make-posn (+ (* len 1/4) (- (/ (image-width inst6text) 2) 8)) (+ (* wid 4/5) 32))
                        (make-posn (+ (* len 1/4) (- (/ (image-width inst7text) 2) 8)) (+ (* wid 4/5) 64))
                        (make-posn (+ (* len 1/4) (- (/ (image-width inst8text) 2) 8)) (+ (* wid 4/5) 96))
                        (make-posn (* len 3/32) (* wid 4/5))
                        (make-posn (* len 3/32) (+ (* wid 4/5) 32))
                        (make-posn (* len 3/32) (+ (* wid 4/5) 64))
                        (make-posn (* len 3/32) (+ (* wid 4/5) 96))
                        (make-posn (* len 1/4) (* wid 4/5))
                        (make-posn (* len 1/4) (+ (* wid 4/5) 32))
                        (make-posn (* len 1/4) (+ (* wid 4/5) 64))
                        (make-posn (* len 1/4) (+ (* wid 4/5) 96))
                        (make-posn (* len 1/4) (* wid 5/6))
                        (make-posn (* len 3/4) (* wid 5/6))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (/ len 2) (/ wid 2))
                        (make-posn (/ len 2) (/ wid 2)))
                       (rectangle len wid "solid" background)))

(big-bang w
          [to-draw key-board]
          [on-key change-worldstate]
          [on-mouse mousehandler]
          )