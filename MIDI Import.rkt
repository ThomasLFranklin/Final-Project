;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |MIDI Import|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
; Use code for importing new MIDI files ONLY 
(require rsound)
(require rsound/piano-tones)
(require midi-readwrite)

(MIDIFile->notelist (midi-file-parse "path of midi file.mid"))