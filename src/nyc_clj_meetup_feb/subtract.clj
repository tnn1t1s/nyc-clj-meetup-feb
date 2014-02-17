(ns nyc-clj-meetup-feb.subtract
      (:use overtone.live 
            overtone.studio.scope))

;; jump into waves
;(spectrogram)
;
; sound is made up of vibration.
; one full vibration cycle is known ; a cycle. 
; The frequency in which this occurs (how many cycles per second) is measured in hertz (Hz).

(demo 3 (sin-osc 440))
; overtones 
(demo 3 (+ (sin-osc 440) (sin-osc 880) (sin-osc 1760)))

;; saws and squares
;; can be built from sin waves
; here we stack 100 sinwaves; each overtone is represetned at 1/N amplitude
(demo 3 (apply + (map #(* (/ 1 %) (sin-osc (* %1 100))) (range 1 100))))

; here we stack 50 sinwaves every other overtone is represented at 1/N amplitude
(demo 3 (apply + (map #(* (/ 1 %) (sin-osc (* %1 100))) (range 1 100 2))))

;;
;; but this is expensive, 
;; expensive for a computer, and really expensive for something like the korgms20
;;
;; it's cheaper and faster and louder to build osillator modules
;; by modeling the waveform and generating it in code, or, electronically.
(defn squares
  [freq]
  (letfn
    [(osc [cur-value so-far]
       (let [so-far (mod so-far freq)
             next-val (if (zero? so-far)
                        (- cur-value)
                        cur-value)]
         (cons next-val
               (lazy-seq (osc next-val
                              (inc so-far))))))]
    (osc 1 0)))

; you can't hear this, but you get the idea
;(take 10 (squares 5))

;; electronically (schematic?)

;; luckily we don't need to deal with this in practice.
;; supercollider and thus, overtone, has implementations of all the basic oscillator modules
;; sin, square , saw , triangle , etc.

(demo 3 (saw 100))
(demo 3 (square 100))

;; now we basics
;; we can start to build complicated sounds by 
;; adding , subtracting, multiplying, filtering and otherwise
;; manipulating the waves.
(comment
; saw
(demo 2 (saw 220))
(demo 2 (+ (* 0.5 (saw 220)) (* 0.5 (saw 220))))
(demo 2 (apply + (map #(* 0.33 (saw %1)) [220 440 880])))
(demo 2 (apply + (map #(* 0.33 (saw (* 220 (Math/pow 2 %1)))) [0 1 2])))

; a filter filters out frequencies 
; the cutoff is the frequency at which to start filtering
; resonance adds a boost the the overtone directly at the cutoff frequency
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 220 0.1))
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 440 0.1))
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 880 0.1))

; subtractor
(demo 20 (rlpf (saw 220) (mouse-x 40 4000) (mouse-y 0.0 1.0)))

; hi pass filter
(def x [220 440 880])
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 16000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 12000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 8000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 4000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 2000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 1000))
)

;; we need one more thing before we can start building an ms20 in overtone
;; ,the ability control synths w/ signals
;
; our version of control voltage and patch cables
(defonce lfo-patch (control-bus))
(defsynth low-frequency-oscillator [out-bus 0 freq 5]
    (out:kr out-bus (sin-osc:kr freq)))

(definst simple-saw [amp 1.0 freq 220 cutoff-amp 0.5 cutoff-bus 0]
  (* amp (rlpf (saw freq) (+ (mouse-x 400 4000) (* cutoff-amp (in:kr cutoff-bus))) (mouse-y 0.1 1.0))))

(comment
  (do
    (def lfo (low-frequency-oscillator lfo-patch 5))
    (simple-saw :cutoff-bus lfo-patch :cutoff-amp 400)))

;; turn the knobs
;(ctl lfo :freq 1)
;(ctl lfo :freq 10)
;(ctl lfo :freq 100)
;(ctl simple-saw :cutoff-amp 500)


;(stop)









