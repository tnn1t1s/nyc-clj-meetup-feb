(ns nyc-clj-meetup-feb.synth-101
      (:use overtone.live 
            overtone.studio.scope))

;(spectrogram)

; saw
(demo 2 (saw 220))
(demo 2 (+ (saw 220) (saw 440)))
(demo 2 (apply + [(saw 220) (saw 440)]))
(demo 2 (apply + (map #(* 0.33 (saw %1)) [220 440 880])))
(demo 2 (apply + (map #(* 0.33 (saw (* 220 (Math/pow 2 %1)))) [0 1 2])))

; filter
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 200 0.2))
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 400 0.1))
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 600 0.1))
(demo 3 (rlpf (apply + (map #(* 0.33 (saw %1)) [220 440 880])) 800 0.1))

; hi pass
(def x [220 440 880])
(count x)
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 16000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 12000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 8000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 4000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 2000))
(demo 3 (hpf (apply + (map #(* (/ 1 (count x)) (saw %1)) x)) 1000))

; computer controlled
(demo 10 (rlpf (saw 440) (mouse-x 40 4000) (mouse-y 0.0 1.0)))

; control voltage and patch cables
(defonce lfo-patch (control-bus))
(defsynth low-frequency-oscillator [out-bus 0 freq 5]
    (out:kr out-bus (sin-osc:kr freq)))


(definst simple-saw [amp 1.0 freq 220 cutoff-amp 0.5 cutoff-bus 0]
  (* amp (rlpf (saw freq) (+ (mouse-x 400 4000) (* cutoff-amp (in:kr cutoff-bus))) (mouse-y 0.0 1.0))))

(def lfo (low-frequency-oscillator lfo-patch 5))
(simple-saw :cutoff-bus lfo-patch :cutoff-amp 400)

;(stop)
; now you're ready









