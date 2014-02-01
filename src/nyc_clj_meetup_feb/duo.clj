(ns nyc-clj-meetup-feb.duo
  (:use overtone.live))

; get on the bus
(defonce sin-bus (audio-bus))
(defonce mod-bus (control-bus))

(defsynth sin-synth [out-bus 0 freq 5]
  (out:kr out-bus (sin-osc:kr freq)))

(defsynth mod-synth [out-bus 0 freq 440]
  (out:kr out-bus (sin-osc:kr freq)))

(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))

(defsynth modulated-freq-tri [freq-bus 0 cutoff-bus 0 freq 220 freq-amp 55 cutoff 440 mult 1.0 rez 1.0]
  (let [freq freq]
    (out 0 (pan2 (rlpf (saw freq) (+ cutoff (* mult (in:kr cutoff-bus))) rez)))))

; (out 0 (pan2 (rlpf (lf-tri freq) (+ cutoff (in:kr cutoff-bus)) 1.0)))a))

(comment
(do
(def sin-synth-inst (sin-synth [:tail early-g] sin-bus))
(def mod-synth-inst (mod-synth [:tail early-g] mod-bus))
(def mft (modulated-freq-tri [:tail later-g] sin-bus mod-bus))
(def mf2 (modulated-freq-tri [:tail later-g] sin-bus mod-bus))
))

(comment
(ctl mod-synth-inst :freq 5)
(ctl mft :mult 100.0)
(ctl mft :rez 0.1)
(ctl mf2 :rez 0.2)
(ctl mf2 :freq 224)
(ctl mf2 :cutoff 300)
)





