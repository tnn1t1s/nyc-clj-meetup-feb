(ns nyc-clj-meetup-feb.duo
  (:use overtone.live
        overtone.studio.scope))


; intro to oscilloscope and spectrogram
; (demo 5 (rlpf (saw 440) (mouse-x 10 10000) 1.0))
(spectrogram)

; get on the bus
(defonce sin-bus (audio-bus))
(defonce mod-bus (control-bus))
(defonce lfo-bus (control-bus))

(defsynth sin-synth [out-bus 0 freq 5]
  (out:kr out-bus (sin-osc:kr freq)))

(defsynth mod-synth [out-bus 0 freq 440]
  (out:kr out-bus (sin-osc:kr freq)))

(defsynth lfo-synth [out-bus 0 freq 60]
  (out:kr out-bus (sin-osc:kr freq)))

(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))

(ctl mf3 :freq-amp 7.0)

(defsynth monotron-duo [
  freq-bus 0 
  cutoff-bus 0
  rez-bus 0
  freq 220
  freq-amp 55
  cutoff 440
  cutoff-amp 1.0
  rez 1.0
  rez-amp 1.0
  ]
  (let [
  cutoff (+ cutoff (* cutoff-amp (in:kr cutoff-bus)))
  freq (+ freq (* freq-amp (in:kr freq-bus)))
  rez (+ rez (* rez-amp (in:kr rez-bus)))
  ]
    (out 0 (pan2 (rlpf (saw freq) cutoff rez)))))

(comment
(do
(def sin-synth-inst (sin-synth [:tail early-g] sin-bus))
(def mod-synth-inst (mod-synth [:tail early-g] mod-bus))
(def lfo-synth-inst (mod-synth [:tail early-g] lfo-bus))
(def mft (monotron-duo [:tail later-g] sin-bus mod-bus lfo-bus 439))
(def mf2 (monotron-duo [:tail later-g] sin-bus mod-bus lfo-bus 440))
(def mf3 (monotron-duo [:tail later-g] sin-bus mod-bus lfo-bus 441))
))


(comment
(ctl mod-synth-inst :freq 1)
(ctl mft :cutoff-amp 400.0)
(ctl mft :rez 0.01)
(ctl mf2 :rez 0.01)
(ctl mf3 :rez 0.01)
(ctl mft :freq 440)
(ctl mf2 :freq 439)
;(ctl mf3 :freq 1760)
(ctl mf3 :freq 1760)
(ctl mf2 :cutoff 300)
)





