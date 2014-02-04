(ns nyc-clj-meetup-feb.ms20

(stop)
; intro to oscilloscope and spectrogram
; (demo 5 (rlpf (saw 440) (mouse-x 10 10000) 1.0))
(spectrogram)

;; sequencer buffers
(defonce buf-0 (buffer 16))
(defonce buf-1 (buffer 16))
;; (buffer-write! buf-0 (repeatedly 16 #(choose [1])))
;; (buffer-write! buf-1 (repeatedly 16 #(choose [110 220 440 660 880])))
;; (buffer-write! buf-1 (repeatedly 16 #(choose [110])))

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

; timing bus setup
(def BEAT-FRACTION "Number of global pulses per beat" 30)
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count
(defonce meter-cnt-bus (control-bus))
(defonce note-bus  (control-bus))
(defonce sequence-bus  (control-bus))

(defonce main-g (group "get-on-the-bus main"))
(defonce early-g (group "early birds" :head main-g))
(defonce later-g (group "latecomers" :after early-g))

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 100]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth meter-cnt [meter-cnt-bus 0 div 16]
  (out:kr meter-cnt-bus (mod (in:kr beat-cnt-bus) div)))

;; this generates a sequence of frequenciess from buffer
 (defsynth note-sequencer
   "Plays a sequence of notes to a bus"
     [buf 0 meter-count-bus 0 out-bus 1]
       (out out-bus (buf-rd:kr 1 buf (in:kr meter-count-bus) 1 0)))

;; sets up a buf trigger on bus
(defsynth trigger-sequencer [sequence-buf 0 out-bus 1]
  (let [cnt (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg (and (buf-rd:kr 1 sequence-buf cnt) beat-trg)]
    (out out-bus bar-trg)))

(defsynth korg-ms20 [
  sequence-bus 0
  note-bus 0
  freq-mod-bus 0 
  cutoff-mod-bus 0
  rez-bus 0
  freq-amp 55
  cutoff 440
  cutoff-amp 1.0
  rez 1.0
  rez-amp 1.0
  dcy 0.5
  sustain 0.0
  ]
  (let [
  trig (in:kr sequence-bus)
  note (in:kr note-bus)
  amp-env (env-gen (envelope [10e-10, 1, 1, 10e-10] [0.01, sustain, dcy] :exp) :gate trig :action NO-ACTION)
  cutoff (+ cutoff (* cutoff-amp (in:kr cutoff-mod-bus)))
  freq (+ note (* freq-amp (in:kr freq-mod-bus)))
  rez (+ rez (* rez-amp (in:kr rez-bus)))
  ]
    (out 0 (pan2 (* amp-env (rlpf (saw freq) cutoff rez))))))

(comment
(do
(def sin-synth-inst (sin-synth [:tail early-g] sin-bus))
(def mod-synth-inst (mod-synth [:tail early-g] mod-bus))
(def lfo-synth-inst (mod-synth [:tail early-g] lfo-bus))
(def r-trg (root-trg))
(def r-cnt (root-cnt [:after r-trg]))
(def b-trg (beat-trg [:after r-trg]))
(def b-cnt (beat-cnt [:after r-trg]))
(def m-cnt16  (meter-cnt meter-cnt-bus 16))
(def trigger-seq (trigger-sequencer buf-0 sequence-bus))
(def note-seq (note-sequencer buf-1 meter-cnt-bus note-bus))
))

;(stop)
(comment
(def mft (korg-ms20 [:tail later-g] sequence-bus note-bus sin-bus mod-bus lfo-bus))
)

;; playing with these is interesting enough
(comment
(ctl sin-synth-inst :freq 1)
(ctl mod-synth-inst :freq 5)
(ctl lfo-synth-inst :freq 10)
(ctl mft :dcy 5.0)
(ctl mft :rez 0.2)
(ctl mft :cutoff 200)
(ctl mft :cutoff-amp 20)
(ctl mft :freq-amp 0.0)
(ctl mft :rez-amp 2.5)
)

;; procession of simulacra

;; now, connect touchosc
;;




