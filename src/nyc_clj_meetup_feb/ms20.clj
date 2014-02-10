(ns nyc-clj-meetup-feb.ms20
      (:use [overtone.live]
            [overtone.studio.scope]
            [nyc-clj-meetup-feb.touchosc]))



;(spectrogram)

;; first, create sequencer buffers.
;; these are stored on the server and
;; contain the beats, and notes, that our sequencer will step through.
(defonce trigger-buffer (buffer 16))
(defonce note-buffer (buffer 16))

;; write into the buffer
(buffer-write! trigger-buffer (repeatedly 16 #(choose [1 1])))
;; (buffer-write! note-buffer (repeatedly 16 #(choose [110 220 440 660 880])))
(buffer-write! note-buffer (repeatedly 16 #(choose [110])))

;; in overtone, the yellow patch cables on the korg ms20 are typically called buses
;; we create 3 buses here: one for a sin wave, a general mod, and a low frequency 
;; oscillator 
(defonce sin-bus (audio-bus))
(defonce mod-bus (control-bus))
(defonce lfo-bus (control-bus))

;; like the ms20, we'll need modules to generate signals that will
;; be carried over the bus
(defsynth sin-synth [out-bus 0 freq 5]
  (out:kr out-bus (sin-osc:kr freq)))

(defsynth mod-synth [out-bus 0 freq 440]
  (out:kr out-bus (sin-osc:kr freq)))

(defsynth lfo-synth [out-bus 0 freq 60]
  (out:kr out-bus (sin-osc:kr freq)))

; timing bus setup
;; the ms20 has an internal clock source
(def BEAT-FRACTION "Number of global pulses per beat" 30)
(defonce root-trg-bus (control-bus)) ;; internal clock  e.g. 'clock' on the ms20
(defonce root-cnt-bus (control-bus)) ;; ms20 doesn't have the this. 
(defonce beat-trg-bus (control-bus)) ;; or this
(defonce beat-cnt-bus (control-bus)) ;; or this
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

;; this reads a sequence of frequencies from buffer and plays them out to the bus
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

;; our instrument
(defsynth korg-ms20 [
  sequence-bus 0
  note-bus 0
  freq-mod-bus 0 
  cutoff-mod-bus 0
  rez-bus 0
  freq-amp 55
  freq-mult 0 
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
  freq (+ note (* freq-amp (* freq-mult (in:kr freq-mod-bus))))
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
  (def trigger-seq (trigger-sequencer trigger-buffer sequence-bus))
  (def note-seq (note-sequencer note-buffer meter-cnt-bus note-bus)))
)

;;  factory defaults
(comment
(do
  (def mft (korg-ms20 [:tail later-g] sequence-bus note-bus sin-bus mod-bus lfo-bus))
  (ctl sin-synth-inst :freq 10)
  (ctl mod-synth-inst :freq 0.5)
  (ctl lfo-synth-inst :freq 1)
  (ctl mft :dcy 4.0)
  (ctl mft :rez 0.1)
  (ctl mft :cutoff 400)
  (ctl mft :cutoff-amp 50)
  (ctl mft :freq-amp 0.0)
  (ctl mft :freq-mult 0.0)
  (ctl mft :rez-amp 1.5)
  (ctl sin-synth-inst :freq 10)
  (ctl mod-synth-inst :freq 2)
  (ctl lfo-synth-inst :freq 100))
)

;; now, connect touchosc
(comment
(do
  (osc-handle server "/3/rotary1" (fn [msg] (control-scale mft :cutoff (first (:args msg)) 60 600)))
  (osc-handle server "/3/rotary2" (fn [msg] (control-scale mft :rez (first (:args msg)) 0 1)))
  (osc-handle server "/3/rotary3" (fn [msg] (control-scale mft :dcy (first (:args msg)) 0 10)))
  (osc-handle server "/3/rotary4" (fn [msg] (control-scale sin-synth-inst :freq (first (:args msg)) 0 100)))
  (osc-handle server "/3/rotary5" (fn [msg] (control-scale mod-synth-inst :freq (first (:args msg)) 0 10)))
  (osc-handle server "/3/rotary6" (fn [msg] (control-scale lfo-synth-inst :freq (first (:args msg)) 0 10)))
  (osc-handle server "/3/toggle1" (fn [msg] (control-scale mft :freq-mult (first (:args msg)) 0 1)))
  (osc-handle server "/3/toggle2" (fn [msg] (control-scale mft :cutoff-amp (first (:args msg)) 0 100)))
  (osc-handle server "/3/toggle3" (fn [msg] (control-scale mft :rez-amp (first (:args msg)) 0 1))))
)




