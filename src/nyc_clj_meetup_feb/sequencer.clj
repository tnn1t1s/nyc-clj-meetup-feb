(ns nyc-clj-meetup-feb.sequencer
  (:use overtone.live))


; sequencer buffers
;; this one will keep the beats
(defonce buf-0 (buffer 8))
;; this will keep the frequency
(defonce buf-1 (buffer 8))

;; preload some basic patterns
(buffer-write! buf-0 [1 1 1 1 1 1 1 1])
(buffer-write! buf-1 [220 660 110 660 440 220 660 110])
;; get random
(buffer-write! buf-1 (repeatedly 8 #(choose [110 220 440 660 880])))

; timing bus setup
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count
(defonce meter-cnt-bus (control-bus))
(defonce note-bus  (control-bus))

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 100]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth meter-cnt [meter-cnt-bus 0 div 8]
  (out:kr meter-cnt-bus (mod (in:kr beat-cnt-bus) div)))

;; this one generates a sequence of frequenciess from buffer
(defsynth note-sequencer
  "Plays a sequence of notes to a bus"
  [buf 0 meter-count-bus 0 out-bus 1]
  (out out-bus (buf-rd:kr 1 buf (in:kr meter-count-bus) 1 0)))

;; this is a synth; it needs notes and it needs beats 
(definst ping-env [note-bus 0 release 0.1 beat-num 0 sequencer 0]
 (let [note (in:kr note-bus)
       src1 (sin-osc note)
       cnt (in:kr beat-cnt-bus)
       beat-trg (in:kr beat-trg-bus)
       bar-trg (and 
          (buf-rd:kr 1 sequencer cnt)
          beat-trg)]
  (* (decay bar-trg release) src1)))

;; start up the timer synths
(comment
(do 
(def r-trg (root-trg))
(def r-cnt (root-cnt [:after r-trg]))
(def b-trg (beat-trg [:after r-trg]))
(def b-cnt (beat-cnt [:after r-trg]))
(def m-cnt8  (meter-cnt meter-cnt-bus 8))
(def note-seq (note-sequencer buf-1 meter-cnt-bus note-bus))
)
)

;; create the synth
(ping-env :note-bus note-bus :beat-num 7 :sequencer buf-0)


;; now link touchosc
(defn freq-scale
 [val]
 (scale-range val 0 1 50 1000))

(defn control-synth-release
 [synth val]
 (let [val (scale-range val 0 1 0 10)]
      (ctl synth :release val))) 
;; start an osc server on port 44100
;(def server (osc-server 44100 "osc-clj-meetup"))
; (zero-conf-on)

; (osc-listen server (fn [msg] (println msg)) :debug)
; (osc-rm-listener server :debug)

(comment
(do
(osc-handle server "/2/toggle1" (fn [msg] (buffer-write! buf-0 0 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle2" (fn [msg] (buffer-write! buf-0 1 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle3" (fn [msg] (buffer-write! buf-0 2 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle4" (fn [msg] (buffer-write! buf-0 3 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle5" (fn [msg] (buffer-write! buf-0 4 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle6" (fn [msg] (buffer-write! buf-0 5 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle7" (fn [msg] (buffer-write! buf-0 6 [(int (first (:args msg)))])))
(osc-handle server "/2/toggle8" (fn [msg] (buffer-write! buf-0 7 [(int (first (:args msg)))])))
)

(do
(osc-handle server "/2/fader1" (fn [msg] (buffer-write! buf-1 0 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader2" (fn [msg] (buffer-write! buf-1 1 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader3" (fn [msg] (buffer-write! buf-1 2 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader4" (fn [msg] (buffer-write! buf-1 3 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader5" (fn [msg] (buffer-write! buf-1 4 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader6" (fn [msg] (buffer-write! buf-1 5 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader7" (fn [msg] (buffer-write! buf-1 6 [(freq-scale (first (:args msg)))])))
(osc-handle server "/2/fader8" (fn [msg] (buffer-write! buf-1 7 [(freq-scale (first (:args msg)))])))
)
)
