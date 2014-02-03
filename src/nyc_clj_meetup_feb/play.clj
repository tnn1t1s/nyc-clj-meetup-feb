(ns nyc-clj-meetup-feb.play
  (:use overtone.live))

;(stop)

; casio tone for the painfully alone
;; kick / noisy snare / and a drony synth
;; 1) try 16ths
;; 2) write multi buffers
;; 3) touchosc control
;; 4) done
;; this should be more than 2 hours work max

; sequencer buffers
;; this one will keep the kicks1
(defonce buf-0 (buffer 16))
(defonce buf-1 (buffer 16))
;; snares 
(defonce buf-2 (buffer 16))
(defonce buf-3 (buffer 16))
;; lead 
(defonce buf-4 (buffer 16))
(defonce buf-5 (buffer 16))


;; preload some basic patterns
;; kicks
(buffer-write! buf-0 [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
; break!
;(buffer-write! buf-0 (repeatedly 16 #(choose [0 1])))

;; notes
;; get random
(buffer-write! buf-1 (repeatedly 16 #(choose [110 220 440 660 880])))

; snare
;(buffer-write! buf-2 [0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0])
;(buffer-write! buf-2 (repeatedly 16 #(choose [0 1])))
;(buffer-write! buf-3 [220 220 220 220 220 220 220 220])

; lead 
;(buffer-write! buf-4 (repeatedly 16 #(choose [0 1])))
;(buffer-write! buf-5 [220 220 220 220 220 220 220 220])

; timing bus setup
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count
(defonce meter-cnt-bus (control-bus))
(defonce note-bus  (control-bus))
(defonce sequence-bus  (control-bus))
(defonce sequence-bus-2  (control-bus))
(defonce sequence-bus-3  (control-bus))

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 180]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth meter-cnt [meter-cnt-bus 0 div 16]
  (out:kr meter-cnt-bus (mod (in:kr beat-cnt-bus) div)))

;; this one generates a sequence of frequenciess from buffer
(defsynth note-sequencer
  "Plays a sequence of notes to a bus"
  [buf 0 meter-count-bus 0 out-bus 1]
  (out out-bus (buf-rd:kr 1 buf (in:kr meter-count-bus) 1 0)))

;; sets up a buf trigger on bus
(defsynth trigger-sequencer [sequence-buf 0 out-bus 1]
 (let [cnt (in:kr beat-cnt-bus)
       beat-trg (in:kr beat-trg-bus)
       bar-trg (and
          (buf-rd:kr 1 sequence-buf cnt)
          beat-trg)]
  (out out-bus bar-trg)))
 
;; this is a basic saw synth; it takes sequence and note inputs
(definst ping-it [sequence-bus 0 note-bus 0 release 0.1 cutoff 440 rez 1.0]
 (let [trig (in:kr sequence-bus)
       note (in:kr note-bus)
       src (rlpf (saw note) cutoff rez)]
  (* (decay trig release) src)))

;; this is a sin synth. refactoring the sequence input bus makes it much easier to 
;; define and route multiples. 
(definst sin-it [sequence-bus 0 note-bus 0 release 0.1]
 (let [trig (in:kr sequence-bus)
       note (in:kr note-bus)
       src (sin-osc note)]
  (* (decay trig release) src)))

;; 909 kick clone
;; http://www.nireaktor.com/reaktor-tutorials/how-to-make-a-909-kick-in-reaktor/
(definst kick-909 [sequence-bus 0 master 1.0 freq 60 noise-amp 0.2 noize-decay 0.1 tone-decay 0.3 rez 0.1]
 (let [trig (in:kr sequence-bus)
       noize (* (decay trig noize-decay) (white-noise))
       tone (* (decay trig tone-decay) (rlpf (saw freq) freq rez))]
  (* master (+ (* noise-amp noize) tone))))

(definst snare [sequence-bus 0 master 1.0 freq 110 noise-amp 0.5 noize-decay 0.1 tone-amp 0.1 tone-decay 0.3 rez 0.1]
 (let [trig (in:kr sequence-bus)
       noize (* (decay trig noize-decay) (white-noise))
       tone (* (decay trig tone-decay) (rlpf (saw freq) freq rez))]
  (* master (+ (* noise-amp noize) (* tone-amp tone)))))

;(stop)
(comment
(ctl kick-909 :noise-amp 0.05) 
(ctl kick-909 :noise-decay 0.1) 
(ctl kick-909 :freq 60) 
(ctl kick-909 :rez 0.1) 
(ctl snare :master 1.0)
(ctl snare :tone-amp 0.01)
(ctl snare :freq 880)
(ctl snare :noise-amp 0.2)
(ctl snare :noize-decay 0.1)
(ctl ping-it :release 0.2)
(ctl ping-it :cutoff 300)
(ctl ping-it :rez 0.1)
)
 


;; start up the timer synths
(comment
(do 
(def r-trg (root-trg))
(def r-cnt (root-cnt [:after r-trg]))
(def b-trg (beat-trg [:after r-trg]))
(def b-cnt (beat-cnt [:after r-trg]))
(def m-cnt8  (meter-cnt meter-cnt-bus 8))
(def note-seq (note-sequencer buf-1 meter-cnt-bus note-bus))
(def note-seq-2 (note-sequencer buf-3 meter-cnt-bus note-bus))
(def trigger-seq (trigger-sequencer buf-0 sequence-bus))
(def trigger-seq-2 (trigger-sequencer buf-2 sequence-bus-2))
(def trigger-seq-3 (trigger-sequencer buf-4 sequence-bus-3))
)
)

;; change tempo
(ctl r-trg :rate 220)

(kick-909 :sequence-bus sequence-bus)
;(snare    :sequence-bus sequence-bus-2)
;(ping-it  :sequence-bus sequence-bus-3 :note-bus note-bus)

;; create the synth
;(ping-env :note-bus note-bus :sequencer buf-0)
;(ping-it :sequence-bus sequence-bus :note-bus note-bus :release 0.1)
;(sin-it :sequence-bus sequence-bus :note-bus note-bus :release 0.1)
;(ctl sin-it :release 0.75)
;(ctl ping-it :release 0.75)
;(ctl ping-it :cutoff 220)
;(ctl ping-it :rez 0.1)


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
