(ns nyc-clj-meetup-feb.play
  (:use 
    [overtone.live]
    [nyc-clj-meetup-feb.touchosc]))


;; casio tone for the painfully alone

;; this example shows we can extend the simple trigger sequencer
;; and instrument we built for the ms20 clone into a sort of full featured
;; polypohonic sequencer synthesizer beat box type thing.

;; it's all built inside the supercollider server and controlled via the client.
;; so in practice, later, we could all connect to the server and control the 
;; synth via touchosc.

;; first, we need a data structure to hold our patterns

;; buffers are like vectors that live on the server.  
;; we will use them to store the sequence of events and notes to use
;; in our sequencer
;;
; kicks
(defonce buf-0 (buffer 16))
(defonce buf-1 (buffer 16))
(buffer-write! buf-0 [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
(buffer-write! buf-0 [1 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0])
;(buffer-write! buf-0 (repeatedly 16 #(choose [0 1])))
(buffer-write! buf-1 (repeatedly 16 #(choose [110 220 440 660 880])))

; snares 
(defonce buf-2 (buffer 16))
(defonce buf-3 (buffer 16))
(buffer-write! buf-2 [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0])
(buffer-write! buf-2 (repeatedly 16 #(choose [0 1])))
(buffer-write! buf-3 (repeatedly 16 #(choose [110 220 440 660 880])))

; lead 
(defonce buf-4 (buffer 16))
(defonce buf-5 (buffer 16))
(buffer-write! buf-4 (repeatedly 16 #(choose [0 1])))
(buffer-write! buf-5 (repeatedly 16 #(choose [110 220 440 660 880])))

; hat
(defonce buf-6 (buffer 16))
(defonce buf-7 (buffer 16))
(buffer-write! buf-6 (repeatedly 16 #(choose [0 1])))
(buffer-write! buf-7 (repeatedly 16 #(choose [110 220 440 660 880])))

; inst-1
(defonce buf-8 (buffer 16))
(defonce buf-9 (buffer 16))
(buffer-write! buf-8 (repeatedly 16 #(choose [0 1])))
(buffer-write! buf-9 (repeatedly 16 #(choose [110 220 440 660 880])))

; inst-2
(defonce buf-10 (buffer 16))
(defonce buf-11 (buffer 16))
(buffer-write! buf-10 (repeatedly 16 #(choose [0 1])))

;; buffer utilities
; clear all 
; clear all
;(map #(buffer-write! %1 (repeatedly 16 (fn [] 0))) [buf-0 buf-2 buf-4 buf-6 buf-8 buf-10])

;; timing bus setup
;; these are like the 'patch cables' that we saw in the ms20
;; instead of carrying control-voltage, they will carry signals that
;; we generate via control synths that we build later
;
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count
(defonce meter-cnt-bus (control-bus))
(defonce note-bus-1  (control-bus))
(defonce note-bus-2  (control-bus))
(defonce note-bus-3  (control-bus))
(defonce note-bus-4  (control-bus))
(defonce sequence-bus-1  (control-bus))
(defonce sequence-bus-2  (control-bus))
(defonce sequence-bus-3  (control-bus))
(defonce sequence-bus-4  (control-bus))
(defonce sequence-bus-5  (control-bus))
(defonce sequence-bus-6  (control-bus))

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 220]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth meter-cnt [meter-cnt-bus 0 div 16]
  (out:kr meter-cnt-bus (mod (in:kr beat-cnt-bus) div)))

;; this synth generates a sequence of frequencies from buffer
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
(definst ping-it [sequence-bus 0 note-bus 0 master 0.5 release 0.1 cutoff 880 rez 0.5]
 (let [trig (in:kr sequence-bus)
       note (in:kr note-bus)
       src (rlpf (saw note) cutoff rez)]
  (* master (* (decay trig release) src))))

;; this is a square synth. refactoring the sequence input bus makes it much easier to 
;; define and route multiples. 
(definst square-it [sequence-bus 0 note-bus 0 master 0.5 release 0.1 cutoff-high 1760]
 (let [trig (in:kr sequence-bus)
       note (in:kr note-bus)
       src (hpf (square note) cutoff-high)]
  (* master (* (decay trig release) src))))


;; bass synth
;; https://github.com/overtone/overtone/blob/master/src/overtone/synth/retro.clj
(definst tb-303
  "A clone of the sound of a Roland TB-303 bass synthesizer."
  [sequence-bus 0
   note-bus 0
   master   0.5       ; master volume
   wave     1         ; 0=saw, 1=square
   cutoff   100       ; bottom rlpf frequency
   env      1000      ; + cutoff is top of rlpf frequency
   rez      0.2       ; rlpf resonance
   sus      0         ; sustain level
   dec      1.0       ; decay
   position 0]         ; position in stereo field
  (let [trig (in:kr sequence-bus)
        note (in:kr note-bus)
        freq-val   note
        amp-env    (env-gen (envelope [10e-10, 1, 1, 10e-10]
                                          [0.01, sus, dec]
                                          :exp)
                              :gate trig :action NO-ACTION)
        filter-env (env-gen (envelope [10e-10, 1, 10e-10]
                                          [0.01, dec]
                                          :exp)
                              :gate trig :action NO-ACTION)
        waves      [(* (saw freq-val) amp-env)
                    (* (pulse freq-val 0.5) amp-env)]
        tb303      (rlpf (select wave waves)
                           (+ cutoff (* filter-env env)) rez)]
    (* master (pan2 tb303 position))))

;; 909 kick clone
;; http://www.nireaktor.com/reaktor-tutorials/how-to-make-a-909-kick-in-reaktor/
(definst kick-909 [sequence-bus 0 master 1.0 freq 55 noise-amp 0.2 noize-decay 0.1 tone-decay 0.2 rez 0.1]
 (let [trig (in:kr sequence-bus)
       noize (* (decay trig noize-decay) (hpf (white-noise) 800))
       tone (* (decay trig tone-decay) (rlpf (saw freq) freq rez))]
  (* master (+ (* noise-amp noize) tone))))

;; a snare
(definst snare [sequence-bus 0 master 1.0 freq 110 band-freq 3520 noise-amp 0.5 noize-decay 0.1 tone-amp 0.1 tone-decay 0.3 rez 0.1]
 (let [trig (in:kr sequence-bus)
       noize (* (decay trig noize-decay) (white-noise))
       tone (* (decay trig tone-decay) (rlpf (saw freq) freq rez))]
  (* master (bpf (+ (* noise-amp noize) (* tone-amp tone)) band-freq))))

;; hats
;; 1, 1.3420, 1.2312, 1.6532, 1.9523, 2.1523
(definst hat [sequence-bus 0 master 1.0 freq 880 dly 0.1 hi-cutoff 12000]
 (let [trig (in:kr sequence-bus)
       noize (+ (square freq) (square (* 1.342 freq)) (square (* 1.2313 freq)) (square (* 1.6532 freq)) (square (* 1.9523 freq)) (square (* 2.1523 freq)))]
  (* master (* (decay trig dly) (hpf noize hi-cutoff)))))

;; start up the timer synths
(comment
(do 
(def r-trg (root-trg))
(def r-cnt (root-cnt [:after r-trg]))
(def b-trg (beat-trg [:after r-trg]))
(def b-cnt (beat-cnt [:after r-trg]))
(def m-cnt-16  (meter-cnt meter-cnt-bus 16))
(def note-seq-1 (note-sequencer buf-1 meter-cnt-bus note-bus-1))
(def note-seq-2 (note-sequencer buf-3 meter-cnt-bus note-bus-2))
(def note-seq-3 (note-sequencer buf-5 meter-cnt-bus note-bus-3))
(def note-seq-4 (note-sequencer buf-7 meter-cnt-bus note-bus-4))
(def trigger-seq-1 (trigger-sequencer buf-0 sequence-bus-1))
(def trigger-seq-2 (trigger-sequencer buf-2 sequence-bus-2))
(def trigger-seq-3 (trigger-sequencer buf-4 sequence-bus-3))
(def trigger-seq-4 (trigger-sequencer buf-6 sequence-bus-4))
(def trigger-seq-5 (trigger-sequencer buf-8 sequence-bus-5))
(def trigger-seq-6 (trigger-sequencer buf-10 sequence-bus-6))
)
)
;(stop)
;; change tempo
;(ctl r-trg :rate 220)
;; create the instruments
(comment
(do
  (kick-909 :sequence-bus sequence-bus-1)
  (snare    :sequence-bus sequence-bus-2)
  (hat :sequence-bus sequence-bus-3)
  (ping-it  :sequence-bus sequence-bus-4 :note-bus note-bus-4)
  (tb-303  :sequence-bus sequence-bus-5 :note-bus note-bus-3 :master 0.7)
  (square-it  :sequence-bus sequence-bus-6 :note-bus note-bus-2))
)


; factory defaults
(comment
(ctl kick-909 :master 0.0) 
(ctl snare :master 1.0)
(ctl kick-909 :master 1.0) 
(ctl kick-909 :noise-amp 0.05) 
(ctl kick-909 :noise-decay 0.01) 
(ctl kick-909 :tone-decay 0.2) 
(ctl kick-909 :freq 55)
(ctl kick-909 :rez 0.05) 
(ctl snare :tone-amp 0.0)
(ctl snare :freq 1760)
(ctl snare :band-freq 1760)
(ctl snare :noise-amp 0.1)
(ctl snare :noize-decay 0.5)
(ctl ping-it :release 2.0)
(ctl ping-it :cutoff 1200)
(ctl ping-it :rez 0.1)
(ctl square-it :release 0.9)
(ctl tb-303 :master 0.4)
(ctl tb-303 :cutoff 300)
(ctl tb-303 :sus 0.1)
(ctl tb-303 :rez 0.2)
(ctl hat :master 1.5)
)

; touchosc setup

; myfx returns a function that write index id into buf 
(defn myfx [buf id] (fn [msg] 
                      (buffer-write! buf id [(int (first (:args msg)))])))

(do
  (map #(osc-handle server (str "/2/multitoggle/1/"(+ %1 1)) (myfx buf-0 %1)) (range 0 16)) 
  (map #(osc-handle server (str "/2/multitoggle/2/"(+ %1 1)) (myfx buf-2 %1)) (range 0 16))
  (map #(osc-handle server (str "/2/multitoggle/3/"(+ %1 1)) (myfx buf-4 %1)) (range 0 16))
  (map #(osc-handle server (str "/2/multitoggle/4/"(+ %1 1)) (myfx buf-6 %1)) (range 0 16))
  (map #(osc-handle server (str "/2/multitoggle/5/"(+ %1 1)) (myfx buf-8 %1)) (range 0 16))
  (map #(osc-handle server (str "/2/multitoggle/6/"(+ %1 1)) (myfx buf-10 %1)) (range 0 16))
  (defn myfx-scale [buf id] (fn [msg] (buffer-write! buf id [(scale-range (int (first (:args msg))) 0 1 110 880)])))
  (map #(osc-handle server (str "/2/multifader/" (+ %1 1)) (myfx-scale buf-5 %1)) (range 0 16)))
