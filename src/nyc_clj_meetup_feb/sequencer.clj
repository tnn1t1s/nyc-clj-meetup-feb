(ns nyc-clj-meetup-feb.sequencer
  (:use overtone.live))


;; start an osc server on port 44100
(def server (osc-server 44100 "osc-clj-meetup"))
(zero-conf-on)

; (osc-listen server (fn [msg] (println msg)) :debug)
; (osc-rm-listener server :debug)
;
;{:src-port 49492, :src-host new-host.home, :path /2/toggle1, :type-tag f, :args (0.0)}
;{:src-port 49492, :src-host new-host.home, :path /2/fader2, :type-tag f, :args (0.027918782)}

(defn control-ping-scale
 [synth key val]
 (let [val (scale-range val 0 1 50 1000)]
      (ctl synth key val)))

(defn control-synth-release
 [synth val]
 (let [val (scale-range val 0 1 0 10)]
      (ctl synth :release val))) 

(control-ping-scale (:0 ping-map) :freq 0.5)
;(control-ping-scale b :freq 0.8)
;(control-ping-scale c :freq 0.5)
;(control-ping-scale d :freq 0.1)
;(ctl ping-env :release 4.0)
(stop)

(def ping-map {
  :0 (ping-env :beat-num 0 :sequencer buf-0)
  :1 (ping-env :beat-num 1 :sequencer buf-0)
  :2 (ping-env :beat-num 2 :sequencer buf-0)
  :3 (ping-env :beat-num 3 :sequencer buf-0)
  :4 (ping-env :beat-num 4 :sequencer buf-0)
  :5 (ping-env :beat-num 5 :sequencer buf-0)
  :6 (ping-env :beat-num 6 :sequencer buf-0)
  :7 (ping-env :beat-num 7 :sequencer buf-0)
})

; (stop)
;; start up the timer synths
(do 
(def r-trg (root-trg))
(def r-cnt (root-cnt [:after r-trg]))
(def b-trg (beat-trg [:after r-trg]))
(def b-cnt (beat-cnt [:after r-trg]))
)

(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

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


; (env-gen (adsr) :gate gate :action NO-ACTION)
(definst ping-env [freq 440 release 0.1 beat-num 0 sequencer 0]
 (let [src1 (sin-osc freq)
       cnt (in:kr beat-cnt-bus)
       beat-trg (in:kr beat-trg-bus)
       bar-trg (and 
          (buf-rd:kr 1 sequencer cnt)
          (= beat-num (mod cnt 8)) 
          beat-trg)]
  (* (decay bar-trg release) src1)))

; now ping-env programmable beats
; (ctl ping-env :release 0.9)

(defonce buf-0 (buffer 8))
(buffer-write! buf-0 [1 1 1 1 1 1 1 1])


(ctl ping-env :release 4.0)


(osc-handle server "/1/fader1" (fn [msg] (control-synth-release ping-env (first (:args msg)))))
(control-synth-release ping-env 0.01)

;; link touchosc
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
(osc-handle server "/2/fader1" (fn [msg] (control-ping-scale (:0 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader2" (fn [msg] (control-ping-scale (:1 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader3" (fn [msg] (control-ping-scale (:2 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader4" (fn [msg] (control-ping-scale (:3 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader5" (fn [msg] (control-ping-scale (:4 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader6" (fn [msg] (control-ping-scale (:5 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader7" (fn [msg] (control-ping-scale (:6 ping-map) :freq (first (:args msg)))))
(osc-handle server "/2/fader8" (fn [msg] (control-ping-scale (:7 ping-map) :freq (first (:args msg)))))
)
