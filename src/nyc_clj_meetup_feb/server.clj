(ns nyc-clj-meetup-feb.server
  (:use overtone.live))

;; this is a simple touchosc mini synth; two oscillators and a rez lowpass filter

;; start an osc server on port 44100
(def server (osc-server 44100 "osc-clj-meetup"))

;;(zero-conf-on)


;;(osc-listen server (fn [msg] (println msg)) :debug)
;;(osc-rm-listener server :debug)

;;(osc-handle server "/1/fader4" (fn [msg] (println msg)))


;;(definst foo [freq 440] (sin-osc freq))
(definst foo [master 1.0 mute-osc-1 1.0 mute-osc-2 1.0 freq-osc1 440 freq-osc2 880 cutoff 440 rez 1.0] 
    (* master 
      (rlpf (+ (* mute-osc-1 (saw freq-osc1)) (* mute-osc-2 (saw freq-osc2))) cutoff rez)))



(defn control-foo 
 [key val] 
 (let [val (scale-range val 0 1 50 1000)]
      (ctl foo key val)))

(defn control-foo-no-scale
 [key val] 
      (ctl foo key val))

(defn control-foo-flip-it
 [key val] 
      (ctl foo key (- 1.0 val)))

(osc-handle server "/1/fader1" (fn [msg] (control-foo :freq-osc1 (first (:args msg)))))
(osc-handle server "/1/fader2" (fn [msg] (control-foo :freq-osc2 (first (:args msg)))))
(osc-handle server "/1/fader3" (fn [msg] (control-foo :cutoff (first (:args msg)))))
(osc-handle server "/1/fader4" (fn [msg] (control-foo-flip-it :rez (first (:args msg)))))
(osc-handle server "/1/fader5" (fn [msg] (control-foo-no-scale :master (first (:args msg)))))

;; toggles
(osc-handle server "/1/toggle1" (fn [msg] (control-foo-no-scale :mute-osc-1 (first (:args msg)))))
(osc-handle server "/1/toggle2" (fn [msg] (control-foo-no-scale :mute-osc-2 (first (:args msg)))))

(foo)
;;(stop)

;;(osc-listen server (fn [msg] (println msg)) :debug)
;;(osc-rm-listener server :debug)




