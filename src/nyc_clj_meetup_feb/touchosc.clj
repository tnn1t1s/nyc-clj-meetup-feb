(ns nyc-clj-meetup-feb.touchosc
      (:use overtone.live))

;; start an osc server on port 44100
(def server (osc-server 44100 "osc-clj-meetup"))
(zero-conf-on)

(osc-listen server (fn [msg] (println msg)) :debug)
;(osc-rm-listener server :debug)
;(osc-close server)
;
(defn control-scale
   [inst key val x y]
   (let [val (scale-range val 0 1 x y)]
           (do (ctl inst key val))))

