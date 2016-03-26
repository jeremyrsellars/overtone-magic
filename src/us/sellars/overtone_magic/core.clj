(ns us.sellars.overtone-magic.core
  (:require [overtone.core]
            [overtone.music.pitch :refer [midi->hz]]
            [overtone.sc.server :refer :all]
            [overtone.sc.ugens :refer :all]
            [overtone.sc.envelope :refer :all]
            [overtone.sc.synth :refer :all]
            [overtone.studio.inst :refer :all]
            [us.sellars.overtone-magic.players.sustain
             :refer [sustain-midi-poly-player sustain-midi-player-stop]]))

;cd "C:\Program Files (x86)\SuperCollider-3.6.6"&scsynth.exe -u 57110

;(connect-external-server)
;(use 'overtone.inst.synth)
;(use 'overtone.samples.flute)
;(require '[overtone.inst.sampled-piano :refer [sampled-piano]])
;(use 'overtone.inst.sampled-flute)

    (definst triangle-wave [note 60 amp 1 gate 1]
      (let [attack 0.01 decay 0.1 sustain 0.8 release 1.0 vol 0.6]
        (* (env-gen (adsr attack decay sustain release) gate :action FREE)
          (lf-tri (midicps note))
          vol)))

    (clojure.repl/doc env-sine)
    (clojure.repl/doc dadsr)
    (clojure.repl/doc adsr)


(definst whistle-wip [note 81 amp 1 gate 1]
  (let [freq (midicps note)
        input  (lf-saw freq)
        shift1 (lf-saw 4)
        shift2 (lf-saw 7)
        shift3 (lf-saw 5)
        shift4 (lf-saw 2)
        comp1  (> input shift1)
        comp2  (> input shift2)
        comp3  (> input shift3)
        comp4  (> input shift4)
        output (+ (- input comp1) (- input comp2) (- input comp3) (- input comp4))
        output (- output input)
        output (leak-dc:ar (* output 0.25))

        attack 0.01 decay 0.1 sustain 0.8 release 0.2]

    (+
      ; inital breath puff
      (* (env-gen (adsr 0.15 1 0.0 0.2) gate :action FREE)
          (bpf (pink-noise) freq 0.016125)
          8 (amp-comp-a))
      ; sustained breath
      (* (env-gen (asr 0.1 0.7 0.2) gate :action FREE)
          (bpf (pink-noise) freq 0.006125)
          16 (sin-osc 2) (amp-comp-a))

      ; harmonics
      (* (env-gen (adsr 1 decay sustain release) gate :action FREE)
          (+ (sin-osc (* (midicps note) 0.990))
             (sin-osc (* (midicps note) 0.993))
             (sin-osc (* (midicps note) 0.998))
             (sin-osc (* (midicps note) 1.0001))
             (sin-osc (* (midicps note) 1.0003))
             (sin-osc (* (midicps note) 1.0005))
             (* 0.5 (sin-osc (* (midicps note) 0.499999)))
             (* 0.25 (sin-osc (* (midicps note) 0.25001)))
             (sin-osc (midicps note)))
          amp 0.125)

      #_(* (env-gen (adsr attack decay sustain release) gate :action FREE)
          (lf-tri (/ (midicps note) 2))
          amp)

      #_(* amp output
         (env-gen (adsr 0.01 0.1 sustain 0.3) gate :action FREE)))))

    (def triangulator (sustain-midi-poly-player whistle))


#_(def smpp (sustain-midi-poly-player overtone.inst.sampled-piano/sampled-piano))
#_(sustain-midi-player-stop smpp)
(stop)



(midi->hz 60)
