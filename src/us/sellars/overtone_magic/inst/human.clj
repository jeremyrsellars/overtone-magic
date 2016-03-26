(ns us.sellars.overtone-magic.inst.human
  (:require [overtone.core]
            [overtone.music.pitch :refer [midi->hz]]
            [overtone.sc.server :refer :all]
            [overtone.sc.ugens :refer :all]
            [overtone.sc.envelope :refer :all]
            [overtone.sc.synth :refer :all]
            [overtone.studio.inst :refer :all]))

(definst whistle [note 81 amp 1 gate 1]
  (let [freq (midicps note)
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
          amp 0.125))))
