(use 'overtone.core)
(connect-external-server)

(ns us.sellars.overtone-magic.core
  (:require [overtone.core]
            [overtone.music.pitch :refer [midi->hz note]]
            [overtone.sc.sample :refer :all]
            [overtone.sc.server :refer :all]
            [overtone.sc.envelope :refer :all]
            [overtone.sc.node :refer :all]
            [overtone.sc.synth :refer :all]
            [overtone.sc.ugens :refer :all]
            [overtone.sc.cgens.buf-io :refer [scaled-play-buf]]
            [overtone.studio.inst :refer :all]
            [us.sellars.overtone-magic.players.sustain
             :refer [sustain-midi-poly-player sustain-midi-player-stop]]
            #_[us.sellars.overtone-magic.players.mono
             :refer [fm-midi-mono-player]]))
#_
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

(def one-mouth-whistle (fm-midi-mono-player whistle-wip))
(stop)
(fm-midi-mono-player-stop one-mouth-whistle)

#_(def smpp (sustain-midi-poly-player overtone.inst.sampled-piano/sampled-piano))
#_(sustain-midi-player-stop smpp)
(stop)

;(def w (whistle))
;(ctl w :note 77 :gate 0)



#_(ns us.sellars.overtone-magic.players.mono
  #_(:use [overtone.studio.midi]
        [overtone.sc.node]
        [overtone.sc.dyn-vars])
  (:require [overtone.core]
    #_[overtone.sc.node :refer :all]
            #_[overtone.libs.event :as e :refer [remove-event-handler]]
            #_[overtone.studio.midi :refer :all]))

(require '[overtone.core]
    '[overtone.sc.node :refer :all]
      '[overtone.libs.event :as e :refer [remove-event-handler]]
       '[overtone.studio.midi :refer :all]
        '[overtone.sc.dyn-vars :refer :all])
(def sustain-control-number 64)
(def button-control-number 15)

(defn fm-midi-mono-player
  "Sets up the event handlers and manages synth instances to easily play
  a monophonic instrument with a midi controller.  The play-fn should
  take the note and velocity as the only two arguments, and the synth
  should have a gate parameter that can be set to zero when a :note-off
  event is received (or the sustain pedal is released).

  == While not sustained ==
  When a key is depressed, the note played and its token is tracked in notes*.
  When a key is released, the note is stopped and removed from notes*.
  == When pedal moves down ==
  Notes are copied from notes* to sustained-notes*
  == While sustained ==
  When a key is depressed, the note played and its token is tracked in both notes* and sustained-notes*.
  When a key is released, the note is removed from notes*.
  == When pedal is released ==
  sustained-notes* are stopped unless they are still in notes*.  *sustained-notes is cleared.

    (definst triangle-wave [note 60 amp 1 gate 1]
      (let [attack 0.8 decay 0.5 sustain 0.8 release 1.0 vol 0.4]
        (* (env-gen (adsr 0.01 0.1 0.6 0.3) gate :action FREE)
          (sin-osc (midicps note))
          vol)))

    (def triangulator (fm-midi-mono-player triangle-wave))
  "
  ([play-fn] (fm-midi-mono-player play-fn ::fm-midi-mono-player))
  ([play-fn player-key] (fm-midi-mono-player play-fn [:midi] player-key))
  ([play-fn device-key player-key]
   (let [notes*           (atom '())
         node*           (atom nil)
         on-event-key   (concat device-key [:note-on])
         off-event-key  (concat device-key [:note-off])
         ;ctl-key        (concat [::fm-midi-mono-player] ctl-event-key)
         on-key         (concat [::fm-midi-mono-player] on-event-key)
         off-key        (concat [::fm-midi-mono-player] off-event-key)
         dump-state     (fn dump-state []
                          (println "fingers:" @notes* ".  playing: " @node*))
         play           (fn play [pfn note]
                          (let [n (swap! node* #(or % (pfn)))]
                            (swap! notes* #(cons note %))
                            (ctl n :note note)))
         release-note   (fn release-note [note desc]
                          (println "releasing " note " because " desc)
                          (let [new-notes (swap! notes* (fn [notes] (remove #(== note %) notes)))
                                n @node*]
                            (if (empty? new-notes)
                              (with-inactive-node-modification-error :silent
                                (println desc " releasing.... " n)
                                (node-control n [:gate 0 :after-touch 0])
                                (reset! node* nil))
                              (ctl n :note (first new-notes)))))]

       #_(e/on-event ctl-event-key (fn [{control-number :data1, value :data2}])
                                   (println {:control-number control-number, :value value})
                                   (when (= control-number button-control-number)
                                     (doseq [[node note] @playing-notes*]
                                        (release-note node "button ")))
                   ctl-key)

       (e/on-event on-event-key (fn [{note :note velocity :velocity}]
                                  (let [amp (float (/ velocity 127))]
                                    (play #(play-fn :note note :amp amp :velocity velocity) note)
                                    (dump-state)))
                   on-key)

       (e/on-event off-event-key (fn [{note :note velocity :velocity}]
                                   (let [velocity (float (/ velocity 127))]
                                     (release-note note "key up")
                                     (dump-state)))
                   off-key)

       ;; TODO listen for '/n_end' event for nodes that free themselves
       ;; before recieving a note-off message.
       (let [player (with-meta {:notes* notes*
                                :on-key on-key
                                :off-key off-key
                                :device-key device-key
                                :player-key player-key
                                :playing? (atom true)}
                      {:type ::fm-midi-mono-player})]
         (swap! poly-players* assoc player-key player)
         player))))

(defn fm-midi-mono-player-stop
  ([]
   (remove-event-handler [::fm-midi-mono-player :midi :note-on])
   (remove-event-handler [::fm-midi-mono-player :midi :note-off]))
  ([player-or-key]
   (if (keyword? player-or-key)
     (midi-player-stop (get @poly-players* player-or-key))
     (let [player player-or-key]
       (when-not (= :fm-midi-mono-player (type player))
         (throw (IllegalArgumentException. (str "Expected a midi-poly-player. Got: " (prn-str (type player))))))
       (remove-event-handler (:on-key player))
       (remove-event-handler (:off-key player))
       (reset! (:playing? player) false)
       (swap! poly-players* dissoc (:player-key player))
       player))))

#_(comment)
  (use 'overtone.core)
  (use 'overtone.inst.synth)
  (require '[overtone.studio.sustain-midi-player :as smp])

  (fm-midi-mono-player-stop smpp)

  (def smpp (fm-midi-mono-player whistle-wip))
  (fm-midi-player-stop smpp)
  (stop)


(defn sample-player
  "Play the specified sample with either a mono or stereo player
   depending on the number of channels in the sample. Always creates a
   stereo signal.

   Accepts same args as both players, namely:

   {:buf 0 :rate 1.0 :start-pos 0.0 :loop? 0 :amp 1 :out-bus 0}

   If you wish to specify a group target vector i.e. [:head foo-g] this
   argument must go *after* the smpl argument:

   (sample-player my-samp [:head foo-g] :rate 0.5)"
  [smpl & pargs] {:pre [(sample? smpl)]}
  (let [{:keys [path args]}     smpl
        {:keys [id n-channels]} (get @cached-samples* [path args])
        [target pos pargs]      (extract-target-pos-args pargs
                                                               (foundation-default-group)
                                                               :tail)]
    (cond
      (= n-channels 1) (apply mono-partial-player [pos target] id pargs)
      (= n-channels 2) (apply stereo-partial-player [pos target] id pargs))))

(def c2 (sample "C:\\sound\\BPB Mini Analogue Collection\\Samples\\VHS Brass (Korg Poly-800)\\c2.wav"))


(def x (sample-player c2 :rate 0.8))
(stop)


(def brass-files
  (sorted-map
    (note "c1") "c1.wav"
    (note "c2") "c2.wav"
    (note "c3") "c3.wav"
    (note "c4") "c4.wav"
    (note "c5") "c5.wav"
    (note "e1") "e1.wav"
    (note "e2") "e2.wav"
    (note "e3") "e3.wav"
    (note "e4") "e4.wav"
    (note "g#1") "g#1.wav"
    (note "g#2") "g#2.wav"
    (note "g#3") "g#3.wav"
    (note "g#4") "g#4.wav"
  ))

(def brass-samples
  (reduce-kv
    (fn [a k v]
      (assoc a k
        (sample (str "C:\\sound\\BPB Mini Analogue Collection\\Samples\\VHS Brass (Korg Poly-800)\\" v))))
    (sorted-map)
    brass-files))


(defn best-fit [note notes]
  (->> notes
       (filter (fn [[n _]] (>= note n)))
       last))

(let [s brass-samples
      fit (fn fit [note]
            (let [[n _] (best-fit note s)]
              n))
      test-fit (fn [expected note]
                 (when-not (== expected (fit note))
                   (str "Expected " expected ", but got " (fit note))))]
  (filter identity
    (list (test-fit 24 24)
          (test-fit 24 25)
          (test-fit 24 26)
          (test-fit 24 27)
          (test-fit 28 28)
          (test-fit 28 29)
          (test-fit 28 30)
          (test-fit 28 31)
          (test-fit 32 32)
          (test-fit 32 33))))

(defn calc-rate [new-note sample-note]
  (-> (- new-note sample-note)
      (/ 12)
      (+ 1)))


(defsynth sampled-brass
  [note 60 level 1 loop? 0
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1 out-bus 0 amp 1]
  (let [note-value (:value note)
        [sample-note buf] (best-fit note-value brass-samples)
        rate (calc-rate note-value sample-note)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate
                     :action FREE)]
    (out out-bus (* env amp (scaled-play-buf 2 buf :rate rate :level level :loop loop? :action FREE)))))

(definst sampled-brass
  [note 60 level 1 loop? 0
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1 out-bus 0 amp 1]
  (let [note-value (:value note)
        [sample-note buf] (best-fit note-value brass-samples)
        rate (calc-rate note-value sample-note)
        _ (println rate)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate
                     :action FREE)]
    (* env amp (scaled-play-buf 1 buf :rate rate :level level :loop loop? :action FREE))))

(clojure.repl/doc index:kr)

;(def x (midi-poly-player sampled-brass))
(def horn (sustain-midi-poly-player sampled-brass))




