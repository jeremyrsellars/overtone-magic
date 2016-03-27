(ns us.sellars.overtone-magic.players.mono
  (:use [overtone.studio.midi]
        [overtone.sc.node]
        [overtone.sc.dyn-vars])
  (:require [overtone.libs.event :as e :refer [remove-event-handler]]))

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
                            (swap! notes* cons note note)
                            (ctl n :note note)))
         release-note   (fn release-note [note desc]
                          (let [new-notes (swap! notes* (fn [notes] (remove #(== note %))))
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
                                     (when-let [n (get @notes* note)]
                                       (release-note note "key up"))
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

(defn sustain-midi-player-stop
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

  (smp/sustain-midi-player-stop smpp)
  (def smpp (smp/fm-midi-mono-player overtone.inst.sampled-piano/sampled-piano))
  (smp/sustain-midi-player-stop smpp)
  (stop)
