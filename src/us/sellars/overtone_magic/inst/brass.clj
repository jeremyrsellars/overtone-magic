; (use 'overtone.core)
; (connect-external-server)

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


(let [notes [60 72]
      fit (fn fit [note]
            (let [[n _] (best-fit note n)]
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

(def x (midi-poly-player sampled-brass))
