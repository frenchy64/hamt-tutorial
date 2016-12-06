(ns com.ambrosebs.fast-attr
  (:use criterium.core)
  (:require [potemkin :refer [def-map-type unify-gensyms]]
            [clojure.pprint :refer [pprint]]))

(defrecord A [a])

; (Map (Set Kw) [(Vec Kw) Map (U nil Map) -> Map])
(def map-ctor-cache 
  (atom {}))

; (Atom (Map (Set Kw) Int))
;; number of times a particular keyset is looked up
(def keys-frequencies 
  (atom {}))

(defn has-cached-class [kws]
  (@map-ctor-cache kws))

(def ^:const gen-threshold 16)

(declare generate-fast-kw-map)

(def current-thread (gensym))

(let [actually-me current-thread]
  (.start 
    (Thread.
      (fn []
        (loop []
          (Thread/sleep 100)
          ;; kill the thread if we recompile
          (when (= actually-me current-thread)
            (doseq [[k v] @keys-frequencies]
              (when (< gen-threshold v)
                (generate-fast-kw-map k)))
            (recur)))))))

(defn get-occured! [kws]
  {:pre [(set? kws)]}
  (swap! keys-frequencies update kws (fnil inc 0)))

(defn assoc-occured! [kws]
  {:pre [(set? kws)]}
  (swap! keys-frequencies update kws (fnil inc 0)))

(defn quoted-cached-map-fn [kws]
  {:pre [(set? kws)]}
  (let [kws-sorted (sort kws)
        munge-kws (mapv (fn [k]
                          (symbol
                            (munge
                              (str "__KW__"
                                   (namespace k)
                                   (name k)))))
                        kws-sorted)
        cls-nme (gensym "CachedMap")
        kw-set (set kws)
        kw-set-sym (gensym 'kw-set)
        distribute-kws (gensym 'distribute-kws)
        entire-map (gensym 'entire-map)
        k (gensym 'k)
        v (gensym 'v)
        mta (gensym 'mta)
        ]
    `(do
       (deftype ~cls-nme [~@munge-kws ~entire-map ~mta ~kw-set-sym]
         clojure.lang.IPersistentMap
         clojure.lang.IKeywordLookup
         (~'valAt [this# k#]
           (.valAt this# k# nil))
         (~'valAt [_# k# default-value#]
           (case k#
             ~@(interleave kws-sorted
                           (map (fn [ret]
                                  `(do
                                     ;(prn "cached result: " ~ret)
                                     ~ret))
                                munge-kws))
             (get ~entire-map k# default-value#)))
         clojure.lang.IPersistentCollection
         (~'cons [this# [k# v#]]
           (assoc this# k# v#))
         (~'assoc [_# ~k ~v]
           (if (keyword? ~k)
             (case ~k
               ;; if key is already present, just replace the 
               ;; relevant field
               ~@(mapcat (fn [k-exists i]
                           {:pre [(keyword? k-exists)]}
                           [k-exists
                            `(new ~cls-nme
                                  ~@(assoc munge-kws i v)
                                  (assoc ~entire-map ~k ~v)
                                  ~mta
                                  ~kw-set-sym)])
                         kws-sorted
                         (range))
               ;; if it's a new key, try and find a fast version
               (let [kw-set2# (conj ~kw-set-sym ~k)]
                 (if-let [ctor# (has-cached-class kw-set2#)]
                   (ctor# kw-set2# ~entire-map ~mta)
                   ;; give up
                   (with-meta
                     (assoc ~entire-map ~k ~v)
                     ~mta))))
             ;; just extend the entire map
             (new ~cls-nme ~@munge-kws (assoc ~entire-map ~k ~v) ~mta ~kw-set-sym)))
         (~'getLookupThunk [this# k#]
           (let [~'gclass (class this#)]              
             (case k#
               ~@(let [hinted-target (with-meta 'gtarget {:tag cls-nme})] 
                   (mapcat 
                     (fn [kw fld]
                       [kw
                        `(reify clojure.lang.ILookupThunk
                           (get [~'thunk ~'gtarget]
                             (if (identical? (class ~'gtarget) ~'gclass)
                               (. ~hinted-target ~(symbol (str "-" fld)))
                               ~'thunk)))])
                     kws-sorted
                     munge-kws))
               nil)))

         (~'without [_# k#]
           (if (keyword? k#)
             (if nil #_(has-cached-class)
               nil ;(construct-cached-class)
               (with-meta
                 (dissoc ~entire-map k#)
                 ~mta))
             (new ~cls-nme ~@munge-kws (dissoc ~entire-map k#) ~mta ~kw-set-sym))))
       (fn [kws# ~entire-map mta#]
         (let [~distribute-kws (vec (sort kws#))]
           (new ~cls-nme
                ~@(map (fn [i]
                         {:pre [(integer? i)]}
                         `(get ~entire-map 
                               (nth ~distribute-kws ~i)))
                       (range (count kws)))
                ~entire-map
                mta#
                kws#))))))

; kws : (Set Kw)
; entire-map : Map
; mta : (U nil Map)
(deftype UncachedMap [kws entire-map mta]
  clojure.lang.IPersistentMap
  (valAt [this k]
    (.valAt this k nil))
  (valAt [_ k default-value]
    #_
    (when (< (rand) 0.001)
      (get-occured! kws))
    (get entire-map k default-value))
  clojure.lang.IPersistentCollection
  (cons [this [k v]]
    (assoc this k v))
  (assoc [_ k v]
    (let [entire-map (assoc entire-map k v)]
      (if (keyword? k)
        (let [kws (conj kws k)]
          (assoc-occured! kws)
          (if-let [ctor (has-cached-class kws)]
            (do
              (prn "use optimised map")
              (ctor kws entire-map mta))
            ;; give up
            (new UncachedMap kws entire-map mta)))
        ;; just extend the entire map
        (new UncachedMap kws entire-map mta))))
  (without [_ k]
    (if (keyword? k)
      (if-let [ctor (has-cached-class (disj kws k))]
        (ctor (disj kws k) (dissoc entire-map k) mta)
        (new UncachedMap (disj kws k) (dissoc entire-map k) mta))
      (new UncachedMap kws (dissoc entire-map k) mta)))
  (seq [_] (seq entire-map))
  (empty [_] (with-meta (empty entire-map) mta))
  )

(defn uncached-map [& args]
  (loop [m (transient {})
         kws #{}
         args args]
    (if (not args)
      (do
        (assoc-occured! kws)
        (if-let [ctor (has-cached-class kws)]
          (ctor kws (persistent! m) nil)
          (new UncachedMap kws (persistent! m) nil)))
      (let [[k & args] args
            _ (assert args "Odd number of arguments")
            [v & args] args]
        (recur (assoc! m k v)
               (if (keyword? k)
                 (conj kws k)
                 kws)
               args)))))

(defn uncached-1arg [m]
  (into (uncached-map) m))

(defn generate-fast-kw-map [kws]
  {:pre [(set? kws)]}
  (swap! map-ctor-cache
         update kws
         (fn [f]
           (if f
             f
             (eval
               (quoted-cached-map-fn kws))))))

(def per-hash
  (hash-map
    :a 1
    :b 2
    :c 3
    :d 4
    :e 5
    :f 6
    :g 7
    :h 8
    :i 9
    :j 10
    ))

(assert (instance? clojure.lang.PersistentHashMap per-hash))

(def opt-hm
  (let [kw-set (set (filter keyword? (keys per-hash)))]
    (;pprint
      (eval
        (quoted-cached-map-fn
          kw-set))
      kw-set
      per-hash
      nil)))

(def no-cache
  (uncached-map
    :a 1
    :b 2
    :c 3
    :d 4
    :e 5
    :f 6
    :g 7
    :h 8
    :i 9
    :j 10))

(comment
  (time
    (get per-hash :a))
  (time
    (get opt-hm :a))
  (time
    (get opt-hm :b))
  (get opt-hm 1)
  (pprint
    (quoted-cached-map-fn (set (keys per-hash))))
  )

(defn get-per-hash []
  (let [m per-hash]
    (with-progress-reporting
      (quick-bench
        (:a m)))))

(defn get-big-case []
  (let [m per-hash]
    (with-progress-reporting
      (quick-bench
        (case :a
          :a 1
          :b 2
          :c 3
          :d 4
          :e 5
          :f 6
          :g 7
          :h 8
          :i 9
          :j 10
          )))))

(defn compile-big-case []
  (let [m per-hash]
    (with-progress-reporting
      (quick-bench
        (eval
          '(case :a
             :a 1
             :b 2
             :c 3
             :d 4
             :e 5
             :f 6
             :g 7
             :h 8
             :i 9
             :j 10
             ))))))

(defn get-record []
  (let [m (into (->A 1)
                per-hash)]
    (with-progress-reporting
      (quick-bench 
        (:a m)))))

(defn get-id-map []
  (let [m (java.util.IdentityHashMap. per-hash)]
    (with-progress-reporting
      (quick-bench 
        (:a m)))))

(defn get-conc-map []
  (let [m (java.util.concurrent.ConcurrentHashMap. per-hash)]
    (with-progress-reporting
      (quick-bench 
        (:a m)))))

(defn get-opt-hm []
  (let [m opt-hm]
    (with-progress-reporting
      (quick-bench 
        (:a m)))))

(defn get-nocache-hm []
  (let [m no-cache]
    (with-progress-reporting
      (quick-bench 
        (:a m)))))

(defn exercise-bench [f n]
  (loop [i 20
         m (f (into {:a 1 :b 2 :c 3 :d 4
                     :e 5 :f 6 :g 7 :h 8}
                    (map #(vector % %) (range n))))]
    (when-not (zero? i)
      (dotimes [_ 100000] 
        (+ (:a m) (:b m) (:c m) (:d m)
           (:e m) (:f m) (:g m) (:h m)))
      (recur (dec i) (update m :a inc)))))

(def extra 100)

(defrecord Exercise [a b c d e f g h])

(defmacro time-to-out
  "Evaluates expr and prints the time it took.  Returns the value of
  expr."
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         ms# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
         _# (prn (str "Elapsed time: " ms# " msecs"))
         ]
     [ret# ms#]))

(def latest-graph (atom {}))

(defn record-fs [m extra]
  (run! (fn [[label f]]
          (let [[ret ms] (time-to-out (exercise-bench f extra))]
            (swap! latest-graph assoc-in [label extra] ms)))
        m))

(reset! latest-graph {})
#_
(dotimes [i 20]
  (let [extra (* i 100)]
    (record-fs {:plain-map #(into {} %)
                :record map->Exercise
                :rt-cache uncached-1arg}
               extra)
    #_(exercise-bench #(into {} %) extra)
    #_(exercise-bench map->Exercise extra)
    #_(exercise-bench uncached-1arg extra)))

(def data '{:plain-map {0 837.900884, 1300 2049.42444, 300 1505.881845, 600 1387.997231, 1400 1817.21074, 1000 1521.329677, 500 1759.423488, 1800 1757.819692, 1700 2129.781288, 1900 2973.424349, 100 1435.768863, 1200 2573.8774, 800 1784.948714, 1600 1936.875666, 1500 2023.47916, 1100 1855.281003, 200 1447.167303, 900 1481.740526, 700 1729.756068, 400 1331.124945}, :record {0 840.862397, 1300 1212.918047, 300 904.195596, 600 857.498675, 1400 1047.268085, 1000 1079.314484, 500 849.688625, 1800 1400.642201, 1700 1181.103518, 1900 2108.923538, 100 894.77844, 1200 1334.392182, 800 1099.811693, 1600 1232.646281, 1500 1110.733413, 1100 1598.098832, 200 921.830137, 900 936.40417, 700 884.393927, 400 868.788413}, :rt-cache {0 892.363186, 1300 1047.200465, 300 944.139623, 600 835.013332, 1400 1112.890638, 1000 904.91121, 500 907.794038, 1800 3016.390694, 1700 2109.647289, 1900 3844.42986, 100 972.581187, 1200 1294.22056, 800 938.415172, 1600 1261.062708, 1500 1203.444674, 1100 1569.635548, 200 921.286823, 900 960.614487, 700 841.138563, 400 850.684909}})

(defn to-tex [data]
  (vec
    (for [[k v] data]
      [k (apply str
                (mapv
                  (fn [[k v]]
                    (str "(" k "," v ") "))
                  (sort-by first v)))])))
