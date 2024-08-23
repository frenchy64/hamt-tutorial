(ns com.ambrosebs.map.visualize
  (:require [rhizome.dot :as dot]
            [rhizome.viz :as viz]
            [com.ambrosebs.map :as map]
            [clojure.core :as core])
  (:import (com.ambrosebs.map BitmapIndexedNode HashCollisionNode
                              PersistentHashMap ArrayNode)))

(set! *warn-on-reflection* true)

(defn binary-str 
  ([bitmap] (binary-str bitmap 32))
  ([bitmap padding]
   (let [s (Integer/toBinaryString bitmap)]
     (if (= padding (count s))
       s
       (str (apply str (repeat (- padding (count s)) \0))
            s)))))


(defprotocol IMapVisualize
  ;; return a map {:label n}
  (visualize* [this level]))

(extend-protocol IMapVisualize
  BitmapIndexedNode
  (visualize* [this level]
    (let [bitmap (map/node-bitmap this)
          ^objects array (.array this)]
      {:label (str "Level " level " BitmapIndexedNode bitmap:\n" (binary-str #_Integer/toHexString bitmap)
                   "\nCapacity: " (Integer/bitCount bitmap) "/" (/ (alength array) 2))
       :shape :box
       :bitmap bitmap
       :children (let [;; map from number of 1's to the right of the
                       ;; current position, to the current position (0-based)
                       n-ones-mapping
                       (loop [s (map-indexed vector (reverse (binary-str bitmap)))
                              n1s 0
                              m {}]
                         (if (empty? s)
                           m
                           (let [[pos f] (first s)]
                             (if (= \1 f)
                               (recur (next s)
                                      (inc n1s)
                                      (assoc m n1s pos))
                               (recur (next s)
                                      n1s
                                      m)))))]


                   (vec
                     (->> (loop [ps (partition 2 array)
                                 seen-entries 0
                                 out []]
                            (if-let [[k v] (first ps)]
                              (let [level-str 
                                    (str "Hash bits " (* level 5)
                                         "-"
                                         (dec
                                           (if (= level 5)
                                             (+ 2 (* level 5))
                                             (* (inc level) 5)))
                                         ": ")
                                    next-out
                                    (if (nil? k)
                                      (when v
                                        (update (visualize* v (inc level))
                                                :label #(str level-str
                                                             (binary-str
                                                               (get n-ones-mapping seen-entries)
                                                               (if (= level 6)
                                                                 2
                                                                 5))
                                                             "\n" %)))
                                      {:label (str k "\n"
                                                   level-str
                                                   (binary-str
                                                     (map/mask (core/hash k)
                                                           (* level 5))
                                                     (if (= level 6)
                                                       2
                                                       5)))
                                       :shape :box
                                       :children [{:label (str v)}]})]
                                (recur (next ps)
                                       (if (and (nil? k)
                                                (nil? v))
                                         seen-entries
                                         (inc seen-entries))
                                       (conj out next-out)))
                              (rseq out)))
                          (remove nil?))))}))

  HashCollisionNode
  (visualize* [this level]
    (let [array (map/hash-collision-node-array this)]
      {:label (str "HashCollisionNode:" hash)
       :shape :box
       :children (vec
                   (keep (fn [[k v]]
                           (if (nil? k)
                             (when v
                               (visualize* v (inc level)))
                             {:label (str k ": " (core/hash k))
                              :shape :box
                              :children [{:label (str v)}]}))
                         (partition 2 array)))}))

  ArrayNode
  (visualize* [this level]
    (let [^objects array (.-array this)]
      {:label (str "Level " level " ArrayNode:\n"
                   "\n" count "/" (alength array))
       :shape :box
       :children (vec
                   (->> (map-indexed
                          (fn [n v]
                            (when v
                              (update (visualize* v (inc level))
                                      :label #(str "(" (binary-str n 5) ")\n" %))))
                          array)
                        (remove nil?)))}))

  PersistentHashMap
  (visualize* [this level]
    (let [root (.-root this)]
      {:label "PersistentHashMap"
       :children (when root
                   [(visualize* root 0)])})))

(defn visualize [^PersistentHashMap m]
  (let [data (visualize* m 0)]
    ;(spit "tree.svg"
    (viz/view-tree #_tree->svg (comp vector? :children) :children data
                   :node->descriptor (fn [n] 
                                       {:label (:label n)
                                        :shape (:shape n)}))
    ;)
    ))

;cat tree.dot  | dot -Gdpi=64 -Tpng:cairo:cairo > tree.png
(defn dot-to-disk [m dot-name]
  (let [data (visualize* m 0)]
    (spit (str dot-name ".dot")
    (dot/tree->dot (comp vector? :children) :children data
               :node->descriptor (fn [n] 
                                   {:label (:label n)
                                    :shape (:shape n)})))))

(comment
(run!
  (fn [k]
    (println k (binary-str (hash k))))
  [:a
   :b
   :c
   :d
   :e
   :f
   :g
   :h
   :i
   :j])
    (visualize* (create {}))
    (visualize (create {:a 1
                        :b 2}))
    (visualize (create {:d 1
                        :f 2}))
    (visualize (create (hash-map
                         :a 1
                         ;:b 2
                         :c 3
                         ;:d 4
                         ;:e 5
                         ;:f 6
                         :g 7
                         ;:h 8
                         ;:i 9
                         ;:j 10
                         )))
    (dot-to-disk (hash-map
                   :a 1
                   :b 2
                   :c 3
                   :d 4
                   :e 5
                   :f 6
                   :g 7
                   :h 8
                   :i 9
                   :j 10)
                 "example")
    (visualize (create {5 true
                        9 true}))
    (let [n 23] (create (zipmap (range n) (range n))))
    (let [n 23] (visualize (create (zipmap (range n) (range n)))))
    (let [n 24]
      (visualize (create (zipmap (range n) (range n)))))
    (let [n 100]
      (visualize (create (zipmap (range n) (range n)))))
  (visualize
    (create (zipmap (range 16) (range 16))))

  (visualize
    (-> (create {}) transient (assoc! -9 0) persistent! (assoc -10 0)))

  (use 'rhizome.viz)
  (def t '([1 2] ([3 4] ([5 6 7]))))
  (tree->svg (comp vector? :children) :children (visualize* (create {:a 1}))
             :node->descriptor (fn [n] {:label (:label n)}))

    )
