(ns com.ambrosebs.map.reader-macro
  (:refer-clojure :exclude [bit-shift-right
                            bit-shift-left
                            hash-map])
  (:require [clojure.core :as core]
            [com.ambrosebs.map :as map]))

;; from https://github.com/klutometis/reader-macros
(let [macros (.getDeclaredField clojure.lang.LispReader "macros")]
  (.setAccessible macros true)
  (let [^objects macros (.get macros nil)]
    (def set-macro-character
      (fn [character read]
        (aset macros (int character) read)))

    (def get-macro-character
      (fn [character]
        (aget macros (int character))))
    (defn swap-macro-character
      [character f & args]
      (set-macro-character
        character
        (apply f (get-macro-character character) args)))))

(defonce installed (atom false))
(defonce original-curly-paren-pattern 
  (get-macro-character \{))
(defonce original-hash-map 
  core/hash-map)

(defn install-map []
  (locking installed
    (when-not @installed
      (reset! installed true)
      (swap-macro-character \{ 
                            (fn [f]
                              (fn [& args]
                                (map/create (apply f args)))))
      (alter-var-root #'core/hash-map (constantly map/hash-map))
      nil)))

(defn uninstall-map []
  (locking installed
    (reset! installed false)
    (set-macro-character \{ original-curly-paren-pattern)
    (alter-var-root #'core/hash-map (constantly original-hash-map))
    nil))

