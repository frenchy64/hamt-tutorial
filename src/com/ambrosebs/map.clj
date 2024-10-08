;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns com.ambrosebs.map
  (:refer-clojure :exclude [bit-shift-right
                            bit-shift-left
                            hash-map
                            cond])
  (:require [clojure.core :as core])
  (:import (clojure.lang IPersistentMap MapEntry Box)
           (java.util.concurrent.atomic AtomicReference)))

(set! *warn-on-reflection* true)

(defmacro bit-shift-right [x n]
  `(clojure.lang.Numbers/unsignedShiftRightInt ~x ~n))

(defmacro bit-shift-left [x n]
  `(clojure.lang.Numbers/shiftLeftInt ~x ~n))

(defmacro identical-ints? [x y]
  `(com.ambrosebs.map.Util/identicalInts ~x ~y))

;; does not test keywords
(defmacro cond [& [c1 c2 :as clauses]]
  (assert clauses "Final clause must be keyword test")
  (when-not (next clauses)
    (throw (IllegalArgumentException.
             "cond requires an even number of forms")))
  (if (keyword? c1)
    c2
    (list 'if c1
          c2
          (let [nnclauses (next (next clauses))]
            (assert nnclauses "Final clause must be keyword test")
            (cons `cond nnclauses)))))


(defonce ^:private NOT-FOUND
  (Object.))

(definterface INode
  (assocNode
    ^com.ambrosebs.map.INode 
    [^int shift
     ^int hash
     key
     val
     ;; set to the node if this assoc adds an extra leaf
     ^clojure.lang.Box added-leaf
     ])
  (assocNode
    ^com.ambrosebs.map.INode
    [^java.util.concurrent.atomic.AtomicReference edit
     ^int shift
     ^int hash
     key
     val
     ^clojure.lang.Box added-leaf
     ])
  (withoutNode
    ^com.ambrosebs.map.INode
    [^int shift
     ^int hash
     key])
  (withoutNode
    ^com.ambrosebs.map.INode
    [^java.util.concurrent.atomic.AtomicReference edit
     ^int shift
     ^int hash
     key
     ^clojure.lang.Box removed-leaf])
  (findNode
    ^clojure.lang.IMapEntry
    [^int shift
     ^int hash
     key])
  (findNode
    [^int shift
     ^int hash
     key
     not-found])
  (^clojure.lang.ISeq
   nodeSeq [])
  (kvreduceNode
    [^clojure.lang.IFn f
     init])
  (foldNode
    [^clojure.lang.IFn combinef
     ^clojure.lang.IFn reducef
     ^clojure.lang.IFn fjtask
     ^clojure.lang.IFn fjfork
     ^clojure.lang.IFn fjjoin])
  (^java.util.Iterator 
    nodeIterator
    [^clojure.lang.IFn f]))

(definterface EnsureEditable
  (ensureEditable [edit])
  (ensureEditable [edit count array]))

(definterface EditAndSet
  (editAndSet [edit i a])
  (editAndSet [edit i a j b]))

(definterface EditAndRemovePair
  (editAndRemovePair [edit bit i]))

(defn bit-index [bitmap bit]
  #_
  {:pre [(int bitmap)
         (int bit)]}
  ;; count all bits below the most
  ;; significant `1`
  (Integer/bitCount
    (bit-and
      bitmap
      (unchecked-dec-int bit))))

(defn mask 
  "Return the `shift` least significant bits
  in hash."
  [hash shift]
  (-> (bit-shift-right hash shift)
      (bit-and 0x01f)))


(defn bitpos [hash shift]
  #_
  {:pre [(number? hash)
         (number? shift)]}
  (bit-shift-left 1 (mask hash shift)))

(declare ^INode EMPTY-BMIN)

(defn inode-array [size]
  (make-array
    com.ambrosebs.map.INode
    size))

(defn clone-and-set 
  ([^objects array i a]
   (doto (aclone array)
     (aset i a)))
  ([^objects array i a j b]
   (doto (aclone array)
     (aset i a)
     (aset j b))))

(declare hash-collision-node-ctor
         array-node-ctor)

(defn create-node 
  ([edit shift key1 val1 key2hash key2 val2]
   ;(prn "create-node")
   (let [key1hash (core/hash key1)]
     (cond
       (identical-ints? key1hash key2hash)
       (hash-collision-node-ctor
         nil
         key1hash
         2
         (object-array
           [key1 val1 key2 val2]))

       :else
       (let [added-leaf (Box. nil)]
         (-> EMPTY-BMIN
             ^com.ambrosebs.map.INode
             (.assocNode
               edit shift key1hash
               key1 val1 added-leaf)
             (.assocNode
               edit shift key2hash
               key2 val2 added-leaf))))))
  ([shift key1 val1 key2hash key2 val2]
   (let [key1hash (core/hash key1)]
     (cond
       (identical-ints? key1hash key2hash)
       (hash-collision-node-ctor
         nil
         key1hash
         2
         (object-array
           [key1 val1 key2 val2]))

       :else
       (let [added-leaf (Box. nil)
             edit (AtomicReference.)]
         (-> EMPTY-BMIN
             ^com.ambrosebs.map.INode
             (.assocNode
               edit shift key1hash
               key1 val1 added-leaf)
             (.assocNode
               edit shift key2hash
               key2 val2 added-leaf)))))))

(defn remove-pair [^objects array i]
  (let [i (int i)
        new-array (object-array (- (alength array) 2))
        _ (System/arraycopy array 0 new-array 0 (* 2 i))
        _ (System/arraycopy array (* 2 (inc i))
                            new-array
                            (* 2 i)
                            (- (alength new-array)
                               (* 2 i)))
        ]
    new-array))

(declare ^INode bitmap-indexed-node-ctor
         nodeSeq-create
         node-iter-ctor
         nodeSeq-kvreduce)

(definterface IBitmap
  (nodeBitmap []))

(deftype BitmapIndexedNode
  [^:volatile-mutable bitmap ;int
   ^objects array  ;nodes or nil
   ^java.util.concurrent.atomic.AtomicReference edit]
  java.io.Serializable

  IBitmap
  (nodeBitmap [this] bitmap)

  INode
  (assocNode
    [this shift hash key val added-leaf]
    #_
    {:pre [(number? shift)
           (number? hash)]}
    #_
    (prn "BitmapIndexedNode assocNode, no edit" 
         ;shift hash key val added-leaf
         )
    (let [^Box added-leaf added-leaf
          bit (bitpos hash shift)
          idx (bit-index bitmap bit)]
      #_
      (prn "bitmap BitmapIndexedNode assocNode - edit" (binary-str bitmap))
      (cond
        (not (zero? (bit-and bitmap bit)))
        (let [^INode key-or-null (aget array (* 2 idx))
              ^INode val-or-node (aget array (inc (* 2 idx)))]
          (cond
            (nil? key-or-null)
            (let [n (.assocNode val-or-node
                                 (+ shift 5)
                                 hash
                                 key
                                 val
                                 added-leaf)]
              (if (identical? n val-or-node)
                this
                (bitmap-indexed-node-ctor
                  nil
                  bitmap
                  (clone-and-set
                    array
                    (inc (* 2 idx))
                    n))))

            (= key key-or-null)
            (if (identical? val val-or-node)
              this
              (bitmap-indexed-node-ctor
                nil
                bitmap
                (clone-and-set
                  array
                  (inc (* 2 idx))
                  val)))

            :else
            (let [_ (set! (.val added-leaf) added-leaf)]
              (bitmap-indexed-node-ctor
                nil
                bitmap
                (clone-and-set
                  array
                  (* 2 idx)
                  nil
                  (inc (* 2 idx))
                  (create-node
                    (+ shift 5)
                    key-or-null
                    val-or-node
                    hash
                    key
                    val))))))

        :else
        (let [n (Integer/bitCount bitmap)]
          (cond
            (>= n 16)
            (let [^objects nodes (inode-array 32)
                  jdx (mask hash shift)
                  _ (aset nodes jdx
                          (.assocNode
                            EMPTY-BMIN
                            (+ shift 5)
                            hash
                            key
                            val
                            added-leaf))
                  _ (loop [i 0
                           j 0]
                      (when (< i 32)
                        (if (-> bitmap
                                  (bit-shift-right i)
                                  (bit-and 1)
                                  zero?
                                  not)
                          (do
                            (if (nil? (aget array j))
                              (aset nodes i (aget array (inc j)))
                              (aset nodes i
                                    (.assocNode 
                                      EMPTY-BMIN
                                      (+ shift 5)
                                      (core/hash
                                        (aget array j))
                                      (aget array j)
                                      (aget array (inc j))
                                      added-leaf)))
                            (recur (inc i)
                                   (+ j 2)))
                          (recur (inc i)
                                 j))))]
              (array-node-ctor
                nil
                (inc n)
                nodes))

            :else
            (let [new-array (object-array (* 2 (inc n)))
                  _ (System/arraycopy 
                      array
                      0
                      new-array
                      0
                      (* 2 idx))
                  _ (aset new-array (* 2 idx) key)
                  _ (set! (.val added-leaf)
                          added-leaf)
                  _ (aset new-array (inc (* 2 idx)) val)
                  _ (System/arraycopy
                      array
                      (* 2 idx)
                      new-array
                      (* 2 (inc idx))
                      (* 2 (- n idx)))]
              (bitmap-indexed-node-ctor
                nil
                (bit-or bitmap bit)
                new-array)))))))

  (withoutNode
    [this shift hash key]
    (let [bit (bitpos hash shift)]
      (cond
        (zero? (bit-and bitmap bit))
        this
        
        :else
        (let [idx (bit-index bitmap bit)
              ^INode key-or-null (aget array (* 2 idx))
              ^INode val-or-node (aget array (inc (* 2 idx)))]
          (cond
            (nil? key-or-null)
            (let [n (.withoutNode 
                      val-or-node
                      (+ shift 5)
                      hash
                      key)]
              (cond
                (identical? n val-or-node)
                this

                n
                (bitmap-indexed-node-ctor
                  nil
                  bitmap
                  (clone-and-set
                    array
                    (inc (* 2 idx))
                    n))

                (identical-ints? bitmap bit)
                nil

                :else
                (bitmap-indexed-node-ctor
                  nil
                  (bit-xor
                    bitmap
                    bit)
                  (remove-pair
                    array
                    idx))))

            (= key key-or-null)
            (if (identical-ints? bitmap bit)
              nil
              (bitmap-indexed-node-ctor
                nil
                (bit-xor
                  bitmap
                  bit)
                (remove-pair
                  array
                  idx)))

            :else this)))))
    (findNode 
      [this shift hash key]
      ;(prn "bitmap-indexed-node-ctor findNode")
      (let [not-found nil
            bit (bitpos hash shift)]
        (cond
          ;; there is no entry at the `bit` position,
          ;; since the associated bit in `bitmap` is zero.
          (zero? (bit-and bitmap bit))
          not-found

          :else
          (let [idx (bit-index bitmap bit)
                ^INode key-or-null (aget array (* 2 idx))
                ^INode val-or-node (aget array (inc (* 2 idx)))]
            (cond
              ;; if key is nil, this means there's a sub-trie
              ;; in the val position.
              ;; Now, recursively perform the lookup in the
              ;; sub-trie.
              (nil? key-or-null)
              (.findNode val-or-node
                          (+ shift 5)
                          hash key)

              (= key key-or-null)
              (MapEntry/create
                key-or-null
                val-or-node)

              :else
              not-found)))))
  (findNode
    [this shift hash key not-found]
    (let [bit (bitpos hash shift)]
      (cond
        (zero? (bit-and bitmap bit))
        not-found

        :else
        (let [idx (bit-index bitmap bit)
              ^INode key-or-null (aget array (* 2 idx))
              ^INode val-or-node (aget array (inc (* 2 idx)))]
          (cond
            (nil? key-or-null)
            (.findNode val-or-node
                        (+ shift 5)
                        hash
                        key
                        not-found)

            (= key key-or-null)
            val-or-node

            :else
            not-found)))))

  (nodeSeq [this]
    ;(prn "seq of BitmapIndexedNode")
    ;(pprint array)
    (nodeSeq-create array))

  (nodeIterator [this f]
    (node-iter-ctor array f))

  (kvreduceNode [this f init]
    (nodeSeq-kvreduce array f init))

  (foldNode [this 
              combinef reducef fjtask fjfork fjjoin]
    (nodeSeq-kvreduce
      array reducef (combinef)))

  EnsureEditable
  (ensureEditable [this edit]
    (if (identical? (.-edit this) edit)
      this
      (let [n (Integer/bitCount bitmap)
            new-array (object-array
                        ;; make room for next assoc
                        (if (>= n 0)
                          (* 2 (inc n))
                          4))
            _ (System/arraycopy
                array
                0
                new-array
                0
                (* 2 n))]
        (bitmap-indexed-node-ctor
          edit
          bitmap
          new-array))))

  EditAndSet
  (editAndSet [this edit i a]
    (let [^BitmapIndexedNode editable
          (.ensureEditable this edit)
          _ (aset ^objects (.-array editable) i a)]
      editable))

  (editAndSet [this edit i a j b]
    (let [^BitmapIndexedNode editable
          (.ensureEditable this edit)
          _ (aset ^objects (.-array editable) i a)
          _ (aset ^objects (.-array editable) j b)]
      editable))

  EditAndRemovePair
  (editAndRemovePair [this edit bit i]
    (if (identical-ints? bitmap bit)
      nil
      (let [^BitmapIndexedNode editable
            (.ensureEditable this edit)
            ;_ (prn "bitmap before" (binary-str (.-bitmap editable)))
            _ (set! (.-bitmap editable)
                    (bit-xor (.-bitmap editable)
                             bit))
            ;_ (prn "bitmap after" (binary-str (.-bitmap editable)))
            _ (System/arraycopy
                (.-array editable)
                (* 2 (inc i))
                (.-array editable)
                (* 2 i)
                (- (alength ^objects (.-array editable))
                   (* 2 (inc i))))
            _ (aset ^objects (.-array editable)
                    (- (alength ^objects (.-array editable))
                       2)
                    nil)
            _ (aset ^objects (.-array editable)
                    (- (alength ^objects (.-array editable))
                       1)
                    nil)]
        editable)))

  (assocNode [this edit shift hash key val added-leaf]
             #_
    {:pre [(number? shift)
           (number? hash)
           (instance? Box added-leaf)]}
    #_
    (assert (= hash (core/hash key)))
    #_
    (prn "BitmapIndexedNode assocNode, with edit" 
         ;shift hash key val added-leaf
         )
    (let [^Box added-leaf added-leaf
          bit (bitpos hash shift)
          idx (bit-index bitmap bit)]
      ;(prn "bit pos" bit)
      ;(prn "bit index" idx)
      #_
      (prn "bitmap BitmapIndexedNode assocNode + edit" 
           (binary-str bitmap)
           (binary-str bit)
           (bit-and bitmap bit)
           idx
           )
      (cond
        ;; we've found a match
        (not (zero? (bit-and bitmap bit)))
        (let [^INode key-or-null (aget array (* 2 idx))
              ^INode val-or-node (aget array (inc (* 2 idx)))]
          (cond
            (nil? key-or-null)
            (let [n (.assocNode val-or-node
                                 edit
                                 (+ shift 5)
                                 hash
                                 key
                                 val
                                 added-leaf)]
              (if (identical? n val-or-node)
                this
                (.editAndSet this
                               edit
                               (inc (* 2 idx))
                               n)))

            (= key key-or-null)
            (if (identical? val val-or-node)
              this
              (.editAndSet this
                             edit
                             (inc (* 2 idx))
                             val))

            :else
            (let [_ (set! (.-val added-leaf)
                          added-leaf)]
              (.editAndSet this
                             edit
                             (* 2 idx)
                             nil
                             (inc (* 2 idx))
                             (create-node
                               edit
                               (+ shift 5)
                               key-or-null
                               val-or-node
                               hash
                               key
                               val)))))
        :else
        (let [;_ (prn "else branch, bitCount")
              n (Integer/bitCount bitmap)]
          ;(prn "bitmap" (binary-str bitmap) bitmap)
          ;(prn "bit count" n)
          (cond
            (< (* n 2) (alength array))
            (let [;_ (prn "array big enough")
                  _ (set! (.-val added-leaf)
                          added-leaf)
                  ^BitmapIndexedNode
                  editable (.ensureEditable
                             this
                             edit)
                  _ (System/arraycopy
                      (.-array editable)
                      (* 2 idx)
                      (.-array editable)
                      (* 2 (inc idx))
                      (* 2 (- n idx)))
                  _ (aset ^objects (.-array editable)
                          (* 2 idx)
                          key)
                  _ (aset ^objects (.-array editable)
                          (inc (* 2 idx))
                          val)
                  ;_ (prn "bitmap before assocNode" 
                  ;       (binary-str (.-bitmap editable))
                  ;       (seq (.-array editable)))
                  _ (set! (.-bitmap editable)
                          (bit-xor (.-bitmap editable)
                                   bit))]
                  ;(prn "bitmap after assocNode" 
                  ;     (binary-str (.-bitmap editable))
                  ;     (seq (.-array editable)))
              editable)

            (>= n 16)
            (let [;_ (prn "n >= 16")
                  ^objects
                  nodes (inode-array 32)
                  jdx (mask hash shift)
                  _ (aset nodes jdx
                          (.assocNode 
                            EMPTY-BMIN
                            edit
                            (+ shift 5)
                            hash
                            key 
                            val
                            added-leaf))
                  _ (loop [i 0
                           j 0]
                      (when (< i 32)
                        (cond
                          (-> bitmap
                              (bit-shift-right i)
                              (bit-and 1)
                              zero?
                              not)
                          (let [_ (if (nil? (aget array j))
                                    (aset nodes i (aget array (inc j)))
                                    (aset nodes i
                                          (.assocNode
                                            EMPTY-BMIN
                                            edit
                                            (+ shift 5)
                                            (core/hash
                                              (aget array j))
                                            (aget array j)
                                            (aget array (inc j))
                                            added-leaf)))]
                            (recur (inc i)
                                   (+ j 2)))

                          :else
                          (recur (inc i)
                                 j))))]
              (array-node-ctor
                edit
                (inc n)
                nodes))

            :else
            (let [new-array (let [size (* 2 (+ n 4))]
                              ;(prn "create new array" size)
                              (object-array size))
                  _ (System/arraycopy
                      array
                      0 
                      new-array
                      0
                      (* 2 idx))
                  ;_ (print "new array, first half")
                  ;_ (pprint new-array)
                  _ (aset new-array (* 2 idx) key)
                  _ (set! (.-val added-leaf) added-leaf)
                  _ (aset new-array (inc (* 2 idx)) val)
                  _ (System/arraycopy
                      array
                      (* 2 idx)
                      new-array
                      (* 2 (inc idx))
                      (* 2 (- n idx)))
                  ;_ (print "new array, second half")
                  ;_ (pprint new-array)
                  ^BitmapIndexedNode
                  editable (.ensureEditable
                             this edit)
                  _ (set! (.-array editable) new-array)
                  _ (set! (.-bitmap editable)
                          (bit-or (.-bitmap editable)
                                  bit))]
              editable))))))

  (withoutNode [this edit shift hash key removed-leaf]
    (let [^Box removed-leaf removed-leaf
          bit (bitpos hash shift)]
      (cond
        (zero? (bit-and bitmap bit))
        this

        :else
        (let [idx (bit-index bitmap bit)
              ^INode key-or-null (aget array (* 2 idx))
              ^INode val-or-node (aget array (inc (* 2 idx)))]
          (cond
            (nil? key-or-null)
            (let [n (.withoutNode
                      val-or-node
                      edit
                      (+ shift 5)
                      hash
                      key
                      removed-leaf)]
              (cond
                (identical? n val-or-node)
                this

                n
                (.editAndSet
                  this
                  edit
                  (inc (* 2 idx))
                  n)

                (identical-ints? bitmap bit)
                nil

                :else
                (.editAndRemovePair
                  this
                  edit
                  bit
                  idx)))

            (= key key-or-null)
            (let [_ (set! (.-val removed-leaf) removed-leaf)]
              ;; https://clojure.atlassian.net/browse/CLJ-2297?focusedCommentId=13252
              ;; Note that this patch doesn't have a similar change for the transient overload of `without`;
              ;; because it relies on a helper method `editAndRemovePair` that correctly handles the empty case, that method doesn't have this bug.
              ;; - Ben Bader
              ;; TODO: collapse  - rhickey
              (.editAndRemovePair
                this edit bit idx))

            :else
            this))))))

(defn find-index [^objects array count key]
  (loop [i 0]
    (if (< i (* 2 count))
      -1
      (if (= key (aget array i))
        i
        (recur (+ 2 i))))))

(definterface IHashCollisionNode
  (hashCollisionNodeArray ;^objects
                          []))

(deftype HashCollisionNode
  [hash ;int
   ^:volatile-mutable count  ;int
   ^:volatile-mutable ^objects array ;Object[]
   edit ; AtomicReference<Thread>
   ]
  IHashCollisionNode
  (hashCollisionNodeArray [this] array)

  INode
  (assocNode
    [this shift hash key val added-leaf]
    (let [^Box added-leaf added-leaf]
      ;(prn "HashCollisionNode assocNode")
      (if (identical-ints? hash (.-hash this))
        (let [idx (find-index array count key)]
          (cond
            (not (identical-ints? -1 idx))
            (if (identical? val (aget array (inc idx)))
              this
              (hash-collision-node-ctor
                nil
                hash
                count
                (clone-and-set
                  array
                  (inc idx)
                  val)))

            :else
            (let [new-array (object-array (* 2 (inc count)))
                  _ (System/arraycopy
                      array 0 new-array 0 (* 2 count))
                  _ (aset new-array (* 2 count) key)
                  _ (aset new-array (inc (* 2 count)) val)
                  _ (set! (.-val added-leaf) added-leaf)]
              (hash-collision-node-ctor
                edit
                hash
                (inc count)
                new-array))))
        ;; nest it in a bitmap node
        (->
          (bitmap-indexed-node-ctor
            nil
            (bitpos (.-hash this) shift)
            (object-array [nil this]))
          (.assocNode
            shift
            hash
            key
            val
            added-leaf)))))

  (withoutNode [this shift hash key]
    (let [idx (find-index array count key)]
      (cond
        (identical-ints? -1 idx)
        this

        (identical-ints? 1 count)
        nil

        :else
        (hash-collision-node-ctor
          nil
          hash
          (dec count)
          (remove-pair
            array
            (/ idx 2))))))

  (findNode [this shift hash key]
    (let [idx (find-index array count key)]
      (if (< idx 0)
        nil
        (MapEntry/create (aget array idx)
                         (aget array (inc idx))))))

  (findNode [this shift hash key not-found]
    (let [idx (find-index array count key)]
      (if (< idx 0)
        not-found
        (MapEntry/create (aget array idx)
                         (aget array (inc idx))))))

  (nodeSeq [this]
    (nodeSeq-create array))

  (nodeIterator [this f]
    (node-iter-ctor array f))

  (kvreduceNode [this f init]
    (nodeSeq-kvreduce array f init))

  (foldNode [this 
              combinef reducef fjtask fjfork fjjoin]
    (nodeSeq-kvreduce
      array reducef (combinef)))

  EnsureEditable
  (ensureEditable [this edit]
    (if (identical? (.-edit this) edit)
      this
      (let [;; make room for next assoc
            new-array (object-array (* 2 (inc count)))
            _ (System/arraycopy
                array 0 new-array 0 (* 2 count))]
        (hash-collision-node-ctor
          edit hash count new-array))))

  (ensureEditable [this edit count array]
    (if (identical? (.-edit this) edit)
      (let [_ (set! (.-array this) array)
            _ (set! (.-count this) count)]
        this)
      (hash-collision-node-ctor
        edit hash count array)))

  EditAndSet
  (editAndSet [this edit i a]
    (let [^HashCollisionNode editable
          (.ensureEditable this edit)
          _ (aset ^objects (.-array editable) i a)]
      editable))

  (editAndSet [this edit i a j b]
    (let [^HashCollisionNode editable
          (.ensureEditable this edit)
          _ (aset ^objects (.-array editable) i a)
          _ (aset ^objects (.-array editable) j b)]
      editable))

  (assocNode [this edit shift hash key val added-leaf]
    (let [^Box added-leaf added-leaf]
      (if (identical-ints? hash (.-hash this))
        (let [idx (find-index array count key)]
          (cond
            (not (identical-ints? idx -1))
            (if (identical? (aget array (inc idx))
                            val)
              this
              (.editAndSet 
                this edit (inc idx) val))

            (> (alength array) (* 2 count))
            (let [_ (set! (.-val added-leaf) added-leaf)
                  ^HashCollisionNode
                  editable (.editAndSet
                             this
                             edit
                             (* 2 count)
                             key
                             (inc (* 2 count))
                             val)
                  _ (set! (.-count editable)
                          (inc (.-count editable)))]
              editable)

            :else
            (let [new-array (object-array (+ (alength array) 2))
                  _ (System/arraycopy
                      array
                      0
                      new-array
                      0
                      (alength array))
                  _ (aset new-array (alength array) key)
                  _ (aset new-array (inc (alength array)) val)
                  _ (set! (.-val added-leaf) added-leaf)]
              (.ensureEditable
                this
                edit
                (inc count)
                new-array))))
        ;; nest it in a bitmap node
        (->
          (bitmap-indexed-node-ctor
            edit
            (bitpos (.-hash this) shift)
            (object-array [nil this nil nil]))
          (.assocNode
            edit shift hash key val added-leaf)))))

  (withoutNode [this edit shift hash key removed-leaf]
    (let [^Box removed-leaf removed-leaf
          idx (find-index array count key)]
      (if (identical-ints? -1 idx)
        this
        (let [_ (set! (.-val removed-leaf) removed-leaf)]
          (if (identical-ints? 1 count)
            nil
            (let [^HashCollisionNode
                  editable
                  (.ensureEditable
                    this edit)
                  _ (aset ^objects (.-array editable) idx
                          (aget ^objects (.-array editable)
                                (- (* 2 count) 2)))
                  _ (aset ^objects (.-array editable) (inc idx)
                          (aget ^objects (.-array editable)
                                (- (* 2 count) 1)))
                  _ (aset ^objects (.-array editable) (- (* 2 count) 2)
                          nil)
                  _ (aset ^objects (.-array editable) (- (* 2 count) 1)
                          nil)
                  _ (set! (.-count editable)
                          (dec (.-count editable)))]
              editable)))))))

(defn nodeSeq-ctor 
  ([array i] (nodeSeq-ctor nil array i nil))
  ([meta ^objects array i s]
   #_
   {:pre [(or (nil? meta)
              (map? meta))
          (integer? i)
          (or (nil? s)
              (seq? s))]}
   (proxy [clojure.lang.ASeq] [meta]
     (withMeta [new-meta]
       (if (identical? meta new-meta)
         this
         (nodeSeq-ctor new-meta array i s)))
     (first []
       ;(pprint array)
       ;(prn i)
       (if s
         (first s)
         (MapEntry/create (aget array i)
                          (aget array (inc i)))))
     (next []
       (if s
         (nodeSeq-create array i (next s))
         (nodeSeq-create array (+ i 2) nil))))))

(defn nodeSeq-create 
  ([array] (nodeSeq-create array 0 nil))
  ([^objects array i s]
   #_
   {:pre [(integer? i)
          (or (nil? s)
              (seq? s))]}
   #_
   (assert
     (every? (fn [[k v]]
               (if (nil? k)
                 (or (nil? v)
                     (instance? INode v))
                 true))
             (partition 2 array))
     (pr-str array))
   (if s
     (nodeSeq-ctor nil array i s)
     (loop [j i]
       (when (< j (alength array))
         (if (aget array j)
           (nodeSeq-ctor nil array j nil)
           (let [^INode node (aget array (inc j))]
             ;(prn "node" node)
             (or
               (when node
                 (let [nodeSeq (.nodeSeq node)]
                   (when nodeSeq
                     (nodeSeq-ctor 
                       nil array (+ j 2) nodeSeq))))
               (recur (+ j 2))))))))))

(defn nodeSeq-kvreduce [^objects array f init]
  (loop [i 0
         init init]
    (if (< i (alength array))
      (if (aget array i)
        (let [init (f init (aget array i) (aget array (inc i)))]
          (if (reduced? init)
            init
            (recur (+ i 2) init)))
        (let [^INode node (aget array (inc i))
              init (if node
                     (.kvreduceNode node f init)
                     init)]
          (if (reduced? init)
            init
            (recur (+ i 2) init))))
      init)))

(defn hash-collision-node-ctor
  [edit hash count array]
  #_
  (assert (= (class array)
             (class (object-array 0))))
  ;(prn "hash-collision-node-ctor")
  (new HashCollisionNode
       hash
       count
       array
       edit))

(defn array? [a]
  (.isArray (class a)))

(defn ^BitmapIndexedNode bitmap-indexed-node-ctor
  [edit bitmap array]
  #_
  {:pre [(or (nil? edit)
             (instance? AtomicReference edit))
         (integer? bitmap)
         (array? array)]}
  #_
  (prn "bitmap-indexed-node-ctor" 
       edit
       bitmap
       (Integer/toString
         bitmap
         2))
  ;(pprint array)
  (new BitmapIndexedNode
       bitmap
       array
       edit))

(defn pack [^objects array count edit idx]
  (let [bitmap (volatile! 0)]
    (let [new-array (object-array (* 2 (inc count)))
          _ (let [j (volatile! 1)]
              (dotimes [i idx]
                (when (aget array i)
                  (let [_ (aset new-array @j (aget array i))
                        _ (vswap! bitmap 
                                  bit-xor 
                                  (bit-shift-left
                                    1
                                    i))
                        _ (vswap! j + 2)])))
              (loop [i (inc idx)]
                (when (< i (alength array))
                  (when (aget array i)
                    (let [_ (aset new-array @j (aget array i))
                          _ (vswap! bitmap bit-xor
                                    (bit-shift-left
                                      1
                                      i))
                          _ (vswap! j + 2)]))
                  (recur (inc i)))))]
      (bitmap-indexed-node-ctor
        edit @bitmap new-array))))

(declare array-node-iter-ctor
         array-nodeSeq-create
         fold-tasks)

(deftype ArrayNode [^int count
                    ^objects array
                    ^AtomicReference edit]
  INode
  (assocNode [this shift hash key val added-leaf]
    ;(prn "ArrayNode assocNode")
    (let [idx (mask hash shift)
          ^INode node (aget array idx)]
      (cond
        (nil? node)
        (array-node-ctor
          nil
          (inc count)
          (clone-and-set array idx
                         (.assocNode EMPTY-BMIN
                                      (+ shift 5)
                                      hash
                                      key
                                      val
                                      added-leaf)))

        :else
        (let [n (.assocNode node
                            (+ shift 5)
                            hash
                            key
                            val
                            added-leaf)]
          (if (identical? n node)
            this
            (array-node-ctor
              nil
              count
              (clone-and-set
                array idx n)))))))

  (withoutNode [this shift hash key]
    (let [idx (mask hash shift)
          ^INode node (aget array idx)]
      (if (nil? node)
        this
        (let [n (.withoutNode
                  node
                  (+ shift 5)
                  hash
                  key)]
          (cond
            (identical? n node)
            this

            (nil? n)
            (if (<= count 8) ;; shrink
              (pack this nil idx)
              (array-node-ctor
                nil
                (dec count)
                (clone-and-set
                  array
                  idx
                  n)))

            :else
            (array-node-ctor
              nil
              count
              (clone-and-set array idx n)))))))

  (findNode [this shift hash key]
    (let [idx (mask hash shift)
          ^INode node (aget array idx)]
      (if (nil? node)
        nil
        (.findNode
          node
          (+ shift 5)
          hash
          key))))

  (findNode [this shift hash key not-found]
    (let [idx (mask hash shift)
          ^INode node (aget array idx)]
      (if (nil? node)
        not-found
        (.findNode
          node
          (+ shift 5)
          hash
          key
          not-found))))

  (nodeSeq [this]
    (array-nodeSeq-create array))

  (nodeIterator [this f]
    (array-node-iter-ctor array f))

  (kvreduceNode [this f init]
    ;;TODO fuse?
    (run! identity
          (map (fn [^INode node]
                 (when node
                   (let [init (.kvreduceNode
                                node f init)]
                     (when (reduced? init)
                       init))))
               array))
    init)

  (foldNode [this 
              combinef reducef fjtask fjfork fjjoin]
    (let [tasks
          (filterv
            (fn [^INode node]
              (when node
                (fn []
                  (.foldNode
                    node
                    combinef reducef fjtask fjfork fjjoin))
                  ))
            array)]
      (fold-tasks tasks 
          combinef reducef fjtask fjfork fjjoin)))

  EnsureEditable
  (ensureEditable [^ArrayNode this edit]
    (if (identical? (.-edit this) edit)
      this
      (array-node-ctor
        edit count (aclone ^objects (.-array this)))))

  EditAndSet
  (editAndSet [this edit i n]
    (let [^ArrayNode
          editable (.ensureEditable this edit)
          _ (aset ^objects (.-array editable) i n)]
      editable))

  (assocNode [this edit shift hash key val added-leaf]
    (let [idx (mask hash shift)
          ^INode
          node (aget array idx)]
      (cond
        (nil? node)
        (let [^ArrayNode
              editable (.editAndSet
                         this edit idx
                         (.assocNode
                           EMPTY-BMIN
                           edit
                           (+ shift 5)
                           hash
                           key
                           val
                           added-leaf))
              _ (set! (.-count editable) 
                      (inc (.-count editable)))]
          editable)

        :else
        (let [n (.assocNode
                  node
                  edit
                  (+ shift 5)
                  hash
                  key
                  val
                  added-leaf)]
          (if (identical? n node)
            this
            (.editAndSet
              this
              edit
              idx
              n))))))

  (withoutNode [this edit shift hash key removed-leaf]
    (let [idx (mask hash shift)
          ^INode node (aget array idx)]
      (if (nil? node)
        this
        (let [n (.withoutNode
                  node
                  edit
                  (+ shift 5)
                  hash
                  key
                  removed-leaf)]
          (cond
            (identical? n node)
            this

            (nil? n)
            (if (<= count 8) ;; shrink
              (pack array count edit idx)
              (let [^ArrayNode
                    editable (.editAndSet
                               this
                               edit
                               idx
                               n)
                    _ (set! (.-count editable)
                            (dec (.-count editable)))]
                editable))

            :else
            (.editAndSet this edit idx n))))))
  )

(declare array-nodeSeq-create)

(defn array-nodeSeq-ctor [meta nodes i s]
  (proxy [clojure.lang.ASeq] [meta]
    (withMeta [new-meta]
      (if (identical? new-meta meta)
        this
        (array-nodeSeq-ctor
          meta nodes i s)))
    (first []
      (first s))
    (next []
      (array-nodeSeq-create nil nodes i (next s)))))

(defn array-nodeSeq-create 
  ([nodes] (array-nodeSeq-create
             nil nodes 0 nil))
  ([meta ^objects nodes i s]
   (if s
     (array-nodeSeq-ctor meta nodes i s)
     (loop [j i]
       (when (< j (alength nodes)) 
         (cond
           (aget nodes j)
           (if-let [ns (.nodeSeq ^INode (aget nodes j))]
             (array-nodeSeq-create meta nodes (inc j) ns)
             (recur (inc j)))

           :else (recur (inc j))))))))

(defn array-node-ctor [edit 
                       ;^int
                       count array]
  (new ArrayNode count array edit))

(defonce NULL (Object.))

(definterface INodeIter
  (advance ^boolean []))

(deftype NodeIter [^objects array
                   f
                   ^:unsynchronized-mutable i
                   ^:unsynchronized-mutable next-entry
                   ^java.util.Iterator next-iter]

  INodeIter
  (advance [this]
    (loop []
      (if (< i (alength array))
        (let [key (aget array i)
              node-or-val (aget array (inc i))
              _ (set! (.i this)
                      (+ 2 (.i this)))]
          (cond
            (some? key)
            (let [_ (set! (.next-entry this)
                          (f key node-or-val))]
              true)

            (some? node-or-val)
            (let [^java.util.Iterator
                  iter (.nodeIterator
                         ^INode node-or-val f)]
              (if (and (some? iter)
                       (.hasNext iter))
                (do (set! (.next-iter this) iter)
                    true)
                (recur)))
            :else (recur)))

        false)))

  java.util.Iterator
  (hasNext [this]
    (if (or (not (identical? next-entry
                             NULL))
            (some? next-iter))
      true
      (.advance this)))

  (next [this]
    (let [ret next-entry]
      (cond
        (not (identical? ret NULL))
        (do (set! (.next-entry this) NULL)
            ret)

        (some? next-iter)
        (let [ret (.next next-iter)
              _
              (when (not (.hasNext next-iter))
                (set! (.next-iter this) nil))
              ]
          ret)

        (.advance this)
        (.next this)

        :else
        (throw (java.util.NoSuchElementException.))))))


(defn node-iter-ctor [array f]
  (new NodeIter array f
       0
       NULL
       nil))

(deftype ArrayNodeIter [^objects array f ^:volatile-mutable i 
                        ^:volatile-mutable 
                        ^java.util.Iterator
                        nested-iter]
  java.util.Iterator
  (hasNext [this]
    (loop []
      (let [res (when nested-iter
                  (if (.hasNext nested-iter)
                    true
                    (set! nested-iter nil)))]
        (if (boolean? res)
          res
          (if (< i (alength array))
            (let [^INode node (aget array (inc i))]
              (when node
                (set! nested-iter (.nodeIterator node f)))
              (recur))
            false)))))

  (next [this]
    (if (.hasNext this)
      (.next nested-iter)
      (throw (java.util.NoSuchElementException.)))))

(defn array-node-iter-ctor [array f]
  (new ArrayNodeIter array f 0 nil))

(defn fold-tasks [tasks combinef reducef fjtask fjfork fjjoin]
  (cond
    (empty? tasks)
    (combinef)

    (identical-ints? 1 (count tasks))
    ((nth tasks 0))

    :else
    (let [t1 (subvec tasks 0 (/ (count tasks) 2))
          t2 (subvec (/ (count tasks) 2))
          forked (fjfork (fjtask
                           (fn []
                             (fold-tasks
                               t2
                               combinef reducef fjtask 
                               fjfork fjjoin))))]
      (combinef (fold-tasks
                  t1
                  combinef reducef fjtask 
                  fjfork 
                  fjjoin)
                (fjjoin forked)))))

(def ^INode EMPTY-BMIN
  (bitmap-indexed-node-ctor
    nil
    0
    (object-array 0)))

(declare hash-map-ctor
         phm-iterator
         EMPTY
         transient-hash-map)

(def ^java.util.Iterator EMPTY-ITER 
  (reify java.util.Iterator
    (hasNext [this] false)
    (next [this] (throw (java.util.NoSuchElementException.)))))

(deftype PersistentHashMap
  [^int count
   ^com.ambrosebs.map.INode root
   has-null
   null-value
   ;; must be fully qualified tag! Compiler.java makes strange assumptions
   ^clojure.lang.IPersistentMap _meta
   ^:unsynchronized-mutable _hash
   ^:unsynchronized-mutable _hasheq]

  clojure.lang.IFn
  (invoke [coll k]
    (get coll k))

  (invoke [coll k not-found]
    (get coll k not-found))

  java.util.Map
  (size [this] (.count this))
  (containsKey [_ key]
    (if (nil? key)
      has-null
      (if root
        (not
          (identical?
            NOT-FOUND
            (.findNode root 
                        0
                        (core/hash key)
                        key
                        NOT-FOUND)))
        false)))

  clojure.lang.Associative
  (entryAt [_ key]
    (if (nil? key)
      (if has-null
        (MapEntry/create nil null-value)
        nil)
      (if root
        (.findNode root 0 (core/hash key) key)
        nil)))

  (assoc [this key val]
    (let [ret
          (when (nil? key)
            (if (and has-null
                     (identical? val null-value))
              this
              (hash-map-ctor
                (.meta this)
                (if has-null
                  count
                  (inc count))
                root
                true
                val)))]
      (if ret
        ret
        (let [added-leaf (Box. nil)
              newroot 
              (.assocNode
                (if root
                  root
                  ^INode EMPTY-BMIN)
                0
                (core/hash key)
                key
                val
                added-leaf)]
          (if (identical? newroot root)
            this
            (hash-map-ctor
              (.meta this)
              (if (nil? (.val added-leaf))
                count
                (inc count))
              newroot
              has-null
              null-value))))))
  (valAt [this key not-found]
    (if (nil? key)
      (if has-null
        null-value
        not-found)
      (if-not (nil? root)
        (.findNode root
                    0
                    (core/hash key)
                    key
                    not-found)
        not-found)))
  (valAt [this key]
    (.valAt this key nil))

  clojure.lang.IPersistentMap
  (assocEx [this key val]
    (if (.containsKey this key)
      (throw (clojure.lang.Util/runtimeException 
               "Key already present"))
      (.assoc this key val)))

  (without [this key]
    (cond
      (nil? key)
      (if has-null
        (hash-map-ctor
          (.meta this)
          (dec count)
          root
          false
          nil)
        this)

      (nil? root)
      this

      :else
      (let [new-root (.withoutNode root 0 (core/hash key) key)]
        (if (identical? new-root root)
          this
          (hash-map-ctor
            (.meta this)
            (dec count)
            new-root
            has-null
            null-value)))))

  (get [this key] (.valAt this key))

  (iterator [this]
    (phm-iterator
      this
      (fn [key val]
        (MapEntry/create key val))))

  (cons [this o]
    (cond
      (instance? java.util.Map$Entry o)
      (let [e o]
        (assoc this (key e) (val e)))

      (vector? o)
      (let [v o]
        (when (not= 2 (count v))
          (throw (new IllegalArgumentException "Vector arg to map conj must be a pair")))
        (assoc this (nth v 0) (nth v 1)))

      :else
      (loop [ret this
             es (seq o)]
        (if (nil? es)
          ret
          (let [e (first es)]
            (recur (assoc ret (key e) (val e))
                   (next es)))))))

  clojure.lang.MapEquivalence

  (equiv [this obj]
    (cond
      (not (instance? java.util.Map obj ))
      false

      (and (instance? IPersistentMap obj)
           (not (instance? 
                  clojure.lang.MapEquivalence 
                  obj)))
      false

      :else
      (let [m obj]
        (cond
          (not (= (core/count m) (core/count this)))
          false

          :else
          (loop [s (seq this)]
            (if s
              (let [e (first s)
                    found (contains? m (key e))]
                (if (or (not found)
                        (not (= (val e)
                                (get m (key e)))))
                  false
                  (recur (next s))))
              true))))))

  (equals [this obj]
    (clojure.lang.APersistentMap/mapEquals this obj))

  (hashCode [this]
    (if (= -1 _hash)
      (set! (.-_hash this) 
            (clojure.lang.APersistentMap/mapHash this))
      _hash))

  clojure.lang.IHashEq
  (hasheq [this]
    (if (= -1 _hasheq)
      (set! (.-_hasheq this) 
            (clojure.lang.Murmur3/hashUnordered this))
      _hasheq))

  clojure.lang.IMapIterable
  (keyIterator [this]
    (phm-iterator
      this
      (fn [key val]
        key)))

  (valIterator [this]
    (phm-iterator
      this
      (fn [key val]
        val)))
  
  clojure.lang.IKVReduce
  (kvreduce [this f init]
    (let [init (if has-null
                 (f init nil null-value)
                 init)]
      (cond
        (reduced? init)
        @init

        root
        (let [init (.kvreduceNode root f init)]
          (if (reduced? init)
            @init
            init))

        :else
        init)))

  #_ ;; FIXME: which interface provides fold?
  (fold [this n combinef reducef jfinvoke fjtask fjfork fjjoin]
    ;; we are ignoring n for now - rhickey
    (let [top (reify Callable
                (call [this]
                  (let [ret (combinef)
                        ret (if root
                              (combinef ret
                                        (.fold root
                                               combinef
                                               reducef
                                               fjtask
                                               fjfork
                                               fjjoin))
                              ret)]
                    (if has-null
                      (combinef ret
                                (reducef (combinef))
                                nil
                                null-value)
                      ret))))]

      (fjinvoke top)))

  (count [this] count)

  (seq [this]
    ;(prn "seq of PHM")
    ;(pprint root)
    (let [s (when root
              (.nodeSeq root))]
      (if has-null
        (new clojure.lang.Cons
             (MapEntry/create nil null-value)
             s)
        s)))

  (empty [this]
    (.withMeta ^clojure.lang.IObj EMPTY (.meta this)))


  clojure.lang.IObj
  (withMeta [this meta]
    (if (identical? _meta meta)
      this
      (hash-map-ctor
        meta
        count
        root
        has-null
        null-value)))

  clojure.lang.IEditableCollection
  (asTransient [this]
    (transient-hash-map this))

  (meta [this] _meta))


(defn phm-iterator [^PersistentHashMap m f]
  (let [^INode root (.-root m)
        ^java.util.Iterator
        root-iter (if (nil? root)
                    EMPTY-ITER
                    (.nodeIterator root f))]
    (if (.-has-null m)
      (let [seen (volatile! false)]
        (reify java.util.Iterator
          (hasNext [this]
            (if-not @seen
              true
              (.hasNext root-iter)))
          (next [this]
            (if-not @seen
              (let [_ (vreset! seen true)]
                (f nil (.-null-value m)))
              (.next root-iter)))))
      root-iter)))

(defn transient-hash-map 
  ([^PersistentHashMap m]
   (transient-hash-map (new AtomicReference
                            (Thread/currentThread))
                       (.-root m)
                       (.-count m)
                       (.-has-null m)
                       (.-null-value m)))
  ([^AtomicReference edit root count has-null null-value]
   (let [leaf-flag (Box. nil)]
     (let [root (volatile! root)
           count (volatile! count)
           has-null (volatile! has-null)
           null-value (volatile! null-value)]
       (letfn [(ensureEditable []
                 (when (nil? (.get ^AtomicReference edit))
                   (throw (new IllegalAccessError 
                               "Transient used after persistent! call"))))
               (doAssoc [this key val]
                 ;(prn "transient-hash-map doAssoc" key val)
                 (if (nil? key)
                   (let [_ (when-not (identical? null-value val)
                             (vreset! null-value val))
                         _ (when (not @has-null)
                             (vswap! count inc)
                             (vreset! has-null true))]
                     this)
                   (let [_ (set! (.-val leaf-flag) nil)
                         ^INode old-root (or @root EMPTY-BMIN)
                         ;_ (prn "before transient assocNode"
                         ;       (class old-root))
                         ;_ (ppnode old-root)
                         n (.assocNode
                             old-root
                             edit
                             0
                             (core/hash key)
                             key
                             val
                             leaf-flag)
                         ;_ (prn "after transient assocNode")
                         ;_ (ppnode n)
                         _ (when-not (identical? n @root)
                             (vreset! root n))
                         _ (when (some? (.-val leaf-flag))
                             (vswap! count inc))]
                     ;(ppnode @root)
                     this)))

               (doWithout [this key]
                 (cond
                   (nil? key)
                   (if-not @has-null
                     this
                     (let [_ (vreset! has-null false)
                           _ (vreset! null-value nil)
                           _ (vswap! count dec)]
                       this))

                   (nil? @root)
                   this

                   :else
                   (let [_ (set! (.-val leaf-flag) nil)
                         ^INode root-now @root
                         n (.withoutNode
                             root-now
                             edit
                             0
                             (core/hash key)
                             key
                             leaf-flag)
                         _ (when-not (identical? n root-now)
                             (vreset! root n))
                         _ (when-not (nil? (.-val leaf-flag))
                             (vswap! count dec))]
                     this)))

               (doPersistent [this]
                 (let [_ (.set edit nil)]
                   (hash-map-ctor
                     @count
                     @root
                     @has-null
                     @null-value)))
               (doValAt [key not-found]
                 (cond 
                   (nil? key)
                   (if @has-null
                     @null-value
                     not-found)

                   (nil? @root)
                   not-found

                   :else
                   (let [^INode root-now @root]
                     (.findNode root-now 0 (core/hash key) key not-found))))
               (doCount []
                 @count)]
         (proxy [clojure.lang.AFn
                 clojure.lang.ITransientMap] []
           (conj [o]
             (ensureEditable)
             ;(prn "conj transient-hash-map" o)
             (cond
               (instance? java.util.Map$Entry o)
               (assoc! this (key o) (val o))

               (vector? o)
               (let [_ (when-not (= 2 (count o))
                         (throw (IllegalArgumentException.
                                  "Vector arg to map conj must be a pair")))]
                 (assoc! this (nth o 0) (nth o 1)))

               :else
               (let [ret this]
                 (reduce (fn [this a]
                           (assoc! this (key a) (val a)))
                         this
                         o))))

           (invoke 
             ([arg1]
              (get this arg1))
             ([arg1 not-found]
              (get this arg1 not-found)))

           (valAt 
             ([key]
              (get this key nil))
             ([key not-found]
              (ensureEditable)
              (doValAt key not-found)))
          
           (assoc [key val]
             (ensureEditable)
             (doAssoc this key val))

           (without [key]
             (ensureEditable)
             (doWithout this key))

           (persistent []
             (ensureEditable)
             (doPersistent this))

           (count []
             (ensureEditable)
             (doCount))))))))

(defn hash-map-ctor 
  ([count root has-null null-value]
   (hash-map-ctor nil count root has-null null-value))
  ([mta count root has-null null-value]
   #_
   {:pre [(or (nil? mta)
              (map? mta))
          (integer? count)
          (or (nil? root)
              (instance? INode root))
          (boolean? has-null)]}
   (PersistentHashMap. 
     count root has-null null-value mta
     -1 -1)))

(def EMPTY (hash-map-ctor 0 nil false nil))

(defn create-with-check
  [other]
  (cond
    (seq? other)
    (loop [i 0
           ret (transient EMPTY)
           items other]
      (if (seq items)
        (let [fst (first items)
              nitems (next items)
              _ (when-not nitems
                  (throw (IllegalArgumentException.
                           (str "No values supplied for key: "
                                fst))))
              snd (first nitems)
              nnitems (next nitems)
              ret (assoc! ret fst snd)]
          (when-not (= (count ret) (inc i))
            (throw (IllegalArgumentException.
                     (str "Duplicate key: " fst))))
          (recur (inc i) ret nnitems))
        (persistent! ret)))

    (instance? (class (object-array 0)) other)
    (let [^objects init other]
      (loop [ret (transient EMPTY)
             i 0]
        (if (< i (alength init))
          (do
            (when (not= (count ret)
                        (inc (/ i 2)))
              (throw (IllegalArgumentException. (str "Duplicate key: " (aget init i)))))
            (recur 
              (assoc! ret 
                      (aget init i)
                      (aget init (inc i)))
              (+ 2 i)))
          (persistent! ret))))
    :else (throw (Exception. "Bad argument to create-with-check"))))

(defn create 
  ([meta init]
   (with-meta (create init) meta))
  ([other]
   (cond
    (instance? java.util.Map other)
    (->
      (reduce 
        (fn [ret o]
          ;(prn "create assoc!")
          (assoc! ret 
                  (key o)
                  (val o)))
        (transient EMPTY)
        other)
      persistent!)

    ;;FIXME this is slow, why do this?
    ; @param init {key1,val1,key2,val2,...}
    (instance? (class (object-array 0)) other)
    (let [^objects init other]
      (loop [ret (transient EMPTY)
             i 0]
        (if (< i (alength init))
          (recur 
            (assoc! ret 
                    (aget init i)
                    (aget init (inc i)))
            (+ 2 i))
          (persistent! ret))))

    (seq? other)
    (loop [ret (transient EMPTY)
           items other]
      (if (seq items)
        (let [fst (first items)
              nitems (next items)
              _ (when-not nitems
                  (throw (IllegalArgumentException.
                           (str "No values supplied for key: "
                                fst))))
              snd (first nitems)
              nnitems (next nitems)
              ret (assoc! ret fst snd)]
          (recur ret nnitems))
        (persistent! ret)))
    :else (throw (IllegalArgumentException.
                   (str "Cannot create: " (class other)))))))

(defn hash-map 
  ([] EMPTY)
  ([& args]
   (create-with-check args)))
