# Hash Array Mapped Tries

<p align="center">
  <img src="tree.png" width="1000"/>
</p>

This library contains: 

- a [reimplementation](#persistenthashmap-port) of Clojure's PersistentHashMap in Clojure
- [visualization tools](#visualizing-hamts) for the underlying trie.
- a [draft paper](#paper) that gives a from-scratch tutorial
  on hash array mapped tries.
- a [prototype](#keyword-map-optimizations) for dynamically generated `defrecord`s
  based on runtime key frequency.

It was completed as part of the B503 Algorithms graduate course
at Indiana University Bloomington (Fall 2016).
It was also just a great excuse to learn all this stuff.

## PersistentHashMap port

The top-level directory of this repository is a leiningen
project containing

### Visualizing HAMTs

## Paper

The accompanying [paper](paper/paper.pdf) outlines 

## Keyword map optimizations

## License

Copyright © 2016 Ambrose Bonnaire-Sergeant

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
