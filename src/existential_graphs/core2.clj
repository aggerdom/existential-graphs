(ns existential-graphs.core2
  (:require [spectacles.lenses :as lens]))

;; Helpers
(defn path-depth [path] (+ 2 (count path))) ;; TODO: Implement
(defn is-double-cut? [G path]
  (let [node (get-in G path)]
    (and (< 0 (count node))
         (= (first node) :cut) ;; Node value is :cut
         (= (count node) 2) ;; No other children but second cut
         (= (first (second node)) :cut))))

(comment
  (is-double-cut? [:SA [:cut [:cut]]] []) ;; => false
  (is-double-cut? [:SA [:cut [:cut]]] [1]) ;; => true
  (is-double-cut? [:SA [:cut [:A [:cut]]]] [1]) ;; => false
  (is-double-cut? [:SA [:A] [:cut [:cut]]] [1]) ;; => false
  (is-double-cut? [:SA [:A] [:cut [:cut]]] [2]) ;; => true
  (is-double-cut? [:SA [:cut [:cut [:A]]]] [1]) ;; => true
  (is-double-cut? [:SA [:cut [:cut [:A]]]] [1 2 3])) ;; => false (nonexistent path)

(defn exists-ancestral-copy? [G path] true) ;; TODO: Implement
(defn have-common-parent? [G src-path dest-path] true) ;; TODO: Implement

(defn insert [G subG path]
  "Insert node as a child of a specific path into graph.
   Empty path places node as child of the root."
  (if (empty? path) (conj G subG)
      (update G (first path)
              #(insert % subG (rest path)))))

(defn replace-node [G f path]
  "Replace node at a specific position with a new node obtained by
   calling f on the existing val"
  #dbg
   (if (= (count path) 1) (update G (first path) #(f %))
       (update (first path) #(replace-node % f (rest path)))))

(replace-node [:A [:B]]  (fn [x] (vector :C x))   [1])
(replace-node [:SA [:A]] (fn [x] [:cut [:cut x]]) [1])

(defn drop-vec-position [v ix]
  (into (subvec v 0 ix)
        (subvec v (inc ix))))

(defn erase [G path]
  {:pre [(< 0 (count path))]}
  (if (= (count path) 1) (drop-vec-position G (first path))
      (update G (first path) #(erase G (rest path)))))

;; Rules ==================================================================
;; Specify path to place child (path points to parent)
(defn insertion [G subG path]
  {:pre [(odd? (path-depth path))]
   :post []
   :docstring "Any subgraph may be inserted at an odd numbered depth"}
  (insert G subG path))

;; Specify path of child to erase (path points to child)
(defn erasure [G path]
  {:pre [(odd? (path-depth path))] ;; odd? because it's *in* even depth
   :post []
   :docstring "Any subgraph in an even depth may be erased"}
  (erase G path))

(erasure [:SA [:A] [:cut [:B]]] [1])
(erasure [:SA [:A] [:cut [:B]]] [2])
;; (erasure [:SA [:A] [:cut [:B]]] [2 1]) ;; ERROR

(defn double-cut [G path]
  {:pre []
   :post []
   :docstring (str "A pair of cuts w nothing between them may be drawn"
                   "around any subgraph (including empty graph)")}
  (cond (= path []) (throw "Not understood")
        (and (= 1 (count path))
             (nil? (first path))) (conj G [:cut [:cut]])
        (= 1 (count path)) false
        :else (update G (first path) #(double-cut % (rest path)))))

;; Examples
;; (double-cut [:SA] []) ;; => [:SA [:cut [:cut]]]
(double-cut [:SA] [nil]) ;; => [:SA [:cut [:cut]]]
;; (double-cut [:SA [:A]] []) ;; => [:SA [:A] [:cut [:cut]]]
(double-cut [:SA [:A]] [nil]) ;; => [:SA [:A] [:cut [:cut]]]
(double-cut [:SA [:A]] [1]) ;; => [:SA [:cut [:cut [:A]]]]
(double-cut [:SA [:A [:B [:C]]]] [1 1])
(double-cut [:SA [:A [:B [:C]]]] [1]) ;; WORKS
(double-cut [:SA [:A [:B [:C]]]] [1 1]) ;; TODO: BROKEN!!!


(defn double-cut-erasure [G path]
  {:pre [(is-double-cut? G path)]
   :post []
   :docstring "A pair of cuts w nothing between them may be erased"}
  ;; Path points to first cut
  (let [G1 G
        [_ [_ & children]] (get-in G path)
        ;; Append all children of second cut to the parent
        G2 (into (get-in G1 (butlast path)) children)
        ;; Remove the second cut from the altered parent
        G3 (erase G2 path)]
    G3))

(comment
  (double-cut-erasure [:SA [:cut [:cut [:A] [:B]]]] [1])
  ;; => [:SA [:A] [:B]]
  )

(fn iteration [G src-path dest-path]
  {:pre [(have-common-parent? G src-path dest-path)]
   :post []
   :docstring (str "Any subgraph P depending on node n may be copied"
                   "into any node depending on n.")})

(defn deiteration [G src-path]
  {:pre [(exists-ancestral-copy? src-path)]
   :post []
   :docstring (str "Any subgraph P in node n may be erased if there is"
                   "a copy of it in a node ancestral to n."
                   "(A node on which n depends which n depends)")})

(insert [:SA] [:cut [:cut]] [])
(insertion [:SA] [:cut [:cut]] []) ;; Double cut must use it's own rule
(insert [:SA [:cut]] [:cut] [1])
(insert [:SA] [:A] [])

(get-in [:SA] [1 2])

(remove-from-v)
(into (subvec [0 1 2] 0 0)
      (subvec [0 1 2] 1))

(get-in [] [nil])
(subvec [0] 2)

(update [0 1 2] 1 #(do [%] 3))

