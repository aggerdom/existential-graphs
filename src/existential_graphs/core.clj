(ns existential-graphs.core
  (:require [spectacles.lenses :as lens]))

;; Helpers
(defn path-depth [path] (+ 2 (count (take-while #(not (nil? %)) path))))
(defn is-double-cut? [G path]
  (let [node (get-in G path)]
    (and (< 0 (count node))
         (= (first node) :cut) ;; Node value is :cut
         (= (count node) 2) ;; No other children but second cut
         (= (first (second node)) :cut))))

(defn exists-ancestral-copy? [G path] true) ;; TODO: Implement
(defn have-common-parent? [G src-path dest-path] true) ;; TODO: Implement

(defn insert [G subG path]
  "Insert node as a child of a specific path into graph.
   Empty path places node as child of the root."
  (cond (empty? path) (conj G subG)
        (nil? (first path)) (conj G subG)
        :else (update G (first path)
                      #(insert % subG (rest path)))))

(defn replace-node [G f path]
  "Replace node at a specific position with a new node obtained by
   calling f on the existing val"
  (if (= (count path) 1) (update G (first path) #(f %))
      (update (first path) #(replace-node % f (rest path)))))

(defn drop-vec-position [v ix]
  (into (subvec v 0 ix)
        (subvec v (inc ix))))


(defn erase [G path]
  {:pre [(< 0 (count path))]}
  (if (= (count path) 1) (drop-vec-position G (first path))
      (update G (first path) #(erase G (rest path)))))


;; Rules ==================================================================
;;      SA     -- 2
;;     cut     -- 3
;; A B cut     -- 4 (Insertion "in odd"=add B as child of odd number of cuts) [1 nil]
;;      C      -- 5 (Erasure "in even" = we can erase C) [1 1 1] (<- path is odd len)
;; Specify path to place child (path points to parent)
(defn insertion [G subG path]
  {:pre [(odd? (path-depth path))]
   :post []
   :docstring "Any subgraph may be inserted at an odd numbered depth"}
  (insert G subG path))


;; Specify path of child to erase (path points to node to erase)
(defn erasure [G path]
  {:pre [(even? (dec (path-depth path)))] ;; We can erase at any even depth (root is 2)
   :post []
   :docstring "Any subgraph in an even depth may be erased"}
  (erase G path))


(defn double-cut [G path]
  ;; If path points to a node, then:
  ;;  - double cut will be placed around that node
  ;;  - That node will be placed as a child under two cuts in its present pos
  ;; If path ends in nil, then:
  ;;  - two cuts will be placed as a child of (butlast node)
  ;;  - A double cut will be created without any children
  {:pre []
   :post []
   :docstring (str "A pair of cuts w nothing between them may be drawn"
                   "around any subgraph (including empty graph)")}
  (cond (= path []) (throw "Not understood")
        ;; CASE: Empty double-cut
        (and (= 1 (count path)) (nil? (first path)))
        (conj G [:cut [:cut]])
        ;; CASE: Cut around node
        (= 1 (count path))
        (update G (first path) (fn [cur-node] [:cut [:cut cur-node]]))
        ;; CASE: We're not drilled down yet
        :else (update G (first path) #(double-cut % (rest path)))))

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

(defn iteration [G src-path dest-path]
  {:pre [(have-common-parent? G src-path dest-path)]
   :post []
   :docstring (str "Any subgraph P depending on node n may be copied"
                   "into any node depending on n.")}
  (insert G (get-in G src-path) dest-path))

(defn deiteration [G src-path]
  {:pre [(exists-ancestral-copy? src-path)]
   :post []
   :docstring (str "Any subgraph P in node n may be erased if there is"
                   "a copy of it in a node ancestral to n."
                   "(A node on which n depends which n depends)")}
  (erase G src-path))



