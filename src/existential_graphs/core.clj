(ns existential-graphs.core
  (:require [existential-graphs.tree :as T]
   [dorothy.core :as dot]
            [dorothy.jvm :refer (render save! show!)]))

;; Define data structure
;; ENTITIES
;; :SA  := Sheet of assertion
;; :cut := logical negation, increases depth by one for it's children
;; :prop := proposition

(def example-tree
  [:SA
   [:A]
   [:cut
    [:B]
    [:cut
     [:C]]]])

;; Common Definitions
;; Adjusted Definitions

(do "helper-definitions"
    (defn leaf? [node] (= (count node) 1))
    (defn children? [node] (> (count node) 1))
    (defn node-value [node] (first node))
    (defn node-children [node] (rest node))
    (defn add-child-l [p c] (into [(first p) c] (rest p)))
    (defn add-child-r [p c] (conj p c))
    (defn node-count [T]
      (loop [count 0, stack [T]]
        (if (empty? stack) count
            (recur (inc count)
                   (concat (rest (first stack))
                           (rest stack))))))
    (defn edges [T]
      (if (leaf? T) []
          (concat
           ()
      )
    ))

(defn visualize-tree [T]
  (let [edges]))


;; RULES
(let [g (dot/graph [
                    [:22 :2]
                    [:8 :2]
                    [:10 :5 :1]
                    [:10 :2 :1]])]
  (-> g dot/dot show!))

(comment "R.I.I. Any subgraph may be inserted at an odd numbered level.
         R.I.E. Any subgraph at an even numbered level may be erased.
         R.E.D.I. A pair of cuts with nothing between them may be drawn
         around any subgraph.
         R.E.D.E. A pair of cuts with nothing between them may be erased.
         Iteration: Any subgraph P depending on node n
                    may be copied into any node depending on n
         Deiteration: Any subgraph P in node n may be erased if there is
                    a copy of it in a node ancestral to n")

