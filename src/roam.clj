(ns roam
  "Parsing for Roam-style markdown."
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.util.regex Pattern]))

(def ^:dynamic *trace* nil)

(defn- mapm [m kf vf]
  (with-meta 
    (zipmap (map kf (keys m)) (map vf (vals m)))
    (meta m)))

(defn default-p [tail]
  (when tail
    (constantly tail)))

(defn- lit-p [match tail]
  (fn [input]
    (when (= match input)
      tail)))

(def ^:private nop (default-p '((no-op))))

(defn- compile-literals [s]
  (vec
    (for [[match tail] s
          :when        (char? match)]
      (lit-p match tail))))

(defn- regex? [o] (instance? java.util.regex.Pattern o))

(defn- compile-regexes [s]
  (vec
    (for [[match tail] s
          :when        (regex? match)
          :let         [pred (.asMatchPredicate match)]]
      (fn [input]
        (when (.test pred (str input))
          tail)))))

;; When presented with an input (which might be a character or nil)
;; we will find a list of "instructions" by looking for a matching
;; rule. Matching proceeds as follows:
;;
;; 1. If the input matches a literal character, use its instructions
;; 1.a. If the input is nil and there is an `:eol` rule, use its instructions
;; 2. If there are regex patterns, test them (in undefined order) use
;;    the first matching pattern's instructions.
;; 3. If there is a `:default` rule, use its instructions
;; 4. Fall back to a list with one `no-op` instruction.

(defn- compile-state [s]
  (let [preds (compile-literals s)
        preds (into preds (compile-regexes s))
        preds (if-not (:eol s)     preds (conj preds (lit-p nil (:eol s))))
        preds (if-not (:default s) preds (conj preds (default-p (:default s))))
        preds (or (not-empty preds) nop)]
    (apply some-fn preds)))

(defn- compile-parser-instructions [all-states]
  (mapm all-states identity compile-state))

(def ^:private ws #"\s+")
(def ^:private tagbody #"[a-zA-Z0-9_/\-@!*\\:']")

;; The parser operates as a virtual machine with:
;;
;; registers
;; c     - the most recently read character
;; x     - text. mostly used for 'pending' characters
;; y     - text. used for second capture group
;; z     - text. used to hold during lookahead
;; a     - accumulator. primary capture group
;; state - the current state

;; instructions
;;
;; mov c x    - move the contents of c into register x
;; swap x y   - exchange the contents of register x and register y
;; append x c - append contents of x to register c. if x is empty, does nothing
;; empty x    - set register x to empty string
;; state k    - set state to keyword k
;; push       - push the state register onto the state stack
;; dup        - duplicate the top of the state stack
;; pop        - pop the top of the state stack into the state register
;; finish     - make a vector of the state register and accumulator,
;;              conj it to the segment list
;; finish2    - like finish, but use the state, accumulator and register y to
;;              make a 3-tuple
;; finish-with-empty - like finish but does not elide empty spans.
;;
;; This table maps current state and input to a list of instructions.
;;
;; See the comment above `compile-state` for the input matching rules.

;; Turn this assembler into a macro assembler. Just don't use
;; macros. XD
;; these functions return instructions for the common cases of two
;; delimiters surrounding a text span.
(defn- digraph-hold-and-wait
  [next-state]
  (list '(mov c x) '(push) (list 'state next-state)))

;; the one-arg form is for beginning a span of some new type. the
;; zero-arg form is for exiting a span back to whatever was there
;; before.
(defn- digraph-completed
  ([]
   (list '(empty x) '(pop) '(finish) '(pop)))
  ([next-state]
   (list '(empty x) '(dup) '(pop) '(finish) (list 'state next-state))))
(defn- digraph-not-completed
  []
  (list '(append x a) '(empty x) '(append c a) '(pop)))

(def ^:private
  parser-instruction-source
  {:text/plain              {\*       (digraph-hold-and-wait :maybe-bold)
                             \_       (digraph-hold-and-wait :maybe-italics)
                             \[       '((mov c x) (push) (state :maybe-internal-link))
                             \:       '((mov c x) (push) (state :maybe-was-attr))
                             \`       '((finish) (push) (state :maybe-inline-code))
                             \#       '((mov c x) (push) (state :maybe-tag))
                             \!       '((mov c x) (push) (state :maybe-image-link))
                             \^       (digraph-hold-and-wait :maybe-highlight)
                             \~       (digraph-hold-and-wait :maybe-strikethrough)
                             \{       (digraph-hold-and-wait :maybe-macro)
                             :default '((append x a) (empty x) (append c a))}
   :maybe-macro             {\{       (digraph-completed :macro)}
   :macro                   {\}       (digraph-hold-and-wait :maybe-not-macro)
                             \[       '((no-op))
                             \]       '((no-op))
                             \:       '((mov a y) (empty a) (push) (state :before-macro-argument))
                             ws       '((no-op))
                             :default '((append x a) (empty x) (append c a))}
   :before-macro-argument   {ws       '((no-op))
                             :default '((append c a) (state :macro-argument))}
   :macro-argument          {\}       (digraph-hold-and-wait :maybe-not-macro-argument)
                             :default '((append x a) (empty x) (append c a))}
   :maybe-not-macro-argument {\}      '((empty x) (pop) (pop) (swap a y) (finish2) (empty a) (pop))
                              :default (digraph-not-completed)
                              }
   :maybe-not-macro         {\}       (digraph-completed)
                             :default (digraph-not-completed)}
   :maybe-strikethrough     {\~       (digraph-completed :text/strikethrough)
                             :default (digraph-not-completed)}
   :text/strikethrough      {\~       (digraph-hold-and-wait :maybe-not-strikethrough)
                             \`       '((finish) (push) (state :maybe-inline-code))
                             :default '((append x a) (empty x) (append c a))}
   :maybe-not-strikethrough {\~       (digraph-completed)
                             :default (digraph-not-completed)}
   :maybe-highlight         {\^       (digraph-completed :text/highlight)
                             :default (digraph-not-completed)}
   :text/highlight          {\^       (digraph-hold-and-wait :maybe-not-highlight)
                             \`       '((finish) (push) (state :maybe-inline-code))
                             :default '((append x a) (empty x) (append c a))}
   :maybe-not-highlight     {\^       (digraph-completed)
                             :default (digraph-not-completed)}
   :maybe-was-attr          {\:       '((empty x) (state :attr) (finish) (pop))
                             :default '((append x a) (empty x) (append c a) (pop))}
   :attr                    {:default '()}
   :maybe-bold              {\*       (digraph-completed :text/bold)
                             :default (digraph-not-completed)}
   :maybe-not-bold          {\*       (digraph-completed)
                             :default (digraph-not-completed)}
   :text/bold               {\*       (digraph-hold-and-wait :maybe-not-bold)
                             \`       '((finish) (push) (state :maybe-inline-code))
                             :default '((append x a) (empty x) (append c a))}
   :maybe-italics           {\_       (digraph-completed :text/italics)
                             :default (digraph-not-completed)}
   :maybe-not-italics       {\_       (digraph-completed)
                             :default (digraph-not-completed)}
   :text/italics            {\_       (digraph-hold-and-wait :maybe-not-italics)
                             \`       '((finish) (push) (state :maybe-inline-code))
                             :default '((append x a) (empty x) (append c a))}
   :maybe-internal-link     {\[       '((empty x) (dup) (pop) (finish) (state :internal-link))
                             :default '((mov a z) (append x a) (empty x) (mov a y) (empty a) (append c y) (append c a) (state :maybe-hyperlink-text))}
   :internal-link           {\]       '((mov c x) (push) (state :maybe-not-internal-link))
                             :default '((append x a) (empty x) (append c a))}
   :maybe-not-internal-link {\]       '((empty x) (pop) (finish) (pop))
                             :default '((append x a) (empty x) (append c a) (pop))}
   :maybe-hyperlink-text    {\]       '((append c y) (mov a x) (empty a) (state :expect-hyperlink-target))
                             :default '((append c y) (append c a))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :expect-hyperlink-target {\(       '((append c y) (state :hyperlink-target))
                             :default '((append c y) (mov y a) (pop))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :hyperlink-target        {\)       '((mov a y) (mov z a) (state :text/plain) (finish)
                                        (mov y a) (mov x y) (empty x) (state :hyperlink) (finish2) (pop))
                             :default '((append c y) (append c a))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :hyperlink               {:default '((no-op))}
   :maybe-tag               {tagbody  '((pop) (finish) (push) (empty a) (empty x) (append c a) (state :tag))
                             \[       '((append c x) (state :maybe-internal-link))
                             :default '((append x a) (empty x) (append c a) (pop))}
   :tag                     {:default '((finish) (pop) (append c a))
                             tagbody  '((append c a))}
   :maybe-inline-code       {\`       '((state :maybe-empty-inline-code))
                             :default '((append c a) (state :inline-code))}
   :maybe-empty-inline-code {\`       '((state :source-language))
                             :default '((state :inline-code) (finish-with-empty) (pop))}
   :source-language         {ws       '((mov a y) (empty a) (state :source))
                             tagbody  '((append c a))}
   :source                  {\`       '((empty x) (append c x) (state :maybe-not-source-1))
                             :default '((append c a))
                             :eol     '((finish2))}
   :maybe-not-source-1      {\`       '((append c x) (state :maybe-not-source-2))
                             :default '((append x a) (state :source))}
   :maybe-not-source-2      {\`       '((empty x) (state :source) (finish2) (pop))
                             :default '((append x a) (state :source))}
   :inline-code             {\`       '((finish) (pop))
                             :default '((append c a))}
   :maybe-image-link        {\[       '((mov a z) (append x a) (empty x) (mov a y) (empty a) (append c y) (state :maybe-image-alt-text))
                             :default '((append x a) (empty x) (append c a) (pop))}
   :maybe-image-alt-text    {\]       '((append c y) (mov a x) (empty a) (state :expect-image-target))
                             :default '((append c y) (append c a))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :expect-image-target     {\(       '((append c y) (state :image-target))
                             :default '((append c y) (mov y a) (pop))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :image-target            {\)       '((mov a y) (mov z a) (state :text/plain) (finish)
                                        (mov y a) (mov x y) (empty x) (state :image) (finish2) (pop))
                             :default '((append c y) (append c a))
                             :eol     '((mov y a) (empty y) (state :text/plain) (finish))}
   :image                   {:default '((no-op))}
   })

(def ^:private parser-states (set (keys parser-instruction-source)))

(def ^:private parser-instructions (compile-parser-instructions parser-instruction-source))

(defn- get-instructions [machine]
  ((get parser-instructions (:state machine))
   (get machine 'c)))

(def ^:private register-names #{'a 'x 'y 'z 'c})

(defn- assert-register [& rs]
  (doseq [r rs]
    (assert (some #{r} register-names) (format (name r) " does not name a register"))))

(defn- assert-known-state [s]
  (assert (contains? parser-states s) (format "Transition to undefined state %s requested" s)))

(defn machine [initial-state]
  (assert-known-state initial-state)
  {'x ""
   'y ""
   'a ""
   'c nil
   :state initial-state
   :stack []
   :segments []})

(defn- decode-insn [_ insn]
  (first insn))

(defmulti execute decode-insn)

(defmethod execute 'mov [machine [_ from to]]
  (assert-register from to)
  (assoc machine to (get machine from)))

(defmethod execute 'swap [machine [_ fst snd]]
  (assert-register fst snd)
  (-> machine
      (assoc fst (get machine snd))
      (assoc snd (get machine fst))))

(defmethod execute 'append [machine [_ from to]]
  (assert-register from to)
  (update machine to str (get machine from)))

(defmethod execute 'empty [machine [_ r]]
  (assert-register r)
  (assoc machine r ""))

(defmethod execute 'state [machine [_ s]]
  (assert-known-state s)
  (assoc machine :state s))

(defmethod execute 'push [machine [_]]
  (update machine :stack conj (:state machine)))

(defmethod execute 'pop [machine [_]]
  (assert (not (empty? (:stack machine))))
  (let [s (peek (:stack machine))]
    (-> machine
        (update :stack pop)
        (assoc :state s))))

(defmethod execute 'dup [machine [_]]
  (assert (not (empty? (:stack machine))))
  (let [s (peek (:stack machine))]
    (update machine :stack conj s)))

(defmethod execute 'finish [machine [_]]
  (if (empty? (get machine 'a))
    machine
    (-> machine
        (update :segments conj [(get machine :state) (get machine 'a)])
        (assoc 'a ""))))

(defmethod execute 'finish-with-empty [machine [_]]
  (-> machine
      (update :segments conj [(get machine :state) ""])))

(defmethod execute 'finish2 [machine [_]]
  (if (empty? (get machine 'a))
    machine
    (-> machine
        (update :segments conj [(get machine :state) (get machine 'a) (get machine 'y)])
        (assoc 'a "")
        (assoc 'y ""))))

(defmethod execute 'no-op [machine _]
  machine)

(defn- with-input [m c]
  (assoc m 'c c))

(defn- run [m]
  (let [insns (get-instructions m)]
    (let [m' (reduce execute m insns)]
      (when *trace* (tap> (assoc m' :insns insns)))
      m')))

(defn- initialize [initial-state]
  (machine initial-state))

(defn- finalize [m]
  (:segments
   (execute m '[finish])))

(defn eol? [idx text]
  (> idx (count text)))

(defn- step [m c]
  (run (with-input m c)))

(defn parse
  [text]
  (loop [idx 0
         m   (initialize :text/plain)]
    (if (eol? idx text)
      (finalize m)
      (recur (inc idx) (step m (get text idx))))))
