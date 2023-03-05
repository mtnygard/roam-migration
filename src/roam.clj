(ns roam
  "Parsing for Roam-style markdown."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; set `*trace*` to :print to print to console
;; set it to :visualize to emit a dot file
(def ^:dynamic *trace* nil)

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

;;; This table maps current state and input to a list of instructions.
(def ^:private
  parser-instructions
  '{:text/plain              {\*       ((mov c x) (push) (state :maybe-bold))
                              \_       ((mov c x) (push) (state :maybe-italics))
                              \[       ((mov c x) (push) (state :maybe-internal-link))
                              \:       ((mov c x) (push) (state :maybe-was-attr))
                              \`       ((finish) (push) (state :inline-code))
                              :default ((append x a) (empty x) (append c a))}
    :maybe-was-attr          {\:       ((empty x) (state :attr) (finish) (pop))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :attr                    {:default ()}
    :maybe-bold              {\*       ((empty x) (dup) (pop) (finish) (state :text/bold))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :maybe-not-bold          {\*       ((empty x) (pop) (finish) (pop))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :text/bold               {\*       ((mov c x) (push) (state :maybe-not-bold))
                              :default ((append x a) (empty x) (append c a))}
    :maybe-italics           {\_       ((empty x) (dup) (pop) (finish) (state :text/italics))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :maybe-not-italics       {\_       ((empty x) (pop) (finish) (pop))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :text/italics            {\_       ((mov c x) (push) (state :maybe-not-italics))
                              :default ((append x a) (empty x) (append c a))}
    :maybe-internal-link     {\[       ((empty x) (dup) (pop) (finish) (state :internal-link))
                              :default ((mov a z) (append x a) (empty x) (mov a y) (empty a) (append c y) (append c a) (state :maybe-hyperlink-text))}
    :internal-link           {\]       ((mov c x) (push) (state :maybe-not-internal-link))
                              :default ((append x a) (empty x) (append c a))}
    :maybe-not-internal-link {\]       ((empty x) (pop) (finish) (pop))
                              :default ((append x a) (empty x) (append c a) (pop))}
    :maybe-hyperlink-text    {\]       ((append c y) (mov a x) (empty a) (state :expect-hyperlink-target))
                              :default ((append c y) (append c a))
                              :eol     ((mov y a) (empty y) (state :text/plain) (finish))}
    :expect-hyperlink-target {\(       ((append c y) (state :hyperlink-target))
                              :default ((append c y) (mov y a) (pop))
                              :eol     ((mov y a) (empty y) (state :text/plain) (finish))}
    :hyperlink-target        {\)       ((mov a y) (mov z a) (state :text/plain) (finish)
                                        (mov y a) (mov x y) (empty x) (state :hyperlink) (finish2) (pop))
                              :default ((append c y) (append c a))
                              :eol     ((mov y a) (empty y) (state :text/plain) (finish))}
    :hyperlink               {:default ()}
    :inline-code             {\`       ((finish) (pop))
                              :default ((append c a) (state :inline-code))}
    })

(def ^:private parser-states (set (keys parser-instructions)))

(defn- get-instructions [machine]
  (let [input (or (get machine 'c) :eol)]
    (or
      (get-in parser-instructions [(:state machine) input])
      (get-in parser-instructions [(:state machine) :default])
      '[no-op])))

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