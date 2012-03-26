(ns clj-stacktrace.smuggle
  "Functions for collecting stack traces and smuggling them into other
  threads. The API is experimental and may be subject to change."
  (:require [clj-stacktrace.core :as core]
            [clj-stacktrace.repl :as repl]))

(def ^{:dynamic true} *mark-depth* 0)

(defn mark-elem
  "Calls f, marking the call stack such that nothing on the stack
  earlier than calling f will be printed. Specifically,
  trim-stack-to-mark will return only the frames later than f, not the
  call to f, the call to mark-elem or anything earlier than them."
  [n f]
  (binding [*mark-depth* n]
    (try
      (f)
      (catch Throwable t
        (cond
         (::mark-depth (ex-data t)) (throw t)
         (::rethrow (ex-data t)) (throw t)
         :else (throw (ex-info "mark-elem" {::mark-depth n ::cause t})))))))

(let [ns-name (str (.getName *ns*))
      fn-name (name (.sym #'mark-elem))]
  (defn marker-elem? [elem]
    (and (= (:ns elem) ns-name)
         (= (:fn elem) fn-name))))

(defn trim-stack-to-mark
  "Takes a single cause-map and returns it with a potentially stack.
  The stack is shorter if mark-elem was used in some in which case
  the stack will be trimmed to exclude the mark-elem and everything
  outside it. Optionally trim an extra n elements after the mark."
  [n parsed-exception]
  (let [elems (:trace-elems parsed-exception)]
    (assoc parsed-exception
      :trimmed-elems (->> elems
                          (take-while #(not (marker-elem? %)))
                          (drop-last n)))))

(defn elems-from-here
  "Returns stack elements from here out to the thread start, or out to
  the first mark found, whichever comes first. See mark-elem for how
  to create a mark."
  []
  (->> (core/parse-exception (Exception.))
       (trim-stack-to-mark *mark-depth*)
       :trimmed-elems
       next))

(def ^{:dynamic true} *prevs* ())

(defn smuggle-chain
  "Adds the given message and stack elements to those already
  collected for this thread, and returns the new vector of parsed
  exceptions to smuggle."
  [message elems]
  (conj *prevs*
        {:message message
         :trimmed-elems (core/collapse-elems elems)}))

(defn splice-chain
  [e]
  (let [data (ex-data e)
        mark-depth (or (::mark-depth data) 0)
        e-seq (core/cause-seq (core/parse-exception (or (::cause data) e)))]
    (concat (::rethrow (:data (first e-seq)))
            (map #(trim-stack-to-mark mark-depth %) (next e-seq))
            (drop-last e-seq)
            *prevs*)))

(defn forward-snapshot [message]
  (smuggle-chain message (next (elems-from-here))))

(defn returnable-snapshot [throwable]
  (let [e-seq (core/cause-seq (core/parse-exception throwable))]
    (concat (drop-last e-seq)
            [(trim-stack-to-mark *mark-depth* (last e-seq))])))

(defn rethrow [message e-seq]
  (throw (ex-info message {::rethrow e-seq})))

(defn pst-multicol-on [on color? e]
  (repl/pst-multicol-on on color? (splice-chain e)))

(defmacro printing-spliced [chain & body]
  `(binding [*prevs* ~chain]
     (repl/with-pst (pst-multicol-on *out* false)
       (mark-elem 1 (fn [] ~@body)))))

(defmacro printing [& body]
  `(repl/with-pst (pst-multicol-on *out* false)
    (mark-elem 0 (fn [] ~@body))))

(defn smuggling-fn*
  "Primary entry-point for smuggling stack traces from one thread to
  another. Collects the stack at the point where smuggling-fn* is
  called and returns a function meant to be called in another thread
  which will catch any thrown exception and print the full trace
  including smuggled causes."
  [f message]
  (let [prevs (forward-snapshot message)]
    (bound-fn [& args]
      (printing-spliced prevs
        (repl/with-pst (pst-multicol-on *out* false)
          (mark-elem 2 #(apply f args)))))))


  (defn my-recv [msg f]
    (printing-spliced (:chain msg)
      (f (:obj msg))))

  (defn my-send [obj f]
    (my-recv {:chain (forward-snapshot "sending message from here")
              :obj obj}
             f))

#_(my-send :oops inc)

  (defn my-recv-rpc [p msg f]
    (printing-spliced (:chain msg)
      (try
        (f (:obj msg))
        (catch Throwable t
          (deliver p (returnable-snapshot t))))))

  (defn my-send-rpc [obj f]
    (let [err (promise)]
      (my-recv-rpc err
                   {:chain (forward-snapshot "sending rpc from here")
                    :obj obj}
                   f)
      (rethrow "rpc return error" @err)))

#_(printing
   (my-send-rpc :oops inc))
