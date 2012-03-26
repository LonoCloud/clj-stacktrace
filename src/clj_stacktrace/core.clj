(ns clj-stacktrace.core
  (:require [clojure.string :as string]))

(defn- clojure-code?
  "Returns true if the filename is non-null and indicates a clj source file."
  [class-name file]
  (or (re-find #"^user" class-name)
      (= file "NO_SOURCE_FILE")
      (and file (re-find #"\.clj$" file))))

(defn- clojure-ns
  "Returns the clojure namespace name implied by the bytecode class name."
  [class-name]
  (string/replace (or (get (re-find #"([^$]+)\$" class-name) 1)
                      (get (re-find #"(.+)\.[^.]+$" class-name) 1))
                  #"_" "-"))

;; drop everything before and including the first $
;; drop any __123 suffixes
;; sub _PLACEHOLDER_ for the corresponding char
;; replace function nesting $'s with °
(def clojure-fn-subs
  [[#"^[^$]*\$" ""]
   [#"__\d+($|\$)"  "$1"]
   [#"_QMARK_"  "?"]
   [#"_BANG_"   "!"]
   [#"_PLUS_"   "+"]
   [#"_GT_"     ">"]
   [#"_LT_"     "<"]
   [#"_EQ_"     "="]
   [#"_STAR_"   "*"]
   [#"_SLASH_"  "/"]
   [#"_"        "-"]
   [#"\$"       "°"]])

(defn- clojure-fn
  "Returns the clojure function name implied by the bytecode class name."
  [class-name]
  (reduce
   (fn [base-name [pattern sub]] (string/replace base-name pattern sub))
   class-name
   clojure-fn-subs))

(defn- clojure-anon-fn?
  "Returns true if the bytecode class name implies an anonymous inner fn."
  [class-name]
  (boolean (re-find #"\$.*\$" class-name)))

(defn parse-trace-elem
  "Returns a map of information about the java trace element.
  All returned maps have the keys:
  :file      String of source file name.
  :line      Number of source line number of the enclosing form.
  Additionally for elements from Java code:
  :java      true, to indicate a Java elem.
  :class     String of the name of the class to which the method belongs.
  Additionally for elements from Clojure code:
  :clojure   true, to inidcate a Clojure elem.
  :ns        String representing the namespace of the function.
  :fn        String representing the name of the enclosing var for the function.
  :anon-fn   true iff the function is an anonymous inner fn."
  [^StackTraceElement elem]
  (let [class-name (.getClassName elem)
        file       (.getFileName  elem)
        line       (let [l (.getLineNumber elem)] (if (pos? l) l))
        parsed     {:file file :line line}]
    (if (clojure-code? class-name file)
      (assoc parsed
        :clojure true
        :ns      (clojure-ns class-name)
        :fn      (clojure-fn class-name)
        :anon-fn (clojure-anon-fn? class-name))
      (assoc parsed
        :java true
        :class class-name
        :method (.getMethodName elem)))))

(defn parse-trace-elems
  "Returns a seq of maps providing usefull information about the java stack
  trace elements. See parse-trace-elem."
  [elems]
  (map parse-trace-elem elems))

(defn- trim-redundant
  "Returns the portion of the tail of causer-elems that is not duplicated in
  the tail of caused-elems. This corresponds to the \"...26 more\" that you
  see at the bottom of regular trace dumps."
  [causer-parsed-elems caused-parsed-elems]
  (loop [rcauser-parsed-elems (reverse causer-parsed-elems)
         rcaused-parsed-elems (reverse caused-parsed-elems)]
    (if-let [rcauser-bottom (first rcauser-parsed-elems)]
      (if (= rcauser-bottom (first rcaused-parsed-elems))
        (recur (next rcauser-parsed-elems) (next rcaused-parsed-elems))
        (reverse rcauser-parsed-elems)))))

(defn- parse-cause-exception
  "Like parse-exception, but for causing exceptions. The returned map has all
  of the same keys as the map returned by parse-exception, and one added one:
  :trimmed-elems  A subset of :trace-elems representing the portion of the
                  top of the stacktrace not shared with that of the caused
                  exception."
  [^Throwable causer-e caused-parsed-elems]
  (let [parsed-elems (parse-trace-elems (.getStackTrace causer-e))
        base {:class         (class causer-e)
              :message       (.getMessage causer-e)
              :trace-elems   parsed-elems
              :trimmed-elems (trim-redundant parsed-elems caused-parsed-elems)}]
    (conj base
          (when-let [cause (.getCause causer-e)]
            [:cause (parse-cause-exception cause parsed-elems)])
          (when-let [data (ex-data causer-e)]
            [:data data]))))

(defn parse-exception
  "Returns a Clojure map providing usefull informaiton about the exception.
  The map has keys
  :class        Class of the exception.
  :message      Regular exception message string.
  :trace-elems  Parsed stack trace elems, see parse-trace-elem.
  :cause        See parse-cause-exception."
  [^Throwable e]
  (let [parsed-elems (parse-trace-elems (.getStackTrace e))
        base {:class       (class e)
              :message     (.getMessage e)
              :trace-elems parsed-elems}]
    (conj base
          (when-let [cause (.getCause e)]
            [:cause (parse-cause-exception cause parsed-elems)])
          (when-let [data (ex-data e)]
            [:data data]))))

(defn cause-seq [e]
  "Takes a parsed exception and returns a seq of parsed exception
  causes, starting with the root cause and proceeding to the final or
  outer-most cause. Note this order is reversed compared to the order
  used by .printStackTrace and clojure.repl/pst"
  (->> e
       (iterate :cause)
       (take-while identity)
       reverse
       seq))

;; XXX test these -- some may not be working or needed
(def tag-map
  {["clojure.lang.AFn" "applyToHelper"] :apply
   ["clojure.lang.AFn" "applyTo"] :apply
   ["clojure.lang.AFn" "run"] :invoke
   ["clojure.lang.RestFn" "applyTo"] :apply
   ["clojure.lang.RestFn" "invoke"] :invoke
   ["clojure.lang.MultiFn" "invoke"] :invoke
   ["clojure.lang.Var" "invoke"] :var-deref
   ["clojure.lang.Var" "applyTo"] :var-deref
   ["clojure.lang.LazySeq" "sval"] :seq
   ["clojure.lang.LazySeq" "seq"] :seq
   ["clojure.lang.RT" "seq"] :seq
   ["clojure.lang.Agent$Action" "doRun"] :agent-run
   ["clojure.lang.Compiler" "eval"] :eval})

(defn elem-collapse-type
  "For the given trace element and tag-map, returns a keyword describing
  the reason for collapsing this element, or nil if the frame should not
  be collapsed."
  [{:keys [class method]} tag-map]
  (tag-map [class method]))

(defn collapse-elems
  "Returns the given parsed elements except for those that are not
  generally helpful to understanding the stack trace. This includes
  calls to apply, the invoke methods of clojure functions, and others.
  The nature of each removed element is recorded in a set of :tags on
  the element that caused it."
  [parsed-elems]
  (first
   (reduce (fn [[stack tags] elem]
             (if-let [new-tag (elem-collapse-type elem tag-map)]
               [stack (conj tags new-tag)]
               [(conj stack (conj elem (when (seq tags) [:tags tags]))) #{}]))
           [[] #{}]
           parsed-elems)))

(defn shorten-messages
  "Frequently an exception message in a chain of causes will repeat
  the message of its cause, perhaps with some additional text. This
  function takes a sequence of parsed causes and replaces the
  duplicate text in such situations with elipses."
  [parsed-causes]
  (for [[c prev-c] (map list parsed-causes (cons nil parsed-causes))]
    (if-not (:message prev-c)
      c
      (assoc c :message
             (string/replace (or (:message c) "No message")
                             (:message prev-c)
                             "…")))))

(defmacro safety-net
  "Catches and prints exceptions thrown in its body using the stock
  Java stacktrace printer. Meant to be used internally by this library
  in contexts where a bug in the library might otherwise ruin the
  chances of getting any stack trace at all."
  [original-exception & body]
  `(try
     ~@body
     (catch Throwable t#
       (println "Error in clj-stacktrace:")
       (.printStackTrace t# *out*)
       (println "\nOriginal exception:")
       (.printStackTrace ~original-exception *out*)
       (throw ~original-exception))))
