(ns yadic.potemkin.types
  (:use
    [clojure [set :only (union)]]
    [yadic.potemkin.macros :only (equivalent? normalize-gensyms safe-resolve unify-gensyms)])
  (:require
    [yadic.riddley.walk :as r]))

;;;

(definterface PotemkinType)


(defn clean-deftype [x]
  (let [version (let [{:keys [major minor incremental ]} *clojure-version*]
                  (str major "." minor "." incremental))]
    (remove
      #(when-let [min-version (-> % meta :min-version)]
        (neg? (.compareTo version min-version)))
      x)))

(declare merge-deftypes* deftype->deftype*)

(defn abstract-type? [x]
  (and (symbol? x) (= :potemkin/abstract-type (-> x safe-resolve meta :tag))))

(def ^:dynamic *expanded-types* #{})

(defn expand-deftype [x]
  (let [abstract-types (->> x
                            (filter abstract-type?)
                            (map resolve)
                            (remove *expanded-types*)
                            set)
        abstract-type-bodies (binding [*expanded-types* (union *expanded-types* abstract-types)]
                               (->> abstract-types
                                    (map deref)
                                    (map clean-deftype)
                                    (map expand-deftype)
                                    (map deftype->deftype*)
                                    doall))]
    (apply merge-deftypes*
           (concat
             abstract-type-bodies
             [(deftype->deftype*
                (if (abstract-type? (second x))
                  x
                  (remove abstract-type? x)))]))))

;;;

(defn transform-deftype*
  [f x]
  (r/walk-exprs
    #(and (sequential? %) (= 'deftype* (first %)))
    f
    x))

(defn deftype->deftype* [x]
  (let [x (r/macroexpand x)
        find-deftype* (fn find-deftype* [x]
                        (when (sequential? x)
                          (let [f (first x)]
                            (if (= 'deftype* f)
                              x
                              (first (filter find-deftype* x))))))
        remove-nil-implements (fn [x]
                                (concat
                                  (take 5 x)
                                  [(->> (nth x 5) (remove nil?) vec)]
                                  (drop 6 x)))]
    (->> x
         find-deftype*
         remove-nil-implements)))

(defn deftype*->deftype [x]
  (let [[_ dname _ params _ implements & body] (deftype->deftype* x)]
    (list* 'deftype (symbol (name dname)) params (concat (remove #{'clojure.lang.IType} implements) body))))

(defn deftype*->fn-map [x]
  (let [fns (drop 6 x)
        fn->key (fn [f] [(first f) (map #(-> % meta :tag) (second f))])]
    (zipmap
      (map fn->key fns)
      fns)))

(defn merge-deftypes*
  ([a]
   a)
  ([a b & rest]
   (let [fns (vals
               (merge
                 (deftype*->fn-map a)
                 (deftype*->fn-map b)))
         a-implements (nth a 5)
         merged (transform-deftype*
                  #(concat
                    (take 5 %)
                    [(->> (nth % 5) (concat a-implements) distinct vec)]
                    fns)
                  b)]
     (if-not (empty? rest)
       (apply merge-deftypes* merged rest)
       merged))))

;;;

(defmacro def-abstract-type
  "An abstract type, which can be used in conjunction with deftype+."
  [name & body]
  `(def
     ~(with-meta name {:tag :potemkin/abstract-type})
     '(deftype ~name [] ~@body)))

(defonce type-bodies (atom {}))

(defmacro deftype+
  "A deftype that won't evaluate if an equivalent datatype with the same name already exists,
   and allows abstract types to be used."
  [name params & body]
  (let [body (->> (list* 'deftype name params 'yadic.potemkin.types.PotemkinType body)
                  clean-deftype
                  expand-deftype
                  deftype*->deftype)

        classname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))

        prev-body (when (class? (ns-resolve *ns* name))
                    (@type-bodies classname))]

    (when-not (and prev-body
                   (equivalent?
                     (transform-deftype* identity prev-body)
                     (transform-deftype* identity body)))
      (swap! type-bodies assoc classname
             (r/macroexpand-all body))

      body)))



;;;


