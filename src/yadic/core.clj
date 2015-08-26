(ns yadic.core
  (:require [yadic.potemkin.collections :refer [def-map-type keys*]]
            [clojure.stacktrace :refer [print-cause-trace]])
  (:import [java.util NoSuchElementException]
           [java.lang.reflect Method Constructor]
           [clojure.lang RestFn IDeref Symbol]
           [java.lang AutoCloseable]))

; Utils
; ==================================

(defn has-arity [f arity]
  (if (instance? RestFn f)
    (>= arity (.getRequiredArity f))
    (let [invoke-param-counts (->> (.getDeclaredMethods ^Class (class f))
                                   (filter #(= "invoke" (.getName ^Method %)))
                                   (map (fn [m] (count (.getParameterTypes m)))))]
      (some #(= % arity) invoke-param-counts))))

(defn valid-activator-fn [f]
  (if (has-arity f 1)
    f
    (throw (IllegalArgumentException. (str "Activator functions are expected to be arity 1, i.e. (fn [container]). "
                                           "Function is not arity 1: " f)))))

; Activator
; ==================================

(defprotocol Activator
  (activate [this container])
  (close [this instance]))

; Functions
; ----------------------------------

(deftype FnActivator [f]
  Activator
  (activate [this container]
    (f container))
  (close [this instance]
    (when (instance? AutoCloseable instance)
      (.close instance)))

  Object
  (toString [this]
    (str "Function activator using " f)))

(defn fn-activator [f]
  (FnActivator. (valid-activator-fn f)))

(defn fn->activator [f container-keys]
  (when-not (has-arity f (count container-keys))
    (throw (IllegalArgumentException. (str "Function is not arity " (count container-keys) ". Cannot create using keys " container-keys " " f))))

  (fn [container]
    (apply f (map container container-keys))))

(defmacro act [params & body]
  (let [container-keys (map (comp keyword #(.getName ^Symbol %)) params)]
    `(fn->activator (fn ~params ~@body) ~(vec container-keys))))


; Conversion to activators
; ----------------------------------

(defn as-activator [x]
  (cond (fn? x) (fn-activator x)
        (satisfies? Activator x) x
        (nil? x) (throw (IllegalArgumentException. (str "Cannot create activator from nil")))
        true (throw (IllegalArgumentException. (str "Cannot create activator from " (type x) " " x)))))

; Activators
; (a collection of arity-1 constructor functions, taking a Container and return an instantiated instance)
; ==================================

(def-map-type Activators [m]
  (get [this k default-value]
       (get m k default-value))

  (assoc [this k activator]
    (Activators. (assoc m k (as-activator activator))))

  (dissoc [this k]
          (if (contains? this k)
            (Activators. (dissoc m k))
            this))

  (keys [this]
        (keys m)))

(defn ->activators
  ([& keys-and-activators]
   (if (and (map? (first keys-and-activators))
            (= 1 (count keys-and-activators)))
     (Activators. (first keys-and-activators))
     (reduce
       (fn [as [k v]] (assoc as k v))
       (Activators. {})
       (partition 2 keys-and-activators)))))

(def empty-activators (->activators))

; Container
; ==================================

(def-map-type NotFoundContainer []
  (get [this k default-value]
       (if default-value
         default-value
         (throw (NoSuchElementException. (str "Cannot resolve '" k "'")))))

  (assoc [this key value]
    (throw (UnsupportedOperationException.)))

  (dissoc [this key]
          (throw (UnsupportedOperationException.)))
  (keys [this]
        #{}))

(def not-found-container
  (NotFoundContainer.))

(defn activate* [k activator container]
  (try
    (activate activator container)
    (catch Exception e
      (throw (RuntimeException. (str "Failed to instantiate key '" k "' "
                                     "using activator " (class activator) " " activator "-\n    "
                                     (.getMessage e))
                                e)))))

(def-map-type Container [parent-container activators cache-atom]
  (get [this k default-value]
       (locking cache-atom
         (if (contains? (:instances @cache-atom) k)
           (get-in @cache-atom [:instances k])
           (let [value (if-let [activator (get activators k)]
                         (activate* k activator this)
                         (get parent-container k default-value))]
             (swap! cache-atom (fn [m]
                                 (-> m
                                     (assoc-in [:instances k] value)
                                     (assoc :creation-order (cons k (:creation-order m))))))
             value))))

  (assoc [this key value]
    (throw (UnsupportedOperationException. "assoc and dissoc are not yet defined for a Container")))

  (dissoc [this key]
          (throw (UnsupportedOperationException. "assoc and dissoc are not yet defined for a Container")))

  (keys [this]
        (set (concat (keys parent-container) (keys activators))))

  AutoCloseable
  (close [this]
         (locking cache-atom
           (doseq [k (:creation-order @cache-atom)]
             (when (contains? activators k)
               (let [activator         (k activators)
                     realised-instance (get-in @cache-atom [:instances k])]
                 (try
                   (close activator realised-instance)
                   (catch Exception e
                     (throw (RuntimeException. (str "Could not close instance " realised-instance " "
                                                    "for key " k "' "
                                                    "using activator " (class activator) " " activator "- "
                                                    (.getMessage e))
                                               e))))))))))

(defn ->container
  ([activators]
   (->container not-found-container activators))
  ([parent-container activators]
   (assert (map? parent-container) (str "Not a map " parent-container))
   (assert (map? activators) (str "Not a map " activators))
   (Container. parent-container activators (atom {:instances {}}))))
