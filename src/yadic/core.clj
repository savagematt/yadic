(ns yadic.core
  (:require [yadic.potemkin.collections :refer [def-map-type keys*]]
            [clojure.stacktrace :refer [print-cause-trace]])
  (:import [java.util NoSuchElementException UUID]
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

; IDeref
; ----------------------------------

(deftype DerefActivator [d timeout-ms timeout-val]
  Activator
  (activate [this container]
    (if timeout-ms
      (deref d timeout-ms timeout-val)
      @d))

  (close [this instance]
    (when (instance? AutoCloseable instance)
      (.close instance)))

  Object
  (toString [this]
    (str "DerefActivator using " d)))

(defn deref->activator
  ([d] (DerefActivator. d nil nil))
  ([d timeout-ms timeout-val] (DerefActivator. d timeout-ms timeout-val)))

; Classes
; ----------------------------------

(defn has-param-count [expected]
  #(= expected (count (.getParameterTypes %))))

(defn types-match [parameter-types]
  (fn [m]
    (->> (map vector (.getParameterTypes m) parameter-types) ;zip
         (every? (fn [[needed got]]
                   (.isAssignableFrom needed got))))))

(defn get-constructor [c parameter-types]
  (->> (.getConstructors c)
       (filter (has-param-count (count parameter-types)))
       (filter (types-match parameter-types))
       first))

(deftype ClassActivator [^Class c parameter-keys]
  Activator
  (activate [this container]
    (let [parameters      (map #(get container %) parameter-keys)
          parameter-types (vec (map #(or (class %) Object) parameters))
          constructor     (get-constructor c parameter-types)]
      (when (nil? constructor)
        (throw (IllegalArgumentException.
                 (str "No constructor of " c " "
                      "takes parameter types " parameter-types " "
                      "from keys " parameter-keys))))
      (.newInstance ^Constructor constructor (to-array parameters))))

  (close [this instance]
    (when (instance? AutoCloseable instance)
      (.close instance)))

  Object
  (toString [this]
    (str c " using " parameter-keys)))

(defn class->activator
  ([^Class c] (class->activator c []))
  ([^Class c parameter-keys]
   (when-not (some (has-param-count (count parameter-keys)) (.getConstructors c))
     (throw (IllegalArgumentException.
              (str "No constructor of " c " takes " (count parameter-keys) " parameters for keys " parameter-keys))))
   (ClassActivator. c parameter-keys)))

; Concrete value
; ----------------------------------


(defn concrete [instance]
  (fn->activator (constantly instance) []))

; Conversion to activators
; ----------------------------------

(defn as-activator [x]
  (cond (fn? x) (fn-activator x)
        (satisfies? Activator x) x
        (instance? IDeref x) (deref->activator x)
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

(def empty-activators (Activators. {}))

(defn ->activators
  ([] empty-activators)
  ([m]
   (Activators. m))
  ([k activator & keys-and-activators]
   (reduce
     (fn [as [k v]] (assoc as k v))
     (assoc (Activators. {}) k activator)
     (partition 2 keys-and-activators))))

; Container
; ==================================

(defprotocol IHaveRealisedContent
  (realised-content* [this]))

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
        #{})

  IHaveRealisedContent
  (realised-content* [this] nil)

  Object
  (toString [this]
            "Empty Container"))

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
       (if (contains? (:instances @cache-atom) k)
         (get-in @cache-atom [:instances k])
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
               value)))))

  (assoc [this key value]
    (throw (UnsupportedOperationException. "assoc and dissoc are not yet defined for a Container")))

  (dissoc [this key]
          (throw (UnsupportedOperationException. "assoc and dissoc are not yet defined for a Container")))

  (keys [this]
        (set (concat (keys parent-container) (keys activators))))

  IHaveRealisedContent
  (realised-content* [this]
                     (:instances @cache-atom))

  Object
  (toString [this]
            (str (merge (realised-content* this)
                   (realised-content* parent-container))))

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

(defn- decorating-activator* [original-key decorated-key decorating-activator]
  (reify
    Activator
    (activate [this container]
      (let [activators-with-old-activator (->activators original-key (fn [_] (get container decorated-key)))
            old-activator-container       (->container container activators-with-old-activator)]

        (activate (as-activator decorating-activator) old-activator-container)))

    (close [this instance]
      (close decorating-activator instance))))

(defn- decorating-activator-from-parent* [k parent-container new-activator]
  (reify
    Activator
    (activate [this container]
      (let [activators-with-old-activator (->activators k (fn [_] (get parent-container k)))
            old-activator-container       (->container container activators-with-old-activator)]

        (activate (as-activator new-activator) old-activator-container)))

    (close [this instance]
      (close new-activator instance))))

(defn decorate
  ([activators k decorating-activator]
   (let [decorated-key (keyword (str (name k) "-" (UUID/randomUUID)))
         old-activator (activators k)]

     (when-not old-activator
       (throw (UnsupportedOperationException. (str "No activator to decorate for " k))))

     (assoc activators
       decorated-key old-activator
       k (decorating-activator*
           k
           decorated-key
           decorating-activator))))

  ([activators parent-container k decorating-activator]
   (let [old-activator (activators k)
         new-activator (decorating-activator-from-parent* k parent-container decorating-activator)]

     (when old-activator
       (throw (UnsupportedOperationException. (str "There is already an activator for " k " in activators"))))

     (assoc activators k new-activator))))
