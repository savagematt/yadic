(ns yadic.core-test
  (:require [midje.sweet :refer :all]
            [yadic.core :refer :all])
  (:import [java.util UUID ArrayList]
           [java.lang AutoCloseable]))

; Basic
; ==================================

(fact "A container calls activators to create instances"
  (let [activators (->activators :a (constantly 3))
        container  (->container activators)]
    (:a container) => 3))

(fact "You cannot add non-activators"
  (->activators :a "not an activator")
  => (throws IllegalArgumentException))

(fact "Activators can request other dependencies"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (fn [container] (* 2 (:a container))))
        container  (->container activators)]
    (:a container) => 3
    (:b container) => 6))

(fact "Containers call activators lazily"
  (let [a-created  (atom false)
        activators (->activators
                     :a (fn [_] (reset! a-created true) "A")
                     :b (fn [_] "B"))
        container  (->container activators)]
    (:b container) => "B"
    @a-created => false))

(fact "Containers cache the results of calling activators"
  (let [activators  (->activators
                      :a (fn [_] (UUID/randomUUID)))
        container   (->container activators)
        first-value (:a container)]

    (:a container) => first-value))

(fact "Containers are maps, so destructuring works"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (fn [{:keys [a]}] (* 5 a)))         ; <--- Destructuring here
        container  (->container activators)]
    (:b container) => 15))


; Activation using functions
; ==================================

(fact "fn->activator turns any function into an activator function"
  (let [activators (->activators
                     :a (constantly 3)
                     :b (constantly 5)
                     :c (-> (fn [x y] (* x y))
                            (fn->activator [:a :b])))
        container  (->container activators)]
    (:c container) => 15))

(fact "fn->activator throws exception if function arity doesn't match container keys"
  (->activators :a (fn->activator (fn [a b] nil)
                                  [:a]))
  => (throws IllegalArgumentException))

(fact "functions that aren't arity 1 (with argument expected to be a container) will be rejected"
  (->activators :a (fn [a b]))
  => (throws IllegalArgumentException))

(fact "There's a handy macro for creating functions activators"
  (-> (->activators :a (act [b] (str "hello " b))
                    :b (constantly "world"))
      (->container)
      (:a))
  => "hello world")

; Keys
; ==================================

(fact "Calling (keys container) doesn't instantiate anything"
  (let [a-created  (atom false)
        activators (->activators :a (fn [_] (reset! a-created true) "A"))
        container  (->container activators)]
    (keys container) => [:a]
    @a-created => false))

(fact "Calling (contains? container k) doesn't instantiate anything"
  (let [a-created  (atom false)
        activators (->activators :a (fn [_] (reset! a-created true) "A"))
        container  (->container activators)]
    (contains? container :a) => true
    @a-created => false))

(fact "Keys include parent container keys"
  (let [parent-container (->container (->activators :a (constantly "a")
                                                    :b (constantly "b")))
        child-container  (->container parent-container (->activators :b (constantly "b")))]
    (keys child-container) => (contains #{:a :b} :in-any-order)))

; toString
; ==================================

(fact "toString doesn't instantiate anything and only shows content that has been activated"
  (let [created          (atom #{})
        parent-container (->container (->activators :parent-activated
                                                    (fn [_] (swap! created conj :parent-activated) "parent-activated")
                                                    :parent-not-activated
                                                    (fn [_] (swap! created conj :parent-not-activated) "parent-not-activated")))
        child-container  (->container parent-container
                                      (->activators :child-activated
                                                    (fn [_] (swap! created conj :child-activated) "child-activated")
                                                    :child-not-activated
                                                    (fn [_] (swap! created conj :child-not-activated) "child-not-activated")))]
    (child-container :parent-activated)
    (child-container :child-activated)

    (str child-container)
    => (str "{:child-activated \"child-activated\", "
            ":parent-activated \"parent-activated\"}")

    @created => (contains [:parent-activated :child-activated] :in-any-order)))

; get
; ==================================

(fact "get will return default value if key is not found"
  (get (->container empty-activators) :a :some-default-value)
  => :some-default-value

  (get (-> (->container empty-activators)
           (->container empty-activators))
       :a
       :some-default-value)
  => :some-default-value)

; Destructors
; ==================================

(defn close-recording
  [close-was-called-with activator]
  (let [activator (as-activator activator)]

    (reify
      Activator
      (activate [this container]
        (activate activator container))

      (close [this instance]
        (swap! close-was-called-with conj instance)
        (close activator instance)))))

(fact "Activators can specify destructors, which are called when the container is closed"
  (let [close-was-called-with (atom [])
        activators            (->activators :a
                                            (close-recording close-was-called-with
                                              (concrete "the instance")))
        container             (->container activators)]
    (:a container) => "the instance"
    (.close container)
    @close-was-called-with => ["the instance"]))

(fact "Destructors aren't called if the instance isn't realised"
  (let [close-order (atom [])
        activators  (->activators
                      :a (close-recording close-order
                           (concrete "never-activated")))
        container   (->container activators)]
    (.close container)
    @close-order => []))

(fact "Destructors are called in reverse order of activation"
  (let [close-order (atom [])
        ks          (map (comp keyword str) (range 100))
        activators  (->> ks
                         (map (fn [k]
                                (close-recording close-order
                                  (concrete k))))
                         (zipmap ks)
                         (->activators))
        container   (->container activators)]

    (doseq [k ks]
      (get container k))

    (.close container)

    @close-order => (reverse ks)))

(fact "Destructors are called in reverse order of activation, even through dependency chains"
  (let [close-order (atom [])
        activators  (->activators :a (close-recording close-order
                                       (act [] "a"))
                                  :b (close-recording close-order
                                       (act [a] (str a "b")))
                                  :c (close-recording close-order
                                       (act [b] (str b "c")))
                                  :d (close-recording close-order
                                       (act [c] (str c "d")))
                                  :e (close-recording close-order
                                       (act [d] (str d "e"))))
        container   (->container activators)]

    (get container :b) => "ab"

    (get container :e) => "abcde"

    (.close container)

    @close-order => ["abcde" "abcd" "abc" "ab" "a"]))

(deftype ACloseableType [closed-atom]
  AutoCloseable
  (close [this]
    (reset! closed-atom true)))

(fact "Function activators that produce AutoCloseable instances will close the instance"
  (let [close-was-called (atom false)
        activators       (->activators :a (constantly (ACloseableType. close-was-called)))
        container        (->container activators)]
    (:a container) => truthy
    (.close container)
    @close-was-called => true))

(fact "Container is AutoCloseable, and will be closed when using with-open"
  (let [close-was-called-with (atom nil)
        activators            (->activators :a (reify
                                                 Activator
                                                 (activate [this container] "the instance")
                                                 (close [this instance]
                                                   (reset! close-was-called-with instance))))]

    (with-open [container (->container activators)]
      (:a container) => "the instance")

    @close-was-called-with => "the instance"))

(fact "Container only closes its own instances, not instances in the parent"
  (let [parent-instance-was-closed (atom false)
        parent-activators          (->activators :a (constantly (ACloseableType. parent-instance-was-closed)))
        parent-container           (->container parent-activators)
        child-container            (->container parent-container empty-activators)]

    (:a child-container) => truthy

    (.close child-container)

    @parent-instance-was-closed => false))

; Decoration
; ==================================

(fact "(decorate) allows us to add activators which wrap instances created by activators with the same key"
  (let [activators (-> (->activators :a (constantly "a")
                                     :b (fn [container] (str "b saw " (:a container))))
                       (decorate :a (fn [r] (str "(decorated-once " (:a r) ")")))
                       (decorate :a (fn [r] (str "(decorated-twice " (:a r) ")"))))
        container  (->container activators)]
    (:a container) => "(decorated-twice (decorated-once a))"
    (:b container) => "b saw (decorated-twice (decorated-once a))"))

(fact "You can't decorate a key which is not already in activators"
  (-> (->activators)
      (decorate :a (fn [_])))
  => (throws IllegalStateException))

(fact  {:midje/description (str "If you have a reference to the parent container, it's possible to decorate a key from "
                                "the parent container. "
                                "This type of decoration is impossible without a reference to the parent container")}

  (let [parent-activators (->activators :a (constantly "hello"))
        parent-container  (->container parent-activators)
        child-container   (->container parent-container
                                       (-> empty-activators
                                           (decorate parent-container :a
                                                     (act [a] (str a " world")))
                                           (decorate :a
                                                     (act [a] (str a "!!!")))))]

    (:a child-container) => "hello world!!!"))

(fact "Destructors still get called on decorated activators"
  (let [close-called-with (atom [])
        activators        (-> (->activators
                                :a (reify
                                     Activator
                                     (activate [this container]
                                       "hello")
                                     (close [this instance]
                                       (swap! close-called-with conj instance))))

                              (decorate :a (reify
                                             Activator
                                             (activate [this {:keys [a]}]
                                               (str a " world"))
                                             (close [this instance]
                                               (swap! close-called-with conj instance)))))
        container         (->container activators)]

    (:a container) => "hello world"

    (.close container)

    @close-called-with => ["hello world" "hello"]))


; Activation using IDeref
; ==================================

(fact "IDerefs can be used as activators"
  (let [a-promise  (promise)
        activators (->activators :atom (atom "atom")
                                 :promise a-promise
                                 :delay (delay "delay")
                                 :future (future "future")
                                 :never-returns (deref->activator (future (Thread/sleep 100000000))
                                                                  1
                                                                  "timed-out"))
        container  (->container activators)]

    (deliver a-promise "promise")

    (:atom container) => "atom"
    (:promise container) => "promise"
    (:delay container) => "delay"
    (:future container) => "future"
    (:never-returns container) => "timed-out"))

; ClassActivator
; ==================================

(fact "Class activators call constructors based on keys and the types of the resolved values of those keys"
  (let [activators  (->activators :values (constantly [1 2 3])
                                  :array-list (class->activator ArrayList [:values]))
        container   (->container activators)
        ; ArrayList has arity 1 constructors for both int and Collection.
        ; The correct one is picked based on the type of :values
        constructed (:array-list container)]

    (class constructed) => ArrayList
    constructed => [1 2 3]))

(fact "You get a helpful early exception at activator creation time if there are no constructors of the right arity"
  (class->activator ArrayList [:no :matching :constructor :for :this :many])
  => (throws IllegalArgumentException))

; starter-upper activator-
; useful if you have components without
; explicit dependencies which require
; activation in a particular order
; ==================================

(fact "starter-upper activator"
  (fact "returns a function which eagerly activates keys, in order"
    (let [activate-order (atom [])
          activators     (->activators {:a               (act [] (swap! activate-order conj :a))
                                        :b               (act [] (swap! activate-order conj :b))
                                        :c               (act [] (swap! activate-order conj :c))
                                        :never-activated (act [] (swap! activate-order conj :never-activated))
                                        :startup         (starter-upper :b :a :c)})
          container      (->container activators)]

      (let [startup (get container :startup)]

        startup => truthy

        @activate-order => []

        (startup) => [:b :a :c]

        @activate-order => [:b :a :c])))

  (fact "does not affect components that have already been activated"
    (let [activate-order (atom [])
          activators     (->activators {:a       (act [] (swap! activate-order conj :a))
                                        :b       (act [] (swap! activate-order conj :b))
                                        :startup (starter-upper :a :b)})
          container      (->container activators)]

      (let [_       (get container :b)
            startup (get container :startup)]
        (fact ":b is already activated"
          @activate-order => [:b])

        (fact "startup still returns the keys it will attempt to activate"
          (startup) => [:a :b])

        (fact "startup still activates :a"

          @activate-order => [:b :a])))))

; eagerly-instantiate
; ==================================

(fact "We can eagerly instantiate everything in a container (which rather defeats the purpose of yadic, but still...)"
  (let [activate-order (atom [])
        container      (->container (->activators {:a (act [] (swap! activate-order conj :a))}))]

    (eagerly-instantiate container)

    @activate-order => [:a]))
