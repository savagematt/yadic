# yadic-clj

Lazy maps for Clojure. Useful as a dependency injection container, and for other purposes.

A rough port of [yadic](https://github.com/bodar/yadic.java) to Clojure, 
with thanks to Dan Bodart for the original idea and implementation.

## Usage

## Similarities to Stuart Sierra's Component

yadic is similar in some ways to [Component](https://github.com/stuartsierra/component)

The main differences are:

* Construction is lazy
* Decoration is supported as a first-class concept
* We can support components with dependencies that are not defined at compile time
* `AutoCloseable` components are closed by default, without additional coding
* Container/system is not a persistent data structure
* Less boilerplate. i.e. no `(start [this] (assoc this :some-component-key (some-component-constructor some-dependency))`
* The concepts of constructor definitions (System/Component or Activators in yadic) and a container of the 
instances created by the Activators/Components are decomplected
* Systems/Containers can be scope- we support having a application-scoped set of dependencies, which are
then used by components in an application-scoped System/Container

Finally, and most importantly, yadic is intended to be a more general library than Component. This is demonstrated by 
the drop-in replacement for Component, impemented with yadic and the drop-in replacement for Prismatic's excellent 
[Graph](https://github.com/Prismatic/plumbing) library (see section below).


## Benefits over Component

### Laziness

Lazy instantiation means we can use a complete system definition without worrying about the 
overhead of instantiating components which we will not use.

### Decoration as a first-class citizen

We find decoration to be extremely useful for separating concerns. We often build up. [Compojure](https://github.com/weavejester/compojure) middleware

### Container scopes

### Separation of construction logic from state

### Containers are not persistent data structures

## Disadvantages vs. Component

### Laziness


## Core concepts

### Activators

### Container

### Scopes

### Decoration 


## Prismatic Graph drop-in

## Component drop-in


## License

Copyright © 2015 Matthew Savage

Distributed under the MIT license http://opensource.org/licenses/MIT
