/**
 * Copyright (C) 2015 Typesafe Inc. <http://www.typesafe.com>
 */
package akka.util

import language.higherKinds

/**
 * An immutable multi-map that expresses the value type as a type function of the key
 * type. Create it with a type constructor that expresses the relationship:
 *
 * {{{
 * trait Key { type Type = T }
 * case class MyValue[T](...)
 *
 * // type function from Key to MyValue
 * type KV[K <: Key] = MyValue[K#Type]
 *
 * val map = TypedMultiMap.empty[Key, KV]
 *
 * // a plain Int => String map would use this function:
 * type plain[K <: Int] = String
 *
 * // a map T => T would use this function:
 * type identity[T <: AnyRef] = T
 * }}}
 *
 * Caveat: using keys which take type parameters does not work due to conflicts
 * with the existential interpretation of `Key[_]`. A workaround is to define
 * a key type like above and provide a subtype that provides its type parameter
 * as type member `Type`.
 */
class TypedMultiMap[T <: AnyRef, K[_ <: T]] private (map: Map[T, Set[Any]]) {

  def keySet: Set[T] = map.keySet

  def inserted(key: T)(value: K[key.type]): TypedMultiMap[T, K] = {
    val set = map.get(key).getOrElse(Set.empty)
    new TypedMultiMap[T, K](map.updated(key, set + value))
  }

  def get(key: T): Set[K[key.type]] =
    map.get(key).asInstanceOf[Option[Set[K[key.type]]]].getOrElse(Set.empty)

  def valueRemoved(value: Any): TypedMultiMap[T, K] = {
    val s = Set(value)
    val m = map.collect {
      case (k, set) if set != s ⇒ (k, set - value)
    }
    new TypedMultiMap[T, K](m)
  }

  def removed(key: T)(value: K[key.type]): TypedMultiMap[T, K] = {
    map get key match {
      case None ⇒ this
      case Some(set) ⇒
        if (set(value)) {
          val newset = set - value
          val newmap = if (newset.isEmpty) map - key else map.updated(key, newset)
          new TypedMultiMap[T, K](newmap)
        } else this
    }
  }
}

object TypedMultiMap {
  def empty[T <: AnyRef, K[_ <: T]]: TypedMultiMap[T, K] = new TypedMultiMap[T, K](Map.empty)
}