package org.dbpedia.extraction.mappings.rml.util

/**
  * Counter for debug purpose
  */
object Counter {

  private var counter = 0

  def increment() =
  {
    counter += 1
  }

  def print() =
  {
    println(counter)
  }

  def reset() =
  {
    counter = 0
  }
}
