package org.dbpedia.extraction.destinations

/**
 * The quads generated by the DBpedia framework are organized in a number of datasets.
 * TODO: remove this class? It has no real purpose, it's a just a string holder.
 */
class Dataset(val name: String, val description: String)
{
  def this(name: String)
  {
    this(name, null)
  }

  override def toString = name

  override def hashCode = name.hashCode
  
  override def equals(other : Any) = other match {
    case that: Dataset => (this.name == that.name)
    case _ => false
  }
}
