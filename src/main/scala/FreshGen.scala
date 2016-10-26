package dwit

object FreshGen {
  import Syntax._

  private var typeId: TypeId = 0
  private var holeId: HoleId = 0

  // Generate a fresh hole with a unique ID
  def freshHole(typ: Type): VHole = {
    holeId += 1;
    VHole(holeId, typ)
  }

  // Generate a fresh type with a unique ID
  def freshType(): TAlpha = {
    typeId += 1;
    val res = TAlpha(typeId)
    res
  }

  def reset(): Unit = {
    typeId = 0
    holeId = 0
  }
}
