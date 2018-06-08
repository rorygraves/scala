package scala
package reflect
package internal

import util._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/** Handling range positions
 *  atPos, the main method in this trait, will add positions to a tree,
 *  and will ensure the following properties:
 *
 *    1. All nodes between the root of the tree and nodes that already have positions
 *       will be assigned positions.
 *    2. No node which already has a position will be assigned a different range; however
 *       a RangePosition might become a TransparentPosition.
 *    3. The position of each assigned node includes the positions of each of its children.
 *    4. The positions of all solid descendants of children of an assigned node
 *       are mutually non-overlapping.
 *
 * Here, the solid descendant of a node are:
 *
 *   If the node has a TransparentPosition, the solid descendants of all its children
 *   Otherwise, the singleton consisting of the node itself.
 */
trait Positions extends api.Positions { self: SymbolTable =>
  type Position = scala.reflect.internal.util.Position
  val NoPosition = scala.reflect.internal.util.NoPosition
  implicit val PositionTag = ClassTag[Position](classOf[Position])

  def useOffsetPositions: Boolean = true

  /** A position that wraps a set of trees.
   *  The point of the wrapping position is the point of the default position.
   *  If some of the trees are ranges, returns a range position enclosing all ranges
   *  Otherwise returns default position that is either focused or not.
   */
  def wrappingPos(default: Position, trees: List[Tree]): Position = wrappingPos(default, trees, focus = true)
  def wrappingPos(default: Position, trees: List[Tree], focus: Boolean): Position = {
    if (useOffsetPositions) default else {
      var rest = trees
      var min = Int.MaxValue
      var max = Int.MinValue
      while (rest ne Nil) {
        val head = rest.head
        rest = rest.tail
        val pos = head.pos
        if (pos.isRange) {
          min = Math.min(min, pos.start)
          max = Math.max(max, pos.end)
        }
      }
      if (min > max)
      //there are no ranges
        if (focus) default.focus else default
      else Position.range(default.source, min, default.point, max)
    }
  }

  /** A position that wraps the non-empty set of trees.
   *  The point of the wrapping position is the point of the first trees' position.
   *  If some of the trees are non-synthetic, returns a range position enclosing the non-synthetic trees
   *  Otherwise returns a synthetic offset position to point.
   */
  def wrappingPos(trees: List[Tree]): Position = {
    val headpos = trees.head.pos
    if (useOffsetPositions || !headpos.isDefined) headpos
    else wrappingPos(headpos, trees)
  }

  /** Ensure that given tree has no positions that overlap with
   *  any of the positions of `others`. This is done by
   *  shortening the range, assigning TransparentPositions
   *  to some of the nodes in `tree` or focusing on the position.
   */
  def ensureNonOverlapping(tree: Tree, others: List[Tree]){ ensureNonOverlapping(tree, others, focus = true) }
  def ensureNonOverlapping(tree: Tree, others: List[Tree], focus: Boolean) {
    if (useOffsetPositions) return

    def isOverlapping(pos: Position) =
      pos.isRange && (others exists (pos overlaps _.pos))

    val treePos = tree.pos
    if (isOverlapping(treePos)) {
      val children = tree.children
      children foreach (ensureNonOverlapping(_, others, focus))
      if (treePos.isOpaqueRange) {
        val wpos = wrappingPos(treePos, children, focus)
        tree setPos (if (isOverlapping(wpos)) treePos.makeTransparent else wpos)
      }
    }
  }

  def rangePos(source: SourceFile, start: Int, point: Int, end: Int): Position =
    if (useOffsetPositions) Position.offset(source, point)
    else Position.range(source, start, point, end)


  abstract class ChildSolidDescendantsCollector extends Traverser {
    // don't traverse annotations
    override def traverseModifiers(mods: Modifiers): Unit = ()

    override def traverse(tree: Tree): Unit =
      if (tree ne EmptyTree) {
        if (tree.pos.isTransparent) super.traverse(tree)
        else {
          traverseSolidChild(tree)
        }
      }
    def traverseSolidChild(t: Tree): Unit
    def apply(t: Tree): Unit = super.traverse(t)
  }

  private[this] def reportTree(prefix: String, tree: Tree) {
    val source = if (tree.pos.isDefined) tree.pos.source else ""
    inform("== " + prefix + " tree [" + tree.id + "] of type " + tree.productPrefix + " at " + tree.pos.show + source)
    inform("")
    inform(treeStatus(tree))
    inform("")
  }

  private[this] def positionError(topTree: Tree, msg: String)(body: => Unit) {
    inform("======= Position error\n" + msg)
    body
    inform("\nWhile validating #" + topTree.id)
    inform(treeStatus(topTree))
    inform("\nChildren:")
    topTree.children foreach (t => inform("  " + treeStatus(t, topTree)))
    inform("=======")
    throw new ValidateException(msg)
  }

  private val posStartOrdering: Ordering[Tree] = new Ordering[Tree] {
    override def compare(x: Tree, y: Tree): Int = {
      def posOf(t: Tree): Int = {
        val pos = t.pos
        if (t.pos == NoPosition) Int.MinValue else t.pos.start
      }
      Integer.compare(posOf(x), posOf(y))
    }
  }
  def validatePositions(tree: Tree): Unit = if (!isPastTyper && !useOffsetPositions) {
    val trace = settings.Yposdebug && (settings.verbose || settings.Yrangepos)
    val topTree = tree
    val childSolidDescendantBuffer = collection.mutable.ArrayBuffer[Tree]()
    val solidChildrenCollector = new ChildSolidDescendantsCollector {
      def traverseSolidChild(t: Tree): Unit = {
        childSolidDescendantBuffer += t
      }
    }

    def loop(tree: Tree, encltree: Tree) {
      if (!tree.isEmpty && tree.canHaveAttrs) {
        val treePos = tree.pos
        if (trace)
          inform("[%10s] %s".format("validate", treeStatus(tree, encltree)))

        if (!treePos.isDefined)
          positionError(topTree, "Unpositioned tree #" + tree.id) {
            inform("%15s %s".format("unpositioned", treeStatus(tree, encltree)))
            inform("%15s %s".format("enclosing", treeStatus(encltree)))
            encltree.children foreach (t => inform("%15s %s".format("sibling", treeStatus(t, encltree))))
          }
        solidChildrenCollector(tree)
        val childSolidDescendants: Array[Tree] = childSolidDescendantBuffer.toArray

        if (treePos.isRange) {
          val enclPos = encltree.pos
          if (!enclPos.isRange)
            positionError(topTree, "Synthetic tree [" + encltree.id + "] contains nonsynthetic tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }
          if (!(enclPos includes treePos))
            positionError(topTree, "Enclosing tree [" + encltree.id + "] does not include tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }

          if (childSolidDescendants.length > 1) {
            scala.util.Sorting.quickSort(childSolidDescendants)(posStartOrdering)
            var t1 = childSolidDescendants(0)
            var i = 1
            while (i < childSolidDescendants.length) {
              val t2 = childSolidDescendants(i)
              if (t1.pos.overlaps(t2.pos)) {
                reportTree("First overlapping", t1)
                reportTree("Second overlapping", t2)
              }
              if (t2.pos.isRange)
                t1 = t2
              i += 1
            }
          }
        }
        childSolidDescendantBuffer.clear()
        for (ct <- childSolidDescendants) {
          loop(ct, tree)
        }
      }
    }
    loop(tree, tree)
  }

  /** Set position of all children of a node
   *  @param  pos   A target position.
   *                Uses the point of the position as the point of all positions it assigns.
   *                Uses the start of this position as an Offset position for unpositioned trees
   *                without children.
   *  @param  trees  The children to position. All children must be positionable.
   */
  private def setChildrenPos(pos: Position, trees: List[Tree]): Unit = try {
    for (tree <- trees) {
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        val children = tree.children
        if (children.isEmpty) {
          tree setPos pos.focus
        } else {
          setChildrenPos(pos, children)
          tree setPos wrappingPos(pos, children)
        }
      }
    }
  } catch {
    case ex: Exception =>
      inform("error while set children pos "+pos+" of "+trees)
      throw ex
  }


  class ValidateException(msg : String) extends Exception(msg)


  /** A locator for trees with given positions.
   *  Given a position `pos`, locator.apply returns
   *  the smallest tree that encloses `pos`.
   */
  class Locator(pos: Position) extends Traverser {
    var last: Tree = _
    def locateIn(root: Tree): Tree = {
      this.last = EmptyTree
      traverse(root)
      this.last
    }
    protected def isEligible(t: Tree) = !t.pos.isTransparent
    override def traverse(t: Tree) {
      t match {
        case tt : TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
          traverse(tt.original)
        case _ =>
          if (t.pos includes pos) {
            if (isEligible(t)) last = t
            super.traverse(t)
          } else t match {
            case mdef: MemberDef =>
              val annTrees = mdef.mods.annotations match {
                case Nil if mdef.symbol != null =>
                  // After typechecking, annotations are moved from the modifiers
                  // to the annotation on the symbol of the annotatee.
                  mdef.symbol.annotations.map(_.original)
                case anns => anns
              }
              traverseTrees(annTrees)
            case _ =>
          }
      }
    }
  }

  class TypedLocator(pos: Position) extends Locator(pos) {
    override protected def isEligible(t: Tree) = super.isEligible(t) && t.tpe != null
  }

  trait PosAssigner extends Traverser {
    var pos: Position
  }
  protected[this] lazy val posAssigner: PosAssigner = new DefaultPosAssigner

  protected class DefaultPosAssigner extends PosAssigner {
    var pos: Position = _
    override def traverse(t: Tree) {
      if (!t.canHaveAttrs) ()
      else if (t.pos == NoPosition) {
        t.setPos(pos)
        super.traverse(t)   // TODO: bug? shouldn't the traverse be outside of the if?
        // @PP: it's pruning whenever it encounters a node with a
        // position, which I interpret to mean that (in the author's
        // mind at least) either the children of a positioned node will
        // already be positioned, or the children of a positioned node
        // do not merit positioning.
        //
        // Whatever the author's rationale, it does seem like a bad idea
        // to press on through a positioned node to find unpositioned
        // children beneath it and then to assign whatever happens to
        // be in `pos` to such nodes. There are supposed to be some
        // position invariants which I can't imagine surviving that.
      }
    }
  }

  /** Position a tree.
   *  This means: Set position of a node and position all its unpositioned children.
   */
  def atPos[T <: Tree](pos: Position)(tree: T): T = {
    if (useOffsetPositions || !pos.isOpaqueRange) {
      posAssigner.pos = pos
      posAssigner.traverse(tree)
      tree
    }
    else {
      if (!tree.isEmpty && tree.canHaveAttrs && tree.pos == NoPosition) {
        tree.setPos(pos)
        val children = tree.children
        if (children.nonEmpty) {
          if (children.tail.isEmpty) atPos(pos)(children.head)
          else setChildrenPos(pos, children)
        }
      }
      tree
    }
  }
}
