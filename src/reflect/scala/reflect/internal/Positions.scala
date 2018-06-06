package scala
package reflect
package internal

import util._
import scala.collection.mutable.ListBuffer

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
          max = Math.max(min, pos.end)
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

  final def childSolidDescendants(tree: Tree): Array[Tree] =
    validatePositionsTL.get.buildChildSolidDescendants(tree)

  def validatePositions(tree: Tree):Unit = {
    if (!useOffsetPositions && !isPastTyper)
      validatePositionsTL.get.check(tree)
  }
  private object validatePositionsTL extends ThreadLocal[ValidatePositions] {
    override def initialValue() = new ValidatePositions
  }
  private class ValidatePositions extends Traverser {
    private[this] var topTree: Tree = _
    private[this] var trace = false

    def check(tree: Tree) {
      trace = settings.Yposdebug && (settings.verbose || settings.Yrangepos)
      topTree = tree
      validate(tree, tree)
    }

    private[this] def reportTree(prefix: String, tree: Tree) {
      val source = if (tree.pos.isDefined) tree.pos.source else ""
      inform("== " + prefix + " tree [" + tree.id + "] of type " + tree.productPrefix + " at " + tree.pos.show + source)
      inform("")
      inform(treeStatus(tree))
      inform("")
    }

    private[this] def positionError(msg: String)(body: => Unit) {
      inform("======= Position error\n" + msg)
      body
      inform("\nWhile validating #" + topTree.id)
      inform(treeStatus(topTree))
      inform("\nChildren:")
      topTree.children foreach (t => inform("  " + treeStatus(t, topTree)))
      inform("=======")
      throw new ValidateException(msg)
    }

    def validate(tree: Tree, encltree: Tree): Unit = {

      if (!tree.isEmpty && tree.canHaveAttrs) {
        val treePos = tree.pos
        if (trace)
          inform("[%10s] %s".format("validate", treeStatus(tree, encltree)))

        if (!treePos.isDefined)
          positionError("Unpositioned tree #" + tree.id) {
            inform("%15s %s".format("unpositioned", treeStatus(tree, encltree)))
            inform("%15s %s".format("enclosing", treeStatus(encltree)))
            encltree.children foreach (t => inform("%15s %s".format("sibling", treeStatus(t, encltree))))
          }
        val childSolidDescendants = buildChildSolidDescendants(tree)
        if (treePos.isRange) {
          val enclPos = encltree.pos
          if (!enclPos.isRange)
            positionError("Synthetic tree [" + encltree.id + "] contains nonsynthetic tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }
          if (!(enclPos includes treePos))
            positionError("Enclosing tree [" + encltree.id + "] does not include tree [" + tree.id + "]") {
              reportTree("Enclosing", encltree)
              reportTree("Enclosed", tree)
            }

          overlaps(childSolidDescendants) match {
            case List() => ;
            case xs => {
              positionError("Overlapping trees " + xs.map { case (x, y) => (x.id, y.id) }.mkString("", ", ", "")) {
                reportTree("Ancestor", tree)
                for ((x, y) <- xs) {
                  reportTree("First overlapping", x)
                  reportTree("Second overlapping", y)
                }
              }
            }
          }
        }
        for (ct <- childSolidDescendants) validate(ct, tree)
      }
    }

    /** A free range from `lo` to `hi` */
    private[this] def free(lo: Int, hi: Int): Range =
      Range(Position.range(null, lo, lo, hi), EmptyTree)

    /** The maximal free range list */
    private[this] val maxFree: List[Range] = free(0, Int.MaxValue) :: Nil

    /** Adds a singleton list of a non-empty range from `lo` to `hi`, or else the empty List */
    private[this] def addMaybeFree(lo: Int, hi: Int, tail: List[Range]) =
      if (lo < hi) free(lo, hi) :: tail
      else tail

    /** Insert `pos` into ranges `rs` if possible;
      *  otherwise add conflicting trees to `conflicting`.
      */
    private[this] def insert(rs: List[Range], t: Tree, conflicting: ListBuffer[Tree]): List[Range] = rs match {
      case List() =>
        assert(conflicting.nonEmpty)
        rs
      case r :: rs1 =>
        val tPos = t.pos
        val rPos = r.pos
        assert(!tPos.isTransparent)
        if (r.isFree && (rPos includes tPos)) {
          //      inform("subdividing "+r+"/"+tPos)
          addMaybeFree(tPos.end, rPos.end, Range(tPos, t) :: addMaybeFree(rPos.start, tPos.start, rs1) )
        } else {
          if (!r.isFree && (rPos overlaps tPos)) conflicting += r.tree
          r :: insert(rs1, t, conflicting)
        }
    }

    //Traverser  functionallity for descendants
    private[this] val traverserResult = Array.newBuilder[Tree]
    private[this] val emptyTree = new Array[Tree](0)
    //just because there isnt yet a traverserResult.isEmpty
    private[this] var traverserResultIsEmpty = true

    // don't traverse annotations
    override def traverseModifiers(mods: Modifiers): Unit = ()

    override def traverse(tree: Tree): Unit =
      if (tree ne EmptyTree) {
        if (tree.pos.isTransparent) super.traverse(tree)
        else {
          traverserResult += tree
          traverserResultIsEmpty = false
        }
      }

    def buildChildSolidDescendants(tree: Tree) = {
      super.traverse(tree)
      if (traverserResultIsEmpty) emptyTree else {
        traverserResultIsEmpty = true

        val r = traverserResult.result()
        traverserResult.clear()
        r
      }
    }

    // overlaps


    private[this] val overlapConflicting = new ListBuffer[Tree]

    /** Does given list of trees have mutually non-overlapping positions?
      * pre: None of the trees is transparent
      */
    private[this] def overlaps(cts: Array[Tree]): List[(Tree, Tree)] = {
      // manually unrolled to avoid ObjectRefs and unneeded ListBuffers
      var ranges: List[Range] = maxFree
      var idx = 0
      while (idx < cts.length) {
        val ct = cts(idx)
        idx += 1
        if (ct.pos.isOpaqueRange) {
          ranges = insert(ranges, ct, overlapConflicting)
          if (overlapConflicting.nonEmpty) {
            val res = overlapConflicting.toList map (t => (t, ct))
            overlapConflicting.clear()
            return res
          }
        }
      }
      Nil
    }
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

  case class Range(pos: Position, tree: Tree) {
    def isFree = tree == EmptyTree
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
