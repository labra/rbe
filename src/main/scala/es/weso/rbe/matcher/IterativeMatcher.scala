package es.weso.rbe.matcher
import util._
import es.weso.typing.PosNegTyping
import es.weso.utils.ConsoleDebugger
import es.weso.validating._
import es.weso.collection.Bag
import es.weso.utils.SeqUtils._
import es.weso.utils.TryUtils._
import es.weso.rbe._

case class IterativeMatcher[Edge,Node,Label](
    schema: Schema[Edge,Node,Label],
    graph: Graph[Edge,Node]
    ) extends Matcher[Edge,Node,Label] {
  
    // These types are specialized versions of the general types for readability
  type RBE_ = Rbe[(DirectedEdge[Edge],NodeShape[Label,Node])] 
  type Table_ = Table[Edge,Node,Label]
  type Schema_ = Schema[Edge,Node,Label]
  type Arc_ = (Node,Edge,Node)
  type Neigh_ = Neigh[Edge,Node]
  type Neighs_ = Seq[Neigh_]
  type Candidate_ = Candidate[Edge,Node,Label]
  type Candidates_ = Seq[Candidate_]
  type Typing_ = PosNegTyping[Node,Label]
  type SingleResult_ = (Typing_,Set[(Node,Edge,Node)])
  type Result_ = Try[Seq[SingleResult_]]
  type Graph_ = Graph[Edge,Node]
  type Check_ = Checked[Node,ConstraintReason,ConstraintError[Node]]
    
  /**
   * Given a label create a table of candidates
   */
  def mkTable(
      shape: SingleShape[DirectedEdge[Edge],Node,Label]
     ): Try[(Table_, Rbe[ConstraintRef])] = {
     val (table,rbe) = mkTableAux(shape.rbe,Table.empty)
     ConsoleDebugger.debugStep(s"Table created: table = $table\nrbe = $rbe")
     Success((table,rbe)) 
  }
  
  private def mkTableAux(
      rbe: RBE_,
      current: Table_): (Table_,Rbe[ConstraintRef]) = {
    rbe match {
      case Empty => (current, Empty)
      case Symbol((p,c),m,n) => {
        val newElem = current.elems + 1
        val cref = ConstraintRef(newElem)
        val newTable = current.copy(
          elems = newElem,
          constraints = current.constraints + (cref -> c),
          edges = current.addEdge(p,cref)
         )
         (newTable,Symbol(cref,m,n))
      }
       case And(s1,s2) => {
         val (t1,r1) = mkTableAux(s1,current)
         val (t2,r2) = mkTableAux(s2,t1)
         (t2, And(r1,r2))
       }
       case Or(s1,s2) => {
         val (t1,r1) = mkTableAux(s1,current)
         val (t2,r2) = mkTableAux(s2,t1)
         (t2, Or(r1,r2))
       }
       case Plus(s) => {
         val (t,r) = mkTableAux(s,current)
         (t, Plus(r))
       }
       case Star(s) => {
         val (t,r) = mkTableAux(s,current)
         (t, Star(r))
       }
       case Repeat(s,n,m) => {
         val (t,r) = mkTableAux(s,current)
         (t, Repeat(r,n,m))
       }
       case _ => throw SESchemaException(s"mkTableAux: Unsupported rbe: $rbe")
    }
  }
  
  private def mkArc[Edge](
      directedEdge: DirectedEdge[Edge],
      n1: Node, n2: Node): (Node,Edge,Node) = {
    directedEdge match {
      case DirectEdge(edge) => (n1,edge,n2)
      case InverseEdge(edge) => (n2,edge,n1)
    }
  }
  
  private def checkCandidate(
      shape: NodeShape[Label,Node], 
      node: Node, 
      edge: DirectedEdge[Edge], 
      nodeToCheck: Node, 
      c: ConstraintRef): Candidate_ = {
    ConsoleDebugger.debugStep(s"Check candidate $c with shape $shape. Edge: $edge, node: $node")
    shape match {
      case Ref(label) => 
        Pending(c,nodeToCheck,label,mkArc(edge, node, nodeToCheck),edge)  
        
      case RefNot(label) => 
        PendingNot(c,nodeToCheck,label,mkArc(edge, node, nodeToCheck),edge)  
        
      case DisjRef(labels) => 
        PendingAlt(c,nodeToCheck,labels,mkArc(edge, node, nodeToCheck),edge) 
        
      case OrShape(vs) => {
        PendingOr(c,nodeToCheck,vs,mkArc(edge, node, nodeToCheck),edge)
      }
        
      case ConjRef(labels) => 
        PendingSeq(c,nodeToCheck,labels,mkArc(edge, node, nodeToCheck),edge) 
        
      case p: Pred[Node] => {
        ConsoleDebugger.debugStep(s"Checking condition with node $nodeToCheck and predicate ${p.name}")
        p.pred(nodeToCheck).fold(
            (x: NDResponse[Node,ConstraintReason]) => {
             ConsoleDebugger.debugStep(s"Condition satisfied with node $x")
             Pos(c,mkArc(edge, node, nodeToCheck),edge)
            },
            (es:Seq[ConstraintError[Node]]) => {
             ConsoleDebugger.debugStep(s"Condition failed on $nodeToCheck with predicate ${p.name}. Error = $es")
             Neg(c,mkArc(edge, node, nodeToCheck),edge,es)  // TODO: Check how to integrate error messages 
            }
        )
      } 
      case _ => throw SESchemaException(s"testCandidate: unknown shape $shape")
    }
  }
  
  private def lookupEdgeConstraints(table: Table_, directedEdge: DirectedEdge[Edge]): Seq[ConstraintRef] = {
    table.edges.get(directedEdge).getOrElse(Set()).toSeq
  }
  
  private def lookupConstraintShape(table: Table_, c: ConstraintRef): NodeShape[Label,Node] = {
    table.constraints.get(c) match {
      case None => throw SESchemaException(s"Cannot find constraintRef $c in table $table")
      case Some(ns) => ns
    }
  }
  
  def possibleCandidates(table: Table_, node: Node, neigh: Neigh_): Candidates_ = {
    val edge = neigh.directedEdge
    val nodeToCheck = neigh.node
    lookupEdgeConstraints(table, edge).map(c => 
      checkCandidate(lookupConstraintShape(table, c),node,edge,nodeToCheck,c)
    )
  }
  
  /**
   * Calculates the candidates of a node
   *
   * @param table Shape table
   * @param node node to calculate candidates
   * @param neighs neighbours of a node
   */
  def candidates(table: Table_, node: Node, neighs: Neighs_): Seq[Seq[Candidate_]] = {
    neighs.map(possibleCandidates(table,node,_))
  }
  
  def filterCandidates(
      table: Table_, 
      out: Neighs_,
      node: Node,
      Rbe: Rbe[ConstraintRef],
      open:Boolean, 
      extras: Seq[DirectedEdge[Edge]]): Seq[Candidates_] = {
    ConsoleDebugger.debugStep(s"filterCandidates: out = $out, Rbe = $Rbe")
    val css = zipCandidates(table,node,out)
    ConsoleDebugger.debugStep(s"zip candidates: $css") 
    css.filter(cs => matchCandidateRbe(cs,Rbe,open,extras))
  }
  
  /*
  private def pending(cs: Seq[Candidate_]): Seq[(Node,Label)] = {
    cs.filter(_.isPending).
       map{ case c => c match {
        case Pending(c,n,l,arc,_) => (n,l)
        case PendingSeq(c,n,ls,arc,_) => ??? // TODO: ls.map(l => (n,l))
        case _ => throw SESchemaException(s"pending: Unexpected value: $c")
       }}
  }
  
  private def pendings(css: Seq[Candidates_]): Seq[(Node,Label)] = {
    css.map(cs => pending(cs)).flatten
  } */
  
  private def matchCandidateRbe(cs: Seq[Candidate_], 
      rbe: Rbe[ConstraintRef], 
      open: Boolean,
      extras: Seq[DirectedEdge[Edge]]): Boolean = {
    ConsoleDebugger.debugStep(s"--Checking candidate: $cs with sorbe: $rbe. Bag: ${candidatesToBag(cs)}. Interval: ${rbe.interval(candidatesToBag(cs))}")
    val b = !containsContradictions(cs,extras) && 
            rbe.containsWithRepeats(candidatesToBag(cs),open)
    ConsoleDebugger.debugStep(s"--Result of checking candidate $cs with $rbe = $b")
    b
  }
  
  // TODO: The following code could be optimized using some mathematical formula
  // A contradiction appears when a value N has sign -1 and another value N has sign +1
  // allow contradictions if the predicate belongs to EXTRAs
  private def containsContradictions(
      cs: Seq[Candidate_],
      extras: Seq[DirectedEdge[Edge]]): Boolean = {
    val noExtras = cs.filter(c => !(extras contains c.edge))
    val pos = noExtras.filter(_.sign == 1).map(_.value)
    val neg = noExtras.filter(_.sign == -1).map(_.value)
    //    pos.intersect(neg).length != 0
    neg.length != 0
  }
  
  // TODO: It ignores extra predicates (value None) and negative candidates by now
  private def candidatesToBag(cs: Seq[Candidate_]): Bag[ConstraintRef] = {
    Bag.toBag(cs.filter(_.sign == 1).map(_.value))
  }

  def zipCandidates(table: Table_, node: Node, out: Neighs_): Seq[Seq[Candidate_]] = {
    zipN(candidates(table,node, out))
  }
  
  private def calculateCandidates(
      table: Table_, 
      out: Neighs_, 
      rbe: Rbe[ConstraintRef],
      node: Node,
      open: Boolean,
      extras: Seq[DirectedEdge[Edge]]): Try[Seq[Candidates_]] = {
    Try{
     filterCandidates(table,out,node,rbe,open,extras) 
    }
  }
  
  private def combineTypings(t1: Typing_, t2: Typing_): Try[Typing_] = {
    t1.combine(t2)
  }
  /**
   * Resolve candidate
   *
   * @param n node to resolve
   * @param g graph
   * @param c Candidate
   * @param rest current result
   */
  private def resolveCandidate(
      n: Node)(
      c: Candidate_,
      rest: Result_ 
      ): Result_ = {
    c match {
      case Missing(_,_,_) => {
       rest 
      }
      case Pending(c,obj,label,arc,_) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInTyping(obj,label,result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      }
      
      case PendingNot(c,obj,label,arc,_) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => noMatchNodeInTyping(obj,label,result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      }
      
      case PendingSeq(c,obj,labels,arc,_) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInAllLabelsTyping(obj,labels,result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      }
      
      case PendingAlt(c,obj,labels,arc,_) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          ConsoleDebugger.debugStep(s"Pending alternatives $c, labels=$labels, results=$results")
          val rs = results.map(result => matchNodeInSomeLabelsTyping(obj,labels,result))
          ConsoleDebugger.debugStep(s"rs: $rs") 
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      }
      
      case PendingOr(c,obj,vs,arc,_) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          ConsoleDebugger.debugStep(s"Pending alternatives $c, vs=$vs, results=$results")
          val rs = results.map(result => matchNodeInSomeTyping(obj,vs,result))
          ConsoleDebugger.debugStep(s"rs: $rs") 
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult(arc,r)
        } 
      } 
      
      case Pos(ref,arc,_) => { 
        // Basic matching with no pending
        // TODO: Accumulate triples checked?
        ConsoleDebugger.debugStep(s"Basic candidate matched. Constraint: $ref, node: $n, arc: $arc" )
        addArcResult(arc,rest)
      }
      case Neg(ref,arc,_,es) => { 
        // TODO: Throw exception?
        ConsoleDebugger.debugStep(s"Neg candidate: $ref. Node: $n. Arc = $arc, violations: $es")
        rest
      }

    }
  }
  
  private def addArcResult(arc: Arc_, result: Result_): Result_ = {
    result match {
      case Success(rs) => Success(rs.map((pair) => (pair._1,pair._2 + arc)))
      case f@Failure(_) => f
    }
  }
  private def resolveCandidates(
      cs: Candidates_, 
      node: Node, 
      label: Label,
      t: Typing_): Result_ = {
    val checked : Set[(Node,Edge,Node)] = Set()
    val zero : Result_ = t.addPosType(node, label).map(t => Seq((t,checked))) 
    cs.foldRight(zero)(resolveCandidate(node))
  }
  
  // TODO: Move to utils 
  def filterSuccessSeq[A](ts: Seq[Try[Seq[A]]]): Try[Seq[Seq[A]]] = {
    filterSuccess(ts)
  }
  
  // TODO: Move to utils 
  def filterSuccessSeqFlatten[A](ts: Seq[Try[Seq[A]]]): Try[Seq[A]] = {
    filterSuccessSeq(ts).map(s => s.flatten)
  }
  
  private def resolveAllCandidates(
      css: Seq[Candidates_],
      node: Node, 
      label: Label,
      currentTyping: Typing_): Result_ = {
  val attempts : Seq[Result_] = 
    css.map(cs => resolveCandidates(cs,node,label,currentTyping))
  filterSuccessSeqFlatten(attempts)
  }
  
  
  /**
   * Matches a node with several labels in a graph 
   * Takes into account the current result typing and triples
   * Succeeds if the node matches with all the labels
   */
  private def matchNodeInAllLabelsTyping(
      node: Node, 
      labels: Seq[Label], 
      current: SingleResult_): Result_ = {
    def comb(x:Label, current: SingleResult_): Result_ = {
      matchNodeInTyping(node,x,current)
    }
    combineAll(labels,current,comb _)
  }
  
  // TODO: Check if it doesn't evaluate the whole seq 
  def passSome[A,B](ls:Seq[A],eval: A => Try[Seq[B]]): Try[Seq[B]] = {
    val maybes = ls.map(x => eval(x)).filter(x => x.isSuccess && !x.get.isEmpty).map(_.get)
    ConsoleDebugger.debugStep(s"passSome: $maybes")
    if (maybes.isEmpty) 
      Failure(throw new Exception("None of the alternatives pass"))
    else 
      Success(maybes.head)
  }
  
  /**
   * Matches a node with several labels in a graph 
   * Takes into account the current result typing and triples
   * Succeeds if the node matches with at least one of the labels
   */
  private def matchNodeInSomeLabelsTyping(
      node: Node, 
      labels: Seq[Label], 
      current: SingleResult_): Result_ = {
    ConsoleDebugger.debugStep(s"matchNodeInSomeLabels. Labels: $labels, current: $current")
    def eval(x:Label): Result_ = matchNodeInTyping(node,x,current)
    val r = passSome(labels,eval _)
    ConsoleDebugger.debugStep(s"after passSome -> $r")
    r
  }
  
 /**
   * Matches a node with several labels in a graph 
   * Takes into account the current result typing and triples
   * Succeeds if the node matches with at least one of the labels
   */
  private def matchNodeInSomeTyping(
      node: Node, 
      vs: Seq[NodeShape[Label,Node]],
      current: SingleResult_): Result_ = {
    ConsoleDebugger.debugStep(s"matchNodeInSome. vs: $vs, current: $current")
    def eval(x:NodeShape[Label,Node]): Result_ = { 
      x match {
        case Ref(label) => matchNodeInTyping(node,label,current)
        case p: Pred[Node] => { 
          val r = p.pred(node)
          if (r.isOK) Success(Seq(current))
          else 
            Success(Seq()) //Failure(throw new Exception(s"Failed predicate $p with errors ${r.errors}"))
        }
        case _ => throw new Exception(s"matchNodeInSomeTyping: Unsupported $x")
      }
    }
    val r = passSome(vs,eval _)
    ConsoleDebugger.debugStep(s"after passSome -> $r")
    r
  }

  private def matchNodeInTyping(
      node: Node,
      label: Label,
      current: SingleResult_): Result_ = {
    val shape = schema.m(label)
    shape match {
      case s : SingleShape[DirectedEdge[Edge],Node,Label] =>
        matchNodeSingleShapeInTyping(node,label,s,current)
      case _ => throw new Exception(s"matchNodeInTyping: unsupported shape $shape")
    }
  }
  /**
   * Matches a node with a label in a graph 
   * Takes into account the current result typing and visited triples
   */
  private def matchNodeSingleShapeInTyping(
      node: Node,
      label: Label,
      shape: SingleShape[DirectedEdge[Edge],Node,Label],
      current: SingleResult_): Result_ = {
    ConsoleDebugger.debugStep(s"-- Checking $node with $label which is associated with shape $shape\nCurrent: $current" )

    val currentTyping = current._1
    // If the node has already been checked, return without 
    // checking again to avoid recursion
    if (currentTyping.getLabels(node) contains label) {
      // val s : Seq[(Edge,Node)] = Seq()
      Success(Seq(current))
    }
    else {
      // TODO: Maybe, we could try again in a more dynamic setting
      val neighs = graph.neighbours(node)
      ConsoleDebugger.debugStep(s"Neighs: $neighs")
      for {
        (table, sorbe) <- mkTable(shape)
        val open = !shape.closed
        allCandidates <- {
          ConsoleDebugger.debugStep(s"neighs before calculating candidates: $neighs")
          val cs = calculateCandidates(table, neighs, sorbe, node, open, shape.extras)
          cs
        }
        newTyping <- currentTyping.addPosType(node, label)
        results <- resolveAllCandidates(allCandidates, node, label, newTyping)
      } yield {
        if (shape.closed) {
          // filter out results that don't affect all triples in neighbourhood
          // that are not part of ignored
          val extras = shape.extras
          results.filter(r => containsAllTriples(node, extras, neighs, r._2))
        } else results
      }
    }
  }

  
  /**
   * Checks that a a node doesn't match with a label in a graph 
   * Takes into account the current result typing and triples
   */
  private def noMatchNodeInTyping(
      node: Node, 
      label: Label,
      current: SingleResult_): Result_ = {
    ConsoleDebugger.debugStep(s"-- Checking that node $node doesn't match with label $label, current: $current" )

    val res = matchNodeInTyping(node,label,current)
    if (res.isFailure || res.get.isEmpty) {
      Success(Seq(current))
    } else {
      Failure(SESchemaException(s"$node matches label $label"))
    }
  }
  
  
  
  def containsAllTriples(
      node: Node, 
      extras: Seq[DirectedEdge[Edge]],
      out: Neighs_, 
      triples: Set[(Node,Edge,Node)]): Boolean = {
    def notInIgnored(neigh: Neigh_): Boolean = {
      !(schema.ignored contains neigh.directedEdge)
    }
    def notInExtras(extras: Seq[DirectedEdge[Edge]])(neigh: Neigh_): Boolean = {
      !(extras contains neigh.directedEdge)
    }
    def onlyDirect(neigh: Neigh_): Boolean = {
      neigh.isDirect 
    }
    ConsoleDebugger.debugStep(s"containsAllTriples: node: $node, out: $out, triples: $triples") 
    val outTriples : Seq[(Node,Edge,Node)] = 
      out.filter(notInIgnored).
          filter(notInExtras(extras)).
          filter(onlyDirect).  // TODO: Modify this line when allowing CLOSED ^ 
          map(_.mkTriple(node))
    ConsoleDebugger.debugStep(s"containsAllTriples: outTriples after filters: $outTriples") 
    val cond = outTriples.forall(triples contains _)
    ConsoleDebugger.debugStep(s"containsAllTriples: $cond")
    cond
  }
  
  
  override def matchNode(
      node: Node, 
      label: Label): Result_ = {
    ConsoleDebugger.debugStep(s"Matching node $node with $label\nGraph: $graph")
    matchNodeInTyping(node,label,(PosNegTyping.empty,Set()))
  }
  
  def matchNodesLabels(
      ls: Seq[(Node,Label)]): Result_ = {
    val empty : SingleResult_ = (PosNegTyping.empty,Set())
    def comb(pair:(Node,Label), current: SingleResult_): Result_ = {
      matchNodeInTyping(pair._1,pair._2,current)
    }
    ConsoleDebugger.debugStep(s"nodesLabels: ls = $ls")
    combineAll(ls,empty,comb _)
  }

  
  
}