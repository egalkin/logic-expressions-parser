package analyzer

import java.io.File
import java.util.StringJoiner

import guru.nidi.graphviz.engine.{Format, Graphviz}


object Tree {

  def writeCodeToFile(code: String): String = {
    Graphviz.fromString(code).render(Format.PNG).toFile(new File("plots/plot.png"))
    code
  }

  def treeToDotCode(root: Tree): String = {
    val code = StringBuilder.newBuilder
    code.append("digraph G {\n")
    code.append(0 + " [label=\"" + root.node + "\"];\n")
    treeToDotCode(root, code, 0, 1)
    code.append("}\n")
    writeCodeToFile(code.toString())
  }


  def treeToDotCode(root: Tree, code: StringBuilder, parent: Int, newVertNum: Int): Int = {
    var curVertNum = newVertNum
    for (son <- root.children) {
      code.append(curVertNum + " [label=\"" + son.node + "\"];\n")
      code.append(parent + "->" + curVertNum + ";\n")
      curVertNum = treeToDotCode(son, code, curVertNum, curVertNum + 1)
    }
    curVertNum
  }
}

case class Tree(node: String, children: List[Tree] = Nil) {
  def visualize: String = {
    val tree = new StringJoiner("")
    tree.toString
  }
}
