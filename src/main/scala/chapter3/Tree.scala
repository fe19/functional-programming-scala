package chapter3

/*
 * learnings
 *
 */


enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match // this is a method
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

object Tree:
  def size[A](t: Tree[A]): Int = t match // this is a function
    case Leaf(_) => 1
    case Branch(l, r) =>  1 + size(l) + size(r)

