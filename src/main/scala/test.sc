import breeze.linalg._


def f(l: => (Unit, Int)): Unit = ()

f((println("hola"), 2))