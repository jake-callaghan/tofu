main {
  var b : B;
  b.init();

  var a : A;

}

class A {
  var x : X;

  def init() : Unit = {
    b = true;
    var t : Integer;
    t = 1;
  }

  def getX() : X = { return x; }

  var b : Boolean;
}

class B extends A {

  def init() : Unit = {
    b = false;
  }

  def foo() : Unit = {
      x.print();
      x = new X;
      var y : Integer;
      y = 99;
  }
}

class X extends Integer {
  def foo() : Unit = {
      10.print();
  }
}
