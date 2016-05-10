main {
  var x : Integer;
  x = 100;
  x.print();
}

class A {
  var x : Integer;
  def addTwice(y : Integer) : Integer = {
    return x.add(y).add(y);
  }
}
