main {}

class A {

  def one() : Integer = {
    return getX(1);
  }

  def getX(x : Integer) : Integer = {
    var y : Integer;
    y = new Integer;
    y = x;
    return y;
  }

}

class B extends A {

  def one() : Integer = {
    return 1;
  }

  def two() : Integer = {
    return one().add(one());
  }
}
