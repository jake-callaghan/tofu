main {
  var harry_potter : Book; harry_potter = new Book;
  var donald_knuth : Book; donald_knuth = new Book;
  harry_potter.init(1); donald_knuth.init(128);

  var alan : Student; alan = new ComputerScientist;
  var ada : Student; ada = new Mathematician;
  alan.init(0,false); ada.init(0,true);
}

class Book {
  var knowledge : Integer;
  def init(k : Integer) : Unit = { knowledge = k; }
  def knowledge() : Integer =    { return knowledge; }
}

class Gender {}
class Male extends Gender {}
class Female extends Gender {}

class Student {
  var iq : Integer;
  var female : Boolean;

  def gender() : Boolean = { return female; }
  def iq() : Integer = { return iq; }
  def init(iq0 : Integer, gen : Boolean) : Unit = {
    iq = iq0; female = gen;
  }

  def speak() : Unit = {}

  def read(book : Book) : Unit = {
    iq = iq.add(book.knowledge());
  }

  def isEqual(them : Student) : Boolean = {
    var sameiq : Boolean; sameiq = iq.isEqual(them.iq());
    var sameGen : Boolean; sameGen = gender().isEqual(them.gender());
    return sameiq.and(sameGen);
  }
}

class Mathematician extends Student {
  def init(iq0 : Integer, gen : Boolean) : Unit = { iq = 165; female = gen; }
}

class ComputerScientist extends Mathematician {
  var github_commits : Integer;
  def speak() : Unit = { 0.print(); 1.print(); 1.print(); 0.print(); }
  def init(iq0 : Integer, gen : Boolean) : Unit = {
    iq = 999; github_commits = 0; female = gen;
  }
  def code() : Integer = {
    github_commits = github_commits.add(1); return github_commits;
  }
}
