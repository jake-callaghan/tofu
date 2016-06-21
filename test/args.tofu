main {
	var x : X;
	x.foo(1,2,3,4,5);
}

class X {

	def foo(a : Integer, b : Integer, c : Integer, d : Integer, e : Integer) : Unit = {
		e.print();
	}
}
