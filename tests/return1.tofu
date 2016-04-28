Main {

}

class A {
	def foo(b : Boolean) : Integer = {
		if (b) { return 5; }
	}

	def foo2() : Integer = {
		if (1.isLessThan(2)) {
			
		} else {
			return 5;
		}
	}

	def foo3() : Integer = {
		while (1.isLessThan(0)) {
			return 5;
		}
	}
}
