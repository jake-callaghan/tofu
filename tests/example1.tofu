class Main {

}

class ClassA {
	
	def bar() : Void = {
		var local_A : ClassA;
	}

	def abc(d : Char) : String = {}
}

class ClassB extends ClassA {
	
	var x : Int; 
	var y : Int; 
	var z : Int;

	def foo(a : Int, b : Boolean) : Int = {
		var localA : Int;
		var localB : ClassA;
		while (x) {
			localA = localB.getValue() + localA;
			localB = localB.update(1000);
		};
		return localB;
	}

}