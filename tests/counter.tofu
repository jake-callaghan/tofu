(* tofu/examples/counter.tofu *
 * An example program that calculates the largest power of two less than or equal to 5000, and its exponent *
 *)

class Main {
	var counter : Counter; counter = new PowerOfTwoCounter; 
	var counter2 : Counter; counter2 = new Counter;
	var x : Integer; x = 5000;

	while (counter.value().lessThanOrEqual(x)) {
		counter.increment();
		counter2.increment();
	};

	print(counter.value());
	print(counter2.value());
}

class Counter {
	var value : Integer; 
	
	def init() : Unit = {
		value = 0;
	}

	def value() : Integer = { 
		return value; 
	}
	def increment() : Unit = { 
		value = value.add(1); 
	}
}

class PowerOfTwoCounter extends Counter {
	def init() : Unit = {
		value = 1;
	}
	def increment() : Unit = { 
		value = value.multiply(2); 
	}
} 