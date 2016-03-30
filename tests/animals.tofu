class Main {
	
	var tony : Dog; tony = new Dog;
	var ricky : Puppy; ricky = new Puppy;
	
	while (ricky.cuteness().isGreaterThan(0)) {
		ricky.ruinCarpet();
	};

	print(ricky.cuteness());	(* should print out 0 *)
	print(tony.greet(ricky));	(* should print out 8 *)
}

(* a class representing an 'abstract' animal *)
class Animal {
	
	var age : Integer;
	var cuteness : Integer;

	def init() : Unit = {
		age = 0;
		cuteness = 0;
	}
	
	def haveBirthday() : Unit = { age = age.add(1); }
	
	def age() : Integer = { return age; }
	
	def cuteness() : Integer = { return cuteness; }
	
	def greet(a : Animal) : Integer = { return cuteness.add(a.cuteness()); }

}

class Dog extends Animal {
	def init() : Unit = {
		age = 1;
		cuteness = 8;
	}
}

class Puppy extends Dog {

	def init() : Unit = {
		age = 0;
		cuteness = 1000;
	}

	def ruinCarpet() : Unit = {
		cuteness = cuteness.minus(250);
	}
}  
