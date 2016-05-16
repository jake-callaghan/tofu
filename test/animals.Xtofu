main {
	var x : Integer;
	x = 2;
	x.print();
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
	var x : Integer;

	def init() : Unit = {
		age = 1;
		cuteness = 8;
		x = 1;
	}
}

class Puppy extends Dog {

	def init() : Unit = {
		age = 0;
		cuteness = 1000;
		x = 2;
	}

	def ruinCarpet() : Unit = {
		var y : Integer; y = 250;
		cuteness = cuteness.minus(y);
	}
}

(*<<
2
>>*)
