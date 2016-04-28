main { 
	var box : Box;
	box = new Box;
	(*box.set(3);*)
	print(box.get());
}

class Box {
	var x : Integer;

	def set(y : Integer, m : Object) : Unit = {
		x = y;
		m.isEqual(y);
	}	

	def get() : Integer = {
		return x;
	}
}

class Bag extends Box {
	var y : Integer;
	var z : Integer;

	def get() : Integer = {
		return y.add(z);
	}
}
