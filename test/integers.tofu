main {

	var x : Integer;
	x = 15;
	x.add(x).print();

}

class Box extends Integer {
	var value : Integer;

	def add(int : Integer) : Integer = {
		return value.add(int);

	}

	def minus(int : Integer) : Integer = {
		value = value.minus(int);
		return value;
	}
}

(*<<
30
>>*)
