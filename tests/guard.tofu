main {
	var x : D;
	x = new D;
	while (x) {};
}

class A extends Boolean { var a : A; def method1() : Unit = {} }
class B extends A {}
class C extends B {}
class D extends C {}
