main {}
class A {}
class B extends A {}
class C extends B {}
class D extends C {}
class E extends X {} (* -> X is undefined *)
