class C { public: int z; void zz(); };
class B { public: C c; };
class A { public: B b; };
void f() {
    A a;
    a.b.c.
}
