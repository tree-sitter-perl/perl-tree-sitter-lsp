class Inner {
public:
    void deep();
    int leaf;
};
class Box {
public:
    Inner inner;
    void grow();
};
void f() {
    Box b;
    b.inner.
}
