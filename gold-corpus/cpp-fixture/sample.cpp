namespace geo {
class Shape {
public:
    virtual double area() const;
    Shape* clone() const;
};
class Circle : public Shape {
    double radius;
public:
    double area() const override;
};
}
int compute(int x) {
    return x * 2;
}
int main() {
    int n = compute(21);
    return 0;
}
