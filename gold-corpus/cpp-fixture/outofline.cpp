namespace ns {
class Widget {
public:
    int area();
    void draw();
};
}
int ns::Widget::area() { return 0; }
void ns::Widget::draw() {}
