#define aTHX_ my_ctx,
#define wrap(x) realFunc(x)
#define newThing(x) Perl_newThing(aTHX_ x)
int realFunc(int x) { return x; }
int Perl_newThing(void* c, int x) { return x; }
void g(void) {
    int a = wrap(5);
    int b = newThing(7);
}
