typedef unsigned short U16;
#define PERL_BITFIELD16 U16
#define BASEOP PERL_BITFIELD16 op_type:9; U16 op_flags;
#define REFCNT U16 op_refcnt;
struct op { BASEOP };
struct unop { BASEOP U16* op_first; };
struct sv { REFCNT };
void f(struct op* o, struct unop* u, struct sv* s) {
    unsigned a = o->op_type;
    unsigned b = u->op_type;
    unsigned c = s->op_refcnt;
}
