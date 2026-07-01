typedef unsigned short uint16_t;
typedef uint16_t U16;
#define PERL_BITFIELD16 U16
enum opcode { OP_NULL, OP_CONST, OP_SCOPE, OP_max };
struct op { PERL_BITFIELD16 op_type:9; };
int classify(struct op* o) {
    return o->op_type == OP_CONST || o->op_type == OP_NULL;
}
int is_scope(struct op* o) {
    if (o->op_type == OP_SCOPE) return 1;
    return o->op_type < OP_max;
}
