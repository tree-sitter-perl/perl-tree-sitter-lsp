typedef unsigned short U16;
typedef unsigned int U32;
#ifdef PERL_CORE_WIDE
#define PERL_BITFIELD16 U32
#else
#define PERL_BITFIELD16 U16
#endif
struct op { PERL_BITFIELD16 op_type:9; };
