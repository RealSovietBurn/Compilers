#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define NO_ATTR 0



static Token lookahead;
static Buffer *sc_buf;
static int synerrno;


void parser(Buffer*);
void match(int, int);void syn_eh(int);void syn_printe();