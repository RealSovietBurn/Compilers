#include "buffer.h"
#include "token.h"
#include "stable.h"

#define NO_ATTR		0
#define ELSE		0
#define IF			1 
#define INPUT		2
#define OUTPUT		3
#define PLATYPUS	4
#define REPEAT		5
#define THEN		6
#define USING		7
#define KWT_SIZE	8


static Token lookahead;
static Buffer *sc_buf;
static int synerrno;

extern Token mlwpar_next_token(Buffer *);
extern int line;
extern STD sym_table;
extern Buffer *str_LTBL;
extern char *kw_table[KWT_SIZE];

void parser(Buffer *);
void match(int, int);
void syn_eh(int);
void syn_printe();
void program();
void gen_encode(char *);
