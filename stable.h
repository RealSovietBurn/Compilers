

#ifndef STABLE_H_
#define STABLE_H_
/* ------------------------ Bit Masks ------------------------- */
/* zero */
#define BIT_MASK_ZERO 0x0000			/*	0000 0000 0000 0000	*/
/* default */
#define BIT_MASK_DEFAULT 0xFFF8			/*	1111 1111 1111 1000	*/
/* reset */
#define BIT_MASK_RESET_UPDATE_FLAG 0xFFFE	/*	1111 1111 1111 1110	*/
/* update */
#define BIT_MASK_SET_UPDATE_FLAG 0x0001		/*	0000 0000 0000 0001	*/
/* integer */
#define BIT_MASK_INTEGER 0x0004		/*	0000 0000 0000 0100	*/
/* floating point */
#define BIT_MASK_FLOAT 0x0002		/*	0000 0000 0000 0010	*/
/* string */
#define BIT_MASK_STRING 0x0006		/*	0000 0000 0000 0110	*/
/* ------------------------ Bit Masks ------------------------- */

typedef union InitialValue {
 int int_val; /* integer variable initial value */
 float fpl_val; /* floating-point variable initial value */
 int str_offset; /* string variable initial value (offset) */
} InitialValue;
typedef struct SymbolTableVidRecord {
 unsigned short status_field; /* variable record status field*/
 char * plex; /* pointer to lexeme (VID name) in CA */
 int o_line; /* line of first occurrence */
 InitialValue i_value; /* variable initial value */
 size_t reserved; /*reserved for future use*/
}STVR;
typedef struct SymbolTableDescriptor {
 STVR *pstvr; /* pointer to array of STVR */
 int st_size; /* size in number of STVR elements */
 int st_offset; /*offset in number of STVR elements */
 Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;

/* Function Prototypes */
STD st_create(int);
int st_install(STD, char *, char, int);
int st_lookup(STD, char *);
int st_update_type(STD, int, char);
int st_update_value(STD, int, InitialValue);
char st_get_type (STD, int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD, char);
static void st_setsize(void);
static void st_incoffset(void);
 STD* getStd(void);
#endif
