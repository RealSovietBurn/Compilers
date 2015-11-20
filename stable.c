#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "buffer.h"
#include "stable.h"

/* global sym_table, from platy_tt.c */
extern STD sym_table;
/* global b_destroy to clear the buffer*/
// This is a pointer to sym_table from platy_tt.c. Used to avoid using extern variable in scanner.c
STD * pStd = &sym_table;


extern void b_destroy(Buffer * const);

STD st_create(int st_size) {
	STD std;
	std.pstvr = (STVR *) malloc (sizeof(STVR)*st_size);
	std.plsBD =  b_create(200, 15, 'a'); // Add constants here instead of 200 and 15
	std.st_offset = 0;

	if (std.pstvr == NULL) std.st_size = 0;
	else 
		std.st_size = st_size;
	
	return std;
}

// This function must be tested. There may be tons of errors.
int st_install(STD sym_table, char *lexeme, char type, int line){
	int i;  // used in for loop
    int lexemeOffset = 0; // return offset for STVR
    STVR stvr; //  STVR to install

    /* if symbol table invalid, return -1 */
    if (sym_table.st_size == 0)
	{
        return -1;
	}
	/* check for existing symbol */
    lexemeOffset = st_lookup(sym_table, lexeme);

    /* if symbol found, return it offset */
    if (lexemeOffset != -1)
	{
        return lexemeOffset;
	}

	/* set offset to next one */
    lexemeOffset = sym_table.st_offset;

	/* set plex, to the plsBD */
    stvr.plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));
    stvr.o_line = line;

	/* add lexeme to buffer */
    for (i = 0; i <= strlen(lexeme); i++)
	{
        if (!b_addc(sym_table.plsBD, lexeme[i]))
		{
            return -1;
		}
    }

    /* set field to default default value */
    stvr.status_field = BIT_MASK_DEFAULT;

	switch (type) {
	case 'S': 
		/* set i_value */
        stvr.i_value.str_offset = -1;
		/* set as string */
        stvr.status_field |= BIT_MASK_STRING;
		/* set update flag */
        stvr.status_field &= BIT_MASK_RESET_UPDATE_FLAG;
        stvr.status_field |= BIT_MASK_SET_UPDATE_FLAG;
		break;
	case 'I':
		/* Set i_value */
        stvr.i_value.int_val = 0;
        /* Set as integer */
        stvr.status_field |= BIT_MASK_INTEGER;
		break;
	case 'F':
			/* Set i_value */
        stvr.i_value.fpl_val = 0.0f;
        /* Set as floating-point */
        stvr.status_field |= BIT_MASK_FLOAT;  
		break;
	}

	/* install STVR in array */
    sym_table.pstvr[sym_table.st_offset] = stvr;

	/* increment by 1 */
    st_incoffset();

    return lexemeOffset;
}


int st_update_type(STD sym_table,int vid_offset,char v_type){
	if ((sym_table.pstvr[vid_offset].status_field & BIT_MASK_SET_UPDATE_FLAG) == 1) { // Status has already been updated
		return -1;
	} else {
		if (v_type == 'S') return -1; // Just in case if user would like to update String;
		sym_table.pstvr[vid_offset].status_field = BIT_MASK_DEFAULT;
		switch (v_type) {
	case 'I':
        sym_table.pstvr[vid_offset].status_field |= BIT_MASK_INTEGER;
		break;
	case 'F':
        /* Set as floating-point */
        sym_table.pstvr[vid_offset].status_field |= BIT_MASK_FLOAT;  
		break;
	}
		sym_table.pstvr[vid_offset].status_field |= BIT_MASK_SET_UPDATE_FLAG; // Set flag to 1
	}
	return vid_offset;
}

int st_lookup(STD sym_table, char *lexeme) {
	int iterator = sym_table.st_offset;
	for (iterator; iterator > 0; iterator--){ // Iterator - 1, as offset maybe 1 (first element), but it's actual index is 0
		if (strcmp(sym_table.pstvr[iterator - 1].plex, lexeme) == 0){
			return iterator;
		}
	}
	return -1; // Not found
}

static void st_incoffset(void){
	(*getStd()).st_offset++;
}

static void st_setsize(void){
	(*getStd()).st_size = 0;
}

int st_sort(STD sym_table, char s_order){
	return 0;
}

// Think about this. It must return -1 on  failure. BUT WHAT IS A FAILURE HERE? Except STVR array can be null (I doubt that)
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){	if (sym_table.pstvr == NULL) return -1; 	i_value = sym_table.pstvr[vid_offset].i_value;	return vid_offset;}
char st_get_type (STD sym_table, int vid_offset){
	if ((sym_table.pstvr[vid_offset].status_field & BIT_MASK_FLOAT) == BIT_MASK_FLOAT){
		return 'F';
	}

	if ((sym_table.pstvr[vid_offset].status_field & BIT_MASK_INTEGER) == BIT_MASK_INTEGER){
		return 'I';
	}

	if ((sym_table.pstvr[vid_offset].status_field & BIT_MASK_STRING) == BIT_MASK_STRING){
		return 'S';
	}
}

/*May have errors*/
void st_destroy(STD sym_table) {
	//int i = 0;
	//b_destroy(sym_table.plsBD);
	//free(sym_table.plsBD);
    //for (i = 0; i < sym_table.st_offset; i++) {
	//	free(sym_table.pstvr[i].plex);
//	}
//	free(sym_table.pstvr);
}

// I'm pretty sure in these functions
int st_print(STD sym_table){
	int i = 0;
	if (sym_table.pstvr == NULL) return -1;
	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");
	for (i = 0; i < sym_table.st_offset; i++) {
		printf("%2d          %s\n",sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}
	return i;
}

// Some bullshit here
int st_store(STD sym_table){
	int i = 0;
	char * fileName = "$stable.ste";
	FILE *f = fopen( fileName, "w");
	fprintf(f, "%d ", sym_table.st_size);
	for ( i = 0; i < sym_table.st_offset; i++){
		fprintf(f, " %04x %d %s %d %c", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line, st_get_type(sym_table, i+1) );
	}
}

 STD* getStd(void){
	return pStd;
}