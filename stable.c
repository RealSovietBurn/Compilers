#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "buffer.h"
#include "stable.h"

/* global sym_table, from platy_tt.c */
extern STD sym_table;

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


// This function must be tested. There may be tons of errors
int st_install(STD sym_table, char *lexeme, char type, int line){

	int i;  // used in for loop
    int lexemeOffset; // return offset for STVR
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

    /* check type, if string or int or float */
    if (type == 'S')
	{
		/* set i_value */
        stvr.i_value.str_offset = -1;
		/* set as string */
        stvr.status_field |= BIT_MASK_STRING;
		/* set update flag */
        stvr.status_field &= BIT_MASK_RESET_UPDATE_FLAG;
        stvr.status_field |= BIT_MASK_SET_UPDATE_FLAG;
    }	
    else if (type == 'I')
	{
		/* Set i_value */
        stvr.i_value.int_val = 0;
        /* Set as integer */
        stvr.status_field |= BIT_MASK_INTEGER;        
    }
    else if (type == 'F')
	{
		/* Set i_value */
        stvr.i_value.fpl_val = 0.0f;
        /* Set as floating-point */
        stvr.status_field |= BIT_MASK_FLOAT;        
    }

	/* install STVR in array */
    sym_table.pstvr[sym_table.st_offset] = stvr;

	/* increment by 1 */
    st_incoffset();

    return lexemeOffset;
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
	sym_table.st_offset++;
}
