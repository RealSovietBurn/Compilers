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

/******************************************************************************
Purpose:		  This function creates a new (empty) symbol table.
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  int - size to be passed
Return value:	  STD - the newly created STD
Algorithm:		  Initializes memory for a new symbol table that can handle the
				  size passed
*******************************************************************************/
STD st_create(int st_size) {
	STD std;
	std.pstvr = (STVR *) malloc(sizeof(STVR)*st_size);
	std.plsBD =  b_create(200, 15, 'a');
	std.st_offset = 0;
	if(std.pstvr == NULL) std.st_size = 0;
	else std.st_size = st_size;
	return std;
}

/******************************************************************************
Purpose:		  This function installs a new entry (VID record) in the symbol table.
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
                  char* - string to be passed as the lexeme
Return value:	  int - the offset of the entry or failure status
Algorithm:		  Uses the st_lookup() to scan if an entry is at the passed point
				  and if it is not there, the program then creates and initializes
				  the corresponding values for a new entry
*******************************************************************************/
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

	if (lexemeOffset >= sym_table.st_size)
	{
		return -1;
	}


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

/******************************************************************************
Purpose:		  Checks the update flag (LSB) of the status_field of the entry.
                  If it is equal to 1, the type has been already updated and 
				  the function returns –1. Otherwise, the function updates 
				  the data type indicator of the status_field, sets the LSB 
				  of the status_field to 1, and returns vid_offset.
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
				  int - vid offset to be passed
				  char - v_type to be passed
Return value:	  int - the vid_offset or -1 for failure
Algorithm:		  Grabs the symbol table and increases it's offset
*******************************************************************************/
int st_update_type(STD sym_table,int vid_offset,char v_type){
	if((sym_table.pstvr[vid_offset].status_field & BIT_MASK_SET_UPDATE_FLAG) == 1) return -1;
	else{
		if(v_type=='S') return -1; // Just in case if user would like to update String;
		sym_table.pstvr[vid_offset].status_field = BIT_MASK_DEFAULT;
		switch(v_type) {
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

/******************************************************************************
Purpose:		  This function searches for a lexeme (variable name) in the
				  symbol table. The search is performed backward from the 
				  last entry to the beginning of the array of STVR. If it has
				  been found, it returns the offset of the entry from the 
				  beginning of the array of STVR. Otherwise, it returns –1.
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
                  char* - string to be passed as the lexeme
Return value:	  int - the offset of the entry or failure status
Algorithm:		  Iterates through the symbol table to search for an individual
				  lexeme. If it is found it returns the offset of the entry in the
				  array, and otherwise, returns -1 as failure status.
*******************************************************************************/
int st_lookup(STD sym_table, char *lexeme) {
	int iterator = sym_table.st_offset;
	for (iterator; iterator > 0; iterator--){ // Iterator - 1, as offset maybe 1 (first element), but it's actual index is 0
		if (strcmp(sym_table.pstvr[iterator - 1].plex, lexeme) == 0){
			return iterator;
		}
	}
	return -1; // Not found
}

/******************************************************************************
Purpose:		  This internal function increases st_offset
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  void
Return value:	  none
Algorithm:		  Grabs the symbol table and increases it's offset
*******************************************************************************/
static void st_incoffset(void){
	(*getStd()).st_offset++;
}

/******************************************************************************
Purpose:		  This internal function sets st_size to 0.
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  void
Return value:	  none
Algorithm:		  Grabs the symbol table and sets it's size to zero
*******************************************************************************/
static void st_setsize(void){
	(*getStd()).st_size = 0;
}


/******************************************************************************
Purpose:		  This function compares two strings alphabetically in
				  ascending order
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  const void - first string
				  const void - second string
Return value:	  int - compared value
Algorithm:		  Grabs the first value of each passed string and compares them
				  in ascending order
*******************************************************************************/
int st_ascending_cmp(const void *a, const void *b) {
     char const *aa = (char const  *)a;
     char const *bb = (char const  *)b;
     return strcmp(aa, bb);
}

/******************************************************************************
Purpose:		  This function compares two strings alphabetically in
				  descending order
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  const void - first string
				  const void - second string
Return value:	  int - compared value
Algorithm:		  Grabs the first value of each passed string and compares them
				  in descending order
*******************************************************************************/
int st_descending_cmp(const void *a, const void *b) {
     char const *aa = (char const  *)a;
     char const *bb = (char const  *)b;
     return -strcmp(aa, bb);
}

/******************************************************************************
Purpose:		  This function is not implemented, it does not sort the array.
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
                  char - order in which to sort
Return value:	  int - vid_offset or failure status
Algorithm:		  returns zero, does not sort
*******************************************************************************/
int st_sort(STD sym_table, char s_order){
	if(s_order == 'D'){
		if(sym_table.pstvr == NULL) return -1;
		qsort(sym_table.pstvr,sym_table.st_offset,sizeof(sym_table.pstvr[0]),st_descending_cmp);
		return 1;
	}
	else if (s_order == 'A'){
		if(sym_table.pstvr == NULL) return -1;
		qsort(sym_table.pstvr,sym_table.st_offset,sizeof(sym_table.pstvr[0]),st_ascending_cmp);
		return 1;
	}
	return -1;
}

/******************************************************************************
Purpose:		  The function updates the i_value of the variable specified
                  by vid_offset. On success it returns vid_offset. 
                  On failure it returns –1.
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
                  int - vid_offset to be passed
                  InitialValue - i_value to be passed
Return value:	  int - vid_offset or failure status
Algorithm:		  Updates the symbol table's value to the value passed as an
                  argument.
*******************************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	if (sym_table.pstvr == NULL) return -1; 
	i_value = sym_table.pstvr[vid_offset].i_value;
	return vid_offset;
}

/******************************************************************************
Purpose:		  The function returns the type of the variable specified by
                  vid_offset. It returns F for floating-point type,
                  I for integer type, or S for string type.
                  On failure it returns –1.
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
                  int - vid_offset to be passed
Return value:	  char - the variable type or -1 for failure
Algorithm:		  Compares the type of variable specified by vid_offset passed
                  and returns the according value
*******************************************************************************/
char st_get_type (STD sym_table, int vid_offset){
	if((sym_table.pstvr[vid_offset].status_field & BIT_MASK_STRING) == BIT_MASK_STRING) return 'S';
	if((sym_table.pstvr[vid_offset].status_field & BIT_MASK_FLOAT) == BIT_MASK_FLOAT) return 'F';
	if((sym_table.pstvr[vid_offset].status_field & BIT_MASK_INTEGER) == BIT_MASK_INTEGER) return 'I';
	return -1;
}

/******************************************************************************
Purpose:		  This function frees the memory occupied by the symbol table
dynamic areas and sets st_size to 0.
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
Return value:	  none
Algorithm:		  Frees all memory of the symbol table and sets it's size to 0
*******************************************************************************/
void st_destroy(STD sym_table) {
	/* free buffer when needed */
	if (sym_table.plsBD != NULL)
	{
		b_destroy(sym_table.plsBD);
	}
	if (sym_table.pstvr != NULL)
	{
		free(sym_table.pstvr);
	}
	st_setsize();
}

/******************************************************************************
Purpose:		  This function prints the contents of the symbol table to the standard output (screen)
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  STD - symbol table to be passed
Return value:	  int - number of entries printed or failure (-1)
Algorithm:		  Iterates through all symbols and prints them to the screen
*******************************************************************************/
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

/******************************************************************************
Purpose:		  This function stores the symbol table into a file named
$stable.ste. This file is a text file. If the file already
exists in the current directory, the function overwrites it.
The function uses fprintf() to write to the file. 
First, it writes st_size. Then for each symbol table entry,
it writes the status_field (in hex format), the length of
the lexeme, the lexeme, the line number, and the initial value.
To output the appropriate initial value you must use the
st_get_type() function. The data items in the file are
separated with a space (see fileio.c example). 
On success the function prints a message “Symbol Table stored”
and returns the number of records stored; it returns –1 on failure.
Author:			  Nick Horlings
History/Versions: 1.0
Called functions: none
Parameters:		  STD - the symbol table to be passed
Return value:	  int - number of records stored or failure status
Algorithm:		  Iterates through each symbol table entry and writes the
hex equivalency of the data to the symbol table file.
*******************************************************************************/
int st_store(STD sym_table){
	int i = 0;
	char * fileName = "$stable.ste";
	FILE *f = fopen(fileName, "w");
	if(!f) return -1;
	if(sym_table.pstvr == NULL) return -1;
	fprintf(f, "%d", sym_table.st_size);
	for(i = 0; i < sym_table.st_offset; ++i){
		fprintf(f, " %04X %d %s %d ", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);
		switch(st_get_type(sym_table, i)){
			case 'S':
				fprintf(f, "%d", sym_table.pstvr[i].i_value.str_offset);
				break;
			case 'F':
				fprintf(f, "%.2f", sym_table.pstvr[i].i_value.fpl_val);
				break;
			case 'I':
				fprintf(f, "%d", sym_table.pstvr[i].i_value.int_val);
				break;
			}
	}
	fclose(f);
	printf("\nSymbol Table stored.\n");
	return i;
}

/******************************************************************************
Purpose:		  this function is used to get sym_table from platy_tt in functions
from scanner.c. It allows to avoid using sym_table as global variable
in scanner.c
Author:			  Oleg Matviyishyn
History/Versions: 1.0
Called functions: none
Parameters:		  void
Return value:	  pStd - pointer to symbol table.
Algorithm:		  Returns pointer to symbol table
*******************************************************************************/
STD* getStd(void){
	return pStd;
}