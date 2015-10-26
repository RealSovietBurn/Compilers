/* File buffer.c
* Compiler: MS Visual Studio 2012
* Author: Oleg Matviyishyn, 040 764 529
* Course: CST 8152 – Compilers, Lab Section: 011
* Assignment: 1
* Date: 2015-09-29
* Professor: Sv. Ranev
* Purpose: Contains definitions and implementations of functions from buffer.h 
* Function list: b_create ();
				 b_addc();
				 b_reset();
   				 b_destroy();
 				 b_isfull ();
 				 b_size ();
				 b_capacity ();
				 b_setmark ();
				 b_mark ();
				 b_mode ();
				 b_inc_factor();
				 b_load ();
				 b_isempty (;
				 b_eob ();
				 b_getc ();
				 b_print ();
			     b_pack ();
				 b_rflag ();
				 b_retract ();
				 b_retract_to_mark ();
				 b_getc_offset (); 
*/


#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "buffer.h"
#include <crtdbg.h>

/************************************************************************************************************************
Purpose: Creates the buffer, sets initial variables
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: calloc(c,s) 
Parameters: short init_capacity >= 0, char inc_factor > 0 but < 256, char o_mode must be 'a', 'm' or 'f'
Return value: Buffer * b if success, NULL if failure
Algorithm: Allocate memory for one Buffer structure, set up inc_factor and capacity in accordance to function parameters
************************************************************************************************************************/
Buffer * b_create (short init_capacity, char inc_factor, char o_mode) {

	short short_inc_factor = 0; /*inc_factor as a short*/
	
	Buffer * b = (Buffer *) calloc(1, sizeof(Buffer)); /* Giving memory for one Buffer */

	if (b == NULL) { /* if failed to allocate */
		return NULL;
	}

	switch(o_mode){   /*Check the parameters*/
	 case 'f': case 'a': case 'm': break;
	 default:
	  printf("%s%s%s",o_mode,": ","Wrong mode parameter.");
	  b_destroy(b);
	  return NULL;
	}

	if (init_capacity < 0 || init_capacity > SHRT_MAX) {
		b_destroy(b);
		return NULL;
	}

	if (short_inc_factor > 256) {
		printf("Wrong inc_factor value!\n");
		b_destroy(b);
		return NULL;
	}

	if (inc_factor < 0) short_inc_factor = inc_factor + 256;
	else
		short_inc_factor = (short)inc_factor;
	
	if (init_capacity != 0) {
		b->cb_head = (char *)malloc(init_capacity * sizeof(char)); /*Assigning the pointer to cb_head */
		if (b->cb_head == NULL)
			return NULL; // Memory allocation failure
	}

	b->capacity = init_capacity * sizeof(char);

	if (o_mode == 'f' || short_inc_factor == 0) {
		b->inc_factor = 0;
		b->mode = 0;
	} 

	else if (o_mode == 'a' && (short_inc_factor > 0 && short_inc_factor <= 255)) {
		b->mode = 1;
		b->inc_factor = (char)short_inc_factor;
	}

	else if (o_mode == 'm' && (short_inc_factor > 0 && short_inc_factor <= 100)) {
		b->mode = -1;
		b->inc_factor = (char)short_inc_factor;
	} else { 
		b_destroy(b);
		printf("PLAYBT.EXE: Could not create buffer");
		return NULL;
	}

	return b;
}

/************************************************************************************************************************
Purpose: Adds character to the buffer, increments the size of the buffer according to the mode
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: malloc(s), realloc(p,s), free(p), 
Parameters: pBuffer const, char
Return value: pBD is success, NULL is failure
Algorithm: Add character to the buffer. If character can't be added to the buffer, increases the size of the buffer
		   in accordance to the one of three modes.
************************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){ 

	short short_inc_factor = 0;
	short i = 0; /* Variable for loop iteration */
	short newCapacity = 0;
	short availableSpace = 0;
	short newIncrement = 0;
	char * tmp = NULL; /*temporary variable, for allocation check*/

	pBD->r_flag = 0;

	if (pBD->inc_factor < 0) {
		short_inc_factor = pBD->inc_factor + 256;
	} else short_inc_factor = pBD->inc_factor;

	if (!b_isfull(pBD)) { /* if it's not full */
		pBD->cb_head[pBD->addc_offset++] = symbol; /*increase addc_offset and load symbol to the buffer */
		return pBD;
	} else {

		if (pBD->mode == 0) return NULL;

		if (pBD->mode == 1) {

			if (pBD->capacity + short_inc_factor < SHRT_MAX) { /* If didn't go out if the range of short; */
				tmp = (char *)realloc((char *)pBD->cb_head, (pBD->capacity + short_inc_factor)*sizeof(char)); /* Reallocate original array and save the pointer*/
				if (tmp != NULL) { /* If reallocation successeded */
					pBD->cb_head = tmp; /* Overwrite original pointer */
					pBD->capacity += (short_inc_factor * sizeof(char)); /*Increase capacity*/
					pBD->addc_offset++; /*increase addc_offset */
					pBD->cb_head[pBD->addc_offset - 1] = symbol;
					pBD->r_flag = SET_R_FLAG;
					return pBD;
				}
			}
		}
	if (pBD->mode == -1) {

			availableSpace = SHRT_MAX - pBD->addc_offset;
			newIncrement = availableSpace * short_inc_factor / 100;
			newCapacity = pBD->capacity + newIncrement*sizeof(char);

			if (newCapacity + 1 > SHRT_MAX) {
				return NULL;
			} 

			if (newIncrement == 0 && pBD->capacity + 1 <= SHRT_MAX )
			newCapacity = pBD->capacity + 1*sizeof(char);

			tmp = (char *) realloc ((char *)pBD->cb_head, newCapacity);
				
			if (tmp != NULL)
				    pBD->cb_head = tmp;
					pBD->capacity = newCapacity;
					if (pBD->addc_offset + 1 <= SHRT_MAX)
					pBD->addc_offset++; /* increase addc_offset */ 	
					pBD->cb_head[pBD->addc_offset-1] = symbol;	
					return pBD;
				}
	}
	return NULL; /* If none characters added, return null*/
}

/************************************************************************************************************************
Purpose: Resets all values of the Bufer structure to the initial ones
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: None 
Parameters: Buffer * const
Return value: -1 if error, 1 if success
Algorithm: 
************************************************************************************************************************/
int b_reset (Buffer * const pBD){
	if (pBD == NULL)
		return R_FAIL_1;
	pBD -> addc_offset = 0;
	pBD -> eob = 0;
	pBD -> getc_offset = 0;
	pBD -> mark_offset = 0;
	pBD -> r_flag = 0;
	return 1;
}

/************************************************************************************************************************
Purpose: Releases all resourses
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: free(p)
Parameters: Buffer *
Return value: void
Algorithm:
************************************************************************************************************************/
void b_destroy (Buffer * const pBD){
	
	if (pBD != NULL) {
		if (pBD->cb_head != NULL) {
			free(pBD->cb_head);
		}
		free (pBD);
	}
}

/************************************************************************************************************************
Purpose: Checks, if the buffer is full
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: free(p)
Parameters: Buffer * const
Return value: -1 if error, 0 if buffer is not full and 1 if it's full
Algorithm:
************************************************************************************************************************/
int b_isfull (Buffer * const pBD){ /*Processing runtimes*/
	if (pBD == NULL ) return R_FAIL_1; 
	if (pBD->addc_offset < pBD->capacity) return 0;
	return SUCCESS;
}

/************************************************************************************************************************
Purpose: Returns size of the buffer
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const
Return value: -1 is error, or pBD->addc_offset
Algorithm:
************************************************************************************************************************/
short b_size (Buffer * const pBD){
	if (pBD == NULL) return R_FAIL_1; /*Processing runtimes*/
	return pBD->addc_offset;
}

/************************************************************************************************************************
Purpose: Returns capacity
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const
Return value: pBD->capacity
Algorithm:
************************************************************************************************************************/
short b_capacity(Buffer * const pBD){
	if (pBD == NULL || pBD->capacity < 0) return R_FAIL_1; /*Processing runtimes*/
	return pBD->capacity;
}
/************************************************************************************************************************
Purpose: Sets the mark
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: 
Parameters: Buffer * const, short
Return value: NULL is error, &(pBD->cb_head[mark]) if mark > 0 && mark <= pBD->addc_offset
Algorithm:
************************************************************************************************************************/
char * b_setmark (Buffer * const pBD, short mark){

	if (pBD == NULL) return NULL; /*Processing runtimes*/
	if (mark >= 0 && mark <= pBD->addc_offset) {
		pBD->mark_offset = mark;
		return pBD->cb_head + mark;
	} else
		return NULL;
}

/************************************************************************************************************************
Purpose: Returns mark
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const
Return value: pBD->mark_offset
Algorithm:
************************************************************************************************************************/
short b_mark (Buffer * const pBD) {
	if (pBD == NULL) return R_FAIL_1;
	return pBD->mark_offset;
}

/************************************************************************************************************************
Purpose: Returns mode
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: 
Parameters: Buffer * const
Return value: -2 if error, otherwise pBD->mode
Algorithm:
************************************************************************************************************************/
int b_mode (Buffer * const pBD){ 
	if (pBD == NULL)  /*Processing runtimes*/
		return R_FAIL_2; 
	return pBD->mode;
}

/************************************************************************************************************************
Purpose: Returns inc_factor
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer *
Return value: 256 if error, pBD->inc_factor
Algorithm:
************************************************************************************************************************/
size_t b_inc_factor (Buffer * const pBD) {

	if (pBD == NULL) return INC_FACTOR_FAILURE;

	if (pBD -> inc_factor < 0) return (size_t)(pBD -> inc_factor + 256);

	return pBD->inc_factor;
}

/************************************************************************************************************************
Purpose: Reads the file and loads character to the buffer
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: fgetc(f), b_addc(pBufer, char)
Parameters: Buffer * const
Return value: totalCharacters, or NULL if failure
Algorithm: Until the end of the file reached, a character is added to the buffer and totalCharacters incremented. If failed
		   to add character - return NULL
************************************************************************************************************************/
int b_load (FILE * const fi, Buffer * const pBD) {

	Buffer * temp = NULL;

	char c = 0;
	int totalCharacters = 0;

	while (!feof(fi)) {	
		c = (char)fgetc(fi);
		if (c != EOF){
		temp = b_addc(pBD, c);
		totalCharacters++;
		}
		if (ftell(fi) > 0) { /*Check the file size, so if it won't return LOAD_FAIL, if the file is empty*/
			if (temp == NULL) return LOAD_FAIL;
		}
	} 
	return totalCharacters;
} 

/************************************************************************************************************************
Purpose: Checks, if the buffer is empty
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const
Return value: -1 if error, 1 if empty, 0 if it's not
Algorithm:
************************************************************************************************************************/
int b_isempty(Buffer * const pBD) {
	if (pBD == NULL) return R_FAIL_1;
	if (pBD->addc_offset == 0) return SUCCESS;
	else return 0;
}

/************************************************************************************************************************
Purpose: Return eob
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const 
Return value: pBD->eob
Algorithm:
************************************************************************************************************************/
int b_eob(Buffer * const pBD) {
	if (pBD == NULL) return R_FAIL_1;
	return pBD->eob;
}

/************************************************************************************************************************
Purpose: Gets character from the buffer at current offset
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions:
Parameters: Buffer * const
Return value: -1 of getc_offset == pBD->addc_offset, NULL if error, and pBD->cb_head[pBD->getc_offset-1) otherwise
Algorithm:
************************************************************************************************************************/
char b_getc (Buffer * const pBD){
	if (pBD == NULL) return R_FAIL_2;

	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1;
		return R_FAIL_1;
	} else {
		pBD->eob = 0;
	}
	return pBD->cb_head[pBD->getc_offset++];
}
/************************************************************************************************************************
Purpose: Prints the contents of the buffer
Author: Oleg Matviyishyn
History/Versions: 2.0 2015-10-26
Called functions: b_isempty(Buffer *), printf(const char *, ...)
Parameters: Buffer * const
Return value: numberPrinted, which is the number of printed characters
Algorithm: While end of buffer is not reached and character != EOF, print the character and incement numberPrinted
************************************************************************************************************************/
int b_print (Buffer * const pBD) {
	char c = 5; /*Initializing a variable, which will be printed*/
	int numberPrinted = 0; /*Number of printed characters*/
	if (pBD == NULL) return R_FAIL_1; /*Runtime error*/
	if (b_isempty(pBD)){
		printf("The buffer is empty.\n");
		return numberPrinted;
	}
	pBD->getc_offset = 0;
	c = b_getc(pBD);
	/* until eob */
	while (b_eob(pBD) == 0)
	{
		printf("%c",c);
		++numberPrinted;
		c = b_getc(pBD);
	}

	printf("\n");

	return numberPrinted;
}

/************************************************************************************************************************
Purpose: Packs the buffer
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-29
Called functions: realloc(p,s)
Parameters: Buffer * const
Return value: Buffer * pBD, NULL if failure
Algorithm: If there is some space left, change the size of the buffer to addc_offset + 1
************************************************************************************************************************/
Buffer * b_pack (Buffer * const pBD) {
	short newCapacity = 0; /*The variable for new capacity*/
	char * tmp = NULL;

	if (pBD == NULL) return NULL;

	if (pBD->addc_offset + 1 <= SHRT_MAX)
	newCapacity = (pBD->addc_offset + 1) * sizeof(char);
	else return NULL;

	tmp = (char *) realloc(pBD->cb_head, newCapacity);
	
	if (tmp != NULL) {
		pBD->cb_head = tmp;
		pBD->r_flag = SET_R_FLAG;
	}
	else
		return NULL;
	
	pBD->capacity = newCapacity;
	
	return pBD; 
}

/************************************************************************************************************************
Purpose: Return r_flag
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-28
Called functions:
Parameters: Buffer * const
Return value: r_flag, NULL if failure
Algorithm:
************************************************************************************************************************/
char b_rflag (Buffer * const pBD) {
	if (pBD == NULL) return R_FAIL_1;
	return pBD->r_flag;
}

/************************************************************************************************************************
Purpose: Decrement getc_offset by 1
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-28
Called functions:
Parameters: Buffer * const
Return value: getc_offset, -1 if failure
Algorithm: 
************************************************************************************************************************/
short b_retract (Buffer * const pBD) {
	if (pBD == NULL)
		return R_FAIL_1;
	pBD->getc_offset-=1;
	if (pBD->getc_offset < 0) 
		return R_FAIL_1;
	else return pBD->getc_offset;
}
/************************************************************************************************************************
Purpose: Set getc_offset to mark_offset
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-28
Called functions: realloc(p,s)
Parameters: Buffer * const
Return value: Buffer * pBD, -1 if failure
Algorithm:
************************************************************************************************************************/
short b_retract_to_mark (Buffer * const pBD) {
	if (pBD == NULL || pBD->mark_offset < 0) return R_FAIL_1;
	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/************************************************************************************************************************
Purpose: Returns getc_offset
Author: Oleg Matviyishyn
History/Versions: 1.02 2015-09-28
Called functions: realloc(p,s)
Parameters: Buffer * const
Return value: getc_offset, -1 if failure
Algorithm:
************************************************************************************************************************/
short b_getc_offset (Buffer * const pBD) {
	if (pBD == NULL || pBD->getc_offset < 0) return R_FAIL_1;
	return pBD->getc_offset;
}