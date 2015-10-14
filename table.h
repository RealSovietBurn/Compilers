/* Filename: table.h
 * Transition Table and function declarations necessary for the scanner implementation  
 * as required for CST8152 - Assignment #2.
 * Version: 1.15.02
 * Date: 29 September 2015
 * Provided by: Svillen Ranev
 * The file is incomplete. You are to complete it.
 ***************************************************
 * REPLACE THIS HEADER WITH YOUR HEADER
 ***************************************************
 */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
 */

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */
 

#define ES -2 /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */

#define TABLE_COLUMNS 6 // check transition table
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
//				[a-zA-z]   0    [1-9]     .        %        other
/* State 0 */  {    1   ,  6  ,   4   ,   IS   ,   IS   ,   IS   },
/* State 1 */  {    1   ,  1  ,   1   ,    2   ,    3   ,    2   },
/* State 2 */  {},
/* State 3 */  {},
/* State 4 */  {},
/* State 5 */  {},
/* State 6 */  {},
/* State 7 */  {}, 
/* State 8 */  {},
/* State 9 */  {},
/* State 10 */ {},
/* State 11 */ {},
/* State 12 */ {},

/* Accepting state table definition */

#define ASWR      1  /* accepting state with retract */
#define ASNR      0 /* accepting state with no retract */
#define NOAS     -1  /* not accepting state */

int as_table[ ] = {YOUR INITIALIZATION HERE - USE ASWR, ASNR, NOAS };

/* Accepting action function declarations */

FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

Token aa_funcXX(char *lexeme); 

Replace XX with the number of the accepting state: 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={


HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
THE REST OF THE ELEMENTS MUST BE SET TO NULL.

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
                     