/* Filename: table.h
 * Transition Table and function declarations necessary for the scanner implementation  
 * as required for CST8152 - Assignment #2.
 * Version: 1.15.02
 * Date: 29 September 2015
 * Provided by: Svillen Ranev
 * The file is incomplete. You are to complete it.
 ***************************************************
 * 
File name: table.h
Compiler: MS Visual Studio 2012
Author: Nick Horlings, 040-781-692; Oleg Matviyishyn, 040-764-529
Course: CST 8152 – Compilers, Lab Section: 012
Assignment: 2
Date: October 27th, 2015
Professor: Sv. Ranev
Purpose: This header file appropriates values
		 for the transition table used in handling
		 the scanner's movements while processing
		 a file and provides accepting functions definitions
Function list: aa_func02()			   aa_func05()			   aa_func08()			   aa_func10()			   aa_func12()
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
 

#define ES 12 /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */

#define TABLE_COLUMNS 7 
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
//				[a-zA-z]   0    [1-7]  [8-9]    .        %        other
/* State 0 */  {    1   ,  6  ,   4   ,  4 ,     IS   ,   IS   ,   IS   },
/* State 1 */  {    1   ,  1  ,   1   ,  1,      2    ,    3   ,    2   },
/* State 2 */  {    IS  ,  IS ,   IS  ,  IS,     IS   ,   IS   ,   IS   },
/* State 3 */  {    IS  ,  IS ,   IS  ,  IS,     IS   ,   IS   ,   IS   },
/* State 4 */  {    ES  ,  4  ,   4   ,  4,      7    ,   5    ,    5   },
/* State 5 */  {    IS  ,  IS ,   IS  ,  IS,     IS   ,   IS   ,    IS  },
/* State 6 */  {    ES  ,  ES ,   9   ,  ES,     7    ,   ES   ,    5   },
/* State 7 */  {    8   ,  7  ,   7   ,  7,      8    ,   8    ,    8   }, 
/* State 8 */  {    IS  ,  IS ,   IS  ,  IS,     IS   ,   IS   ,    IS  },
/* State 9 */  {    IS  ,  9  ,   9  ,   ES,     ES   ,   ES   ,    10  },
/* State 10 */ {    IS  ,  IS ,   IS  ,  IS,     IS   ,   IS   ,    IS  },
/* State 11 */ {    IS  ,  IS ,   IS  ,  IS ,    IS   ,   IS   ,    IS  },
/* State 12 */ {    IS  ,  IS ,   IS  ,  IS ,	 IS   ,   IS   ,    IS  },
/* State 13 */ {    IS  ,  IS ,   IS  ,  IS ,    IS   ,   IS   ,    IS  }
     };

/* Accepting state table definition */

#define ASWR      1  /* accepting state with retract */
#define ASNR      0 /* accepting state with no retract */
#define NOAS     -1  /* not accepting state */

int as_table[ ] = {
	NOAS,/* State0 */
	NOAS,/* State1 */
	ASWR,/* State2 */
	ASNR,/* State3 */
	NOAS,/* State4 */
	ASWR,/* State5 */
	NOAS,/* State6 */
	NOAS,/* State7 */
	ASWR,/* State8 */
	NOAS,/* State9 */
	ASWR,/* State10 */
	NOAS,/* State11 */
	ASNR,/* State12 */
	ASWR/* State13 */
};

/* Accepting action function declarations */

Token aa_func02(char *lexeme);	/*	AVID or KW */
Token aa_func03(char *lexeme);	/* SVID */
Token aa_func05(char *lexeme);	/* DIL */
Token aa_func08(char *lexeme);	/* FPL */
Token aa_func10(char *lexeme);	/* OIL */
Token aa_func12(char *lexeme);	/* error token */


/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={
	NULL,     /* State 0 */
	NULL,     /* State 1 */
	aa_func02,/* State 2 */
	aa_func03,/* State 3 */
	NULL,     /* State 4 */
	aa_func05,/* State 5 */
	NULL,     /* State 6 */
	NULL,     /* State 7 */
	aa_func08,/* State 8 */
	NULL,     /* State 9 */
	aa_func10,/* State 10 */
	NULL,     /* State 11 */
	aa_func12,/* State 12 */
//	aa_func12 /* State 13 */
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
                     