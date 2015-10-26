/* Filename: scanner.c
/* PURPOSE:
 *    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    scanner_init() must be called before using the scanner.
 *    The file is incomplete;
 *    Provided by: Svillen Ranev
 *    Version: 1.15.02
 *    Date: 29 September 2015
 *******************************************************************
 *    REPLACE THIS HEADER WITH YOUR HEADER
 *******************************************************************
 */

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

Token mlwpar_next_token(Buffer * sc_buf)
{
   Token t; /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0; /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   int accept = NOAS; /* type of state - initially not accepting */                                     
/* 
lexstart is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the first character of the current lexeme,
which is being processed by the scanner.
lexend is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the last character of the current lexeme,
which is being processed by the scanner.

*/ 
        
        
     //   DECLARE YOUR VARIABLES HERE IF NEEDED 
       
   char* lexeme = NULL;
   int iterator = 0;
   int errLexIt = 0;

        while (1){ /* endless loop broken by token returns it will generate a warning */
                
 //       GET THE NEXT SYMBOL FROM THE INPUT BUFFER 
        
        c = b_getc(sc_buf);
            
/* special cases or token driven processing */       
   if (c == 'SEOF') {t.code = SEOF_T; return t;}
   else if (c == '\n') {line++; continue;}

   else if (c == ' ') continue;
   
   else if (b_eob(sc_buf)) { t.code = SEOF_T; return t;}
   //If the input file does not have a proper end of file statement, and the
   //buffer reaches the end of buffer, return end of file token
   //otherwise, the buffer will infinitely loop and print the last error token

   else if (c == 255) {
	   t.code = SEOF_T;
	   return t;
   }

   else if (c == '\t') continue;
   else if (c == '\0') continue;
   else if (c == '{'){ t.code = LBR_T; /*no attribute */ return t; }
   else if(c == '+'){ t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; }
   else if(c == '-'){ t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; }
   else if(c == '}'){ t.code = RBR_T; return t; }
   else if(c == '/'){ t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; }
   else if(c == '*'){ t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; }
   else if(c == '('){ t.code = LPR_T; return t; }
   else if(c == ')'){ t.code = RPR_T; return t; }
   else if (c == ','){ t.code = COM_T; return t; }
   else if (c == ';'){ t.code = EOS_T; return t; }
   else if (c == '#') { t.code = SCC_OP_T; return t; }
   else if (c == '='){
	   c = b_getc(sc_buf);
	   if (c == '=') {
		   t.code = REL_OP_T;
		   t.attribute.rel_op = EQ;
		   return t;
	   } else {
		   b_retract(sc_buf);
	   }
	   t.code = ASS_OP_T;
	   return t;
   }
   else if (c == '>'){ t.code = REL_OP_T; t.attribute.rel_op = GT; return t; }
   else if (c == '<'){ // there also maybe not equal sign
	   c = b_getc(sc_buf);
	   if (c == '>'){
		   t.code = REL_OP_T;
		   t.attribute.rel_op = NE; // it's not equal
		   return t;
	   }
	   else { // is nor >, it's usual rel_op_t;
		   b_retract(sc_buf);
		   t.code = REL_OP_T; 
		   t.attribute.rel_op = LT;
		   return t;
	   }
	
  }
   // Have to delete this pragma region before submission, as it is c++ thing
   // err_lex MAY BE WRONG. PAY ATTENTION
#pragma region logicalOperatorsProcessing
   else  if (c == '.'){
	  b_setmark(sc_buf, b_getc_offset(sc_buf)-1); // set mark, if there's something wrong after, Maybe it's -1. Must be tested
	   c = b_getc(sc_buf);
	   if (c == 'A') { // Is good so far
		   c = b_getc(sc_buf);
		   if (c == 'N'){ // Is good so far
			   c = b_getc(sc_buf);
			   if (c == 'D') { // Is good so far
				   c = b_getc(sc_buf);
				   if (c == '.') { // This is .AND. operator
					   t.code = LOG_OP_T;
					   t.attribute.log_op = AND;
					   return t;
				   }
				   else {
					   b_retract_to_mark(sc_buf);
					   c = b_getc(sc_buf);
					   t.attribute.err_lex[0] = c;
					   t.attribute.err_lex[1] = '\0';
					   
					   t.code = ERR_T;
					   return t;
				   }
			   }
			   else { // NO D
			        b_retract_to_mark(sc_buf);
					c = b_getc(sc_buf);
				    t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					t.code = ERR_T;
					return t;
			   }
		   }
		   else { // NO N
     		   b_retract_to_mark(sc_buf);
		       c = b_getc(sc_buf);
			   t.attribute.err_lex[0] = c;
			   t.attribute.err_lex[1] = '\0';
			   t.code = ERR_T;
			   return t;
		   }
	   }
	   else if (c == 'O') { // or it may be .OR. 
		   c = b_getc(sc_buf);
		   if (c == 'R') { // good so far
			   c = b_getc(sc_buf);
			   if (c == '.') { // It's .OR.
				   t.code = LOG_OP_T;
				   t.attribute.log_op = OR;
				   return t;
			   }
			   else { // Not .
				   b_retract_to_mark(sc_buf);
				   c = b_getc(sc_buf);
				   t.attribute.err_lex[0] = c;
				   t.attribute.err_lex[1] = '\0';
				   t.code = ERR_T;
				   return t;
			   }
		   }
		   else { // Not R
			   b_retract_to_mark(sc_buf);
			   c = b_getc(sc_buf);
			   t.attribute.err_lex[0] = c;
			   t.attribute.err_lex[1] = '\0';
			   t.code = ERR_T;
			   return t;
		   }
	   }
	   else {
		   b_retract_to_mark(sc_buf);
		   c = b_getc(sc_buf);
		   t.attribute.err_lex[0] = c;
		   t.attribute.err_lex[1] = '\0';				   
		   t.code = ERR_T;
		   return t;
	   }
   }
#pragma endregion

   else if (c == '!') { // Trying to process comment;	
	   b_setmark(sc_buf, b_getc_offset(sc_buf) - 1); /* Set the mark to the beginning of the comment */
	   c = b_getc(sc_buf);   //  b_setmark(sc_buf, b_getc_offset(sc_buf)); // set mark. Maybe its b_getc_offset - 1. Must be tested; Is used for t.attribute.err_lex
	   if (c == '<') {
		   // processing comment here
		   while (c != '\n')
		   {
			   c = b_getc(sc_buf);
		   }
	   } 
	   else {
		   t.code = ERR_T;
		   b_retract_to_mark(sc_buf); /* If there is an error - return to the beginning of the comment*/
		   c = b_getc(sc_buf);	   
		   t.attribute.err_lex[0] = c;
		   c = b_getc(sc_buf);
		   t.attribute.err_lex[1] = c;
		   t.attribute.err_lex[2] = '\0';

		   while (c != '\n')
		   {
			   c = b_getc(sc_buf);
		   }
		   return t;
	   }
   }


   else if (c == '"'){ // String start. MAY BE FINISHED. MUST  BE TESTED

	  b_setmark (sc_buf,(b_getc_offset(sc_buf)));
			lexstart=b_getc_offset(sc_buf);
			c = b_getc(sc_buf);

			while(c != '"')
			{
				/* If \n, increase line counter */
				if(c== '\n' || c == 'SEOF')
				{
					++line;
				}

				/* If '\0', return an error token */
				if(c == '\0')
				{
					lexend =(b_getc_offset(sc_buf));
					t.code = ERR_T;	
					b_setmark(sc_buf,lexstart-1);
					b_retract_to_mark(sc_buf);

					while(lexstart <= lexend)
					{
						c=b_getc(sc_buf);
						if(errLexIt <ERR_LEN)
							t.attribute.err_lex[errLexIt++]=c;
						++lexstart;
					}

					if(errLexIt > (ERR_LEN-3))
					{
						t.attribute.err_lex[ERR_LEN-3]='.';
						t.attribute.err_lex[ERR_LEN-2]='.';
						t.attribute.err_lex[ERR_LEN-1]='.';
						t.attribute.err_lex[ERR_LEN]='\0';
					}

					b_retract(sc_buf);
					return t;					
				}
				c = b_getc(sc_buf);
			}

			t.code = STR_T;	
			t.attribute.str_offset = b_size(str_LTBL);
			lexend = (b_getc_offset(sc_buf)-1);
			b_retract_to_mark(sc_buf);
			
			while(lexstart != lexend)
			{
				c = b_getc(sc_buf);
				b_addc(str_LTBL,c);
				++lexstart;
			}
			
			b_addc(str_LTBL,'\0');
			c = b_getc(sc_buf);
			return t;
   }

   else if (isdigit(c) || isalpha(c)){
	   lexstart = b_getc_offset(sc_buf);
	   b_setmark(sc_buf, b_getc_offset(sc_buf)-1); // setting mark at the beginning

	   state = 0;

	   state = get_next_state(state, c, &accept); // getting the first state
	   c = b_getc(sc_buf);

	   while (accept == NOAS) {
		   state = get_next_state(state, c, &accept);
		   c = b_getc(sc_buf);
	   }

	   if (accept == ASWR) // If it's accept with retruct  0123456701234567012345670
	   {
		   b_retract(sc_buf);
	   }

	   lexend = b_getc_offset(sc_buf);
	   lex_buf = b_create(lexend - lexstart + 1, 1, 'f');	/* create a temp buffer to hold the lexeme */

	   if (lex_buf == NULL)
	   {
		   ++scerrnum;
		   t.code =ERR_T;
	       strcpy(t.attribute.err_lex,"RUN TIME ERROR");
			return t;
	   }

	   b_retract_to_mark(sc_buf);

	   while (lexstart != lexend)
	   {
		   c = b_getc(sc_buf);
	       b_addc(lex_buf, c);	/* add chars to lex buffer */
		   ++lexstart;
	   }

	   b_addc(lex_buf, '\0'); /*Make it C-Type*/

	   t = aa_table[state](lex_buf->cb_head);
	
	   free(lexeme);
	   b_destroy(lex_buf);
	   return t;
   } else {
	   t.code = ERR_T;
	   t.attribute.err_lex[0] = c;
	   t.attribute.err_lex[1] = '\0';
	   return t;
   }
   /*
     CHECK OTHER CHARS HERE if NEEDED, SET A TOKEN AND RETURN IT.
     FOR ILLEGAL CHARACTERS SET ERROR TOKEN. 
     THE ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN 
     IN A CASE OF RUNTIME ERROR, THE FUNCTION MUST STORE 
     A NON-NEGATIVE NUMBER INTO THE GLOBAL VARIABLE scerrnum
     AND RETURN AN ERROR TOKEN. THE ERROR TOKEN ATTRIBUTE MUST
     BE THE STRING "RUN TIME ERROR: "        */        
   }//end while(1)
}


// Do not modify it
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
/*
The assert(int test) macro can be used to add run-time diagnostic to programs
and to "defend" from producing unexpected results.
assert() is a macro that expands to an if statement;
if test evaluates to false (zero) , assert aborts the program
(by calling abort()) and sends the following message on stderr:

Assertion failed: test, file filename, line linenum

The filename and linenum listed in the message are the source file name
and line number where the assert macro appears.
If you place the #define NDEBUG directive ("no debugging")
in the source code before the #include <assert.h> directive,
the effect is to comment out the assert statement.
*/
     // assert(next != IS);

/*
The other way to include diagnostics in a program is to use
conditional preprocessing as shown bellow. It allows the programmer
to send more details describing the run-time problem. 
Once the program is tested thoroughly #define DEBUG is commented out
or #undef DEBUF is used - see the top of the file.
*/ 
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

int char_class (char c)
{
	/* check if it's a number */
     if (isdigit(c))
	{
		if (c == '0')
			return 1;
		 if (c <= '7')
			 return 2;
		 if (c >= '8')
			 return  3;
		
	}
	else if (c == '.')
		return 4;
	else if (c == '%')
		return 5;
	
	if (isalpha(c))
		return 0;

	return 6;

}


// Accept AVID token
Token aa_func02(char lexeme[]){
	Token t;
	int keyword = iskeyword(lexeme);

	if (keyword > 0) {
		t.code = KW_T;
		t.attribute.kwt_idx = keyword;
		return t;
	} else { // Set AVID
		if (strlen(lexeme) > VID_LEN){
			strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
			t.attribute.vid_lex[VID_LEN] = '\0';
		} else {
			strcpy(t.attribute.vid_lex, lexeme);
			t.attribute.vid_lex[strlen(lexeme)] = '\0';
		}
		t.code = AVID_T;
		return t;
	}
}

// Accept SVID token
Token aa_func03(char lexeme[]){
	Token t;
	if (strlen(lexeme) > VID_LEN) {
		strncpy(t.attribute.vid_lex, lexeme, VID_LEN-1);
		t.attribute.vid_lex[VID_LEN-1] = '%';
		t.attribute.vid_lex[VID_LEN] = '\0';
	} else {
		strcpy(t.attribute.vid_lex, lexeme);
		t.attribute.vid_lex[strlen(lexeme)] = '%';
		t.attribute.vid_lex[strlen(lexeme)]= '\0';
	}
	t.code = SVID_T;
  return t;
}


// FPL token
Token aa_func08(char lexeme[]){
  
	Token t;
	double floatValue = 0; // Using double to avoid possible float over or underflows

	floatValue = atof(lexeme);

	/*  if it's not in range */
	if ((float)floatValue > FLT_MAX) // should add <. Somehow floatValue < FLT_MIN is not working
	{
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else
	{
		t.code = FPL_T;
		t.attribute.flt_value = (float)floatValue;
	}
	
	return t;
}


/* IL*/
Token aa_func05(char lexeme[]){

	Token t;
	int intValue = 0;

	intValue = atoi(lexeme);
	if (sizeof(intValue) > sizeof(INT_MAX) || sizeof(intValue) > sizeof(INT_MIN)){
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN] = '\0';
	} else {
		t.code = INL_T;
		t.attribute.int_value = intValue;
	}
  return t;
}

// OIL
Token aa_func10(char lexeme[]){
	
	Token t;
	int val = 0;
	
	val = atool(lexeme);

	/* if error found or out of range, return error token */
	if (val > SHRT_MAX || val < 0)
	{
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	else
	{
		t.code = INL_T;
		t.attribute.int_value = val;
	}
	
	return t;
}


// Error token
Token aa_func12(char lexeme[]){

	Token t;
	int lengthLex = strlen(lexeme);
	t.code = ERR_T;

	if (lengthLex < (ERR_LEN - 3))
	{
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[lengthLex] = '\0';
	}
	else
	{
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	return t;
}




long atool(char * lexeme){
	long val = 0;
	char *ptr = NULL;
	val = strtol(lexeme, &ptr, 8);
	return val;
}

int iskeyword(char * kw_lexeme){
	
	int i = 0;
	for ( i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_table[i], kw_lexeme) == 0)
			return i;
	}

	return R_FAIL_1;

}
