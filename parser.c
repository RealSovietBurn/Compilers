
#define _CRT_SECURE_NO_WARNINGSclo

#include "parser.h" 
#include <stdlib.h>


void parser(Buffer * in_buf)
{
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}


void match(int pr_token_code, int pr_token_attribute)
{
	int m = 1;

	switch (lookahead.code)
	{
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		m = lookahead.attribute.get_int == pr_token_attribute;
		break;
	case SEOF_T:
		if (lookahead.code == pr_token_code)
			return;
	}

	m = m && (lookahead.code == pr_token_code);

	switch (m)
	{
	case 1:
		if ((lookahead = mlwpar_next_token(sc_buf)).code == ERR_T)
		{
			syn_printe();
			lookahead = mlwpar_next_token(sc_buf);
			synerrno++;
		}

		return;
	case 0:
		syn_eh(pr_token_code);
		return;
	}
}


void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;

	while (1)
	{
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T)
			exit(synerrno);

		lookahead = mlwpar_next_token(sc_buf);

		if (lookahead.code == sync_token_code)
		{
			if (sync_token_code != SEOF_T)
				lookahead = mlwpar_next_token(sc_buf);

			return;
		}
	}//end of while(1)		
}


void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_setmark(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/


void gen_incode(char * string)
{
	printf("%s\n", string);
}

void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}


void additive_arithmetic_expression_p(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	}
}

void arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		unary_arithmetic_expression();
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}


void assignment_expression(void)
{
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	default:
		syn_printe();
	}
}


void assignment_statement(void)
{
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}


void conditional_expression(void)
{
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}


void input_statement(void)
{
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}


void iteration_statement(void)
{
	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}


void logical_and_expression(void)
{
	relational_expression();
	logical_and_expression_p();
}


void logical_and_expression_p(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_and_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
	}
}


void logical_or_expression(void)
{
	logical_and_expression();
	logical_or_expression_p();
}


void logical_or_expression_p(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case OR:
			match(LOG_OP_T, OR);
			logical_and_expression();
			logical_or_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
	}
}


void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

// LOL
void multiplicative_arithmetic_expression_p(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MULT:
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		case DIV:
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}


void opt_statements(void)
{
	switch (lookahead.code)
	{
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case PLATYPUS:
		case ELSE:
		case THEN:
		case REPEAT:
			gen_incode("PLATY: Opt_statements parsed");
			return;
		}
	case AVID_T:
	case SVID_T:
		statements();
		break;
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
}


void opt_variable_list(void)
{
	switch (lookahead.code)
	{
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}


void output_statement(void)
{
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	opt_variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}


void primary_a_relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}


void primary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}


void primary_s_relational_expression(void)
{
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}


void primary_string_expression(void)
{
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}


void program(void)
{
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}


void relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		relational_expression_p();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		relational_expression_p_str();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}


void relational_expression_p(void)
{
	if (lookahead.code == REL_OP_T)
	{	
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_a_relational_expression();
			return;	
	}
	syn_printe();
}


void relational_expression_p_str(void)
{
	if (lookahead.code == REL_OP_T)
	{
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_s_relational_expression();
			return;	
	}
	syn_printe();
}


void selection_statement(void)
{
	match(KW_T, IF);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}


void statement(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case IF:
			selection_statement();
			break;
		case USING:
			iteration_statement();
			break;
		case INPUT:
			input_statement();
			break;
		case OUTPUT:
			output_statement();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}


void statements(void)
{
	statement();
	statements_p();
}


void statements_p(void)
{
	switch (lookahead.code)
	{
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case PLATYPUS:
		case ELSE:
		case THEN:
		case REPEAT:
			return;
		}
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	}
}


void string_expression(void)
{
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}


void string_expression_p(void)
{
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
}

void unary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, PLUS);
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			break;
		default:
			syn_printe();
			return;
		}
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
		return;
	default:
		syn_printe();
	}
}


void variable_identifier(void)
{
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
}


void variable_list(void)
{
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}


void variable_list_p(void)
{
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
		break;
	}
}
