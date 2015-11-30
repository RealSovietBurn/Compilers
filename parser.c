#include "parser.h"

extern Buffer * str_LTBL;
extern int line; // May not be working. There is getLine() function, in case it's not working

void parser(Buffer * in_buf){
 sc_buf = in_buf;
 lookahead = mlwpar_next_token(sc_buf);
 program(); match(SEOF_T,NO_ATTR);
 gen_incode("PLATY: Source file parsed");
}void match(int pr_token_code,int pr_token_attribute){	short iterator = 0;	int attribute[4] = {KW_T, LOG_OP_T, ART_OP_T, REL_OP_T};	// If it has one of the attributes...	for (iterator; iterator < 4; iterator++){		if ( pr_token_attribute == attribute[iterator] && pr_token_code == lookahead.code) {			if (lookahead.code == SEOF_T) return;			else if (lookahead.code == ERR_T){				syn_printe();				lookahead = mlwpar_next_token(sc_buf);				synerrno++;				return;			}			else {				lookahead = mlwpar_next_token(sc_buf);			}		}	}	// If no attributes - match the code only	if ( pr_token_code == lookahead.code) {			if (lookahead.code == SEOF_T) return;			else if (lookahead.code == ERR_T){				syn_printe();				lookahead = mlwpar_next_token(sc_buf);				synerrno++;				return;			}			else {				lookahead = mlwpar_next_token(sc_buf);			}	}	// It comes here only if match was not successful	syn_eh(pr_token_code);	return;}void syn_eh(int sync_token_code){	syn_printe();	synerrno++;	do {	if(lookahead.code == SEOF_T && sync_token_code != SEOF_T)
		exit(synerrno);	lookahead = mlwpar_next_token(sc_buf);		if (lookahead.code == sync_token_code && lookahead.code != SEOF_T){		lookahead = mlwpar_next_token(sc_buf);		return;	}	if (lookahead.code == sync_token_code && lookahead.code == SEOF_T){		return;	}	} while (1);	}void syn_printe(){
	printf("PLATY: Syntax error:  Line:%3d\n",line);
printf("*****  Token code:%3d Attribute: ", lookahead.code);
switch(lookahead.code){
	case  ERR_T: 
		printf("%s\n",lookahead.attribute.err_lex);
		break;
	case  SEOF_T: 
		printf("NA\n" );
		 break;
	case  AVID_T: 
	case  SVID_T:
		printf("%s\n", getStd()->pstvr[lookahead.attribute.get_int].plex);
		 break;
	case  FPL_T: 
		printf("%5.1f\n",lookahead.attribute.flt_value);
		 break;
	case INL_T: 
        printf("%d\n",lookahead.attribute.get_int);
		break;
	case STR_T:
		printf("%s\n",b_setmark(str_LTBL,lookahead.attribute.str_offset));
		break;
    case SCC_OP_T: 
        printf("NA\n" );
		break;
	case  ASS_OP_T:
		printf("NA\n" );
		break;
	case  ART_OP_T:
		printf("%d\n",lookahead.attribute.get_int);
		break;
	case  REL_OP_T:  
		printf("%d\n",lookahead.attribute.get_int);
		break;
	case  LOG_OP_T:
		printf("%d\n",lookahead.attribute.get_int);
		break;
	case  LPR_T: 
		printf("NA\n" );
		break;
	case  RPR_T: 
	    printf("NA\n" );
		break;
	case LBR_T: 
        printf("NA\n" );
		break;
	case RBR_T: 
        printf("NA\n" );
		break;	
	case KW_T:
	    printf("%s\n",kw_table [lookahead.attribute.get_int]);
		break;
	case COM_T: 
	    printf("NA\n");
		break;
	case EOS_T: 
	    printf("NA\n" );
		break; 		
	default: 
		printf("PLATY: Scanner error: invalid token code: %d\n", lookahead.code);
    }
}