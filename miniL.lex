   /* cs152-miniL phase1 */
   
   
%{   
   /* write your C code here for definitions of variables and including headers */

	int lineCount = 1 ;
	int colCount = 1 ;
   #include "y.tab.h"
   extern int yylex();

%}

   /* some common rules */
DIGIT 	[0-9]
CHAR	[a-zA-Z]
EMPTY   " "
NEWLINE \n
TAB	\t
RTRN	\r
ANY     .
COMM    ##
UNDER   _
EXP	{CHAR}({CHAR}|{DIGIT})*
IDENT	{EXP}|({EXP}{UNDER}+({CHAR}|{DIGIT})+)

%%
   /* specific lexer rules in regex */

"function" 	{ return FUNCTION; colCount += 8 ; }
"beginparams"	{ return BEGIN_PARAMS; colCount += 11 ; }
"endparams"	{ return END_PARAMS ; colCount += 9 ; }
"beginlocals"	{ return BEGIN_LOCALS; colCount += 11 ; }
"endlocals"	{ return END_LOCALS ; colCount += 9 ; }
"beginbody"	{ return BEGIN_BODY ; colCount += 9 ; }
"endbody"	{ return END_BODY ; colCount += 7 ; }
"integer"	{ return INTEGER ; colCount += 7 ; }
"array"		{ return ARRAY; colCount += 5 ; }
"of"		{ return OF; colCount += 2 ; }
"if"		{ return IF ; colCount += 2 ; }
"then"		{ return THEN ; colCount += 4 ; }
"endif"		{ return ENDIF ; colCount += 5 ; }
"else"		{ return ELSE ; colCount += 4 ; }
"while"		{ return WHILE ; colCount += 5 ; }
"do"		{ return DO ; colCount += 2 ; }
"beginloop"	{ return BEGINLOOP ; colCount += 9 ; }
"endloop"	{ return ENDLOOP ; colCount += 7 ; }
"continue"	{ return CONTINUE ; colCount += 8 ; }
"break"		{ return BREAK ; colCount += 5 ; }
"read"		{ return READ ; colCount += 4 ; }
"write"		{ return WRITE ; colCount += 5 ; }
"not"		{ return NOT; colCount += 3 ; }
"true"		{ return TRUE ; colCount += 4 ; }
"false"		{ return FALSE ; colCount += 5 ; }
"return"	{ return RETURN ; colCount += 6 ; }
"-"		{ return SUB ; colCount++ ; }
"+"		{ return ADD ; colCount++ ; }
"*"		{ return MULT ; colCount++ ; }
"/"		{ return DIV ; colCount++ ; }
"=="		{ return EQ ; colCount++ ; }
"<>"		{ return NEQ ; colCount += 2 ; }
"<"		{ return LT ; colCount++ ; }
">"		{ return GT ; colCount++ ; }
"<="		{ return LTE ; colCount += 2 ; }
">="		{ return GTE ; colCount += 2 ; }

{IDENT}		{ yylval.op_val = yytext ; return IDENT ; colCount += strlen(yytext) ; }
{DIGIT}+	{ yylval.int_val= atoi(yytext) ; return NUMBER ; colCount += strlen(yytext) ; }

";"		{ return SEMICOLON ; colCount++ ; }
":"		{ return COLON ; colCount++ ; }
","		{ return COMMA ; colCount++ ; }
"("		{ return L_PAREN ; colCount++ ; }
")"		{ return R_PAREN ; colCount++ ; }
"["		{ return L_SQUARE_BRACKET ; colCount++ ; }
"]"		{ return R_SQUARE_BRACKET ; colCount++ ; }
":="		{ return ASSIGN ; colCount += 2 ; }

{NEWLINE}	{ lineCount++ ; colCount = 0 ; }
{EMPTY}		{ colCount++ ;}
{TAB}+		{ colCount += 8 ; }
{RTRN}+		{ lineCount++ ; colCount = 0 ; }
{COMM}{ANY}*{NEWLINE} { lineCount++ ; colCount = 0 ; }

{DIGIT}{IDENT}	{ printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", lineCount, colCount, yytext) ; return 1 ; }
{EXP}{UNDER}+	{ printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", lineCount, colCount, yytext) ; return 1 ; }
.		{ printf("Error at line %d, column %d: Unrecognized symbol \"%s\"\n", lineCount, ++colCount, yytext) ; return 1 ; }

