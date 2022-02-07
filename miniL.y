    /* cs152-miniL phase2 */
%{
void yyerror(const char *msg);
#include "calc.tab.h"
#include <stdio.h>
extern int yylex();
%}

%union{
  /* put your types here */
}

%error-verbose
%locations

%token <ival> NUMBER
%token <identval> IDENT

%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token BREAK
%token WRITE
%token NOT
%token TRUE
%token FALSE
%token ADD
%token RETURN
//%token NUMBER
%token SUB
%token MULT
%token DIV
%token MOD
%token EQ
%token NEQ
%token LT
%token GT
%token LTE
%token GTE
%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token ASSIGN


/* %start program */

%% 

  /* write your rules here */
  Program: Functions {printf("Program -> Functions\n");};

  Functions: %empty {printf("Functions -> epsilon\n");} 
        | Function Functions {printf("Functions -> Function Functions\n");}
        ;

  Identifier: IDENT {printf("ident -> IDENT %s\n", yylval.identval);};

  Identifiers: Identifier {printf("identifiers -> identifier\n");};

  Function: FUNCTION Identifier SEMICOLON BEGIN_PARAMS Declarations END_PARAMS BEGIN_LOCALS Declarations END_LOCALS BEGIN_BODY Statements

  Declarations: %empty {printf("Declarations -> epsilon\n");} 
        | DECLARATION SEMICOLON Declarations {printf("Declarations -> Declaration SEMICOLON Declaraions\n");}
        ;

  DECLARATION: Identifiers COLON INTEGER {printf("Declaration -> Identifiers COLON INTEGER\n");} 
        | Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
        ;

  Statements: %empty{printf("Statemnets -> epsilon\n");} 
        | Statement SEMICOLON Statements {printf("Statements -> Statement SEMICOLON Statements\n");}
        ;

  ElseStatement: ELSE Statements {printf("ElseStatement -> ELSE Statements\n");} 
        | %empty {printf("Elsestatement -> epsilon\n");}
        ;

  Statement: Var ASSIGN Expression{printf("Statement -> Var ASSIGN Expression\n");} 
        | IF BoolExp THEN Statements ElseStatement ENDIF{printf("Statement -> IF BoolExp THEN Statements ElseStatement ENDIF\n");} 
        | WHILE BoolExp BEGINLOOP Statements ENDLOOP{printf("Statement -> WHILE BoolExp BEGINLOOP Statements ENDLOOP\n");} 
        | DO BEGINLOOP Statements ENDLOOP WHILE BoolExp{printf("Statement -> DO BEGINLOOP Statements ENDLOOP WHILE BoolExp\n");} 
     //   | READ Var {printf("Statement -> READ Var\n");}
        ;

  BoolExp: Expression Comp Expression {printf("BoolExp -> Expression comp Expression\n");} 
            | NOT BoolExp {printf("BoolExp -> NOT BoolExp\n");}
            ;

  Comp: ASSIGN {printf("Comp -> ASSIGN\n");} 
        | NEQ{printf("Comp -> NEQ\n");} 
        | LT{printf("Comp -> LT\n");} 
        | GT{printf("Comp -> GT\n");} 
        | LTE{printf("Comp -> LTE\n");} 
        | GTE{printf("Comp -> GTE\n");}
        ;

  Expression: MultExp{printf("Expression -> MultExp\n");} 
              | MultExp ADD Expression{printf("Expression -> MultExp ADD Expression\n");} 
              | MultExp ADD Expression{printf("Expression -> MultExp SUB Expression\n");}
              ;

  MultExp: Term {printf("MultExp -> Term\n");} 
          | MultExp Term MULT MultExp{printf("MultExp -> Term MULT MultExp\n");} 
          | MultExp Term DIV MultExp{printf("MultExp -> Term DIV MultExp\n");} 
          | MultExp Term MOD MultExp{printf("MultExp -> Term MOD MultExp\n");
          };

  Term: Var {printf("Term -> Var\n");} 
        | NUMBER {printf("Term -> NUMBER\n");} 
        | L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression R_PAREN\n");} 
        | Identifier L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression COMMA R_PAREN\n");} 
        | Identifier L_PAREN Expression COMMA R_PAREN {printf("Term -> L_PAREN Expression COMMA R_PAREN\n");} 
        | Identifier L_PAREN R_PAREN {printf("Term -> L_PAREN R_PAREN\n");}
        ;

  Var: Identifier {printf("Var -> Identifier\n");} 
      | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("Var -> identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
      ;

%% 

int main(int argc, char **argv) {
  if (argc > 1) {
    yyin = fopen(argv[1], "r");
    if(yyin == 0){
      rintf("Error open file %s\n", argv[0]);
    }
  }
   yyparse();
   return 0;
}

void yyerror(const char *msg) {
    /* implement your error handling */
    printf("** Line %d: %s\n", yyloc.first_line, msg);
}
