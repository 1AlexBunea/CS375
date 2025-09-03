%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 10 Jan 24   */

/* Copyright (c) 2023 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09; 02 Aug 12;
   30 Jul 13; 25 Jul 19 ; 28 Feb 22; 08 Jul 22; 13 Nov 23 */

/*
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
  */


/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */

       /* To use:
                     make pars1y              has 1 shift/reduce conflict
                     pars1y                   execute the parser
                     i:=j .
                     ^D                       control-D to end input

                     pars1y                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D

                     pars1y                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D

           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */

        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/

#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "pprint.h"
#include "parse.h"
#include "codegen.h"

        /* define the type of the Yacc stack element to be TOKEN */

#define YYSTYPE TOKEN

TOKEN parseresult;

%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH

%right thenthen ELSE // Same precedence, but "shift" wins.

%%

program    :  PROGRAM IDENTIFIER LPAREN idlist RPAREN SEMICOLON vblock DOT 
                                       { parseresult = makeprogram($2, $4, $7); }
           |  PROGRAM IDENTIFIER SEMICOLON vblock DOT
                                       { parseresult = makeprogram($2, NULL, $4); }
           |  vblock DOT                        { parseresult = $1; }
           ;
  statement  :  BEGINBEGIN statement endpart
                                       { $$ = makeprogn($1,cons($2, $3)); }
             |  IF expr THEN statement endif   { $$ = makeif($1, $2, $4, $5); }
             |  FOR IDENTIFIER ASSIGN expr TO expr DO statement
                                       { $$ = makefor(1, $1, binop($3, $2, $4), $5, $6, $7, $8); }
             |  FOR IDENTIFIER ASSIGN expr DOWNTO expr DO statement  
                                       { $$ = makefor(-1, $1, binop($3, $2, $4), $5, $6, $7, $8); }
             |  funcall
             |  assignment
             ;
  endpart    :  SEMICOLON statement endpart    { $$ = cons($2, $3); }
             |  END                            { $$ = NULL; }
             ;
  endif      :  ELSE statement                 { $$ = $2; }
             |  /* empty */                    { $$ = NULL; }  %prec thenthen
             ;
  assignment :  variable ASSIGN expr           { $$ = binop($2, $1, $3); }
             ;
  expr       :  expr PLUS term                 { $$ = binop($2, $1, $3); }
             |  term 
             ;
  term       :  term TIMES factor              { $$ = binop($2, $1, $3); }
             |  factor
             ;
  factor     :  LPAREN expr RPAREN             { $$ = $2; }
             |  variable
             |  NUMBER
             |  STRING
             ;
  variable   : IDENTIFIER
             ;
  funcall    : IDENTIFIER LPAREN args RPAREN   { $$ = makefuncall($2, $1, $3); }
             ;
  args       : expr COMMA args                 { $$ = cons($1, $3); }
             | expr                            { $$ = cons($1, NULL); }
             | /* empty */                     { $$ = NULL; }
             ;

  idlist     : IDENTIFIER COMMA idlist         { $$ = cons($1, $3); }
             | IDENTIFIER                      { $$ = cons($1, NULL); }
             ;
  vblock     : VAR varspecs block              { $$ = $3; }
             | block
             ;
  varspecs   : vargroup SEMICOLON varspecs
             | vargroup SEMICOLON
             ;
  vargroup   : idlist COLON type               { instvars($1, $3); }
             ;
  type       : simpletype
             ;
  simpletype : IDENTIFIER                      { $$ = findtype($1); }
             ;
  block      : BEGINBEGIN statement endpart   { $$ = makeprogn($1,cons($2, $3)); }
             ;

%%

/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.

   To add more flags, use the next power of 2: the next one would be 32.
   To turn on all flags, set DEBUG to the next power of 2, minus 1.
  */

#define DEBUG        31             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */

 int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;
    if (DEBUG & DB_CONS)
       { printf("cons\n");
         dbugprinttok(item);
         dbugprinttok(list);
       };
    return item;
  }

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  { op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
    return op;
  }

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     if (DEBUG & DB_MAKEIF)
        { printf("makeif\n");
          dbugprinttok(tok);
          dbugprinttok(exp);
          dbugprinttok(thenpart);
          dbugprinttok(elsepart);
        };
     return tok;
   }

TOKEN makeprogn(TOKEN tok, TOKEN statements)
  {  tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
   }

TOKEN makeprogram(TOKEN name, TOKEN args, TOKEN statements) {
  TOKEN prog_tok = talloc();
  TOKEN args_progn = NULL;
  prog_tok->tokentype = OPERATOR;
  prog_tok->whichval = PROGRAMOP;
  prog_tok->operands = name;
  if (args != NULL) {
      args_progn = talloc();
      args_progn->tokentype = OPERATOR;
      args_progn->whichval = PROGNOP;
      args_progn->operands = args;
      name->link = args_progn;
      args_progn->link = statements;
  } else {
      name->link = statements;
  }
  if (statements != NULL) statements->link = NULL;
  return prog_tok;
}

/* install variables in symbol table */
void instvars(TOKEN idlist, TOKEN typetok)
  {  SYMBOL sym, typesym; int align;
     typesym = typetok->symtype;
     align = alignsize(typesym);
     while ( idlist != NULL )   /* for each id */
       {  sym = insertsym(idlist->stringval);
          sym->kind = VARSYM;
          sym->offset =     /* "next" */
              wordaddress(blockoffs[blocknumber],
                          align);
          sym->size = typesym->size;
          blockoffs[blocknumber] =   /* "next" */
                         sym->offset + sym->size;
          sym->datatype = typesym;
          sym->basicdt = typesym->basicdt;
          idlist = idlist->link;
        };
  }

TOKEN findtype(TOKEN tok) { /* find type name */
  SYMBOL sym = searchst(tok->stringval);
  tok->symentry = sym;
  tok->symtype = sym;
  return tok;
}

TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args)
  { tok->tokentype = OPERATOR;
    tok->whichval = FUNCALLOP;
    tok->operands = fn;
    fn->link = args;
    return tok;
  }

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }
 
TOKEN makelabel()
  { TOKEN tok = talloc();
    TOKEN numtok = talloc();
    tok->tokentype = OPERATOR;
    tok->whichval = LABELOP;
    numtok->tokentype = NUMBERTOK;
    numtok->intval = labelnumber++;
    numtok->basicdt = INTEGER;
    numtok->link = NULL;
    tok->operands = numtok;
    return tok;
  }

TOKEN makegoto(int label)
  { TOKEN tok = talloc();
    TOKEN labeltok = talloc();
    tok->tokentype = OPERATOR;
    tok->whichval = GOTOOP;
    labeltok->tokentype = NUMBERTOK;
    labeltok->intval = label;
    labeltok->link = NULL;
    tok->operands = labeltok;
    return tok;
  }

TOKEN makeintc(int num)
  { TOKEN tok = talloc();
    tok->tokentype = NUMBERTOK;
    tok->basicdt = INTEGER;
    tok->intval = num;
    return tok;
  }

TOKEN makeplus(TOKEN lhs, TOKEN rhs, TOKEN tok)
  { if (tok == NULL) tok = talloc();
    tok->tokentype = OPERATOR;
    tok->whichval = PLUSOP;
    tok->operands = lhs;
    lhs->link = rhs;
    rhs->link = NULL;
    return tok;
  }

TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr,
              TOKEN tokc, TOKEN statement)
  { TOKEN label_tok, if_tok, progn_tok, cond_tok, inner_progn, increment, 
          assign2, goto_stmt, loop_var, increment_expr, one_tok;
    int loop_label;
    
    /* Generate label number */
    loop_label = labelnumber;  /* get current label number before incrementing */
    
    /* Create label token */
    label_tok = makelabel();   /* this will increment labelnumber */
    
    /* Get loop variable from assignment */
    loop_var = asg->operands;  /* left side of assignment */
    
    /* Create condition: (for TO) <= or (for DOWNTO) >= */
    cond_tok = talloc();
    cond_tok->tokentype = OPERATOR;
    if (sign == 1) {
        cond_tok->whichval = LEOP;  /* <= for TO */
    } else {
        cond_tok->whichval = GEOP;  /* >= for DOWNTO */
    }
    
    /* Copy loop variable for condition */
    TOKEN loop_var_copy = talloc();
    *loop_var_copy = *loop_var;  /* copy the token */
    
    cond_tok->operands = loop_var_copy;
    loop_var_copy->link = endexpr;
    endexpr->link = NULL;
    
    /* Create increment: i := i + 1 (or i := i - 1 for DOWNTO) */
    one_tok = makeintc(1);
    
    /* Copy loop variable again for increment */
    TOKEN loop_var_copy2 = talloc();
    *loop_var_copy2 = *loop_var;
    
    TOKEN loop_var_copy3 = talloc();
    *loop_var_copy3 = *loop_var;
    
    if (sign == 1) {
        increment_expr = makeplus(loop_var_copy2, one_tok, NULL);
    } else {
        increment_expr = talloc();
        increment_expr->tokentype = OPERATOR;
        increment_expr->whichval = MINUSOP;
        increment_expr->operands = loop_var_copy2;
        loop_var_copy2->link = one_tok;
        one_tok->link = NULL;
    }
    
    assign2 = talloc();
    assign2->tokentype = OPERATOR;
    assign2->whichval = ASSIGNOP;
    assign2->operands = loop_var_copy3;
    loop_var_copy3->link = increment_expr;
    increment_expr->link = NULL;
    
    /* Create goto statement */
    goto_stmt = makegoto(loop_label);
    
    /* Create inner progn: (progn statement assignment goto) */
    inner_progn = talloc();
    inner_progn->tokentype = OPERATOR;
    inner_progn->whichval = PROGNOP;
    inner_progn->operands = statement;
    statement->link = assign2;
    assign2->link = goto_stmt;
    goto_stmt->link = NULL;
    
    /* Create if statement: (if condition inner_progn) */
    if_tok = talloc();
    if_tok->tokentype = OPERATOR;
    if_tok->whichval = IFOP;
    if_tok->operands = cond_tok;
    cond_tok->link = inner_progn;
    inner_progn->link = NULL;
    
    /* Create outer progn: (progn assignment label if) */
    progn_tok = tok;  /* reuse tok */
    progn_tok->tokentype = OPERATOR;
    progn_tok->whichval = PROGNOP;
    progn_tok->operands = asg;
    asg->link = label_tok;
    label_tok->link = if_tok;
    if_tok->link = NULL;
    
    return progn_tok;
  }



void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

int main(void)          /*  */
  { int res;
    initsyms();
    res = yyparse();
    printstlevel(1);    /* to see level 0 too, change to:   printst();  */
    printf("yyparse result = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
    /* uncomment following to call code generator. */
     /* 
    gencode(parseresult, blockoffs[blocknumber], labelnumber);
 */
  }
