/* lex1.c         14 Feb 01; 31 May 12; 11 Jan 18; 20 Jan 24       */

/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2024 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "lexan.h"

extern int CHARCLASS[];


// Helper function for special characters
// This function helps determine whether the token is a delimiter or an operator
// This excludes certain special case operators: and, or, not, div, mod, in
TOKEN delimiterOrOperator(char *str, TOKEN tok) {
  // Check all delimiters
  int isOperator = 0;
  int tokentype = 0;
  if (strcmp(str, "+") == 0) {
    isOperator = 1;
    tokentype = PLUS;
  } else if (strcmp(str, "-") == 0) {
    isOperator = 1;
    tokentype = MINUS;
  } else if (strcmp(str, "*") == 0) {
    isOperator = 1;
    tokentype = TIMES;
  } else if (strcmp(str, "/") == 0) {
    isOperator = 1;
    tokentype = DIVIDE;
  } else if (strcmp(str, ":=") == 0) {
    isOperator = 1;
    tokentype = ASSIGN;
  } else if (strcmp(str, "=") == 0) {
    isOperator = 1;
    tokentype = EQ;
  } else if (strcmp(str, "!=") == 0) {
    isOperator = 1;
    tokentype = NE;
  } else if (strcmp(str, "<") == 0) {
    isOperator = 1;
    tokentype = LT;
  } else if (strcmp(str, "<=") == 0) {
    isOperator = 1;
    tokentype = LE;
  } else if (strcmp(str, ">=") == 0) {
    isOperator = 1;
    tokentype = GE;
  } else if (strcmp(str, ">") == 0) {
    isOperator = 1;
    tokentype = GT;
  } else if (strcmp(str, "^") == 0) {
    isOperator = 1;
    tokentype = POINT;
  } else if (strcmp(str, ".") == 0) {
    isOperator = 1;
    tokentype = DOT;
  } else if (strcmp(str, ",") == 0) { // Start of the Delimiters
    tokentype = COMMA;
  } else if (strcmp(str, ";") == 0) {
    tokentype = SEMICOLON;
  } else if (strcmp(str, ":") == 0) {
    tokentype = COLON;
  } else if (strcmp(str, "(") == 0) {
    tokentype = LPAREN;
  } else if (strcmp(str, ")") == 0) {
    tokentype = RPAREN;
  } else if (strcmp(str, "[") == 0) {
    tokentype = LBRACKET;
  } else if (strcmp(str, "]") == 0) {
    tokentype = RBRACKET;
  } else if (strcmp(str, "..") == 0) {
    tokentype = DOTDOT;
  } 

  if (isOperator == 1) {
    tok->tokentype = OPERATOR;
    tok->whichval = tokentype - OPERATOR_BIAS;
  } else { // The string 
    tok->tokentype = DELIMITER;
    tok->whichval = tokentype - DELIMITER_BIAS;
  }

  return (tok);
}


/* Lookup function to check if a string is a reserved word */
int reservedlookup(char *str) {
    if (strcmp(str, "array") == 0) return ARRAY;
    if (strcmp(str, "begin") == 0) return BEGINBEGIN;
    if (strcmp(str, "case") == 0) return CASE;
    if (strcmp(str, "const") == 0) return CONST;
    if (strcmp(str, "do") == 0) return DO;
    if (strcmp(str, "downto") == 0) return DOWNTO;
    if (strcmp(str, "else") == 0) return ELSE;
    if (strcmp(str, "end") == 0) return END;
    if (strcmp(str, "file") == 0) return FILEFILE;
    if (strcmp(str, "for") == 0) return FOR;
    if (strcmp(str, "function") == 0) return FUNCTION;
    if (strcmp(str, "goto") == 0) return GOTO;
    if (strcmp(str, "if") == 0) return IF;
    if (strcmp(str, "label") == 0) return LABEL;
    if (strcmp(str, "nil") == 0) return NIL;
    if (strcmp(str, "of") == 0) return OF;
    if (strcmp(str, "packed") == 0) return PACKED;
    if (strcmp(str, "procedure") == 0) return PROCEDURE;
    if (strcmp(str, "program") == 0) return PROGRAM;
    if (strcmp(str, "record") == 0) return RECORD;
    if (strcmp(str, "repeat") == 0) return REPEAT;
    if (strcmp(str, "set") == 0) return SET;
    if (strcmp(str, "then") == 0) return THEN;
    if (strcmp(str, "to") == 0) return TO;
    if (strcmp(str, "type") == 0) return TYPE;
    if (strcmp(str, "until") == 0) return UNTIL;
    if (strcmp(str, "var") == 0) return VAR;
    if (strcmp(str, "while") == 0) return WHILE;
    if (strcmp(str, "with") == 0) return WITH;

    return IDENTIFIER;
}

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
  {
      int c;
      while ((c = peekchar()) != EOF
             && (c == ' ' || c == '\n' || c == '\t'))
          getchar();
    }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  {
    char str[16];
    int c;
    int idx = 0;

    while ((c = peekchar()) != EOF && CHARCLASS[c] == ALPHA) {
      c = getchar();
      if (idx < 15) {
        str[idx++] = tolower(c); // Make sure to lowercase the letter
      }
    }
    // Finish parsing the string and now determine if it is an identifier or a reserved word
    str[idx] = '\0';

    int tokentype = reservedlookup(str);
    if (tokentype == IDENTIFIER) {
        tok->tokentype = IDENTIFIERTOK;
        strcpy(tok->stringval, str);
    } else {
        tok->tokentype = RESERVED;
        tok->whichval = tokentype - RESERVED_BIAS;
    }
    
    return (tok);
  }

/* Used to parse strings: 'hello' */
TOKEN getstring (TOKEN tok)
  {
    char str[16];
    int c;
    int idx = 0;
    
    /* consume the opening single quote */
    c = getchar();  /* this should be the opening ' */
    
    /* read characters until we find the closing quote */
    while ((c = peekchar()) != EOF)  {
      // If the next char is a quote and the one after is not, then we break
      // If the next two chars are both quotes, we can include one
      if (c == '\'') {
        int d = peek2char();
        if (d == '\'') {
          // consume 1 of the quotes then add the next
          c = getchar();
        } else {
          // End of the string
          break;
        }
      }


      c = getchar();
      if (idx < 15) { 
        str[idx++] = tolower(c);
      }
    }
    
    /* consume the closing single quote */
    if (c == '\'') {
      getchar();
    }
    
    str[idx] = '\0';
    tok->tokentype = STRINGTOK;
    tok->basicdt = STRINGTYPE;
    strcpy(tok->stringval, str);  /* store the string in the token */
    return (tok);
  }


/* Used to parse special characters */
TOKEN special (TOKEN tok)
  {
    char str[16];
    int c;
    int idx = 0;

    while ((c = peekchar()) != EOF && CHARCLASS[c] == SPECIAL) {
      c = getchar();
      if (idx < 15) {
        str[idx++] = tolower(c);
      }
    }

    str[idx] = '\0';
    
    // Do a lookup to see whether the string is a delimiter or operator
    if (1 == 1) {

    } else {

    } 

    return (delimiterOrOperator(str, tok));
  }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
  { long num;
    int  c, charval;
    num = 0;
    while ( (c = peekchar()) != EOF
            && CHARCLASS[c] == NUMERIC)
      {   c = getchar();
          charval = (c - '0');
          num = num * 10 + charval;
        }
    tok->tokentype = NUMBERTOK;
    tok->basicdt = INTEGER;
    tok->intval = num;
    return (tok);
  }

