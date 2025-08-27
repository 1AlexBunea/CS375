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


/* Lookup function to check if a string is a reserved word or special operator */
void reservedlookup(char *str, TOKEN tok) {
    // Check for special operator words first
    if (strcmp(str, "and") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = AND - OPERATOR_BIAS;
        return;
    } else if (strcmp(str, "or") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = OR - OPERATOR_BIAS;
        return;
    } else if (strcmp(str, "not") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = NOT - OPERATOR_BIAS;
        return;
    } else if (strcmp(str, "div") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = DIV - OPERATOR_BIAS;
        return;
    } else if (strcmp(str, "mod") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = MOD - OPERATOR_BIAS;
        return;
    } else if (strcmp(str, "in") == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = IN - OPERATOR_BIAS;
        return;
    }
    
    // Check for regular reserved words
    if (strcmp(str, "array") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = ARRAY - RESERVED_BIAS;
    } else if (strcmp(str, "begin") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = BEGINBEGIN - RESERVED_BIAS;
    } else if (strcmp(str, "case") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = CASE - RESERVED_BIAS;
    } else if (strcmp(str, "const") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = CONST - RESERVED_BIAS;
    } else if (strcmp(str, "do") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = DO - RESERVED_BIAS;
    } else if (strcmp(str, "downto") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = DOWNTO - RESERVED_BIAS;
    } else if (strcmp(str, "else") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = ELSE - RESERVED_BIAS;
    } else if (strcmp(str, "end") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = END - RESERVED_BIAS;
    } else if (strcmp(str, "file") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = FILEFILE - RESERVED_BIAS;
    } else if (strcmp(str, "for") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = FOR - RESERVED_BIAS;
    } else if (strcmp(str, "function") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = FUNCTION - RESERVED_BIAS;
    } else if (strcmp(str, "goto") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = GOTO - RESERVED_BIAS;
    } else if (strcmp(str, "if") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = IF - RESERVED_BIAS;
    } else if (strcmp(str, "label") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = LABEL - RESERVED_BIAS;
    } else if (strcmp(str, "nil") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = NIL - RESERVED_BIAS;
    } else if (strcmp(str, "of") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = OF - RESERVED_BIAS;
    } else if (strcmp(str, "packed") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = PACKED - RESERVED_BIAS;
    } else if (strcmp(str, "procedure") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = PROCEDURE - RESERVED_BIAS;
    } else if (strcmp(str, "program") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = PROGRAM - RESERVED_BIAS;
    } else if (strcmp(str, "record") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = RECORD - RESERVED_BIAS;
    } else if (strcmp(str, "repeat") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = REPEAT - RESERVED_BIAS;
    } else if (strcmp(str, "set") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = SET - RESERVED_BIAS;
    } else if (strcmp(str, "then") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = THEN - RESERVED_BIAS;
    } else if (strcmp(str, "to") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = TO - RESERVED_BIAS;
    } else if (strcmp(str, "type") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = TYPE - RESERVED_BIAS;
    } else if (strcmp(str, "until") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = UNTIL - RESERVED_BIAS;
    } else if (strcmp(str, "var") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = VAR - RESERVED_BIAS;
    } else if (strcmp(str, "while") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = WHILE - RESERVED_BIAS;
    } else if (strcmp(str, "with") == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = WITH - RESERVED_BIAS;
    } else {
        /* It's an identifier */
        tok->tokentype = IDENTIFIERTOK;
        strcpy(tok->stringval, str);
    }
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

    reservedlookup(str, tok);
    
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

