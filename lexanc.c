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

/* Lookup function to check if a string is a reserved word */
int reservedlookup(char *s) {
    /* Convert string to lowercase for case-insensitive comparison */
    char lowercase[16];
    int i;
    for (i = 0; s[i] && i < 15; i++) {
        lowercase[i] = tolower(s[i]);
    }
    lowercase[i] = '\0';
    
    /* Check against all reserved words */
    if (strcmp(lowercase, "array") == 0) return ARRAY;
    if (strcmp(lowercase, "begin") == 0) return BEGINBEGIN;
    if (strcmp(lowercase, "case") == 0) return CASE;
    if (strcmp(lowercase, "const") == 0) return CONST;
    if (strcmp(lowercase, "do") == 0) return DO;
    if (strcmp(lowercase, "downto") == 0) return DOWNTO;
    if (strcmp(lowercase, "else") == 0) return ELSE;
    if (strcmp(lowercase, "end") == 0) return END;
    if (strcmp(lowercase, "file") == 0) return FILEFILE;
    if (strcmp(lowercase, "for") == 0) return FOR;
    if (strcmp(lowercase, "function") == 0) return FUNCTION;
    if (strcmp(lowercase, "goto") == 0) return GOTO;
    if (strcmp(lowercase, "if") == 0) return IF;
    if (strcmp(lowercase, "label") == 0) return LABEL;
    if (strcmp(lowercase, "nil") == 0) return NIL;
    if (strcmp(lowercase, "of") == 0) return OF;
    if (strcmp(lowercase, "packed") == 0) return PACKED;
    if (strcmp(lowercase, "procedure") == 0) return PROCEDURE;
    if (strcmp(lowercase, "program") == 0) return PROGRAM;
    if (strcmp(lowercase, "record") == 0) return RECORD;
    if (strcmp(lowercase, "repeat") == 0) return REPEAT;
    if (strcmp(lowercase, "set") == 0) return SET;
    if (strcmp(lowercase, "then") == 0) return THEN;
    if (strcmp(lowercase, "to") == 0) return TO;
    if (strcmp(lowercase, "type") == 0) return TYPE;
    if (strcmp(lowercase, "until") == 0) return UNTIL;
    if (strcmp(lowercase, "var") == 0) return VAR;
    if (strcmp(lowercase, "while") == 0) return WHILE;
    if (strcmp(lowercase, "with") == 0) return WITH;
    /* If not found, it's an identifier */
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
      if (idx < 15) {  /* leave room for null terminator */
        str[idx++] = c;
      }
    }
    // Finish parsing the string and now determine if it is an identifier or a reserved word
    str[idx] = '\0';
    /* Check if it's a reserved word or identifier */
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
    printf("Parsing String\n");
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
      if (idx < 15) {  /* leave room for null terminator */
        str[idx++] = c;
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

