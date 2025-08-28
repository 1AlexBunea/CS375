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
  } else if (strcmp(str, "<>") == 0) {
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
      while ((c = peekchar()) != EOF)
      {
          if (c == ' ' || c == '\n' || c == '\t') {
            getchar();  /* consume whitespace */
          } else if (c == '{') {
            getchar();
            while ((c = peekchar()) != EOF && c != '}') {
                getchar();  // consume characters until }
            }
            
            if (c == '}') {
                getchar();  /* consume the closing } */
            }
          } else if (c == '(') {
              if ((c = peek2char()) == '*') { 
                  getchar();  // consume the (
                  getchar();  // consume the *
                  while ((c = peekchar()) != EOF)
                  {
                      if (c == '*')
                      {
                          getchar();  // consume the *
                          if ((c = peekchar()) == ')') {
                              getchar();  // consume the )
                              break;  // end of comment
                          }
                      }
                      else {
                          getchar();  // consume other characters
                      }
                  }
              } else {
                break;
              }
          }
          else {
              break;  // Not whitespace or comment, stop skipping
          }
      }
    }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  {
    char str[16];
    int c;
    int idx = 0;

    while ((c = peekchar()) != EOF && (CHARCLASS[c] == ALPHA || CHARCLASS[c] == NUMERIC)) {
      c = getchar();
      if (idx < 15) {
        str[idx++] = tolower(c); // Make sure to lowercase for identifiers/keywords
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
        str[idx++] = c;  // Preserve original case in strings
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

    c = peekchar();
    if (c == EOF || CHARCLASS[c] != SPECIAL) {
        return tok;
    }

    // Look for multi-character operators and delimiters first
    if (c == ':') {
        c = getchar();
        str[idx++] = c;
        if (peekchar() == '=') {
            c = getchar();
            str[idx++] = c;
            str[idx] = '\0';
            return (delimiterOrOperator(str, tok));
        }
    } else if (c == '<') {
        c = getchar();
        str[idx++] = c;
        int next = peekchar();
        if (next == '=' || next == '>') {
            c = getchar();
            str[idx++] = c;
            str[idx] = '\0';
            return (delimiterOrOperator(str, tok));
        }
    } else if (c == '>') {
        c = getchar();
        str[idx++] = c;
        if (peekchar() == '=') {
            c = getchar();
            str[idx++] = c;
            str[idx] = '\0';
            return (delimiterOrOperator(str, tok));
        }
    } else if (c == '.') {
        c = getchar();
        str[idx++] = c;
        if (peekchar() == '.') {
            c = getchar();
            str[idx++] = c;
            str[idx] = '\0';
            return (delimiterOrOperator(str, tok));
        }
    } else {
        // Single character operator or delimiter
        c = getchar();
        str[idx++] = c;
    }

    str[idx] = '\0';
    return (delimiterOrOperator(str, tok));
  }

/* Manual string to integer conversion */
int string_to_int(char *str, int *overflow) {
    int result = 0;
    int i = 0;
    *overflow = 0;
    
    while (str[i] != '\0' && isdigit(str[i])) {
        int digit = str[i] - '0';
        
        // Check for overflow before multiplication
        if (result > (2147483647 - digit) / 10) {
            *overflow = 1;
            return 0;
        }
        
        result = result * 10 + digit;
        i++;
    }
    
    return result;
}

/* Manual string to double conversion */
double string_to_double(char *str, int *error) {
    double result = 0.0;
    double fraction = 0.0;
    double divisor = 1.0;
    int exponent = 0;
    int exp_sign = 1;
    int i = 0;
    int decimal_found = 0;
    int exp_found = 0;
    *error = 0;
    
    // Parse integer part
    while (str[i] != '\0' && isdigit(str[i])) {
        result = result * 10.0 + (str[i] - '0');
        i++;
    }
    
    // Parse decimal part
    if (str[i] == '.') {
        decimal_found = 1;
        i++;
        while (str[i] != '\0' && isdigit(str[i])) {
            fraction = fraction * 10.0 + (str[i] - '0');
            divisor *= 10.0;
            i++;
        }
        result += fraction / divisor;
    }
    
    // Parse exponent
    if (str[i] == 'e' || str[i] == 'E') {
        exp_found = 1;
        i++;
        
        // Check for sign
        if (str[i] == '+') {
            i++;
        } else if (str[i] == '-') {
            exp_sign = -1;
            i++;
        }
        
        // Parse exponent value
        while (str[i] != '\0' && isdigit(str[i])) {
            exponent = exponent * 10 + (str[i] - '0');
            i++;
        }
        exponent *= exp_sign;
        
        // Apply exponent manually
        if (exponent > 0) {
            for (int j = 0; j < exponent; j++) {
                result *= 10.0;
                if (result > 3.402823E+38) {
                    *error = 1;
                    return 0.0;
                }
            }
        } else if (exponent < 0) {
            for (int j = 0; j < -exponent; j++) {
                result /= 10.0;
                if (result != 0.0 && result < 1.175495E-38 && result > -1.175495E-38) {
                    *error = 1;
                    return 0.0;
                }
            }
        }
    }
    
    // Check bounds
    if (result > 3.402823E+38 || result < -3.402823E+38) {
        *error = 1;
        return 0.0;
    }
    
    if (result != 0.0 && (result < 1.175495E-38 && result > -1.175495E-38)) {
        *error = 1;
        return 0.0;
    }
    
    return result;
}

/* Get and convert unsigned numbers of all types. */
// If there is an error, we return the type(int vs real), the value is irrelevant, can just default to 0
TOKEN number (TOKEN tok)
  { 
    char str[256];
    int c;
    int idx = 0;
    int has_decimal = 0;
    int has_exponent = 0;
    int significant_digits = 0;
    int after_decimal = 0;
    int leading_zeros = 1;  // Track if we're still in leading zeros
    
    // Read digits before decimal point
    while ((c = peekchar()) != EOF && isdigit(c)) {
        c = getchar();
        str[idx++] = c;
        
        // Count significant digits (skip leading zeros)
        if (c != '0' || !leading_zeros) {
            leading_zeros = 0;
            significant_digits++;
        }
    }
    
    // Check for decimal point
    if ((c = peekchar()) == '.') {
        // Look ahead to see if there's a digit after the decimal
        int lookahead = peek2char();
        if (isdigit(lookahead)) {
            has_decimal = 1;
            c = getchar();  // consume the '.'
            str[idx++] = c;
            after_decimal = 1;
            
            // Read digits after decimal point
            while ((c = peekchar()) != EOF && isdigit(c)) {
                c = getchar();
                str[idx++] = c;
                
                // Count significant digits after decimal
                if (after_decimal) {
                    if (c != '0' || !leading_zeros) {
                        leading_zeros = 0;
                        significant_digits++;
                    }
                }
            }
        }
    }
    
    // Check for exponent (makes it a real number regardless of decimal point)
    if ((c = peekchar()) == 'e' || c == 'E') {
        has_exponent = 1;
        c = getchar();  // consume 'e' or 'E'
        str[idx++] = c;
        
        // Check for optional sign
        if ((c = peekchar()) == '+' || c == '-') {
            c = getchar();
            str[idx++] = c;
        }
        
        // Must have at least one digit after exponent
        if ((c = peekchar()) != EOF && isdigit(c)) {
            while ((c = peekchar()) != EOF && isdigit(c)) {
                c = getchar();
                str[idx++] = c;
            }
        }
    }
    
    str[idx] = '\0';
    
    // Determine if this is an integer or real number
    if (has_decimal || has_exponent) {
        // Real number
        tok->tokentype = NUMBERTOK;
        tok->basicdt = REAL;
        
        // For real numbers, limit to 8 significant digits
        if (significant_digits > 8) {
            // Create a modified string with only 8 significant digits
            char limited_str[256];
            int limit_idx = 0;
            int sig_count = 0;
            int found_first_sig = 0;
            int decimal_seen = 0;
            
            for (int i = 0; i < idx && limit_idx < 255; i++) {
                char ch = str[i];
                
                if (ch == '.') {
                    decimal_seen = 1;
                    limited_str[limit_idx++] = ch;
                } else if (ch == 'e' || ch == 'E') {
                    // Copy the rest of the exponent
                    while (i < idx && limit_idx < 255) {
                        limited_str[limit_idx++] = str[i++];
                    }
                    break;
                } else if (isdigit(ch)) {
                    if (ch != '0' || found_first_sig) {
                        found_first_sig = 1;
                        sig_count++;
                    }
                    
                    if (sig_count <= 8) {
                        limited_str[limit_idx++] = ch;
                    } else if (!decimal_seen) {
                        // If we haven't seen decimal yet and exceeded 8 digits,
                        // replace remaining digits with zeros
                        limited_str[limit_idx++] = '0';
                    }
                } else {
                    limited_str[limit_idx++] = ch;
                }
            }
            limited_str[limit_idx] = '\0';
            
            int error = 0;
            tok->realval = string_to_double(limited_str, &error);
            if (error) {
                printf("Error: Floating point number out of range\n");
                tok->realval = 0.0;
            }
        } else {
            int error = 0;
            tok->realval = string_to_double(str, &error);
            if (error) {
                printf("Error: Floating point number out of range\n");
                tok->realval = 0.0;
            }
        }
    } else {
        // Integer number
        tok->tokentype = NUMBERTOK;
        tok->basicdt = INTEGER;
        
        int overflow = 0;
        int result = string_to_int(str, &overflow);
        if (overflow) {
            printf("Error: Integer overflow - value exceeds 32-bit signed integer range\n");
            tok->intval = 0;
        } else {
            tok->intval = result;
        }
    }
    
    return (tok);
  }

