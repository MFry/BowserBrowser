__author__ = 'Michal'

# JavaScript: Comments & Keywords
#
# In this exercise you will write token definition rules for all of the
# tokens in our subset of JavaScript *except* IDENTIFIER, NUMBER and
# STRING. In addition, you will handle // end of line comments
# as well as /* delimited comments */.
#
# We will assume that JavaScript is case sensitive and that keywords like
# 'if' and 'true' must be written in lowercase. There are 26 possible
# tokens that you must handle. The 'tokens' variable below has been
# initialized below, listing each token's formal name (i.e., the value of
# token.type). In addition, each token has its associated textual string
# listed in a comment. For example, your lexer must convert && to a token
# with token.type 'ANDAND' (unless the && is found inside a comment).
#
# Hint 1: Use an exclusive state for /* comments */. You may want to define
# t_comment_ignore and t_comment_error as well.

import ply.lex as lex

def test_lexer(lexer,input_string):
  lexer.input(input_string)
  result = [ ]
  while True:
    tok = lexer.token()
    if not tok: break
    result = result + [tok.type]
  return result

tokens = (
        'ANDAND',       # &&
        'COMMA',        # ,
        'DIVIDE',       # /
        'ELSE',         # else
        'EQUAL',        # =
        'EQUALEQUAL',   # ==
        'FALSE',        # false
        'FUNCTION',     # function
        'GE',           # >=
        'GT',           # >
       'IDENTIFIER',    # Defined below
        'IF',           # if
        'LBRACE',       # {
        'LE',           # <=
        'LPAREN',       # (
        'LT',           # <
        'MINUS',        # -
        'NOT',          # !
       'NUMBER',        # Defined below
        'OROR',         # ||
        'PLUS',         # +
        'RBRACE',       # }
        'RETURN',       # return
        'RPAREN',       # )
        'SEMICOLON',    # ;
       'STRING',        # Defined below
        'TIMES',        # *
        'TRUE',         # true
        'VAR',          # var
)


# The single comment state can be done by using the maximal munch rule and doesn't need a state of its own
states = (
              ('commentSingle','exclusive'),
              ('commentMulti', 'exclusive'),
)
#
# Write your code here.
#

# Single line comments

def t_commentSingle(token):
    r'//'
    token.lexer.begin('commentSingle')

def t_commentSingle_end(token):
    r'\n'
    token.lexer.lineno += token.value.count('\n')
    token.lexer.begin('INITIAL')

t_commentSingle_ignore = r'\t\v\r '

def t_commentSingle_error(t):
    t.lexer.skip(1)

#Multiline comments

def t_commentMulti(token):
    r'/\*'
    token.lexer.begin('commentMulti')

def t_commentMulti_end(token):
    r'\*/'
    token.lexer.begin('INITIAL')

def t_commentMulti_newline(token):
    r'\n'
    token.lexer.lineno += token.value.count('\n')

t_commentMulti_ignore = r'\t\v\r '

def t_commentMulti_error(t):
    t.lexer.skip(1)

#Standard Javascript tokens
#We employ writing these tokens in shorthand, thus:

t_ANDAND = r'&&'
t_COMMA = r','
t_DIVIDE = r'/'
t_ELSE = r'else'
t_EQUAL = r'='
t_EQUALEQUAL = r'=='
t_FALSE = r'false'
t_FUNCTION = r'function'
t_GE = r'>='
t_GT = r'>'
t_IF = r'if'
t_LBRACE = r'{'
t_LE = r'<='
t_LPAREN = r'\('
t_LT = r'<'
t_MINUS = r'-'
t_NOT = r'!'
t_OROR = r'\|\|'
t_PLUS = r'\+'
t_RBRACE = r'\}'
t_RETURN = r'return'
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_TIMES = r'\*'
t_TRUE = r'true'
t_VAR = r'var'

# JavaScript: Numbers & Strings
#
# In this exercise you will finish out the token definitions for JavaScript
# by handling Numbers, Identifiers and Strings.
#
# We have split the lexing of JavaScript into two exercises so that
# you have a chance to demonstrate your mastery of the concepts
# independently (i.e., so that you can get one of them right even if the
# other proves difficult). We could easily make a full JavaScript lexer by
# putting all of the rules together.
#
# For this assignment, a JavaScript IDENTIFIER must start with an upper- or
# lower-case character. It can then contain any number of upper- or
# lower-case characters or underscores. Its token.value is the textual
# string of the identifier.
#       Yes:    my_age
#       Yes:    cRaZy
#       No:     _starts_with_underscore
#
# For this assignment, a JavaScript NUMBER is one or more digits. A NUMBER
# can start with an optional negative sign. A NUMBER can contain a decimal
# point, which can then be followed by zero or more additional digits. Do
# not worry about hexadecimal (only base 10 is allowed in this problem).
# The token.value of a NUMBER is its floating point value (NOT a string).
#       Yes:    123
#       Yes:    -456
#       Yes:    78.9
#       Yes:    10.
#       No:     +5
#       No:     1.2.3
#
# For this assignment, a JavaScript STRING is zero or more characters
# contained in double quotes. A STRING may contain escaped characters.
# Notably, \" does not end a string. The token.value of a STRING is
# its contents (not including the outer double quotes).
#       Yes:    "hello world"
#       Yes:    "this has \"escaped quotes\""
#       No:     "no"t one string"
#
# Hint: float("2.3") = 2.3

def t_STRING(token):
    r'"([^"\\]|(\\.))*"'# My original solution that wasn't as robust r'\".*[^\\]\"'
    token.value = token.value[1:-1]
    return token

def t_IDENTIFIER(token):
    r'[a-zA-Z](?: [a-zA-Z_]+)?'
    return token

def t_NUMBER(token):
    r'[-]?[0-9]+(?:\.[0-9]+)?'
    token.value = float(token.value)
    return token



t_ignore = ' \t\v\r' # whitespace

def t_newline(t):
        r'\n'
        t.lexer.lineno += t.value.count('\n')

def t_error(t):
        print ("JavaScript Lexer: Illegal character " + t.value[0])
        t.lexer.skip(1)

# We have included two test cases to help you debug your lexer. You will
# probably want to write some of your own.

lexer = lex.lex()

def test_lexer(input_string):
  lexer.input(input_string)
  result = [ ]
  while True:
    tok = lexer.token()
    if not tok: break
    result = result + [tok.type]
  return result

input1 = """ - !  && () * , / ; { || } + < <= = == > >= else false function
if return true var """

output1 = ['MINUS', 'NOT', 'ANDAND', 'LPAREN', 'RPAREN', 'TIMES', 'COMMA',
'DIVIDE', 'SEMICOLON', 'LBRACE', 'OROR', 'RBRACE', 'PLUS', 'LT', 'LE',
'EQUAL', 'EQUALEQUAL', 'GT', 'GE', 'ELSE', 'FALSE', 'FUNCTION', 'IF',
'RETURN', 'TRUE', 'VAR']

print (test_lexer(input1) == output1)

input2 = """
if // else mystery
=/*=*/=
true /* false
*/ return"""

output2 = ['IF', 'EQUAL', 'EQUAL', 'TRUE', 'RETURN']

print (test_lexer(input2) == output2)

input3 = 'some_identifier -12.34 "a \\"escape\\" b"'
output3 = ['IDENTIFIER', 'some_identifier', 'NUMBER', -12.34, 'STRING',
'a \\"escape\\" b']
print (test_lexer(input3) == output3)


input4 = '-12x34'
output4 = ['NUMBER', -12.0, 'IDENTIFIER', 'x', 'NUMBER', 34.0]
print (test_lexer(input4) == output4)
