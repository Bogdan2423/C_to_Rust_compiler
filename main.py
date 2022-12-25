import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'int' : 'INT',
    'float' : 'FLOAT',
    'double' : 'DOUBLE',
    'char' : 'CHAR',
 }

tokens = [
    'NAME' ,'NUMBER', 'ID' ,
     'PLUS' ,'MINUS' ,'TIMES' ,'DIVIDE' ,'EQUALS',
    'LPAREN' ,'RPAREN', 'LBRACKET', 'RBRACKET', 'SEMICOLON', 'COMMA'
] + list(reserved.values())


# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\{'
t_RBRACKET = r'\}'
t_SEMICOLON = r'\;'
t_COMMA = r'\,'
t_IF = r'if'
t_INT = r'int'
t_FLOAT = r'float'
t_DOUBLE = r'double'
t_CHAR = r'char'


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_NUMBER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer

lexer = lex.lex()

# Parsing rules

precedence = (
    ('left' ,'PLUS' ,'MINUS'),
    ('left' ,'TIMES' ,'DIVIDE'),
    ('right' ,'UMINUS'),
)

# dictionary of names
names = { }


def p_line(t):
    'line : statement SEMICOLON'
    t[0] = t[1]

def p_statement_assign(t):
    '''statement : ID EQUALS expression
                | declaration EQUALS expression'''
    names[t[1]] = t[3]
    print(t[1], t[3])

def p_function_declaration(t):
    'statement : declaration LPAREN args RPAREN LBRACKET body RBRACKET'
    print('Declared function: ', t)

def p_declaration(t):
    'declaration : type ID'
    print(t[1], t[2])
    t[0] = t[2]

def p_body(t):
    '''body : line body
            | empty'''

def p_args(t):
    '''args : declaration
            | declaration COMMA declaration
            | empty'''

def p_statement_expr(t):
    'statement : expression'
    print(t[1])

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    if t[2] == '+'  : t[0] = t[1] + t[3]
    elif t[2] == '-': t[0] = t[1] - t[3]
    elif t[2] == '*': t[0] = t[1] * t[3]
    elif t[2] == '/': t[0] = t[1] / t[3]

def p_expression_uminus(t):
    'expression : MINUS expression %prec UMINUS'
    t[0] = -t[2]

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]

def p_expression_name(t):
    'expression : NAME'
    try:
        t[0] = names[t[1]]
    except LookupError:
        print("Undefined name '%s'" % t[1])
        t[0] = 0

def p_type(t):
    '''type : INT
              | FLOAT
              | DOUBLE
              | CHAR'''
    t[0] = t[1]

def p_empty(t):
    'empty :'
    pass

def p_error(t):
    print("Syntax error at '%s'" % t)

parser = yacc.yacc()


f = open("input.c", "r")
lines = f.read()
parser.parse(lines)
f.close()

'''
while True:
    try:
        s = input('calc > ')
    except EOFError:
        break
    parser.parse(s)
'''