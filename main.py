import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'if'     : 'IF',
    'else'   : 'ELSE',
    'while'  : 'WHILE',
    'for'    : 'FOR' ,
    'void'   : 'VOID',
    'int'    : 'INT',
    'float'  : 'FLOAT',
    'double' : 'DOUBLE',
    'char'   : 'CHAR',
    'return' : 'RETURN',
 }

tokens = [
     'NUMBER', 'ID' ,
     'PLUS' ,'MINUS' ,'TIMES' ,'DIVIDE' ,'EQUALS',
     'LPAREN' ,'RPAREN', 'LBRACKET', 'RBRACKET',
     'SEMICOLON', 'COMMA'
] + list(reserved.values())


# Tokens

t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_EQUALS    = r'='
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LBRACKET  = r'\{'
t_RBRACKET  = r'\}'
t_SEMICOLON = r'\;'
t_COMMA     = r'\,'
t_IF        = r'if'
t_VOID      = r'void'
t_INT       = r'int'
t_FLOAT     = r'float'
t_DOUBLE    = r'double'
t_CHAR      = r'char'
t_RETURN    = r'return'


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


precedence = (
    ('left' ,'PLUS' ,'MINUS'),
    ('left' ,'TIMES' ,'DIVIDE'),
    ('right' ,'UMINUS'),
)


types_dict = {
    'int' : 'i32'
}

output_f = open("output.rs", "w")

def p_program(t):
    "program : function_declaration"

def p_line(t):
    '''line : statement SEMICOLON
            | expression SEMICOLON
            | loop_statement
            | if_statement'''
    t[0] = t[1] + ";"

def p_assign(t):
    '''statement : ID EQUALS expression
                | declaration EQUALS expression'''
    t[0] = t[1] + " = " + str(t[3])
    print("Assign: ",t[0])

def p_void_function_declaration(t):
    'statement : VOID ID LPAREN args RPAREN LBRACKET body RBRACKET'
    print('Declared function: ', t[3])

def p_function_declaration(t):
    'function_declaration : type ID LPAREN args RPAREN LBRACKET body RBRACKET'

    print('Declared function: ', t[1], t[2], t[4], t[7])

    output = ""
    if (t[2]=='main'):
        for line in t[7]:
            if len(line)>=6 and line[0:6] == 'return':
                t[7].remove(line)
        output += "fn main("

    for i in range(len(t[4])):
        output += t[4][i]
        if i < (len(t[4])-1):
            output += ", "
    output += "){\n"
    for line in t[7]:
        output += "    " + line + "\n"
    output+="}"

    output_f.write(output)

def p_declaration(t):
    'declaration : type ID'
    t[0] = "mut " + t[2] + ": " + types_dict[t[1]]

def p_body(t):
    '''body : line body
            | empty'''
    if len(t) == 3:
        if t[2] is not None:
            t[0] = [t[1]] + t[2]
        else:
            t[0] = [t[1]]
    elif t[1] is not None:
        t[0] = [t[1]]

    print("Body:", t[0])

def p_if(t):
    '''if_statement : IF LPAREN expression RPAREN LBRACKET body RBRACKET else_statement
                    | IF LPAREN expression RPAREN line else_statement
    '''
    print("if")

def p_else(t):
    '''else_statement : ELSE LBRACKET body RBRACKET
                        | ELSE line
                        | empty
    '''
    print("else")

def p_while(t):
    '''loop_statement : WHILE LPAREN expression RPAREN LBRACKET body RBRACKET
    '''
    print("while loop")

def p_for(t):
    '''loop_statement : FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LBRACKET body RBRACKET
    '''
    print("for loop")

def p_args(t):
    '''args : declaration COMMA args
            | declaration
            | empty'''

    if len(t) == 4:
        t[0] = [t[1]] + t[3]
    elif t[1] is not None:
        t[0] = [t[1]]

    print("Args:", t[0])


def p_return_statement(t):
    '''statement : RETURN ID
                | RETURN NUMBER'''
    t[0] = "return " + str(t[2])

    print("Return:", t[0])

def p_expression_compare(t):
    '''expression : expression EQUALS expression
    '''

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

output_f.close()