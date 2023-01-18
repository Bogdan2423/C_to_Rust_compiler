import ply.yacc as yacc
import ply.lex as lex
import os

reserved = {
    'if'     : 'IF',
    'else'   : 'ELSE',
    'while'  : 'WHILE',
    'for'    : 'FOR' ,
    'void'   : 'VOID',
    'int'    : 'INT',
    'float'  : 'FLOAT',
    'double' : 'DOUBLE',
    'return' : 'RETURN',
    'printf' : 'PRINTF',
    '=='     : 'COMPARISON',
    '++'     : 'INCREMENT',
    '--'     : 'DECREMENT',
    '<='     : 'LESSEQUAL',
    '>='     : 'GREATEREQUAL'
 }

tokens = [
     'NUMBER', 'ID' , 'STRING',
     'PLUS' ,'MINUS' ,'TIMES' ,'DIVIDE' ,'EQUALS',
     'GREATER', 'LESS', 'MODULO',
     'LPAREN' ,'RPAREN', 'LBRACKET', 'RBRACKET',
     'SEMICOLON', 'COMMA'
] + list(reserved.values())


# Tokens

t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_EQUALS    = r'='
t_GREATER   = r'>'
t_LESS      = r'<'
t_GREATEREQUAL = r'>='
t_LESSEQUAL = r'<='
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LBRACKET  = r'\{'
t_RBRACKET  = r'\}'
t_SEMICOLON = r'\;'
t_COMMA     = r'\,'
t_COMPARISON = r'=='
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_MODULO    = r'%'
t_IF        = r'if'
t_VOID      = r'void'
t_INT       = r'int'
t_FLOAT     = r'float'
t_DOUBLE    = r'double'
t_RETURN    = r'return'
t_PRINTF    = r'printf'


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_STRING(t):
    r'".*"'
    return t


def t_NUMBER(t):
    r'\d+\.?\d*'
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


types_dict = {
    'int' : 'i32',
    'float' : 'f32',
    'double' : 'f64',
}

var_dict = dict()

output_f = open("output.rs", "w")

def p_program(t):
    '''program : function_declaration program
                | empty'''


def p_function_declaration(t):
    '''function_declaration : type ID LPAREN args RPAREN LBRACKET body RBRACKET
                            | VOID ID LPAREN args RPAREN LBRACKET body RBRACKET'''

    output = ""
    if (t[2]=='main'):
        for line in t[7]:
            if len(line)>=6 and line[0:6] == 'return':
                t[7].remove(line)
        output += "fn main("

    else:
        output += "fn " + t[2]+"("

    for i in range(len(t[4])):
        output += t[4][i]
        if i < (len(t[4])-1):
            output += ", "
    output += ")"

    if t[2]!="main" and t[1]!="void":
        output+="->"+t[1]

    output +="{\n"
    for line in t[7]:
        output += "    " + line + "\n"
    output+="}"

    output_f.write(output+os.linesep)


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


def p_line(t):
    '''line : statement SEMICOLON
            | expression SEMICOLON
            | loop_statement
            | if_statement'''
    t[0] = t[1]
    if len(t)==3:
        t[0]+=";"



def p_if(t):
    '''if_statement : IF LPAREN expression RPAREN LBRACKET body RBRACKET else_statement
                    | IF LPAREN expression RPAREN line else_statement
    '''
    if "<" not in t[3] and ">" not in t[3] and "==" not in t[3]:
        t[0] = "if " + t[3] + " != 0 {\n"
    else:
        t[0] = "if " + t[3] + " {\n"
    if len(t) == 9:
        for line in t[6]:
            t[0] += "    " + line + "\n"
        t[0]+="}"

        t[0] += t[8]

    else:
        t[0] += "    " + t[5] + "\n"
        t[0] += "   }"
        t[0] += t[6]

def p_else(t):
    '''else_statement : ELSE LBRACKET body RBRACKET
                        | ELSE line
                        | empty
    '''
    t[0] = ""
    if len(t) >= 3:
        t[0] += "   else{\n"
        if len(t) == 3:
            t[0] += "   " + t[2]
        elif len(t) == 5:
            for line in t[3]:
                t[0] += "    " + line + "\n"
        t[0]+="\n   }"

def p_while(t):
    '''loop_statement : WHILE LPAREN expression RPAREN LBRACKET body RBRACKET
                        | WHILE LPAREN expression RPAREN line
    '''
    t[0] = "while " + t[3] + " {\n"
    if len(t)==8:
        for line in t[6]:
            t[0] += "   " + line + "\n"
    else:
        t[0] += t[5]
    t[0] += "   }"

def p_for(t):
    '''loop_statement : FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LBRACKET body RBRACKET
                        | FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN line
    '''
    t[0] = t[3]
    t[0] += ";\n    while " + t[5] + " {\n"
    if len(t) == 11:
        for line in t[10]:
            t[0] += "   " + line + "\n"
    else:
        t[0] += "   "+t[9] + "\n"
    t[0] += "   "+t[7]
    t[0] += ";\n   }"

def p_args(t):
    '''args : declaration COMMA args
            | declaration
            | empty'''

    if len(t) == 4:
        t[0] = [t[1][4:]] + t[3]
    elif t[1] is not None:
        t[0] = [t[1][4:]]
    else:
        t[0] = []

def p_function_args(t):
    '''function_args : expression COMMA function_args
                    | expression
                    | empty
    '''

    if len(t) == 4:
        t[0] = [t[1]] + t[3]
    elif t[1] is not None:
        t[0] = [t[1]]
    else:
        t[0] = []

def p_return_statement(t):
    '''statement : RETURN ID
                | RETURN expression
                | RETURN'''
    if len(t)==3:
        t[0] = "return " + str(t[2])
    else:
        t[0] = "return"

def p_function_call(t):
    '''expression : ID LPAREN function_args RPAREN
    '''
    t[0] = t[1]+"("
    for i in range(len(t[3])):
        if t[3][i] in var_dict and (var_dict[t[3][i]]=='f32' or var_dict[t[3][i]]=='f64'):
            t[0] += t[3][i]+".into()"
        else:
            t[0] += t[3][i]
        if i < (len(t[3]) - 1):
            t[0] += ", "
    t[0] += ")"


def p_printf(t):
    ''' expression : PRINTF LPAREN STRING COMMA function_args RPAREN
                     | PRINTF LPAREN STRING RPAREN'''
    string = ""
    i = 0
    while i<len(t[3]):
        if t[3][i]=='%' and i+1<len(t[3])-1:
            string+='{}'
            i+=2
        else:
            string+=t[3][i]
            i+=1

    t[0] = "println!("+string
    if len(t)==7:
        t[0] += ", "
        for i in range(len(t[5])):
            t[0] += t[5][i]
            if i < (len(t[5]) - 1):
                t[0] += ", "

    t[0] += ")"

def p_declaration(t):
    'declaration : type ID'
    t[0] = "let mut " + t[2] + ": " + t[1]
    var_dict[t[2]] = t[1]

def p_assign(t):
    '''statement : ID EQUALS expression
                | declaration EQUALS expression'''
    t[0] = t[1] + " = (" + str(t[3])
    if t[1][0:7]=="let mut":
        var_name = t[1][8]
        i = 9
        while t[1][i]!=":":
            var_name+=t[1][i]
            i+=1
    else:
        var_name = t[1]
    var_type = var_dict[var_name]
    t[0] += ") as "+var_type

def p_operation_assignment(t):
    '''statement : expression math_operator EQUALS expression
        '''
    t[0] = t[1] + t[2] + t[3]
    if t[1] in var_dict:
        t[0] += "("+t[4]+") as "+var_dict[t[1]]
    else:
        t[0] += t[4]

def p_increment(t):
    '''statement : ID INCREMENT
                | ID DECREMENT
    '''
    if t[1] in var_dict and var_dict[t[1]]=='i32':
        if t[2] == '++':
            t[0] = t[1] + "+= 1"
        else:
            t[0] = t[1] + "-= 1"
    else:
        if t[2] == '++':
            t[0] = t[1] + "+= 1.0"
        else:
            t[0] = t[1] + "-= 1.0"

def p_compare_expression(t):
    '''expression : expression compare_operator expression
    '''
    t[0] = t[1] + t[2]
    if t[1] in var_dict:
        t[0] += "("+t[3]+") as "+var_dict[t[1]]
    else:
        t[0] += t[3]


def p_expression_binop(t):
    '''expression : expression math_operator expression
    '''
    t[0] = t[1] + t[2]
    if t[1] in var_dict:
        t[0] += t[3]+" as "+var_dict[t[1]]
    else:
        t[0] += t[3]


def p_math_operator(t):
    '''math_operator : PLUS
                      |  MINUS
                      |  TIMES
                      |  DIVIDE
                      |  MODULO '''
    t[0] = t[1]

def p_compare_operator(t):
    '''compare_operator : COMPARISON
                    |  GREATER
                    |  LESS
                    |  LESSEQUAL
                    |  GREATEREQUAL'''
    t[0] = t[1]

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = "("+t[2]+")"

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = str(t[1])

def p_expression_id(t):
    'expression : ID'
    t[0] = t[1]


def p_type(t):
    '''type : INT
              | FLOAT
              | DOUBLE'''
    t[0] = types_dict[t[1]]

def p_empty(t):
    'empty :'

def p_error(t):
    print("Syntax error at '%s'" % t)

parser = yacc.yacc()


f = open("input.c", "r")
lines = f.read()
parser.parse(lines)
f.close()

output_f.close()