
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftPLUSMINUSleftTIMESDIVIDErightUMINUSCHAR COMMA DIVIDE DOUBLE ELSE EQUALS FLOAT FOR ID IF INT LBRACKET LPAREN MINUS NUMBER PLUS RBRACKET RETURN RPAREN SEMICOLON TIMES VOID WHILEprogram : function_declaration program\n                | emptyline : statement SEMICOLON\n            | expression SEMICOLON\n            | loop_statement\n            | if_statementstatement : ID EQUALS expression\n                | declaration EQUALS expressionstatement : VOID ID LPAREN args RPAREN LBRACKET body RBRACKETfunction_declaration : type ID LPAREN args RPAREN LBRACKET body RBRACKETdeclaration : type IDbody : line body\n            | emptyif_statement : IF LPAREN expression RPAREN LBRACKET body RBRACKET else_statement\n                    | IF LPAREN expression RPAREN line else_statement\n    else_statement : ELSE LBRACKET body RBRACKET\n                        | ELSE line\n                        | empty\n    loop_statement : WHILE LPAREN expression RPAREN LBRACKET body RBRACKET\n    loop_statement : FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LBRACKET body RBRACKET\n    args : declaration COMMA args\n            | declaration\n            | emptyfunction_args : ID COMMA function_args\n                    | ID\n                    | empty\n    statement : RETURN ID\n                | RETURN expressionexpression : ID LPAREN function_args RPAREN\n    expression : expression EQUALS EQUALS expression\n    expression : expression PLUS expression\n                  | expression MINUS expression\n                  | expression TIMES expression\n                  | expression DIVIDE expressionexpression : MINUS expression %prec UMINUSexpression : LPAREN expression RPARENexpression : NUMBERexpression : IDtype : INT\n              | FLOAT\n              | DOUBLE\n              | CHARempty :'
    
_lr_action_items = {'$end':([0,1,2,3,9,42,],[-43,0,-43,-2,-1,-10,]),'INT':([0,2,11,18,19,24,28,29,42,44,45,57,70,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[5,5,5,5,5,5,-5,-6,-10,-3,-4,5,5,5,5,5,-43,5,5,-15,5,-18,-19,-43,5,-17,-14,5,-16,-20,]),'FLOAT':([0,2,11,18,19,24,28,29,42,44,45,57,70,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[6,6,6,6,6,6,-5,-6,-10,-3,-4,6,6,6,6,6,-43,6,6,-15,6,-18,-19,-43,6,-17,-14,6,-16,-20,]),'DOUBLE':([0,2,11,18,19,24,28,29,42,44,45,57,70,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[7,7,7,7,7,7,-5,-6,-10,-3,-4,7,7,7,7,7,-43,7,7,-15,7,-18,-19,-43,7,-17,-14,7,-16,-20,]),'CHAR':([0,2,11,18,19,24,28,29,42,44,45,57,70,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[8,8,8,8,8,8,-5,-6,-10,-3,-4,8,8,8,8,8,-43,8,8,-15,8,-18,-19,-43,8,-17,-14,8,-16,-20,]),'ID':([4,5,6,7,8,12,19,22,24,28,29,31,32,33,38,39,44,45,47,48,49,50,51,56,57,58,64,75,80,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[10,-39,-40,-41,-42,16,21,41,21,-5,-6,52,53,41,41,60,-3,-4,41,41,41,41,41,41,73,41,41,60,41,21,21,21,-43,21,73,-15,21,-18,-19,-43,21,-17,-14,21,-16,-20,]),'LPAREN':([10,19,21,22,24,28,29,32,33,35,36,37,38,41,44,45,47,48,49,50,51,52,53,56,58,64,80,81,84,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[11,22,39,22,22,-5,-6,22,22,56,57,58,22,39,-3,-4,22,22,22,22,22,70,39,22,22,22,22,22,22,22,-43,22,-15,22,-18,-19,-43,22,-17,-14,22,-16,-20,]),'RPAREN':([11,13,14,15,16,18,20,34,39,40,41,53,54,55,59,60,61,62,63,65,66,67,68,69,70,71,74,75,76,77,78,82,97,101,],[-43,17,-22,-23,-11,-43,-21,-37,-43,63,-38,-27,-28,-35,-7,-25,76,-26,-36,-31,-32,-33,-34,-8,-43,79,81,-43,-29,-30,83,-24,102,-9,]),'COMMA':([14,16,60,],[18,-11,75,]),'EQUALS':([16,21,27,30,34,40,41,46,53,54,55,59,63,65,66,67,68,69,71,73,74,76,77,85,],[-11,38,46,51,-37,46,-38,64,-38,46,-35,46,-36,-31,-32,-33,-34,46,46,38,46,-29,46,46,]),'LBRACKET':([17,79,81,83,93,102,],[19,84,86,88,99,105,]),'RBRACKET':([19,23,24,25,28,29,43,44,45,84,86,87,88,89,91,92,94,95,96,98,99,100,103,104,105,106,107,108,],[-43,42,-43,-13,-5,-6,-12,-3,-4,-43,-43,-43,-43,96,98,-15,-18,101,-19,-43,-43,-17,-14,106,-43,-16,108,-20,]),'VOID':([19,24,28,29,44,45,57,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[31,31,-5,-6,-3,-4,31,31,31,31,-43,31,31,-15,31,-18,-19,-43,31,-17,-14,31,-16,-20,]),'RETURN':([19,24,28,29,44,45,57,81,84,86,87,88,90,92,93,94,96,98,99,100,103,105,106,108,],[32,32,-5,-6,-3,-4,32,32,32,32,-43,32,32,-15,32,-18,-19,-43,32,-17,-14,32,-16,-20,]),'MINUS':([19,21,22,24,27,28,29,32,33,34,38,40,41,44,45,47,48,49,50,51,53,54,55,56,58,59,63,64,65,66,67,68,69,71,74,76,77,80,81,84,85,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[33,-38,33,33,48,-5,-6,33,33,-37,33,48,-38,-3,-4,33,33,33,33,33,-38,48,-35,33,33,48,-36,33,-31,-32,-33,-34,48,48,48,-29,48,33,33,33,48,33,-43,33,-15,33,-18,-19,-43,33,-17,-14,33,-16,-20,]),'NUMBER':([19,22,24,28,29,32,33,38,44,45,47,48,49,50,51,56,58,64,80,81,84,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[34,34,34,-5,-6,34,34,34,-3,-4,34,34,34,34,34,34,34,34,34,34,34,34,-43,34,-15,34,-18,-19,-43,34,-17,-14,34,-16,-20,]),'WHILE':([19,24,28,29,44,45,81,84,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[35,35,-5,-6,-3,-4,35,35,35,-43,35,-15,35,-18,-19,-43,35,-17,-14,35,-16,-20,]),'FOR':([19,24,28,29,44,45,81,84,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[36,36,-5,-6,-3,-4,36,36,36,-43,36,-15,36,-18,-19,-43,36,-17,-14,36,-16,-20,]),'IF':([19,24,28,29,44,45,81,84,86,87,88,92,93,94,96,98,99,100,103,105,106,108,],[37,37,-5,-6,-3,-4,37,37,37,-43,37,-15,37,-18,-19,-43,37,-17,-14,37,-16,-20,]),'SEMICOLON':([21,26,27,34,41,53,54,55,59,63,65,66,67,68,69,72,76,77,85,101,],[-38,44,45,-37,-38,-27,-28,-35,-7,-36,-31,-32,-33,-34,-8,80,-29,-30,90,-9,]),'PLUS':([21,27,34,40,41,53,54,55,59,63,65,66,67,68,69,71,74,76,77,85,],[-38,47,-37,47,-38,-38,47,-35,47,-36,-31,-32,-33,-34,47,47,47,-29,47,47,]),'TIMES':([21,27,34,40,41,53,54,55,59,63,65,66,67,68,69,71,74,76,77,85,],[-38,49,-37,49,-38,-38,49,-35,49,-36,49,49,-33,-34,49,49,49,-29,49,49,]),'DIVIDE':([21,27,34,40,41,53,54,55,59,63,65,66,67,68,69,71,74,76,77,85,],[-38,50,-37,50,-38,-38,50,-35,50,-36,50,50,-33,-34,50,50,50,-29,50,50,]),'ELSE':([28,29,44,45,87,92,94,96,98,100,103,106,108,],[-5,-6,-3,-4,93,-15,-18,-19,93,-17,-14,-16,-20,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,2,],[1,9,]),'function_declaration':([0,2,],[2,2,]),'empty':([0,2,11,18,19,24,39,70,75,84,86,87,88,98,99,105,],[3,3,15,15,25,25,62,15,62,25,25,94,25,94,25,25,]),'type':([0,2,11,18,19,24,57,70,81,84,86,88,90,93,99,105,],[4,4,12,12,12,12,12,12,12,12,12,12,12,12,12,12,]),'args':([11,18,70,],[13,20,78,]),'declaration':([11,18,19,24,57,70,81,84,86,88,90,93,99,105,],[14,14,30,30,30,14,30,30,30,30,30,30,30,30,]),'body':([19,24,84,86,88,99,105,],[23,43,89,91,95,104,107,]),'line':([19,24,81,84,86,88,93,99,105,],[24,24,87,24,24,24,100,24,24,]),'statement':([19,24,57,81,84,86,88,90,93,99,105,],[26,26,72,26,26,26,26,97,26,26,26,]),'expression':([19,22,24,32,33,38,47,48,49,50,51,56,58,64,80,81,84,86,88,93,99,105,],[27,40,27,54,55,59,65,66,67,68,69,71,74,77,85,27,27,27,27,27,27,27,]),'loop_statement':([19,24,81,84,86,88,93,99,105,],[28,28,28,28,28,28,28,28,28,]),'if_statement':([19,24,81,84,86,88,93,99,105,],[29,29,29,29,29,29,29,29,29,]),'function_args':([39,75,],[61,82,]),'else_statement':([87,98,],[92,103,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> function_declaration program','program',2,'p_program','main.py',95),
  ('program -> empty','program',1,'p_program','main.py',96),
  ('line -> statement SEMICOLON','line',2,'p_line','main.py',99),
  ('line -> expression SEMICOLON','line',2,'p_line','main.py',100),
  ('line -> loop_statement','line',1,'p_line','main.py',101),
  ('line -> if_statement','line',1,'p_line','main.py',102),
  ('statement -> ID EQUALS expression','statement',3,'p_assign','main.py',106),
  ('statement -> declaration EQUALS expression','statement',3,'p_assign','main.py',107),
  ('statement -> VOID ID LPAREN args RPAREN LBRACKET body RBRACKET','statement',8,'p_void_function_declaration','main.py',112),
  ('function_declaration -> type ID LPAREN args RPAREN LBRACKET body RBRACKET','function_declaration',8,'p_function_declaration','main.py',116),
  ('declaration -> type ID','declaration',2,'p_declaration','main.py',147),
  ('body -> line body','body',2,'p_body','main.py',151),
  ('body -> empty','body',1,'p_body','main.py',152),
  ('if_statement -> IF LPAREN expression RPAREN LBRACKET body RBRACKET else_statement','if_statement',8,'p_if','main.py',164),
  ('if_statement -> IF LPAREN expression RPAREN line else_statement','if_statement',6,'p_if','main.py',165),
  ('else_statement -> ELSE LBRACKET body RBRACKET','else_statement',4,'p_else','main.py',181),
  ('else_statement -> ELSE line','else_statement',2,'p_else','main.py',182),
  ('else_statement -> empty','else_statement',1,'p_else','main.py',183),
  ('loop_statement -> WHILE LPAREN expression RPAREN LBRACKET body RBRACKET','loop_statement',7,'p_while','main.py',196),
  ('loop_statement -> FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LBRACKET body RBRACKET','loop_statement',11,'p_for','main.py',201),
  ('args -> declaration COMMA args','args',3,'p_args','main.py',206),
  ('args -> declaration','args',1,'p_args','main.py',207),
  ('args -> empty','args',1,'p_args','main.py',208),
  ('function_args -> ID COMMA function_args','function_args',3,'p_function_args','main.py',218),
  ('function_args -> ID','function_args',1,'p_function_args','main.py',219),
  ('function_args -> empty','function_args',1,'p_function_args','main.py',220),
  ('statement -> RETURN ID','statement',2,'p_return_statement','main.py',228),
  ('statement -> RETURN expression','statement',2,'p_return_statement','main.py',229),
  ('expression -> ID LPAREN function_args RPAREN','expression',4,'p_function_call','main.py',235),
  ('expression -> expression EQUALS EQUALS expression','expression',4,'p_expression_compare','main.py',246),
  ('expression -> expression PLUS expression','expression',3,'p_expression_binop','main.py',250),
  ('expression -> expression MINUS expression','expression',3,'p_expression_binop','main.py',251),
  ('expression -> expression TIMES expression','expression',3,'p_expression_binop','main.py',252),
  ('expression -> expression DIVIDE expression','expression',3,'p_expression_binop','main.py',253),
  ('expression -> MINUS expression','expression',2,'p_expression_uminus','main.py',257),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_expression_group','main.py',261),
  ('expression -> NUMBER','expression',1,'p_expression_number','main.py',265),
  ('expression -> ID','expression',1,'p_expression_id','main.py',269),
  ('type -> INT','type',1,'p_type','main.py',274),
  ('type -> FLOAT','type',1,'p_type','main.py',275),
  ('type -> DOUBLE','type',1,'p_type','main.py',276),
  ('type -> CHAR','type',1,'p_type','main.py',277),
  ('empty -> <empty>','empty',0,'p_empty','main.py',281),
]
