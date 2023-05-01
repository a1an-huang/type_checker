import sys
from decaf_lexer import *
from decaf_ast import *

names = {}
classlist = []

InMethods = [MethodRecord("scan_int", "public", "static", [], TypeRecord("INT"), BlockStatement([], 0, 0)), 
             MethodRecord("scan_float", "public", "static", [], TypeRecord("FLOAT"), BlockStatement([], 0, 0))] 

classlist.append(ClassRecord("In", "", InMethods))

OutMethods = [MethodRecord("print", "public", "static", [Param("i", TypeRecord("INT"), 0, 0)], TypeRecord("VOID"), BlockStatement([], 0, 0)),
              MethodRecord("print", "public", "static", [Param("f", TypeRecord("FLOAT"), 0, 0)], TypeRecord("VOID"), BlockStatement([], 0, 0)),
              MethodRecord("print", "public", "static", [Param("b", TypeRecord("BOOLEAN"), 0, 0)], TypeRecord("VOID"), BlockStatement([], 0, 0)),
              MethodRecord("print", "public", "static", [Param("s", TypeRecord("STRING"), 0, 0)], TypeRecord("VOID"), BlockStatement([], 0, 0))]

classlist.append(ClassRecord("Out", "", OutMethods))

precedence = (('right', 'ASSIGN'),
              ('left', 'OR'),
              ('left', 'AND'),
              ('nonassoc', 'EQ', 'NOT_EQ'),
              ('nonassoc', 'LT', 'LTE', 'GT', 'GTE'),
              ('left', 'PLUS', 'MINUS'),
              ('left', 'STAR', 'F_SLASH'),
              ('right', 'UMINUS', 'UPLUS', 'NOT')
              )

def p_program(p):
    'program : class_decl_list'
    return True

def p_class_decl_list(p):
    '''class_decl_list : class_decl class_decl_list
                       | empty'''
    pass

def p_class_decl(p):
    '''class_decl : CLASS ID LEFT_CB class_body_decl_list RIGHT_CB
                  | CLASS ID EXTENDS ID LEFT_CB class_body_decl_list  RIGHT_CB'''
    
    if p[3] == "extends":
        classlist.append(ClassRecord(p[2], p[4], p[6]))
    else:
        classlist.append(ClassRecord(p[2], "", p[4]))

def p_class_body_decl_list(p):
    'class_body_decl_list : class_body_decl_list class_body_decl'

    dlist = p[1]
    decl = p[2]
    
    if isinstance(decl, list):
        for field in decl:
            dlist.append(field)
            p[0] = dlist
    else:
        dlist.append(decl)
        p[0] = dlist

def p_class_body_decl_single(p):
    'class_body_decl_list : class_body_decl'

    decl_list = p[1]
    if not isinstance(decl_list, list):
        p[0] = [decl_list]
    else:
        p[0] = decl_list

def p_class_body_decl(p):
    '''class_body_decl : field_decl
                       | method_decl
                       | constructor_decl'''
    p[0] = p[1]

def p_field_decl(p):
    'field_decl : modifier var_decl'

    var_decl = p[2]
    vars = var_decl.vars
    mod = p[1]
    fieldRecords = []
    for var in vars:
        frecord = FieldRecord(var, mod.vis, mod.app, var_decl.type) 
        fieldRecords.append(frecord)

    p[0] = fieldRecords

def p_modifier_pub_sta(p):
    'modifier : PUBLIC STATIC'
    p[0] = ModInfo("public", "static")

def p_modifier_pri_sta(p):
    'modifier : PRIVATE STATIC'
    p[0] = ModInfo("private", "static")

def p_modifier_pub(p):
    'modifier : PUBLIC'
    p[0] = ModInfo("public", "instance")

def p_modifier_pri(p):
    'modifier : PRIVATE'
    p[0] = ModInfo("private", "instance")

def p_modifier_sta(p):
    'modifier : STATIC'
    p[0] = ModInfo("private", "static")

def p_modifier_empty(p):
    'modifier : empty'
    p[0] = ModInfo("private", "instance")

def p_var_decl(p):
    'var_decl : type variables SEMI_COLON'
    p[0] = VarTypeInfo(p[1], p[2])

def p_type(p):
    '''type : TYPE_INT
            | TYPE_FLOAT
            | TYPE_BOOLEAN
            | ID'''

    if p[1] == "int":
        p[0] = TypeRecord("INT")
    elif p[1] == "float":
        p[0] = TypeRecord("FLOAT")
    elif p[1] == "boolean":
        p[0] = TypeRecord("BOOLEAN")
    else:
        p[0] = TypeRecord(p[1])

def p_variables(p):
    'variables : variables COMMA variable'

    var_list = p[1]
    var = p[3]
    var_list.append(var)
    p[0] = var_list

def p_variable_single(p):
    'variables : variable'
    p[0] = [p[1]]

def p_variable(p):
    'variable : ID'
    p[0] = p[1]

def p_method_decl(p):
    '''method_decl : modifier type ID LEFT_PN formals RIGHT_PN block
                   | modifier TYPE_VOID ID LEFT_PN formals RIGHT_PN block'''
    
    if p[2] == "void":
        p[0] = MethodRecord(p[3], p[1].vis, p[1].app, p[5], TypeRecord("VOID"), p[7])
    else:
        p[0] = MethodRecord(p[3], p[1].vis, p[1].app, p[5], p[2], p[7])

def p_constructor_decl(p):
    'constructor_decl : modifier ID LEFT_PN formals RIGHT_PN block'
    p[0] = ConstructorRecord(p[2], p[1].vis, p[4], p[6])

def p_formals_empty(p):
    'formals : empty'
    p[0] = []

def p_formals(p):
    'formals : formals COMMA formal_param'
    param_list = p[1]
    param = p[3]
    param_list.append(param)
    p[0] = param_list

def p_formal_param_single(p):
    'formals : formal_param'
    p[0] = [p[1]]

def p_formal_param(p):
    'formal_param : type variable'
    p[0] = Param(p[2], p[1], p.lineno(1), p.lineno(2))

def p_block(p):
    'block : LEFT_CB stmt_list RIGHT_CB'
    stmts = p[2]
    firststmt = stmts[0]
    laststmt = stmts[len(stmts) - 1]
    p[0] = BlockStatement(stmts, firststmt.startLine, laststmt.endLine)

def p_stmt_list_empty(p):
    'stmt_list : empty'
    p[0] = [SkipStatement(-1, -1)]
    
def p_stmt_list(p):
    'stmt_list : stmt_list stmt'  
    stmts = p[1]
    stmt = p[2]
    if isinstance(stmts[0], SkipStatement) and len(stmts) == 1:
        stmts[0] = stmt
    else:
        stmts.append(stmt)
    p[0] = stmts

def p_stmt_if(p):
    'stmt : IF LEFT_PN expr RIGHT_PN stmt'

    cond = p[3]
    thenstmt = p[5]
    p[0] = IfStatement(cond, thenstmt, cond.startLine, thenstmt.endLine)

def p_stmt_if_else(p):
    'stmt : IF LEFT_PN expr RIGHT_PN stmt ELSE stmt'

    cond = p[3]
    thenstmt = p[5]
    elsestmt = p[7]
    p[0] = IfElseStatement(cond, thenstmt, elsestmt, cond.startLine, elsestmt.endLine)

def p_stmt_while(p):
    'stmt : WHILE LEFT_PN expr RIGHT_PN stmt'

    expr = p[3]
    stmt = p[5]
    p[0] = WhileStatement(expr, stmt, expr.startLine, stmt.endLine)

def p_stmt_for(p):
    'stmt : FOR LEFT_PN for_cond1 SEMI_COLON for_cond2 SEMI_COLON for_cond3 RIGHT_PN stmt'

    init = p[3]
    loopCond = p[5]
    update = p[7]
    body = p[9]
    p[0] = ForStatement(init, loopCond, update, body, init.startLine, body.endLine)

def p_stmt_return(p):
    'stmt : RETURN return_val SEMI_COLON'

    retvalue = p[2]
    p[0] = ReturnStatement(retvalue, p.lineno(1), p.lineno(3))

def p_stmt_stmt_expr(p):
    'stmt : stmt_expr SEMI_COLON'

    expr = p[1]
    p[0] = ExpressionStatement(expr, expr.startLine, expr.endLine)

def p_stmt_break(p):
    'stmt : BREAK SEMI_COLON'

    p[0] = BreakStatement(p.lineno(1), p.lineno(2))

def p_stmt_continue(p):
    'stmt : CONTINUE SEMI_COLON'

    p[0] = ContinueStatement(p.lineno(1), p.lineno(2))

def p_stmt_block(p):
    'stmt : block'
    p[0] = p[1]

def p_stmt_var_decl(p):
    'stmt : var_decl'

    p[0] = VariableStatement(p[1], p.lineno(1), p.lineno(1))

def p_for_cond1(p):
    '''for_cond1 : stmt_expr
                 | empty'''
    p[0] = p[1]

def p_for_cond2(p):
    '''for_cond2 : expr
                 | empty'''
    p[0] = p[1]

def p_for_cond3(p):
    '''for_cond3 : stmt_expr
                 | empty'''
    p[0] = p[1]

def p_return_val(p):
    '''return_val : expr
                  | empty'''
    p[0] = p[1]

def p_literal_int_const(p):
    'literal : INT_CONST'
    p[0] = IntegerConstant(int(p[1]), p.lineno(1), p.lineno(1))

def p_literal_float_const(p):
    'literal : FLOAT_CONST'
    p[0] = FloatConstant(float(p[1]), p.lineno(1), p.lineno(1))

def p_literal_string_const(p):
    'literal : STRING_CONST'
    p[0] = StringConstant(str(p[1]), p.lineno(1), p.lineno(1))

def p_literal_null(p):
    'literal : NULL'
    p[0] = NullConstant(p.lineno(1), p.lineno(1))

def p_literal_true(p):
    'literal : TRUE'
    p[0] = TrueConstant(p.lineno(1), p.lineno(1))

def p_literal_false(p):
    'literal : FALSE'
    p[0] = FalseConstant(p.lineno(1), p.lineno(1))

def p_primary_literal(p):
    'primary : literal'
    p[0] = p[1]

def p_primary_this(p):
    'primary : THIS'
    p[0] = ThisExpression(p.lineno(1), p.lineno(1))

def p_primary_super(p):
    'primary : SUPER'
    p[0] = SuperExpression(p.lineno(1), p.lineno(1))

def p_primary_paren_expr(p):
    'primary : LEFT_PN expr RIGHT_PN'
    p[0] = p[2]

def p_primary_new_obj(p):
    'primary : NEW ID LEFT_PN arguments RIGHT_PN'
    p[0] = NewObjectExpression(p[2], p[4], p.lineno(1), p.lineno(5))

def p_primary_lhs(p):
    'primary : lhs'
    p[0] = p[1]

def p_primary_method_invocation(p):
    'primary : method_invocation'
    p[0] = p[1]

def p_arguments_empty(p):
    'arguments : empty'
    p[0] = []

def p_arguments(p):
    'arguments : arguments COMMA expr'

    args = p[1]
    expr = p[3]
    args.append(expr)
    p[0] = args

def p_arguments_single(p):
    'arguments : expr'
    p[0] = [p[1]]

def p_lhs(p):
    'lhs : field_access'
    p[0] = p[1]

def p_field_access_id(p):
    'field_access : ID'
    p[0] = VariableExpression(p[1], p.lineno(1), p.lineno(1))

def p_field_access_primary(p):
    'field_access : primary DOT ID'
    p[0] = FieldAccessExpression(p[1], p[3], p.lineno(1), p.lineno(3))

def p_method_invocation(p):
    'method_invocation : field_access LEFT_PN arguments RIGHT_PN'

    field_access = p[1]
    args = p[3]

    p[0] = MethodCallExpression(field_access.base, field_access.name, args, p.lineno(1), p.lineno(4))

def p_expr(p):
    '''expr : primary
            | assign'''
    
    p[0] = p[1]
    
#def p_expr(p):
#   '''expr : primary
#            | assign
#            | expr arith_op expr
#            | expr bool_op expr
#            | unary_op expr'''
#    pass
        
def p_assign_eq(p):
    'assign : lhs ASSIGN expr'
    lhs = p[1]
    rhs = p[3]
    p[0] = AssignExpression(lhs, rhs, p.lineno(1), p.lineno(3))

def p_assign_post_inc(p):
    'assign : lhs INCREMENT'
    p[0] = AutoExpression(p[1], "increment", "post", p.lineno(1), p.lineno(2))

def p_assign_post_dec(p):
    'assign : lhs DECREMENT'
    p[0] = AutoExpression(p[1], "decrement", "post", p.lineno(1), p.lineno(2))

def p_assign_pre_inc(p):
    'assign : INCREMENT lhs'
    p[0] = AutoExpression(p[2], "increment", "pre", p.lineno(1), p.lineno(2))

def p_assign_pre_dec(p):
    'assign : DECREMENT lhs'
    p[0] = AutoExpression(p[2], "decrement", "pre", p.lineno(1), p.lineno(2))

#def p_assign(p):
#    '''assign : lhs ASSIGN expr
#              | lhs PLUS PLUS
#              | PLUS PLUS lhs
#              | lhs MINUS MINUS
#              | MINUS MINUS lhs'''
#    pass

def p_add_expr(p):
    'expr : expr PLUS expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_sub_expr(p):
    'expr : expr MINUS expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_mult_exp(p):
    'expr : expr STAR expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_div_expr(p):
    'expr : expr F_SLASH expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_conj_expr(p):
    'expr : expr AND expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_disj_expr(p):
    'expr : expr OR expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_equals_expr(p):
    'expr : expr EQ expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_notequals_expr(p):
    'expr : expr NOT_EQ expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_lt_expr(p):
    'expr : expr LT expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_lte_expr(p):
    'expr : expr LTE expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_gt_expr(p):
    'expr : expr GT expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_gte_expr(p):
    'expr : expr GTE expr'
    operator = p[2]
    op1 = p[1]
    op2 = p[3]
    p[0] = BinaryExpression(operator, op1, op2, p.lineno(1), p.lineno(3))

def p_pos_expr(p):
    'expr : PLUS expr %prec UPLUS'
    expr = p[2]
    p[0] = UnaryExpression(expr, p[1], expr.startLine, expr.endLine)

def p_minus_expr(p):
    'expr : MINUS expr %prec UMINUS'
    expr = p[2]
    p[0] = UnaryExpression(expr, p[1], expr.startLine, expr.endLine)

def p_not_expr(p):
    'expr : NOT expr'
    expr = p[2]
    p[0] = UnaryExpression(expr, p[1], expr.startLine, expr.endLine)

#def p_arith_op(p):
#    '''arith_op : PLUS
#                | MINUS
#                | STAR
#                | F_SLASH'''
#    pass

#def p_bool_op(p):
#    '''bool_op : AND
#               | OR
#               | EQ
#               | NOT_EQ
#               | LT
#               | GT
#               | LTE
#               | GTE'''
#    pass

#def p_unary_op(p):
#    '''unary_op : PLUS
#                | MINUS
#                | NOT'''
#    pass

def p_stmt_expr(p):
    '''stmt_expr : assign
                 | method_invocation'''
    p[0] = p[1]

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print()
    if p:
        print("Syntax error at token,", p.type, ", line", p.lineno)
    else:
        print("Syntax error at EOF")
    print()
    sys.exit()