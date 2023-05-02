class TypeRecord:
    def __init__(self, type):
        self.type = type

    def is_builtin(self) -> bool:
        return self.type == "INT" or self.type == "BOOLEAN" or self.whichType == "FLOAT" or self.whichType == "VOID" or self.whichType == "STRING" or self.whichType == "NULL" or self.whichType == "ERROR"

    def is_subtype(self, type_record) -> bool:
        if self.type == type_record.type:
            return True

        if self.type == "INT" and type_record.type == "FLOAT":
            return True

        if self.type == "NULL" and not type_record.is_builtin():
            return True

        if self.is_builtin():
            return self.type == type_record.type


class IfStatement:
    def __init__(self, expr, stmt, start_line, end_line):
        self.expr = expr
        self.stmt = stmt
        self.start_line = start_line
        self.end_line = end_line

    def type_check(self, containing_class, declaration) -> bool:
        return self.expr.get_type(containing_class).type == "BOOLEAN" and self.stmt.typeCheck(containing_class, declaration)


class WhileStatement:
    def __init__(self, expr, stmt, start_line, end_line):
        self.expr = expr
        self.stmt = stmt
        self.start_line = start_line
        self.end_line = end_line

    def type_check(self, containing_class, declaration) -> bool:
        return self.expr.get_type(containing_class).type == "BOOLEAN" and self.stmt.typeCheck(containing_class, declaration)


class ForStatement:
    def __init__(self, init, loop_cond, update, body, start_line, end_line):
        self.init = init
        self.loop_cond = loop_cond
        self.update = update
        self.body = body
        self.start_line = start_line
        self.end_line = end_line

    def type_check(self, containing_class, declaration) -> bool:
        return self.loop_cond.get_type(containing_class).type == "BOOLEAN" and self.init.typeCheck(containing_class, declaration).type != "ERROR" and self.update.typeCheck(containing_class, declaration).type != "ERROR" and self.body.typeCheck(containing_class, declaration).type


class ReturnStatement:
    def __init__(self, rtrn, start_line, end_line):
        self.rtrn = rtrn
        self.start_line = start_line
        self.end_line = end_line

    def type_check(self, containing_class, declaration) -> bool:
        if not self.rtrn:
            is_void = declaration.returnType.type == "VOID"
            return is_void
        else:
            is_subty = self.rtrn.get_type(
                containing_class).is_subtype(declaration.returnType)
            return is_subty


class ExprStatement:
    def __init__(self, expr, start_line, end_line):
        self.expr = expr
        self.start_line = start_line
        self.end_line = end_line

    def typeCheck(self, containing_class, declaration):
        return self.expr.get_type(containing_class).type != "ERROR"


class BlockStatement:
    pass


class ConstantIntegerExpression:
    def __init__(self, info, start_line, end_line):
        self.info = info
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("INT")

    def get_type(self, containing_class):
        return self.type


class ConstantFloatExpression:
    def __init__(self, info, start_line, end_line):
        self.info = info
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("FLOAT")

    def get_type(self, containing_class):
        return self.type


class ConstantStringExpression:
    def __init__(self, info, start_line, end_line):
        self.info = info
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("STRING")

    def get_type(self, containing_class):
        return self.type


class ConstantTrueExpression:
    def __init__(self, start_line, end_line):
        self.info = "true"
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("BOOLEAN")

    def get_type(self, containing_class):
        return self.type


class ConstantFalseExpression:
    def __init__(self, start_line, end_line):
        self.info = "false"
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("BOOLEAN")

    def get_type(self, containing_class):
        return self.type


class ConstantNullExpression:
    def __init__(self, start_line, end_line):
        self.info = "null"
        self.start_line = start_line
        self.end_line = end_line
        self.type = TypeRecord("NULL")

    def get_type(self, containing_class):
        return self.type


class VariableExpression:
    pass


class UnaryExpression:
    def __init__(self, operator, operand, start_line, end_line):
        self.operator = operator
        self.operand = operand
        self.start_line = start_line
        self.end_line = end_line
        self.type = None

    def get_type(self, containing_class):
        if not self.type:
            if self.operator == "!":
                if self.operand.get_type(containing_class).type == "BOOLEAN":
                    self.type = TypeRecord("BOOLEAN")
                else:
                    self.type = TypeRecord("ERROR")
            elif self.operator == "-" or self.operator == "+":
                if self.operand.get_type(containing_class).type == "INT":
                    self.type = TypeRecord("INT")
                elif self.operand.get_type(containing_class).type == "FLOAT":
                    self.type = TypeRecord("FLOAT")
                else:
                    self.type = TypeRecord("ERROR")
        return self.type


class BinaryExpression:
    def __init__(self, operator, operand1, operand2, start_line, end_line):
        self.operator = operator
        self.operand1 = operand1
        self.operand2 = operand2
        self.start_line = start_line
        self.end_line = end_line
        self.type = None

    def get_type(self, containing_class):
        if not self.type:
            if self.operator == "+" or self.operator == "-" or self.operator == "*" or self.operator == "/":
                if self.operand1.get_type(containing_class).type == "INT" and self.operand2.get_type(containing_class).type == "INT":
                    self.type = TypeRecord("INT")
                elif self.operand1.get_type(containing_class).type == "FLOAT" and self.operand2.get_type(containing_class).type == "FLOAT":
                    self.type = TypeRecord("FLOAT")
                else:
                    self.type = TypeRecord("ERROR")
            elif self.operator == "&&" or self.operator == "||":
                if self.operand1.get_type(containing_class).type == "BOOLEAN" and self.operand2.get_type(containing_class).type == "BOOLEAN":
                    self.type = TypeRecord("BOOLEAN")
                else:
                    self.type = TypeRecord("ERROR")
            elif self.operator == "<" or self.operator == ">" or self.operator == "<=" or self.operator == ">=":
                if self.operand1.get_type(containing_class).type == "INT" and self.operand2.get_type(containing_class).type == "INT":
                    self.type = TypeRecord("BOOLEAN")
                elif self.operand1.get_type(containing_class).type == "FLOAT" and self.operand2.get_type(containing_class).type == "FLOAT":
                    self.type = TypeRecord("BOOLEAN")
                else:
                    self.type = TypeRecord("ERROR")
            elif self.operator == "==" or self.operator == "=":
                if self.operand1.get_type(containing_class).is_subtype(self.operand2.get_type(containing_class)) or self.operand2.get_type(containing_class).is_subtype(self.operand1.get_type(containing_class)):
                    self.type = TypeRecord("BOOLEAN")
                else:
                    self.type = TypeRecord("ERROR")
        return self.type


class AssignExpression:
    def __init__(self, left, right, start_line, end_line):
        self.left = left
        self.right = right
        self.start_line = start_line
        self.end_line = end_line

    def get_type(self, containing_class):
        if not self.type:
            right_side = self.right.get_type(containing_class)
            left_side = self.left.get_type(containing_class)
            if right_side.is_subtype(left_side) and right_side.type != "ERROR" and left_side.type != "ERROR":
                self.type = right_side
            else:
                self.type = TypeRecord("ERROR")
        return self.type


class AutoExpression:
    def __init__(self, operand, inc_dec, post_pre, start_line, end_line):
        self.operand = operand
        self.inc_dec = inc_dec
        self.post_pre = post_pre
        self.start_line = start_line
        self.end_line = end_line

    def get_type(self, containing_class):
        if self.operand.get_type(containing_class).type == "INT":
            self.type = TypeRecord("INT")
        elif self.operand.get_type(containing_class).type == "FLOAT":
            self.type = TypeRecord("FLOAT")
        else:
            self.type = TypeRecord("ERROR")
        return self.type


class FieldAccessExpression:
    pass


class MethodCallExpression:
    pass


class NewObjectExpression:
    pass


class ThisExpression:
    def __init__(self, start_line, end_line):
        self.rep = "this"
        self.start_line = start_line
        self.end_line = end_line
        self.type = None

    def get_type(self, containing_class):
        self.type = TypeRecord(containing_class.name)
        return self.type


class SuperExpression:
    def __init__(self, start_line, end_line):
        self.rep = "super"
        self.start_line = start_line
        self.end_line = end_line

    def get_type(self, containing_class):
        if not containing_class.superName:
            return TypeRecord("ERROR")
        else:
            return TypeRecord(containing_class.superName)


class ClassReferenceExpression:
    def __init__(self, name, start_line, end_line):
        self.name = name
        self.start_line = start_line
        self.end_line = start_line
