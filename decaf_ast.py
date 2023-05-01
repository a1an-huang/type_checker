class ClassRecord:
        def __init__(self, name, superName, listofDeclarations):
            self.name = name
            self.superName = superName
            self.listofDeclarations = listofDeclarations
            self.constructorRecords = []
            self.methodRecords = []
            self.fieldRecords = []

            ConstructorRecord.conscounter = 1
            MethodRecord.methcounter = 1
            FieldRecord.fieldcounter = 1

            for record in listofDeclarations:
                if (isinstance(record, ConstructorRecord)):
                    record.cotclass = name
                    self.constructorRecords.append(record)
                    numofvars = len(record.block.vartable)
                    VariableRecord.varcount = numofvars + 1
                    for param in record.params:
                        var = VariableRecord(param.name, "formal", param.type)
                        if param.name in record.block.vartable:
                              print("Error! Conflicting names!")
                        record.block.vartable[param.name] = var 
                        for s in record.block.stmts:
                              ContainingBlockStatementVarID(s, record.block)
                
                if (isinstance(record, MethodRecord)):
                    record.cotclass = name
                    self.methodRecords.append(record)
                    numofvars = len(record.block.vartable)
                    VariableRecord.varcount = numofvars + 1
                    for param in record.params:
                        var = VariableRecord(param.name, "formal", param.type)
                        if param.name in record.block.vartable:
                              print("Error! Conflicting names!")
                        record.block.vartable[param.name] = var 
                        for s in record.block.stmts:
                              ContainingBlockStatementVarID(s, record.block)
                
                if (isinstance(record, FieldRecord)):
                    record.cotclass = name
                    self.fieldRecords.append(record)

        def printClass(self):
            print("--------------------------------------------------------------------------")
            print("Class Name: " + self.name)
            print("Superclass Name: " + self.superName)
            print("Fields: ")
            for field in self.fieldRecords:
                field.printField()
            print("Constructors: ")
            for constructor in self.constructorRecords:
                constructor.printConstructor()
            print("Methods: ")
            for method in self.methodRecords:
                method.printMethod()

class ConstructorRecord:
        
        conscounter = 1

        def __init__(self, name, vis, params, block):
            self.id = ConstructorRecord.conscounter
            self.vis = vis
            self.name = name
            self.params = params
            self.block = block        
            ConstructorRecord.conscounter += 1

        def printConstructor(self):
            varIDs = []
            s = "Constructor: "
            s += str(self.id) + ","
            s += self.vis
            print(s)
            keys = self.block.vartable.keys()
            print("Constructor Parameters: ")
            for key in keys:
                var = self.block.vartable[key]
                if(var.kind == "formal"):
                    varIDs.append(var.id)
            
            ids = ""
            if varIDs:
                for id in range(0, len(varIDs) - 1):
                    ids += str(varIDs[id]) + ", "
                ids += str(varIDs[len(varIDs) - 1])

            print(ids)
            print("Variable Table: ")
            for key in keys:
                print(self.block.vartable[key].toString())

            print("Constructor Body:")
            print(self.block.toString())

class MethodRecord:
        
        methcounter = 1

        def __init__(self, name, vis, app, params, retype, block):
            self.id = MethodRecord.methcounter
            self.name = name
            self.vis = vis
            self.app = app
            self.params = params
            self.retype = retype
            self.block = block
            MethodRecord.methcounter += 1

        def printMethod(self):
            s = str(self.id) + ", "
            s += self.name + ", "
            s += self.cotclass + ", "
            s += self.vis + ", "
            s += self.app + ", "
            s += self.retype.toString()
            print(f"METHOD: {s}")
            varIDs = []
            keys = self.block.vartable.keys()
            for key in keys:
                var = self.block.vartable[key]
                if(var.kind == "formal"):
                    varIDs.append(var.id)
            
            ids = ""
            if varIDs:
                for id in range(0, len(varIDs) - 1):
                    ids += str(varIDs[id]) + ", "
            
                ids += str(varIDs[len(varIDs) - 1])

            print(f"Method parameters: {ids}")

            print("Variable Table: ")
            for key in keys:
                print(self.block.vartable[key].toString())
            
            print("Method Body:")
            print(self.block.toString())

class FieldRecord:
        
        fieldcounter = 1

        def __init__(self, name, vis, app, type):
              self.id = FieldRecord.fieldcounter
              self.name = name
              self.vis = vis
              self.app = app
              self.type = type
              FieldRecord.fieldcounter += 1

        def printField(self):
            print("FIELD: ")
            s = str(self.id) + ", "
            s += self.name + ", "
            s += self.cotclass + ", "
            s += self.vis + ", "
            s += self.app + ", "
            s += self.type.toString()
            print(s)

class VariableRecord:
        
        varcount = 1

        def __init__(self, name, kind, type):
              self.id = VariableRecord.varcount
              self.name = name
              self.kind = kind
              self.type = type
              VariableRecord.varcount += 1

        def toString(self):
            s = "VARIABLE: "
            s += str(self.id) + ", "
            s += self.name + ", "
            s += self.kind + ", "
            s += self.type.toString()
            return s

class VarTypeInfo:
      
      def __init__(self, type, vars):
            self.type = type
            self.vars = vars

class ModInfo:
      def __init__(self, vis, app):
            self.vis = vis
            self.app = app

class TypeRecord:

        def __init__(self, type):
            self.type = type
        
        def toString(self):
            if (self.type == "INT" or self.type == "FLOAT" or self.type == "BOOLEAN" or self.type == "STRING" or self.type == "VOID"):
                  return self.type
            return "user(" + self.type + ")"
        

class Param:
      def __init__(self, name, type, startLine, endLine):
            self.name = name
            self.type = type
            self.startLine = startLine
            self.endLine = endLine
        

class IfStatement:
      def __init__(self, cond, thenstmt, startLine, endLine):
            self.cond = cond
            self.thenstmt = thenstmt
            self.startLine = startLine
            self.endLine = endLine

      def toString(self):
            s = "If(" + self.cond.toString()
            s += ", " + self.thenstmt.toString()
            s += ")"
            return s       

class IfElseStatement:
        def __init__(self, cond, thenstmt, elsestmt, startLine, endLine):
            self.cond = cond
            self.thenstmt = thenstmt
            self.elsestmt = elsestmt
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "IfElse(" + self.cond.toString()
              s += ", " + self.thenstmt.toString()
              s += ", " + self.elsestmt.toString()
              s += ")"
              return s            

class WhileStatement:
        def __init__(self, expr, stmt, startLine, endLine):
            self.expr = expr
            self.stmt = stmt
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "While(" + self.expr.toString()
              s += ", " + self.stmt.toString()
              s += ")"
              return s

class ForStatement:
        def __init__(self, init, loopCond, update, body, startLine, endLine):
            self.init = init
            self.loopCond = loopCond
            self.update = update
            self.body = body
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "For(" + self.init.toString()
              s += ", " + self.loopCond.toString()
              s += ", " + self.update.toString()
              s += ", " + self.body.toString()
              s += ")"
              return s

class ReturnStatement:
        def __init__(self, retVal, startLine, endLine):
            self.retVal = retVal
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "Return(" + self.retVal.toString()
              s += ")"
              return s

class ExpressionStatement:
        def __init__(self, expr, startLine, endLine):
            self.expr = expr
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "Expr(" + self.expr.toString()
              s += ")"
              return s

class BlockStatement:
        def __init__(self, stmts, startLine, endLine):
            self.stmts = stmts
            self.startLine = startLine
            self.endLine = endLine
            self.cotblock = None

            VariableRecord.varcount = 1

            vartable = {}

            for s in stmts: 
                  ContainingBlockStatement(s, self)
                  if isinstance(s, VariableStatement):
                        for var in s.varDecl.vars:
                              newVar = VariableRecord(var, "local", s.varDecl.type)
                              if newVar in vartable:
                                    print("Error; this variable has already been defined in this block!\n")
                              vartable[var] = newVar

            self.vartable = vartable

        def toString(self):
              listofstms = self.stmts
              s = "Block([ \n"
              if (len(listofstms) > 0):
                for stmt in range(0, len(listofstms) - 1):
                      if not isinstance(listofstms[stmt], VariableStatement):
                        s += " " + listofstms[stmt].toString() + "\n ,"
                if not isinstance(listofstms[len(listofstms) - 1], VariableStatement):
                    s += " " + listofstms[len(listofstms) - 1].toString() + "\n"
              s += "])"
              return s

class BreakStatement:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
            s = "Break("
            s += ")"
            return s

class ContinueStatement:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "Continue("
              s += ")"
              return s

class SkipStatement:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine

        def toString(self):
              s = "Skip("
              s += ")"
              return s

class VariableStatement:
      def __init__(self, varDecl, startLine, endLine):
            self.varDecl = varDecl
            self.startLine = startLine
            self.endLine = endLine

class IntegerConstant:
        def __init__(self, integer, startLine, endLine):
            self.integer = integer
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantInteger("
                s += str(self.integer)
                s += ")"
                return s
        
class FloatConstant:
        def __init__(self, float, startLine, endLine):
            self.float = float
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantFloat("
                s += str(self.float)
                s += ")"
                return s

class StringConstant:
        def __init__(self, string, startLine, endLine):
            self.string = string
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantString("
                s += self.string
                s += ")"
                return s

class NullConstant:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantNull("
                s += "null"
                s += ")"
                return s

class TrueConstant:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantTrue("
                s += "true"
                s += ")"
                return s
        
class FalseConstant:
        def __init__(self, startLine, endLine):
            self.startLine = startLine
            self.endLine = endLine
        
        def toString(self):
                s = "ConstantFalse("
                s += "false"
                s += ")"
                return s
        
class VariableExpression:
      def __init__(self, name, startLine, endLine):
        self.name = name
        self.startLine = startLine
        self.endLine = endLine

        self.id = 0
            
      def toString(self):
            s = "Variable("
            if self.id == 0:
                  self.idLookUp()
            s += str(self.id)
            s += ")"
            return s
      
      def idLookUp(self):
            cotblock = self.cotblock
            while True:
                if (self.name in cotblock.vartable):
                    self.id = (cotblock.vartable[self.name]).id
                    return
                cotblock = cotblock.cotblock
                if cotblock is None:
                    self.id = -1
                    return
                         
class UnaryExpression:
	def __init__(self, operand, operator, startLine, endLine):
		self.operand = operand
		self.operator = operator
		self.startLine = startLine
		self.endLine = endLine
		
	def toString(self):
		s = "UnaryExpression("
		s += self.operator + ","
		s += self.operand.toString()
		s += ")"
		return s
		
class BinaryExpression:
	def __init__(self, operator, op1, op2, startLine, endLine):
		self.operator = operator
		self.op1 = op1
		self.op2 = op2
		self.startLine = startLine
		self.endLine = endLine
		
	def toString(self):
		s = "BinaryExpression("
		s += self.operator + ", "
		s += self.op1.toString() + ", "
		s += self.op2.toString()
		s += ")"
		return s
		
class AssignExpression:
	def __init__(self, lhs, rhs, startLine, endLine):
		self.lhs = lhs
		self.rhs = rhs
		self.startLine = startLine
		self.endLine = endLine
				
	def toString(self):
		s = "Assign("
		s += self.lhs.toString() + ", "
		s += self.rhs.toString()
		s += ")"
		return s
		
class AutoExpression:
	def __init__(self, operand, incrDecr, prePost, startLine, endLine):
		self.operand = operand
		self.incrDecr = incrDecr
		self.prePost = prePost
		self.startLine = startLine
		self.endLine = endLine
				
	def toString(self):
		s = "AutoExpression("
		s += self.operand.toString() + ", "
		s += self.incrDecr + ", "
		s += self.prePost
		s += " "
		return s
		
class FieldAccessExpression:
	def __init__(self, base, name, startLine, endLine):
		self.base = base
		self.name = name
		self.startLine = startLine
		self.endLine = endLine
			
	def toString(self):
		s = "FieldAccess("
		s += self.base.toString() + ", "
		s += self.name
		s += ")"
		return s
	
class MethodCallExpression:
	def __init__(self, base, name, args, startLine, endLine):
		self.base = base
		self.name = name
		self.args = args
		self.startLine = startLine
		self.endLine = endLine
			
	def toString(self):
		s = "MethodCall("
		s += self.base.toString() + ", "
		s += self.name + ", "
		s += "Arguments([ "
		if(isinstance(self.args, list) and len(self.args)>0):
			for i in range(0, len(self.args)-1):
				s += self.args[i].toString()+", "
			s += self.args[len(self.args) - 1].toString()			
		s += " ]"		
		s += " )"
		return s
	
class NewObjectExpression:
	def __init__(self, base, args, startLine, endLine):
		self.base = base
		self.args = args
		self.startLine = startLine
		self.endLine = endLine
                
	def toString(self):
              s = "NewObject( "
              if (self.base is not None):
                    if not isinstance(self.base, str):
                          s += self.base.toString() + ", "
                    else:
                          s += self.base + ", "
              s += "Arguments([ "
              if (len(self.args) > 0):
                    s += "\n"
                    for ind in range(0, len(self.args) - 1):
                          s += self.args[ind].toString() + ", \n"
                    s += self.args[len(self.args) - 1].toString() + " \n"
              s += "])"
              s += " )"
              return s
        
class ThisExpression:
	def __init__(self, startLine, endLine):
		self.startLine = startLine
		self.endLine = endLine
			
	def toString(self):
		return "This"
		
class SuperExpression:
	def __init__(self, startLine, endLine):
		self.startLine = startLine
		self.endLine = endLine
			
	def toString(self):
		return "Super"
		
class ClassReferenceExpression:
	def __init__(self, classname, startLine, endLine):
		self.classname = classname
		self.startLine = startLine
		self.endLine = endLine
					
	def toString(self):
		s = "ClassReference("
		s += self.classname
		s += ")"
		return s

def ContainingBlockStatement(stmt, block):
      if stmt is None:
            return
      else:
            stmt.cotblock = block
            if isinstance(stmt, IfStatement):
                  ContainingBlockExpression(stmt.cond, block)
                  ContainingBlockStatement(stmt.thenstmt, block)
            if isinstance(stmt, IfElseStatement):
                  ContainingBlockExpression(stmt.cond, block)
                  ContainingBlockStatement(stmt.thenstmt, block)
                  ContainingBlockStatement(stmt.elsestmt, block)
            if isinstance(stmt, WhileStatement):
                  ContainingBlockExpression(stmt.expr, block)
                  ContainingBlockStatement(stmt.stmt, block)
            if isinstance(stmt, ForStatement):
                  ContainingBlockExpression(stmt.init, block)
                  ContainingBlockExpression(stmt.loopCond, block)
                  ContainingBlockExpression(stmt.update, block)
                  ContainingBlockStatement(stmt.body, block)
            if isinstance(stmt, ExpressionStatement):
                  ContainingBlockExpression(stmt.expr, block)
            if isinstance(stmt, ReturnStatement):
                  ContainingBlockExpression(stmt.retVal, block)

def ContainingBlockExpression(expr, block):
      if expr is None:
            return
      else:
            expr.cotblock = block
            if isinstance(expr, UnaryExpression):
                  ContainingBlockExpression(expr.operand, block)
            if isinstance(expr, BinaryExpression):
                  ContainingBlockExpression(expr.op1, block)
                  ContainingBlockExpression(expr.op2, block)
            if isinstance(expr, AssignExpression):
                  ContainingBlockExpression(expr.lhs, block)
                  ContainingBlockExpression(expr.rhs, block)
            if isinstance(expr, AutoExpression):
                  ContainingBlockExpression(expr.operand, block)
            if isinstance(expr, FieldAccessExpression):
                  ContainingBlockExpression(expr.base, block)
            if isinstance(expr, MethodCallExpression):
                  ContainingBlockExpression(expr.base, block)
                  for arg in expr.args:
                        ContainingBlockExpression(arg, block)
            if isinstance(expr, NewObjectExpression):
                  for arg in expr.args:
                    ContainingBlockExpression(arg, block)

def ContainingBlockStatementVarID(stmt, block):
      if stmt is None:
            return
      else:
            if isinstance(stmt, IfStatement):
                  ContainingBlockExpressionVarID(stmt.cond, block)
                  ContainingBlockStatementVarID(stmt.thenstmt, block)
            if isinstance(stmt, IfElseStatement):
                  ContainingBlockExpressionVarID(stmt.cond, block)
                  ContainingBlockStatementVarID(stmt.thenstmt, block)
                  ContainingBlockStatementVarID(stmt.elsestmt, block)
            if isinstance(stmt, WhileStatement):
                  ContainingBlockExpressionVarID(stmt.expr, block)
                  ContainingBlockStatementVarID(stmt.stmt, block)
            if isinstance(stmt, ForStatement):
                  ContainingBlockExpressionVarID(stmt.init, block)
                  ContainingBlockExpressionVarID(stmt.loopCond, block)
                  ContainingBlockExpressionVarID(stmt.update, block)
                  ContainingBlockStatementVarID(stmt.body, block)
            if isinstance(stmt, ExpressionStatement):
                  ContainingBlockExpressionVarID(stmt.expr, block)
            if isinstance(stmt, ReturnStatement):
                  ContainingBlockExpressionVarID(stmt.retVal, block)
            if isinstance(stmt, BlockStatement):
                  for b in stmt.stmts:
                        ContainingBlockStatementVarID(b, block)

def ContainingBlockExpressionVarID(expr, block):
      if expr is None:
            return
      else:
            if isinstance(expr, UnaryExpression):
                  ContainingBlockExpressionVarID(expr.operand, block)
            if isinstance(expr, BinaryExpression):
                  ContainingBlockExpressionVarID(expr.op1, block)
                  ContainingBlockExpressionVarID(expr.op2, block)
            if isinstance(expr, AssignExpression):
                  ContainingBlockExpressionVarID(expr.lhs, block)
                  ContainingBlockExpressionVarID(expr.rhs, block)
            if isinstance(expr, AutoExpression):
                  ContainingBlockExpressionVarID(expr.operand, block)
            if isinstance(expr, FieldAccessExpression):
                  ContainingBlockExpressionVarID(expr.base, block)
            if isinstance(expr, MethodCallExpression):
                  ContainingBlockExpressionVarID(expr.base, block)
                  for arg in expr.args:
                        ContainingBlockExpressionVarID(arg, block)
            if isinstance(expr, NewObjectExpression):
                  for arg in expr.args:
                    ContainingBlockExpressionVarID(arg, block)
            if isinstance(expr, VariableExpression):
                  expr.idLookUp()
