# Generated from main/crazy/parser/Cra2Pre.g4 by ANTLR 4.9.2
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .Cra2PreParser import Cra2PreParser
else:
    from Cra2PreParser import Cra2PreParser

# This class defines a complete generic visitor for a parse tree produced by Cra2PreParser.

class Cra2PreVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by Cra2PreParser#program.
    def visitProgram(self, ctx:Cra2PreParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#var_decl.
    def visitVar_decl(self, ctx:Cra2PreParser.Var_declContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#ctype.
    def visitCtype(self, ctx:Cra2PreParser.CtypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#comp_stmt.
    def visitComp_stmt(self, ctx:Cra2PreParser.Comp_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#stmt.
    def visitStmt(self, ctx:Cra2PreParser.StmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#assi_stmt.
    def visitAssi_stmt(self, ctx:Cra2PreParser.Assi_stmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cra2PreParser#expr.
    def visitExpr(self, ctx:Cra2PreParser.ExprContext):
        return self.visitChildren(ctx)



del Cra2PreParser