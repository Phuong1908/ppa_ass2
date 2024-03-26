# Generated from main/crazy/parser/Cra2Pre.g4 by ANTLR 4.9.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO


def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\21")
        buf.write("G\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b")
        buf.write("\t\b\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3")
        buf.write("\3\3\3\3\3\3\3\3\3\5\3\"\n\3\3\4\3\4\3\4\3\4\3\4\3\4\5")
        buf.write("\4*\n\4\3\5\3\5\3\5\3\5\3\5\5\5\61\n\5\3\5\3\5\3\5\3\6")
        buf.write("\3\6\3\6\3\6\3\6\3\6\5\6<\n\6\3\7\3\7\3\7\3\7\3\7\3\7")
        buf.write("\3\b\3\b\3\b\3\b\2\2\t\2\4\6\b\n\f\16\2\2\2D\2\20\3\2")
        buf.write("\2\2\4!\3\2\2\2\6)\3\2\2\2\b+\3\2\2\2\n;\3\2\2\2\f=\3")
        buf.write("\2\2\2\16C\3\2\2\2\20\21\7\3\2\2\21\22\7\17\2\2\22\23")
        buf.write("\7\f\2\2\23\24\5\4\3\2\24\25\5\b\5\2\25\26\7\13\2\2\26")
        buf.write("\27\7\2\2\3\27\30\b\2\1\2\30\3\3\2\2\2\31\32\7\4\2\2\32")
        buf.write("\33\7\17\2\2\33\34\7\r\2\2\34\35\5\6\4\2\35\36\7\f\2\2")
        buf.write("\36\37\b\3\1\2\37\"\3\2\2\2 \"\b\3\1\2!\31\3\2\2\2! \3")
        buf.write("\2\2\2\"\5\3\2\2\2#$\7\b\2\2$*\b\4\1\2%&\7\t\2\2&*\b\4")
        buf.write("\1\2\'(\7\7\2\2(*\b\4\1\2)#\3\2\2\2)%\3\2\2\2)\'\3\2\2")
        buf.write("\2*\7\3\2\2\2+,\7\5\2\2,\60\b\5\1\2-.\5\n\6\2./\b\5\1")
        buf.write("\2/\61\3\2\2\2\60-\3\2\2\2\60\61\3\2\2\2\61\62\3\2\2\2")
        buf.write("\62\63\7\6\2\2\63\64\b\5\1\2\64\t\3\2\2\2\65\66\5\b\5")
        buf.write("\2\66\67\b\6\1\2\67<\3\2\2\289\5\f\7\29:\b\6\1\2:<\3\2")
        buf.write("\2\2;\65\3\2\2\2;8\3\2\2\2<\13\3\2\2\2=>\7\17\2\2>?\7")
        buf.write("\n\2\2?@\5\16\b\2@A\7\f\2\2AB\b\7\1\2B\r\3\2\2\2CD\7\16")
        buf.write("\2\2DE\b\b\1\2E\17\3\2\2\2\6!)\60;")
        return buf.getvalue()


class Cra2PreParser ( Parser ):

    grammarFileName = "Cra2Pre.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'program'", "'var'", "'begin'", "'end'", 
                     "'boolean'", "'integer'", "'real'", "':='", "'.'", 
                     "';'", "':'" ]

    symbolicNames = [ "<INVALID>", "PROGRAM", "VAR", "BEGIN", "END", "BOOLEAN", 
                      "INTEGER", "REAL", "ASSOPE", "DOT", "SEMICOLON", "COLON", 
                      "INTLIT", "ID", "WS", "ERROR" ]

    RULE_program = 0
    RULE_var_decl = 1
    RULE_ctype = 2
    RULE_comp_stmt = 3
    RULE_stmt = 4
    RULE_assi_stmt = 5
    RULE_expr = 6

    ruleNames =  [ "program", "var_decl", "ctype", "comp_stmt", "stmt", 
                   "assi_stmt", "expr" ]

    EOF = Token.EOF
    PROGRAM=1
    VAR=2
    BEGIN=3
    END=4
    BOOLEAN=5
    INTEGER=6
    REAL=7
    ASSOPE=8
    DOT=9
    SEMICOLON=10
    COLON=11
    INTLIT=12
    ID=13
    WS=14
    ERROR=15

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class ProgramContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self.v = None # Var_declContext
            self.c = None # Comp_stmtContext

        def PROGRAM(self):
            return self.getToken(Cra2PreParser.PROGRAM, 0)

        def ID(self):
            return self.getToken(Cra2PreParser.ID, 0)

        def SEMICOLON(self):
            return self.getToken(Cra2PreParser.SEMICOLON, 0)

        def DOT(self):
            return self.getToken(Cra2PreParser.DOT, 0)

        def EOF(self):
            return self.getToken(Cra2PreParser.EOF, 0)

        def var_decl(self):
            return self.getTypedRuleContext(Cra2PreParser.Var_declContext,0)


        def comp_stmt(self):
            return self.getTypedRuleContext(Cra2PreParser.Comp_stmtContext,0)


        def getRuleIndex(self):
            return Cra2PreParser.RULE_program

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitProgram" ):
                return visitor.visitProgram(self)
            else:
                return visitor.visitChildren(self)




    def program(self):

        localctx = Cra2PreParser.ProgramContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 14
            self.match(Cra2PreParser.PROGRAM)
            self.state = 15
            self.match(Cra2PreParser.ID)
            self.state = 16
            self.match(Cra2PreParser.SEMICOLON)
            self.state = 17
            localctx.v = self.var_decl()
            self.state = 18
            localctx.c = self.comp_stmt()
            self.state = 19
            self.match(Cra2PreParser.DOT)
            self.state = 20
            self.match(Cra2PreParser.EOF)
            localctx.s = "[[" + localctx.v.s + "],[]," + localctx.c.s + "]. " 
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Var_declContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self._ID = None # Token
            self.t = None # CtypeContext

        def VAR(self):
            return self.getToken(Cra2PreParser.VAR, 0)

        def ID(self):
            return self.getToken(Cra2PreParser.ID, 0)

        def COLON(self):
            return self.getToken(Cra2PreParser.COLON, 0)

        def SEMICOLON(self):
            return self.getToken(Cra2PreParser.SEMICOLON, 0)

        def ctype(self):
            return self.getTypedRuleContext(Cra2PreParser.CtypeContext,0)


        def getRuleIndex(self):
            return Cra2PreParser.RULE_var_decl

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitVar_decl" ):
                return visitor.visitVar_decl(self)
            else:
                return visitor.visitChildren(self)




    def var_decl(self):

        localctx = Cra2PreParser.Var_declContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_var_decl)
        try:
            self.state = 31
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [Cra2PreParser.VAR]:
                self.enterOuterAlt(localctx, 1)
                self.state = 23
                self.match(Cra2PreParser.VAR)
                self.state = 24
                localctx._ID = self.match(Cra2PreParser.ID)
                self.state = 25
                self.match(Cra2PreParser.COLON)
                self.state = 26
                localctx.t = self.ctype()
                self.state = 27
                self.match(Cra2PreParser.SEMICOLON)
                localctx.s = 'var('+ (None if localctx._ID is None else localctx._ID.text)+ ',' + localctx.t.s +')'
                pass
            elif token in [Cra2PreParser.BEGIN]:
                self.enterOuterAlt(localctx, 2)
                localctx.s = ""
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class CtypeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None

        def INTEGER(self):
            return self.getToken(Cra2PreParser.INTEGER, 0)

        def REAL(self):
            return self.getToken(Cra2PreParser.REAL, 0)

        def BOOLEAN(self):
            return self.getToken(Cra2PreParser.BOOLEAN, 0)

        def getRuleIndex(self):
            return Cra2PreParser.RULE_ctype

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitCtype" ):
                return visitor.visitCtype(self)
            else:
                return visitor.visitChildren(self)




    def ctype(self):

        localctx = Cra2PreParser.CtypeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_ctype)
        try:
            self.state = 39
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [Cra2PreParser.INTEGER]:
                self.enterOuterAlt(localctx, 1)
                self.state = 33
                self.match(Cra2PreParser.INTEGER)
                localctx.s = "integer" 
                pass
            elif token in [Cra2PreParser.REAL]:
                self.enterOuterAlt(localctx, 2)
                self.state = 35
                self.match(Cra2PreParser.REAL)
                localctx.s = "real" 
                pass
            elif token in [Cra2PreParser.BOOLEAN]:
                self.enterOuterAlt(localctx, 3)
                self.state = 37
                self.match(Cra2PreParser.BOOLEAN)
                localctx.s = "boolean" 
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Comp_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self.v = None # StmtContext

        def BEGIN(self):
            return self.getToken(Cra2PreParser.BEGIN, 0)

        def END(self):
            return self.getToken(Cra2PreParser.END, 0)

        def stmt(self):
            return self.getTypedRuleContext(Cra2PreParser.StmtContext,0)


        def getRuleIndex(self):
            return Cra2PreParser.RULE_comp_stmt

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitComp_stmt" ):
                return visitor.visitComp_stmt(self)
            else:
                return visitor.visitChildren(self)




    def comp_stmt(self):

        localctx = Cra2PreParser.Comp_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_comp_stmt)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 41
            self.match(Cra2PreParser.BEGIN)
            t='['
            self.state = 46
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==Cra2PreParser.BEGIN or _la==Cra2PreParser.ID:
                self.state = 43
                localctx.v = self.stmt()
                t += localctx.v.s


            self.state = 48
            self.match(Cra2PreParser.END)
            localctx.s = t + "]"
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class StmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self.t = None # Comp_stmtContext
            self.t1 = None # Assi_stmtContext

        def comp_stmt(self):
            return self.getTypedRuleContext(Cra2PreParser.Comp_stmtContext,0)


        def assi_stmt(self):
            return self.getTypedRuleContext(Cra2PreParser.Assi_stmtContext,0)


        def getRuleIndex(self):
            return Cra2PreParser.RULE_stmt

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitStmt" ):
                return visitor.visitStmt(self)
            else:
                return visitor.visitChildren(self)




    def stmt(self):

        localctx = Cra2PreParser.StmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_stmt)
        try:
            self.state = 57
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [Cra2PreParser.BEGIN]:
                self.enterOuterAlt(localctx, 1)
                self.state = 51
                localctx.t = self.comp_stmt()
                localctx.s = localctx.t.s
                pass
            elif token in [Cra2PreParser.ID]:
                self.enterOuterAlt(localctx, 2)
                self.state = 54
                localctx.t1 = self.assi_stmt()
                localctx.s = localctx.t1.s
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Assi_stmtContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self._ID = None # Token
            self.e = None # ExprContext

        def ID(self):
            return self.getToken(Cra2PreParser.ID, 0)

        def ASSOPE(self):
            return self.getToken(Cra2PreParser.ASSOPE, 0)

        def SEMICOLON(self):
            return self.getToken(Cra2PreParser.SEMICOLON, 0)

        def expr(self):
            return self.getTypedRuleContext(Cra2PreParser.ExprContext,0)


        def getRuleIndex(self):
            return Cra2PreParser.RULE_assi_stmt

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAssi_stmt" ):
                return visitor.visitAssi_stmt(self)
            else:
                return visitor.visitChildren(self)




    def assi_stmt(self):

        localctx = Cra2PreParser.Assi_stmtContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_assi_stmt)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 59
            localctx._ID = self.match(Cra2PreParser.ID)
            self.state = 60
            self.match(Cra2PreParser.ASSOPE)
            self.state = 61
            localctx.e = self.expr()
            self.state = 62
            self.match(Cra2PreParser.SEMICOLON)
            localctx.s = "assign("+ (None if localctx._ID is None else localctx._ID.text) + "," + localctx.e.s +")"
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.s = None
            self._INTLIT = None # Token

        def INTLIT(self):
            return self.getToken(Cra2PreParser.INTLIT, 0)

        def getRuleIndex(self):
            return Cra2PreParser.RULE_expr

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExpr" ):
                return visitor.visitExpr(self)
            else:
                return visitor.visitChildren(self)




    def expr(self):

        localctx = Cra2PreParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_expr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 65
            localctx._INTLIT = self.match(Cra2PreParser.INTLIT)
            localctx.s = (None if localctx._INTLIT is None else localctx._INTLIT.text)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





