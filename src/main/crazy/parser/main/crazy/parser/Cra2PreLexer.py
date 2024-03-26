# Generated from main/crazy/parser/Cra2Pre.g4 by ANTLR 4.9.2
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO


from lexererr import *



def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\21")
        buf.write("i\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16")
        buf.write("\t\16\4\17\t\17\4\20\t\20\3\2\3\2\3\2\3\2\3\2\3\2\3\2")
        buf.write("\3\2\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3")
        buf.write("\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7")
        buf.write("\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\n\3")
        buf.write("\n\3\13\3\13\3\f\3\f\3\r\6\rW\n\r\r\r\16\rX\3\16\6\16")
        buf.write("\\\n\16\r\16\16\16]\3\17\6\17a\n\17\r\17\16\17b\3\17\3")
        buf.write("\17\3\20\3\20\3\20\2\2\21\3\3\5\4\7\5\t\6\13\7\r\b\17")
        buf.write("\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21\3\2\5")
        buf.write("\3\2\62;\3\2c|\5\2\13\f\17\17\"\"\2k\2\3\3\2\2\2\2\5\3")
        buf.write("\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2")
        buf.write("\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2")
        buf.write("\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2")
        buf.write("\37\3\2\2\2\3!\3\2\2\2\5)\3\2\2\2\7-\3\2\2\2\t\63\3\2")
        buf.write("\2\2\13\67\3\2\2\2\r?\3\2\2\2\17G\3\2\2\2\21L\3\2\2\2")
        buf.write("\23O\3\2\2\2\25Q\3\2\2\2\27S\3\2\2\2\31V\3\2\2\2\33[\3")
        buf.write("\2\2\2\35`\3\2\2\2\37f\3\2\2\2!\"\7r\2\2\"#\7t\2\2#$\7")
        buf.write("q\2\2$%\7i\2\2%&\7t\2\2&\'\7c\2\2\'(\7o\2\2(\4\3\2\2\2")
        buf.write(")*\7x\2\2*+\7c\2\2+,\7t\2\2,\6\3\2\2\2-.\7d\2\2./\7g\2")
        buf.write("\2/\60\7i\2\2\60\61\7k\2\2\61\62\7p\2\2\62\b\3\2\2\2\63")
        buf.write("\64\7g\2\2\64\65\7p\2\2\65\66\7f\2\2\66\n\3\2\2\2\678")
        buf.write("\7d\2\289\7q\2\29:\7q\2\2:;\7n\2\2;<\7g\2\2<=\7c\2\2=")
        buf.write(">\7p\2\2>\f\3\2\2\2?@\7k\2\2@A\7p\2\2AB\7v\2\2BC\7g\2")
        buf.write("\2CD\7i\2\2DE\7g\2\2EF\7t\2\2F\16\3\2\2\2GH\7t\2\2HI\7")
        buf.write("g\2\2IJ\7c\2\2JK\7n\2\2K\20\3\2\2\2LM\7<\2\2MN\7?\2\2")
        buf.write("N\22\3\2\2\2OP\7\60\2\2P\24\3\2\2\2QR\7=\2\2R\26\3\2\2")
        buf.write("\2ST\7<\2\2T\30\3\2\2\2UW\t\2\2\2VU\3\2\2\2WX\3\2\2\2")
        buf.write("XV\3\2\2\2XY\3\2\2\2Y\32\3\2\2\2Z\\\t\3\2\2[Z\3\2\2\2")
        buf.write("\\]\3\2\2\2][\3\2\2\2]^\3\2\2\2^\34\3\2\2\2_a\t\4\2\2")
        buf.write("`_\3\2\2\2ab\3\2\2\2b`\3\2\2\2bc\3\2\2\2cd\3\2\2\2de\b")
        buf.write("\17\2\2e\36\3\2\2\2fg\13\2\2\2gh\b\20\3\2h \3\2\2\2\6")
        buf.write("\2X]b\4\b\2\2\3\20\2")
        return buf.getvalue()


class Cra2PreLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    PROGRAM = 1
    VAR = 2
    BEGIN = 3
    END = 4
    BOOLEAN = 5
    INTEGER = 6
    REAL = 7
    ASSOPE = 8
    DOT = 9
    SEMICOLON = 10
    COLON = 11
    INTLIT = 12
    ID = 13
    WS = 14
    ERROR = 15

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'program'", "'var'", "'begin'", "'end'", "'boolean'", "'integer'", 
            "'real'", "':='", "'.'", "';'", "':'" ]

    symbolicNames = [ "<INVALID>",
            "PROGRAM", "VAR", "BEGIN", "END", "BOOLEAN", "INTEGER", "REAL", 
            "ASSOPE", "DOT", "SEMICOLON", "COLON", "INTLIT", "ID", "WS", 
            "ERROR" ]

    ruleNames = [ "PROGRAM", "VAR", "BEGIN", "END", "BOOLEAN", "INTEGER", 
                  "REAL", "ASSOPE", "DOT", "SEMICOLON", "COLON", "INTLIT", 
                  "ID", "WS", "ERROR" ]

    grammarFileName = "Cra2Pre.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9.2")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None


    def action(self, localctx:RuleContext, ruleIndex:int, actionIndex:int):
        if self._actions is None:
            actions = dict()
            actions[14] = self.ERROR_action 
            self._actions = actions
        action = self._actions.get(ruleIndex, None)
        if action is not None:
            action(localctx, actionIndex)
        else:
            raise Exception("No registered action for:" + str(ruleIndex))


    def ERROR_action(self, localctx:RuleContext , actionIndex:int):
        if actionIndex == 0:
            raise ErrorToken(self.text)
     


