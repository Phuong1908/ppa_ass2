import sys
import os
import traceback
from antlr4 import *
from antlr4.error.ErrorListener import ConsoleErrorListener, ErrorListener

from swiplserver import PrologMQI, PrologThread, create_posix_path



ERR_PATH = './main/crazy/parser/'
GEN_PATH = '../target/main/crazy/parser'
if not ERR_PATH in sys.path:
    sys.path.append(ERR_PATH)
if os.path.isdir(GEN_PATH) and not GEN_PATH in sys.path:
    sys.path.append(GEN_PATH)

from Cra2PreLexer import Cra2PreLexer
from Cra2PreParser import Cra2PreParser
from lexererr import *
import subprocess

JASMIN_JAR = "./external/jasmin.jar"
TEST_DIR = "./test/testcases/"
SOL_DIR = "./test/solutions/"
Lexer = Cra2PreLexer
Parser = Cra2PreParser


class TestUtil:
    @staticmethod
    def makeSource(inputStr, num):
        filename = TEST_DIR + str(num) + ".txt"
        file = open(filename, "w")
        file.write(inputStr)
        file.close()
        return FileStream(filename)


class TestLexer:
    @staticmethod
    def test(input, expect, num):
        inputfile = TestUtil.makeSource(input, num)
        TestLexer.check(SOL_DIR, inputfile, num)
        dest = open(SOL_DIR + str(num) + ".txt", "r")
        line = dest.read()
        return line == expect

    @staticmethod
    def check(soldir, inputfile, num):
        dest = open(os.path.join(soldir, str(num) + ".txt"), "w")
        lexer = Lexer(inputfile)
        try:
            TestLexer.printLexeme(dest, lexer)
        except (ErrorToken, UncloseString, IllegalEscape) as err:
            dest.write(err.message)
        finally:
            dest.close()

    @staticmethod
    def printLexeme(dest, lexer):
        tok = lexer.nextToken()
        if tok.type != Token.EOF:
            dest.write(tok.text+",")
            TestLexer.printLexeme(dest, lexer)
        else:
            dest.write("<EOF>")


class NewErrorListener(ConsoleErrorListener):
    INSTANCE = None

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        raise SyntaxException("Error on line " + str(line) +
                              " col " + str(column) + ": " + offendingSymbol.text)


NewErrorListener.INSTANCE = NewErrorListener()


class SyntaxException(Exception):
    def __init__(self, msg):
        self.message = msg


class TestParser:
    @staticmethod
    def createErrorListener():
        return NewErrorListener.INSTANCE

    @staticmethod
    def test(input, expect, num):
        inputfile = TestUtil.makeSource(input, num)
        TestParser.check(SOL_DIR, inputfile, num)
        dest = open(SOL_DIR + str(num) + ".txt", "r")
        line = dest.read()
        return line == expect

    @staticmethod
    def check(soldir, inputfile, num):
        dest = open(os.path.join(soldir, str(num) + ".txt"), "w")
        lexer = Lexer(inputfile)
        listener = TestParser.createErrorListener()
        
        try:
            tokens = CommonTokenStream(lexer)
            parser = Parser(tokens)
            parser.removeErrorListeners()
            parser.addErrorListener(listener)
            parser.program()
            dest.write("successful")
        except (ErrorToken, UncloseString, IllegalEscape) as err:
            dest.write(err.message)
        except SyntaxException as f:
            dest.write(f.message)
        except Exception as e:
            dest.write(traceback.format_exc())
        finally:
            dest.close()


class TestPreGen:
    @staticmethod
    def test(input, expect, num):
        inputfile = TestUtil.makeSource(input, num)
        TestPreGen.check(SOL_DIR, inputfile, num)
        dest = open(os.path.join(SOL_DIR, str(num) + ".txt"), "r")
        line = dest.read()
        return line == expect

    @staticmethod
    def check(soldir, inputfile, num):
        dest = open(os.path.join(soldir, str(num) + ".txt"), "w")
        lexer = Lexer(inputfile)
        listener = TestParser.createErrorListener()
        try:
            tokens = CommonTokenStream(lexer)
            parser = Parser(tokens)
            parser.removeErrorListeners()
            parser.addErrorListener(listener)
            tree = parser.program()
            dest.write(tree.s)
        except (ErrorToken, UncloseString, IllegalEscape) as err:
            dest.write(err.message)
        except SyntaxException as f:
            dest.write(f.message)
        except Exception as e:
            dest.write(traceback.format_exc())
        finally:
            dest.close()
        
class TestVM:
    @staticmethod
    def test(input, expect, num):
        inputfile = TestUtil.makeSource(input, num)
        TestVM.check(SOL_DIR, TEST_DIR, num)
        dest = open(os.path.join(SOL_DIR, str(num) + ".txt"), "r")
        line = dest.read()
        return line == expect
    

    @staticmethod
    def check(soldir, testdir, num):
        destname = os.path.join(soldir, str(num) + ".txt")
        os.replace(os.path.join(testdir, str(num) + ".txt"),"input.txt")
        with PrologMQI() as mqi:
            with mqi.create_thread() as prolog:
                path = create_posix_path("./main/crazy/vm/ass2.pl")
                prolog.query(f'consult("{path}").')
                prolog.query('go')
               
        os.replace("output.txt",destname)
