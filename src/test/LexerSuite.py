import unittest
from TestUtils import TestLexer


class LexerSuite(unittest.TestCase):

    def test_lowercase_identifier(self):
        """test identifiers"""
        self.assertTrue(TestLexer.test("abc", "abc,<EOF>", 101))

    def test_lowercase_identifier2(self):
        """test identifiers"""
        self.assertTrue(TestLexer.test("abc", "abc,<EOF>", 102))

