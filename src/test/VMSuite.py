import unittest
from TestUtils import TestVM


class VMSuite(unittest.TestCase):
    def test_simple_program(self):
        input = """[[],[],[call(writeInt,[3])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 401))

    def test_simple_expression(self):
        input = """[[],[],[call(writeInt,[add(3,1)])]]."""
        expect = "4"
        self.assertTrue(TestVM.test(input, expect, 402))

    def test_redeclaration(self):
        input = (
            """[[var(a,integer),var(b,integer),var(a,real)],[],[call(writeInt,[1])]]."""
        )
        expect = "Redeclared identifier: var(a,real)"
        self.assertTrue(TestVM.test(input, expect, 403))

    def test_sub_expression(self):
        input = """[[],[],[call(writeInt,[sub(3,1)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 404))

    def test_time_expression(self):
        input = """[[],[],[call(writeInt,[times(3,1)])]]."""
        expect = "3"
        self.assertTrue(TestVM.test(input, expect, 405))

    def test_idiv_expression(self):
        input = """[[],[],[call(writeInt,[idiv(5,2)])]]."""
        expect = "2"
        self.assertTrue(TestVM.test(input, expect, 406))

    def test_imod_expression(self):
        input = """[[],[],[call(writeInt,[imod(5,2)])]]."""
        expect = "1"
        self.assertTrue(TestVM.test(input, expect, 407))

    def test_combined_expression(self):
        input = """[[],[],[call(writeInt,[add(3,add(5,2))])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 408))

    def test_combined_expression_1(self):
        input = """[[],[],[call(writeInt,[add(3,times(5,add(2,1)))])]]."""
        expect = "18"
        self.assertTrue(TestVM.test(input, expect, 409))
