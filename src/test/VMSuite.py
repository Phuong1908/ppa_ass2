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

    def test_builtin_writeBool(self):
        input = """[[],[],[call(writeBool,[true])]]."""
        expect = "true"
        self.assertTrue(TestVM.test(input, expect, 410))

    def test_builtin_writeReal(self):
        input = """[[],[],[call(writeReal,[3.14])]]."""
        expect = "3.14"
        self.assertTrue(TestVM.test(input, expect, 411))

    def test_builtin_writeIntLn(self):
        input = """[[],[],[call(writeIntLn,[3])]]."""
        expect = "3\n"
        self.assertTrue(TestVM.test(input, expect, 412))

    def test_builtin_writeStr(self):
        input = """[[],[],[call(writeStr,[str("Hello, World!")])]]."""
        expect = "Hello, World!"
        self.assertTrue(TestVM.test(input, expect, 413))

    def test_builtin_writeStrLn(self):
        input = """[[],[],[call(writeStrLn,[str("Hello, World!")])]]."""
        expect = "Hello, World!\n"
        self.assertTrue(TestVM.test(input, expect, 414))

    # def test_ele_single_dimension(self):
    #     input = """[[],[],[call(writeInt,[ele(array([1, 2, 3, 4, 5]), [2])])]]."""
    #     expect = "3"
    #     self.assertTrue(TestVM.test(input, expect, 415))

    # def test_ele_two_dimension(self):
    #     input = """[[],[],[call(writeInt,[ele(array([[1, 2], [3, 4], [5, 6]]), [1, 1])])]]."""
    #     expect = "4"
    #     self.assertTrue(TestVM.test(input, expect, 416))

    # def test_ele_type_mismatch(self):
    #     input = """[[],[],[call(writeInt,[ele(array([1, 2, 3, 4, 5]), [2, 3])])]]."""
    #     expect = "Type mismatch: ele(array([1, 2, 3, 4, 5]), [2, 3])"
    #     self.assertTrue(TestVM.test(input, expect, 417))

    # def test_ele_index_out_of_bound(self):
    #     input = """[[],[],[call(writeInt,[ele(array([1, 2, 3, 4, 5]), [5])])]]."""
    #     expect = "Index out of bound: [5]"
    #     self.assertTrue(TestVM.test(input, expect, 418))

    def test_assign_global_variable(self):
        input = """[[var(x, integer)],[],[assign(x, 5), call(writeInt, [x])]]."""
        expect = "5"
        self.assertTrue(TestVM.test(input, expect, 419))

    def test_assign_local_variable(self):
        input = """[[var(x, integer)],[],[var(y, integer), assign(y, 10), call(writeInt, [y])]]."""
        expect = "10"
        self.assertTrue(TestVM.test(input, expect, 420))

    def test_assign_undeclared_identifier(self):
        input = """[[],[],[assign(z, 5)]]."""
        expect = "Undeclared identifier: z"
        self.assertTrue(TestVM.test(input, expect, 421))

    def test_assign_type_mismatch(self):
        input = """[[],[],[var(x, integer), assign(x, true)]]."""
        expect = "Type mismatch: assign(x,true)"
        self.assertTrue(TestVM.test(input, expect, 422))

    def test_if_true_branch(self):
        input = """[[var(x, integer)],[],[if(true, [call(writeStrLn, [str("True branch executed")])], [call(writeStrLn, [str("False branch executed")])])]]."""
        expect = "True branch executed\n"
        self.assertTrue(TestVM.test(input, expect, 430))

    def test_if_false_branch(self):
        input = """[[var(x, boolean)],[],[assign(x, false), if(x, [call(writeStrLn, [str("True branch executed")])], [call(writeStrLn, [str("False branch executed")])])]]."""
        expect = "False branch executed\n"
        self.assertTrue(TestVM.test(input, expect, 431))

    def test_if_no_else_branch_true(self):
        input = """[[var(x, boolean)],[],[assign(x, true), if(x, [call(writeStrLn, [str("True branch executed")])])]]."""
        expect = "True branch executed\n"
        self.assertTrue(TestVM.test(input, expect, 432))

    def test_if_no_else_branch_false(self):
        input = """[[var(x, boolean)],[],[assign(x, false), if(x, [call(writeStrLn, [str("True branch executed")])])]]."""
        expect = ""
        self.assertTrue(TestVM.test(input, expect, 433))

    def test_if_type_mismatch(self):
        input = """[[var(x, integer)],[],[assign(x, 5), if(x, [call(writeStrLn, [str("True branch executed")])], [call(writeStrLn, [str("False branch executed")])])]]."""
        expect = "Type mismatch: if(x,[call(writeStrLn,[str(True branch executed)])],[call(writeStrLn,[str(False branch executed)])])"
        self.assertTrue(TestVM.test(input, expect, 434))

    def test_while_loop(self):
        input = """
            [
                [var(x, integer)],
                [],
                [
                    assign(x, 0),
                    while(less(x, 3),
                        [
                            call(writeStrLn, [x]),
                            assign(x, add(x, 1))
                        ]
                    ),
                    call(writeStrLn, [str("Loop finished")])
                ]
            ].
        """
        expect = "0\n1\n2\nLoop finished\n"
        self.assertTrue(TestVM.test(input, expect, 440))

    def test_while_sum(self):
        input = """
            [
                [var(sum, integer), var(i, integer)],
                [],
                [
                    assign(sum, 0),
                    assign(i, 1),
                    while(le(i, 5),
                        [
                            assign(sum, add(sum, i)),
                            assign(i, add(i, 1))
                        ]
                    ),
                    call(writeStrLn, [sum])
                ]
            ].
        """
        expect = "15\n"
        self.assertTrue(TestVM.test(input, expect, 441))

    def test_while_even_numbers(self):
        input = """
            [
                [var(i, integer)],
                [],
                [
                    assign(i, 0),
                    while(le(i, 8),
                        [
                            if(eql(imod(i, 2), 0),
                                [call(writeStrLn, [i])]
                            ),
                            assign(i, add(i, 1))
                        ]
                    )
                ]
            ].
        """
        expect = "0\n2\n4\n6\n8\n"
        self.assertTrue(TestVM.test(input, expect, 442))

    def test_while_factorial(self):
        input = """
            [
                [var(result, integer), var(i, integer)],
                [],
                [
                    assign(result, 1),
                    assign(i, 1),
                    while(le(i, 5),
                        [
                            assign(result, times(result, i)),
                            assign(i, add(i, 1))
                        ]
                    ),
                    call(writeStrLn, [result])
                ]
            ].
        """
        expect = "120\n"
        self.assertTrue(TestVM.test(input, expect, 443))

    def test_do_sum(self):
        input = """
            [
                [var(sum, integer), var(i, integer)],
                [],
                [
                    assign(sum, 0),
                    assign(i, 1),
                    do(
                        [
                            assign(sum, add(sum, i)),
                            assign(i, add(i, 1))
                        ],
                        le(i, 5)
                    ),
                    call(writeStrLn, [sum])
                ]
            ].
        """
        expect = "15\n"
        self.assertTrue(TestVM.test(input, expect, 445))

    def test_do_even_numbers(self):
        input = """
            [
                [var(i, integer)],
                [],
                [
                    assign(i, 0),
                    do(
                        [
                            if(eql(imod(i, 2), 0),
                                [call(writeStrLn, [i])]
                            ),
                            assign(i, add(i, 1))
                        ],
                        le(i, 8)
                    )
                ]
            ].
        """
        expect = "0\n2\n4\n6\n8\n"
        self.assertTrue(TestVM.test(input, expect, 446))
