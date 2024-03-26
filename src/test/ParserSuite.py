import unittest
from TestUtils import TestParser


class ParserSuite(unittest.TestCase):
    def test_simple_program(self):
        """Simple program: int main() {} """
        input = """program abc;
					begin
					end."""
        expect = "successful"
        self.assertTrue(TestParser.test(input, expect, 201))
    def test_no_end_dot(self):
        input = """program abc;
					const i = 30.0E3;
					begin
					end"""
        expect = "Error on line 4 col 8: <EOF>"
        self.assertTrue(TestParser.test(input, expect, 202))
    def test_no_end_semi(self):
        input = """program abc;
					var i: string;
					begin
					 i:= 'af''sf'
					end."""
        expect = "Error on line 5 col 5: end"
        self.assertTrue(TestParser.test(input, expect, 203))
    def test_wrong_expr(self):
        input = """program abc;
					begin
					var i:integer;
					    if true then
					        a:=10;
					    else
					        i:=i 1;
					end."""						
        expect = "Error on line 7 col 18: 1"
        self.assertTrue(TestParser.test(input, expect, 204))

    def test_long_program(self):
        input = """program sel;
					var i, j, min, t: integer;
					var k:integer;
					var a:array [10] of integer;
					         procedure fibonacci(i,k,l:integer;j,m:real);
					                begin
					                        const max = 25;
					                        var i: integer;
					                        var f: array [25] of integer;
					                        f[0]:= 1;
					                        f[1]:= 1;
					                        loop max do f[i]:= f[i-1] + f[i-2];
					                end;
					begin
					k:=1;
					do
					begin
					        a[k]:=readInt();
					        if k <10 then begin
					                k:=k-1;
					                continue;
					        end
					        while k < 100+1 do while k >= 100 do continue;
					        k:=k+1;
					end
					while (k>10) ;
					loop 10 do
					begin
					        min:=i;
					        loop 10 do
					                if a[j]<a[min] then min:=j;
					        t:=a[min];
					        a[min]:=a[i];
					        a[i]:=t;
					end 
					loop 1 do
					        write(a[i],' ');
					end."""						
        expect = "successful"
        self.assertTrue(TestParser.test(input, expect, 205))

