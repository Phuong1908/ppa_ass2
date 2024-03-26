import unittest
from TestUtils import TestPreGen


class PreGenSuite(unittest.TestCase):
    def test_simple_program(self):
        """Simple program: int main() {} """
        input = """program abc;
					begin
					end."""
        expect = "[[],[],[]]. "
        self.assertTrue(TestPreGen.test(input, expect, 301))
    def test_program_with_const(self):
        input = """program abc;
					const i = 30.0E3;
					begin
					end."""
        expect = "[[const(i,30.0E3)],[],[]]. "
        self.assertTrue(TestPreGen.test(input, expect, 302))
    def test_program_with_assign(self):
        input = """program abc;
					var i: string;
					begin
					 i:= 'af''sf';
					end."""
        expect = """[[var(i,string)],[],[assign(i,str("af'sf"))]]. """
        self.assertTrue(TestPreGen.test(input, expect, 303))
    def test_program_with_if(self):
        input = """program abc;
					begin
					var i:integer;
					    if true then
					        a:=10;
					    else
					        i:=i-1;
					end."""						
        expect = "[[],[],[var(i,integer),if(true,assign(a,10),assign(i,sub(i,1)))]]. "
        self.assertTrue(TestPreGen.test(input, expect, 304))

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
        expect = """[[var(i,integer),var(j,integer),var(min,integer),var(t,integer),var(k,integer),var(a,arr([10],integer))],[proc(fibonacci,[par(i,integer),par(k,integer),par(l,integer),par(j,real),par(m,real)],[const(max,25),var(i,integer),var(f,arr([25],integer)),assign(ele(f,[0]),1),assign(ele(f,[1]),1),loop(max,assign(ele(f,[i]),add(ele(f,[sub(i,1)]),ele(f,[sub(i,2)]))))])],[assign(k,1),do([assign(ele(a,[k]),call(readInt,[])),if(less(k,10),[assign(k,sub(k,1)),continue(null)]),while(add(less(k,100),1),while(ge(k,100),continue(null))),assign(k,add(k,1))],greater(k,10)),loop(10,[assign(min,i),loop(10,if(less(ele(a,[j]),ele(a,[min])),assign(min,j))),assign(t,ele(a,[min])),assign(ele(a,[min]),ele(a,[i])),assign(ele(a,[i]),t)]),loop(1,call(write,[ele(a,[i]),str(" ")]))]]. """
        self.assertTrue(TestPreGen.test(input, expect, 305))

