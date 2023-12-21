import FloatingPoint     ::*;
import ConfigReg         ::*;

interface Ifc_div_op;
    method Action put(Float a,Float b, Bool c, Bit#(2) op);
	method Maybe#(Float) get;
	method Bit#(2) copy_op;
	method Float copy_x;
endinterface

module mk_div_op(Ifc_div_op);
	Reg#(Maybe#(Float)) y <- mkReg(tagged Invalid);

	Reg#(Bit#(2)) op <- mkReg(0);
	Reg#(Float) x <- mkReg(0);

	Reg#(Maybe#(Float)) temp_y <- mkDWire(tagged Invalid);

	rule pass_y;
		y <= temp_y;
	endrule

    method Action put(Float a, Float b, Bool c, Bit#(2) opcode);
		if (c) begin
			if (opcode[1] == 0)
				temp_y <= tagged Valid(a/b);
			else 
				temp_y <= tagged Valid(a);
		end
		else 
			temp_y <= tagged Invalid;
		op <= opcode;
		x <= b;
    endmethod

	method Maybe#(Float) get = y;
	method Bit#(2) copy_op = op;
	method Float copy_x = x;
endmodule 

