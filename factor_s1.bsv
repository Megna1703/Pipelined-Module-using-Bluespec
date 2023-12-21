import FloatingPoint     ::*;
import ConfigReg         ::*;

typedef FloatingPoint#(8,23) Float;

interface Ifc_factor_s1;
    method Action put(Bit#(2) op, Maybe#(Float) x_in);
    method Maybe#(Float) factor_result; 
	method Bit#(2) copy_op;
endinterface

module mkfactor_s1(Ifc_factor_s1);    // factor has a latency of 2 cycles
    Reg#(Maybe#(Float)) factor_res <- mkReg(tagged Invalid);
	Reg#(Bit#(2)) opcode <- mkReg(0);

    method Action put(Bit#(2) op, Maybe#(Float) x_in);
		if (isValid(x_in)) begin
			let x = x_in.Valid;
			if (op == 2'b00) begin
				factor_res <= tagged Valid(2 * x);
			end else if (op == 2'b01) begin
				factor_res <= tagged Valid(-1 * x);
			end else begin 
				factor_res <= tagged Valid(1 * x);
			end
		end else 
			factor_res <= tagged Invalid;
		opcode <= op;
    endmethod

    method Maybe#(Float) factor_result; 
        return factor_res;
    endmethod 

	method Bit#(2) copy_op;
        return opcode;
    endmethod
endmodule

    