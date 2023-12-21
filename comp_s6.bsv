import FloatingPoint     ::*;
import ConfigReg         ::*;
typedef FloatingPoint#(8,23) Float;

interface Ifc_comp_s6;
    method Action put(Bit#(2) op, Float x1, Maybe#(Float) div_res);
    method Maybe#(Float) comp_result; 
endinterface

module mk_comp_s6(Ifc_comp_s6);
    Reg#(Maybe#(Float)) comp_res <- mkReg(tagged Invalid);

    Float alpha = 1.67;
    Float lambda = 1.05;
    Float alpha_lambda = 1.75;

	Reg#(Maybe#(Float)) temp_cres <- mkDWire(tagged Invalid);

	rule pass_value;
		comp_res <= temp_cres;
	endrule

	method Action put(Bit#(2) op, Float x1, Maybe#(Float) div_res);
		Float temp_res=0;
		if (isValid(div_res)) begin
			if (op[1] == 0)	temp_res = div_res.Valid;
			else if (x1.sign == True) begin 
				if (op == 2'b10) temp_res = alpha * x1; //alpha*factorx
				else temp_res = alpha_lambda * div_res.Valid; //alpha*lambda
			end	else begin 
				if (op == 2'b10) temp_res = x1; //factorx
				else temp_res = lambda * x1;
			end	
			temp_cres <= tagged Valid(temp_res);
		end
	endmethod

    method Maybe#(Float) comp_result = comp_res; 
endmodule 