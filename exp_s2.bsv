import FloatingPoint ::*;
import ConfigReg ::*;
typedef FloatingPoint#(8,23) Float;

function Float generate_2_x (Bit#(8) x);

    if (x[7:3] > 0) // doing for x greater than 5
        return 32; //32
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 16; //16
	else if (x[1] == 1 && x[0] == 1)
	    return 8; //8
	else if (x[1] == 1)
		return 4; //4
	else if (x[0] == 1)
		return 2; //2
	else
		return 1; //1

endfunction

function Float generate_2_negx (Bit#(8) x);
    if (x[7:3] > 0) // doing for x greater than 5
        return -32;
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return -16; //16
	else if (x[1] == 1 && x[0] == 1)
	    return -8; //8
	else if (x[1] == 1)
		return -4; //4
	else if (x[0] == 1)
		return -2; //2
	else 
		return -1; //1
endfunction 

function Float generate_e_2_x (Bit#(8) x);
    
    if (x[7:3] > 0) // doing for x greater than 4
        return 78701400162304; //e^32             // I replaced the value of e^32 as it exceeds the value
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 8871381.143; //e^16
	else if (x[1] == 1 && x[0] == 1)
	    return 2978.486; //e^8
	else if (x[1] == 1)
		return 54.575; //e^4
	else if (x[0] == 1)
		return 7.387; //e^2
	else
		return 2.718; //e

endfunction

function Float generate_e_2_negx (Bit#(8) x);

    if (x[7:3] > 0) // doing for x greater than 4
        return 0.000000000000012706; //e^-32
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 0.0000001127; //e^-16
	else if (x[1] == 1 && x[0] == 1)
	    return 0.000335; //e^-8
	else if (x[1] == 1)
		return 0.0183; //e^-4
	else if (x[0] == 1)
		return 0.135; //e^-2
	else
		return 0.368; //e^-1

endfunction 

interface Ifc_exp;
    method Action put(Bit#(2) opcode, Maybe#(Float) x);
    method Maybe#(Float) get; 
	method Float copy_x;
	method Bit#(2) copy_op;
endinterface

// (*synthesize*)
module mkExp(Ifc_exp);
	// Stage 0
	Reg#(Maybe#(Float)) x_wire <- mkWire;
	Reg#(Bit#(2)) op_wire <- mkWire;
    Reg#(Float) stage0_x <- mkReg(defaultValue);

	// Stage 1
	Reg#(Bit#(2)) op1 <- mkReg(0);
	Reg#(Float) x1 <- mkReg(0);
	Reg#(Bool) s1_valid <- mkReg(False);
	Reg#(Float) stage1_x <- mkReg(defaultValue);
	Reg#(Float) stage1_int_res <- mkReg(defaultValue);

	// Stage 2
	Reg#(Bit#(2)) op2 <- mkReg(0);
	Reg#(Float) x2 <- mkReg(0);
	Reg#(Bool) s2_valid <- mkReg(False);
    Reg#(Float) stage2_x <- mkReg(defaultValue);
	Reg#(Float) stage2_int_res <- mkReg(defaultValue);

	// Stage 3
	Reg#(Bit#(2)) op3 <- mkReg(0);
	Reg#(Float) x3 <- mkReg(0);
	Reg#(Bool) s3_valid <- mkReg(False);
	Reg#(Float) txv_1 <- mkReg(defaultValue);
	Reg#(Float) stage3_int_res <- mkReg(defaultValue);
	Reg#(Float) tsum_1 <- mkReg(defaultValue);

	// Stage 4
	Reg#(Bit#(2)) op4 <- mkReg(0);
	Reg#(Float) x4 <- mkReg(0);
	Reg#(Bool) s4_valid <- mkReg(False);
    Reg#(Float) txv_2 <- mkReg(defaultValue);
	Reg#(Float) stage4_int_res <- mkReg(defaultValue);
	Reg#(Float) tsum_2 <- mkReg(defaultValue);

	// Stage 5
	Reg#(Bit#(2)) op5 <- mkReg(0);
	Reg#(Float) x5 <- mkReg(0);
	Reg#(Bool) s5_valid <- mkReg(False);
	Reg#(Float) txv_3 <- mkReg(defaultValue);
	Reg#(Float) stage5_int_res <- mkReg(defaultValue);
	Reg#(Float) tsum_3 <- mkReg(defaultValue);

	// Stage 6
	Reg#(Bit#(2)) op6 <- mkReg(0);
	Reg#(Float) x6 <- mkReg(0);
	Reg#(Bool) s6_valid <- mkReg(False);
	Reg#(Float) txv_4 <- mkReg(defaultValue);
	Reg#(Float) stage6_int_res <- mkReg(defaultValue);
	Reg#(Float) tsum_4 <- mkReg(defaultValue);

    // Stage 7 
	Reg#(Bit#(2)) op7 <- mkReg(0);
	Reg#(Float) x7 <- mkReg(0);
    Reg#(Maybe#(Float)) result <- mkReg(defaultValue);

	// Stage 1
	rule stage1_check_if_smol;
		Float x = x_wire.Valid;
		op1 <= op_wire;
		x1 <= x;

		if (x.exp < 127) begin
			stage1_x <= x;
			stage1_int_res <= 1;
		end
		else begin
			Bit#(8) exp_diff = x.exp - 127;
			if (x.sign == False) begin 
				Float xdiff = x - generate_2_x(exp_diff);
				stage1_x <= xdiff;
				stage1_int_res <= generate_e_2_x(exp_diff);
			end 
			else begin 
				Float xdiff = x - generate_2_negx(exp_diff);
				stage1_x <= xdiff;
				stage1_int_res <= generate_e_2_negx(exp_diff);
			end  
		end 
	endrule

	rule s1_valid_pass;
		s1_valid <= isValid(x_wire);
	endrule

	// Stage 2
	rule stage2_check_if_smol;  
		Float x = stage1_x;   
		op2 <= op1;
		x2 <= x1;

		if (x.exp < 127) begin
			stage2_x <= x;
			stage2_int_res <= stage1_int_res;
		end
		else begin
			Bit#(8) exp_diff = x.exp - 127;
			if (x.sign == False) begin 
				Float xdiff = x - generate_2_x(exp_diff);
				stage2_x <= xdiff;
				stage2_int_res <= stage1_int_res * generate_e_2_x(exp_diff);
			end else begin 
				Float xdiff = x - generate_2_negx(exp_diff);
				stage2_x <= xdiff;
				stage2_int_res <= stage1_int_res * generate_e_2_negx(exp_diff);
			end 
		end 
	endrule

	rule s2_valid_pass;
		s2_valid <= s1_valid;
	endrule

	// Stage 3 taylor first term 
    rule stage3;
		Float z = FloatingPoint {
            sign:       False,
            exp:      8'b01111111,
            sfd:      23'b00000000000000000000000
        }; 
		op3 <= op2;
		x3 <= x2;

		txv_1 <= stage2_x;
        Float tsum = z + stage2_x; 
		tsum_1 <= tsum;
		stage3_int_res <= stage2_int_res;
	endrule 

	rule s3_valid_pass;
		s3_valid <= s2_valid;
	endrule

	// Stage 4 taylor second term 
	rule stage4;
		txv_2 <= txv_1;
		op4 <= op3;
		x4 <= x3;
		Float tsum_temp = tsum_1;
		Float tsum = tsum_temp + txv_1*txv_1*0.5;
        tsum_2 <= tsum;
		stage4_int_res <= stage3_int_res;
	endrule 

	rule s4_valid_pass;
		s4_valid <= s3_valid;
	endrule

	// Stage 5 taylor third term 
	rule stage5;
		txv_3 <= txv_2;
		op5 <= op4;
		x5 <= x4;
		Float tsum_temp = tsum_2;
		Float tsum = tsum_temp + txv_2*txv_2*txv_2*0.167;
		tsum_3 <= tsum;
		stage5_int_res <= stage4_int_res;
	endrule 

	rule s5_valid_pass;
		s5_valid <= s4_valid;
	endrule

	// Stage 6 taylor four term
	rule stage6;
		txv_4 <= txv_3;
		op6 <= op5;
		x6 <= x5;
		Float tsum_temp = tsum_3;
		Float tsum = (tsum_temp + txv_3*txv_3*txv_3*txv_3*0.0417);
        tsum_4 <= tsum;
        stage6_int_res <= stage5_int_res;
	endrule 

	rule s6_valid_pass;
		s6_valid <= s5_valid;
	endrule

    // Stage 7 taylor five term
    rule stage7;
		op7 <= op6;
		x7 <= x6;
        Float tx_5 = txv_4;
        Float tsum_temp = tsum_4;
		Float tsum = (tsum_temp + tx_5*tx_5*tx_5*tx_5*tx_5*0.00833) * stage6_int_res;
		
		if (s6_valid)
        	result <= tagged Valid(tsum);
		else 
			result <= tagged Invalid;
    endrule 

	method Action put(Bit#(2) opcode, Maybe#(Float) x_input);
		x_wire <= x_input;
		op_wire <= opcode;
		// $display("%b hi", opcode);
		// $finish;
	endmethod

	// Final stage
    method Maybe#(Float) get;
		return result;
	endmethod

	method Bit#(2) copy_op;
		return op7;
	endmethod 
	
	method Float copy_x;
		return x7;
	endmethod
endmodule