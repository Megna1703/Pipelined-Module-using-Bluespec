import FloatingPoint     ::*;
import Vector            ::*;
import Real              ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import DReg  :: *;
import FloatingPoint ::*;
import ConfigReg ::*;
typedef FloatingPoint#(8,23) Float;
//============================ add.bsv =================================

`define STAGES_FADD_SP 4
typedef struct {
   Bit#(1) 	valid;
   FloatingPoint#(e,m)	value;
   Exception  	ex;
} ReturnType#(numeric type e, numeric type m) deriving (Bits,FShow);

typedef struct {
		Maybe#(FloatingPoint#(e,m)) res;
		Exception exc;
		RoundMode rmode;
		} CommonState#(numeric type e, numeric type m) deriving (Bits, Eq);


function Bool isNaNOrInfinity( FloatingPoint#(e,m) din );
   return (din.exp == '1);
endfunction

function Bool isSubNormal( FloatingPoint#(e,m) din );
   return (din.exp == 0);
endfunction

function Integer bias( FloatingPoint#(e,m) din );
   return (2 ** (valueof(e)-1)) - 1;
endfunction

function Bit#(e) unbias( FloatingPoint#(e,m) din );
   return (din.exp - fromInteger(bias(din)));
endfunction

function Bit#(1) getHiddenBit( FloatingPoint#(e,m) din );
   return (isSubNormal(din)) ? 0 : 1;
endfunction

function Integer minexp( FloatingPoint#(e,m) din );
  return 1-bias(din);
endfunction

function Integer minexp_subnormal( FloatingPoint#(e,m) din );
   return minexp(din)-valueof(m);
endfunction

function Integer maxexp( FloatingPoint#(e,m) din );
   return bias(din);
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) round( RoundMode rmode, FloatingPoint#(e,m) din, Bit#(2) guard )
   provisos(  Add#(m, 1, m1)
	    , Add#(m, 2, m2)
	    );

   FloatingPoint#(e,m) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaNOrInfinity(din)) begin
      out = din;
   end
   else begin
      let din_inc = din;

      Bit#(TAdd#(m,2)) sfd = unpack({1'b0, getHiddenBit(din), din.sfd}) + 1;

      if (msb(sfd) == 1) begin
	 if (din.exp == fromInteger(maxexp(din) + bias(out))) begin
	    din_inc = infinity(din_inc.sign);
	 end
	 else begin
	    din_inc.exp = din_inc.exp + 1;
	    din_inc.sfd = truncate(sfd >> 1);
	 end
      end
      else if ((din.exp == 0) && (truncateLSB(sfd) == 2'b01)) begin
	 din_inc.exp = 1;
	 din_inc.sfd = truncate(sfd);
      end
      else begin
	 din_inc.sfd = truncate(sfd);
      end

      if (guard != 0) begin
	 exc.inexact = True;
      end

      case(rmode)
	 Rnd_Nearest_Even:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din;
	       'b10: out = (lsb(din.sfd) == 1) ? din_inc : din;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Nearest_Away_Zero:
	 begin
	    case (guard)
	       'b00: out = din;
//	       'b01: out = din_inc;
	       'b01: out = din;
	       'b10: out = din_inc;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Plus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din;
	    else
	       out = din_inc;
	 end

	 Rnd_Minus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din_inc;
	    else
	       out = din;
	 end

	 Rnd_Zero:
	 begin
	    out = din;
	 end
      endcase
   end

   if (isInfinity(out)) begin
      exc.overflow = True;
   end

   return tuple2(out,exc);
endfunction

function Tuple3#(FloatingPoint#(e,m),Bit#(2),Exception) normalize( FloatingPoint#(e,m) din, Bit#(x) sfdin )
   provisos(
      Add#(1, a__, x),
      Add#(m, b__, x),
      // per request of bsc
      Add#(c__, TLog#(TAdd#(1, x)), TAdd#(e, 1))
      );

   FloatingPoint#(e,m) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;
   Int#(TAdd#(e,1)) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   let zeros = countZerosMSB(sfdin);

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
	 // carry, no sfd adjust necessary

	 if (out.exp == 0)
	    out.exp = 2;
	 else
	    out.exp = out.exp + 1;

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
	 // already normalized

	 if (out.exp == 0)
	    out.exp = 1;

	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(x))) begin
	 // exactly zero
	 out.exp = 0;
      end
      else begin
	 // try to normalize
	 Int#(TAdd#(e,1)) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(TAdd#(e,1)) maxshift = exp - fromInteger(minexp(out));

`ifdef denormal_support
if (shift > maxshift) begin
	    // result will be subnormal

	    sfdin = sfdin << maxshift;
	    out.exp = 0;
	 end
	 else begin
	    // result will be normal

	    sfdin = sfdin << shift;
	    out.exp = out.exp - truncate(pack(shift));
	 end

 	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
`else
         if (shift <= maxshift) begin
	    // result will be normal

	    sfdin = sfdin << shift;
       out.exp = out.exp - truncate(pack(shift));
       end
	    sfdin = sfdin << 2;
	 

 	 // carry, hidden bits
	
      end
`endif
      
      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(valueOf(m));

      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;

      guard[0] = |sfdin;
   end

   if ((out.exp == 0) && (guard != 0))
      exc.underflow = True;

   return tuple3(out,guard,exc);
endfunction

function FloatingPoint#(e,m) canonicalize (FloatingPoint#(e,m) in);
	if (in.exp == '1 && in.sfd != 0)
	  return FloatingPoint{sign:False, exp:'1, sfd:1<<(valueof(m)-1)};
	else
	  return in;
endfunction 
function Tuple2#(FloatingPoint#(e,m),Exception) fp_add (Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m),RoundMode) operands)

   provisos(
      Add#(a__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
      );

   function Tuple7#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e),
				Bit#(e)) s1_stage(Tuple3#(
								FloatingPoint#(e,m),
					      FloatingPoint#(e,m),
					      RoundMode) op);

      match {.opA, .opB, .rmode } = op;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Int#(TAdd#(e,2)) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(TAdd#(e,2)) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));

      Bit#(TAdd#(m,5)) sfdA = {1'b0, getHiddenBit(opA), opA.sfd, 3'b0};
      Bit#(TAdd#(m,5)) sfdB = {1'b0, getHiddenBit(opB), opB.sfd, 3'b0};

      Bit#(TAdd#(m,5)) x;
      Bit#(TAdd#(m,5)) y;
      Bool sgn;
      Bool sub;
      Bit#(e) exp;
      Bit#(e) expdiff;

      if ((expB > expA) || ((expB == expA) && (sfdB > sfdA))) begin
	 exp = opB.exp;
	 expdiff = truncate(pack(expB - expA));
	 x = sfdB;
	 y = sfdA;
	 sgn = opB.sign;
	 sub = (opB.sign != opA.sign);
      end
      else begin
	 exp = opA.exp;
	 expdiff = truncate(pack(expA - expB));
	 x = sfdA;
	 y = sfdB;
	 sgn = opA.sign;
	 sub = (opA.sign != opB.sign);
      end

      if (isSNaN(opA)) begin
	 s.res = tagged Valid nanQuiet(opA);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opB)) begin
	 s.res = tagged Valid nanQuiet(opB);
	 s.exc.invalid_op = True;
      end
      else if (isQNaN(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isQNaN(opB)) begin
	 s.res = tagged Valid opB;
      end
      else if (isInfinity(opA) && isInfinity(opB)) begin
	 if (opA.sign == opB.sign)
	    s.res = tagged Valid infinity(opA.sign);
	 else begin
	    s.res = tagged Valid qnan();
	    s.exc.invalid_op = True;
	 end
      end
      else if (isInfinity(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isInfinity(opB)) begin
	 s.res = tagged Valid opB;
      end

      return tuple7(s,
		    x,
		    y,
		    sgn,
		    sub,
		    exp,
		    expdiff);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s2_stage(Tuple7#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e),
					      Bit#(e)) op);

      match {.s, .opA, .opB, .sign, .subtract, .exp, .diff} = op;

      if (s.res matches tagged Invalid) begin
	 if (diff < fromInteger(valueOf(m) + 5)) begin
	    Bit#(TAdd#(m,5)) guard = opB;

	    guard = opB << (fromInteger(valueOf(m) + 5) - diff);
	    opB = opB >> diff;
	    opB[0] = opB[0] | (|guard);
	 end
	 else if (|opB == 1) begin
	    opB = 1;
	 end
      end

      return tuple6(s,
		    opA,
		    opB,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s3_stage(Tuple6#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e)) op);

      match {.s, .a, .b, .sign, .subtract, .exp} = op;

      let sum = a + b;
      let diff = a - b;

      return tuple6(s,
		    sum,
		    diff,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple4#(CommonState#(e,m),
		    FloatingPoint#(e,m),
		    Bit#(2),
		    Bool) s4_stage(Tuple6#(CommonState#(e,m),
					   Bit#(TAdd#(m,5)),
					   Bit#(TAdd#(m,5)),
					   Bool,
					   Bool,
					   Bit#(e)) op);

      match {.s, .addres, .subres, .sign, .subtract, .exp} = op;

      FloatingPoint#(e,m) out = defaultValue;
      Bit#(2) guard = 0;

      if (s.res matches tagged Invalid) begin
	 Bit#(TAdd#(m,5)) result;

	 if (subtract) begin
	    result = subres;
	 end
	 else begin
            result = addres;
	 end

	 out.sign = sign;
	 out.exp = exp;

	 let y = normalize(out, result);
	 out = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);
      end

      return tuple4(s,
		    out,
		    guard,
		    subtract);
   endfunction

   function Tuple2#(FloatingPoint#(e,m),
		    Exception) s5_stage(Tuple4#(CommonState#(e,m),
						FloatingPoint#(e,m),
						Bit#(2),
						Bool) op);

      match {.s, .rnd, .guard, .subtract} = op;

      FloatingPoint#(e,m) out = rnd;

      if (s.res matches tagged Valid .x) begin
	 out = x;
      end
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      // adjust sign for exact zero result
      if (isZero(out) && !s.exc.inexact && subtract) begin
	 out.sign = (s.rmode == Rnd_Minus_Inf);
      end

      return tuple2(canonicalize(out),s.exc);
   endfunction


	 return s5_stage( s4_stage( s3_stage( s2_stage( s1_stage(operands))))); //INTERFACE CHANGES
endfunction
////////////////////////////
interface Ifc_add#(numeric type e, numeric type m, numeric type nos);
	method Action send(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode) operands);

	method ReturnType#(e,m) receive();
endinterface
module mk_add(Ifc_add#(e,m,nos))
	provisos(
		 Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1)),
		 Add#(b__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
	);


	Vector#(nos,Reg#(Tuple2#(FloatingPoint#(e,m),Exception))) rg_stage_out <- replicateM(mkReg(tuple2(unpack(0),unpack(0))));
	Vector#(nos,Reg#(Bit#(1))) rg_stage_valid <- replicateM(mkDReg(0));

	rule rl_pipeline;
		 for(Integer i = 1 ; i <= valueOf(nos) -1 ; i = i+1)
		 begin
				rg_stage_out[i] <= rg_stage_out[i-1];
				rg_stage_valid[i] <= rg_stage_valid[i-1];
		 end
	endrule
	method Action send(Tuple3#(FloatingPoint#(e,m),
				FloatingPoint#(e,m),
				RoundMode) operands);		
					 
					 rg_stage_out[0] <= fp_add(operands);
					 rg_stage_valid[0] <= 1;

	endmethod
//	method Tuple2#(Bit#(1),Tuple2#(FloatingPoint#(e,m),Exception)) receive();
	method ReturnType#(e,m) receive();
		let x = ReturnType{valid:rg_stage_valid[valueOf(nos)-1] ,value:tpl_1(rg_stage_out[valueOf(nos)-1]) ,ex:tpl_2(rg_stage_out[valueOf(nos)-1])};
		return x;
//		return tuple2(rg_stage_valid[nos-1],rg_stage_out[nos-1]);
	endmethod 
endmodule

// (*synthesize*)
module mk_fpu_add_sub_sp_instance(Ifc_add#(8,23,`STAGES_FADD_SP));
	let ifc();
	mk_add _temp(ifc);
	return (ifc);
endmodule

interface Ifc_add_s3;
    method Action put(Bit#(2) op, Float copy_xin, Maybe#(Float) x);
    method ActionValue#(Maybe#(Float)) add_result; 
    method Float copy_x; 
	method Bit#(2) copy_op;
endinterface

// (*synthesize*)
module mkadd_s3(Ifc_add_s3); 
	Reg#(Maybe#(Float)) inp1 <- mkReg(tagged Invalid), inp2 <- mkReg(tagged Invalid), inp3 <- mkReg(tagged Invalid), inp4 <- mkReg(tagged Invalid);

	Reg#(Bit#(2)) op1 <- mkReg(0), op2 <- mkReg(0), op3 <- mkReg(0), op4 <- mkReg(0);

	Reg#(Float) x1 <- mkReg(0), x2 <- mkReg(0), x3 <- mkReg(0), x4 <- mkReg(0);

    Reg#(Bool) add1_valid <- mkReg(False);
    Reg#(Bool) add2_valid <- mkReg(False);
    Reg#(Bool) add3_valid <- mkReg(False);
    Reg#(Bool) add4_valid <- mkReg(False);

    Ifc_add#(8,23,4) add <- mk_add;

	Reg#(Maybe#(Float)) temp_inp1 <- mkDWire(tagged Invalid);

    rule add2;
		op2 <= op1;
		inp2 <= inp1;
		x2 <= x1;
    endrule 

    rule add3;
		op3 <= op2;
		inp3 <= inp2;
		x3 <= x2;
    endrule

    rule add4;
		op4 <= op3;
	    inp4 <= inp3;
		x4 <= x3;
    endrule

	rule pass_inp;
		inp1 <= temp_inp1;
	endrule

	method Action put(Bit#(2) op, Float copy_xin, Maybe#(Float) x);
		op1 <= op;
		temp_inp1 <= x;
		x1 <= copy_xin;
		RoundMode rm = Rnd_Nearest_Even;
        add.send(tuple3(x.Valid, 1 , rm));
	endmethod 
	
	method ActionValue#(Maybe#(Float)) add_result; 
		let r = add.receive;
		if (isValid(inp4))
			return tagged Valid(r.value);
		else
			return tagged Invalid;
	endmethod 

    method Float copy_x = x4;
	method Bit#(2) copy_op = op4;
endmodule

