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

//============================ ADD module =================================

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

	method ReturnType#(e,m) receive();
		let x = ReturnType{valid:rg_stage_valid[valueOf(nos)-1] ,value:tpl_1(rg_stage_out[valueOf(nos)-1]) ,ex:tpl_2(rg_stage_out[valueOf(nos)-1])};
		return x;
	endmethod 
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
//============================ End of ADD module ===========================

//============================ SUB module ==================================

function Tuple2#(FloatingPoint#(e,m),Exception) fp_sub (Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m),RoundMode) operands)

   provisos(
      // per request of bsc
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

interface Ifc_sub#(numeric type e, numeric type m, numeric type nos);
	method Action send(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode) operands);
	method ReturnType#(e,m) receive();
endinterface
module mk_sub(Ifc_sub#(e,m,nos))
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
					 
					 rg_stage_out[0] <= fp_sub(operands);
					 rg_stage_valid[0] <= 1;

	endmethod

	method ReturnType#(e,m) receive();
		let x = ReturnType{valid:rg_stage_valid[valueOf(nos)-1] ,value:tpl_1(rg_stage_out[valueOf(nos)-1]) ,ex:tpl_2(rg_stage_out[valueOf(nos)-1])};
		return x;

	endmethod 
endmodule


interface Ifc_sub_s4;
    method Action put(Bit#(2) op, Float copy_x, Maybe#(Float) add_res);
    method ActionValue#(Maybe#(Float)) sub_result; 
    method Float copy_x; 
	method Bit#(2) copy_op;
endinterface

// (*synthesize*)
module mksub_s4(Ifc_sub_s4); 
	Reg#(Bit#(2)) op1 <- mkReg(0), op2 <- mkReg(0), op3 <- mkReg(0), op4 <- mkReg(0);

	Reg#(Maybe#(Float)) x1 <- mkReg(tagged Invalid), x2 <- mkReg(tagged Invalid), x3 <- mkReg(tagged Invalid), x4 <- mkReg(tagged Invalid);
	
    Ifc_sub#(8,23,4) sub <- mk_sub;

	Reg#(Maybe#(Float)) temp_x1 <- mkDWire(tagged Invalid);

    rule sub2;
		op2 <= op1;
		x2 <= x1;
    endrule 

    rule sub3;
		op3 <= op2;
		x3 <= x2;
    endrule

    rule sub4;
		op4 <= op3;
		x4 <= x3;
    endrule

	rule pass_inp;
		x1 <= temp_x1;
	endrule

	method Action put(Bit#(2) op, Float copy_x, Maybe#(Float) add_res);
		op1 <= op;
		if (isValid(add_res)) begin 
			if (op[1] == 0)
				temp_x1 <= add_res;
			else
				temp_x1 <= tagged Valid(copy_x);
		end

		RoundMode rm = Rnd_Nearest_Even;
        sub.send(tuple3(add_res.Valid, -2, rm));
	endmethod 

    method ActionValue#(Maybe#(Float)) sub_result; 
		let r = sub.receive();
		if (isValid(x4))
			return tagged Valid(r.value);
		else 
			return tagged Invalid;
    endmethod 
	
	method Float copy_x = x4.Valid; 
	method Bit#(2) copy_op = op4;
endmodule 
//============================= End of SUB module ============================

//================================ DIV module ================================

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

// ============================= End of DIV module =============================

// ============================== EXP module ===================================

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


//============================== End of exp.bsv =========================

//============================== factor.bsv =============================

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

//=========================== End of factor.bsv =========================
//============================== comp.bsv ===============================

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
//============================ End of comp.bsv ===========================

//=============================== main.bsv ===============================
interface Ifc_main;
    method Action put(Bit#(2) op, Float x);
    method Maybe#(Float) main_result;
endinterface

(*synthesize*)
module mk_main(Ifc_main);
    Reg#(Maybe#(Float)) input_x <- mkReg(tagged Invalid);
	Reg#(Bit#(2)) opcode <- mkReg(0);

	PulseWire enable_stage_1 <- mkPulseWire;

    Ifc_factor_s1 factor_s1 <- mkfactor_s1;
    Ifc_exp exp_s2 <- mkExp;
    Ifc_add_s3 add_s3 <- mkadd_s3;
    Ifc_sub_s4 sub_s4 <- mksub_s4;
    Ifc_div_op div_s5 <- mk_div_op;
	Ifc_comp_s6 comp_s6 <- mk_comp_s6;
    
	// STAGE 1 - Factor
    rule stage_1;
		if (isValid(input_x))
			//$display($time, " stage1 - %b, %b", opcode, input_x.Valid);
        factor_s1.put(opcode, input_x); // get is changed to put
    endrule
    
	// STAGE 2
    rule stage_2;
        let factor_res = factor_s1.factor_result;
		let op = factor_s1.copy_op;
		exp_s2.put(op, factor_res);
		if (isValid(factor_res)) begin
			//$display($time, " stage2 - %b, %b", factor_res.Valid, op);
		end
    endrule 
    
	// STAGE 3
    rule stage_3;
        let exp_res = exp_s2.get;
        let x2 = exp_s2.copy_x;
		let op = exp_s2.copy_op;
		if (isValid(exp_res)) begin
			//$display($time, " stage3 - %b, %b %b", exp_res.Valid, x2, op);

		end

		add_s3.put(op, x2, exp_res);
    endrule 
    
	// STAGE 4
    rule stage_4;
        let addres <- add_s3.add_result;
        let x = add_s3.copy_x;
		let op = add_s3.copy_op;
		if (isValid(addres)) begin
			//$display($time, " stage4 - %b, %b %b", addres.Valid, x, op);
		end

		sub_s4.put(op, x, addres);
    endrule 
    
	// STAGE 5
    rule stage_5;
        let sub_res <- sub_s4.sub_result;
        let x = sub_s4.copy_x; // Factor_x or add_res
        let op = sub_s4.copy_op;
		if (isValid(sub_res)) begin
			//$display($time, " stage5 - %b, %b %b", sub_res.Valid, x, op);
		end

		Float input_1 = 0;

		if (op == 2'b10)
			input_1 = 1;
		else 
			input_1 = sub_res.Valid;

		div_s5.put(input_1, x, isValid(sub_res), op);
    endrule 
    
	// STAGE 6
    rule stage_6;
        let div_res = div_s5.get; // will be sub for op 10, 11
        let op = div_s5.copy_op;
        let x = div_s5.copy_x; // will be factorx for op 10, 11
		if (isValid(div_res)) begin
			//$display($time, " stage6 - %b, %b %b", div_res.Valid, x, op);
		end

        comp_s6.put(op, x, div_res);
    endrule 

	rule disable_inp (!enable_stage_1);
        input_x <= tagged Invalid;
    endrule

    method Action put(Bit#(2) op, Float x);
		//$display($time, " get - %b, %b", op, x);
        input_x <= tagged Valid(x);
        opcode <= op;
		enable_stage_1.send();
    endmethod

    method Maybe#(Float) main_result = comp_s6.comp_result;
endmodule 
//=========================== testbench ================================
(*synthesize*)
module mkTb();
    Reg#(int) status <- mkReg(0);
    Ifc_main ifc_main <- mk_main;

    rule stage;
        status <= status + 1;
    endrule 

    rule get_input1(status == 0);
        ifc_main.put(2'b00, 2.5);
    endrule
    rule get_input2(status == 1);
        ifc_main.put(2'b11, -0.5);
    endrule
	rule get_input3(status == 2);
	    ifc_main.put(2'b01, 8);
    endrule
	rule get_input4(status == 3);
	    ifc_main.put(2'b10, -2.8);
    endrule
	// rule get_input5(status == 4);
	//     ifc_main.put(2'b11, 9.9);
    // endrule
	
    rule result;
		let r = ifc_main.main_result;
		if (isValid(r))
			$display($time, " Result = %b : cycle = %d", r.Valid , status );
	endrule

    rule finish(status == 22);
        $finish(0);
    endrule
endmodule: mkTb
