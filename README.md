## Pipelined-Module
### Aim of the project: 
#### To design an area-efficient pipelined module to support unary operators ( activation functions used in deep learning )
The activation functions are simple mathematical functions that convert a given input into a desired output within a specific range. In this project, we used : tanh(x), sigmoid(x), leaky_ReLu(x), SeLu(x).
These functions are evaluated using several pipelining stages, each stage supports a unary operation like addition, subtraction, division, etc.
#### 1) tanh(x) 
> tanh(x) uses exponential, addition, subtraction, and division operations. In the first expression, we will have to calculate $e^x$ and  $(e^-)^x$ separately, but when we simplify it to the second expression we will just use the exponential stage once $e^(2x)$. In this case, I need one more stage to factor x to 2x.
> 
![](Git_Images/tanh(x).png)

#### 2) sigmoid(x)
> sigmoid (x) uses exponential, addition, and division operations. It also uses a stage to factor x to -x. 
>
![](Git_Images/sigmoid(x).png)

#### 3) leaky_ReLu(x) 
> LReLu(x) uses a stage to compare value of x with 0. Here α is the learning rate, and after referring to sources from the internet we took its value as 1.67.
>
![](Git_Images/LReLu.png)
#### 4) SeLu(x)
> Just like LReLu(x), SeLu(x) also uses a stage to compare value of x with 0. Here λ is the scale, and its value is taken as 1.05.
> 
![](Git_Images/SeLu.png)
### Pipeline stages  
>  
After simplifying the expressions of the activation functions such that we use a minimum number of pipeline stages, we got **6** stages. The stages are Factor, Exponential, Addition, Subtraction, Division and Comparison. We made the order of the stages such that we can reuse the previous stages' output as input for the upcoming stages. For example, in the tanh(x) function value of $e^2x$ which is evaluated in the second stage is given as inputs to the addition and subtraction stages.
>   
![](Git_Images/stages.png)
>
As shown in the above table, some functions don't use several stages. The data from the previous stage just passes through such stages without any change. For example, in the *LReLu* function the input just passes through the first five stages and only the comparison stage is used.
>
### Design 
>
The figure shown below is the overall pipelined module.
>
![Pipline Stages ](Git_Images/pipeline.png)
>
*x* represents the given input. Here OP represents the function (which is also given as an input along with x), which enables the pipeline stages based on the function used. 
   * 00: tanh(x)
   * 01: sigmoid(x)
   * 10: LReLu(x)
   * 11: SeLu(x)
>
### Implementation of the pipeline stages using Bluespec 
Each of the six stages represents a specific operation, such as addition, subtraction, and division. In Bluespec, modules are used to encapsulate these stages of the design, providing a structured and modular approach to hardware description. These modules are used as submodules in the top module which represents the overall pipelined design.
>
The folders FAC, ADD, SUB, DIV and COMP contain the modules. For example, the ADD file has *add_s3.bsv* and *add.bsv*. As the datatype we use is **Floating point**, we have made separate modules for the unary operations used to do bit-by-bit operations.
>
FloatingPoint library in Bluespec and Shakti's Fbox were used as a reference for making the addition and subtraction modules represented by *add.bsv* and *sub.bsv* files respectively.
>
### STAGE 1 - Factor 
>
The module named *mkfactor_s1* performs factorization operations on a floating-point number x based on an operation code (opcode)
>
Factorization is performed based on the rules and opcode:
* tanh(x): Doubles the input value.
* sigmoid (x): Negates the input value.
* LReLu(x) or SeLu(x): Leaves the input value unchanged.
>
1. opcode :
   * The opcode is a 2-bit value that determines the type of factorization operation to be performed.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_factor_s1 interface
   *  method Action put(Bit#(2) op, Maybe#(Float) x): Initiates the factorization process by providing an opcode (op) and a floating-point number (x).
   * method Maybe#(Float) factor_result: Retrieves the result of the factorization.
   * method Bit#(2) copy_op: This is used to pass the opcode to the next stage.
3. Registers and Data Flow:
   * The module uses registers (input_x, factor_res, opcode) to manage the data flow and store intermediate results.
   * The Maybe# type is used to check the validity of a result.
4. Method:
   * The method defines various conditions, each corresponding to a different factorization operation based on the opcode:
   * tanh(x): Doubles the input value (factor_res <= 2 * input_x) if op = 2'b00
   * sigmoid(x): Negates the input value (factor_res <= -1 * input_x) if op = 2'b01
   * LReLu(x) or SeLu(x): Leaves the input value unchanged (factor_res <= 1 * input_x) if op = 2'b10 or 2'b11
>
### STAGE 2 - Exp
1. opcode :
   * 00, 01, or 11: Invoke the exponentiation operation using the mkExp module. Otherwise, directly passes through the input value without exponentiation.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_exp_s2 interface
   * method Action put(Bit#(2) opcode, Maybe#(Float) x): Initiates the exponentiation process by providing an opcode (op) and factor_res (x).
   * method Maybe#(Float) get: Gets the result of the exponentiation operation.
   * method Float copy_x: Used to pass factor_res to the next stage.
   * method Bit#(2) copy_op: This is used to pass the opcode to the next stage.
3. Logic of the Exp module (mkExp):
   * Stage 1: Checks if the input value is small; if so, directly passes through the value. Otherwise, performs factorization based on the magnitude. For example, if x is 0.7, it is passed on to the next stage.
   * Stage 2: Similar to Stage 1 but includes additional terms for larger values. For example, if the value of x is 3.2, then it factorizes the calculation of $e^(3.2)$ into $e^2$ and $e^(1.2)$ based on the exponent term in floating point representation. Then $e^1.2$ is factorised to $e^1$ and $e^0.2$. Hence $e^3$ is calaculated using predefined values of $e^2$ and $e^1$. Now, 0.2 is passed onto the stages which use *Taylor series* terms to calculate $e^(0.2)$ (We have used the first 5 terms of the Taylor series of $e^x$).
   * Stages 3-7: Computes successive terms of the Taylor series.
3. Registers and Data Flow:
   * Registers and wires (x_wire, op_wire, stage0_x, stage1_x, etc.) manage the flow of data between different stages.
   * Reg#(Bool) type registers are used to check the validity of every stage.
   * The result is tagged valid when s6_valid becomes True.
4. Rules:
   * tanh(x), sigmoid(x) and SeLu(x): Calculates the exponential value (op = 2'b00, 2'b01, 2'b11)
   * SeLu(x): Leaves the input value unchanged (op = 2'b10)

### STAGE 3 - Add
1. opcode :
   * 00, 01: Invoke the addition operation using the mk_add module. Otherwise, directly passes through the input value without addition.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_add_s3 interface
   * method Action put(Bit#(2) op, Float copy_xin, Maybe#(Float) x): Takes an opcode (op), factor_res, and exp_res as input and sets the input and opcode register according to the opcode condition.
   *method ActionValue#(Maybe#(Float)) add_result: Returns the result of the addition operation performed in the add submodule.
   * method Float copy_x: Returns the value of factor_res to pass to the next stage.
   * method Bit#(2) copy_op:  This is used to pass the opcode to the next stage.
3. Registers and Data Flow:
3. Registers and Data Flow:
   * Registers (op1, op2,..op4, x1,x2,..x4) are used to manage the flow of data between different stages.
   * Reg#(Bool) type add1_valid,..,add4_valid: Flags indicating the validity of the stages of addition operation.
4. Rules:
   * tanh(x), sigmoid(x): Undergoes addition operation (op = 2'b00, 2'b01)
   * LReLu(x) or SeLu(x): Leaves the input value unchanged (op = 2'b10, 2'b11);

### STAGE 4 - Sub
1. opcode :
   * 00, 11: Invoke the subtraction operation using the mk_sub module. Otherwise, directly passes through the input value without subtraction.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_sub_s4 interface
   * method Action put(Bit#(2) op, Float copy_x, Maybe#(Float) add_res): Takes an opcode (op), factor_res and add_res as input and sets the input and opcode register accordingly.
   * method ActionValue#(Maybe#(Float)) sub_result: Returns the result of the subtraction operation.
   * method Float copy_x: Returns the value of factor_res to pass to the next stage.
   * method Bit#(2) copy_op: This is used to pass the opcode to the next stage.
3. Registers and Data Flow:
   * Registers (op1, op2,..op4, x1,x2,..x4) are used to manage the flow of data between different stages.
   * Reg#(Bool) type sub1_valid,..,sub4_valid: Flags indicating the validity of the stages of subtraction operation.
4. Rules:
   * If op = 2'b00 or 2'b01, then add_res is used. If op = 2'b10 or 2'b11, then factor_res is used.
   * tanh(x), SeLu(x): Undergoes subtraction operation.
   * sigmoid(x) or LReLu(x): Leaves the input value unchanged.

### STAGE 5 - Div
1. opcode :
   * 00, 01: Invoke the division operation using the mk_div_op module. Otherwise, directly passes through the input value without division.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_div_s5 interface
   * method Action put(Float a, Float b, Bool c, Bit#(2) op): Takes an opcode, input_1( 1 if op = 2'b01, else sub_res),and factor_res as inputs for either division or pass-through operation.
   * method Maybe#(Float) get: Returns the result of the division operation.
   * method Bit#(2) copy_x: Returns the value of factor_res to pass to the next stage.
   * method Bit#(2) copy_op: This is used to pass the opcode to the next stage.
3. Registers and Data Flow:
   * Registers (op, x) manage the flow of data between different stages.
4. Rules:
   * tanh(x), sigmoid(x): Undergoes division operation (op = 2'b00, 2b'01)
   * LReLu(x) or SeLu(x): Leaves the input value unchanged (op = 2'b10, 2'b11);

### STAGE 6 - Comp
1. opcode :
   * 10, 11: Invoke the division operation using the mk_comp_s6 module. Otherwise, directly passes through the input value without division.
2. Interface Methods:
   * The module implements the methods specified in the Ifc_comp_s6 interface
   * method Action put(Bit#(2) op, Float x1, Maybe#(Float) div_res): Takes an opcode and three floating-point numbers (v, x, and z) .
   * method Maybe#(Float) comp_result: Returns the result of the comparison operation.
3. Registers and Data Flow:
   * Registers (comp_res) manage the flow of data between different stages.
   * λ, and α are constant values stored in registers.
4. Rules:
   * LReLu(x) or SeLu(x): Undergoes comparison operation.
   * tanh(x), sigmoid(x): Leaves the input value unchanged and the result is taken as div_res.
### Overall Latency and Optimisation

### Test cases & Results 
 #### Test case 3 - LReLu(x)
```
module mkTb();
    Reg#(int) status <- mkReg(0);
    Ifc_main ifc_main <- mk_main;

    rule stage;
        status <= status + 1;
    endrule 

    rule get_input1(status == 0);
        ifc_main.put(2'b00, 12.5);
    endrule
    rule get_input2(status == 1);
        ifc_main.put(2'b11, 2.5);
    endrule
    rule get_input3(status == 2);
        ifc_main.put(2'b01, 0);
    endrule
    rule get_input4(status == 3);
        ifc_main.put(2'b10, -2.8);	
    rule result;
         let r = ifc_main.main_result;
         if (isValid(r))
         $display($time, " Result = %b : cycle = %d", r.Valid , status );
    endrule

    rule finish(status == 22);
        $finish(0);
    endrule
endmodule: mkTb

```
Result : 
```
                 195 Result = 00111111100000000000000000000000 : cycle =          19
                 205 Result = 01000000001010000000000000000000 : cycle =          20
                 215 Result = 00000000000000000000000000000000 : cycle =          21
                 225 Result = 11000000100101011010000111001010 : cycle =          22

```

 #### Test case 4 - SeLu(x)
 ```
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
    rule result;
	let r = ifc_main.main_result;
	if (isValid(r))
	$display($time, " Result = %b : cycle = %d", r.Valid , status );
    endrule

    rule finish(status == 22);
        $finish(0);
    endrule
endmodule: mkTb

 ```
Result:
```
                 195 Result = 00111111011111001001001001001011 : cycle =          19
                 205 Result = 10111111001100000100110100001100 : cycle =          20
                 215 Result = 10111111011111111101010000011100 : cycle =          21
                 225 Result = 11000000100101011010000111001010 : cycle =          22
```

### Synthesis 

=== mk_comp_s6 ===

   Number of cells:               4831

=== mk_main ===

   Number of cells:                 88

=== mkadd_s3 ===

   Number of cells:               2140

=== mkdiv_s5 ===

   Number of cells:              67327

=== mkfactor_s1 ===

   Number of cells:               2625

=== mksub_s4 ===

   Number of cells:               2000

=== design hierarchy ===

   Number of cells:              79006
   

### To Run 
Use the command *make* 

### Result 
Hence the module is pipelined and well-optimised with 6 stages

