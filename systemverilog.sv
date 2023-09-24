// Quartus Prime SystemVerilog Template
// 
// Mixed-width RAM with separate read and write addresses and data widths
// that are controlled by the parameters RW and WW.	 RW and WW must specify a
// read/write ratio supported by the memory blocks in your target device.
// Otherwise, Quartus Prime will not infer a RAM.

module mixed_width_ram
	#(parameter int
		WORDS = 256,
		RW = 8,
		WW = 32)
(
	input we, 
	input clk,
	input [$clog2((RW < WW) ? WORDS : (WORDS * RW)/WW) - 1 : 0] waddr, 
	input [WW-1:0] wdata, 
	input [$clog2((RW < WW) ? (WORDS * WW)/RW : WORDS) - 1 : 0] raddr, 
	output logic [RW-1:0] q
);
   
	// Use a multi-dimensional packed array to model the different read/write
	// width
	localparam int R = (RW < WW) ? WW/RW : RW/WW;
	localparam int B = (RW < WW) ? RW: WW;

	logic [R-1:0][B-1:0] ram[0:WORDS-1];

	generate if(RW < WW) begin
		// Smaller read?
		always_ff@(posedge clk)
		begin
			if(we) ram[waddr] <= wdata;
			q <= ram[raddr / R][raddr % R];
		end
	end
	else begin 
		// Smaller write?
		always_ff@(posedge clk)
		begin
			if(we) ram[waddr / R][waddr % R] <= wdata;
			q <= ram[raddr];
		end
	end 
	endgenerate
   
endmodule : mixed_width_ram
end_template
begin_template Mixed-Width True Dual Port RAM
// Quartus Prime SystemVerilog Template
//
// True Dual-Port RAM with single clock and different data width on the two ports and width new data on read during write on same port
//
// The first datawidth and the widths of the addresses are specified
// The second data width is equal to DATA_WIDTH1 * RATIO, where RATIO = (1 << (ADDRESS_WIDTH1 - ADDRESS_WIDTH2)
// RATIO must have value that is supported by the memory blocks in your target
// device.  Otherwise, no RAM will be inferred.  
//
// Read-during-write behavior returns old data for mixed ports and the new data on the same port
// This style of RAM cannot be used on Stratix 10, 
// which does not support mixed-width True Dual-Port RAM

module mixed_width_true_dual_port_ram
	#(parameter int
		DATA_WIDTH1 = 8,
		ADDRESS_WIDTH1 = 10,
		ADDRESS_WIDTH2 = 8)
(
		input [ADDRESS_WIDTH1-1:0] addr1,
		input [ADDRESS_WIDTH2-1:0] addr2,
		input [DATA_WIDTH1      -1:0] data_in1, 
		input [DATA_WIDTH1*(1<<(ADDRESS_WIDTH1 - ADDRESS_WIDTH2))-1:0] data_in2, 
		input we1, we2, clk,
		output reg [DATA_WIDTH1-1      :0] data_out1,
		output reg [DATA_WIDTH1*(1<<(ADDRESS_WIDTH1 - ADDRESS_WIDTH2))-1:0] data_out2);
    
	localparam RATIO = 1 << (ADDRESS_WIDTH1 - ADDRESS_WIDTH2); // valid values are 2,4,8... family dependent
	localparam DATA_WIDTH2 = DATA_WIDTH1 * RATIO;
	localparam RAM_DEPTH = 1 << ADDRESS_WIDTH2;

	// Use a multi-dimensional packed array to model the different read/ram width
	reg [RATIO-1:0] [DATA_WIDTH1-1:0] ram[0:RAM_DEPTH-1];
    
	reg [DATA_WIDTH1-1:0] data_reg1;
	reg [DATA_WIDTH2-1:0] data_reg2;

	// Port A
	always@(posedge clk)
	begin
		if(we1) 
		begin 
			ram[addr1 / RATIO][addr1 % RATIO] <= data_in1;
			data_reg1 <= data_in1;
		end
		else
		begin 
			data_reg1 <= ram[addr1 / RATIO][addr1 % RATIO];
		end
	end
	assign data_out1 = data_reg1;

	// port B
	always@(posedge clk)
	begin
		if(we2)
		begin
			ram[addr2] <= data_in2;
			data_reg2 <= data_in2;
		end
		else
		begin
			data_reg2 <= ram[addr2];
		end
	end
    
	assign data_out2 = data_reg2;
endmodule : mixed_width_true_dual_port_ram
end_template
begin_template Byte-enabled Simple Dual Port RAM
// Quartus Prime SystemVerilog Template
//
// Simple Dual-Port RAM with different read/write addresses and single read/write clock
// and with a control for writing single bytes into the memory word

module byte_enabled_simple_dual_port_ram
	#(parameter int
		ADDR_WIDTH = 6,
		BYTE_WIDTH = 8,
		BYTES = 4,
			WIDTH = BYTES * BYTE_WIDTH
)
( 
	input [ADDR_WIDTH-1:0] waddr,
	input [ADDR_WIDTH-1:0] raddr,
	input [BYTES-1:0] be,
	input [WIDTH-1:0] wdata, 
	input we, clk,
	output reg [WIDTH - 1:0] q
);
	localparam int WORDS = 1 << ADDR_WIDTH ;

	// use a multi-dimensional packed array to model individual bytes within the word
	logic [BYTES-1:0][BYTE_WIDTH-1:0] ram[0:WORDS-1];

	always_ff@(posedge clk)
	begin
		if(we) begin
		// edit this code if using other than four bytes per word
			if(be[0]) ram[waddr][0] <= wdata[BYTE_WIDTH-1:0];
			if(be[1]) ram[waddr][1] <= wdata[2*BYTE_WIDTH-1:BYTE_WIDTH];
			if(be[2]) ram[waddr][2] <= wdata[3*BYTE_WIDTH-1:2*BYTE_WIDTH];
			if(be[3]) ram[waddr][3] <= wdata[4*BYTE_WIDTH-1:3*BYTE_WIDTH];
	end
		q <= ram[raddr];
	end
endmodule : byte_enabled_simple_dual_port_ram
end_template
begin_template Byte-enabled True Dual Port RAM
// Quartus Prime SystemVerilog Template
//
// True Dual-Port RAM with single clock
// and individual controls for writing into separate bytes of the memory word (byte-enable)
//
// Read-during-write returns either new or old data depending
// on the order in which the simulator executes the process statements.
// Quartus Prime will consider this read-during-write scenario as a 
// don't care condition to optimize the performance of the RAM.  If you
// need a read-during-write behavior to be determined, you
// must instantiate the altsyncram Megafunction directly.

module byte_enabled_true_dual_port_ram
	#(
		parameter int
		BYTE_WIDTH = 8,
		ADDRESS_WIDTH = 6,
		BYTES = 4,
		DATA_WIDTH = BYTE_WIDTH * BYTES
)
(
	input [ADDRESS_WIDTH-1:0] addr1,
	input [ADDRESS_WIDTH-1:0] addr2,
	input [BYTES-1:0] be1,
	input [BYTES-1:0] be2,
	input [DATA_WIDTH-1:0] data_in1, 
	input [DATA_WIDTH-1:0] data_in2, 
	input we1, we2, clk,
	output [DATA_WIDTH-1:0] data_out1,
	output [DATA_WIDTH-1:0] data_out2);
	localparam RAM_DEPTH = 1 << ADDRESS_WIDTH;

	// model the RAM with two dimensional packed array
	logic [BYTES-1:0][BYTE_WIDTH-1:0] ram[0:RAM_DEPTH-1];

	reg [DATA_WIDTH-1:0] data_reg1;
	reg [DATA_WIDTH-1:0] data_reg2;


	// port A
	always@(posedge clk)
	begin
		if(we1) begin
		// edit this code if using other than four bytes per word
			if(be1[0]) ram[addr1][0] = data_in1[BYTE_WIDTH-1:0];
			if(be1[1]) ram[addr1][1] = data_in1[2*BYTE_WIDTH-1:BYTE_WIDTH];
			if(be1[2]) ram[addr1][2] = data_in1[3*BYTE_WIDTH-1:2*BYTE_WIDTH];
			if(be1[3]) ram[addr1][3] = data_in1[4*BYTE_WIDTH-1:3*BYTE_WIDTH];
		end
	end

	always@(posedge clk)
	begin
		data_reg1 <= ram[addr1];
	end

	assign data_out1 = data_reg1;
   
	// port B
	always@(posedge clk)
	begin
		if(we2) begin
		// edit this code if using other than four bytes per word
			if(be2[0]) ram[addr2][0] = data_in2[BYTE_WIDTH-1:0];
			if(be2[1]) ram[addr2][1] = data_in2[2*BYTE_WIDTH-1:BYTE_WIDTH];
			if(be2[2]) ram[addr2][2] = data_in2[3*BYTE_WIDTH-1:2*BYTE_WIDTH];
			if(be2[3]) ram[addr2][3] = data_in2[4*BYTE_WIDTH-1:3*BYTE_WIDTH];
		end
	end

	always@(posedge clk)
	begin
		data_reg2 <= ram[addr2];
	end

	assign data_out2 = data_reg2;

endmodule : byte_enabled_true_dual_port_ram
end_template
end_group
begin_group Pipelining
begin_template Hyper-Pipelining Module
// Quartus Prime SystemVerilog Template
//
// Hyper-Pipelining Module

(* altera_attribute = "-name AUTO_SHIFT_REGISTER_RECOGNITION off" *) 
module hyperpipe 
	#(parameter int
		CYCLES = 1,
		PACKED_WIDTH = 1,
		UNPACKED_WIDTH = 1
) 
(
	input clk,
	input [PACKED_WIDTH-1:0] din [UNPACKED_WIDTH-1:0],
	output [PACKED_WIDTH-1:0] dout [UNPACKED_WIDTH-1:0]
);

	generate if (CYCLES == 0) begin : GEN_COMB_INPUT
		assign dout = din;
	end
	else begin : GEN_REG_INPUT
		integer i;
		reg [PACKED_WIDTH-1:0] R_data [CYCLES-1:0][UNPACKED_WIDTH-1:0];
          
		always_ff@(posedge clk) 
		begin
			R_data[0] <= din;
			for(i = 1; i < CYCLES; i = i + 1)
				R_data[i] <= R_data[i-1];
		end
		assign dout = R_data[CYCLES-1];
	end
	endgenerate

endmodule : hyperpipe
end_template
begin_template Hyper-Pipelining Module Instantiation
// Quartus Prime SystemVerilog Template
//
// Hyper-Pipelining Module Instantiation

hyperpipe # (
		.CYCLES         ( ),
		.PACKED_WIDTH   ( ),
		.UNPACKED_WIDTH ( )
	) hp (
		.clk      ( ),
		.din      ( ),
		.dout     ( )
	);
end_template
begin_template Hyper-Pipelining Variable Latency Module
// Quartus Prime SystemVerilog Template
//
// Hyper-Pipelining Variable Latency Module

module hyperpipe_vlat 
	#(parameter int
		WIDTH = 1,
		MAX_PIPE = 100 // Valid range: 0 to 100 inclusive
)
(
	input clk,
	input  [WIDTH-1:0] din,
	output [WIDTH-1:0] dout
);

	// Capping the value of MAX_PIPE to 100 because MAX_PIPE > 100 could cause errors
	localparam MAX_PIPE_CAPPED = (MAX_PIPE > 100) ? 100 : ((MAX_PIPE < 0) ? 0 : MAX_PIPE);

	// Converting MAX_PIPE_CAPPED to string so it can be used as a string when setting altera_attribute
	localparam MAX_PIPE_STR = {((MAX_PIPE_CAPPED / 100) % 10) + 8'd48, ((MAX_PIPE_CAPPED / 10) % 10) + 8'd48, (MAX_PIPE_CAPPED % 10) + 8'd48};

	(* altera_attribute = {"-name ADV_NETLIST_OPT_ALLOWED NEVER_ALLOW; -name HYPER_RETIMER_ADD_PIPELINING ", MAX_PIPE_STR} *)
	logic [WIDTH-1:0] vlat_r /* synthesis preserve */;

	always_ff@(posedge clk) 
	begin
		vlat_r <= din;
	end

	assign dout = vlat_r;

endmodule : hyperpipe_vlat
end_template
begin_template Hyper-Pipelining Variable Latency Instantiation
// Quartus Prime SystemVerilog Template
//
// Hyper-Pipelining Variable Latency Module
//
hyperpipe_vlat # (
		.WIDTH    ( ),
		.MAX_PIPE ( ) // Valid range: 0 to 100 inclusive
	) hp (
		.clk      ( ),
		.din      ( ),
		.dout     ( )
	);
end_template
end_group
end_group
begin_group Constructs
begin_group Design Units
begin_template Package Declaration
package <name>;

	// Parameter Declaration(s)
	// Function Declaration(s)
	// Task Declarations(s)
	// Type Declaration(s)

endpackage	

// You can refer to specific objects in a package by name
<package_name>::<object_name>

// You can also import objects from a package into the current scope
// using an import declaration
import <package_name>::<object_name>;
import <package_name>::*;         
end_template
begin_group Interfaces
begin_template Master Slave Example
// Full example with a top module instantiating and interface and a master and slave submodules
// The master and slave submodules use different modports of the same interface

interface my_interface(input clk);
parameter DATA_WIDTH = 1;
reg [DATA_WIDTH-1:0] data;
reg 			read;
   
modport master(output read, input data, input clk);
modport slave(input read, output data, input clk);
endinterface

// The slave accepts generic interface. The instantiating module will pass the .slave modport
module slave_sub #(parameter DATA_WIDTH = 1)
(interface i, input [DATA_WIDTH-1:0] data);

always@(posedge i.clk) 
	begin
		if(i.read) i.data <= data;
	end
endmodule

// The master accepts my_interface.master. The instantiating module can just pass the interface
module master_sub #(parameter DATA_WIDTH = 1)
(my_interface.master i, input sel, output [DATA_WIDTH-1:0] data);

always@(posedge i.clk) 
	begin
		i.read <= sel;
	end
assign data = i.data;
endmodule

module master_slave(input clk, input sel, output [3:0] data_o, input [3:0] data_i);
my_interface #(4) i(.clk(clk));
   
master_sub #(4) master_inst(i, sel, data_o);
slave_sub  #(4) slave_inst(i.slave, data_i);
endmodule // master_slave
end_template
begin_template Interface Declaration
interface <interface_name>(<port_list>);

	// Variable Declaration(s)
	// Modport Declaration(s)
	// Function/Task Declaration(s)
	// Always Construct(s)
	// Generate(s)

	// An interface cannot instantiate a module but it may
	// instantiate an interface

endinterface
end_template
begin_template Modport Declaration
// A modport limits the access to the objects in an interface.  Each port
// must correspond to an object in the enclosing interface.  
modport <modport_name>(<port_list>);

// Examples
modport slave(input clk, input sel, output byte data);
modport master(input clk, output sel, input byte data);
end_template
begin_template Interface Port Declarations
// Both modules and interfaces may declare interface ports.  

// An interface port may refer to a specific interface and/or modport
<interface_name> <port_name>
<interface_name>.<modport_name> <port_name>

// A generic interface port will be bound to a specific interface
// and/or modport during instantiation.  As a result, the top-level
// module in your design cannot contain a generic interface port.
interface <port_name>
end_template
begin_template Interface Instantiations
// Basic interface instantiation
<interface_name> <inst_name>(<port_connects>);

// Interface instantiation with parameter overrides
<interface_name> #(<parameters>) <inst_name>(<port_connects>);

// Array of instances
<interface_name> #(<parameters) <inst_name>  [<msb>:<lsb>] (<port_conects>);
end_template
end_group
begin_template Extern Module Declaration
// An extern module declaration specifies the module's parameters
// and ports.  It provides a prototype for a module that does not
// depend directly on the module declaration.  
extern module <module_name> #(<parameters>) (<ports>);
end_template
end_group
begin_group State Machines
begin_template State Machine With Enumerated Types
// SystemVerilog state machine implementation that uses enumerated types.
// Altera recommends using this coding style to describe state machines in SystemVerilog.
// In Quartus Prime integrated synthesis, the enumerated type
// that defines the states for the state machine must be
// of an unsigned integer type. If you do not specify the
// enumerated type as int unsigned, a signed int type is used by default.
// In this case, the Quartus Prime integrated synthesis synthesizes the design, but
// does not infer or optimize the logic as a state machine.
//
module enum_fsm (input clk, reset, input int data[3:0], output int o);
enum int unsigned { S0 = 0, S1 = 2, S2 = 4, S3 = 8 } state, next_state;
always_comb begin : next_state_logic
next_state = S0;
case(state)
S0: next_state = S1;
S1: next_state = S2;
S2: next_state = S3;
S3: next_state = S3;
endcase
end
always_comb begin
case(state)
S0: o = data[3];
S1: o = data[2];
S2: o = data[1];
S3: o = data[0];
endcase
end
always_ff@(posedge clk or negedge reset) begin
if(~reset)
state <= S0;
else
state <= next_state;
end
endmodule
end_template
end_group
begin_group Declarations
begin_group Type Declarations
begin_template Integer Type Declaration
// Built-in integer vector types.  By default, these types
// are signed.  You can explicitly declare them as unsigned or
// signed by adding the appropriate keyword after the type.
// These types have implicit bounds of [Size - 1 : 0].  2-state
// types support the bit values of {0,1}. 4-state types support
// {0,1,X,Z}.  If you assign a value of X or Z to a 2-state
// type, the value will be treated as 0.

Type            Size         States

integer         32             4
int             32             2
shortint        16             2
longint         64             2
byte             8             2

// Examples
int an_int = -37;
int unsigned an_unsigned_int = 37;
byte packet;

// Built-in integer atom types.  You can use them to declare 
// signed or unsigned vectors of arbitrary size.  By default,
// these types are unsigned unless you explicitly declare them
// as signed.

Type            Size        States

reg              1             4
logic            1             4
bit              1             2

// Examples

logic signed [31:0] an_integer_variable;
bit signed [31:0] an_int_variable;
bit [31:0] an_unsigned_int_variable;
end_template
begin_template Struct Type Declaration
typedef struct { 
	// Member Declaration(s)
} <type_name>;

struct { 
	// Member Declaration(s)
} <variable_name>;

// Packed structs can be treated as vectors with the range
// [N-1:0], where N is the total number of bits in the type.
// A packed struct can contain struct members, but they must
// also be packed.
struct packed {
	// Member Declaration(s)
} <variable_name>;

// Examples
typedef struct { int x, y, z; } coordinate_t;
struct { int x, y, z; } coordinate = '{ -1, -1, -1 };

// packed_coordinate[31:0]  == packed_coordinate.z
// packed_coordinate[63:32] == packed_coordinate.y
// packed_coordinate[95:64] == packed_coordinate.x
struct packed { int x, y, z; } packed_coordinate;

// A packed array of packed structure can also be declared
typedef struct packed{
bit  [1:0] param1;
bit        param5;
} my_struct;

// unpacked array can be declared of either packed or unpacked structure
my_struct upast [1:0]; 
// for packed array the structure must be packed
my_struct [1:0] b;
end_template
begin_template Array Type Declaration
// Multiple packed, unpacked or mixed type array dimensions can be declared
// including in module ports
module arrays(output logic [7:0] out[0:1], input byte in[0:7]);

// multiple packed dimensions
reg [7:0][7:0] a;

// multiple packed and unpacked dimensions
reg [7:0][7:0] b[1:0][1:0];

assign a[0] = in[0];
assign out [1] = b[1][0] [7] ;

endmodule // arrays
end_template
begin_template String Type Declaration

// String constants are supported and can be returned via packed array from function
// They can be used also to override parameters in module instantiation
module t0(output o);
function [8*8-1:0] f(input dummy);
integer i;
f = "Hi";
f[7:0] = 8'd73;
endfunction

initial begin
("f() == %s", f(1'b1));
end

sub #(f(1'b1)) inst(o);

endmodule // t0

module sub(output o);
parameter P = "Bye";
initial begin
("P == %s", P);
end
assign o = 1'b1;
endmodule
end_template
begin_template Enum Type Declaration
typedef enum <optional_base_type> {
	// Enum Literals
} <type_name>;

enum <optional_base_type> { 
	// Enum Literals
} <variable_name>;

// You can declare a single enum literal or multiple enum literals
// with a single construct.  If you don't assign an enum literal a 
// value, it is automatically assigned a value by incrementing the
// previous enum literal's value.  If the first literal has no
// explicit value, it is assigned the value 0.   

// Declare a single enum literal
<literal_name>
<literal_name> = <constant_expression>

// Declare multiple enums
<literal_name>[<num>]
<literal_name>[<num>] = <constant_expression>
<literal_name>[<begin>:<end>]
<literal_name>[<begin>:<end>] = <constant_expression>

// Examples

// Implicit base type (int)
// A == 0, B0 == 1, B1 == 2, C3 == 3, C4 == 4
typedef enum { A, B[2], C[3:4] } enum_t;  

// Explicit base type
typedef enum bit [3:0] { X = 4'ha, Y = 4'hb, Z = 4'hc } enum_t;

// Quartus Prime will infer state machines from enum types when possible
// but the type must be unsigned.  The implicit base type for 
// an enum is 'int', a signed type.
typedef enum int unsigned { S0, S1, S2, S3, S4 } state_t;
end_template
end_group
begin_template Function Declaration
// A function must execute in a single simulation cycle; therefore, it 
// cannot contain timing controls or tasks.  You set the return value of a 
// function by assigning to the function name as if it were a variable or
// by using the return statement. SystemVerilog allows you to specify default 
// values for function arguments.  In addition, functions may contain input 
// and inout arguments, and the return type of a function may be void.

function <func_return_type> <func_name>(<arg_decls>);
	// Optional Block Declarations
	// Statements
endfunction

// Examples

function int add(int a, int b = 1);
	return a + b;
endfunction
end_template
begin_template Task Declaration
// A task may have input, output, and inout arguments.  It may also
// contain timing controls.  A task does not return a value and, thus, 
// may not be used in an expression.  SystemVerilog allows you to
// specify default values for task arguments.  

task <task_name>(<arg_decls>);
	// Optional Block Declarations
	// Statements
endtask
end_template
begin_template Package Import Declaration
// Import declaration(s) from a package into the current scope.  You can
// import a specific object or all objects.  
import <package_identifier>::<object_name>;
import <package_identifier>::*;
end_template
end_group
begin_group Module Items
begin_template always_comb Construct
// This construct should be used to infer purely combinational logic.
always_comb
begin
	// Statements
end
end_template
begin_template always_latch Construct
// This construct should be used to infer latched logic.  
always_latch
begin
	// Statements
end
end_template
begin_template always_ff Construct
// This construct should be used to infer sequential logic such as
// registers and state machines.
always_ff@(<edge_events>)
begin
	// Statements
end
end_template
end_group
begin_group Sequential Statements
begin_group Loops
begin_template For Loop
for(<for_init>; <expression>; <for_step>)
begin
	// Statements
end	

// <for_init> may set the value for multiple variables.
// Likewise, <for_step> may update the value of multiple variables.

for(i = 0, i2 = 0; i < 8; i++, i2 *= 8)
begin
	// Statements
end
end_template
begin_template Do...While Loop
do 
begin
	// Statements
end 
while(<expression>);
end_template
end_group
begin_group Jump Statements
begin_template Return Statement
return <expression>;
end_template
begin_template Break Statement
break;
end_template
begin_template Continue Statement
continue;
end_template
end_group
end_group
begin_group Expressions
begin_template Assignment Operators
// SystemVerilog supports the assignment operators

// arithmetic
	++ 
	-- 
	+=
	-=
	*=
	/=
	%=

// logical
	&=
	^=
	|=
	<<=
	>>=
	<<<=
	>>>=
end_template
begin_template Assignment Patterns
// Assignment patterns are used to construct values for unpacked
// arrays and structs.  They resemble concatenations except that
// the opening brace is preceded by an apostrophe.

int array_of_ints[1:0] = '{1, 1};
struct { int a, b; } a_struct = '{1, 1};
end_template
begin_template Casting
module casting(output logic o);   
typedef enum { APPLE, ORANGE, LEMON } my_fruit;
my_fruit fruit;
     
// casting to change the size of a self-determined expression 
// no truncation warning
assign o = 1'(3'b001) ;
   
// casting is required to assign an integral value to an object with an enum typ
assign fruit = my_fruit'(0);
endmodule // casting
end_template
begin_template Enum Methods
// Return the first enum literal in the enum data type returned by 
// the expression
<expression>.first()

// Return the last enum literal in the enum data type returned by 
// the expression
<expression>.last()

// Return the enum literal that follows the enum literal returned by
// the expression
<expression>.next()

// Return the enum literal that precedes the enum literal returned by
// the expression.
<expression>.prev()

// Return the number of enum literals in the enum data type returned
// by the expression
<expression>.num()

// Examples
enum { A, B, C, D, E } enum_object = C;

enum_object.first() == A
enum_object.last() == E
enum_object.next() == D
enum_object.prev() == B
enum_object.num() == 5
end_template
begin_template Array Reduction Methods
module reduction(output byte  and_out, xor_out, or_out, sout, pout,
			input byte in[3:0]);
assign and_out = in.and;
assign xor_out = in.xor;
assign  or_out = in.or;
assign sout = in.sum;
assign pout = in.product;
endmodule 
end_template
begin_group Array Querying
begin_template Array Querying
// Return the number of packed + unpacked dimensions in an array object
// or data type
$dimensions(<array_object_or_data_type>)

// Return the number of unpacked dimensions in an array object or data type.
$unpacked_dimensions(<array_object_or_data_type>)

// Return the specific bounds of an array object or data type.
$left(<array_object_or_data_type>, <dimension>)
$right(<array_object_or_data_type>, <dimension>)
$low(<array_object_or_data_type>, <dimension>)
$high(<array_object_or_data_type>, <dimension>)

// Return the number of elements in an array object or data type
$size(<array_object_or_data_type>, <dimension>)

// Return 1 if $left >= $right; otherwise, return -1.
$increment(<array_object_or_data_type>, <dimension>)
end_template
begin_template Expression Size
// Return the number of bits in a data type or expression
$bits(<expression_or_data_type)
end_template
end_group
end_group
begin_group Compiler Directives
begin_template `define
`define <name> <macro_text>
`define <name>(<args>) <macro_text>

// SystemVerilog supports three special strings in the macro text.  The
// first two strings allow you to construct string literals from macro
// arguments.  The third string allows you to construct identifiers
// from macro arguments.
// 
//     `"            -->          Include " character in macro expansion
//     `\`"          -->          Include \" in macro expansion
//     ``            -->          Delimits without introducing white space

// Example(s)
`define msg(type, text)   `"type: `\`"text`\`"`"
`msg(warning, undefined macro) returns "warning: \"undefined macro\""

`define make_name(prefix, base, suffix) prefix``base``suffix
`make_name(altera_, tmp, _variable) returns altera_tmp_variable
end_template
end_group
end_group
begin_group Intel Parameterizable Macros
begin_template simple_dual_port_ram
	//Quartus Prime Parameterizable Macro Template
	//Simple Dual Port RAM
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/simple_dual_port_ram.v	
	
	simple_dual_port_ram #(
									.IN_CLOCK_EN_A                           ("NORMAL"),
									.IN_CLOCK_EN_B                           ("NORMAL"),
									.OUT_CLOCK_EN_B                          ("NORMAL"),
									.DATA_WIDTH_A                            (8),
									.ADDR_WIDTH_A                            (11),
									.BYTE_EN_WIDTH_A                         (1),
									.DATA_WIDTH_B                            (8),
									.ADDR_WIDTH_B                            (11),
									.OUT_DATA_REG_CLK_B                      ("UNREGISTERED"),
									.ADDR_REG_CLK_B                          ("CLOCK0"),
									.OUT_DATA_ACLR_B                         ("NONE"),
									.OUT_DATA_SCLR_B                         ("NONE"),
									.ADDR_ACLR_B                             ("NONE"),
									.READ_DURING_WRITE_MODE_MIXED_PORTS      ("DONT_CARE"),
									.INIT_FILE                               (""),
									.INIT_FILE_LAYOUT                        ("PORT_A"),
									.MAX_DEPTH                               (2048),
									.RDCONTROL_REG_B                         ("CLOCK0"),
									.BYTEENA_REG_B                           ("CLOCK0"),
									.BYTE_SIZE                               (8)
	) <instance_name> (
									.clock0                                  (_connected_to_clock0_),              //input, width = 1
									.clock1                                  (_connected_to_clock1_),              //input, width = 1
									.clocken0                                (_connected_to_clocken0_),            //input, width = 1
									.clocken1                                (_connected_to_clocken1_),            //input, width = 1
									.aclr0                                   (_connected_to_aclr0_),               //input, width = 1
									.aclr1                                   (_connected_to_aclr1_),               //input, width = 1
									.sclr                                    (_connected_to_sclr_),                //input, width = 1
									.data_a                                  (_connected_to_data_a_),              //input, width = DATA_WIDTH_A
									.address_a                               (_connected_to_address_a_),           //input, width = ADDR_WIDTH_A
									.wren_a                                  (_connected_to_wren_a_),              //input, width =1
									.byteena_a                               (_connected_to_byteena_a_),           //input, width = (BYTE_EN_WIDTH_A != 0 ? BYTE_EN_WIDTH_A : 1)
									.address_b                               (_connected_to_address_b_),           //input, width = ADDR_WIDTH_B
									.rden_b                                  (_connected_to_rden_b_),              //input, width = 1
									.addressstall_a                          (_connected_to_addressstall_a_),       //input, width = 1
									.addressstall_b                          (_connected_to_addressstall_b_),       //input, width = 1
									.q_b                                     (_connected_to_q_b_)                  //output, width = DATA_WIDTH_B
	); 
end_template
begin_template true_dual_port_ram
	//Quartus Prime Parameterizable Macro Template
	//True Dual Port RAM
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/true_dual_port_ram.v
	
	true_dual_port_ram #(
									//Port A Parameters
									.DATA_WIDTH_A                            (8),
									.ADDR_WIDTH_A                            (11),
									.BYTE_EN_WIDTH_A                         (1),
									.OUT_DATA_REG_CLK_A                      ("UNREGISTERED"),
									.OUT_DATA_ACLR_A                         ("NONE"),
									.OUT_DATA_SCLR_A                         ("NONE"),
									.READ_DURING_WRITE_MODE_A                ("NEW_DATA_NO_NBE_READ"),
									.IN_CLK_EN_A                             ("NORMAL"),
									.OUT_CLK_EN_A                            ("NORMAL"),
									//Port B Parameters
									.DATA_WIDTH_B                            (8),
									.ADDR_WIDTH_B                            (11),
									.BYTE_EN_WIDTH_B                         (1),
									.OUT_DATA_REG_CLK_B                      ("UNREGISTERED"),
									.OUT_DATA_ACLR_B                         ("NONE"),
									.OUT_DATA_SCLR_B                         ("NONE"),
									.READ_DURING_WRITE_MODE_B                ("NEW_DATA_NO_NBE_READ"),
									.IN_CLK_EN_B                             ("NORMAL"),
									.OUT_CLK_EN_B                            ("NORMAL"),
									//Parameters common for Port A and Port B
									.BYTE_SIZE                               (8),
									.INIT_FILE                               (""),
									.INIT_FILE_LAYOUT                        ("PORT_A"),
									.MAX_DEPTH                               (2048)
	) <instance_name> (
									.clock0                                  (_connected_to_clock0_),              //input, width = 1
									.clock1                                  (_connected_to_clock1_),              //input, width = 1
									.clocken0                                (_connected_to_clocken0_),            //input, width = 1
									.clocken1                                (_connected_to_clocken1_),            //input, width = 1
									.aclr                                    (_connected_to_aclr_),                //input, width = 1
									.sclr                                    (_connected_to_sclr_),                //input, width = 1
									.data_a                                  (_connected_to_data_a_),              //input, width = DATA_WIDTH_A
									.address_a                               (_connected_to_address_a_),           //input, width = ADDR_WIDTH_A
									.wren_a                                  (_connected_to_wren_a_),              //input, width =1
									.rden_a                                  (_connected_to_rden_a_),              //input, width = 1
									.byteena_a                               (_connected_to_byteena_a_),           //input, width = BYTE_EN_WIDTH_A
									.data_b                                  (_connected_to_data_b_),              //input, width = DATA_WIDTH_B
									.address_b                               (_connected_to_address_b_),           //input, width = ADDR_WIDTH_B
									.wren_b                                  (_connected_to_wren_b_),              //input, width =1
									.rden_b                                  (_connected_to_rden_b_),              //input, width = 1
									.byteena_b                               (_connected_to_byteena_b_),           //input, width = BYTE_EN_WIDTH_B
									.q_a                                     (_connected_to_q_a_),                 //output, width = DATA_WIDTH_A
									.q_b                                     (_connected_to_q_b_)                  //output, width = DATA_WIDTH_B
	);
end_template
begin_template async_fifo
	//Quartus Prime Parameterizable Macro Template
	//ASYNC FIFO
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/async_fifo.v
	
	async_fifo #(
									.DATA_WIDTH_A                            (8),
									.ADDR_WIDTH_A                            (11),
									.DATA_WIDTH_B                            (8),
									.ADDR_WIDTH_B                            (11),
									.RDSYNC_DELAYPIPE                        (2),
									.WRSYNC_DELAYPIPE                        (2),
									.ENABLE_SHOWAHEAD                        ("OFF"),
									.UNDERFLOW_CHECKING                      ("ON"),
									.OVERFLOW_CHECKING                       ("ON"),
									.ADD_USEDW_MSB_BIT                       ("OFF"),
									.WRITE_ACLR_SYNCH                        ("OFF"),
									.READ_ACLR_SYNCH                         ("OFF"),
									.ADD_RAM_OUTPUT_REGISTER                 ("OFF"),
									.MAXIMUM_DEPTH                           (2048),
									.BYTE_EN_WIDTH                           (1),
									.BYTE_SIZE                               (8)
                                      
	
	) <instance_name> (
									.data                                    (_connected_to_data_),                //input, width = DATA_WIDTH_A
									.rdclk                                   (_connected_to_rdclk_),               //input, width = 1
									.wrclk                                   (_connected_to_wrclk_),               //input, width = 1
									.aclr                                    (_connected_to_aclr_),                //input, width = 1
									.rdreq                                   (_connected_to_rdreq_),               //input, width = 1
									.wrreq                                   (_connected_to_wrreq_),               //input, width = 1
									.byteena                                 (_connected_to_byteena_),             //input, width = BYTE_EN_WIDTH
									.rdfull                                  (_connected_to_rdfull_),              //output, width = 1
									.wrfull                                  (_connected_to_wrfull_),              //output, width = 1
									.rdempty                                 (_connected_to_rdempty_),             //output, width = 1
									.wrempty                                 (_connected_to_wrempty_),             //output, width = 1
									.rdusedw                                 (_connected_to_rdusedw_),             //output, width = ADDR_WIDTH_B
									.wrusedw                                 (_connected_to_wrusedw_),             //output, width = ADDR_WIDTH_A
									.q                                       (_connected_to_q_)                    //output, width = DATA_WIDTH_B
	);
end_template
begin_template sync_fifo
	//Quartus Prime Parameterizable Macro Template
	//SYNC FIFO
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/sync_fifo.v
	
	sync_fifo #(
									.ADD_RAM_OUTPUT_REGISTER                 ("OFF"),
									.ALMOST_EMPTY_VALUE                      (1),
									.ALMOST_FULL_VALUE                       (1),
									.ENABLE_SCLR                             ("OFF"),
									.ENABLE_ACLR                             ("OFF"),
									.ALLOW_RWCYCLE_WHEN_FULL                 ("ON"),
									.ENABLE_SHOWAHEAD                        ("OFF"),
									.DATA_WIDTH                              (8),
									.ADDR_WIDTH                              (11),
									.OVERFLOW_CHECKING                       ("ON"),
									.UNDERFLOW_CHECKING                      ("ON"),
									.MAXIMUM_DEPTH                           (2048),
									.BYTE_SIZE                               (8),
									.BYTE_EN_WIDTH                           (1)
	

	) <instance_name> (
									.clock                                   (_connected_to_clock_),               //input, width = 1
									.data                                    (_connected_to_data_),                //input, width = DATA_WIDTH
									.rdreq                                   (_connected_to_rdreq_),               //input, width = 1
									.sclr                                    (_connected_to_sclr_),                //input, width = 1, synch reset
									.aclr                                    (_connected_to_aclr_),                //input, width = 1, asynch reset
									.wrreq                                   (_connected_to_wrreq_),               //input, width = 1
									.byteena                                 (_connected_to_byteena_),             //input, width = BYTE_EN_WIDTH
									.almost_empty                            (_connected_to_almost_empty_),         //output, width = 1
									.almost_full                             (_connected_to_almost_full_),         //output, width = 1
									.q                                       (_connected_to_q_),                   //output, width = DATA_WIDTH
									.usedw                                   (_connected_to_usedw_),               //output, width = ADDR_WIDTH
									.empty                                   (_connected_to_empty_),               //output, width = 1
									.full                                    (_connected_to_full_)                 //output, width = 1
	);
end_template
begin_template ipm_iopll_basic
	//Quartus Prime Parameterizable Macro Template
	//IPM IOPLL BASIC
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_iopll_basic.v

	ipm_iopll_basic #(
				.REFERENCE_CLOCK_FREQUENCY ("100.0 MHz"),
				.N_CNT                     (1),
				.M_CNT                     (6),
				.C0_CNT                    (1),
				.C1_CNT                    (1),
				.C2_CNT                    (1),
				.C3_CNT                    (1),
				.C4_CNT                    (1),
				.C5_CNT                    (1),
				.C6_CNT                    (1),
				.PLL_SIM_MODEL             ("")			// It is a simulation specific parameter to select the technology dependent IOPLL simulation model. Allowed values are "Stratix 10", "Agilex 7 F-Series", "Agilex 7 (F-Series)", "Agilex 7 I-Series", "Agilex 7 (I-Series)", "Agilex 7 M-Series", "Agilex 7 (M-series)".
	) <instance_name> (
				.refclk                    (_connected_to_refclk_),              //input, width = 1
				.reset                     (_connected_to_reset_),               //input, width = 1
				.outclk0                   (_connected_to_outclk0_),             //output, width = 1
				.outclk1                   (_connected_to_outclk1_),             //output, width = 1
				.outclk2                   (_connected_to_outclk2_),             //output, width = 1
				.outclk3                   (_connected_to_outclk3_),             //output, width = 1
				.outclk4                   (_connected_to_outclk4_),             //output, width = 1
				.outclk5                   (_connected_to_outclk5_),             //output, width = 1
				.outclk6                   (_connected_to_outclk6_),             //output, width = 1
				.locked                    (_connected_to_locked_)               //output, width = 1
	);
end_template
begin_template ipm_iopll_advanced
	//Quartus Prime Parameterizable Macro Template
	//IPM IOPLL ADVANCED
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_iopll_advanced.sv

	ipm_iopll_advanced #(
				.REFERENCE_CLOCK_FREQUENCY ("100.0 MHz"),
				.N_CNT                     (1),
				.M_CNT                     (6),
				.C0_CNT                    (1),
				.C1_CNT                    (1),
				.C2_CNT                    (1),
				.C3_CNT                    (1),
				.C4_CNT                    (1),
				.C5_CNT                    (1),
				.C6_CNT                    (1),
				.OPERATION_MODE            ("direct"),
				.CLOCK_TO_COMPENSATE       (1),
				.PHASE_SHIFT0              ("0 ps"),
				.PHASE_SHIFT1              ("0 ps"),
				.PHASE_SHIFT2              ("0 ps"),
				.PHASE_SHIFT3              ("0 ps"),
				.PHASE_SHIFT4              ("0 ps"),
				.PHASE_SHIFT5              ("0 ps"),
				.PHASE_SHIFT6              ("0 ps"),
				.DUTY_CYCLE0               (50),
				.DUTY_CYCLE1               (50),
				.DUTY_CYCLE2               (50),
				.DUTY_CYCLE3               (50),
				.DUTY_CYCLE4               (50),
				.DUTY_CYCLE5               (50),
				.DUTY_CYCLE6               (50),
				.PLL_SIM_MODEL             ("")			// It is a simulation specific parameter to select the technology dependent IOPLL simulation model. Allowed values are "Stratix 10", "Agilex 7 F-Series", "Agilex 7 (F-Series)", "Agilex 7 I-Series", "Agilex 7 (I-Series)", "Agilex 7 M-Series", "Agilex 7 (M-Series)".
	) <instance_name> (
				.refclk                    (_connected_to_refclk_),              //input,  width = 1
				.reset                     (_connected_to_reset_),               //input,  width = 1
				.outclk0                   (_connected_to_outclk0_),             //output, width = 1
				.outclk1                   (_connected_to_outclk1_),             //output, width = 1
				.outclk2                   (_connected_to_outclk2_),             //output, width = 1
				.outclk3                   (_connected_to_outclk3_),             //output, width = 1
				.outclk4                   (_connected_to_outclk4_),             //output, width = 1
				.outclk5                   (_connected_to_outclk5_),             //output, width = 1
				.outclk6                   (_connected_to_outclk6_),             //output, width = 1
				.locked                    (_connected_to_locked_),              //output, width = 1
				.fbclk                     (_connected_to_fbclk_),               //input,  width = 1
				.fbclkout                  (_connected_to_fbclkout_),            //output, width = 1
				.extclk_out                (_connected_to_extclk_out_),          //output, width = 1
				.zdbfbclk                  (_connected_to_zdbfbclk_)             //inout,  width = 1	
	);
end_template
begin_template ipm_cdc_sync_rst
	//Quartus Prime Parameterizable Macro Template
	//IPM_CDC_SYNC_RST
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_sync_rst.sv
	
	ipm_cdc_sync_rst #(
				.RST_TYPE				("ACTIVE_HIGH"),
				.NUM_STAGES				(3)
	) <instance_name> (
				.clk					(_connected_to_clk_),                //input, width = 1
				.srst_in                          	(_connected_to_srst_in_),            //input, width = 1
				.srst_out				(_connected_to_srst_out_)            //output, width = 1
	);
end_template
begin_template ipm_cdc_async_rst
	//Quartus Prime Parameterizable Macro Template
	//  IPM_CDC_ASYNC_RST
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_async_rst.sv
	
	ipm_cdc_async_rst #(
				.RST_TYPE				("ACTIVE_HIGH"),
				.NUM_STAGES                       	(3)
	) <instance_name> (
				.clk					(_connected_to_clk_),                //input, width = 1
				.arst_in                          	(_connected_to_arst_in_),            //input, width = 1
				.srst_out                         	(_connected_to_srst_out_)            //output, width = 1
	);
end_template
begin_template ipm_cdc_1clk_sync
	//Quartus Prime Parameterizable Macro Template
	//IPM_CDC_1CLK_SYNC
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_1clk_sync.sv
	
	ipm_cdc_1clk_sync #(
				.INITIAL_VALUE				(0),
				.NUM_STAGES                     	(3)
	) <instance_name> (
				.clk					(_connected_to_clk_),                //input, width = 1
				.async_in                        	(_connected_to_async_in_),           //input, width = 1
				.sync_out				(_connected_to_sync_out_)            //output, width = 1
	);
end_template
begin_template ipm_cdc_2clks_sync
	//Quartus Prime Parameterizable Macro Template
	//IPM_CDC_2CLKS_SYNC
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_2clks_sync.sv
	
	ipm_cdc_2clks_sync #(
				.INITIAL_VALUE				(0),
				.NUM_STAGES				(3)
	) <instance_name> (
				.src_clk                       		(_connected_to_src_clk_),            //input, width = 1
				.src_sig	                        (_connected_to_src_sig_),            //input, width = 1,
				.dst_clk	                        (_connected_to_dst_clk_),            //input, width = 1,
				.dst_sig				(_connected_to_dst_sig_)             //output, width = 1
	);
end_template
begin_template ipm_cdc_glitchless_clk_mux
	//Quartus Prime Parameterizable Macro Template
	//IPM_CDC_GLITCHLESS_CLK_MUX
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_glitchless_clk_mux.sv
	
	ipm_cdc_glitchless_clk_mux #(
					.CLK_TYPE		("RELATED_CLKS")
	) <instance_name> (
					.sel			(_connected_to_sel_),            //input, width = 1
					.clk_A                  (_connected_to_clk_A_),          //input, width = 1
					.clk_B                  (_connected_to_clk_B_),          //input, width = 1
					.clk_out                (_connected_to_clk_out_)         //output, width = 1
	);
end_template
begin_template ipm_cdc_bus_sync
	//Quartus Prime Parameterizable Macro Template
	//IPM_CDC_BUS_SYNC
	//Documentation :
	//https://www.intel.com/content/www/us/en/docs/programmable/772350/
	//Macro Location :
	//$QUARTUS_ROOTDIR/libraries/megafunctions/ipm_cdc_bus_sync.sv
	
	ipm_cdc_bus_sync #(
				.DATA_WIDTH                     (4)
	) <instance_name> (
				.src_clk                        (_connected_to_src_clk_),            //input, width = 1
				.src_sig                        (_connected_to_src_sig_),            //input, width = DATA_WIDTH
				.dst_clk                        (_connected_to_dst_clk_),            //input, width = 1
				.dst_sig                        (_connected_to_dst_sig_),            //output, width = DATA_WIDTH
				.src_sync_req                   (_connected_to_src_sync_req_),       //output, width = 1
				.dst_sync_ack                   (_connected_to_dst_sync_ack_),       //output, width = 1
				.src_req                        (_connected_to_src_req_),            //output, width = 1
				.dst_ack                        (_connected_to_dst_ack_)             //output, width = 1
	);
end_template
end_group
end_group
