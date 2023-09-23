//begin_group Verilog HDL
//begin_group Full Designs
//begin_group RAMs and ROMs
//begin_template Single Port RAM
// Quartus Prime Verilog Template
// Quartus Prime Single port RAM with single read/write address 

module single_port_ram 
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data,
	input [(ADDR_WIDTH-1):0] addr,
	input we, clk,
	output reg [(DATA_WIDTH-1):0] q
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	always @ (posedge clk)
	begin
		if (we)
			ram[addr] = data;

		// Read returns OLD data.	To return
		// NEW data, use = (blocking write) rather than <= (non-blocking write)
		// in the write assignment.	 NOTE: NEW data requires extra bypass
		// logic around the RAM on Stratix10.
		q <= ram[addr];
	end
endmodule
// end_template
// begin_template Single Port RAM with Initial Contents
// Quartus Prime Verilog Template
// Single port RAM with single read/write address and initial contents 
// specified with an initial block

module single_port_ram_with_init
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data,
	input [(ADDR_WIDTH-1):0] addr,
	input we, clk,
	output reg [(DATA_WIDTH-1):0] q
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	// Specify the initial contents.  You can also use the $readmemb
	// system task to initialize the RAM variable from a text file.
	// See the $readmemb template page for details.	
	initial 
	begin : INIT
		integer i;
		for(i = 0; i < 2**ADDR_WIDTH; i = i + 1)
			ram[i] = {DATA_WIDTH{1'b1}};
	end
	 
	always @ (posedge clk)
	begin
		if (we)
			ram[addr] <= data;
			
		// Read returns OLD data.	To return
		// NEW data, use = (blocking write) rather than <= (non-blocking write)
		// in the write assignment.	 NOTE: NEW data requires extra bypass
		// logic around the RAM on Stratix10.			
		q <= ram[addr];
	end
endmodule
end_template
begin_template Simple Dual Port RAM (single clock)
// Quartus Prime Verilog Template
// Simple Dual Port RAM with separate read/write addresses and
// single read/write clock

module simple_dual_port_ram_single_clock
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data,
	input [(ADDR_WIDTH-1):0] read_addr, write_addr,
	input we, clk,
	output reg [(DATA_WIDTH-1):0] q
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	always @ (posedge clk)
	begin
		// Write
		if (we)
			ram[write_addr] <= data;

		// Read (if read_addr == write_addr) returns OLD data.	To return
		// NEW data, use = (blocking write) rather than <= (non-blocking write)
		// in the write assignment.	 NOTE: NEW data may require extra bypass
		// logic around the RAM.
		q <= ram[read_addr];
	end

endmodule
end_template
begin_template Simple Dual Port RAM (dual clock)
// Quartus Prime Verilog Template
// Simple Dual Port RAM with separate read/write addresses and
// separate read/write clocks

module simple_dual_port_ram_dual_clock
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data,
	input [(ADDR_WIDTH-1):0] read_addr, write_addr,
	input we, read_clock, write_clock,
	output reg [(DATA_WIDTH-1):0] q
);
	
	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];
	
	always @ (posedge write_clock)
	begin
		// Write
		if (we)
			ram[write_addr] <= data;
	end
	
	always @ (posedge read_clock)
	begin
		// Read 
		q <= ram[read_addr];
	end
	
endmodule
end_template
begin_template True Dual Port RAM (single clock)
// Quartus Prime Verilog Template
// True Dual Port RAM with single clock
//
// Read-during-write behavior is undefined for mixed ports 
// and "new data" on the same port

module true_dual_port_ram_single_clock
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data_a, data_b,
	input [(ADDR_WIDTH-1):0] addr_a, addr_b,
	input we_a, we_b, clk,
	output reg [(DATA_WIDTH-1):0] q_a, q_b
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	// Port A 
	always @ (posedge clk)
	begin
		if (we_a) 
		begin
			ram[addr_a] = data_a;
		end
		q_a <= ram[addr_a];
	end 

	// Port B 
	always @ (posedge clk)
	begin
		if (we_b) 
		begin
			ram[addr_b] = data_b;
		end
		q_b <= ram[addr_b];
	end

endmodule
end_template
begin_template True Dual Port RAM (single clock, old data on mixed ports read during write)
// Quartus Prime Verilog Template
// True Dual Port RAM with single clock
// Read-during-write behavior is "old data" for mixed ports 
// and "new data" on the same port
//
// This style of RAM cannot be used on Stratix 10, 
// which does not support "old data" read-during-write for mixed ports

module true_dual_port_ram_single_clock_old_rw
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data_a, data_b,
	input [(ADDR_WIDTH-1):0] addr_a, addr_b,
	input we_a, we_b, clk,
	output reg [(DATA_WIDTH-1):0] q_a, q_b
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	// Port A 
	always @ (posedge clk)
	begin
		if (we_a) 
		begin
			ram[addr_a] <= data_a;
			q_a <= data_a;
		end
		else 
		begin
			q_a <= ram[addr_a];
		end 
	end 

	// Port B 
	always @ (posedge clk)
	begin
		if (we_b) 
		begin
			ram[addr_b] <= data_b;
			q_b <= data_b;
		end
		else 
		begin
			q_b <= ram[addr_b];
		end 
	end

endmodule
end_template
begin_template True Dual Port RAM (dual clock)
// Quartus Prime Verilog Template
// True Dual Port RAM with dual clocks
// This style of RAM cannot be used on Stratix 10, 
// which does not support True Dual Port RAM with dual clocks

module true_dual_port_ram_dual_clock
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data_a, data_b,
	input [(ADDR_WIDTH-1):0] addr_a, addr_b,
	input we_a, we_b, clk_a, clk_b,
	output reg [(DATA_WIDTH-1):0] q_a, q_b
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	always @ (posedge clk_a)
	begin
		// Port A 
		if (we_a) 
		begin
			ram[addr_a] <= data_a;
			q_a <= data_a;
		end
		else 
		begin
			q_a <= ram[addr_a];
		end 
	end

	always @ (posedge clk_b)
	begin
		// Port B 
		if (we_b) 
		begin
			ram[addr_b] <= data_b;
			q_b <= data_b;
		end
		else 
		begin
			q_b <= ram[addr_b];
		end 
	end

endmodule
end_template
begin_template Quad Port RAM
// Quartus Prime Verilog Template
// Quad Port RAM with separate read/write addresses and
// single read/write clock
// This style of RAM cannot be used on Arria 10, 
// which does not support Quad Port RAM

module quad_port_ram
#(parameter DATA_WIDTH=2, parameter ADDR_WIDTH=6)
(
	input [(DATA_WIDTH-1):0] data_a, data_b,
	input [(ADDR_WIDTH-1):0] read_addr_a, read_addr_b, write_addr_a, write_addr_b,
	input we_a, we_b, clk,
	output reg [(DATA_WIDTH-1):0] q_a,
	output reg [(DATA_WIDTH-1):0] q_b
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	always @ (posedge clk)
	begin
		// Write
		if (we_a)
			ram[write_addr_a] = data_a;
	end
	always @ (posedge clk)
	begin
		q_a <= ram[read_addr_a];
	end

	always @ (posedge clk)
	begin
		// Write
		if (we_b)
			ram[write_addr_b] = data_b;
	end

	always @ (posedge clk)
	begin
		q_b <= ram[read_addr_b];
	end

endmodule
end_template
begin_template Single Port ROM
// Quartus Prime Verilog Template
// Single Port ROM

module single_port_rom
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=8)
(
	input [(ADDR_WIDTH-1):0] addr,
	input clk, 
	output reg [(DATA_WIDTH-1):0] q
);

	// Declare the ROM variable
	reg [DATA_WIDTH-1:0] rom[2**ADDR_WIDTH-1:0];

	// Initialize the ROM with $readmemb.  Put the memory contents
	// in the file single_port_rom_init.txt.  Without this file,
	// this design will not compile.

	// See Verilog LRM 1364-2001 Section 17.2.8 for details on the
	// format of this file, or see the "Using $readmemb and $readmemh"
	// template later in this section.

	initial
	begin
		$readmemb("single_port_rom_init.txt", rom);
	end

	always @ (posedge clk)
	begin
		q <= rom[addr];
	end

endmodule
end_template
begin_template Dual Port ROM
// Quartus Prime Verilog Template
// Dual Port ROM

module dual_port_rom
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=8)
(
	input [(ADDR_WIDTH-1):0] addr_a, addr_b,
	input clk, 
	output reg [(DATA_WIDTH-1):0] q_a, q_b
);

	// Declare the ROM variable
	reg [DATA_WIDTH-1:0] rom[2**ADDR_WIDTH-1:0];

	// Initialize the ROM with $readmemb.  Put the memory contents
	// in the file dual_port_rom_init.txt.  Without this file,
	// this design will not compile.
	// See Verilog LRM 1364-2001 Section 17.2.8 for details on the
	// format of this file.

	initial
	begin
		$readmemb("dual_port_rom_init.txt", rom);
	end

	always @ (posedge clk)
	begin
		q_a <= rom[addr_a];
		q_b <= rom[addr_b];
	end

endmodule
end_template
begin_template Mixed-Width Port RAM
// Quartus Prime Verilog Template
// 
// To infer mixed-width RAMs, Quartus Prime requires multidimensional array support only
// available in SystemVerilog.  See the SystemVerilog templates.
end_template
begin_template Using $readmemb and $readmemh
// The $readmemb and $readmemh system tasks load the contents of a 2-D
// array variable from a text file.  Quartus Prime supports these system tasks in
// initial blocks.  They may be used to initialized the contents of inferred
// RAMs or ROMs.  They may also be used to specify the power-up value for
// a 2-D array of registers. 
// 
// Usage:
//
// ("file_name", memory_name [, start_addr [, end_addr]]);
// ("file_name", memory_name [, start_addr [, end_addr]]);
//
// File Format:
// 
// The text file can contain Verilog whitespace characters, comments,
// and binary ($readmemb) or hexadecimal ($readmemh) data values.  Both
// types of data values can contain x or X, z or Z, and the underscore
// character.
// 
// The data values are assigned to memory words from left to right,
// beginning at start_addr or the left array bound (the default).  The
// next address to load may be specified in the file itself using @hhhhhh, 
// where h is a hexadecimal character.  Spaces between the @ and the address 
// character are not allowed.  It shall be an error if there are too many 
// words in the file or if an address is outside the bounds of the array.
//
// Example:
//
// reg [7:0] ram[0:2];
// 
// initial
// begin
//     $readmemb("init.txt", rom);
// end
//
// <init.txt>
//
// 11110000      // Loads at address 0 by default
// 10101111   
// @2 00001111   
end_template
end_group
begin_group Shift Registers
begin_template Basic Shift Register
// Quartus Prime Verilog Template
// One-bit wide, N-bit long shift register

module basic_shift_register 
#(parameter N=256)
(
	input clk, enable,
	input sr_in,
	output sr_out
);

	// Declare the shift register
	reg [N-1:0] sr;

	// Shift everything over, load the incoming bit
	always @ (posedge clk)
	begin
		if (enable == 1'b1)
		begin
			sr[N-1:1] <= sr[N-2:0];
			sr[0] <= sr_in;
		end
	end

	// Catch the outgoing bit
	assign sr_out = sr[N-1];

endmodule
end_template
begin_template Basic Shift Register with Asynchronous Reset
// Quartus Prime Verilog Template
// One-bit wide, N-bit long shift register with asynchronous reset

module basic_shift_register_asynchronous_reset
#(parameter N=256)
(
	input clk, enable, reset,
	input sr_in,
	output sr_out
);

	// Declare the shift register
	reg [N-1:0] sr;

	// Shift everything over, load the incoming bit
	always @ (posedge clk or posedge reset)
	begin
		if (reset == 1'b1)
		begin
			// Load N zeros 
			sr <= {N{1'b0}};
		end
		else if (enable == 1'b1)
		begin
			sr[N-1:1] <= sr[N-2:0];
			sr[0] <= sr_in;
		end
	end

	// Catch the outgoing bit
	assign sr_out = sr[N-1];

endmodule
end_template
begin_template Barrel Shifter
// Quartus Prime Verilog Template
// Barrel shifter

module barrel_shifter
#(parameter M=8, parameter N=2**M)
(
	input [N-1:0] data,
	input [M-1:0] distance,
	input clk, enable, shift_left,
	output reg [N-1:0] sr_out
);

	// Declare temporary registers
	reg [2*N-1:0] tmp;

	// Shift/rotate in the specified direction and
	// by the specified amount
	always @ (posedge clk)
	begin
		tmp = {data,data};

		if (enable == 1'b1)
			if (shift_left)
			begin
				tmp = tmp << distance;	
				sr_out <= tmp[2*N-1:N];
			end
			else
			begin
				tmp = tmp >> distance;
				sr_out <= tmp[N-1:0];
			end
	end

endmodule
end_template
begin_template Basic 64-Stage Shift Register with Multiple Taps
// Quartus Prime Verilog Template
// Basic 64-stage shift register with multiple taps

module basic_shift_register_with_multiple_taps
#(parameter WIDTH=8, parameter LENGTH=64)
(
	input clk, enable,
	input [WIDTH-1:0] sr_in,
	output [WIDTH-1:0] sr_tap_one, sr_tap_two, sr_tap_three, sr_out
);

	// Declare the shift register
	reg [WIDTH-1:0] sr [LENGTH-1:0];

	// Declare an iterator
	integer n;

	always @ (posedge clk)
	begin
		if (enable == 1'b1)
		begin
			// Shift everything over, load the incoming data
			for (n = LENGTH-1; n>0; n = n-1)
			begin
				sr[n] <= sr[n-1];
			end

			// Shift one position in
			sr[0] <= sr_in;
		end
	end

	assign sr_tap_one = sr[LENGTH/4-1];
	assign sr_tap_two = sr[LENGTH/2-1];
	assign sr_tap_three = sr[3*LENGTH/4-1];

	// Catch the outgoing data
	assign sr_out = sr[LENGTH-1];

endmodule
end_template
end_group
begin_group State Machines
begin_template 4-State Mealy State Machine
// Quartus Prime Verilog Template
// 4-State Mealy state machine

// A Mealy machine has outputs that depend on both the state and 
// the inputs.  When the inputs change, the outputs are updated
// immediately, without waiting for a clock edge.  The outputs
// can be written more than once per state or per clock cycle.

module four_state_mealy_state_machine
(
	input	clk, in, reset,
	output reg [1:0] out
);

	// Declare state register
	reg		[1:0]state;

	// Declare states
	parameter S0 = 0, S1 = 1, S2 = 2, S3 = 3;

	// Determine the next state synchronously, based on the
	// current state and the input
	always @ (posedge clk or posedge reset) begin
		if (reset)
			state <= S0;
		else
			case (state)
				S0:
					if (in)
					begin
						state <= S1;
					end
					else
					begin
						state <= S1;
					end
				S1:
					if (in)
					begin
						state <= S2;
					end
					else
					begin
						state <= S1;
					end
				S2:
					if (in)
					begin
						state <= S3;
					end
					else
					begin
						state <= S1;
					end
				S3:
					if (in)
					begin
						state <= S2;
					end
					else
					begin
						state <= S3;
					end
			endcase
	end

	// Determine the output based only on the current state
	// and the input (do not wait for a clock edge).
	always @ (state or in)
	begin
			case (state)
				S0:
					if (in)
					begin
						out = 2'b00;
					end
					else
					begin
						out = 2'b10;
					end
				S1:
					if (in)
					begin
						out = 2'b01;
					end
					else
					begin
						out = 2'b00;
					end
				S2:
					if (in)
					begin
						out = 2'b10;
					end
					else
					begin
						out = 2'b01;
					end
				S3:
					if (in)
					begin
						out = 2'b11;
					end
					else
					begin
						out = 2'b00;
					end
			endcase
	end

endmodule
end_template
begin_template 4-State Moore State Machine
// Quartus Prime Verilog Template
// 4-State Moore state machine

// A Moore machine's outputs are dependent only on the current state.
// The output is written only when the state changes.  (State
// transitions are synchronous.)

module four_state_moore_state_machine
(
	input	clk, in, reset,
	output reg [1:0] out
);

	// Declare state register
	reg		[1:0]state;

	// Declare states
	parameter S0 = 0, S1 = 1, S2 = 2, S3 = 3;

	// Output depends only on the state
	always @ (state) begin
		case (state)
			S0:
				out = 2'b01;
			S1:
				out = 2'b10;
			S2:
				out = 2'b11;
			S3:
				out = 2'b00;
			default:
				out = 2'b00;
		endcase
	end

	// Determine the next state
	always @ (posedge clk or posedge reset) begin
		if (reset)
			state <= S0;
		else
			case (state)
				S0:
					state <= S1;
				S1:
					if (in)
						state <= S2;
					else
						state <= S1;
				S2:
					if (in)
						state <= S3;
					else
						state <= S1;
				S3:
					if (in)
						state <= S2;
					else
						state <= S3;
			endcase
	end

endmodule
end_template
begin_template Safe State Machine
// Quartus Prime Verilog Template
// Safe state machine

module safe_state_machine
(
	input	clk, in, reset,
	output reg [1:0] out
);

	// Declare the state register to be "safe" to implement
	// a safe state machine that can recover gracefully from
	// an illegal state (by returning to the reset state).
	(* syn_encoding = "safe" *) reg [1:0] state;

	// Declare states
	parameter S0 = 0, S1 = 1, S2 = 2, S3 = 3;

	// Output depends only on the state
	always @ (state) begin
		case (state)
			S0:
				out = 2'b01;
			S1:
				out = 2'b10;
			S2:
				out = 2'b11;
			S3:
				out = 2'b00;
			default:
				out = 2'b00;
		endcase
	end

	// Determine the next state
	always @ (posedge clk or posedge reset) begin
		if (reset)
			state <= S0;
		else
			case (state)
				S0:
					state <= S1;
				S1:
					if (in)
						state <= S2;
					else
						state <= S1;
				S2:
					if (in)
						state <= S3;
					else
						state <= S1;
				S3:
					if (in)
						state <= S2;
					else
						state <= S3;
			endcase
	end

endmodule
end_template
begin_template User-Encoded State Machine
// Quartus Prime Verilog Template
// User-encoded state machine

module user_encoded_state_machine
(
	input	clk, in, reset,
	output reg [1:0] out
);

	// Declare state register
	(* syn_encoding = "user" *) reg [1:0] state;

	// Declare states
	parameter S0 = 0, S1 = 1, S2 = 2, S3 = 3;

	// Output depends only on the state
	always @ (state) begin
		case (state)
			S0:
				out = 2'b01;
			S1:
				out = 2'b10;
			S2:
				out = 2'b11;
			S3:
				out = 2'b00;
			default:
				out = 2'b00;
		endcase
	end

	// Determine the next state
	always @ (posedge clk or posedge reset) begin
		if (reset)
			state <= S0;
		else
			case (state)
				S0:
					state <= S1;
				S1:
					if (in)
						state <= S2;
					else
						state <= S1;
				S2:
					if (in)
						state <= S3;
					else
						state <= S1;
				S3:
					if (in)
						state <= S2;
					else
						state <= S3;
			endcase
	end

endmodule
end_template
end_group
begin_group Arithmetic
begin_group Adders
begin_template Signed Adder
// Quartus Prime Verilog Template
// Signed adder

module signed_adder
#(parameter WIDTH=16)
(
	input signed [WIDTH-1:0] dataa,
	input signed [WIDTH-1:0] datab,
	input cin,
	output [WIDTH:0] result
);

	assign result = dataa + datab + cin;

endmodule
end_template
begin_template Unsigned Adder
// Quartus Prime Verilog Template
// Unsigned Adder

module unsigned_adder
#(parameter WIDTH=16)
(
	input [WIDTH-1:0] dataa,
	input [WIDTH-1:0] datab,
	input cin,
	output [WIDTH:0] result
);

	assign result = dataa + datab + cin;

endmodule
end_template
begin_template Signed Adder/Subtractor (Addsub)
// Quartus Prime Verilog Template
// Signed adder/subtractor

module signed_adder_subtractor
#(parameter WIDTH=16)
(
	input signed [WIDTH-1:0] dataa,
	input signed [WIDTH-1:0] datab,
	input add_sub,	  // if this is 1, add; else subtract
	input clk,
	output reg [WIDTH:0] result
);

	always @ (posedge clk)
	begin
		if (add_sub)
			result <= dataa + datab;
		else
			result <= dataa - datab;
	end

endmodule
end_template
begin_template Unsigned Adder/Subtractor (Addsub)
// Quartus Prime Verilog Template
// Unsigned Adder/Subtractor

module unsigned_adder_subtractor
#(parameter WIDTH=16)
(
	input [WIDTH-1:0] dataa,
	input [WIDTH-1:0] datab,
	input add_sub,	  // if this is 1, add; else subtract
	input clk,
	output reg [WIDTH:0] result
);

	always @ (posedge clk)
	begin
		if (add_sub)
			result <= dataa + datab;
		else
			result <= dataa - datab;
	end

endmodule
end_template
begin_template Pipelined Binary Adder Tree
// Quartus Prime Verilog Template
// Pipelined binary adder tree

module pipelined_binary_adder_tree
#(parameter WIDTH=16)
(
	input [WIDTH-1:0] A, B, C, D, E,
	input clk,
	output [WIDTH-1:0] out
);

	wire [WIDTH-1:0] sum1, sum2, sum3, sum4;
	reg [WIDTH-1:0] sumreg1, sumreg2, sumreg3, sumreg4;

	always @ (posedge clk)
	begin
		sumreg1 <= sum1;
		sumreg2 <= sum2; 
		sumreg3 <= sum3;
		sumreg4 <= sum4;
	end

	// 2-bit additions
	assign sum1 = A + B;
	assign sum2 = C + D;
	assign sum3 = sumreg1 + sumreg2;
	assign sum4 = sumreg3 + E;
	assign out = sumreg4;

endmodule
end_template
end_group
begin_group Counters
begin_template Binary Counter
// Quartus Prime Verilog Template
// Binary counter

module binary_counter
#(parameter WIDTH=64)
(
	input clk, enable, reset,
	output reg [WIDTH-1:0] count
);

	// Reset if needed, or increment if counting is enabled
	always @ (posedge clk or posedge reset)
	begin
		if (reset)
			count <= 0;
		else if (enable == 1'b1)
			count <= count + 1'b1;
	end

endmodule
end_template
begin_template Binary Up/Down Counter
// Quartus Prime Verilog Template
// Binary up/down counter

module binary_up_down_counter
#(parameter WIDTH=64)
(
	input clk, enable, count_up, reset,
	output reg [WIDTH-1:0] count
);

	// Reset if needed, increment or decrement if counting is enabled
	always @ (posedge clk or posedge reset)
	begin
		if (reset)
			count <= 0;
		else if (enable == 1'b1)
			count <= count + (count_up ? 1 : -1);
	end

endmodule
end_template
begin_template Binary Up/Down Counter with Saturation
// Quartus Prime Verilog Template
// Binary up/down counter with saturation

module binary_up_down_counter_with_saturation
#(parameter WIDTH=32)
(
	input clk, enable, count_up, reset,
	output reg [WIDTH-1:0] count
);

	reg [WIDTH-1:0] direction;
	reg [WIDTH-1:0] limit;

	// Reset if needed, increment or decrement if counter is not saturated
	always @ (posedge clk or posedge reset)
	begin
		if (reset)
			count <= 0;
		else if (enable == 1'b1)
		begin
			if (count_up)
			begin
				direction <= 1;
				limit <= {WIDTH{1'b1}};	 // max value is all 1's
			end
			else
			begin
				direction <= -1; 
				limit <= {WIDTH{1'b0}};
			end

			if (count != limit)
				count <= count + direction;
		end
	end

endmodule
end_template
begin_template Gray Counter
// Quartus Prime Verilog Template
// Gray counter

module gray_counter
#(parameter WIDTH=8)
(
	input clk, enable, reset,
	output reg [WIDTH-1:0] gray_count
);

// Implementation:

// There's an imaginary bit in the counter, at q[-1], that resets to 1
// (unlike the rest of the bits of the counter) and flips every clock cycle.
// The decision of whether to flip any non-imaginary bit in the counter
// depends solely on the bits below it, down to the imaginary bit.	It flips
// only if all these bits, taken together, match the pattern 10* (a one
// followed by any number of zeros).

// Almost every non-imaginary bit has a submodule instance that sets the
// bit based on the values of the lower-order bits, as described above.
// The rules have to differ slightly for the most significant bit or else 
// the counter would saturate at it's highest value, 1000...0.

	// q is the counter, plus the imaginary bit
	reg q [WIDTH-1:-1];

	// no_ones_below[x] = 1 iff there are no 1's in q below q[x]
	reg no_ones_below [WIDTH-1:-1];

	// q_msb is a modification to make the msb logic work
	reg q_msb;

	integer i, j, k;

	always @ (posedge reset or posedge clk)
	begin
		if (reset)
		begin

			// Resetting involves setting the imaginary bit to 1
			q[-1] <= 1;
			for (i = 0; i <= WIDTH-1; i = i + 1)
				q[i] <= 0;

		end
		else if (enable)
		begin
			// Toggle the imaginary bit
			q[-1] <= ~q[-1];

			for (i = 0; i < WIDTH-1; i = i + 1)
			begin

				// Flip q[i] if lower bits are a 1 followed by all 0's
				q[i] <= q[i] ^ (q[i-1] & no_ones_below[i-1]);

			end

			q[WIDTH-1] <= q[WIDTH-1] ^ (q_msb & no_ones_below[WIDTH-2]);
		end
	end


	always @(*)
	begin

		// There are never any 1's beneath the lowest bit
		no_ones_below[-1] <= 1;

		for (j = 0; j < WIDTH-1; j = j + 1)
			no_ones_below[j] <= no_ones_below[j-1] & ~q[j-1];

		q_msb <= q[WIDTH-1] | q[WIDTH-2];

		// Copy over everything but the imaginary bit
		for (k = 0; k < WIDTH; k = k + 1)
			gray_count[k] <= q[k];
	end	


endmodule
end_template
end_group
begin_group Multipliers
begin_template Unsigned Multiply
// Quartus Prime Verilog Template
// Unsigned multiply

module unsigned_multiply
#(parameter WIDTH=8)
(
	input [WIDTH-1:0] dataa,
	input [WIDTH-1:0] datab,
	output [2*WIDTH-1:0] dataout
);

	assign dataout = dataa * datab;

endmodule
end_template
begin_template Signed Multiply
// Quartus Prime Verilog Template
// Signed multiply

module signed_multiply
#(parameter WIDTH=8)
(
	input signed [WIDTH-1:0] dataa,
	input signed [WIDTH-1:0] datab,
	output [2*WIDTH-1:0] dataout
);

	assign dataout = dataa * datab;

endmodule
end_template
begin_template Unsigned Multiply with Input and Output Registers
// Quartus Prime Verilog Template
// Unsigned multiply with input and output registers

module unsigned_multiply_with_input_and_output_registers
#(parameter WIDTH=8)
(
	input clk,
	input [WIDTH-1:0] dataa,
	input [WIDTH-1:0] datab,
	output reg [2*WIDTH-1:0] dataout
);

	// Declare input and output registers
	reg [WIDTH-1:0] dataa_reg;
	reg [WIDTH-1:0] datab_reg;
	wire [2*WIDTH-1:0] mult_out;

	// Store the result of the multiply
	assign mult_out = dataa_reg * datab_reg;

	// Update data
	always @ (posedge clk)
	begin
		dataa_reg <= dataa;
		datab_reg <= datab;
		dataout <= mult_out;
	end

endmodule
end_template
begin_template Signed Multiply with Input and Output Registers
// Quartus Prime Verilog Template
// Signed multiply with input and output registers

module signed_multiply_with_input_and_output_registers
#(parameter WIDTH=8)
(
	input clk,
	input signed [WIDTH-1:0] dataa,
	input signed [WIDTH-1:0] datab,
	output reg signed [2*WIDTH-1:0] dataout
);

	// Declare input and output registers
	reg signed [WIDTH-1:0] dataa_reg;
	reg signed [WIDTH-1:0] datab_reg;
	wire signed [2*WIDTH-1:0] mult_out;

	// Store the result of the multiply
	assign mult_out = dataa_reg * datab_reg;

	// Update data
	always @ (posedge clk)
	begin
		dataa_reg <= dataa;
		datab_reg <= datab;
		dataout <= mult_out;
	end

endmodule
end_template
begin_template Multiplier for Complex Numbers
// Quartus Prime Verilog Template
// Multiplier for complex numbers

module multiplier_for_complex_numbers
#(parameter WIDTH=18)
(
	input clk, ena,
	input signed [WIDTH-1:0] dataa_real, dataa_img,
	input signed [WIDTH-1:0] datab_real, datab_img,
	output reg signed [2*WIDTH-1:0] dataout_real, dataout_img
);

	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
			dataout_real = dataa_real * datab_real - dataa_img * datab_img;
			dataout_img  = dataa_real * datab_img  + datab_real * dataa_img;
		end
	end

endmodule
end_template
begin_template complex_mult_agx5
// Infer a complex multiply mode DSP for Agilex5.  For bit width larger than
// 16 or for chains of complex mults, or if pipeline stages require use of
// synchronous clear, consider using the general template instead.
//
// (a + bi) * (c + di) = ac - bd + i(ad + bc)
//
// real_out = ac - bd
// imag_out = ad + bc
//
module complex_mult_agx5
#(
	parameter IWIDTH = 16
)
(
	input clk,
	input aclr,
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] bi,
	input signed [IWIDTH-1:0] c,
	input signed [IWIDTH-1:0] di,
	output signed [IWIDTH*2:0] real_out,
	output signed [IWIDTH*2:0] imag_out
);
	reg signed [IWIDTH-1:0] a_r  [2:0];
	reg signed [IWIDTH-1:0] bi_r [2:0];
	reg signed [IWIDTH-1:0] c_r  [2:0];
	reg signed [IWIDTH-1:0] di_r [2:0];

	reg signed [IWIDTH*2:0] rout_r;
	reg signed [IWIDTH*2:0] iout_r;

	always @ (posedge clk, posedge aclr) begin
		if (aclr) begin
			a_r[2]  <= 0; 
			a_r[1]  <= 0; 
			a_r[0]  <= 0; 
			bi_r[2] <= 0; 
			bi_r[1] <= 0; 
			bi_r[0] <= 0; 
			c_r[2]  <= 0; 
			c_r[1]  <= 0; 
			c_r[0]  <= 0; 
			di_r[2] <= 0; 
			di_r[1] <= 0; 
			di_r[0] <= 0; 
			rout_r  <= 0;
			iout_r  <= 0;
		end else begin
			a_r[2] <= a_r[1];
			a_r[1] <= a_r[0];
			a_r[0] <= a;
			bi_r[2] <= bi_r[1];
			bi_r[1] <= bi_r[0];
			bi_r[0] <= bi;
			c_r[2] <= c_r[1];
			c_r[1] <= c_r[0];
			c_r[0] <= c;
			di_r[2] <= di_r[1];
			di_r[1] <= di_r[0];
			di_r[0] <= di;
			rout_r = a_r[2] * c_r[2] - bi_r[2] * di_r[2];
			iout_r = a_r[2] * di_r[2] + bi_r[2] * c_r[2];
		end
	end

	assign real_out = rout_r;
	assign imag_out = iout_r;

endmodule
end_template
begin_template complex_mult_gen_18x18
// Create a complex multiply using generally available DSP modes on Intel FPGA
// architectures. For widths <= 18 this will use 2 DSPs in 18x18 Sum-of-2
// Mode.
//
// (a + bi) * (c + di) = ac - bd + i(ad + bc)
//
// real_out = ac - bd
// imag_out = ad + bc
//
module complex_mult_gen_18x18
#(
	parameter IWIDTH = 18,
	parameter CLEAR_TYPE = "NONE" // NONE, ACLR, SCLR
)
(
	input clk,
	input clr,
	input signed [IWIDTH*2:0] real_casc,
	input signed [IWIDTH*2:0] imag_casc,
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] bi,
	input signed [IWIDTH-1:0] c,
	input signed [IWIDTH-1:0] di,
	output signed [IWIDTH*2:0] real_out,
	output signed [IWIDTH*2:0] imag_out
);

	reg signed [IWIDTH*2:0] rout_r;
	reg signed [IWIDTH*2:0] iout_r;

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if (clr) begin
				rout_r <= 0;
				iout_r <= 0;
			end else begin
				rout_r <= (a * c - bi * di) + real_casc;
				iout_r <= (a * di + bi * c) + imag_casc;
			end
		end
	end
	else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				rout_r <= 0;
				iout_r <= 0;
			end else begin
				rout_r <= (a * c - bi * di) + real_casc;
				iout_r <= (a * di + bi * c) + imag_casc;
			end
		end
	end
	else begin
		always @ (posedge clk) begin
			rout_r <= (a * c - bi * di) + real_casc;
			iout_r <= (a * di + bi * c) + imag_casc;
		end
	end
	endgenerate

	assign real_out = rout_r;
	assign imag_out = iout_r;

endmodule
end_template
begin_template complex_mult_gen_27x27
// Create a complex multiply using generally available DSP modes on Intel FPGA
// architectures. For widths <= 27 this will use 2 DSPs in 27x27 independent
// mode cascaded together for the real and imaginary components (4 total).
//
// (a + bi) * (c + di) = ac - bd + i(ad + bc)
//
// real_out = ac - bd
// imag_out = ad + bc
//
module complex_mult_gen_27x27
#(
	parameter IWIDTH = 27,
	parameter CLEAR_TYPE = "NONE" // NONE, ACLR, SCLR
)
(
	input clk,
	input clr,
	input signed [IWIDTH*2:0] real_casc,
	input signed [IWIDTH*2:0] imag_casc,
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] bi,
	input signed [IWIDTH-1:0] c,
	input signed [IWIDTH-1:0] di,
	output signed [IWIDTH*2:0] real_out,
	output signed [IWIDTH*2:0] imag_out
);

	reg signed [IWIDTH*2:0] rout_r [1:0];
	reg signed [IWIDTH*2:0] iout_r [1:0];

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if (clr) begin
				rout_r[0] <= 0;
				rout_r[1] <= 0;
				iout_r[0] <= 0;
				iout_r[1] <= 0;
			end else begin
				rout_r[0] <= (a * c) + real_casc;
				rout_r[1] <= rout_r[0] - (bi * di);
				iout_r[0] <= (a * di) + imag_casc;
				iout_r[1] <= iout_r[0] + (bi * c);
			end
		end
	end
	else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				rout_r[0] <= 0;
				rout_r[1] <= 0;
				iout_r[0] <= 0;
				iout_r[1] <= 0;
			end else begin
				rout_r[0] <= (a * c) + real_casc;
				rout_r[1] <= rout_r[0] - (bi * di);
				iout_r[0] <= (a * di) + imag_casc;
				iout_r[1] <= iout_r[0] + (bi * c);
			end
		end
	end
	else begin
		always @ (posedge clk) begin
			rout_r[0] <= (a * c) + real_casc;
			rout_r[1] <= rout_r[0] - (bi * di);
			iout_r[0] <= (a * di) + imag_casc;
			iout_r[1] <= iout_r[0] + (bi * c);
		end
	end
	endgenerate

	assign real_out = rout_r[1];
	assign imag_out = iout_r[1];

endmodule
end_template
begin_template frac_mult
// Fractal multipliers are implemented in LEs with a focus on density.  Useful
// if doing many small multipliers and don't fit well into the tensor-mode DSPs,
// or if already exhausting all other DSP resources.

(*altera_attribute = "-name FRACTAL_SYNTHESIS ON"*)
module frac_mult
#(
	parameter IWIDTH = 8
)
(
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] b,
	output signed [IWIDTH*2-1:0] prod
);

	assign prod = a * b;

endmodule
end_template
begin_template preaddorsub_mult
// Fixed preadd or presub followed by a multiply in a single DSP. The DSP will
// be in 18x18 or 27x27 mode depending on the IWIDTH.
//
// For dynamic add/sub which uses logic to implement the preadder, see the
// preaddsub_mult template.
//
// res = (a+/-b)*c
//
module preaddorsub_mult
#(
	parameter IWIDTH = 17,
	parameter MODE = "ADD"
)
(
	input signed [IWIDTH-1:0]    a,
	input signed [IWIDTH-1:0]    b,
	input signed [IWIDTH-1:0]    c,
	output signed [IWIDTH*2-1:0] res
);

	wire signed [IWIDTH:0] postadd;
	generate if (MODE == "ADD") begin
		assign postadd = a + b;
	end else if (MODE == "SUB") begin
		assign postadd = a - b;
	end
	endgenerate

	assign res = postadd*c;

endmodule
end_template
begin_template preaddsub_mult
// Preadd/sub in leading logic and DSP afterwards. Will use a single DSP in
// 18x18 or 27x27 mode depending on IWIDTH. The DSP block does not have dynamic
// preadder, so keeping the logic outside is the smallest form.
//
// For fixed add/sub which does integrate into a single the DSP, see the the
// preaddorsub_mult template.
//
// res = (a+/-b)*c
//
module preaddsub_mult
#(
	parameter IWIDTH = 17
)
(
	input signed [IWIDTH-1:0]    a,
	input signed [IWIDTH-1:0]    b,
	input signed [IWIDTH-1:0]    c,
	input                        sub,
	output signed [IWIDTH*2-1:0] res
);

	wire signed [IWIDTH:0] postadd;
	assign postadd = sub ? a-b : a+b;
	assign res = postadd*c;

endmodule
end_template
begin_template preaddsub_square
// Preadd/sub in leading logic and DSP afterwards. Will use a single DSP in
// 18x18 or 27x27 mode depending on IWIDTH.
//
// res = (a+/-b)^2
//
module preaddsub_square
#(
	parameter IWIDTH = 17
)
(
	input signed [IWIDTH-1:0]    a,
	input signed [IWIDTH-1:0]    b,
	input                        sub,
	output signed [IWIDTH*2-1:0] res
);

	wire signed [IWIDTH:0] postadd;
	assign postadd = sub ? a-b : a+b;
	assign res = postadd*postadd;

endmodule
end_template
end_group
begin_group Multiply Accumulators
begin_template Unsigned Multiply-Accumulate
// Quartus Prime Verilog Template
// Unsigned multiply-accumulate

module unsigned_multiply_accumulate
#(parameter WIDTH=8)
(
	input clk, aclr, clken, sload,
	input [WIDTH-1:0] dataa,
	input [WIDTH-1:0] datab,
	output reg [2*WIDTH-1:0] adder_out
);

	// Declare registers and wires
	reg  [WIDTH-1:0] dataa_reg, datab_reg;
	reg  sload_reg;
	reg	 [2*WIDTH-1:0] old_result;
	wire [2*WIDTH-1:0] multa;

	// Store the results of the operations on the current data
	assign multa = dataa_reg * datab_reg;

	// Store the value of the accumulation (or clear it)
	always @ (adder_out, sload_reg)
	begin
		if (sload_reg)
			old_result <= 0;
		else
			old_result <= adder_out;
	end

	// Clear or update data, as appropriate
	always @ (posedge clk or posedge aclr)
	begin
		if (aclr)
		begin
			dataa_reg <= 0;
			datab_reg <= 0;
			sload_reg <= 0;
			adder_out <= 0;
		end
		else if (clken)
		begin
			dataa_reg <= dataa;
			datab_reg <= datab;
			sload_reg <= sload;
			adder_out <= old_result + multa;
		end
	end
endmodule
end_template
begin_template Signed Multiply-Accumulate
// Quartus Prime Verilog Template
// Signed multiply-accumulate

module signed_multiply_accumulate
#(parameter WIDTH=8)
(
	input clk, aclr, clken, sload,
	input signed [WIDTH-1:0] dataa,
	input signed [WIDTH-1:0] datab,
	output reg signed [2*WIDTH-1:0] adder_out
);

	// Declare registers and wires
	reg  signed [WIDTH-1:0] dataa_reg, datab_reg;
	reg  sload_reg;
	reg	 signed [2*WIDTH-1:0] old_result;
	wire signed [2*WIDTH-1:0] multa;

	// Store the results of the operations on the current data
	assign multa = dataa_reg * datab_reg;

	// Store (or clear) old results
	always @ (adder_out, sload_reg)
	begin
		if (sload_reg)
			old_result <= 0;
		else
			old_result <= adder_out;
	end

	// Clear or update data, as appropriate
	always @ (posedge clk or posedge aclr)
	begin
		if (aclr)
		begin
			dataa_reg <= 0;
			datab_reg <= 0;
			sload_reg <= 0;
			adder_out <= 0;
		end
		else if (clken)
		begin
			dataa_reg <= dataa;
			datab_reg <= datab;
			sload_reg <= sload;
			adder_out <= old_result + multa;
		end
	end
endmodule
end_template
begin_template Sum-of-Four Multiply-Accumulate
// Quartus Prime Verilog Template
// Sum-of-four multiply-accumulate
// For use with the Stratix III device family

module sum_of_four_multiply_accumulate
#(parameter INPUT_WIDTH=18, parameter OUTPUT_WIDTH=44)
(
	input clk, ena,
	input [INPUT_WIDTH-1:0] dataa, datab, datac, datad,
	input [INPUT_WIDTH-1:0] datae, dataf, datag, datah,
	output reg [OUTPUT_WIDTH-1:0] dataout
);

	// Each product can be up to 2*INPUT_WIDTH bits wide.
	// The sum of four of these products can be up to 2 bits wider.
	wire [2*INPUT_WIDTH+1:0] mult_sum;

	// Store the results of the operations on the current inputs
	assign mult_sum = (dataa * datab + datac * datad) + (datae * dataf + datag * datah);

	// Store the value of the accumulation
	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
        	    dataout <= dataout + mult_sum;
		end
	end
endmodule
end_template
begin_template Sum-of-Four Multiply-Accumulate with Asynchronous Reset
// Quartus Prime Verilog Template
// Sum-of-four multiply-accumulate with asynchronous reset
// For use with the Stratix III device family

module sum_of_four_multiply_accumulate_with_asynchronous_reset
#(parameter INPUT_WIDTH=18, parameter OUTPUT_WIDTH=44)
(
	input clk, ena, aclr,
	input [INPUT_WIDTH-1:0] dataa, datab, datac, datad,
	input [INPUT_WIDTH-1:0] datae, dataf, datag, datah,
	output reg [OUTPUT_WIDTH-1:0] dataout
);

	// Each product can be up to 2*INPUT_WIDTH bits wide.
	// The sum of four of these products can be up to 2 bits wider.
	wire [2*INPUT_WIDTH+1:0] mult_sum;

	// Store the results of the operations on the current inputs
	assign mult_sum = (dataa * datab + datac * datad) + (datae * dataf + datag * datah);

	// Store the value of the accumulation
	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
        	    dataout <= ((aclr == 1) ? 0 : dataout) + mult_sum;
		end
	end
endmodule
end_template
begin_template complex_mult_acc_gen_18x18
// Create a complex multiply accumulate (generic) by handling the real and
// imaginary components separately. In 18x18 this will use sum-of-2 mode DSP.
//
// (a+bi) * (c+di) = ac - bd + real_prev i(ad + bc + imag_prev)
//
module complex_mult_acc_gen_18x18
#(
	parameter IWIDTH = 18,
	parameter OWIDTH = 64,
	parameter CLEAR_TYPE = "NONE" /* NONE, ACLR, SCLR */
)
(
	input clk,
	input clr,
	input acc,
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] bi,
	input signed [IWIDTH-1:0] c,
	input signed [IWIDTH-1:0] di,
	output signed [OWIDTH-1:0] real_out,
	output signed [OWIDTH-1:0] imag_out
);

	reg signed [OWIDTH-1:0] real_prev;
	reg signed [OWIDTH-1:0] imag_prev;

	wire signed [OWIDTH-1:0] real_res;
	wire signed [OWIDTH-1:0] imag_res;

	assign real_res = acc ? real_prev : {OWIDTH{1'b0}};
	assign imag_res = acc ? imag_prev : {OWIDTH{1'b0}};

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if (clr) begin
				real_prev <= 0;
				imag_prev <= 0;
			end else begin
				real_prev <= (a * c - bi * di) + real_res;
				imag_prev <= (a * di + bi * c) + imag_res;
			end
		end
	end
	else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				real_prev <= 0;
				imag_prev <= 0;
			end else begin
				real_prev <= (a * c - bi * di) + real_res;
				imag_prev <= (a * di + bi * c) + imag_res;
			end
		end
	end
	else begin
		always @ (posedge clk) begin
			real_prev <= (a * c - bi * di) + real_res;
			imag_prev <= (a * di + bi * c) + imag_res;
		end
	end
	endgenerate

	assign real_out = real_prev;
	assign imag_out = imag_prev;

endmodule
end_template
begin_template complex_mult_acc_gen_27x27
// Create a complex multiply accumulate (generic) by handling the real and
// imaginary components separately. In 27x27 this uses two DSPs in cascade
// with the accumulator being present on the second DSP of each chain.
//
// (a+bi) * (c+di) = ac - bd + real_prev i(ad + bc + imag_prev)
//
module complex_mult_acc_gen_27x27
#(
	parameter IWIDTH = 27,
	parameter OWIDTH = 64,
	parameter CLEAR_TYPE = "NONE" /* NONE, ACLR, SCLR */
)
(
	input clk,
	input clr,
	input acc,
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] bi,
	input signed [IWIDTH-1:0] c,
	input signed [IWIDTH-1:0] di,
	output signed [OWIDTH-1:0] real_out,
	output signed [OWIDTH-1:0] imag_out
);

	reg signed [OWIDTH-1:0] real_prev[1:0];
	reg signed [OWIDTH-1:0] imag_prev[1:0];

	wire signed [OWIDTH-1:0] real_res;
	wire signed [OWIDTH-1:0] imag_res;

	assign real_res = acc ? real_prev[1] : {OWIDTH{1'b0}};
	assign imag_res = acc ? imag_prev[1] : {OWIDTH{1'b0}};

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if (clr) begin
				real_prev[0] <= {OWIDTH{1'b0}};
				real_prev[1] <= {OWIDTH{1'b0}};
				imag_prev[0] <= {OWIDTH{1'b0}};
				imag_prev[1] <= {OWIDTH{1'b0}};
			end else begin
				real_prev[0] <= (a * c);
				real_prev[1] <= real_prev[0] - (bi * di) + real_res;
				imag_prev[0] <= (a * di);
				imag_prev[1] <= imag_prev[0] + (bi * c) + imag_res;
			end
		end
	end
	else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				real_prev[0] <= {OWIDTH{1'b0}};
				real_prev[1] <= {OWIDTH{1'b0}};
				imag_prev[0] <= {OWIDTH{1'b0}};
				imag_prev[1] <= {OWIDTH{1'b0}};
			end else begin
				real_prev[0] <= (a * c);
				real_prev[1] <= real_prev[0] - (bi * di) + real_res;
				imag_prev[0] <= (a * di);
				imag_prev[1] <= imag_prev[0] + (bi * c) + imag_res;
			end
		end
	end
	else begin
		always @ (posedge clk) begin
			real_prev[0] <= (a * c);
			real_prev[1] <= real_prev[0] - (bi * di) + real_res;
			imag_prev[0] <= (a * di);
			imag_prev[1] <= imag_prev[0] + (bi * c) + imag_res;
		end
	end
	endgenerate

	assign real_out = real_prev[1];
	assign imag_out = imag_prev[1];

endmodule
end_template
begin_template mult_acc_gen
// Infer a multiply-accumulate DSP in either 18x18 or 27x27 mode depending on
// IWIDTH parameter.
module mult_acc_gen
#(
	parameter IWIDTH = 18,
	parameter CWIDTH = 64,
	parameter OWIDTH = 64,
	parameter CLEAR_TYPE = "NONE" /* NONE, SCLR, ACLR */
)
(
	input clk,
	input clr,
	input acc,
	input signed [CWIDTH-1:0] chain_in, /* must be driven by another DSP */
	input signed [IWIDTH-1:0] a,
	input signed [IWIDTH-1:0] b,
	output signed [OWIDTH-1:0] res
);

	reg signed [OWIDTH-1:0] res_r;
	wire [OWIDTH-1:0] prev;
	assign prev = acc ? res_r : {OWIDTH{1'b0}};

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if(clr) begin
				res_r <= 0;
			end else begin
				res_r <= (a*b) + chain_in + prev;
			end
		end
	end else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				res_r <= 0;
			end else begin
				res_r <= (a*b) + chain_in + prev;
			end
		end
	end
	else begin
		always @ (posedge clk) begin
			res_r <= (a*b) + chain_in + prev;
		end
	end
	endgenerate

	assign res = res_r;

endmodule
end_template
begin_template mult_add_acc
// Infer a multiply-add-accumulate DSP in 18x18+36 w/ accumulator enabled. If
// the multiply is larger than 18x18 it will infer 27x27 DSP with add and
// accumulate outside of the block.
//
// maddacc = a*b + c + prev
//
module mult_add_acc
#(
	parameter MWIDTH = 18,
	parameter AWIDTH = 36,
	parameter RWIDTH = 64,
	parameter CLEAR_TYPE = "NONE" /* NONE, SCLR, ACLR */
)
(
	input clk,
	input clr,
	input acc,
	input signed [MWIDTH-1:0] a,
	input signed [MWIDTH-1:0] b,
	input signed [AWIDTH-1:0] c,
	output signed [RWIDTH-1:0] maddacc
);

	reg signed [RWIDTH-1:0] res_r;
	wire [RWIDTH-1:0] prev;
	assign prev = acc ? res_r : {RWIDTH{1'b0}};

	generate if (CLEAR_TYPE == "ACLR") begin
		always @ (posedge clk, posedge clr) begin
			if(clr) begin
				res_r <= 0;
			end else begin
				res_r <= (a*b) + c + prev;
			end
		end
	end else if (CLEAR_TYPE == "SCLR") begin
		always @ (posedge clk) begin
			if (clr) begin
				res_r <= 0;
			end else begin
				res_r <= (a*b) + c + prev;
			end
		end
	end else begin
		always @ (posedge clk) begin
			res_r <= (a*b) + c + prev;
		end
	end
	endgenerate

	assign maddacc = res_r;

endmodule
end_template
end_group
begin_group Sums of Multipliers
begin_template Sum of Four Multipliers
// Quartus Prime Verilog Template
// Sum of four multipliers

module sum_of_four_multipliers
#(parameter WIDTH=18)
(
	input clk, ena,
	input [WIDTH-1:0] dataa, datab, datac, datad,
	input [WIDTH-1:0] datae, dataf, datag, datah,
	output reg [2*WIDTH+1:0] dataout
);

	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
			dataout <= (dataa * datab + datac * datad) + (datae * dataf + datag * datah);
		end
	end
endmodule
end_template
begin_template Sum of Two Multipliers with Pipeline Registers
// Quartus Prime Verilog Template
// Sum of two multipliers with pipeline registers

module sum_of_two_multipliers_pipeline
#(parameter WIDTH=16)
(
	input clock, aclr,
	input [WIDTH-1:0] dataa, datab, datac, datad,
	output reg [2*WIDTH:0] result
);

	reg [WIDTH-1:0] dataa_reg, datab_reg, datac_reg, datad_reg;
	reg [2*WIDTH-1:0] mult0_result, mult1_result;

	always @ (posedge clock or posedge aclr) begin
    	if (aclr) begin
        	dataa_reg <= {(WIDTH){1'b0}};
			datab_reg <= {(WIDTH){1'b0}};
    	    datac_reg <= {(WIDTH){1'b0}};
        	datad_reg <= {(WIDTH){1'b0}};
			mult0_result <= {(2*WIDTH){1'b0}};
    	    mult1_result <= {(2*WIDTH){1'b0}};
        	result <= {(2*WIDTH+1){1'b0}};
		end
    	else begin
        	dataa_reg <= dataa;
			datab_reg <= datab;
    	    datac_reg <= datac;
        	datad_reg <= datad;
			mult0_result <= dataa_reg * datab_reg;
    	    mult1_result <= datac_reg * datad_reg;
        	result <= mult0_result + mult1_result;
		end
	end
endmodule
end_template
begin_template Sum of Four Multipliers in Scan Chain Mode
// Quartus Prime Verilog Template
// Sum of four multipliers in scan chain mode

module sum_of_four_multipliers_scan_chain
#(parameter WIDTH=18)
(
	input clk, ena,
	input [WIDTH-1:0] dataa, 
	input [WIDTH-1:0] datab0, datab1, datab2, datab3,
	output reg [2*WIDTH+1:0] dataout
);

	// Four scan chain registers
	reg [WIDTH-1:0] a0, a1, a2, a3;

	always @ (posedge clk)
	begin
		if (ena == 1)
		begin

			// The scan chain (which mimics the behavior of a shift register)
			a0 <= dataa;
			a1 <= a0;
			a2 <= a1;
			a3 <= a2;

			// The order of the operands is important for correct inference
			dataout <= (a3 * datab3 + a2 * datab2) + (a1 * datab1 + a0 * datab0);
		end
	end
endmodule
end_template
begin_template Sum of Eight Multipliers in Chainout Mode
// Quartus Prime Verilog Template
// Sum of eight multipliers in chainout mode

module sum_of_eight_multipliers_chainout
#(parameter WIDTH=18)
(
	input clk, ena,
	input [WIDTH-1:0] a0, a1, a2, a3, a4, a5, a6, a7,
	input [WIDTH-1:0] b0, b1, b2, b3, b4, b5, b6, b7,
	output reg [2*WIDTH+1:0] dataout
);

	// Declare wires
	wire [2*WIDTH+1:0] sum1, sum2;

	// Store the results of the first two sums
	assign	sum1 = (a0 * b0 + a1 * b1) + (a2 * b2 + a3 * b3);
	assign	sum2 = (a4 * b4 + a5 * b5) + (a6 * b6 + a7 * b7);

	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
			dataout <= sum1 + sum2;
		end
	end
endmodule 
end_template
begin_template Sum of Two Multipliers with a Wide Datapath
// Quartus Prime Verilog Template
// Sum of two multipliers with a wide datapath

module sum_of_two_multipliers_wide_datapath
#(parameter WIDTH_A=36, WIDTH_B=18)
(
	input clk, ena,
	input [WIDTH_A-1:0] a0, a1,
	input [WIDTH_B-1:0] b0, b1,
	output reg [WIDTH_A+WIDTH_B:0] dataout
);

	always @ (posedge clk)
	begin
		if (ena == 1)
		begin
			dataout <= a0 * b0 + a1 * b1;
		end
	end
endmodule
end_template
end_group
begin_group DSP Features 
begin_group DSP Features for 28-nm Device 
begin_template Single Multiply
// Quartus Prime Verilog Template
// Independent multiply
// For use with the 28-nm device families

module single_mult(a, b, p);
//This template is applicable to 9x9, 18x18, 27x27, 36x18, 36x36 modes on Stratix-V
//This template is applicable to 9x9, 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
parameter a_width = 18;
parameter b_width = 18;
input	[a_width-1:0] a;
input	[b_width-1:0] b;
output 	[a_width+b_width-1:0] p;

//each multiplier can be signed or unsigned
//for mixed-sign multiplication, refer to mixed-sign template
wire signed	[a_width-1:0] a;
wire signed	[b_width-1:0] b;
wire signed [a_width+b_width-1:0] p;

assign p = a * b;

endmodule
end_template
begin_template Sum of Two Multipliers
// Quartus Prime Verilog Template
// Sum of two multipliers
// For use with the 28-nm device families

module sum_of_2(a1, b1, a2, b2, p);
//This template is applicable to sum-of-2 18x18, 27x27, 36x18 modes on Stratix-V
//This template is applicable to sum-of-2 18x19(signed) mode on Arria-V and Cyclone-V
parameter a_width = 18;
parameter b_width = 18;
input	[a_width-1:0] a1;
input	[b_width-1:0] b1;
input	[a_width-1:0] a2;
input	[b_width-1:0] b2;
output	[a_width+b_width:0] p;

wire signed	[a_width-1:0] a1;
wire signed	[b_width-1:0] b1;
wire signed	[a_width-1:0] a2;
wire signed	[b_width-1:0] b2;
wire signed	[a_width+b_width:0] p;

//Static add/sub is supported
assign p = a1 * b1 + a2 * b2;

endmodule
end_template
begin_template Sum of Four Multipliers
// Quartus Prime Verilog Template
// Sum of four multipliers
// For use with the 28-nm device families

module sum_of_4(a1, b1, a2, b2, a3, b3, a4, b4, p);
//This template is applicable to sum-of-4 18x18 mode on Stratix-V
parameter a_width = 18;
parameter b_width = 18;
input	[a_width-1:0] a1;
input	[b_width-1:0] b1;
input	[a_width-1:0] a2;
input	[b_width-1:0] b2;
input	[a_width-1:0] a3;
input	[b_width-1:0] b3;
input	[a_width-1:0] a4;
input	[b_width-1:0] b4;
output	[a_width+b_width+1:0] p;

wire signed	[a_width-1:0] a1;
wire signed	[b_width-1:0] b1;
wire signed	[a_width-1:0] a2;
wire signed	[b_width-1:0] b2;
wire signed	[a_width-1:0] a3;
wire signed	[b_width-1:0] b3;
wire signed	[a_width-1:0] a4;
wire signed	[b_width-1:0] b4;
wire signed	[a_width+b_width+1:0] p;

//Static add/sub is supported
assign p = a1 * b1 + a2 * b2 - a3 * b3 + a4 * b4;

endmodule
end_template
begin_template Mixed Sign Multiply
// Quartus Prime Verilog Template
// Mixed sign multiply
// For use with the 28-nm device families

module mixed_sign(a, b, p);
parameter a_width = 18;
parameter b_width = 18;
input	[a_width-1:0] a;
input	[b_width-1:0] b;
output 	[a_width+b_width-1:0] p;

wire signed	[a_width-1:0] a;
wire [b_width-1:0] b;
wire signed 	[a_width+b_width-1:0] p;

//Note that mixed-sign multiplier also has a_width+b_width bits result
//Guaranteed no overflow
assign p = $signed(a) * $signed({1'b0,b});

endmodule
end_template
begin_template Dynamic Add/Sub Control
// Quartus Prime Verilog Template
// Dynamic add/sub control
// For use with the 28-nm device families

module dynamic_addsub(a1, b1, a2, b2, a3, b3, a4, b4, a5, b5, a6, b6,
	addnsub12, addnsub34, addnsub56, subnadd_chain34, subnadd_chain56,
	clock, ena, reset, s);
parameter a_width = 18;
parameter b_width = 18;
parameter o_width = 64;
input [a_width-1:0] a1;
input [b_width-1:0] b1;
input [a_width-1:0] a2;
input [b_width-1:0] b2;
input [a_width-1:0] a3;
input [b_width-1:0] b3;
input [a_width-1:0] a4;
input [b_width-1:0] b4;
input [a_width-1:0] a5;
input [b_width-1:0] b5;
input [a_width-1:0] a6;
input [b_width-1:0] b6;
input addnsub12;
input addnsub34;
input addnsub56;
input subnadd_chain34;
input subnadd_chain56;
input clock;
input ena;
input reset;
output [o_width-1:0] s;

reg signed [a_width-1:0] a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, a6_reg;
reg signed [b_width-1:0] b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, b6_reg;
reg signed [o_width-1:0] s12, s1234, s;

//Dynamic add/sub can be used in basic modes: sum-of-2 18x18, 36+18x18, two level sum-of-4 18x18, sum-of-2 27x27 and sum-of-2 36x18.
//Input and output signals of the dynamic add/sub operation must be explicitly defined. 
wire signed [a_width+b_width-1:0] p1, p2, p3, p4, p5, p6;
wire signed [a_width+b_width:0] p12, p34, p56;
assign p1 = a1_reg*b1_reg;
assign p2 = a2_reg*b2_reg;
assign p3 = a3_reg*b3_reg;
assign p4 = a4_reg*b4_reg;
assign p5 = a5_reg*b5_reg;
assign p6 = a6_reg*b6_reg;
assign p12 = addnsub12 ? (p1 + p2) : (p1 - p2);
assign p34 = addnsub34 ? (p3 + p4) : (p3 - p4);
assign p56 = addnsub56 ? (p5 + p6) : (p5 - p6);

//Dynamic add/sub is also applicable to chainout adder or accumulator (not both). 
//Dynamic add/sub is not applicable to 18x18 systolic mode.
always @(posedge clock or posedge reset)
if (reset) begin
	a1_reg <= 0;
	a2_reg <= 0;
	a3_reg <= 0;
	a4_reg <= 0;
	a5_reg <= 0;
	a6_reg <= 0;
	b1_reg <= 0;
	b2_reg <= 0;
	b3_reg <= 0;
	b4_reg <= 0;
	b5_reg <= 0;
	b6_reg <= 0;
	s12 <= 0;
	s1234 <= 0;
	s <= 0;
end else begin
	if (ena) begin
		a1_reg <= a1;
		a2_reg <= a2;
		a3_reg <= a3;
		a4_reg <= a4;
		a5_reg <= a5;
		a6_reg <= a6;
		b1_reg <= b1;
		b2_reg <= b2;
		b3_reg <= b3;
		b4_reg <= b4;
		b5_reg <= b5;
		b6_reg <= b6;
		s12 <= p12;
		s1234 <= subnadd_chain34 ? (s12 - p34) : (s12 + p34);
		s <= subnadd_chain56 ? (s1234 - p56) : (s1234 + p56);
	end
end

endmodule
end_template
begin_template Sum of an 18x18 Multiplier and a 36-bit Addend
// Quartus Prime Verilog Template
// Sum of an 18x18 multiplier and a 36-bit addend  
// For use with the 28-nm device families

module plus36_18x18(addend, a, b, p);
//This template is applicable to 36+18x18 mode on Stratix-V
//This template is applicable to 36+18x19(signed) mode on Arria-V and Cyclone-V
//Note that the addend shouldn't be from another multiplier
parameter a_width = 18;
parameter b_width = 18;
input	[a_width+b_width-1:0] addend;
input	[a_width-1:0] a;
input	[b_width-1:0] b;
output 	[a_width+b_width:0] p;

wire signed	[a_width+b_width-1:0] addend;
wire signed	[a_width-1:0] a;
wire signed	[b_width-1:0] b;
wire signed	[a_width+b_width:0] p;

//addend must be the first operand
//Static add/sub is supported
assign p = addend - a * b;

endmodule
end_template
begin_template Complex 25x18 Multiply
// Quartus Prime Verilog Template
// Complex 25x18 multiply
// For use with the 28-nm device families

module complex_25x18(x_r, x_i, y_r, y_i, clock, ena1, ena0, reset, p_r, p_i);
//This template is applicable to complex 25x18 mode on Stratix-V
input [24:0] x_r;
input [24:0] x_i;
input [17:0] y_r;
input [17:0] y_i;
//Stratix-V DSP supports up to 3 clock/ena pairs and 2 async reset signals
input clock;
input ena1;
input ena0;
input reset;
output [43:0] p_r;
output [43:0] p_i;

//All inputs/outputs have to be signed.
//All input registers must use the same {clock, ena, reset}
//All output registers must use the same {clock, ena, reset}
reg signed [24:0] x_r_reg, x_i_reg;
reg signed [17:0] y_r_reg, y_i_reg;
reg signed [43:0] p_r, p_i;

wire signed [25:0] a1;
wire signed [18:0] a2;
wire signed [18:0] a3;
wire signed [43:0] p1;
wire signed [43:0] p2;
wire signed [43:0] p3;

assign a1 = x_r_reg - x_i_reg;
assign p1 = a1 * y_i_reg;
assign a2 = y_r_reg - y_i_reg;
assign p2 = a2 * x_r_reg;
assign a3 = y_r_reg + y_i_reg;
assign p3 = a3 * x_i_reg;

always @(posedge clock or posedge reset)
begin
	if (reset == 1'b1)
	begin
		x_r_reg <= 0;
		x_i_reg <= 0;
		y_r_reg <= 0;
		y_i_reg <= 0;
		p_r <= 0;
		p_i <= 0;
	end
	else
	begin
		if (ena0 == 1'b1)
		begin
			x_r_reg <= x_r;
			x_i_reg <= x_i;
			y_r_reg <= y_r;
			y_i_reg <= y_i;
		end
		if (ena1 == 1'b1)
		begin
			p_r <= p1 + p2;
			p_i <= p1 + p3;
		end
	end
end

endmodule
end_template
begin_template Chainout Adder/Accumulator Feature
// Quartus Prime Verilog Template
// Chainout adder/accumulator feature
// For use with the 28-nm device families

module chainout_accum(a1, b1, a2, b2, a3, b3, a4, b4, a5, b5, 
		accum, load_value, clock, ena, reset, s);
//This template can be used with 18x18, sum-of-2 18x18, 36+18x18, 27x27, 36x18 modes on Stratix-V
//This template can be used with 18x19(signed), 36+18x19(signed), 27x27 modes on Arria-V and Cyclone-V
parameter a_width = 18;
parameter b_width = 18;
//When this template applies to single 18x18, max chain_width is 44, othewise 64
parameter chain_width = 44;
//preload_value can have only one bit 1
parameter [chain_width-1:0] preload_value = 'h400;
//Double accumulator feature is not available on Stratix-V
parameter enable_double_accum = "false";
input	[a_width-1:0] a1;
input	[b_width-1:0] b1;
input	[a_width-1:0] a2;
input	[b_width-1:0] b2;
input	[a_width-1:0] a3;
input	[b_width-1:0] b3;
input	[a_width-1:0] a4;
input	[b_width-1:0] b4;
input	[a_width-1:0] a5;
input	[b_width-1:0] b5;
input	accum;
input	load_value;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//All output registers must have the same {clock, ena, reset}
input	clock;
input	ena;
input	reset;
output 	[chain_width-1:0] s;

wire signed	[a_width-1:0] a1;
wire signed	[b_width-1:0] b1;
wire signed	[a_width-1:0] a2;
wire signed	[b_width-1:0] b2;
wire signed	[a_width-1:0] a3;
wire signed	[b_width-1:0] b3;
wire signed	[a_width-1:0] a4;
wire signed	[b_width-1:0] b4;
wire signed	[a_width-1:0] a5;
wire signed	[b_width-1:0] b5;
wire signed [a_width+b_width-1:0] p1, p2, p3, p4, p5, p6;
reg signed [chain_width-1:0] s1, s2, s3, s4, s5, s, s_double;

//Pick an applicable basic mode template
single_mult mult1 (a1, b1, p1);
defparam mult1.a_width = a_width;
defparam mult1.b_width = b_width;

single_mult mult2 (a2, b2, p2);
defparam mult2.a_width = a_width;
defparam mult2.b_width = b_width;

single_mult mult3 (a3, b3, p3);
defparam mult3.a_width = a_width;
defparam mult3.b_width = b_width;

single_mult mult4 (a4, b4, p4);
defparam mult1.a_width = a_width;
defparam mult1.b_width = b_width;

single_mult mult5 (a5, b5, p5);
defparam mult5.a_width = a_width;
defparam mult5.b_width = b_width;

//When this template applies to single 18x18, the number of multipliers has to be even
//Create a 0x0 if the number of multipliers is odd
wire signed zero_bit;
soft sbz (1'b0, zero_bit);
single_mult mult6 ({a_width{zero_bit}}, {b_width{zero_bit}}, p6);
defparam mult6.a_width = a_width;
defparam mult6.b_width = b_width;

//accumulator path
wire signed [chain_width-1:0] acc_sel;
assign acc_sel = accum ? ((enable_double_accum == "true") ? s_double : s) : (load_value ? preload_value : 0);


always @(posedge clock or posedge reset)
if (reset) begin
	s1 <= 0;
	s2 <= 0;
	s3 <= 0;
	s4 <= 0;
	s5 <= 0;
	s <= 0;
	s_double <= 0;
end else begin
	if (ena) begin
		//chainout adder support static add or sub
		//basic mode result (p) must be the second operand
		s1 <= p1;
		s2 <= s1 + p2;
		s3 <= s2 + p3;
		s4 <= s3 - p4;
		s5 <= s4 + p5;
		//chainout accumulator only support addition when use with chainout adder
		s <= acc_sel + (s5 + p6);	//loopback path (acc_sel) must be the first operand
		s_double <= s;
	end
end

endmodule
end_template
begin_template Chainout Adder with Rounding
// Quartus Prime Verilog Template
// Chainout adder with rounding
// For use with the 28-nm device families

module chainout_rnd(a1, b1, a2, b2, a3, b3, clock, ena, reset, s);
//This template can be used with 18x18, sum-of-2 18x18, 36+18x18, 27x27, 36x18 modes on Stratix-V
//This template can be used with 18x19(signed), 36+18x19(signed), 27x27 modes on Arria-V and Cyclone-V
parameter a_width = 27;
parameter b_width = 27;
//When this template applies to single 18x18, max chain_width is 44, othewise 64
parameter chain_width = 64;
parameter [chain_width-1:0] rounding_bit = 'h2000;
input	[a_width-1:0] a1;
input	[b_width-1:0] b1;
input	[a_width-1:0] a2;
input	[b_width-1:0] b2;
input	[a_width-1:0] a3;
input	[b_width-1:0] b3;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//All output registers must have the same {clock, ena, reset}
input	clock;
input	ena;
input	reset;
output 	[chain_width-1:0] s;

wire signed	[a_width-1:0] a1;
wire signed	[b_width-1:0] b1;
wire signed	[a_width-1:0] a2;
wire signed	[b_width-1:0] b2;
wire signed	[a_width-1:0] a3;
wire signed	[b_width-1:0] b3;
wire signed [a_width+b_width-1:0] p1, p2, p3;
reg signed [chain_width-1:0] s1, s2, s;

//Pick an applicable basic mode template
single_mult mult1 (a1, b1, p1);
defparam mult1.a_width = a_width;
defparam mult1.b_width = b_width;

single_mult mult2 (a2, b2, p2);
defparam mult2.a_width = a_width;
defparam mult2.b_width = b_width;

single_mult mult3 (a3, b3, p3);
defparam mult3.a_width = a_width;
defparam mult3.b_width = b_width;

always @(posedge clock or posedge reset)
if (reset) begin
	s1 <= 0;
	s2 <= 0;
	s <= 0;
end else begin
	if (ena) begin
		//chainout adder support static add or sub
		//basic mode result (p) must be the second operand
		s1 <= p1;
		s2 <= s1 + p2;
		//rounding bit sign has to match with other operands
		s  <= $signed(rounding_bit) + (s2 + p3);
	end
end

endmodule
end_template
begin_template Multiplier with One Operand from Coefficient ROM
// Quartus Prime Verilog Template
// Multiplier with one operand from coefficient ROM
// For use with the 28-nm device families

module input_coef(a1, c1_sel, a2, c2_sel,clock, ena1, ena0, reset, s);
//This template can be used with 18x18, sum-of-2 18x18, sum-of-4 18x18, 27x27, sum-of-2 27x27 modes on Stratix-V
//This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
parameter a_width = 18;
parameter coef_width = 18;
//up to 8 coefficients (3-bit address)
parameter sel_width = 3;
input [a_width-1:0] a1;
input [a_width-1:0] a2;
input [sel_width-1:0] c1_sel;
input [sel_width-1:0] c2_sel;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//each registered data input can use independent {clock, ena}
input clock; 
input ena1;
input ena0;
//all registered data input must use the same reset
input reset;
output [a_width+coef_width:0] s;

reg signed [a_width-1:0] a1_reg, a2_reg;
reg	[sel_width-1:0] c1_sel_reg, c2_sel_reg;

//coefficient storage (ROM inferencing template)
//Two 18x18 in one DSP block must use coefficient storage simultaneously
reg signed [coef_width-1:0] c1, c2;
reg signed [coef_width-1:0] c1_coef[2**sel_width-1:0];
reg signed [coef_width-1:0] c2_coef[2**sel_width-1:0];

initial
begin
	c1_coef[0] = 18'd0123;
	c1_coef[1] = -18'd0123;
	c1_coef[2] = 18'd4567;
	c1_coef[3] = -18'd4567;
	c1_coef[4] = 18'd8000;
	c1_coef[5] = -18'd9000;
	c1_coef[6] = 18'd0;
	c1_coef[7] = 18'd0;
	c2_coef[0] = 18'd9876;
	c2_coef[1] = -18'd9876;
	c2_coef[2] = 18'd5432;
	c2_coef[3] = -18'd5432;
	c2_coef[4] = 18'd1000;
	c2_coef[5] = -18'd1000;
	c2_coef[6] = 18'd1;
	c2_coef[7] = 18'd0;
end

always @ (c1_sel_reg)
begin
	c1 = c1_coef[c1_sel_reg];
end
always @ (c2_sel_reg)
begin
	c2 = c2_coef[c2_sel_reg];
end

//input register
always @(posedge clock or posedge reset)
begin
	if (reset == 1'b1)
	begin
		a1_reg <= 0;
		a2_reg <= 0;
		c1_sel_reg <= 0;
		c2_sel_reg <= 0;
	end
	else 
	begin
		if (ena0 == 1'b1)
		begin
			a1_reg <= a1;
			a2_reg <= a2;
		end
		if (ena1 == 1'b1)
		begin
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end
	end
end

//Pick an applicable basic mode template
sum_of_2 dsp0 (a1_reg, c1, a2_reg, c2, s);
defparam dsp0.a_width = a_width;
defparam dsp0.b_width = coef_width;

endmodule
end_template
begin_template Multiplier with One Operand from PreAdder
// Quartus Prime Verilog Template
// Multiplier with one operand from preadder
// For use with the 28-nm device families

module preadder_input(a, b, c, clock, ena, reset, s);
//This template can be used with 27x27, sum-of-2 27x27 modes on Stratix-V
//     Preadder supports 26-bit preadder (25-bit operands), c input supports 22-bit
//This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
//     Preadder supports 19(signed)/27-bit preadder (18(signed)/26-bit operands), c input supports 18/27-bit
parameter ab_width = 25;
parameter c_width = 22;
input [ab_width-1:0] a;
input [ab_width-1:0] b;
input [c_width-1:0] c;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//When preadder is used, all registered data inputs must use the same {clock, ena, reset}
input clock; 
input ena;
input reset;
output [ab_width+c_width:0] s;

reg signed [ab_width-1:0] a_reg, b_reg;
wire signed [ab_width:0] ab;
reg	[c_width-1:0] c_reg;

//Preadder
//Preadder supports static add/sub
assign ab = a_reg + b_reg;

//input register
always @(posedge clock or posedge reset)
begin
	if (reset == 1'b1)
	begin
		a_reg <= 0;
		b_reg <= 0;
		c_reg <= 0;
	end
	else 
	begin
		if (ena == 1'b1)
		begin
			a_reg <= a;
			b_reg <= b;
			c_reg <= c;
		end
	end
end

//Pick an applicable basic mode template
single_mult dsp0 (ab, c_reg, s);
defparam dsp0.a_width = ab_width+1;
defparam dsp0.b_width = c_width;

endmodule
end_template
begin_template Multiplier with One Operand from PreAdder and the Other from Coefficient ROM
// Quartus Prime Verilog Template
// Multiplier with one operand from preadder and the other from coefficient ROM
// For use with the 28-nm device families

module preadder_coef(a1, b1, c1_sel, a2, b2, c2_sel,clock, ena1, ena0, reset, s);
//This template can be used with 18x18, sum-of-2 18x18, 27x27, sum-of-2 27x27 modes on Stratix-V
//     Preadder supports 18/26-bit preadder (17/25-bit operands)
//This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
//     Preadder supports 19(signed)/27-bit preadder (18(signed)/26-bit operands)
parameter ab_width = 17;
parameter coef_width = 18;
//up to 8 coefficients (3-bit address)
parameter sel_width = 3;
input [ab_width-1:0] a1;
input [ab_width-1:0] b1;
input [ab_width-1:0] a2;
input [ab_width-1:0] b2;
input [sel_width-1:0] c1_sel;
input [sel_width-1:0] c2_sel;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//When preadder is used, all registered data inputs on each multiplier must use the same {clock, ena}
//All registered inputs must use the same reset
input clock; 
input ena1;
input ena0;
input reset;
output [ab_width+coef_width+1:0] s;

reg signed [ab_width-1:0] a1_reg, b1_reg, a2_reg, b2_reg;
wire signed [ab_width:0] ab1, ab2;
reg	[sel_width-1:0] c1_sel_reg, c2_sel_reg;

//coefficient storage (ROM inferencing template)
//Two 18x18 in one DSP block must use coefficient storage simultaneously
reg signed [coef_width-1:0] c1, c2;
reg signed [coef_width-1:0] c1_coef[2**sel_width-1:0];
reg signed [coef_width-1:0] c2_coef[2**sel_width-1:0];

initial
begin
	c1_coef[0] = 18'd0123;
	c1_coef[1] = -18'd0123;
	c1_coef[2] = 18'd4567;
	c1_coef[3] = -18'd4567;
	c1_coef[4] = 18'd8000;
	c1_coef[5] = -18'd9000;
	c1_coef[6] = 18'd0;
	c1_coef[7] = 18'd0;
	c2_coef[0] = 18'd9876;
	c2_coef[1] = -18'd9876;
	c2_coef[2] = 18'd5432;
	c2_coef[3] = -18'd5432;
	c2_coef[4] = 18'd1000;
	c2_coef[5] = -18'd1000;
	c2_coef[6] = 18'd1;
	c2_coef[7] = 18'd0;
end

always @ (c1_sel_reg)
begin
	c1 = c1_coef[c1_sel_reg];
end
always @ (c2_sel_reg)
begin
	c2 = c2_coef[c2_sel_reg];
end

//Preadder
//Preadder supports static add/sub
//Two 18x18 in one DSP block must use preadder simultaneously
//Two 18x18 in one DSP block must have the same add/sub
assign ab1 = a1_reg - b1_reg;
assign ab2 = a2_reg - b2_reg;

//input register
always @(posedge clock or posedge reset)
begin
	if (reset == 1'b1)
	begin
		a1_reg <= 0;
		a2_reg <= 0;
		b1_reg <= 0;
		b2_reg <= 0;
		c1_sel_reg <= 0;
		c2_sel_reg <= 0;
	end
	else 
	begin
		if (ena0 == 1'b1)
		begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_sel_reg <= c1_sel;
		end
		if (ena1 == 1'b1)
		begin
			a2_reg <= a2;
			b2_reg <= b2;
			c2_sel_reg <= c2_sel;
		end
	end
end

//Pick an applicable basic mode template
sum_of_2 dsp0 (ab1, c1, ab2, c2, s);
defparam dsp0.a_width = ab_width+1;
defparam dsp0.b_width = coef_width;

endmodule
end_template
begin_template Multiplier with Both Operands from the Same PreAdder
// Quartus Prime Verilog Template
// Multiplier with both operands from the same preadder
// For use with the 28-nm later device families

module square(a1, b1, a2, b2, clock, ena1, ena0, reset, s);
//This template can be used with sum-of-2 18x18 mode on Stratix-V
//     Preadder supports 18-bit preadder (17-bit operands)
parameter ab_width = 17;
input [ab_width-1:0] a1;
input [ab_width-1:0] b1;
input [ab_width-1:0] a2;
input [ab_width-1:0] b2;
//DSP supports up to 3 clock/ena pairs and 2 async reset signals
//When preadder is used, all registered data inputs on each multiplier must use the same {clock, ena}
//All registered inputs must use the same reset
input clock; 
input ena1;
input ena0;
input reset;
output [2*ab_width+1:0] s;

reg signed [ab_width-1:0] a1_reg, b1_reg, a2_reg, b2_reg;
wire signed [ab_width:0] ab1, ab2;

//Preadder
//Preadder supports static add/sub
//Two 18x18 in one DSP block must use preadder simultaneously
//Two 18x18 in one DSP block must have the same add/sub
assign ab1 = a1_reg - b1_reg;
assign ab2 = a2_reg - b2_reg;

//input register
always @(posedge clock or posedge reset)
begin
	if (reset == 1'b1)
	begin
		a1_reg <= 0;
		a2_reg <= 0;
		b1_reg <= 0;
		b2_reg <= 0;
	end
	else 
	begin
		if (ena0 == 1'b1)
		begin
			a1_reg <= a1;
			b1_reg <= b1;
		end
		if (ena1 == 1'b1)
		begin
			a2_reg <= a2;
			b2_reg <= b2;
		end
	end
end

//Pick an applicable basic mode template
sum_of_2 dsp0 (ab1, ab1, ab2, ab2, s);
defparam dsp0.a_width = ab_width+1;
defparam dsp0.b_width = ab_width+1;

endmodule
end_template
end_group
begin_group DSP Features for 20-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub and Dynamic Negate
// Quartus Prime Verilog Template
// Sum of two 18x19 multipliers with full registers (input, pipeline and output) + dynamic add/sub + dynamic negate
// Formula: final_output[t] = a1[t-4]*b1[t-4] +/- a2[t-4]*b2[t-4] +/- a3[t-3]*b3[t-3] +/- a4[t-3]*b4[t-3]
//		Note: Arria/Cyclone 10 LP ES devices do not support dynamic negate and subtraction for chainout/accumulation.  
// For use with 20-nm device families

module m18x19_sum_of_2_full_regs_dynSub_dynNegate (
	a1, b1, a2, b2, a3, b3, a4, b4,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2, 
	addnsub1, addnsub2, negate, 
	final_output
);

	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18;
	parameter B_WIDTH = 19;
	// The formula for the output width of 1 sum of two 18x19 multipliers. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1);
	// This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1;

	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2, a3, a4;
	input signed [B_WIDTH-1:0] b1, b2, b3, b4;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic addition and subtraction control signals
	input addnsub1, addnsub2, negate;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output;

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
	// Sum Of 2 Multipliers Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;

	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;

	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg;

	// Sub Input Register
	reg addnsub1_reg, addnsub2_reg;

	// Sub Pipeline Register
	reg addnsub1_pipeline_reg, addnsub2_pipeline_reg;

	// Negate Input Register
	reg negate_reg;

	// Negate Pipeline Register
	reg negate_pipeline_reg;

	// Output Register
	reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;

	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.	
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		a3_reg <= 0;
		b3_reg <= 0;
		a4_reg <= 0;
		b4_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		addnsub1_reg <= 0;
		addnsub2_reg <= 0;
		negate_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			a3_reg <= a3;
			b3_reg <= b3;
			a4_reg <= a4;
			b4_reg <= b4;
			addnsub1_reg <= addnsub1; 
			addnsub2_reg <= addnsub2; 
			negate_reg <= negate;
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// The Pipeline registers must use the same reset as the output register
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		a3_pipeline_reg <= 0;
		b3_pipeline_reg <= 0;
		a4_pipeline_reg <= 0;
		b4_pipeline_reg <= 0;
		addnsub1_pipeline_reg <= 0;
		addnsub2_pipeline_reg <= 0;
		negate_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			a3_pipeline_reg <= a3_reg;
			b3_pipeline_reg <= b3_reg;
			a4_pipeline_reg <= a4_reg;
			b4_pipeline_reg <= b4_reg;
			addnsub1_pipeline_reg <= addnsub1_reg;
			addnsub2_pipeline_reg <= addnsub2_reg;
			negate_pipeline_reg <= negate_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		final_output_reg <= 0;
	end else begin
		if (ena3) begin
			s1_output_reg <= s1;
				
			// Dynamic negate
			if (negate_pipeline_reg) begin  
				final_output_reg <= s1_output_reg - s2;
			end else begin
				final_output_reg <= s1_output_reg + s2;
			end
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	assign m3 = (a3_pipeline_reg * b3_pipeline_reg);
	assign m4 = (a4_pipeline_reg * b4_pipeline_reg);
	
	// Dynamic add/sub
	assign s1 = addnsub1_pipeline_reg ? 
					(m1 - m2) : (m1 + m2);
			
	// Dynamic add/sub
	assign s2 = addnsub2_pipeline_reg ?
					(m3 - m4) : (m3 + m4);
	
	// Final output 
	assign final_output = final_output_reg;
	

endmodule
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent
// Quartus Prime Verilog Template
// Sum of two 18x19 multipliers with full registers (input, pipeline and output) + preadder + coefficients
// Formula: final_output[t] = (a1[t-3]+b1[t-3])*c1_coef[t-3] + (a2[t-3]+b2[t-3])*c2_coef[t-3]
// Two 18x18 in one DSP block must use coefficient storage simultaneously
// For use with 20-nm device families.

module m18x19_sum_of_2_full_regs_preadd_coef (
	a1, b1, a2, b2, c1_sel, c2_sel,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	final_output
);
	
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18;
	parameter COEF_WIDTH = 18;
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3;
	// The formula for the multiplier width of one (A+B) x Coefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH;
// This example uses n=2 multipliers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1;
	
	// Data input ports
	input signed [AB_WIDTH-1:0] a1, a2;
	input signed [AB_WIDTH-1:0] b1, b2;
	
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel, c2_sel;
	
	//	Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;

	// Output signal
	// Max output width is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output;
	
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
	
	// Coefficient selection result
	reg signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2;

	// Multiplier result		
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2;
	
	// Input Register
	reg signed [AB_WIDTH-1:0] a1_reg, a2_reg;
	reg signed [AB_WIDTH-1:0] b1_reg, b2_reg;
	reg signed [SEL_WIDTH-1:0] c2_sel_reg, c1_sel_reg;
	
	// Input Pipeline Register
	reg signed [AB_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg;
	reg signed [AB_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg;
	reg signed [SEL_WIDTH-1:0] c1_sel_pipeline_reg, c2_sel_pipeline_reg;
	
	// Output Register
	reg signed [FINAL_OUTPUT_WIDTH-1:0] s_output_reg;
	
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same {clock, ena}
	// The coefficient select input may use a different clock than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		c1_sel_reg <= 0;
		c2_sel_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end
	end

	// Input pipeline register
	// All pipeline registers must use the same {clock, ena, reset}
	// The pipeline register must use the same reset as the output register
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		c1_sel_pipeline_reg <= 0;
		c2_sel_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			c1_sel_pipeline_reg <= c1_sel_reg;
			c2_sel_pipeline_reg <= c2_sel_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s_output_reg <= 0;
	end else begin
		if (ena3) begin
			// Static add/sub is supported
			s_output_reg <= m1 + m2;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline_reg + b1_pipeline_reg;
	assign ab2 = a2_pipeline_reg + b2_pipeline_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
		
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
	end

	always @ (c1_sel_pipeline_reg)
	begin
		c1_coef_wire = c1_coef[c1_sel_pipeline_reg];
	end
	always @ (c2_sel_pipeline_reg)
	begin
		c2_coef_wire = c2_coef[c2_sel_pipeline_reg];
	end
	
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
	
	// Final output
	assign final_output = s_output_reg;
	

endmodule
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
// Quartus Prime Verilog Template
// Two 'sum of two 18x19 multipliers' with full registers (input, pipeline and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-4]*b1[t-4] + a2[t-4]*b1[t-5] + a3[t-3]*b1[t-6] + a4[t-3]*b1[t-7]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
//		Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
// For use with 20-nm device families

module m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst (
	a1, b1, a2, a3, a4,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2, 
	accum, loadconst, 
	final_output
);

	// This template will only work within the AxB data width range from 2x2 to 18x19.s
	parameter A_WIDTH = 18;
	parameter B_WIDTH = 19;
	
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400;
	
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64;
	
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE";
	
	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2, a3, a4;
	input signed [B_WIDTH-1:0] b1;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, loadconst;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output;
	
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;
	
	// Data Input Cascade Delay register
	// There are two input delay registers in one DSP block: one for b1 and another for b2.
	// In 18x18_sumOf2 mode, only b2 delay register can be used. 
	reg signed [B_WIDTH-1:0] b2_delay_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg;
	
	// LOADCONST Input Register
	reg loadconst_reg;
	
	// LOADCONST Pipeline Register
	reg loadconst_pipeline_reg;
	
	// ACCUMULATE Input Register
	reg accum_reg;
	
	// ACCUMULATE Pipeline Register
	reg accum_pipeline_reg;
	
	// Summation Result and Output Register
	reg signed [A_WIDTH+B_WIDTH:0] s1_output_reg;
	wire signed [A_WIDTH+B_WIDTH:0] s2;
	
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
	
	// accumulator path
	assign acc_sel = accum_pipeline_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline_reg? PRELOAD_VALUE : 0;
	
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and Delay registers 
	// All input and delay registers must use the same reset signal, 
	// Each DATA input and delay register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		a3_reg <= 0;
		b3_reg <= 0;
		a4_reg <= 0;
		b4_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		loadconst_reg <= 0;
		accum_reg <= 0;
		// Delay register
		b2_delay_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_reg;
			a3_reg <= a3;
			b3_reg <= b2_delay_reg;
			a4_reg <= a4;
			b4_reg <= b3_reg;
			b2_delay_reg <= b2_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// Pipeline register must use the same reset as the output register
// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		a3_pipeline_reg <= 0;
		b3_pipeline_reg <= 0;
		a4_pipeline_reg <= 0;
		b4_pipeline_reg <= 0;
		loadconst_pipeline_reg <= 0;
		accum_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			a3_pipeline_reg <= a3_reg;
			b3_pipeline_reg <= b3_reg;
			a4_pipeline_reg <= a4_reg;
			b4_pipeline_reg <= b4_reg;
			loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		s_reg <= 0;
		s_double <= 0;
	end else begin
		if (ena3) begin
			// Sum of 2 multiplier. Support static add/sub b
			s1_output_reg <= m1 + m2;
			// Accumulate and chainout adder 	
			s_reg <= acc_sel + (s1_output_reg + s2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	assign m3 = (a3_pipeline_reg * b3_pipeline_reg);
	assign m4 = (a4_pipeline_reg * b4_pipeline_reg);
	
	// Sum of 2 multiplier. Support static add/sub
	assign s2 = (m3 + m4);

	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_systolic with Preadder and Coefficent
// Quartus Prime Verilog Template
// 18x19systolic with full registers (input, pipeline and output) + preadder + coefficients
// Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) + ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c4_coef
//          where (zero_bit_a+zero_bit_b)*c4_coef is a dummy multiplier
// Two 18x18 in one DSP block must use coefficient storage simultaneously
//		Note: Systolic mode do not support dynamic negate and subtraction  
// For use with 20-nm device families.

module m18x19systolic_full_regs_preadd_coef (
	a1, b1, a2, b2, a3, b3, c1_sel, c2_sel, c3_sel,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	final_output
);
	
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18;
	parameter COEF_WIDTH = 18;
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3;
	// The formula for the multiplier width of one (A+B)xCoefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH;
	// This example uses n=4 multipliers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH +3;
	
	
	// Data input ports
	input signed [AB_WIDTH-1:0] a1, a2, a3;
	input signed [AB_WIDTH-1:0] b1, b2, b3;
	
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel, c2_sel, c3_sel;
	
	//	Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;

	// Output signal
	// Max output width is 44
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output;
	
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c3_coef[2**SEL_WIDTH-1:0];
	// Extra empty Coefficient storage to fulfil even number requirement for systolic mode
	reg signed [COEF_WIDTH-1:0] c4_coef[0:0];
	
	// Coefficient selection result
	reg signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire, c3_coef_wire;
	// Extra empty coefficient to fulfil even number requirement for systolic mode
	reg signed [COEF_WIDTH-1:0] c4_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2, ab3;
	// Extra empty preadder to fulfil even number requirement for systolic mode
	wire signed [AB_WIDTH:0] ab4;

	// Multiplier result		
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2, m3, m4;
	
	// Input Register
	reg signed [AB_WIDTH-1:0] a1_reg, a2_reg, a3_reg;
	reg signed [AB_WIDTH-1:0] b1_reg, b2_reg, b3_reg;
	reg signed [SEL_WIDTH-1:0] c2_sel_reg, c1_sel_reg, c3_sel_reg;
	
	// Input Pipeline Register
	reg signed [AB_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg;
	reg signed [AB_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg;
	reg signed [SEL_WIDTH-1:0] c1_sel_pipeline_reg, c2_sel_pipeline_reg, c3_sel_pipeline_reg;
	
	// Output Register
	reg signed [FINAL_OUTPUT_WIDTH-1:0] s1_reg, s2_reg, s3_reg, s4_reg;
	
	// When this template is used, the number of multipliers has to be even
	// A dummy 0x0 multiplier can be created if the number of multipliers is odd, to make up the number to even.
	// The following is required for the dummy multiplier. 
	reg signed [AB_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline_reg /* synthesis preserve */;
	reg signed [AB_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline_reg /* synthesis preserve */;
	reg signed [SEL_WIDTH-1:0] zero_bit_c_reg, zero_bit_c_pipeline_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
	
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same {clock, ena}
	// The coefficient select input may use a different clock than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		a3_reg <= 0;
		b3_reg <= 0;
		c1_sel_reg <= 0;
		c2_sel_reg <= 0;
		c3_sel_reg <= 0;
		zero_bit_a_reg <= 0;
		zero_bit_b_reg <= 0;
		zero_bit_c_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			a3_reg <= a3;
			b3_reg <= b3;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			c3_sel_reg <= c3_sel;
			zero_bit_a_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_b_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_c_reg <= 1'b0;
		end
	end

	// Input pipeline register
	// All pipeline registers must use the same {clock, ena, reset}
	// The pipeline register must use the same reset as the output register
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		a3_pipeline_reg <= 0;
		b3_pipeline_reg <= 0;
		c1_sel_pipeline_reg <= 0;
		c2_sel_pipeline_reg <= 0;
		c3_sel_pipeline_reg <= 0;
		zero_bit_a_pipeline_reg <= 0;
		zero_bit_b_pipeline_reg <= 0;
		zero_bit_c_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			a3_pipeline_reg <= a3_reg;
			b3_pipeline_reg <= b3_reg;
			c1_sel_pipeline_reg <= c1_sel_reg;
			c2_sel_pipeline_reg <= c2_sel_reg;
			c3_sel_pipeline_reg <= c3_sel_reg;
			zero_bit_a_pipeline_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline_reg <= zero_bit_b_reg;
			zero_bit_c_pipeline_reg <= zero_bit_c_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_reg <= 0;
		s2_reg <= 0;
		s3_reg <= 0;
		s4_reg <= 0;
	end else begin
		if (ena3) begin
			// Static add/sub is supported
			s1_reg <= m1;
			s2_reg <= s1_reg + m2;
			s3_reg <= s2_reg + m3;
			s4_reg <= s3_reg + m4;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline_reg + b1_pipeline_reg;
	assign ab2 = a2_pipeline_reg + b2_pipeline_reg;
	assign ab3 = a3_pipeline_reg + b3_pipeline_reg;
	assign ab4 = zero_bit_a_pipeline_reg + zero_bit_b_pipeline_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
		
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
		
		c3_coef[0] = 18'b100101011001000110;
		c3_coef[1] = 18'b010100101111101011;
		c3_coef[2] = 18'b001001010000001010;
		c3_coef[3] = 18'b101011010101101011;
		c3_coef[4] = 18'b001000110101101010;
		c3_coef[5] = 18'b001010111000111011;
		c3_coef[6] = 18'b101010011010101010;
		c3_coef[7] = 18'b010101010101101100;
		
		// To fulfil even number requirement for systolic mode
		c4_coef[0] = 18'b000000000000000000;
	end

	always @ (c1_sel_pipeline_reg)
	begin
		c1_coef_wire = c1_coef[c1_sel_pipeline_reg];
	end
	always @ (c2_sel_pipeline_reg)
	begin
		c2_coef_wire = c2_coef[c2_sel_pipeline_reg];
	end
	always @ (c3_sel_pipeline_reg)
	begin
		c3_coef_wire = c3_coef[c3_sel_pipeline_reg];
	end
	always @ (zero_bit_c_pipeline_reg)
	begin
		c4_coef_wire = c4_coef[zero_bit_c_pipeline_reg];
	end
	
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
	assign m3 = (c3_coef_wire * ab3);
	// When this template is used, the number of multipliers has to be even
// Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	assign m4 = (c4_coef_wire * ab4);
	
	// Final output
	assign final_output = s4_reg;
	

endmodule
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
// Quartus Prime Verilog Template
// 18x19systolic with full registers (input, pipeline, systolic and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] + a3[t-6]*b1[t10] + a4[t-5]*b1[t-11] + a5[t-4]*b1[t-12] + zero_bit_a*zero_bit_b
//          where zero_bit_a*zero_bit_b is a dummy multiplier
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
//		Note: Systolic mode do not support dynamic negate and subtraction(sub)  
// For use with 20-nm device families

module m18x19systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst (
	a1, a2, a3, a4, a5, b1,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2, 
	accum, loadconst, 
	final_output
);

	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18;
	parameter B_WIDTH = 19;
	
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400;
	
	// The max chain width for systolic mode is 44. 
	parameter CHAIN_WIDTH = 44;
	
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE";
	
	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2, a3, a4, a5;
	input signed [B_WIDTH-1:0] b1;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, loadconst;
	
	// Output signal
	// Max output width for chaining is 44
	output signed [CHAIN_WIDTH-1:0] final_output;
	
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4, m5, m6;
	
	// Summation result
	reg signed [CHAIN_WIDTH-1:0] s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg, a5_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg, b5_reg;
	
	// Data Input Cascade Delay register
	// There are two input delay registers in one DSP block: one in each of the two multipliers.
	// In 18x19 systolic mode, both delay registers in a DSP block can be used.  
	reg signed [B_WIDTH-1:0] b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg, a5_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg, b5_pipeline_reg;
	
	// LOADCONST Input Register
	reg loadconst_reg;
	
	// LOADCONST Pipeline Register
	reg loadconst_pipeline_reg;
	
	// ACCUMULATE Input Register
	reg accum_reg;
	
	// ACCUMULATE Pipeline Register
	reg accum_pipeline_reg;
	
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
	
	// When this template is used, the number of multipliers has to be even
	// A dummy 0x0 multiplier can be created if the number of multipliers is odd, to make up the number to even.
	// The following is required for the dummy multiplier. 
	reg signed [A_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline_reg /* synthesis preserve */;
	reg signed [B_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
	
	// accumulator path
	assign acc_sel = accum_pipeline_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline_reg? PRELOAD_VALUE : 0;
	
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	// All input and delay registers must use the same reset signal. 
	// Each DATA input and delay register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		a3_reg <= 0;
		b3_reg <= 0;
		a4_reg <= 0;
		b4_reg <= 0;
		a5_reg <= 0;
		b5_reg <= 0;
		zero_bit_a_reg <= 0;
		zero_bit_b_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		loadconst_reg <= 0;
		accum_reg <= 0;
		// Delay register
		b1_delay_reg <= 0;
		b2_delay_reg <= 0;
		b3_delay_reg <= 0;
		b4_delay_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_delay_reg;
			a3_reg <= a3;
			b3_reg <= b2_delay_reg;
			a4_reg <= a4;
			b4_reg <= b3_delay_reg;
			a5_reg <= a5;
			b5_reg <= b4_delay_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
			b1_delay_reg <= b1_reg;
			b2_delay_reg <= b2_reg;
			b3_delay_reg <= b3_reg;
			b4_delay_reg <= b4_reg;
			// input for dummy multiplier 0x0
			zero_bit_a_reg <= {A_WIDTH{zero_bit}};
			zero_bit_b_reg <= {B_WIDTH{zero_bit}};
		end
	end
	
	//Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// Pipeline register must use the same reset as the output register
// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		a3_pipeline_reg <= 0;
		b3_pipeline_reg <= 0;
		a4_pipeline_reg <= 0;
		b4_pipeline_reg <= 0;
		a5_pipeline_reg <= 0;
		b5_pipeline_reg <= 0;
		zero_bit_a_pipeline_reg <= 0;
		zero_bit_b_pipeline_reg <= 0;
		loadconst_pipeline_reg <= 0;
		accum_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			a3_pipeline_reg <= a3_reg;
			b3_pipeline_reg <= b3_reg;
			a4_pipeline_reg <= a4_reg;
			b4_pipeline_reg <= b4_reg;
			a5_pipeline_reg <= a5_reg;
			b5_pipeline_reg <= b5_reg;
			zero_bit_a_pipeline_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline_reg <= zero_bit_b_reg;
			loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	// Even though the output registers are not explicitly declared, they will be inferred later during compilation. Thus, it is important to place the s1_output_reg-s5_output_reg operation
	// within the output register enable (i.e. ena3=1) condition. 
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		s2_output_reg <= 0;
		s3_output_reg <= 0;
		s4_output_reg <= 0;
		s5_output_reg <= 0;
		s_reg <= 0;
		s_double <= 0;
	end else begin
		if (ena3) begin
			// chainout adder support static add or sub
			// basic mult result (m) must be the second operand	
			s1_output_reg <= m1;
			s2_output_reg <= s1_output_reg + m2;
			s3_output_reg <= s2_output_reg + m3;
			s4_output_reg <= s3_output_reg + m4;
			s5_output_reg <= s4_output_reg + m5;
			// chainout accumulator only support addition when use with chainout adder
			s_reg <= acc_sel + (s5_output_reg + m6); // loopback path (acc_sel) must be the first operand
			// Double Accumulate
			s_double <= s_reg;
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	assign m3 = (a3_pipeline_reg * b3_pipeline_reg);
	assign m4 = (a4_pipeline_reg * b4_pipeline_reg);
	assign m5 = (a5_pipeline_reg * b5_pipeline_reg);
	// When this template is used, the number of multipliers has to be even
// Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	assign m6 = (zero_bit_a_pipeline_reg * zero_bit_b_pipeline_reg);
	
	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M27x27 with Dynamic Negate
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, pipeline and output) + dynamic negate
// Formula: final_output[t] = a1[t-4]*b1[t-4] +/- a2[t-3]*b2[t-3]
// 	Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
// For use with 20-nm device families

module m27x27_full_regs_dynNegate (
	a1, b1, a2, b2,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2, 
	negate, 
	final_output
);

	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27;
	parameter B_WIDTH = 27;
	// This example uses n=2 multipliers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1;
	
	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2;
	input signed [B_WIDTH-1:0] b1, b2;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic NEGATE control signals
	input negate;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output;

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg;
	
	// Negate Input Register
	reg negate_reg;

	// Negate Pipeline Register
	reg negate_pipeline_reg;
	
	// Output Register
	reg signed [A_WIDTH+B_WIDTH-1:0] m1_output_reg;
	reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;
	
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. negate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		negate_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			negate_reg <= negate;
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// The Pipeline register must use the same reset as the output register
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		negate_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			negate_pipeline_reg <= negate_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		m1_output_reg <= 0;
		final_output_reg <= 0;
	end else begin
		if (ena3) begin
			m1_output_reg <= m1;
				
			// Dynamic negate
			if (negate_pipeline_reg) begin  
				final_output_reg <= m1_output_reg - m2;
			end else begin
				final_output_reg <= m1_output_reg + m2;
			end
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	
	// Final output
	assign final_output = final_output_reg;
	

endmodule
end_template
begin_template M27x27 with Preadder and Coefficent
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, pipeline and output) + preadder + coefficients
// Formula: final_output[t] = (a[t-3]+b[t-3])*c_coef[t-3]
// For use with 20-nm device families

module m27x27_full_regs_preadd_coef (
	a, b, c_sel,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	final_output
);
	
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter AB_WIDTH = 26;
	parameter COEF_WIDTH = 27;
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3;
	
	// Data input ports
	input signed [AB_WIDTH-1:0] a;
	input signed [AB_WIDTH-1:0] b;
	
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel;
	
	//	Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output;
	
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
	
	// Coefficient selection result
	reg signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
	
	// Input Register
	reg signed [AB_WIDTH-1:0] a_reg;
	reg signed [AB_WIDTH-1:0] b_reg;
	reg signed [SEL_WIDTH-1:0] c_sel_reg;
	
	// Input Pipeline Register
	reg signed [AB_WIDTH-1:0] a_pipeline_reg;
	reg signed [AB_WIDTH-1:0] b_pipeline_reg;
	reg signed [SEL_WIDTH-1:0] c_sel_pipeline_reg;
	
	// Output Register
	reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
	
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same {clock, ena}
	// The coefficient select input may use a different clock than that of the preadder inputs.
// All registered inputs must use the same reset
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		a_reg <= 0;
		b_reg <= 0;
		c_sel_reg <= 0;
	end else begin
		if (ena1) begin
			a_reg <= a;
			b_reg <= b;
			c_sel_reg <= c_sel;
		end
	end

	// Input pipeline register
	// All pipeline registers must use the same {clock, ena, reset}
	// The Pipeline register must use the same reset as the output register
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a_pipeline_reg <= 0;
		b_pipeline_reg <= 0;
		c_sel_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a_pipeline_reg <= a_reg;
			b_pipeline_reg <= b_reg;
			c_sel_pipeline_reg <= c_sel_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		final_output_reg <= 0;
	end else begin
		if (ena3) begin
			// Static add/sub is supported
			final_output_reg <= (c_coef_wire * ab);
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline_reg + b_pipeline_reg;

	// Coefficients
	initial
	begin
		c_coef[0] = 27'b110101111001110100001010100;
		c_coef[1] = 27'b001010100111101011101010111;
		c_coef[2] = 27'b001010111111101011000100000;
		c_coef[3] = 27'b101010111111101011111111111;
		c_coef[4] = 27'b001010000011010110101101101;
		c_coef[5] = 27'b111010110000001011000011101;
		c_coef[6] = 27'b001010111111010111111110110;
		c_coef[7] = 27'b001010111111101011010111011;
		
	end

	always @ (c_sel_pipeline_reg)
	begin
		c_coef_wire = c_coef[c_sel_pipeline_reg];
	end

	// Final output
	assign final_output = final_output_reg;
	

endmodule
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, pipeline and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-4]*b1[t-4] + a2[t-3]*b1[t-4]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
//		Note: The Input Delay register is not supported in 27x27 mode. 
//		Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
// For use with 20-nm device families

module m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst (
	a1, a2, b1,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	accum, loadconst,
	final_output
);

	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27;
	parameter B_WIDTH = 27;
	
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400;
	
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64;
	
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE";
	
	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2;
	input signed [B_WIDTH-1:0] b1;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, loadconst;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output;

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg;
	
	// LOADCONST Input Register
	reg loadconst_reg;
	
	// LOADCONST Pipeline Register
	reg loadconst_pipeline_reg;
	
	// ACCUMULATE Input Register
	reg accum_reg;
	
	// ACCUMULATE Pipeline Register
	reg accum_pipeline_reg;
	
	// Output Register
	reg signed [A_WIDTH+B_WIDTH-1:0] s1_output_reg;
	
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
	
	// accumulator path
	assign acc_sel = accum_pipeline_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline_reg? PRELOAD_VALUE : 0;
	
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		loadconst_reg <= 0;
		accum_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// Pipeline register must use the same reset as the output register
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		loadconst_pipeline_reg <= 0;
		accum_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		s_reg <= 0;
		s_double <= 0;
	end else begin
		if (ena3) begin
			// First 27x27 result. Support static add/sub 
			// Addend must be the first operand 
			s1_output_reg <= m1;
			// Accumulate and chainout adder 	
			s_reg <= acc_sel + (s1_output_reg + m2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	
	// Final output
	assign final_output = s_reg;
	

endmodule
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate
// Quartus Prime Verilog Template
// 18x19_plus36 with full registers (input, pipeline and output) + dynamic add/sub + dynamic negate
// Formula: final_output[t] = ((a1[t-4]*b1[t-4])+/-c1[t-4]) +/- ((a2[t-3]*b2[t-3])+/-c2[t-3])
//		Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation.  
// For use with 20-nm device families

module m18x19plus36_full_regs_dynSub_dynNegate (
	a1, a2, b1, b2, c1, c2,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	addnsub1, addnsub2, negate,
	final_output
);

	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18;
	parameter B_WIDTH = 19;
	parameter C_WIDTH = 36;
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1);
	// This example uses n=2 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1;

	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2;
	input signed [B_WIDTH-1:0] b1, b2;
	input signed [C_WIDTH-1:0] c1, c2;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic addition and subtraction control signals
	input addnsub1, addnsub2, negate;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output;
	
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
	// 18x19_plus36 Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg;
	reg signed [C_WIDTH-1:0] c1_pipeline_reg, c2_pipeline_reg;
	
	// Sub Input Register
	reg addnsub1_reg, addnsub2_reg;
	
	// Sub Pipeline Register
	reg addnsub1_pipeline_reg, addnsub2_pipeline_reg;
	
	// Negate Input Register
	reg negate_reg;
	
	// Negate Pipeline Register
	reg negate_pipeline_reg;
	
	// Output Register 
	reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	reg signed [FINAL_OUTPUT_WIDTH-1:0] s_reg;
	
	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		c1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		c2_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		addnsub1_reg <= 0;
		addnsub2_reg <= 0;
		negate_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			addnsub1_reg <= addnsub1; 
			addnsub2_reg <= addnsub2; 
			negate_reg <= negate;
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// The Pipeline registers must use the same reset as the output register
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		c1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		c2_pipeline_reg <= 0;
		addnsub1_pipeline_reg <= 0;
		addnsub2_pipeline_reg <= 0;
		negate_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			c1_pipeline_reg <= c1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			c2_pipeline_reg <= c2_reg;
			addnsub1_pipeline_reg <= addnsub1_reg;
			addnsub2_pipeline_reg <= addnsub2_reg;
			negate_pipeline_reg <= negate_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		s_reg <= 0;
	end else begin
		if (ena3) begin
			s1_output_reg <= s1;
				
			// Dynamic negate
			if (negate_pipeline_reg) begin  
				s_reg <= s1_output_reg - s2;
			end else begin
				s_reg <= s1_output_reg + s2;
			end
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	
	// First 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s1 = addnsub1_pipeline_reg? 
					(c1_pipeline_reg - m1) : (c1_pipeline_reg + m1);
					
	// Second 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s2 = addnsub2_pipeline_reg? 
					(c2_pipeline_reg - m2) : (c2_pipeline_reg + m2);
	
	// Final output
	assign final_output = s_reg;
	

endmodule
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
// Quartus Prime Verilog Template
// Two 18x19_plus36 with full registers (input, pipeline and output) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + ((a1[t-4]*b1[t-4])+c1[t-4]) + ((a2[t-3]*b2[t-3])+c2[t-3])
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// 	Note: Input cascade chain is not supported in 18x19_plus36 mode.
// For use with 20-nm device families

module m18x19plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst (
	a1, a2, b1, b2, c1, c2,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	accum, loadconst,
	final_output
);

	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18;
	parameter B_WIDTH = 19;
	parameter C_WIDTH = 36;
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1);
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400;
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64;
	
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE";
	
	// Data input ports
	input signed [A_WIDTH-1:0] a1, a2;
	input signed [B_WIDTH-1:0] b1, b2;
	input signed [C_WIDTH-1:0] c1, c2;
	
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;
	
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, loadconst;
	
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output;
	
	// Multiplier Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] m1, m2;
	
	// Data Input Register
	reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
	
	// Data Input Pipeline Register
	reg signed [A_WIDTH-1:0] a1_pipeline_reg, a2_pipeline_reg;
	reg signed [B_WIDTH-1:0] b1_pipeline_reg, b2_pipeline_reg;
	reg signed [C_WIDTH-1:0] c1_pipeline_reg, c2_pipeline_reg;
	
	// LOADCONST Input Register
	reg loadconst_reg;
	
	// LOADCONST Pipeline Register
	reg loadconst_pipeline_reg;
	
	// ACCUMULATE Input Register
	reg accum_reg;
	
	// ACCUMULATE Pipeline Register
	reg accum_pipeline_reg;
	
	// Summation Result and Output Register
	reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	wire signed [SUM_OUTPUT_WIDTH-1:0] s2;
	
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
	
	// accumulator path
	assign acc_sel = accum_pipeline_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline_reg? PRELOAD_VALUE : 0;
	
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		// Input registers (for DATA)
		a1_reg <= 0;
		b1_reg <= 0;
		c1_reg <= 0;
		a2_reg <= 0;
		b2_reg <= 0;
		c2_reg <= 0;
		// Input registers (for DYNAMIC CONTROL SIGNAL)
		loadconst_reg <= 0;
		accum_reg <= 0;
	end else begin
		if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
	
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All pipeline registers must use the same {clock, ena, reset}
	// Pipeline register must use the same reset as the output register
// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a1_pipeline_reg <= 0;
		b1_pipeline_reg <= 0;
		c1_pipeline_reg <= 0;
		a2_pipeline_reg <= 0;
		b2_pipeline_reg <= 0;
		c2_pipeline_reg <= 0;
		loadconst_pipeline_reg <= 0;
		accum_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a1_pipeline_reg <= a1_reg;
			b1_pipeline_reg <= b1_reg;
			c1_pipeline_reg <= c1_reg;
			a2_pipeline_reg <= a2_reg;
			b2_pipeline_reg <= b2_reg;
			c2_pipeline_reg <= c2_reg;
			loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		s1_output_reg <= 0;
		s_reg <= 0;
		s_double <= 0;
	end else begin
		if (ena3) begin
			// First 18x19_plus36. Support static add/sub. 
			// Addend must be the first operand 
			s1_output_reg <= c1_pipeline_reg + m1;
			// Accumulate and chainout adder 	
			s_reg <= acc_sel + (s1_output_reg + s2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
	
	// Multiplier
	assign m1 = (a1_pipeline_reg * b1_pipeline_reg);
	assign m2 = (a2_pipeline_reg * b2_pipeline_reg);
	
	// Second 18x19_plus36. Support static add/sub
	// Addend must be the first operand
	assign s2 = c2_pipeline_reg + m2;
	
	// Final output
	assign final_output = s_reg;

endmodule
end_template
begin_template Single Multiplier with Preadder and Coefficent
// Quartus Prime Verilog Template
// m18x19_full mode by utilizing half DSP resource
// Single multiplier with full registers (input, pipeline and output) + preadder + coefficients
// Formula: final_output[t] = (a[t-3]+b[t-3])*c_coef[t-3]
//    Note: This mode does not support chainout adder, dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
// For use with 20-nm device families

module single_mult_full_regs_preadd_coef (
	a, b, c_sel,
	clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2,
	final_output
);
	
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18;
	parameter COEF_WIDTH = 18;
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3;
	
	// Data input ports
	input signed [AB_WIDTH-1:0] a;
	input signed [AB_WIDTH-1:0] b;
	
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel;
	
	//	Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, clock2, clock3, ena1, ena2, ena3, reset1, reset2;

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output;
	
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
	
	// Coefficient selection result
	reg signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
	
	// Input Register
	reg signed [AB_WIDTH-1:0] a_reg;
	reg signed [AB_WIDTH-1:0] b_reg;
	reg signed [SEL_WIDTH-1:0] c_sel_reg;
	
	// Input Pipeline Register
	reg signed [AB_WIDTH-1:0] a_pipeline_reg;
	reg signed [AB_WIDTH-1:0] b_pipeline_reg;
	reg signed [SEL_WIDTH-1:0] c_sel_pipeline_reg;
	
	// Output Register
	reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
	
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same {clock, ena}
	// The coefficient select input may use a different clock than that of the preadder inputs.
// All registered inputs must use the same reset
	always @(posedge clock1 or posedge reset1)
	if (reset1) begin
		a_reg <= 0;
		b_reg <= 0;
		c_sel_reg <= 0;
	end else begin
		if (ena1) begin
			a_reg <= a;
			b_reg <= b;
			c_sel_reg <= c_sel;
		end
	end

	// Input pipeline register
	// All pipeline registers must use the same {clock, ena, reset}
	// The Pipeline register must use the same reset as the output register
	always @(posedge clock2 or posedge reset2)
	if (reset2) begin
		a_pipeline_reg <= 0;
		b_pipeline_reg <= 0;
		c_sel_pipeline_reg <= 0;
	end else begin
		if (ena2) begin
			a_pipeline_reg <= a_reg;
			b_pipeline_reg <= b_reg;
			c_sel_pipeline_reg <= c_sel_reg;
		end
	end
	
	// Output register
	// Output register must share the same reset with input pipeline register
	always @(posedge clock3 or posedge reset2)
	if (reset2) begin
		final_output_reg <= 0;
	end else begin
		if (ena3) begin
			// Static add/sub is supported
			final_output_reg <= (c_coef_wire * ab);
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline_reg + b_pipeline_reg;

	// Coefficients
	initial
	begin
		c_coef[0] = 18'b110101111001110100;
		c_coef[1] = 18'b001010100111101011;
		c_coef[2] = 18'b001010111111101011;
		c_coef[3] = 18'b101010111111101011;
		c_coef[4] = 18'b001010000011010110;
		c_coef[5] = 18'b111010110000001011;
		c_coef[6] = 18'b001010111111010111;
		c_coef[7] = 18'b001010111111101011;
		
	end

	always @ (c_sel_pipeline_reg)
	begin
		c_coef_wire = c_coef[c_sel_pipeline_reg];
	end

	// Final output
	assign final_output = final_output_reg;
	

endmodule
end_template
end_group
begin_group DSP Features for 14-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub, Dynamic Negate and Output Chaining using SCLR
// Quartus Prime Verilog Template
// Two 'sum of 2 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + dynamic add/sub + dynamic negate + chainout adder
// Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-5]*b2[t-5] +/- a3[t-4]*b3[t-4] +/- a4[t-4]*b4[t-4]
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18, B_WIDTH = 19,
	// The formula for the output width of 1 sum of two 18x19 multipliers. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH+1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1,
	input signed [A_WIDTH-1:0] a2,
	input signed [A_WIDTH-1:0] a3,
	input signed [A_WIDTH-1:0] a4,
	input signed [B_WIDTH-1:0] b1,
	input signed [B_WIDTH-1:0] b2,
	input signed [B_WIDTH-1:0] b3, 
	input signed [B_WIDTH-1:0] b4,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 sync reset signals
	input clock1,
	input clock2,
	input clock3,
	input ena1,
	input ena2, 
	input ena3,
	input sclr1,
	input sclr2,
    
	// Dynamic addition and subtraction control signals
	input addnsub1,
	input addnsub2,
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
	// Sum Of 2 Multipliers Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;

	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg;
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg;

	// Sub Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_reg, addnsub2_reg;
	// Sub Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline1_reg, addnsub2_pipeline1_reg;
	// Sub Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline2_reg, addnsub2_pipeline2_reg;

	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;

	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;

	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal.
	// Each DATA input register may have a different pair of clock/ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers (e.g. sub and negate) can have a different clock/ena signal pair than that of the DATA input register,
	// but all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signals.
	always @(posedge clock1) begin
		if (ena1) begin
			if (sclr1) begin
				// Input registers (for DATA)
				a1_reg <= 0;
				b1_reg <= 0;
				a2_reg <= 0;
				b2_reg <= 0;
				a3_reg <= 0;
				b3_reg <= 0;
				a4_reg <= 0;
				b4_reg <= 0;
				// Input registers (for DYNAMIC CONTROL SIGNAL)
				addnsub1_reg <= 0;
				addnsub2_reg <= 0;
				negate_reg <= 0;
			end else begin
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				a4_reg <= a4;
				b4_reg <= b4;
				addnsub1_reg <= addnsub1; 
				addnsub2_reg <= addnsub2; 
				negate_reg <= negate;
			end
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same clock/ena signal pair
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed independently
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline1_reg <= 0;
				b1_pipeline1_reg <= 0;
				a2_pipeline1_reg <= 0;
				b2_pipeline1_reg <= 0;
				a3_pipeline1_reg <= 0;
				b3_pipeline1_reg <= 0;
				a4_pipeline1_reg <= 0;
				b4_pipeline1_reg <= 0;
				addnsub1_pipeline1_reg <= 0;
				addnsub2_pipeline1_reg <= 0;
				negate_pipeline1_reg <= 0;
			end else begin
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				a3_pipeline1_reg <= a3_reg;
				b3_pipeline1_reg <= b3_reg;
				a4_pipeline1_reg <= a4_reg;
				b4_pipeline1_reg <= b4_reg;
				addnsub1_pipeline1_reg <= addnsub1_reg;
				addnsub2_pipeline1_reg <= addnsub2_reg;
				negate_pipeline1_reg <= negate_reg;
			end
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same clock/ena signal pair
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed independently
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline2_reg <= 0;
				b1_pipeline2_reg <= 0;
				a2_pipeline2_reg <= 0;
				b2_pipeline2_reg <= 0;
				a3_pipeline2_reg <= 0;
				b3_pipeline2_reg <= 0;
				a4_pipeline2_reg <= 0;
				b4_pipeline2_reg <= 0;
				addnsub1_pipeline2_reg <= 0;
				addnsub2_pipeline2_reg <= 0;
				negate_pipeline2_reg <= 0;
			end else begin
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				a3_pipeline2_reg <= a3_pipeline1_reg;
				b3_pipeline2_reg <= b3_pipeline1_reg;
				a4_pipeline2_reg <= a4_pipeline1_reg;
				b4_pipeline2_reg <= b4_pipeline1_reg;
				addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
				addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with the input pipeline and second pipeline register banks
	always @(posedge clock3) begin
		if (ena3) begin
			if (sclr2) begin
				s1_output_reg <= 0;
				final_output_reg <= 0;
			end else begin
				s1_output_reg <= s1;
                
				// Dynamic negate
				if (negate_pipeline2_reg) begin  
					final_output_reg <= s1_output_reg - s2;
				end else begin
					final_output_reg <= s1_output_reg + s2;
				end
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
    
	// Dynamic add/sub
	assign s1 = addnsub1_pipeline2_reg ? 
					(m1 - m2) : (m1 + m2);
            
	// Dynamic add/sub
	assign s2 = addnsub2_pipeline2_reg ?
					(m3 - m4) : (m3 + m4);
    
	// Final output 
	assign final_output = final_output_reg;
    

endmodule
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// Sum of two 18x19 multipliers with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = (a1[t-4]+b1[t-4])*c1_coef[t-4] + (a2[t-4]+b2[t-4])*c2_coef[t-4]
// Both multiplier in one DSP block must use coefficient input simultaneously
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_preadd_coef_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3,
	// The formula for the multiplier width of one (A+B) x Coefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH,
// This example uses n=2 multipliers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a1,
	input signed [AB_WIDTH-1:0] a2,
	input signed [AB_WIDTH-1:0] b1,
	input signed [AB_WIDTH-1:0] b2,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel,
	input [SEL_WIDTH-1:0] c2_sel,
    
	//    Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2;

	// Multiplier result
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0]c2_sel_reg, c1_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0]c1_sel_pipeline1_reg, c2_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0]c1_sel_pipeline2_reg, c2_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s_output_reg;
    
	// Data Input register 
	// The DSP block supports up to 3 clock/ena pairs and 2 reset signals
	// When preadder is used, the inputs to the preadder must use the same clock/ena pair
	// The coefficient select input may use a different clock than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c1_sel_reg <= 0;
			c2_sel_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c1_sel_pipeline1_reg <= 0;
			c2_sel_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c1_sel_pipeline1_reg <= c1_sel_reg;
			c2_sel_pipeline1_reg <= c2_sel_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c1_sel_pipeline2_reg <= 0;
			c2_sel_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
			c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s_output_reg <= 0;
		end else if (ena3) begin
			// Static add/sub is supported
			s_output_reg <= m1 + m2;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline2_reg + b1_pipeline2_reg;
	assign ab2 = a2_pipeline2_reg + b2_pipeline2_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
        
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
	end

	assign c1_coef_wire = c1_coef[c1_sel_pipeline2_reg];
	assign c2_coef_wire = c2_coef[c2_sel_pipeline2_reg];
    
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
    
	// Final output
	assign final_output = s_output_reg;
    

endmodule
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
// Quartus Prime Verilog Template
// Two 'sum of two 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-5]*b1[t-6] + a3[t-4]*b1[t-7] + a4[t-4]*b1[t-8]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.s
	parameter A_WIDTH = 18, B_WIDTH = 19,
	// PRELOAD_VALUE should be a value of 2 to the power of N, where N is less than 64
	// thus it should contain only one bit '1' and '0' for others
	parameter PRELOAD_VALUE = 'h400,
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2, 
	input signed [A_WIDTH-1:0] a3, 
	input signed [A_WIDTH-1:0] a4,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 sync reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input sclr1, 
	input sclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;
    
	// Data Input Cascade Delay register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b2_delay_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg;
    
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Summation Result and Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH:0] s1_output_reg;
	wire signed [A_WIDTH+B_WIDTH:0] s2;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and Delay registers 
	// All input and delay registers must use the same reset signal, 
	// Each DATA input and delay register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1) begin 
		if (ena1) begin
			if (sclr1) begin
				// Input registers (for DATA)
				a1_reg <= 0;
				b1_reg <= 0;
				a2_reg <= 0;
				b2_reg <= 0;
				a3_reg <= 0;
				b3_reg <= 0;
				a4_reg <= 0;
				b4_reg <= 0;
				// Input registers (for DYNAMIC CONTROL SIGNAL)
				loadconst_reg <= 0;
				accum_reg <= 0;
				// Delay register
				b2_delay_reg <= 0;
			end else begin
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b1_reg;
				a3_reg <= a3;
				b3_reg <= b2_delay_reg;
				a4_reg <= a4;
				b4_reg <= b3_reg;
				b2_delay_reg <= b2_reg;
				loadconst_reg <= loadconst; 
				accum_reg <= accum; 
			end
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline1_reg <= 0;
				b1_pipeline1_reg <= 0;
				a2_pipeline1_reg <= 0;
				b2_pipeline1_reg <= 0;
				a3_pipeline1_reg <= 0;
				b3_pipeline1_reg <= 0;
				a4_pipeline1_reg <= 0;
				b4_pipeline1_reg <= 0;
				loadconst_pipeline1_reg <= 0;
				accum_pipeline1_reg <= 0;
			end else begin
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				a3_pipeline1_reg <= a3_reg;
				b3_pipeline1_reg <= b3_reg;
				a4_pipeline1_reg <= a4_reg;
				b4_pipeline1_reg <= b4_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline2_reg <= 0;
				b1_pipeline2_reg <= 0;
				a2_pipeline2_reg <= 0;
				b2_pipeline2_reg <= 0;
				a3_pipeline2_reg <= 0;
				b3_pipeline2_reg <= 0;
				a4_pipeline2_reg <= 0;
				b4_pipeline2_reg <= 0;
				loadconst_pipeline2_reg <= 0;
				accum_pipeline2_reg <= 0;
			end else begin
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				a3_pipeline2_reg <= a3_pipeline1_reg;
				b3_pipeline2_reg <= b3_pipeline1_reg;
				a4_pipeline2_reg <= a4_pipeline1_reg;
				b4_pipeline2_reg <= b4_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3) begin
		if (ena3) begin
			if (sclr2) begin
				s1_output_reg <= 0;
				s_reg <= 0;
				s_double <= 0;
			end else begin
				// Sum of 2 multiplier. Supports static add/sub
				s1_output_reg <= m1 + m2;
				// Accumulate and chainout adder
				s_reg <= acc_sel + (s1_output_reg + s2);
				// Double Accumulate
				s_double <= s_reg;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
    
	// Sum of 2 multiplier. Support static add/sub
	assign s2 = (m3 + m4);

	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_systolic with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// 18x19_systolic with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) - ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c0_coef
//          where (zero_bit_a+zero_bit_b)*c0_coef is a dummy multiplier
// When this template is used, the number of multipliers has to be even
// A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even.
// Both multipliers in one DSP block must use coefficient inputs simultaneously
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_systolic_full_regs_preadd_coef_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3,
	// The formula for the multiplier width of one (A+B)xCoefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH,
	// This example uses n=4 multipliers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 3
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a1, 
	input signed [AB_WIDTH-1:0] a2, 
	input signed [AB_WIDTH-1:0] a3,
	input signed [AB_WIDTH-1:0] b1, 
	input signed [AB_WIDTH-1:0] b2, 
	input signed [AB_WIDTH-1:0] b3,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel, 
	input [SEL_WIDTH-1:0] c2_sel, 
	input [SEL_WIDTH-1:0] c3_sel,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 44
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c3_coef[2**SEL_WIDTH-1:0];
	// Extra empty Coefficient storage to fulfil even number requirement for systolic mode
	reg signed [COEF_WIDTH-1:0] c0_coef[0:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire, c3_coef_wire;
	// Extra empty coefficient to fulfil even number requirement for systolic mode
	wire signed [COEF_WIDTH-1:0] c0_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2, ab3;
	// Extra empty preadder to fulfil even number requirement for systolic mode
	wire signed [AB_WIDTH:0] ab0;

	// Multiplier result
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2, m3, m0;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_reg, a2_reg, a3_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_reg, b2_reg, b3_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c2_sel_reg, c1_sel_reg, c3_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, c3_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, c3_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s1_reg, s2_reg, s3_reg, s0_reg;
    
	// The following is required for the dummy multiplier. 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline1_reg, zero_bit_a_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline1_reg, zero_bit_b_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] zero_bit_c_reg, zero_bit_c_pipeline1_reg, zero_bit_c_pipeline2_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
    
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same clock/ena pair
	// The coefficient select input may use a different clock than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			a3_reg <= 0;
			b3_reg <= 0;
			c1_sel_reg <= 0;
			c2_sel_reg <= 0;
			c3_sel_reg <= 0;
			zero_bit_a_reg <= 0;
			zero_bit_b_reg <= 0;
			zero_bit_c_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			a3_reg <= a3;
			b3_reg <= b3;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			c3_sel_reg <= c3_sel;
			zero_bit_a_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_b_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_c_reg <= 1'b0;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			a3_pipeline1_reg <= 0;
			b3_pipeline1_reg <= 0;
			c1_sel_pipeline1_reg <= 0;
			c2_sel_pipeline1_reg <= 0;
			c3_sel_pipeline1_reg <= 0;
			zero_bit_a_pipeline1_reg <= 0;
			zero_bit_b_pipeline1_reg <= 0;
			zero_bit_c_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			a3_pipeline1_reg <= a3_reg;
			b3_pipeline1_reg <= b3_reg;
			c1_sel_pipeline1_reg <= c1_sel_reg;
			c2_sel_pipeline1_reg <= c2_sel_reg;
			c3_sel_pipeline1_reg <= c3_sel_reg;
			zero_bit_a_pipeline1_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline1_reg <= zero_bit_b_reg;
			zero_bit_c_pipeline1_reg <= zero_bit_c_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {clock, ena, reset}
	// For systolic designs, the second pipeline register bank must use the same {clock, ena, reset} as the output register bank
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			a3_pipeline2_reg <= 0;
			b3_pipeline2_reg <= 0;
			c1_sel_pipeline2_reg <= 0;
			c2_sel_pipeline2_reg <= 0;
			c3_sel_pipeline2_reg <= 0;
			zero_bit_a_pipeline2_reg <= 0;
			zero_bit_b_pipeline2_reg <= 0;
			zero_bit_c_pipeline2_reg <= 0;
		end else if (ena3) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			a3_pipeline2_reg <= a3_pipeline1_reg;
			b3_pipeline2_reg <= b3_pipeline1_reg;
			c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
			c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
			c3_sel_pipeline2_reg <= c3_sel_pipeline1_reg;
			zero_bit_a_pipeline2_reg <= zero_bit_a_pipeline1_reg;
			zero_bit_b_pipeline2_reg <= zero_bit_b_pipeline1_reg;
			zero_bit_c_pipeline2_reg <= zero_bit_c_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s0_reg <= 0;
			s1_reg <= 0;
			s2_reg <= 0;
			s3_reg <= 0;
		end else if (ena3) begin
			s0_reg <= m0;
			// Static add/sub is supported
			s1_reg <= s0_reg + m1;
			s2_reg <= s1_reg + m2;
			s3_reg <= s2_reg - m3;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline2_reg + b1_pipeline2_reg;
	assign ab2 = a2_pipeline2_reg + b2_pipeline2_reg;
	assign ab3 = a3_pipeline2_reg + b3_pipeline2_reg;
	assign ab0 = zero_bit_a_pipeline2_reg + zero_bit_b_pipeline2_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
        
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
        
		c3_coef[0] = 18'b100101011001000110;
		c3_coef[1] = 18'b010100101111101011;
		c3_coef[2] = 18'b001001010000001010;
		c3_coef[3] = 18'b101011010101101011;
		c3_coef[4] = 18'b001000110101101010;
		c3_coef[5] = 18'b001010111000111011;
		c3_coef[6] = 18'b101010011010101010;
		c3_coef[7] = 18'b010101010101101100;
        
		// To fulfil even number requirement for systolic mode
		c0_coef[0] = 18'b000000000000000000;
	end

	assign c1_coef_wire = c1_coef[c1_sel_pipeline2_reg];
	assign c2_coef_wire = c2_coef[c2_sel_pipeline2_reg];
	assign c3_coef_wire = c3_coef[c3_sel_pipeline2_reg];
	assign c0_coef_wire = c0_coef[zero_bit_c_pipeline2_reg];
    
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
	assign m3 = (c3_coef_wire * ab3);
	// When this template is used, the number of multipliers has to be even
	// Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	assign m0 = (c0_coef_wire * ab0);
    
	// Final output
	assign final_output = s3_reg;
    

endmodule
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// 18x19_systolic with full registers (input, pipeline, systolic and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = zero_bit_a*zero_bit_b + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] - a3[t-6]*b1(t-10) + a4[t-5]*b1[t-11] + a5(t-4)*b1(t-12) + acc_sel
//          where zero_bit_a*zero_bit_b is a dummy multiplier
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// When this template is used, the number of multipliers has to be even
// A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even. 
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18, B_WIDTH = 19,
    
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
    
	// The max chain width for systolic mode is 44. 
	parameter CHAIN_WIDTH = 44,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2, 
	input signed [A_WIDTH-1:0] a3, 
	input signed [A_WIDTH-1:0] a4, 
	input signed [A_WIDTH-1:0] a5,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 44
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4, m5, m0;
    
	// Summation result
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg, a5_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg, b5_reg;
    
	// Data Input Cascade Delay register
	// There are two input delay registers in one DSP block: one in each of the two multipliers.
	// In 18x19 systolic mode, both delay registers in a DSP block can be used.  
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg;
    
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// The following is required for the dummy multiplier. 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline1_reg, zero_bit_a_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline1_reg, zero_bit_b_pipeline2_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	// All input and delay registers must use the same reset signal. 
	// Each DATA input and delay register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock/ena signal pair.
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			a3_reg <= 0;
			b3_reg <= 0;
			a4_reg <= 0;
			b4_reg <= 0;
			a5_reg <= 0;
			b5_reg <= 0;
			zero_bit_a_reg <= 0;
			zero_bit_b_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
			// Delay register
			b1_delay_reg <= 0;
			b2_delay_reg <= 0;
			b3_delay_reg <= 0;
			b4_delay_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_delay_reg;
			a3_reg <= a3;
			b3_reg <= b2_delay_reg;
			a4_reg <= a4;
			b4_reg <= b3_delay_reg;
			a5_reg <= a5;
			b5_reg <= b4_delay_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
			b1_delay_reg <= b1_reg;
			b2_delay_reg <= b2_reg;
			b3_delay_reg <= b3_reg;
			b4_delay_reg <= b4_reg;
			// input for dummy multiplier 0x0
			zero_bit_a_reg <= {A_WIDTH{zero_bit}};
			zero_bit_b_reg <= {B_WIDTH{zero_bit}};
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input ipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			a3_pipeline1_reg <= 0;
			b3_pipeline1_reg <= 0;
			a4_pipeline1_reg <= 0;
			b4_pipeline1_reg <= 0;
			a5_pipeline1_reg <= 0;
			b5_pipeline1_reg <= 0;
			zero_bit_a_pipeline1_reg <= 0;
			zero_bit_b_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			a3_pipeline1_reg <= a3_reg;
			b3_pipeline1_reg <= b3_reg;
			a4_pipeline1_reg <= a4_reg;
			b4_pipeline1_reg <= b4_reg;
			a5_pipeline1_reg <= a5_reg;
			b5_pipeline1_reg <= b5_reg;
			zero_bit_a_pipeline1_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline1_reg <= zero_bit_b_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {clock, ena, reset}
	// For systolic designs, the second pipeline register bank must use the same {clock, ena, reset} as the output register bank
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			a3_pipeline2_reg <= 0;
			b3_pipeline2_reg <= 0;
			a4_pipeline2_reg <= 0;
			b4_pipeline2_reg <= 0;
			a5_pipeline2_reg <= 0;
			b5_pipeline2_reg <= 0;
			zero_bit_a_pipeline2_reg <= 0;
			zero_bit_b_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena3) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			a3_pipeline2_reg <= a3_pipeline1_reg;
			b3_pipeline2_reg <= b3_pipeline1_reg;
			a4_pipeline2_reg <= a4_pipeline1_reg;
			b4_pipeline2_reg <= b4_pipeline1_reg;
			a5_pipeline2_reg <= a5_pipeline1_reg;
			b5_pipeline2_reg <= b5_pipeline1_reg;
			zero_bit_a_pipeline2_reg <= zero_bit_a_pipeline1_reg;
			zero_bit_b_pipeline2_reg <= zero_bit_b_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	// Even though the output registers are not explicitly declared, they will be inferred later during compilation.
	// Thus, it is important to place the s1_output_reg-s5_output_reg operation within the output register enable (i.e. ena3=1) condition. 
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s2_output_reg <= 0;
			s3_output_reg <= 0;
			s4_output_reg <= 0;
			s5_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// chainout adder supports static add/sub
			s1_output_reg <= m0;
			s2_output_reg <= s1_output_reg + m1; // the multiplication result must be the second operand
			s3_output_reg <= s2_output_reg + m2;
			s4_output_reg <= s3_output_reg - m3;
			s5_output_reg <= s4_output_reg + m4;
			// chainout accumulator only supports addition when using the chainout adder
			s_reg <= acc_sel + (s5_output_reg + m5); // loopback path (acc_sel) must be the first operand
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
	assign m5 = (a5_pipeline2_reg * b5_pipeline2_reg);
    
	// Dummy multiplier
	assign m0 = (zero_bit_a_pipeline2_reg * zero_bit_b_pipeline2_reg);
    
	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M27x27 with Dynamic Negate with Output Chaining using ACLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + dynamic negate
// Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-4]*b2[t-4]
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m27x27_full_regs_chainoutadder_dynNegate_14nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27, B_WIDTH = 27,
	// This example uses n=2 multipliers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1,
	input signed [B_WIDTH-1:0] b2,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic NEGATE control signals
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
    
	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH-1:0] m1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal.
	// Each DATA input register may have a different pair of clock/ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers (e.g. negate) can have a different clock/ena signal pair than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			negate_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			negate_reg <= negate;
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same clock/ena pair
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			negate_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			negate_pipeline1_reg <= negate_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same clock/ena pair
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			negate_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			negate_pipeline2_reg <= negate_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			m1_output_reg <= 0;
			final_output_reg <= 0;
		end else if (ena3) begin
			m1_output_reg <= m1;
                
			// Dynamic negate
			if (negate_pipeline2_reg) begin  
				final_output_reg <= m1_output_reg - m2;
			end else begin
				final_output_reg <= m1_output_reg + m2;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Final output
	assign final_output = final_output_reg;
    

endmodule
end_template
begin_template M27x27 with Preadder and Coefficent using SCLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using synchronous clear + preadder + coefficients
// Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m27x27_full_regs_preadd_coef_sclr_14nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter AB_WIDTH = 26, COEF_WIDTH = 27,
	// Select up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3
)(
	// Data input ports
	input signed [AB_WIDTH-1:0] a,
	input signed [AB_WIDTH-1:0] b,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 sync reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input sclr1, 
	input sclr2,

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
    
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same clock/ena signal pair
	// The coefficient select input may use a different clock than that of the preadder inputs.
	// All registered inputs must use the same reset
	always @(posedge clock1) begin
		if (ena1) begin
			if (sclr1) begin
				a_reg <= 0;
				b_reg <= 0;
				c_sel_reg <= 0;
			end else begin
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a_pipeline1_reg <= 0;
				b_pipeline1_reg <= 0;
				c_sel_pipeline1_reg <= 0;
			end else begin
				a_pipeline1_reg <= a_reg;
				b_pipeline1_reg <= b_reg;
				c_sel_pipeline1_reg <= c_sel_reg;
			end
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock2) begin
		if (ena2) begin
			if (sclr2) begin
				a_pipeline2_reg <= 0;
				b_pipeline2_reg <= 0;
				c_sel_pipeline2_reg <= 0;
			end else begin
				a_pipeline2_reg <= a_pipeline1_reg;
				b_pipeline2_reg <= b_pipeline1_reg;
				c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3) begin
		if (ena3) begin
			if (sclr2) begin
				final_output_reg <= 0;
			end else begin
				// Static add/sub is supported
				final_output_reg <= (c_coef_wire * ab);
			end
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline2_reg + b_pipeline2_reg;

	// Coefficients
	initial begin
		c_coef[0] = 27'b110101111001110100001010100;
		c_coef[1] = 27'b001010100111101011101010111;
		c_coef[2] = 27'b001010111111101011000100000;
		c_coef[3] = 27'b101010111111101011111111111;
		c_coef[4] = 27'b001010000011010110101101101;
		c_coef[5] = 27'b111010110000001011000011101;
		c_coef[6] = 27'b001010111111010111111110110;
		c_coef[7] = 27'b001010111111101011010111011;
	end

	assign c_coef_wire = c_coef[c_sel_pipeline2_reg];

	// Final output
	assign final_output = final_output_reg;
    

endmodule
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-4]*b1[t-5]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
//     Note: The Input Delay register is not supported in 27x27 mode. 
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27, B_WIDTH = 27,
    
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
    
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
    
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
    
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
    
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
    
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH-1:0] s1_output_reg;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// First 27x27 result. Support static add/sub 
			// Addend must be the first operand 
			s1_output_reg <= m1;
			// Accumulate and chainout adder
			s_reg <= acc_sel + (s1_output_reg + m2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Final output
	assign final_output = s_reg;
    

endmodule
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate using ACLR
// Quartus Prime Verilog Template
// 18x19_plus36 with full registers (input, pipeline and output) using asynchronous clear + dynamic add/sub + dynamic negate
// Formula: final_output[t] = ((a1[t-5]*b1[t-5])+/-c1[t-5]) +/- ((a2[t-4]*b2[t-4])+/-c2[t-4])
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_plus36_full_regs_dynSub_dynNegate_14nm #(
	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18, B_WIDTH = 19, C_WIDTH = 36,
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// This example uses n=2 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1, 
	input signed [B_WIDTH-1:0] b2,
	input signed [C_WIDTH-1:0] c1, 
	input signed [C_WIDTH-1:0] c2,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic addition and subtraction control signals
	input addnsub1, 
	input addnsub2, 
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
	// 18x19_plus36 Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline1_reg, c2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline2_reg, c2_pipeline2_reg;
    
	// Sub Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_reg, addnsub2_reg;
    
	// Sub Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline1_reg, addnsub2_pipeline1_reg;
    
	// Sub Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline2_reg, addnsub2_pipeline2_reg;
    
	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
    
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
    
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;
    
	// Output Register 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s_reg;
    
	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			c1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= 0;
			addnsub2_reg <= 0;
			negate_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			addnsub1_reg <= addnsub1; 
			addnsub2_reg <= addnsub2; 
			negate_reg <= negate;
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			c1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c2_pipeline1_reg <= 0;
			addnsub1_pipeline1_reg <= 0;
			addnsub2_pipeline1_reg <= 0;
			negate_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			c1_pipeline1_reg <= c1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c2_pipeline1_reg <= c2_reg;
			addnsub1_pipeline1_reg <= addnsub1_reg;
			addnsub2_pipeline1_reg <= addnsub2_reg;
			negate_pipeline1_reg <= negate_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed independently
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			c1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c2_pipeline2_reg <= 0;
			addnsub1_pipeline2_reg <= 0;
			addnsub2_pipeline2_reg <= 0;
			negate_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			c1_pipeline2_reg <= c1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c2_pipeline2_reg <= c2_pipeline1_reg;
			addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
			addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
			negate_pipeline2_reg <= negate_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
		end else if (ena3) begin
			s1_output_reg <= s1;
                
			// Dynamic negate
			if (negate_pipeline2_reg) begin  
				s_reg <= s1_output_reg - s2;
			end else begin
				s_reg <= s1_output_reg + s2;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// First 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s1 = addnsub1_pipeline2_reg? 
					(c1_pipeline2_reg - m1) : (c1_pipeline2_reg + m1);
                    
	// Second 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s2 = addnsub2_pipeline2_reg? 
					(c2_pipeline2_reg - m2) : (c2_pipeline2_reg + m2);
    
	// Final output
	assign final_output = s_reg;
    

endmodule
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// Two 18x19_plus36 with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + ((a1[t-5]*b1[t-5])+c1[t-5]) + ((a2[t-4]*b2[t-4])+c2[t-4])
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// Note: Input cascade chain is not supported in 18x19_plus36 mode.
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_14nm #(
	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18, B_WIDTH = 19, C_WIDTH = 36,
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
)(
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1, 
	input signed [B_WIDTH-1:0] b2,
	input signed [C_WIDTH-1:0] c1, 
	input signed [C_WIDTH-1:0] c2,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline1_reg, c2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline2_reg, c2_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
    
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
    
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
    
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
    
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Summation Result and Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	wire signed [SUM_OUTPUT_WIDTH-1:0] s2;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input must use the same reset signal, 
	// Each DATA input register may have different pair of clock and ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			c1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			c1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c2_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			c1_pipeline1_reg <= c1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c2_pipeline1_reg <= c2_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently 
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			c1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c2_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			c1_pipeline2_reg <= c1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c2_pipeline2_reg <= c2_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// First 18x19_plus36. Support static add/sub. 
			// Addend must be the first operand 
			s1_output_reg <= c1_pipeline2_reg + m1;
			// Accumulate and chainout adder
			s_reg <= acc_sel + (s1_output_reg + s2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Second 18x19_plus36. Support static add/sub
	// Addend must be the first operand
	assign s2 = c2_pipeline2_reg + m2;
    
	// Final output
	assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_full Single Multiplier with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// m18x19_full mode by utilizing half a DSP block resource
// Single multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
//    Note: This mode does not support chainout adder nor dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
// For use with 14-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_single_mult_full_regs_preadd_coef_14nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a,
	input signed [AB_WIDTH-1:0] b,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel,
    
	// Register clock and control signals
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	input clock1, 
	input clock2, 
	input clock3, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
    
	// Data Input register 
	// DSP supports up to 3 clock/ena pairs and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same clock/ena signal pair
	// The coefficient select input may use a different clock than that of the preadder inputs.
	// All registered inputs must use the same reset
	always @(posedge clock1 or posedge aclr1) begin
		if (aclr1) begin
			a_reg <= 0;
			b_reg <= 0;
			c_sel_reg <= 0;
		end else if (ena1) begin
			a_reg <= a;
			b_reg <= b;
			c_sel_reg <= c_sel;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {clock, ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a_pipeline1_reg <= 0;
			b_pipeline1_reg <= 0;
			c_sel_pipeline1_reg <= 0;
		end else if (ena2) begin
			a_pipeline1_reg <= a_reg;
			b_pipeline1_reg <= b_reg;
			c_sel_pipeline1_reg <= c_sel_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {clock, ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock2 or posedge aclr2) begin
		if (aclr2) begin
			a_pipeline2_reg <= 0;
			b_pipeline2_reg <= 0;
			c_sel_pipeline2_reg <= 0;
		end else if (ena2) begin
			a_pipeline2_reg <= a_pipeline1_reg;
			b_pipeline2_reg <= b_pipeline1_reg;
			c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock3 or posedge aclr2) begin
		if (aclr2) begin
			final_output_reg <= 0;
		end else if (ena3) begin
			// Static add/sub is supported
			final_output_reg <= (c_coef_wire * ab);
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline2_reg + b_pipeline2_reg;

	// Coefficients
	initial
	begin
		c_coef[0] = 18'b110101111001110100;
		c_coef[1] = 18'b001010100111101011;
		c_coef[2] = 18'b001010111111101011;
		c_coef[3] = 18'b101010111111101011;
		c_coef[4] = 18'b001010000011010110;
		c_coef[5] = 18'b111010110000001011;
		c_coef[6] = 18'b001010111111010111;
		c_coef[7] = 18'b001010111111101011;
        
	end

	assign c_coef_wire = c_coef[c_sel_pipeline2_reg];

	// Final output
	assign final_output = final_output_reg;
    

endmodule
end_template
end_group
begin_group DSP Features for 10-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub, Dynamic Negate and Output Chaining using SCLR
// Quartus Prime Verilog Template
// Two 'sum of 2 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + dynamic add/sub + dynamic negate + chainout adder
// Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-5]*b2[t-5] +/- a3[t-4]*b3[t-4] +/- a4[t-4]*b4[t-4]
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18, B_WIDTH = 19,
	// The formula for the output width of 1 sum of two 18x19 multipliers. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH+1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1,
	input signed [A_WIDTH-1:0] a2,
	input signed [A_WIDTH-1:0] a3,
	input signed [A_WIDTH-1:0] a4,
	input signed [B_WIDTH-1:0] b1,
	input signed [B_WIDTH-1:0] b2,
	input signed [B_WIDTH-1:0] b3, 
	input signed [B_WIDTH-1:0] b4,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 sync reset signals
	input clock,
	input ena1,
	input ena2, 
	input ena3,
	input sclr1,
	input sclr2,
    
	// Dynamic addition and subtraction control signals
	input addnsub1,
	input addnsub2,
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
	// Sum Of 2 Multipliers Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;

	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg;
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg;

	// Sub Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_reg, addnsub2_reg;
	// Sub Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline1_reg, addnsub2_pipeline1_reg;
	// Sub Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline2_reg, addnsub2_pipeline2_reg;

	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;

	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;

	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal.
	// Each DATA input register may have a different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers (e.g. sub and negate) can have a different ena signal than that of the DATA input register,
	// but all DYNAMIC CONTROL SIGNAL input registers must share the same ena signals.
	always @(posedge clock) begin
		if (ena1) begin
			if (sclr1) begin
				// Input registers (for DATA)
				a1_reg <= 0;
				b1_reg <= 0;
				a2_reg <= 0;
				b2_reg <= 0;
				a3_reg <= 0;
				b3_reg <= 0;
				a4_reg <= 0;
				b4_reg <= 0;
				// Input registers (for DYNAMIC CONTROL SIGNAL)
				addnsub1_reg <= 0;
				addnsub2_reg <= 0;
				negate_reg <= 0;
			end else begin
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				a4_reg <= a4;
				b4_reg <= b4;
				addnsub1_reg <= addnsub1; 
				addnsub2_reg <= addnsub2; 
				negate_reg <= negate;
			end
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same ena signal
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed independently
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline1_reg <= 0;
				b1_pipeline1_reg <= 0;
				a2_pipeline1_reg <= 0;
				b2_pipeline1_reg <= 0;
				a3_pipeline1_reg <= 0;
				b3_pipeline1_reg <= 0;
				a4_pipeline1_reg <= 0;
				b4_pipeline1_reg <= 0;
				addnsub1_pipeline1_reg <= 0;
				addnsub2_pipeline1_reg <= 0;
				negate_pipeline1_reg <= 0;
			end else begin
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				a3_pipeline1_reg <= a3_reg;
				b3_pipeline1_reg <= b3_reg;
				a4_pipeline1_reg <= a4_reg;
				b4_pipeline1_reg <= b4_reg;
				addnsub1_pipeline1_reg <= addnsub1_reg;
				addnsub2_pipeline1_reg <= addnsub2_reg;
				negate_pipeline1_reg <= negate_reg;
			end
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same ena signal
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed independently
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline2_reg <= 0;
				b1_pipeline2_reg <= 0;
				a2_pipeline2_reg <= 0;
				b2_pipeline2_reg <= 0;
				a3_pipeline2_reg <= 0;
				b3_pipeline2_reg <= 0;
				a4_pipeline2_reg <= 0;
				b4_pipeline2_reg <= 0;
				addnsub1_pipeline2_reg <= 0;
				addnsub2_pipeline2_reg <= 0;
				negate_pipeline2_reg <= 0;
			end else begin
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				a3_pipeline2_reg <= a3_pipeline1_reg;
				b3_pipeline2_reg <= b3_pipeline1_reg;
				a4_pipeline2_reg <= a4_pipeline1_reg;
				b4_pipeline2_reg <= b4_pipeline1_reg;
				addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
				addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with the input pipeline and second pipeline register banks
	always @(posedge clock) begin
		if (ena3) begin
			if (sclr2) begin
				s1_output_reg <= 0;
				final_output_reg <= 0;
			end else begin
				s1_output_reg <= s1;
                
				// Dynamic negate
				if (negate_pipeline2_reg) begin  
					final_output_reg <= s1_output_reg - s2;
				end else begin
					final_output_reg <= s1_output_reg + s2;
				end
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
    
	// Dynamic add/sub
	assign s1 = addnsub1_pipeline2_reg ? 
					(m1 - m2) : (m1 + m2);
            
	// Dynamic add/sub
	assign s2 = addnsub2_pipeline2_reg ?
					(m3 - m4) : (m3 + m4);
    
	// Final output 
	assign final_output = final_output_reg;
    

endmodule
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// Sum of two 18x19 multipliers with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = (a1[t-4]+b1[t-4])*c1_coef[t-4] + (a2[t-4]+b2[t-4])*c2_coef[t-4]
// Both multiplier in one DSP block must use coefficient input simultaneously
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_preadd_coef_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3,
	// The formula for the multiplier width of one (A+B) x Coefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH,
// This example uses n=2 multipliers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a1,
	input signed [AB_WIDTH-1:0] a2,
	input signed [AB_WIDTH-1:0] b1,
	input signed [AB_WIDTH-1:0] b2,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel,
	input [SEL_WIDTH-1:0] c2_sel,
    
	//    Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2;

	// Multiplier result
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c2_sel_reg, c1_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline1_reg, c2_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline2_reg, c2_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s_output_reg;
    
	// Data Input register 
	// The DSP block supports single clock and 3 ena and 2 reset signals
	// When preadder is used, the inputs to the preadder must use the same ena signal
	// The coefficient select input may use a different ena signal than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c1_sel_reg <= 0;
			c2_sel_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c1_sel_pipeline1_reg <= 0;
			c2_sel_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c1_sel_pipeline1_reg <= c1_sel_reg;
			c2_sel_pipeline1_reg <= c2_sel_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c1_sel_pipeline2_reg <= 0;
			c2_sel_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
			c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s_output_reg <= 0;
		end else if (ena3) begin
			// Static add/sub is supported
			s_output_reg <= m1 + m2;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline2_reg + b1_pipeline2_reg;
	assign ab2 = a2_pipeline2_reg + b2_pipeline2_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
        
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
	end

	assign c1_coef_wire = c1_coef[c1_sel_pipeline2_reg];
	assign c2_coef_wire = c2_coef[c2_sel_pipeline2_reg];
    
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
    
	// Final output
	assign final_output = s_output_reg;

endmodule
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
// Quartus Prime Verilog Template
// Two 'sum of two 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-5]*b1[t-6] + a3[t-4]*b1[t-7] + a4[t-4]*b1[t-8]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.s
	parameter A_WIDTH = 18, B_WIDTH = 19,
	// PRELOAD_VALUE should be a value of 2 to the power of N, where N is less than 64
	// thus it should contain only one bit '1' and '0' for others
	parameter PRELOAD_VALUE = 'h400,
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2, 
	input signed [A_WIDTH-1:0] a3, 
	input signed [A_WIDTH-1:0] a4,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 sync reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input sclr1, 
	input sclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg;
    
	// Data Input Cascade Delay register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b2_delay_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg;
    
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Summation Result and Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH:0] s1_output_reg;
	wire signed [A_WIDTH+B_WIDTH:0] s2;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and Delay registers 
	// All input and delay registers must use the same reset signal, 
	// Each DATA input and delay register may have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different ena signal than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock) begin 
		if (ena1) begin
			if (sclr1) begin
				// Input registers (for DATA)
				a1_reg <= 0;
				b1_reg <= 0;
				a2_reg <= 0;
				b2_reg <= 0;
				a3_reg <= 0;
				b3_reg <= 0;
				a4_reg <= 0;
				b4_reg <= 0;
				// Input registers (for DYNAMIC CONTROL SIGNAL)
				loadconst_reg <= 0;
				accum_reg <= 0;
				// Delay register
				b2_delay_reg <= 0;
			end else begin
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b1_reg;
				a3_reg <= a3;
				b3_reg <= b2_delay_reg;
				a4_reg <= a4;
				b4_reg <= b3_reg;
				b2_delay_reg <= b2_reg;
				loadconst_reg <= loadconst; 
				accum_reg <= accum; 
			end
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline1_reg <= 0;
				b1_pipeline1_reg <= 0;
				a2_pipeline1_reg <= 0;
				b2_pipeline1_reg <= 0;
				a3_pipeline1_reg <= 0;
				b3_pipeline1_reg <= 0;
				a4_pipeline1_reg <= 0;
				b4_pipeline1_reg <= 0;
				loadconst_pipeline1_reg <= 0;
				accum_pipeline1_reg <= 0;
			end else begin
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				a3_pipeline1_reg <= a3_reg;
				b3_pipeline1_reg <= b3_reg;
				a4_pipeline1_reg <= a4_reg;
				b4_pipeline1_reg <= b4_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline2_reg <= 0;
				b1_pipeline2_reg <= 0;
				a2_pipeline2_reg <= 0;
				b2_pipeline2_reg <= 0;
				a3_pipeline2_reg <= 0;
				b3_pipeline2_reg <= 0;
				a4_pipeline2_reg <= 0;
				b4_pipeline2_reg <= 0;
				loadconst_pipeline2_reg <= 0;
				accum_pipeline2_reg <= 0;
			end else begin
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				a3_pipeline2_reg <= a3_pipeline1_reg;
				b3_pipeline2_reg <= b3_pipeline1_reg;
				a4_pipeline2_reg <= a4_pipeline1_reg;
				b4_pipeline2_reg <= b4_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock) begin
		if (ena3) begin
			if (sclr2) begin
				s1_output_reg <= 0;
				s_reg <= 0;
				s_double <= 0;
			end else begin
				// Sum of 2 multiplier. Supports static add/sub
				s1_output_reg <= m1 + m2;
				// Accumulate and chainout adder
				s_reg <= acc_sel + (s1_output_reg + s2);
				// Double Accumulate
				s_double <= s_reg;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
    
	// Sum of 2 multiplier. Support static add/sub
	assign s2 = (m3 + m4);

	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_systolic with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// 18x19_systolic with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) - ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c0_coef
//          where (zero_bit_a+zero_bit_b)*c0_coef is a dummy multiplier
// When this template is used, the number of multipliers has to be even
// A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even.
// Both multipliers in one DSP block must use coefficient inputs simultaneously
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_systolic_full_regs_preadd_coef_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3,
	// The formula for the multiplier width of one (A+B)xCoefficient.
	parameter MULT_OUTPUT_WIDTH = (AB_WIDTH+1)+ COEF_WIDTH,
	// This example uses n=4 multipliers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 3
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a1, 
	input signed [AB_WIDTH-1:0] a2, 
	input signed [AB_WIDTH-1:0] a3,
	input signed [AB_WIDTH-1:0] b1, 
	input signed [AB_WIDTH-1:0] b2, 
	input signed [AB_WIDTH-1:0] b3,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c1_sel, 
	input [SEL_WIDTH-1:0] c2_sel, 
	input [SEL_WIDTH-1:0] c3_sel,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 44
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c1_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c2_coef[2**SEL_WIDTH-1:0];
	reg signed [COEF_WIDTH-1:0] c3_coef[2**SEL_WIDTH-1:0];
	// Extra empty Coefficient storage to fulfil even number requirement for systolic mode
	reg signed [COEF_WIDTH-1:0] c0_coef[0:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c1_coef_wire, c2_coef_wire, c3_coef_wire;
	// Extra empty coefficient to fulfil even number requirement for systolic mode
	wire signed [COEF_WIDTH-1:0] c0_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab1, ab2, ab3;
	// Extra empty preadder to fulfil even number requirement for systolic mode
	wire signed [AB_WIDTH:0] ab0;

	// Multiplier result
	wire signed [MULT_OUTPUT_WIDTH-1:0] m1, m2, m3, m0;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_reg, a2_reg, a3_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_reg, b2_reg, b3_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c2_sel_reg, c1_sel_reg, c3_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, c3_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, c3_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s1_reg, s2_reg, s3_reg, s0_reg;
    
	// The following is required for the dummy multiplier. 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline1_reg, zero_bit_a_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline1_reg, zero_bit_b_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] zero_bit_c_reg, zero_bit_c_pipeline1_reg, zero_bit_c_pipeline2_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
    
	// Data Input register 
	// DSP supports single clock, 3 ena and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same ena signal.
	// The coefficient select input may use a different ena signal than that of the preadder inputs. 
	// All registered inputs must use the same reset
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			a3_reg <= 0;
			b3_reg <= 0;
			c1_sel_reg <= 0;
			c2_sel_reg <= 0;
			c3_sel_reg <= 0;
			zero_bit_a_reg <= 0;
			zero_bit_b_reg <= 0;
			zero_bit_c_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			a3_reg <= a3;
			b3_reg <= b3;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			c3_sel_reg <= c3_sel;
			zero_bit_a_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_b_reg <= {AB_WIDTH{zero_bit}};
			zero_bit_c_reg <= 1'b0;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			a3_pipeline1_reg <= 0;
			b3_pipeline1_reg <= 0;
			c1_sel_pipeline1_reg <= 0;
			c2_sel_pipeline1_reg <= 0;
			c3_sel_pipeline1_reg <= 0;
			zero_bit_a_pipeline1_reg <= 0;
			zero_bit_b_pipeline1_reg <= 0;
			zero_bit_c_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			a3_pipeline1_reg <= a3_reg;
			b3_pipeline1_reg <= b3_reg;
			c1_sel_pipeline1_reg <= c1_sel_reg;
			c2_sel_pipeline1_reg <= c2_sel_reg;
			c3_sel_pipeline1_reg <= c3_sel_reg;
			zero_bit_a_pipeline1_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline1_reg <= zero_bit_b_reg;
			zero_bit_c_pipeline1_reg <= zero_bit_c_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {ena, reset}
	// For systolic designs, the second pipeline register bank must use the same {ena, reset} as the output register bank
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			a3_pipeline2_reg <= 0;
			b3_pipeline2_reg <= 0;
			c1_sel_pipeline2_reg <= 0;
			c2_sel_pipeline2_reg <= 0;
			c3_sel_pipeline2_reg <= 0;
			zero_bit_a_pipeline2_reg <= 0;
			zero_bit_b_pipeline2_reg <= 0;
			zero_bit_c_pipeline2_reg <= 0;
		end else if (ena3) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			a3_pipeline2_reg <= a3_pipeline1_reg;
			b3_pipeline2_reg <= b3_pipeline1_reg;
			c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
			c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
			c3_sel_pipeline2_reg <= c3_sel_pipeline1_reg;
			zero_bit_a_pipeline2_reg <= zero_bit_a_pipeline1_reg;
			zero_bit_b_pipeline2_reg <= zero_bit_b_pipeline1_reg;
			zero_bit_c_pipeline2_reg <= zero_bit_c_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s0_reg <= 0;
			s1_reg <= 0;
			s2_reg <= 0;
			s3_reg <= 0;
		end else if (ena3) begin
			s0_reg <= m0;
			// Static add/sub is supported
			s1_reg <= s0_reg + m1;
			s2_reg <= s1_reg + m2;
			s3_reg <= s2_reg - m3;
		end
	end

	// Preadder
	// Preadder supports static add/sub
	// Both 18x18 in one DSP block must use preadder simultaneously
	// Both 18x18 in one DSP block must have the same add/sub
	assign ab1 = a1_pipeline2_reg + b1_pipeline2_reg;
	assign ab2 = a2_pipeline2_reg + b2_pipeline2_reg;
	assign ab3 = a3_pipeline2_reg + b3_pipeline2_reg;
	assign ab0 = zero_bit_a_pipeline2_reg + zero_bit_b_pipeline2_reg;

	// Coefficients
	initial
	begin
		c1_coef[0] = 18'b001010111111101011;
		c1_coef[1] = 18'b001010111111101011;
		c1_coef[2] = 18'b001010110000001011;
		c1_coef[3] = 18'b001010000011101011;
		c1_coef[4] = 18'b001010111111101011;
		c1_coef[5] = 18'b001010111111101011;
		c1_coef[6] = 18'b001010100111101011;
		c1_coef[7] = 18'b110101111001110100;
        
		c2_coef[0] = 18'b001010101001000110;
		c2_coef[1] = 18'b011010111111101011;
		c2_coef[2] = 18'b001011011000001010;
		c2_coef[3] = 18'b101010100011101011;
		c2_coef[4] = 18'b001010110101101010;
		c2_coef[5] = 18'b001010110111011011;
		c2_coef[6] = 18'b011010101110101010;
		c2_coef[7] = 18'b010101011010100100;
        
		c3_coef[0] = 18'b100101011001000110;
		c3_coef[1] = 18'b010100101111101011;
		c3_coef[2] = 18'b001001010000001010;
		c3_coef[3] = 18'b101011010101101011;
		c3_coef[4] = 18'b001000110101101010;
		c3_coef[5] = 18'b001010111000111011;
		c3_coef[6] = 18'b101010011010101010;
		c3_coef[7] = 18'b010101010101101100;
        
		// To fulfil even number requirement for systolic mode
		c0_coef[0] = 18'b000000000000000000;
	end

	assign c1_coef_wire = c1_coef[c1_sel_pipeline2_reg];
	assign c2_coef_wire = c2_coef[c2_sel_pipeline2_reg];
	assign c3_coef_wire = c3_coef[c3_sel_pipeline2_reg];
	assign c0_coef_wire = c0_coef[zero_bit_c_pipeline2_reg];
    
	// Multiplier
	assign m1 = (c1_coef_wire * ab1);
	assign m2 = (c2_coef_wire * ab2);
	assign m3 = (c3_coef_wire * ab3);
	// When this template is used, the number of multipliers has to be even
	// Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	assign m0 = (c0_coef_wire * ab0);
    
	// Final output
	assign final_output = s3_reg;
    

endmodule
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// 18x19_systolic with full registers (input, pipeline, systolic and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = zero_bit_a*zero_bit_b + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] - a3[t-6]*b1(t-10) + a4[t-5]*b1[t-11] + a5(t-4)*b1(t-12) + acc_sel
//          where zero_bit_a*zero_bit_b is a dummy multiplier
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// When this template is used, the number of multipliers has to be even
// A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even. 
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter A_WIDTH = 18, B_WIDTH = 19,
    
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
    
	// The max chain width for systolic mode is 44. 
	parameter CHAIN_WIDTH = 44,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2, 
	input signed [A_WIDTH-1:0] a3, 
	input signed [A_WIDTH-1:0] a4, 
	input signed [A_WIDTH-1:0] a5,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock,
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 44
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4, m5, m0;
    
	// Summation result
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg, a5_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg, b5_reg;
    
	// Data Input Cascade Delay register
	// There are two input delay registers in one DSP block: one in each of the two multipliers.
	// In 18x19 systolic mode, both delay registers in a DSP block can be used.  
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg;
    
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// The following is required for the dummy multiplier. 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] zero_bit_a_reg, zero_bit_a_pipeline1_reg, zero_bit_a_pipeline2_reg /* synthesis preserve */;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] zero_bit_b_reg, zero_bit_b_pipeline1_reg, zero_bit_b_pipeline2_reg /* synthesis preserve */;
	wire signed zero_bit;
	soft sbz (1'b0, zero_bit);
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	// All input and delay registers must use the same reset signal. 
	// Each DATA input and delay register may have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different ena signal than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			a3_reg <= 0;
			b3_reg <= 0;
			a4_reg <= 0;
			b4_reg <= 0;
			a5_reg <= 0;
			b5_reg <= 0;
			zero_bit_a_reg <= 0;
			zero_bit_b_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
			// Delay register
			b1_delay_reg <= 0;
			b2_delay_reg <= 0;
			b3_delay_reg <= 0;
			b4_delay_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_delay_reg;
			a3_reg <= a3;
			b3_reg <= b2_delay_reg;
			a4_reg <= a4;
			b4_reg <= b3_delay_reg;
			a5_reg <= a5;
			b5_reg <= b4_delay_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
			b1_delay_reg <= b1_reg;
			b2_delay_reg <= b2_reg;
			b3_delay_reg <= b3_reg;
			b4_delay_reg <= b4_reg;
			// input for dummy multiplier 0x0
			zero_bit_a_reg <= {A_WIDTH{zero_bit}};
			zero_bit_b_reg <= {B_WIDTH{zero_bit}};
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input ipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			a3_pipeline1_reg <= 0;
			b3_pipeline1_reg <= 0;
			a4_pipeline1_reg <= 0;
			b4_pipeline1_reg <= 0;
			a5_pipeline1_reg <= 0;
			b5_pipeline1_reg <= 0;
			zero_bit_a_pipeline1_reg <= 0;
			zero_bit_b_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			a3_pipeline1_reg <= a3_reg;
			b3_pipeline1_reg <= b3_reg;
			a4_pipeline1_reg <= a4_reg;
			b4_pipeline1_reg <= b4_reg;
			a5_pipeline1_reg <= a5_reg;
			b5_pipeline1_reg <= b5_reg;
			zero_bit_a_pipeline1_reg <= zero_bit_a_reg;
			zero_bit_b_pipeline1_reg <= zero_bit_b_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// For systolic designs, the second pipeline register bank must use the same {ena, reset} as the output register bank
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			a3_pipeline2_reg <= 0;
			b3_pipeline2_reg <= 0;
			a4_pipeline2_reg <= 0;
			b4_pipeline2_reg <= 0;
			a5_pipeline2_reg <= 0;
			b5_pipeline2_reg <= 0;
			zero_bit_a_pipeline2_reg <= 0;
			zero_bit_b_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena3) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			a3_pipeline2_reg <= a3_pipeline1_reg;
			b3_pipeline2_reg <= b3_pipeline1_reg;
			a4_pipeline2_reg <= a4_pipeline1_reg;
			b4_pipeline2_reg <= b4_pipeline1_reg;
			a5_pipeline2_reg <= a5_pipeline1_reg;
			b5_pipeline2_reg <= b5_pipeline1_reg;
			zero_bit_a_pipeline2_reg <= zero_bit_a_pipeline1_reg;
			zero_bit_b_pipeline2_reg <= zero_bit_b_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	// Even though the output registers are not explicitly declared, they will be inferred later during compilation.
	// Thus, it is important to place the s1_output_reg-s5_output_reg operation within the output register enable (i.e. ena3=1) condition. 
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s2_output_reg <= 0;
			s3_output_reg <= 0;
			s4_output_reg <= 0;
			s5_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// chainout adder supports static add/sub
			s1_output_reg <= m0;
			s2_output_reg <= s1_output_reg + m1; // the multiplication result must be the second operand
			s3_output_reg <= s2_output_reg + m2;
			s4_output_reg <= s3_output_reg - m3;
			s5_output_reg <= s4_output_reg + m4;
			// chainout accumulator only supports addition when using the chainout adder
			s_reg <= acc_sel + (s5_output_reg + m5); // loopback path (acc_sel) must be the first operand
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
	assign m5 = (a5_pipeline2_reg * b5_pipeline2_reg);
    
	// Dummy multiplier
	assign m0 = (zero_bit_a_pipeline2_reg * zero_bit_b_pipeline2_reg);
    
	// Final output
assign final_output = s_reg;

endmodule
end_template
begin_template M27x27 with Dynamic Negate with Output Chaining using ACLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + dynamic negate
// Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-4]*b2[t-4]
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal

module m27x27_full_regs_chainoutadder_dynNegate_10nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27, B_WIDTH = 27,
	// This example uses n=2 multipliers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1,
	input signed [B_WIDTH-1:0] b2,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic NEGATE control signals
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
    
	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH-1:0] m1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] final_output_reg;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal.
	// Each DATA input register may have a different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers (e.g. negate) can have a different ena signal than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			negate_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b2;
			negate_reg <= negate;
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same ena signal
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			negate_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			negate_pipeline1_reg <= negate_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same ena signal
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			negate_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			negate_pipeline2_reg <= negate_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			m1_output_reg <= 0;
			final_output_reg <= 0;
		end else if (ena3) begin
			m1_output_reg <= m1;
                
			// Dynamic negate
			if (negate_pipeline2_reg) begin  
				final_output_reg <= m1_output_reg - m2;
			end else begin
				final_output_reg <= m1_output_reg + m2;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Final output
	assign final_output = final_output_reg;
    
endmodule
end_template
begin_template M27x27 with Preadder and Coefficent using SCLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using synchronous clear + preadder + coefficients
// Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m27x27_full_regs_preadd_coef_sclr_10nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter AB_WIDTH = 26, COEF_WIDTH = 27,
	// Select up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3
)(
	// Data input ports
	input signed [AB_WIDTH-1:0] a,
	input signed [AB_WIDTH-1:0] b,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 sync reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input sclr1, 
	input sclr2,

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
    
	// Data Input register 
	// DSP supports single clock, 3 ena and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same ena signal
	// The coefficient select input may use a different ena signal than that of the preadder inputs.
	// All registered inputs must use the same reset
	always @(posedge clock) begin
		if (ena1) begin
			if (sclr1) begin
				a_reg <= 0;
				b_reg <= 0;
				c_sel_reg <= 0;
			end else begin
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a_pipeline1_reg <= 0;
				b_pipeline1_reg <= 0;
				c_sel_pipeline1_reg <= 0;
			end else begin
				a_pipeline1_reg <= a_reg;
				b_pipeline1_reg <= b_reg;
				c_sel_pipeline1_reg <= c_sel_reg;
			end
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a_pipeline2_reg <= 0;
				b_pipeline2_reg <= 0;
				c_sel_pipeline2_reg <= 0;
			end else begin
				a_pipeline2_reg <= a_pipeline1_reg;
				b_pipeline2_reg <= b_pipeline1_reg;
				c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock) begin
		if (ena3) begin
			if (sclr2) begin
				final_output_reg <= 0;
			end else begin
				// Static add/sub is supported
				final_output_reg <= (c_coef_wire * ab);
			end
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline2_reg + b_pipeline2_reg;

	// Coefficients
	initial begin
		c_coef[0] = 27'b110101111001110100001010100;
		c_coef[1] = 27'b001010100111101011101010111;
		c_coef[2] = 27'b001010111111101011000100000;
		c_coef[3] = 27'b101010111111101011111111111;
		c_coef[4] = 27'b001010000011010110101101101;
		c_coef[5] = 27'b111010110000001011000011101;
		c_coef[6] = 27'b001010111111010111111110110;
		c_coef[7] = 27'b001010111111101011010111011;
	end

	assign c_coef_wire = c_coef[c_sel_pipeline2_reg];

	// Final output
	assign final_output = final_output_reg;
    
endmodule
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-4]*b1[t-5]
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
//     Note: The Input Delay register is not supported in 27x27 mode. 
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm #(
	// This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
	parameter A_WIDTH = 27, B_WIDTH = 27,
    
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
    
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);

	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
    
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
    
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
    
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
    
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH-1:0] s1_output_reg;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different ena signal than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			a2_reg <= a2;
			b2_reg <= b1_reg;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// First 27x27 result. Support static add/sub 
			// Addend must be the first operand 
			s1_output_reg <= m1;
			// Accumulate and chainout adder
			s_reg <= acc_sel + (s1_output_reg + m2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Final output
	assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate using ACLR
// Quartus Prime Verilog Template
// 18x19_plus36 with full registers (input, pipeline and output) using asynchronous clear + dynamic add/sub + dynamic negate
// Formula: final_output[t] = ((a1[t-5]*b1[t-5])+/-c1[t-5]) +/- ((a2[t-4]*b2[t-4])+/-c2[t-4])
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_plus36_full_regs_dynSub_dynNegate_10nm #(
	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18, B_WIDTH = 19, C_WIDTH = 36,
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// This example uses n=2 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
	parameter FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1, 
	input signed [B_WIDTH-1:0] b2,
	input signed [C_WIDTH-1:0] c1, 
	input signed [C_WIDTH-1:0] c2,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic addition and subtraction control signals
	input addnsub1, 
	input addnsub2, 
	input negate,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [FINAL_OUTPUT_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2;
	// 18x19_plus36 Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] s1, s2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline1_reg, c2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline2_reg, c2_pipeline2_reg;
    
	// Sub Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_reg, addnsub2_reg;
    
	// Sub Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline1_reg, addnsub2_pipeline1_reg;
    
	// Sub Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg addnsub1_pipeline2_reg, addnsub2_pipeline2_reg;
    
	// Negate Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_reg;
    
	// Negate Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline1_reg;
    
	// Negate Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg negate_pipeline2_reg;
    
	// Output Register 
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [FINAL_OUTPUT_WIDTH-1:0] s_reg;
    
	// Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	// All input registers must use the same reset signal, 
	// Each DATA input register may have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different ena signal than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			c1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= 0;
			addnsub2_reg <= 0;
			negate_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			addnsub1_reg <= addnsub1; 
			addnsub2_reg <= addnsub2; 
			negate_reg <= negate;
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			c1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c2_pipeline1_reg <= 0;
			addnsub1_pipeline1_reg <= 0;
			addnsub2_pipeline1_reg <= 0;
			negate_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			c1_pipeline1_reg <= c1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c2_pipeline1_reg <= c2_reg;
			addnsub1_pipeline1_reg <= addnsub1_reg;
			addnsub2_pipeline1_reg <= addnsub2_reg;
			negate_pipeline1_reg <= negate_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed independently
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			c1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c2_pipeline2_reg <= 0;
			addnsub1_pipeline2_reg <= 0;
			addnsub2_pipeline2_reg <= 0;
			negate_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			c1_pipeline2_reg <= c1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c2_pipeline2_reg <= c2_pipeline1_reg;
			addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
			addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
			negate_pipeline2_reg <= negate_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
		end else if (ena3) begin
			s1_output_reg <= s1;
                
			// Dynamic negate
			if (negate_pipeline2_reg) begin  
				s_reg <= s1_output_reg - s2;
			end else begin
				s_reg <= s1_output_reg + s2;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// First 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s1 = addnsub1_pipeline2_reg? 
					(c1_pipeline2_reg - m1) : (c1_pipeline2_reg + m1);
                    
	// Second 18x19_plus36
	// Dynamic add/sub
	// Addend must be the first operand 
	assign s2 = addnsub2_pipeline2_reg? 
					(c2_pipeline2_reg - m2) : (c2_pipeline2_reg + m2);
    
	// Final output
	assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
// Quartus Prime Verilog Template
// Two 18x19_plus36 with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output[t] = acc_sel + ((a1[t-5]*b1[t-5])+c1[t-5]) + ((a2[t-4]*b2[t-4])+c2[t-4])
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// Note: Input cascade chain is not supported in 18x19_plus36 mode.
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_10nm #(
	// This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	parameter A_WIDTH = 18, B_WIDTH = 19, C_WIDTH = 36,
	// The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
	parameter SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1),
	// PRELOAD_VALUE can be 2 power of N, which N should less than 64
	// PRELOAD_VALUE should contain only one bit 1
	parameter PRELOAD_VALUE = 'h400,
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
    
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
)(
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2,
	input signed [B_WIDTH-1:0] b1, 
	input signed [B_WIDTH-1:0] b2,
	input signed [C_WIDTH-1:0] c1, 
	input signed [C_WIDTH-1:0] c2,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [SUM_OUTPUT_WIDTH-1:0] m1, m2;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_reg, c2_reg;
    
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline1_reg, c2_pipeline1_reg;
    
	// Data Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [C_WIDTH-1:0] c1_pipeline2_reg, c2_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
    
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
    
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
    
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
    
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Summation Result and Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SUM_OUTPUT_WIDTH-1:0] s1_output_reg;
	wire signed [SUM_OUTPUT_WIDTH-1:0] s2;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input must use the same reset signal, 
	// Each DATA input register may have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different ena signal than that of the DATA input register.
	// However all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			// Input registers (for DATA)
			a1_reg <= 0;
			b1_reg <= 0;
			c1_reg <= 0;
			a2_reg <= 0;
			b2_reg <= 0;
			c2_reg <= 0;
			// Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= 0;
			accum_reg <= 0;
		end else if (ena1) begin
			a1_reg <= a1;
			b1_reg <= b1;
			c1_reg <= c1;
			a2_reg <= a2;
			b2_reg <= b2;
			c2_reg <= c2;
			loadconst_reg <= loadconst; 
			accum_reg <= accum; 
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline1_reg <= 0;
			b1_pipeline1_reg <= 0;
			c1_pipeline1_reg <= 0;
			a2_pipeline1_reg <= 0;
			b2_pipeline1_reg <= 0;
			c2_pipeline1_reg <= 0;
			loadconst_pipeline1_reg <= 0;
			accum_pipeline1_reg <= 0;
		end else if (ena2) begin
			a1_pipeline1_reg <= a1_reg;
			b1_pipeline1_reg <= b1_reg;
			c1_pipeline1_reg <= c1_reg;
			a2_pipeline1_reg <= a2_reg;
			b2_pipeline1_reg <= b2_reg;
			c2_pipeline1_reg <= c2_reg;
			loadconst_pipeline1_reg <= loadconst_reg;
			accum_pipeline1_reg <= accum_reg;
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the output register banks
	// The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed independently 
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a1_pipeline2_reg <= 0;
			b1_pipeline2_reg <= 0;
			c1_pipeline2_reg <= 0;
			a2_pipeline2_reg <= 0;
			b2_pipeline2_reg <= 0;
			c2_pipeline2_reg <= 0;
			loadconst_pipeline2_reg <= 0;
			accum_pipeline2_reg <= 0;
		end else if (ena2) begin
			a1_pipeline2_reg <= a1_pipeline1_reg;
			b1_pipeline2_reg <= b1_pipeline1_reg;
			c1_pipeline2_reg <= c1_pipeline1_reg;
			a2_pipeline2_reg <= a2_pipeline1_reg;
			b2_pipeline2_reg <= b2_pipeline1_reg;
			c2_pipeline2_reg <= c2_pipeline1_reg;
			loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
			accum_pipeline2_reg <= accum_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			s1_output_reg <= 0;
			s_reg <= 0;
			s_double <= 0;
		end else if (ena3) begin
			// First 18x19_plus36. Support static add/sub. 
			// Addend must be the first operand 
			s1_output_reg <= c1_pipeline2_reg + m1;
			// Accumulate and chainout adder
			s_reg <= acc_sel + (s1_output_reg + s2);
			// Double Accumulate
			s_double <= s_reg;
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
    
	// Second 18x19_plus36. Support static add/sub
	// Addend must be the first operand
	assign s2 = c2_pipeline2_reg + m2;
    
	// Final output
	assign final_output = s_reg;

endmodule
end_template
begin_template M18x19_full Single Multiplier with Preadder and Coefficent using ACLR
// Quartus Prime Verilog Template
// m18x19_full mode by utilizing half a DSP block resource
// Single multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
// Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
//    Note: This mode does not support chainout adder nor dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m18x19_single_mult_full_regs_preadd_coef_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 18x19.
	parameter AB_WIDTH = 18, COEF_WIDTH = 18,
	// up to 8 coefficients (3-bit address)
	parameter SEL_WIDTH = 3
) (
	// Data input ports
	input signed [AB_WIDTH-1:0] a,
	input signed [AB_WIDTH-1:0] b,
    
	// Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
	input [SEL_WIDTH-1:0] c_sel,
    
	// Register clock and control signals
	// DSP supports single clock, 3 ena and 2 async reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input aclr1, 
	input aclr2,

	// Output signal
	// Max output width is 64
	output signed [AB_WIDTH+COEF_WIDTH:0] final_output
);
    
	// Coefficient storage (ROM inferencing template)
	reg signed [COEF_WIDTH-1:0] c_coef[2**SEL_WIDTH-1:0];
    
	// Coefficient selection result
	wire signed [COEF_WIDTH-1:0] c_coef_wire;

	// Preadder result
	wire signed [AB_WIDTH:0] ab;
    
	// Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_reg;
    
	// Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline1_reg;
    
	// Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] a_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH-1:0] b_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [SEL_WIDTH-1:0] c_sel_pipeline2_reg;
    
	// Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [AB_WIDTH+COEF_WIDTH:0] final_output_reg;
    
	// Data Input register 
	// DSP supports single clock, 3 ena and 2 async reset signals
	// When preadder is used, the inputs to the preadder must use the same ena signal
	// The coefficient select input may use a different ena signal than that of the preadder inputs.
	// All registered inputs must use the same reset
	always @(posedge clock or posedge aclr1) begin
		if (aclr1) begin
			a_reg <= 0;
			b_reg <= 0;
			c_sel_reg <= 0;
		end else if (ena1) begin
			a_reg <= a;
			b_reg <= b;
			c_sel_reg <= c_sel;
		end
	end

	// Input pipeline register
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a_pipeline1_reg <= 0;
			b_pipeline1_reg <= 0;
			c_sel_pipeline1_reg <= 0;
		end else if (ena2) begin
			a_pipeline1_reg <= a_reg;
			b_pipeline1_reg <= b_reg;
			c_sel_pipeline1_reg <= c_sel_reg;
		end
	end

	// Second pipeline register
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			a_pipeline2_reg <= 0;
			b_pipeline2_reg <= 0;
			c_sel_pipeline2_reg <= 0;
		end else if (ena2) begin
			a_pipeline2_reg <= a_pipeline1_reg;
			b_pipeline2_reg <= b_pipeline1_reg;
			c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock or posedge aclr2) begin
		if (aclr2) begin
			final_output_reg <= 0;
		end else if (ena3) begin
			// Static add/sub is supported
			final_output_reg <= (c_coef_wire * ab);
		end
	end

	// Preadder
	// Preadder supports static add/sub
	assign ab = a_pipeline2_reg + b_pipeline2_reg;

	// Coefficients
	initial
	begin
		c_coef[0] = 18'b110101111001110100;
		c_coef[1] = 18'b001010100111101011;
		c_coef[2] = 18'b001010111111101011;
		c_coef[3] = 18'b101010111111101011;
		c_coef[4] = 18'b001010000011010110;
		c_coef[5] = 18'b111010110000001011;
		c_coef[6] = 18'b001010111111010111;
		c_coef[7] = 18'b001010111111101011;
        
	end

	assign c_coef_wire = c_coef[c_sel_pipeline2_reg];

	// Final output
	assign final_output = final_output_reg;
    

endmodule
end_template
begin_template M9x9_sumof4 with Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
// Quartus Prime Verilog Template
// Two 'sum of four 9x9 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + chainout adder + accumulate + double accumulate + preload constant
// Formula: final_output = acc_sel + (a1[t-5]*b1[t-5]) + (a2[t-5]*b2[t-5]) + (a3[t-5]*b3[t-5]) + (a4[t-5]*b4[t-5]) + (a5[t-4]*b5[t-4]) + (a6[t-4]*b6[t-4]) + (a7[t-4]*b7[t-4]) + (a8[t-4]*b8[t-4])
//          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
// For use with 10-nm device families
// All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
// When synchronous clear is used, the ena signal has a higher priority than the clear signal
// Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

module m9x9_sum_of_4_full_regs_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm #(
	// This template will only work within the AxB data width range from 2x2 to 9x9.s
	parameter A_WIDTH = 9, B_WIDTH = 9,
	// PRELOAD_VALUE should be a value of 2 to the power of N, where N is less than 64
	// thus it should contain only one bit '1' and '0' for others
	parameter PRELOAD_VALUE = 'h400,
	// Maximum chain width is 64
	parameter CHAIN_WIDTH = 64,
	// Double accumulation enable control parameter
	parameter enable_double_accum = "TRUE"
) (
	// Data input ports
	input signed [A_WIDTH-1:0] a1, 
	input signed [A_WIDTH-1:0] a2, 
	input signed [A_WIDTH-1:0] a3, 
	input signed [A_WIDTH-1:0] a4,
	input signed [A_WIDTH-1:0] a5, 
	input signed [A_WIDTH-1:0] a6, 
	input signed [A_WIDTH-1:0] a7, 
	input signed [A_WIDTH-1:0] a8,   
	input signed [B_WIDTH-1:0] b1,
	input signed [B_WIDTH-1:0] b2,
	input signed [B_WIDTH-1:0] b3, 
	input signed [B_WIDTH-1:0] b4,
	input signed [B_WIDTH-1:0] b5,
	input signed [B_WIDTH-1:0] b6,
	input signed [B_WIDTH-1:0] b7, 
	input signed [B_WIDTH-1:0] b8,
    
	// Register clock and control signals
	// DSP supports single clock with 3 ena, and 2 sync reset signals
	input clock, 
	input ena1, 
	input ena2, 
	input ena3, 
	input sclr1, 
	input sclr2,
    
	// Dynamic ACCUMULATE and LOADCONST control signals
	input accum, 
	input loadconst,
    
	// Output signal
	// Max output width for chaining is 64
	output signed [CHAIN_WIDTH-1:0] final_output
);
    
	// Multiplier Result
	wire signed [A_WIDTH+B_WIDTH-1:0] m1, m2, m3, m4, m5, m6, m7, m8;
    
	// Data Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, a6_reg, a7_reg, a8_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, b6_reg, b7_reg, b8_reg;
       
	// Data Input Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg, a6_pipeline1_reg, a7_pipeline1_reg, a8_pipeline1_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg, b6_pipeline1_reg, b7_pipeline1_reg, b8_pipeline1_reg;
    
	// Data Input Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH-1:0] a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg, a6_pipeline2_reg, a7_pipeline2_reg, a8_pipeline2_reg;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [B_WIDTH-1:0] b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg, b6_pipeline2_reg, b7_pipeline2_reg, b8_pipeline2_reg;
    
	// LOADCONST Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_reg;
	// LOADCONST Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline1_reg;
	// LOADCONST Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg loadconst_pipeline2_reg;
    
	// ACCUMULATE Input Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_reg;
	// ACCUMULATE Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline1_reg;
	// ACCUMULATE Second Pipeline Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg accum_pipeline2_reg;
    
	// Summation Result and Output Register
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [A_WIDTH+B_WIDTH+1:0] s1_output_reg;
	wire signed [A_WIDTH+B_WIDTH+1:0] s2;
    
	// Accumulate, double acc
	wire signed [CHAIN_WIDTH-1:0] selected_value, select_feedback;
	wire signed [CHAIN_WIDTH-1:0] acc_sel;
	(* altera_attribute = {" -name SYNCHRONIZER_IDENTIFICATION OFF "} *) reg signed [CHAIN_WIDTH-1:0] s_reg, s_double;
    
	// accumulator path
	assign acc_sel = accum_pipeline2_reg ? select_feedback : selected_value;
	assign select_feedback = (enable_double_accum == "TRUE")? s_double: s_reg;
	assign selected_value = loadconst_pipeline2_reg? PRELOAD_VALUE : 0;
    
	// Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input registers must use the same reset signal, 
	// Each DATA input must have different ena signal.
	// The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different ena signal than that of the DATA input register.
	// But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.
	always @(posedge clock) begin 
		if (ena1) begin
			if (sclr1) begin
				// Input registers (for DATA)
				a1_reg <= 0;
				b1_reg <= 0;
				a2_reg <= 0;
				b2_reg <= 0;
				a3_reg <= 0;
				b3_reg <= 0;
				a4_reg <= 0;
				b4_reg <= 0;
				a5_reg <= 0;
				b5_reg <= 0;
				a6_reg <= 0;
				b6_reg <= 0;
				a7_reg <= 0;
				b7_reg <= 0;
				a8_reg <= 0;
				b8_reg <= 0;
				// Input registers (for DYNAMIC CONTROL SIGNAL)
				loadconst_reg <= 0;
				accum_reg <= 0;
			end else begin
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				a4_reg <= a4;
				b4_reg <= b4;
				a5_reg <= a5;
				b5_reg <= b5;
				a6_reg <= a6;
				b6_reg <= b6;
				a7_reg <= a7;
				b7_reg <= b7;
				a8_reg <= a8;
				b8_reg <= b8;
				loadconst_reg <= loadconst; 
				accum_reg <= accum; 
			end
		end
	end
    
	// Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All input pipeline registers must use the same {ena, reset}
	// The input pipeline register bank must use the same reset as the second pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline1_reg <= 0;
				b1_pipeline1_reg <= 0;
				a2_pipeline1_reg <= 0;
				b2_pipeline1_reg <= 0;
				a3_pipeline1_reg <= 0;
				b3_pipeline1_reg <= 0;
				a4_pipeline1_reg <= 0;
				b4_pipeline1_reg <= 0;
				a5_pipeline1_reg <= 0;
				b5_pipeline1_reg <= 0;
				a6_pipeline1_reg <= 0;
				b6_pipeline1_reg <= 0;
				a7_pipeline1_reg <= 0;
				b7_pipeline1_reg <= 0;
				a8_pipeline1_reg <= 0;
				b8_pipeline1_reg <= 0;
				loadconst_pipeline1_reg <= 0;
				accum_pipeline1_reg <= 0;
			end else begin
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				a3_pipeline1_reg <= a3_reg;
				b3_pipeline1_reg <= b3_reg;
				a4_pipeline1_reg <= a4_reg;
				b4_pipeline1_reg <= b4_reg;
				a5_pipeline1_reg <= a5_reg;
				b5_pipeline1_reg <= b5_reg;
				a6_pipeline1_reg <= a6_reg;
				b6_pipeline1_reg <= b6_reg;
				a7_pipeline1_reg <= a7_reg;
				b7_pipeline1_reg <= b7_reg;
				a8_pipeline1_reg <= a8_reg;
				b8_pipeline1_reg <= b8_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end
		end
	end
    
	// Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	// All second pipeline registers must use the same {ena, reset}
	// The second pipeline register bank must use the same reset as the input pipeline and output register banks
	// The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed independently 
	always @(posedge clock) begin
		if (ena2) begin
			if (sclr2) begin
				a1_pipeline2_reg <= 0;
				b1_pipeline2_reg <= 0;
				a2_pipeline2_reg <= 0;
				b2_pipeline2_reg <= 0;
				a3_pipeline2_reg <= 0;
				b3_pipeline2_reg <= 0;
				a4_pipeline2_reg <= 0;
				b4_pipeline2_reg <= 0;
 				a5_pipeline2_reg <= 0;
				b5_pipeline2_reg <= 0;
				a6_pipeline2_reg <= 0;
				b6_pipeline2_reg <= 0;
				a7_pipeline2_reg <= 0;
				b7_pipeline2_reg <= 0;
				a8_pipeline2_reg <= 0;
				b8_pipeline2_reg <= 0;              
				loadconst_pipeline2_reg <= 0;
				accum_pipeline2_reg <= 0;
			end else begin
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				a3_pipeline2_reg <= a3_pipeline1_reg;
				b3_pipeline2_reg <= b3_pipeline1_reg;
				a4_pipeline2_reg <= a4_pipeline1_reg;
				b4_pipeline2_reg <= b4_pipeline1_reg;
				a5_pipeline2_reg <= a5_pipeline1_reg;
				b5_pipeline2_reg <= b5_pipeline1_reg;
				a6_pipeline2_reg <= a6_pipeline1_reg;
				b6_pipeline2_reg <= b6_pipeline1_reg;
				a7_pipeline2_reg <= a7_pipeline1_reg;
				b7_pipeline2_reg <= b7_pipeline1_reg;
				a8_pipeline2_reg <= a8_pipeline1_reg;
				b8_pipeline2_reg <= b8_pipeline1_reg;             
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end
		end
	end
    
	// Output register
	// The output register bank must share the same reset with input pipeline and second pipeline register banks
	always @(posedge clock) begin
		if (ena3) begin
			if (sclr2) begin
				s1_output_reg <= 0;
				s_reg <= 0;
				s_double <= 0;
			end else begin
				// Sum of 4 multipliers. Supports static add/sub
				s1_output_reg <= m1 + m2 + m3 + m4;
				// Accumulate and chainout adder
				s_reg <= acc_sel + (s1_output_reg + s2);
				// Double Accumulate
				s_double <= s_reg;
			end
		end
	end
    
	// Multiplier
	assign m1 = (a1_pipeline2_reg * b1_pipeline2_reg);
	assign m2 = (a2_pipeline2_reg * b2_pipeline2_reg);
	assign m3 = (a3_pipeline2_reg * b3_pipeline2_reg);
	assign m4 = (a4_pipeline2_reg * b4_pipeline2_reg);
	assign m5 = (a5_pipeline2_reg * b5_pipeline2_reg);
	assign m6 = (a6_pipeline2_reg * b6_pipeline2_reg);
	assign m7 = (a7_pipeline2_reg * b7_pipeline2_reg);
	assign m8 = (a8_pipeline2_reg * b8_pipeline2_reg);
    
	// Sum of 4 multipliers. Support static add/sub
	assign s2 = (m5 + m6 + m7 + m8);

	// Final output
assign final_output = s_reg;

endmodule
end_template
end_group
end_group
end_group
begin_group Pipelining
begin_template Hyper-Pipelining Module
// Quartus Prime Verilog Template
//
// Hyper-Pipelining Module

(* altera_attribute = "-name AUTO_SHIFT_REGISTER_RECOGNITION off" *) 
module hyperpipe 
#(parameter CYCLES = 1, parameter WIDTH = 1) 
(
	input clk,
	input [WIDTH-1:0] din,
	output [WIDTH-1:0] dout
);

	generate if (CYCLES==0) begin : GEN_COMB_INPUT
		assign dout = din;
	end 
	else begin : GEN_REG_INPUT  
		integer i;
		reg [WIDTH-1:0] R_data [CYCLES-1:0];
        
		always @ (posedge clk) 
		begin   
			R_data[0] <= din;      
			for(i = 1; i < CYCLES; i = i + 1) 
            	R_data[i] <= R_data[i-1];
		end
		assign dout = R_data[CYCLES-1];
	end
	endgenerate  

endmodule
end_template
begin_template Hyper-Pipelining Module Instantiation
// Quartus Prime Verilog Template
//
// Hyper-Pipelining Module Instantiation

hyperpipe # (
		.CYCLES  ( ),
		.WIDTH   ( )
	) hp (
		.clk      ( ),
		.din      ( ),
		.dout     ( )
	);
end_template
begin_template Hyper-Pipelining Variable Latency Module
// Quartus Prime Verilog Template
//
// Hyper-Pipelining Variable Latency Module

module hyperpipe_vlat 
#(parameter WIDTH = 1, parameter MAX_PIPE = 100) // Valid range for MAX_PIPE: 0 to 100 inclusive
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
	reg [WIDTH-1:0] vlat_r /* synthesis preserve */;

	always @ (posedge clk) begin
		vlat_r <= din;
	end

	assign dout = vlat_r;

endmodule
end_template
begin_template Hyper-Pipelining Variable Latency Instantiation
// Quartus Prime Verilog Template
//
// Hyper-Pipelining Variable Latency Module Instantiation

hyperpipe_vlat # (
		.WIDTH    ( ),
		.MAX_PIPE ( )  // Valid range: 0 to 100 inclusive
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
begin_template Module Declaration (style1)
module <module_name>

#(
	// Parameter Declarations
	parameter <param_name> = <default_value>,
	parameter [<msb>:<lsb>] <param_name> = <default_value>,
	parameter signed [<msb>:<lsb>] <param_name> = <default_value>
	...
)

(
	// Input Ports
	input <port_name>,
	input wire <port_name>,
	input [<msb>:<lsb>] <port_name>,
	input signed [<msb>:<lsb>] <port_name>,
	...

	// Output Ports
	output <port_name>,
	output [<msb>:<lsb>] <port_name>,
	output reg [<msb>:<lsb>] <port_name>,
	output signed [<msb>:<lsb>] <port_name>,
	output reg signed [<msb>:<lsb>] <port_name>,
	...

	// Inout Ports
	inout <port_name>,
	inout [<msb>:<lsb>] <port_name>,
	inout signed [<msb>:<lsb>] <port_name>
	...
);

	// Module Item(s)

endmodule
end_template
begin_template Module Declaration (style2)
module <module_name>(<port_name>, <port_name>, ...);

	// Input Port(s)
	input <port_name>;
	input wire <port_name>;
	input [<msb>:<lsb>] <port_name>;
	input signed [<msb>:<lsb>] <port_name>;
	...

	// Output Port(s)
	output <port_name>;
	output [<msb>:<lsb>] <port_name>;
	output reg [<msb>:<lsb>] <port_name>;
	output signed [<msb>:<lsb>] <port_name>;
	output reg signed [<msb>:<lsb>] <port_name>;
	...

	// Inout Port(s)
	inout <port_name>;
	inout [<msb>:<lsb>] <port_name>;
	inout signed [<msb>:<lsb>] <port_name>;
	...

	// Parameter Declaration(s)
	parameter <param_name> = <default_value>;
	parameter [<msb>:<lsb>] <param_name> = <default_value>;
	parameter signed [<msb>:<lsb>] <param_name> = <default_value>;
	...

	// Additional Module Item(s)

endmodule
end_template
end_group
begin_group Declarations
begin_template Net Declaration
// A net models connectivity in a design.

// Scalar net
wire <net_name>;

// Scalar net with a declaration assignment.  This assignment is 
// equivalent to a separate continuous assignment to the net.
wire <net_name> = <declaration_assignment>;

// Unsigned vector 
wire [<msb>:<lsb>] <net_name>;

// Nets may be declared with many different types with different
// electrical characteristics:

// wire/tri          Basic connection w/ typical electrical behavior

// supply1/supply0   Tied to VCC/GND 

// tri1/tri0         Default to 1/0 if left undriven

// wor/trior         Multiple drivers resolved by OR

// wand/triand       Multiple drivers resolved by AND
end_template
begin_template Variable Declaration
// A variable stores a value.  It may be assigned a value in a 
// sequential block but not in a continous assignment.  Variables
// may be referenced in most expressions, except for expressions
// in port connections to module inout and output ports.

// NOTE: reg is a type of variable that models both combinational
// or sequential logic.  It does not indicate that Quartus Prime 
// should infer a hardware register, which occurs when an variable
// is assigned a value inside an edge-controlled always construct.  

// Scalar reg
reg <variable_name>;

// Scalar reg with initial value.  If the variable has no assigned value,
// Quartus Prime Integrated Synthesis will use the initial value.  Integrated 
// Synthesis will also infer power-up conditions for registers and memories 
// from the initial value. 
reg <variable_name> = <initial_value>;

// Unsigned vector
reg [<msb>:<lsb>] <variable_name>;
reg [<msb>:<lsb>] <variable_name> = <initial_value>;

// Signed vector
reg signed [<msb>:<lsb>] <variable_name>;
reg signed [<msb>:<lsb>] <variable_name> = <initial_value>;

// 2-D array.
reg [<msb>:<lsb>] <variable_name>[<msb>:<lsb>];

// 32-bit signed integer
integer <variable_name>;
end_template
begin_template Function Declaration
// A function must declare one or more input arguments.  It must also
// execute in a single simulation cycle; therefore, it cannot contain
// timing controls or tasks.  You set the return value of a 
// function by assigning to the function name as if it were a variable.

function <func_return_type> <func_name>(<input_arg_decls>);
	// Optional Block Item Declarations, e.g. Local Variables
	begin
		// Statements    
	end
endfunction
end_template
begin_template Task Declaration
// A task may have input, output, and inout arguments.  It may also
// contain timing controls.  A task does not return a value and, thus, 
// may not be used in an expression.

task <task_name>(<arg_decls>);
	// Optional Block Item Declarations, e.g Local Variables
	begin
		// Statements
	end
endtask
end_template
begin_template Genvar Declaration
// A genvar is a signed integer object that functions as a loop variable
// in generate-for loops.  As a result, it may only be assigned a value
// inside the initial and step conditions of a generate-for.  It must be
// assigned a constant expression value and should only be referenced
// inside the scope of the generate-for.

genvar <genvar_id>;
genvar <genvar_id1>, <genvar_id2>, ... <genvar_idN>;
end_template
end_group
begin_group Module Items
begin_template Continuous Assignment
// The left-hand side of a continuous assignment must be a structural
// net expression.  That is, it must be a net or a concatentation of
// nets, and any index expressions must be constant.

assign <net_lvalue> = <value>;
end_template
begin_template Always Construct (Combinational)
always@(*)
begin
	// Statements
end
end_template
begin_template Always Construct (Sequential)
// <edge_events> may contain any number of posedge or negedge events
// separated by "or", e.g. always@(posedge clk or negedge reset)
always@(<edge_events>)
begin
	// Statements
end
end_template
begin_template Module Instantiations
// Basic module instantiation
<module_name> <inst_name>(<port_connects>);

// Module instantiation with parameter overrides
<module_name> #(<parameters>) <inst_name>(<port_connects>);

// Array of instances
<module_name> #(<parameters) <inst_name> [<msb>:<lsb>](<port_connects>);
end_template
begin_group Generates
begin_template Complete Generate Example
module generate_design #(parameter N = 4) (input a, b, output [N * (N + 1) / 2 - 1 : 0] o);

genvar i, j;
generate for(i = 0; i < N; i = i + 1) begin: GEN_LOOP
	// Width of q on each iteration depends on genvar i
	(* keep = 1*) reg [i:0] q;

	if(i % 2 == 0) begin: GEN_TRUE
		always@* q[i:0] = {i+1{a}};
	end
	else begin : GEN_FALSE
		always@* q[i:0] = {i+1{b}};
	end
end
endgenerate

generate for(j = 0; j < N; j = j + 1) begin : GEN_LOOP2

	assign o[j + j * (j + 1) / 2 : 0 + j * (j + 1) / 2] = GEN_LOOP[j].q[j:0];
end
endgenerate
endmodule
end_template
begin_template Hierarchical Names in Generate constructs
module generate_labels(input [1:0] a, b, output [1:0] o1, o2);
genvar i;

generate for(i = 0; i < 2; i = i + 1) begin: GEN_LOOP
	case(i)
		0: begin : GEN_CASE0
		wire q = a[0] & b[0];
		assign o1[0] = q;
		end
		1: begin : GEN_CASE1
		wire q = a[1] & b[1];
		assign o1[1] = q;
		end
	endcase
end
endgenerate
// access objects defined inside the genrate loop by using their hierarchical name
assign 	 o2[0] = GEN_LOOP[0].GEN_CASE0.q;
assign 	 o2[1] = GEN_LOOP[1].GEN_CASE1.q;
   
endmodule
end_template
begin_template Generate Construct
// Generate costructs allow you to conditionally replicate HDL code in
// your design.  Everything in a generate construct must be legal Verilog
// HDL, even if the code itself isn't active.

generate 
	// Generate Items
endgenerate
end_template
begin_template Generate Conditional
// A <constant_expression> may only involve constant literals, parameters,
// genvars, or constant function calls. 

// If
if (<constant_expression>) 
begin : <if_block_name>
	// Generate Items
end 

// If-Else
if(<constant_expression>)
begin : <if_block_name>
	// Generate Items
end
else 
begin : <else_block_name>
	// Generate Items
end

// NOTE: Block names are optional but Altera recommends them.
end_template
begin_template Generate For
for(<genvar_id> = <constant_expr>; <constant_expr>; <genvar_id> = <constant_expr>) 
begin : <required_block_name>
	// Generate Items
end
end_template
begin_template Generate Case
case(<constant_expr>)
<constant_expr>: 
	begin : <block_name>
		// Generate Items
	end
<constant_expr>:
	begin : <block_name>
		// Generate Items
	end
// ...
default: 
	begin : <block_name>

	end
endcase

// NOTE: Block names are optional but Altera recommends them.
end_template
end_group
end_group
begin_group Sequential Statements
begin_template System Tasks
// arguments must be compile time constants

$display(message_arguments);

// Example

$display("a=%d; b=%b, h=%h; str=%s", a, b, h, str);

$warning("a=%d", a);

(* critical *) $warning("Critical Issue found"); // display critical warning in Quartus

$info("a=%d", a);
end_template
begin_template Blocking Assignment
// Use blocking assignments when assigning to loop variables, temporary
// variables, or variables that model combinational logic.

<variable_lvalue> = <expression>;
end_template
begin_template Nonblocking Assignment
// Use nonblocking assignments when assigning to variables that
// model sequential logic (registers, memories, state machines).

<variable_lvalue> <= <expression>;
end_template
begin_template If Statement
if(<expression>) 
begin 
	// Statements
end

if(<expression>)
begin 
	// Statements
end
else 
begin
	// Statements
end
end_template
begin_template Case Statement
// x and z values are NOT treated as don't-care's
case(<expr>)
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
default: <statement> 
endcase
end_template
begin_template Casex Statement
// x and z values are don't-care's 
casex(<expr>)
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
default: <sequential statement> 
endcase
end_template
begin_template Casez Statement
// z values are don't-care's
casez(<expr>)
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
<case_item_exprs>: <sequential statement>
default: <sequential statement> 
endcase
end_template
begin_group Loops
begin_template For Loop
for(<variable_name> = <value>; <expression>; <variable_name> = <value>)
begin
	// Statements
end	
end_template
begin_template While Loop
while(<expression>)
begin
	// Statements
end
end_template
end_group
begin_template Sequential Block
// Named blocks may include declarations at the beginning
begin : <block_name>
	// Optional Declarations
	// Statements
end

// Anonymous blocks may not include declarations according to
// the Verilog-2001 standard, which states that they do not define
// a new scope.  However, Quartus Prime does create scopes for anonymous blocks
// and allows declarations at the beginning.  The declared objects cannot 
// be referenced from outside the block.  Not all tools support this 
// non-standard Verilog behavior, but it is available a standard feature
// in SystemVerilog.
begin 
	// Optional Declarations 
	// Statements
end
end_template
end_group
begin_group Expressions
begin_template Unary Operators
+            // Unary plus
-            // Unary minus
!            // Logical NOT
~            // Bitwise NOT
&            // Reductive AND
~&           // Reductive NAND
|            // Reductive OR
~|           // Reductive NOR
^            // Reductive XOR
^~           // Reductive XNOR
~^           // Reductive XNOR
end_template
begin_template Binary Operators
**           // Power
*            // Multiply
/            // Divide
%            // Modulo
+            // Plus
-            // Minus
<<           // Shift Left (Logical)
>>           // Shift Right (Logical)
<<<          // Shift Left (Arithmetic)
>>>          // Shift Right (Arithmetic)
<            // Less Than
<=           // Less Than or Equal To
>            // Greater Than
>=           // Greater Than or Equal To
==           // Logical Equality (any x or z results in an x result)
!=           // Logical Inequality (any x or z results in an x result)
===          // Case Equality (x and z match exactly, result 1 or 0)
!===         // Case Inequality (x and z match exactly, result 1 or 0)
&            // Bitwise AND
~&           // Bitwise NAND
^            // Bitwise XOR
^~           // Bitwise XNOR
~^           // Bitwise XNOR
|            // Bitwise OR
~|           // Bitwise NOR
&&           // Logical AND
||           // Logical OR
or           // Logical OR for event expressions
end_template
begin_template Conditional Operator
(expression) ? (true_value_expr) : (false_value_expr)
end_template
end_group
end_group
begin_group Logic
begin_group Registers
begin_template Basic Positive Edge Register
// Update the register output on the clock's rising edge
always @ (posedge <clock_signal>)
begin
	<register_variable> <= <data>;
end
end_template
begin_template Basic Positive Edge Register with Power-Up = VCC
// Set the initial value to 1
reg <register_variable> = 1'b1;

// After initialization, update the register output on the clock's rising edge
always @ (posedge <clock_signal>)
begin
	<register_variable> <= <data>;
end
end_template
begin_template Basic Negative Edge Register
// Update the register output on the clock's falling edge
always @ (negedge <clock_signal>)
begin
	<register_variable> <= <data>;
end
end_template
begin_template Basic Negative Edge Register with Power-Up = VCC
// Set the initial value to 1
reg <register_variable> = 1'b1;

// After initialization, update the register output on the clock's rising edge
always @ (negedge <clock_signal>)
begin
	<register_variable> <= <data>;
end
end_template
begin_template Basic Positive Edge Register with Asynchronous Reset
always @ (negedge <reset> or posedge <clock_signal>)
begin
	// Reset whenever the reset signal goes low, regardless of the clock
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// If not resetting, update the register output on the clock's rising edge
	else
	begin
		<register_variable> <= <data>;
	end
end
end_template
begin_template Basic Negative Edge Register with Asynchronous Reset
always @ (negedge <reset> or negedge <clock_signal>)
begin
	// Reset whenever the reset signal goes low, regardless of the clock
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// If not resetting, update the register output on the clock's falling edge
	else
	begin
		<register_variable> <= <data>;
	end
end
end_template
begin_template Basic Positive Edge Register with Asynchronous Reset and Clock Enable
always @ (negedge <reset> or posedge <clock_signal>)
begin
	// Reset whenever the reset signal goes low, regardless of the clock
	// or the clock enable
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// If not resetting, and the clock signal is enabled on this register,
	// update the register output on the clock's rising edge
	else
	begin
		if (<clock_enable>)
		begin
			<register_variable> <= <data>;
		end
	end
end
end_template
begin_template Basic Negative Edge Register with Asynchronous Reset and Clock Enable
always @ (negedge <reset> or negedge <clock_signal>)
begin
	// Reset whenever the reset signal goes low, regardless of the clock
	// or the clock enable
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// If not resetting, and the clock signal is enabled on this register,
	// update the register output on the clock's falling edge
	else
	begin
		if (<clock_enable>)
		begin
			<register_variable> <= <data>;
		end
	end
end
end_template
begin_template Full-Featured Positive Edge Register with All Secondary Signals
// In Altera devices, register signals have a set priority.
// The HDL design should reflect this priority.
always @ (negedge <reset> or posedge <asynch_load> or posedge <clock_signal>)
begin
	// The asynchronous reset signal has highest priority
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// Asynchronous load has next priority
	else if (<asynch_load>)
	begin
		<register_variable> <= <other_data>;
	end
	else
	begin
		// At a clock edge, if asynchronous signals have not taken priority,
		// respond to the appropriate synchronous signal.
		// Check for synchronous reset, then synchronous load.
		// If none of these takes precedence, update the register output 
		// to be the register input.
		if (<clock_enable>)
		begin
			if (!<synch_reset>)
			begin
				<register_variable> <= 1'b0;
			end
			else if (<synch_load>)
			begin
				<register_variable> <= <other_data>;
			end
			else
			begin
				<register_variable> <= <data>;
			end
		end
	end
end
end_template
begin_template Full-Featured Negative Edge Register with All Secondary Signals
// In Altera devices, register signals have a set priority.
// The HDL design should reflect this priority.
always @ (negedge <reset> or posedge <asynch_load> or negedge <clock_signal>)
begin
	// The asynchronous reset signal has highest priority
	if (!<reset>)
	begin
		<register_variable> <= 1'b0;
	end
	// Asynchronous load has next priority
	else if (<asynch_load>)
	begin
		<register_variable> <= <other_data>;
	end
	else
	begin
		// At a clock edge, if asynchronous signals have not taken priority,
		// respond to the appropriate synchronous signal.
		// Check for synchronous reset, then synchronous load.
		// If none of these takes precedence, update the register output 
		// to be the register input.
		if (<clock_enable>)
		begin
			if (!<synch_reset>)
			begin
				<register_variable> <= 1'b0;
			end
			else if (<synch_load>)
			begin
				<register_variable> <= <other_data>;
			end
			else
			begin
				<register_variable> <= <data>;
			end
		end
	end
end
end_template
end_group
begin_group Latches
begin_template Basic Latch
// Update the variable only when updates are enabled
always @ (*)
begin
	if (<enable>)
	begin
		<latch_variable> <= <data>;
	end
end
end_template
begin_template Basic Latch with Reset
always @(*)
begin
	// The reset signal overrides the hold signal; reset the value to 0
	if (!<reset>)
	begin
		<latch_variable> <= 1'b0;
	end
	// Otherwise, change the variable only when updates are enabled
	else if (<enable>)
	begin
		<latch_variable> <= <data>;
	end
end
end_template
end_group
begin_group Tri-State
begin_template Tri-State Buffer
// When tri-state buffers are output enabled, they output a value. 
// Otherwise their "output" is set to high-impedence.
inout <bidir_variable>;
assign <bidir_variable> = (<output_enable> ? <data> : 1'bZ);
end_template
begin_template Tri-State Register
// Tri-state registers are registers on inout ports.  As with any
// registers, their output can be updated synchronously or asynchronously.
reg <bidir_variable>;
always @ (posedge <clock_signal> or negedge <asynch_output_enable>)
begin
	if (!<asynch_output_enable>)
	begin
		<bidir_variable> <= 1'bZ;
	end
	else
	begin
		<bidir_variable> <= (<output_enable>) ? <data> : 1'bZ;
	end
end
end_template
begin_template Bidirectional I/O
module bidirectional_io 
#(parameter WIDTH=4)
(input <output_enable>, input [WIDTH-1:0] <data>, inout [WIDTH-1:0] <bidir_variable>, output [WIDTH-1:0] <read_buffer>);

	// If we are using the bidir as an output, assign it an output value, 
	// otherwise assign it high-impedence
	assign <bidir_variable> = (<output_enable> ? <data> : {WIDTH{1'bz}});

	// Read in the current value of the bidir port, which comes either
	// from the input or from the previous assignment.
	assign <read_buffer> = <bidir_variable>;

endmodule
end_template
begin_template Open-Drain Buffer
// An open-drain buffer is similar to a tri-state buffer, but only has one
// possible output (GND).  If the output is not enabled, the "output" is set
// to high-impedence.
inout <bidir_variable>;
assign <bidir_variable> = (<output_enable> ? 1'b0 : 1'bZ);
end_template
end_group
end_group
begin_group Synthesis Attributes
begin_template full_case Attribute
// Indicates that Quartus Prime should consider a case statement
// to be full, even if the case items do not cover all possible
// values of the case expression.

(* full_case *) case(...)	
end_template
begin_template parallel_case Attribute
// Indicates that Quartus Prime should consider the case items
// in a case statement to be mutually exclusive, even if they
// are not.  Without this attribute, the Quartus Prime software
// may add priority logic when elaborating your case statement.
// The Quartus Prime software will only add this logic if one or 
// more case items overlap or if one or more case items are
// constant expressions.

(* parallel_case *) case(...)	
end_template
begin_template keep Attribute
// Prevents Quartus Prime from minimizing or removing a particular
// signal net during combinational logic optimization.	Apply
// the attribute to a net or variable declaration.

(* keep *) wire <net_name>;
(* keep *) reg <variable_name>;
end_template
begin_template maxfan Attribute
// Sets the maximum number of fanouts for a register or combinational
// cell.  The Quartus Prime software will replicate the cell and split
// the fanouts among the duplicates until the fanout of each cell
// is below the maximum.

// Register q should have no more than 8 fanouts
(* maxfan = 8 *) reg q;
end_template
begin_template preserve Attribute
// Prevents Quartus Prime from optimizing away a register as well
// as from being retimed.  For HyperFlex architectures, users may want
// "preserve_syn_only", which does allow retiming.
// Apply the attribute to the variable declaration for an object that infers
// a register.

(* preserve *) <variable_declaration>;
(* preserve *) module <module_name>(...);
end_template
begin_template preserve_syn_only Attribute
// Prevents Quartus Prime from optimizing away or merging a register 
// during Synthesis, but allows the register to be retimed during Retimer.
// Often used instead of "preserve" for HyperFlex architectures.
// Apply the attribute to the variable declaration for an object that infers
// a register.

(* preserve_syn_only *) <variable_declaration>;
(* preserve_syn_only *) module <module_name>(...);
end_template
begin_template preserve_for_debug Attribute
// Marks name so that Quartus Prime will preserve or keep the object
// during Synthesis, if the associated object/hierarchy also has the attribute
// PRESERVE_FOR_DEBUG_ENABLE set true on it as well. Useful for toggling on/off debug
// logic between compilation sessions without changing the underlying RTL.
// Set on either a register or a comb node.

(* preserve_for_debug *) <variable_declaration>;
(* preserve_for_debug *) module <module_name>(...);
end_template
begin_template noprune Attribute
// Prevents Quartus Prime from removing or optimizing a fanout free register.
// Apply the attribute to the variable declaration for an object that infers
// a register.

(* noprune *)  <variable_declaration>;
end_template
begin_template dont_merge Attribute
// Prevents Quartus Prime from merging a register with a duplicate
// register

(* dont_merge *) <variable_declaration>;
(* dont_merge *) module <module_name>(...);
end_template
begin_template dont_replicate Attribute
// Prevents Quartus Prime from replicating a register.

(* dont_replicate *) <variable_declaration>;
(* dont_replicate *) module <module_name>(...);
end_template
begin_template dont_retime Attribute
// Prevents Quartus Prime from retiming a register

(* dont_retime *) <variable_declaration>;
(* dont_retime *) module <module_name>(...);
end_template
begin_template direct_enable Attribute
// Identifies the logic cone that should be used as the clock enable
// for a register.  Sometimes a register has a complex clock enable
// condition, which may or may not contain the critical path in your
// design.  With this attribute, you can force Quartus Prime to route
// the critical portion directly to the clock enable port of a register
// and implement the remaining clock enable condition using regular 
// logic.

(* direct_enable *) <variable_or_net_declaration>;

// Example
(* direct_enable *) variable e1;
reg e2;
reg q, data;

always@(posedge clk) 
begin
	if(e1 | e2) 
	begin
		q <= data;
	end
end
end_template
begin_template useioff Attribute
// Controls the packing input, output, and output enable registers into
// I/O cells.  Using a register in an I/O cell can improve performance
// by minimizing setup, clock-to-output, and clock-to-output-enable times.

// Apply the attribute to a port declaration
(* useioff *) output reg [7:0] result;        // enable packing
(* useioff = 0 *) output reg [7:0] result;    // disable packing
end_template
begin_template ramstyle Attribute
// Controls the implementation of an inferred memory.  Apply the
// attribute to a variable declaration that infers a RAM, ROM, or shift-register.

// Legal values = "M9K", "M10K", "M20K", "M144K", "MLAB", "no_rw_check", "logic"

(* ramstyle = "M20K" *) reg [<msb>:<lsb>] <variable_name>[<msb>:<lsb>];

// If the attribute is set to "logic", then the RAM is implemented in logic cells

// The "no_rw_check" value indicates that your design does not depend
// on the behavior of the inferred RAM when there are simultaneous reads
// and writes to the same address.  Thus, the Quartus Prime software may ignore
// the read-during-write behavior of your HDL source and choose a behavior
// that matches the behavior of the RAM blocks in the target device.

// You may combine "no_rw_check" with a block type by separating the values
// with a comma:  "M20K, no_rw_check" or "no_rw_check, M20K"  
end_template
begin_template max_depth Attribute
// Controls the implementation of an inferred memory.  Apply the
// attribute to a variable declaration that infers a RAM or ROM.  

// Legal values = 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4192

(* max_depth = 1024*) reg [<msb>:<lsb>]        <variable_name2>[<msb>:<lsb>];
 
// Control the depth of an inferred memory block using the max_depth attribute. 
// By using this attribute, you can optimize the usage of the memory block.
// Values other than exact powers of 2 are ignored.
end_template
begin_template multstyle Attribute
// Controls the implementation of multiplication operators in your HDL 
// source.  Using this attribute, you can control whether the Quartus Prime 
// software should preferentially implement a multiplication operation in 
// general logic or dedicated hardware, if available in the target device.  

// Legal values = "dsp" or "logic"

// Examples (in increasing order of priority)

// Control the implementation of all multiplications in a module
(* multstyle = "dsp" *) module foo(...);

// Control the implementation of all multiplications whose result is
// directly assigned to a variable
(* multstyle = "logic" *) wire signed [31:0] result;
assign result = a * b; // implement this multiplication in logic

// Control the implementation of a specific multiplication
wire signed [31:0] result;
assign result = a * (* multstyle = "dsp" *) b;
end_template
begin_template syn_encoding Attribute
// Controls the encoding of the states in an inferred state machine.

// Legal values = "user" or "safe" or "user, safe"

// The value "user" instructs the Quartus Prime software to encode each state 
// with its corresponding value from the Verilog source. By changing the 
// values of your state constants, you can change the encoding of your state 
// machine

// The value "safe" instructs the Quartus Prime software to add extra logic 
// to detect illegal states (unreachable states) and force the state machine 
// into the reset state. You cannot implement a safe state machine by 
// specifying manual recovery logic in your design; the Quartus Prime software 
// eliminates this logic while optimizing your design.

// Examples

// Implement state as a safe state machine
(* syn_encoding = "safe" *) reg [7:0] state;
end_template
begin_template chip_pin Attribute
// Assigns pin location to ports on a module.

(* chip_pin = "<comma-separated list of locations>" *) <io_declaration>;

// Example
(* chip_pin = "B3, A3, A4" *) input [2:0] i;
end_template
begin_template altera_attribute Attribute
// Associates arbitrary Quartus Prime assignments with objects in your HDL
// source.  Each assignment uses the QSF format, and you can associate
// multiple assignments by separating them with ";".

// Preserve all registers in this hierarchy
(* altera_attribute = "-name PRESERVE_REGISTER on" *) module <name>(...);

// Cut timing paths from register q1 to register q2
(* altera_attribute = "-name CUT on -from q1" *) reg q2;

// Assign I/O standard to a pin
module top(
(* altera_attribute = "-name IO_STANDARD \"3.3-V LVCMOS\"" *) input in,
(* altera_attribute = "-name IO_STANDARD LVDS" *) input clk,
	output out); // out will get the default I/O standard for the device
end_template
end_group
begin_group Altera Primitives
begin_group Buffers
begin_template alt_inbuf
	//<data_in> must be declared as an input pin 
	alt_inbuf <instance_name> (.i(<data_in>), .o(<data_out>)); 

	defparam <instance_name>.io_standard = "2.5 V"; 
	defparam <instance_name>.location = "IOBANK_2";
	defparam <instance_name>.enable_bus_hold = "on";
	defparam <instance_name>.weak_pull_up_resistor = "off";
	defparam <instance_name>.termination = "parallel 50 ohms with calibration";
end_template
begin_template alt_inbuf_diff
	// <data_in_pos> and <data_in_neg> must be declared as input pins
	alt_inbuf_diff <instance_name> (.i(<data_in_pos>), .ibar(<data_in_neg>), .o(<data_out>));

	defparam <instance_name>.io_standard = "LVDS";
	defparam <instance_name>.location = "IOBANK_1";
	defparam <instance_name>.weak_pull_up_resistor = "off";
	defparam <instance_name>.enable_bus_hold = "off";
end_template
begin_template alt_iobuf
	alt_iobuf <instance_name> (
					.i(<data_in>), 
					.oe(<enable_signal>), 
					.o(<data_out>), 
					.io(<bidir>)	//<bidir> must be declared as an inout pin 
					); 

	defparam <instance_name>.io_standard = "3.3-V PCI"; 
	defparam <instance_name>.current_strength = "minimum current"; 
	defparam <instance_name>.slow_slew_rate = "on"; 
	defparam <instance_name>.location = "IOBANK_1"; 
	defparam <instance_name>.enable_bus_hold = "on";
	defparam <instance_name>.weak_pull_up_resistor = "off";
	defparam <instance_name>.termination = "series 50 ohms"; 
end_template
begin_template alt_outbuf
	alt_outbuf <instance_name> (.i(<data_in>), .o(<data_out>)); //<data_out> must be declared as an output pin

	defparam <instance_name>.io_standard = "2.5 V";
	defparam <instance_name>.slow_slew_rate = "on";
	defparam <instance_name>.enable_bus_hold = "on";
	defparam <instance_name>.weak_pull_up_resistor = "off";
	defparam <instance_name>.termination = "series 50 ohms";
end_template
begin_template alt_outbuf_diff
	// <data_out_pos> and <data_out_neg> must be declared as output pins
	alt_outbuf_diff <instance_name> (.i(<data_in>), .o(<data_out_pos>), .obar(<data_out_neg>));

	defparam <instance_name>.io_standard = "none";
	defparam <instance_name>.current_strength = "none";
	defparam <instance_name>.current_strength_new = "none";
	defparam <instance_name>.slew_rate = -1;
	defparam <instance_name>.location = "none";
	defparam <instance_name>.enable_bus_hold = "none";
	defparam <instance_name>.weak_pull_up_resistor = "none"; 
	defparam <instance_name>.termination = "none"; 
end_template
begin_template alt_outbuf_tri
	alt_outbuf_tri <instance_name> (
						.i(<data_in>), 
						.oe(<enable_signal>), 
						.o(<data_out>)	//<data_out> must be declared as an output pin
						); 

	defparam <instance_name>.io_standard = "1.8 V"; 
	defparam <instance_name>.current_strength  = "maximum current"; 
	defparam <instance_name>.slow_slew_rate = "off"; 
	defparam <instance_name>.enable_bus_hold = "on";
	defparam <instance_name>.weak_pull_up_resistor = "off";
	defparam <instance_name>.termination = "series 50 ohms"; 
end_template
begin_template cascade
	// <data_out> cannot feed an output pin, a register, or an XOR gate
	cascade <instance_name> (.in(<data_in>), .out(<data_out>)); 
end_template
begin_template carry_sum
	carry_sum <instance_name> (
					.sin(<sum_in>), 
					.cin(<carry_in>),  //<carry_in> cannot be fed by an input pin
					.sout(<sum_out>), 
					.cout(<carry_out>) //<carry_out> cannot feed an output pin
					); 
end_template
begin_template global
	global <instance_name> (.in(<data_in>), .out(<data_out>));
end_template
begin_template lcell
	lcell <instance_name> (.in(<data_in>), .out(<data_out>));
end_template
begin_template opndrn
	// <data_out> may feed an inout pin
	opndrn <instance_name> (.in(<data_in>), .out(<data_out>));
end_template
begin_template tri
// The tri primitive cannot be used in Verilog as tri is a reserved word 
// in the Verilog language. Use the alt_outbuf_tri primitive instead, or 
// use the equivalent behavioral Verilog, for example:
assign out = oe ? in : 1'bZ;
end_template
end_group
begin_group Registers and Latches
begin_template dff
	dff <instance_name> (
				.d(<data_in>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.q(<data_out>)
				);
end_template
begin_template dffe
	dffe <instance_name> (
				.d(<data_in>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.ena(<clock_enable>), 
				.q(<data_out>)
				);
end_template
begin_template dffea
	dffea <instance_name> (
				.d(<data_in>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.ena(<clock_enable>), 
				.adata(<asynch_data_in>), 
				.aload(<asynch_load_signal>), 
				.q(<data_out>)
				);
end_template
begin_template dffeas
	dffeas <instance_name> (
				.d(<data_in>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>),
				.prn(<active_low_preset>),
				.ena(<clock_enable>), 
				.asdata(<asynch_data_in>), 
				.aload(<asynch_load_signal>), 
				.sclr(<synchronous_clear>), 
				.sload(<synchronous_load>), 
				.q(<data_out>)
				); 
end_template
begin_template jkff
	jkff <instance_name> (
				.j(<synchronous_set>), 
				.k(<synchronous_reset>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.q(<data_out>)
				);
end_template
begin_template jkffe
	jkffe <instance_name> (
				.j(<synchronous_set>), 
				.k(<synchronous_reset>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.ena(<clock_enable>), 
				.q(<data_out>)
				);
end_template
begin_template latch
	latch <instance_name> ( 
				.d(<data_in>), 
				.ena(<clock_enable>), 
				.q(<data_out>)
				);
end_template
begin_template srff
	srff <instance_name> (
				.s(<synchronous_set>), 
				.r(<synchronous_reset>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.q(<data_out>)
				); 
end_template
begin_template srffe
	srffe <instance_name> (
				.s(<synchronous_set>), 
				.r(<synchronous_reset>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.ena(<clock_enable>), 
				.q(<data_out>)
				);
end_template
begin_template tff
	tff <instance_name> (
				.t(<toggle_signal>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.q(<data_out>)
				);
end_template
begin_template tffe
	tffe <instance_name> (
				.t(<toggle_signal>), 
				.clk(<clock_signal>), 
				.clrn(<active_low_clear>), 
				.prn(<active_low_preset>), 
				.ena(<clock_enable>), 
				.q(<data_out>)
				);
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
				.PLL_SIM_MODEL             ("")			// It is a simulation specific parameter to select the technology dependent IOPLL simulation model. Allowed values are "Stratix 10", "Agilex 7 F-Series", "Agilex 7 (F-Series)", "Agilex 7 I-Series", "Agilex 7 (I-Series)", "Agilex 7 M-Series", "Agilex 7 (M-Series)".
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
				.RST_TYPE                         ("ACTIVE_HIGH"),
				.NUM_STAGES                       (3)
	) <instance_name> (
				.clk                              (_connected_to_clk_),                //input, width = 1
				.srst_in                          (_connected_to_srst_in_),            //input, width = 1
				.srst_out                         (_connected_to_srst_out_)            //output, width = 1
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
				.RST_TYPE                         ("ACTIVE_HIGH"),
				.NUM_STAGES                       (3)
	) <instance_name> (
				.clk                              (_connected_to_clk_),                //input, width = 1
				.arst_in                          (_connected_to_arst_in_),            //input, width = 1
				.srst_out                         (_connected_to_srst_out_)            //output, width = 1
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
				.INITIAL_VALUE                  (0),
				.NUM_STAGES                     (3)
	) <instance_name> (
				.clk                            (_connected_to_clk_),                 //input, width = 1
				.async_in                       (_connected_to_async_in_),            //input, width = 1
				.sync_out			(_connected_to_sync_out_)           //output, width = 1
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
					.INITIAL_VALUE                  (0),
					.NUM_STAGES                     (3)
	) <instance_name> (
					.src_clk                        (_connected_to_src_clk_),            //input, width = 1
					.src_sig                        (_connected_to_src_sig_),            //input, width = 1
					.dst_clk                        (_connected_to_dst_clk_),            //input, width = 1
					.dst_sig                        (_connected_to_dst_sig_)             //output, width = 1
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
					.CLK_TYPE                   	("RELATED_CLKS")
	) <instance_name> (
					.sel				(_connected_to_sel_),            //input, width = 1
                                      	.clk_A                      	(_connected_to_clk_A_),          //input, width = 1
                                      	.clk_B                     	(_connected_to_clk_B_),          //input, width = 1
				      	.clk_out                    	(_connected_to_clk_out_)         //output, width = 1
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
					.DATA_WIDTH			(4)
	) <instance_name> (
					.src_clk			(_connected_to_src_clk_),            //input, width = 1
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
