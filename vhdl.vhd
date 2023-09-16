begin_group VHDL
begin_group Full Designs
begin_group RAMs and ROMs
begin_template Single-Port RAM
-- Quartus Prime VHDL Template
-- Single port RAM with single read/write address 

library ieee;
use ieee.std_logic_1164.all;

entity single_port_ram is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		addr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we		: in std_logic := '1';
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end entity;

architecture rtl of single_port_ram is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM signal.	
	signal ram : memory_t;

begin

	process(clk)
	begin
	if(rising_edge(clk)) then
		-- Read returns OLD data. To return NEW data, use the following code:	 
		-- if(we = '1') then
		--     ram(addr) <= data;
		--     q <= data;
		-- else
		--     q <= ram(addr);
		-- end if;
		-- NOTE: NEW data requires extra bypass logic around the RAM on Stratix10.
	
		if(we = '1') then
			ram(addr) <= data;
		end if;
		q <= ram(addr);

		end if;
	end process;

end rtl;
end_template
begin_template Single-Port RAM with Initial Contents
-- Quartus Prime VHDL Template
-- Single-port RAM with single read/write address and initial contents	

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_port_ram_with_init is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		addr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we		: in std_logic := '1';
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end single_port_ram_with_init;

architecture rtl of single_port_ram_with_init is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	function init_ram
		return memory_t is 
		variable tmp : memory_t := (others => (others => '0'));
	begin 
		for addr_pos in 0 to 2**ADDR_WIDTH - 1 loop 
			-- Initialize each address with the address itself
			tmp(addr_pos) := std_logic_vector(to_unsigned(addr_pos, DATA_WIDTH));
		end loop;
		return tmp;
	end init_ram;	 

	-- Declare the RAM signal and specify a default value.	Quartus Prime
	-- will create a memory initialization file (.mif) based on the 
	-- default value.
	signal ram : memory_t := init_ram;

begin

	process(clk)
	begin
	if(rising_edge(clk)) then
		-- Read returns OLD data. To return NEW data, use the following code:	 
		-- if(we = '1') then
		--     ram(addr) <= data;
		--     q <= data;
		-- else
		--     q <= ram(addr);
		-- end if;
		-- NOTE: NEW data requires extra bypass logic around the RAM on Stratix10.

		if(we = '1') then
			ram(addr) <= data;
		end if;
		q <= ram(addr);

		end if;
	end process;

end rtl;
end_template
begin_template Simple Dual-Port RAM (single clock)
-- Quartus Prime VHDL Template
-- Simple Dual-Port RAM with different read/write addresses but
-- single read/write clock

library ieee;
use ieee.std_logic_1164.all;

entity simple_dual_port_ram_single_clock is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		raddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		waddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we		: in std_logic := '1';
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end simple_dual_port_ram_single_clock;

architecture rtl of simple_dual_port_ram_single_clock is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM signal.	
	signal ram : memory_t;

begin

	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we = '1') then
			ram(waddr) <= data;
		end if;
 
		-- On a read during a write to the same address, the read will
		-- return the OLD data at the address
		q <= ram(raddr);
	end if;
	end process;

end rtl;
end_template
begin_template Simple Dual-Port RAM (dual clock)
-- Quartus Prime VHDL Template
-- Simple Dual-Port RAM with different read/write addresses and
-- different read/write clock

library ieee;
use ieee.std_logic_1164.all;

entity simple_dual_port_ram_dual_clock is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		rclk	: in std_logic;
		wclk	: in std_logic;
		raddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		waddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we		: in std_logic := '1';
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end simple_dual_port_ram_dual_clock;

architecture rtl of simple_dual_port_ram_dual_clock is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM signal.	
	signal ram : memory_t;

begin

	process(wclk)
	begin
	if(rising_edge(wclk)) then 
		if(we = '1') then
			ram(waddr) <= data;
		end if;
	end if;
	end process;

	process(rclk)
	begin
	if(rising_edge(rclk)) then 
		q <= ram(raddr);
	end if;
	end process;

end rtl;
end_template
begin_template Simple Dual-Port RAM (with enable signals)
-- Quartus Prime VHDL Template
-- Simple Dual Port RAM with write enable, read enable, and clock enable

library ieee;
use ieee.std_logic_1164.all;

entity simple_dual_port_ram_with_enables is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		waddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		raddr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		ce		: in std_logic := '1';
		we		: in std_logic := '1';
		re		: in std_logic := '1';
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end simple_dual_port_ram_with_enables;

architecture rtl of simple_dual_port_ram_with_enables is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM 
	shared variable ram : memory_t;

begin


	-- Port A
	process(clk)
	begin
	if(rising_edge(clk)) then
		if(ce = '1') THEN
			if(we = '1') then
				ram(waddr) := data;
			end if;
			if(re = '1') then
				q <= ram(raddr);
			end if;
		end if;
	end if;
	end process;


end rtl;
end_template
begin_template True Dual-Port RAM (single clock)
-- Quartus Prime VHDL Template
-- True Dual-Port RAM with single clock
--
-- Read-during-write behavior is undefined for mixed ports 
-- and "new data" on the same port


library ieee;
use ieee.std_logic_1164.all;

entity true_dual_port_ram_single_clock is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data_a	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		data_b	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we_a	: in std_logic := '1';
		we_b	: in std_logic := '1';
		q_a		: out std_logic_vector((DATA_WIDTH -1) downto 0);
		q_b		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end true_dual_port_ram_single_clock;

architecture rtl of true_dual_port_ram_single_clock is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM 
	shared variable ram : memory_t;

begin


	-- Port A
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_a = '1') then
			ram(addr_a) := data_a;
		end if;
		q_a <= ram(addr_a);
	end if;
	end process;

	-- Port B 
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_b = '1') then
			ram(addr_b) := data_b;
		end if;
  	    q_b <= ram(addr_b);
	end if;
	end process;

end rtl;
end_template
begin_template True Dual-Port RAM (single clock, old data on mixed ports read during write)
-- Quartus Prime VHDL Template
-- True Dual-Port RAM with single clock
--
-- Read-during-write behavior is "old data" for mixed ports 
-- and "new data" on the same port
--
-- This style of RAM cannot be used on Stratix 10, 
-- which does not support "old data" read-during-write for mixed ports


library ieee;
use ieee.std_logic_1164.all;

entity true_dual_port_ram_single_clock_old_rw is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data_a	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		data_b	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we_a	: in std_logic := '1';
		we_b	: in std_logic := '1';
		q_a		: out std_logic_vector((DATA_WIDTH -1) downto 0);
		q_b		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end true_dual_port_ram_single_clock_old_rw;

architecture rtl of true_dual_port_ram_single_clock_old_rw is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM 
	shared variable ram : memory_t;

begin


	-- Port A
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_a = '1') then
			ram(addr_a) <= data_a;
			q_a <= data_a;			
		else
			q_a <= ram(addr_a);
		end if;
	end if;
	end process;

	-- Port B 
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_b = '1') then
			ram(addr_b) := data_b;
			q_b <= data_a;					
		else
			q_b <= ram(addr_b);		
		end if;
	end if;
	end process;

end rtl;
end_template
begin_template True Dual Port RAM (dual clock)
-- Quartus Prime VHDL Template
-- True Dual-Port RAM with dual clocks
--
-- This style of RAM cannot be used on Stratix 10, 
-- which does not support True Dual Port RAM with dual clocks

library ieee;
use ieee.std_logic_1164.all;

entity true_dual_port_ram_dual_clock is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk_a	: in std_logic;
		clk_b	: in std_logic;
		addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data_a	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		data_b	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we_a	: in std_logic := '1';
		we_b	: in std_logic := '1';
		q_a		: out std_logic_vector((DATA_WIDTH -1) downto 0);
		q_b		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end true_dual_port_ram_dual_clock;

architecture rtl of true_dual_port_ram_dual_clock is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM 
	shared variable ram : memory_t;

begin

	-- Port A
	process(clk_a)
	begin
	if(rising_edge(clk_a)) then 
		if(we_a = '1') then
			ram(addr_a) := data_a;
		end if;
		q_a <= ram(addr_a);
	end if;
	end process;

	-- Port B
	process(clk_b)
	begin
	if(rising_edge(clk_b)) then 
		if(we_b = '1') then
			ram(addr_b) := data_b;
		end if;
		q_b <= ram(addr_b);
	end if;
	end process;

end rtl;
end_template
begin_template Quad Port RAM
-- Quartus Prime VHDL Template
-- Quad Port RAM with separate read/write addresses and
-- single read/write clock
-- This style of RAM cannot be used on Arria 10, 
-- which does not support Quad Port RAM

library ieee;
use ieee.std_logic_1164.all;

entity quad_port_ram is

	generic 
	(
		DATA_WIDTH : natural := 2;
		ADDR_WIDTH : natural := 6
	);

	port 
	(
		clk		: in std_logic;
		read_addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		read_addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		write_addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		write_addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		data_a	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		data_b	: in std_logic_vector((DATA_WIDTH-1) downto 0);
		we_a	: in std_logic := '1';
		we_b	: in std_logic := '1';
		q_a		: out std_logic_vector((DATA_WIDTH -1) downto 0);
		q_b		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end quad_port_ram;

architecture rtl of quad_port_ram is

	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	-- Declare the RAM 
	shared variable ram : memory_t;

begin

	-- Port A
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_a = '1') then
			ram(write_addr_a) := data_a;
		end if;
	end if;
	end process;

	process(clk)
	begin
	if(rising_edge(clk)) then 
		q_a <= ram(read_addr_a);
	end if;
	end process;
	
	-- Port B 
	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we_b = '1') then
			ram(write_addr_b) := data_b;
		end if;
	end if;
	end process;

	process(clk)
	begin
	if(rising_edge(clk)) then 
		q_b <= ram(read_addr_b);
	end if;
	end process;
	
end rtl;
end_template
begin_template Mixed-Width RAM
-- Quartus Prime VHDL Template
--
-- Simple Dual-Port RAM with separate read and write addresses and data widths
-- that are controlled by the parameters RW and WW.  RW and WW must specify a
-- read/write ratio that's supported by the memory blocks in your target
-- device.  Otherwise, no RAM will be inferred.  

library ieee;
use ieee.std_logic_1164.all;
library altera;
use altera.altera_standard_functions.all;

entity mixed_width_ram is
    
	generic (
		WORDS : natural := 256;
		RW    : natural := 8;
		WW    : natural := 32);

	port (
		we    : in std_logic;
		clk   : in std_logic;
		waddr : in natural range 0 to (WORDS * maximum(RW, WW)) / WW - 1;
		wdata : in std_logic_vector(WW - 1 downto 0);
		raddr : in natural range 0 to (WORDS * maximum(RW, WW)) / RW - 1;
		q     : out std_logic_vector(RW - 1 downto 0));

end mixed_width_ram;

architecture rtl of mixed_width_ram is

	constant B : natural := minimum(RW, WW);
	constant R : natural := maximum(RW, WW)/B;

	-- Use a multidimensional array to model mixed-width 
	type word_t is array(R - 1 downto 0) of std_logic_vector(B - 1 downto 0);
	type ram_t is array (0 to WORDS - 1) of word_t;

	signal ram : ram_t;
    
begin  -- rtl

	-- Must handle read < write and write > read separately 
	smaller_read: if RW < WW generate
		signal wdata_local : word_t;
	begin 

		-- Re-organize the write data to match the RAM word type
		unpack: for i in 0 to R - 1 generate    
			wdata_local(i) <= wdata(B*(i+1) - 1 downto B*i);
		end generate unpack;

		process(clk, we)
		begin
			if(rising_edge(clk)) then 
				if(we = '1') then
					ram(waddr) <= wdata_local;
				end if;
				q <= ram(raddr / R )(raddr mod R);
			end if;
		end process;  
	end generate smaller_read;

	not_smaller_read: if RW >= WW generate
		signal q_local : word_t;
	begin

		-- Re-organize the read data from the RAM to match the output
		unpack: for i in 0 to R - 1 generate    
			q(B*(i+1) - 1 downto B*i) <= q_local(i);
		end generate unpack;
        
		process(clk, we)
		begin
			if(rising_edge(clk)) then 
				if(we = '1') then
					ram(waddr / R)(waddr mod R) <= wdata;
				end if;
				q_local <= ram(raddr);
			end if;
		end process;  
	end generate not_smaller_read;

end rtl;
end_template
begin_template Mixed-Width True Dual Port RAM
-- Quartus Prime VHDL Template
--
-- True Dual-Port RAM with single clock and different data width on the two ports and width new data on read during write on same port
--
-- The first datawidth and the widths of the addresses are specified
-- The second data width is equal to DATA_WIDTH1 * RATIO, where RATIO = (1 << (ADDRESS_WIDTH1 - ADDRESS_WIDTH2)
-- RATIO must have value that is supported by the memory blocks in your target
-- device.  Otherwise, no RAM will be inferred.  
--
-- Read-during-write behavior returns old data for mixed ports and the new data on the same port
-- This style of RAM cannot be used on Stratix 10, 
-- which does not support mixed-width True Dual-Port RAM

library ieee;
use ieee.std_logic_1164.all;

entity mixed_width_true_dual_port_ram is
    
	generic (
		DATA_WIDTH1    : natural :=  8;
		ADDRESS_WIDTH1 : natural :=  10;
		ADDRESS_WIDTH2 : natural :=  8);

	port (
	we1   : in std_logic;
		we2   : in std_logic;
	clk   : in std_logic;
	addr1 : in natural range 0 to (2 ** ADDRESS_WIDTH1 - 1);
		addr2 : in natural range 0 to (2 ** ADDRESS_WIDTH2 - 1);
	data_in1 : in  std_logic_vector(DATA_WIDTH1 - 1 downto 0);
	data_in2 : in  std_logic_vector(DATA_WIDTH1 * (2 ** (ADDRESS_WIDTH1 - ADDRESS_WIDTH2)) - 1 downto 0);                
	data_out1   : out std_logic_vector(DATA_WIDTH1 - 1 downto 0);
	data_out2   : out std_logic_vector(DATA_WIDTH1 * 2 ** (ADDRESS_WIDTH1 - ADDRESS_WIDTH2) - 1 downto 0));

end mixed_width_true_dual_port_ram;

architecture rtl of mixed_width_true_dual_port_ram is
	constant RATIO       : natural := 2 ** (ADDRESS_WIDTH1 - ADDRESS_WIDTH2) ;
	constant DATA_WIDTH2 : natural := DATA_WIDTH1 * RATIO; 
	constant RAM_DEPTH   : natural := 2 ** ADDRESS_WIDTH2;

	-- Use a multidimensional array to model mixed-width 
	type word_t is array(RATIO - 1 downto 0) of std_logic_vector(DATA_WIDTH1 - 1 downto 0);
	type ram_t is array (0 to RAM_DEPTH - 1) of word_t;

	-- declare the RAM
	shared variable  ram : ram_t;
	
	signal w1_local : word_t;
	signal q1_local : word_t;

begin  -- rtl
	-- Re-organize the write data to match the RAM word type
	unpack: for i in 0 to RATIO - 1 generate    
		w1_local(i) <= data_in2(DATA_WIDTH1*(i+1) - 1 downto DATA_WIDTH1*i);
		data_out2(DATA_WIDTH1*(i+1) - 1 downto DATA_WIDTH1*i) <= q1_local(i);
	end generate unpack;

	--port A
	process(clk)
	begin
		if(rising_edge(clk)) then 
			if(we2 = '1') then
				ram(addr2) := w1_local;
			end if;
			q1_local <= ram(addr2);
		end if;
	end process;

	-- port B
	process(clk)
	begin
		if(rising_edge(clk)) then 
			if(we1 ='1') then
				ram(addr1 / RATIO)(addr1 mod RATIO) := data_in1;
			end if;
			data_out1 <= ram(addr1 / RATIO )(addr1 mod RATIO);
		end if;
	end process;  
end rtl;
end_template
begin_template Byte-enabled Simple Dual Port RAM
-- Quartus Prime VHDL Template
-- Simple Dual-Port RAM with different read/write addresses and single read/write clock
-- and with a control for writing single bytes into the memory word

library ieee;
use ieee.std_logic_1164.all;
library work;

entity byte_enabled_simple_dual_port_ram is

	generic (
		ADDR_WIDTH : natural := 6;
		BYTE_WIDTH : natural := 8;
		BYTES : natural := 4);
  
	port (
		we, clk : in  std_logic;
		be      : in  std_logic_vector (BYTES - 1 downto 0);
		wdata   : in  std_logic_vector(BYTES*BYTE_WIDTH - 1 downto 0);
		waddr   : in  integer range 0 to 2 ** ADDR_WIDTH -1 ;
		raddr   : in  integer range 0 to 2 ** ADDR_WIDTH - 1;
		q       : out std_logic_vector(BYTES*BYTE_WIDTH-1 downto 0));
end byte_enabled_simple_dual_port_ram;

architecture rtl of byte_enabled_simple_dual_port_ram is
	--  build up 2D array to hold the memory
	type word_t is array (0 to BYTES-1) of std_logic_vector(BYTE_WIDTH-1 downto 0);
	type ram_t is array (0 to 2 ** ADDR_WIDTH - 1) of word_t;
	-- declare the RAM
	signal ram : ram_t;
	signal q_local : word_t;

begin  -- rtl
	-- Re-organize the read data from the RAM to match the output
	unpack: for i in 0 to BYTES - 1 generate    
		q(BYTE_WIDTH*(i+1) - 1 downto BYTE_WIDTH*i) <= q_local(i);
	end generate unpack;
        
	process(clk)
	begin
		if(rising_edge(clk)) then 
			if(we = '1') then
				-- edit this code if using other than four bytes per word
				if(be(0) = '1') then
					ram(waddr)(0) <= wdata(BYTE_WIDTH-1 downto 0);
				end if;
				if be(1) = '1' then
					ram(waddr)(1) <= wdata(2*BYTE_WIDTH-1 downto BYTE_WIDTH);
				end if;
				if be(2) = '1' then
					ram(waddr)(2) <= wdata(3*BYTE_WIDTH-1 downto 2*BYTE_WIDTH);
				end if;
				if be(3) = '1' then
					ram(waddr)(3) <= wdata(4*BYTE_WIDTH-1 downto 3*BYTE_WIDTH);
				end if;
			end if;
			q_local <= ram(raddr);
		end if;
	end process;  
end rtl;
end_template
begin_template Byte-enabled True Dual Port RAM
-- Quartus Prime VHDL Template
-- True Dual-Port RAM with single clock
-- and individual controls for writing into separate bytes of the memory word (byte-enable)
--
-- Read-during-write returns either new or old data depending
-- on the order in which the simulator executes the process statements.
-- Quartus Prime will consider this read-during-write scenario as a 
-- don't care condition to optimize the performance of the RAM.  If you
-- need a read-during-write behavior to be determined, you
-- must instantiate the altsyncram Megafunction directly.

library ieee;
use ieee.std_logic_1164.all;
library work;

entity byte_enabled_true_dual_port_ram is

	generic (
		ADDR_WIDTH : natural := 8;
		BYTE_WIDTH : natural := 8;
		BYTES : natural := 4);
  
	port (
		we1, we2, clk : in  std_logic;
		be1      : in  std_logic_vector (BYTES - 1 downto 0);
		be2      : in  std_logic_vector (BYTES - 1 downto 0);    
		data_in1 : in  std_logic_vector(BYTES*BYTE_WIDTH - 1 downto 0);
		data_in2 : in  std_logic_vector(BYTES*BYTE_WIDTH - 1 downto 0);    
		addr1   : in  integer range 0 to 2 ** ADDR_WIDTH -1 ;
		addr2   : in  integer range 0 to 2 ** ADDR_WIDTH - 1;
		data_out1 : out std_logic_vector(BYTES*BYTE_WIDTH-1 downto 0);
		data_out2 : out std_logic_vector(BYTES*BYTE_WIDTH-1 downto 0));
end byte_enabled_true_dual_port_ram;

architecture rtl of byte_enabled_true_dual_port_ram is
	--  build up 2D array to hold the memory
	type word_t is array (0 to BYTES-1) of std_logic_vector(BYTE_WIDTH-1 downto 0);
	type ram_t is array (0 to 2 ** ADDR_WIDTH - 1) of word_t;

	shared variable ram : ram_t;
	
	signal q1_local : word_t;
	signal q2_local : word_t;  

begin  -- rtl
	-- Reorganize the read data from the RAM to match the output
	unpack: for i in 0 to BYTES - 1 generate    
		data_out1(BYTE_WIDTH*(i+1) - 1 downto BYTE_WIDTH*i) <= q1_local(i);
		data_out2(BYTE_WIDTH*(i+1) - 1 downto BYTE_WIDTH*i) <= q2_local(i);    
	end generate unpack;
        
	process(clk)
	begin
		if(rising_edge(clk)) then 
			if(we1 = '1') then
				-- edit this code if using other than four bytes per word
				if(be1(0) = '1') then
					ram(addr1)(0) := data_in1(BYTE_WIDTH-1 downto 0);
				end if;
				if be1(1) = '1' then
					ram(addr1)(1) := data_in1(2*BYTE_WIDTH-1 downto BYTE_WIDTH);
				end if;
				if be1(2) = '1' then
					ram(addr1)(2) := data_in1(3*BYTE_WIDTH-1 downto 2*BYTE_WIDTH);
				end if;
				if be1(3) = '1' then
					ram(addr1)(3) := data_in1(4*BYTE_WIDTH-1 downto 3*BYTE_WIDTH);
				end if;
			end if;
		end if;
	end process;

	process(clk)
	begin
		if(rising_edge(clk)) then 
			q1_local <= ram(addr1);
		end if;
	end process;

	process(clk)
	begin
	if(rising_edge(clk)) then 
		if(we2 = '1') then
			-- edit this code if using other than four bytes per word
			if(be2(0) = '1') then
				ram(addr2)(0) := data_in2(BYTE_WIDTH-1 downto 0);
			end if;
			if be2(1) = '1' then
				ram(addr2)(1) := data_in2(2*BYTE_WIDTH-1 downto BYTE_WIDTH);
			end if;
			if be2(2) = '1' then
				ram(addr2)(2) := data_in2(3*BYTE_WIDTH-1 downto 2*BYTE_WIDTH);
			end if;
			if be2(3) = '1' then
				ram(addr2)(3) := data_in2(4*BYTE_WIDTH-1 downto 3*BYTE_WIDTH);
			end if;
		end if;
	end if;
	end process;  

	process(clk)
	begin
		if(rising_edge(clk)) then 
			q2_local <= ram(addr2);
		end if;
	end process;

	
end rtl;
end_template
begin_template Single-Port ROM
-- Quartus Prime VHDL Template
-- Single-Port ROM

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_port_rom is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 8
	);

	port 
	(
		clk		: in std_logic;
		addr	: in natural range 0 to 2**ADDR_WIDTH - 1;
		q		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end entity;

architecture rtl of single_port_rom is

	-- Build a 2-D array type for the ROM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	function init_rom
		return memory_t is 
		variable tmp : memory_t := (others => (others => '0'));
	begin 
		for addr_pos in 0 to 2**ADDR_WIDTH - 1 loop 
			-- Initialize each address with the address itself
			tmp(addr_pos) := std_logic_vector(to_unsigned(addr_pos, DATA_WIDTH));
		end loop;
		return tmp;
	end init_rom;	 

	-- Declare the ROM signal and specify a default value.	Quartus Prime
	-- will create a memory initialization file (.mif) based on the 
	-- default value.
	signal rom : memory_t := init_rom;

begin

	process(clk)
	begin
	if(rising_edge(clk)) then
		q <= rom(addr);
	end if;
	end process;

end rtl;
end_template
begin_template Dual-Port ROM
-- Quartus Prime VHDL Template
-- Dual-Port ROM

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dual_port_rom is

	generic 
	(
		DATA_WIDTH : natural := 8;
		ADDR_WIDTH : natural := 8
	);

	port 
	(
		clk		: in std_logic;
		addr_a	: in natural range 0 to 2**ADDR_WIDTH - 1;
		addr_b	: in natural range 0 to 2**ADDR_WIDTH - 1;
		q_a		: out std_logic_vector((DATA_WIDTH -1) downto 0);
		q_b		: out std_logic_vector((DATA_WIDTH -1) downto 0)
	);

end entity;

architecture rtl of dual_port_rom is

	-- Build a 2-D array type for the ROM
	subtype word_t is std_logic_vector((DATA_WIDTH-1) downto 0);
	type memory_t is array(2**ADDR_WIDTH-1 downto 0) of word_t;

	function init_rom
		return memory_t is 
		variable tmp : memory_t := (others => (others => '0'));
	begin 
		for addr_pos in 0 to 2**ADDR_WIDTH - 1 loop 
			-- Initialize each address with the address itself
			tmp(addr_pos) := std_logic_vector(to_unsigned(addr_pos, DATA_WIDTH));
		end loop;
		return tmp;
	end init_rom;	 

	-- Declare the ROM signal and specify a default value.	Quartus Prime
	-- will create a memory initialization file (.mif) based on the 
	-- default value.
	signal rom : memory_t := init_rom;

begin

	process(clk)
	begin
	if(rising_edge(clk)) then
		q_a <= rom(addr_a);
		q_b <= rom(addr_b);
	end if;
	end process;

end rtl;
end_template
end_group
begin_group Shift Registers
begin_template Basic Shift Register
-- Quartus Prime VHDL Template
-- Basic Shift Register

library ieee;
use ieee.std_logic_1164.all;

entity basic_shift_register is

	generic
	(
		NUM_STAGES : natural := 256
	);

	port 
	(
		clk		: in std_logic;
		enable	: in std_logic;
		sr_in	    : in std_logic;
		sr_out	: out std_logic
	);

end entity;

architecture rtl of basic_shift_register is

	-- Build an array type for the shift register
	type sr_length is array ((NUM_STAGES-1) downto 0) of std_logic;

	-- Declare the shift register signal
	signal sr: sr_length;

begin

	process (clk)
	begin
		if (rising_edge(clk)) then

			if (enable = '1') then

				-- Shift data by one stage; data from last stage is lost
				sr((NUM_STAGES-1) downto 1) <= sr((NUM_STAGES-2) downto 0);

				-- Load new data into the first stage
				sr(0) <= sr_in;

			end if;
		end if;
	end process;

	-- Capture the data from the last stage, before it is lost
	sr_out <= sr(NUM_STAGES-1);

end rtl;
end_template
begin_template Basic Shift Register with Asynchronous Reset
-- Quartus Prime VHDL Template
-- One-bit wide, N-bit long shift register with asynchronous reset

library ieee;
use ieee.std_logic_1164.all;

entity basic_shift_register_asynchronous_reset is

	generic
	(
		NUM_STAGES : natural := 256
	);

	port 
	(
		clk	    : in std_logic;
		enable	: in std_logic;
		reset   : in std_logic;
		sr_in	    : in std_logic;
		sr_out	: out std_logic
	);

end entity;

architecture rtl of basic_shift_register_asynchronous_reset is

	-- Build an array type for the shift register
	type sr_length is array ((NUM_STAGES-1) downto 0) of std_logic;

	-- Declare the shift register signal
	signal sr: sr_length;

begin

	process (clk, reset)
	begin
		if (reset = '1') then
			sr <= (others=>'0');
		elsif (rising_edge(clk)) then

			if (enable = '1') then

				-- Shift data by one stage; data from last stage is lost
				sr((NUM_STAGES-1) downto 1) <= sr((NUM_STAGES-2) downto 0);

				-- Load new data into the first stage
				sr(0) <= sr_in;

			end if;
		end if;
	end process;

	-- Capture the data from the last stage, before it is lost
	sr_out <= sr(NUM_STAGES-1);

end rtl;
end_template
begin_template Barrel Shifter
-- Quartus Prime VHDL Template
-- Barrel Shifter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity barrel_shifter is

	generic
	(
		DIST_WIDTH : natural := 3;
		NUM_STAGES : natural := 256
	);

	port 
	(
		clk			: in std_logic;
		enable		: in std_logic;
		is_left		: in std_logic;
		data		: in std_logic_vector((NUM_STAGES-1) downto 0);
		distance	: in unsigned((DIST_WIDTH-1) downto 0);
		sr_out		: out std_logic_vector((NUM_STAGES-1) downto 0)
	);

end entity;

architecture rtl of barrel_shifter is

	-- Declare the shift register signal
	signal sr : unsigned ((NUM_STAGES-1) downto 0);

begin

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (enable = '1') then

				-- Perform rotation with functions rol and ror
				if (is_left = '1') then
					sr <= unsigned(data) rol to_integer(distance);
				else
					sr <= unsigned(data) ror to_integer(distance);
				end if;

			end if;
		end if;
	end process;

	sr_out <= std_logic_vector(sr);

end rtl;
end_template
begin_template Basic Shift Register with Multiple Taps
-- Quartus Prime VHDL Template
-- Basic Shift Register with Multiple Taps

library ieee;
use ieee.std_logic_1164.all;

entity basic_shift_register_with_multiple_taps is

	generic
	(
		DATA_WIDTH : natural := 8;
		NUM_STAGES : natural := 64
	);

	port 
	(
		clk			 : in std_logic;
		enable		 : in std_logic;
		sr_in		 : in std_logic_vector((DATA_WIDTH-1) downto 0);
		sr_tap_one	 : out std_logic_vector((DATA_WIDTH-1) downto 0);
		sr_tap_two	 : out std_logic_vector((DATA_WIDTH-1) downto 0);
		sr_tap_three : out std_logic_vector((DATA_WIDTH-1) downto 0);
		sr_out		 : out std_logic_vector((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of basic_shift_register_with_multiple_taps is

	-- Build a 2-D array type for the shift register
	subtype sr_width is std_logic_vector((DATA_WIDTH-1) downto 0);
	type sr_length is array ((NUM_STAGES-1) downto 0) of sr_width;

	-- Declare the shift register signal
	signal sr: sr_length;

begin

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (enable = '1') then

				-- Shift data by one stage; data from last stage is lost
				sr((NUM_STAGES-1) downto 1) <= sr((NUM_STAGES-2) downto 0);

				-- Load new data into the first stage
				sr(0) <= sr_in;

			end if;
		end if;
	end process;

	-- Capture data from multiple stages in the shift register
	sr_tap_one <= sr((NUM_STAGES/4)-1);
	sr_tap_two <= sr((NUM_STAGES/2)-1);
	sr_tap_three <= sr((3*NUM_STAGES/4)-1);
	sr_out <= sr(NUM_STAGES-1);

end rtl;
end_template
end_group
begin_group State Machines
begin_template Four-State Mealy State Machine
-- Quartus Prime VHDL Template
-- Four-State Mealy State Machine

-- A Mealy machine has outputs that depend on both the state and
-- the inputs.	When the inputs change, the outputs are updated
-- immediately, without waiting for a clock edge.  The outputs
-- can be written more than once per state or per clock cycle.

library ieee;
use ieee.std_logic_1164.all;

entity four_state_mealy_state_machine is

	port
	(
		clk		 : in	std_logic;
		input	 : in	std_logic;
		reset	 : in	std_logic;
		output	 : out	std_logic_vector(1 downto 0)
	);

end entity;

architecture rtl of four_state_mealy_state_machine is

	-- Build an enumerated type for the state machine
	type state_type is (s0, s1, s2, s3);

	-- Register to hold the current state
	signal state : state_type;

begin

	process (clk, reset)
	begin

		if reset = '1' then
			state <= s0;

		elsif (rising_edge(clk)) then

			-- Determine the next state synchronously, based on
			-- the current state and the input
			case state is
				when s0=>
					if input = '1' then
						state <= s1;
					else
						state <= s0;
					end if;
				when s1=>
					if input = '1' then
						state <= s2;
					else
						state <= s1;
					end if;
				when s2=>
					if input = '1' then
						state <= s3;
					else
						state <= s2;
					end if;
				when s3=>
					if input = '1' then
						state <= s3;
					else
						state <= s1;
					end if;
			end case;

		end if;
	end process;

	-- Determine the output based only on the current state
	-- and the input (do not wait for a clock edge).
	process (state, input)
	begin
			case state is
				when s0=>
					if input = '1' then
						output <= "00";
					else
						output <= "01";
					end if;
				when s1=>
					if input = '1' then
						output <= "01";
					else
						output <= "11";
					end if;
				when s2=>
					if input = '1' then
						output <= "10";
					else
						output <= "10";
					end if;
				when s3=>
					if input = '1' then
						output <= "11";
					else
						output <= "10";
					end if;
			end case;
	end process;

end rtl;
end_template
begin_template Four-State Moore State Machine
-- Quartus Prime VHDL Template
-- Four-State Moore State Machine

-- A Moore machine's outputs are dependent only on the current state.
-- The output is written only when the state changes.  (State
-- transitions are synchronous.)

library ieee;
use ieee.std_logic_1164.all;

entity four_state_moore_state_machine is

	port(
		clk		 : in	std_logic;
		input	 : in	std_logic;
		reset	 : in	std_logic;
		output	 : out	std_logic_vector(1 downto 0)
	);

end entity;

architecture rtl of four_state_moore_state_machine is

	-- Build an enumerated type for the state machine
	type state_type is (s0, s1, s2, s3);

	-- Register to hold the current state
	signal state   : state_type;

begin

	-- Logic to advance to the next state
	process (clk, reset)
	begin
		if reset = '1' then
			state <= s0;
		elsif (rising_edge(clk)) then
			case state is
				when s0=>
					if input = '1' then
						state <= s1;
					else
						state <= s0;
					end if;
				when s1=>
					if input = '1' then
						state <= s2;
					else
						state <= s1;
					end if;
				when s2=>
					if input = '1' then
						state <= s3;
					else
						state <= s2;
					end if;
				when s3 =>
					if input = '1' then
						state <= s0;
					else
						state <= s3;
					end if;
			end case;
		end if;
	end process;

	-- Output depends solely on the current state
	process (state)
	begin
		case state is
			when s0 =>
				output <= "00";
			when s1 =>
				output <= "01";
			when s2 =>
				output <= "10";
			when s3 =>
				output <= "11";
		end case;
	end process;

end rtl;
end_template
begin_template Safe State Machine
-- Quartus Prime VHDL Template
-- Safe State Machine

library ieee;
use ieee.std_logic_1164.all;

entity safe_state_machine is

	port(
		clk		 : in	std_logic;
		input	 : in	std_logic;
		reset	 : in	std_logic;
		output	 : out	std_logic_vector(1 downto 0)
	);

end entity;

architecture rtl of safe_state_machine is

	-- Build an enumerated type for the state machine
	type state_type is (s0, s1, s2);

	-- Register to hold the current state
	signal state   : state_type;

	-- Attribute "safe" implements a safe state machine.
	-- This is a state machine that can recover from an
	-- illegal state (by returning to the reset state).
	attribute syn_encoding : string;
	attribute syn_encoding of state_type : type is "safe";

begin

	-- Logic to advance to the next state
	process (clk, reset)
	begin
		if reset = '1' then
			state <= s0;
		elsif (rising_edge(clk)) then
			case state is
				when s0=>
					if input = '1' then
						state <= s1;
					else
						state <= s0;
					end if;
				when s1=>
					if input = '1' then
						state <= s2;
					else
						state <= s1;
					end if;
				when s2=>
					if input = '1' then
						state <= s0;
					else
						state <= s2;
					end if;
			end case;
		end if;
	end process;

	-- Logic to determine output
	process (state)
	begin
		case state is
			when s0 =>
				output <= "00";
			when s1 =>
				output <= "01";
			when s2 =>
				output <= "10";
		end case;
	end process;

end rtl;
end_template
begin_template User-Encoded State Machine
-- Quartus Prime VHDL Template
-- User-Encoded State Machine

library ieee;
use ieee.std_logic_1164.all;

entity user_encoded_state_machine is

	port 
	(
		updown	  : in std_logic;
		clock	  : in std_logic;
		lsb		  : out std_logic;
		msb		  : out std_logic
	);

end entity;

architecture rtl of user_encoded_state_machine is

	-- Build an enumerated type for the state machine
	type count_state is (zero, one, two, three);

	-- Registers to hold the current state and the next state
	signal present_state, next_state	   : count_state;

	-- Attribute to declare a specific encoding for the states
	attribute syn_encoding				  : string;
	attribute syn_encoding of count_state : type is "11 01 10 00";

begin

	-- Determine what the next state will be, and set the output bits
	process (present_state, updown)
	begin
		case present_state is
			when zero =>
				if (updown = '0') then
					next_state <= one;
					lsb <= '0';
					msb <= '0';
				else
					next_state <= three;
					lsb <= '1';
					msb <= '1';
				end if;
			when one =>
				if (updown = '0') then
					next_state <= two;
					lsb <= '1';
					msb <= '0';
				else
					next_state <= zero;
					lsb <= '0';
					msb <= '0';
				end if;
			when two =>
				if (updown = '0') then
					next_state <= three;
					lsb <= '0';
					msb <= '1';
				else
					next_state <= one;
					lsb <= '1';
					msb <= '0';
				end if;
			when three =>
				if (updown = '0') then
					next_state <= zero;
					lsb <= '1';
					msb <= '1';
				else
					next_state <= two;
					lsb <= '0';
					msb <= '1';
				end if;
		end case;
	end process;

	-- Move to the next state
	process
	begin
		wait until rising_edge(clock);
		present_state <= next_state;
	end process;

end rtl;
end_template
end_group
begin_group Arithmetic
begin_group Adders
begin_template Signed Adder
-- Quartus Prime VHDL Template
-- Signed Adder

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signed_adder is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a	   : in signed	((DATA_WIDTH-1) downto 0);
		b	   : in signed	((DATA_WIDTH-1) downto 0);
		result : out signed ((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of signed_adder is
begin

	result <= a + b;

end rtl;
end_template
begin_template Unsigned Adder
-- Quartus Prime VHDL Template
-- Unsigned Adder

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unsigned_adder is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a	   : in unsigned  ((DATA_WIDTH-1) downto 0);
		b	   : in unsigned  ((DATA_WIDTH-1) downto 0);
		result : out unsigned ((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of unsigned_adder is
begin

	result <= a + b;

end rtl;
end_template
begin_template Signed Adder/Subtractor
-- Quartus Prime VHDL Template
-- Signed Adder/Subtractor

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signed_adder_subtractor is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a		: in signed ((DATA_WIDTH-1) downto 0);
		b		: in signed ((DATA_WIDTH-1) downto 0);
		add_sub : in std_logic;
		result	: out signed ((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of signed_adder_subtractor is
begin

	process(a,b,add_sub)
	begin
		-- Add if "add_sub" is 1, else subtract
		if (add_sub = '1') then
			result <= a + b;
		else
			result <= a - b;
		end if;
	end process;

end rtl;
end_template
begin_template Unsigned Adder/Subtractor
-- Quartus Prime VHDL Template
-- Unsigned Adder/Subtractor

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unsigned_adder_subtractor is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a		: in unsigned ((DATA_WIDTH-1) downto 0);
		b		: in unsigned ((DATA_WIDTH-1) downto 0);
		add_sub : in std_logic;
		result	: out unsigned ((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of unsigned_adder_subtractor is
begin

	process(a,b,add_sub)
	begin
		-- add if "add_sub" is 1, else subtract
		if (add_sub = '1') then
			result <= a + b;
		else
			result <= a - b;
		end if;
	end process;

end rtl;
end_template
begin_template Pipelined Binary Adder Tree
-- Quartus Prime VHDL Template
-- Pipelined Binary Adder Tree

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity pipelined_binary_adder_tree is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a	   : in unsigned ((DATA_WIDTH-1) downto 0);
		b	   : in unsigned ((DATA_WIDTH-1) downto 0);
		c	   : in unsigned ((DATA_WIDTH-1) downto 0);
		d	   : in unsigned ((DATA_WIDTH-1) downto 0);
		e	   : in unsigned ((DATA_WIDTH-1) downto 0);
		clk	   : in std_logic;
		result : out unsigned ((DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of pipelined_binary_adder_tree is

	-- Declare registers to hold intermediate sums
	signal sum1, sum2, sum3 : unsigned ((DATA_WIDTH-1) downto 0);

begin

	process (clk)
	begin
		if (rising_edge(clk)) then

			-- Generate and store intermediate values in the pipeline
			sum1 <= a + b;
			sum2 <= c + d;
			sum3 <= sum1 + sum2;

			-- Generate and store the last value, the result
			result <= sum3 + e;

		end if;
	end process;

end rtl;
end_template
end_group
begin_group Counters
begin_template Binary Counter
-- Quartus Prime VHDL Template
-- Binary Counter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity binary_counter is

	generic
	(
		MIN_COUNT : natural := 0;
		MAX_COUNT : natural := 255
	);

	port
	(
		clk		  : in std_logic;
		reset	  : in std_logic;
		enable	  : in std_logic;
		q		  : out integer range MIN_COUNT to MAX_COUNT
	);

end entity;

architecture rtl of binary_counter is
begin

	process (clk)
		variable   cnt		   : integer range MIN_COUNT to MAX_COUNT;
	begin
		if (rising_edge(clk)) then

			if reset = '1' then
				-- Reset the counter to 0
				cnt := 0;

			elsif enable = '1' then
				-- Increment the counter if counting is enabled			   
				cnt := cnt + 1;

			end if;
		end if;

		-- Output the current count
		q <= cnt;
	end process;

end rtl;
end_template
begin_template Binary Up/Down Counter
-- Quartus Prime VHDL Template
-- Binary Up/Down Counter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity binary_up_down_counter is

	generic
	(
		MIN_COUNT : natural := 0;
		MAX_COUNT : natural := 255
	);

	port
	(
		clk		   : in std_logic;
		reset	   : in std_logic;
		enable	   : in std_logic;
		updown	   : in std_logic;
		q		   : out integer range MIN_COUNT to MAX_COUNT
	);

end entity;

architecture rtl of binary_up_down_counter is
	signal direction : integer;
begin

	process (updown)
	begin
		-- Determine the increment/decrement of the counter
		if (updown = '1') then
			direction <= 1;
		else
			direction <= -1;
		end if;
	end process;


	process (clk)
		variable   cnt			: integer range MIN_COUNT to MAX_COUNT;
	begin
		
		-- Synchronously update counter
		if (rising_edge(clk)) then

			if reset = '1' then
				-- Reset the counter to 0
				cnt := 0;

			elsif enable = '1' then
				-- Increment/decrement the counter
				cnt := cnt + direction;

			end if;
		end if;

		-- Output the current count
		q <= cnt;
	end process;

end rtl;
end_template
begin_template Binary Up/Down Counter with Saturation
-- Quartus Prime VHDL Template
-- Binary Up/Down Counter with Saturation

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity binary_up_down_counter_with_saturation is

	generic
	(
		MIN_COUNT : natural := 0;
		MAX_COUNT : natural := 255
	);

	port 
	(
		clk		   : in std_logic;
		reset	   : in std_logic;
		enable	   : in std_logic;
		updown	   : in std_logic;
		q		   : out integer range MIN_COUNT to MAX_COUNT
	);

end entity;

architecture rtl of binary_up_down_counter_with_saturation is
	signal direction : integer;
	signal limit : integer range MIN_COUNT to MAX_COUNT;
begin

	process (updown)
	begin
		-- Set counter increment/decrement, and corresponding limit
		if (updown = '1') then
			direction <= 1;
			limit <= MAX_COUNT;
		else
			direction <= -1;
			limit <= MIN_COUNT;
		end if;
	end process;


	process (clk)
		variable cnt : integer range MIN_COUNT to MAX_COUNT;
	begin

		-- Synchronously update the counter
		if (rising_edge(clk)) then

			if (reset = '1') then
				-- Reset the counter to 0
				cnt := 0;

			elsif (enable = '1' and cnt /= limit) then
				-- Increment/decrement the counter, 
				-- if the limit is not exceeded
				cnt := cnt + direction;

			end if;
		end if;

		-- Output the current count
		q <= cnt;
	end process;

end rtl;
end_template
begin_template Gray Counter
-- Quartus Prime VHDL Template
-- Gray Counter

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gray_counter is

	generic
	(
		WIDTH : natural := 8
	);

	port 
	(
		clk		   : in std_logic;
		reset	   : in std_logic;
		enable	   : in std_logic;
		gray_count : out std_logic_vector(WIDTH-1 downto 0)
	);

end entity;

-- Implementation:

-- There is an imaginary bit in the counter, at q(0), that resets to 1
-- (unlike the rest of the bits of the counter) and flips every clock cycle.
-- The decision of whether to flip any non-imaginary bit in the counter
-- depends solely on the bits below it, down to the imaginary bit.	It flips
-- only if all these bits, taken together, match the pattern 10* (a one
-- followed by any number of zeros).

-- Almost every non-imaginary bit has a component instance that sets the 
-- bit based on the values of the lower-order bits, as described above.
-- The rules have to differ slightly for the most significant bit or else 
-- the counter would saturate at it's highest value, 1000...0.

architecture rtl of gray_counter is
  
	-- q contains all the values of the counter, plus the imaginary bit
	-- (values are shifted to make room for the imaginary bit at q(0))
	signal q  : std_logic_vector (WIDTH downto 0);

	-- no_ones_below(x) = 1 iff there are no 1's in q below q(x)
	signal no_ones_below  : std_logic_vector (WIDTH downto 0);

	-- q_msb is a modification to make the msb logic work
	signal q_msb : std_logic;
  
begin

	q_msb <= q(WIDTH-1) or q(WIDTH);

	process(clk, reset, enable)
	begin

		if(reset = '1') then

			-- Resetting involves setting the imaginary bit to 1
			q(0) <= '1';
			q(WIDTH downto 1) <= (others => '0');

		elsif(rising_edge(clk) and enable='1') then

			-- Toggle the imaginary bit
			q(0) <= not q(0);
	  
			for i in 1 to WIDTH loop

				-- Flip q(i) if lower bits are a 1 followed by all 0's
				q(i) <= q(i) xor (q(i-1) and no_ones_below(i-1));
		
			end loop;  -- i

			q(WIDTH) <= q(WIDTH) xor (q_msb and no_ones_below(WIDTH-1));

		end if;

	end process;

	-- There are never any 1's beneath the lowest bit
	no_ones_below(0) <= '1';

	process(q, no_ones_below)
	begin
		for j in 1 to WIDTH loop
			no_ones_below(j) <= no_ones_below(j-1) and not q(j-1);
		end loop;
	end process;

	-- Copy over everything but the imaginary bit
	gray_count <= q(WIDTH downto 1);
	  
end rtl;
end_template
end_group
begin_group Multipliers
begin_template Unsigned Multiply
-- Quartus Prime VHDL Template
-- Unsigned Multiply

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unsigned_multiply is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a	   : in unsigned ((DATA_WIDTH-1) downto 0);
		b	   : in unsigned ((DATA_WIDTH-1) downto 0);
		result  : out unsigned ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of unsigned_multiply is
begin

	result <= a * b;

end rtl;
end_template
begin_template Signed Multiply
-- Quartus Prime VHDL Template
-- Signed Multiply

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signed_multiply is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a	   : in signed ((DATA_WIDTH-1) downto 0);
		b	   : in signed ((DATA_WIDTH-1) downto 0);
		result  : out signed ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of signed_multiply is
begin

	result <= a * b;

end rtl;
end_template
begin_template Unsigned Multiply with Input and Output Registers
-- Quartus Prime VHDL Template
-- Unsigned Multiply with Input and Output Registers

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unsigned_multiply_with_input_and_output_registers is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a		: in unsigned ((DATA_WIDTH-1) downto 0);
		b		: in unsigned ((DATA_WIDTH-1) downto 0);
		clk		: in std_logic;
		clear	    : in std_logic;
		result	: out unsigned ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of unsigned_multiply_with_input_and_output_registers is

	-- Declare I/O registers
	signal a_reg, b_reg : unsigned ((DATA_WIDTH-1) downto 0);
	signal out_reg	  : unsigned ((2*DATA_WIDTH-1) downto 0);

begin

	process (clk, clear)
	begin
		if (clear ='1') then

			-- Reset all register data to 0
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			out_reg <= (others => '0');

		elsif (rising_edge(clk)) then

			-- Store input and output values in registers
			a_reg <= a;
			b_reg <= b;
			out_reg <= a_reg * b_reg;

		end if;
	end process;

	-- Output multiplication result
	result <= out_reg;

end rtl;
end_template
begin_template Signed Multiply with Input and Output Registers
-- Quartus Prime VHDL Template
-- Signed Multiply with Input and Output Registers

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signed_multiply_with_input_and_output_registers is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	( 
		a		: in signed ((DATA_WIDTH-1) downto 0);
		b		: in signed ((DATA_WIDTH-1) downto 0);
		clk		: in std_logic;
		clear	    : in std_logic;
		result	: out signed ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of signed_multiply_with_input_and_output_registers is

	-- Declare I/O registers
	signal a_reg, b_reg : signed ((DATA_WIDTH-1) downto 0);
	signal out_reg	  : signed ((2*DATA_WIDTH-1) downto 0);

begin

	process (clk, clear)
	begin
		if (clear = '1') then

			-- Reset all register data to 0
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			out_reg <= (others => '0');

		elsif (rising_edge(clk)) then

			-- Store input and output values in registers
			a_reg <= a;
			b_reg <= b;
			out_reg <= a_reg * b_reg;

		end if;
	end process;

	-- Output multiplication result
	result <= out_reg;

end rtl;
end_template
begin_template Multiplier for Complex Numbers
-- Quartus Prime VHDL Template
-- Multiplier for complex numbers

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity multiplier_for_complex_numbers is

	generic
	(
		WIDTH : natural := 18
	);

	port 
	(
		clk, ena	                    : in std_logic;

		-- dataa and datab each have a real and imaginary part
		dataa_real, dataa_img	: in signed ((WIDTH-1) downto 0);
		datab_real, datab_img	: in signed ((WIDTH-1) downto 0);

		dataout_real, dataout_img	: out signed ((2*WIDTH-1) downto 0)
	);

end entity;

architecture rtl of multiplier_for_complex_numbers is
begin

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then

				-- Calculate both the real and imaginary parts of the product
				dataout_real <= dataa_real * datab_real - dataa_img * datab_img;
				dataout_img <= dataa_real * datab_img + datab_real * dataa_img;

			end if;
		end if;
	end process;
end rtl;
end_template
end_group
begin_group Multiply Accumulators
begin_template Unsigned Multiply-Accumulate
-- Quartus Prime VHDL Template
-- Unsigned Multiply-Accumulate

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity unsigned_multiply_accumulate is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a		   : in unsigned ((DATA_WIDTH-1) downto 0);
		b		   : in unsigned ((DATA_WIDTH-1) downto 0);
		clk		   : in std_logic;
		sload	   : in std_logic;
		accum_out    : out unsigned ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of unsigned_multiply_accumulate is

	-- Declare registers for intermediate values
	signal a_reg : unsigned ((DATA_WIDTH-1) downto 0);
	signal b_reg : unsigned ((DATA_WIDTH-1) downto 0);
	signal sload_reg : std_logic;
	signal mult_reg : unsigned ((2*DATA_WIDTH-1) downto 0);
	signal adder_out : unsigned ((2*DATA_WIDTH-1) downto 0);
	signal old_result : unsigned ((2*DATA_WIDTH-1) downto 0);

begin

	mult_reg <= a_reg * b_reg;

	process (adder_out, sload_reg)
	begin
		if (sload_reg = '1') then
			-- Clear the accumulated data
			old_result <= (others => '0');
		else
			old_result <= adder_out;
		end if;
	end process;

	process (clk)
	begin
		if (rising_edge(clk)) then
			a_reg <= a;
			b_reg <= b;
			sload_reg <= sload;

			-- Store accumulation result in a register
			adder_out <= old_result + mult_reg;

		end if;
	end process;

	-- Output accumulation result
	accum_out <= adder_out;

end rtl;
end_template
begin_template Signed Multiply-Accumulate
-- Quartus Prime VHDL Template
-- Signed Multiply-Accumulate

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signed_multiply_accumulate is

	generic
	(
		DATA_WIDTH : natural := 8
	);

	port 
	(
		a		   : in signed((DATA_WIDTH-1) downto 0);
		b		   : in signed ((DATA_WIDTH-1) downto 0);
		clk		   : in std_logic;
		sload	   : in std_logic;
		accum_out    : out signed ((2*DATA_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of signed_multiply_accumulate is

	-- Declare registers for intermediate values
	signal a_reg : signed ((DATA_WIDTH-1) downto 0);
	signal b_reg : signed ((DATA_WIDTH-1) downto 0);
	signal sload_reg : std_logic;
	signal mult_reg : signed ((2*DATA_WIDTH-1) downto 0);
	signal adder_out : signed ((2*DATA_WIDTH-1) downto 0);
	signal old_result : signed ((2*DATA_WIDTH-1) downto 0);

begin

	mult_reg <= a_reg * b_reg;

	process (adder_out, sload_reg)
	begin
		if (sload_reg = '1') then
			-- Clear the accumulated data
			old_result <= (others => '0');
		else
			old_result <= adder_out;
		end if;
	end process;

	process (clk)
	begin
		if (rising_edge(clk)) then
			a_reg <= a;
			b_reg <= b;
			sload_reg <= sload;

			-- Store accumulation result in a register
			adder_out <= old_result + mult_reg;

		end if;
	end process;

	-- Output accumulation result
	accum_out <= adder_out;

end rtl;
end_template
begin_template Sum-of-Four Multiply-Accumulate
-- Quartus Prime VHDL Template
-- Sum-of-four multiply-accumulate
-- For use with the Stratix III device family

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_four_multiply_accumulate is

	generic
	(
		INPUT_WIDTH	  : natural := 18;
		OUTPUT_WIDTH   : natural := 44
	);

	port
	(
		clk, ena		       : in std_logic;
		a, b, c, d, e, f, g, h	: in signed ((INPUT_WIDTH-1) downto 0);
		dataout			: out signed ((OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of sum_of_four_multiply_accumulate is

	-- Each product can be up to 2*INPUT_WIDTH bits wide.
	-- The sum of four of these products can be up to 2 bits wider.
	signal mult_sum : signed ((2*INPUT_WIDTH+1) downto 0); 

	signal accum_reg : signed ((OUTPUT_WIDTH-1) downto 0);

	-- At least one product (of the four we're adding together) 
	-- must be as wide as the sum
	signal resized_a_times_b : signed ((2*INPUT_WIDTH+1) downto 0);
	signal resized_e_times_f : signed ((2*INPUT_WIDTH+1) downto 0);
begin

	-- Resize the product a*b so we won't lose carry bits when adding
	resized_a_times_b <= RESIZE(a * b, 2*INPUT_WIDTH+2);
	resized_e_times_f <= RESIZE(e * f, 2*INPUT_WIDTH+2);

	-- Store the results of the operations on the current inputs
	mult_sum <= (resized_a_times_b + c *d) + (resized_e_times_f + g * h);

	-- Store the value of the accumulation in a register
	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then
				accum_reg <= accum_reg + mult_sum;
			end if;
		end if;
	end process;

	dataout <= accum_reg;
end rtl;
end_template
begin_template Sum-of-Four Multiply-Accumulate with Asynchronous Reset
-- Quartus Prime VHDL Template
-- Sum-of-four multiply-accumulate with asynchronous reset
-- For use with the Stratix III device family

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_four_multiply_accumulate_with_asynchronous_reset is

	generic
	(
		INPUT_WIDTH	  : natural := 18;
		OUTPUT_WIDTH   : natural := 44
	);

	port
	(
		clk, ena, aclr		: in std_logic;
		a, b, c, d, e, f, g, h	: in signed ((INPUT_WIDTH-1) downto 0);
		dataout			: out signed ((OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of sum_of_four_multiply_accumulate_with_asynchronous_reset is

	-- Each product can be up to 2*INPUT_WIDTH bits wide.
	-- The sum of four of these products can be up to 2 bits wider.
	signal mult_sum : signed ((2*INPUT_WIDTH+1) downto 0); 

	signal accum_reg : signed ((OUTPUT_WIDTH-1) downto 0);

	-- At least one product (of the four we're adding together) 
	-- must be as wide as the sum
	signal resized_a_times_b : signed ((2*INPUT_WIDTH+1) downto 0);
	signal resized_e_times_f : signed ((2*INPUT_WIDTH+1) downto 0);
begin

	-- Resize the product a*b so we won't lose carry bits when adding
	resized_a_times_b <= RESIZE(a * b, 2*INPUT_WIDTH+2);
	resized_e_times_f <= RESIZE(e * f, 2*INPUT_WIDTH+2);

	-- Store the results of the operations on the current inputs
	mult_sum <= (resized_a_times_b + c *d) + (resized_e_times_f + g * h);

	-- Store the value of the accumulation in a register
	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then
				if (aclr = '1') then
					accum_reg <= RESIZE(mult_sum, accum_reg'length);
				else
					accum_reg <= accum_reg + mult_sum;
				end if;
			end if;
		end if;
	end process;

	dataout <= accum_reg;
end rtl;
end_template
end_group
begin_group Sums of Multipliers
begin_template Sum of Four Multipliers
-- Quartus Prime VHDL Template
-- Sum of four multipliers

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_four_multipliers is

	generic
	(
		WIDTH : natural := 18
	);

	port
	(
		clk, ena		        : in std_logic;
		a, b, c, d, e, f, g, h	: in signed ((WIDTH-1) downto 0);
		dataout			: out signed ((2*WIDTH+1) downto 0)
	);

end entity;

architecture rtl of sum_of_four_multipliers is

	-- At least one product (of the four we're adding together) 
	-- must be as wide as the sum
	signal resized_a_times_b : signed ((2*WIDTH+1) downto 0);
	signal resized_e_times_f : signed ((2*WIDTH+1) downto 0);

begin

	-- Resize the product a*b so we won't lose carry bits when adding
	resized_a_times_b <= RESIZE(a * b, 2*WIDTH+2);
	resized_e_times_f <= RESIZE(e * f, 2*WIDTH+2);

	-- dataout is the sum of four products
	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then
				dataout <= (resized_a_times_b + c *d) + (resized_e_times_f + g * h);
			end if;
		end if;
	end process;

end rtl;
end_template
begin_template Sum of Two Multipliers with Pipeline Registers
-- Quartus Prime VHDL Template
-- Sum of two multipliers with pipeline registers
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_two_multipliers_pipeline is

	generic
	(
		WIDTH : natural := 16
	);

	port
	(
		clk, aclr : in std_logic;
		a, b, c, d : in unsigned ((WIDTH-1) downto 0);
		result : out unsigned ((2*WIDTH) downto 0)
	);


end entity;

architecture rtl of sum_of_two_multipliers_pipeline is
	signal a_reg, b_reg, c_reg, d_reg : unsigned ((WIDTH-1) downto 0);
	signal pdt_reg, pdt2_reg : unsigned ((2*WIDTH-1) downto 0);
	signal result_reg : unsigned ((2*WIDTH) downto 0);
begin
	PROCESS (clk, aclr)
	BEGIN
		IF (aclr = '1') THEN
			a_reg <= (OTHERS => '0');
			b_reg <= (OTHERS => '0');
			c_reg <= (OTHERS => '0');
			d_reg <= (OTHERS => '0');
			pdt_reg <= (OTHERS => '0');
			pdt2_reg <= (OTHERS => '0');
		ELSIF (clk'event AND clk = '1') THEN
			a_reg <= a;
			b_reg <= b;
			c_reg <= c;
			d_reg <= d;
			pdt_reg <= a_reg * b_reg;
			pdt2_reg <= c_reg * d_reg;
			result_reg <= resize(pdt_reg,2*WIDTH+1) + resize(pdt2_reg,2*WIDTH+1);
		END IF;
	END PROCESS;
	result <= result_reg;
end rtl;
end_template
begin_template Sum of Four Multipliers in Scan Chain Mode
-- Quartus Prime VHDL Template
-- Sum of four multipliers in scan chain mode
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_four_multipliers_scan_chain is

	generic
	(
		WIDTH : natural := 18
	);

	port
	(
		clk, ena		        : in std_logic;
		dataa			: in signed ((WIDTH-1) downto 0);
		c0, c1, c2, c3		: in signed ((WIDTH-1) downto 0);
		dataout			: out signed ((2*WIDTH+1) downto 0)
	);

end entity;

architecture rtl of sum_of_four_multipliers_scan_chain is
	-- Four scan chain registers
	signal a0, a1, a2, a3 : signed ((WIDTH-1) downto 0);

	-- At least one product (of the four we're adding together) 
	-- must be as wide as the sum
	signal resized_a3_times_c3 : signed ((2*WIDTH+1) downto 0);
	signal resized_a1_times_c1 : signed ((2*WIDTH+1) downto 0);
begin

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then

				-- The scan chain (which mimics a shift register)
				a0 <= dataa;
				a1 <= a0;
				a2 <= a1;
				a3 <= a2;
				
				-- Resize product a3*c3 so we won't lose carry bits
				resized_a3_times_c3 <= RESIZE(a3 * c3, 2*WIDTH+2);
				resized_a1_times_c1 <= RESIZE(a1 * c1, 2*WIDTH+2);

				-- The order of the operands is important for correct inference
				dataout <= (resized_a3_times_c3 + a2 * c2) + (resized_a1_times_c1 + a0 * c0); 
			end if;
		end if;
	end process;
end rtl;
end_template
begin_template Sum of Eight Multipliers in Chainout Mode
-- Quartus Prime VHDL Template
-- Sum of eight multipliers in chainout mode

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_eight_multipliers_chainout is

	generic
	(
		WIDTH : natural := 18
	);

	port
	(
		clk, ena			        : in std_logic;
		a0, a1, a2, a3, a4, a5, a6, a7	: in signed ((WIDTH-1) downto 0);
		b0, b1, b2, b3 ,b4, b5, b6, b7	: in signed ((WIDTH-1) downto 0);
		dataout				: out signed ((2*WIDTH+2) downto 0)
	);

end entity;

architecture rtl of sum_of_eight_multipliers_chainout is 

	-- Declare signals for intermediate values
	signal sum1, sum2 : signed ((2*WIDTH+2) downto 0);

	-- At least two products (of the eight we're adding together) 
	-- must be as wide as the sum
	signal resized_a0_times_b0 : signed ((2*WIDTH+2) downto 0);
	signal resized_a2_times_b2 : signed ((2*WIDTH+2) downto 0);
	signal resized_a4_times_b4 : signed ((2*WIDTH+2) downto 0);
	signal resized_a6_times_b6 : signed ((2*WIDTH+2) downto 0);

begin

	-- Resize products a0*b0 and a4*b4 so we won't lose carry bits 
	resized_a0_times_b0 <= RESIZE(a0 * b0, 2*WIDTH+3);
	resized_a2_times_b2 <= RESIZE(a2 * b2, 2*WIDTH+3);
	resized_a4_times_b4 <= RESIZE(a4 * b4, 2*WIDTH+3);
	resized_a6_times_b6 <= RESIZE(a6 * b6, 2*WIDTH+3);

	-- Store the results of the first two sums
	sum1 <= (resized_a0_times_b0 + a1 * b1) + (resized_a2_times_b2 + a3 * b3);
	sum2 <= (resized_a4_times_b4 + a5 * b5) + (resized_a6_times_b6 + a7 * b7);

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then
				dataout <= sum1 + sum2;
			end if;
		end if;
	end process;
end rtl;
end_template
begin_template Sum of Two Multipliers with a Wide Datapath
-- Quartus Prime VHDL Template
-- Sum of two multipliers with a wide datapath

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_two_multipliers_wide_datapath is

	generic
	(
		WIDTH_A	: natural := 36;
		WIDTH_B	: natural := 18
	);

	port
	(
		clk, ena	        : in std_logic;
		a0, a1		: in signed ((WIDTH_A-1) downto 0);
		b0, b1		: in signed ((WIDTH_B-1) downto 0);
		dataout		: out signed ((WIDTH_A+WIDTH_B) downto 0)
	);

end entity;

architecture rtl of sum_of_two_multipliers_wide_datapath is
	-- At least one product (of the two we're adding together) 
	-- must be as wide as the sum
	signal resized_a0_times_b0 : signed ((WIDTH_A+WIDTH_B) downto 0);
begin

	-- Resize the product a0*b0 so we won't lose carry bits when adding
	resized_a0_times_b0 <= RESIZE(a0 * b0, WIDTH_A+WIDTH_B+1);

	process (clk)
	begin
		if (rising_edge(clk)) then
			if (ena = '1') then
				dataout <= resized_a0_times_b0 + a1 * b1;
			end if;
		end if;
	end process;
end rtl;
end_template
end_group
begin_group DSP Features 
begin_group DSP Features for 28-nm Device 
begin_template Single Multiply
-- Quartus Prime VHDL Template
-- Independent multiply
-- For use with the 28-nm device families

-- This template is applicable to 9x9, 18x18, 27x27, 36x18, 36x36 modes on Stratix-V
-- This template is applicable to 9x9, 18x19(signed), 27x27 modes on Arria-V and Cyclone-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_mult is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
	-- each multiplier can be signed or unsigned
	-- for mixed-sign multiplication, refer to mixed-sign template
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in signed	((B_WIDTH-1) downto 0);
		p	   : out signed ((A_WIDTH+B_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of single_mult is
begin

	p <= a * b;

end rtl;
end_template
begin_template Sum of Two Multipliers
-- Quartus Prime VHDL Template
-- Sum of two multipliers
-- For use with the 28-nm device families

-- This template is applicable to sum-of-2 18x18, 27x27, 36x18 modes on Stratix-V
-- This template is applicable to sum-of-2 18x19(signed) mode on Arria-V and Cyclone-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_2 is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+B_WIDTH) downto 0)
	);

end entity;

architecture rtl of sum_of_2 is

signal p1, p2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

begin

	p1 <= (a1 * b1);
	p2 <= (a2 * b2);

	-- Static add/sub is supported
	p <= resize(p1, A_WIDTH+B_WIDTH+1) + resize(p2, A_WIDTH+B_WIDTH+1);

end rtl;
end_template
begin_template Sum of Four Multipliers
-- Quartus Prime VHDL Template
-- Sum of four multipliers
-- For use with the 28-nm device families

-- This template is applicable to sum-of-4 18x18 mode on Stratix-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sum_of_4 is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		b3	   : in signed	((B_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		b4	   : in signed	((B_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+B_WIDTH+1) downto 0)
	);

end entity;

architecture rtl of sum_of_4 is

signal p1, p2, p3, p4 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

begin

	p1 <= (a1 * b1);
	p2 <= (a2 * b2);
	p3 <= (a3 * b3);
	p4 <= (a4 * b4);	

	-- Static add/sub is supported
	-- resize all p1, p2, p3, p4 to be the same size as p
	p <= resize(p1, A_WIDTH+B_WIDTH+2) + resize(p2, A_WIDTH+B_WIDTH+2) - 
		resize(p3, A_WIDTH+B_WIDTH+2) + resize(p4, A_WIDTH+B_WIDTH+2);

end rtl;
end_template
begin_template Mixed Sign Multiply
-- Quartus Prime VHDL Template
-- Mixed sign multiply
-- For use with the 28-nm device families

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mixed_sign is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in unsigned	((B_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+B_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of mixed_sign is

signal p_extend : signed ((A_WIDTH+B_WIDTH) downto 0);

begin

-- Note that mixed-sign multiplier also has a_width+b_width bits result
-- Guaranteed no overflow
	p_extend <= a * signed( resize(b, B_WIDTH+1) );
	p <= resize(p_extend, A_WIDTH+B_WIDTH);

end rtl;
end_template
begin_template Dynamic Add/Sub Control
-- Quartus Prime VHDL Template
-- Dynamic add/sub control
-- For use with the 28-nm device families

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dynamic_addsub is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18;
		O_WIDTH : natural := 64
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		b3	   : in signed	((B_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		b4	   : in signed	((B_WIDTH-1) downto 0);
		a5	   : in signed	((A_WIDTH-1) downto 0);
		b5	   : in signed	((B_WIDTH-1) downto 0);
		a6	   : in signed	((A_WIDTH-1) downto 0);
		b6	   : in signed	((B_WIDTH-1) downto 0);
		addnsub12			: in std_logic;
		addnsub34			: in std_logic;
		addnsub56			: in std_logic;
		subnadd_chain34	: in std_logic;
		subnadd_chain56	: in std_logic;
		clock : in std_logic;
		ena 	: in std_logic;
		reset	: in std_logic;
		s : out signed ((O_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of dynamic_addsub is

signal a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, a6_reg : signed ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, b6_reg : signed ((B_WIDTH-1) downto 0);
signal s12, s1234, s1234_reg, s_tmp : signed ((O_WIDTH-1) downto 0);

--Dynamic add/sub can be used in basic modes: sum-of-2 18x18, 36+18x18, two level sum-of-4 18x18, sum-of-2 27x27 and sum-of-2 36x18.
--Input and output signals of the dynamic add/sub operation must be explicitly defined. 
signal p1, p2, p3, p4, p5, p6 : signed ((A_WIDTH+B_WIDTH-1) downto 0);
signal p12, p34, p56 : signed ((A_WIDTH+B_WIDTH) downto 0);

begin

	p1 <= a1_reg * b1_reg;
	p2 <= a2_reg * b2_reg;
	p3 <= a3_reg * b3_reg;
	p4 <= a4_reg * b4_reg;
	p5 <= a5_reg * b5_reg;
	p6 <= a6_reg * b6_reg;
	
	with addnsub12 select
	p12 <= 	(resize(p1, A_WIDTH+B_WIDTH+1) + resize(p2, A_WIDTH+B_WIDTH+1)) when '1',
				(resize(p1, A_WIDTH+B_WIDTH+1) - resize(p2, A_WIDTH+B_WIDTH+1)) when others;
	
	with addnsub34 select
	p34 <= 	(resize(p3, A_WIDTH+B_WIDTH+1) + resize(p4, A_WIDTH+B_WIDTH+1)) when '1',
				(resize(p3, A_WIDTH+B_WIDTH+1) - resize(p4, A_WIDTH+B_WIDTH+1)) when others;
	
	with addnsub56 select
	p56 <= 	(resize(p5, A_WIDTH+B_WIDTH+1) + resize(p6, A_WIDTH+B_WIDTH+1)) when '1',
				(resize(p5, A_WIDTH+B_WIDTH+1) - resize(p6, A_WIDTH+B_WIDTH+1)) when others;

	with subnadd_chain34 select
	s1234 <= 	(s12 - resize(p34, O_WIDTH)) when '1',
					(s12 + resize(p34, O_WIDTH)) when others;

	with subnadd_chain56 select
	s_tmp <= 	(s1234_reg - resize(p56, O_WIDTH)) when '1',
					(s1234_reg + resize(p56, O_WIDTH)) when others;				

					

--Dynamic add/sub is also applicable to chainout adder or accumulator (not both). 
--Dynamic add/sub is not applicable to 18x18 systolic mode.
process(clock, reset)
begin
	if (reset = '1') then
	
		a1_reg <= (others => '0');
		a2_reg <= (others => '0');
		a3_reg <= (others => '0');
		a4_reg <= (others => '0');
		a5_reg <= (others => '0');
		a6_reg <= (others => '0');
		b1_reg <= (others => '0');
		b2_reg <= (others => '0');
		b3_reg <= (others => '0');
		b4_reg <= (others => '0');
		b5_reg <= (others => '0');
		b6_reg <= (others => '0');
		s12 <= (others => '0');
		s1234_reg <= (others => '0');
		s <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
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
			s12 <= resize(p12, O_WIDTH);
			s1234_reg <= s1234;
			s <= s_tmp;
		end if;
	
	end if;

end process;

end rtl;
end_template
begin_template Sum of an 18x18 Multiplier and a 36-bit Addend
-- Quartus Prime VHDL Template
-- Sum of an 18x18 multiplier and a 36-bit addend  
-- For use with the 28-nm device families

-- This template is applicable to 36+18x18 mode on Stratix-V
-- This template is applicable to 36+18x19(signed) mode on Arria-V and Cyclone-V
-- Note that the addend shouldn't be from another multiplier

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity plus36_18x18 is
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in signed	((B_WIDTH-1) downto 0);
		addend : in signed ((A_WIDTH+B_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+B_WIDTH) downto 0)
	);

end entity;

architecture rtl of plus36_18x18 is

signal p1 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

begin

	p1 <= a * b;
	
	-- addend must be the first operand
	-- Static add/sub is supported
	p <= resize(addend, A_WIDTH+B_WIDTH+1) - resize(p1, A_WIDTH+B_WIDTH+1);

end rtl;
end_template
begin_template Complex 25x18 Multiply
-- Quartus Prime VHDL Template
-- Complex 25x18 multiply
-- For use with the 28-nm device families

-- This template is applicable to complex 25x18 mode on Stratix-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity complex_25x18 is
	port 
	(
		x_r	   : in signed	(24 downto 0);
		x_i	   : in signed	(24 downto 0);
		y_r	   : in signed	(17 downto 0);
		y_i	   : in signed	(17 downto 0);
		-- Stratix-V DSP supports up to 3 clock/ena pairs and 2 async reset signals
		clock 	: in std_logic;
		ena0		: in std_logic;
		ena1		: in std_logic;
		reset		: in std_logic;
		p_r		: out signed (43 downto 0);
		p_i		: out signed (43 downto 0)
	);

end entity;

architecture rtl of complex_25x18 is

signal a1							: signed	(25 downto 0);
signal a2, a3						: signed	(18 downto 0);
signal p1,p2,p3	   			: signed	(43 downto 0);
-- All inputs/outputs have to be signed.
-- All input registers must use the same {clock, ena, reset}
-- All output registers must use the same {clock, ena, reset}
signal x_r_reg, x_i_reg			: signed	(24 downto 0);
signal y_r_reg, y_i_reg			: signed	(17 downto 0);

begin

a1 <= resize(x_r_reg, 26) - resize(x_i_reg, 26);
p1 <= a1 * y_i_reg;

a2 <= resize(y_r_reg, 19) - resize(y_i_reg, 19);
p2 <= a2 * x_r_reg;

a3 <= resize(y_r_reg, 19) + resize(y_i_reg, 19);
p3 <= a3 * x_i_reg;

process(clock, reset)
begin
	if (reset = '1') then

		x_r_reg <= (others => '0');
		x_i_reg <= (others => '0');
		y_r_reg <= (others => '0');
		y_i_reg <= (others => '0');
		p_r <= (others => '0');
		p_i <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena0 = '1') then
			x_r_reg <= x_r;
			x_i_reg <= x_i;
			y_r_reg <= y_r;
			y_i_reg <= y_i;
		end if;
		
		if (ena1 = '1') then
			p_r <= p1 + p2;
			p_i <= p1 + p3;
		end if;
	
	end if;

end process;


end rtl;
end_template
begin_template Chainout Adder/Accumulator Feature
-- Quartus Prime VHDL Template
-- Chainout adder/accumulator feature
-- For use with the 28-nm device families

-- This template can be used with 18x18, sum-of-2 18x18, 36+18x18, 27x27, 36x18 modes on Stratix-V
-- This template can be used with 18x19(signed), 36+18x19(signed), 27x27 modes on Arria-V and Cyclone-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity chainout_accum is
	generic
	(
		A_WIDTH 		: natural := 18;
		B_WIDTH 		: natural := 18;
		-- When this template applies to single 18x18, max chain_width is 44, othewise 64
		CHAIN_WIDTH : natural := 44;
		-- preload_value can have only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Double accumulator feature is not available on Stratix-V
		enable_double_accum : boolean := FALSE
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		b3	   : in signed	((B_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		b4	   : in signed	((B_WIDTH-1) downto 0);
		a5	   : in signed	((A_WIDTH-1) downto 0);
		b5	   : in signed	((B_WIDTH-1) downto 0);
		accum : in std_logic;
		load_value : in std_logic;
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- All output registers must have the same {clock, ena, reset}
		clock : in std_logic;
		ena	: in std_logic;
		reset	: in std_logic;
		s		: out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of chainout_accum is
signal zero_bit					: signed	(0 downto 0);
signal zero_bit_a				: signed	((A_WIDTH-1) downto 0);
signal zero_bit_b				: signed	((B_WIDTH-1) downto 0);
signal p1, p2, p3, p4, p5, p6			: signed	((A_WIDTH+B_WIDTH-1) downto 0);
signal s1, s2, s3, s4, s5 ,s_reg		: signed	((CHAIN_WIDTH-1) downto 0);
signal acc_sel, s_double				: signed	((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback 		: signed	((CHAIN_WIDTH-1) downto 0);

component single_mult 
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in signed	((B_WIDTH-1) downto 0);
		p 		: out signed ((A_WIDTH+B_WIDTH-1) downto 0)
	);

end component;

begin
-- Assign zero bit
zero_bit <= B"0";

-- accumulator path
with accum select
	acc_sel <= 	select_feedback when '1',
					selected_value when others;

with enable_double_accum select
	select_feedback <= 	s_double when TRUE,
								s_reg when others;
					
with load_value select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
							resize(zero_bit, CHAIN_WIDTH) when others;
s	<= s_reg;
	
process(clock, reset)
begin
	if (reset = '1') then

		s1 <= (others => '0');
		s2 <= (others => '0');
		s3 <= (others => '0');
		s4 <= (others => '0');
		s5 <= (others => '0');
		s_reg <= (others => '0');
		s_double <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
			-- chainout adder support static add or sub
			-- basic mode result (p) must be the second operand
			s1 <= resize(p1, CHAIN_WIDTH);
			s2 <= s1 + resize(p2, CHAIN_WIDTH);
			s3 <= s2 + resize(p3, CHAIN_WIDTH);
			s4 <= s3 - resize(p4, CHAIN_WIDTH);
			s5 <= s4 + resize(p5, CHAIN_WIDTH);			
			-- chainout accumulator only support addition when use with chainout adder
			s_reg <= acc_sel + (s5 + resize(p6, CHAIN_WIDTH));	-- loopback path (acc_sel) must be the first operand
			s_double <= s_reg;
		end if;
	
	end if;

end process;

mult1 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a1,
		b	=> b1,
		p 	=> p1
	);

mult2 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a2,
		b	=> b2,
		p 	=> p2
	);
	
mult3 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a3,
		b	=> b3,
		p 	=> p3
	);

mult4 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a4,
		b	=> b4,
		p 	=> p4
	);

mult5 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a5,
		b	=> b5,
		p 	=> p5
	);	

-- When this template applies to single 18x18, the number of multipliers has to be even
-- Create a 0x0 if the number of multipliers is odd

zero_bit_a <= resize(zero_bit, A_WIDTH);
zero_bit_b <= resize(zero_bit, B_WIDTH);

mult6 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> zero_bit_a,
		b	=> zero_bit_b,
		p 	=> p6
	);		
	
end rtl;
end_template
begin_template Chainout Adder with Rounding
-- Quartus Prime VHDL Template
-- Chainout adder with rounding
-- For use with the 28-nm device families

-- This template can be used with 18x18, sum-of-2 18x18, 36+18x18, 27x27, 36x18 modes on Stratix-V
-- This template can be used with 18x19(signed), 36+18x19(signed), 27x27 modes on Arria-V and Cyclone-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity chainout_rnd is
	generic
	(
		A_WIDTH 		: natural := 27;
		B_WIDTH 		: natural := 27;
		-- When this template applies to single 18x18, max chain_width is 44, othewise 64
		CHAIN_WIDTH : natural := 64;
		ROUNDING_BIT : std_logic_vector := X"2000"		
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		b3	   : in signed	((B_WIDTH-1) downto 0);
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- All output registers must have the same {clock, ena, reset}
		clock : in std_logic;
		ena	: in std_logic;
		reset	: in std_logic;
		s		: out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of chainout_rnd is

signal p1, p2, p3					: signed	((A_WIDTH+B_WIDTH-1) downto 0);
signal s1, s2						: signed	((CHAIN_WIDTH-1) downto 0);

component single_mult 
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in signed	((B_WIDTH-1) downto 0);
		p 		: out signed ((A_WIDTH+B_WIDTH-1) downto 0)
	);

end component;

begin

process(clock, reset)
begin
	if (reset = '1') then

		s1 <= (others => '0');
		s2 <= (others => '0');
		s <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
			-- chainout adder support static add or sub
			-- basic mode result (p) must be the second operand
			s1 <= resize(p1, CHAIN_WIDTH);
			s2 <= s1 + resize(p2, CHAIN_WIDTH);
			-- rounding bit sign has to match with other operands
			s  <= resize(signed (ROUNDING_BIT), CHAIN_WIDTH) + (s2 + resize(p3, CHAIN_WIDTH));
		end if;
	
	end if;

end process;

mult1 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a1,
		b	=> b1,
		p 	=> p1
	);

mult2 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a2,
		b	=> b2,
		p 	=> p2
	);
	
mult3 : single_mult
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => B_WIDTH
	)	
	port MAP
	(
		a	=> a3,
		b	=> b3,
		p 	=> p3
	);
	
end rtl;
end_template
begin_template Multiplier with One Operand from Coefficient ROM
-- Quartus Prime VHDL Template
-- Multiplier with one operand from coefficient ROM
-- For use with the 28-nm device families

-- This template can be used with 18x18, sum-of-2 18x18, sum-of-4 18x18, 27x27, sum-of-2 27x27 modes on Stratix-V
-- This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity input_coef is
	generic
	(
		A_WIDTH 	: natural := 17;
		COEF_WIDTH 	: natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3
	);

	port 
	(
		a1	   	: in signed	((A_WIDTH-1) downto 0);
		a2	   	: in signed	((A_WIDTH-1) downto 0);
		c1_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- each registered data input can use independent {clock, ena}
		clock 	: in std_logic;
		ena		: in std_logic; 
		-- all registered data input must use the same reset
		reset		: in std_logic;
		s			: out signed ((A_WIDTH + COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of input_coef is
-- This template use integer type as the coeffecient constant
-- Can use other preferred type for example signed/unsigned  

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c1_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"001010111111101011",
			"001010000011101011",
			"001010110000001011",
			"001010111111101011",
			"001010111111101011");
			 
signal c2_coef : coef_type :=
			("010101011010100100",
			"011010101110101010",
			"001010110111011011",
			"001010110101101010",
			"101010100011101011",
			"001011011000001010",
			"011010111111101011",
			"001010101001000110");

constant MULTIPLY_RESULT_WIDTH : natural := (A_WIDTH + COEF_WIDTH);


-- Two 18x18 in one DSP block must use coefficient storage simultaneously
signal a1_reg	   : signed	((A_WIDTH-1) downto 0);
signal a2_reg	   : signed	((A_WIDTH-1) downto 0);
signal c1_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c1_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal p				: signed	((A_WIDTH+COEF_WIDTH) downto 0);


-- Pick an applicable basic mode template
component sum_of_2 
	generic
	(
		A_WIDTH : natural := A_WIDTH;
		B_WIDTH : natural := COEF_WIDTH
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((COEF_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((COEF_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+COEF_WIDTH) downto 0)
	);

end component;

begin

-- Register 
process(clock, reset)
begin
	if (reset = '1') then

		a1_reg <= (others => '0');
		a2_reg <= (others => '0');
		c1_sel_reg <= (others => '0');
		c2_sel_reg <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
			a1_reg <= a1;
			a2_reg <= a2;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end if;
		
	end if;
end process;

	s <= resize(p, (A_WIDTH+COEF_WIDTH+1));

	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_reg))));

	dsp0 : sum_of_2
	generic map 
	(
		A_WIDTH => A_WIDTH,
		B_WIDTH => COEF_WIDTH
	)	
	port MAP
	(
		a1	=> a1_reg,
		b1	=> c1_coef_wire,
		a2	=> a2_reg,
		b2	=> c2_coef_wire,
		p => p
	);
end rtl;
end_template
begin_template Multiplier with One Operand from Pre-Adder
-- Quartus Prime VHDL Template
-- Multiplier with one operand from pre-adder
-- For use with the 28-nm device families

-- This template can be used with 27x27, sum-of-2 27x27 modes on Stratix-V
--      Preadder supports 26-bit preadder (25-bit operands), c input supports 22-bit
-- This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
--      Preadder supports 19(signed)/27-bit preadder (18(signed)/26-bit operands), c input supports 18/27-bit

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity preadder_input is
	generic
	(
		AB_WIDTH : natural := 25;
		C_WIDTH 	: natural := 22
	);

	port 
	(
		a	   : in signed	((AB_WIDTH-1) downto 0);
		b	   : in signed	((AB_WIDTH-1) downto 0);
		c	   : in signed	((C_WIDTH-1) downto 0);
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- When preadder is used, all registered data inputs must use the same {clock, ena, reset}
		clock : in std_logic;
		ena	: in std_logic;
		reset	: in std_logic;
		s		: out signed ((AB_WIDTH+C_WIDTH) downto 0)
	);

end entity;

architecture rtl of preadder_input is

signal a_reg, b_reg				: signed	((AB_WIDTH-1) downto 0);
signal ab			   			: signed	((AB_WIDTH) downto 0);
signal c_reg						: signed	((C_WIDTH-1) downto 0);

-- Pick an applicable basic mode template
component single_mult 
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a	   : in signed	((A_WIDTH-1) downto 0);
		b	   : in signed	((B_WIDTH-1) downto 0);
		p 		: out signed ((A_WIDTH+B_WIDTH-1) downto 0)
	);

end component;

begin

-- Preadder
-- Preadder supports static add/sub
ab <= resize(a_reg, AB_WIDTH+1) + resize(b_reg, AB_WIDTH+1);

process(clock, reset)
begin
	if (reset = '1') then

		a_reg <= (others => '0');
		b_reg <= (others => '0');
		c_reg <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
			a_reg <= a;
			b_reg <= b;
			c_reg <= c;
		end if;
	
	end if;

end process;

dsp0 : single_mult
	generic map 
	(
		A_WIDTH => AB_WIDTH+1,
		B_WIDTH => C_WIDTH
	)	
	port MAP
	(
		a	=> ab,
		b	=> c_reg,
		p 	=> s
	);

end rtl;
end_template
begin_template Multiplier with One Operand from Pre-Adder and the Other from Coefficient ROM
-- Quartus Prime VHDL Template
-- Multiplier with one operand from pre-adder and the other from coefficient ROM
-- For use with the 28-nm device families

-- This template can be used with 18x18, sum-of-2 18x18, 27x27, sum-of-2 27x27 modes on Stratix-V
--      Preadder supports 18/26-bit preadder (17/25-bit operands)
-- This template can be used with 18x19(signed), sum-of-2 18x19(signed), 27x27 modes on Arria-V and Cyclone-V
--      Preadder supports 19(signed)/27-bit preadder (18(signed)/26-bit operands)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity preadder_coef is
	generic
	(
		AB_WIDTH 	: natural := 17;
		COEF_WIDTH 	: natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3
	);

	port 
	(
		a1	   	: in signed	((AB_WIDTH-1) downto 0);
		b1	   	: in signed	((AB_WIDTH-1) downto 0);
		a2	   	: in signed	((AB_WIDTH-1) downto 0);
		b2	   	: in signed	((AB_WIDTH-1) downto 0);
		-- up to 8 coefficients (3-bit address)
		c1_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- When preadder is used, all registered data inputs on each multiplier must use the same {clock, ena}
		-- All registered inputs must use the same reset
		clock 	: in std_logic;
		ena		: in std_logic; 
		reset		: in std_logic;
		s			: out signed ((AB_WIDTH+COEF_WIDTH+1) downto 0)
	);

end entity;

architecture rtl of preadder_coef is
-- This template use integer type as the coeffecient constant
-- Can use other preferred type for example signed/unsigned  

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c1_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"001010111111101011",
			"001010000011101011",
			"001010110000001011",
			"001010111111101011",
			"001010111111101011");
			 
signal c2_coef : coef_type :=
			("010101011010100100",
			"011010101110101010",
			"001010110111011011",
			"001010110101101010",
			"101010100011101011",
			"001011011000001010",
			"011010111111101011",
			"001010101001000110");

-- Two 18x18 in one DSP block must use coefficient storage simultaneously
signal a1_reg	   : signed	((AB_WIDTH-1) downto 0);
signal a2_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b1_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b2_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c1_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c1_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal ab1		   : signed	((AB_WIDTH) downto 0);
signal ab2			: signed	((AB_WIDTH) downto 0);
signal p				: signed	((AB_WIDTH+COEF_WIDTH+1) downto 0);


-- Pick an applicable basic mode template
component sum_of_2 
	generic
	(
		A_WIDTH : natural := AB_WIDTH+1;
		B_WIDTH : natural := COEF_WIDTH
	);

	port 
	(
		a1	   : in signed	((AB_WIDTH) downto 0);
		b1	   : in signed	((COEF_WIDTH-1) downto 0);
		a2	   : in signed	((AB_WIDTH) downto 0);
		b2	   : in signed	((COEF_WIDTH-1) downto 0);
		p : out signed ((AB_WIDTH+COEF_WIDTH+1) downto 0)
	);

end component;


begin

-- Register 
process(clock, reset)
begin
	if (reset = '1') then

		a1_reg <= (others => '0');
		a2_reg <= (others => '0');
		b1_reg <= (others => '0');
		b2_reg <= (others => '0');
		c1_sel_reg <= (others => '0');
		c2_sel_reg <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena = '1') then
			a1_reg <= a1;
			a2_reg <= a2;
			b1_reg <= b1;
			b2_reg <= b2;
			c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
		end if;
		
	end if;
end process;


	-- Preadder
	-- Preadder supports static add/sub
	-- Two 18x18 in one DSP block must use preadder simultaneously
	-- Two 18x18 in one DSP block must have the same add/sub
	ab1 <= resize(a1_reg, AB_WIDTH+1) + resize(b1_reg, AB_WIDTH+1);
	ab2 <= resize(a2_reg, AB_WIDTH+1) + resize(b2_reg, AB_WIDTH+1);

	s <= resize(p, (AB_WIDTH+COEF_WIDTH+2));
	
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_reg))));
	
	dsp0 : sum_of_2
	generic map 
	(
		A_WIDTH => AB_WIDTH+1,
		B_WIDTH => COEF_WIDTH
	)	
	port MAP
	(
		a1	=> ab1,
		b1	=> c1_coef_wire,
		a2	=> ab2,
		b2	=> c2_coef_wire,
		p => p
	);
end rtl;
end_template
begin_template Multiplier with Both Operands from the Same Pre-Adder
-- Quartus Prime VHDL Template
-- Multiplier with both operands from the same pre-adder
-- For use with the 28-nm device families

-- This template can be used with sum-of-2 18x18 mode on Stratix-V
--      Preadder supports 18-bit preadder (17-bit operands)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity square is
	generic
	(
		AB_WIDTH : natural := 17
	);

	port 
	(
		a1	   : in signed	((AB_WIDTH-1) downto 0);
		b1	   : in signed	((AB_WIDTH-1) downto 0);
		a2	   : in signed	((AB_WIDTH-1) downto 0);
		b2	   : in signed	((AB_WIDTH-1) downto 0);
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		-- When preadder is used, all registered data inputs on each multiplier must use the same {clock, ena}
		-- All registered inputs must use the same reset
		clock : in std_logic;
		ena1 	: in std_logic;
		ena0	: in std_logic; 
		reset	: in std_logic;
		s		: out signed (((2*AB_WIDTH)+1) downto 0)
	);

end entity;

architecture rtl of square is

signal a1_reg, a2_reg, b1_reg, b2_reg	   : signed	((AB_WIDTH-1) downto 0);
signal ab1, ab2								   : signed	((AB_WIDTH) downto 0);
signal p												: signed	((2*(AB_WIDTH+1)) downto 0);

-- Pick an applicable basic mode template
component sum_of_2 
	generic
	(
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 18
	);

	port 
	(
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		p : out signed ((A_WIDTH+B_WIDTH) downto 0)
	);

end component;

begin

-- input register
process(clock, reset)
begin
	if (reset = '1') then

		a1_reg <= (others => '0');
		a2_reg <= (others => '0');
		b1_reg <= (others => '0');
		b2_reg <= (others => '0');
		
	elsif rising_edge(clock) then

		if (ena0 = '1') then
			a1_reg <= a1;
			b1_reg <= b1;
		end if;

		if (ena1 = '1') then
			a2_reg <= a2;
			b2_reg <= b2;
		end if;
		
	end if;

end process;

-- Preadder
-- Preadder supports static add/sub
-- Two 18x18 in one DSP block must use preadder simultaneously
-- Two 18x18 in one DSP block must have the same add/sub
ab1 <= resize(a1_reg, AB_WIDTH+1) - resize(b1_reg, AB_WIDTH+1);
ab2 <= resize(a2_reg, AB_WIDTH+1) - resize(b2_reg, AB_WIDTH+1);

s <= resize(p, (2*AB_WIDTH)+2);

dsp0 : sum_of_2
	generic map 
	(
		A_WIDTH => AB_WIDTH+1,
		B_WIDTH => AB_WIDTH+1
	)	
	port MAP
	(
		a1	=> ab1,
		b1	=> ab1,
		a2	=> ab2,
		b2	=> ab2,
		p => p
	);

end rtl;
end_template
end_group
begin_group DSP Features for 20-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub and Dynamic Negate
-- Quartus Prime VHDL Template
-- Sum of two 18x19 multipliers with full registers (input, pipeline and output) + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = a1[t-4]*b1[t-4] +/- a2[t-4]*b2[t-4] +/- a3[t-3]*b3[t-3] +/- a4[t-3]*b4[t-3]
	-- Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation.  
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_dynSub_dynNegate is
	generic
	(
	-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The formula for the output width of 1 sum of two 18x19 multipliers. 
		-- SUM_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 38 + 1
		
	);

	port 
	(
		-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		b3	   : in signed	((B_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		b4	   : in signed	((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate   : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_dynSub_dynNegate is

-- Multiplier Result
signal m1, m2, m3, m4 : signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Sum Of 2 Multipliers Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg 	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg	   : signed	((B_WIDTH-1) downto 0);


--Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg	   : signed	((B_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline_reg: std_logic;
signal addnsub2_pipeline_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline_reg: std_logic;

--Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal final_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.	
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0');
			a4_reg <= (others => '0');
			b4_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= '0';
			addnsub2_reg <= '0';
			negate_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
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
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline registers must use the same reset as the output register
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			a3_pipeline_reg <= (others => '0');
			b3_pipeline_reg <= (others => '0');
			a4_pipeline_reg <= (others => '0');
			b4_pipeline_reg <= (others => '0');
			addnsub1_pipeline_reg <= '0';
			addnsub2_pipeline_reg <= '0';
			negate_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			final_output_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				s1_output_reg <= s1;
	         	
				-- Dynamic negate
				if (negate_pipeline_reg = '1') then  
				final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(s2,FINAL_OUTPUT_WIDTH);
				else 
				final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(s2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
			
		end if;

	end process;
	
	-- Multiplier
	-- a1 *
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);
	m3 <= (a3_pipeline_reg * b3_pipeline_reg);
	m4 <= (a4_pipeline_reg * b4_pipeline_reg);
	
	-- Dynamic add/sub
	with addnsub1_pipeline_reg select
	s1 <= (resize(m1, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m1, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH)) when others;
			
	-- Dynamic add/sub
	with addnsub2_pipeline_reg select
	s2 <= (resize(m3, SUM_OUTPUT_WIDTH) - resize(m4, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m3, SUM_OUTPUT_WIDTH) + resize(m4, SUM_OUTPUT_WIDTH)) when others;
	
	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent
-- Quartus Prime VHDL Template
-- Sum of two 18x19 multipliers with full registers (input, pipeline and output) + preadder + coefficients
-- Formula: final_output[t] = (a1[t-3]+b1[t-3])*c1_coef[t-3] + (a2[t-3]+b2[t-3])*c2_coef[t-3]
-- Two 18x18 in one DSP block must use coefficient storage simultaneously
-- For use with 20-nm device families.
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_preadd_coef is
	generic
	(
	-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH : natural := 18;
		COEF_WIDTH 	: natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3;
		-- The formula for the multipler width of one (A+B) x Coefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH + 1) + COEF_WIDTH;
		MULT_OUTPUT_WIDTH : natural := (18+1)+ 18;
		-- This example uses n=2 multiplers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 37 + 1
	);

	port 
	(
		-- Data input ports
		a1	   : in signed	((AB_WIDTH-1) downto 0);
		b1	   : in signed	((AB_WIDTH-1) downto 0);
		a2	   : in signed	((AB_WIDTH-1) downto 0);
		b2	   : in signed	((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_preadd_coef is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c1_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"001010111111101011",
			"001010000011101011",
			"001010110000001011",
			"001010111111101011",
			"001010111111101011");
			 
signal c2_coef : coef_type :=
			("010101011010100100",
			"011010101110101010",
			"001010110111011011",
			"001010110101101010",
			"101010100011101011",
			"001011011000001010",
			"011010111111101011",
			"001010101001000110"); 

-- Coefficient selection result
signal c1_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab1		   : signed	((AB_WIDTH) downto 0);
signal ab2			: signed	((AB_WIDTH) downto 0);

-- Multiplier result		
signal m1, m2 : signed ((MULT_OUTPUT_WIDTH-1) downto 0);

-- Input Register
signal a1_reg, a2_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c1_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal s_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

begin
  
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
	-- When preadder is used, the inputs to the preadder must use the same {clock, ena}
	-- The coefficient select input may use a different clock than that of the preadder inputs. 
	-- All registered inputs must use the same reset
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The pipeline register must use the same reset as the output register
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			c1_sel_pipeline_reg <= (others => '0');
			c2_sel_pipeline_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline_reg <= a1_reg;
				b1_pipeline_reg <= b1_reg;
				a2_pipeline_reg <= a2_reg;
				b2_pipeline_reg <= b2_reg;
				c1_sel_pipeline_reg <= c1_sel_reg;
			c2_sel_pipeline_reg <= c2_sel_reg;
			end if;
			
		end if;

	end process;
	
	-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s_output_reg <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
			-- Static add/sub is supported
				s_output_reg <= (resize(m1, FINAL_OUTPUT_WIDTH) + resize(m2, FINAL_OUTPUT_WIDTH));
			end if;
			
		end if;

	end process;
	
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab1 <= resize(a1_pipeline_reg, AB_WIDTH+1) + resize(b1_pipeline_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline_reg, AB_WIDTH+1) + resize(b2_pipeline_reg, AB_WIDTH+1);
	
	-- Coefficients
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline_reg))));
		
	-- Multiplier
	m1 <= c1_coef_wire * ab1;
	m2 <= c2_coef_wire * ab2;
	
	-- Final output
	final_output <= s_output_reg;


end rtl;
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
-- Quartus Prime VHDL Template
-- Two 'sum of two 18x19 multipliers' with full registers (input, pipeline and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-4]*b1[t-4] + a2[t-4]*b1[t-5] + a3[t-3]*b1[t-6] + a4[t-3]*b1[t-7]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum	   : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is

-- Multiplier Result
signal m1, m2, m3, m4  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg	   : signed	((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
-- There are two input delay registers in one DSP block: one for b1 and another for b2.
-- In 18x18_sumOf2 mode, only b2 delay register can be used. 
signal b2_delay_reg: signed	((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg	   : signed	((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg : signed ((A_WIDTH+B_WIDTH) downto 0);
signal s2 : signed ((A_WIDTH+B_WIDTH) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed	((CHAIN_WIDTH-1) downto 0);

begin

-- accumulator path
with accum_pipeline_reg select
	acc_sel <= 	select_feedback when '1',
					selected_value when others;

with enable_double_accum select
	select_feedback <= 	s_double when TRUE,
								s_reg when others;
					
with loadconst_pipeline_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
							(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0');
			a4_reg <= (others => '0');
			b4_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
			-- Input Cascade Delay register
			b2_delay_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
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
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- Pipeline register must use the same reset as the output register
-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			a3_pipeline_reg <= (others => '0');
			b3_pipeline_reg <= (others => '0');
			a4_pipeline_reg <= (others => '0');
			b4_pipeline_reg <= (others => '0');
			loadconst_pipeline_reg <= '0';
			accum_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
					-- Sum of 2 multiplier. Support static add/sub 
						s1_output_reg <= (resize(m1, A_WIDTH+B_WIDTH+1) + resize(m2, A_WIDTH+B_WIDTH+1));
						-- Accumulate and chainout adder
						s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
						-- Double Accumulate
						s_double <= s_reg;
			end if;
			
		end if;

	end process;
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);
	m3 <= (a3_pipeline_reg * b3_pipeline_reg);
	m4 <= (a4_pipeline_reg * b4_pipeline_reg);
	
	-- Sum of 2 multiplier. Support static add/sub
	s2 <= (resize(m3, A_WIDTH+B_WIDTH+1) + resize(m4, A_WIDTH+B_WIDTH+1));

	-- Final output
final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_systolic with Preadder and Coefficent
-- Quartus Prime VHDL Template
-- 18x19systolic with full registers (input, pipeline and output) + preadder + coefficients
-- Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) + ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c4_coef
--          where (zero_bit_a+zero_bit_b)*c4_coef is a dummy multiplier
-- Two 18x18 in one DSP block must use coefficient storage simultaneously
	-- Note: Systolic mode do not support dynamic negate and subtraction  
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19systolic_full_regs_preadd_coef is 
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH : natural := 18;
		COEF_WIDTH 	: natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3;
		-- The formula for the multipler width of one (A+B)xCoefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH+1) + COEF_WIDTH
		MULT_OUTPUT_WIDTH : natural := (18 + 1)+ 18;
		-- This example uses n=4 multiplers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 3
		FINAL_OUTPUT_WIDTH : natural := 37 +3
	);

	port 
	(
		-- Data input ports
		a1	   : in signed	((AB_WIDTH-1) downto 0);
		b1	   : in signed	((AB_WIDTH-1) downto 0);
		a2	   : in signed	((AB_WIDTH-1) downto 0);
		b2	   : in signed	((AB_WIDTH-1) downto 0);
		a3	   : in signed	((AB_WIDTH-1) downto 0);
		b3	   : in signed	((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c3_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19systolic_full_regs_preadd_coef is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1)) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);
TYPE zero_coef_type IS ARRAY(0 to 0) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
signal c1_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"001010111111101011",
			"001010000011101011",
			"001010110000001011",
			"001010111111101011",
			"001010111111101011");
			 
signal c2_coef : coef_type :=
			("010101011010100100",
			"011010101110101010",
			"001010110111011011",
			"001010110101101010",
			"101010100011101011",
			"001011011000001010",
			"011010111111101011",
			"001010101001000110"); 
			
signal c3_coef : coef_type :=
			("100101011001000110",
			"010100101111101011",
			"001001010000001010",
			"101011010101101011",
			"001000110101101010",
			"001010111000111011",
			"101010011010101010",
			"010101010101101100"); 
			
-- To fulfil even number requirement for systolic mode
signal c4_coef : zero_coef_type;

-- Coefficient selection result
signal c1_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c3_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);
signal c4_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab1		   : signed	((AB_WIDTH) downto 0);
signal ab2			: signed	((AB_WIDTH) downto 0);
signal ab3			: signed	((AB_WIDTH) downto 0);
signal ab4			: signed	((AB_WIDTH) downto 0);

-- Multiplier result
signal m1, m2, m3, m4	: signed ((MULT_OUTPUT_WIDTH-1) downto 0);
-- Summation result
signal s1_reg, s2_reg, s3_reg, s4_reg	: signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, zero_bit_a_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, zero_bit_b_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c1_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

--Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg,  zero_bit_a_pipeline_reg   : signed	((AB_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg,  zero_bit_b_pipeline_reg   : signed	((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);


-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd, to make up the number to even.
-- The following is required for the dummy multiplier. 
signal zero_bit				: signed	(0 downto 0);
signal zero_bit_a				: signed	((AB_WIDTH-1) downto 0);
signal zero_bit_b				: signed	((AB_WIDTH-1) downto 0);
signal zero_bit_c				: std_logic_vector ((SEL_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_a_pipeline_reg: signal is true;
attribute preserve of zero_bit_b_pipeline_reg: signal is true;


begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.	
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
			c3_sel_reg <= (others => '0');
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
			zero_bit_c_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				c1_sel_reg <= c1_sel;
				c2_sel_reg <= c2_sel;
				c3_sel_reg <= c3_sel;
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
				zero_bit_c_reg <= zero_bit_c;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline registers must use the same reset as the output register
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
		-- Input pipeline registers (for DATA)
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			a3_pipeline_reg <= (others => '0');
			b3_pipeline_reg <= (others => '0'); 
			c1_sel_pipeline_reg <= (others => '0');
			c2_sel_pipeline_reg <= (others => '0');
			c3_sel_pipeline_reg <= (others => '0');
			zero_bit_a_pipeline_reg <= (others => '0');
			zero_bit_b_pipeline_reg <= (others => '0');
			zero_bit_c_pipeline_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	-- Even though the output registers are not explicitly declared, they will be inferred later during compilation. Thus, it is important to place the s1-s5 operation
	-- within the output register enable(ena3)=1 condition. 
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_reg <= (others => '0');
			s2_reg <= (others => '0');
			s3_reg <= (others => '0');
			s4_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
			
				s1_reg <= resize(m1, FINAL_OUTPUT_WIDTH);
				--static add/sub supported
				s2_reg <= s1_reg + resize(m2, FINAL_OUTPUT_WIDTH);
				s3_reg <= s2_reg - resize(m3, FINAL_OUTPUT_WIDTH);
				s4_reg <= s3_reg + resize(m4, FINAL_OUTPUT_WIDTH);
				
			end if;
			
		end if;

	end process;
	
	-- Assign zero bit
	zero_bit <= B"0";
	zero_bit_a <= resize(zero_bit, AB_WIDTH);
	zero_bit_b <= resize(zero_bit, AB_WIDTH);
	zero_bit_c <= std_logic_vector(resize(zero_bit, SEL_WIDTH));
	
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab1 <= resize(a1_pipeline_reg, AB_WIDTH+1) + resize(b1_pipeline_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline_reg, AB_WIDTH+1) + resize(b2_pipeline_reg, AB_WIDTH+1);
	ab3 <= resize(a3_pipeline_reg, AB_WIDTH+1) + resize(b3_pipeline_reg, AB_WIDTH+1);
	ab4 <= resize(zero_bit_a_pipeline_reg, AB_WIDTH+1) + resize(zero_bit_b_pipeline_reg, AB_WIDTH+1);
	
	-- Coefficients
	c4_coef(0) <= ("000000000000000000"); 
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline_reg))));
	c3_coef_wire <= signed(c3_coef(to_integer(unsigned(c3_sel_pipeline_reg))));
	c4_coef_wire <= signed(c4_coef(to_integer(unsigned(zero_bit_c_pipeline_reg))));
	
	
	-- Multiplier
	m1 <= resize(c1_coef_wire,COEF_WIDTH) * resize(ab1,AB_WIDTH+1);
	m2 <= resize(c2_coef_wire,COEF_WIDTH) * resize(ab2,AB_WIDTH+1);
	m3 <= resize(c3_coef_wire,COEF_WIDTH) * resize(ab3,AB_WIDTH+1);
	-- When this template is used, the number of multipliers has to be even
-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m4 <= resize(c4_coef_wire,COEF_WIDTH) * resize(ab4,AB_WIDTH+1);
		
	-- Final output 
	final_output <= s4_reg;

end rtl;
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
-- Quartus Prime VHDL Template
-- 18x19systolic with full registers (input, pipeline, systolic and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] + a3[t-6]*b1[t10] + a4[t-5]*b1[t-11] + a5[t-4]*b1[t-12] + zero_bit_a*zero_bit_b
--          where zero_bit_a*zero_bit_b is a dummy multiplier
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
	-- Note: Systolic mode do not support dynamic negate and subtraction(sub)  
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is 
	generic
	(
	-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The max chain width for systolic mode is 44. 
		CHAIN_WIDTH : natural := 44;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
		-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		a3	   : in signed	((A_WIDTH-1) downto 0);
		a4	   : in signed	((A_WIDTH-1) downto 0);
		a5	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum	   : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is

-- Multiplier result
signal m1, m2, m3, m4, m5, m6: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Summation result
signal s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg : signed ((CHAIN_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, zero_bit_a_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, zero_bit_b_reg	   : signed	((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
-- There are two input delay registers in one DSP block: one in each of the two multipliers. 
-- In 18x19 systolic mode, both delay registers in a DSP block can be used. 
signal b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg : signed	((B_WIDTH-1) downto 0);

--Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg, a3_pipeline_reg, a4_pipeline_reg, a5_pipeline_reg,  zero_bit_a_pipeline_reg   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg, b3_pipeline_reg, b4_pipeline_reg, b5_pipeline_reg,  zero_bit_b_pipeline_reg   : signed	((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline_reg : std_logic;

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed	((CHAIN_WIDTH-1) downto 0);

-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd, to make up the number to even.
-- The following is required for the dummy multiplier. 
signal zero_bit					: signed	(0 downto 0);
signal zero_bit_a				: signed	((A_WIDTH-1) downto 0);
signal zero_bit_b				: signed	((B_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_a_pipeline_reg: signal is true;
attribute preserve of zero_bit_b_pipeline_reg: signal is true;

begin

-- accumulator path
with accum_pipeline_reg select
	acc_sel <= 	select_feedback when '1',
					selected_value when others;

with enable_double_accum select
	select_feedback <= 	s_double when TRUE,
								s_reg when others;
					
with loadconst_pipeline_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
							(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			a4_reg <= (others => '0');
			b4_reg <= (others => '0');
			a5_reg <= (others => '0');
			b5_reg <= (others => '0');
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
			-- Input Cascade Delay register
			b1_delay_reg <= (others => '0');
			b2_delay_reg <= (others => '0');
			b3_delay_reg <= (others => '0');
			b4_delay_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
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
				-- input for dummy multiplier 0x0
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- Pipeline register must use the same reset as the output register
-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
		a3_pipeline_reg <= (others => '0');
			b3_pipeline_reg <= (others => '0'); 
			a4_pipeline_reg <= (others => '0');
			b4_pipeline_reg <= (others => '0');
		a5_pipeline_reg <= (others => '0');
			b5_pipeline_reg <= (others => '0');
			zero_bit_a_pipeline_reg <= (others => '0');
			zero_bit_b_pipeline_reg <= (others => '0');
			loadconst_pipeline_reg <= '0';
			accum_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	-- Even though the output registers are not explicitly declared, they will be inferred later during compilation. Thus, it is important to place the s1_output_reg-s5_output_reg operation
	-- within the output register enable (i.e. ena3=1) condition. 
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			s2_output_reg <= (others => '0');
			s3_output_reg <= (others => '0');
			s4_output_reg <= (others => '0');
			s5_output_reg <= (others => '0');
		s_reg <= (others => '0');
		s_double <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				-- chainout adder support static add or sub
				-- basic mult result (m) must be the second operand	
				s1_output_reg <= resize(m1, CHAIN_WIDTH);
				s2_output_reg <= s1_output_reg + resize(m2, CHAIN_WIDTH);
				s3_output_reg <= s2_output_reg + resize(m3, CHAIN_WIDTH);
				s4_output_reg <= s3_output_reg - resize(m4, CHAIN_WIDTH);
				s5_output_reg <= s4_output_reg + resize(m5, CHAIN_WIDTH);
				-- chainout accumulator only support addition when use with chainout adder
				s_reg <= acc_sel + (s5_output_reg + resize(m6, CHAIN_WIDTH)); -- loopback path (acc_sel) must be the first operand
				-- Double Accumulate
				s_double <= s_reg;
			end if;
			
		end if;

	end process;
	
	-- Assign zero bit
zero_bit <= B"0";
zero_bit_a <= resize(zero_bit, A_WIDTH);
zero_bit_b <= resize(zero_bit, B_WIDTH);
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);
	m3 <= (a3_pipeline_reg * b3_pipeline_reg);
	m4 <= (a4_pipeline_reg * b4_pipeline_reg);
	m5 <= (a5_pipeline_reg * b5_pipeline_reg);
	-- When this template is used, the number of multipliers has to be even
-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m6 <= (zero_bit_a_pipeline_reg * zero_bit_b_pipeline_reg);
		
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M27x27 with Dynamic Negate
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, pipeline and output) + dynamic negate
-- Formula: final_output[t] = a1[t-4]*b1[t-4] +/- a2[t-3]*b2[t-3]
-- Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_dynNegate is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- This example uses n=2 multiplers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 27 + 27 + 1
	);

	port 
	(
	-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
	clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic NEGATE control signals
		negate   : in std_logic;
	-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_dynNegate is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg	   : signed	((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg	   : signed	((B_WIDTH-1) downto 0);

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline_reg: std_logic;

-- Output Register
signal m1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);
signal final_output_reg: signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

begin
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. negate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			negate_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				negate_reg <= negate;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline register must use the same reset as the output register
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			negate_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline_reg <= a1_reg;
				b1_pipeline_reg <= b1_reg;
				a2_pipeline_reg <= a2_reg;
				b2_pipeline_reg <= b2_reg;
				negate_pipeline_reg <= negate_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			m1_output_reg <= (others => '0');
			final_output_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				m1_output_reg <= m1;
	         	
				-- Dynamic negate
				if (negate_pipeline_reg = '1') then  
				final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(m2,FINAL_OUTPUT_WIDTH);
				else 
				final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(m2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
			
		end if;
	end process;
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);		

	-- Final output
final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Preadder and Coefficent
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, pipeline and output) + preadder + coefficients
-- Formula: final_output[t] = (a[t-3]+b[t-3])*c_coef[t-3]
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_preadd_coef is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		AB_WIDTH : natural := 26;
		COEF_WIDTH 	: natural := 27;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3
	);

	port 
	(
	-- Data input ports
		a	   : in signed	((AB_WIDTH-1) downto 0);
		b	   : in signed	((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_preadd_coef is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100001010100",
			"001010100111101011101010111",
			"001010111111101011000100000",
			"101010111111101011111111111",
			"001010000011010110101101101",
			"111010110000001011000011101",
			"001010111111010111111110110",
			"001010111111101011010111011");

-- Coefficient selection result
signal c_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab		   : signed	((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline_reg   : signed	((AB_WIDTH-1) downto 0);
signal b_pipeline_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

begin
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
	-- When preadder is used, the inputs to the preadder must use the same {clock, ena}
	-- The coefficient select input may use a different clock than that of the preadder inputs.
-- All registered inputs must use the same reset
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			c_sel_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline register must use the same reset as the output register
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a_pipeline_reg <= (others => '0');
			b_pipeline_reg <= (others => '0');
			c_sel_pipeline_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a_pipeline_reg <= a_reg;
				b_pipeline_reg <= b_reg;
				c_sel_pipeline_reg <= c_sel_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			final_output_reg <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
			-- Static add/sub is supported
				final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
			end if;
			
		end if;
	end process;
	
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline_reg, AB_WIDTH+1) + resize(b_pipeline_reg, AB_WIDTH+1);
	
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline_reg))));

	-- Final output
final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, pipeline and output) + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-4]*b1[t-4] + a2[t-3]*b1[t-4]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
	-- Note: The Input Delay register is not supported in 27x27 mode. 
	-- Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation. 
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum	   : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg	   : signed	((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg	   : signed	((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline_reg : std_logic;

-- Output Register
signal s1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed	((CHAIN_WIDTH-1) downto 0);

begin

-- accumulator path
with accum_pipeline_reg select
	acc_sel <= 	select_feedback when '1',
					selected_value when others;

with enable_double_accum select
	select_feedback <= 	s_double when TRUE,
								s_reg when others;
					
with loadconst_pipeline_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
							(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b1_reg;
				loadconst_reg <= loadconst;
			accum_reg <= accum;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- Pipeline register must use the same reset as the output register
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			loadconst_pipeline_reg <= '0';
			accum_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline_reg <= a1_reg;
				b1_pipeline_reg <= b1_reg;
				a2_pipeline_reg <= a2_reg;
				b2_pipeline_reg <= b2_reg;
				loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
					-- First 27x27 result. Support static add/sub 
						s1_output_reg <= m1;
						-- Accumulate and chainout adder 						 
						s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(m2,CHAIN_WIDTH));
						--- Double Accumulate
						s_double <= s_reg;
			end if;
			
		end if;

	end process;
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);		

	-- Final output
final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate
-- Quartus Prime VHDL Template
-- 18x19_plus36 with full registers (input, pipeline and output) + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = ((a1[t-4]*b1[t-4])+/-c1[t-4]) +/- ((a2[t-3]*b2[t-3])+/-c2[t-3])
	-- Note: Arria 10 ES devices do not support dynamic negate and subtraction for chainout/accumulation.  
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19plus36_full_regs_dynSub_dynNegate is 
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
	A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 multiplers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1
		FINAL_OUTPUT_WIDTH : natural := 39
	);

	port 
	(
		-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		c1	   : in signed	((C_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		c2	   : in signed	((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate   : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		-- The formula for the final_output width should be the larger value of either (A_WIDTH+B_WIDTH+2) or (C_WIDTH+2).
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19plus36_full_regs_dynSub_dynNegate is

-- Multiplier Result
signal m1, m2: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- 18x19_plus36 Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg 	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg	   : signed	((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg	   : signed	((C_WIDTH-1) downto 0);


-- Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg   : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg   : signed	((B_WIDTH-1) downto 0);
signal c1_pipeline_reg, c2_pipeline_reg   : signed	((C_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline_reg: std_logic;
signal addnsub2_pipeline_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline_reg: std_logic;

-- Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.	
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= '0';
			addnsub2_reg <= '0';
			negate_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				addnsub1_reg <= addnsub1; 
				addnsub2_reg <= addnsub2; 
				negate_reg <= negate;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline registers must use the same reset as the output register
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			c1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			c2_pipeline_reg <= (others => '0');
			addnsub1_pipeline_reg <= '0';
			addnsub2_pipeline_reg <= '0';
			negate_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline_reg <= a1_reg;
				b1_pipeline_reg <= b1_reg;
				c1_pipeline_reg <= c1_reg;
				a2_pipeline_reg <= a2_reg;
				b2_pipeline_reg <= b2_reg;
				c2_pipeline_reg <= c2_reg;
				addnsub1_pipeline_reg <= addnsub1_reg;
				addnsub2_pipeline_reg <= addnsub2_reg;
				negate_pipeline_reg <= negate_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				s1_output_reg <= s1;
	         	
				-- Dynamic negate
				if (negate_pipeline_reg = '1') then  
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) - resize(s2,FINAL_OUTPUT_WIDTH);
				else 
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) + resize(s2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
			
		end if;

	end process;
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);
	
	-- First 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub1_pipeline_reg select
	s1 <=  resize(c1_pipeline_reg, SUM_OUTPUT_WIDTH) - resize(m1, SUM_OUTPUT_WIDTH)  when '1',
			resize(c1_pipeline_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH) when others;
			
	-- Second 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub2_pipeline_reg select
	s2 <=  resize(c2_pipeline_reg, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH) when '1',
			resize(c2_pipeline_reg, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH) when others;
	
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant
-- Quartus Prime VHDL Template
-- Two 18x19_plus36 with full registers (input, pipeline and output) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + ((a1[t-4]*b1[t-4])+c1[t-4]) + ((a2[t-3]*b2[t-3])+c2[t-3])
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
	-- Note: Input cascade chain is not supported in 18x19_plus36 mode.
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst is
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1	   : in signed	((A_WIDTH-1) downto 0);
		b1	   : in signed	((B_WIDTH-1) downto 0);
		c1	   : in signed	((C_WIDTH-1) downto 0);
		a2	   : in signed	((A_WIDTH-1) downto 0);
		b2	   : in signed	((B_WIDTH-1) downto 0);
		c2	   : in signed	((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum	   : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst is

-- Multiplier Result
signal m1, m2  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg	   : signed	((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg      : signed	((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg      : signed	((C_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline_reg, a2_pipeline_reg  : signed	((A_WIDTH-1) downto 0);
signal b1_pipeline_reg, b2_pipeline_reg  : signed	((B_WIDTH-1) downto 0);
signal c1_pipeline_reg, c2_pipeline_reg  : signed	((C_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg: signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed	((CHAIN_WIDTH-1) downto 0);

begin

-- Accumulator path
with accum_pipeline_reg select
	acc_sel <= 	select_feedback when '1',
					selected_value when others;

with enable_double_accum select
	select_feedback <= 	s_double when TRUE,
								s_reg when others;
					
with loadconst_pipeline_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
							(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
   
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				loadconst_reg <= loadconst;
			accum_reg <= accum;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All pipeline registers must use the same {clock, ena, reset}
	-- Pipeline register must use the same reset as the output register
-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a1_pipeline_reg <= (others => '0');
			b1_pipeline_reg <= (others => '0');
			c1_pipeline_reg <= (others => '0');
			a2_pipeline_reg <= (others => '0');
			b2_pipeline_reg <= (others => '0');
			c2_pipeline_reg <= (others => '0');
			loadconst_pipeline_reg <= '0';
			accum_pipeline_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline_reg <= a1_reg;
				b1_pipeline_reg <= b1_reg;
				c1_pipeline_reg <= c1_reg;
				a2_pipeline_reg <= a2_reg;
				b2_pipeline_reg <= b2_reg;
				c2_pipeline_reg <= c2_reg;
				loadconst_pipeline_reg <= loadconst_reg;
			accum_pipeline_reg <= accum_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
					-- First 18x19_plus36. Support static add/sub. 
						-- Addend must be the first operand 
						s1_output_reg <= resize(c1_pipeline_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH);
						-- Accumulate and chainout adder 						 
						s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
						--- Double Accumulate
						s_double <= s_reg;
			end if;
			
		end if;

	end process;
	
	-- Multiplier
	m1 <= (a1_pipeline_reg * b1_pipeline_reg);
	m2 <= (a2_pipeline_reg * b2_pipeline_reg);
	
	-- Second 18x19_plus36. Support static add/sub
	-- Addend must be the first operand
	s2 <= resize(c2_pipeline_reg, SUM_OUTPUT_WIDTH) + resize(m2,SUM_OUTPUT_WIDTH);

	-- Final output
final_output <= s_reg;

end rtl;
end_template
begin_template Single Multiplier with Preadder and Coefficent
-- Quartus Prime VHDL Template
-- m18x19_full mode by utilizing half DSP resource
-- Single multiplier with full registers (input, pipeline and output) + preadder + coefficients
-- Formula: final_output[t] = (a[t-3]+b[t-3])*c_coef[t-3]
	-- Note: This mode does not support chainout adder, dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
-- For use with 20-nm device families
-- For more information on the 20nm DSP features, please refer to http://www.altera.com/literature/hb/arria-10/a10_dsp.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_mult_full_regs_preadd_coef is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH : natural := 18;
		COEF_WIDTH 	: natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH 	: natural := 3
	);

	port 
	(
	-- Data input ports
		a	   : in signed	((AB_WIDTH-1) downto 0);
		b	   : in signed	((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel	: in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 async reset signals
		clock1	: in std_logic;
		clock2	: in std_logic;
		clock3	: in std_logic;
		ena1		: in std_logic;
		ena2		: in std_logic;
		ena3		: in std_logic;
		reset1	: in std_logic;
		reset2	: in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of single_mult_full_regs_preadd_coef is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"101010111111101011",
			"001010000011010110",
			"111010110000001011",
			"001010111111010111",
			"001010111111101011");

-- Coefficient selection result
signal c_coef_wire	: signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab		   : signed	((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg	   : signed	((AB_WIDTH-1) downto 0);
signal b_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c_sel_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline_reg   : signed	((AB_WIDTH-1) downto 0);
signal b_pipeline_reg	   : signed	((AB_WIDTH-1) downto 0);
signal c_sel_pipeline_reg	: std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

begin
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 async reset signals
	-- When preadder is used, the inputs to the preadder must use the same {clock, ena}
	-- The coefficient select input may use a different clock than that of the preadder inputs.
-- All registered inputs must use the same reset
	process(clock1, reset1)
	begin
		if (reset1 = '1') then
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			c_sel_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end if;
			
		end if;

	end process;
	
	-- Input pipeline register
	-- All pipeline registers must use the same {clock, ena, reset}
	-- The Pipeline register must use the same reset as the output register
	process(clock2, reset2)
	begin
		if (reset2 = '1') then
			a_pipeline_reg <= (others => '0');
			b_pipeline_reg <= (others => '0');
			c_sel_pipeline_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a_pipeline_reg <= a_reg;
				b_pipeline_reg <= b_reg;
				c_sel_pipeline_reg <= c_sel_reg;
			end if;
			
		end if;

	end process;
	
-- Output register
	-- Output register must share the same reset with input pipeline register
	process(clock3, reset2)
	begin
		if (reset2 = '1') then
			final_output_reg <= (others => '0');
			
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
			-- Static add/sub is supported
				final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
			end if;
			
		end if;
	end process;
	
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline_reg, AB_WIDTH+1) + resize(b_pipeline_reg, AB_WIDTH+1);
	
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline_reg))));

	-- Final output
final_output <= final_output_reg;

end rtl;
end_template
end_group
begin_group DSP Features for 14-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub, Dynamic Negate and Output Chaining using SCLR
-- Quartus Prime VHDL Template
-- Two 'sum of 2 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-5]*b2[t-5] +/- a3[t-4]*b3[t-4] +/- a4[t-4]*b4[t-4]
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_14nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The formula for the output width of 1 sum of two 18x19 multipliers. 
		-- SUM_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 38 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		b3       : in signed    ((B_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		b4       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		sclr1     : in std_logic;
		sclr2     : in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_14nm is

-- Multiplier Result
signal m1, m2, m3, m4 : signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Sum Of 2 Multipliers Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg       : signed    ((B_WIDTH-1) downto 0);

--Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg       : signed    ((B_WIDTH-1) downto 0);

--Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline1_reg: std_logic;
signal addnsub2_pipeline1_reg: std_logic;

-- Sub Second Pipeline Register
signal addnsub1_pipeline2_reg: std_logic;
signal addnsub2_pipeline2_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

--Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal final_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, b1_reg, b2_reg, b3_reg, b4_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, addnsub1_reg, addnsub2_reg, addnsub1_pipeline1_reg, addnsub2_pipeline1_reg, addnsub1_pipeline2_reg, addnsub2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, s1_output_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.   
	process(clock1)
	begin
		if rising_edge(clock1) then

			if (ena1 = '1') then
				if (sclr1 = '1') then
					-- Input registers (for DATA)
					a1_reg <= (others => '0');
					b1_reg <= (others => '0');
					a2_reg <= (others => '0');
					b2_reg <= (others => '0');
					a3_reg <= (others => '0');
					b3_reg <= (others => '0');
					a4_reg <= (others => '0');
					b4_reg <= (others => '0');
					-- Input registers (for DYNAMIC CONTROL SIGNAL)
					addnsub1_reg <= '0';
					addnsub2_reg <= '0';
					negate_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2)
	begin
		if rising_edge(clock2) then

			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline1_reg <= (others => '0');
					b1_pipeline1_reg <= (others => '0');
					a2_pipeline1_reg <= (others => '0');
					b2_pipeline1_reg <= (others => '0');
					a3_pipeline1_reg <= (others => '0');
					b3_pipeline1_reg <= (others => '0');
					a4_pipeline1_reg <= (others => '0');
					b4_pipeline1_reg <= (others => '0');
					addnsub1_pipeline1_reg <= '0';
					addnsub2_pipeline1_reg <= '0';
					negate_pipeline1_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock2)
	begin
		if rising_edge(clock2) then

			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline2_reg <= (others => '0');
					b1_pipeline2_reg <= (others => '0');
					a2_pipeline2_reg <= (others => '0');
					b2_pipeline2_reg <= (others => '0');
					a3_pipeline2_reg <= (others => '0');
					b3_pipeline2_reg <= (others => '0');
					a4_pipeline2_reg <= (others => '0');
					b4_pipeline2_reg <= (others => '0');
					addnsub1_pipeline2_reg <= '0';
					addnsub2_pipeline2_reg <= '0';
					negate_pipeline2_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3)
	begin
		if rising_edge(clock3) then

			if (ena3 = '1') then
				if (sclr2 = '1') then
					s1_output_reg <= (others => '0');
					final_output_reg <= (others => '0');
				else
					s1_output_reg <= s1;
                    
					-- Dynamic negate
					if (negate_pipeline2_reg = '1') then  
					final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(s2,FINAL_OUTPUT_WIDTH);
					else 
					final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(s2,FINAL_OUTPUT_WIDTH);
					end if;
				end if;
			end if;
            
		end if;
	end process;
    
	-- Multipliers
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
    
	-- Dynamic add/sub
	with addnsub1_pipeline2_reg select
	s1 <= (resize(m1, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m1, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH)) when others;
            
	-- Dynamic add/sub
	with addnsub2_pipeline2_reg select
	s2 <= (resize(m3, SUM_OUTPUT_WIDTH) - resize(m4, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m3, SUM_OUTPUT_WIDTH) + resize(m4, SUM_OUTPUT_WIDTH)) when others;
    
	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- Sum of two 18x19 multipliers with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a1[t-4]+b1[t-4])*c1_coef[t-4] + (a2[t-4]+b2[t-4])*c2_coef[t-4]
-- Both multiplier in one DSP block must use coefficient input simultaneously
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_preadd_coef_14nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH   : natural := 18;
		COEF_WIDTH : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH  : natural := 3;
		-- The formula for the multipler width of one (A+B) x Coefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH + 1) + COEF_WIDTH;
		MULT_OUTPUT_WIDTH   : natural := (18+1)+ 18;
		-- This example uses n=2 multiplers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH  : natural := 37 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((AB_WIDTH-1) downto 0);
		b1       : in signed    ((AB_WIDTH-1) downto 0);
		a2       : in signed    ((AB_WIDTH-1) downto 0);
		b2       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_preadd_coef_14nm is
-- This template uses std_logic_vector type as the coefficient constant
-- It is possible use other preferred type, for example, signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1)) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c1_coef : coef_type := 
			("001010111111101011",
			"001010111111101011",
			"001010110000001011",
			"001010000011101011",
			"001010111111101011",
			"001010111111101011",
			"001010100111101011",
			"110101111001110100");
             
signal c2_coef : coef_type :=
			("001010101001000110",
			"011010111111101011",
			"001011011000001010",
			"101010100011101011",
			"001010110101101010",
			"001010110111011011",
			"011010101110101010",
			"010101011010100100"); 

-- Coefficient selection result
signal c1_coef_wire   : signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire   : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab1            : signed    ((AB_WIDTH) downto 0);
signal ab2            : signed    ((AB_WIDTH) downto 0);

-- Multiplier result        
signal m1, m2 : signed ((MULT_OUTPUT_WIDTH-1) downto 0);

-- Input Register
signal a1_reg, a2_reg : signed    ((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_reg     : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg     : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal s_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_sel_reg, c2_sel_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, s_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same {clock, ena}
	-- The coefficient select input may use a different clock than that of the preadder inputs. 
	-- All registered inputs must use the same reset
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c1_sel_pipeline1_reg <= (others => '0');
			c2_sel_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c1_sel_pipeline1_reg <= c1_sel_reg;
				c2_sel_pipeline1_reg <= c2_sel_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c1_sel_pipeline2_reg <= (others => '0');
			c2_sel_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
				c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			s_output_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				-- Static add/sub is supported
				s_output_reg <= (resize(m1, FINAL_OUTPUT_WIDTH) + resize(m2, FINAL_OUTPUT_WIDTH));
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab1 <= resize(a1_pipeline2_reg, AB_WIDTH+1) + resize(b1_pipeline2_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline2_reg, AB_WIDTH+1) + resize(b2_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline2_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline2_reg))));
        
	-- Multiplier
	m1 <= c1_coef_wire * ab1;
	m2 <= c2_coef_wire * ab2;
    
	-- Final output
	final_output <= s_output_reg;


end rtl;
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
-- Quartus Prime VHDL Template
-- Two 'sum of two 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-5]*b1[t-6] + a3[t-4]*b1[t-7] + a4[t-4]*b1[t-8]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_14nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
        
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		sclr1     : in std_logic;
		sclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_14nm is

-- Multiplier Result
signal m1, m2, m3, m4  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
signal b2_delay_reg: signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg : signed ((A_WIDTH+B_WIDTH) downto 0);
signal s2 : signed ((A_WIDTH+B_WIDTH) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, b1_reg, b2_reg, b3_reg, b4_reg, b2_delay_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
								s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
					(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1)
	begin
		if rising_edge(clock1) then
			if (ena1 = '1') then
				if (sclr1 = '1') then
					-- Input registers (for DATA)
					a1_reg <= (others => '0');
					b1_reg <= (others => '0');
					a2_reg <= (others => '0');
					b2_reg <= (others => '0');
					a3_reg <= (others => '0');
					b3_reg <= (others => '0');
					a4_reg <= (others => '0');
					b4_reg <= (others => '0');
					-- Input Cascade Delay register
					b2_delay_reg <= (others => '0');
					-- Input registers (for DYNAMIC CONTROL SIGNAL)
					loadconst_reg <= '0';
					accum_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently 
	process(clock2)
	begin
		if rising_edge(clock2) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline1_reg <= (others => '0');
					b1_pipeline1_reg <= (others => '0');
					a2_pipeline1_reg <= (others => '0');
					b2_pipeline1_reg <= (others => '0');
					a3_pipeline1_reg <= (others => '0');
					b3_pipeline1_reg <= (others => '0');
					a4_pipeline1_reg <= (others => '0');
					b4_pipeline1_reg <= (others => '0');
					loadconst_pipeline1_reg <= '0';
					accum_pipeline1_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently 
	process(clock2)
	begin
		if rising_edge(clock2) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline2_reg <= (others => '0');
					b1_pipeline2_reg <= (others => '0');
					a2_pipeline2_reg <= (others => '0');
					b2_pipeline2_reg <= (others => '0');
					a3_pipeline2_reg <= (others => '0');
					b3_pipeline2_reg <= (others => '0');
					a4_pipeline2_reg <= (others => '0');
					b4_pipeline2_reg <= (others => '0');
					loadconst_pipeline2_reg <= '0';
					accum_pipeline2_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3)
	begin
		if rising_edge(clock3) then
			if (ena3 = '1') then
				if (sclr2 = '1') then
					s1_output_reg <= (others => '0');
					s_reg <= (others => '0');
					s_double <= (others => '0');
				else
					-- Sum of 2 multiplier. Support static add/sub 
					s1_output_reg <= (resize(m1, A_WIDTH+B_WIDTH+1) + resize(m2, A_WIDTH+B_WIDTH+1));
					-- Accumulate and chainout adder
					s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
					-- Double Accumulate
					s_double <= s_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
    
	-- Sum of 2 multiplier. Support static add/sub
	s2 <= (resize(m3, A_WIDTH+B_WIDTH+1) + resize(m4, A_WIDTH+B_WIDTH+1));

	-- Final output
final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_systolic with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- 18x19_systolic with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) - ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c0_coef
--          where (zero_bit_a+zero_bit_b)*c0_coef is a dummy multiplier
-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even.
-- Both multipliers in one DSP block must use coefficient inputs simultaneously
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_systolic_full_regs_preadd_coef_14nm is 
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH   : natural := 18;
		COEF_WIDTH : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH  : natural := 3;
		-- The formula for the multipler width of one (A+B)xCoefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH+1) + COEF_WIDTH
		MULT_OUTPUT_WIDTH : natural := (18 + 1)+ 18;
		-- This example uses n=4 multiplers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 3
		FINAL_OUTPUT_WIDTH : natural := 37 +3
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((AB_WIDTH-1) downto 0);
		b1       : in signed    ((AB_WIDTH-1) downto 0);
		a2       : in signed    ((AB_WIDTH-1) downto 0);
		b2       : in signed    ((AB_WIDTH-1) downto 0);
		a3       : in signed    ((AB_WIDTH-1) downto 0);
		b3       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c3_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_systolic_full_regs_preadd_coef_14nm is
-- This template uses std_logic_vector type as the coeffecient constant
-- It is possible to use other preferred type, for example, signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1)) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
signal c1_coef : coef_type := 
			("001010111111101011",
			"001010111111101011",
			"001010110000001011",
			"001010000011101011",
			"001010111111101011",
			"001010111111101011",
			"001010100111101011",
			"110101111001110100");
             
signal c2_coef : coef_type :=
			("001010101001000110",
			"011010111111101011",
			"001011011000001010",
			"101010100011101011",
			"001010110101101010",
			"001010110111011011",
			"011010101110101010",
			"010101011010100100"); 
            
signal c3_coef : coef_type :=
			("100101011001000110",
			"010100101111101011",
			"001001010000001010",
			"101011010101101011",
			"001000110101101010",
			"001010111000111011",
			"101010011010101010",
			"010101010101101100"); 
            
-- To fulfil even number requirement for systolic mode
signal c0_coef : coef_type :=
			("000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000");

-- Coefficient selection result
signal c0_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c1_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c3_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab0            : signed    ((AB_WIDTH) downto 0);
signal ab1            : signed    ((AB_WIDTH) downto 0);
signal ab2            : signed    ((AB_WIDTH) downto 0);
signal ab3            : signed    ((AB_WIDTH) downto 0);

-- Multiplier result
signal m1, m2, m3, m0    : signed ((MULT_OUTPUT_WIDTH-1) downto 0);
-- Summation result
signal s1_reg, s2_reg, s3_reg, s0_reg    : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, zero_bit_a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, zero_bit_b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg,  zero_bit_a_pipeline1_reg   : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg,  zero_bit_b_pipeline1_reg   : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg,  zero_bit_a_pipeline2_reg   : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg,  zero_bit_b_pipeline2_reg   : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- The following is required for the dummy multiplier. 
signal zero_bit                  : signed    (0 downto 0);
signal zero_bit_a                : signed    ((AB_WIDTH-1) downto 0);
signal zero_bit_b                : signed    ((AB_WIDTH-1) downto 0);
signal zero_bit_c                : std_logic_vector ((SEL_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_c_reg: signal is true;
attribute preserve of zero_bit_a_pipeline1_reg: signal is true;
attribute preserve of zero_bit_b_pipeline1_reg: signal is true;
attribute preserve of zero_bit_c_pipeline1_reg: signal is true;
attribute preserve of zero_bit_a_pipeline2_reg: signal is true;
attribute preserve of zero_bit_b_pipeline2_reg: signal is true;
attribute preserve of zero_bit_c_pipeline2_reg: signal is true;

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, zero_bit_a_reg, b1_reg, b2_reg, b3_reg, zero_bit_b_reg, c1_sel_reg, c2_sel_reg, c3_sel_reg, zero_bit_c_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, zero_bit_a_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, zero_bit_b_pipeline1_reg, c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, c3_sel_pipeline1_reg, zero_bit_c_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg,  zero_bit_a_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg,  zero_bit_b_pipeline2_reg, c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, c3_sel_pipeline2_reg, zero_bit_c_pipeline2_reg, s1_reg, s2_reg, s3_reg, s0_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.    
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
			c3_sel_reg <= (others => '0');
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
			zero_bit_c_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				c1_sel_reg <= c1_sel;
				c2_sel_reg <= c2_sel;
				c3_sel_reg <= c3_sel;
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
				zero_bit_c_reg <= zero_bit_c;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			a3_pipeline1_reg <= (others => '0');
			b3_pipeline1_reg <= (others => '0'); 
			c1_sel_pipeline1_reg <= (others => '0');
			c2_sel_pipeline1_reg <= (others => '0');
			c3_sel_pipeline1_reg <= (others => '0');
			zero_bit_a_pipeline1_reg <= (others => '0');
			zero_bit_b_pipeline1_reg <= (others => '0');
			zero_bit_c_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- For systolic designs, the second pipeline register bank must use the same {clock, ena, reset} as the output register bank
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			a3_pipeline2_reg <= (others => '0');
			b3_pipeline2_reg <= (others => '0'); 
			c1_sel_pipeline2_reg <= (others => '0');
			c2_sel_pipeline2_reg <= (others => '0');
			c3_sel_pipeline2_reg <= (others => '0');
			zero_bit_a_pipeline2_reg <= (others => '0');
			zero_bit_b_pipeline2_reg <= (others => '0');
			zero_bit_c_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with input pipeline and second pipeline register banks
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			s0_reg <= (others => '0');
			s1_reg <= (others => '0');
			s2_reg <= (others => '0');
			s3_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				s0_reg <= resize(m0, FINAL_OUTPUT_WIDTH);
				--static add/sub supported
				s1_reg <= s0_reg + resize(m1, FINAL_OUTPUT_WIDTH);
				s2_reg <= s1_reg + resize(m2, FINAL_OUTPUT_WIDTH);
				s3_reg <= s2_reg - resize(m3, FINAL_OUTPUT_WIDTH);
			end if;
            
		end if;
	end process;
    
	-- Assign zero bit
	zero_bit <= B"0";
	zero_bit_a <= resize(zero_bit, AB_WIDTH);
	zero_bit_b <= resize(zero_bit, AB_WIDTH);
	zero_bit_c <= std_logic_vector(resize(zero_bit, SEL_WIDTH));
    
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab0 <= resize(zero_bit_a_pipeline2_reg, AB_WIDTH+1) + resize(zero_bit_b_pipeline2_reg, AB_WIDTH+1);
	ab1 <= resize(a1_pipeline2_reg, AB_WIDTH+1) + resize(b1_pipeline2_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline2_reg, AB_WIDTH+1) + resize(b2_pipeline2_reg, AB_WIDTH+1);
	ab3 <= resize(a3_pipeline2_reg, AB_WIDTH+1) + resize(b3_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c0_coef_wire <= signed(c0_coef(to_integer(unsigned(zero_bit_c_pipeline2_reg))));
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline2_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline2_reg))));
	c3_coef_wire <= signed(c3_coef(to_integer(unsigned(c3_sel_pipeline2_reg))));
    
    
	-- Multiplier
	m1 <= resize(c1_coef_wire,COEF_WIDTH) * resize(ab1,AB_WIDTH+1);
	m2 <= resize(c2_coef_wire,COEF_WIDTH) * resize(ab2,AB_WIDTH+1);
	m3 <= resize(c3_coef_wire,COEF_WIDTH) * resize(ab3,AB_WIDTH+1);
	-- When this template is used, the number of multipliers has to be even
	-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m0 <= resize(c0_coef_wire,COEF_WIDTH) * resize(ab0,AB_WIDTH+1);
        
	-- Final output 
	final_output <= s3_reg;

end rtl;
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- 18x19_systolic with full registers (input, pipeline, systolic and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = zero_bit_a*zero_bit_b + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] - a3[t-6]*b1(t-10) + a4[t-5]*b1[t-11] + a5(t-4)*b1(t-12) + acc_sel
--          where zero_bit_a*zero_bit_b is a dummy multiplier
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even. 
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf
 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm is 
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The max chain width for systolic mode is 44. 
		CHAIN_WIDTH : natural := 44;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		a5       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm is

-- Multiplier result
signal m1, m2, m3, m4, m5, m0: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Summation result
signal s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg : signed ((CHAIN_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, zero_bit_a_reg  : signed   ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, zero_bit_b_reg  : signed   ((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
signal b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg  : signed   ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg,  zero_bit_a_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg,  zero_bit_b_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg,  zero_bit_a_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg,  zero_bit_b_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

-- The following is required for the dummy multiplier. 
signal zero_bit        : signed    (0 downto 0);
signal zero_bit_a      : signed    ((A_WIDTH-1) downto 0);
signal zero_bit_b      : signed    ((B_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_a_pipeline1_reg: signal is true;
attribute preserve of zero_bit_b_pipeline1_reg: signal is true;
attribute preserve of zero_bit_a_pipeline2_reg: signal is true;
attribute preserve of zero_bit_b_pipeline2_reg: signal is true;

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, zero_bit_a_reg, b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, zero_bit_b_reg, b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg, zero_bit_a_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg, zero_bit_b_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg, zero_bit_a_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg, zero_bit_b_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s_reg, s_double, s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			a4_reg <= (others => '0');
			b4_reg <= (others => '0');
			a5_reg <= (others => '0');
			b5_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
			-- Input Cascade Delay register
			b1_delay_reg <= (others => '0');
			b2_delay_reg <= (others => '0');
			b3_delay_reg <= (others => '0');
			b4_delay_reg <= (others => '0');
			-- input for dummy multiplier 0x0
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
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
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently 
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			a3_pipeline1_reg <= (others => '0');
			b3_pipeline1_reg <= (others => '0'); 
			a4_pipeline1_reg <= (others => '0');
			b4_pipeline1_reg <= (others => '0');
			a5_pipeline1_reg <= (others => '0');
			b5_pipeline1_reg <= (others => '0');
			zero_bit_a_pipeline1_reg <= (others => '0');
			zero_bit_b_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- For systolic designs, the second pipeline register bank must use the same {clock, ena, reset} as the output register bank
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently 
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			a3_pipeline2_reg <= (others => '0');
			b3_pipeline2_reg <= (others => '0'); 
			a4_pipeline2_reg <= (others => '0');
			b4_pipeline2_reg <= (others => '0');
			a5_pipeline2_reg <= (others => '0');
			b5_pipeline2_reg <= (others => '0');
			zero_bit_a_pipeline2_reg <= (others => '0');
			zero_bit_b_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	-- Even though the output registers are not explicitly declared, they will be inferred later during compilation.
	-- Thus, it is important to place the s1_output_reg-s5_output_reg operation within the output register enable (i.e. ena3=1) condition. 
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s2_output_reg <= (others => '0');
			s3_output_reg <= (others => '0');
			s4_output_reg <= (others => '0');
			s5_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				-- chainout adder support static add or sub
				-- basic mult result (m) must be the second operand    
				s1_output_reg <= resize(m0, CHAIN_WIDTH);
				s2_output_reg <= s1_output_reg + resize(m1, CHAIN_WIDTH);
				s3_output_reg <= s2_output_reg + resize(m2, CHAIN_WIDTH);
				s4_output_reg <= s3_output_reg - resize(m3, CHAIN_WIDTH);
				s5_output_reg <= s4_output_reg + resize(m4, CHAIN_WIDTH);
				-- chainout accumulator only support addition when use with chainout adder
				s_reg <= acc_sel + (s5_output_reg + resize(m5, CHAIN_WIDTH)); -- loopback path (acc_sel) must be the first operand
				-- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Assign zero bit
zero_bit <= B"0";
zero_bit_a <= resize(zero_bit, A_WIDTH);
zero_bit_b <= resize(zero_bit, B_WIDTH);
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
	m5 <= (a5_pipeline2_reg * b5_pipeline2_reg);
	-- When this template is used, the number of multipliers has to be even
	-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m0 <= (zero_bit_a_pipeline2_reg * zero_bit_b_pipeline2_reg);
        
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M27x27 with Dynamic Negate with Output Chaining using ACLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + dynamic negate
-- Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-4]*b2[t-4]
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_chainoutadder_dynNegate_14nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- This example uses n=2 multiplers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 27 + 27 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic NEGATE control signals
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_chainoutadder_dynNegate_14nm is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg    : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg    : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg    : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg    : signed    ((B_WIDTH-1) downto 0);

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

-- Output Register
signal m1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);
signal final_output_reg: signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, m1_output_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. negate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			negate_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				negate_reg <= negate;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			negate_pipeline1_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				negate_pipeline1_reg <= negate_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			negate_pipeline2_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			m1_output_reg <= (others => '0');
			final_output_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				m1_output_reg <= m1;
                 
				-- Dynamic negate
				if (negate_pipeline2_reg = '1') then  
					final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(m2,FINAL_OUTPUT_WIDTH);
				else 
					final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(m2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);        

	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Preadder and Coefficent using SCLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using synchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_preadd_coef_sclr_14nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		AB_WIDTH    : natural := 26;
		COEF_WIDTH  : natural := 27;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH   : natural := 3
	);

	port 
	(
		-- Data input ports
		a        : in signed    ((AB_WIDTH-1) downto 0);
		b        : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1   : in std_logic;
		clock2   : in std_logic;
		clock3   : in std_logic;
		ena1     : in std_logic;
		ena2     : in std_logic;
		ena3     : in std_logic;
		sclr1    : in std_logic;
		sclr2    : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_preadd_coef_sclr_14nm is
-- This template uses std_logic_vector type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100001010100",
			"001010100111101011101010111",
			"001010111111101011000100000",
			"101010111111101011111111111",
			"001010000011010110101101101",
			"111010110000001011000011101",
			"001010111111010111111110110",
			"001010111111101011010111011");

-- Coefficient selection result
signal c_coef_wire : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab          : signed    ((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_reg   : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline1_reg    : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline1_reg    : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a_pipeline2_reg    : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline2_reg    : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a_reg, b_reg, c_sel_reg, a_pipeline1_reg, b_pipeline1_reg, c_sel_pipeline1_reg, a_pipeline2_reg, b_pipeline2_reg, c_sel_pipeline2_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same {clock, ena}
	-- The coefficient select input may use a different clock than that of the preadder inputs.
	-- All registered inputs must use the same reset
	process(clock1)
	begin
		if rising_edge(clock1) then
			if (ena1 = '1') then
				if (sclr1 = '1') then
					a_reg <= (others => '0');
					b_reg <= (others => '0');
					c_sel_reg <= (others => '0');
				else
					a_reg <= a;
					b_reg <= b;
					c_sel_reg <= c_sel;
				end if;
			end if;
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock2)
	begin
		if rising_edge(clock2) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a_pipeline1_reg <= (others => '0');
					b_pipeline1_reg <= (others => '0');
					c_sel_pipeline1_reg <= (others => '0');
				else
					a_pipeline1_reg <= a_reg;
					b_pipeline1_reg <= b_reg;
					c_sel_pipeline1_reg <= c_sel_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock2)
	begin
		if rising_edge(clock2) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a_pipeline2_reg <= (others => '0');
					b_pipeline2_reg <= (others => '0');
					c_sel_pipeline2_reg <= (others => '0');
				else
					a_pipeline2_reg <= a_pipeline1_reg;
					b_pipeline2_reg <= b_pipeline1_reg;
					c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3)
	begin
		if rising_edge(clock3) then
			if (ena3 = '1') then
				if (sclr2 = '1') then
					final_output_reg <= (others => '0');
				else
					-- Static add/sub is supported
					final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
				end if;
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline2_reg, AB_WIDTH+1) + resize(b_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline2_reg))));

	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-4]*b1[t-5]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
--     Note: The Input Delay register is not supported in 27x27 mode. 
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_14nm is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Output Register
signal s1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b1_reg;
				loadconst_reg <= loadconst;
				accum_reg <= accum;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				-- First 27x27 result. Support static add/sub 
				s1_output_reg <= m1;
				-- Accumulate and chainout adder                          
				s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(m2,CHAIN_WIDTH));
				--- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);        

	-- Final output
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate using ACLR
-- Quartus Prime VHDL Template
-- 18x19_plus36 with full registers (input, pipeline and output) using asynchronous clear + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = ((a1[t-5]*b1[t-5])+/-c1[t-5]) +/- ((a2[t-4]*b2[t-4])+/-c2[t-4])
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_plus36_full_regs_dynSub_dynNegate_14nm is 
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 multiplers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1
		FINAL_OUTPUT_WIDTH : natural := 39
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		c1       : in signed    ((C_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		c2       : in signed    ((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		-- The formula for the final_output width should be the larger value of either (A_WIDTH+B_WIDTH+2) or (C_WIDTH+2).
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_plus36_full_regs_dynSub_dynNegate_14nm is

-- Multiplier Result
signal m1, m2: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- 18x19_plus36 Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg       : signed    ((C_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline1_reg, c2_pipeline1_reg   : signed    ((C_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline2_reg, c2_pipeline2_reg   : signed    ((C_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline1_reg: std_logic;
signal addnsub2_pipeline1_reg: std_logic;

-- Sub Second Pipeline Register
signal addnsub1_pipeline2_reg: std_logic;
signal addnsub2_pipeline2_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

-- Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_reg, c2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_pipeline1_reg, c2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_pipeline2_reg, c2_pipeline2_reg, addnsub1_reg, addnsub2_reg, addnsub1_pipeline1_reg, addnsub2_pipeline1_reg, addnsub1_pipeline2_reg, addnsub2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, s1_output_reg, s_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.    
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= '0';
			addnsub2_reg <= '0';
			negate_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				addnsub1_reg <= addnsub1; 
				addnsub2_reg <= addnsub2; 
				negate_reg <= negate;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			c1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c2_pipeline1_reg <= (others => '0');
			addnsub1_pipeline1_reg <= '0';
			addnsub2_pipeline1_reg <= '0';
			negate_pipeline1_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				c1_pipeline1_reg <= c1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c2_pipeline1_reg <= c2_reg;
				addnsub1_pipeline1_reg <= addnsub1_reg;
				addnsub2_pipeline1_reg <= addnsub2_reg;
				negate_pipeline1_reg <= negate_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock2, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			c1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c2_pipeline2_reg <= (others => '0');
			addnsub1_pipeline2_reg <= '0';
			addnsub2_pipeline2_reg <= '0';
			negate_pipeline2_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				c1_pipeline2_reg <= c1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c2_pipeline2_reg <= c2_pipeline1_reg;
				addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
				addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register must share the same reset with the input pipeline and second pipeline register banks
	process(clock3, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				s1_output_reg <= s1;
                 
				-- Dynamic negate
				if (negate_pipeline2_reg = '1') then  
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) - resize(s2,FINAL_OUTPUT_WIDTH);
				else 
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) + resize(s2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
            
		end if;

	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
    
	-- First 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub1_pipeline2_reg select
	s1 <=  resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) - resize(m1, SUM_OUTPUT_WIDTH)  when '1',
			resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH) when others;
            
	-- Second 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub2_pipeline2_reg select
	s2 <=  resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH) when '1',
			resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH) when others;
    
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- Two 18x19_plus36 with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + ((a1[t-5]*b1[t-5])+c1[t-5]) + ((a2[t-4]*b2[t-4])+c2[t-4])
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- Note: Input cascade chain is not supported in 18x19_plus36 mode.
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_14nm is
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		c1       : in signed    ((C_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		c2       : in signed    ((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1    : in std_logic;
		clock2    : in std_logic;
		clock3    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_14nm is

-- Multiplier Result
signal m1, m2  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg      : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg      : signed    ((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg      : signed    ((C_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg  : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg  : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline1_reg, c2_pipeline1_reg  : signed    ((C_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg  : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg  : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline2_reg, c2_pipeline2_reg  : signed    ((C_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg: signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_reg, c2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_pipeline1_reg, c2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_pipeline2_reg, c2_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- Accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input must use the same reset signal, 
	-- Each DATA input register may have different pair of clock and ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock/ena signal pair than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock and ena signal.
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				loadconst_reg <= loadconst;
				accum_reg <= accum;
			end if;
            
		end if;

	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	process(clock2, aclr2 )
	begin
		if (aclr2  = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			c1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c2_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				c1_pipeline1_reg <= c1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c2_pipeline1_reg <= c2_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypass differently 
	process(clock2, aclr2 )
	begin
		if (aclr2  = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			c1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c2_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				c1_pipeline2_reg <= c1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c2_pipeline2_reg <= c2_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with input pipeline and second pipeline register banks
	process(clock3, aclr2 )
	begin
		if (aclr2  = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
            
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
				-- First 18x19_plus36. Support static add/sub. 
				-- Addend must be the first operand 
				s1_output_reg <= resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH);
				-- Accumulate and chainout adder                          
				s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
				--- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
    
	-- Second 18x19_plus36. Support static add/sub
	-- Addend must be the first operand
	s2 <= resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m2,SUM_OUTPUT_WIDTH);

	-- Final output
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_full Single Multiplier with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- m18x19_full mode by utilizing half a DSP block resource
-- Single multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
--    Note: This mode does not support chainout adder nor dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
-- For use with 14-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_single_mult_full_regs_preadd_coef_14nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH    : natural := 18;
		COEF_WIDTH  : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH   : natural := 3
	);

	port 
	(
		-- Data input ports
		a       : in signed    ((AB_WIDTH-1) downto 0);
		b       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel   : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports up to 3 clock/ena pairs, and 2 reset signals
		clock1  : in std_logic;
		clock2  : in std_logic;
		clock3  : in std_logic;
		ena1    : in std_logic;
		ena2    : in std_logic;
		ena3    : in std_logic;
		aclr1   : in std_logic;
		aclr2   : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of m18x19_single_mult_full_regs_preadd_coef_14nm is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"101010111111101011",
			"001010000011010110",
			"111010110000001011",
			"001010111111010111",
			"001010111111101011");

-- Coefficient selection result
signal c_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab           : signed    ((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_reg   : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline1_reg      : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline1_reg      : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline1_reg  : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a_pipeline2_reg      : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline2_reg      : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline2_reg  : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a_reg, b_reg, c_sel_reg, a_pipeline1_reg, b_pipeline1_reg, c_sel_pipeline1_reg, a_pipeline2_reg, b_pipeline2_reg, c_sel_pipeline2_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Data Input register 
	-- DSP supports up to 3 clock/ena pairs and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same clock/ena signal pair
	-- The coefficient select input may use a different clock than that of the preadder inputs.
	-- All registered inputs must use the same reset
	process(clock1, aclr1)
	begin
		if (aclr1 = '1') then
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			c_sel_reg <= (others => '0');
		elsif rising_edge(clock1) then

			if (ena1 = '1') then
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {clock, ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock2, aclr2 )
	begin
		if (aclr2  = '1') then
			a_pipeline1_reg <= (others => '0');
			b_pipeline1_reg <= (others => '0');
			c_sel_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a_pipeline1_reg <= a_reg;
				b_pipeline1_reg <= b_reg;
				c_sel_pipeline1_reg <= c_sel_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {clock, ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock2, aclr2 )
	begin
		if (aclr2  = '1') then
			a_pipeline2_reg <= (others => '0');
			b_pipeline2_reg <= (others => '0');
			c_sel_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock2) then

			if (ena2 = '1') then
				a_pipeline2_reg <= a_pipeline1_reg;
				b_pipeline2_reg <= b_pipeline1_reg;
				c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- THe output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock3, aclr2 )
	begin
		if (aclr2  = '1') then
			final_output_reg <= (others => '0');
            
		elsif rising_edge(clock3) then

			if (ena3 = '1') then
			-- Static add/sub is supported
				final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline2_reg, AB_WIDTH+1) + resize(b_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline2_reg))));

	-- Final output
final_output <= final_output_reg;

end rtl;
end_template
end_group
begin_group DSP Features for 10-nm Device 
begin_template M18x19_sumof2 with Dynamic Sub, Dynamic Negate and Output Chaining using SCLR
-- Quartus Prime VHDL Template
-- Two 'sum of 2 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-5]*b2[t-5] +/- a3[t-4]*b3[t-4] +/- a4[t-4]*b4[t-4]
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_10nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The formula for the output width of 1 sum of two 18x19 multipliers. 
		-- SUM_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 Sum of two 18x19 multipliers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 38 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		b3       : in signed    ((B_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		b4       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock     : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		sclr1     : in std_logic;
		sclr2     : in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_dynSub_dynNegate_sclr_10nm is

-- Multiplier Result
signal m1, m2, m3, m4 : signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Sum Of 2 Multipliers Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg       : signed    ((B_WIDTH-1) downto 0);

--Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg       : signed    ((B_WIDTH-1) downto 0);

--Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline1_reg: std_logic;
signal addnsub2_pipeline1_reg: std_logic;

-- Sub Second Pipeline Register
signal addnsub1_pipeline2_reg: std_logic;
signal addnsub2_pipeline2_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

--Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal final_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, b1_reg, b2_reg, b3_reg, b4_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, addnsub1_reg, addnsub2_reg, addnsub1_pipeline1_reg, addnsub2_pipeline1_reg, addnsub1_pipeline2_reg, addnsub2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, s1_output_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may have different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different ena signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.    
	process(clock)
	begin
		if rising_edge(clock) then

			if (ena1 = '1') then
				if (sclr1 = '1') then
					-- Input registers (for DATA)
					a1_reg <= (others => '0');
					b1_reg <= (others => '0');
					a2_reg <= (others => '0');
					b2_reg <= (others => '0');
					a3_reg <= (others => '0');
					b3_reg <= (others => '0');
					a4_reg <= (others => '0');
					b4_reg <= (others => '0');
					-- Input registers (for DYNAMIC CONTROL SIGNAL)
					addnsub1_reg <= '0';
					addnsub2_reg <= '0';
					negate_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock)
	begin
		if rising_edge(clock) then

			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline1_reg <= (others => '0');
					b1_pipeline1_reg <= (others => '0');
					a2_pipeline1_reg <= (others => '0');
					b2_pipeline1_reg <= (others => '0');
					a3_pipeline1_reg <= (others => '0');
					b3_pipeline1_reg <= (others => '0');
					a4_pipeline1_reg <= (others => '0');
					b4_pipeline1_reg <= (others => '0');
					addnsub1_pipeline1_reg <= '0';
					addnsub2_pipeline1_reg <= '0';
					negate_pipeline1_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock)
	begin
		if rising_edge(clock) then

			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline2_reg <= (others => '0');
					b1_pipeline2_reg <= (others => '0');
					a2_pipeline2_reg <= (others => '0');
					b2_pipeline2_reg <= (others => '0');
					a3_pipeline2_reg <= (others => '0');
					b3_pipeline2_reg <= (others => '0');
					a4_pipeline2_reg <= (others => '0');
					b4_pipeline2_reg <= (others => '0');
					addnsub1_pipeline2_reg <= '0';
					addnsub2_pipeline2_reg <= '0';
					negate_pipeline2_reg <= '0';
				else
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
				end if;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock)
	begin
		if rising_edge(clock) then

			if (ena3 = '1') then
				if (sclr2 = '1') then
					s1_output_reg <= (others => '0');
					final_output_reg <= (others => '0');
				else
					s1_output_reg <= s1;
                    
					-- Dynamic negate
					if (negate_pipeline2_reg = '1') then  
					final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(s2,FINAL_OUTPUT_WIDTH);
					else 
					final_output_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(s2,FINAL_OUTPUT_WIDTH);
					end if;
				end if;
			end if;
            
		end if;
	end process;
    
	-- Multipliers
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
    
	-- Dynamic add/sub
	with addnsub1_pipeline2_reg select
	s1 <= (resize(m1, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m1, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH)) when others;
            
	-- Dynamic add/sub
	with addnsub2_pipeline2_reg select
	s2 <= (resize(m3, SUM_OUTPUT_WIDTH) - resize(m4, SUM_OUTPUT_WIDTH)) when '1',
			(resize(m3, SUM_OUTPUT_WIDTH) + resize(m4, SUM_OUTPUT_WIDTH)) when others;
    
	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M18x19_sumof2 with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- Sum of two 18x19 multipliers with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a1[t-4]+b1[t-4])*c1_coef[t-4] + (a2[t-4]+b2[t-4])*c2_coef[t-4]
-- Both multiplier in one DSP block must use coefficient input simultaneously
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_preadd_coef_10nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH   : natural := 18;
		COEF_WIDTH : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH  : natural := 3;
		-- The formula for the multipler width of one (A+B) x Coefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH + 1) + COEF_WIDTH;
		MULT_OUTPUT_WIDTH   : natural := (18+1)+ 18;
		-- This example uses n=2 multiplers, hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 1
		FINAL_OUTPUT_WIDTH  : natural := 37 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((AB_WIDTH-1) downto 0);
		b1       : in signed    ((AB_WIDTH-1) downto 0);
		a2       : in signed    ((AB_WIDTH-1) downto 0);
		b2       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_preadd_coef_10nm is
-- This template uses std_logic_vector type as the coefficient constant
-- It is possible use other preferred type, for example, signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1)) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c1_coef : coef_type := 
			("001010111111101011",
			"001010111111101011",
			"001010110000001011",
			"001010000011101011",
			"001010111111101011",
			"001010111111101011",
			"001010100111101011",
			"110101111001110100");
             
signal c2_coef : coef_type :=
			("001010101001000110",
			"011010111111101011",
			"001011011000001010",
			"101010100011101011",
			"001010110101101010",
			"001010110111011011",
			"011010101110101010",
			"010101011010100100"); 

-- Coefficient selection result
signal c1_coef_wire   : signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire   : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab1            : signed    ((AB_WIDTH) downto 0);
signal ab2            : signed    ((AB_WIDTH) downto 0);

-- Multiplier result        
signal m1, m2 : signed ((MULT_OUTPUT_WIDTH-1) downto 0);

-- Input Register
signal a1_reg, a2_reg : signed    ((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_reg     : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg     : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal s_output_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_sel_reg, c2_sel_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, s_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Data Input register 
	-- DSP supports single clock, 3 ena and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same {ena}
	-- The coefficient select input may use a different clock enable than that of the preadder inputs. 
	-- All registered inputs must use the same reset
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				c1_sel_reg <= c1_sel;
			c2_sel_reg <= c2_sel;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c1_sel_pipeline1_reg <= (others => '0');
			c2_sel_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c1_sel_pipeline1_reg <= c1_sel_reg;
				c2_sel_pipeline1_reg <= c2_sel_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c1_sel_pipeline2_reg <= (others => '0');
			c2_sel_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c1_sel_pipeline2_reg <= c1_sel_pipeline1_reg;
				c2_sel_pipeline2_reg <= c2_sel_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			s_output_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				-- Static add/sub is supported
				s_output_reg <= (resize(m1, FINAL_OUTPUT_WIDTH) + resize(m2, FINAL_OUTPUT_WIDTH));
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab1 <= resize(a1_pipeline2_reg, AB_WIDTH+1) + resize(b1_pipeline2_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline2_reg, AB_WIDTH+1) + resize(b2_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline2_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline2_reg))));
        
	-- Multiplier
	m1 <= c1_coef_wire * ab1;
	m2 <= c2_coef_wire * ab2;
    
	-- Final output
	final_output <= s_output_reg;


end rtl;
end_template
begin_template M18x19_sumof2 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
-- Quartus Prime VHDL Template
-- Two 'sum of two 18x19 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-5]*b1[t-6] + a3[t-4]*b1[t-7] + a4[t-4]*b1[t-8]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
        
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		sclr1     : in std_logic;
		sclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_sum_of_2_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm is

-- Multiplier Result
signal m1, m2, m3, m4  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
signal b2_delay_reg: signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg : signed ((A_WIDTH+B_WIDTH) downto 0);
signal s2 : signed ((A_WIDTH+B_WIDTH) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, b1_reg, b2_reg, b3_reg, b4_reg, b2_delay_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
								s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
					(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal. 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena1 = '1') then
				if (sclr1 = '1') then
					-- Input registers (for DATA)
					a1_reg <= (others => '0');
					b1_reg <= (others => '0');
					a2_reg <= (others => '0');
					b2_reg <= (others => '0');
					a3_reg <= (others => '0');
					b3_reg <= (others => '0');
					a4_reg <= (others => '0');
					b4_reg <= (others => '0');
					-- Input Cascade Delay register
					b2_delay_reg <= (others => '0');
					-- Input registers (for DYNAMIC CONTROL SIGNAL)
					loadconst_reg <= '0';
					accum_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline1_reg <= (others => '0');
					b1_pipeline1_reg <= (others => '0');
					a2_pipeline1_reg <= (others => '0');
					b2_pipeline1_reg <= (others => '0');
					a3_pipeline1_reg <= (others => '0');
					b3_pipeline1_reg <= (others => '0');
					a4_pipeline1_reg <= (others => '0');
					b4_pipeline1_reg <= (others => '0');
					loadconst_pipeline1_reg <= '0';
					accum_pipeline1_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline2_reg <= (others => '0');
					b1_pipeline2_reg <= (others => '0');
					a2_pipeline2_reg <= (others => '0');
					b2_pipeline2_reg <= (others => '0');
					a3_pipeline2_reg <= (others => '0');
					b3_pipeline2_reg <= (others => '0');
					a4_pipeline2_reg <= (others => '0');
					b4_pipeline2_reg <= (others => '0');
					loadconst_pipeline2_reg <= '0';
					accum_pipeline2_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena3 = '1') then
				if (sclr2 = '1') then
					s1_output_reg <= (others => '0');
					s_reg <= (others => '0');
					s_double <= (others => '0');
				else
					-- Sum of 2 multiplier. Support static add/sub 
					s1_output_reg <= (resize(m1, A_WIDTH+B_WIDTH+1) + resize(m2, A_WIDTH+B_WIDTH+1));
					-- Accumulate and chainout adder
					s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
					-- Double Accumulate
					s_double <= s_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
    
	-- Sum of 2 multiplier. Support static add/sub
	s2 <= (resize(m3, A_WIDTH+B_WIDTH+1) + resize(m4, A_WIDTH+B_WIDTH+1));

	-- Final output
final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_systolic with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- 18x19_systolic with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = ((a1[t-6]+b1[t-6])*c1_coef[t-6]) + ((a2[t-5]+b2[t-5])*c2_coef[t-5]) - ((a3[t-4]+b3[t-4])*c3_coef[t-4]) + (zero_bit_a+zero_bit_b)*c0_coef
--          where (zero_bit_a+zero_bit_b)*c0_coef is a dummy multiplier
-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even.
-- Both multipliers in one DSP block must use coefficient inputs simultaneously
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_systolic_full_regs_preadd_coef_10nm is 
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH   : natural := 18;
		COEF_WIDTH : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH  : natural := 3;
		-- The formula for the multipler width of one (A+B)xCoefficient.
		-- MULT_OUTPUT_WIDTH = (AB_WIDTH+1) + COEF_WIDTH
		MULT_OUTPUT_WIDTH : natural := (18 + 1)+ 18;
		-- This example uses n=4 multiplers (including dummy multiplier), hence the final output width is MULT_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = MULT_OUTPUT_WIDTH + 3
		FINAL_OUTPUT_WIDTH : natural := 37 +3
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((AB_WIDTH-1) downto 0);
		b1       : in signed    ((AB_WIDTH-1) downto 0);
		a2       : in signed    ((AB_WIDTH-1) downto 0);
		b2       : in signed    ((AB_WIDTH-1) downto 0);
		a3       : in signed    ((AB_WIDTH-1) downto 0);
		b3       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c1_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c2_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		c3_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_systolic_full_regs_preadd_coef_10nm is
-- This template uses std_logic_vector type as the coeffecient constant
-- It is possible to use other preferred type, for example, signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1)) of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
signal c1_coef : coef_type := 
			("001010111111101011",
			"001010111111101011",
			"001010110000001011",
			"001010000011101011",
			"001010111111101011",
			"001010111111101011",
			"001010100111101011",
			"110101111001110100");
             
signal c2_coef : coef_type :=
			("001010101001000110",
			"011010111111101011",
			"001011011000001010",
			"101010100011101011",
			"001010110101101010",
			"001010110111011011",
			"011010101110101010",
			"010101011010100100"); 
            
signal c3_coef : coef_type :=
			("100101011001000110",
			"010100101111101011",
			"001001010000001010",
			"101011010101101011",
			"001000110101101010",
			"001010111000111011",
			"101010011010101010",
			"010101010101101100"); 
            
-- To fulfil even number requirement for systolic mode
signal c0_coef : coef_type :=
			("000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000",
			"000000000000000000");

-- Coefficient selection result
signal c0_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c1_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c2_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);
signal c3_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab0            : signed    ((AB_WIDTH) downto 0);
signal ab1            : signed    ((AB_WIDTH) downto 0);
signal ab2            : signed    ((AB_WIDTH) downto 0);
signal ab3            : signed    ((AB_WIDTH) downto 0);

-- Multiplier result
signal m1, m2, m3, m0    : signed ((MULT_OUTPUT_WIDTH-1) downto 0);
-- Summation result
signal s1_reg, s2_reg, s3_reg, s0_reg    : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, zero_bit_a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, zero_bit_b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg,  zero_bit_a_pipeline1_reg   : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg,  zero_bit_b_pipeline1_reg   : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg,  zero_bit_a_pipeline2_reg   : signed    ((AB_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg,  zero_bit_b_pipeline2_reg   : signed    ((AB_WIDTH-1) downto 0);
signal c1_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c2_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal c3_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);
signal zero_bit_c_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- The following is required for the dummy multiplier. 
signal zero_bit                  : signed    (0 downto 0);
signal zero_bit_a                : signed    ((AB_WIDTH-1) downto 0);
signal zero_bit_b                : signed    ((AB_WIDTH-1) downto 0);
signal zero_bit_c                : std_logic_vector ((SEL_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_c_reg: signal is true;
attribute preserve of zero_bit_a_pipeline1_reg: signal is true;
attribute preserve of zero_bit_b_pipeline1_reg: signal is true;
attribute preserve of zero_bit_c_pipeline1_reg: signal is true;
attribute preserve of zero_bit_a_pipeline2_reg: signal is true;
attribute preserve of zero_bit_b_pipeline2_reg: signal is true;
attribute preserve of zero_bit_c_pipeline2_reg: signal is true;

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, zero_bit_a_reg, b1_reg, b2_reg, b3_reg, zero_bit_b_reg, c1_sel_reg, c2_sel_reg, c3_sel_reg, zero_bit_c_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, zero_bit_a_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, zero_bit_b_pipeline1_reg, c1_sel_pipeline1_reg, c2_sel_pipeline1_reg, c3_sel_pipeline1_reg, zero_bit_c_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg,  zero_bit_a_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg,  zero_bit_b_pipeline2_reg, c1_sel_pipeline2_reg, c2_sel_pipeline2_reg, c3_sel_pipeline2_reg, zero_bit_c_pipeline2_reg, s1_reg, s2_reg, s3_reg, s0_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may hava different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different ena signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.     
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			c1_sel_reg <= (others => '0');
			c2_sel_reg <= (others => '0');
			c3_sel_reg <= (others => '0');
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
			zero_bit_c_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				a3_reg <= a3;
				b3_reg <= b3;
				c1_sel_reg <= c1_sel;
				c2_sel_reg <= c2_sel;
				c3_sel_reg <= c3_sel;
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
				zero_bit_c_reg <= zero_bit_c;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			a3_pipeline1_reg <= (others => '0');
			b3_pipeline1_reg <= (others => '0'); 
			c1_sel_pipeline1_reg <= (others => '0');
			c2_sel_pipeline1_reg <= (others => '0');
			c3_sel_pipeline1_reg <= (others => '0');
			zero_bit_a_pipeline1_reg <= (others => '0');
			zero_bit_b_pipeline1_reg <= (others => '0');
			zero_bit_c_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena2 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- For systolic designs, the second pipeline register bank must use the same {ena, reset} as the output register bank
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			a3_pipeline2_reg <= (others => '0');
			b3_pipeline2_reg <= (others => '0'); 
			c1_sel_pipeline2_reg <= (others => '0');
			c2_sel_pipeline2_reg <= (others => '0');
			c3_sel_pipeline2_reg <= (others => '0');
			zero_bit_a_pipeline2_reg <= (others => '0');
			zero_bit_b_pipeline2_reg <= (others => '0');
			zero_bit_c_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with input pipeline and second pipeline register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			s0_reg <= (others => '0');
			s1_reg <= (others => '0');
			s2_reg <= (others => '0');
			s3_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				s0_reg <= resize(m0, FINAL_OUTPUT_WIDTH);
				--static add/sub supported
				s1_reg <= s0_reg + resize(m1, FINAL_OUTPUT_WIDTH);
				s2_reg <= s1_reg + resize(m2, FINAL_OUTPUT_WIDTH);
				s3_reg <= s2_reg - resize(m3, FINAL_OUTPUT_WIDTH);
			end if;
            
		end if;
	end process;
    
	-- Assign zero bit
	zero_bit <= B"0";
	zero_bit_a <= resize(zero_bit, AB_WIDTH);
	zero_bit_b <= resize(zero_bit, AB_WIDTH);
	zero_bit_c <= std_logic_vector(resize(zero_bit, SEL_WIDTH));
    
	-- Preadder
	-- Preadder supports static add/sub
	-- Both 18x18 in one DSP block must use preadder simultaneously
	-- Both 18x18 in one DSP block must have the same add/sub
	ab0 <= resize(zero_bit_a_pipeline2_reg, AB_WIDTH+1) + resize(zero_bit_b_pipeline2_reg, AB_WIDTH+1);
	ab1 <= resize(a1_pipeline2_reg, AB_WIDTH+1) + resize(b1_pipeline2_reg, AB_WIDTH+1);
	ab2 <= resize(a2_pipeline2_reg, AB_WIDTH+1) + resize(b2_pipeline2_reg, AB_WIDTH+1);
	ab3 <= resize(a3_pipeline2_reg, AB_WIDTH+1) + resize(b3_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c0_coef_wire <= signed(c0_coef(to_integer(unsigned(zero_bit_c_pipeline2_reg))));
	c1_coef_wire <= signed(c1_coef(to_integer(unsigned(c1_sel_pipeline2_reg))));
	c2_coef_wire <= signed(c2_coef(to_integer(unsigned(c2_sel_pipeline2_reg))));
	c3_coef_wire <= signed(c3_coef(to_integer(unsigned(c3_sel_pipeline2_reg))));
    
    
	-- Multiplier
	m1 <= resize(c1_coef_wire,COEF_WIDTH) * resize(ab1,AB_WIDTH+1);
	m2 <= resize(c2_coef_wire,COEF_WIDTH) * resize(ab2,AB_WIDTH+1);
	m3 <= resize(c3_coef_wire,COEF_WIDTH) * resize(ab3,AB_WIDTH+1);
	-- When this template is used, the number of multipliers has to be even
	-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m0 <= resize(c0_coef_wire,COEF_WIDTH) * resize(ab0,AB_WIDTH+1);
        
	-- Final output 
	final_output <= s3_reg;

end rtl;
end_template
begin_template M18x19_systolic with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- 18x19_systolic with full registers (input, pipeline, systolic and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = zero_bit_a*zero_bit_b + a1[t-8]*b1[t-8] + a2[t-7]*b1[t-9] - a3[t-6]*b1(t-10) + a4[t-5]*b1[t-11] + a5(t-4)*b1(t-12) + acc_sel
--          where zero_bit_a*zero_bit_b is a dummy multiplier
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- When this template is used, the number of multipliers has to be even
-- A dummy 0x0 multiplier can be created if the number of multipliers is odd to make up the number to even. 
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf
 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm is 
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		-- The max chain width for systolic mode is 44. 
		CHAIN_WIDTH : natural := 44;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		a5       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 44
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_systolic_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm is

-- Multiplier result
signal m1, m2, m3, m4, m5, m0: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- Summation result
signal s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg : signed ((CHAIN_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, zero_bit_a_reg  : signed   ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, zero_bit_b_reg  : signed   ((B_WIDTH-1) downto 0);

-- Data Input Cascade Delay register
signal b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg  : signed   ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg,  zero_bit_a_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg,  zero_bit_b_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg,  zero_bit_a_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg,  zero_bit_b_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

-- The following is required for the dummy multiplier. 
signal zero_bit        : signed    (0 downto 0);
signal zero_bit_a      : signed    ((A_WIDTH-1) downto 0);
signal zero_bit_b      : signed    ((B_WIDTH-1) downto 0);
attribute preserve: boolean;
attribute preserve of zero_bit_a_reg: signal is true;
attribute preserve of zero_bit_b_reg: signal is true;
attribute preserve of zero_bit_a_pipeline1_reg: signal is true;
attribute preserve of zero_bit_b_pipeline1_reg: signal is true;
attribute preserve of zero_bit_a_pipeline2_reg: signal is true;
attribute preserve of zero_bit_b_pipeline2_reg: signal is true;

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, zero_bit_a_reg, b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, zero_bit_b_reg, b1_delay_reg, b2_delay_reg, b3_delay_reg, b4_delay_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg, zero_bit_a_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg, zero_bit_b_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg, zero_bit_a_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg, zero_bit_b_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s_reg, s_double, s1_output_reg, s2_output_reg, s3_output_reg, s4_output_reg, s5_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different pair of clock ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal. 
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
		-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			a3_reg <= (others => '0');
			b3_reg <= (others => '0'); 
			a4_reg <= (others => '0');
			b4_reg <= (others => '0');
			a5_reg <= (others => '0');
			b5_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
			-- Input Cascade Delay register
			b1_delay_reg <= (others => '0');
			b2_delay_reg <= (others => '0');
			b3_delay_reg <= (others => '0');
			b4_delay_reg <= (others => '0');
			-- input for dummy multiplier 0x0
			zero_bit_a_reg <= (others => '0');
			zero_bit_b_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena1 = '1') then
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
				zero_bit_a_reg <= zero_bit_a;
				zero_bit_b_reg <= zero_bit_b;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently 
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			a3_pipeline1_reg <= (others => '0');
			b3_pipeline1_reg <= (others => '0'); 
			a4_pipeline1_reg <= (others => '0');
			b4_pipeline1_reg <= (others => '0');
			a5_pipeline1_reg <= (others => '0');
			b5_pipeline1_reg <= (others => '0');
			zero_bit_a_pipeline1_reg <= (others => '0');
			zero_bit_b_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- For systolic designs, the second pipeline register bank must use the same {clock, ena, reset} as the output register bank
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently 
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			a3_pipeline2_reg <= (others => '0');
			b3_pipeline2_reg <= (others => '0'); 
			a4_pipeline2_reg <= (others => '0');
			b4_pipeline2_reg <= (others => '0');
			a5_pipeline2_reg <= (others => '0');
			b5_pipeline2_reg <= (others => '0');
			zero_bit_a_pipeline2_reg <= (others => '0');
			zero_bit_b_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock) then

			if (ena3 = '1') then
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
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	-- Even though the output registers are not explicitly declared, they will be inferred later during compilation.
	-- Thus, it is important to place the s1_output_reg-s5_output_reg operation within the output register enable (i.e. ena3=1) condition. 
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s2_output_reg <= (others => '0');
			s3_output_reg <= (others => '0');
			s4_output_reg <= (others => '0');
			s5_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				-- chainout adder support static add or sub
				-- basic mult result (m) must be the second operand    
				s1_output_reg <= resize(m0, CHAIN_WIDTH);
				s2_output_reg <= s1_output_reg + resize(m1, CHAIN_WIDTH);
				s3_output_reg <= s2_output_reg + resize(m2, CHAIN_WIDTH);
				s4_output_reg <= s3_output_reg - resize(m3, CHAIN_WIDTH);
				s5_output_reg <= s4_output_reg + resize(m4, CHAIN_WIDTH);
				-- chainout accumulator only support addition when use with chainout adder
				s_reg <= acc_sel + (s5_output_reg + resize(m5, CHAIN_WIDTH)); -- loopback path (acc_sel) must be the first operand
				-- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Assign zero bit
zero_bit <= B"0";
zero_bit_a <= resize(zero_bit, A_WIDTH);
zero_bit_b <= resize(zero_bit, B_WIDTH);
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
	m5 <= (a5_pipeline2_reg * b5_pipeline2_reg);
	-- When this template is used, the number of multipliers has to be even
	-- Create a 0x0 multiplier as below to make up for the even number requirement if the number of multipliers is odd
	m0 <= (zero_bit_a_pipeline2_reg * zero_bit_b_pipeline2_reg);
        
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M27x27 with Dynamic Negate with Output Chaining using ACLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + dynamic negate
-- Formula: final_output[t] = a1[t-5]*b1[t-5] +/- a2[t-4]*b2[t-4]
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_chainoutadder_dynNegate_10nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- This example uses n=2 multiplers, hence the final output width is A_WIDTH + B_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = A_WIDTH + B_WIDTH + 1
		FINAL_OUTPUT_WIDTH : natural := 27 + 27 + 1
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic NEGATE control signals
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_chainoutadder_dynNegate_10nm is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg    : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg    : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg    : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg    : signed    ((B_WIDTH-1) downto 0);

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

-- Output Register
signal m1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);
signal final_output_reg: signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, m1_output_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may hava different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. negate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal. 
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			negate_reg <= '0';
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b2;
				negate_reg <= negate;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			negate_pipeline1_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				negate_pipeline1_reg <= negate_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			negate_pipeline2_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			m1_output_reg <= (others => '0');
			final_output_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				m1_output_reg <= m1;
                 
				-- Dynamic negate
				if (negate_pipeline2_reg = '1') then  
					final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  - resize(m2,FINAL_OUTPUT_WIDTH);
				else 
					final_output_reg <= resize(m1_output_reg,FINAL_OUTPUT_WIDTH)  + resize(m2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);        

	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Preadder and Coefficent using SCLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using synchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_preadd_coef_sclr_10nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		AB_WIDTH    : natural := 26;
		COEF_WIDTH  : natural := 27;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH   : natural := 3
	);

	port 
	(
		-- Data input ports
		a        : in signed    ((AB_WIDTH-1) downto 0);
		b        : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel    : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock   : in std_logic;
		ena1     : in std_logic;
		ena2     : in std_logic;
		ena3     : in std_logic;
		sclr1    : in std_logic;
		sclr2    : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_preadd_coef_sclr_10nm is
-- This template uses std_logic_vector type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100001010100",
			"001010100111101011101010111",
			"001010111111101011000100000",
			"101010111111101011111111111",
			"001010000011010110101101101",
			"111010110000001011000011101",
			"001010111111010111111110110",
			"001010111111101011010111011");

-- Coefficient selection result
signal c_coef_wire : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab          : signed    ((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_reg   : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline1_reg    : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline1_reg    : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline1_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a_pipeline2_reg    : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline2_reg    : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline2_reg    : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a_reg, b_reg, c_sel_reg, a_pipeline1_reg, b_pipeline1_reg, c_sel_pipeline1_reg, a_pipeline2_reg, b_pipeline2_reg, c_sel_pipeline2_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Data Input register 
	-- DSP supports single clock, 3 ena and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same {ena}
	-- The coefficient select input may use a different clock enable than that of the preadder inputs.
	-- All registered inputs must use the same reset
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena1 = '1') then
				if (sclr1 = '1') then
					a_reg <= (others => '0');
					b_reg <= (others => '0');
					c_sel_reg <= (others => '0');
				else
					a_reg <= a;
					b_reg <= b;
					c_sel_reg <= c_sel;
				end if;
			end if;
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a_pipeline1_reg <= (others => '0');
					b_pipeline1_reg <= (others => '0');
					c_sel_pipeline1_reg <= (others => '0');
				else
					a_pipeline1_reg <= a_reg;
					b_pipeline1_reg <= b_reg;
					c_sel_pipeline1_reg <= c_sel_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a_pipeline2_reg <= (others => '0');
					b_pipeline2_reg <= (others => '0');
					c_sel_pipeline2_reg <= (others => '0');
				else
					a_pipeline2_reg <= a_pipeline1_reg;
					b_pipeline2_reg <= b_pipeline1_reg;
					c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena3 = '1') then
				if (sclr2 = '1') then
					final_output_reg <= (others => '0');
				else
					-- Static add/sub is supported
					final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
				end if;
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline2_reg, AB_WIDTH+1) + resize(b_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline2_reg))));

	-- Final output
	final_output <= final_output_reg;

end rtl;
end_template
begin_template M27x27 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- m27x27 multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + input cascade chain (and input delay register) + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + a1[t-5]*b1[t-5] + a2[t-4]*b1[t-5]
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
--     Note: The Input Delay register is not supported in 27x27 mode. 
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm is
	generic
	(
		-- This template will only work where one of the two multiplier operands(signed A and signed B) data width falls within >19 and <=27.
		A_WIDTH : natural := 27;
		B_WIDTH : natural := 27;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m27x27_full_regs_inputcascade_chainoutadder_acc_doubleacc_preloadConst_10nm is

-- Multiplier Result
signal m1, m2 : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Output Register
signal s1_output_reg: signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL).
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may hava different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal. 
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				a2_reg <= a2;
				b2_reg <= b1_reg;
				loadconst_reg <= loadconst;
				accum_reg <= accum;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				-- First 27x27 result. Support static add/sub 
				s1_output_reg <= m1;
				-- Accumulate and chainout adder                          
				s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(m2,CHAIN_WIDTH));
				--- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);        

	-- Final output
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Dynamic Sub and Dynamic Negate using ACLR
-- Quartus Prime VHDL Template
-- 18x19_plus36 with full registers (input, pipeline and output) using asynchronous clear + dynamic add/sub + dynamic negate
-- Formula: final_output[t] = ((a1[t-5]*b1[t-5])+/-c1[t-5]) +/- ((a2[t-4]*b2[t-4])+/-c2[t-4])
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_plus36_full_regs_dynSub_dynNegate_10nm is 
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- This example uses n=2 multiplers, hence the final output width is SUM_OUTPUT_WIDTH + (n-1)
		-- FINAL_OUTPUT_WIDTH = SUM_OUTPUT_WIDTH +1
		FINAL_OUTPUT_WIDTH : natural := 39
	);

	port 
	(
		-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		c1       : in signed    ((C_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		c2       : in signed    ((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic addition and subtraction control signals
		addnsub1  : in std_logic;
		addnsub2  : in std_logic;
		negate    : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		-- The formula for the final_output width should be the larger value of either (A_WIDTH+B_WIDTH+2) or (C_WIDTH+2).
		final_output : out signed ((FINAL_OUTPUT_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_plus36_full_regs_dynSub_dynNegate_10nm is

-- Multiplier Result
signal m1, m2: signed ((A_WIDTH+B_WIDTH-1) downto 0);
-- 18x19_plus36 Result
signal s1, s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg       : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg       : signed    ((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg       : signed    ((C_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline1_reg, c2_pipeline1_reg   : signed    ((C_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline2_reg, c2_pipeline2_reg   : signed    ((C_WIDTH-1) downto 0);

-- Sub Input Register
signal addnsub1_reg : std_logic;
signal addnsub2_reg : std_logic;

-- Sub Pipeline Register
signal addnsub1_pipeline1_reg: std_logic;
signal addnsub2_pipeline1_reg: std_logic;

-- Sub Second Pipeline Register
signal addnsub1_pipeline2_reg: std_logic;
signal addnsub2_pipeline2_reg: std_logic;

-- Negate Input Register
signal negate_reg : std_logic;

-- Negate Pipeline Register
signal negate_pipeline1_reg: std_logic;

-- Negate Second Pipeline Register
signal negate_pipeline2_reg: std_logic;

-- Output Register
signal s1_output_reg : signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s_reg : signed ((FINAL_OUTPUT_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_reg, c2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_pipeline1_reg, c2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_pipeline2_reg, c2_pipeline2_reg, addnsub1_reg, addnsub2_reg, addnsub1_pipeline1_reg, addnsub2_pipeline1_reg, addnsub1_pipeline2_reg, addnsub2_pipeline2_reg, negate_reg, negate_pipeline1_reg, negate_pipeline2_reg, s1_output_reg, s_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
  
	-- Input register (for DATA and DYNAMIC CONTROL SIGNAL) 
	-- All input registers must use the same reset signal, 
	-- Each DATA input register may hava different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. sub and negate) can have different ena signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal.     
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			addnsub1_reg <= '0';
			addnsub2_reg <= '0';
			negate_reg <= '0';
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				addnsub1_reg <= addnsub1; 
				addnsub2_reg <= addnsub2; 
				negate_reg <= negate;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline registers can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			c1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c2_pipeline1_reg <= (others => '0');
			addnsub1_pipeline1_reg <= '0';
			addnsub2_pipeline1_reg <= '0';
			negate_pipeline1_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				c1_pipeline1_reg <= c1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c2_pipeline1_reg <= c2_reg;
				addnsub1_pipeline1_reg <= addnsub1_reg;
				addnsub2_pipeline1_reg <= addnsub2_reg;
				negate_pipeline1_reg <= negate_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline registers can be bypassed differently
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			c1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c2_pipeline2_reg <= (others => '0');
			addnsub1_pipeline2_reg <= '0';
			addnsub2_pipeline2_reg <= '0';
			negate_pipeline2_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				c1_pipeline2_reg <= c1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c2_pipeline2_reg <= c2_pipeline1_reg;
				addnsub1_pipeline2_reg <= addnsub1_pipeline1_reg;
				addnsub2_pipeline2_reg <= addnsub2_pipeline1_reg;
				negate_pipeline2_reg <= negate_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register must share the same reset with the input pipeline and second pipeline register banks
	process(clock, aclr2)
	begin
		if (aclr2 = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				s1_output_reg <= s1;
                 
				-- Dynamic negate
				if (negate_pipeline2_reg = '1') then  
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) - resize(s2,FINAL_OUTPUT_WIDTH);
				else 
					s_reg <= resize(s1_output_reg,FINAL_OUTPUT_WIDTH) + resize(s2,FINAL_OUTPUT_WIDTH);
				end if;
			end if;
            
		end if;

	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
    
	-- First 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub1_pipeline2_reg select
	s1 <=  resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) - resize(m1, SUM_OUTPUT_WIDTH)  when '1',
			resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH) when others;
            
	-- Second 18x19_plus36
	-- Dynamic add/sub
	-- Addend must be the first operand 
	with addnsub2_pipeline2_reg select
	s2 <=  resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) - resize(m2, SUM_OUTPUT_WIDTH) when '1',
			resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m2, SUM_OUTPUT_WIDTH) when others;
    
	-- Final output 
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_plus36 with Input Cascade, Output Chaining, Accumulator, Double Accumulator and Preload Constant using ACLR
-- Quartus Prime VHDL Template
-- Two 18x19_plus36 with full registers (input, 2 pipeline stages and output) using asynchronous clear + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output[t] = acc_sel + ((a1[t-5]*b1[t-5])+c1[t-5]) + ((a2[t-4]*b2[t-4])+c2[t-4])
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- Note: Input cascade chain is not supported in 18x19_plus36 mode.
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_10nm is
	generic
	(
		-- This template will only work for (AxB)+C where the AxB data width range from 2x2 to 18x19, and the add add/sub data input (C) data width range from 2 to 36 
		A_WIDTH : natural := 18;
		B_WIDTH : natural := 19;
		C_WIDTH : natural := 36;
		-- The formula for the output width of one (AxB)+C should be either (A_WIDTH+B_WIDTH+1) or (C_WIDTH+1), whichever is larger. 
		-- SUM_OUTPUT_WIDTH = (A_WIDTH+B_WIDTH+1)
		SUM_OUTPUT_WIDTH : natural := (18+19+1);
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		c1       : in signed    ((C_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		c2       : in signed    ((C_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		aclr1     : in std_logic;
		aclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m18x19_plus36_full_regs_chainoutadder_acc_doubleacc_preloadConst_10nm is

-- Multiplier Result
signal m1, m2  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg      : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg      : signed    ((B_WIDTH-1) downto 0);
signal c1_reg, c2_reg      : signed    ((C_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg  : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg  : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline1_reg, c2_pipeline1_reg  : signed    ((C_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg  : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg  : signed    ((B_WIDTH-1) downto 0);
signal c1_pipeline2_reg, c2_pipeline2_reg  : signed    ((C_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg: signed ((SUM_OUTPUT_WIDTH-1) downto 0);
signal s2 : signed ((SUM_OUTPUT_WIDTH-1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, b1_reg, b2_reg, c1_reg, c2_reg, a1_pipeline1_reg, a2_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, c1_pipeline1_reg, c2_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, c1_pipeline2_reg, c2_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- Accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
						s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
						(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input must use the same reset signal, 
	-- Each DATA input register may hava different ena signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same ena signal. 
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			-- Input registers (for DATA)
			a1_reg <= (others => '0');
			b1_reg <= (others => '0');
			c1_reg <= (others => '0');
			a2_reg <= (others => '0');
			b2_reg <= (others => '0');
			c2_reg <= (others => '0');
			-- Input registers (for DYNAMIC CONTROL SIGNAL)
			loadconst_reg <= '0';
			accum_reg <= '0';
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a1_reg <= a1;
				b1_reg <= b1;
				c1_reg <= c1;
				a2_reg <= a2;
				b2_reg <= b2;
				c2_reg <= c2;
				loadconst_reg <= loadconst;
				accum_reg <= accum;
			end if;
            
		end if;

	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypass differently 
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			a1_pipeline1_reg <= (others => '0');
			b1_pipeline1_reg <= (others => '0');
			c1_pipeline1_reg <= (others => '0');
			a2_pipeline1_reg <= (others => '0');
			b2_pipeline1_reg <= (others => '0');
			c2_pipeline1_reg <= (others => '0');
			loadconst_pipeline1_reg <= '0';
			accum_pipeline1_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline1_reg <= a1_reg;
				b1_pipeline1_reg <= b1_reg;
				c1_pipeline1_reg <= c1_reg;
				a2_pipeline1_reg <= a2_reg;
				b2_pipeline1_reg <= b2_reg;
				c2_pipeline1_reg <= c2_reg;
				loadconst_pipeline1_reg <= loadconst_reg;
				accum_pipeline1_reg <= accum_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypass differently 
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			a1_pipeline2_reg <= (others => '0');
			b1_pipeline2_reg <= (others => '0');
			c1_pipeline2_reg <= (others => '0');
			a2_pipeline2_reg <= (others => '0');
			b2_pipeline2_reg <= (others => '0');
			c2_pipeline2_reg <= (others => '0');
			loadconst_pipeline2_reg <= '0';
			accum_pipeline2_reg <= '0';
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a1_pipeline2_reg <= a1_pipeline1_reg;
				b1_pipeline2_reg <= b1_pipeline1_reg;
				c1_pipeline2_reg <= c1_pipeline1_reg;
				a2_pipeline2_reg <= a2_pipeline1_reg;
				b2_pipeline2_reg <= b2_pipeline1_reg;
				c2_pipeline2_reg <= c2_pipeline1_reg;
				loadconst_pipeline2_reg <= loadconst_pipeline1_reg;
				accum_pipeline2_reg <= accum_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with input pipeline and second pipeline register banks
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			s1_output_reg <= (others => '0');
			s_reg <= (others => '0');
			s_double <= (others => '0');
            
		elsif rising_edge(clock) then

			if (ena3 = '1') then
				-- First 18x19_plus36. Support static add/sub. 
				-- Addend must be the first operand 
				s1_output_reg <= resize(c1_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m1, SUM_OUTPUT_WIDTH);
				-- Accumulate and chainout adder                          
				s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
				--- Double Accumulate
				s_double <= s_reg;
			end if;
            
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
    
	-- Second 18x19_plus36. Support static add/sub
	-- Addend must be the first operand
	s2 <= resize(c2_pipeline2_reg, SUM_OUTPUT_WIDTH) + resize(m2,SUM_OUTPUT_WIDTH);

	-- Final output
	final_output <= s_reg;

end rtl;
end_template
begin_template M18x19_full Single Multiplier with Preadder and Coefficent using ACLR
-- Quartus Prime VHDL Template
-- m18x19_full mode by utilizing half a DSP block resource
-- Single multiplier with full registers (input, 2 pipeline stages and output) using asynchronous clear + preadder + coefficients
-- Formula: final_output[t] = (a[t-4]+b[t-4])*c_coef[t-4]
--    Note: This mode does not support chainout adder nor dynamic ACCUMULATE/LOADCONST/SUB/NEGATE.
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m18x19_single_mult_full_regs_preadd_coef_10nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		AB_WIDTH    : natural := 18;
		COEF_WIDTH  : natural := 18;
		-- up to 8 coefficients (3-bit address)
		SEL_WIDTH   : natural := 3
	);

	port 
	(
		-- Data input ports
		a       : in signed    ((AB_WIDTH-1) downto 0);
		b       : in signed    ((AB_WIDTH-1) downto 0);
		-- Coefficient selection ports. Selection of up to 8 coefficients (3-bit address)
		c_sel   : in std_logic_vector ((SEL_WIDTH)-1 downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock  : in std_logic;
		ena1    : in std_logic;
		ena2    : in std_logic;
		ena3    : in std_logic;
		aclr1   : in std_logic;
		aclr2   : in std_logic;
		-- Output signal
		-- Max output width is 64
		final_output : out signed ((AB_WIDTH+COEF_WIDTH) downto 0)
	);

end entity;

architecture rtl of m18x19_single_mult_full_regs_preadd_coef_10nm is
-- This template uses integer type as the coeffecient constant
-- Can use other preferred type, for example signed/unsigned 

-- Array type define for coef or ROM purpose
TYPE coef_type IS ARRAY(0 to ((2**SEL_WIDTH)-1))  of std_logic_vector((COEF_WIDTH-1) DOWNTO 0);

-- Coefficient storage (ROM inferencing template)
SIGNAL c_coef : coef_type := 
			("110101111001110100",
			"001010100111101011",
			"001010111111101011",
			"101010111111101011",
			"001010000011010110",
			"111010110000001011",
			"001010111111010111",
			"001010111111101011");

-- Coefficient selection result
signal c_coef_wire    : signed ((COEF_WIDTH)-1 downto 0);

-- Preadder result
signal ab           : signed    ((AB_WIDTH) downto 0);

-- Data Input Register
signal a_reg       : signed    ((AB_WIDTH-1) downto 0);
signal b_reg       : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_reg   : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Pipeline Register
signal a_pipeline1_reg      : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline1_reg      : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline1_reg  : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Data Input Second Pipeline Register
signal a_pipeline2_reg      : signed    ((AB_WIDTH-1) downto 0);
signal b_pipeline2_reg      : signed    ((AB_WIDTH-1) downto 0);
signal c_sel_pipeline2_reg  : std_logic_vector ((SEL_WIDTH)-1 downto 0);

-- Output Register
signal final_output_reg : signed ((AB_WIDTH+COEF_WIDTH) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a_reg, b_reg, c_sel_reg, a_pipeline1_reg, b_pipeline1_reg, c_sel_pipeline1_reg, a_pipeline2_reg, b_pipeline2_reg, c_sel_pipeline2_reg, final_output_reg : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin
	-- Data Input register 
	-- DSP supports single clock, 3 ena and 2 reset signals
	-- When preadder is used, the inputs to the preadder must use the same clock enable signal
	-- The coefficient select input may use a different clock enable than that of the preadder inputs.
	-- All registered inputs must use the same reset
	process(clock, aclr1)
	begin
		if (aclr1 = '1') then
			a_reg <= (others => '0');
			b_reg <= (others => '0');
			c_sel_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena1 = '1') then
				a_reg <= a;
				b_reg <= b;
				c_sel_reg <= c_sel;
			end if;
            
		end if;
	end process;
    
	-- Input pipeline register
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			a_pipeline1_reg <= (others => '0');
			b_pipeline1_reg <= (others => '0');
			c_sel_pipeline1_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a_pipeline1_reg <= a_reg;
				b_pipeline1_reg <= b_reg;
				c_sel_pipeline1_reg <= c_sel_reg;
			end if;
            
		end if;
	end process;
    
	-- Second pipeline register
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			a_pipeline2_reg <= (others => '0');
			b_pipeline2_reg <= (others => '0');
			c_sel_pipeline2_reg <= (others => '0');
		elsif rising_edge(clock) then

			if (ena2 = '1') then
				a_pipeline2_reg <= a_pipeline1_reg;
				b_pipeline2_reg <= b_pipeline1_reg;
				c_sel_pipeline2_reg <= c_sel_pipeline1_reg;
			end if;
            
		end if;
	end process;
    
	-- Output register
	-- THe output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock, aclr2 )
	begin
		if (aclr2  = '1') then
			final_output_reg <= (others => '0');
            
		elsif rising_edge(clock) then

			if (ena3 = '1') then
			-- Static add/sub is supported
				final_output_reg <= resize(c_coef_wire,COEF_WIDTH) * resize(ab,AB_WIDTH+1);
			end if;
            
		end if;
	end process;
    
	-- Preadder
	-- Preadder supports static add/sub
	ab <= resize(a_pipeline2_reg, AB_WIDTH+1) + resize(b_pipeline2_reg, AB_WIDTH+1);
    
	-- Coefficients
	c_coef_wire <= signed(c_coef(to_integer(unsigned(c_sel_pipeline2_reg))));

	-- Final output
final_output <= final_output_reg;

end rtl;
end_template
begin_template M9x9_sumof4 with Output Chaining, Accumulator, Double Accumulator and Preload Constant using SCLR
-- Quartus Prime VHDL Template
-- Two 'sum of four 9x9 multipliers' with full registers (input, 2 pipeline stages and output) using synchronous clear + chainout adder + accumulate + double accumulate + preload constant
-- Formula: final_output = acc_sel + (a1[t-5]*b1[t-5]) + (a2[t-5]*b2[t-5]) + (a3[t-5]*b3[t-5]) + (a4[t-5]*b4[t-5]) + (a5[t-4]*b5[t-4]) + (a6[t-4]*b6[t-4]) + (a7[t-4]*b7[t-4]) + (a8[t-4]*b8[t-4])
--          where acc_sel = final_output[t-2] or preload_value or 0 depending on dynamic control inputs "accum" and "loadconst"
-- For use with 10-nm device families
-- All registers support asynchronous or synchronous clear but all registers within the same design must use the same clear type
-- When synchronous clear is used, the ena signal has a higher priority than the clear signal
-- Synchronizer register identification is disabled in all registers used in this template to ensure the DSP block's internal registers are fully packed for maximum DSP performance (fMAX). Ensure proper timing constraints are done if this template is used in asynchronous clock domains to avoid potential metastability issue. For more information on managing metastability in Quartus, refer to https://www.altera.co.jp/ja_JP/pdfs/literature/hb/qts/qts_qii51018.pdf

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity m9x9_sum_of_4_full_regs_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm is
	generic
	(
		-- This template will only work within the AxB data width range from 2x2 to 18x19.
		A_WIDTH : natural := 9;
		B_WIDTH : natural := 9;
		-- PRELOAD_VALUE can be 2 power of N, which N should less than 64
		-- PRELOAD_VALUE should contain only one bit 1
		PRELOAD_VALUE : std_logic_vector := X"400";
		-- Maximum chain width is 64
		CHAIN_WIDTH : natural := 64;
        
		-- Double accumulation enable control parameter
		enable_double_accum : boolean := TRUE
	);

	port 
	(
	-- Data input ports
		a1       : in signed    ((A_WIDTH-1) downto 0);
		a2       : in signed    ((A_WIDTH-1) downto 0);
		a3       : in signed    ((A_WIDTH-1) downto 0);
		a4       : in signed    ((A_WIDTH-1) downto 0);
		a5       : in signed    ((A_WIDTH-1) downto 0);
		a6       : in signed    ((A_WIDTH-1) downto 0);
		a7       : in signed    ((A_WIDTH-1) downto 0);
		a8       : in signed    ((A_WIDTH-1) downto 0);
		b1       : in signed    ((B_WIDTH-1) downto 0);
		b2       : in signed    ((B_WIDTH-1) downto 0);
		b3       : in signed    ((B_WIDTH-1) downto 0);
		b4       : in signed    ((B_WIDTH-1) downto 0);
		b5       : in signed    ((B_WIDTH-1) downto 0);
		b6       : in signed    ((B_WIDTH-1) downto 0);
		b7       : in signed    ((B_WIDTH-1) downto 0);
		b8       : in signed    ((B_WIDTH-1) downto 0);
		-- Register clock and control signals
		-- DSP supports single clock, 3 ena, and 2 reset signals
		clock    : in std_logic;
		ena1      : in std_logic;
		ena2      : in std_logic;
		ena3      : in std_logic;
		sclr1     : in std_logic;
		sclr2     : in std_logic;
		-- Dynamic ACCUMULATE and LOADCONST control signals
		accum     : in std_logic;
		loadconst : in std_logic;
		-- Output signal
		-- Max output width for chaining is 64
		final_output : out signed ((CHAIN_WIDTH-1) downto 0)
	);

end entity;

architecture rtl of m9x9_sum_of_4_full_regs_chainoutadder_acc_doubleacc_preloadConst_sclr_10nm is

-- Multiplier Result
signal m1, m2, m3, m4, m5, m6, m7, m8  : signed ((A_WIDTH+B_WIDTH-1) downto 0);

-- Data Input Register
signal a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, a6_reg, a7_reg, a8_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, b6_reg, b7_reg, b8_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Pipeline Register
signal a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg, a6_pipeline1_reg, a7_pipeline1_reg, a8_pipeline1_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg, b6_pipeline1_reg, b7_pipeline1_reg, b8_pipeline1_reg   : signed    ((B_WIDTH-1) downto 0);

-- Data Input Second Pipeline Register
signal a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg, a6_pipeline2_reg, a7_pipeline2_reg, a8_pipeline2_reg   : signed    ((A_WIDTH-1) downto 0);
signal b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg, b6_pipeline2_reg, b7_pipeline2_reg, b8_pipeline2_reg   : signed    ((B_WIDTH-1) downto 0);

-- LOADCONST Input Register
signal loadconst_reg : std_logic;

-- LOADCONST Pipeline Register
signal loadconst_pipeline1_reg : std_logic;

-- LOADCONST Second Pipeline Register
signal loadconst_pipeline2_reg : std_logic;

-- ACCUMULATE Input Register
signal accum_reg : std_logic;

-- ACCUMULATE Pipeline Register
signal accum_pipeline1_reg : std_logic;

-- ACCUMULATE Second Pipeline Register
signal accum_pipeline2_reg : std_logic;

-- Summation Result and Output Register
signal s1_output_reg : signed ((A_WIDTH+B_WIDTH+1) downto 0);
signal s2 : signed ((A_WIDTH+B_WIDTH+1) downto 0);

-- Accumulate, double acc
signal acc_sel, s_reg, s_double: signed ((CHAIN_WIDTH-1) downto 0);
signal selected_value, select_feedback: signed    ((CHAIN_WIDTH-1) downto 0);

--Disable synchronizer register identification
attribute altera_attribute : string;
attribute altera_attribute of a1_reg, a2_reg, a3_reg, a4_reg, a5_reg, a6_reg, a7_reg, a8_reg, b1_reg, b2_reg, b3_reg, b4_reg, b5_reg, b6_reg, b7_reg, b8_reg, a1_pipeline1_reg, a2_pipeline1_reg, a3_pipeline1_reg, a4_pipeline1_reg, a5_pipeline1_reg, a6_pipeline1_reg, a7_pipeline1_reg, a8_pipeline1_reg, b1_pipeline1_reg, b2_pipeline1_reg, b3_pipeline1_reg, b4_pipeline1_reg, b5_pipeline1_reg, b6_pipeline1_reg, b7_pipeline1_reg, b8_pipeline1_reg, a1_pipeline2_reg, a2_pipeline2_reg, a3_pipeline2_reg, a4_pipeline2_reg, a5_pipeline2_reg, a6_pipeline2_reg, a7_pipeline2_reg, a8_pipeline2_reg, b1_pipeline2_reg, b2_pipeline2_reg, b3_pipeline2_reg, b4_pipeline2_reg, b5_pipeline2_reg, b6_pipeline2_reg, b7_pipeline2_reg, b8_pipeline2_reg, loadconst_reg, loadconst_pipeline1_reg, loadconst_pipeline2_reg, accum_reg, accum_pipeline1_reg, accum_pipeline2_reg, s1_output_reg, s_reg, s_double : signal is "-name SYNCHRONIZER_IDENTIFICATION OFF";

begin

	-- accumulator path
	with accum_pipeline2_reg select
	acc_sel <= select_feedback when '1',
				selected_value when others;

	with enable_double_accum select
	select_feedback <= s_double when TRUE,
								s_reg when others;
                    
	with loadconst_pipeline2_reg select
	selected_value <= resize(signed(preload_value), CHAIN_WIDTH) when '1',
					(others => '0') when others;
  
	-- Input registers (for DATA and DYNAMIC CONTROL SIGNAL), and delay registers 
	-- All input and delay registers must use the same reset signal. 
	-- Each DATA input and delay register may have different clock enable signal.
	-- The DYNAMIC CONTROL SIGNAL input registers(e.g. loadconst and accumulate) can have different clock enable signal than that of the DATA input register.
	-- But all DYNAMIC CONTROL SIGNAL input registers must share the same clock enable signal. 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena1 = '1') then
				if (sclr1 = '1') then
					-- Input registers (for DATA)
					a1_reg <= (others => '0');
					b1_reg <= (others => '0');
					a2_reg <= (others => '0');
					b2_reg <= (others => '0');
					a3_reg <= (others => '0');
					b3_reg <= (others => '0');
					a4_reg <= (others => '0');
					b4_reg <= (others => '0');
					a5_reg <= (others => '0');
					b5_reg <= (others => '0');
					a6_reg <= (others => '0');
					b6_reg <= (others => '0');
					a7_reg <= (others => '0');
					b7_reg <= (others => '0');
					a8_reg <= (others => '0');
					b8_reg <= (others => '0');
					-- Input registers (for DYNAMIC CONTROL SIGNAL)
					loadconst_reg <= '0';
					accum_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Input pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All input pipeline registers must use the same {ena, reset}
	-- The input pipeline register bank must use the same reset as the second pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL input pipeline register can be bypassed differently 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline1_reg <= (others => '0');
					b1_pipeline1_reg <= (others => '0');
					a2_pipeline1_reg <= (others => '0');
					b2_pipeline1_reg <= (others => '0');
					a3_pipeline1_reg <= (others => '0');
					b3_pipeline1_reg <= (others => '0');
					a4_pipeline1_reg <= (others => '0');
					b4_pipeline1_reg <= (others => '0');
					a5_pipeline1_reg <= (others => '0');
					b5_pipeline1_reg <= (others => '0');
					a6_pipeline1_reg <= (others => '0');
					b6_pipeline1_reg <= (others => '0');
					a7_pipeline1_reg <= (others => '0');
					b7_pipeline1_reg <= (others => '0');
					a8_pipeline1_reg <= (others => '0');
					b8_pipeline1_reg <= (others => '0');                   
					loadconst_pipeline1_reg <= '0';
					accum_pipeline1_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Second pipeline register (for DATA and DYNAMIC CONTROL SIGNAL)
	-- All second pipeline registers must use the same {ena, reset}
	-- The second pipeline register bank must use the same reset as the input pipeline and output register banks
	-- The DYNAMIC CONTROL SIGNAL second pipeline register can be bypassed differently 
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena2 = '1') then
				if (sclr2 = '1') then
					a1_pipeline2_reg <= (others => '0');
					b1_pipeline2_reg <= (others => '0');
					a2_pipeline2_reg <= (others => '0');
					b2_pipeline2_reg <= (others => '0');
					a3_pipeline2_reg <= (others => '0');
					b3_pipeline2_reg <= (others => '0');
					a4_pipeline2_reg <= (others => '0');
					b4_pipeline2_reg <= (others => '0');
					a5_pipeline2_reg <= (others => '0');
					b5_pipeline2_reg <= (others => '0');
					a6_pipeline2_reg <= (others => '0');
					b6_pipeline2_reg <= (others => '0');
					a7_pipeline2_reg <= (others => '0');
					b7_pipeline2_reg <= (others => '0');
					a8_pipeline2_reg <= (others => '0');
					b8_pipeline2_reg <= (others => '0');                    
					loadconst_pipeline2_reg <= '0';
					accum_pipeline2_reg <= '0';
				else
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
				end if;
			end if;
		end if;
	end process;
    
	-- Output register
	-- The output register bank must share the same reset with the input pipeline and second pipeline register banks
	process(clock)
	begin
		if rising_edge(clock) then
			if (ena3 = '1') then
				if (sclr2 = '1') then
					s1_output_reg <= (others => '0');
					s_reg <= (others => '0');
					s_double <= (others => '0');
				else
					-- Sum of 4 multiplier. Support static add/sub 
					s1_output_reg <= (resize(m1, A_WIDTH+B_WIDTH+2) + resize(m2, A_WIDTH+B_WIDTH+2) + resize(m3, A_WIDTH+B_WIDTH+2) + resize(m4, A_WIDTH+B_WIDTH+2));
					-- Accumulate and chainout adder
					s_reg <= acc_sel + (resize(s1_output_reg,CHAIN_WIDTH) + resize(s2,CHAIN_WIDTH));
					-- Double Accumulate
					s_double <= s_reg;
				end if;
			end if;
		end if;
	end process;
    
	-- Multiplier
	m1 <= (a1_pipeline2_reg * b1_pipeline2_reg);
	m2 <= (a2_pipeline2_reg * b2_pipeline2_reg);
	m3 <= (a3_pipeline2_reg * b3_pipeline2_reg);
	m4 <= (a4_pipeline2_reg * b4_pipeline2_reg);
	m5 <= (a5_pipeline2_reg * b5_pipeline2_reg);
	m6 <= (a6_pipeline2_reg * b6_pipeline2_reg);
	m7 <= (a7_pipeline2_reg * b7_pipeline2_reg);
	m8 <= (a8_pipeline2_reg * b8_pipeline2_reg);    
    
	-- Sum of 2 multiplier. Support static add/sub
	s2 <= (resize(m5, A_WIDTH+B_WIDTH+2) + resize(m6, A_WIDTH+B_WIDTH+2) + resize(m7, A_WIDTH+B_WIDTH+2) + resize(m8, A_WIDTH+B_WIDTH+2));

	-- Final output
final_output <= s_reg;

end rtl;
end_template
end_group
end_group
end_group
begin_group Configurations
begin_group Configuration Declarations
begin_template Configurable Gate Architecture
-- Quartus Prime VHDL Template
-- Configurable gate architecture

library ieee;
use ieee.std_logic_1164.all;
entity configurable_gate is
	port 
	(
		i1 : in std_logic;
		i2 : in std_logic;
		o1 : out std_logic
	);
end configurable_gate;


-- Three possible architectures
architecture and_gate of configurable_gate is
begin
	o1 <= i1 AND i2;
end and_gate;

architecture or_gate of configurable_gate is
begin
	o1 <= i1 OR i2;
end or_gate;

architecture xor_gate of configurable_gate is
begin
	o1 <= i1 XOR i2;
end xor_gate;


-- A block configuration is used to choose between the architectures.
configuration cfg of configurable_gate is  -- Configuration Declaration
	for or_gate                              	     -- Block Configuration
	end for;
end cfg;
end_template
begin_template Configurable Component Gates
-- Quartus Prime VHDL Template
-- Configurable component gates

entity configurable_component_gates1 is
	port
	(
		i1, i2 : in bit;
		o1, o2 : out bit
	);
end configurable_component_gates1;

architecture rtl of configurable_component_gates1 is
	component cgate is
	port 
	(
		i1, i2 : in bit;
		o      : out bit
	);
	end component;
begin
	-- Each instance can be mapped to a different 
	-- entity and/or architecture
	inst1 : cgate
	port map
	(
		i1 => i1,
		i2 => i2,
		o  => o1
	);

	inst2 : cgate
	port map
	(
		i1 => i1,
		i2 => i2,
		o  => o2
	);
end rtl;



-- An entity that corresponds to the above component
entity configurable_gate is
	port 
	(
		i1, i2 : in bit;
		o      : out bit
	);
end configurable_gate;


-- Three possible architectures
architecture and_gate of configurable_gate is
begin
	o <= i1 AND i2;
end and_gate;

architecture or_gate of configurable_gate is
begin
	o <= i1 OR i2;
end or_gate;

architecture xor_gate of configurable_gate is
begin
	o <= i1 XOR i2;
end xor_gate;



-- This component configuration matches different component instances
-- with different architectures of one entity.
configuration cfg of configurable_component_gates1 is
	for rtl
		for inst1 : cgate use entity work.configurable_gate(and_gate);
		end for;
		for inst2 : cgate use entity work.configurable_gate(xor_gate);
		end for;
	end for;
end cfg;
end_template
begin_template Configurable Component Ports
-- Quartus Prime VHDL Template
-- Configurable names for ports of a binary counter

library ieee;
use ieee.std_logic_1164.all;
entity configurable_counter_ports1 is
	port
	(
		i1 : in std_logic;
		i2 : in std_logic;
		i3 : in std_logic;
		o  : out integer
	);
end configurable_counter_ports1;

architecture rtl of configurable_counter_ports1 is
	component c     -- A very generic component!  
	port 
	(
		i1,i2,i3 : in std_logic;    -- These port names won't match the ports
		o        : out integer      -- of the instantiated entity.
	);
	end component;
begin

	inst1 : c
	port map 
	(
		i1 => i1,
		i2 => i2,
		i3 => i3,
		o  => o
	);

end rtl;


-- Component configurations bind component ports correctly
configuration cfg of configurable_counter_ports1 is
	for rtl
		-- Specify that the component will be a counter
		for inst1 : c use entity work.binary_counter(rtl)
		-- Specify how the counter's port names correspond
		-- to the component's port names
		port map (                  
			clk => i1,
			reset => i2,
			enable => i3,
			q => o
		);
		end for;
	end for;
end cfg;



-- The binary counter:

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity binary_counter is
	generic
	(
		MIN_COUNT : natural := 0;
		MAX_COUNT : natural := integer'high
	);
	port
	(
		clk		  : in std_logic;
		reset	      : in std_logic;
		enable	  : in std_logic;
		q		  : out integer range MIN_COUNT to MAX_COUNT
	);
end entity;

architecture rtl of binary_counter is
begin
	process (clk)
		variable  cnt : integer range MIN_COUNT to MAX_COUNT;
	begin
		if (rising_edge(clk)) then
			if reset = '1' then
				cnt := 0;
			elsif enable = '1' then
				cnt := cnt + 1;
			end if;
		end if;

		q <= cnt;
	end process;
end rtl;
end_template
end_group
begin_group Configuration Specifications
begin_template Configurable Component Gates
-- Quartus Prime VHDL Template
-- Configurable component gates

entity configurable_component_gates2 is
	port
	(
		i1, i2 : in bit;
		o1, o2 : out bit
	);
end configurable_component_gates2;

architecture rtl of configurable_component_gates2 is
	component cgate is
	port 
	(
		i1, i2 : in bit;
		o      : out bit
	);
	end component;

	-- Bind component instances to entity/architecture pairs.
	-- In this case, both instances are bound to the same entity, but
	-- different architectures.
	for inst1 : cgate use entity work.configurable_gate(and_gate);
	for inst2 : cgate use entity work.configurable_gate(xor_gate);

begin

	inst1 : cgate
	port map
	(
		i1 => i1,
		i2 => i2,
		o  => o1
	);

	inst2 : cgate
	port map
	(
		i1 => i1,
		i2 => i2,
		o  => o2
	);

end rtl;



entity configurable_gate is
	port 
	(
		i1, i2 : in bit;
		o      : out bit
	);
end configurable_gate;


-- Three possible architectures
architecture and_gate of configurable_gate is
begin
	o <= i1 and i2;
end and_gate;

architecture or_gate of configurable_gate is
begin
	o <= i1 or i2;
end or_gate;

architecture xor_gate of configurable_gate is
begin
	o <= i1 xor i2;
end xor_gate;
end_template
begin_template Configurable Component Ports
-- Quartus Prime VHDL Templates
-- Configurable port names for a binary counter

library ieee;
use ieee.std_logic_1164.all;
entity configurable_counter_ports2 is
	port
	(
		i1 : in std_logic;
		i2 : in std_logic;
		i3 : in std_logic;
		o  : out integer
	);
end configurable_counter_ports2;

architecture rtl of configurable_counter_ports2 is
	component c     -- A very generic component!
	port (
		i1,i2,i3 : in std_logic;    -- These port names won't match the ports
		o        : out integer      -- of the instantiated entity.
	);
	end component;

	-- Bind the generic component to a specific entity (a counter)
	for inst1 : c use entity work.binary_counter(rtl)
	port map (
		clk => i1,      -- Show how the counter's port names correspond
		reset => i2,    -- to the component's port names
		enable => i3,
		q => o
	);
begin
	inst1 : c
	port map (
		i1 => i1,
		i2 => i2,
		i3 => i3,
		o  => o
	);
end rtl;



-- The binary counter:

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity binary_counter is
	generic
	(
		MIN_COUNT : natural := 0;
		MAX_COUNT : natural := integer'high
	);
	port
	(
		clk		  : in std_logic;
		reset	      : in std_logic;
		enable	  : in std_logic;
		q		  : out integer range MIN_COUNT to MAX_COUNT
	);
end entity;

architecture rtl of binary_counter is
begin
	process (clk)
		variable  cnt : integer range MIN_COUNT to MAX_COUNT;
	begin
		if (rising_edge(clk)) then
			if reset = '1' then
				cnt := 0;
			elsif enable = '1' then
				cnt := cnt + 1;
			end if;
		end if;

		q <= cnt;
	end process;
end rtl;
end_template
end_group
end_group
begin_group Pipelining
begin_template Hyper-Pipelining Module
-- Quartus Prime VHDL Template
--
-- Hyper-Pipelining Module

library IEEE;
use IEEE.std_logic_1164.all;
library altera;
use altera.altera_syn_attributes.all;

entity hyperpipe is
	generic (
 		CYCLES : integer := 1;
 		WIDTH : integer := 1
 	);
 	port (
 		clk : in std_logic;
 		din : in std_logic_vector (WIDTH - 1 downto 0);
 		dout : out std_logic_vector (WIDTH - 1 downto 0)
 	);
end entity;

architecture arch of hyperpipe is
 
	type hyperpipe_t is array(CYCLES-1 downto 0) of
 		std_logic_vector(WIDTH-1 downto 0);
	signal HR : hyperpipe_t;
	
	-- Prevent large hyperpipes from going into memory-based altshift_taps,
	-- since that won't take advantage of Hyper-Registers
 	attribute altera_attribute of HR :
 		signal is "-name AUTO_SHIFT_REGISTER_RECOGNITION off";
 
begin
 	wire : if CYCLES = 0 GENERATE
 		-- The 0 bit is just a pass-thru, when CYCLES is set to 0
 		dout <= din;
 	end generate wire;
 
 	hp : if CYCLES > 0 GENERATE
 		process (clk) begin
 			if (clk'event and clk = '1')then
 				HR <= HR(HR'high-1 downto 0) & din;
 			end if;
 		end process;
 		dout <= HR(HR'high);
 	end generate hp;

end arch;
end_template
begin_template Hyper-Pipelining Module Instantiation
-- Quartus Prime VHDL Template
--
-- Hyper-Pipelining Module Instantiation

-- Template Declaration
component hyperpipe
	generic (
		CYCLES : integer;
		WIDTH : integer
 	);
 	port (
 		clk : in std_logic;
 		din : in std_logic_vector(WIDTH - 1 downto 0);
 		dout : out std_logic_vector(WIDTH - 1 downto 0)
 	);
end component;

-- Instantiation Template:
	hp : hyperpipe
 		generic map (
 			CYCLES => ,
 			WIDTH => 
 		)
 		port map (
 			clk => ,
 			din => ,
 			dout =>
 		);
end_template
begin_template Hyper-Pipelining Variable Latency Module
-- Quartus Prime VHDL Template
--
-- Hyper-Pipelining Variable Latency Module

library ieee;
use ieee.std_logic_1164.all;

entity hyperpipe_vlat is

	generic
	(
		WIDTH     : integer := 1;
		MAX_PIPE  : integer := 100 -- Valid range: 0 to 100 inclusive
	);

	port 
	(
		clk    : in  std_logic;
		din    : in  std_logic_vector(WIDTH-1 downto 0);
		dout   : out std_logic_vector(WIDTH-1 downto 0)
	);

end entity;

architecture rtl of hyperpipe_vlat is

	signal vlat_r: std_logic_vector(WIDTH-1 downto 0);

	attribute preserve : boolean;
	attribute preserve of vlat_r: signal is true;

	attribute altera_attribute : string;
	-- Capping the value of MAX_PIPE to 100 because MAX_PIPE > 100 could cause errors
	constant MAX_PIPE_CAPPED: integer range 0 to 100 := MAX_PIPE;

	-- Converting MAX_PIPE_CAPPED to string so it can be used as a string when setting altera_attribute
	attribute altera_attribute of vlat_r: signal is "-name ADV_NETLIST_OPT_ALLOWED NEVER_ALLOW; -name HYPER_RETIMER_ADD_PIPELINING " & INTEGER'IMAGE(MAX_PIPE_CAPPED);
    
begin

	process (clk)
	begin
		if (rising_edge(clk)) then
			vlat_r <= din;
		end if;
	end process;

	dout <= vlat_r;
    
end rtl;
end_template
begin_template Hyper-Pipelining Variable Latency Instantiation
-- Quartus Prime VHDL Template
--
-- Hyper-Pipelining Variable Latency Module

-- Template Declaration
component hyperpipe_vlat
	generic (
		WIDTH      : integer;
		MAX_PIPE   : integer := 100 -- Valid range: 0 to 100 inclusive
 	);
 	port (
 		clk        : in std_logic;
 		din        : in std_logic_vector(WIDTH - 1 downto 0);
 		dout       : out std_logic_vector(WIDTH - 1 downto 0)
 	);
end component;

-- Instantiation Template:
	hp : hyperpipe_vlat
 		generic map (
 			WIDTH => ,
 			MAX_PIPE => 
 		)
 		port map (
 			clk => ,
 			din => ,
 			dout =>
 		);
end_template
end_group
end_group
begin_group Constructs
begin_group Design Units
begin_template Library Clause
-- A library clause declares a name as a library.  It 
-- does not create the library; it simply forward declares 
-- it. 
library <library_name>;
end_template
begin_template Use Clause
-- Use clauses import declarations into the current scope.	
-- If more than one use clause imports the same name into the
-- the same scope, none of the names are imported.

-- Import all the declarations in a package
use <library_name>.<package_name>.all;

-- Import a specific declaration from a package
use <library_name>.<package_name>.<object_name>;

-- Import a specific entity from a library
use <library_name>.<entity_name>;

-- Import from the work library.  The work library is an alias
-- for the library containing the current design unit.
use work.<package_name>.all;


-- Commonly imported packages:

	-- STD_LOGIC and STD_LOGIC_VECTOR types, and relevant functions
	use ieee.std_logic_1164.all;

	-- SIGNED and UNSIGNED types, and relevant functions
	use ieee.numeric_std.all;

	-- Basic sequential functions and concurrent procedures
	use ieee.VITAL_Primitives.all;

	-- Library of Parameterized Modules: 
	-- customizable, device-independent logic functions
	use lpm.lpm_components.all;

	-- Altera Megafunctions
	use altera_mf.altera_mf_components.all;
end_template
begin_template Entity
entity <entity_name> is
	generic
	(
		<name>	: <type>  :=	<default_value>;
		...
		<name>	: <type>  :=	<default_value>
	);


	port
	(
		-- Input ports
		<name>	: in  <type>;
		<name>	: in  <type> := <default_value>;

		-- Inout ports
		<name>	: inout <type>;

		-- Output ports
		<name>	: out <type>;
		<name>	: out <type> := <default_value>
	);
end <entity_name>;
end_template
begin_template Architecture

-- Library Clause(s) (optional)
-- Use Clause(s) (optional)

architecture <arch_name> of <entity_name> is

	-- Declarations (optional)

begin

	-- Process Statement (optional)

	-- Concurrent Procedure Call (optional)

	-- Concurrent Signal Assignment (optional)

	-- Conditional Signal Assignment (optional)

	-- Selected Signal Assignment (optional)

	-- Component Instantiation Statement (optional)

	-- Generate Statement (optional)

end <arch_name>;
end_template
begin_template Package

-- Library Clause(s) (optional)
-- Use Clause(s) (optional)

package <package_name> is

	-- Type Declaration (optional)

	-- Subtype Declaration (optional)

	-- Constant Declaration (optional)

	-- Signal Declaration (optional)

	-- Component Declaration (optional)

end <package_name>;
end_template
begin_template Package Body

package body <package_name> is

	-- Type Declaration (optional)

	-- Subtype Declaration (optional)

	-- Constant Declaration (optional)

	-- Function Declaration (optional)

	-- Function Body (optional)

	-- Procedure Declaration (optional)

	-- Procedure Body (optional)

end <package_name>;
end_template
begin_template Configuration Declaration
-- A configuration can control how the various parts of a design fit
-- together.  It can specify which architecture is used with a given
-- entity.  It can select the entity (and architecture) corresponding
-- to a component instantiation.   It can control how the ports and 
-- generics of a component map to the ports and generics of the entity 
-- it instantiates.

configuration <configuration_name> of <entity_name> is
	for <architecture_name>
	
		-- Use Clause (optional)

		-- Block Configuration or Component Configuration (optional)

	end for;
end <configuration_name>;


-- Note: A configuration declaration is used to configure one or more 
-- instances of an entity.  Quartus Prime must be able to determine which
-- instance(s) to configure, or it will ignore the configuration declaration.
-- Quartus Prime is able to determine which instances to configure in the
-- following cases:
-- 1. The configuration declaration pertains to the top-level entity.
-- 2. The configuration declaration is named in a component configuration
--    that is inside another, higher-level configuration declaration.
-- 3. The configuration declaration is named in a configuration specification.
end_template
begin_template Block Configuration
-- Block configurations go inside configuration declarations.  See the 
-- full-design configuration templates for examples using block configurations.

for <architecture_name, block_label, _or_ generate_label>

	-- Use Clause (optional)

	-- Block Configuration or Component Configuration (optional)

end for;
end_template
begin_template Component Configuration
-- Component configurations go inside configuration declarations.  See the 
-- full-design configuration templates for examples using component configurations.

for <instance_name>:<component_name>

	-- Optionally specify either an entity or configuration (not both).
	-- Only use the semicolon if there is no port/generic binding to follow.
	use entity <library_name>.<entity_name>(<optional_architecture_name>);
	use configuration <library_name>.<configuration_name>;
	
	-- Optionally specify port and generic bindings.
	-- Use these if the names of the ports/generics of the component
	-- don't match the names of the corresponding ports/generics of the
	-- entity being instantiated.
	generic map
	(
		<instantiated_entity_generic_name> => <component_generic_name>,
		...
	)
	port map 
	(
		<instantiated_entity_input_name> => <component_input_name>,
		<instantiated_entity_output_name> => <component_output_name>,
		<instantiated_entity_inout_name> => <component_inout_name>,
		...
	);

	-- Block Configuration (optional)

end for;
end_template
end_group
begin_group Declarations
begin_group Type and Subtype Declarations
begin_template Integer Type Declaration
-- Basic integer type declaration
type <name> is range <low> to <high>;

-- Examples
type index_t is range 0 to 7;
type addr_t is range 255 downto 0;
end_template
begin_template Array Type Declaration
-- Basic 1-D array type declaration
type <name> is array(<range_expr>) of <subtype_indication>;

-- Specify a multidimensional array in a single declaration
type <name> is array(<range_expr>,..) of <subtype_indication>;

-- Examples

-- Declare array types with fixed ranges.
type byte_t is array(7 downto 0) of std_logic;
type mem_t	is array(7 downto 0) of std_logic_vector(7 downto 0);

-- Declare an array type with an unconstrained range.  When
-- you declare an object of this type, you can specify the
-- range constraint in the subtype indication.
type vector_t is array(natural range <>) of std_logic;
end_template
begin_template Enum Type Declaration
type <name> is (<enum_literal>, <enum_literal>, ...);

-- Example
type state_t is (IDLE, READING, WRITING, DONE);
end_template
begin_template Record Type Declaration
type <name> is 
	record 
		<member_ids> : <subtype_indication>;
		...
	end record;

-- Example
type packet_t is 
	record
		address : integer range 0 to 256;
		data	   : std_logic_vector(7 downto 0);
	end record;
end_template
end_group
begin_template Signal Declaration
-- Signal with no default value.  Your design should assign an explicit
-- value to such a signal using an assignment statement.  You assign
-- to a signal with the "<=" operator. 

signal <name> : <type>;

-- Signal with a default value.	 If you do not assign a value to the
-- signal with an assignment, Quartus Prime Integrated Synthesis will 
-- initialize it with the default value.  Integrated Synthesis also
-- derives power-up conditions for memories and registers from the
-- default value.

signal <name> : <type> := <default_value>;

-- Commonly declared signals

signal <name> : std_logic;
signal <name> : std_logic_vector(<msb_index> downto <lsb_index>);
signal <name> : integer;
signal <name> : integer range <low> to <high>;
end_template
begin_template Variable Declaration
-- Variables should be declared in a process statement or subprogram.
-- They are useful for storing intermediate calculations.  You assign
-- to a variable with the ":=" operator.

-- Variable with no default value.	Your design should assign an 
-- explicit value to this variable before referring to it in a 
-- statement or an expression

variable <name> : <type>;

-- Variable with a default value.

variable <name> : <type> := <default_value>;

-- Commonly declared variables

variable <name> : std_logic;
variable <name> : std_logic_vector(<msb_index> downto <lsb_index>);
variable <name> : integer;
variable <name> : integer range <low> to <high>;
end_template
begin_template Constant Declaration
constant <constant_name> : <type> := <constant_value>;
end_template
begin_template Component Declaration
-- A component declaration declares the interface of an entity or
-- a design unit written in another language.  VHDL requires that
-- you declare a component if you do not intend to instantiate
-- an entity directly.	The component need not declare all the
-- generics and ports in the entity.  It may omit generics/ports
-- with default values.

component <component_name>

	generic
	(
		<name> : <type>;
		<name> : <type> := <default_value>
	);

	port
	(
		-- Input ports
		<name>	: in  <type>;
		<name>	: in  <type> := <default_value>;

		-- Inout ports
		<name>	: inout <type>;

		-- Output ports
		<name>	: out <type>;
		<name>	: out <type> := <default_value>
	);

end component;
end_template
begin_template Subprogram Declaration
-- Procedure Declaration
procedure <name>(<formal_parameter_decls>);

-- Function Declaration
function <name>(<formal_parameter_decls>) return <type_mark>;
end_template
begin_template Subprogram Body
<subprogram_decl> is
	-- Declaration(s)
begin
	-- Statement(s)
end;
end_template
begin_template Attribute Declaration
-- Attributes allow you to associate additional properties with another
-- object in your design.  Quartus Prime Integrated Synthesis supports
-- attributes as a way for you to control the synthesis of your design.
-- In general, you should import attributes from the altera_syn_attributes
-- package in the altera library with the use clause:
--	   use altera.syn_altera_attributes.all;

attribute <name> : <type>;
end_template
begin_template Configuration Specification
-- A configuration specification is a way of configuring a component instance.
-- The configuration specification can select the entity (and architecture) 
-- corresponding to a component instantiation.  It can also specify a
-- configuration declaration used to configure the component instance.

-- Specify the entity being instantiated
for <instance_name> : <component_name> use entity <library_name>.<entity_name>;

-- Specify the entity and architecture being instantiated 
for <instance_name> : <component_name> use entity <library_name>.<entity_name>(<architecture_name>);

-- Specify a configuration to configure this instance of the component
for <instance_name> : <component_name> use configuration <library_name>.<configuration_name>;


-- Examples

for inst : my_component use entity work.my_entity;

for inst : my_component use entity work.my_entity(my_arch);

for inst : my_component use configuration work.my_configuration;
end_template
end_group
begin_group Concurrent Statements
begin_template Conditional Signal Assignment
<optional_label>: <target> <= 
	<value> when <condition> else
	<value> when <condition> else 
	<value> when <condition> else
	...
	<value>;
end_template
begin_template Selected Signal Assignment
<optional_label>: with <expression> select
	<target> <= <value> when <choices>,
				<value> when <choices>,
				<value> when <choices>,
				...
				<value> when others;
end_template
begin_template Concurrent Procedure Call
<optional_label>: <procedure_name> (<arguments>);
end_template
begin_template Combinational Process
<optional_label>:
	process(<sensitivity_list>) is
		-- Declaration(s)
	begin
		-- Sequential Statement(s)
	end process;
end_template
begin_template Sequential Process
<optional_label>:
	process(reset, clk) is 
		-- Declaration(s) 
	begin 
		if(reset = '1') then
			-- Asynchronous Sequential Statement(s) 
		elsif(rising_edge(clk)) then
			-- Synchronous Sequential Statement(s)
		end if;
	end process; 
end_template
begin_group Generates
begin_template Generate For
<generate_label>: 
	for <loop_id> in <range> generate
		-- Concurrent Statement(s)
	end generate;
end_template
begin_template Generate If
<generate_label>: 
	if <condition> generate
		-- Concurrent Statement(s)
	end generate;
end_template
end_group
begin_group Instances
begin_template Component Instantiation
<instance_name> : <component_name> 
	generic map
	(
		<name> => <value>,
		...
	)
	port map 
	(
		<formal_input> => <expression>,
		<formal_output> => <signal>,
		<formal_inout> => <signal>,
		...
	);
end_template
begin_template Direct Entity Instantiation
-- To instantiate an entity directly, the entity must be written in VHDL.
-- You must also add the file containing the entity declaration to your 
-- Quartus Prime project.
<instance_name>: entity <library>.<entity_name>
	generic map
	(
		<name> => <value>,
		...
	)
	port map 
	(
		<formal_input> => <expression>,
		<formal_output> => <signal>,
		<formal_inout> => <signal>,
		...
	);
end_template
begin_template Direct Entity Instantiation with Architecture
-- To instantiate an entity directly, the entity must be written in VHDL.
-- You must also add the file containing the entity declaration to your
-- Quartus Prime project.
<instance_name>: entity <library>.<entity_name>(<architecture_name>)
	generic map
	(
		<name> => <value>,
		...
	)
	port map 
	(
		<formal_input> => <expression>,
		<formal_output> => <signal>,
		<formal_inout> => <signal>,
		...
	);
end_template
end_group
end_group
begin_group Sequential Statements
begin_template Sequential Signal Assignment
<optional_label>: <signal_name> <= <expression>;	
end_template
begin_template Variable Assignment
<optional_label>: <variable_name> := <expression>;	
end_template
begin_template Procedure Call
<optional_label>: <procedure_name> (<arguments>);
end_template
begin_template If Statement
if <expression> then
	-- Sequential Statement(s)
elsif <expression> then
	-- Sequential Statement(s)
else
	-- Sequential Statement(s);
end if;
end_template
begin_template Case Statement
-- All choice expressions in a VHDL case statement must be constant
-- and unique.	Also, the case statement must be complete, or it must
-- include an others clause. 
case <expression> is
	when <constant_expression> =>
		-- Sequential Statement(s)
	when <constant_expression> =>
		-- Sequential Statement(s)
	when others =>
		-- Sequential Statement(s)
end case;
end_template
begin_template For Loop Statement
<optional_label>: 
	for <loop_id> in <range> loop
		-- Sequential Statement(s)
	end loop;
end_template
begin_template While Loop Statement
<optional_label>: 
	while <condition> loop
		-- Sequential Statement(s)
	end loop;
end_template
begin_template Next Statement
-- Unconditional next.
<optional_label>: next <optional_loop_label>;

-- Conditional next
<optional_loop_label>: next <optional_loop_label> when <condition>;	
end_template
begin_template Exit Statement
-- Unconditional exit
<optional_label>: exit <optional_loop_label>;

-- Conditional exit
<optional_label>: exit <optional_loop_label> when <condition>;
end_template
begin_template Return Statement
-- Inside a function, this statement must return a value.
<optional_label>: return <expression>;

-- Inside a procedure, this stamement must NOT return a value.
<optional_label>: return;
end_template
begin_template Null Statement
-- A null statement does nothing.
<optional_label>: null;	
end_template
end_group
begin_group Expressions
begin_template Unary Operators
-- Unary Expressions
+  -- positive
-  -- negative
NOT   -- negation
ABS   -- absolute value
end_template
begin_template Binary Operators
-- Binary Expressions
AND
OR
NAND
NOR
XOR
XNOR
=
/=
<
<=
>
>=
SLL   -- Shift Left Logical
SRL   -- Shift Right Logical
SLA   -- Shift Left Arithmetic: same as "logical" shift but uses sign extension (the leftmost bit is considered the sign bit)
SRA   -- Shift Right Arithmetic: same as "logical" shift but uses sign extension (the rightmost bit is considered the sign bit)
ROL   -- Rotate Left: Same as a shift, but bits that would "fall off" the left side during a shift will reappear on the right side in a rotation.
ROR   -- Rotate Right
+   -- Addition
-   -- Subtraction
&   -- Concatenation
*   -- Multiplication
/   -- Division
MOD   -- Modulus: If C <= A MOD B, then A = B*N + C (for some integral N), and ABS(C) < ABS(B).  Also, C must be positive if B is positive, and C must be negative if B is negative.  
REM   -- Remainder: If C <= A REM B, then A = (A/B)*B + C, and ABS(C) < ABS(B).  Also, C must be positive if A is positive, and C must be negative if A is negative.
**   -- Exponent
end_template
end_group
end_group
begin_group VHDL 2008 Constructs
begin_template Block Comment
/*
	Descriptive comment goes here.
	It can span multiple lines.
*/
end_template
begin_template Simplified Sensitivity List
<optional_label>:
	process(all) is
		-- Declaration(s)
	begin
		-- Sequential Statement(s)
	end process;
end_template
begin_template Enhanced If Generate Statement
<generate_label>: 
if <optional_label1>: <condition> generate
	--declarations
	begin
		-- Concurrent Statement(s)
	end <optional_label1>;
elsif <optional_label2>: <condition> generate
	--declarations
	begin
		-- Concurrent Statement(s)
	end <optional_label2>;
else <optional_label3>: generate
	--declarations
	begin
		-- Concurrent Statement(s)
	end <optional_label3>;	
end generate;
end_template
begin_template Case Generate Statement
--
-- All choice expressions in a VHDL case generate statement must be constant
-- and unique.	Also, the case statement must be complete, or it must
-- include an others clause. 
<generate_label>: 
case <expression> generate
	when <optional_label1>: <constant_expression> =>
		--declarations
		begin
			-- Concurrent Statement(s)
		end <optional_label1>;	
	when <optional_label2>: <constant_expression> =>
		--declarations
		begin
			-- Concurrent Statement(s)
		end <optional_label2>;	
	when <optional_label3>: others =>
		--declarations
		begin
			-- Concurrent Statement(s)
		end <optional_label3>;	
end generate;
end_template
begin_template Matching Case Statement
-- All choice expressions in a VHDL case statement must be constant
-- and unique.	Also, the case statement must be complete, or it must
-- include an others clause. 
-- The character '-' in the choice expressions will be considered as don't care while matching
case? <expression> is
	when <constant_expression> =>
		-- Sequential Statement(s)
	when <constant_expression> =>
		-- Sequential Statement(s)
	when others =>
		-- Sequential Statement(s)
end case?;
end_template
begin_template Matching Operators
?=	--matching equality: defined for STD_ULOGIC and BIT, and their one dimensional arrays ( a single '-' is considered to be equal to every scalar value of type BIT or STD_ULOGIC)
?/=	--matching inequality: defined for STD_ULOGIC and BIT, and their one dimensional arrays ( a single '-' is considered to be equal to every scalar value of type BIT or STD_ULOGIC)
end_template
begin_template Unconstrained Array Elements
-- Examples

-- Declare unconstrained array types with unconstrained elements.
type my_type1 is array(natural range <>) of std_logic_vector;
-- Three level deep unconstrained array
type my_type2 is array(natural range <>) of my_type1;

--partially constrained types/subtypes
type my_type3 is array(5 to 9) of my_type1(8 downto 4); --the std_logic_vector is still unconstrained
type my_type4 is array(4 to 6) of my_type1(open)(8 downto 4);
subtype my_subtype1 is my_type1(open)(10 to 12);
subtype my_subtype2 is my_type2(3 downto 0)(open)(3 to 5);

--fully constrained types/subtypes
type fully_constrained1 is array(7 downto 3) of my_type3(open)(open)(5 to 9);
type fully_constrained2 is array(0 to 3) of my_type1(7 to 9)(5 downto 2);
subtype fully_constrained3 is my_subtype1(4 to 6);
end_template
begin_template Enhanced Bit-string Literals
-- Examples
8B"XX_01LH" --"00XX01LH"
10D"56"	--"0000110110"
5UB"0010X1"	--"010X1"
5SX"FW" --"1WWWW"
5UX"17"	--"10111"

-- Error cases
3UB"0010X1"	--Error: only 0's can be removed from an unsigned bit-string literal to adjust the length
4SX"F0" --Error: for a signed bit-string literal, all the removed bits should be the same, and they should be the same as the left-most bit in the remaining bit-string
		--here, the final string would be "0000",so all the removed bits are '1' but they do not match the left-most bit ('0') of the final bit-string
end_template
begin_template Condition Operator
-- Examples of explicit invocation
signal in1 : std_logic;
signal in2,in3 : bit;

if ?? in1 then
	out1 <= '1';
elsif ?? (in2 and in3) then 
	out1 <= '0';
end if;


-- Examples of implicit Invocations
signal in1,in2 : std_logic;
signal in3,in4 : bit;

while in1 loop
	<sequential statements>
end loop;

outdata <=  "00" when not in1 and not in2 else
		"01" when not in1 and in2 else	
		"10" when in3 and not in4 else	
		"11" when in3 and in4;

-- Error cases
--error1
signal in1 : std_ulogic;
signal in2 : bit;

if ?? (in1 and in2) then --error: in1 and in2 are of different types
	out1 <= '0';
end if;

--error2
signal in1 : integer;

if ?? in1 then --error: in1 is of integer type - condition operator is only defined for bit and std_ulogic
	out1 <= '0';
end if;

--error3
signal in1 : std_ulogic;
signal in2 : bit;

if in1 and in2 = '1' then -- error: cannot mix std_ulogic and boolean types
	out1 <= '0';
end if;	
end_template
end_group VHDL 2008 Constructs
begin_group Logic
begin_group Registers
begin_template Basic Positive Edge Register
-- Update the register output on the clock's rising edge
process (<clock_signal>)
begin
	if (rising_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Positive Edge Register with Power-Up = VCC
-- Set the initial value to 1
signal <register_variable> : std_logic := '1';

-- After initialization, update the register output on the clock's 
-- rising edge
process (<clock_signal>)
begin
	if (rising_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Negative Edge Register
-- Update the register output on the clock's falling edge
process (<clock_signal>)
begin
	if (falling_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Negative Edge Register with Power-Up = VCC
-- Set the initial value to 1
signal <register_variable> : STD_LOGIC := '1';

-- After initialization, update the register output on the clock's 
-- falling edge
process (<clock_signal>)
begin
	if (falling_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Positive Edge Register with Asynchronous Reset
process (<clock_signal>, <reset>)
begin
	-- Reset whenever the reset signal goes low, regardless of the clock
	if (reset = '0') then
		<register_variable> <= '0';
	-- If not resetting, update the register output on the clock's rising edge
	elsif (rising_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Negative Edge Register with Asynchronous Reset
process (<clock_signal>, <reset>)
begin
	-- Reset whenever the reset signal goes low, regardless of the clock
	if (reset = '0') then
		<register_variable> <= '0';
	-- If not resetting, update the register output on the clock's falling edge
	elsif (falling_edge(<clock_signal>)) then
		<register_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Positive Edge Register with Asynchronous Reset and Clock Enable
process (<clock_signal>, <reset>)
begin
	-- Reset whenever the reset signal goes low, regardless of the clock
	-- or the clock enable
	if (reset = '0') then
		<register_variable> <= '0';
	-- If not resetting, and the clock signal is enabled on this register, 
	-- update the register output on the clock's rising edge
	elsif (rising_edge(<clock_signal>)) then
		if (<clock_enable> = '1') then
			<register_variable> <= <data>;
		end if;
	end if;
end process;
end_template
begin_template Basic Negative Edge Register with Asynchronous Reset and Clock Enable
process (<clock_signal>, <reset>)
begin
	-- Reset whenever the reset signal goes low, regardless of the clock
	-- or the clock enable
	if (reset = '0') then
		<register_variable> <= '0';
	-- If not resetting, and the clock signal is enabled on this register, 
	-- update the register output on the clock's falling edge
	elsif (falling_edge(<clock_signal>)) then
		if (<clock_enable> = '1') then
			<register_variable> <= <data>;
		end if;
	end if;
end process;
end_template
begin_template Full-Featured Positive Edge Register with All Secondary Signals
-- In Altera devices, register signals have a set priority.
-- The HDL design should reflect this priority.
process(<reset>, <aload>, <adata>, <clock_signal>)
begin
	-- The asynchronous reset signal has the highest priority
	if (<reset> = '0') then
		<register_variable> <= '0';
	-- Asynchronous load has next-highest priority
	elsif (<aload> = '1') then
		<register_variable> <= <adata>;
	else 
		-- At a clock edge, if asynchronous signals have not taken priority,
		-- respond to the appropriate synchronous signal.
		-- Check for synchronous reset, then synchronous load.
		-- If none of these takes precedence, update the register output
		-- to be the register input.
		if (rising_edge(<clock_signal>)) then
			if (<clock_enable> = '1') then
				if (<synch_reset> = '0') then
					<register_variable> <= '0';
				elsif (<synch_load> = '1') then
					<register_variable> <= <synch_data>;
				else
					<register_variable> <= <data>;
				end if;
			end if;
		end if;
	end if;
end process;
end_template
begin_template Full-Featured Negitive Edge Register with All Secondary Signals
-- In Altera devices, register signals have a set priority.
-- The HDL design should reflect this priority.
process(<reset>, <aload>, <adata>, <clock_signal>)
begin
	-- The asynchronous reset signal has the highest priority
	if (<reset> = '0') then
		<register_variable> <= '0';
	-- Asynchronous load has next-highest priority
	elsif (<aload> = '1') then
		<register_variable> <= <adata>;
	else 
		-- At a clock edge, if asynchronous signals have not taken priority,
		-- respond to the appropriate synchronous signal.
		-- Check for synchronous reset, then synchronous load.
		-- If none of these takes precedence, update the register output
		-- to be the register input.
		if (falling_edge(<clock_signal>)) then
			if (<clock_enable> = '1') then
				if (<synch_reset> = '0') then
					<register_variable> <= '0';
				elsif (<synch_load> = '1') then
					<register_variable> <= <synch_data>;
				else
					<register_variable> <= <data>;
				end if;
			end if;
		end if;
	end if;
end process;
end_template
end_group
begin_group Latches
begin_template Basic Latch
-- Update the variable only when updates are enabled
process(<enable>, <data>)
begin
	if (<enable> = '1') then
		<latch_variable> <= <data>;
	end if;
end process;
end_template
begin_template Basic Latch with Reset
process(<reset>, <enable>, <data>)
begin
	-- The reset signal overrrides the enable signal; reset the value to 0
	if (<reset> = '0') then
		<latch_variable> <= '0';
	-- Otherwise, change the variable only when updates are enabled
	elsif (<enable> = '1') then
		<latch_variable> <= <data>;
	end if;
end process;
end_template
end_group
begin_group Tri-State
begin_template Tri-State Buffer
-- Altera devices contain tri-state buffers in the I/O.  Thus, a tri-state
-- buffer must feed a top-level I/O in the final design.  Otherwise, the
-- Quartus Prime software will convert the tri-state buffer into logic.
<target> <= <data> when (<output_enable> = '1') else 'Z';
end_template
begin_template Tri-State Register
process (<clock_signal>, <asynch_output_enable>)
begin
	if (<asynch_output_enable> = '0') then
		<bidir_variable> <= 'Z';
	else
		if (rising_edge(<clock_signal>)) then
			if (<output_enable> = '0') then
				<bidir_variable> <= 'Z';
			else
				<bidir_variable> <= <data>;
			end if;
		end if;
	end if;
end process;
end_template
begin_template Bidirectional I/O
library ieee;
use ieee.std_logic_1164.all;
entity bidirectional_io is
generic
(
	WIDTH	: integer  :=	4
);
port
(
	<output_enable> : in std_logic;
	<data> : in std_logic_vector(WIDTH-1 downto 0);
	<bidir_variable> : inout std_logic_vector(WIDTH-1 downto 0);
	<read_buffer> : out std_logic_vector(WIDTH-1 downto 0)
);
end bidirectional_io;

architecture rtl of bidirectional_io is
begin
	-- If we are using the inout as an output, assign it an output value, 
	-- otherwise assign it high-impedence
	<bidir_variable> <= <data> when <output_enable> = '1' else (others => 'Z');

	-- Read in the current value of the bidir port, which comes either 
	-- from the input or from the previous assignment
	<read_buffer> <= <bidir_variable>;
end rtl;
end_template
begin_template Open-Drain Buffer
-- Altera devices contain tri-state buffers in the I/O.  Thus, an open-drain 
-- buffer must feed a top-level I/O in the final design.  Otherwise, the 
-- Quartus Prime software will convert the open-drain buffer into logic.
<target> <= '0' when (<output_enable> = '1') else 'Z';
end_template
end_group
end_group
begin_group Synthesis Attributes
begin_template Using Synthesis Attributes
-- Before using an attribute, you must first declare it or import
-- its declaration from a package.  All Altera-supported attributes are 
-- declared in the altera_syn_attributes package in the altera library.  You 
-- can import these declarations with the following use clause:
use altera.altera_syn_attributes.all;

-- For more detailed information on any attribute, refer to the
-- Quartus Prime Handbook or Help.
end_template
begin_template keep Attribute
-- Prevents Quartus Prime from minimizing or removing a particular
-- signal net during combinational logic optimization.	Apply
-- the attribute to a net or variable declaration.
attribute keep : boolean;

attribute keep of <object> : <object_class> is true;
end_template
begin_template maxfan Attribute
-- Sets the maximum number of fanouts for a register or combinational
-- cell.  The Quartus Prime software will replicate the cell and split
-- the fanouts among the duplicates until the fanout of each cell
-- is below the maximum.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute maxfan : natural;

attribute maxfan of <object> : <object_class> is <value>;
end_template
begin_template preserve Attribute
-- Prevents Quartus Prime from optimizing away a register as well
-- as from being retimed.  For HyperFlex architectures, users may want
-- "preserve_syn_only", which does allow retiming.
-- Apply the attribute to the variable declaration for an object that infers
-- a register.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute preserve : boolean;

attribute preserve of <object> : <object_class> is true;
end_template
begin_template preserve_syn_only Attribute
-- Prevents Quartus Prime from optimizing away or merging a register 
-- during Synthesis, but allows the register to be retimed during Retimer.
-- Often used instead of "preserve" for HyperFlex architectures.
-- Apply the attribute to the variable declaration for an object that infers
-- a register.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute preserve_syn_only : boolean;

attribute preserve_syn_only of <object> : <object_class> is true;
end_template
begin_template preserve_for_debug Attribute
-- Marks name so that Quartus Prime will preserve or keep the object
-- during Synthesis, if the associated object/hierarchy also has the attribute
-- PRESERVE_FOR_DEBUG_ENABLE set true on it as well. Useful for toggling on/off debug
-- logic between compilation sessions without changing the underlying RTL.
-- Set on either a register or a comb node.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute preserve_for_debug : boolean;

attribute preserve_for_debug of <object> : <object_class> is true;
end_template
begin_template noprune Attribute
-- Prevents Quartus Prime from removing or optimizing a fanout free register.
-- Apply the attribute to the variable declaration for an object that infers
-- a register.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute noprune : boolean;

attribute noprune of <object> : <object_class> is true;
end_template
begin_template dont_merge Attribute
-- Prevents Quartus Prime from merging a register with a duplicate
-- register

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute dont_merge : boolean;

attribute dont_merge of <object> : <object_class> is true;
end_template
begin_template dont_replicate Attribute
-- Prevents Quartus Prime from replicating a register.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute dont_replicate : boolean;

attribute dont_replicate of <object> : <object_class> is true;
end_template
begin_template dont_retime Attribute
-- Prevents Quartus Prime from retiming a register

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute dont_retime : boolean;

attribute dont_retime of <object> : <object_class> is true;
end_template
begin_template direct_enable Attribute
-- Identifies the logic cone that should be used as the clock enable
-- for a register.  Sometimes a register has a complex clock enable
-- condition, which may or may not contain the critical path in your
-- design.  With this attribute, you can force Quartus Prime to route
-- the critical portion directly to the clock enable port of a register
-- and implement the remaining clock enable condition using regular 
-- logic.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute direct_enable : boolean;

attribute direct_enable of <object> : <object_class> is true;

-- Example
signal e1, e2, q, data : std_logic;

attribute direct_enable of e1 : signal is true;

process(clk)
begin
	if(rising_edge(clk) and (e1 or e2)) then
		q <= data;
	end if;
end
end_template
begin_template useioff Attribute
-- Controls the packing input, output, and output enable registers into
-- I/O cells.  Using a register in an I/O cell can improve performance
-- by minimizing setup, clock-to-output, and clock-to-output-enable times.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute useioff : boolean;

attribute useioff of <object> : <object_class> is true;

-- Apply the attribute to a port object (a signal)

attribute useioff of my_input : signal is true;     -- enable packing
attribute useioff of my_input : signal is false;    -- disable packing
end_template
begin_template ramstyle Attribute
-- Controls the implementation of an inferred memory.  Apply the
-- attribute to a variable declaration that infers a RAM, ROM, or shift-register.

-- Legal values = "M9K", "M10K", "M20K", "M144K", "MLAB", "no_rw_check", "logic"

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute ramstyle : string;

attribute ramstyle of <object> : <object_class> is <string_value>;

-- If the attribute is set to "logic", then the RAM is implemented in logic cells

-- The "no_rw_check" value indicates that your design does not depend
-- on the behavior of the inferred RAM when there are simultaneous reads
-- and writes to the same address.  Thus, the Quartus Prime software may ignore
-- the read-during-write behavior of your HDL source and choose a behavior
-- that matches the behavior of the RAM blocks in the target device.

-- You may combine "no_rw_check" with a block type by separating the values
-- with a comma:  "M20K, no_rw_check" or "no_rw_check, M20K"  

-- Example

-- Implement all RAMs in this architecture with M20K blocks
attribute ramstyle of rtl : architecture is "M20K";

-- Implement this RAM with an MLAB and ignore read-during-write behavior
signal ram : ram_t;
attribute ramstyle of ram : signal is "MLAB, no_rw_check";
end_template
begin_template romstyle Attribute
-- Controls the implementation of an inferred ROM.  Apply the
-- attribute to a variable declaration that infers ROM or to a
-- entity or architecture containing inferred ROMs.   

-- Legal values = "M9K", "M10K", "M20K", "M144K", "MLAB"

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute romstyle : string;

attribute romstyle of <object> : <object_class> is <string_value>;

-- Example

-- Implement all ROMs in this architecture with M20K blocks
attribute romstyle of rtl : architecture is "M20K";

-- Implement this ROM with an MLAB
signal rom : rom_t;
attribute romstyle of rom : signal is "MLAB";
end_template
begin_template max_depth Attribute
-- Controls the implementation of an inferred memory.  Apply the
-- attribute to a variable declaration that infers a RAM or ROM.  

-- Legal values = 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4192

type ram_block is array (0 to 31) of std_logic_vector (2 downto 0);
signal mem : ram_block;
attribute max_depth : natural;
attribute max_depth OF mem : signal is 2048;
-- Control the depth of an inferred memory block using the max_depth attribute. 
-- By using this attribute, you can optimize the usage of the memory block.
-- Values other than exact powers of 2 are ignored.
end_template
begin_template multstyle Attribute
-- Controls the implementation of multiplication operators in your HDL 
-- source.  Using this attribute, you can control whether the Quartus Prime 
-- software should preferentially implement a multiplication operation in 
-- general logic or dedicated hardware, if available in the target device.  

-- Legal values = "dsp" or "logic"

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute multstyle : string;

attribute multstyle of <object> : <object_class> is <string_value>;

-- Examples (in increasing order of priority)

-- Control the implementation of all multiplications in an entity
attribute multstyle of foo : entity is "dsp";

-- Control the implementation of all multiplications whose result is
-- directly assigned to a signal
signal result : integer;

attribute multstyle of result : signal is "logic";

result <= a * b; -- implement this multiply in logic
end_template
begin_template syn_encoding Attribute
-- Controls the encoding of the states in an inferred state machine.

-- Legal values = "sequential", "gray", "johnson", "compact", "onehot",
--                "auto", "default", "safe", or a space-delimited list of
--                 binary encodings, e.g. "00100 11010 10110"

-- The value "safe" instructs the Quartus Prime software to add extra logic 
-- to detect illegal states (unreachable states) and force the state machine 
-- into the reset state. You cannot implement a safe state machine by 
-- specifying manual recovery logic in your design; the Quartus Prime software 
-- eliminates this logic while optimizing your design.  You can combine
-- "safe" with any encoding style (but not a list of binary encodings), e.g.
-- "sequential, safe"

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute syn_encoding : string;

attribute syn_encoding of <object> : <object_class> is <string_value>;

-- Implement all state machines with type state_t as safe, gray-encoded
-- state machines
type state_t is (S0, S1, S2, S3, S4);
attribute syn_encoding of state_t : type is "gray, safe";
end_template
begin_template enum_encoding Attribute
-- Controls the encoding of an enumerated type.   

-- Legal values = "sequential", "gray", "johnson", "onehot", "default", 
--                "auto", or a space-delimited list of binary encodings, 
--                e.g. "00100 11010 10110"

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute syn_encoding : string;

attribute syn_encoding of <object> : <object_class> is <string_value>;

-- Implement all state machines with type state_t as safe, gray-encoded
-- state machines
type enum_t is (apple, orange, pear, cherry);
attribute enum_encoding of enum_t : type is "onehot";
end_template
begin_template chip_pin Attribute
-- Assigns pin location to ports on an entity.

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute chip_pin : string;

attribute chip_pin of <object> : <object_class> is <string_value>;

-- Example
attribute chip_pin of my_input : signal is "B3, A3, A4";
end_template
begin_template altera_attribute Attribute
-- Associates arbitrary Quartus Prime assignments with objects in your HDL
-- source.  Each assignment uses the QSF format, and you can associate
-- multiple assignments by separating them with ";".

-- Declare the attribute or import its declaration from 
-- altera.altera_syn_attributes
attribute altera_attribute : string;

attribute altera_attribute of <object> : <object_class> is <string_value>;

-- Preserve all registers in this hierarchy
attribute altera_attribute of foo : entity is "-name PRESERVE_REGISTER on";

-- Cut timing paths from register q1 to register q2
signal q1, q2 : std_logic;
attribute altera_attribute of q2 : signal is "-name CUT on -from q1";
end_template
end_group
begin_group Altera Primitives
begin_group Buffers
begin_template ALT_INBUF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all; 
-- Instantiating ALT_INBUF
	<instance_name> : ALT_INBUF
	generic map (
			IO_STANDARD => "LVDS",
			LOCATION => "IOBANK_1A",
			ENABLE_BUS_HOLD => "off",
			WEAK_PULL_UP_RESISTOR => "off"
			)
	port map ( 
			i => <data_in>,	  -- <data_in> must be declared as an input pin
			o => <data_out>
			);
end_template
begin_template ALT_INBUF_DIFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all; 
-- Instantiating ALT_INBUF_DIFF
	<instance_name> : ALT_INBUF_DIFF
	generic map (
			IO_STANDARD => "LVDS",
			LOCATION => "IOBANK_1A",
			ENABLE_BUS_HOLD => "off",
			WEAK_PULL_UP_RESISTOR => "off"
			) 
	port map ( 
			i => <data_in_pos>,		 -- <data_in_pos> must be an input pin
			ibar => <data_in_neg>,	 -- <data_in_neg> must be an input pin
			o => <data_out>
			);
end_template
begin_template ALT_IOBUF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all; 
-- Instantiating ALT_IOBUF
	<instance_name> : ALT_IOBUF
	generic map (
			IO_STANDARD => "Differential 1.2-V HSTL Class I",
			CURRENT_STRENGTH_NEW => "4mA",
			ENABLE_BUS_HOLD => "none",
			WEAK_PULL_UP_RESISTOR => "off",
			LOCATION => "IOBANK_3C"
			) 
	port map (
			i => <data_in>, 
			oe => <enable_signal>, 
			o => <data_out>, 
			io => <bidir>	  -- <bidir> must be declared as an inout pin
			);
end_template
begin_template ALT_OUTBUF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating ALT_OUTBUF
	<instance_name> : ALT_OUTBUF
	generic map (
			IO_STANDARD => "LVDS",
			LOCATION => "IOBANK_2A",
			CURRENT_STRENGTH => "minimum current",
			ENABLE_BUS_HOLD => "off",
			WEAK_PULL_UP_RESISTOR => "off"
			)
	port map ( i => <data_in>, o => <data_out>);  -- <data_out> must be declared as an output pin
end_template
begin_template ALT_OUTBUF_DIFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all; 
-- Instantiating ALT_OUTBUF_DIFF
	<instance_name> : ALT_OUTBUF_DIFF
	generic map (
			IO_STANDARD => "LVDS",
			LOCATION => "IOBANK_2A",
			CURRENT_STRENGTH => "minimum current",
			ENABLE_BUS_HOLD => "off",
			WEAK_PULL_UP_RESISTOR => "off"
			)
	-- <data_out_pos> and <data_out_neg> must be declared as output pins
	port map ( i => <data_in>, o => <data_out_pos>, obar => <data_out_neg>);
end_template
begin_template ALT_OUTBUF_TRI
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating ALT_OUTBUF_TRI
	<instance_name> : ALT_OUTBUF_TRI
	generic map (
			IO_STANDARD => "Differential 1.8-V SSTL Class I",
			LOCATION => "IOBANK_2C",
			CURRENT_STRENGTH => "8mA",
			ENABLE_BUS_HOLD => "off",
			WEAK_PULL_UP_RESISTOR => "off"
			) 
	port map ( 
			i => <data_in>, 
			oe => <enable_signal>, 
			o => <data_out>	  -- <data_out> must be declared as an output pin
			);
end_template
begin_template CASCADE
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating CASCADE
	<instance_name> : CASCADE
	-- <data_out> cannot feed an output pin, a register, or an XOR gate
	port map(a_in => <data_in>, a_out => <data_out>);
end_template
begin_template CARRY_SUM
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating CARRY_SUM
	<instance_name> : CARRY_SUM
	-- <carry_in> cannot be fed by an input pin
	-- <carry_out> cannot feed an output pin
	port map(sin => <sum_in>, cin => <carry_in>, sout => <sum_out>, cout => <carry_out>);
end_template
begin_template GLOBAL
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating GLOBAL
	<instance_name> : GLOBAL
	port map (a_in => <data_in>, a_out => <data_out>);
end_template
begin_template LCELL
-- Add the library and use clauses before the design unit declaration
library altera_mf; 
use altera_mf.altera_mf_components.all;
-- Instantiating LCELL
	<instance_name> : LCELL
	port map (a_in => <data_in>, a_out => <data_out>);
end_template
begin_template OPNDRN
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating OPNDRN
	<instance_name> : OPNDRN
	-- <data_out> may feed an inout pin
	port map (a_in => <data_in>, a_out => <data_out>);
end_template
begin_template TRI
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating TRI
	<instance_name> : TRI
	-- <data_out> may feed an inout pin
	port map (a_in => <data_in>, oe => <enable_signal>, a_out => <data_out>);
end_template
end_group
begin_group Registers and Latches
begin_template DFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating DFF
	<instance_name> : DFF
	port map (
			d => <data_in>, 
			clk => <clock_signal>, 
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template DFFE
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating DFFE
	<instance_name> : DFFE
	port map (
			d => <data_in>,
			clk => <clock_signal>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			ena => <clock_enable>,
			q => <data_out>
			);
end_template
begin_template DFFEA
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating DFFEA
	<instance_name> : DFFEA
	port map (
			d => <data_in>,
			clk => <clock_signal>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			ena => <clock_enable>,
			adata => <asynch_data_in>,
			aload => <asynch_load_signal>,
			q => <data_out>
			);
end_template
begin_template DFFEAS
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating DFFEAS
	<instance_name> : DFFEAS
	port map (
			d => <data_in>,
			clk => <clock_signal>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			ena => <clock_enable>,
			asdata => <asynch_data_in>,
			aload => <asynch_load_signal>,
			sclr => <synchronous_clear>,
			sload => <synchronous_load>,
			q => <data_out>
			);
end_template
begin_template JKFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating JKFF
	<instance_name> : JKFF
	port map (
			j => <synchronous_set>,
			k => <synchronous_reset>,
			clk => <clock_signal>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template JKFFE
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating JKFFE
	<instance_name> : JKFFE
	port map (
			j => <synchronous_set>, 
			k => <synchronous_reset>,
			clk => <clock_signal>,
			ena => <clock_enable>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template LATCH
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating LATCH
	<instance_name> : LATCH
	port map (
			d => <data_in>,
			ena => <clock_enable>,
			q => <data_out>
			);
end_template
begin_template SRFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating SRFF
	<instance_name> : SRFF
	port map (
			s => <synchronous_set>,
			r => <synchronous_reset>,
			clk => <clock_signal>, 
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template SRFFE
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating SRFFE
	<instance_name> : SRFFE
	port map (
			s => <synchronous_set>,
			r => <synchronous_reset>,
			clk => <clock_signal>,
			ena => <clock_enable>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template TFF
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating TFF
	<instance_name> : TFF
	port map (
			t => <toggle>,
			clk => <clock_signal>, 
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
begin_template TFFE
-- Add the library and use clauses before the design unit declaration
library altera; 
use altera.altera_primitives_components.all;
-- Instantiating TFFE
	<instance_name> : TFFE
	port map (
			t => <toggle>, 
			clk => <clock_signal>,
			ena => <clock_enable>,
			clrn => <active_low_clear>,
			prn => <active_low_preset>,
			q => <data_out>
			);
end_template
end_group
end_group
begin_group Intel Parameterizable Macros
begin_template SIMPLE_DUAL_PORT_RAM

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating SIMPLE_DUAL_PORT_RAM
	<instance_name> : SIMPLE_DUAL_PORT_RAM
	generic map (
			IN_CLOCK_EN_A =>                             "NORMAL",
			IN_CLOCK_EN_B =>                             "NORMAL",
			OUT_CLOCK_EN_B =>                            "NORMAL",
			DATA_WIDTH_A =>                              8,
			ADDR_WIDTH_A =>                              11,
			BYTE_EN_WIDTH_A =>                           1,
			DATA_WIDTH_B =>                              8,
			ADDR_WIDTH_B =>                              11,
			OUT_DATA_REG_CLK_B =>                        "UNREGISTERED",
			ADDR_REG_CLK_B =>                            "CLOCK0",
			OUT_DATA_ACLR_B =>                           "NONE",
			OUT_DATA_SCLR_B =>                           "NONE",
			ADDR_ACLR_B =>                               "NONE",
			READ_DURING_WRITE_MODE_MIXED_PORTS =>        "DONT_CARE",
			INIT_FILE =>                                 "",
			INIT_FILE_LAYOUT =>                          "PORT_A",
			MAX_DEPTH =>                                 2048,
			RDCONTROL_REG_B =>                           "CLOCK0",
			BYTEENA_REG_B =>                             "CLOCK0",
			BYTE_SIZE =>                                 8
			)
	port map ( 
			clock0 =>                                    _connected_to_clock0_,                        -- input, width = 1	  
			clock1 =>                                    _connected_to_clock1_,                        -- input, width = 1
			data_a =>                                    _connected_to_data_a_,                        -- input, width = DATA_WIDTH_A	  
			address_a =>                                 _connected_to_address_a_,                     -- input, width = ADDR_WIDTH_A	  
			address_b =>                                 _connected_to_address_b_,                     -- input, width = ADDR_WIDTH_B	  
			wren_a =>                                    _connected_to_wren_a_,                        -- input, width = 1
			rden_b =>                                    _connected_to_rden_b_,                        -- input, width = 1
			clocken0 =>                                  _connected_to_clocken0_,                      -- input, width = 1
			clocken1 =>                                  _connected_to_clocken1_,                      -- input, width = 1
			aclr0 =>                                     _connected_to_aclr0_,                         -- input, width = 1
			aclr1 =>                                     _connected_to_aclr1_,                         -- input, width = 1
			sclr =>                                      _connected_to_sclr_,                          -- input, width = 1	  
			byteena_a =>                                 _connected_to_byteena_a_,                     -- input, width = BYTE_EN_WIDTH_A	  
			addressstall_a =>                            _connected_to_addressstall_a_,                -- input, width = 1	  
			addressstall_b =>                            _connected_to_addressstall_b_,                -- input, width = 1	  
			q_b =>                                       _connected_to_q_b_                            -- output, width = DATA_WIDTH_B
			);
end_template
begin_template TRUE_DUAL_PORT_RAM

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating TRUE_DUAL_PORT_RAM
	<instance_name> : TRUE_DUAL_PORT_RAM
	generic map (
			-- Port A Parameters
			DATA_WIDTH_A =>                              8,
			ADDR_WIDTH_A =>                              11,
			BYTE_EN_WIDTH_A =>                           1,
			OUT_DATA_REG_CLK_A =>                        "UNREGISTERED",
			OUT_DATA_ACLR_A =>                           "NONE",
			OUT_DATA_SCLR_A =>                           "NONE",
			READ_DURING_WRITE_MODE_A =>                  "NEW_DATA_NO_NBE_READ",
			IN_CLK_EN_A =>                               "NORMAL",
			OUT_CLK_EN_A =>                              "NORMAL",
			-- Port B Parameters
			DATA_WIDTH_B =>                              8,
			ADDR_WIDTH_B =>                              11,
			BYTE_EN_WIDTH_B =>                           1,
			OUT_DATA_REG_CLK_B =>                        "UNREGISTERED",
			OUT_DATA_ACLR_B =>                           "NONE",
			OUT_DATA_SCLR_B =>                           "NONE",
			READ_DURING_WRITE_MODE_B =>                  "NEW_DATA_NO_NBE_READ",
			IN_CLK_EN_B =>                               "NORMAL",
			OUT_CLK_EN_B =>                              "NORMAL",
			-- Parameters common for Port A and Port B
			BYTE_SIZE =>                                 8,
			INIT_FILE =>                                 "",
			INIT_FILE_LAYOUT =>                          "PORT_A",
			MAX_DEPTH =>                                 2048
			)
	port map ( 
			clock0 =>                                    _connected_to_clock0_,                        -- input, width = 1		  
			clock1 =>                                    _connected_to_clock1_,                        -- input, width = 1		  
			clocken0 =>                                  _connected_to_clocken0_,                      -- input, width = 1		  
			clocken1 =>                                  _connected_to_clocken1_,                      -- input, width = 1		  
			aclr =>                                      _connected_to_aclr_,                          -- input, width = 1		  
			sclr =>                                      _connected_to_sclr_,                          -- input, width = 1		  
			data_a =>                                    _connected_to_data_a_,                        -- input, width = DATA_WIDTH_A		  
			address_a =>                                 _connected_to_address_a_,                     -- input, width = ADDR_WIDTH_A		  
			wren_a =>                                    _connected_to_wren_a_,                        -- input, width = 1		  
			rden_a =>                                    _connected_to_rden_a_,                        -- input, width = 1		  
			byteena_a =>                                 _connected_to_byteena_a_,                     -- input, width = BYTE_EN_WIDTH_A		  
			data_b =>                                    _connected_to_data_b_,                        -- input, width = DATA_WIDTH_B		  
			address_b =>                                 _connected_to_address_b_,                     -- input, width = ADDR_WIDTH_B		  
			wren_b =>                                    _connected_to_wren_b_,                        -- input, width = 1		  
			rden_b =>                                    _connected_to_rden_b_,                        -- input, width = 1		  
			byteena_b =>                                 _connected_to_byteena_b_,                     -- input, width = BYTE_EN_WIDTH_B		  
			q_a =>                                       _connected_to_q_a_,                           -- output, width = DATA_WIDTH_A		  
			q_b =>                                       _connected_to_q_b_                            -- output, width = DATA_WIDTH_B	
			);
end_template
begin_template ASYNC_FIFO

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating ASYNC_FIFO
	<instance_name> : ASYNC_FIFO
	generic map (
			DATA_WIDTH_A =>                              8,
			ADDR_WIDTH_A =>                              11,
			DATA_WIDTH_B =>                              8,
			ADDR_WIDTH_B =>                              11,
			RDSYNC_DELAYPIPE =>                          2,
			WRSYNC_DELAYPIPE =>                          2,
			ENABLE_SHOWAHEAD =>                          "OFF",
			UNDERFLOW_CHECKING =>                        "ON",
			OVERFLOW_CHECKING =>                         "ON",
			ADD_USEDW_MSB_BIT =>                         "OFF",
			WRITE_ACLR_SYNCH =>                          "OFF",
			READ_ACLR_SYNCH =>                           "OFF",
			ADD_RAM_OUTPUT_REGISTER =>                   "OFF",
			MAXIMUM_DEPTH =>                             2048,
			BYTE_EN_WIDTH =>                             1,
			BYTE_SIZE =>                                 8
			)
	port map ( 
			data =>                                      _connected_to_data_,                          -- input, width = DATA_WIDTH_A		  
			rdclk =>                                     _connected_to_rdclk_,                         -- input, width = 1		  
			wrclk =>                                     _connected_to_wrclk_,                         -- input, width = 1		  
			aclr =>                                      _connected_to_aclr_,                          -- input, width = 1		  
			rdreq =>                                     _connected_to_rdreq_,                         -- input, width = 1		  
			wrreq =>                                     _connected_to_wrreq_,                         -- input, width = 1		  
			byteena =>                                   _connected_to_byteena_,                       -- input, width = BYTE_EN_WIDTH		  
			rdfull =>                                    _connected_to_rdfull_,                        -- output, width = 1		  
			wrfull =>                                    _connected_to_wrfull_,                        -- output, width = 1		  
			rdempty =>                                   _connected_to_rdempty_,                       -- output, width = 1		  
			wrempty =>                                   _connected_to_wrempty_,                       -- output, width = 1		  
			rdusedw =>                                   _connected_to_rdusedw_,                       -- output, width = ADDR_WIDTH_B		  
			wrusedw =>                                   _connected_to_wrusedw_,                       -- output, width = ADDR_WIDTH_A		  
			q =>                                         _connected_to_q_                              -- output, width = DATA_WIDTH_B	
			);
end_template
begin_template SYNC_FIFO

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating SYNC_FIFO
	<instance_name> : SYNC_FIFO
	generic map (
			ADD_RAM_OUTPUT_REGISTER =>                   "OFF",
			ALMOST_EMPTY_VALUE =>                        1,
			ALMOST_FULL_VALUE =>                         1,
			ENABLE_SCLR =>                               "OFF",
			ENABLE_ACLR =>                               "OFF",
			ALLOW_RWCYCLE_WHEN_FULL =>                   "ON",
			ENABLE_SHOWAHEAD =>                          "OFF",
			DATA_WIDTH =>                                8,
			ADDR_WIDTH =>                                11,
			OVERFLOW_CHECKING =>                         "ON",
			UNDERFLOW_CHECKING =>                        "ON",
			MAXIMUM_DEPTH =>                             2048,
			BYTE_SIZE =>                                 8,
			BYTE_EN_WIDTH =>                             1
			)
	port map ( 
			clock =>                                     _connected_to_clock_,                          -- input, width = 1		  
			data =>                                      _connected_to_data_,                           -- input, width = DATA_WIDTH		  
			rdreq =>                                     _connected_to_rdreq_,                          -- input, width = 1		  
			sclr =>                                      _connected_to_sclr_,                           -- input, width = 1		  
			aclr =>                                      _connected_to_aclr_,                          -- input, width = 1		  
			wrreq =>                                     _connected_to_wrreq_,                          -- input, width = 1		  
			byteena =>                                   _connected_to_byteena_,                        -- input, width = BYTE_EN_WIDTH		  
			empty =>                                     _connected_to_empty_,                          -- output, width = 1		  
			full =>                                      _connected_to_full_,                           -- output, width = 1		  
			almost_empty =>                              _connected_to_almost_empty_,                   -- output, width = 1		  
			almost_full =>                               _connected_to_almost_full_,                    -- output, width = 1		  
			q =>                                         _connected_to_q_,                              -- output, width = DATA_WIDTH		  
			usedw =>                                     _connected_to_usedw_                           -- output, width = ADDR_WIDTH
			);
end_template
begin_template IPM_IOPLL_BASIC

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_IOPLL_BASIC
	<instance_name> : IPM_IOPLL_BASIC
	generic map (
			REFERENCE_CLOCK_FREQUENCY =>		"100.0 MHz",
			N_CNT =>                                1,
			M_CNT =>                                6,
			C0_CNT =>                               1,           	
			C1_CNT =>                               1,           	
			C2_CNT =>                               1,           	
			C3_CNT =>                               1,           	
			C4_CNT =>                               1,           	
			C5_CNT =>                               1,           	
			C6_CNT =>                               1,
			PLL_SIM_MODEL =>                        ""		-- It is a simulation specific parameter to select the technology dependent IOPLL simulation model. Allowed values are "Stratix 10", "Agilex 7 F-Series", "Agilex 7 (F-Series)", "Agilex 7 I-Series", "Agilex 7 (I-Series)", "Agilex 7 M-Series", "Agilex 7 (M-Series)".
			)
	port map (
			refclk =>                               _connected_to_refclk_,                        -- input, width = 1
			reset =>                                _connected_to_reset_,                         -- input, width = 1 
			outclk0 =>                              _connected_to_outclk0_,                       -- output, width = 1
			outclk1 =>                              _connected_to_outclk1_,                       -- output, width = 1
			outclk2 =>                              _connected_to_outclk2_,                       -- output, width = 1
			outclk3 =>                              _connected_to_outclk3_,                       -- output, width = 1
			outclk4 =>                              _connected_to_outclk4_,                       -- output, width = 1
			outclk5 =>                              _connected_to_outclk5_,                       -- output, width = 1
			outclk6 =>                              _connected_to_outclk6_,                       -- output, width = 1
			locked =>                               _connected_to_locked_                         -- output, width = 1
			);
end_template
begin_template IPM_IOPLL_ADVANCED

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_IOPLL_ADVANCED
	<instance_name> :IPM_IOPLL_ADVANCED
	generic map (
			REFERENCE_CLOCK_FREQUENCY =>		"100.0 MHz",
			N_CNT =>                                1,
			M_CNT =>                                6,
			C0_CNT =>                               6,
			C1_CNT =>                               6,
			C2_CNT =>                               6,
			C3_CNT =>                               6,
			C4_CNT =>                               6,
			C5_CNT =>                               6,
			C6_CNT =>                               6,
			OPERATION_MODE =>                       "direct",
			CLOCK_TO_COMPENSATE =>                  1,
			PHASE_SHIFT0 =>                         "0 ps",
			PHASE_SHIFT1 =>                         "0 ps",
			PHASE_SHIFT2 =>                         "0 ps",
			PHASE_SHIFT3 =>                         "0 ps",
			PHASE_SHIFT4 =>                         "0 ps",
			PHASE_SHIFT5 =>                         "0 ps",
			PHASE_SHIFT6 =>                         "0 ps",
			DUTY_CYCLE0 =>                          50,
			DUTY_CYCLE1 =>                          50,
			DUTY_CYCLE2 =>                          50,
			DUTY_CYCLE3 =>                          50,
			DUTY_CYCLE4 =>                          50,
			DUTY_CYCLE5 =>                          50,
			DUTY_CYCLE6 =>                          50,
			PLL_SIM_MODEL =>                        ""			-- It is a simulation specific parameter to select the technology dependent IOPLL simulation model. Allowed values are "Stratix 10", "Agilex 7 F-Series", "Agilex 7 (F-Series)", "Agilex 7 I-Series", "Agilex 7 (I-Series)", "Agilex 7 M-Series", "Agilex 7 (M-Series)".
			)
	port map (
			refclk =>                               _connected_to_refclk_,			-- input,  width = 1
			reset =>                                _connected_to_reset_,                   -- input,  width = 1
			fbclk =>                                _connected_to_fbclk_,                   -- input,  width = 1
			outclk0 =>                              _connected_to_outclk0_,                 -- output, width = 1
			outclk1 =>                              _connected_to_outclk1_,                 -- output, width = 1
			outclk2 =>                              _connected_to_outclk2_,                 -- output, width = 1
			outclk3 =>                              _connected_to_outclk3_,                 -- output, width = 1
			outclk4 =>                              _connected_to_outclk4_,                 -- output, width = 1
			outclk5 =>                              _connected_to_outclk5_,                 -- output, width = 1
			outclk6 =>                              _connected_to_outclk6_,                 -- output, width = 1
			locked =>                               _connected_to_locked_,                  -- output, width = 1
			fbclkout =>                             _connected_to_fbclkout_,                -- output, width = 1
			extclk_out =>                           _connected_to_extclk_out_,              -- output, width = 1
			zdbfbclk =>                             _connected_to_zdbfbclk_                 -- inout,  width = 1
			);
end_template
begin_template IPM_CDC_SYNC_RST

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_SYNC_RST
	<instance_name> : IPM_CDC_SYNC_RST
	generic map (
			RST_TYPE =>                               "ACTIVE_HIGH",
			NUM_STAGES =>                              3
			)
	port map ( 
			clk =>                                      _connected_to_clk_,                         -- input, width = 1	  
			srst_in =>                                  _connected_to_srst_in_,                     -- input, width = 1		  
			srst_out =>                                 _connected_to_srst_out_                     -- output, width = 1		 	
			);
end_template
begin_template IPM_CDC_ASYNC_RST

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_ASYNC_RST
	<instance_name> : IPM_CDC_ASYNC_RST
	generic map (
			RST_TYPE  =>                              "ACTIVE_HIGH",
			NUM_STAGES =>                              3
			)
	port map ( 
			clk      =>                                _connected_to_clk_,                         -- input, width = 1	  
			arst_in  =>                                _connected_to_arst_in_,                     -- input, width = 1		  
			srst_out =>                                _connected_to_srst_out_                     -- output, width = 1		  	
			);
end_template
begin_template IPM_CDC_1CLK_SYNC

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_1CLK_SYNC
	<instance_name> : IPM_CDC_1CLK_SYNC
	generic map (
			INITIAL_VALUE =>                            0,
			NUM_STAGES =>                               3
			)
	port map ( 
			clk =>                                      _connected_to_clk_,                        -- input, width = 1	  
			async_in =>                                 _connected_to_async_in_,                   -- input, width = 1		  
			sync_out =>                                 _connected_to_sync_out_                    -- output, width = 1		  
			);
end_template
begin_template IPM_CDC_2CLKS_SYNC

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_2CLKS_SYNC
	<instance_name> : IPM_CDC_2CLKS_SYNC
	generic map (
			INITIAL_VALUE =>                           0,
			NUM_STAGES =>                              3
			)
	port map ( 
			src_clk =>                                 _connected_to_src_clk_,                   -- input, width = 1	  
			src_sig =>                                 _connected_to_src_sig_,                   -- input, width = 1
		  	dst_clk =>                                 _connected_to_dst_clk_,		     -- input, width = 1
			dst_sig =>                                 _connected_to_dst_sig_                    -- output, width = 1		  
			);
end_template
begin_template IPM_CDC_BUS_SYNC

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_BUS_SYNC
	<instance_name> : IPM_CDC_BUS_SYNC
	generic map (
			DATA_WIDTH =>                              4
			)
	port map ( 
			src_clk =>                                 _connected_to_src_clk_,                   -- input, width = 1	  
			src_sig =>                                 _connected_to_src_sig_,                   -- input, width = DATA_WIDTH
		  	dst_clk =>                                 _connected_to_dst_clk_,		     -- input, width = 1
			dst_sig =>                                 _connected_to_dst_sig_ ,                  -- output, width = DATA_WIDTH
			src_sync_req =>                            _connected_to_src_sync_req_,              --output, width = 1
			dst_sync_ack =>                   	   _connected_to_dst_sync_ack_,       	     --output, width = 1
			src_req =>                                 _connected_to_src_req_,                   --output, width = 1
			dst_ack =>                                 _connected_to_dst_ack_                    --output, width = 1
			);
end_template
begin_template IPM_CDC_GLITCHLESS_CLK_MUX

-- Documentation :
-- https://www.intel.com/content/www/us/en/docs/programmable/772350/
-- Macro Location :
-- $QUARTUS_ROOTDIR/eda/sim_lib/altera_lnsim_components.vhd
-- Add the library and use clauses before the design unit declaration
library altera_lnsim; 
use altera_lnsim.altera_lnsim_components.all; 
-- Instantiating IPM_CDC_GLITCHLESS_CLK_MUX
	<instance_name> : IPM_CDC_GLITCHLESS_CLK_MUX
	generic map (
			CLK_TYPE =>                              "RELATED_CLKS"
			)
	port map ( 
			sel =>                                   _connected_to_sel_,                   -- input, width = 1	  
			clk_A =>                                 _connected_to_clk_A_,                 -- input, width = 1
		  	clk_B =>                                 _connected_to_clk_B_,		       -- input, width = 1
			clk_out =>                               _connected_to_clk_out_                -- output, width = 1		  
			);
end_template
end_group
end_group
