-- Single-port RAM with single read/write port
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_port_ram is
    generic (
        DATA_WIDTH : natural := 8;
        ADDR_WIDTH : natural := 8
    );
    port (
        clk : in std_logic;
        we : in std_logic;
        addr : in unsigned(ADDR_WIDTH-1 downto 0);
        data_in : in unsigned(DATA_WIDTH-1 downto 0);
        data_out : out unsigned(DATA_WIDTH-1 downto 0)
    );
end entity single_port_ram;

architecture rtl of single_port_ram is
    type ram_type is array (2**ADDR_WIDTH-1 downto 0) of unsigned(DATA_WIDTH-1 downto 0);
    signal ram : ram_type;
begin   
    process (clk)
    begin
        if rising_edge(clk) then
            if we = '1' then
                ram(to_integer(addr)) <= data_in;
            end if;
            data_out <= ram(to_integer(addr));
        end if;
    end process;
end architecture rtl;
```



