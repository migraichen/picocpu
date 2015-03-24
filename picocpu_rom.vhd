library ieee; 
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all; 
  
entity picocpu_rom is 
  port (  
    clk_in                     : in  std_logic; 
    address_in                 : in  std_logic_vector( 9 downto 0);
    instruction_out            : out std_logic_vector(17 downto 0) := ( others => '0' )
  ); 
end picocpu_rom; 
  
architecture behavioral of picocpu_rom is 

  constant rom_addr_width_c : natural := address_in'length;
  constant rom_data_width_c : natural := instruction_out'length;

  type rom_type is array (0 to (2**rom_addr_width_c)-1) of std_logic_vector(rom_data_width_c-1 downto 0);
  signal rom : rom_type;
  attribute ram_init_file : string;
  attribute ram_init_file of rom : signal is "rom.hex";
  attribute ramstyle : string;
  attribute ramstyle of rom : signal is "M10K";

begin  
  
  process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then
      instruction_out <= rom(to_integer(unsigned(address_in)));
    end if;
  end process;
  
end behavioral; 
