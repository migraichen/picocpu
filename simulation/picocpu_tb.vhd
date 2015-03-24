library ieee; 
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all; 
  
entity picocpu_tb is 
end picocpu_tb; 
  
architecture behavioral of picocpu_tb is 
  
component picocpu_rom is 
  port (  
    clk_in                    : in  std_logic; 
    address_in                : in  std_logic_vector( 9 downto 0);
    instruction_out           : out std_logic_vector(17 downto 0)
  ); 
end component;    
  
component picocpu is 
  port (  
    clk_in                    : in  std_logic; 
    rst_n_in                  : in  std_logic;
    -- Signale zum Programmspeicher
    inst_addr_out             : out std_logic_vector( 9 downto 0); 
    inst_data_in              : in  std_logic_vector(17 downto 0);
    -- Output / Input Port
    port_id_out               : out std_logic_vector( 7 downto 0); 
    port_wr_pulse_out         : out std_logic;
    port_data_out             : out std_logic_vector( 7 downto 0);
    port_rd_pulse_out         : out std_logic;
    port_data_in              : in  std_logic_vector( 7 downto 0);
    -- Interrupt Port
    int_in                    : in  std_logic;
    int_ack_out               : out std_logic
  ); 
end component; 
  
  constant sysclk_period_c    : time                          := 25 ns; 
  
  signal   sysclk             : std_logic                     := '0'; 
  signal   rst_n              : std_logic                     := '0'; 
  
  -- Signale zwischen dem Programmspeicher und der Prozessor
  signal   inst_address       : std_logic_vector( 9 downto 0) := ( others => '0' );
  signal   instruction        : std_logic_vector(17 downto 0) := ( others => '0' );
  
  -- Signale des Output / Inpot Port
  signal   port_id            : std_logic_vector( 7 downto 0) := ( others => '0' );
  signal   port_wr_pulse      : std_logic                     := '0';
  signal   port_data_out      : std_logic_vector( 7 downto 0) := ( others => '0' );
  signal   port_rd_pulse      : std_logic                     := '0';
  signal   port_data_in       : std_logic_vector( 7 downto 0) := ( others => '0' );
  
  -- Signale des Interrupt Port
  signal   int                : std_logic                     := '0';
  signal   int_ack            : std_logic                     := '0';
  
  -- Debug Instruction Decode
  signal   inst_decoded       : string(1 to 19)               := "                   ";

begin  
  
  -- Erzeuge sysclk-Signal 
  system_clock : process 
  begin 
    sysclk <= '0'; 
    wait for sysclk_period_c/2; 
    sysclk <= '1'; 
    wait for sysclk_period_c/2; 
  end process; 
  
  -- Erzeuge rst_n-Signal 
  system_reset : process 
  begin 
    rst_n <= '1'; 
    wait for sysclk_period_c*10; 
    rst_n <= '0'; 
    wait for sysclk_period_c*10; 
    rst_n <= '1'; 
    wait; 
  end process; 
  
  system_interrupt : process
  begin
    int <= '0';
    wait for 1300 ns;
    wait until rising_edge(sysclk);
    int <= '1';
    wait until int_ack <= '1';
    wait until rising_edge(sysclk);
    int <= '0';
  end process; 
 
socket_picocpu_rom : picocpu_rom 
  port map ( 
    clk_in                     => sysclk,
    address_in                 => inst_address,
    instruction_out            => instruction
  ); 
  
socket_picocpu : picocpu 
  port map (  
    clk_in                     => sysclk,
    rst_n_in                   => rst_n,
    -- Signale zum Programmspeicher
    inst_addr_out              => inst_address,
    inst_data_in               => instruction,
    -- Output / Input Port
    port_id_out                => port_id,
    port_wr_pulse_out          => port_wr_pulse,
    port_data_out              => port_data_out, 
    port_rd_pulse_out          => port_rd_pulse,
    port_data_in               => port_data_in,
    -- Interrupt Port
    int_in                     => int,
    int_ack_out                => int_ack 
  ); 
  
  ---------------------------------
  -- DEBUG :: Instruction Decode --
  ---------------------------------
  
sim_debug : process (sysclk, instruction)
  
  variable picocpu_opcode : string(1 to 19);
  
  variable     sx_decode : string(1 to 2);                     --sX register specification
  variable     sy_decode : string(1 to 2);                     --sY register specification
  variable     kk_decode : string(1 to 2);                     --constant value specification
  variable    aaa_decode : string(1 to 3);                     --address specification
    
  function hexcharacter (nibble: std_logic_vector(3 downto 0))
  return character is
  variable hex: character;
  begin
    case nibble is
      when "0000" => hex := '0';
      when "0001" => hex := '1';
      when "0010" => hex := '2';
      when "0011" => hex := '3';
      when "0100" => hex := '4';
      when "0101" => hex := '5';
      when "0110" => hex := '6';
      when "0111" => hex := '7';
      when "1000" => hex := '8';
      when "1001" => hex := '9';
      when "1010" => hex := 'A';
      when "1011" => hex := 'B';
      when "1100" => hex := 'C';
      when "1101" => hex := 'D';
      when "1110" => hex := 'E';
      when "1111" => hex := 'F';
      when others => hex := 'x';
    end case;
    return hex;
  end hexcharacter;
   
  begin
    
    -- decode first register
    sx_decode(1) := 's';
    sx_decode(2) := hexcharacter(instruction(11 downto 8));             

    -- decode second register
    sy_decode(1) := 's';
    sy_decode(2) := hexcharacter(instruction(7 downto 4));  

    -- decode constant value
    kk_decode(1) := hexcharacter(instruction(7 downto 4));
    kk_decode(2) := hexcharacter(instruction(3 downto 0));

    -- address value
    aaa_decode(1) := hexcharacter("00" & instruction(9 downto 8));
    aaa_decode(2) := hexcharacter(instruction(7 downto 4));
    aaa_decode(3) := hexcharacter(instruction(3 downto 0));

    -- decode instruction
    case instruction(17 downto 12) is
      when "000000" => picocpu_opcode := "LOAD " & sx_decode & ',' & kk_decode & "         ";
      when "000001" => picocpu_opcode := "LOAD " & sx_decode & ',' & sy_decode & "         ";
      when "001010" => picocpu_opcode := "AND " & sx_decode & ',' & kk_decode & "          ";
      when "001011" => picocpu_opcode := "AND " & sx_decode & ',' & sy_decode & "          ";
      when "001100" => picocpu_opcode := "OR " & sx_decode & ',' & kk_decode & "           ";
      when "001101" => picocpu_opcode := "OR " & sx_decode & ',' & sy_decode & "           ";
      when "001110" => picocpu_opcode := "XOR " & sx_decode & ',' & kk_decode & "          ";
      when "001111" => picocpu_opcode := "XOR " & sx_decode & ',' & sy_decode & "          ";
      when "010010" => picocpu_opcode := "TEST " & sx_decode & ',' & kk_decode & "         ";
      when "010011" => picocpu_opcode := "TEST " & sx_decode & ',' & sy_decode & "         ";
      when "011000" => picocpu_opcode := "ADD " & sx_decode & ',' & kk_decode & "          ";
      when "011001" => picocpu_opcode := "ADD " & sx_decode & ',' & sy_decode & "          ";
      when "011010" => picocpu_opcode := "ADDCY " & sx_decode & ',' & kk_decode & "        ";
      when "011011" => picocpu_opcode := "ADDCY " & sx_decode & ',' & sy_decode & "        ";
      when "011100" => picocpu_opcode := "SUB " & sx_decode & ',' & kk_decode & "          ";
      when "011101" => picocpu_opcode := "SUB " & sx_decode & ',' & sy_decode & "          ";
      when "011110" => picocpu_opcode := "SUBCY " & sx_decode & ',' & kk_decode & "        ";
      when "011111" => picocpu_opcode := "SUBCY " & sx_decode & ',' & sy_decode & "        ";
      when "010100" => picocpu_opcode := "COMPARE " & sx_decode & ',' & kk_decode & "      ";
      when "010101" => picocpu_opcode := "COMPARE " & sx_decode & ',' & sy_decode & "      ";
      when "100000" => 
        case instruction(3 downto 0) is
          when "0110" => picocpu_opcode := "SL0 " & sx_decode & "             ";
          when "0111" => picocpu_opcode := "SL1 " & sx_decode & "             ";
          when "0100" => picocpu_opcode := "SLX " & sx_decode & "             ";
          when "0000" => picocpu_opcode := "SLA " & sx_decode & "             ";
          when "0010" => picocpu_opcode := "RL " & sx_decode & "              ";
          when "1110" => picocpu_opcode := "SR0 " & sx_decode & "             ";
          when "1111" => picocpu_opcode := "SR1 " & sx_decode & "             ";
          when "1010" => picocpu_opcode := "SRX " & sx_decode & "             ";
          when "1000" => picocpu_opcode := "SRA " & sx_decode & "             ";
          when "1100" => picocpu_opcode := "RR " & sx_decode & "              ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when "101100" => picocpu_opcode := "OUTPUT " & sx_decode & ',' & kk_decode & "       ";
      when "101101" => picocpu_opcode := "OUTPUT " & sx_decode & ",(" & sy_decode & ")     ";
      when "000100" => picocpu_opcode := "INPUT " & sx_decode & ',' & kk_decode & "        ";
      when "000101" => picocpu_opcode := "INPUT " & sx_decode & ",(" & sy_decode & ")      ";
      when "101110" => picocpu_opcode := "STORE " & sx_decode & ',' & kk_decode & "        ";
      when "101111" => picocpu_opcode := "STORE " & sx_decode & ",(" & sy_decode & ")      ";
      when "000110" => picocpu_opcode := "FETCH " & sx_decode & ',' & kk_decode & "        ";
      when "000111" => picocpu_opcode := "FETCH " & sx_decode & ",(" & sy_decode & ")      ";
      when "110100" => picocpu_opcode := "JUMP " & aaa_decode & "           ";
      when "110101" =>
        case instruction(11 downto 10) is
          when "00" => picocpu_opcode := "JUMP Z," & aaa_decode & "         ";
          when "01" => picocpu_opcode := "JUMP NZ," & aaa_decode & "        ";
          when "10" => picocpu_opcode := "JUMP C," & aaa_decode & "         ";
          when "11" => picocpu_opcode := "JUMP NC," & aaa_decode & "        ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when "110000" => picocpu_opcode := "CALL " & aaa_decode & "           ";
      when "110001" =>
        case instruction(11 downto 10) is
          when "00" => picocpu_opcode := "CALL Z," & aaa_decode & "         ";
          when "01" => picocpu_opcode := "CALL NZ," & aaa_decode & "        ";
          when "10" => picocpu_opcode := "CALL C," & aaa_decode & "         ";
          when "11" => picocpu_opcode := "CALL NC," & aaa_decode & "        ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when "101010" => picocpu_opcode := "RETURN             ";
      when "101011" =>
        case instruction(11 downto 10) is
          when "00" => picocpu_opcode := "RETURN Z           ";
          when "01" => picocpu_opcode := "RETURN NZ          ";
          when "10" => picocpu_opcode := "RETURN C           ";
          when "11" => picocpu_opcode := "RETURN NC          ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when "111000" =>
        case instruction(0) is
          when '0' => picocpu_opcode := "RETURNI DISABLE    ";
          when '1' => picocpu_opcode := "RETURNI ENABLE     ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when "111100" =>
        case instruction(0) is
          when '0' => picocpu_opcode := "DISABLE INTERRUPT  ";
          when '1' => picocpu_opcode := "ENABLE INTERRUPT   ";
          when others => picocpu_opcode := "Invalid Instruction";
        end case;
      when others => picocpu_opcode := "Invalid Instruction";
    end case;
    
    inst_decoded <= picocpu_opcode;
    
  end process;   
    
end behavioral; 
