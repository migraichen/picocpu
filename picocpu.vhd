library ieee; 
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all; 
  
entity picocpu is 
  port (  
    clk_in                    : in  std_logic; 
    rst_n_in                  : in  std_logic;
    -- Signale zumdo  Programmspeicher
    inst_addr_out             : out std_logic_vector( 9 downto 0)     := ( others => '0' ); 
    inst_data_in              : in  std_logic_vector(17 downto 0);
    -- Output / Input Port
    port_id_out               : out std_logic_vector( 7 downto 0)     := ( others => '0' ); 
    port_wr_pulse_out         : out std_logic                         := '0';
    port_data_out             : out std_logic_vector( 7 downto 0)     := ( others => '0' );
    port_rd_pulse_out         : out std_logic                         := '0';
    port_data_in              : in  std_logic_vector( 7 downto 0);
    -- Interrupt Port
    int_in                    : in  std_logic;
    int_ack_out               : out std_logic                         := '0'
  ); 
end picocpu; 
  
architecture behavioral of picocpu is 

  constant reg_num_c     : natural                      := 16;
  constant reg_width_c   : natural                      :=  8;
  constant zero_c        : std_logic_vector(reg_width_c-1 downto 0) := ( others => '0' );
  constant int_vec_c     : std_logic_vector(9 downto 0) := ( others => '1' );

  -- Instructions Codes
  constant load_c        : std_logic_vector(4 downto 0) := "00000";  
  constant input_c       : std_logic_vector(4 downto 0) := "00010";
  constant fetch_c       : std_logic_vector(4 downto 0) := "00011";
  constant and_c         : std_logic_vector(4 downto 0) := "00101";
  constant or_c          : std_logic_vector(4 downto 0) := "00110";
  constant xor_c         : std_logic_vector(4 downto 0) := "00111";
  constant test_c        : std_logic_vector(4 downto 0) := "01001";
  constant compare_c     : std_logic_vector(4 downto 0) := "01010";
  constant add_c         : std_logic_vector(4 downto 0) := "01100";
  constant addcy_c       : std_logic_vector(4 downto 0) := "01101";
  constant sub_c         : std_logic_vector(4 downto 0) := "01110";
  constant subcy_c       : std_logic_vector(4 downto 0) := "01111";
  constant sr_c          : std_logic_vector(4 downto 0) := "10000";
  constant return_c      : std_logic_vector(4 downto 0) := "10101";
  constant output_c      : std_logic_vector(4 downto 0) := "10110";
  constant store_c       : std_logic_vector(4 downto 0) := "10111";
  constant call_c        : std_logic_vector(4 downto 0) := "11000";
  constant jump_c        : std_logic_vector(4 downto 0) := "11010";
  constant returni_c     : std_logic_vector(4 downto 0) := "11100";
  constant interrupt_c   : std_logic_vector(4 downto 0) := "11110";

  constant shift_0       : std_logic_vector(2 downto 0) := "110";
  constant shift_1       : std_logic_vector(2 downto 0) := "111";
  constant shift_x       : std_logic_vector(2 downto 0) := "010";
  constant shift_a       : std_logic_vector(2 downto 0) := "000";
  constant rotate        : std_logic_vector(2 downto 0) := "100";

  signal t_state         : std_logic                    := '0';
    
  signal rst_n           : std_logic                    := '1'; 
  signal pc              : std_logic_vector(9 downto 0) := ( others => '0' );
  signal next_addr       : std_logic_vector(9 downto 0) := ( others => '0' );
  signal stack_top       : std_logic_vector(9 downto 0) := ( others => '0' );
  signal stack_top_p     : std_logic_vector(9 downto 0) := ( others => '0' );
  
  alias opcode           : std_logic_vector(7 downto 0) is inst_data_in(17 downto 10);
  alias inst             : std_logic_vector(4 downto 0) is opcode(7 downto 3);
  alias condition        : std_logic                    is opcode(2);
  alias addr             : std_logic_vector(9 downto 0) is inst_data_in( 9 downto  0);
  alias sX               : std_logic_vector(3 downto 0) is inst_data_in(11 downto  8);
  alias sY               : std_logic_vector(3 downto 0) is inst_data_in( 7 downto  4);
  alias const            : std_logic_vector(7 downto 0) is inst_data_in( 7 downto  0);
  alias direction        : std_logic                    is inst_data_in(3);
  alias sr_code          : std_logic_vector(2 downto 0) is inst_data_in( 2 downto  0);
  alias int_enable       : std_logic                    is inst_data_in(0);  
    
  signal zero            : std_logic                    := '0';                    
  signal not_zero        : std_logic                    := '0';
  signal carry           : std_logic                    := '0';
  signal not_carry       : std_logic                    := '0';
  
  type regs_t is array(0 to reg_num_c-1) of std_logic_vector(reg_width_c-1 downto 0);
  signal regs            : regs_t := ( others => ( others => '0' ));
  signal res             : std_logic_vector(reg_width_c downto 0) := ( others => '0' );
  signal res_reg         : std_logic_vector(reg_width_c downto 0) := ( others => '0' );

  signal int_en_flag     : std_logic                    := '0';
  signal zero_flag       : std_logic                    := '0';
  alias  carry_flag      : std_logic                    is res_reg(res_reg'high);
  signal preserved_zero  : std_logic                    := '0';
  signal preserved_carry : std_logic                    := '0';
  
  constant stack_width_c : natural                      := 5;
  type stack_t is array (0 to (2**stack_width_c)-1) of std_logic_vector(addr'range);
  signal stack           : stack_t := ( others => ( others => '0'));
  signal stack_ptr       : std_logic_vector(stack_width_c-1 downto 0) := ( others => '0' );
  signal stack_wr        : std_logic                    := '0';
  signal stack_rd        : std_logic                    := '0';
  signal stack_data_in   : std_logic_vector(9 downto 0) := ( others => '0' );  
  
  constant scratchpad_width_c : natural                 := const'length;                  
  type scratchpad_t is array (0 to (2**scratchpad_width_c)-1) of std_logic_vector(reg_width_c-1 downto 0);
  signal scratchpad : scratchpad_t := ( others => ( others => '0'));
  
  function parity (din : std_logic_vector) return std_logic is
    variable temp : std_logic := '0';
  begin
    for i in din'range loop
      temp := temp xor din(i);
    end loop;
    return temp;
  end parity;  
  
begin  
  
  -------------------------------------------------------------------------------------
  -- TODO: Allgemain fehlen noch die Scratch Pad lesen und schreiben und             --
  --       Input sowie Output. Alle Befehle Testen.                                  -- 
  -------------------------------------------------------------------------------------
  
  -------------------------------------------------------------------------------------
  -- General Signals                                                                 --
  -------------------------------------------------------------------------------------
  -- TODO: reset nur zurücksetzen aud steigende oder fallende flanken, je nach Takt --
  --       damit t_state zu clk immer passt.                                         --
  -- FEHLER: wenn interrupt genau während eines Jump befehls kommt                  --
  -- FEHLER: wenn interrupt genau während eines call befehls kommt                  --
  --                                                                                 --
  -- KONTROLLIEREN:                                                                  --
  -- was passiert wenn interrupt genau mit einem Jump, call, return Befehl auftritt  -- 
  -------------------------------------------------------------------------------------
  
  -- Reset einsyncronisieren
  process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then 
      rst_n <= rst_n_in;
    end if; 
  end process;
    
  -- picocpu State Maschine (da jeder Befehl genau zwei Takte benoetigen darf)  
  process(clk_in, rst_n)
  begin 
    if rst_n = '0' then
      t_state <= '0';
    elsif clk_in'event and clk_in = '1' then 
      t_state <= not t_state;
    end if;
  end process;  
  
  ----------------------------------------------
  -- Operational Control & Instruction Decode --
  ----------------------------------------------
  
  zero       <= '1' when opcode(1 downto 0) = "00" else '0';
  not_zero   <= '1' when opcode(1 downto 0) = "01" else '0';
  carry      <= '1' when opcode(1 downto 0) = "10" else '0';
  not_carry  <= '1' when opcode(1 downto 0) = "11" else '0';
            
  ---------------------
  -- Program Counter --
  ---------------------
  
  next_addr <= addr        when inst   = jump_c    and condition   = '0' else
               addr        when inst   = jump_c    and condition   = '1' and zero      = '1' and zero_flag  = '1' else
               addr        when inst   = jump_c    and condition   = '1' and not_zero  = '1' and zero_flag  = '0' else 
               addr        when inst   = jump_c    and condition   = '1' and carry     = '1' and carry_flag = '1' else
               addr        when inst   = jump_c    and condition   = '1' and not_carry = '1' and carry_flag = '0' else
               addr        when inst   = call_c    and condition   = '0' else
               addr        when inst   = call_c    and condition   = '1' and zero      = '1' and zero_flag  = '1' else
               addr        when inst   = call_c    and condition   = '1' and not_zero  = '1' and zero_flag  = '0' else 
               addr        when inst   = call_c    and condition   = '1' and carry     = '1' and carry_flag = '1' else
               addr        when inst   = call_c    and condition   = '1' and not_carry = '1' and carry_flag = '0' else
               stack_top_p when inst   = return_c  and condition   = '0' else
               stack_top_p when inst   = return_c  and condition   = '1' and zero      = '1' and zero_flag  = '1' else 
               stack_top_p when inst   = return_c  and condition   = '1' and not_zero  = '1' and zero_flag  = '0' else 
               stack_top_p when inst   = return_c  and condition   = '1' and carry     = '1' and carry_flag = '1' else
               stack_top_p when inst   = return_c  and condition   = '1' and not_carry = '1' and carry_flag = '0' else   
               stack_top   when inst   = returni_c else
               std_logic_vector(unsigned(pc) + 1);   
       
  process(clk_in, rst_n)
  begin 
    if rst_n = '0' then
      pc <= ( others => '0' );
    elsif clk_in'event and clk_in = '1' then 
      if t_state = '0' then
        if int_in = '1' and int_en_flag = '1' then 
          pc <= int_vec_c;
        else
          pc <= next_addr;  
        end if;
      end if;
    end if;
  end process;
  
  inst_addr_out <= pc;
  
  ---------------------------
  -- Program Counter Stack -- 
  ---------------------------
  -- TODO: TESTEN          --
  ---------------------------

  stack_wr <= '1' when t_state = '0' and pc = int_vec_c else
              '1' when t_state = '1' and inst = call_c and condition = '0'                                          else 
              '1' when t_state = '1' and inst = call_c and condition = '1' and zero      = '1' and zero_flag  = '1' else
              '1' when t_state = '1' and inst = call_c and condition = '1' and not_zero  = '1' and zero_flag  = '0' else 
              '1' when t_state = '1' and inst = call_c and condition = '1' and carry     = '1' and carry_flag = '1' else
              '1' when t_state = '1' and inst = call_c and condition = '1' and not_carry = '1' and carry_flag = '0' else 
              '0';

  stack_rd <= '1' when t_state = '0' and inst = return_c and condition = '0' else
              '1' when t_state = '0' and inst = return_c and condition = '1' and zero      = '1' and zero_flag  = '1' else
              '1' when t_state = '0' and inst = return_c and condition = '1' and not_zero  = '1' and zero_flag  = '0' else 
              '1' when t_state = '0' and inst = return_c and condition = '1' and carry     = '1' and carry_flag = '1' else
              '1' when t_state = '0' and inst = return_c and condition = '1' and not_carry = '1' and carry_flag = '0' else
              '1' when t_state = '0' and inst = returni_c else
              '0';

  stack_top   <= std_logic_vector(unsigned(stack(to_integer(unsigned(stack_ptr)-1))));
  stack_top_p <= std_logic_vector(unsigned(stack(to_integer(unsigned(stack_ptr)-1)))+1);
  
  process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then   
      if pc = int_vec_c then 
        stack_data_in <= next_addr;
      else 
        stack_data_in <= pc;
      end if;  
    end if; 
  end process;
  
  process(clk_in, rst_n)
  begin 
    if rst_n = '0' then 
      stack_ptr <= ( others => '0' );
    elsif clk_in'event and clk_in = '1' then
      if stack_wr = '1' then  
        stack(to_integer(unsigned(stack_ptr))) <= stack_data_in; 
        stack_ptr <= std_logic_vector(unsigned(stack_ptr) + 1);
      elsif stack_rd = '1' then 
        stack_ptr <= std_logic_vector(unsigned(stack_ptr) - 1);
      end if;  
    end if;  
  end process;  

  ------------------------------------------------------------
  --   Arithmetic Logic Union                               --
  ------------------------------------------------------------
  -- TODO: Alle Befehle Testen                              --
  ------------------------------------------------------------
  
         -- Logical Group
  res <= '0' & const                                                                                                                                       when inst = load_c    and condition = '0' else
         '0' & regs(to_integer(unsigned(sY)))                                                                                                              when inst = load_c    and condition = '1' else
         '0' & regs(to_integer(unsigned(sX))) and const                                                                                                    when inst = and_c     and condition = '0' else
         '0' & regs(to_integer(unsigned(sX))) and regs(to_integer(unsigned(sY)))                                                                           when inst = and_c     and condition = '1' else
         '0' & regs(to_integer(unsigned(sX))) or const                                                                                                     when inst = or_c      and condition = '0' else
         '0' & regs(to_integer(unsigned(sX))) or regs(to_integer(unsigned(sY)))                                                                            when inst = or_c      and condition = '1' else                           
         '0' & regs(to_integer(unsigned(sX))) xor const                                                                                                    when inst = xor_c     and condition = '0' else
         '0' & regs(to_integer(unsigned(sX))) xor regs(to_integer(unsigned(sY)))                                                                           when inst = xor_c     and condition = '1' else
         parity(res_reg) & regs(to_integer(unsigned(sX))) and const                                                                                        when inst = test_c    and condition = '0' else
         parity(res_reg) & regs(to_integer(unsigned(sX))) and regs(to_integer(unsigned(sY)))                                                               when inst = test_c    and condition = '1' else
         -- Arithmetic Group
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) + unsigned('0' & const))                                                          when inst = add_c     and condition = '0' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) + unsigned('0' & regs(to_integer(unsigned(sY)))))                                 when inst = add_c     and condition = '1' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) + unsigned('0' & const) + unsigned(zero_c & carry_flag))                          when inst = addcy_c   and condition = '0' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) + unsigned('0' & regs(to_integer(unsigned(sY)))) + unsigned(zero_c & carry_flag)) when inst = addcy_c   and condition = '1' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & const))                                                          when inst = sub_c     and condition = '0' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & regs(to_integer(unsigned(sY)))))                                 when inst = sub_c     and condition = '1' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & const) - unsigned(zero_c & carry_flag))                          when inst = subcy_c   and condition = '0' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & regs(to_integer(unsigned(sY)))) - unsigned(zero_c & carry_flag)) when inst = subcy_c   and condition = '1' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & const))                                                          when inst = compare_c and condition = '0' else
         std_logic_vector(unsigned('0' & regs(to_integer(unsigned(sX)))) - unsigned('0' & regs(to_integer(unsigned(sY)))))                                 when inst = compare_c and condition = '1' else
         -- Shift and Rotate Groupe
         regs(to_integer(unsigned(sX)))(sX'right) & '0' & regs(to_integer(unsigned(sX)))(reg_width_c-1 downto 1)                                           when inst = sr_c      and direction = '1' and sr_code = shift_0 else
         regs(to_integer(unsigned(sX)))(sX'right) & '1' & regs(to_integer(unsigned(sX)))(reg_width_c-1 downto 1)                                           when inst = sr_c      and direction = '1' and sr_code = shift_1 else
         regs(to_integer(unsigned(sX)))(sX'right) & regs(to_integer(unsigned(sX)))(sX'left) & regs(to_integer(unsigned(sX)))(reg_width_c-1 downto 1)       when inst = sr_c      and direction = '1' and sr_code = shift_x else
         regs(to_integer(unsigned(sX)))(sX'right) & carry_flag & regs(to_integer(unsigned(sX)))(reg_width_c-1 downto 1)                                    when inst = sr_c      and direction = '1' and sr_code = shift_a else
         regs(to_integer(unsigned(sX)))(sX'right) & regs(to_integer(unsigned(sX)))(sX'right) & regs(to_integer(unsigned(sX)))(reg_width_c-1 downto 1)      when inst = sr_c      and direction = '1' and sr_code = rotate  else
         regs(to_integer(unsigned(sX)))(sX'left)  & regs(to_integer(unsigned(sX)))(reg_width_c-2 downto 0) & '0'                                           when inst = sr_c      and direction = '0' and sr_code = shift_0 else
         regs(to_integer(unsigned(sX)))(sX'left)  & regs(to_integer(unsigned(sX)))(reg_width_c-2 downto 0) & '1'                                           when inst = sr_c      and direction = '0' and sr_code = shift_1 else        
         regs(to_integer(unsigned(sX)))(sX'left)  & regs(to_integer(unsigned(sX)))(reg_width_c-2 downto 0) & regs(to_integer(unsigned(sX)))(sX'right)      when inst = sr_c      and direction = '0' and sr_code = shift_x else        
         regs(to_integer(unsigned(sX)))(sX'left)  & regs(to_integer(unsigned(sX)))(reg_width_c-2 downto 0) & carry_flag                                    when inst = sr_c      and direction = '0' and sr_code = shift_a else                 
         regs(to_integer(unsigned(sX)))(sX'left)  & regs(to_integer(unsigned(sX)))(reg_width_c-2 downto 0) & regs(to_integer(unsigned(sX)))(sX'left)       when inst = sr_c      and direction = '0' and sr_code = rotate  else
         -- Return from Interrupt      
         preserved_carry & zero_c                                                                                                                          when inst = returni_c                                           else 
         ( others => '0' );
          
    process(clk_in, rst_n)
    begin 
      if rst_n = '0' then
        res_reg        <= ( others => '0' );
      elsif clk_in'event and clk_in = '1' then
        if t_state = '0' then
          res_reg <= res;        
        end if;
      end if;
    end process;
  
  process(clk_in, rst_n)
  begin 
    if rst_n = '0' then
      zero_flag <= '0';
    elsif clk_in'event and clk_in = '1' then
      if t_state = '1' then
        if inst = returni_c then
          zero_flag <= preserved_zero;
        elsif res_reg(reg_width_c-1 downto 0) = zero_c then
          zero_flag <= '1';
        else
          zero_flag <= '0';    
        end if;
      end if;
    end if;
  end process;  
  
  ------------------
  -- 16 Registers --
  ------------------
  
  process(clk_in, rst_n)
  begin 
    if rst_n = '0' then
      regs <= ( others => (others => '0' ));
    elsif clk_in'event and clk_in = '1' then 
      if t_state = '1' then
        -- Logical Group
        if inst = load_c and condition = '0' then
          regs(to_integer(unsigned(sX))) <= const;
        elsif inst = load_c and condition = '1' then
          regs(to_integer(unsigned(sX))) <= regs(to_integer(unsigned(sY)));
        elsif inst = and_c or inst = or_c or inst = xor_c then 
          regs(to_integer(unsigned(sX))) <= res_reg(reg_width_c-1 downto 0);
        -- Arithmetic Group
        elsif inst = add_c or inst = addcy_c or inst = sub_c or inst = subcy_c then
          regs(to_integer(unsigned(sX))) <= res_reg(reg_width_c-1 downto 0);
        -- Shiift and Rotate Group 
        elsif inst = sr_c then
          regs(to_integer(unsigned(sX))) <= res_reg(reg_width_c-1 downto 0);
        -- Fetch from Scratch Pad
        elsif inst = fetch_c and condition = '0' then -- constant address
          regs(to_integer(unsigned(sX))) <= scratchpad(to_integer(unsigned(const)));
        elsif inst = fetch_c and condition = '1' then -- register address
          regs(to_integer(unsigned(sX))) <= scratchpad(to_integer(unsigned(sY)));
        end if;  
      end if;
    end if;
  end process;  
  
  ----------------------------------------------
  -- Port Address Control & Input Multiplexer --
  ----------------------------------------------
  
  ------------------------
  -- Scratch Pad Memory --
  ------------------------
  
  process(clk_in, rst_n)
  begin
    if rst_n = '0' then
      scratchpad <= ( others => ( others => '0'));
    elsif clk_in'event and clk_in = '1' then 
      if t_state = '0' then
        if inst = store_c and condition = '0' then
          scratchpad(to_integer(unsigned(const))) <= regs(to_integer(unsigned(sX)));
        elsif inst = store_c and condition = '1' then
          scratchpad(to_integer(unsigned(sY))) <= regs(to_integer(unsigned(sX)));
        end if; 
      end if;
    end if;  
  end process;
    
  ------------------------------------------------------------------
  -- Interrupt Controller                                         --
  ------------------------------------------------------------------
  -- TODO:: Reset flags after jump call return instruction        -- 
  ------------------------------------------------------------------
    
  process(clk_in, rst_n)
  begin
    if rst_n = '0' then
      int_en_flag     <= '0';
      preserved_zero  <= '0';
      preserved_carry <= '0'; 
    elsif clk_in'event and clk_in = '1' then  
      if t_state = '1' then
        if int_in = '1' then
          int_en_flag     <= '0';
          preserved_zero  <= zero_flag;
          preserved_carry <= carry_flag; 
        elsif inst = returni_c then
          int_en_flag <= int_enable;
        elsif inst = interrupt_c then
          int_en_flag <= int_enable;
        end if;    
      end if;
    end if;
  end process;
  
  int_ack_out <= '1' when t_state = '0' and pc = int_vec_c else '0';
  
end behavioral; 
