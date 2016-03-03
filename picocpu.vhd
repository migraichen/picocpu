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

  ------------------------------------
  -- intern verwendete Definitionen --
  ------------------------------------

  constant reg_num_c          : natural                                       := 16;
  constant reg_width_c        : natural                                       :=  8;
  constant inst_depth_c       : natural                                       := 10;
  constant zero_c             : std_logic_vector( reg_width_c-1 downto 0)     := ( others => '0' );
  constant int_vec_c          : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '1' );

  constant opcode_msb_c       : natural                                       := inst_data_in'high;
  constant opcode_width_c     : natural                                       :=  8;
  
  -- Instructions Codes   
  constant inst_msb_c         : natural                                       := inst_data_in'high;
  constant inst_width_c       : natural                                       :=  5;
  -- write to register from (constant, register, read port or scratchpad - "000_00" bis "100_11" )
  constant load_c             : std_logic_vector(inst_width_c-1 downto 0)     := b"000_00"; -- read from constant or register
--constant                    : std_logic_vector(inst_width_c downto 0)       := b"000_01"; -- read from spare input (e.g ext ram or divider/multiplier/other calculator)
  constant input_c            : std_logic_vector(inst_width_c-1 downto 0)     := b"000_10"; -- read from port value, write port id, 
  constant fetch_c            : std_logic_vector(inst_width_c-1 downto 0)     := b"000_11"; -- read from scratchpad
  -- ALU
  -- logical and arithmetik operations
--constant inv_c              : std_logic_vector(inst_width_c-1 downto 0)     := b"001_00"; -- ALU logical not  
  constant and_c              : std_logic_vector(inst_width_c-1 downto 0)     := b"001_01"; -- ALU logical and
  constant or_c               : std_logic_vector(inst_width_c-1 downto 0)     := b"001_10"; -- ALU locigal or 
  constant xor_c              : std_logic_vector(inst_width_c-1 downto 0)     := b"001_11"; -- ALU logical xor
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"010_00"; -- ALU logical ??? ( don't write to register )
  constant test_c             : std_logic_vector(inst_width_c-1 downto 0)     := b"010_01"; -- ALU logical and ( don't write to register )
  constant compare_c          : std_logic_vector(inst_width_c-1 downto 0)     := b"010_10"; -- ALU logical sub ( don't write to register )
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"010_11"; -- ALU logical ??? ( don't write to register )
  constant add_c              : std_logic_vector(inst_width_c-1 downto 0)     := b"011_00"; -- ALU arithmetik add
  constant addcy_c            : std_logic_vector(inst_width_c-1 downto 0)     := b"011_01"; -- ALU arithmetik add with carry
  constant sub_c              : std_logic_vector(inst_width_c-1 downto 0)     := b"011_10"; -- ALU arithmetik sub
  constant subcy_c            : std_logic_vector(inst_width_c-1 downto 0)     := b"011_11"; -- ALU arithmetik sub with carry
  -- shift operations
  constant sr_c               : std_logic_vector(inst_width_c-1 downto 0)     := b"100_00"; -- ALU shift operation
  -- constant                 : std_logic_vector(inst_width_c-1 downto 0)     := b"100_01"; -- ALU spare (e.g. divider/multiplier/other calculator) 
  -- constant                 : std_logic_vector(inst_width_c-1 downto 0)     := b"100_10"; -- ALU spare (e.g. divider/multiplier/other calculator)
  -- constant                 : std_logic_vector(inst_width_c-1 downto 0)     := b"100_11"; -- ALU spare (e.g. divider/multiplier/other calculator)
  -- address operation (no write to register - "1010_0" bis "1111_1" )
  -- constant                 : std_logic_vector(inst_width_c-1 downto 0)     := b"1010_0"; -- spare
  constant return_c           : std_logic_vector(inst_width_c-1 downto 0)     := b"1010_1"; -- jump to stack address
  -- write operations
  constant output_c           : std_logic_vector(inst_width_c-1 downto 0)     := b"1011_0"; -- write port id and port value
  constant store_c            : std_logic_vector(inst_width_c-1 downto 0)     := b"1011_1"; -- write in scratchpad
  -- (PC operation) Jump to address
  constant call_c             : std_logic_vector(inst_width_c-1 downto 0)     := b"1100_0"; -- Jump to address, store pc to stack
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"1100_1"; --
  constant jump_c             : std_logic_vector(inst_width_c-1 downto 0)     := b"1101_0"; -- Jump to address
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"1101_1"; -- 
  constant returni_c          : std_logic_vector(inst_width_c-1 downto 0)     := b"1110_0"; -- Jump from interrupt, reload flags
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"1110_1"; -- 
  
  -- Setzt oder lösche Flags
  constant interrupt_c        : std_logic_vector(inst_width_c-1 downto 0)     := b"1111_0"; -- enable / disable interrupts
--constant                    : std_logic_vector(inst_width_c-1 downto 0)     := b"1111_1"; -- divide or multiply

  -- Es fehlen noch überlegungen zu einer Memory management Unit 
  -- leider finde ich dazu gerade keine Informationen


  constant sr_code_width_c    : natural :=  3;
  -- ACHTUNG :: sub Codes für Shift right und shift Left sind nicht gleich !!! (x und rotate sind vertauscht)
  -- Vermutlich wegen des Rotierens in der ALU
  constant shift_a            : std_logic_vector(sr_code_width_c-1 downto 0)  := "000";
  constant shift_x            : std_logic_vector(sr_code_width_c-1 downto 0)  := "010";
  constant rotate             : std_logic_vector(sr_code_width_c-1 downto 0)  := "100";
  constant shift_0            : std_logic_vector(sr_code_width_c-1 downto 0)  := "110";
  constant shift_1            : std_logic_vector(sr_code_width_c-1 downto 0)  := "111";
    
  -- Positionen wo in dem Befehl welche Informationen zu finden sind
  constant condition_pos_c    : natural                                       := inst_data_in'high-inst_width_c;
  constant addr_lsb_c         : natural                                       :=  0;
  constant addr_width_c       : natural                                       := inst_depth_c;
  constant sX_msb_c           : natural                                       := inst_data_in'high-inst_width_c-1;
  constant sX_width_c         : natural                                       :=  4;
  constant sY_msb_c           : natural                                       := inst_data_in'high-inst_width_c-sX_width_c-1;
  constant sY_width_c         : natural                                       :=  4;
  constant const_lsb_c        : natural                                       :=  0;
  constant const_width_c      : natural                                       := reg_width_c;
  constant direction_pos_c    : natural                                       :=  3;
  constant sr_code_lsb_c      : natural                                       :=  0;
  constant int_enable_pos_c   : natural                                       :=  0;
  
  -- Berechne die Bereiche der obrigen Informationen  
  subtype opcode_r  is integer range opcode_msb_c                    downto opcode_msb_c-opcode_width_c+1;
  subtype inst_r    is integer range inst_msb_c                      downto inst_msb_c-inst_width_c+1;
  subtype addr_r    is integer range addr_lsb_c+addr_width_c-1       downto addr_lsb_c;
  subtype sX_r      is integer range sX_msb_c                        downto sX_msb_c-sX_width_c+1;
  subtype sY_r      is integer range sY_msb_c                        downto sY_msb_c-sX_width_c+1;
  subtype const_r   is integer range const_lsb_c+const_width_c-1     downto const_lsb_c; 
  subtype sr_code_r is integer range sr_code_lsb_c+sr_code_width_c-1 downto sr_code_lsb_c;  
  
  -- intern verwendete Bezeichnungen
  alias opcode                : std_logic_vector( opcode_width_c-1 downto 0)  is inst_data_in(opcode_r);          -- 17 downto 10
  alias inst                  : std_logic_vector(   inst_width_c-1 downto 0)  is inst_data_in(inst_r);            -- 17 downto 13
  alias condition             : std_logic                                     is inst_data_in(condition_pos_c);   -- 12
  alias addr                  : std_logic_vector(   addr_width_c-1 downto 0)  is inst_data_in(addr_r);            --  9 downto 0
  alias sX                    : std_logic_vector(     sX_width_c-1 downto 0)  is inst_data_in(sX_r);              -- 11 downto 8
  alias sY                    : std_logic_vector(     sY_width_c-1 downto 0)  is inst_data_in(sY_r);              --  7 downto 4
  alias const                 : std_logic_vector(    reg_width_c-1 downto 0)  is inst_data_in(const_r);           --  7 downto 0 
  alias direction             : std_logic                                     is inst_data_in(direction_pos_c);   --  3
  alias sr_code               : std_logic_vector(sr_code_width_c-1 downto 0)  is inst_data_in(sr_code_r);         --  2 downto 0
  alias int_enable            : std_logic                                     is inst_data_in(int_enable_pos_c);  --  0
    
  -------------------------------  
  -- intern verwendete Signale --  
  -------------------------------  
    
  type t_state is ( get, set ); -- PicoCPU State Maschine
  signal state                : t_state                                       := get;  
    
  signal rst_n                : std_logic                                     := '1'; 
  signal int                  : std_logic                                     := '0';
  signal pc                   : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );
  signal next_addr            : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );
  signal last_addr            : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );    
  signal cond_valid           : std_logic                                     := '0';  
  
  signal zero                 : std_logic                                     := '0';                    
  signal not_zero             : std_logic                                     := '0';
  signal carry                : std_logic                                     := '0';
  signal not_carry            : std_logic                                     := '0';
  
  signal alu_a                : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  signal alu_b                : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  signal alu_c                : std_logic                                     := '0';
  signal res                  : std_logic_vector(reg_width_c downto 0)        := ( others => '0' );
  signal res_reg              : std_logic_vector(reg_width_c downto 0)        := ( others => '0' );

  signal int_en_flag          : std_logic                                     := '0';
  signal zero_flag            : std_logic                                     := '0';
  alias  carry_flag           : std_logic                                     is res_reg(res_reg'high);
  signal preserved_zero       : std_logic                                     := '0';
  signal preserved_carry      : std_logic                                     := '0';
  
  attribute ram_style         : string;
  attribute keep              : string;
  
  type regs_t is array(0 to reg_num_c-1) of std_logic_vector(reg_width_c-1 downto 0);
  signal regs_x               : regs_t                                        := ( others => ( others => '0' ));
  signal regs_y               : regs_t                                        := ( others => ( others => '0' ));
  attribute keep              of regs_x                                       : signal is "true";
  attribute keep              of regs_y                                       : signal is "true";
  attribute ram_style         of regs_x                                       : signal is "distributed";
  attribute ram_style         of regs_y                                       : signal is "distributed";
  signal wr_reg               : std_logic                                     := '0';
  signal reg_in_addr          : std_logic_vector(3 downto 0)                  := ( others => '0' );
  signal reg_in               : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  signal reg_x_out_addr       : std_logic_vector(3 downto 0)                  := ( others => '0' );  
  signal reg_x_out            : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );   
  signal reg_y_out_addr       : std_logic_vector(3 downto 0)                  := ( others => '0' );
  signal reg_y_out            : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  
  constant stack_width_c      : natural                                       := 5;
  type stack_t is array (0 to (2**stack_width_c)-1) of std_logic_vector(addr'range);
  signal stack                : stack_t                                       := ( others => ( others => '0'));
  attribute keep              of stack                                        : signal is "true";
  attribute ram_style         of stack                                        : signal is "distributed";
  signal stack_ptr            : std_logic_vector(stack_width_c-1 downto 0)    := ( others => '0' );
  signal stack_wr             : std_logic                                     := '0';
  signal stack_rd             : std_logic                                     := '0';
  signal stack_data_in        : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );  
  signal stack_top            : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );
  signal stack_top_p          : std_logic_vector(inst_depth_c-1 downto 0)     := ( others => '0' );
  
  signal port_out_en          : std_logic                                     := '0';
  signal port_in_en           : std_logic                                     := '0';
  signal port_id_en           : std_logic                                     := '0';
  signal port_id              : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  signal port_data_i          : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );  
  signal port_data_o          : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  
  constant scratchpad_width_c : natural                                       := 2;                  
  type scratchpad_t is array (0 to (2**scratchpad_width_c)-1) of std_logic_vector(reg_width_c-1 downto 0);
  signal scratchpad           : scratchpad_t                                  := ( others => ( others => '0'));
  attribute keep              of scratchpad                                   : signal is "true";
  attribute ram_style         of scratchpad                                   : signal is "distributed";
  signal wr_scratchpad        : std_logic                                     := '0';
  signal scratchpad_addr      : std_logic_vector(scratchpad_width_c-1 downto 0):= ( others => '0' );
  signal scratchpad_in        : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  signal scratchpad_out       : std_logic_vector(reg_width_c-1 downto 0)      := ( others => '0' );
  
  function parity (din : std_logic_vector) return std_logic is
    variable temp : std_logic := '0';
  begin
    for i in din'range loop
      temp := temp xor din(i);
    end loop;
    return temp;
  end parity;  
  
begin  
  
  -- TODO:: 
  --        Interrupts nochmal überprüfen, Interrups zeitgleich mit call, jump, return ???
  --        Dynamischer gestallten: Registerbreite 4, 8, 16, 32, 64, 128
  --                                Anzahl der Register, 
  --                                Größe des Scratchpad
  --        Überlegungen zu prozessor skalierung 
  --        
  -- Slice Logic Utilization: 
  -- Number of Slice Registers:              89  out of  54576     0%  
  -- Number of Slice LUTs:                  337  out of  27288     1%  
  --    Number used as Logic:               301  out of  27288     1%  
  --    Number used as Memory:               36  out of   6408     0%  
  --       Number used as RAM:               36
  ------------------------------------------------------------------
  -- General Signals                                              --
  ------------------------------------------------------------------   
  
  -- Reset und Interrupt Einsynchronisieren
  socket_sync_ext_signal : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then 
      rst_n <= rst_n_in;
      int   <= int_in;
    end if; 
  end process;
    
  -- PicoCPU State Maschine (da jeder Befehl genau zwei Takte benoetigen darf)  
  socket_picocpu_state_maschine : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then 
      if rst_n = '0' then
        state <= get;
      else
        case state is 
          when get => state <= set;
          when set => state <= get;
        end case;
      end if;
    end if;
  end process;  
  
  ------------------------------------------------------------------
  -- Operational Control & Instruction Decode                     --
  ------------------------------------------------------------------ 
  
  zero          <= '1' when opcode(1 downto 0) = "00" else '0';
  not_zero      <= '1' when opcode(1 downto 0) = "01" else '0';
  carry         <= '1' when opcode(1 downto 0) = "10" else '0';
  not_carry     <= '1' when opcode(1 downto 0) = "11" else '0';
             
  ------------------------------------------------------------------
  -- Instruction Decode                                           --
  ------------------------------------------------------------------   
	
  cond_valid    <= '1' when condition = '0'                                            else 
                   '1' when condition = '1' and zero        = '1' and zero_flag  = '1' else
                   '1' when condition = '1' and not_zero    = '1' and zero_flag  = '0' else 
                   '1' when condition = '1' and carry       = '1' and carry_flag = '1' else
                   '1' when condition = '1' and not_carry   = '1' and carry_flag = '0' else
                   '0';
	
  stack_wr      <= '1' when state = get and pc   = int_vec_c                           else
                   '1' when state = get and inst = call_c   and cond_valid = '1'       else 
                   '0';

  stack_rd      <= '1' when state = get and inst = return_c and cond_valid = '1'       else
                   '1' when state = get and inst = returni_c                           else
                   '0';	
  
  wr_reg        <= '1' when state = set and inst = load_c   else 
                   '1' when state = set and inst = input_c  else
			             '1' when state = set and inst = fetch_c  else
			             '1' when state = set and inst = and_c    else
			             '1' when state = set and inst = or_c     else
			             '1' when state = set and inst = xor_c    else
			             '1' when state = set and inst = add_c    else
			             '1' when state = set and inst = addcy_c  else
			             '1' when state = set and inst = sub_c    else
			             '1' when state = set and inst = subcy_c  else
			             '1' when state = set and inst = sr_c     else
			             '0';  
  
  port_out_en   <= '1' when state = get and inst = output_c else '0';
  port_in_en    <= '1' when state = get and inst = input_c  else '0';
	port_id_en    <= port_out_en or port_in_en;	  
			  
  wr_scratchpad <= '1' when state = set and inst = store_c  else '0'; 
  
	------------------------------------------------------------------
  -- Program Counter                                              --
  ------------------------------------------------------------------    
  
  next_addr     <= addr        when inst    = jump_c    and cond_valid   = '1'    else
                   addr        when inst    = call_c    and cond_valid   = '1'    else
                   stack_top_p when inst    = return_c  and cond_valid   = '1'    else 
                   stack_top   when inst    = returni_c                           else
                   std_logic_vector(unsigned(pc) + 1);  
  
  socket_test : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then    
      if state = get then 
        last_addr <= next_addr;
      end if;
    end if;
  end process;
  
  socket_program_counter : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then 
      if rst_n = '0' then
        pc <= ( others => '0' );
      else
        if state = get then
          if int = '1' and int_en_flag = '1' then 
            pc <= int_vec_c;
          else
            pc <= next_addr;          
          end if;
        end if;  
      end if;
    end if;
  end process;
  
  inst_addr_out <= pc;
  
  ------------------------------------------------------------------
  -- Program Counter Stack                                        --
  ------------------------------------------------------------------  
  
  stack_top     <= std_logic_vector(unsigned(stack(to_integer(unsigned(stack_ptr)-1))));
  stack_top_p   <= std_logic_vector(unsigned(stack_top)+1);
  
  stack_data_in <= last_addr when pc = int_vec_c else pc;  
  
  socket_stack_ptr : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then
      if rst_n = '0' then 
        stack_ptr <= ( others => '0' );
      else
        if stack_wr = '1' then  
          stack(to_integer(unsigned(stack_ptr))) <= stack_data_in; 
          stack_ptr <= std_logic_vector(unsigned(stack_ptr) + 1);
        elsif stack_rd = '1' then 
          stack_ptr <= std_logic_vector(unsigned(stack_ptr) - 1);
        end if;
	  end if;  
	end if;  
  end process; 
  
  ------------------------------------------------------------------
  -- Arithmetic Logic Union                                       --
  ------------------------------------------------------------------

  alu_a         <=                 reg_x_out;
  alu_b         <=                      const when condition = '0'                   else 
                                   reg_y_out;
  alu_c         <=            parity(res_reg) when inst = test_c                     else 
                                          '0' when inst = sr_c and sr_code = shift_0 else
                                          '1' when inst = sr_c and sr_code = shift_1 else
                    reg_x_out(reg_x_out'left) when inst = sr_c and sr_code = shift_x else
                                   carry_flag when inst = sr_c and sr_code = shift_a else
                   reg_x_out(reg_x_out'right) when inst = sr_c and sr_code = rotate  else
                                         '0';         
           
  res           <= alu_c & alu_a and alu_c & alu_b                                                                     when inst = and_c                         else 
                   alu_c & alu_a or  alu_c & alu_b                                                                     when inst = or_c                          else 
                   alu_c & alu_a xor alu_c & alu_b                                                                     when inst = xor_c                         else
                   alu_c & alu_a and alu_c & alu_b                                                                     when inst = test_c                        else
                   -- Arithmetic Group
                   std_logic_vector(unsigned(alu_c & alu_a) + unsigned(alu_c & alu_b))                                 when inst = add_c                         else        
                   std_logic_vector(unsigned(alu_c & alu_a) + unsigned(alu_c & alu_b) + unsigned(zero_c & carry_flag)) when inst = addcy_c                       else 
                   std_logic_vector(unsigned(alu_c & alu_a) - unsigned(alu_c & alu_b))                                 when inst = sub_c                         else 
                   std_logic_vector(unsigned(alu_c & alu_a) - unsigned(alu_c & alu_b) - unsigned(zero_c & carry_flag)) when inst = subcy_c                       else 
                   std_logic_vector(unsigned(alu_c & alu_a) - unsigned(alu_c & alu_b))                                 when inst = compare_c                     else        
                   -- Shift and Rotate Groupe
                   alu_a(alu_a'right) & alu_c & alu_a(reg_width_c-1 downto 1)                                          when inst = sr_c      and direction = '1' else
                   alu_a(alu_a'left)  & alu_a(reg_width_c-2 downto 0) & alu_c                                          when inst = sr_c      and direction = '0' else  
                   -- Return from Interrupt      
                   preserved_carry & zero_c                                                                            when inst = returni_c                     else 
                   ( others => '0' );    
  
  socket_alu : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then
      if rst_n = '0' then
        res_reg   <= ( others => '0' );
      else
        if state = get then
          res_reg <= res;        
        end if;
      end if;
    end if;
  end process;
 
  socket_flags : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then
      if rst_n = '0' then
        zero_flag <= '0';
      else
        if inst = returni_c and state = set then
          zero_flag <= preserved_zero;
        elsif res_reg(reg_width_c-1 downto 0) = zero_c then
          zero_flag <= '1';
        else
          zero_flag <= '0';    
        end if;
      end if;
    end if;
  end process; 
 
  -------------------------------------------------------------------------------------------------
  -- 16 Registers -- 2x SLICEM / MLAB used as Dual-Port RAM on Spartan 6 / ARRIA II              --
  -------------------------------------------------------------------------------------------------
  -- Hier werden die Register doppelt abgespeichert, das benötigt 9 Slices mehr                  --
  -- hat aber den Vorteil, das mit sX und sY gleichzeitig zwei Register ausgelesen werden können --
  -------------------------------------------------------------------------------------------------
  
  reg_in_addr    <= sX;
  reg_in         <= const           when inst = load_c  and condition = '0' else 
                    reg_y_out       when inst = load_c  and condition = '1' else 
				            scratchpad_out  when inst = fetch_c                     else
				            port_data_i     when inst = input_c                     else
				            res_reg(reg_in'range);
	
  reg_x_out_addr <= sX;
  reg_y_out_addr <= sY;
  reg_x_out      <= regs_x(to_integer(unsigned(reg_x_out_addr)));
  reg_y_out      <= regs_y(to_integer(unsigned(reg_y_out_addr)));
  
  socket_regs_x : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then 
      if wr_reg = '1' then
        regs_x(to_integer(unsigned(reg_in_addr))) <= reg_in;
		  end if;
    end if;
  end process;    

  socket_regs_y : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then 
      if wr_reg = '1' then
        regs_y(to_integer(unsigned(reg_in_addr))) <= reg_in;
		  end if;
    end if;
  end process;    
  
  ------------------------------------------------------------------
  -- Port Address Control                                         --
  ------------------------------------------------------------------
  
  port_id     <= const     when inst = output_c and condition = '0' else 
                 reg_y_out when inst = output_c and condition = '1' else 
			           const     when inst = input_c  and condition = '0' else
			           reg_y_out when inst = input_c  and condition = '1' else 
			           ( others => '0' );
                 
  port_data_o <= reg_x_out;
 
  
  socket_port_id_ctrl : process(clk_in)
  begin 
    if clk_in'event and clk_in = '1' then 
      if rst_n = '0' then
        port_id_out       <= ( others => '0' );
      else
        if port_id_en = '1' then 
          port_id_out       <= port_id; 
        else 
          port_id_out       <= ( others => '0' ); 
        end if;   
      end if;
    end if;  
  end process;
  
  socket_port_output_ctrl : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then 
      if rst_n = '0' then 
        port_wr_pulse_out <= '0';
        port_data_out     <= ( others => '0' );
      else
	      if port_out_en = '1' then
		      port_wr_pulse_out <= '1';
          port_data_out     <= port_data_o;	    
		    else 
		      port_wr_pulse_out <= '0';
          port_data_out     <= ( others => '0' );
		    end if;
      end if;
	  end if;  
  end process; 

  socket_port_input_ctrl : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then 
      if rst_n = '0' then
        port_rd_pulse_out <= '0';
      else
        if port_in_en = '1' then 
          port_rd_pulse_out <= '1';
        else 
          port_rd_pulse_out <= '0';
        end if; 
      end if;
    end if;      
  end process;
  
  port_data_i  <= port_data_in;
 
  ------------------------------------------------------------------
  -- Scratch Pad Memory -- SLICEM / MLAB used as Singel-Port RAM  --
  ------------------------------------------------------------------
    
  scratchpad_addr  <= const(scratchpad_addr'range) when condition = '0' else reg_x_out(scratchpad_addr'range);	 
  scratchpad_in    <= reg_x_out;	
  scratchpad_out   <= scratchpad(to_integer(unsigned(scratchpad_addr)));
  
  socket_scratchpad : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then 
      if wr_scratchpad = '1' then
        scratchpad(to_integer(unsigned(scratchpad_addr))) <= scratchpad_in;
      end if;
    end if;  
  end process;
  
  ------------------------------------------------------------------
  -- Interrupt Controller                                         --
  ------------------------------------------------------------------
    
  socket_interrupt_shadow_flags : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then  
      if rst_n = '0' then
        preserved_zero  <= '0';
        preserved_carry <= '0'; 
      else
        if int = '1' and pc = int_vec_c and state = set then
          preserved_zero  <= zero_flag;
          preserved_carry <= carry_flag; 
        end if;
      end if;
    end if;
  end process;
  
  socket_interrupt_enable_flag : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then  
      if rst_n = '0' then
        int_en_flag     <= '0';
      else
        if pc = int_vec_c then
          int_en_flag     <= '0';
	      elsif inst = returni_c then
          int_en_flag     <= int_enable;
        elsif inst = interrupt_c then
          int_en_flag     <= int_enable;
        end if;    
      end if;
    end if;
  end process;
  
  socket_int_ack : process(clk_in)
  begin
    if clk_in'event and clk_in = '1' then  
      if state = set and pc = int_vec_c then
        int_ack_out <= '1';
	    else
        int_ack_out <= '0';
      end if;
    end if;
  end process;   
  
end behavioral; 
