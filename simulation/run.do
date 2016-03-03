# run-file 
    
variable SimTime 3800ns  
  
# Create and map the work library  
vlib rom_init
vmap work rom_init

vcom -93 cpu_rom.vhd

vlib work  
vmap work work  
  
# Compile the VHDL files
vcom -93 ../picocpu_rom.vhd 
vcom -93 ../picocpu.vhd 
vcom -93 picocpu_tb.vhd 
   
# Load the simulator with optimizations turned off 
vsim -novopt work.picocpu_tb 

# Load the Memory with the HEX-File
# Interrupt Program
#mem load -infile call.hex -format hex /picocpu_tb/socket_picocpu_rom/rom
# Register Operation
#mem load -infile rom.hex -format hex /picocpu_tb/socket_picocpu_rom/rom 

do wave.do
  
run $SimTime  
  
wave zoomrange 0 $SimTime 
  
# End simulation 
#quit -sim 
