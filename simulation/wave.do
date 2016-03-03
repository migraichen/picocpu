onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /picocpu_tb/sysclk
add wave -noupdate /picocpu_tb/socket_picocpu/rst_n
add wave -noupdate -divider ROM
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu_rom/address_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu_rom/instruction_out
add wave -noupdate -divider internal
add wave -noupdate /picocpu_tb/socket_picocpu/int_en_flag
add wave -noupdate /picocpu_tb/socket_picocpu/int_in
add wave -noupdate /picocpu_tb/socket_picocpu/int
add wave -noupdate /picocpu_tb/socket_picocpu/int_ack_out
add wave -noupdate /picocpu_tb/inst_decoded
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/pc
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/next_addr
add wave -noupdate /picocpu_tb/socket_picocpu/last_addr
add wave -noupdate -divider State
add wave -noupdate /picocpu_tb/socket_picocpu/state
add wave -noupdate -divider Register
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/wr_reg
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_in_addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/regs_x
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_x_out_addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_x_out
add wave -noupdate -radix unsigned /picocpu_tb/socket_picocpu/regs_y
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_y_out_addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/reg_y_out
add wave -noupdate -divider Stack
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_wr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_ptr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_data_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_rd
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_top
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_top_p
add wave -noupdate -divider Scratchpad
add wave -noupdate /picocpu_tb/socket_picocpu/wr_scratchpad
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/scratchpad_addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/scratchpad_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/scratchpad
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/scratchpad_out
add wave -noupdate -divider {I/O Port}
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_wr_pulse_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_id_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_data_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_rd_pulse_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_id_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_data_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/port_data_i
add wave -noupdate -divider ALU
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/res
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/res_reg
add wave -noupdate /picocpu_tb/socket_picocpu/carry_flag
add wave -noupdate /picocpu_tb/socket_picocpu/zero_flag
add wave -noupdate /picocpu_tb/sysclk
add wave -noupdate /picocpu_tb/inst_decoded
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/pc
add wave -noupdate /picocpu_tb/socket_picocpu/carry
add wave -noupdate /picocpu_tb/socket_picocpu/zero
add wave -noupdate /picocpu_tb/socket_picocpu/preserved_carry
add wave -noupdate /picocpu_tb/socket_picocpu/preserved_zero
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1012500 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 283
configure wave -valuecolwidth 449
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits us
update
WaveRestoreZoom {893750 ps} {1131250 ps}
