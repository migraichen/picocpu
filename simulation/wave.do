onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /picocpu_tb/sysclk
add wave -noupdate /picocpu_tb/rst_n
add wave -noupdate /picocpu_tb/int
add wave -noupdate /picocpu_tb/int_ack
add wave -noupdate /picocpu_tb/socket_picocpu/int_en_flag
add wave -noupdate -divider {Instruktion ROM}
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu_rom/address_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu_rom/instruction_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu_rom/rom
add wave -noupdate -divider picocpu
add wave -noupdate /picocpu_tb/inst_decoded
add wave -noupdate /picocpu_tb/socket_picocpu/t_state
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/pc
add wave -noupdate /picocpu_tb/socket_picocpu/rst_n
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/regs
add wave -noupdate /picocpu_tb/socket_picocpu/res
add wave -noupdate /picocpu_tb/socket_picocpu/zero_flag
add wave -noupdate /picocpu_tb/socket_picocpu/carry_flag
add wave -noupdate /picocpu_tb/socket_picocpu/addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_ptr
add wave -noupdate /picocpu_tb/socket_picocpu/res
add wave -noupdate /picocpu_tb/socket_picocpu/preserved_zero
add wave -noupdate /picocpu_tb/socket_picocpu/preserved_carry
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/scratchpad
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_ptr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/pc
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/next_addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_top
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_top_p
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/inst_addr_out
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {925818 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 283
configure wave -valuecolwidth 164
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
WaveRestoreZoom {819607 ps} {1294607 ps}
