onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /picocpu_tb/sysclk
add wave -noupdate /picocpu_tb/socket_picocpu/rst_n
add wave -noupdate /picocpu_tb/socket_picocpu/t_state
add wave -noupdate /picocpu_tb/socket_picocpu/int_in
add wave -noupdate /picocpu_tb/inst_decoded
add wave -noupdate /picocpu_tb/socket_picocpu/int_ack_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/inst_addr_out
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/next_addr
add wave -noupdate /picocpu_tb/socket_picocpu/stack_wr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_data_in
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_ptr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/stack_top
add wave -noupdate /picocpu_tb/socket_picocpu/stack_rd
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/addr
add wave -noupdate -radix hexadecimal /picocpu_tb/socket_picocpu/int_ret_addr
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1336594 ps} 0}
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
WaveRestoreZoom {0 ps} {3800 ns}
