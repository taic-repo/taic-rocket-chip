set proj_path [lindex $argv 0]
set xpr [lindex $argv 1]
set xsa [lindex $argv 2]

open_project $proj_path/$xpr
set_property top system_wrapper [current_fileset]
reset_run synth_1
reset_run impl_1
launch_runs synth_1 -jobs 6
wait_on_run synth_1
launch_runs impl_1 -to_step write_bitstream -jobs 6
wait_on_run impl_1
write_hw_platform -fixed -include_bit -force -file $proj_path/$xsa
close_project