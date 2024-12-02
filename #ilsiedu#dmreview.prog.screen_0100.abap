PROCESS BEFORE OUTPUT.
  MODULE status_0100.
  MODULE modify_screen.

  MODULE get_subscreen.

  CALL SUBSCREEN subscreen INCLUDING gv_repid gv_dynnr.

PROCESS AFTER INPUT.
  MODULE user_exit_command_0100 AT EXIT-COMMAND.

  CALL SUBSCREEN subscreen.

  MODULE user_command_0100.

PROCESS ON VALUE-REQUEST.
  FIELD: file MODULE f4_file.
