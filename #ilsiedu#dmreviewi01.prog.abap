*&---------------------------------------------------------------------*
*& Include          /ILSIEDU/DMREVIEWI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'BACK'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'RUNM'.
      READ TABLE gt_runmode INTO gs_runmode WITH KEY runmode = gs_runmode-runmode.
      gv_newrm = abap_true.
    WHEN 'EXECUTE'.
      "Perform based on action
      gv_stop = abap_false.
      CASE action.
        WHEN 'U'. "Upload new file
          PERFORM check_runid.

          PERFORM validate_file.

          PERFORM load_data.

          CHECK go_data IS NOT INITIAL.

          PERFORM save_data.
        WHEN 'M'. "Match records to records in DB
          "Typically would run in background...
          PERFORM match_records.
        WHEN 'A'. "Update all records for a run
          "Typically would run in background...
          PERFORM get_run.
          PERFORM process_all.
        WHEN 'R'. "Review all/some records for a run
          PERFORM get_run.
          PERFORM build_alv_tab.
          CHECK <fs_table> IS ASSIGNED.
          "Display ALV
          CALL SCREEN 0200.
          "Use class to display errors, but run processing from here
      ENDCASE.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      lcl_grid=>free_grid( ).
      FREE go_grid.
      PERFORM set_end.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_FILE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_file INPUT.
  PERFORM f4_file.
ENDMODULE.
