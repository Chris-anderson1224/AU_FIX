*&---------------------------------------------------------------------*
*& Include          /ILSIEDU/DMREVIEWO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  CHECK gv_init = abap_true.
  gv_init = abap_false.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GS_RUNMODE-RUNMODE'
      values          = gt_rmdd
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module GET_SUBSCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE get_subscreen OUTPUT.

  CHECK gv_newrm = abap_true.
  gv_newrm = abap_false.

  CLEAR: gv_repid, gv_dynnr.

  TRY.
      "Set Class
      DATA(lv_cls) = gt_runmode[ runmode = gs_runmode-runmode ]-class.
      CREATE OBJECT go_rmclass TYPE (lv_cls).

      "Get Program/Dynnr
      go_rmclass->get_dynnr( IMPORTING ev_dynnr = gv_dynnr ev_repid = gv_repid ).
    CATCH cx_root.
  ENDTRY.

  IF gv_dynnr IS INITIAL OR gv_repid IS INITIAL.
    PERFORM generic_subscreen.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-group1 IS NOT INITIAL.

    CASE action.
      WHEN 'U'. "Upload
        screen-active = COND #( WHEN screen-group1 = 'FIL' THEN 1 ELSE 0 ).
      WHEN 'M'. "Match
        screen-active = COND #( WHEN screen-group1 = 'MAT' THEN 1 ELSE 0 ).
      WHEN 'R' OR 'A'. "Review/Update All
        screen-active = COND #( WHEN screen-group1 = 'REV' THEN 1 ELSE 0 ).
      WHEN OTHERS. "Initial
        screen-active = 0.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  IF go_grid IS NOT BOUND.

    PERFORM log_run.
    lcl_grid=>init_grid( ).

    SET PF-STATUS 'STATUS_0200'.
    SET TITLEBAR 'TITLE_0200'.

  ELSEIF gv_set_data = abap_true.
    go_grid->set_data( ).
  ENDIF.
ENDMODULE.
