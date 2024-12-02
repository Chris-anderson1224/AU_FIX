*&---------------------------------------------------------------------*
*& Include          /ILSIEDU/DMREVIEWF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_program .

  gv_init = abap_true.

  SELECT *
    FROM /ilsiedu/dmrm AS m
    INNER JOIN /ilsiedu/dmrmt AS t
      ON m~runmode = t~runmode
      AND t~spras = sy-langu
    INTO CORRESPONDING FIELDS OF TABLE gt_runmode.

  LOOP AT gt_runmode INTO DATA(ls_rm) WHERE active = abap_true AND class IS NOT INITIAL.
    APPEND VALUE #( key = ls_rm-runmode text = ls_rm-text ) TO gt_rmdd.
  ENDLOOP.

  SELECT action AS key
    FROM /ilsiedu/dmractn
    INTO CORRESPONDING FIELDS OF TABLE gt_actndd.

  DATA: lt_domval TYPE TABLE OF dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = '/ILSIEDU/DMR_ACTION'
      text      = abap_true
      langu     = sy-langu
    TABLES
      dd07v_tab = lt_domval.

  SELECT *
    FROM /ilsiedu/dmractn
    INTO TABLE @DATA(lt_actn)
    WHERE active = @abap_true.

  LOOP AT gt_actndd ASSIGNING FIELD-SYMBOL(<fs_a>).
    READ TABLE lt_domval INTO DATA(ls_d) WITH KEY domvalue_l = <fs_a>-key.
    CHECK sy-subrc = 0.
    READ TABLE lt_actn TRANSPORTING NO FIELDS WITH KEY action = <fs_a>-key.
    CHECK sy-subrc = 0.
    <fs_a>-text = ls_d-ddtext.
  ENDLOOP.

  DELETE gt_actndd WHERE text IS INITIAL.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'ACTION'
      values = gt_actndd.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_file .
  CHECK file IS NOT INITIAL.
  CHECK gv_stop = abap_false.
  /ilsiedu/cl_excel_upload=>validate_template( iv_filepath = file iv_struc = go_rmclass->get_upload_struc( ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_file .

  DATA: lt_filelist TYPE filetable,
        lv_rc       TYPE i.

* Select upload file:
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select file for import'
      file_filter             = '*.xlsx'
    CHANGING
      file_table              = lt_filelist
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lt_filelist IS NOT INITIAL.
    READ TABLE lt_filelist INDEX 1 INTO file.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form load_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM load_data .

  CHECK gv_stop = abap_false.

  DATA: lo_upl TYPE REF TO /ilsiedu/cl_excel_upload,
        lt_ret TYPE bapiret2_t.

  CLEAR go_data.

  lo_upl = NEW /ilsiedu/cl_excel_upload( iv_file = file iv_struc = go_rmclass->get_upload_struc( ) ).

  lt_ret = lo_upl->import( ).

  IF lt_ret IS NOT INITIAL.

    PERFORM log_run. "Log the run initially
    DATA(lt_ret2) = go_logger->convert_bapiret2_t( lt_ret ).
    go_logger->log_return( lt_ret2 ). "Log the errors
    PERFORM set_end. "Set the end time

    gv_stop = abap_true.

    cl_demo_output=>new(
    )->begin_section( 'Error loading Data Migration file'
    )->write_data( lt_ret
    )->display( ).
  ELSE.
    go_data = lo_upl->get_data_table( ).
    ASSIGN go_data->* TO <fs_table>.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_data .

  CHECK gv_stop = abap_false.

  CLEAR: gs_runhdr, gt_rundata.

  "Create header row
  gs_runhdr-runid = runid.
  gs_runhdr-runmode = gs_runmode-runmode.
  gs_runhdr-uname = sy-uname.

  GET TIME STAMP FIELD DATA(lv_ts).
  CONVERT TIME STAMP lv_ts TIME ZONE sy-zonlo
    INTO DATE gs_runhdr-upl_date TIME gs_runhdr-upl_time.

  gs_runhdr-total = lines( <fs_table> ).

  FIELD-SYMBOLS <fs_char> TYPE c.

  "Now build individual rows
  MOVE-CORRESPONDING gs_runhdr TO gs_rundata.

  LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row>).

    "Generate record ID
    gs_rundata-recid = /ilsiedu/cl_dmr_logger=>gen_recid( ).

    ASSIGN <fs_row> TO <fs_char> CASTING.

    "Move data to row data
    gs_rundata-rowdata = <fs_char>.

    APPEND gs_rundata TO gt_rundata.

  ENDLOOP.

  UNASSIGN: <fs_row>, <fs_table>.
  CLEAR go_data.

  gs_runhdr-total = lines( gt_rundata ).

  "Post data in DB
  PERFORM save_db.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_db
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_db .

  CHECK gv_stop = abap_false.

  "Post data in DB
  MODIFY /ilsiedu/dmrhdr FROM gs_runhdr.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s003(/ilsiedu/dmr).
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  MODIFY /ilsiedu/dmrdata FROM TABLE gt_rundata.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s003(/ilsiedu/dmr).
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_db
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_prc .

  CHECK gv_stop = abap_false.

  "Post data in DB
  MODIFY /ilsiedu/dmrhdr FROM gs_prchdr.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s003(/ilsiedu/dmr).
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  MODIFY /ilsiedu/dmrdata FROM TABLE gt_prcdata.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s003(/ilsiedu/dmr).
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_runid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_runid .
  "Ensure RunID hasn't been used before
  SELECT COUNT( * )
    FROM /ilsiedu/dmrhdr
    WHERE runid = runid.

  IF sy-dbcnt NE 0.
    MESSAGE s001(/ilsiedu/dmr) DISPLAY LIKE 'E'.
    gv_stop = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form match_records
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM match_records .

  "Get Records
  PERFORM get_run.
  CHECK gv_stop = abap_false.

  IF gs_runhdr-matched = abap_true.
    MESSAGE s004(/ilsiedu/dmr) DISPLAY LIKE 'E'.
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  "Log the run
  PERFORM log_run.

  DATA: lt_rettab TYPE /ilsiedu/return_t.

  "Start Matching Process
  go_rmclass->match_records(
    IMPORTING
      ev_error  = gv_stop
      et_rettab = lt_rettab
    CHANGING
      cv_header = gs_runhdr
      ct_data   = gt_rundata
      co_logger = go_logger
  ).

  "Post any return info
  go_logger->log_return( lt_rettab ).

  IF gv_stop = abap_false.

    gs_runhdr-matched = abap_true.

    PERFORM save_db.

  ENDIF.

  PERFORM set_end.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_run
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_run .

  CLEAR: gs_runhdr, gt_rundata.

  "Get header
  SELECT SINGLE *
    FROM /ilsiedu/dmrhdr
    INTO gs_runhdr
    WHERE runid = runid.

  IF sy-subrc NE 0.
    MESSAGE s002(/ilsiedu/dmr) WITH runid DISPLAY LIKE 'E'.
    gv_stop = abap_true.
    EXIT.
  ENDIF.

  "Get records
  SELECT *
    FROM /ilsiedu/dmrdata
    INTO TABLE gt_rundata
    WHERE runid = runid.

  IF sy-subrc NE 0.
    MESSAGE s002(/ilsiedu/dmr) WITH runid DISPLAY LIKE 'E'.
    gv_stop = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_alv_tab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_alv_tab .

  "Get struc
  CHECK go_rmclass IS BOUND.
  DATA(lv_alv) = go_rmclass->get_alv_struc( ).
  DATA(lv_data) = go_rmclass->get_upload_struc( ).

  DATA: ls_char TYPE c LENGTH 4000,
        ls_alvc TYPE c LENGTH 4000.

  DATA: lo_data TYPE REF TO data.

  FIELD-SYMBOLS: <fs_data> TYPE any,
                 <fs_alv>  TYPE any.

  ASSIGN ls_char TO <fs_data> CASTING TYPE (lv_data).
  ASSIGN ls_alvc TO <fs_alv>  CASTING TYPE (lv_alv).

  CREATE DATA lo_data TYPE TABLE OF (lv_alv).
  ASSIGN lo_data->* TO <fs_table>.

  "Build outtab
  LOOP AT gt_rundata INTO DATA(ls_rd).
    CLEAR ls_alvc.

    IF ls_rd-manual NE abap_true.
      ls_rd-manual = abap_false.
    ENDIF.
    IF ls_rd-corrected NE abap_true.
      ls_rd-corrected = abap_false.
    ENDIF.

    ls_char = ls_rd-rowdata.
    MOVE-CORRESPONDING ls_rd TO <fs_alv>.
    MOVE-CORRESPONDING <fs_data> TO <fs_alv>.

    ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <fs_alv> TO FIELD-SYMBOL(<fs_msg>).
    IF sy-subrc = 0
       AND ls_rd-error_id IS NOT INITIAL
       AND ls_rd-error_num IS NOT INITIAL
       AND ls_rd-error_type IS NOT INITIAL.
      MESSAGE ID ls_rd-error_id TYPE ls_rd-error_type NUMBER ls_rd-error_num INTO <fs_msg>.
      UNASSIGN <fs_msg>.
    ENDIF.

    INSERT <fs_alv> INTO TABLE <fs_table>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_all .
  "Build process tab
  CLEAR: gs_prchdr, gs_prcdata, gt_prcdata.

  gt_prcdata = gt_rundata.
  gs_prchdr = gs_runhdr.

  PERFORM log_run.

  DATA: lt_rettab TYPE /ilsiedu/return_t.

  PERFORM process_records CHANGING lt_rettab.

  go_logger->log_return( lt_rettab ).
  PERFORM set_end.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_records
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_records CHANGING lt_rettab TYPE /ilsiedu/return_t.

  "Process records
  go_rmclass->process_records(
    IMPORTING
      ev_error  = gv_stop
      et_rettab = lt_rettab
    CHANGING
      cv_header = gs_prchdr
      ct_data   = gt_prcdata
      co_logger = go_logger
  ).

  IF gv_stop = abap_true.
    MESSAGE s005(/ilsiedu/dmr) DISPLAY LIKE 'E'.
  ENDIF.

  gv_set_data = abap_true.

  PERFORM save_prc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form gen_logger
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gen_logger.

  IF go_logger IS NOT INITIAL.
    CLEAR go_logger.
  ENDIF.

  go_logger = NEW /ilsiedu/cl_dmr_logger( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form log_run
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM log_run.

  PERFORM gen_logger.

  "Get the parameters
  go_logger->log_run( ).
  go_logger->log_extra( iv_action = action iv_prg_runid = runid ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_end
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_end.

  CHECK go_logger IS NOT INITIAL.

  go_logger->set_end( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form generic_subscreen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM generic_subscreen.

  gv_dynnr = '0999'.
  gv_repid = sy-repid.

ENDFORM.
