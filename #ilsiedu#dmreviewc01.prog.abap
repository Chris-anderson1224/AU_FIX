*----------------------------------------------------------------------*
***INCLUDE /ILSIEDU/DMREVIEWC01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_grid
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_grid DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init_grid.
    CLASS-METHODS free_grid.
    METHODS constructor.
    METHODS set_data.

    METHODS:
    handle_toolbar
      FOR EVENT TOOLBAR
      OF cl_gui_alv_grid
      IMPORTING e_object e_interactive,
    handle_usercomm
      FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
    handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

  PRIVATE SECTION.
    DATA: go_cntrl TYPE REF TO cl_gui_custom_container,
          go_alv   TYPE REF TO cl_gui_alv_grid,
          gt_style TYPE lvc_t_styl.
    METHODS change_fieldcat.
    METHODS change_layout.
    METHODS release.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_grid
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_grid IMPLEMENTATION.
  METHOD init_grid.

    "Build screen objects
    IF go_grid IS NOT BOUND.
      CREATE OBJECT go_grid.

      SET HANDLER go_grid->handle_toolbar FOR go_grid->go_alv.
      SET HANDLER go_grid->handle_usercomm FOR go_grid->go_alv.
      SET HANDLER go_grid->handle_double_click FOR go_grid->go_alv.

      go_grid->go_alv->set_toolbar_interactive( ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    CREATE OBJECT go_cntrl
      EXPORTING
        container_name = 'CNTRL'.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent          = go_cntrl
        i_appl_events     = space
        i_name            = |{ gs_runmode-text }|
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    DATA: lt_uifuncs TYPE ui_functions.

*    APPEND go_alv->mc_fc_excl_all TO lt_uifuncs.

    go_alv->set_table_for_first_display(
      EXPORTING
        i_structure_name              = go_rmclass->get_alv_struc( )
*        it_toolbar_excluding          = lt_uifuncs "Exclude nothing
      CHANGING
        it_outtab                     = <fs_table> ).

    set_data( ).

  ENDMETHOD.
  METHOD change_fieldcat.
    "Get field catalog
    DATA: lt_fcat TYPE lvc_t_fcat.
    go_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      IF <fs_fcat>-domname = 'XFELD' OR <fs_fcat>-domname = 'FLAG'.
        <fs_fcat>-checkbox = abap_true.
      ELSEIF <fs_fcat>-fieldname = 'RUNID' OR <fs_fcat>-fieldname = 'RECID'.
        <fs_fcat>-tech = abap_true.
      ELSEIF <fs_fcat>-fieldname CP 'ERROR*'.
        <fs_fcat>-no_out = abap_true.
      ELSEIF <fs_fcat>-fieldname = 'MESSAGE'.
        <fs_fcat>-outputlen = 50. "Cap it to prevent taking up the whole screen
      ENDIF.
    ENDLOOP.

    go_rmclass->change_fcat( CHANGING ct_fcat = lt_fcat ).

    go_alv->set_frontend_fieldcatalog( lt_fcat ).

  ENDMETHOD.

  METHOD change_layout.
    DATA: ls_lay TYPE lvc_s_layo.

    go_alv->get_frontend_layout( IMPORTING es_layout = ls_lay ).

    ls_lay-sel_mode = 'A'.

    go_rmclass->change_layout( CHANGING cs_layout = ls_lay ).

    go_alv->set_frontend_layout( ls_lay ).
  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: LS_TOOLBAR TYPE STB_BUTTON.
    "Add separator
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    "Add execute button
    ls_toolbar-function = 'EXECUTE'.
    ls_toolbar-icon = icon_execute_object.
    ls_toolbar-text = 'Execute'.
    ls_toolbar-quickinfo = 'Correct Selection'.
    ls_toolbar-disabled = abap_false.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_usercomm.

    CASE e_ucomm.
      WHEN 'EXECUTE'.
        "Run for specific records

        DATA: lt_rows TYPE lvc_t_row.
        "Build process tab
        CLEAR: gs_prchdr, gs_prcdata, gt_prcdata, lt_rows.
        gs_prchdr = gs_runhdr.

        "Get selected rows
        go_alv->get_selected_rows( IMPORTING et_index_rows = lt_rows ).

        LOOP AT lt_rows INTO DATA(ls_row).
          READ TABLE <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX ls_row-index.
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT 'RECID' OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_recid>).
          CHECK sy-subrc = 0.
          READ TABLE gt_rundata INTO DATA(ls_d) WITH KEY recid = <fs_recid>.
          CHECK sy-subrc = 0.
          APPEND ls_d TO gt_prcdata.
        ENDLOOP.

        CHECK gt_prcdata IS NOT INITIAL.

        DATA: lt_rettab TYPE /ilsiedu/return_t.

        PERFORM process_records CHANGING lt_rettab.

        go_logger->log_return( lt_rettab ).

        DATA: lv_pass TYPE i,
              lv_fail TYPE i,
              lv_ans  TYPE c.

        lv_pass = lv_fail = 0.

        "Get number of records corrected, vs number failed
        LOOP AT gt_prcdata INTO DATA(ls_prc).

          IF ls_prc-corrected = abap_true.
            lv_pass += 1.
          ELSE.
            lv_fail += 1.
          ENDIF.

        ENDLOOP.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = |Process Executed|
            text_question         = |{ lv_pass NUMBER = USER } records corrected and { lv_fail NUMBER = USER } records failed. Would you like to view the logs?|
            text_button_1         = 'Yes'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'No'
            icon_button_2         = 'ICON_INCOMPLETE'
            default_button        = '1'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_ans.

        IF lv_ans = '1'.
          cl_demo_output=>new(
            )->begin_section( 'DM Record Execute Review'
            )->write_data( name = 'Logs' value = lt_rettab
            )->display( ).
        ENDIF.

        "Reload the table data
        PERFORM get_run.
        PERFORM build_alv_tab.

        set_data( ).
    ENDCASE.

  ENDMETHOD.

  METHOD set_data.
    "Build layout, hide fields, set technical, etc.
    change_fieldcat( ).
    change_layout( ).

    go_alv->check_changed_data( ).
    go_alv->refresh_table_display( ).
    gv_set_data = abap_false.
  ENDMETHOD.

  METHOD free_grid.
    CHECK go_grid IS BOUND.
    go_grid->release( ).
  ENDMETHOD.

  METHOD release.
    go_alv->free( ).
    go_cntrl->free( ).

    CLEAR: go_alv, go_cntrl.
  ENDMETHOD.

  METHOD handle_double_click.
    "Get row
    READ TABLE <fs_table> ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX e_row-index.
    CHECK sy-subrc = 0.

    "Now get the record ID
    ASSIGN COMPONENT 'RECID' OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_recid>).
    CHECK sy-subrc = 0 AND <fs_recid> IS NOT INITIAL.

    "Get logs
    DATA(lt_logs) = go_logger->get_logs_recid( iv_recid = <fs_recid> ).

    DATA(lt_logs_now) = VALUE /ilsiedu/return_t( FOR l IN lt_logs WHERE ( runid = go_logger->get_runid( ) ) ( l ) ).

    DELETE lt_logs WHERE runid = go_logger->get_runid( ).

    DATA: lt_lgout  TYPE bapiret2_t,
          lt_lgoutn TYPE bapiret2_t,
          ls_lgout  TYPE bapiret2.

    LOOP AT lt_logs INTO DATA(ls_log).
      MOVE-CORRESPONDING ls_log TO ls_lgout.
      APPEND ls_lgout TO lt_lgout.
    ENDLOOP.
    LOOP AT lt_logs_now INTO ls_log.
      MOVE-CORRESPONDING ls_log TO ls_lgout.
      APPEND ls_lgout TO lt_lgoutn.
    ENDLOOP.

    cl_demo_output=>new(
    )->begin_section( 'DM Record Review'
    )->write( name = 'Record' data = <fs_row>
    )->begin_section( 'Logs'
    )->write_data( name = 'Current Run' value = lt_lgoutn
    )->write_data( name = 'Previous Runs' value = lt_lgout
    )->display( ).

    CLEAR: lt_lgout, lt_lgoutn, ls_lgout, lt_logs, lt_logs_now.

  ENDMETHOD.

ENDCLASS.
