*----------------------------------------------------------------------*
***INCLUDE /ILSIEDU/BOOKING_FIXCL1.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_ovw
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_ovw DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init_grid.
    METHODS constructor.
    METHODS save_data.
    METHODS data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4.
    METHODS set_data.
  PRIVATE SECTION.
    DATA: go_cntrl TYPE REF TO cl_gui_custom_container,
          go_alv   TYPE REF TO cl_gui_alv_grid,
          gt_style TYPE lvc_t_styl.
    METHODS change_fieldcat.
    METHODS change_layout.
    METHODS build_styletab
      IMPORTING it_fcat TYPE lvc_t_fcat.
    METHODS set_style.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_ovw
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_ovw IMPLEMENTATION.

  METHOD init_grid.

    "Build screen objects
    IF go_grid IS NOT BOUND.
      CREATE OBJECT go_grid.
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
        i_name            = 'Booking Fix - Overview'
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

    DATA: lt_uifuncs TYPE ui_functions.

    APPEND go_alv->mc_fc_excl_all TO lt_uifuncs.

    go_alv->set_table_for_first_display(
      EXPORTING
        i_structure_name              = '/ILSIEDU/BKG_FIX_OVW'
        it_toolbar_excluding          = lt_uifuncs
      CHANGING
        it_outtab                     = gt_overview ).

    "Register edit event
    go_alv->register_edit_event( go_alv->mc_evt_modified ).

    set_data( ).

    SET HANDLER data_changed FOR go_alv.

  ENDMETHOD.

  METHOD set_style.

    DATA: lt_style LIKE gt_style,
          ls_style LIKE LINE OF gt_style.

    "Set style of each row in the table
    LOOP AT gt_overview ASSIGNING FIELD-SYMBOL(<fs_ovw>).
      lt_style = gt_style.

      "Set field specific styles
      INSERT VALUE #( fieldname = 'CALC_CPATTEMPFU'
                      style = COND #( WHEN p_displ = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-complete = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN p_credi = abap_false THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-bad_attempt = abap_true THEN cl_gui_alv_grid=>mc_style_enabled
                                                                            ELSE cl_gui_alv_grid=>mc_style_disabled ) ) INTO TABLE lt_style.

      INSERT VALUE #( fieldname = 'CALC_CPEARNEDFU'
                      style = COND #( WHEN p_displ = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-complete = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN ( p_credi = abap_false AND p_earnd = abap_false ) THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-bad_earned = abap_true THEN cl_gui_alv_grid=>mc_style_enabled
                                                                           ELSE cl_gui_alv_grid=>mc_style_disabled ) ) INTO TABLE lt_style.

      INSERT VALUE #( fieldname = 'CALC_CPGRADEDFU'
                      style = COND #( WHEN p_displ = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-complete = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN ( p_credi = abap_false AND p_earnd = abap_false ) THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-bad_graded = abap_true THEN cl_gui_alv_grid=>mc_style_enabled
                                                                           ELSE cl_gui_alv_grid=>mc_style_disabled ) ) INTO TABLE lt_style.

      INSERT VALUE #( fieldname = 'GRADESCALE'
                      style = COND #( WHEN p_displ = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-complete = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN p_scale = abap_false THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-bad_scaleid = abap_true THEN cl_gui_alv_grid=>mc_style_enabled
                                                                            ELSE cl_gui_alv_grid=>mc_style_disabled ) ) INTO TABLE lt_style.

      INSERT VALUE #( fieldname = 'BKGTEMPL_ID'
                      style = COND #( WHEN p_displ = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-complete = abap_true THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN p_templ = abap_false THEN cl_gui_alv_grid=>mc_style_disabled
                                      WHEN <fs_ovw>-bad_templid = abap_true THEN cl_gui_alv_grid=>mc_style_enabled
                                                                            ELSE cl_gui_alv_grid=>mc_style_disabled ) ) INTO TABLE lt_style.


      <fs_ovw>-style = lt_style.
    ENDLOOP.

  ENDMETHOD.

  METHOD change_layout.
    DATA: ls_lay TYPE lvc_s_layo.

    go_alv->get_frontend_layout( IMPORTING es_layout = ls_lay ).

    ls_lay-edit = abap_true.
    ls_lay-stylefname = 'STYLE'.

    go_alv->set_frontend_layout( ls_lay ).

  ENDMETHOD.

  METHOD change_fieldcat.

    "Get field catalog
    DATA: lt_fcat TYPE lvc_t_fcat.
    go_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).

    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

      IF <fs_fcat>-domname = 'XFELD'.
        <fs_fcat>-checkbox = abap_true.
      ENDIF.

      CASE <fs_fcat>-fieldname.
        WHEN 'OVERRIDE_CR'.
          "Mark override credits as technical if not required
          <fs_fcat>-tech = COND #( WHEN gv_mode = 'F' THEN abap_false ELSE abap_true ).
        WHEN 'BKGID'.
          <fs_fcat>-tech = abap_true.
      ENDCASE.

    ENDLOOP.

    go_alv->set_frontend_fieldcatalog( lt_fcat ).

    build_styletab( lt_fcat ).

  ENDMETHOD.

  METHOD build_styletab.

    DATA: ls_style LIKE LINE OF gt_style.

    CLEAR gt_style.

    LOOP AT it_fcat INTO DATA(ls_fcat).
      CHECK: ls_fcat-fieldname NP 'CALC*',
             ls_fcat-fieldname NE 'GRADESCALE',
             ls_fcat-fieldname NE 'BKGTEMPL_ID'.

      ls_style-fieldname = ls_fcat-fieldname.
      ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_style INTO TABLE gt_style.
    ENDLOOP.

  ENDMETHOD.

  METHOD save_data.
    "Get selected rows
    DATA lt_rows TYPE lvc_t_row.
    go_alv->get_selected_rows( IMPORTING et_index_rows = lt_rows ).

    DATA: lt_ovw TYPE /ilsiedu/bkg_fix_ovw_t.

    LOOP AT lt_rows INTO DATA(ls_row).
      APPEND gt_overview[ ls_row-index ] TO lt_ovw.
    ENDLOOP.

    DELETE lt_ovw WHERE complete = abap_true.

    IF lines( lt_ovw ) = 0.
      MESSAGE i011(/ilsiedu/bkg_fix).
      RETURN.
    ENDIF.

    DATA: lv_ans TYPE char1.
    "Popup to confirm
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirm Save'
        text_question         = |Are you sure you want to save { lines( lt_ovw ) } entires?|
        text_button_1         = |Yes|
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = |No|
        icon_button_2         = 'ICON_INCOMPLETE'
        default_button        = '2'
        display_cancel_button = space
      IMPORTING
        answer                = lv_ans.

    IF lv_ans = '2'.
      RETURN.
    ENDIF.

    PERFORM start_process CHANGING lt_ovw.

    gv_set_data = abap_true.

  ENDMETHOD.

  METHOD data_changed.
    "Check the data is correct
    DATA lv_val TYPE piqcpattemp.

    "Loop through modified cells, and validate the data
    LOOP AT er_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).


      READ TABLE gt_overview INTO DATA(ls_rec) INDEX <fs_mod>-row_id.

      IF sy-subrc NE 0.
        <fs_mod>-error = abap_true.
        er_data_changed->add_protocol_entry(
                            i_msgid = '/ILSIEDU/BKG_FIX'
                            i_msgno = '009'
                            i_msgty = 'E'
                            i_msgv1 = <fs_mod>-row_id
                            i_fieldname = <fs_mod>-fieldname
                            i_row_id = <fs_mod>-row_id ).

      ELSE.

        IF <fs_mod>-fieldname CP 'CALC*'.
          "Get credits into a credit field
          lv_val = <fs_mod>-value.

          "Check the credits match an allowed value
          "If aud, must be 0
          IF ls_rec-smrating = 'AUD'.
            IF lv_val IS NOT INITIAL OR lv_val NE 0.
              <fs_mod>-error = abap_true.

              er_data_changed->add_protocol_entry(
                                  i_msgid = '/ILSIEDU/BKG_FIX'
                                  i_msgno = '008'
                                  i_msgty = 'E'
                                  i_msgv1 = '0.00'
                                  i_fieldname = <fs_mod>-fieldname
                                  i_row_id = <fs_mod>-row_id ).
            ENDIF.
          ELSE.
            DATA(lv_fn) = <fs_mod>-fieldname+7(6).
            CASE lv_fn.
              WHEN 'ATTEMP'.
                "Attempted credits should be between min and max
                IF lv_val NOT BETWEEN ls_rec-cpmin AND ls_rec-cpmax.
                  <fs_mod>-error = abap_true.

                  er_data_changed->add_protocol_entry(
                                      i_msgid = '/ILSIEDU/BKG_FIX'
                                      i_msgno = '007'
                                      i_msgty = 'E'
                                      i_msgv1 = |{ ls_rec-cpmin WIDTH = 3 DECIMALS = 2 }|
                                      i_msgv2 = |{ ls_rec-cpmax WIDTH = 3 DECIMALS = 2 }|
                                      i_fieldname = <fs_mod>-fieldname
                                      i_row_id = <fs_mod>-row_id ).
                ENDIF.
              WHEN 'EARNED' OR 'GRADED'.
                IF ls_rec-smstatus NE 2.
                  "If not passed, should be 0
                  IF lv_val NE 0.
                    <fs_mod>-error = abap_true.

                    er_data_changed->add_protocol_entry(
                                        i_msgid = '/ILSIEDU/BKG_FIX'
                                        i_msgno = '008'
                                        i_msgty = 'E'
                                        i_msgv1 = '0.00'
                                        i_fieldname = <fs_mod>-fieldname
                                        i_row_id = <fs_mod>-row_id ).
                  ENDIF.
                ELSEIF ls_rec-cpearnedfu = 0 AND ls_rec-cpgradedfu = 0 AND lv_val NE 0.
                  "Passed and earned/graded already 0, should stay 0
                  <fs_mod>-error = abap_true.

                  er_data_changed->add_protocol_entry(
                                      i_msgid = '/ILSIEDU/BKG_FIX'
                                      i_msgno = '008'
                                      i_msgty = 'E'
                                      i_msgv1 = '0.00'
                                      i_fieldname = <fs_mod>-fieldname
                                      i_row_id = <fs_mod>-row_id ).
                ELSEIF ls_rec-calc_cpattempfu NE lv_val.
                  "Passed, should be same as attempted
                  <fs_mod>-error = abap_true.

                  er_data_changed->add_protocol_entry(
                                      i_msgid = '/ILSIEDU/BKG_FIX'
                                      i_msgno = '010'
                                      i_msgty = 'E'
                                      i_msgv1 = |{ ls_rec-calc_cpattempfu WIDTH = 3 DECIMALS = 2 }|
                                      i_fieldname = <fs_mod>-fieldname
                                      i_row_id = <fs_mod>-row_id ).
                ENDIF.
            ENDCASE.
          ENDIF.

        ELSE.
          IF <fs_mod>-value IS INITIAL.
            "Not credit, anything is allowed
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_data.
    "Build layout, hide fields, set technical, etc.
    change_fieldcat( ).
    change_layout( ).
    set_style( ).

    go_alv->refresh_table_display( ).
    gv_set_data = abap_false.
  ENDMETHOD.

ENDCLASS.
