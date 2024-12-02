CLASS /ilsiedu/cl_dmr_crse DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: co_runmode TYPE /ilsiedu/dmr_mode VALUE 'CR'.

    INTERFACES /ilsiedu/inf_dmr .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gv_message  TYPE string,
          gt_return   TYPE /ilsiedu/return_t,
          gt_bapiret2 TYPE bapiret2_t,
          go_logger   TYPE REF TO /ilsiedu/cl_dmr_logger.

    METHODS set_error
      IMPORTING
        iv_id   TYPE symsgid DEFAULT /ilsiedu/inf_dmr~co_msgid
        iv_num  TYPE symsgno
        iv_type TYPE bapi_mtype
      CHANGING
        cs_rec  TYPE /ilsiedu/dmrdata.
    METHODS set_error_from_msg
      CHANGING
        cs_rec TYPE /ilsiedu/dmrdata.
    METHODS log_msg
      CHANGING
        cs_rec        TYPE /ilsiedu/dmrdata
      RETURNING
        VALUE(rs_msg) TYPE bapiret2.
    METHODS convert_date
      IMPORTING
        iv_date        TYPE char10
      RETURNING
        VALUE(rv_date) TYPE dats.
    METHODS get_objid
      IMPORTING
        iv_otype        TYPE otype
        iv_datum        TYPE dats
        iv_short        TYPE short_d
      RETURNING
        VALUE(rv_objid) TYPE hrobjid.
    METHODS log_rtrn
      IMPORTING
        iv_recid TYPE /ilsiedu/recid
        it_rtrn  TYPE bapiret2_t.
    METHODS get_next_seqnr
      IMPORTING
        iv_recid        TYPE /ilsiedu/recid
      RETURNING
        VALUE(rv_seqnr) TYPE /ilsiedu/seqnr.
ENDCLASS.



CLASS /ilsiedu/cl_dmr_crse IMPLEMENTATION.


  METHOD /ilsiedu/inf_dmr~change_fcat.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~change_layout.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_alv_struc.
    rv_struc = '/ILSIEDU/DMR_CRSBKG_ALV'.
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_dynnr.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_upload_struc.
    rv_struc = '/ILSIEDU/DMR_CRSBKG'.
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~match_records.

    CLEAR: gt_return.

    IF cv_header-runmode NE co_runmode.
      MESSAGE s006(/ilsiedu/dmr) WITH cv_header-runmode co_runmode DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    go_logger = co_logger.

    FIELD-SYMBOLS: <fs_rec> TYPE any.
    DATA: ls_rec TYPE /ilsiedu/dmr_crsbkg,
          ls_stu TYPE hrobject,
          ls_chr TYPE c LENGTH 4000.

    "Get the structure
    DATA(lv_struc) = /ilsiedu/inf_dmr~get_upload_struc( ).

    "Loop through incoming data, and cast the row data
    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      CLEAR: ls_rec, ls_stu.

      "Convert to correct structure
      ls_chr = <fs_data>-rowdata.
      ASSIGN ls_chr TO <fs_rec> CASTING TYPE (lv_struc).
      IF sy-subrc NE 0.
        set_error( EXPORTING iv_num = '050' iv_type = 'E' CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.
        CONTINUE.
      ENDIF.

      ls_rec = <fs_rec>.

      "Validate student exists
      CALL FUNCTION 'HRIQ_STUDENT_NUMBERS_GET'
        EXPORTING
          iv_student12             = ls_rec-stnum
        IMPORTING
          ev_plvar                 = ls_stu-plvar
          ev_objid                 = ls_stu-objid
        EXCEPTIONS
          no_number                = 1
          no_plvar                 = 2
          no_student12_for_objid   = 3
          no_objid_for_partner     = 4
          no_objid_for_student12   = 5
          no_partner_for_objid     = 6
          no_student12_for_partner = 7
          OTHERS                   = 8.

      IF sy-subrc NE 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO gv_message.
        log_msg( CHANGING cs_rec = <fs_data> ).

        MESSAGE e051(/ilsiedu/dmr) WITH ls_rec-stnum INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.

        CONTINUE.

      ENDIF.

      ls_stu-otype = 'ST'.

      DATA(lv_begda) = convert_date( ls_rec-begda ).

      "Get program Object ID
      DATA(lv_scobjid) = get_objid( iv_otype = 'SC' iv_datum = lv_begda iv_short = ls_rec-pgmshort ).

      "Not found?
      IF sy-subrc NE 0 OR lv_scobjid IS INITIAL.

        MESSAGE e052(/ilsiedu/dmr) WITH ls_rec-pgmshort INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.
        CONTINUE.

      ENDIF.

      "Now get the course objid
      DATA: lv_smobjid TYPE piqsmobjid.
      CLEAR lv_smobjid.

      SELECT SINGLE objid
        FROM hrp9103
        INTO lv_smobjid
        WHERE zzoldid = ls_rec-extsmid.

      IF sy-subrc NE 0 OR lv_smobjid IS INITIAL.
        MESSAGE e055(/ilsiedu/dmr) WITH ls_rec-extsmid INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.
        CONTINUE.
      ENDIF.

      DATA: lt_awid TYPE TABLE OF piqawid_s,
            lt_acwk TYPE piqaw_acworkk_t,
            lt_rtrn TYPE bapiret2_t.

      "Now get the students bookings
      CALL FUNCTION 'HRIQ_AW_ACWORK_GET_RFC'
        EXPORTING
          studentobjectid  = ls_stu-objid
          acad_year        = ls_rec-peryr
          acad_session     = ls_rec-perid
          completed_work   = 'X'
          transferred_work = abap_false
        TABLES
          academicworkid   = lt_awid
          academicwork     = lt_acwk
          return           = lt_rtrn.

      LOOP AT lt_rtrn TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        "Error, log about it
        log_rtrn( iv_recid = <fs_data>-recid it_rtrn = lt_rtrn ).
        <fs_data>-manual = abap_true.
        MESSAGE e056(/ilsiedu/dmr) WITH ls_rec-extsmid INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        CONTINUE.
      ENDIF.

      CLEAR lt_rtrn.

      DATA: lt_tmpaw LIKE lt_acwk.
      CLEAR lt_tmpaw.

      "Get the booking for this course
      LOOP AT lt_acwk INTO DATA(ls_acwk) WHERE awotype = 'SM' AND awobjid = lv_smobjid
                                           AND awstatus = ls_rec-smstatus AND bookdate = convert_date( ls_rec-bookdate )
                                           AND bookreason = ls_rec-bkgreason.
        APPEND ls_acwk TO lt_tmpaw.

      ENDLOOP.

      IF lt_tmpaw IS INITIAL.
        "Not found
        MESSAGE e200(/ilsiedu/dmr) INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        CONTINUE.
      ELSEIF lines( lt_tmpaw ) > 1.
        "Multiple found, manual
        MESSAGE e201(/ilsiedu/dmr) INTO gv_message.
        <fs_data>-manual = abap_true.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        CONTINUE.
      ELSE.
        ls_acwk = lt_tmpaw[ 1 ].
        <fs_data>-matchid = ls_acwk-academicworkid.
      ENDIF.

      CLEAR gv_message.
      "1 found, check credits

      DATA: lv_attemp TYPE piqcpattemp,
            lv_earned TYPE piqcpearned,
            lv_graded TYPE piqcpgraded,
            ls_credog TYPE piqsi2s_credits,
            ls_credcv TYPE piqsi2s_credits.

      CLEAR: lv_attemp, lv_earned, lv_graded, ls_credog, ls_credcv.

      "Start by converting credits if needed
      IF ls_rec-unit NE ls_acwk-cpunit.

        ls_credog-cpattemp = ls_rec-attmp.
        ls_credog-cpearned = ls_rec-earn.
        ls_credog-cpgraded = ls_rec-grade.
        ls_credog-cpunit = ls_rec-unit.

        cl_hrpiq00cp=>convert_credits(
          EXPORTING
            iv_target_unit              = ls_acwk-cpunit
            is_credits                  = ls_credog
          IMPORTING
            es_credits                  = ls_credcv
          EXCEPTIONS
            conversation_not_customized = 1
            unit_not_found              = 2
            others                      = 3 ).

        IF SY-SUBRC <> 0.
          set_error_from_msg( CHANGING cs_rec = <fs_data> ).
          CONTINUE.
        ENDIF.

        lv_attemp = ls_credcv-cpattemp.
        lv_earned = ls_credcv-cpearned.
        lv_graded = ls_credcv-cpgraded.

      ELSE.
        lv_attemp = ls_rec-attmp.
        lv_earned = ls_rec-earn.
        lv_graded = ls_rec-grade.
      ENDIF.


      IF lv_attemp NE ls_acwk-cpattemp
        OR lv_earned NE ls_acwk-cpearned
        OR lv_graded NE ls_acwk-cpgraded.
        MESSAGE e202(/ilsiedu/dmr) INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
      ENDIF.
      IF convert_date( ls_rec-canceldate ) NE ls_acwk-canceldate
        OR ls_rec-cancelreason NE ls_acwk-cancelreason.
        IF gv_message IS NOT INITIAL.
          "Both errors.
          MESSAGE e204(/ilsiedu/dmr) INTO gv_message.
          set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        ELSE.
          "Only Cancel
          MESSAGE e203(/ilsiedu/dmr) INTO gv_message.
          set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        ENDIF.
      ENDIF.

      IF gv_message IS INITIAL.
        <fs_data>-corrected = abap_true.
        MESSAGE s205(/ilsiedu/dmr) INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
      ENDIF.

    ENDLOOP.

    et_rettab = gt_return.

  ENDMETHOD.

  METHOD /ilsiedu/inf_dmr~process_records.

    CLEAR: gt_return.

    IF cv_header-runmode NE co_runmode.
      MESSAGE s006(/ilsiedu/dmr) WITH cv_header-runmode co_runmode DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    go_logger = co_logger.

    FIELD-SYMBOLS: <fs_rec> TYPE any.
    DATA: ls_rec TYPE /ilsiedu/dmr_crsbkg,
          ls_stu TYPE hrobject,
          ls_chr TYPE c LENGTH 4000.

    "Get the structure
    DATA(lv_struc) = /ilsiedu/inf_dmr~get_upload_struc( ).

    "Process only the ones with processable errors
    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE corrected = abap_false
                                                        AND error_type = 'E'
                                                        AND manual = abap_false
                                                        AND error_id = '/ILSIEDU/DMR'
                                                        AND error_num BETWEEN 200 AND 205.

      CLEAR: ls_rec, ls_stu.

      "Convert to correct structure
      ls_chr = <fs_data>-rowdata.
      ASSIGN ls_chr TO <fs_rec> CASTING TYPE (lv_struc).
      IF sy-subrc NE 0.
        set_error( EXPORTING iv_num = '050' iv_type = 'E' CHANGING cs_rec = <fs_data> ).
        CONTINUE.
      ENDIF.

      ls_rec = <fs_rec>.

      CALL FUNCTION 'HRIQ_STUDENT_NUMBERS_GET'
        EXPORTING
          iv_student12 = ls_rec-stnum
        IMPORTING
          ev_plvar     = ls_stu-plvar
          ev_objid     = ls_stu-objid.

      DATA: lv_awid TYPE piqawid,
            lt_rtrn TYPE bapiret2_t,
            ls_acwk TYPE piqaw_acwork,
            lv_agri TYPE piqagrid.

      CLEAR: lv_awid, lt_rtrn, ls_acwk, lv_agri.

      IF <fs_data>-matchid IS NOT INITIAL.

        lv_awid = CONV #( <fs_data>-matchid ).
        "Get current AW record (if found)
        CALL FUNCTION 'HRIQ_AW_ACWORK_READ_INTERN'
          EXPORTING
            iv_awid   = lv_awid
          IMPORTING
            es_acwork = ls_acwk
            ev_agrid  = lv_agri
            et_return = lt_rtrn.

        LOOP AT lt_rtrn TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
          EXIT.
        ENDLOOP.

        IF sy-subrc EQ 0.
          "Failure, log return
          log_rtrn( it_rtrn = lt_rtrn iv_recid = <fs_data>-recid ).
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR lt_rtrn.
      "Now, case on what the error is, and fix
      CASE <fs_data>-error_num.
        WHEN '200'.

          ls_stu-otype = 'ST'.
          DATA(lv_begda) = convert_date( ls_rec-begda ).

          "Get program Object ID
          DATA(lv_scobjid) = get_objid( iv_otype = 'SC' iv_datum = lv_begda iv_short = ls_rec-pgmshort ).     "Now get the course objid
          DATA: lv_smobjid TYPE piqsmobjid.
          CLEAR lv_smobjid.

          SELECT SINGLE objid
            FROM hrp9103
            INTO lv_smobjid
            WHERE zzoldid = ls_rec-extsmid.

          "Create new acwork
          ls_acwk-awotype = 'SM'.
          ls_acwk-awobjid = lv_smobjid.
          ls_acwk-awbegdate = lv_begda.
          ls_acwk-awenddate = convert_date( ls_rec-endda ).
          ls_acwk-awstatus = ls_rec-smstatus.
          ls_acwk-canceldate = convert_date( ls_rec-canceldate ).
          ls_acwk-cancelreason = ls_rec-cancelreason.
          ls_acwk-bookdate = convert_date( ls_rec-bookdate ).
          ls_acwk-acad_session = ls_rec-perid.
          ls_acwk-acad_year = ls_rec-peryr.
          ls_acwk-bookreason = ls_rec-bkgreason.
          ls_acwk-cpattemp = ls_rec-attmp.
          ls_acwk-cpearned = ls_rec-earn.
          ls_acwk-cpgraded = ls_rec-grade.
          ls_acwk-cpunit = ls_rec-unit.

          DATA: lv_progcvar TYPE piqprogc_var,
                lt_progtype TYPE piqawprogramtype_t,
                lt_program  TYPE piqawprogram_t.

          CLEAR: lv_progcvar, lt_progtype, lt_program.

          "Get program type for usages
          CALL FUNCTION 'HRIQ_PROGRAM_DATA_GET'
            EXPORTING
              imp_plvar                = ls_stu-plvar
              imp_objid                = lv_scobjid
              imp_keyda                = lv_begda
              imp_auth                 = abap_false
            IMPORTING
              exp_progcvar             = lv_progcvar.

          IF lv_progcvar IS NOT INITIAL.
            APPEND lv_progcvar TO lt_progtype.
          ENDIF.
          APPEND lv_scobjid TO lt_program.

          CALL FUNCTION 'HRIQ_AW_ACWORK_CREATE'
            EXPORTING
              studentobjectid    = ls_stu-objid
              academicwork       = ls_acwk
              process            = 'AW01'
              programtype_usages = lt_progtype
              program_usages     = lt_program
            IMPORTING
              academicworkid     = lv_awid
            TABLES
              return             = lt_rtrn.

        WHEN '202' OR '203' OR '204'.
          "Fix credits
          IF <fs_data>-error_num = '202' OR <fs_data>-error_num = '204'.
            ls_acwk-cpattemp = ls_rec-attmp.
            ls_acwk-cpearned = ls_rec-earn.
            ls_acwk-cpgraded = ls_rec-grade.
            ls_acwk-cpunit = ls_rec-unit.
          ENDIF.

          "Fix Cancel information
          IF <fs_data>-error_num = '203' OR <fs_data>-error_num = '204'.
            ls_acwk-canceldate = convert_date( ls_rec-canceldate ).
            ls_acwk-cancelreason = ls_rec-cancelreason.
          ENDIF.

          "Post change
          CALL FUNCTION 'HRIQ_AW_ACWORK_CHANGE'
            EXPORTING
              academicworkid  = lv_awid
              studentobjectid = ls_stu-objid
              academicwork    = ls_acwk
              process         = 'AW02'
              check           = abap_false
            IMPORTING
              return          = lt_rtrn.

      ENDCASE.

      LOOP AT lt_rtrn TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.

      IF sy-subrc EQ 0.
        ROLLBACK WORK.
        <fs_data>-corrected = abap_false.

      ELSE.
        COMMIT WORK.
        <fs_data>-corrected = abap_true.
        <fs_data>-matchid = lv_awid.

        GET TIME STAMP FIELD DATA(lv_ts).

        CONVERT TIME STAMP lv_ts TIME ZONE sy-zonlo INTO DATE <fs_data>-corr_date TIME <fs_data>-corr_time.
        <fs_data>-uname = sy-uname.
      ENDIF.

      log_rtrn( it_rtrn = lt_rtrn iv_recid = <fs_data>-recid ).

    ENDLOOP.

    et_rettab = gt_return.

  ENDMETHOD.


  METHOD get_objid.
    SELECT SINGLE objid
      FROM hrp1000
      WHERE otype = @iv_otype
        AND begda <= @iv_datum
        AND endda >= @iv_datum
        AND mc_short = @( |{ iv_short CASE = UPPER }| )
      INTO @rv_objid.
  ENDMETHOD.


  METHOD set_error.

    cs_rec-error_id = iv_id.
    cs_rec-error_num = iv_num.
    cs_rec-error_type = iv_type.

  ENDMETHOD.


  METHOD set_error_from_msg.

    "Log the message
    DATA(ls_msg) = log_msg( CHANGING cs_rec = cs_rec ).

    "Put the data in record
    set_error( EXPORTING iv_id = ls_msg-id iv_num = ls_msg-number iv_type = ls_msg-type CHANGING cs_rec = cs_rec ).

  ENDMETHOD.


  METHOD log_msg.

    rs_msg = /ited/cl_log_tools=>get_symsg_bapiret2( ).

    APPEND rs_msg TO gt_bapiret2.

    DATA(ls_ret) = go_logger->convert_bapiret2(
        is_bapiret2     = rs_msg
        iv_create_recid = abap_false
        iv_recid        = cs_rec-recid ).

    ls_ret-seqnr = get_next_seqnr( cs_rec-recid ).

    INSERT ls_ret INTO TABLE gt_return.

  ENDMETHOD.

  METHOD log_rtrn.

    DATA(lt_ret) = go_logger->convert_bapiret2_t(
                     iv_create_recids = abap_false
                     iv_recid         = iv_recid
                     it_bapiret2      = it_rtrn
                   ).

    DATA(lv_seqnr) = get_next_seqnr( iv_recid ).

    LOOP AT lt_ret INTO DATA(ls_ret).
      ls_ret-seqnr = lv_seqnr.
      INSERT ls_ret INTO TABLE gt_return.
      lv_seqnr += 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_next_seqnr.
    rv_seqnr = 0.
    LOOP AT gt_return INTO DATA(ls_r) WHERE recid = iv_recid.
      CHECK ls_r-seqnr < rv_seqnr.
      rv_seqnr = ls_r-seqnr.
    ENDLOOP.

    IF sy-subrc EQ 0.
      rv_seqnr = rv_seqnr + 1.
    ENDIF.
  ENDMETHOD.


  METHOD convert_date.

    rv_date = |{ iv_date+6(4) }{ iv_date+3(2) }{ iv_date(2) }|.

  ENDMETHOD.
ENDCLASS.
