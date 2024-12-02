CLASS /ilsiedu/cl_dmr_grad DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: co_runmode TYPE /ilsiedu/dmr_mode VALUE 'GR'.

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



CLASS /ilsiedu/cl_dmr_grad IMPLEMENTATION.


  METHOD /ilsiedu/inf_dmr~change_fcat.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~change_layout.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_alv_struc.
    rv_struc = '/ILSIEDU/DMR_GRADUATION_ALV'.
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_dynnr.
    "N/A
  ENDMETHOD.


  METHOD /ilsiedu/inf_dmr~get_upload_struc.
    rv_struc = '/ILSIEDU/DMR_GRADUATION'.
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
    DATA: ls_rec TYPE /ilsiedu/dmr_graduation,
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

      DATA(lv_begda) = convert_date( ls_rec-start ).

      "Get program Object ID
      DATA(lv_scobjid) = get_objid( iv_otype = 'SC' iv_datum = lv_begda iv_short = ls_rec-relobj ).

      "Not found?
      IF sy-subrc NE 0 OR lv_scobjid IS INITIAL.

        MESSAGE e052(/ilsiedu/dmr) WITH ls_rec-relobj INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.
        CONTINUE.

      ENDIF.

      DATA: lv_csobjid TYPE piqcsobjid.

      "Validate student is admitted to program
      CALL FUNCTION 'HRIQ_STUDENT_STUDY_FOR_SC_GET'
        EXPORTING
          iv_plvar       = ls_stu-plvar
          iv_st_objid    = ls_stu-objid
          iv_sc_objid    = lv_scobjid
        IMPORTING
          ev_cs_objid    = lv_csobjid
        EXCEPTIONS
          nothing_found  = 1
          internal_error = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO gv_message.
        log_msg( CHANGING cs_rec = <fs_data> ).

        MESSAGE e053(/ilsiedu/dmr) WITH ls_rec-stnum ls_rec-relobj INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.

        CONTINUE.

      ENDIF.

      DATA: lt_guid TYPE piqgrad_guid_t,
            lt_ret  TYPE bapiret2_t.

      CLEAR: lt_guid, lt_ret.


      "Get graduations for program
      CALL FUNCTION 'HRIQ_GRADREC_BY_STUDENT_GET'
        EXPORTING
          iv_plvar     = ls_stu-plvar
          iv_studentid = ls_stu-objid
          iv_csobjid   = lv_csobjid
          iv_read_text = abap_true
        IMPORTING
          et_guid      = lt_guid
          et_return    = lt_ret.

      IF lt_guid IS INITIAL.

        MESSAGE e100(/ilsiedu/dmr) INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).

        log_rtrn( iv_recid = <fs_data>-recid it_rtrn = lt_ret ).
        <fs_data>-manual = abap_true.

        CONTINUE.

      ENDIF.

      DATA: ls_gradrec TYPE piqgradrec,
            lt_confrec TYPE piqconferq_t,
            lt_tmpconf TYPE piqconferq_t,
            lt_cmprrec TYPE piqcmprrecords_t,
            lv_cmprfnd TYPE abap_bool.

      "Read the qualif objid
      DATA(lv_cqobjid) = get_objid( iv_otype = 'CQ' iv_datum = lv_begda iv_short = ls_rec-qual ).

      IF sy-subrc NE 0 OR lv_cqobjid IS INITIAL.

        MESSAGE e054(/ilsiedu/dmr) WITH ls_rec-qual INTO gv_message.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
        <fs_data>-manual = abap_true.
        CONTINUE.

      ENDIF.

      CLEAR: lt_tmpconf, lv_cmprfnd.

      DATA: lv_gradid TYPE piqgradid.

      "Find grad rec for this row
      LOOP AT lt_guid INTO DATA(lv_guid).

        CLEAR: lt_ret, ls_gradrec, lt_confrec, lt_cmprrec, lv_gradid.

        MOVE lv_guid TO lv_gradid.

        CALL FUNCTION 'HRIQ_GRAD_BY_GUID_GET'
          EXPORTING
            iv_csobjid     = lv_csobjid
            iv_gradid      = lv_gradid
          IMPORTING
            es_gradrec     = ls_gradrec
            et_conferq     = lt_confrec
            et_cmprrecords = lt_cmprrec
            et_return      = lt_ret.

        "Check for cmpr record for year/session
        LOOP AT lt_cmprrec INTO DATA(ls_cmpr) WHERE peryr = ls_rec-acadyear AND perid = ls_rec-acadsession.
          lv_cmprfnd = abap_true.
        ENDLOOP.

        "See if there is a qual for the year/session/qual
        LOOP AT lt_confrec INTO DATA(ls_confrec) WHERE ayear = ls_rec-acadyear
                                                   AND aperiod = ls_rec-acadsession
                                                   AND cq_id = lv_cqobjid.

          "Add this one to the temp storage so we can check for others
          APPEND ls_confrec TO lt_tmpconf.

        ENDLOOP.

      ENDLOOP.

      CLEAR gv_message.

      IF lv_cmprfnd = abap_false.
        MESSAGE e100(/ilsiedu/dmr) INTO gv_message.
      ELSEIF lt_tmpconf IS INITIAL.
        "Nothing found.
        MESSAGE e101(/ilsiedu/dmr) INTO gv_message.
        <fs_data>-matchid = ls_cmpr-guid.
      ELSEIF lines( lt_tmpconf ) > 1.
        <fs_data>-manual = abap_true.
        MESSAGE e102(/ilsiedu/dmr) INTO gv_message.
      ELSE.

        DATA(ls_conf) = lt_tmpconf[ 1 ].
        <fs_data>-matchid = ls_conf-refproc_guid.

        IF ls_conf-not_completed NE ls_rec-notach AND ls_conf-confer_date NE convert_date( ls_rec-confrecdate ).
          "Date and achieved status mismatch
          MESSAGE e106(/ilsiedu/dmr) INTO gv_message.
        ELSEIF  ls_conf-confer_date NE convert_date( ls_rec-confrecdate ).
          "Date mismatch
          MESSAGE e105(/ilsiedu/dmr) INTO gv_message.
        ELSEIF ls_rec-notach NE lt_tmpconf[ 1 ]-not_completed.
          "Not achieved doesn't match
          MESSAGE e103(/ilsiedu/dmr) INTO gv_message.
        ENDIF.

      ENDIF.

      IF gv_message IS NOT INITIAL.
        set_error_from_msg( CHANGING cs_rec = <fs_data> ).
      ELSE.
        "Matches
        <fs_data>-corrected = abap_true.
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
    DATA: ls_rec TYPE /ilsiedu/dmr_graduation,
          ls_stu TYPE hrobject,
          ls_chr TYPE c LENGTH 4000.

    "Get the structure
    DATA(lv_struc) = /ilsiedu/inf_dmr~get_upload_struc( ).

    "Loop through incoming data, and cast the row data
    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE manual NE abap_true
                                                        AND corrected NE abap_true
                                                        AND error_id = /ilsiedu/inf_dmr~co_msgid.

      CLEAR: ls_rec, ls_stu.

      "Convert to correct structure
      ls_chr = <fs_data>-rowdata.
      ASSIGN ls_chr TO <fs_rec> CASTING TYPE (lv_struc).
      IF sy-subrc NE 0.
        set_error( EXPORTING iv_num = '050' iv_type = 'E' CHANGING cs_rec = <fs_data> ).
        CONTINUE.
      ENDIF.

      ls_rec = <fs_rec>.

      "Get the SCOBJID
      DATA(lv_scobjid) = get_objid( iv_otype = 'SC' iv_datum = convert_date( ls_rec-start ) iv_short = ls_rec-relobj ).
      "Get the STOBJID
      DATA(lv_stobjid) = get_objid( iv_otype = 'ST' iv_datum = convert_date( ls_rec-start ) iv_short = ls_rec-stnum ).
      "Get the CQOBJID
      DATA(lv_cqobjid) = get_objid( iv_otype = 'CQ' iv_datum = convert_date( ls_rec-start ) iv_short = ls_rec-qual ).
      "Get the CSOBJID
      DATA: lv_csobjid TYPE piqcsobjid.

      CALL FUNCTION 'HRIQ_STUDENT_STUDY_FOR_SC_GET'
        EXPORTING
          iv_plvar    = cl_hrpiq00constants=>c_plvar
          iv_st_objid = lv_stobjid
          iv_sc_objid = lv_scobjid
        IMPORTING
          ev_cs_objid = lv_csobjid.

      DATA: lv_guid    TYPE piqgradid,
            ls_gradrec TYPE piqgradrec,
            lt_confrec TYPE piqconferq_t,
            lt_cmprrec TYPE piqcmprrecords_t,
            lt_ret     TYPE bapiret2_t.

      CLEAR lv_guid.
      MOVE <fs_data>-matchid TO lv_guid.

      DATA: ls_conf TYPE piqconferq.
      CLEAR: ls_conf, lt_ret.

      "Now, process errors we can
      "We only assign one ID type for the errors here, DMR
      CASE <fs_data>-error_num.
        WHEN '100' OR '101'.
          "Grad rec or conferq missing

          DATA: lt_stu TYPE piqst_objid_t.
          CLEAR lt_stu.

          APPEND lv_stobjid TO lt_stu.

          ls_conf-student_id = lv_stobjid.
          ls_conf-gradesymbol = ls_rec-gradesym.
          ls_conf-scale = ls_rec-scale.
          ls_conf-conf_comment = ls_rec-comment.
          ls_conf-confer_date = ls_rec-confrecdate.
          ls_conf-ayear = ls_rec-acadyear.
          ls_conf-aperiod = ls_rec-acadsession.
          ls_conf-valid_from = convert_date( ls_rec-start ).
          ls_conf-valid_to = convert_date( ls_rec-end ).
          ls_conf-transferred = ls_rec-ext.
          ls_conf-cq_id = lv_cqobjid.
          ls_conf-peridforcq = ls_rec-duration.
          ls_conf-peridforcqunit = ls_rec-unit.
          ls_conf-refobj_otype = ls_rec-otype.
          ls_conf-refobj_realo = lv_scobjid.
          ls_conf-refproc_guidtype = 'GRAD'.
          ls_conf-refproc_guid = lv_guid.
          ls_conf-diplomaname = ls_rec-dipname.
          ls_conf-finalw_title = ls_rec-title.
          ls_conf-finalw_titlealt = ls_rec-title2.
          ls_conf-finalw_langu = ls_rec-lang.
          ls_conf-finalw_langualt = ls_rec-lang2.
          ls_conf-acadhonor_sym = ls_rec-acadhon.
          ls_conf-acadhonor_scale = ls_rec-acadhonscale.
          ls_conf-not_completed = ls_rec-notach.

          DATA: lt_obj TYPE hrobject_t.
          CLEAR: lt_obj.

          "Check to ensure the evaluation object is offered.
          CALL FUNCTION 'HRIQ_EVOBJ_GET'
            EXPORTING
              iv_otype           = 'SC'
              iv_objid           = lv_scobjid
              iv_begda           = ls_conf-valid_from
              iv_endda           = ls_conf-valid_from
            TABLES
              et_object          = lt_obj
            EXCEPTIONS
              input_data_missing = 1
              OTHERS             = 2.

          IF sy-subrc <> 0.
            log_msg( CHANGING cs_rec = <fs_data> ).
            CONTINUE.
          ENDIF.


          DATA: lt_period TYPE hri1739_t,
                lt_yearpr TYPE piqsearchyearprd_tab,
                lt_timlim TYPE piqtimelimits_tab,
                lv_commit TYPE abap_bool,
                lt_1766   TYPE STANDARD TABLE OF p1766.

          CLEAR: lt_period, lt_yearpr, lv_commit, lt_1766.

          APPEND VALUE #( peryr = ls_rec-acadyear perid = ls_rec-acadsession ) TO lt_yearpr.

          "Filter out the non graduation ones
          CALL FUNCTION 'HRIQ_EVOBJ_DATA_GET'
            EXPORTING
              it_object       = lt_obj
              iv_begda        = ls_conf-valid_from
              iv_endda        = ls_conf-valid_from
            TABLES
              et_1766         = lt_1766
            EXCEPTIONS
              objects_missing = 1
              OTHERS          = 2.

          IF sy-subrc <> 0.
            log_msg( CHANGING cs_rec = <fs_data> ).
            CONTINUE.
          ENDIF.

          "Now check the periods for these evobjs
          LOOP AT lt_obj INTO DATA(ls_obj).

            READ TABLE lt_1766 INTO DATA(ls_1766) WITH KEY objid = ls_obj-objid.
            CHECK sy-subrc = 0.
            CHECK ls_1766-audtype = '1000'.

            CALL FUNCTION 'HRIQ_EVOBJ_PERIODS_GET'
              EXPORTING
                is_object = ls_obj
              IMPORTING
                et_period = lt_period.

            "Is this evobj offered for this period?
            READ TABLE lt_period TRANSPORTING NO FIELDS WITH KEY peryr = ls_rec-acadyear perid = ls_rec-acadsession.
            IF sy-subrc NE 0.
              CLEAR lt_timlim.
              "Read calendar timelimits, add offering
              CALL FUNCTION 'HRIQ_ACAD_SESSION_DATES_GET'
                EXPORTING
                  is_object           = ls_obj
                  it_yearperiod       = lt_yearpr
                IMPORTING
                  et_timelimits       = lt_timlim
                EXCEPTIONS
                  customizing_error   = 1
                  no_timelimits_found = 2
                  OTHERS              = 3.
              IF sy-subrc <> 0.
                log_msg( CHANGING cs_rec = <fs_data> ).
                CONTINUE.
              ENDIF.
              DELETE lt_timlim WHERE ca_timelimit NE '0100'.
              CALL FUNCTION 'HRIQ_CREATE_OFFER_SM_CE'
                EXPORTING
                  ip_plvar      = ls_obj-plvar
                  ip_objtype    = ls_obj-otype
                  ip_objid      = ls_obj-objid
                  ip_perid      = ls_rec-acadsession
                  ip_year       = ls_rec-acadyear
                  it_timelimits = lt_timlim.

              lv_commit = abap_true.

            ENDIF.

          ENDLOOP.

          IF lv_commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

          IF <fs_data>-error_num = '100'.
            CALL FUNCTION 'HRIQ_GRAD_REGIST'
              EXPORTING
                iv_sc_id        = lv_scobjid
                iv_year         = ls_rec-acadyear
                iv_period       = ls_rec-acadsession
                is_conferq      = ls_conf
                it_students     = lt_stu
                iv_datatransfer = abap_true
                iv_commit       = abap_true
              IMPORTING
                et_return       = lt_ret.
          ELSE.
            CALL FUNCTION 'HRIQ_CONFERQ_CREATE'
              EXPORTING
                is_conferq = ls_conf
                iv_process = 'GR02'
              IMPORTING
                ev_guid    = <fs_data>-matchid
                et_return  = lt_ret.
          ENDIF.

        WHEN '103' OR '105' OR '106'.

          "Get the gradrec from the guid
          CALL FUNCTION 'HRIQ_GRAD_BY_GUID_GET'
            EXPORTING
              iv_csobjid     = lv_csobjid
              iv_gradid      = lv_guid
            IMPORTING
              es_gradrec     = ls_gradrec
              et_conferq     = lt_confrec
              et_cmprrecords = lt_cmprrec
              et_return      = lt_ret.

          CLEAR lt_ret.

          READ TABLE lt_confrec INTO ls_conf WITH KEY ayear = ls_rec-acadyear
                                                      aperiod = ls_rec-acadsession
                                                      cq_id = lv_cqobjid.

          CHECK sy-subrc = 0.

          "Fix not achieved flag
          IF <fs_data>-error_num = '105' OR <fs_data>-error_num = '106'.

            "Fix conferment date
            ls_conf-confer_date = convert_date( ls_rec-confrecdate ).

          ENDIF.

          IF <fs_data>-error_num = '103' OR <fs_data>-error_num = '106'.

            "Fix not achieved
            ls_conf-not_completed = ls_rec-notach.

          ENDIF.

          CALL FUNCTION 'HRIQ_CONFERQ_CHANGE'
            EXPORTING
              iv_guid    = lv_guid
              is_conferq = ls_conf
              iv_process = 'GR02'
            IMPORTING
              et_return  = lt_ret.

      ENDCASE.

      "This error happens even though we actually make the requested change...
      DELETE lt_ret WHERE id = 'HRPIQ00CONFERQ' AND number = '009'.

      log_rtrn( iv_recid = <fs_data>-recid it_rtrn = lt_ret ).

      LOOP AT lt_ret TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A' OR type = 'X'.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        ROLLBACK WORK.
        <fs_data>-corrected = abap_false.
      ELSE.
        COMMIT WORK.
        <fs_data>-corrected = abap_true.

        GET TIME STAMP FIELD DATA(lv_ts).

        CONVERT TIME STAMP lv_ts TIME ZONE sy-zonlo INTO DATE <fs_data>-corr_date TIME <fs_data>-corr_time.
        <fs_data>-uname = sy-uname.
      ENDIF.

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


  METHOD convert_date.

    rv_date = |{ iv_date+6(4) }{ iv_date+3(2) }{ iv_date(2) }|.

  ENDMETHOD.
ENDCLASS.
