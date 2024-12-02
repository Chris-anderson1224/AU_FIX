*&---------------------------------------------------------------------*
*& Report /ilsiedu/booking_fix
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /ilsiedu/booking_fix.

INCLUDE rhiqmpcntr_d.
INCLUDE rhiqmp_reportevents_d.
INCLUDE rhiqmp_reportevents_f01.

DATA: lv_peryr TYPE piqperyr,
      lv_perid TYPE piqperid,
      lv_short TYPE piqsmshort,
      lv_objid TYPE piqoobjid,
      lv_stnum TYPE piqstudent12.

LOAD-OF-PROGRAM.

  "Load program run options
  CREATE OBJECT gr_repev.
  CALL METHOD gr_repev->load_of_program.

  PERFORM init_orgs.
  PERFORM init_mods.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_mode TYPE /ilsiedu/bkgf_run_mode AS LISTBOX VISIBLE LENGTH 30 USER-COMMAND mode.
    SELECT-OPTIONS: so_peryr FOR lv_peryr MATCHCODE OBJECT /ilsiedu/piqperyr MODIF ID ms,
                    so_perid FOR lv_perid MATCHCODE OBJECT /ilsiedu/piqperid NO INTERVALS MODIF ID ms,
                    so_orgid FOR lv_objid NO INTERVALS MODIF ID ms.
    SELECTION-SCREEN COMMENT 56(40) cm_org FOR FIELD so_orgid.
    SELECT-OPTIONS: so_smsht FOR lv_short NO INTERVALS MODIF ID ms.
    SELECTION-SCREEN COMMENT 56(40) cm_sm  FOR FIELD so_orgid.
    SELECT-OPTIONS: so_stnum FOR lv_stnum MATCHCODE OBJECT hrpiq00student NO INTERVALS MODIF ID ms.
    "SELECTION-SCREEN ULINE.
    PARAMETERS : p_lfile TYPE string LOWER CASE MODIF ID fi,
                 p_skipo TYPE xfeld AS CHECKBOX MODIF ID fi.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
    PARAMETERS : p_displ AS CHECKBOX USER-COMMAND disp. "Display Only
    PARAMETERS : p_templ AS CHECKBOX DEFAULT 'X' MODIF ID rp. "Fix Template assignments
    "Should remove scales because they don't actually change anything?
    PARAMETERS : p_scale AS CHECKBOX DEFAULT 'X' MODIF ID rp. "Fix assigned grade scale
    PARAMETERS : p_credi AS CHECKBOX DEFAULT 'X' MODIF ID rp. "Fix attempted/earned/graded credits
    PARAMETERS : p_earnd AS CHECKBOX DEFAULT ' ' MODIF ID rp. "FIX earned credits with attempted from HRPAD506
  SELECTION-SCREEN END OF BLOCK b1.

* Process options:
  INCLUDE rhiqmpcntr_s.
  INCLUDE rhiqmpcntrf01.
  INCLUDE rhiqmpcntr_e.

  INCLUDE /ilsiedu/booking_fix_top.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  PERFORM value_request_file CHANGING p_lfile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_orgid-low.
  PERFORM f4_orgid.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_smsht-low.
  PERFORM f4_smsht.

AT SELECTION-SCREEN OUTPUT.
  PERFORM init_screen.
  PERFORM hide_fields.
  cm_org = VALUE #( gt_f4orgs[ objid = so_orgid-low ]-stext OPTIONAL ).
  cm_sm  = VALUE #( gt_f4mods[ short = so_smsht-low ]-stext OPTIONAL ).

AT SELECTION-SCREEN.

START-OF-SELECTION.

  gv_mode = p_mode.

  CLEAR gt_overview.
  "Which mode?
  CASE p_mode.
    WHEN 'F'.
      "Start by uploading file
      "File path provided?
      IF p_lfile IS INITIAL.
        gv_inten = 'P_LFILE'.
        MESSAGE i002(/ilsiedu/bkg_fix).
        EXIT.
      ENDIF.

      "Generate class
      DATA(lo_excel) = NEW /ilsiedu/cl_excel_upload( iv_struc = '/ILSIEDU/BKG_FIX_MODULES' iv_file = p_lfile ).

      "Import, start from row 1
      gt_return = lo_excel->import( 1 ).

      "If return error?
      LOOP AT gt_return INTO DATA(ls_r) WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        cl_demo_output=>new(
        )->begin_section( |ERROR|
        )->write_data( value = gt_return name = 'Error Return'
        )->display( ).
        EXIT.
      ENDIF.

      CLEAR gt_return.

      "Get data from excel
      gv_data = lo_excel->get_data_table( ).
      ASSIGN gv_data->* TO <ft_modules>.

      "Data found?
      IF <ft_modules> IS NOT ASSIGNED.
        MESSAGE i005(/ilsiedu/bkg_fix) DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF <ft_modules> IS INITIAL.
        MESSAGE i006(/ilsiedu/bkg_fix) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      "Get ALV Data
      SELECT *
        FROM /ilsiedu/bkg_fix
        INTO CORRESPONDING FIELDS OF TABLE @gt_overview
        FOR ALL ENTRIES IN @<ft_modules>
        WHERE OObjid IN @gt_allowo
          AND SMShort = @<ft_modules>-shrt
          AND peryr   = @<ft_modules>-year
          AND perid   = @<ft_modules>-sess.

      SORT gt_overview BY smshort peryr perid.

      "Add 'OVCR' to the fields
      LOOP AT gt_overview INTO DATA(ls_og) GROUP BY ( short = ls_og-smshort peryr = ls_og-peryr perid = ls_og-perid  )
                                           INTO DATA(ls_key).

        READ TABLE <ft_modules> INTO DATA(ls_mod) WITH KEY shrt = ls_key-short
                                                           year = ls_key-peryr
                                                           sess = ls_key-perid.
        CHECK sy-subrc = 0 AND ls_mod-ovcr = abap_true.
        LOOP AT GROUP ls_key ASSIGNING FIELD-SYMBOL(<fs_rec>).
          <fs_rec>-override_cr = abap_true.
        ENDLOOP.
      ENDLOOP.

    WHEN 'M'.

      DATA: lr_orgid LIKE gt_allowo.
      CLEAR lr_orgid.

      "Manual Selection
      IF so_orgid IS INITIAL.
        lr_orgid = gt_allowo.
      ELSE.

        SELECT 'I' AS sign, 'EQ' AS option, objid AS low
          FROM hrp1000
          INTO CORRESPONDING FIELDS OF TABLE @lr_orgid
          WHERE otype = 'O'
            AND objid IN @gt_allowo
            AND objid IN @so_orgid.
      ENDIF.

      "Get ALV Data
      SELECT *
        FROM /ilsiedu/bkg_fix
        INTO CORRESPONDING FIELDS OF TABLE @gt_overview
        WHERE OObjid IN @lr_orgid
          AND SMShort IN @so_smsht
          AND peryr IN @so_peryr
          AND perid IN @so_perid
          AND StuNum IN @so_stnum.

    WHEN OTHERS.
      gv_inten = 'P_MODE'.
      MESSAGE i001(/ilsiedu/bkg_fix).
      EXIT.

  ENDCASE.

  "Data found?
  IF lines( gt_overview ) EQ 0.
    MESSAGE i011(/ilsiedu/bkg_fix) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gv_set_data = abap_true.

  IF p_skipo = abap_true.
    "Process Data
    PERFORM start_process CHANGING gt_overview.
  ENDIF.

  "Display overview to user
  CALL SCREEN '9000'.

FORM start_process CHANGING lt_ovw TYPE /ilsiedu/bkg_fix_ovw_t.

  CHECK p_displ = abap_false.

  IF prfcg_ac = abap_true.
    "Run in background
    CALL FUNCTION 'SPTA_PARA_PROCESS_START_2'
      EXPORTING
        server_group             = prfcg_sg
        max_no_of_tasks          = prfcg_mx
        before_rfc_callback_form = 'F_B4_ADJST'
        in_rfc_callback_form     = 'F_IN_ADJST'
        after_rfc_callback_form  = 'F_AF_ADJST'
        callback_prog            = sy-repid
      CHANGING
        user_param               = gt_overview
      EXCEPTIONS
        invalid_server_group     = 1
        no_resources_available   = 2
        OTHERS                   = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    "Run in foreground
    DATA: ls_task TYPE gty_rfcdata_sm.

    "Build Task
    PERFORM build_task CHANGING ls_task.
    ls_task-imp-stud = lt_ovw.

    "Process normally
    PERFORM process CHANGING ls_task.

    PERFORM update_ovw USING ls_task-exp-ret.

  ENDIF.

ENDFORM.

FORM update_ovw USING lt_ret TYPE /ilsiedu/bkg_fix_ovw_t.
  LOOP AT lt_ret INTO DATA(ls_ret).

    READ TABLE gt_overview ASSIGNING FIELD-SYMBOL(<fs_ovw>) WITH KEY bkgid = ls_ret-bkgid.
    IF sy-subrc = 0.
      <fs_ovw> = ls_ret.
    ELSE.
      APPEND ls_ret TO gt_overview.
    ENDIF.

  ENDLOOP.
ENDFORM.


FORM get_time USING iv_microsec TYPE int4
              CHANGING cv_sec TYPE f
                       cv_min TYPE int4
                       cv_hor TYPE int4.
  CLEAR: cv_min, cv_hor. "Start Fresh
  IF iv_microsec IS NOT INITIAL AND iv_microsec NE 0.
    cv_sec = iv_microsec / 1000000. "Determine Seconds
  ENDIF.
  "Determine Hours
  cv_hor = cv_sec / 3600. "3600 seconds per hour
  "Get remaining seconds
  cv_sec = cv_sec MOD 3600.

  "Get minutes
  cv_min = cv_sec / 60. "60 sec per min
  "Get remaining seconds
  cv_sec = cv_sec MOD 60.
ENDFORM.

FORM init_screen.

  CHECK gv_init = abap_false.
  gv_init = abap_true.

  AUTHORITY-CHECK OBJECT gc_authobj ID gc_actvt DUMMY.
  IF sy-subrc NE 0.
    MESSAGE e003(/ilsiedu/bkg_fix).
    RETURN.
  ENDIF.

  "Populate Mode Dropdown
  DATA: lt_vrm TYPE vrm_values.

  SELECT runmode AS key, runmodet AS value
    FROM /ilsiedu/bkgf_rm
    INTO TABLE @lt_vrm.

  LOOP AT lt_vrm INTO DATA(ls_v).
    AUTHORITY-CHECK OBJECT gc_authobj ID gc_runmode FIELD ls_v-key.
    IF sy-subrc NE 0.
      DELETE lt_vrm.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_MODE'
      values          = lt_vrm
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF lines( lt_vrm ) = 1.
    p_mode = lt_vrm[ 1 ]-key.
  ELSEIF lines( lt_vrm ) = 0.
    MESSAGE e004(/ilsiedu/bkg_fix).
    RETURN.
  ENDIF.

  "Check Display Mode Only
  AUTHORITY-CHECK OBJECT gc_authobj ID gc_actvt FIELD '02'. "Auth for activity Change
  IF sy-subrc NE 0.
    p_displ = abap_true.
    gv_disponly = abap_true.
    p_credi = p_earnd = p_scale = p_templ = abap_false.
  ENDIF.

  SELECT runparam
    FROM /ilsiedu/bkgf_rp
    INTO TABLE gt_allow_rp.

  LOOP AT gt_allow_rp INTO DATA(lv_rp).
    AUTHORITY-CHECK OBJECT gc_authobj ID gc_runparm FIELD lv_rp.
    IF sy-subrc NE 0.
      ASSIGN (lv_rp) TO FIELD-SYMBOL(<fs_fld>).
      <fs_fld> = abap_false.
      DELETE gt_allow_rp.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM hide_fields.
  "Hide/Display/Enable/Disable Fields

  LOOP AT SCREEN.

    IF screen-name = 'P_DISPL'.
      screen-input = COND #( WHEN gv_disponly = abap_true THEN 0 ELSE 1 ).
    ELSEIF screen-name = 'P_MODE' AND p_mode IS INITIAL.
      screen-intensified = 1.
    ELSEIF screen-group1 = 'FI'.
      "Only in file mode
      screen-active = COND #( WHEN p_mode = 'F' THEN 1 ELSE 0 ).
      ASSIGN (screen-name) TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc = 0 AND p_mode = 'F' AND <fs_field> IS INITIAL.
        screen-intensified = 1.
      ENDIF.
    ELSEIF screen-group1 = 'MS'.
      "Only in Module/Student Mode
      screen-active = COND #( WHEN p_mode = 'M' THEN 1 ELSE 0 ).
    ELSEIF screen-group1 = 'RP' AND screen-group3 NE 'TXT'.
      "Disable if diplay only is checked
      "Disable if not authorized
      screen-input = COND #( WHEN p_displ = abap_true THEN 0
                             WHEN NOT line_exists( gt_allow_rp[ runparam = screen-name ] ) THEN 0
                             ELSE 1 ).
    ELSEIF screen-name = 'PCNTR_TR'
        OR screen-name = 'PRFCG_AC'
        OR screen-name = 'PRFCG_SG'
        OR screen-name = 'PRFCG_MX'
        OR screen-name = 'PRFCG_PZ'
        OR screen-name = 'PCNTR_DT'
        OR screen-name = 'TRFCG_SG'
        OR screen-name = 'TRFCG_MX'
        OR screen-name = 'TRFCG_PZ'
        OR screen-name = 'TCNTFRM'.
      "hide fields if display only
      screen-active = COND #( WHEN p_displ = abap_true THEN 0 ELSE 1 ).
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.

FORM f4_orgid.

  DATA: lt_ret TYPE TABLE OF ddshretval,
        lt_mrk TYPE ddshmarks.

  IF so_orgid[] IS NOT INITIAL.
    LOOP AT gt_f4orgs TRANSPORTING NO FIELDS WHERE objid IN so_orgid[].
      APPEND sy-tabix TO lt_mrk.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      window_title    = 'Organizational Unit ID'
      value_org       = 'S'
      multiple_choice = abap_true
      mark_tab        = lt_mrk
    TABLES
      value_tab       = gt_f4orgs
      field_tab       = gt_orfies
      return_tab      = lt_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0.
    CLEAR so_orgid[].

    LOOP AT lt_ret INTO DATA(ls_ret).

      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_ret-fieldval ) TO so_orgid[].

    ENDLOOP.

    so_orgid = VALUE #( so_orgid[ 1 ] OPTIONAL ).

  ENDIF.

ENDFORM.

FORM f4_smsht.

  DATA: lt_ret TYPE TABLE OF ddshretval,
        lt_mrk TYPE ddshmarks.

  PERFORM init_mods.

  IF so_smsht[] IS NOT INITIAL.
    LOOP AT gt_f4mods TRANSPORTING NO FIELDS WHERE short IN so_smsht[].
      APPEND sy-tabix TO lt_mrk.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SHORT'
      window_title    = 'Module Short Code'
      value_org       = 'S'
      multiple_choice = abap_true
      mark_tab        = lt_mrk
    TABLES
      value_tab       = gt_f4mods
      field_tab       = gt_smfies
      return_tab      = lt_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0.
    CLEAR so_smsht[].

    LOOP AT lt_ret INTO DATA(ls_ret).

      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_ret-fieldval ) TO so_smsht[].

    ENDLOOP.

    so_smsht = VALUE #( so_smsht[ 1 ] OPTIONAL ).

  ENDIF.

ENDFORM.

FORM init_mods.

  CLEAR gt_f4mods.

  DATA: lr_orgs TYPE RANGE OF hrobjid.

  IF so_orgid[] IS NOT INITIAL.
    lr_orgs = so_orgid[].
  ELSE.
    lr_orgs = gt_allowo.
  ENDIF.

  DELETE lr_orgs WHERE sign IS INITIAL OR option IS INITIAL OR low IS INITIAL.

  SELECT short, stext
    FROM /ilsiedu/module_orgs
    INTO TABLE @gt_f4mods
    WHERE oobjid IN @lr_orgs.

  SORT gt_f4mods BY short ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_f4mods COMPARING short.

  IF gt_smfies IS INITIAL.
    "Get DFIES tab for search help
    DATA: lo_struc TYPE REF TO cl_abap_structdescr,
          ls_f4sm  TYPE /ilsiedu/smsht_f4.

    lo_struc ?= cl_abap_structdescr=>describe_by_data( ls_f4sm ).

    gt_smfies = lo_struc->get_ddic_field_list( ).
  ENDIF.

  gt_allowsm = VALUE #( FOR sm IN gt_f4mods ( sign = 'I' option = 'EQ' low = sm-short ) ).

ENDFORM.

FORM init_orgs.

  "Derive User Org
  GET BADI gr_userbadi.

  CALL BADI gr_userbadi->get_user_org
    EXPORTING
      iv_uname = sy-uname
    RECEIVING
      rv_orgid = gv_userorg.

  "Get org structure from users org down
  GET BADI gr_orgbadi.

  CALL BADI gr_orgbadi->get_org_struc
    EXPORTING
      iv_updwn = 'D'
      iv_texts = abap_true
      iv_objid = gv_userorg
    IMPORTING
      et_objec = gt_userorgs.

  DELETE gt_userorgs WHERE begda >= sy-datum OR endda <= sy-datum.

  DATA: ls_f4o TYPE /ilsiedu/org_f4.

  "Build out table of fields for F4
  LOOP AT gt_userorgs INTO DATA(ls_uo).
    MOVE-CORRESPONDING ls_uo TO ls_f4o.
    APPEND ls_f4o TO gt_f4orgs.
  ENDLOOP.

  SORT gt_f4orgs BY short ASCENDING objid.
  DELETE ADJACENT DUPLICATES FROM gt_f4orgs COMPARING short objid.

  "Get DFIES tab for search help
  DATA: lo_struc TYPE REF TO cl_abap_structdescr.

  lo_struc ?= cl_abap_structdescr=>describe_by_data( ls_f4o ).

  gt_orfies = lo_struc->get_ddic_field_list( ).

  gt_allowo = VALUE #( FOR or IN gt_f4orgs ( sign = 'I' option = 'EQ' low = or-objid ) ).

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST_FILE
*&---------------------------------------------------------------------*
FORM value_request_file  CHANGING ca_file TYPE string.

  DATA: lt_filelist TYPE filetable,
        lv_rc       TYPE i.

* Select upload file:
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select input data file'
      file_filter             = '*.*'
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
    READ TABLE lt_filelist INDEX 1 INTO ca_file.
  ENDIF.

ENDFORM.                    " VALUE_REQUEST_FILE

*&———————————————————————*
*&      Form F_B4_ATTEM
*&———————————————————————*
*       Perform before RFC call, prep objects
*———————————————————————-*
FORM f_b4_adjst   USING p_before_rfc_imp      TYPE spta_t_before_rfc_imp
               CHANGING cs_before_rfc_exp     TYPE spta_t_before_rfc_exp
                        ct_rfcdata            TYPE spta_t_indxtab
                        ct_failed_objects     TYPE spta_t_failed_objects
                        ct_objects_in_process TYPE spta_t_objects_in_process
                        ct_stud               TYPE /ilsiedu/bkg_fix_ovw_t.
  DATA: lines        TYPE i VALUE 1,
        lt_stud      TYPE /ilsiedu/bkg_fix_ovw_t,
        ls_task_data TYPE gty_rfcdata_sm.

  IF lines( ct_stud ) = 0.
    CLEAR cs_before_rfc_exp-start_rfc.
    RETURN.
  ENDIF.

  APPEND LINES OF ct_stud FROM 1 TO prfcg_pz TO lt_stud.
  DELETE ct_stud FROM 1 TO prfcg_pz.

  ls_task_data-imp-stud = lt_stud.
  ls_task_data-imp-test = pcntr_tr.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = ls_task_data
    IMPORTING
      indxtab = ct_rfcdata.

  cs_before_rfc_exp-start_rfc = abap_true.

ENDFORM.


*&———————————————————————*
*&      Form F_IN_ATTEM
*&———————————————————————*
*       Perform in RFC call, process objects
*———————————————————————-*
FORM f_in_adjst USING ps_in_rfc_imp TYPE spta_t_in_rfc_imp
             CHANGING cs_in_rfc_exp TYPE spta_t_in_rfc_exp
                      ct_rfcdata    TYPE spta_t_indxtab.

  DATA: ls_task_data TYPE gty_rfcdata_sm.

  SET UPDATE TASK LOCAL.
  "Get task Data
  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = ct_rfcdata
    IMPORTING
      data    = ls_task_data.

  PERFORM process CHANGING ls_task_data.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = ls_task_data
    IMPORTING
      indxtab = ct_rfcdata.

ENDFORM.


*&———————————————————————*
*&      Form F_AF_ATTEM
*&———————————————————————*
*       Perform after RFC call
*———————————————————————-*
FORM f_af_adjst USING pt_rfcdata            TYPE spta_t_indxtab
                      pv_rfcsubrc           TYPE sy-subrc
                      pv_rfcmsg             TYPE spta_t_rfcmsg
                      pt_objects_in_process TYPE spta_t_objects_in_process
                      ps_after_rfc_imp      TYPE spta_t_after_rfc_imp
             CHANGING cs_after_rfc_exp      TYPE spta_t_after_rfc_exp
                      ct_stud               TYPE /ilsiedu/bkg_fix_alv_t.

  DATA: ls_task_data TYPE gty_rfcdata_sm.
  "Get task Data
  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = pt_rfcdata
    IMPORTING
      data    = ls_task_data.

  PERFORM update_ovw USING ls_task_data-exp-ret.

  lv_processed += lines( ls_task_data-imp-stud ).
  lv_time += ls_task_data-exp-runt.

  cl_progress_indicator=>progress_indicate( i_text = 'Processed &1% (&2 of &3) of courses adjusting credit values' i_processed = lv_processed i_total = lv_total i_output_immediately = abap_true ).

ENDFORM.

FORM process CHANGING ls_task_data TYPE gty_rfcdata_sm.

  TYPES: BEGIN OF ty_bkg,
           mod TYPE hrpad506,
           fol TYPE piqdbagr_foll_up,
           gen TYPE piqdbagr_gen,
           otj TYPE otjid,
           stu TYPE piqstudent12,
           stn TYPE piqstudentname,
         END OF ty_bkg.

  DATA: lt_bkg TYPE TABLE OF ty_bkg,
        ls_alv TYPE /ilsiedu/bkg_fix_alv.

  DATA: lt_mod TYPE TABLE OF hrpad506,
        lt_fol TYPE TABLE OF piqdbagr_foll_up,
        lt_gen TYPE TABLE OF piqdbagr_gen.

  "Initiate Timer
  DATA(timer) = cl_abap_runtime=>create_lr_timer( ).
  DATA(rfc) = timer->get_runtime( ).

  "Get module list in local table
  DATA(lt_studs) = ls_task_data-imp-stud.
  DATA(lv_test) = ls_task_data-imp-test.

  CLEAR ls_task_data-imp-stud.

  CHECK lt_studs IS NOT INITIAL.

  "Get 506, foll_up, and gen records into local table
  SELECT mod~*, fol~*, gen~*, sms~otjid, stu~student12, nam~stext
    FROM hrpad506 AS mod
    INNER JOIN hrp1001 AS sms
      ON  mod~adatanr = sms~adatanr
      AND sms~otype = 'SM'
      AND sms~sclas = 'ST'
      AND sms~relat = '506'
    INNER JOIN cmacbpst AS stu
      ON  sms~sobid = stu~stobjid
    INNER JOIN hrp1000 AS nam
      ON  stu~stobjid = nam~objid
      AND nam~otype = 'ST'
      AND nam~begda <= @sy-datum
      AND nam~endda >= @sy-datum
    "Left outer join these tables in case they don't exist, so we can update as much as possible
    LEFT OUTER JOIN piqdbagr_assignm AS asg
      ON  mod~id = asg~modreg_id
    LEFT OUTER JOIN piqdbagr_foll_up AS fol
      ON  asg~agrid = fol~agrid
    LEFT OUTER JOIN piqdbagr_gen AS gen
      ON  asg~agrid = gen~agrid
    INTO TABLE @lt_bkg
    FOR ALL ENTRIES IN @lt_studs
      WHERE mod~id = @lt_studs-bkgid.

  "Loop through all bookings for this module
  LOOP AT lt_bkg ASSIGNING FIELD-SYMBOL(<fs_b>).

    "Get booking record
    READ TABLE lt_studs INTO DATA(ls_stud) WITH KEY bkgid = <fs_b>-mod-id.
    IF sy-subrc NE 0.
      APPEND ls_stud TO ls_task_data-exp-ret.
      CONTINUE.
    ENDIF.

    IF ls_task_data-imp-tmpl = 'X'.
      "Set the template for everyone
      <fs_b>-mod-zztempl_id = ls_stud-bkgtempl_id.
    ENDIF.

    IF ls_task_data-imp-scle = 'X'.
      "Set the grade scale for everyone
      <fs_b>-gen-gradescale = ls_stud-gradescale.
    ENDIF.

    IF ls_task_data-imp-cred = 'X'.
      "Set the attempted credits for everyone to the optimum (506 AND foll_up)
      <fs_b>-fol-cpattempfu = <fs_b>-mod-cpattemp = ls_stud-calc_cpattempfu.
      "Set earned/graded credits
      <fs_b>-fol-cpearnedfu = ls_stud-calc_cpearnedfu.
      <fs_b>-fol-cpgradedfu = ls_stud-calc_cpgradedfu.
    ENDIF.

    IF ls_task_data-imp-earn = 'X'.
      <fs_b>-fol-cpearnedfu = ls_stud-calc_cpearnedfu.
      <fs_b>-fol-cpgradedfu = ls_stud-calc_cpgradedfu.
    ENDIF.

    "Set to corrected
    ls_stud-complete = abap_true.
    APPEND ls_stud TO ls_task_data-exp-ret.

    "Add records to DB update tables
    IF <fs_b>-mod-id IS NOT INITIAL.
      APPEND <fs_b>-mod TO lt_mod.
    ENDIF.
    IF <fs_b>-fol-agrid IS NOT INITIAL.
      APPEND <fs_b>-fol TO lt_fol.
    ENDIF.
    IF <fs_b>-gen-agrid IS NOT INITIAL.
      APPEND <fs_b>-gen TO lt_gen.
    ENDIF.

  ENDLOOP.

  CLEAR: lt_bkg, lt_studs.

  IF lv_test IS INITIAL.

    "Post updates to all three DB tables
    MODIFY piqdbagr_gen FROM TABLE lt_gen.
    ls_task_data-exp-cnt += sy-dbcnt.
    COMMIT WORK.

    MODIFY piqdbagr_foll_up FROM TABLE lt_fol.
    ls_task_data-exp-cnt += sy-dbcnt.
    COMMIT WORK.

    MODIFY hrpad506 FROM TABLE lt_mod.
    ls_task_data-exp-cnt += sy-dbcnt.
    COMMIT WORK.
  ENDIF.

  DATA(rfc_end) = timer->get_runtime( ).

  ls_task_data-exp-runt = ( rfc_end - rfc ) / 1000000.

ENDFORM.

FORM build_task CHANGING ls_task TYPE gty_rfcdata_sm.
  ls_task-imp-cred = p_credi.
  ls_task-imp-earn = p_earnd.
  ls_task-imp-scle = p_scale.
  ls_task-imp-test = pcntr_tr.
  ls_task-imp-tmpl = p_templ.
ENDFORM.
