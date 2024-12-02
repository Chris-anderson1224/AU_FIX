CLASS /ilsiedu/cl_dmr_logger DEFINITION
  PUBLIC
  INHERITING FROM /ilsiedu/cl_logger
  CREATE PUBLIC .

  PUBLIC SECTION.

  METHODS log_extra
    IMPORTING
      iv_action TYPE /ilsiedu/dmr_action
      iv_prg_runid TYPE /ilsiedu/dmr_runid.

  METHODS get_logs_recid
    IMPORTING
      iv_recid TYPE /ilsiedu/recid
      iv_thisr TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rt_logs) TYPE /ilsiedu/return_t.

  methods GET_LOGS
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ilsiedu/cl_dmr_logger IMPLEMENTATION.

  METHOD get_logs.
    CHECK gv_runid IS NOT INITIAL.
    super->get_logs(
              IMPORTING
                es_pgrun = es_pgrun
                et_params = et_params
                et_return = et_return ).

    DATA: lt_extra TYPE TABLE OF /ilsiedu/lg_dmr.

    SELECT *
      FROM /ilsiedu/lg_dmr
      INTO TABLE lt_extra
      WHERE runid = es_pgrun-runid.

    ASSIGN lt_extra TO FIELD-SYMBOL(<fs_r>).
    et_extra = <fs_r>.

  ENDMETHOD.

  METHOD log_extra.
    DATA ls_extra TYPE /ilsiedu/lg_dmr.

    ls_extra-action = iv_action.
    ls_extra-runid = gv_runid.
    ls_extra-prg_runid = iv_prg_runid.

    INSERT /ilsiedu/lg_dmr FROM ls_extra.
    COMMIT WORK.

  ENDMETHOD.

  METHOD get_logs_recid.
    IF iv_thisr = abap_false.
    SELECT *
      FROM /ilsiedu/lg_rtrn
      INTO CORRESPONDING FIELDS OF TABLE rt_logs
      WHERE recid = iv_recid.
    ELSE.
    SELECT *
      FROM /ilsiedu/lg_rtrn
      INTO CORRESPONDING FIELDS OF TABLE rt_logs
      WHERE recid = iv_recid
        AND runid = gv_runid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
