*&---------------------------------------------------------------------*
*& Include /ILSIEDU/DMREVIEWTOP                     - Module Pool      /ILSIEDU/DMREVIEW
*&---------------------------------------------------------------------*
PROGRAM /ILSIEDU/DMREVIEW.

CLASS lcl_grid DEFINITION DEFERRED.

DATA: go_logger TYPE REF TO /ilsiedu/cl_dmr_logger,
      go_logdis TYPE REF TO /ilsiedu/cl_dmr_log_display.

DATA: go_grid TYPE REF TO lcl_grid,
      gv_set_data TYPE abap_bool.

DATA: gt_runmode TYPE TABLE OF /ilsiedu/vdmrm,
      gt_rmdd    TYPE vrm_values,
      gv_init    TYPE abap_bool,
      gv_newrm   TYPE abap_bool VALUE abap_true,
      gt_actndd  TYPE vrm_values,
      go_data    TYPE REF TO DATA,
      go_rmclass TYPE REF TO /ilsiedu/inf_dmr,
      gv_dynnr   TYPE dynnr,
      gv_repid   TYPE repid,
      gv_stop    TYPE abap_bool.

DATA: gs_runhdr  TYPE /ilsiedu/dmrhdr,
      gt_rundata TYPE TABLE OF /ilsiedu/dmrdata,
      gs_rundata TYPE /ilsiedu/dmrdata.

DATA: gs_prchdr  TYPE /ilsiedu/dmrhdr,
      gt_prcdata TYPE TABLE OF /ilsiedu/dmrdata,
      gs_prcdata TYPE /ilsiedu/dmrdata.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

"Screen values
DATA: gs_runmode TYPE /ilsiedu/vdmrm,
      action  TYPE /ilsiedu/dmr_action,
      file    TYPE string,
      runid   TYPE /ilsiedu/dmr_runid.


LOAD-OF-PROGRAM.
  "Initialize parameters
  PERFORM init_program.
