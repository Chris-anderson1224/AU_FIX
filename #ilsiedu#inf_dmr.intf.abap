INTERFACE /ilsiedu/inf_dmr
  PUBLIC .


  METHODS get_upload_struc
    RETURNING
      VALUE(rv_struc) TYPE /ilsiedu/struc_name .
  METHODS get_dynnr
    EXPORTING
      !ev_dynnr TYPE dynnr
      !ev_repid TYPE repid .
  METHODS match_records
    EXPORTING
      !ev_error  TYPE xfeld
      !et_rettab TYPE /ilsiedu/return_t
    CHANGING
      !cv_header TYPE /ilsiedu/dmrhdr
      !ct_data   TYPE /ilsiedu/dmrdata_t
      !CO_logger TYPE REF TO /ilsiedu/cl_dmr_logger .
  METHODS process_records
    EXPORTING
      !ev_error  TYPE xfeld
      !et_rettab TYPE /ilsiedu/return_t
    CHANGING
      !cv_header TYPE /ilsiedu/dmrhdr
      !ct_data   TYPE /ilsiedu/dmrdata_t
      !CO_logger TYPE REF TO /ilsiedu/cl_dmr_logger .
  METHODS get_alv_struc
    RETURNING
      VALUE(rv_struc) TYPE /ilsiedu/struc_name .
  METHODS change_fcat
    CHANGING
      !ct_fcat TYPE lvc_t_fcat .
  METHODS change_layout
    CHANGING
      !cs_layout TYPE lvc_s_layo .

  CONSTANTS: co_msgid TYPE symsgid VALUE '/ILSIEDU/DMR'.

ENDINTERFACE.
