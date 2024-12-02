*&---------------------------------------------------------------------*
*& Include /ilsiedu/booking_fix_top
*&---------------------------------------------------------------------*

CLASS lcl_ovw DEFINITION DEFERRED.

DATA: go_grid     TYPE REF TO lcl_ovw,
      gt_overview TYPE /ilsiedu/bkg_fix_ovw_t,
      gv_mode     TYPE char1,
      gv_init     TYPE xfeld,
      gv_set_data TYPE xfeld.

CONSTANTS: gc_authobj TYPE xuobject VALUE 'ZBKGFIX',
           gc_actvt   TYPE fieldname VALUE 'ACTVT',
           gc_orgmode TYPE fieldname VALUE 'ZBKGFIX_OG',
           gc_runmode TYPE fieldname VALUE 'ZBKGFIX_MD',
           gc_runparm TYPE fieldname VALUE 'ZBKGFIX_RP'.

TYPES: BEGIN OF gty_rfcdata_sm,
         BEGIN OF imp,
           stud TYPE /ilsiedu/bkg_fix_ovw_t,
           test TYPE xfeld,
           tmpl TYPE xfeld,
           scle TYPE xfeld,
           cred TYPE xfeld,
           earn TYPE xfeld,
         END OF imp,
         BEGIN OF exp,
           cnt  TYPE i,
           runt TYPE f,
           ret  TYPE /ilsiedu/bkg_fix_ovw_t,
         END OF exp,
       END OF gty_rfcdata_sm.

DATA: lv_hour      TYPE int4,
      lv_min       TYPE int4,
      lv_sec       TYPE f,
      lv_time      TYPE f,
      lv_processed TYPE i,
      lv_total     TYPE i,
      gt_return    TYPE bapiret2_t,
      gt_alvout    TYPE /ilsiedu/bkg_fix_alv_t,
      gt_modules   TYPE TABLE OF /ilsiedu/bkg_fix_modules,
      gv_data      TYPE REF TO data.

DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv,
      dyn_ref         TYPE REF TO data.

FIELD-SYMBOLS : <wa_fieldcatalog> TYPE slis_fieldcat_alv,
                <ft_modules>      LIKE gt_modules.

DATA: gr_orgbadi  TYPE REF TO /ilsiedu/def_org_struc_derive,
      gr_userbadi TYPE REF TO /ilsiedu/def_user_org_derive,
      gv_userorg  TYPE hrobjid,
      gt_userorgs TYPE objec_t,
      gt_f4orgs   TYPE /ilsiedu/org_f4_t,
      gt_allowo   TYPE RANGE OF hrobjid,
      gt_allowsm  TYPE RANGE OF piqsmshort,
      gt_f4mods   TYPE /ilsiedu/smsht_f4_t,
      gt_orfies   TYPE ddfields,
      gt_smfies   TYPE ddfields,
      gv_inten    TYPE string,
      gv_disponly TYPE xfeld,
      gt_allow_rp TYPE TABLE OF /ilsiedu/bkgf_rp.

* Driver includes
INCLUDE /ilsiedu/booking_fixcl1.
INCLUDE /ilsiedu/booking_fixo01.
INCLUDE /ilsiedu/booking_fixi01.
