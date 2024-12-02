*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ILSIEDU/DMRACTN................................*
DATA:  BEGIN OF STATUS_/ILSIEDU/DMRACTN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ILSIEDU/DMRACTN              .
CONTROLS: TCTRL_/ILSIEDU/DMRACTN
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: /ILSIEDU/VDMRM..................................*
TABLES: /ILSIEDU/VDMRM, */ILSIEDU/VDMRM. "view work areas
CONTROLS: TCTRL_/ILSIEDU/VDMRM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/ILSIEDU/VDMRM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/ILSIEDU/VDMRM.
* Table for entries selected to show on screen
DATA: BEGIN OF /ILSIEDU/VDMRM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /ILSIEDU/VDMRM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /ILSIEDU/VDMRM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /ILSIEDU/VDMRM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /ILSIEDU/VDMRM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /ILSIEDU/VDMRM_TOTAL.

*.........table declarations:.................................*
TABLES: */ILSIEDU/DMRACTN              .
TABLES: /ILSIEDU/DMRACTN               .
TABLES: /ILSIEDU/DMRM                  .
TABLES: /ILSIEDU/DMRMT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
