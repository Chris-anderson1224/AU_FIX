*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ILSIEDU/BKGF_OM................................*
DATA:  BEGIN OF STATUS_/ILSIEDU/BKGF_OM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ILSIEDU/BKGF_OM              .
CONTROLS: TCTRL_/ILSIEDU/BKGF_OM
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: /ILSIEDU/BKGF_RM................................*
DATA:  BEGIN OF STATUS_/ILSIEDU/BKGF_RM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ILSIEDU/BKGF_RM              .
CONTROLS: TCTRL_/ILSIEDU/BKGF_RM
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: /ILSIEDU/BKGF_RP................................*
DATA:  BEGIN OF STATUS_/ILSIEDU/BKGF_RP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ILSIEDU/BKGF_RP              .
CONTROLS: TCTRL_/ILSIEDU/BKGF_RP
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: */ILSIEDU/BKGF_OM              .
TABLES: */ILSIEDU/BKGF_RM              .
TABLES: */ILSIEDU/BKGF_RP              .
TABLES: /ILSIEDU/BKGF_OM               .
TABLES: /ILSIEDU/BKGF_RM               .
TABLES: /ILSIEDU/BKGF_RP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
