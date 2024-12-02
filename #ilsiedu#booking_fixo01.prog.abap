*----------------------------------------------------------------------*
***INCLUDE /ILSIEDU/BOOKING_FIXO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module INIT_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_screen OUTPUT.

  IF p_displ = abap_true.
    SET PF-STATUS 'STATUS_9000_DISPL'.
    SET TITLEBAR 'TITLE_9000_DISP'.
  ELSE.
    SET PF-STATUS 'STATUS_9000'.
    SET TITLEBAR 'TITLE_9000'.
  ENDIF.

  IF go_grid IS NOT BOUND.
    lcl_ovw=>init_grid( ).
  ELSEIF gv_set_data = abap_true.
    go_grid->set_data( ).
  ENDIF.

ENDMODULE.
