*----------------------------------------------------------------------*
***INCLUDE ZOBJECT_BOOKMARK_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: return_code TYPE i.

  g_ok_code = sy-ucomm.

** 현재 Tree 상태(Node) 체크
  CALL METHOD cl_gui_cfw=>dispatch
    IMPORTING return_code = return_code.

  IF return_code <> cl_gui_cfw=>rc_noevent.
    " a control event occured => exit PAI
    CLEAR g_ok_code.
    EXIT.
  ENDIF.

  CASE g_ok_code.
    WHEN 'REFRESH' OR 'T_ADD' OR 'T_APPLY' OR 'SAVE'.

      PERFORM command_function USING g_ok_code 'N'.

    WHEN 'BACK' OR 'EXIT'. " Finish program
      IF NOT docking IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD docking->free
          EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2.
        IF sy-subrc <> 0.
          MESSAGE a000.
        ENDIF.

        CLEAR : docking, tree, grid.
      ENDIF.
      LEAVE PROGRAM.
  ENDCASE.

* CAUTION: clear ok code!
  CLEAR g_ok_code.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  DATA : lv_data_status TYPE c.

  ok_code = sy-ucomm.
*********************************************************
*** Dup check.
  IF g_ok_code = 'T_ADD' .
    READ TABLE g_tot_data WITH KEY tabname = g_101_tab_id.

    IF sy-subrc = 0.
       MESSAGE s527(00) WITH  g_101_tab_id.

       lv_data_status = 'X'.
       g_101_tab_id   = g_tot_data-tabname.
       g_101_t_name   = g_tot_data-ddtext.
       g_101_t_desc   = g_tot_data-descript.
    ELSE.
       CLEAR : g_101_t_name, g_101_t_desc.
    ENDIF.
  ENDIF.

*** table name check
  SELECT SINGLE ddtext
    INTO g_101_t_name
    FROM dd02t
   WHERE tabname    = g_101_tab_id
     AND ddlanguage = sy-langu.

  IF sy-subrc <> 0.
     g_101_t_name = g_101_t_desc = 'No Table Name'.
     lv_data_status = 'X'.
  ENDIF.

*********************************************************
  IF g_101_t_desc IS INITIAL.
     g_101_t_desc = g_101_t_name.
  ENDIF.
*********************************************************
  CASE ok_code.
    WHEN 'ADD'.

      IF lv_data_status IS NOT INITIAL.
         CLEAR : g_101_tab_id, g_101_t_name, g_101_t_desc.
      ENDIF.

      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'EXIT'.
      CLEAR : g_101_tab_id, g_101_t_name, g_101_t_desc.

      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.

  ENDCASE.
*********************************************************
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
