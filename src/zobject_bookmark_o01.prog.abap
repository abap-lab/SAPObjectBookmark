*----------------------------------------------------------------------*
***INCLUDE ZOBJECT_BOOKMARK_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
************************
  DATA : node_table TYPE node_table_type,
         events     TYPE cntl_simple_events,
         event      TYPE cntl_simple_event.
  DATA : l_title    TYPE string.
************************
  SET PF-STATUS 'STATUS'.
************************
  IF g_tabnm IS INITIAL.
    l_title = sy-uname.
  ELSE.
    CONCATENATE sy-uname '[' g_tabnm ']' INTO l_title.
  ENDIF.

  SET TITLEBAR '0100' WITH l_title.
************************
  IF init IS INITIAL.
    repid = sy-repid.
    dynnr = sy-dynnr.
************************
* create the docking container
    CREATE OBJECT docking
        EXPORTING repid      = repid
                  dynnr      = dynnr
                  side       = docking->dock_at_left
                  extension  = 350.


* create the TREE container
    CREATE OBJECT tree
        EXPORTING parent            = docking
                  node_selection_mode
                     = cl_gui_simple_tree=>node_sel_mode_single
                  hide_selection    = 'X'
              EXCEPTIONS
                  lifetime_error              = 1
                  cntl_system_error           = 2
                  create_error                = 3
                  failed                      = 4
                  illegal_node_selection_mode = 5.

* create alv control
    CREATE OBJECT grid
        EXPORTING i_parent = docking.

************************
*   define the events which will be passed to the backend
    " node double click

    event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
    event-appl_event = 'X'. " process PAI if event occurs
    APPEND event TO events.

    " expand no children
    event-eventid = cl_gui_simple_tree=>eventid_expand_no_children.
    event-appl_event = 'X'.
    APPEND event TO events.

    event-eventid = cl_gui_simple_tree=>eventid_node_context_menu_req.
    APPEND event TO events.

    CALL METHOD tree->set_registered_events
      EXPORTING
        events = events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

*    IF sy-subrc <> 0.
*      MESSAGE a000.
*    ENDIF.
*
************************
*   assign event handlers in the application class to each desired event
    SET HANDLER g_application->handle_node_double_click  FOR tree.
    SET HANDLER g_application->handle_expand_no_children FOR tree.
    SET HANDLER g_application->handle_node_cm_req        FOR tree.
    SET HANDLER g_application->handle_node_cm_sel        FOR tree.
************************
** Tree 기초 값 Display
    PERFORM build_node_table USING node_table.
************************
*** Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD grid->set_toolbar_interactive.
************************
    init = 'X'.

  ENDIF.
************************
  PERFORM get_data.
************************
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_display OUTPUT.
  CHECK g_node_key(4) <> 'TEMP'.

  IF NOT g_tabnm IS INITIAL.
    PERFORM alv_grid.
  ENDIF.

ENDMODULE.                 " ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
   SET PF-STATUS 'STATUS_101'.
*  SET TITLEBAR 'xxx'.

   CHECK g_ok_code = 'T_CHANGE'.
   CHECK g_101_tab_id IS NOT INITIAL.

   LOOP AT SCREEN.
      IF screen-name = 'g_101_tab_id'.
        screen-input  = '0'.

        MODIFY SCREEN.

      ENDIF.
    ENDLOOP.

ENDMODULE.                 " STATUS_0101  OUTPUT
