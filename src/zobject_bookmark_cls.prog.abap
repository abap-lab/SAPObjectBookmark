*----------------------------------------------------------------------*
***INCLUDE ZOBJECT_BOOKMARK_CLS.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class LCL_APPLICATION
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
         OF cl_gui_simple_tree
        IMPORTING node_key,

      handle_expand_no_children
        FOR EVENT expand_no_children
         OF cl_gui_simple_tree
        IMPORTING node_key,

      handle_node_cm_req
        FOR EVENT node_context_menu_request OF cl_gui_simple_tree
        IMPORTING node_key menu,

      handle_node_cm_sel
        FOR EVENT node_context_menu_select OF cl_gui_simple_tree
        IMPORTING node_key fcode sender.


ENDCLASS.               "LCL_APPLICATION
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_APPLICATION
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance
    " show the key of the double clicked node in a dynpro field
    g_event = 'NODE_DOUBLE_CLICK'.
    g_node_key = node_key.

    PERFORM tabnm_choose USING node_key.

    IF g_tabnm_old <> g_tabnm.
      CLEAR : gv_selid, gv_active, gt_ranges[], g_change, g_up.
      g_tabnm_old = g_tabnm.
    ENDIF.

  ENDMETHOD.

  METHOD handle_expand_no_children.
    " this method handles the expand no children event of the tree
                                       " control instance
    DATA: node_table TYPE node_table_type,
          node       TYPE mtreesnode.

    " show the key of the double clicked node in a dynpro field
    g_event = 'EXPAND_NO_CHILDREN'.
    g_node_key = node_key.

  ENDMETHOD.

  METHOD   handle_node_cm_req.
    CALL METHOD menu->clear.

    IF node_key = 'Root'.
**0. 현재 Tree 구조 저장
      CALL METHOD menu->add_function
        EXPORTING
           fcode = 'T_APPLY'
           text  = 'Table Apply'
           icon  = '@JG@'.           "Tree_APPLY
    ELSE.
** In this case the standard menu is cleared.
** The next line defines one line of the context menu.
**1. TABLE 추가
**2. TABLE 삭제
**3. TABLE LAYOUT 다운로드
**4. TABLE EXCEL UPLOAD
**-----구분기호    ADD_SEPARATOR

**1. TABLE 추가
       CALL METHOD menu->add_function
           EXPORTING
              fcode = 'T_ADD'
*              icon  = icon_insert_favorites
              text  = 'Table ADD'.           "TABLE_ADD

**2. TABLE 삭제
       CALL METHOD menu->add_function
           EXPORTING
              fcode = 'T_DEL'
*              icon  = icon_delete_favorites
              text  = 'Table Delete'.        "TABLE Delete
**구분 선
       CALL METHOD menu->add_separator .     "Separator

**3. TABLE Change
       CALL METHOD menu->add_function
           EXPORTING
              fcode = 'T_CHANGE'
              text  = 'Node info Change'.        "TABLE Change
**구분 선
       CALL METHOD menu->add_separator .     "Separator

**4. TABLE LAYOUT 다운로드
       CALL METHOD menu->add_function
           EXPORTING
              fcode = 'T_LAYOUT'
*              icon  = icon_wd_table_column
              text  = 'Table Layout'.        "Delete Subtree
**5. TABLE EXCEL UPLOAD
       CALL METHOD menu->add_function
           EXPORTING
              fcode = 'T_UPLOAD'
*              icon  = icon_mass_change
              text  = 'Table EXCEL UPLOAD'.  "TABLE EXCEL UPLOAD
    ENDIF.

  ENDMETHOD.

  METHOD handle_node_cm_sel.

    PERFORM command_function USING fcode node_key .

  ENDMETHOD.
ENDCLASS.               "LCL_APPLICATION
*&---------------------------------------------------------------------*
*&       Class lcl_event_receiver
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

ENDCLASS.               "lcl_event_receiver
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_receiver
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
* § 2.In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
    DATA: ls_toolbar  TYPE stb_button.
*....................................................................
* E_OBJECT of event TOOLBAR is of type REF TO CL_ALV_EVENT_TOOLBAR_SET.
* This class has got one attribute, namly MT_TOOLBAR, which
* is a table of type TTB_BUTTON. One line of this table is
* defined by the Structure STB_BUTTON (see data deklaration above).
*

* A remark to the flag E_INTERACTIVE:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*         'e_interactive' is set, if this event is raised due to
*         the call of 'set_toolbar_interactive' by the user.
*         You can distinguish this way if the event was raised
*         by yourself or by ALV
*         (e.g. in method 'refresh_table_display').
*         An application of this feature is still unknown... :-)

* append a separator to normal toolbar
    CLEAR ls_toolbar.
     MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar       TO e_object->mt_toolbar.

* append an icon to show booking table
    CLEAR ls_toolbar.
     MOVE 'FREE_Q'          TO ls_toolbar-function.
     MOVE icon_fencing      TO ls_toolbar-icon.
     MOVE 'DYNAMIC_SELECTION '       TO ls_toolbar-quickinfo.

*     g_count = lines( gs_where-where_tab ).
*     IF g_count = 0.
        MOVE 'Free SQL'     TO ls_toolbar-text.
*     ELSE.
*       ls_toolbar-text = ls_toolbar-text &&  g_count.
*       CONCATENATE 'Free SQL(' ls_toolbar-text ')' into  ls_toolbar-text.
*     ENDIF.
     MOVE ' '               TO ls_toolbar-disabled.
    APPEND ls_toolbar       TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
     MOVE 'MODIFY'          TO ls_toolbar-function.

     IF g_change = 'X'.
        MOVE 'Display'      TO ls_toolbar-text.
        MOVE icon_display   TO ls_toolbar-icon.
        MOVE 'Data Display' TO ls_toolbar-quickinfo.
     ELSE.
        MOVE 'Modify'       TO ls_toolbar-text.
        MOVE icon_toggle_display_change TO ls_toolbar-icon.
        MOVE 'Data Modify'  TO ls_toolbar-quickinfo.
     ENDIF.
     MOVE ' '               TO ls_toolbar-disabled.

    APPEND ls_toolbar       TO e_object->mt_toolbar.

  ENDMETHOD.
*-------------------------------------------------------------------
  METHOD handle_user_command.
* § 3.In event handler method for event USER_COMMAND: Query your
*   function codes defined in step 2 and react accordingly.

    DATA: lt_rows TYPE lvc_t_row.

    CASE e_ucomm.
      WHEN 'FREE_Q'.
        PERFORM dynamic_selection .
*        CALL METHOD grid->get_selected_rows
*                 IMPORTING et_index_rows = lt_rows.
*        CALL METHOD cl_gui_cfw=>flush.
*        IF sy-subrc ne 0.
* add your handling, for example
*          CALL FUNCTION 'POPUP_TO_INFORM'
*               EXPORTING
*                    titel = g_repid
*                    txt2  = sy-subrc
*                    txt1  = 'Error in Flush'.
*        else.
*                  perform show_booking_table tables lt_rows.
*        ENDIF.
      WHEN 'MODIFY'.
        IF g_change = 'X'.
          CLEAR : g_change.
        ELSE.
          g_change = 'X'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
*----------
ENDCLASS.               "lcl_event_receiver
