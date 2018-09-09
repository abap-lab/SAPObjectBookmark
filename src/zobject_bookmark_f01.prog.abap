*----------------------------------------------------------------------*
***INCLUDE ZOBJECT_BOOKMARK_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*----------------------------------------------------------------------*
FORM build_node_table  USING
        node_table TYPE node_table_type.

  DATA: node          LIKE mtreesnode.

  CONCATENATE sy-uname '_T' INTO g_loc_setid.

**** SET(GS03) 확인
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
*     CLIENT        = ' '
*     FORMULA_RETRIEVAL           = ' '
      level         = 0
      setnr         = g_loc_setid
*     VARIABLES_REPLACEMENT       = ' '
*     TABLE         = ' '
      class         = '0000'
*     NO_DESCRIPTIONS             = 'X'
*     NO_RW_INFO    = 'X'
*     DATE_FROM     =
*     DATE_TO       =
*     FIELDNAME     = ' '
    TABLES
      set_values    = g_set_values
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
*     Implement suitable error handling here
*     만일, SET 가 없으면 SET 생성 한다(ID_T 로 생성)
    PERFORM tree_set_generate.

  ENDIF.
**********************************************************
*** Set 존재시 total data 생성
  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE g_tot_data
   FROM setleaf AS a INNER JOIN setlinet AS b
                           ON a~setname     = b~setname
                          AND a~setclass    = b~setclass
                          AND a~lineid      = b~lineid
                     INNER JOIN dd02t AS c
                          ON  a~valfrom     = c~tabname
                          AND b~langu       = c~ddlanguage
  WHERE a~setclass    = '0000'
    AND a~setname     = g_loc_setid
    AND b~langu       = sy-langu
    AND c~as4local    = 'A'.

**********************************************************
*   Build the node table.
*   Caution: The nodes are inserted into the tree according to the order
*   in which they occur in the table. In consequence, a node must not
*   occur in the node table before its parent node.

*   Node with key 'Root'
  node-node_key     = 'Root'.
  " Key of the node
  CLEAR node-relatkey.      " Special case: A root node has no parent
  CLEAR node-relatship.     " node.
  CLEAR node-n_image.       " Folder-/ Leaf-Symbol in state "closed": use default.
  CLEAR node-exp_image.     " Folder-/ Leaf-Symbol in state "open": use default
*    node-hidden      = ' '.   " The node is visible,
*    node-disabled    = ' '.   " selectable,
  node-expander     = 'X'.
  node-isfolder     = 'X'.   " a folder.
  node-text         = g_loc_setid && ' - TABLE_BOOKMARK'.

**************************************
  APPEND node TO node_table.
**************************************
  SORT g_tot_data BY seqnr.

  LOOP AT g_tot_data .
    CLEAR : node.

    CLEAR node-exp_image.
    node-node_key   = g_tot_data-lineid.
    node-relatkey   = 'Root'.
    node-relatship  = cl_gui_simple_tree=>relat_last_child.
    node-n_image    = '@PO@'.
    node-expander   = ' '.   " The node is marked with a '+', although
    " it has no children. When the user clicks on the
    " + to open the node, the event
    " expand_no_children is fired. The programmer can
    " add the children of the
    " node within the event handler of the
    " expand_no_children event
    " (see method handle_expand_no_children
    " of class lcl_application)

    CONCATENATE g_tot_data-tabname ' -' g_tot_data-ddtext INTO node-text.

    IF g_tot_data-tabname(1) <> 'Z' AND g_tot_data-tabname(1) <> 'Y'.
      node-style = cl_gui_simple_tree=>style_emphasized_positive.
    ENDIF.

    APPEND node TO node_table.
  ENDLOOP.

  PERFORM tree_add_nodes USING node_table.

ENDFORM.                    " BUILD_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Form  SET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_list .
  "" get info - fieldcat.
  PERFORM fill_field_category.

  "" Dynamic fieldcatalog Create
  PERFORM create_dynamic_table.

  "" dynamic make data
  PERFORM make_data.
ENDFORM.                    " SET_LIST
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_field_category .
  DATA  lt_nametab TYPE TABLE OF dntab WITH HEADER LINE.

  CLEAR : gt_fcat[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_bypassing_buffer     = 'X'
      i_structure_name       = g_tabnm
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
      langu          = sy-langu
      tabname        = g_tabnm
    TABLES
      nametab        = lt_nametab
    EXCEPTIONS
      no_texts_found = 1.


  CLEAR gt_styl.

  LOOP AT gt_fcat INTO gs_fcat.

    IF gs_fcat-datatype = 'CLNT' AND g_change = 'X'.
      gs_fcat-no_out = 'X'.
    ENDIF.

    READ TABLE lt_nametab WITH KEY fieldname = gs_fcat-fieldname.
*    IF sy-subrc = 0.
*      gs_fcat-coltext = lt_nametab-fieldtext.
*    ENDIF.
    gs_fcat-coltext = lt_nametab-fieldname.

    IF g_change = 'X' OR g_up = 'X'.
      IF gs_fcat-datatype = 'DATS' AND g_up = 'X'.
        gs_fcat-datatype = 'CHAR'.
        gs_fcat-inttype = 'C'.
        gs_fcat-intlen = 10.
      ENDIF.

      IF gs_fcat-key = 'X'.
        gs_styl-fieldname = gs_fcat-fieldname.
        gs_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
        INSERT gs_styl INTO TABLE gt_styl.
      ENDIF.

*      IF g_excel = 'X'.
*        CLEAR gs_fcat-domname.
*        gs_fcat-checktable = '!'.
*        CLEAR: gs_fcat-ref_table, gs_fcat-ref_field.
*      ENDIF.

      gs_fcat-edit = 'X'.
    ENDIF.

    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.
ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dynamic_table .
  CLEAR : gv_tab_cell.

  IF <fs_all> IS ASSIGNED.
    UNASSIGN <fs_all>.
  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_fcat
      i_style_table             = 'X'
    IMPORTING
      ep_table                  = gv_tab_cell
      e_style_fname             = gv_style_fname
    EXCEPTIONS
      generate_subpool_dir_full = 9.

  ASSIGN gv_tab_cell->* TO <fs_all>.

  CREATE DATA gv_ref LIKE LINE OF <fs_all>.
  ASSIGN gv_ref->* TO <s_all>.
ENDFORM.                    " CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  MAKE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data .
  REFRESH <fs_all>.

  CREATE DATA gv_ref LIKE LINE OF <fs_all>.
  ASSIGN gv_ref->* TO <s_all>.

  LOOP AT <fs_tab> ASSIGNING <s_tab>.

    CLEAR <s_all>.
    MOVE-CORRESPONDING <s_tab> TO <s_all>.

    ASSIGN COMPONENT gv_style_fname OF STRUCTURE <s_all> TO <t_cell>.
    INSERT LINES OF gt_styl INTO TABLE <t_cell>.

    APPEND <s_all> TO <fs_all>.

  ENDLOOP.
ENDFORM.                    " MAKE_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid .
* alv object...
  PERFORM alv_object.

* alv layput...
  PERFORM set_layout USING gs_layout.

* alv display...
  PERFORM alv_display.
ENDFORM.                    " ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_object .
*- Create an Instance of ALV Control
  CREATE OBJECT grid
    EXPORTING
      i_parent      = cl_gui_container=>default_screen
      i_appl_events = 'X'.
ENDFORM.                    " ALV_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM set_layout  USING    ps_layout TYPE lvc_s_layo.

  CLEAR ps_layout.
  ps_layout-sel_mode   = 'D'.
  ps_layout-zebra      = 'X'.
  ps_layout-cwidth_opt = 'X'.
  ps_layout-stylefname = gv_style_fname.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display .
  DATA : ls_variant   LIKE disvariant.

*- variant layout
  ls_variant-report  = sy-repid.

  READ TABLE g_tot_data WITH KEY valfrom = g_tabnm.

  CONCATENATE g_tabnm ' - ' g_tot_data-descript INTO gs_layout-grid_title.

*- alv display on screen
  CALL METHOD grid->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
      i_save          = 'A'
      is_variant      = ls_variant
    CHANGING
      it_outtab       = <fs_all>
      it_fieldcatalog = gt_fcat.

* ->Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
*
  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->handle_user_command FOR grid.
  SET HANDLER event_receiver->handle_toolbar      FOR grid.
*********
* § 4.Call method 'set_toolbar_interactive' to raise event TOOLBAR.
  CALL METHOD grid->set_toolbar_interactive.

*   flush
  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = grid.

  CALL METHOD cl_gui_cfw=>flush.
**********
ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  CHECK g_tabnm IS NOT INITIAL.

  PERFORM assign_dbtab.
  PERFORM read_dbtab.
  PERFORM display_data.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DBTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_dbtab .

  CREATE DATA gv_tab TYPE TABLE OF (g_tabnm).
  ASSIGN gv_tab->* TO <fs_tab>.

ENDFORM.                    " ASSIGN_DBTAB
*&---------------------------------------------------------------------*
*&      Form  READ_DBTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_dbtab .
********************************************************
  CLEAR : <fs_tab>[].

  " Selection with parameters
  CLEAR : gt_where[], gt_dyntab[].

  IF gv_active = 0.
*** 무조건 100건으로 출력
    SELECT * INTO TABLE <fs_tab>
             UP TO 100 ROWS
             FROM (g_tabnm)
             CLIENT SPECIFIED
            WHERE mandt = sy-mandt.
  ELSE.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = gt_ranges
      IMPORTING
        where_clauses = gt_where.

    LOOP AT gt_where INTO gs_where WHERE tablename = g_tabnm.
      APPEND LINES OF gs_where-where_tab TO gt_dyntab.
    ENDLOOP.

*** 무조건 100건으로 출력
    SELECT * INTO TABLE <fs_tab>
             UP TO 100 ROWS
             FROM (g_tabnm)
            WHERE (gt_dyntab).
  ENDIF.

  IF sy-dbcnt = 0.
    MESSAGE s001(00) WITH 'No entry selected!'.
  ENDIF.
********************************************************


ENDFORM.                    " READ_DBTAB
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  CREATE DATA gv_old TYPE TABLE OF (g_tabnm).
  ASSIGN gv_old->* TO <fs_old>.

  <fs_old>[] = <fs_tab>[].

  PERFORM set_list.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_SELECTION
*&---------------------------------------------------------------------*
*       text
*       Check PGM-ID : J_1BEFD_LOG_UI - Log Processing UI
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dynamic_selection .
  DATA : lt_dd03l TYPE TABLE OF dd03l   WITH HEADER LINE,
         lt_dfies TYPE TABLE OF dfies   WITH HEADER LINE.


  DATA: lt_fields  LIKE rsdsfields OCCURS 0 WITH HEADER LINE,
        lt_efields LIKE rsdsfields OCCURS 0 WITH HEADER LINE,
        lt_where   TYPE rsds_twhere.

  DATA : lv_title TYPE sy-title,
         lv_count TYPE i.


  CLEAR : gt_tables[], gt_tables.
  gs_tables-prim_tab = g_tabnm.
  APPEND gs_tables TO gt_tables.

*-- Build proposal for first call
  IF lt_fields IS INITIAL.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = g_tabnm
      TABLES
        dfies_tab = lt_dfies.

    LOOP AT lt_dfies WHERE fieldname <> 'MANDT'.

      IF lv_count < 75.
        lt_fields-tablename = lt_dfies-tabname.
        lt_fields-fieldname = lt_dfies-fieldname.
        lt_fields-sign      = 'I'.
        APPEND lt_fields.
        lv_count = lv_count + 1.             "MAX FIELD를 75로 제한 하고 있음

        IF lt_dfies-keyflag <> 'X'.
          APPEND lt_fields TO lt_efields.
        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      kind                     = 'F'
      expressions              = gt_expr
    IMPORTING
      selection_id             = gv_selid
      expressions              = gt_expr
      number_of_active_fields  = gv_active
    TABLES
      tables_tab               = gt_tables
      fields_tab               = lt_fields
      fields_not_selected      = lt_efields
    EXCEPTIONS
      fields_incomplete        = 1
      fields_no_join           = 2
      field_not_found          = 3
      no_tables                = 4
      table_not_found          = 5
      expression_not_supported = 6
      incorrect_expression     = 7
      illegal_kind             = 8
      area_not_found           = 9
      inconsistent_area        = 10
      kind_f_no_fields_left    = 11
      kind_f_no_fields         = 12
      too_many_fields          = 13
      dup_field                = 14
      field_no_type            = 15
      field_ill_type           = 16
      dup_event_field          = 17
      node_not_in_ldb          = 18
      area_no_field            = 19
      OTHERS                   = 20.

  IF sy-subrc <> 0         AND
   sy-msgid IS NOT INITIAL AND
   sy-msgno IS NOT INITIAL.

    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   Display free selection dialog
  CONCATENATE g_tabnm ' Dynamic Selection' INTO lv_title SEPARATED BY space.

  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
    EXPORTING
      selection_id            = gv_selid
      title                   = lv_title
      status                  = 1
      as_window               = 'X'
      tree_visible            = 'X'
    IMPORTING
      where_clauses           = lt_where
      expressions             = gt_expr
      field_ranges            = gt_ranges
      number_of_active_fields = gv_active
    TABLES
      fields_tab              = lt_fields
    EXCEPTIONS
      internal_error          = 1
      no_action               = 2
      selid_not_found         = 3
      illegal_status          = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0           AND
     sy-msgid IS NOT INITIAL AND
     sy-msgno IS NOT INITIAL.

    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  g_tabnm_old = g_tabnm.

ENDFORM.                    " DYNAMIC_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SAVE_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_alv_data .
  FIELD-SYMBOLS : <fs_wa>.

  DATA : lv_valid(1).

  CALL METHOD grid->check_changed_data
    IMPORTING
      e_valid = lv_valid.

  CHECK lv_valid = 'X'.

  "" ### ##
  REFRESH <fs_tab>.

  CREATE DATA gv_ref LIKE LINE OF <fs_tab>.
  ASSIGN gv_ref->* TO <s_tab>.

  LOOP AT <fs_all> ASSIGNING <s_all>.
    CLEAR <s_tab>.
    MOVE-CORRESPONDING <s_all> TO <s_tab>.
    APPEND <s_tab> TO <fs_tab>.
  ENDLOOP.

  DELETE (g_tabnm) FROM TABLE <fs_old>.

  MODIFY (g_tabnm) FROM TABLE <fs_tab>.

  IF sy-subrc <> 0 AND sy-dbcnt = 0.
    CLEAR sy-subrc.
    MESSAGE s377(00) WITH  g_tabnm.
  ELSE.
    MESSAGE s018(00).
  ENDIF.

  CALL METHOD grid->refresh_table_display.
ENDFORM.                    " SAVE_ALV_DATA
*&---------------------------------------------------------------------*
*&      Form  COMMAND_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FCODE  text
*----------------------------------------------------------------------*
FORM command_function  USING p_fcode p_node_key .

  DATA : l_rc       TYPE c,
         l_text     LIKE spop-textline1.

  g_ok_code = p_fcode.
  CASE g_ok_code.
    WHEN 'T_APPLY'.
      l_text = 'Do you really want to Table Apply?'.
    WHEN 'SAVE'.
      l_text = 'Do you really want to Data Save?'.
    WHEN 'T_DEL'.
      l_text = 'Do you really want to Table Delete?'.
    WHEN 'T_LAYOUT'.
      l_text = 'Do you really want to Table Layout?'.
    WHEN 'T_UPLOAD'.
      l_text = 'Do you really want to Table Excel Upload?'.
  ENDCASE.

  IF l_text IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = l_text
*       textline2      = 'this node and all its subnodes?'
        titel          = 'Confirmation'
        cancel_display = ' '
      IMPORTING
        answer         = l_rc.
  ELSE.
    l_rc = 'J'.
  ENDIF.

  IF l_rc EQ 'J'.
    CASE g_ok_code.
      WHEN 'REFRESH'.
        PERFORM set_tree_refresh.

      WHEN 'SAVE'.

        PERFORM save_alv_data.

      WHEN 'T_APPLY'.

* ** 1. 현재 TREE 의 구조를 읽는다(INT TABLE)
        PERFORM set_tree_line.

* ** 2. 추출된 Tree 구조를 Set 에 update 한다.(g_set_lines Setting)
        PERFORM tree_set_generate.

* ** 3. 다시 조회.
        PERFORM set_tree_refresh.

        CLEAR : g_ok_code.

      WHEN 'T_ADD'.

        DATA: node          LIKE mtreesnode .
        CLEAR : node, node_table, node_table[].

********************************
**** Screen  추가하여 table , table name , table_description 를 Define 함
        CALL SCREEN 101  STARTING AT 10 5 .
********************************
        CHECK  g_101_tab_id IS NOT INITIAL.

        g_count = g_count + 1.

        node-node_key   = 'TEMP' && g_count.
        node-relatkey   = 'Root'.
        node-relatship  = cl_gui_simple_tree=>relat_last_child.
        node-n_image    = '@PP@'.
        node-text       = g_101_tab_id && ' - ' && g_101_t_name.
        node-style      = cl_gui_simple_tree=>style_inactive.
*         node-disabled   = 'X'.

        APPEND node TO node_table.

        PERFORM set_tot_data USING 'A' node-node_key g_101_tab_id g_101_t_name g_101_t_desc.

        PERFORM tree_add_nodes USING node_table .

        CLEAR : g_101_tab_id, g_101_t_name, g_101_t_desc.

      WHEN 'T_CHANGE'.

**  1. 현재 Tree node 를 읽고, 추출된 정보(table id/name/desc) 화면에 보낸다.
**  2. 수정된 정보를 다시 tree 구조에 넣는다. 주)node key 같아야 함
**  3. Apply 로 적용할 수 있도록 구조에 넣어준다.(apply 되면 set 에 적용)

        READ TABLE g_tot_data WITH KEY lineid = p_node_key.

        g_101_tab_id  = g_tot_data-tabname.
        g_101_t_name  = g_tot_data-ddtext.
        g_101_t_desc  = g_tot_data-descript.

*** Screen  추가하여 table , table name , table_description 를 Define 함
        CALL SCREEN 101  STARTING AT 10 5 .

        CHECK g_101_tab_id IS NOT INITIAL.

        READ TABLE g_tot_data WITH KEY lineid = p_node_key.

        g_tot_data-tabname  = g_101_tab_id.
        g_tot_data-ddtext   = g_101_t_name.
        g_tot_data-descript = g_101_t_desc.

        MODIFY g_tot_data INDEX sy-tabix.

        CLEAR : g_101_tab_id, g_101_t_name, g_101_t_desc.

      WHEN 'T_DEL'.
        IF p_node_key <> 'N'.
          CALL METHOD tree->delete_node
            EXPORTING
              node_key = p_node_key.

          PERFORM set_tot_data USING 'D' p_node_key g_101_tab_id g_101_t_name g_101_t_desc.

        ENDIF.

      WHEN 'T_LAYOUT'.

      WHEN 'T_UPLOAD'.

    ENDCASE.

  ENDIF.

ENDFORM.                    " COMMAND_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  TREE_APPLY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tree_set_generate .

  DATA: set_header    LIKE rgsbs .

  CLEAR : set_header.

  set_header-class         = '0000'.
  set_header-table         = 'DD02L'.
  set_header-setnr         = g_loc_setid.
  set_header-type          = 'B'.
  set_header-field         = 'TABNAME'.
  set_header-unique        = 'X'.

  CONCATENATE sy-uname '_TABLE_BOOKMARK' INTO set_header-title.

  CALL FUNCTION 'G_SET_GENERATE'
    EXPORTING
*     CLIENT                         = ' '
      langu                          = sy-langu
      set_header                     = set_header
*     TOLERATE_AMBIGUITY             = ' '
*     NOTHING_CHANGED                = ' '
*     FLAG_USE_RGSBS_USER            = ' '
*     REF_SET                        = ' '
    TABLES
*     FORMULA_LINES                  =
      set_lines_basic                = g_set_lines
*     SET_LINES_DATA                 =
*     SET_LINES_MULTI                =
*     SET_LINES_SINGLE               =
    EXCEPTIONS
      bad_formula                    = 1
      double_field                   = 2
      entry_not_found                = 3
      not_unique                     = 4
      old_set_has_wrong_data_element = 5
      old_set_has_wrong_type         = 6
      set_is_recursive               = 7
      setname_too_long               = 8
      subset_does_not_exist          = 9
      subset_has_wrong_class         = 10
      subset_has_wrong_data_element  = 11
      subset_has_wrong_type          = 12
      table_or_fieldname_missing     = 13
      temporary_in_permanent_set     = 14
      variable_does_not_exist        = 15
      variable_has_wrong_data_elem   = 16
      variable_has_wrong_table       = 17
      variable_has_wrong_type        = 18
      wrong_interval                 = 19
      wrong_setclass                 = 20
      subset_has_wrong_table         = 21
      OTHERS                         = 22.

  IF sy-subrc <> 0.
*      Implement suitable error handling here
  ENDIF.
ENDFORM.                    " TREE_APPLY
*&---------------------------------------------------------------------*
*&      Form  TREE_ADD_NODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*----------------------------------------------------------------------*
FORM tree_add_nodes  USING    p_node_table.

  CALL METHOD tree->add_nodes
    EXPORTING
      table_structure_name           = 'MTREESNODE'
      node_table                     = p_node_table
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5.

  IF sy-subrc <> 0.
    MESSAGE a000.
  ENDIF.

*** 첫 화면 tree expand 처리.
  CALL METHOD tree->expand_node
    EXPORTING
      node_key = 'Root'.

ENDFORM.                    " TREE_ADD_NODES
*&---------------------------------------------------------------------*
*&      Form  TABNM_CHOOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_NODE_KEY  text
*----------------------------------------------------------------------*
FORM tabnm_choose  USING    p_node_key.

  CHECK p_node_key(4) <> 'TEMP'.

  READ TABLE g_tot_data WITH KEY lineid = p_node_key.

  g_tabnm   =  g_tot_data-valfrom.

ENDFORM.                    " TABNM_CHOOSE
*&---------------------------------------------------------------------*
*&      Form  SET_TREE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_tree_line .
  CLEAR g_set_lines[].

  LOOP AT g_tot_data.
    CLEAR g_set_lines.

    g_set_lines-from  = g_tot_data-valfrom.
    g_set_lines-to    = g_tot_data-valfrom.
    g_set_lines-title = g_tot_data-descript.

    APPEND g_set_lines.
  ENDLOOP.

ENDFORM.                    " SET_TREE_LINE
*&---------------------------------------------------------------------*
*&      Form  TOT_DATA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1230   text
*      -->P_NODE_NODE_KEY  text
*      -->P_G_101_TAB_ID  text
*      -->P_G_101_T_NAME  text
*      -->P_G_101_T_DESC  text
*----------------------------------------------------------------------*
FORM set_tot_data  USING    p_proc_gubun
                            p_node_node_key
                            p_tab_id
                            p_t_name
                            p_t_desc.

  CASE p_proc_gubun.
    WHEN 'A'.
      CLEAR g_tot_data.
      g_tot_data-setname    = g_loc_setid.
      g_tot_data-lineid     = p_node_node_key.
      g_tot_data-valfrom    = p_tab_id.
      g_tot_data-descript   = p_t_desc.
      g_tot_data-tabname    = p_tab_id.
      g_tot_data-ddtext     = p_t_name.
      g_tot_data-seqnr      = 0.

      APPEND g_tot_data.

    WHEN 'D'.

      DELETE  g_tot_data WHERE setname = g_loc_setid
                           AND lineid  = p_node_node_key.

  ENDCASE.


ENDFORM.                    " TOT_DATA_SET
*&---------------------------------------------------------------------*
*&      Form  SET_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_tree_refresh .
  CLEAR : node_table, node_table[], g_tot_data[].

** Tree initial.
  CALL METHOD tree->delete_all_nodes
    EXCEPTIONS
      failed            = 1
      cntl_system_error = 2.

** Tree node create
  PERFORM build_node_table USING node_table.

ENDFORM.                    " SET_REFRESH
