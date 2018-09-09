*&---------------------------------------------------------------------*
*& Include ZOBJECT_BOOKMARK_TOP
*& Module Pool      ZOBJECT_BOOKMARK
*&
*&---------------------------------------------------------------------*
PROGRAM zobject_bookmark MESSAGE-ID 00.

*************************************************************
**** MAIN DEFINE
*************************************************************
TABLES: dd02l,
        indx,
        sscrfields.

INCLUDE <icon>.

CLASS lcl_application     DEFINITION DEFERRED.
CLASS lcl_event_receiver  DEFINITION DEFERRED.

TYPES: node_table_type LIKE STANDARD TABLE OF mtreesnode
           WITH DEFAULT KEY.

TYPE-POOLS cndp.
**************************************************
DATA : g_application  TYPE REF TO lcl_application.

* Docking & Splitter & Tree Control Container
DATA : docking        TYPE REF TO cl_gui_docking_container,
       tree           TYPE REF TO cl_gui_simple_tree,
       grid           TYPE REF TO cl_gui_alv_grid.
*
DATA : g_ok_code      TYPE sy-ucomm.

DATA : init,
       ok_code        TYPE sy-ucomm,
       repid          TYPE sy-repid,
       dynnr          TYPE sy-dynnr,
       g_toolbar      TYPE REF TO cl_gui_toolbar,
       event_receiver TYPE REF TO lcl_event_receiver.

DATA : g_tabnm        TYPE tabname,
       g_tabnm_old    TYPE tabname.

DATA : g_loc_setid    LIKE rgsbs-setnr,
       g_set_lines    LIKE rgsbv    OCCURS 0 WITH HEADER LINE ,
       g_set_values   LIKE rgsb4    OCCURS 0 WITH HEADER LINE .

*** Base tot data(Set & Tree & Grid)
DATA: BEGIN OF g_tot_data OCCURS 0,
        setname	      TYPE setnamenew,   "Set id
        lineid        TYPE setline,      "Set line id => node key
        valfrom	      TYPE setvalmin,    "Set 형태 Table id
        descript      TYPE setlintext,   "Table description
        tabname	      TYPE tabname,      "table 형태 Table id
        ddtext        TYPE as4text,      "table name
        seqnr         TYPE setlnseqnr,   "Set seq => Tree seq
      END OF g_tot_data.


* Fields on Dynpro 100
DATA : g_event(30),
       g_node_key     TYPE tv_nodekey.

DATA : gv_tab_cell    TYPE REF TO data,
       gv_ref         TYPE REF TO data.

* " Grid 구성
DATA : gt_fcat        TYPE lvc_t_fcat,
       gs_fcat        TYPE lvc_s_fcat,
       gs_layout      TYPE lvc_s_layo,
       gs_variant     TYPE disvariant,
       gt_sort        TYPE lvc_t_sort,
       gs_sort        TYPE lvc_s_sort,
       gt_exclude     TYPE ui_functions,
       gs_toolbar     TYPE stb_button.

DATA:  gt_row         TYPE lvc_t_row,
       gs_row         TYPE lvc_s_row,
       gt_styl        TYPE lvc_t_styl,
       gs_styl        TYPE lvc_s_styl,
       gv_style_fname TYPE lvc_fname.

DATA : gv_tab         TYPE REF TO data,
       gv_all         TYPE REF TO data,
       gv_old         TYPE REF TO data.

DATA : gv_selid       TYPE rsdynsel-selid,
       gv_active      TYPE i,

       gt_tables      TYPE TABLE OF rsdstabs,
       gs_tables      TYPE rsdstabs,

       gt_fields      TYPE TABLE OF rsdsfields,
       gt_expr        TYPE rsds_texpr,
       gt_ranges      TYPE rsds_trange,

       gt_where       TYPE rsds_twhere,
       gs_where       TYPE rsds_where.

DATA : gt_dyntab      TYPE TABLE OF string.

DATA : g_change       TYPE char1,
       g_up           TYPE char1,
       g_count        TYPE i.

**** SCREEN 101
DATA : g_101_tab_id   TYPE dd02l-tabname,      " TABLE ID
       g_101_t_name   TYPE dd02t-ddtext,       " TABLE NAME
       g_101_t_desc   TYPE setlinet-descript.  " 개인 설명
**************************************************

FIELD-SYMBOLS : <fs_tab> TYPE table,
                <fs_all> TYPE table,
                <fs_old> TYPE table.

FIELD-SYMBOLS : <s_tab>,
                <s_all>,
                <t_cell>  TYPE ANY TABLE.
