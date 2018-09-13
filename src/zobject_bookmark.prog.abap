*&---------------------------------------------------------------------*
*& Program ID  : ZOBJECT_BOOKMARK
*& Owner       : SAP FI LAB ( WWW.FI-LAB.COM )
*&---------------------------------------------------------------------*
*& Description : Table 을 즐겨찾기 해 놓고, 조회/수정할 수 있는 기능
*& Create date : 2018.09.01 ( Korea seoul )
*&---------------------------------------------------------------------*

INCLUDE zobject_bookmark_top                    .    " global Data
INCLUDE zobject_bookmark_cls                    .    " CLASS
INCLUDE zobject_bookmark_o01                    .    " PBO-Modules
INCLUDE zobject_bookmark_i01                    .    " PAI-Modules
INCLUDE zobject_bookmark_f01                    .    " FORM-Routines

*&---------------------------------------------------------------------*

START-OF-SELECTION.

CREATE OBJECT g_application.

CALL SCREEN 100.
