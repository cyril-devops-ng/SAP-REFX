*&---------------------------------------------------------------------*
*& Include          ZREFX_DUP_CHECK_CORR_TOP
*&---------------------------------------------------------------------*
**********************************************************************
*10: Type Declaration
**********************************************************************
TYPES: BEGIN OF t_output,
         contract_no  TYPE recnnumber,
         comp_code    TYPE bukrs,
         contr_type   TYPE recncontracttype,
         contract_txt TYPE recntxt,
         cont_st_dat  TYPE recncnbeg,
         frst_end_dat TYPE recncnend1st,
         obj_number   TYPE vibdobjass-objnrtrg,
         valid_from   TYPE rebdrelvalidfrom,
         valid_to     TYPE rebdrelvalidto,
         zcheckin     TYPE zre_checkin,
         zcheckout    TYPE zre_checkout,
       END OF t_output.
TYPES: t_st_output     TYPE STANDARD TABLE OF t_output,
       t_sorted_output TYPE SORTED TABLE OF   t_output
                       WITH NON-UNIQUE KEY contract_no comp_code,
       tr_comp         TYPE RANGE OF bukrs.
**********************************************************************
*20: Data Declaration
**********************************************************************
DATA: go_alv     TYPE REF TO cl_salv_table,
      go_columns TYPE REF TO cl_salv_columns_table,
      go_column  TYPE REF TO cl_salv_column_table,
      go_layout  TYPE REF TO cl_salv_layout,
      go_cont    TYPE REF TO cl_gui_container,
      go_select  TYPE REF TO cl_salv_selections.
DATA: gt_output  TYPE t_st_output.
DATA: gs_output  TYPE t_output.
FIELD-SYMBOLS: <fs_any_msg> type bapiret2.
**********************************************************************
*30: Class Definition
**********************************************************************
CLASS lcl_dup_check DEFINITION DEFERRED.

CLASS lcl_dup_check DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_contract_no    TYPE RANGE OF recnnumber,
           ty_comp_code      TYPE RANGE OF bukrs,
           ty_contr_type     TYPE RANGE OF recncontracttype,
           ty_contract_start TYPE RANGE OF recncnbeg,
           ty_first_end_dat  TYPE RANGE OF recncnend1st.
    CLASS-METHODS create
      IMPORTING
        i_contract_no    TYPE ty_contract_no
        i_comp_code      TYPE ty_comp_code
        i_contr_type     TYPE ty_contr_type
        i_contract_start TYPE ty_contract_start
        i_first_end_dat  TYPE ty_first_end_dat
      RETURNING
        VALUE(r_result)  TYPE REF TO lcl_dup_check.
    METHODS: constructor IMPORTING !if_contract_no   TYPE recnnumber OPTIONAL
                                   !if_contract_type TYPE recncontracttype OPTIONAL
                         RAISING   zcx_refx_exception,
      create_grid   CHANGING ct_tab TYPE ANY  TABLE RAISING zcx_refx_exception,
      show_grid     RAISING zcx_refx_exception,
      retrieve_set  RAISING zcx_refx_exception,
      retrieve_specific_set IMPORTING if_contract_no   TYPE recnnumber
                                      if_contract_type TYPE recncontracttype
                                      if_check_in      TYPE flag
                                      if_check_out     TYPE flag
                            RAISING   zcx_refx_exception,

      on_added_function
        FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
        IMPORTING e_salv_function,
      on_link_click
        FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
        IMPORTING row column,
      refresh_grid,
      check_in IMPORTING it_selection TYPE t_st_output
               RAISING   zcx_refx_exception,
      check_out IMPORTING it_selection TYPE t_st_output
                RAISING   zcx_refx_exception.
  PRIVATE SECTION.
    DATA: lr_contract_no    TYPE RANGE OF recnnumber,
          lr_comp_code      TYPE RANGE OF bukrs,
          lr_contr_type     TYPE RANGE OF recncontracttype,
          lr_contract_start TYPE RANGE OF recncnbeg,
          lr_first_end_dat  TYPE RANGE OF recncnend1st,
          lf_contract_no    TYPE recnnumber,
          lf_contract_type  TYPE recncontracttype.
    CONSTANTS: gc_recn     TYPE sy-tcode VALUE 'RECN',
               gc_refresh  TYPE ui_func  VALUE 'REFRESH',
               gc_checkin  TYPE ui_func  VALUE 'CHECK-IN',
               gc_checkout TYPE ui_func  VALUE 'CHECK-OUT'.

ENDCLASS.
**********************************************************************
*40: Application
**********************************************************************
DATA: go_dup_check TYPE REF TO lcl_dup_check.
**********************************************************************
*50: Macros
**********************************************************************
DEFINE _thiscolumn.
  go_column ?= go_columns->get_column( <fs_component>-name ).
END-OF-DEFINITION.
DEFINE add_func.
  go_alv->get_functions(  )->add_function(
      tooltip     = &1
      name        = &2
      icon        = &3
      text        = &4
      position    = 1
  ).
END-OF-DEFINITION.
DEFINE deactivate_screen.
  screen-input     = '0'.
  screen-active    = '0'.
  screen-invisible = '1'.
END-OF-DEFINITION.
DEFINE activate_screen.
  screen-input     = '1'.
  screen-active    = '1'.
  screen-invisible = '0'.
END-OF-DEFINITION.
DEFINE configure_check_screen.
  LOOP AT SCREEN.
      IF xsdbool( screen-group1 EQ `DC` )
              EQ abap_true.
         deactivate_screen.
      ENDIF.
      IF xsdbool( screen-group1 EQ 'CHK' )
              EQ abap_true.
         activate_screen.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
END-OF-DEFINITION.
DEFINE configure_dupl_screen.
  LOOP AT SCREEN.
      IF xsdbool( screen-group1 EQ `DC` )
              EQ abap_true.
        activate_screen.
      ENDIF.
      IF xsdbool( screen-group1 EQ 'CHK' )
              EQ abap_true.
        deactivate_screen.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
END-OF-DEFINITION.
DEFINE convert_message_to_success.
  LOOP AT lt_msg ASSIGNING <fs_any_msg>.
    IF xsdbool( <fs_any_msg>-id EQ &1 AND (
                <fs_any_msg>-number EQ &2
               ) ) EQ abap_true.
        <fs_any_msg>-type = 'S'.
    ENDIF.
  ENDLOOP.
END-OF-DEFINITION.
