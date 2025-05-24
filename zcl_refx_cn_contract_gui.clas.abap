CLASS zcl_refx_cn_contract_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: gc_create_mode TYPE string   VALUE 'CREATE',
               gc_change_mode TYPE string   VALUE 'CHANGE'.
    TYPES BEGIN OF t_cont_head_gui.
    INCLUDE TYPE zcl_refx_cn_contract_fs=>t_cont_head.
    TYPES: style TYPE lvc_t_styl.
    TYPES END OF t_cont_head_gui.
    TYPES BEGIN OF t_cont_cond_gui.
    INCLUDE TYPE zcl_refx_cn_contract_fs=>t_cont_cond.
    TYPES: style TYPE lvc_t_styl.
    TYPES: END OF t_cont_cond_gui.

    TYPES: tt_cont_head_gui TYPE TABLE OF t_cont_head_gui WITH DEFAULT KEY,
           tt_cont_cond_gui TYPE TABLE OF t_cont_cond_gui WITH DEFAULT KEY.
    TYPES: ty_tr_id         TYPE RANGE OF t_cont_cond_gui-id.
    TYPES: tr_functions     TYPE RANGE OF ui_func.
    CLASS-METHODS:
      display_bapi_log_gui
        IMPORTING it_bapi_ret TYPE bapiret2_t,
      create_bapi_ret_from_sy_msg
        IMPORTING if_type       TYPE sy-msgty OPTIONAL
                  if_id         TYPE sy-msgid OPTIONAL
                  if_num        TYPE sy-msgno OPTIONAL
                  if_message    TYPE bapi_msg OPTIONAL
                  if_msgv1      TYPE sy-msgv1 OPTIONAL
                  if_msgv2      TYPE sy-msgv2 OPTIONAL
                  if_msgv3      TYPE sy-msgv3 OPTIONAL
                  if_msgv4      TYPE sy-msgv4 OPTIONAL
        RETURNING VALUE(rt_msg) TYPE bapiret2_t
        RAISING
                  zcx_refx_exception,
      display_layout_selection RETURNING VALUE(rt_layout_sheet) TYPE zcl_refx_cn_contract_fs=>tt_layout_sheet,
      create_bapi_ret_from_exception IMPORTING io_exception  TYPE REF TO cx_root
                                     RETURNING VALUE(rt_msg) TYPE bapiret2_t.
    DATA: gt_parent_data TYPE tt_cont_head_gui.
    CLASS-DATA:
      blk_ttl TYPE string VALUE 'RE-FX Contract Booking',
      title1  TYPE string VALUE 'RE-FX Contract Booking (File:)',
      title2  TYPE string VALUE 'RE-FX Contract Header Data',
      title3  TYPE string VALUE 'RE-FX Contract Condition Data',
      title4  TYPE string VALUE 'Create Mode',
      title5  TYPE string VALUE 'Change Mode',
      screen  TYPE c LENGTH 4 VALUE '0100'.
    METHODS: constructor IMPORTING if_mode TYPE string it_contr_head TYPE tt_cont_head_gui it_contr_cond TYPE tt_cont_cond_gui
    RAISING zcx_refx_exception,
      create_grid CHANGING ct_tab TYPE STANDARD TABLE
                  RAISING  zcx_refx_exception,
      show_grid RAISING zcx_refx_exception,
      create_refx_cn_contract IMPORTING it_selected_rows TYPE tt_cont_head_gui
                              RAISING   zcx_refx_exception,
      change_refx_cn_contract IMPORTING it_selected_rows TYPE tt_cont_head_gui
                              RAISING   zcx_refx_exception,
      simulate_refx_cn_contract

                                IMPORTING if_simu TYPE flag
                                          it_selected_rows TYPE tt_cont_head_gui
                                RAISING   zcx_refx_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: build_parent_fieldcatalog,
      build_child_fieldcatalog,
      build_log_fieldcatalog,
      get_tool_bar_excluded
            RETURNING VALUE(rt_excluded) TYPE ui_functions,
      handle_data_changed_finished
            FOR EVENT data_changed_finished OF cl_gui_alv_grid
            IMPORTING e_modified et_good_cells,
      handle_data_changed
            FOR EVENT data_changed OF cl_gui_alv_grid
            IMPORTING er_data_changed,
      handle_f4
            FOR EVENT onf4 OF cl_gui_alv_grid
            IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display,
      handle_hotspot_click
            FOR EVENT hotspot_click OF cl_gui_alv_grid
            IMPORTING e_column_id e_row_id es_row_no,
      handle_toolbar
            FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_interactive e_object,
      handle_user_command
            FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,
      handle_double_click
            FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row e_column es_row_no sender,
      add_separator CHANGING ct_toolbar TYPE ttb_button,
      add_button
            IMPORTING i_function TYPE ui_func i_icon TYPE any i_quickinfo TYPE iconquick i_text TYPE text40 OPTIONAL
            CHANGING  ct_toolbar TYPE ttb_button,
      set_key_fields_changeable CHANGING cs_data TYPE ANY TABLE,
      configure_grid,
      display_grid IMPORTING is_layout       TYPE lvc_s_layo

                   CHANGING co_alv_grid     TYPE REF TO cl_gui_alv_grid
                            ct_tab          TYPE ANY TABLE
                            ct_fieldcatalog TYPE lvc_t_fcat
                   RAISING
                     zcx_refx_exception,
      change_field_catalog RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat,
      display_child_tab IMPORTING it_selected_rows TYPE tt_cont_head_gui
      RAISING zcx_refx_exception,
      display_log_tab IMPORTING it_log          TYPE zcl_refx_cn_contract_fs=>tt_cont_log
      RAISING zcx_refx_exception,
      create_bapi_log IMPORTING it_bapi_return  TYPE bapiret2_t OPTIONAL
                                if_type         TYPE sy-msgty OPTIONAL
                                if_id           TYPE sy-msgid OPTIONAL
                                if_num          TYPE sy-msgno OPTIONAL
                                if_message      TYPE bapi_msg OPTIONAL
                                if_msgv1        TYPE sy-msgv1 OPTIONAL
                                if_msgv2        TYPE sy-msgv2 OPTIONAL
                                if_msgv3        TYPE sy-msgv3 OPTIONAL
                                if_msgv4        TYPE sy-msgv4 OPTIONAL
                      RAISING zcx_refx_exception,
      contract_check IMPORTING it_selected_rows TYPE tt_cont_head_gui
      RAISING zcx_refx_exception,
      refresh_alv,
      default_company.
    DATA: gc_100             TYPE REF TO cl_gui_custom_container,
          go_splitter        TYPE REF TO cl_gui_splitter_container,
          go_parent_grid     TYPE REF TO cl_gui_container,
          go_child_grid      TYPE REF TO cl_gui_container,
          go_gui_p_alv       TYPE REF TO cl_gui_alv_grid,
          go_gui_c_alv       TYPE REF TO cl_gui_alv_grid,
          go_gui_l_alv       TYPE REF TO cl_gui_alv_grid,
          go_cols            TYPE REF TO cl_salv_columns_table,
          go_col             TYPE REF TO cl_salv_column_table,
          gt_parent_fieldcat TYPE lvc_t_fcat,
          gt_child_fieldcat  TYPE lvc_t_fcat,
          gt_log_fieldcat    TYPE lvc_t_fcat,
          gs_p_layout        TYPE lvc_s_layo,
          gs_c_layout        TYPE lvc_s_layo,
          gs_l_layout        TYPE lvc_s_layo,
          gs_colpos          TYPE lvc_colpos,
          gs_variant         TYPE disvariant,
          gt_toolbar_ex      TYPE ui_functions,
          gf_mode            TYPE string.

    DATA: gt_child_data TYPE tt_cont_cond_gui,
          lt_child_data TYPE tt_cont_cond_gui,
          gt_log_data   TYPE zcl_refx_cn_contract_fs=>tt_cont_log.

    CONSTANTS: gc_container_name(10)  TYPE c        VALUE 'GC_100',
               gc_contr_head_title    TYPE string   VALUE 'RE-FX Contract Header',
               gc_contr_cond_title    TYPE string   VALUE 'RE-FX Contract Conditions',
               gc_contr_log_title     TYPE string   VALUE 'RE-FX Contract Protocol',
               gc_style_fname(10)     TYPE c        VALUE 'STYLE',
               gc_refresh             TYPE ui_func  VALUE 'REFRESH',
               gc_disp_cond           TYPE ui_func  VALUE 'DISP_COND',
               gc_create_contr        TYPE ui_func  VALUE 'CREATE_CONTR',
               gc_change_contr        TYPE ui_func  VALUE 'CHANGE_CONTR',
               gc_simulate_contr      TYPE ui_func  VALUE 'SIMU_CONTR',
               gc_refresh_text        TYPE string   VALUE 'Refresh',
               gc_disp_cond_text      TYPE string   VALUE 'Display RE-FX Conditions',
               gc_create_contr_text   TYPE string   VALUE 'Create RE-FX Contract',
               gc_change_contr_text   TYPE string   VALUE 'Change RE-FX Contract',
               gc_simulate_contr_text TYPE string   VALUE 'Simulate RE-FX &1 Contract',
               gc_recn                TYPE sy-tcode VALUE 'RECN',
               gc_log                 type ui_func  value 'LOG',
               gc_log_text            type string   value 'Display Log'.
ENDCLASS.



CLASS ZCL_REFX_CN_CONTRACT_GUI IMPLEMENTATION.


  METHOD constructor.
    me->gt_parent_data = it_contr_head.
    me->gt_child_data  = it_contr_cond.
    me->gf_mode        = if_mode.
    default_company(  ).
  ENDMETHOD.


  METHOD create_grid.
**********************************************************************
*10: Create: Container instance
**********************************************************************
    TRY.
        IF gc_100 IS INITIAL.
          gc_100 = NEW #( container_name =  gc_container_name  ).
        ENDIF.
**********************************************************************
*20: Create: Splitter instance
**********************************************************************
        IF go_splitter IS INITIAL.
          go_splitter = NEW #( parent   = cl_gui_container=>default_screen
                               rows     = 2
                               columns  = 1 ).
        ENDIF.
**********************************************************************
*30: Create: Split Containers
**********************************************************************
        IF go_splitter IS BOUND.
          go_parent_grid = go_splitter->get_container(
                              row       = 1
                              column    = 1
                           ).
          go_child_grid  = go_splitter->get_container(
                              row       = 2
                              column    = 1
                           ).
          go_splitter->set_row_height( id = 1 height = 30 ).
**********************************************************************
*40: Set-Up: GUI ALV Grid (Parent Control)
**********************************************************************
          IF go_gui_p_alv IS INITIAL.
            go_gui_p_alv = NEW #(
                    i_parent = go_parent_grid
                 ).
            IF go_gui_p_alv IS BOUND.
              me->build_parent_fieldcatalog(  ).
              me->configure_grid(  ).
              me->gs_p_layout = VALUE #(
                sel_mode    = 'D'
                no_rowins   = abap_true
                no_rowmove  = abap_true
                no_toolbar  = abap_false
                cwidth_opt  = abap_false
                zebra       = abap_true
                stylefname  = gc_style_fname
                grid_title  = gc_contr_head_title
               ).
              go_gui_p_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
              go_gui_p_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
            ELSE.
**********************************************************************
*50: Raise Exceptions
**********************************************************************
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_alv_error.
            ENDIF.
          ENDIF.
**********************************************************************
*60: Set-Up: GUI ALV Grid (Child Control)
**********************************************************************
          IF go_gui_c_alv IS INITIAL.
            go_gui_c_alv = NEW #(
                  i_parent = go_child_grid
               ).
            IF go_gui_c_alv IS BOUND.
              me->build_child_fieldcatalog(  ).
              me->gs_c_layout = VALUE #(
                  sel_mode    = 'A'
                  no_rowins   = abap_true
                  no_rowmove  = abap_true
                  no_toolbar  = abap_false
                  cwidth_opt  = abap_false
                  zebra       = abap_true
                  stylefname  = gc_style_fname
                  grid_title  = gc_contr_cond_title
                 ).
**********************************************************************
*70: Set-Up GUI ALV Grid (Log Control)
**********************************************************************
              me->build_log_fieldcatalog(  ).
              me->gs_l_layout = VALUE #(
                 sel_mode    = 'A'
                 no_rowins   = abap_true
                 no_rowmove  = abap_true
                 no_toolbar  = abap_false
                 cwidth_opt  = abap_false
                 zebra       = abap_true
                 stylefname  = gc_style_fname
                 grid_title  = gc_contr_log_title
                ).
            ELSE.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_alv_error.
            ENDIF.
          ENDIF.
        ENDIF.
**********************************************************************
*80: Catch Exceptions
**********************************************************************
      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_salv_msg.
**********************************************************************
*90: Catch Other Exceptions
**********************************************************************
      CATCH cx_root INTO DATA(lo_root_msg).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_root_msg.
    ENDTRY.
  ENDMETHOD.


  METHOD show_grid.
**********************************************************************
*10: Display Grid
**********************************************************************
    go_gui_p_alv->set_table_for_first_display(
      EXPORTING
        is_layout                     = me->gs_p_layout
        it_toolbar_excluding          = me->gt_toolbar_ex
      CHANGING
        it_outtab                     = me->gt_parent_data
        it_fieldcatalog               = me->gt_parent_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).
**********************************************************************
*20: Set Event Handler for ALV
**********************************************************************
    SET HANDLER handle_toolbar       FOR go_gui_p_alv.
    SET HANDLER handle_user_command  FOR go_gui_p_alv.
    SET HANDLER handle_hotspot_click FOR go_gui_p_alv.
    SET HANDLER handle_data_changed_finished FOR go_gui_p_alv.
    SET HANDLER handle_f4 FOR go_gui_p_alv.
    SET HANDLER handle_data_changed FOR go_gui_p_alv.
    SET HANDLER handle_double_click FOR go_gui_p_alv.
**********************************************************************
*30: Set Toolbar settings for ALV
**********************************************************************
    go_gui_p_alv->set_toolbar_interactive(  ).
    go_gui_p_alv->set_ready_for_input( i_ready_for_input = 1 ).
**********************************************************************
*40: Handle change field catalog
**********************************************************************
    me->change_field_catalog(  ).
**********************************************************************
*50: Raise Exception
**********************************************************************
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_alv_error.
    ENDIF.
  ENDMETHOD.


  METHOD build_child_fieldcatalog.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
**********************************************************************
*20: Get Structure
**********************************************************************
    DATA lt_child_data TYPE zcl_refx_cn_contract_fs=>tt_cont_cond.
    lt_child_data = CORRESPONDING #( me->gt_child_data ).
    lo_str ?= cl_abap_structdescr=>describe_by_data(  p_data = lt_child_data[ 1 ] ).
**********************************************************************
*30: Create Field Catalog
**********************************************************************
    INSERT LINES OF VALUE
    lvc_t_fcat( FOR <fs> IN lo_str->components
         (
            row_pos     = 1
            col_pos     = sy-tabix
            fieldname   = <fs>-name
            no_out      = space
            coltext     = COND #(
                          WHEN <fs>-name = 'ID'                         THEN 'ID'
                          WHEN <fs>-name = 'CONDITION_TYPE'             THEN 'Contract Type'
                          WHEN <fs>-name = 'CALCULATION_OBJECT_TYPE'    THEN 'Calculation Object Type'
                          WHEN <fs>-name = 'CALCULATION_OBJECT_ID'      THEN 'Calculation Object ID'
                          WHEN <fs>-name = 'UNIT_PRICE'                 THEN 'Unit Price'
                          WHEN <fs>-name = 'CALC_RULE'                  THEN 'Calculation Rule'
                          WHEN <fs>-name = 'CALC_RULE_PARAMETER1'       THEN 'Calc. Parameter 1'
                          WHEN <fs>-name = 'CURRENCY1'                  THEN 'Currency 1'
                          WHEN <fs>-name = 'CALC_RULE_PARAMETER2'       THEN 'Calc. Parameter 2'
                          WHEN <fs>-name = 'CURRENCY2'                  THEN 'Currency 2'
                          WHEN <fs>-name = 'EXTERNAL_PURPOSE'           THEN 'External Purpose'
                          WHEN <fs>-name = 'VALID_FROM'                 THEN 'Valid From'
                          WHEN <fs>-name = 'VALID_TO'                   THEN 'Valid To'
                          WHEN <fs>-name = 'DIST_RULE'                  THEN 'Distr. Rule'
                          WHEN <fs>-name = 'ROLE_TYPE'                  THEN 'Role Type'
                          WHEN <fs>-name = 'PARTNER'                    THEN 'Business Partner'
                          WHEN <fs>-name = 'TAX_GROUP'                  THEN 'Tax Group'
                          WHEN <fs>-name = 'GUEST'                      THEN 'Guest'
                        )
          )
     )
    INTO TABLE me->gt_child_fieldcat.
  ENDMETHOD.


  METHOD build_log_fieldcatalog.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
**********************************************************************
*20: Get Structure
**********************************************************************
    lo_str ?= cl_abap_structdescr=>describe_by_data(  p_data = VALUE zcl_refx_cn_contract_fs=>t_cont_log(  ) ).
**********************************************************************
*30: Create Field Catalog
**********************************************************************
    INSERT LINES OF VALUE
    lvc_t_fcat( FOR <fs> IN lo_str->components
         (
            row_pos = 1
            col_pos = sy-tabix
            fieldname = <fs>-name
            no_out = space
            coltext     = COND #(
                          WHEN <fs>-name = 'ICON'       THEN 'Status'
                          WHEN <fs>-name = 'TYPE'       THEN 'Message Type'
                          WHEN <fs>-name = 'ID'         THEN 'Message ID'
                          WHEN <fs>-name = 'NUM'        THEN 'Message Number'
                          WHEN <fs>-name = 'MESSAGE'    THEN 'Message Description'
                          )
           outputlen = cond #( when <fs>-name = 'MESSAGE' THEN 80 )
          )
     )
    INTO TABLE me->gt_log_fieldcat.
  ENDMETHOD.


  METHOD build_parent_fieldcatalog.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
**********************************************************************
*20: Get Structure
**********************************************************************
    DATA lt_parent_data TYPE zcl_refx_cn_contract_fs=>tt_cont_head.
    lt_parent_data = CORRESPONDING #( me->gt_parent_data ).
    lo_str ?= cl_abap_structdescr=>describe_by_data( p_data = lt_parent_data[ 1 ]   ).
**********************************************************************
*30: Create Catalog
**********************************************************************
    INSERT LINES OF VALUE
    lvc_t_fcat( FOR <fs> IN lo_str->components
     (
        row_pos = 1
        col_pos = sy-tabix
        fieldname = <fs>-name
        coltext = COND #( WHEN <fs>-name = 'ID'                     THEN 'ID'
                          WHEN <fs>-name = 'OLD_CONTRACT_NUMBER'    THEN 'Old Contract No.'
                          WHEN <fs>-name = 'COMP_CODE'              THEN 'Company Code'
                          WHEN <fs>-name = 'CONTRACT_TYPE'          THEN 'Contract Type'
                          WHEN <fs>-name = 'CONTRACT'               THEN 'Contract Number'
                          WHEN <fs>-name = 'CONTRACT_TEXT'          THEN 'Contract Description'
                          WHEN <fs>-name = 'CONTRACT_START_DATE'    THEN 'Contract Start Date'
                          WHEN <fs>-name = 'FIRST_END_DATE'         THEN 'First End Date'
                          WHEN <fs>-name = 'POSSESSION_FROM'        THEN 'Possession From'
                          WHEN <fs>-name = 'POSSESSION_TO'          THEN 'Possession To'
                          WHEN <fs>-name = 'FREQUENCY'              THEN 'Frequency'
                          WHEN <fs>-name = 'FREQUENCY_UNIT'         THEN 'Frq. Unit'
                          WHEN <fs>-name = 'CONDITION_AMOUNT_REF'   THEN 'Condition Amount'
                          WHEN <fs>-name = 'PAYMENT_FORM'           THEN 'Payment Form'
                          WHEN <fs>-name = 'PYMT_METH'              THEN 'Payment Method'
                          WHEN <fs>-name = 'PMNTTRMS'               THEN 'Payment Terms'
                          WHEN <fs>-name = 'HOUSEBANKID'            THEN 'House Bank ID'
                          WHEN <fs>-name = 'ZZRE_RATE'              THEN 'Rate'
                          WHEN <fs>-name = 'CURRENCY_CONTRACT'      THEN 'Currency'
                        )
        hotspot = COND #( WHEN xsdbool(  <fs>-name = 'CONTRACT' OR <fs>-name = 'COMP_CODE' or <fs>-name = 'CONTRACT_TYPE')
                          EQ abap_true THEN abap_true )
        no_out = space
     )
     )
    INTO TABLE me->gt_parent_fieldcat.
  ENDMETHOD.


  METHOD get_tool_bar_excluded.
**********************************************************************
*10: Exclude toolbar func
**********************************************************************
    INSERT LINES OF VALUE
    ui_functions(
       ( '&ABC' )
       ( '&ABC' )
       ( '&AQW' )
       (  '&AUF')
       ( '&AVERAGE')
       ( '&BEB2' )
       ( '&BEB3' )
       ( '&BEB9' )
       ( '&BEBN' )
       ( '&CDF' )
       ( '&CFI' )
       ( '&COL_INV' )
       ( '&CRDESIG' )
       ( '&CRTEMPL' )
       ( '&DELETE_FILTER' )
       ( '&DETAIL' )
       ( '&GRAPH' )
       ( '&HTML' )
       ( '&INFO' )
       ( '&LOAD' )
       ( '&LOCAL&COPY')
       ( '&LOCAL&PASTE' )
       ( '&LOCAL&PASTE_NEW_ROW' )
       ( '&MAINTAIN' )
       ( '&MAXIMUM'  )
       ( '&MINIMUM' )
       ( '&ML' )
       ( '&OPTIMIZE' )
       ( '&PC' )
       ( '&PRINT'  )
       ( '&PRINT_BACK_PREVIEW'   )
       ( '&SEND'  )
       ( '&SUBTOT' )
       ( '&SUMC' )
       ( '&VCRYSTAL' )
       ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
       ( cl_gui_alv_grid=>mc_fc_loc_undo   )
       ( cl_gui_alv_grid=>mc_fc_loc_copy_row     )
       ( cl_gui_alv_grid=>mc_fc_loc_delete_row  )
       ( cl_gui_alv_grid=>mc_fc_loc_append_row  )
       ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
       ( cl_gui_alv_grid=>mc_fc_loc_move_row   )
       ( cl_gui_alv_grid=>mc_fc_loc_copy   )
       ( cl_gui_alv_grid=>mc_fc_loc_cut      )
       ( cl_gui_alv_grid=>mc_fc_loc_paste    )
       ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row   )
       ( cl_gui_alv_grid=>mc_fc_loc_undo )
       ( cl_gui_alv_grid=>mc_fc_refresh   )
       ( cl_gui_alv_grid=>mc_fc_check   )
       ( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard )
       ( cl_gui_alv_grid=>mc_fc_views    )
       ( cl_gui_alv_grid=>mc_fc_data_save )
       ( cl_gui_alv_grid=>mc_fc_call_xml_export )
    ) INTO TABLE rt_excluded.
  ENDMETHOD.


  METHOD handle_data_changed_finished.

  ENDMETHOD.


  METHOD add_button.
**********************************************************************
*10: Create Button
**********************************************************************
    INSERT VALUE #( function    = i_function
                    icon        = i_icon
                    quickinfo   = i_quickinfo
                    text        = COND #( WHEN i_text IS SUPPLIED
                                          THEN i_text ELSE space )
                    butn_type   = 0 )
    INTO TABLE ct_toolbar.
  ENDMETHOD.


  METHOD add_separator.
**********************************************************************
*10: Create Separator
**********************************************************************
    INSERT VALUE stb_button( butn_type = 3 )
    INTO   TABLE ct_toolbar.
  ENDMETHOD.


  METHOD handle_data_changed.
  ENDMETHOD.


  METHOD handle_f4.
  ENDMETHOD.


  METHOD handle_hotspot_click.
**********************************************************************
*10: Assign Row Data
**********************************************************************
    FIELD-SYMBOLS: <fs> TYPE t_cont_head_gui.
    ASSIGN me->gt_parent_data[ e_row_id ]
    TO <fs>.
**********************************************************************
*20: Identify Column
**********************************************************************
    CASE e_column_id.
**********************************************************************
*20.1: Display RE-FX Contract
**********************************************************************
      WHEN 'CONTRACT' OR 'COMP_CODE' OR 'CONTRACT_TYPE'.
        SELECT SINGLE
        FROM vicncn
        FIELDS
        @abap_true
        WHERE recnnr = @<fs>-contract
        AND   bukrs  = @<fs>-comp_code
        INTO @DATA(lf_contract_exists).
        CHECK lf_contract_exists = abap_true.
        SET PARAMETER ID 'BUK'      FIELD <fs>-comp_code.
        SET PARAMETER ID 'RECNNR'   FIELD <fs>-contract.

        CALL TRANSACTION gc_recn AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.


  METHOD handle_toolbar.
**********************************************************************
*10: Delete Toolbar func
**********************************************************************
    DELETE e_object->mt_toolbar
    WHERE function IN VALUE tr_functions(
         ( low     =  cl_gui_alv_grid=>mc_fc_info           sign = 'I' option = 'EQ' )
         ( low     =  cl_gui_alv_grid=>mc_fc_graph          sign = 'I' option = 'EQ' )
         ( low     =  cl_gui_alv_grid=>mc_fc_loc_insert_row sign = 'I' option = 'EQ' )
         ( low     =  cl_gui_alv_grid=>mc_fc_refresh        sign = 'I' option = 'EQ' )
    ).
**********************************************************************
*20: Add Refresh Btn
**********************************************************************
    me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
    me->add_button(
        EXPORTING
            i_function  = gc_refresh
            i_icon      = icon_refresh
            i_quickinfo = CONV #( gc_refresh_text )
        CHANGING
            ct_toolbar  = e_object->mt_toolbar
     ).
**********************************************************************
*30: Add Display Cond Btn
**********************************************************************
    me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
    me->add_button(
        EXPORTING
            i_function  = gc_disp_cond
            i_icon      = icon_display
            i_quickinfo = CONV #( gc_disp_cond_text )
            i_text      = CONV #( gc_disp_cond_text )
        CHANGING
            ct_toolbar  = e_object->mt_toolbar
     ).
**********************************************************************
*40: Add Simulate Btn
**********************************************************************
    me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
    DATA(lf_text) = gc_simulate_contr_text.
    lf_text  =
       COND #( WHEN gf_mode = gc_create_mode
               THEN  replace( val = lf_text sub = `&1` with = `Create` )
               WHEN gf_mode = gc_change_mode
               THEN  replace( val = lf_text sub = `&1` with = `Change` ) ) .

    me->add_button(
       EXPORTING
           i_function  = gc_simulate_contr
           i_icon      = icon_simulate
           i_quickinfo = CONV #( lf_text )
           i_text      = CONV #( lf_text )
       CHANGING
           ct_toolbar  = e_object->mt_toolbar
    ).
**********************************************************************
*50: Add Create Btn
**********************************************************************
    IF xsdbool(  me->gf_mode EQ gc_create_mode ) EQ abap_true.
      me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
      me->add_button(
         EXPORTING
             i_function  = gc_create_contr
             i_icon      = icon_create
             i_quickinfo = CONV #( gc_create_contr_text )
             i_text      = CONV #( gc_create_contr_text )
         CHANGING
             ct_toolbar  = e_object->mt_toolbar
      ).
    ENDIF.
    me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
**********************************************************************
*50: Add Change Btn
**********************************************************************
    IF xsdbool(  me->gf_mode EQ gc_change_mode ) EQ abap_true.
      me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
      me->add_button(
         EXPORTING
             i_function  = gc_change_contr
             i_icon      = icon_change
             i_quickinfo = CONV #( gc_change_contr_text )
             i_text      = CONV #( gc_change_contr_text )
         CHANGING
             ct_toolbar  = e_object->mt_toolbar
      ).
    ENDIF.
    me->add_separator( CHANGING ct_toolbar = e_object->mt_toolbar  ).
     me->add_button(
       EXPORTING
           i_function  = gc_log
           i_icon      = icon_protocol
           i_quickinfo = CONV #( gc_log_text )
           i_text      = CONV #( gc_log_text )
       CHANGING
           ct_toolbar  = e_object->mt_toolbar
    ).
  ENDMETHOD.


  METHOD handle_user_command.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lt_selected_rows TYPE tt_cont_head_gui.
**********************************************************************
*20: Retrieve Selected Row Indexes
**********************************************************************
    go_gui_p_alv->get_selected_rows(
        IMPORTING
            et_index_rows = DATA(lt_index_rows)
    ).
**********************************************************************
*30: Retrieve Selected Row Records
**********************************************************************
    LOOP AT lt_index_rows
        ASSIGNING FIELD-SYMBOL(<fs_index_rows>).
      APPEND gt_parent_data[ <fs_index_rows>-index ]
      TO lt_selected_rows.
    ENDLOOP.
**********************************************************************
*40: Handle Command
**********************************************************************
    CASE e_ucomm.
**********************************************************************
*40.1: Refresh cmd
**********************************************************************
      WHEN gc_refresh.
        me->refresh_alv(  ).
**********************************************************************
*40.2: Display Conditions cmd
**********************************************************************
      WHEN gc_disp_cond.
        TRY.
            IF lt_selected_rows IS INITIAL.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            ENDIF.
            me->display_child_tab( lt_selected_rows ).
          CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
            me->display_bapi_log_gui(
                create_bapi_ret_from_exception( io_exception = lo_cx_refx )
             ).
        ENDTRY.
**********************************************************************
*40.3: Simulate Contract cmd
**********************************************************************
      WHEN gc_simulate_contr.

        TRY.
            IF lt_selected_rows IS INITIAL.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            ENDIF.
            me->simulate_refx_cn_contract( if_simu = abap_true it_selected_rows = lt_selected_rows  ).

            me->refresh_alv(  ).
          CATCH zcx_refx_exception INTO lo_cx_refx.
            me->display_bapi_log_gui(
                create_bapi_ret_from_exception( io_exception = lo_cx_refx )
             ).
        ENDTRY.
**********************************************************************
*40.4: Create Contract cmd
**********************************************************************
      WHEN gc_create_contr.
        TRY.
            IF lt_selected_rows IS INITIAL.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            ENDIF.
            me->create_refx_cn_contract( lt_selected_rows ).

            me->refresh_alv(  ).
          CATCH zcx_refx_exception INTO lo_cx_refx.
            me->display_bapi_log_gui(
                create_bapi_ret_from_exception( io_exception = lo_cx_refx )
             ).
        ENDTRY.
**********************************************************************
*40.5: Change contract cmd
**********************************************************************
    when gc_change_contr.
        try.
            IF lt_selected_rows IS INITIAL.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            ENDIF.
            me->change_refx_cn_contract( lt_selected_rows ).

            me->refresh_alv(  ).
        catch zcx_refx_exception into lo_cx_refx.
             me->display_bapi_log_gui(
                create_bapi_ret_from_exception( io_exception = lo_cx_refx )
             ).
        endtry.
**********************************************************************
*40.6: Display log cmd
**********************************************************************
      WHEN gc_log.
      try.
        me->create_bapi_log(  ).
      CATCH zcx_refx_exception INTO lo_cx_refx.
            me->display_bapi_log_gui(
                create_bapi_ret_from_exception( io_exception = lo_cx_refx )
             ).
        ENDTRY.
    ENDCASE.

  ENDMETHOD.


  METHOD set_key_fields_changeable.
  ENDMETHOD.


  METHOD change_field_catalog.
  ENDMETHOD.


  METHOD display_child_tab.
**********************************************************************
*10: Retrieve Child Records
**********************************************************************
    CLEAR: lt_child_data.
    lt_child_data =
    VALUE tt_cont_cond_gui(
        FOR <fs> IN gt_child_data
            WHERE ( id IN VALUE ty_tr_id(
                FOR <ffs> IN it_selected_rows
                ( low = <ffs>-id
                  sign = 'I'
                  option = 'EQ'
                )
             ) ) ( <fs> ) ).
**********************************************************************
*20: Display Child Tab
**********************************************************************
    TRY.
        go_gui_c_alv->set_table_for_first_display(
          EXPORTING
            is_layout                     = me->gs_c_layout
            it_toolbar_excluding          = me->gt_toolbar_ex
          CHANGING
            it_outtab                     = lt_child_data
            it_fieldcatalog               = gt_child_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4 ).
**********************************************************************
*30: Catch Exceptions
**********************************************************************
      CATCH cx_salv_msg cx_root.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid = zcx_refx_exception=>refx_cn_alv_error.

    ENDTRY.
  ENDMETHOD.


  METHOD display_log_tab.
**********************************************************************
*10: Display Log Data
**********************************************************************
    TRY.
        go_gui_c_alv->set_table_for_first_display(
          EXPORTING
            is_layout                     = me->gs_l_layout
            it_toolbar_excluding          = me->gt_toolbar_ex
          CHANGING
            it_outtab                     = me->gt_log_data
            it_fieldcatalog               = gt_log_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4 ).
      CATCH cx_salv_msg cx_root.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid = zcx_refx_exception=>refx_cn_alv_error.

    ENDTRY.
  ENDMETHOD.


  METHOD handle_double_click.
**********************************************************************
*10: Retrieve clicked row
**********************************************************************
    TRY.
        DATA(ls_clicked)
            = gt_parent_data[ e_row ].
**********************************************************************
*20: Display Child
**********************************************************************

        me->display_child_tab( VALUE #( ( ls_clicked ) ) ).
**********************************************************************
*30: Catch Exceptions
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        me->display_bapi_log_gui(
            create_bapi_ret_from_exception( io_exception = lo_cx_refx )
         ).
    ENDTRY.
  ENDMETHOD.


  METHOD create_bapi_log.
**********************************************************************
*10: Retrieve BAPI return
**********************************************************************
    if it_bapi_return is supplied.
        gt_log_data = CORRESPONDING #( it_bapi_return MAPPING num = number ).
    endif.
**********************************************************************
*20: Decorate
**********************************************************************
    loop at gt_log_data ASSIGNING FIELD-SYMBOL(<fs_log>).
     <fs_log>-icon = COND #( WHEN xsdbool(
                            contains_any_of(
                                 val = <fs_log>-type sub = 'EAX' ) )
                    EQ abap_true THEN icon_led_red
                       WHEN <fs_log>-type EQ 'W' THEN icon_led_yellow
                       WHEN <fs_log>-type EQ 'S' THEN icon_led_green
                       WHEN <fs_log>-type EQ 'I' THEN icon_information
                    ).
   endloop.
**********************************************************************
*30: Display Log grid
**********************************************************************
    TRY.
        me->display_log_tab( it_log = gt_log_data ).
**********************************************************************
*40: Catch Exceptions
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.
  ENDMETHOD.


  METHOD create_bapi_ret_from_sy_msg.
**********************************************************************
*10: Create msg
**********************************************************************
    INSERT VALUE #( id         = COND #( WHEN if_id     IS SUPPLIED THEN if_id      ELSE sy-msgid )
                    number     = COND #( WHEN if_num    IS SUPPLIED THEN if_num     ELSE sy-msgno )
                    type       = COND #( WHEN if_type   IS SUPPLIED THEN if_type    ELSE sy-msgty )
                    message_v1 = COND #( WHEN if_msgv1  IS SUPPLIED THEN if_msgv1   ELSE sy-msgv1 )
                    message_v2 = COND #( WHEN if_msgv2  IS SUPPLIED THEN if_msgv2   ELSE sy-msgv2 )
                    message_v3 = COND #( WHEN if_msgv3  IS SUPPLIED THEN if_msgv3   ELSE sy-msgv3 )
                    message_v4 = COND #( WHEN if_msgv4  IS SUPPLIED THEN if_msgv4   ELSE sy-msgv4 )
                    message    = COND #( WHEN xsdbool(
                                              if_message IS SUPPLIED AND if_message IS NOT INITIAL )
                                              EQ abap_true THEN
                                              if_message ELSE space ) )
    INTO TABLE rt_msg .
**********************************************************************
*20: Format Msg if required
**********************************************************************
    TRY.
        IF rt_msg IS NOT INITIAL AND
           rt_msg[ 1 ]-message IS INITIAL.
          ASSIGN rt_msg[ 1 ]-message TO FIELD-SYMBOL(<fs_msg>).
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = COND #( WHEN if_id IS SUPPLIED THEN if_id ELSE sy-msgid )
              no        = COND #( WHEN if_num IS SUPPLIED THEN if_num ELSE sy-msgno )
              v1        = COND #( WHEN if_msgv1 IS SUPPLIED THEN if_msgv1 ELSE sy-msgv1 )
              v2        = COND #( WHEN if_msgv2 IS SUPPLIED THEN if_msgv2 ELSE sy-msgv2 )
              v3        = COND #( WHEN if_msgv3 IS SUPPLIED THEN if_msgv3 ELSE sy-msgv3 )
              v4        = COND #( WHEN if_msgv4 IS SUPPLIED THEN if_msgv4 ELSE sy-msgv4 )
            IMPORTING
              msg       = <fs_msg>
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
        ENDIF.
**********************************************************************
*30: Catch Exceptions
**********************************************************************
      CATCH cx_sy_itab_line_not_found INTO DATA(lo_cx_itab).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_msg_conv_fail
            previous = lo_cx_itab.
**********************************************************************
*40: Catch Other Exceptions
**********************************************************************
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_msg_conv_fail
            previous = lo_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD display_bapi_log_gui.
**********************************************************************
*10: Display Bapi return
**********************************************************************
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = it_bapi_ret.
  ENDMETHOD.


  METHOD configure_grid.
**********************************************************************
*10: Configure variant
**********************************************************************
    me->gs_variant =
        VALUE #( report    = sy-repid
                 username  = sy-uname ).
**********************************************************************
*20: Create Toolbar excl.
**********************************************************************
    me->gt_toolbar_ex =
         me->get_tool_bar_excluded(  ).
  ENDMETHOD.


  METHOD display_grid.
**********************************************************************
*10: Display Tab
**********************************************************************
    TRY.
        co_alv_grid->set_table_for_first_display(
        EXPORTING
            is_layout                     = is_layout
            it_toolbar_excluding          = gt_toolbar_ex
          CHANGING
            it_outtab                     = ct_tab
            it_fieldcatalog               = ct_fieldcatalog
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4 ).
**********************************************************************
*20: Catch Exceptions
**********************************************************************
      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_salv_msg.
**********************************************************************
*30: Catch Other Exceptions
**********************************************************************
      CATCH cx_root INTO DATA(lo_root_msg).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_root_msg.
    ENDTRY.

  ENDMETHOD.


  METHOD display_layout_selection.
**********************************************************************
*10: Type pools
**********************************************************************
    TYPE-POOLS: slis.
**********************************************************************
*20: Data Declaration
**********************************************************************
    DATA: lt_fieldcatalog TYPE slis_t_fieldcat_alv.
    DATA: lf_check_exit TYPE flag.
**********************************************************************
*30: Default Layout Selection
**********************************************************************s
    INSERT LINES OF VALUE
    zcl_refx_cn_contract_fs=>tt_layout_sheet(
    (
             refx_cn_in = 'CONTR_HEAD'
             sheet_no   = 2
             start_row  = 3
    )
    (
             refx_cn_in = 'CONTR_COND'
             sheet_no   = 3
             start_row  = 3
    ) )
    INTO TABLE rt_layout_sheet.
**********************************************************************
*40: Create field catalog
**********************************************************************
    INSERT LINES OF VALUE slis_t_fieldcat_alv(
    (
        fieldname = 'REFX_CN_IN'
        col_pos   = 1
        seltext_l = `RE-FX Contract Data`
        outputlen = 20
        tabname   = 'RT_LAYOUT_SHEET'
    )
    (
        fieldname = 'SHEET_NO'
        col_pos   = 2
        seltext_l = `Sheet No.`
        outputlen = 10
        input     = abap_true
        edit      = abap_true
        tabname   = 'RT_LAYOUT_SHEET'
    )
    (
        fieldname = 'START_ROW'
        col_pos   = 3
        seltext_l = `Start Row.`
        outputlen = 10
        input     = abap_true
        edit      = abap_true
        tabname   = 'RT_LAYOUT_SHEET'
    )
    ) INTO  TABLE lt_fieldcatalog.
**********************************************************************
*50: Display Pop-Up
**********************************************************************
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title                 = `Worksheet Selection (Excel)`
        i_selection             = abap_true
        i_zebra                 = 'X'
        i_screen_start_column   = 5
        i_screen_start_line     = 5
        i_screen_end_column     = 50
        i_screen_end_line       = 10
        i_tabname               = 'RT_LAYOUT_SHEET'
        i_scroll_to_sel_line    = 'X'
        it_fieldcat             = lt_fieldcatalog
        i_callback_program      = sy-repid
        i_callback_user_command = 'USER_COMMAND'
      IMPORTING
        e_exit                  = lf_check_exit
      TABLES
        t_outtab                = rt_layout_sheet
      EXCEPTIONS
        program_error           = 1.
    IF lf_check_exit EQ abap_true.
      CLEAR: rt_layout_sheet.
    ENDIF.
  ENDMETHOD.


  METHOD create_bapi_ret_from_exception.
**********************************************************************
*10: Create BAPI return from Exception
**********************************************************************
    CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
      EXPORTING
        i_r_exception = io_exception
      CHANGING
        c_t_bapiret2  = rt_msg.

  ENDMETHOD.


  METHOD refresh_alv.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA: ls_lvc_s_stbl TYPE lvc_s_stbl.
**********************************************************************
*20: Stable refresh
**********************************************************************
    ls_lvc_s_stbl = VALUE #(
      row = abap_true
      col = abap_true
    ).
**********************************************************************
*30: Refresh display
**********************************************************************
    go_gui_p_alv->refresh_table_display(
      EXPORTING
        is_stable      = ls_lvc_s_stbl
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).
  ENDMETHOD.


  METHOD simulate_refx_cn_contract.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lo_refx_contract TYPE REF TO zcl_refx_cn_contract.
    TRY.
**********************************************************************
*20: Contract Checks
**********************************************************************
        me->contract_check( it_selected_rows ).
**********************************************************************
*30: Catch Exceptions
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            previous = lo_cx_refx.
    ENDTRY.
**********************************************************************
*40: Simulate Contract
**********************************************************************
    TRY.
        DATA lt_contr_head TYPE STANDARD TABLE OF zcl_refx_cn_contract_fs=>t_cont_head.
        DATA lt_contr_cond TYPE STANDARD TABLE OF zcl_refx_cn_contract_fs=>t_cont_cond.
        data gt_return type bapiret2_t.
**********************************************************************
*50: Assign selected rows
**********************************************************************
        LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
          CLEAR: lt_contr_head, lt_contr_cond.
**********************************************************************
*60: Create contract header
**********************************************************************
          APPEND INITIAL LINE TO lt_contr_head ASSIGNING FIELD-SYMBOL(<fs_contr_head>).
          <fs_contr_head> = CORRESPONDING #( <fs_rows> ).
**********************************************************************
*70: Create contract conditions
**********************************************************************
          DATA(l_child_data) =
                     VALUE tt_cont_cond_gui(
                         FOR <fs> IN gt_child_data
                             WHERE ( id EQ <fs_rows>-id ) ( <fs> ) ).

          lt_contr_cond = CORRESPONDING #( l_child_data ).
**********************************************************************
*80: Create new instance of contract
**********************************************************************
          DATA(lo_contract) = NEW zcl_refx_cn_contract(
            if_comp_code     = <fs_rows>-comp_code
            if_contract_type = <fs_rows>-contract_type
            if_contract      = <fs_rows>-contract
            if_test_run      = if_simu
          ).
**********************************************************************
*90: Fill BAPI fields
**********************************************************************
          lo_contract->fill_bapi_fields(
            EXPORTING
              it_contr_head = lt_contr_head
              it_contr_cond = lt_contr_cond
          ).
**********************************************************************
*100: Check Mode
**********************************************************************
          if xsdbool( me->gf_mode eq gc_create_mode ) eq abap_true.
**********************************************************************
*110: Create Mode: Create Contract
**********************************************************************
              data(lt_bapi_return) =
                lo_contract->create_refx_cn_contract(
                    importing
                        ef_comp_code = gt_parent_data[ id = <fs_rows>-id ]-comp_code
                        ef_contract = gt_parent_data[ id = <fs_rows>-id ]-contract
                 ).
**********************************************************************
*120: Retrieve response
**********************************************************************
              append lines of lt_bapi_return to gt_return.
          else.
**********************************************************************
*140: Change Mode: Create Change parameters
**********************************************************************
            lo_contract->create_change_parameters( ).
**********************************************************************
*150: Change contract and retrieve response
**********************************************************************
            lt_bapi_return = lo_contract->change_refx_cn_contract( ).
            append lines of lt_bapi_return to gt_return.
          endif.
**********************************************************************
*160: Refresh Contract instance
**********************************************************************
          clear: lo_contract.
        ENDLOOP.
**********************************************************************
*170: Display Log
**********************************************************************
        create_bapi_log(
            EXPORTING
              it_bapi_return = lt_bapi_return
          ).

      CATCH zcx_refx_exception INTO lo_cx_refx.
*        create_bapi_ret_from_exception( io_exception = lo_cx_refx ).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            previous = lo_cx_refx.
    ENDTRY.
  ENDMETHOD.


  METHOD create_refx_cn_contract.
**********************************************************************
*10: Create contract in update mode
**********************************************************************
    simulate_refx_cn_contract( if_simu = abap_false it_selected_rows = it_selected_rows ).
  ENDMETHOD.


  METHOD change_refx_cn_contract.
**********************************************************************
*20: Change contract in update mode
**********************************************************************
     simulate_refx_cn_contract( if_simu = abap_false it_selected_rows = it_selected_rows ).
  ENDMETHOD.


  METHOD contract_check.
**********************************************************************
*10: Contract Check
**********************************************************************
    LOOP AT it_selected_rows
    ASSIGNING FIELD-SYMBOL(<fs_rows>).
      IF  xsdbool(  line_exists( me->gt_child_data[ id = <fs_rows>-id ] ) ) EQ abap_false.
**********************************************************************
*20: Raise Exception
**********************************************************************
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid           = zcx_refx_exception=>refx_cn_null_contr_cond
            refx_cn_contr_id = CONV #( <fs_rows>-id ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD default_company.
    loop at me->gt_parent_data ASSIGNING FIELD-SYMBOL(<fs_parent>).
        <fs_parent>-comp_code = cond #(  when <fs_parent>-comp_code is initial
            then 'NG01' else <fs_parent>-comp_code
        ).
    endloop.
  ENDMETHOD.
ENDCLASS.
