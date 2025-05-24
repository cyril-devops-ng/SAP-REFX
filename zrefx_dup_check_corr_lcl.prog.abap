*&---------------------------------------------------------------------*
*& Include          ZREFX_DUP_CHECK_CORR_LCL
*&---------------------------------------------------------------------*
CLASS lcl_dup_check IMPLEMENTATION.

  METHOD create.
**********************************************************************
*10: Factory method
**********************************************************************
    TRY.
**********************************************************************
*20: Create instance
**********************************************************************
        r_result = NEW #( ).
**********************************************************************
*30: Assign selection fields
**********************************************************************
        r_result->lr_contract_no    = i_contract_no.
        r_result->lr_comp_code      = i_comp_code.
        r_result->lr_contr_type     = i_contr_type.
        r_result->lr_contract_start = i_contract_start.
        r_result->lr_first_end_dat  = i_first_end_dat.
**********************************************************************
*40: Catch exceptions
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_ex).
**********************************************************************
*50: Handle exceptions
**********************************************************************
        DATA(lt_msg) = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_ex ).
        zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
    ENDTRY.
  ENDMETHOD.
  METHOD constructor.
    TRY.
**********************************************************************
*10: Dupl. check instance
**********************************************************************
        IF p_r_dchk EQ abap_true.
          me->retrieve_set(  ).
**********************************************************************
*20: Check-in/Check-out instance
**********************************************************************
        ELSEIF xsdbool( p_r_cin  EQ abap_true
                     OR p_r_cout EQ abap_true )
                     EQ abap_true.
**********************************************************************
*30: Contract No/Contract Type is mandatory
**********************************************************************
          IF xsdbool(  if_contract_no   IS NOT SUPPLIED
                    OR if_contract_type IS NOT SUPPLIED
                    OR ( if_contract_no   IS INITIAL
                    AND  if_contract_type IS INITIAL ) )
                    EQ abap_true.
**********************************************************************
*40: Raise exception
**********************************************************************
            RAISE EXCEPTION TYPE zcx_refx_exception
              EXPORTING
                textid              = zcx_refx_exception=>refx_cn_req_field_error
                refx_cn_field_name  = `Contract No/Contract Type`
                refx_cn_field_value = `Selection`.
          ENDIF.
**********************************************************************
*50: Assign Contr. No/Contr. type attributes
**********************************************************************
          me->lf_contract_no   = if_contract_no.
          me->lf_contract_type = if_contract_type.
**********************************************************************
*60: Retrieve specific set
**********************************************************************
          me->retrieve_specific_set(
            EXPORTING
              if_contract_no   = if_contract_no
              if_contract_type = if_contract_type
              if_check_in      = p_r_cin
              if_check_out     = p_r_cout
          ).
        ENDIF.
**********************************************************************
*70: Catch exceptions
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.
  ENDMETHOD.

  METHOD create_grid.
**********************************************************************
*10: Create ALV
**********************************************************************
    TRY.
        IF go_alv IS INITIAL.
          cl_salv_table=>factory(
              EXPORTING
                  r_container = cl_gui_container=>default_screen
               IMPORTING
                  r_salv_table = go_alv
               CHANGING
                  t_table      = ct_tab
            ).
        ENDIF.
**********************************************************************
*20: Raise Exception
**********************************************************************
      CATCH cx_salv_msg INTO DATA(lo_alv_ex).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_alv_ex.
    ENDTRY.
**********************************************************************
*40: Set up ALv
**********************************************************************
    IF go_alv IS BOUND.
      TRY.
          IF xsdbool(  p_r_cin EQ abap_true OR p_r_dchk EQ abap_true )
          EQ abap_true.
            add_func 'Check-In'   gc_checkin  '@VY@' 'Check-In'.
          ENDIF.
          IF xsdbool(  p_r_cout EQ abap_true OR p_r_dchk EQ abap_true )
          EQ abap_true.
            add_func 'Check-Out'  gc_checkout '@VZ@' 'Check-Out'.
          ENDIF.
          add_func 'Refresh'    gc_refresh  '@42@' 'Refresh'.

          go_alv->get_functions(  )->set_all( abap_true ).

          SET HANDLER on_added_function FOR go_alv->get_event(  ).
          SET HANDLER on_link_click     FOR go_alv->get_event(  ).
**********************************************************************
*50: Raise Exception
**********************************************************************
        CATCH cx_salv_existing cx_salv_wrong_call cx_salv_method_not_supported.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_cn_alv_error.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD on_added_function.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lt_selected_rows TYPE  TABLE OF t_output.
**********************************************************************
*20: Retrieve Selected Row Indexes
**********************************************************************
    DATA(lt_sel_rows) = go_select->get_selected_rows(  ).
**********************************************************************
*30: Retrieve Selected Row Records
**********************************************************************
    LOOP AT lt_sel_rows
        ASSIGNING FIELD-SYMBOL(<fs_index_rows>).
      IF line_exists(  gt_output[ <fs_index_rows> ] ).
        APPEND gt_output[ <fs_index_rows> ]
        TO lt_selected_rows.
      ENDIF.
    ENDLOOP.
**********************************************************************
*40: Handle tool bar func.
**********************************************************************
    CASE e_salv_function.
**********************************************************************
*50: Refresh func.
**********************************************************************
      WHEN gc_refresh.
        me->refresh_grid(  ).
**********************************************************************
*60: Check-in function
**********************************************************************
      WHEN gc_checkin.
**********************************************************************
*70: Check selection
**********************************************************************
        IF lt_selected_rows IS INITIAL.
          TRY.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
              DATA(lt_msg) = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx ).
              convert_message_to_success 'ZREFX' '029'.
              convert_message_to_success 'ZREFX' '030'.
              zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
**********************************************************************
*80: Process checkin function
**********************************************************************
        ELSE.
          TRY.
              me->check_in( lt_selected_rows ).
**********************************************************************
*90: Exception handling
**********************************************************************
            CATCH zcx_refx_exception INTO lo_cx_refx.
              lt_msg = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx ).
              convert_message_to_success 'ZREFX' '026'.
              convert_message_to_success 'ZREFX' '029'.
              zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
          CLEAR: lt_msg.
          me->refresh_grid(  ).
        ENDIF.
**********************************************************************
*100: Check-out function
**********************************************************************
      WHEN gc_checkout.
**********************************************************************
*110: Check selection
**********************************************************************
        IF lt_selected_rows IS INITIAL.
          TRY.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid = zcx_refx_exception=>refx_cn_null_record.
            CATCH zcx_refx_exception INTO lo_cx_refx.
              lt_msg = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx ).
              convert_message_to_success 'ZREFX' '027'.
              convert_message_to_success 'ZREFX' '030'.
              zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
        ELSE.
**********************************************************************
*120: Process checkout function
**********************************************************************
          TRY.
              me->check_out( lt_selected_rows ).
            CATCH zcx_refx_exception INTO lo_cx_refx.
              lt_msg = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx ).
              convert_message_to_success 'ZREFX' '027'.
              convert_message_to_success 'ZREFX' '030'.
              zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
          ENDTRY.
          CLEAR: lt_msg.
          me->refresh_grid(  ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD on_link_click.
**********************************************************************
*10: Retrieve record
**********************************************************************
    IF line_exists( gt_output[ row ] ).
      ASSIGN gt_output[ row ] TO FIELD-SYMBOL(<fs>).
      CASE column.
**********************************************************************
*20: Handle contract/compcode
**********************************************************************
        WHEN 'CONTRACT_NO' OR 'COMP_CODE'.
          SELECT SINGLE
          FROM vicncn
          FIELDS
          @abap_true
          WHERE recnnr = @<fs>-contract_no
          AND   bukrs  = @<fs>-comp_code
          INTO @DATA(lf_contract_exists).

          CHECK lf_contract_exists = abap_true.
          SET PARAMETER ID 'BUK'      FIELD <fs>-comp_code.
          SET PARAMETER ID 'RECNNR'   FIELD <fs>-contract_no.

          CALL TRANSACTION gc_recn AND SKIP FIRST SCREEN.
**********************************************************************
*30: Handle ObjNumber
**********************************************************************
        WHEN 'OBJ_NUMBER'.
          SELECT SINGLE
          FROM vibdobjass AS oa
          INNER JOIN vicncn AS cn
          ON oa~objnrsrc =
          cn~objnr
          FIELDS
          @abap_true
          WHERE cn~recnnr      EQ @<fs>-contract_no
          AND   cn~bukrs       EQ @<fs>-comp_code
          AND   cn~recntype    EQ @<fs>-contr_type
          AND   oa~objnrtrg    EQ @<fs>-obj_number
          INTO @DATA(lf_obj_exists).

          CHECK lf_obj_exists = abap_true.
          "call rental object transaction
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD refresh_grid.
**********************************************************************
*10: Retrieve set
**********************************************************************
    TRY.
        IF p_r_dchk EQ abap_true.
          me->retrieve_set(  ).
        ELSEIF xsdbool( p_r_cin  EQ abap_true
                     OR p_r_cout EQ abap_true )
                     EQ abap_true.
**********************************************************************
*20: Retrieve specific set
**********************************************************************
          me->retrieve_specific_set(
              EXPORTING
                if_contract_no   = me->lf_contract_no
                if_contract_type = me->lf_contract_type
                if_check_in      = p_r_cin
                if_check_out     = p_r_cout
            ).
        ENDIF.
        go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
**********************************************************************
*30: Catch exception
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx).
        DATA(lt_msg) = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx ).
        convert_message_to_success 'ZREFX' '026'.
        convert_message_to_success 'ZREFX' '027'.
        convert_message_to_success 'ZREFX' '029'.
        convert_message_to_success 'ZREFX' '030'.
        zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg ).
        go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    ENDTRY.
  ENDMETHOD.

  METHOD retrieve_set.
**********************************************************************
*10: Retrieve Data
**********************************************************************
    CLEAR: gt_output.
    SELECT FROM
    vicncn AS cn
    INNER JOIN
    vibdobjass AS obj
    ON obj~objnrsrc =
    cn~objnr INNER JOIN
    zvibdobjass AS zobj
    ON obj~objnrsrc =
    zobj~objnrsrc AND
    obj~objasstype =
    zobj~objasstype AND
    obj~objnrtrg =
    zobj~objnrtrg AND
    obj~validfrom =
    zobj~validfrom
    FIELDS
    cn~recnnr       AS contract_no,
    cn~bukrs        AS comp_code,
    cn~recntype     AS contr_type,
    cn~recntxt      AS contract_txt,
    cn~recnbeg      AS cont_st_dat,
    cn~recnend1st   AS frst_end_dat,
    obj~objnrtrg    AS obj_number,
    obj~validfrom   AS valid_from,
    obj~validto     AS valid_to,
    zobj~zcheckin   AS zcheckin,
    zobj~zcheckout  AS zcheckout
    WHERE cn~recnnr         IN @lr_contract_no
    AND   cn~bukrs          IN @lr_comp_code
    AND   cn~recntype       IN @lr_contr_type
    AND   cn~recnbeg        IN @lr_contract_start
    AND   cn~recnend1st     IN @lr_first_end_dat
    AND   zobj~zcheckin     EQ @abap_true
    AND   zobj~zcheckout    EQ @abap_true
    INTO CORRESPONDING FIELDS OF TABLE
    @gt_output.
  ENDMETHOD.

  METHOD show_grid.
    TRY.
**********************************************************************
*10: Define structure
**********************************************************************
        DATA lo_str TYPE REF TO cl_abap_structdescr.
        lo_str ?= cl_abap_structdescr=>describe_by_data( VALUE t_output( )  ).
        go_columns = go_alv->get_columns(  ).
**********************************************************************
*20: Set field catalog
**********************************************************************
        DO lines(  lo_str->components ) TIMES.
          ASSIGN lo_str->components[ sy-index ] TO FIELD-SYMBOL(<fs_component>).
          CASE <fs_component>-name.
            WHEN 'CONTRACT_NO' OR 'COMP_CODE' OR 'OBJ_NUMBER'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            WHEN 'ZCHECKIN' OR 'ZCHECKOUT'.
              _thiscolumn.
              go_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
              go_column->set_icon( 'X' ).
              go_column->set_output_length( 4 ).
            WHEN 'CONTRACT_TXT'.
              _thiscolumn.
              go_column->set_output_length( 30 ).
          ENDCASE.
        ENDDO.
**********************************************************************
*30: Selection
**********************************************************************
        go_select = go_alv->get_selections(  ).
        IF xsdbool(  go_select IS NOT INITIAL ) EQ abap_true.
          go_select->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        ENDIF.
        go_alv->display(  ).
**********************************************************************
*40: Exception
**********************************************************************
      CATCH cx_salv_not_found INTO DATA(lo_alv_ex).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_alv_error
            previous = lo_alv_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD check_in.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA:lf_confirm      TYPE char1,
         ls_ci           TYPE rebd_rental_object_ci,
         lt_extension_in TYPE TABLE OF bapiparex,
         lt_return       TYPE TABLE OF bapiret2,
         ls_extension_in TYPE bapiparex,
         lf_cleaned      TYPE char1.
**********************************************************************
*20: Confirm
**********************************************************************
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = `Check-In Rental Object`
        text_question         = `Check-In Rental Object(s) Y/N?`
        text_button_1         = `Yes`
        text_button_2         = `No`
        icon_button_1         = 'ICON_CHECKED'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lf_confirm.
**********************************************************************
*30: Check user action
**********************************************************************
    IF xsdbool( lf_confirm EQ '1' ) EQ abap_true.
      TRY.
**********************************************************************
*40: Process checkin for selected records
**********************************************************************
          LOOP AT it_selection ASSIGNING
              FIELD-SYMBOL(<fs_selection>).
            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
              EXPORTING
                percentage = sy-tabix
                text       = `Processing rental object check-in....`.
**********************************************************************
*50: Get contract ObjNumber
**********************************************************************
            SELECT SINGLE
            FROM
            vicncn
            FIELDS
            objnr
            WHERE recnnr EQ @<fs_selection>-contract_no
            INTO @DATA(lf_obj_source).
**********************************************************************
*60: Check-in
**********************************************************************
            IF xsdbool(  lf_obj_source IS INITIAL ) EQ abap_false.
              UPDATE zvibdobjass
              SET zcheckin    = abap_true
                  zoccupied   = abap_true
                  zcheckout   = abap_false
                  zbooked     = abap_false
              WHERE objnrtrg  = <fs_selection>-obj_number
              AND   validfrom = <fs_selection>-valid_from
              AND   objnrsrc  = lf_obj_source.
              COMMIT WORK.
**********************************************************************
*70: Get Rental object
**********************************************************************
              SELECT SINGLE
              FROM vibdro
              FIELDS *
              WHERE objnr EQ @lf_obj_source
              AND zzrecleaned NE @space
              INTO @DATA(ls_rental_object).
**********************************************************************
*80: Check If rental object exists & Is an Actual. check in / check out
**********************************************************************
              IF xsdbool( ls_rental_object IS NOT INITIAL
                     AND ( p_r_cin EQ abap_true OR p_r_cout EQ abap_true ) )
                     EQ abap_true.
**********************************************************************
*90: Unset bapi fields
**********************************************************************
                CLEAR:
                        ls_ci, lt_extension_in, ls_extension_in, lt_return.
**********************************************************************
*100: Set to be cleaned as false
**********************************************************************
                ls_rental_object-zzrecleaned = abap_false.
                ls_ci-ci_vibdro = CORRESPONDING #( ls_rental_object ).
**********************************************************************
*110: Fill bapi extension
**********************************************************************
                cl_abap_container_utilities=>fill_container_c(
                  EXPORTING
                    im_value               =  ls_ci
                  IMPORTING
                    ex_container           =  ls_extension_in-valuepart1
                  EXCEPTIONS
                    illegal_parameter_type = 1
                    OTHERS                 = 2
                ).
                IF sy-subrc IS INITIAL.
                  ls_extension_in-structure = `CI_DATA`.
                  lt_extension_in = VALUE #( ( ls_extension_in )   ).
                ENDIF.
**********************************************************************
*120: Call BAPI Rental Object change
**********************************************************************
                CALL FUNCTION 'BAPI_RE_RO_CHANGE'
                  EXPORTING
                    compcode             = ls_rental_object-bukrs
                    businessentitynumber = ls_rental_object-swenr
                    rentalobjectnumber   = ls_rental_object-smenr
                  TABLES
                    extension_in         = lt_extension_in
                    return               = lt_return.
**********************************************************************
*130: Commit changes
**********************************************************************
                IF xsdbool( line_exists(  lt_return[ type = 'S' id = 'BAPI' number = '002' ] ) )
                        EQ abap_true.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = 'X'.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR lf_obj_source.
          ENDLOOP.
**********************************************************************
*140: Exception check-in
**********************************************************************
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_contr_checked_in.
**********************************************************************
*150: Other exceptions
**********************************************************************
        CATCH zcx_refx_exception.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_contr_checked_in.
        CATCH cx_root INTO DATA(lo_root).
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              previous = lo_root.
      ENDTRY.
    ELSE.
**********************************************************************
*160: User cancelled exception
**********************************************************************
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_action_cancelled.
    ENDIF.
  ENDMETHOD.

  METHOD check_out.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA:lf_confirm       TYPE char1,
         ls_ci            TYPE rebd_rental_object_ci,
         lt_extension_in  TYPE TABLE OF bapiparex,
         lt_return        TYPE TABLE OF bapiret2,
         ls_extension_in  TYPE bapiparex,
         lf_to_be_cleaned TYPE char1.
**********************************************************************
*20: Confirm
**********************************************************************
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = `Check-Out Rental Object`
        text_question         = `Check-Out Rental Object(s) (Y/N)?`
        text_button_1         = `Yes`
        text_button_2         = `No`
        icon_button_1         = 'ICON_CHECKED'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lf_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = `Rental Object`
        text_question         = `Are the Rental Objects to be cleaned? (Y/N)?`
        text_button_1         = `Yes`
        text_button_2         = `No`
        icon_button_1         = 'ICON_CHECKED'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lf_to_be_cleaned.
**********************************************************************
*30: Check user action
**********************************************************************
    IF xsdbool( lf_confirm EQ '1' ) EQ abap_true.
      TRY.
**********************************************************************
*40: Process checkout for selected records
**********************************************************************
          LOOP AT it_selection ASSIGNING
              FIELD-SYMBOL(<fs_selection>).
            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
              EXPORTING
                percentage = sy-tabix
                text       = `Processing rental object check-out....`.
**********************************************************************
*50: Get contract ObjNumber
**********************************************************************
            SELECT SINGLE
            FROM
            vicncn
            FIELDS
            objnr
            WHERE recnnr EQ @<fs_selection>-contract_no
            INTO @DATA(lf_obj_source).
**********************************************************************
*60: Check-out
**********************************************************************
            IF xsdbool( lf_obj_source IS INITIAL ) EQ abap_false.
              UPDATE zvibdobjass
              SET zcheckin    = abap_false
                  zoccupied   = abap_false
                  zbooked     = abap_false
                  zcheckout   = abap_true
              WHERE objnrtrg  = <fs_selection>-obj_number
              AND   validfrom = <fs_selection>-valid_from
              AND   objnrsrc  = lf_obj_source.
              COMMIT WORK.
            ENDIF.
**********************************************************************
*70: Get rental object
**********************************************************************
            IF xsdbool( lf_to_be_cleaned EQ '1' )
                    EQ abap_true.
              lf_to_be_cleaned = 'X'.
              SELECT SINGLE
              FROM vibdro
              FIELDS *
              WHERE objnr EQ @lf_obj_source
              AND zzrecleaned EQ @space
              INTO @DATA(ls_rental_object).
**********************************************************************
*80: Check If rental object exists & Is an Actual. check in / check out
**********************************************************************
              IF xsdbool( ls_rental_object IS NOT INITIAL
                     AND ( p_r_cin EQ abap_true OR p_r_cout EQ abap_true ) )
                     EQ abap_true.
**********************************************************************
*90: Unset bapi fields
**********************************************************************
                CLEAR:
                        ls_ci, lt_extension_in, ls_extension_in, lt_return.
**********************************************************************
*100: Set to be cleaned as true
**********************************************************************
                ls_rental_object-zzrecleaned = lf_to_be_cleaned.
                ls_ci-ci_vibdro = CORRESPONDING #( ls_rental_object ).
**********************************************************************
*110: Fill bapi extension
**********************************************************************
                cl_abap_container_utilities=>fill_container_c(
                  EXPORTING
                    im_value               =  ls_ci
                  IMPORTING
                    ex_container           =  ls_extension_in-valuepart1
                  EXCEPTIONS
                    illegal_parameter_type = 1
                    OTHERS                 = 2
                ).
                IF sy-subrc IS INITIAL.
                  ls_extension_in-structure = `CI_DATA`.
                  lt_extension_in = VALUE #( ( ls_extension_in )   ).
                ENDIF.
**********************************************************************
*120: Call BAPI Change Rental Object
**********************************************************************
                CALL FUNCTION 'BAPI_RE_RO_CHANGE'
                  EXPORTING
                    compcode             = ls_rental_object-bukrs
                    businessentitynumber = ls_rental_object-swenr
                    rentalobjectnumber   = ls_rental_object-smenr
                  TABLES
                    extension_in         = lt_extension_in
                    return               = lt_return.
**********************************************************************
*130: Commit changes
**********************************************************************
                IF xsdbool( line_exists(  lt_return[ type = 'S' id = 'BAPI' number = '002' ] ) )
                        EQ abap_true.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = 'X'.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR: lf_obj_source.
          ENDLOOP.
**********************************************************************
*140: Exception check-out
**********************************************************************
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_contr_checked_out.
**********************************************************************
*150: Other exceptions
**********************************************************************
        CATCH zcx_refx_exception.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_contr_checked_out.
        CATCH cx_root INTO DATA(lo_root).
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              previous = lo_root.
      ENDTRY.
**********************************************************************
*160: User cancelled exception
**********************************************************************
    ELSE.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_action_cancelled.
    ENDIF.
  ENDMETHOD.

  METHOD retrieve_specific_set.
**********************************************************************
*10: Data Declaration
**********************************************************************
    TYPES: BEGIN OF t_s_clause,
             line(72) TYPE c,
           END OF t_s_clause.
    TYPES: t_hrcond     TYPE STANDARD TABLE OF hrcond WITH DEFAULT KEY.
    DATA: lt_sql_clause TYPE STANDARD TABLE OF t_s_clause WITH DEFAULT KEY.
    DATA: lt_conditions TYPE TABLE OF hrcond.
**********************************************************************
*20: Check that contract exists
**********************************************************************
    CLEAR: gt_output.
    SELECT SINGLE
      FROM vicncn
      FIELDS
      @abap_true
      WHERE recnnr = @if_contract_no
      AND   bukrs  = @p_comp
      AND   recntype = @if_contract_type
      INTO @DATA(lf_contract_exists).
**********************************************************************
*30: Raise exception
**********************************************************************
    IF lf_contract_exists EQ abap_false.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid             = zcx_refx_exception=>refx_cn_null_contract
          refx_cn_contract   = if_contract_no
          refx_cn_contr_type = if_contract_type.
    ENDIF.
**********************************************************************
*40: Create gen. where clause
**********************************************************************
    INSERT LINES OF VALUE t_hrcond(
        ( field = 'CN~RECNNR'    opera = 'EQ' low = if_contract_no )
        ( field = 'CN~BUKRS'     opera = 'EQ' low = p_comp )
        ( field = 'CN~RECNTYPE'  opera = 'EQ' low = if_contract_type )
     ) INTO TABLE lt_conditions.
**********************************************************************
*50: Create spec. where clause for checkin
**********************************************************************
    IF p_r_cin EQ abap_true.
      INSERT VALUE #( field = 'ZOBJ~ZCHECKIN' opera = 'EQ' low = abap_false  )
      INTO TABLE lt_conditions.
    ENDIF.
**********************************************************************
*60: Create spec. where clause for checkout
**********************************************************************
    IF p_r_cout EQ abap_true.
      INSERT VALUE #( field = 'ZOBJ~ZCHECKOUT' opera = 'EQ' low = abap_false  )
       INTO TABLE lt_conditions.
    ENDIF.
**********************************************************************
*70: Build sql clause
**********************************************************************
    CALL FUNCTION 'RH_DYNAMIC_WHERE_BUILD'
      EXPORTING
        dbtable         = space
      TABLES
        condtab         = lt_conditions
        where_clause    = lt_sql_clause
      EXCEPTIONS
        empty_condtab   = 01
        no_db_field     = 02
        unknown_db      = 03
        wrong_condition = 04.
**********************************************************************
*80: Sql query execution
**********************************************************************
    SELECT FROM
    vicncn AS cn
    INNER JOIN
    vibdobjass AS obj
    ON obj~objnrsrc =
    cn~objnr INNER JOIN
    zvibdobjass AS zobj
    ON obj~objnrsrc =
    zobj~objnrsrc AND
    obj~objasstype =
    zobj~objasstype AND
    obj~objnrtrg =
    zobj~objnrtrg AND
    obj~validfrom =
    zobj~validfrom
    FIELDS
    cn~recnnr       AS contract_no,
    cn~bukrs        AS comp_code,
    cn~recntype     AS contr_type,
    cn~recntxt      AS contract_txt,
    cn~recnbeg      AS cont_st_dat,
    cn~recnend1st   AS frst_end_dat,
    obj~objnrtrg    AS obj_number,
    obj~validfrom   AS valid_from,
    obj~validto     AS valid_to,
    zobj~zcheckin   AS zcheckin,
    zobj~zcheckout  AS zcheckout
    WHERE (lt_sql_clause)
    INTO CORRESPONDING FIELDS OF TABLE
    @gt_output.
**********************************************************************
*90: Check result for checkin
**********************************************************************
    IF xsdbool(  gt_output IS INITIAL AND if_check_in EQ abap_true )
            EQ abap_true.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_already_checked_in.
    ENDIF.
**********************************************************************
*100: Check result for checkout
**********************************************************************
    IF xsdbool(  gt_output IS INITIAL AND if_check_out EQ abap_true )
            EQ abap_true.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_already_checked_out.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
