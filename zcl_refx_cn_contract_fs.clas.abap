CLASS zcl_refx_cn_contract_fs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF t_layout_sheet,
             refx_cn_in TYPE zrefx_cn_inp_type,
             sheet_no   TYPE int4,
             start_row  TYPE numc2,
           END OF t_layout_sheet.
    TYPES: tt_layout_sheet TYPE TABLE OF t_layout_sheet WITH DEFAULT KEY.
    TYPES: BEGIN OF t_cont_head,
             id                   TYPE   zrefx_migration_id,
             old_contract_number  TYPE   vvosmive,
             comp_code            TYPE   bukrs,
             contract_type        TYPE   recncontracttype,
             contract             TYPE   recnnumber,
             contract_text        TYPE   recntxt,
             contract_start_date  TYPE   recncnbeg,
             first_end_date       TYPE   recncnend1st,
             possession_from      TYPE   rebdpossdatefrom,
             possession_to        TYPE   rebdpossdateto,
             frequency            TYPE   recdfrequency,
             frequency_unit       TYPE   recdfrequencyunit,
             condition_amount_ref TYPE   recdcondvaluerefer,
             payment_form         TYPE   recdpaymentformrh,
             pymt_meth            TYPE   rerapymtmeth,
             pmnttrms             TYPE   rerapymtterm,
             housebankid          TYPE   rerahousebkid,
             zzre_rate            TYPE   zzre_rate,
             currency_contract    TYPE   recnwaers_cn,
           END OF t_cont_head,
           BEGIN OF t_cont_cond,
             id                      TYPE zrefx_migration_id,
             condition_type          TYPE recdcondtype,
             calculation_object_type TYPE recdbusobjtypecalc,
             calculation_object_id   TYPE recdbusobjidcalc,
             unit_price              TYPE recdunitprice,
             calc_rule               TYPE recdcalcrule,
             calc_rule_parameter1    TYPE recdcalcrulepara,
             currency1               TYPE recdcondcurr,
             calc_rule_parameter2    TYPE recdcalcrulepara,
             currency2               TYPE recdcondcurr,
             external_purpose        TYPE recdcondpurposeext,
             valid_from              TYPE recdvalidfrom,
             valid_to                TYPE recdvalidto,
             dist_rule               TYPE recddistrule,
             role_type               TYPE rebprole,
             partner                 TYPE bu_partner,
             tax_group               TYPE rerataxgroup,
             guest                   TYPE zre_guest,
           END OF t_cont_cond,
           BEGIN OF t_cont_log,
             icon    TYPE char4,
             type    TYPE bapi_mtype,
             id      TYPE symsgid,
             num     TYPE symsgno,
             message TYPE bapi_msg,
           END OF t_cont_log,
           tt_cont_head TYPE TABLE OF t_cont_head WITH DEFAULT KEY,
           tt_cont_cond TYPE TABLE OF t_cont_cond WITH DEFAULT KEY,
           tt_cont_log  TYPE TABLE OF t_cont_log WITH DEFAULT KEY.

    CLASS-METHODS create_instance
      IMPORTING
                i_file_name     TYPE rlgrap-filename
                it_layout_sheet TYPE tt_layout_sheet
      RETURNING
                VALUE(r_result) TYPE REF TO zcl_refx_cn_contract_fs
      RAISING   zcx_refx_exception cx_fdt_excel_core.
    DATA: go_excel       TYPE REF TO cl_fdt_xl_spreadsheet READ-ONLY,
          gt_cn_layout   TYPE TABLE OF zrefx_cn_layout READ-ONLY,
          gt_filetable   TYPE filetable READ-ONLY,
          gt_cont_haeder TYPE tt_cont_head,
          gt_cont_cond   TYPE tt_cont_cond.

    METHODS: constructor IMPORTING it_layout_sheet TYPE tt_layout_sheet
                         RAISING   zcx_refx_exception,
      get_gf_xstring RETURNING VALUE(r_result) TYPE xstring,
      set_gf_xstring IMPORTING gf_xstring TYPE xstring,
      process_excel RAISING zcx_refx_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: import_file IMPORTING if_file_name TYPE rlgrap-filename OPTIONAL
                         RAISING   zcx_refx_exception,
             create_excel_ref IMPORTING if_file_name TYPE string if_xstring TYPE xstring
                        RETURNING VALUE(ro_excel) TYPE REF TO cl_fdt_xl_spreadsheet
                        RAISING cx_fdt_excel_core,
                        get_worksheets IMPORTING io_excel TYPE REF TO cl_fdt_xl_spreadsheet,
                get_cn_layout,
                check_cn_layout RAISING zcx_refx_exception,
                process_worksheet IMPORTING is_layout_sheet TYPE t_layout_sheet CHANGING ct_tab TYPE STANDARD TABLE RAISING zcx_refx_exception,
                get_column_count IMPORTING if_refx_cn_inp TYPE zrefx_cn_inp_type RETURNING VALUE(rf_count) TYPE int4.
    DATA: gf_start_row       TYPE num02,
          gf_file_name       TYPE rlgrap-filename,
          gf_xstring         TYPE xstring,
          gt_worksheet_names TYPE if_fdt_doc_spreadsheet=>t_worksheet_names,
          gt_layout_sheet    TYPE TABLE OF t_layout_sheet.

    CONSTANTS:
      gc_xls               TYPE string VALUE '.xls',
      gc_xlsx              TYPE string VALUE '.xlsx',
      gc_max_file_name_len TYPE i VALUE 128,
      gc_contr_head        TYPE string VALUE 'CONTR_HEAD',
      gc_contr_cond        TYPE string VALUE 'CONTR_COND'.


ENDCLASS.



CLASS zcl_refx_cn_contract_fs IMPLEMENTATION.

  METHOD constructor.
**********************************************************************
*10: Retrieve RE-FX CN Layout
**********************************************************************
    me->get_cn_layout(  ).
**********************************************************************
*20: Assign Layout sheet
**********************************************************************
    me->gt_layout_sheet = it_layout_sheet.
**********************************************************************
*30: Check: RE-FX CN Layout
**********************************************************************
    TRY.
        me->check_cn_layout( ).
**********************************************************************
*40: Catch errors
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.
  ENDMETHOD.

  METHOD create_instance.
**********************************************************************
*10: Check: Layout sheet exists
**********************************************************************
    IF it_layout_sheet IS INITIAL.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_null_layout.
    ENDIF.
**********************************************************************
*20: Create Instance
**********************************************************************
    r_result = NEW #( it_layout_sheet ).
**********************************************************************
*30: Assign file name
**********************************************************************
    r_result->gf_file_name = i_file_name.
**********************************************************************
*40: Import file?
**********************************************************************
    TRY.
        r_result->import_file(  ).
**********************************************************************
*50: Catch errors
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.
**********************************************************************
*60: Create excel?
**********************************************************************
    TRY.
        r_result->go_excel = r_result->create_excel_ref(
          if_file_name = CONV #( r_result->gf_file_name )
          if_xstring   = r_result->gf_xstring
        ).
**********************************************************************
*70: Catch errors
**********************************************************************
      CATCH cx_fdt_excel_core INTO DATA(lo_fdt_ex).
        RAISE EXCEPTION lo_fdt_ex.
    ENDTRY.
  ENDMETHOD.
  METHOD import_file.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA: lf_rc            TYPE i,
          lt_records       TYPE solix_tab,
          lf_headerxstring TYPE xstring,
          lf_filelength    TYPE i.
**********************************************************************
*20: Check:  file name is supplied?
**********************************************************************
    IF xsdbool( if_file_name IS INITIAL AND me->gf_file_name IS INITIAL ) EQ abap_true .
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_null_file.
    ENDIF.
**********************************************************************
*30: Retrieve file : Front-end services
**********************************************************************
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = |RE-FX Contract File Selection|
        initial_directory       = CONV #( COND #( WHEN me->gf_file_name IS NOT INITIAL THEN me->gf_file_name ELSE if_file_name ) )
      CHANGING
        file_table              = me->gt_filetable
        rc                      = lf_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
**********************************************************************
*40: Catch errors
**********************************************************************
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_front_end_error.
    ENDIF.
**********************************************************************
*50: Check: file name length
**********************************************************************
    IF line_exists(  me->gt_filetable[ 1 ] ).
      me->gf_file_name = me->gt_filetable[ 1 ].
      IF xsdbool( strlen( me->gt_filetable[ 1 ] ) GT gc_max_file_name_len ) EQ abap_true.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid = zcx_refx_exception=>refx_cn_file_path_error.
      ENDIF.
    ENDIF.
**********************************************************************
*60: Check: file type
**********************************************************************
    IF xsdbool(  contains( val = to_lower( me->gf_file_name )  pcre = `.xls|.xlsx`) ) EQ abap_false.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_invalid_fft.
    ENDIF.
**********************************************************************
*70: Call function: GUI_UPLOAD
**********************************************************************
    TRY.
        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
*           filename                = me->gf_file_name
            filename                = CONV string( me->gf_file_name )
            filetype                = 'BIN'
          IMPORTING
            filelength              = lf_filelength
            header                  = lf_headerxstring
          TABLES
            data_tab                = lt_records
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            OTHERS                  = 17.
        IF sy-subrc <> 0.
**********************************************************************
*80: Catch errors
**********************************************************************
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_cn_upload_error.
        ENDIF.
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_upload_error
            previous = lo_cx_root.
    ENDTRY.
**********************************************************************
*90: BIN to xstring
**********************************************************************
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lf_filelength
      IMPORTING
        buffer       = lf_headerxstring
      TABLES
        binary_tab   = lt_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
**********************************************************************
*100: Assign xstring
**********************************************************************
    me->set_gf_xstring( gf_xstring = lf_headerxstring  ).
  ENDMETHOD.

  METHOD get_gf_xstring.
**********************************************************************
*10: Retrieve xstring
**********************************************************************
    r_result = me->gf_xstring.
  ENDMETHOD.

  METHOD set_gf_xstring.
**********************************************************************
*10: Assign xstring
**********************************************************************
    me->gf_xstring = gf_xstring.
  ENDMETHOD.

  METHOD create_excel_ref.
**********************************************************************
*10: Create excel instance
**********************************************************************
    TRY.
        ro_excel = NEW cl_fdt_xl_spreadsheet(
          document_name = CONV #( me->gf_file_name )
          xdocument     = me->gf_xstring
        ).
      CATCH cx_fdt_excel_core INTO DATA(lo_fdt_ex).
        RAISE EXCEPTION lo_fdt_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD get_worksheets.
**********************************************************************
*10: Retrieve worksheet names
**********************************************************************
    io_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
       IMPORTING
         worksheet_names = me->gt_worksheet_names
     ).
  ENDMETHOD.

  METHOD get_cn_layout.
**********************************************************************
*10: Retrieve RE-FX CN Layout
**********************************************************************
    IF me->gt_cn_layout IS INITIAL.
      SELECT FROM
      zrefx_cn_layout
      FIELDS *
      WHERE status EQ @abap_true
      INTO TABLE @me->gt_cn_layout.
    ENDIF.
  ENDMETHOD.

  METHOD check_cn_layout.
**********************************************************************
*10: Check : Layout sheet exists
**********************************************************************
    IF me->gt_layout_sheet IS INITIAL.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_null_layout.
    ENDIF.
**********************************************************************
*20: Retrieve: worksheet
**********************************************************************
    LOOP AT me->gt_layout_sheet ASSIGNING FIELD-SYMBOL(<fs_layout>).
*      SELECT SINGLE
*      FROM zrefx_cn_layout
*      FIELDS @abap_true
*      WHERE refx_in_type EQ @<fs_layout>-refx_cn_in
*      INTO @DATA(lf_layout_exists).
      DATA(lf_layout_exists) = xsdbool( line_exists( me->gt_cn_layout[ refx_in_type = <fs_layout>-refx_cn_in ] ) ) .
**********************************************************************
*30: Check: Worksheet layout exists
**********************************************************************
      DATA lf_cn_out TYPE string.
      IF lf_layout_exists EQ abap_false.
        CALL FUNCTION 'CONVERSION_EXIT_ZCNIN_OUTPUT'
          EXPORTING
            input  = <fs_layout>-refx_cn_in
          IMPORTING
            output = lf_cn_out.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid         = zcx_refx_exception=>refx_cn_layout_not_found
            refx_cn_layout = |{ lf_cn_out ALPHA = OUT  }|.
      ENDIF.
**********************************************************************
*40: Check: Sheet No
**********************************************************************
      IF <fs_layout>-sheet_no IS INITIAL.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid         = zcx_refx_exception=>refx_cn_null_sheet_no
            refx_cn_layout = |{ <fs_layout>-refx_cn_in ALPHA = OUT  }|.
      ENDIF.
      lf_layout_exists = abap_false.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_excel.
    TRY.
**********************************************************************
*10: Retrieve Worksheets
**********************************************************************
        me->get_worksheets( io_excel = me->go_excel ).
**********************************************************************
*20: Check: worksheets exists
**********************************************************************
        IF me->gt_worksheet_names IS INITIAL.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_cn_null_worksheet.
        ENDIF.
**********************************************************************
*30: Check Layout sheet exists
**********************************************************************
        IF me->gt_layout_sheet IS INITIAL.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid = zcx_refx_exception=>refx_cn_null_layout.
        ENDIF.
**********************************************************************
*40: Check worksheet/layout
**********************************************************************
        LOOP AT me->gt_layout_sheet ASSIGNING FIELD-SYMBOL(<fs_layout_sheet>).
          CASE <fs_layout_sheet>-refx_cn_in.
**********************************************************************
*50: Process Contr.Head
**********************************************************************
            WHEN gc_contr_head.
              me->process_worksheet(
                  EXPORTING
                    is_layout_sheet = <fs_layout_sheet>
                  CHANGING
                    ct_tab = gt_cont_haeder  ).
**********************************************************************
*60: Process Contr.Cond
**********************************************************************
            WHEN gc_contr_cond.
              me->process_worksheet(
                 EXPORTING
                    is_layout_sheet = <fs_layout_sheet>
                 CHANGING
                    ct_tab = gt_cont_cond  ).
          ENDCASE.
        ENDLOOP.
**********************************************************************
*70: Catch errors
**********************************************************************
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.

  ENDMETHOD.

  METHOD process_worksheet.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lf_column_count TYPE int4.
    FIELD-SYMBOLS: <gt_data> TYPE STANDARD TABLE.
    DATA: lfc_day(2)   TYPE n,
          lfc_month(2) TYPE n,
          lfc_year(4)  TYPE n.
**********************************************************************
*20: Retrieve worksheet
**********************************************************************
    DATA(lf_worksheet) = me->gt_worksheet_names[ is_layout_sheet-sheet_no ].
**********************************************************************
*30: Retrieve worksheet data ref
**********************************************************************
    DATA(lo_data_ref) = me->go_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lf_worksheet ).
**********************************************************************
*40: Assign worksheet data ref
**********************************************************************
    ASSIGN lo_data_ref->* TO <gt_data>.
**********************************************************************
*50: Unset data tab
**********************************************************************
    CLEAR ct_tab.
**********************************************************************
*60: Check: Data exists?
**********************************************************************
    IF <gt_data> IS ASSIGNED.
**********************************************************************
*70: Get column count
**********************************************************************
      lf_column_count = get_column_count( is_layout_sheet-refx_cn_in ).
      IF lf_column_count IS INITIAL.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid = zcx_refx_exception=>refx_cn_null_column.
      ENDIF.
**********************************************************************
*80: Read: worksheet from start row
**********************************************************************
      LOOP AT <gt_data> ASSIGNING FIELD-SYMBOL(<fs_data>)
      FROM is_layout_sheet-start_row.
**********************************************************************
*90: Insert records
**********************************************************************
        APPEND INITIAL LINE TO ct_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
**********************************************************************
*100: Read: worksheet columns usin sy-index
**********************************************************************
        DO lf_column_count TIMES.
          ASSIGN COMPONENT sy-index
            OF STRUCTURE <fs_data>
                TO FIELD-SYMBOL(<fs_field>).
**********************************************************************
*110: Retrieve layout field
**********************************************************************
          ASSIGN COMPONENT me->gt_cn_layout[ refx_in_type = is_layout_sheet-refx_cn_in fieldposition = sy-index ]-fieldname
              OF STRUCTURE <fs_tab>
                  TO FIELD-SYMBOL(<fs_value>).
          TRY.
**********************************************************************
*120: Process: field data
**********************************************************************
              CASE me->gt_cn_layout[ refx_in_type = is_layout_sheet-refx_cn_in fieldposition = sy-index ]-fieldname.
                WHEN 'CONTRACT_START_DATE' OR 'FIRST_END_DATE'
                OR   'POSSESSION_FROM'     OR 'POSSESSION_TO'
                OR   'VALID_FROM'          OR 'VALID_TO'.
                  IF match( val = <fs_field> regex = '\d{2}\.\d{2}\.\d{4}$' ) EQ <fs_field>.
                    SPLIT <fs_field> AT `.` INTO DATA(lf_day) DATA(lf_month) DATA(lf_year).
                    lfc_month   = lf_month.
                    lfc_day     = lf_day.
                    lfc_year    = lf_year.
                    <fs_value>  = lfc_year && lfc_month && lfc_day.
                  ELSEIF match( val = <fs_field> regex = '\d{2}\-\d{2}\-\d{4}$' ) EQ <fs_field>.
                    SPLIT <fs_field> AT `.` INTO  lf_day lf_month lf_year.
                    lfc_month   = lf_month.
                    lfc_day     = lf_day.
                    lfc_year    = lf_year.
                    <fs_value>  = lfc_year && lfc_month && lfc_day.
                  ELSE.
                    REPLACE ALL OCCURRENCES OF
                    `-` IN <fs_field> WITH space.
                    CONDENSE <fs_field>.
                    <fs_value> = <fs_field>.
                  ENDIF.
                 WHEN 'CALC_RULE' or 'TAX_GROUP' or 'DIST_RULE'.
                    <fs_value>(2) = <fs_field>.
                 WHEN 'CURRENCY1' or 'CURRENCY2'.
                    <fs_value>(3) = <fs_field>.
                 WHEN 'EXTERNAL_PURPOSE'.
                    <fs_value>(1) = <fs_field>.
                WHEN OTHERS.
                  <fs_value> = <fs_field>.
              ENDCASE.
**********************************************************************
*130: Catch conversion errors
**********************************************************************
            CATCH cx_sy_conversion_error INTO DATA(lo_sy_conv).
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  previous            = lo_sy_conv
                  textid              = zcx_refx_exception=>refx_cn_invalid_typ_conv
                  refx_cn_field_name  = CONV #( me->gt_cn_layout[ fieldposition = sy-index ]-fieldname )
                  refx_cn_field_value = <fs_field>.
**********************************************************************
*140: Catch other errors
**********************************************************************
            CATCH zcx_refx_exception.
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  textid              = zcx_refx_exception=>refx_cn_invalid_typ_conv
                  refx_cn_field_name  = CONV #( me->gt_cn_layout[ fieldposition = sy-index ]-fieldname )
                  refx_cn_field_value = <fs_field>.
**********************************************************************
*150: Catch general errors
**********************************************************************
            CATCH cx_root INTO DATA(lo_cx_root).
              RAISE EXCEPTION TYPE zcx_refx_exception
                EXPORTING
                  previous            = lo_cx_root
                  textid              = zcx_refx_exception=>refx_cn_invalid_typ_conv
                  refx_cn_field_name  = CONV #( me->gt_cn_layout[ fieldposition = sy-index ]-fieldname )
                  refx_cn_field_value = <fs_field>.
          ENDTRY.
        ENDDO.
      ENDLOOP.
    ELSE.
**********************************************************************
*160: Raise null data error
**********************************************************************
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid = zcx_refx_exception=>refx_cn_null_data.
    ENDIF.
  ENDMETHOD.

  METHOD get_column_count.
**********************************************************************
*10: Retrieve column count
**********************************************************************
    rf_count =  REDUCE #( INIT i = 0
                        FOR <fs> IN me->gt_cn_layout
                        WHERE
                        ( refx_in_type EQ if_refx_cn_inp )
                        NEXT i = i + 1 ).
  ENDMETHOD.

ENDCLASS.
