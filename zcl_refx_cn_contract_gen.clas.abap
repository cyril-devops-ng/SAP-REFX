CLASS zcl_refx_cn_contract_gen DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES t_c_rpt_key TYPE TABLE OF zrefx_c_rpt_key WITH DEFAULT KEY.
    TYPES t_c_rpt_fld TYPE TABLE OF zrefx_c_rpt_fld WITH DEFAULT KEY.
    TYPES: BEGIN OF t_selection_apartments,
             CompanyCode    TYPE bukrs,
             RentalObject   type REBDRONO,
             objectnumber  TYPE recaobjnr,
             apartmentname TYPE rebdxro,
             guest type zvibdobjass-ztext,
             campname      TYPE rebdxbe,
             checkindate   TYPE rebdrelvalidfrom,
             checkoutdate  TYPE rebdrelvalidto,
             ContractNo type recnnumber,
           END OF t_selection_apartments.
    types t_r_recnnumber type range of RECNNUMBER.
    CLASS-METHODS report_customizing
      IMPORTING if_data_structure TYPE c
                if_report         TYPE progname
                if_tcode          TYPE tcode
      CHANGING  ct_data           TYPE INDEX TABLE.

    CLASS-METHODS get_config_data
      IMPORTING if_report TYPE progname
                if_tcode  TYPE tcode
                if_struct TYPE c.

    CLASS-METHODS extend_field_catalog IMPORTING if_data_structure TYPE c
                                       CHANGING  ct_fieldcat       TYPE lvc_t_fcat.

    CLASS-METHODS extend_field_catalog_a410 IMPORTING if_data_structure TYPE c
                                            CHANGING  ct_fieldcat       TYPE lvc_t_fcat.

    CLASS-METHODS recpa_print_log IMPORTING is_recpa_doc TYPE zrecpa_print_log
                                            if_reprint   TYPE flag

                                  RAISING   zcx_refx_exception.

    CLASS-METHODS report_customizing_A520
      IMPORTING if_data_structure TYPE c
                if_report         TYPE progname
                if_tcode          TYPE tcode
      CHANGING  ct_data           TYPE INDEX TABLE.

    CLASS-METHODS report_customizing_y410
      IMPORTING it_rental_obj TYPE ANY TABLE
      CHANGING  ct_data       TYPE INDEX TABLE.

    CLASS-METHODS output_customizing_y410
      IMPORTING it_rental_obj TYPE ANY TABLE
      CHANGING  ct_object     TYPE zrecp_t_object_c.

    class-methods ____f4_aprtments
    importing ir_recnnumber type t_r_recnnumber optional
    returning value(rt_apartments) type hrreturn_tab.
    CLASS-DATA gs_config_data TYPE zrefx_c_rpt_conf.
    CLASS-DATA gt_config_keys TYPE t_c_rpt_key.
    CLASS-DATA gt_config_flds TYPE t_c_rpt_fld.
    CLASS-DATA gt_cnoa        TYPE TABLE OF reis_cn_oa_l.

    CONSTANTS gc_a520_RPT   TYPE progname  VALUE 'RFRECPSFA520'.
    CONSTANTS gc_z410_RPT   TYPE progname  VALUE 'ZRFRECPSFA410'.

    CONSTANTS gc_invguid    TYPE fieldname VALUE 'INVGUID_SP'.
    CONSTANTS gc_ksl_curr   TYPE fieldname VALUE 'ZZ_KSL_CURR'.
    CONSTANTS gc_ksl        TYPE fieldname VALUE 'ZZ_KSL'.
    CONSTANTS gc_fccurrkey  TYPE fieldname VALUE 'ZZ_FCCURRKEY'.
    CONSTANTS gc_ctrate     TYPE fieldname VALUE 'ZZ_CTRATE'.
    CONSTANTS gc_fcgrossamt TYPE fieldname VALUE 'ZZ_FCGROSSAMOUNT'.
    CONSTANTS gc_fcnetamt   TYPE fieldname VALUE 'ZZ_FCNETAMOUNT'.
    CONSTANTS gc_fctaxamt   TYPE fieldname VALUE 'ZZ_FCTAXAMOUNT'.
    CONSTANTS gc_lc_curr    TYPE waers     VALUE 'NGN'.
    CONSTANTS gc_fc_curr    TYPE waers     VALUE 'USD'.
    CONSTANTS gc_revenue_gl TYPE hkont     VALUE '0012101000'.
    CONSTANTS gc_y410_app   TYPE recpcpapp VALUE 'Y410'.
    CONSTANTS gc_z410_app   TYPE recpcpapp VALUE 'Z410'.
    CONSTANTS gc_a410_app   TYPE recpcpapp VALUE 'A410'.
    CONSTANTS gc_a520_app   TYPE recpcpapp VALUE 'A520'.
ENDCLASS.


CLASS zcl_refx_cn_contract_gen IMPLEMENTATION.
  METHOD report_customizing.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA dref  TYPE REF TO data.
    DATA keyid TYPE int4.
    FIELD-SYMBOLS <fs_fields> TYPE any.

    " ---------------------------------------------------------------------
    " 20: Define up to 9 key fields
    " ---------------------------------------------------------------------
    key_definition.
    " ---------------------------------------------------------------------
    " 30: Get configuration data
    " ---------------------------------------------------------------------
    IF xsdbool(  gs_config_data IS INITIAL )
       = abap_false.
      RETURN.
    ENDIF.

    get_config_data( if_report = if_report
                     if_tcode  = if_tcode
                     if_struct = if_data_structure ).
    " ---------------------------------------------------------------------
    " 40: Check configuration data exists
    " ---------------------------------------------------------------------
    IF xsdbool( gs_config_data IS NOT INITIAL
            AND gt_config_keys IS NOT INITIAL )
       = abap_false.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Assign report data
    " ---------------------------------------------------------------------
    CASE if_data_structure.
        " ---------------------------------------------------------------------
        " 60: REIS_CN_OA Implementation
        " ---------------------------------------------------------------------
      WHEN `REIS_CN_OA_L`.
        " ---------------------------------------------------------------------
        " 70: Corresponding Data
        " ---------------------------------------------------------------------
        gt_cnoa = CORRESPONDING #( ct_data ).
        " ---------------------------------------------------------------------
        " 80: Create fields reference
        " ---------------------------------------------------------------------
        CREATE DATA dref LIKE gt_cnoa.
        ASSIGN dref->* TO <fs_fields>.
        " ---------------------------------------------------------------------
        " 90: Assign key fields
        " ---------------------------------------------------------------------
        DO 9 TIMES.
          keyid = sy-index.
          IF line_exists( gt_config_keys[ keyid ] ).
            ASSIGN gt_config_keys[ keyid ] TO FIELD-SYMBOL(<fs_keys>).
            ASSIGN COMPONENT <fs_keys>-fieldname OF STRUCTURE <fs_fields>
                   TO FIELD-SYMBOL(<fs_comp>).
            key_assignment.
            UNASSIGN <fs_comp>.
          ENDIF.
        ENDDO.
        " ---------------------------------------------------------------------
        " 100: Sort records
        " ---------------------------------------------------------------------
        sort_records gt_cnoa.
        " ---------------------------------------------------------------------
        " 110: Sort data
        " ---------------------------------------------------------------------
        remove_dup_records gt_cnoa.
        " ---------------------------------------------------------------------
        " 120: Assign change records
        " ---------------------------------------------------------------------
        CLEAR ct_data.
        ct_data = CORRESPONDING #( gt_cnoa ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_config_data.
    " ---------------------------------------------------------------------
    " 10: Get report configuration data
    " *********************************************************************
    SELECT SINGLE FROM zrefx_c_rpt_conf
      FIELDS *
      WHERE tcode         = @if_tcode
        AND report        = @if_report
        AND rel_date_flag = @abap_true
      INTO @gs_config_data.
    " ---------------------------------------------------------------------
    " 20: If data exists get configuration keys
    " ---------------------------------------------------------------------
    IF xsdbool( gs_config_data IS NOT INITIAL )
       = abap_false.
      RETURN.
    ENDIF.

    SELECT FROM zrefx_c_rpt_key
      FIELDS *
      WHERE report  = @gs_config_data-report
        AND tcode   = @gs_config_data-tcode
        AND tabname = @if_struct
        AND aktiv   = @abap_true
      INTO TABLE @gt_config_keys.
    " ---------------------------------------------------------------------
    " 30: Get configuration field labels
    " ---------------------------------------------------------------------
    SELECT FROM zrefx_c_rpt_fld
      FIELDS *
      WHERE report  = @gs_config_data-report
        AND tcode   = @gs_config_data-tcode
        AND tabname = @if_struct
        AND aktiv   = @abap_true
      INTO TABLE @gt_config_flds.
  ENDMETHOD.

  METHOD extend_field_catalog.
*    BREAK csayeh.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA dref   TYPE REF TO data.
    FIELD-SYMBOLS <fs_col_pos> TYPE lvc_s_fcat-col_pos.
    DATA lf_f_prefix TYPE string VALUE 'ZZ_'.
    DATA lt_fieldcat TYPE lvc_t_fcat.
    " ---------------------------------------------------------------------
    " 20: Get configuration
    " ---------------------------------------------------------------------
    get_config_data( if_report = gc_a520_rpt
                     if_tcode  = cl_abap_syst=>get_transaction_code( )
                     if_struct = if_data_structure ).
    " ---------------------------------------------------------------------
    " 20: Create Data ref object
    " ---------------------------------------------------------------------
    CREATE DATA dref TYPE (if_data_structure).
    " ---------------------------------------------------------------------
    " 30: Assign component structure
    " ---------------------------------------------------------------------
    lo_str ?= cl_abap_structdescr=>describe_by_data_ref( dref ).
    " ---------------------------------------------------------------------
    " 40: Get Max. Col. position
    " ---------------------------------------------------------------------
    lt_fieldcat = CORRESPONDING #( ct_fieldcat ).
    SORT lt_fieldcat BY col_pos DESCENDING.
    IF xsdbool( line_exists( lt_fieldcat[ 1 ] ) ) = abap_true.
      ASSIGN lt_fieldcat[ 1 ]-col_pos TO <fs_col_pos>.
    ENDIF.
    IF <fs_col_pos> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Create fcat for ZZ_ fields
    " ---------------------------------------------------------------------
    DO lines( lo_str->components ) TIMES.
      ASSIGN lo_str->components[ sy-index ]-name TO FIELD-SYMBOL(<fs_fname>).
      IF xsdbool( contains( val = <fs_fname>
                            sub = lf_f_prefix
                            off = 0
                            occ = 1 ) )
         = abap_false  OR
         xsdbool( line_exists( ct_fieldcat[ fieldname = <fs_fname> ] ) ) eq abap_true.
        CONTINUE.
      ENDIF.

      <fs_col_pos> += 1.
      INSERT VALUE lvc_s_fcat( fieldname = <fs_fname>
                               tabname   = '1'
                               col_pos   = <fs_col_pos>
                               ref_table = if_data_structure
                               ref_field = <fs_fname> )
             INTO TABLE ct_fieldcat.
    ENDDO.
    " ---------------------------------------------------------------------
    " 60: Field catalog customizing
    " ---------------------------------------------------------------------
    DO lines( lo_str->components ) TIMES.
      ASSIGN lo_str->components[ sy-index ]-name TO <fs_fname>.
      IF xsdbool( line_exists( gt_config_flds[ tabname   = if_data_structure
                                               fieldname = <fs_fname>  ] ) )
         = abap_false.
        CONTINUE.
      ENDIF.

      ASSIGN ct_fieldcat[ fieldname = <fs_fname> ] TO FIELD-SYMBOL(<fs_fieldcatalog>).
      ASSIGN gt_config_flds[ tabname   = if_data_structure
                             fieldname = <fs_fname>  ] TO FIELD-SYMBOL(<fs_cust_fcat>).
      <fs_fieldcatalog> = CORRESPONDING #( BASE ( <fs_fieldcatalog> ) <fs_cust_fcat>
                                  MAPPING scrtext_m = scrtext_m
                                          scrtext_l = scrtext_l
                                          scrtext_s = scrtext_s
                                          reptext   = scrtext_l
                                  EXCEPT tabname ).
      UNASSIGN <fs_fieldcatalog>.
      UNASSIGN <fs_cust_fcat>.
    ENDDO.
  ENDMETHOD.

  METHOD report_customizing_a520.
    " TODO: parameter IF_REPORT is never used (ABAP cleaner)

    " ---------------------------------------------------------------------
    " 10: Data
    " *********************************************************************
    FIELD-SYMBOLS <fs_data> TYPE any.
    DATA dref       TYPE REF TO data.
    DATA tr_invguid TYPE RANGE OF rerainvguid.
    TYPES t_invguid TYPE RANGE OF rerainvguid.
    DATA lt_be   TYPE TABLE OF vibdbe.
    DATA lt_data TYPE TABLE OF recp_sf_doc_a520.

    " ---------------------------------------------------------------------
    " 20: Create Data Ref.
    " *********************************************************************
    CREATE DATA dref TYPE TABLE OF (if_data_structure).
    ASSIGN dref->* TO <fs_data>.
    " ---------------------------------------------------------------------
    " 30: Check
    " ---------------------------------------------------------------------
    IF xsdbool( contains( val = if_tcode
                          sub = 'ZZ'
                          off = 0
                          occ = 1 ) ) = abap_false.
      RETURN.
    ENDIF.
    <fs_data> = CORRESPONDING #( ct_data ).
    " ---------------------------------------------------------------------
    " 40: Retrieve custom data
    " ---------------------------------------------------------------------
    lt_data = CORRESPONDING #( ct_data ).
    tr_invguid = VALUE #( FOR <fs> IN lt_data
                          ( low    = <fs>-invguid_sp
                            sign   = 'I'
                            option = 'EQ' ) ).
    SELECT
      FROM virainvitem
             INNER JOIN
               virainv ON virainvitem~invguid =
                          virainv~invguid
                 INNER JOIN
                   vicncn ON virainvitem~objnr
                             = vicncn~objnr
                     INNER JOIN
                       vibdbe AS be ON  be~bukrs =
                                        vicncn~bukrs
                                    AND be~swenr
                                        = vicncn~benocn
      FIELDS virainv~invguid                       AS invguid_sp,
             MAX( pstngdate )                      AS zz_pstngdate,
             MAX( docdate )                        AS zz_docdate,
             MAX( refdocid )                       AS zz_refdocid,
             MAX( fccurrkey )                      AS zz_fccurrkey,
             MAX( ctrate )                         AS zz_ctrate,
             MAX( ctrule )                         AS zz_ctrule,
             SUM( fcgrossamount )                  AS zz_fcgrossamount,
             SUM( fcnetamount )                    AS zz_fcnetamount,
             SUM( fctaxamount )                    AS zz_fctaxamount,
             CASE
             WHEN    vicncn~recntype = 'A001'
                  OR vicncn~recntype = 'A002'
                  OR vicncn~recntype = 'A003'
                  OR vicncn~recntype = 'A004'
                 THEN 'CAMP'
             WHEN vicncn~recntype = 'A101'
                 THEN 'PORT'
             WHEN     vicncn~recntype = 'A201'
                  AND
                      (    vicncn~benocn = 'ACCP'
                        OR vicncn~benocn = 'ARE'
                        OR vicncn~benocn = 'HPE'
                        OR vicncn~benocn = 'ONE'
                        OR vicncn~benocn = 'WAE' )
                  THEN 'CAMP'
          WHEN     vicncn~recntype = 'A201'
               AND
                   (    vicncn~benocn = 'CALP'
                     OR vicncn~benocn = 'ONP'
                     OR vicncn~benocn = 'WAP'
                     OR vicncn~benocn = 'LOS' )
                 THEN 'PORT'
          END                                      AS zz_bgroup,
             be~xwetext                            AS zz_xwetext

      WHERE virainvitem~invguid IN
            @tr_invguid
      GROUP BY virainv~invguid,
               recntype,
               benocn,
               be~xwetext
    UNION
    SELECT
      FROM virainvitem
             INNER JOIN
               virainv ON virainvitem~invguid =
                          virainv~invguid
                 INNER JOIN
                   vicncn ON virainvitem~objnr
                             = vicncn~objnr
      FIELDS virainv~invguid                         AS invguid_sp,
             MAX( pstngdate )                        AS zz_pstngdate,
             MAX( docdate )                          AS zz_docdate,
             MAX( refdocid )                         AS zz_refdocid,
             MAX( fccurrkey )                        AS zz_fccurrkey,
             MAX( ctrate )                           AS zz_ctrate,
             MAX( ctrule )                           AS zz_ctrule,
             SUM( fcgrossamount )                    AS zz_fcgrossamount,
             SUM( fcnetamount )                      AS zz_fcnetamount,
             SUM( fctaxamount )                      AS zz_fctaxamount,
             CASE
             WHEN    vicncn~recntype = 'A001'
                  OR vicncn~recntype = 'A002'
                  OR vicncn~recntype = 'A003'
                  OR vicncn~recntype = 'A004'
                 THEN 'CAMP'
             WHEN vicncn~recntype = 'A101'
                 THEN 'PORT'
             WHEN     vicncn~recntype = 'A201'
                  AND
                      (    substring( virainvitem~objnrcalc  , 7 , 8 ) = 'ACCP'
                        OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'ARE'
                        OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'HPE'
                        OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'ONE'
                        OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'WAE' )
                  THEN 'CAMP'
          WHEN     vicncn~recntype = 'A201'
               AND
                   (    substring( virainvitem~objnrcalc  , 7 , 8 ) = 'CALP'
                     OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'ONP'
                     OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'WAP'
                     OR substring( virainvitem~objnrcalc  , 7 , 8 ) = 'LOS' )
                 THEN 'PORT'
          END                                                                    AS zz_bgroup,
             substring( virainvitem~objnrcalc  , 7 , 8 )                         AS zz_xwetext

      WHERE virainvitem~invguid IN
            @tr_invguid
        AND vicncn~benocn = @space
      GROUP BY virainv~invguid,
               recntype,
               virainvitem~objnrcalc

    INTO TABLE @DATA(lt_a520).
    " ---------------------------------------------------------------------
    " 40.1 Assign B.Entity for contracts without BE in BENOCN field
    " *********************************************************************
    SELECT FROM vibdbe
      FIELDS *
      INTO TABLE
      @lt_be.

    DATA lt_a520_bi LIKE lt_a520.

    LOOP AT lt_a520 ASSIGNING FIELD-SYMBOL(<f_520>).
      IF line_exists( lt_be[ swenr = <f_520>-zz_xwetext ] ).
        ASSIGN lt_be[ swenr = <f_520>-zz_xwetext ]-xwetext
               TO FIELD-SYMBOL(<fs_b_Entity>).
        <f_520>-zz_xwetext = <fs_b_entity>.
        UNASSIGN <fs_b_entity>.
      ENDIF.
    ENDLOOP.
    " ---------------------------------------------------------------------
    " 40.2: Cases of invoices with multiple rental objects
    " ---------------------------------------------------------------------
    lt_a520_bi = CORRESPONDING #( lt_a520 ).
    SORT lt_a520_bi BY invguid_sp.
    DELETE ADJACENT DUPLICATES FROM lt_a520_bi COMPARING invguid_sp.

    LOOP AT lt_a520_bi ASSIGNING FIELD-SYMBOL(<fs_a520_bi>).
      <fs_a520_bi>-zz_fcgrossamount = REDUCE recactfcgrossamount( INIT val TYPE recactfcgrossamount
                                       FOR <fs_a520> IN
                                       lt_a520 WHERE (
                                                     invguid_sp = <fs_a520_bi>-invguid_sp )
                                       NEXT val = val + <fs_a520>-zz_fcgrossamount ).
      <fs_a520_bi>-zz_fcnetamount   = REDUCE recactfcnetamount( INIT val TYPE recactfcnetamount
                                         FOR <fs_a520> IN
                                         lt_a520 WHERE (
                                                       invguid_sp = <fs_a520_bi>-invguid_sp )
                                         NEXT val = val + <fs_a520>-zz_fcnetamount ).
      <fs_a520_bi>-zz_fctaxamount   = REDUCE recactfctaxamount( INIT val TYPE recactfctaxamount
                                         FOR <fs_a520> IN
                                         lt_a520 WHERE (
                                                       invguid_sp = <fs_a520_bi>-invguid_sp )
                                         NEXT val = val + <fs_a520>-zz_fctaxamount ).
    ENDLOOP.

    " ---------------------------------------------------------------------
    " 50: Create FI Keys
    " ---------------------------------------------------------------------
    DELETE tr_invguid WHERE low
           NOT IN VALUE t_invguid( FOR <f> IN lt_a520
                                   ( low    = <f>-invguid_sp
                                     sign   = 'I'
                                     option = 'EQ' ) ).
    TYPES: BEGIN OF t_fi_key,
             invguid_sp TYPE rerainvguid,
             bukrs      TYPE bukrs,
             gjahr      TYPE gjahr,
             belnr      TYPE belnr_d,
             hkont      TYPE hkont,
             hwaer      TYPE bkpf-hwaer,
             hwae2      TYPE bkpf-hwae2,
             kuty2      TYPE bkpf-kuty2,
             budat      TYPE bkpf-budat,
           END OF t_fi_key.
    TYPES tt_fi_key TYPE TABLE OF t_fi_key WITH DEFAULT KEY.
    " ---------------------------------------------------------------------
    " 60: Get FI data
    " ---------------------------------------------------------------------
    SELECT
      FROM virainvitem
             INNER JOIN
               virainv ON virainv~invguid
                          = virainvitem~invguid
                 INNER JOIN
                   bseg ON
    bseg~bukrs = virainv~bukrs
    AND bseg~belnr = virainvitem~refdocid
    AND bseg~gjahr =  CAST( substring( virainvitem~pstngdate , 1 , 4 ) AS NUMC )
    AND bseg~hkont = @gc_revenue_gl
                     INNER JOIN
                       bkpf ON  bkpf~belnr = bseg~belnr
                            AND bkpf~gjahr = bseg~gjahr
                            AND bkpf~bukrs = bseg~bukrs
      FIELDS virainv~bukrs                                              AS bukrs,
             CAST( substring( virainvitem~pstngdate , 1 , 4 ) AS NUMC ) AS gjahr,
             virainvitem~invguid                                        AS invguid_sp,
             virainvitem~refdocid                                       AS belnr,
             bseg~buzei                                                 AS item,
             @gc_revenue_gl                                             AS hkont,
             bseg~dmbe2                                                 AS zz_ksl,
             bkpf~hwaer                                                 AS hwaer,
             bkpf~hwae2                                                 AS hwae2,
             bkpf~kuty2                                                 AS kuty2,
             bkpf~budat                                                 AS budat
      WHERE virainvitem~invguid IN @tr_invguid
      INTO TABLE @DATA(lt_fi_key2).
    " ---------------------------------------------------------------------
    " 70: Sort FI Data
    " ---------------------------------------------------------------------
    SORT lt_fi_key2 BY bukrs
                       belnr
                       item
                       gjahr
                       invguid_sp.
    DELETE ADJACENT DUPLICATES FROM lt_fi_key2 COMPARING bukrs belnr item gjahr invguid_sp.
    " ---------------------------------------------------------------------
    " 80: Create FI data per invoice number
    " ---------------------------------------------------------------------
    DATA lt_fi_key3 LIKE lt_fi_key2.
    lt_fi_key3 = CORRESPONDING #( lt_fi_key2 ).

    SORT lt_fi_key3 BY invguid_sp.
    DELETE ADJACENT DUPLICATES FROM lt_fi_key3 COMPARING invguid_sp.

    lt_a520 = CORRESPONDING #( lt_a520_bi ).

    LOOP AT lt_fi_key3 ASSIGNING FIELD-SYMBOL(<FS_fi_key3>).
      <fs_fi_key3>-zz_ksl = REDUCE dmbe2( INIT val3 TYPE dmbe2
                                          FOR <fs_ksl> IN
                                       lt_fi_key2 WHERE (
                                                        invguid_sp = <fs_fi_key3>-invguid_sp )
                                       NEXT val3 = val3 + <fs_ksl>-zz_ksl ).
    ENDLOOP.
    " ---------------------------------------------------------------------
    " 90: Assign custom fields
    " *********************************************************************
    LOOP AT <fs_data> ASSIGNING FIELD-SYMBOL(<f_data>).
      ASSIGN COMPONENT gc_invguid  OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_invguid>).
      ASSIGN COMPONENT gc_ksl_curr OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_curr>).
      ASSIGN COMPONENT gc_ksl      OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_f_ksl>).

      IF NOT line_exists( lt_a520[ invguid_sp = <fs_invguid> ] ).
        CONTINUE.
      ENDIF.
      ASSIGN lt_a520[ invguid_sp = <fs_invguid> ] TO FIELD-SYMBOL(<fs_aa520>).
      <f_data> = CORRESPONDING #( BASE ( <f_data> ) <fs_aa520> ).
      IF line_exists( lt_fi_key3[ invguid_sp = <fs_invguid> ] ).
        <fs_f_ksl> = lt_fi_key3[ invguid_sp = <fs_invguid> ]-zz_ksl.

      ENDIF.
      <fs_curr> = gc_fc_curr.
      " ---------------------------------------------------------------------
      " 100: Help Translate LC amount to FC amount
      " ---------------------------------------------------------------------
      ASSIGN COMPONENT gc_fccurrkey  OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_trans_curr>).
      ASSIGN COMPONENT gc_ctrate     OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_exch_rate>).
      ASSIGN COMPONENT gc_fcgrossamt OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_gross_fc>).
      ASSIGN COMPONENT gc_fcnetamt   OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_net_fc>).
      ASSIGN COMPONENT gc_fctaxamt   OF STRUCTURE <f_data> TO FIELD-SYMBOL(<fs_tax_fc>).
      " ---------------------------------------------------------------------
      " 110: If trans. curr is NGN and no exchange rate
      " ---------------------------------------------------------------------
      IF xsdbool( <fs_trans_curr> IS NOT ASSIGNED AND <fs_exch_rate> IS NOT ASSIGNED )
         EQ abap_true.
        CONTINUE.
      ENDIF.
      IF xsdbool( <fs_trans_curr> = gc_lc_curr AND <fs_exch_rate> IS INITIAL )
         = abap_true.
        ASSIGN lt_fi_key3[ invguid_sp = <fs_invguid> ] TO FIELD-SYMBOL(<fi_data>).
        convert_to_fc <fs_gross_fc>.
        convert_to_fc <fs_net_fc>.
        convert_to_fc <fs_tax_fc>.
      ENDIF.
    ENDLOOP.
    " ---------------------------------------------------------------------
    " 120: Transfer custom data to report structure
    " ---------------------------------------------------------------------
    ct_data = CORRESPONDING #( <fs_data> ).
  ENDMETHOD.
  METHOD extend_field_catalog_a410.
" ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA dref   TYPE REF TO data.
    FIELD-SYMBOLS <fs_col_pos> TYPE lvc_s_fcat-col_pos.
    DATA lf_f_prefix TYPE string VALUE 'ZZ_'.
    DATA lt_fieldcat TYPE lvc_t_fcat.
    " ---------------------------------------------------------------------
    " 20: Get configuration
    " ---------------------------------------------------------------------
    get_config_data( if_report = gc_z410_rpt
                     if_tcode  = cl_abap_syst=>get_transaction_code( )
                     if_struct = if_data_structure ).
    " ---------------------------------------------------------------------
    " 20: Create Data ref object
    " ---------------------------------------------------------------------
    CREATE DATA dref TYPE (if_data_structure).
    " ---------------------------------------------------------------------
    " 30: Assign component structure
    " ---------------------------------------------------------------------
    lo_str ?= cl_abap_structdescr=>describe_by_data_ref( dref ).
    " ---------------------------------------------------------------------
    " 40: Get Max. Col. position
    " ---------------------------------------------------------------------
    lt_fieldcat = CORRESPONDING #( ct_fieldcat ).
    SORT lt_fieldcat BY col_pos DESCENDING.
    IF xsdbool( line_exists( lt_fieldcat[ 1 ] ) ) = abap_true.
      ASSIGN lt_fieldcat[ 1 ]-col_pos TO <fs_col_pos>.
    ENDIF.
    IF <fs_col_pos> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    " ---------------------------------------------------------------------
    " 50: Create fcat for ZZ_ fields
    " ---------------------------------------------------------------------
    DO lines( lo_str->components ) TIMES.
      ASSIGN lo_str->components[ sy-index ]-name TO FIELD-SYMBOL(<fs_fname>).
      IF xsdbool( contains( val = <fs_fname>
                            sub = lf_f_prefix
                            off = 0
                            occ = 1 ) )
         = abap_false  OR
         xsdbool( line_exists( ct_fieldcat[ fieldname = <fs_fname> ] ) ) eq abap_true.
        CONTINUE.
      ENDIF.

      <fs_col_pos> += 1.
      INSERT VALUE lvc_s_fcat( fieldname = <fs_fname>
                               tabname   = '1'
                               col_pos   = <fs_col_pos>
                               ref_table = if_data_structure
                               ref_field = <fs_fname> )
             INTO TABLE ct_fieldcat.
    ENDDO.
    " ---------------------------------------------------------------------
    " 60: Field catalog customizing
    " ---------------------------------------------------------------------
    DO lines( lo_str->components ) TIMES.
      ASSIGN lo_str->components[ sy-index ]-name TO <fs_fname>.
      IF xsdbool( line_exists( gt_config_flds[ tabname   = if_data_structure
                                               fieldname = <fs_fname>  ] ) )
         = abap_false.
        CONTINUE.
      ENDIF.

      ASSIGN ct_fieldcat[ fieldname = <fs_fname> ] TO FIELD-SYMBOL(<fs_fieldcatalog>).
      ASSIGN gt_config_flds[ tabname   = if_data_structure
                             fieldname = <fs_fname>  ] TO FIELD-SYMBOL(<fs_cust_fcat>).
      <fs_fieldcatalog> = CORRESPONDING #( BASE ( <fs_fieldcatalog> ) <fs_cust_fcat>
                                  MAPPING scrtext_m = scrtext_m
                                          scrtext_l = scrtext_l
                                          scrtext_s = scrtext_s
                                          reptext   = scrtext_l
                                  EXCEPT tabname ).
      UNASSIGN <fs_fieldcatalog>.
      UNASSIGN <fs_cust_fcat>.
    ENDDO.
  ENDMETHOD.

  METHOD recpa_print_log.

  ENDMETHOD.

  METHOD report_customizing_y410.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    DATA lf_found      TYPE flag.
    DATA lr_rental_obj TYPE RANGE OF rebdbeno.
    DATA lt_doc_list   TYPE TABLE OF zrecp_sf_doc_a410.

    " ---------------------------------------------------------------------
    " 20: Assign data
    " ---------------------------------------------------------------------
    lt_doc_list = ct_data.
    lr_rental_obj = it_rental_obj.
    " ---------------------------------------------------------------------
    " 30: Unset data
    " *********************************************************************
    CLEAR ct_data.
    " ---------------------------------------------------------------------
    " 40: Check rental object exist for contract
    " *********************************************************************
    LOOP AT lt_doc_list ASSIGNING FIELD-SYMBOL(<fs_doc_list>).
      CLEAR lf_found.
      SELECT SINGLE FROM z_refx_i_be_vh
        FIELDS @abap_true
        WHERE ObjectNumber  = @<fs_doc_list>-objnr
          AND RentalObject IN @lr_rental_obj
        INTO @lf_found.
      " ---------------------------------------------------------------------
      " 50: Append return data
      " ---------------------------------------------------------------------
      IF lf_found = abap_true.
        APPEND <fs_doc_list> TO ct_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD output_customizing_y410.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " *********************************************************************
    DATA lf_found      TYPE flag.
    DATA lr_rental_obj TYPE RANGE OF rebdbeno.
    DATA lt_object     TYPE zrecp_t_object_c.

    CHECK lines( it_rental_obj ) > 0.
    " ---------------------------------------------------------------------
    " 20: Assign data
    " ---------------------------------------------------------------------
    lt_object = ct_object.
    lr_rental_obj = it_rental_obj.
    " ---------------------------------------------------------------------
    " 30: Unset data
    " *********************************************************************
    CLEAR ct_object.
    " ---------------------------------------------------------------------
    " 40: Check rental object exist for contract
    " *********************************************************************
    LOOP AT lt_object ASSIGNING FIELD-SYMBOL(<fs_object>).
      CLEAR lf_found.
      SELECT SINGLE FROM z_refx_i_be_vh
        FIELDS @abap_true
        WHERE ObjNr         = @<fs_object>-objnrtrg
          AND RentalObject IN @lr_rental_obj
        INTO @lf_found.
      " ---------------------------------------------------------------------
      " 50: Append return data
      " ---------------------------------------------------------------------
      IF lf_found = abap_true.
        APPEND <fs_object> TO ct_object.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD ____f4_aprtments.
    " ---------------------------------------------------------------------
    " 10: Data Declaration
    " ---------------------------------------------------------------------
    DATA lt_search_apartment TYPE TABLE OF t_selection_apartments.

    " ---------------------------------------------------------------------
    " 20: Apartment Selection
    " ---------------------------------------------------------------------
    SELECT FROM z_refx_i_be_vh
      FIELDS CompanyCode,
             RentalObject,
             ObjectNumber,
             ApartmentName,
             CampName,
             CheckInDate,
             CheckOutDate,
             \_Contract-recnnr AS ContractNo,
             \_zobjass-ztext   AS Guest
      WHERE \_Contract-recnnr
            IN @ir_recnnumber
      INTO CORRESPONDING FIELDS OF
      TABLE @lt_search_apartment.
    " ---------------------------------------------------------------------
    " 30: Call F4 selection with table lt_search_apartment
    " ---------------------------------------------------------------------
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield        = 'RENTALOBJECT'
                 value_org       = 'S'
                 window_title    = `Apartment Selection`
      TABLES     value_tab       = lt_search_apartment
                 return_tab      = rt_apartments
      EXCEPTIONS parameter_error = 1
                 no_values_found = 2
                 OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
