CLASS zcl_refx_cn_contract DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_term_org_assignment        TYPE TABLE OF bapi_re_term_oa WITH DEFAULT KEY .
    TYPES:
      ty_term_payment               TYPE TABLE OF bapi_re_term_py WITH DEFAULT KEY .
    TYPES:
      ty_term_rythm                 TYPE TABLE OF bapi_re_term_rh WITH DEFAULT KEY .
    TYPES:
      ty_partner                    TYPE TABLE OF bapi_re_partner WITH DEFAULT KEY .
    TYPES:
      ty_object_rel                 TYPE TABLE OF bapi_re_object_rel WITH DEFAULT KEY .
    TYPES:
      ty_condition                  TYPE TABLE OF bapi_re_condition WITH DEFAULT KEY .
    TYPES:
      ty_term_org_assignment_create TYPE TABLE OF bapi_re_term_oa_dat WITH DEFAULT KEY .
    TYPES:
      ty_term_payment_create        TYPE TABLE OF bapi_re_term_py_dat WITH DEFAULT KEY .
    TYPES:
      ty_term_rythm_create          TYPE TABLE OF bapi_re_term_rh_dat WITH DEFAULT KEY .
    TYPES:
      ty_partner_create             TYPE TABLE OF bapi_re_partner_dat WITH DEFAULT KEY .
    TYPES:
      ty_object_rel_create          TYPE TABLE OF bapi_re_object_rel_dat WITH DEFAULT KEY .
    TYPES:
      ty_condition_create           TYPE TABLE OF bapi_re_condition_dat WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_bapi_obj,
        name        TYPE string,
        description TYPE string,
      END OF ty_bapi_obj .
    TYPES:
      ty_cn_bapi_descr TYPE TABLE OF ty_bapi_obj WITH DEFAULT KEY .
    TYPES:
      ty_mig_cond_typ  TYPE TABLE OF zrefx_mig_condtp WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_table_keys,
        key    TYPE string,
        value  TYPE string,
        exists TYPE flag,
      END OF ty_table_keys.
    TYPES: tt_table_keys TYPE TABLE OF ty_table_keys WITH DEFAULT KEY.
    CLASS-METHODS create_new_contract
      IMPORTING
        !if_comp_code                  TYPE bukrs
        !if_contract_type              TYPE recncontracttype
        !if_contract                   TYPE recnnumber
        !if_test_run                   TYPE flag
        !if_contract_create            TYPE bapi_re_contract_dat
        !if_term_org_assignment_create TYPE ty_term_org_assignment_create
        !if_term_payment_create        TYPE ty_term_payment_create
        !if_term_rythm_create          TYPE ty_term_rythm_create
        !if_partner_create             TYPE ty_partner_create
        !if_object_rel_create          TYPE ty_object_rel_create
        !if_condition_create           TYPE ty_condition_create
      RETURNING
        VALUE(r_result)                TYPE REF TO zcl_refx_cn_contract
      RAISING
        zcx_refx_exception .
    CLASS-METHODS get_contract
      IMPORTING
        !if_comp_code     TYPE bukrs
        !if_contract_type TYPE recncontracttype
        !if_contract      TYPE recnnumber
      RETURNING
        VALUE(r_result)   TYPE REF TO zcl_refx_cn_contract
      RAISING
        zcx_refx_exception .
    CLASS-METHODS read_refx_cn_bapi_config
      RETURNING
        VALUE(rt_refx_cn_bapi_config) TYPE ztt_refx_c_re_obj .
    METHODS constructor
      IMPORTING
        !if_comp_code     TYPE bukrs OPTIONAL
        !if_contract_type TYPE recncontracttype OPTIONAL
        !if_contract      TYPE recnnumber OPTIONAL
        !if_test_run      TYPE flag OPTIONAL
      RAISING
        zcx_refx_exception .
    METHODS get_gs_contract
      RETURNING
        VALUE(r_result) TYPE bapi_re_contract .
    METHODS get_gt_term_org_assignment
      RETURNING
        VALUE(r_result) TYPE ty_term_org_assignment .
    METHODS get_gt_term_payment
      RETURNING
        VALUE(r_result) TYPE ty_term_payment .
    METHODS get_gt_term_rythm
      RETURNING
        VALUE(r_result) TYPE ty_term_rythm .
    METHODS get_gt_partner
      RETURNING
        VALUE(r_result) TYPE ty_partner .
    METHODS get_gt_object_rel
      RETURNING
        VALUE(r_result) TYPE ty_object_rel .
    METHODS get_gt_condition
      RETURNING
        VALUE(r_result) TYPE ty_condition .
    METHODS refx_bapi_obj_validate
      IMPORTING
        !if_object TYPE ty_bapi_obj
        !it_data   TYPE any
      RAISING
        zcx_refx_exception .
    METHODS set_gt_cn_bapi_obj
      IMPORTING
        !gt_cn_bapi_obj TYPE ty_cn_bapi_descr .
    METHODS set_gt_mig_cond_typ
      IMPORTING
        !gt_mig_cond_typ TYPE ty_mig_cond_typ .
    METHODS create_change_parameters
      RAISING
        zcx_refx_exception .
    METHODS read_cond_currency .
    METHODS fill_bapi_fields
      IMPORTING
        !it_contr_head TYPE zcl_refx_cn_contract_fs=>tt_cont_head
        !it_contr_cond TYPE zcl_refx_cn_contract_fs=>tt_cont_cond
      RAISING
        zcx_refx_exception .
    METHODS create_guests
      IMPORTING
        !if_contract   TYPE recnnumber
        !it_contr_cond TYPE zcl_refx_cn_contract_fs=>tt_cont_cond
        !it_conditions TYPE ty_condition
      RAISING
        zcx_refx_exception .
    METHODS create_refx_cn_contract
      EXPORTING
        !ef_contract     TYPE recnnumber
        !ef_comp_code    TYPE bukrs
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t
      RAISING
        zcx_refx_exception .
    METHODS change_refx_cn_contract
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t
      RAISING
        zcx_refx_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: read_refx_cn_data IMPORTING if_contract_type TYPE recncontracttype
                                         if_contract      TYPE recnnumber
                                         if_comp_code     TYPE bukrs OPTIONAL ,
      validate_refx_cn_contract
        RAISING zcx_refx_exception,
      compare_table IMPORTING it_create_tab              TYPE STANDARD TABLE
                              it_old_tab                 TYPE STANDARD TABLE
                    CHANGING  ct_change_tab              TYPE STANDARD TABLE
                              ct_table_keys              TYPE tt_table_keys OPTIONAL
                    RETURNING VALUE(rf_change_indicator) TYPE char1
                    RAISING   zcx_refx_exception,

      compare_structure IMPORTING is_create_struc            TYPE any
                                  is_old_struc               TYPE any
                        CHANGING  cs_change_struc            TYPE any
                                  cs_change_strucx           TYPE any OPTIONAL
                        RETURNING VALUE(rf_change_indicator) TYPE char1.

    DATA: gs_contract                   TYPE bapi_re_contract,
          gt_term_org_assignment        TYPE TABLE OF bapi_re_term_oa,
          gt_term_payment               TYPE TABLE OF bapi_re_term_py,
          gt_term_rythm                 TYPE TABLE OF bapi_re_term_rh,
          gt_partner                    TYPE TABLE OF bapi_re_partner,
          gt_object_rel                 TYPE TABLE OF bapi_re_object_rel,
          gt_condition                  TYPE TABLE OF bapi_re_condition,
          gs_contract_create            TYPE bapi_re_contract_dat,
          gs_contract_change            TYPE bapi_re_contract_dat,
          gs_contract_change_x          TYPE bapi_re_contract_datx,
          gt_term_org_assignment_create TYPE TABLE OF bapi_re_term_oa_dat,
          gt_term_payment_create        TYPE TABLE OF bapi_re_term_py_dat,
          gt_term_rythm_create          TYPE TABLE OF bapi_re_term_rh_dat,
          gt_partner_create             TYPE TABLE OF bapi_re_partner_dat,
          gt_object_rel_create          TYPE TABLE OF bapi_re_object_rel_dat,
          gt_condition_create           TYPE TABLE OF bapi_re_condition_dat,
          gt_term_org_assignment_change TYPE TABLE OF bapi_re_term_oa_datc,
          gt_term_payment_change        TYPE TABLE OF bapi_re_term_py_datc,
          gt_term_rythm_change          TYPE TABLE OF bapi_re_term_rh_datc,
          gt_partner_change             TYPE TABLE OF bapi_re_partner_datc,
          gt_object_rel_change          TYPE TABLE OF bapi_re_object_rel_datc,
          gt_condition_change           TYPE TABLE OF bapi_re_condition_datc,
          gt_return                     TYPE bapiret2_t,
          gt_return_create              TYPE bapiret2_t,
          gt_return_change              TYPE bapiret2_t,
          gf_comp_code                  TYPE bukrs,
          gf_contract_type              TYPE recncontracttype,
          gf_test_run                   TYPE flag,
          gf_new_comp_code              TYPE bapi_re_contract_key-comp_code,
          gf_new_contract               TYPE bapi_re_contract_key-contract_number,
          gt_extension_in               TYPE STANDARD TABLE OF bapiparex,
          gt_cn_bapi_obj                TYPE TABLE OF ty_bapi_obj,
          gt_mig_cond_typ               TYPE TABLE OF zrefx_mig_condtp,
          gt_cond_curr                  TYPE TABLE OF zre_check_cond,
          gf_contract_detail_flag       TYPE flag,
          gt_table_keys                 TYPE tt_table_keys.
    DATA lt_sorted_conditions TYPE SORTED TABLE OF zcl_refx_cn_contract_fs=>t_cont_cond
       WITH NON-UNIQUE KEY tax_group.
    data: gt_contr_head TYPE zcl_refx_cn_contract_fs=>tt_cont_head,
          gt_contr_cond TYPE zcl_refx_cn_contract_fs=>tt_cont_cond.
    TYPES: BEGIN OF t_tax_grp,
             tax_group TYPE rerataxgroup,
           END OF t_tax_grp.
    TYPES: BEGIN OF t_partners,
             partner   TYPE bu_partner,
             role_type TYPE rebprole,
           END OF t_partners.
    DATA lt_tax_groups TYPE SORTED TABLE OF t_tax_grp WITH UNIQUE KEY tax_group .
    DATA lt_partners   TYPE SORTED TABLE OF t_partners WITH UNIQUE KEY partner role_type.

    CONSTANTS: gc_bukrs    TYPE bukrs  VALUE 'NG01',
               gc_contract TYPE string VALUE 'CONTRACT',
               gc_term_oa  TYPE string VALUE 'RE_TERM_OA',
               gc_term_py  TYPE string VALUE 'RE_TERM_PY',
               gc_term_rh  TYPE string VALUE 'RE_TERM_RH',
               gc_partner  TYPE string VALUE 'RE_PARTNER',
               gc_object   TYPE string VALUE 'RE_OBJECT',
               gc_cond     TYPE string VALUE 'RE_COND'.


    CLASS-DATA gt_refx_cn_bapi_config TYPE ztt_refx_c_re_obj.

ENDCLASS.



CLASS zcl_refx_cn_contract IMPLEMENTATION.


  METHOD create_new_contract.
**********************************************************************
*10: Instance creation
**********************************************************************
    r_result = NEW #(
      if_comp_code     = if_comp_code
      if_contract_type = if_contract_type
      if_contract      = if_contract
      if_test_run      = if_test_run
    ).
**********************************************************************
*20: Set Attributes
**********************************************************************
    r_result->gs_contract_create            = if_contract_create.
    r_result->gt_term_org_assignment_create = if_term_org_assignment_create.
    r_result->gt_term_payment_create        = if_term_payment_create.
    r_result->gt_term_rythm_create          = if_term_rythm_create.
    r_result->gt_partner_create             = if_partner_create.
    r_result->gt_object_rel_create          = if_object_rel_create.
    r_result->gt_condition_create           = if_condition_create.
    r_result->set_gt_cn_bapi_obj(
        VALUE #(
            ( name = gc_contract  description = 'Real Estate Contract - Data' )
            ( name = gc_term_oa   description = 'Organizational Assignment of an RE Object - Data' )
            ( name = gc_term_py   description = 'Posting Term of an RE Object - Data' )
            ( name = gc_term_rh   description = 'Frequency Term of an RE Object - Data' )
            ( name = gc_partner   description = 'Partner of RE Object - Data' )
            ( name = gc_object    description = 'Objects of Real Estate Contract - Data' )
            ( name = gc_cond      description = 'Conditions of an RE Object - Data' )
       )
      ).
**********************************************************************
*30: Configuration Data
**********************************************************************
    r_result->set_gt_mig_cond_typ( VALUE #( (  ) ) ).
**********************************************************************
*40: Validation
**********************************************************************
    TRY.
        r_result->validate_refx_cn_contract( ).
      CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
        RAISE EXCEPTION lo_cx_refx.
    ENDTRY.

  ENDMETHOD.


  METHOD get_contract.
**********************************************************************
*10: Get Instance
**********************************************************************
    r_result = NEW #(
      if_comp_code     = if_comp_code
      if_contract_type = if_contract_type
      if_contract      = if_contract
    ).
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*10: Assign if supplied: Comp. code
**********************************************************************
    IF if_comp_code IS SUPPLIED.
      me->gf_comp_code = if_comp_code.
    ENDIF.
**********************************************************************
*20: Assign if supplied: Contract type
**********************************************************************
    IF if_contract_type IS SUPPLIED.
      me->gf_contract_type = if_contract_type.
    ENDIF.
**********************************************************************
*30: Assign if supplied: Test flag
**********************************************************************
    IF if_test_run IS SUPPLIED.
      me->gf_test_run = if_test_run.
    ENDIF.
**********************************************************************
*40: Get configuration data
**********************************************************************
    set_gt_mig_cond_typ( VALUE #( (  ) ) ).
**********************************************************************
*50: Retrieve Contract
**********************************************************************
    IF ( if_contract_type IS SUPPLIED AND if_contract IS SUPPLIED
        AND if_contract_type IS NOT INITIAL AND if_contract IS NOT INITIAL ).
      me->gf_contract_detail_flag = abap_false.
      me->read_refx_cn_data( if_contract_type = if_contract_type if_contract = if_contract  ).
**********************************************************************
*60: Check: Contract exists?
**********************************************************************
      IF gs_contract IS INITIAL OR line_exists( gt_return[ type = 'E' ] ).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid             = zcx_refx_exception=>refx_cn_null_contract
            refx_cn_contract   = if_contract
            refx_cn_contr_type = if_contract_type.
      ENDIF.
**********************************************************************
*70: Validate contract type
**********************************************************************
      IF gs_contract IS NOT INITIAL AND xsdbool( if_contract_type = gs_contract-contract_type ) EQ abap_false.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid             = zcx_refx_exception=>refx_cn_invalid_cntr_type
            refx_cn_contract   = if_contract
            refx_cn_contr_type = if_contract_type.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD read_refx_cn_data.
**********************************************************************
*10: BAPI Call: BAPI_RE_CN_GET_DETAIL
**********************************************************************
    CALL FUNCTION 'BAPI_RE_CN_GET_DETAIL'
      EXPORTING
        compcode            = COND #( WHEN if_comp_code IS SUPPLIED THEN if_comp_code ELSE gc_bukrs )
        contractnumber      = if_contract
      IMPORTING
        contract            = gs_contract
      TABLES
        term_org_assignment = me->gt_term_org_assignment
        term_payment        = gt_term_payment
        term_rhythm         = gt_term_rythm
        partner             = gt_partner
        object_rel          = gt_object_rel
        condition           = gt_condition
        return              = gt_return.
**********************************************************************
*20: Update global flag
**********************************************************************
    IF xsdbool( line_exists( gt_return[ type = 'E' ] )
        AND     line_exists( gt_return[ type = 'A' ] )
        AND     line_exists( gt_return[ type = 'X' ] )
     ) EQ abap_false.
      me->gf_contract_detail_flag = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_gs_contract.
**********************************************************************
* Get RE-FX CN Gen. Data
**********************************************************************
    r_result = me->gs_contract.
  ENDMETHOD.


  METHOD get_gt_term_org_assignment.
**********************************************************************
* Get RE-FX CN Term Org Assignment
**********************************************************************
    r_result = me->gt_term_org_assignment.
  ENDMETHOD.


  METHOD get_gt_term_payment.
**********************************************************************
* Get RE-FX CN Term Payment
**********************************************************************
    r_result = me->gt_term_payment.
  ENDMETHOD.


  METHOD get_gt_term_rythm.
**********************************************************************
* Get RE-FX CN Term Rythm
**********************************************************************
    r_result = me->gt_term_rythm.
  ENDMETHOD.


  METHOD get_gt_partner.
**********************************************************************
* Get RE-FX CN Partner
**********************************************************************
    r_result = me->gt_partner.
  ENDMETHOD.


  METHOD get_gt_object_rel.
**********************************************************************
* Get RE-FX CN Object Rel.
**********************************************************************
    r_result = me->gt_object_rel.
  ENDMETHOD.


  METHOD get_gt_condition.
**********************************************************************
* Get RE-FX CN Condition
**********************************************************************
    r_result = me->gt_condition.
  ENDMETHOD.


  METHOD create_refx_cn_contract.
**********************************************************************
*10: BAPI Call: BAPI_RE_CN_CREATE
**********************************************************************
    TRY.
        CALL FUNCTION 'BAPI_RE_CN_CREATE'
          EXPORTING
            comp_code_ext       = COND #( WHEN me->gf_comp_code IS INITIAL
                                  THEN gc_bukrs ELSE me->gf_comp_code )
            contract_type       = me->gf_contract_type
            contract            = me->gs_contract_create
            test_run            = me->gf_test_run
          IMPORTING
            compcode            = me->gf_new_comp_code
            contractnumber      = me->gf_new_contract
          TABLES
            term_org_assignment = me->gt_term_org_assignment_create
            term_payment        = me->gt_term_payment_create
            term_rhythm         = me->gt_term_rythm_create
            partner             = me->gt_partner_create
            object_rel          = me->gt_object_rel_create
            condition           = me->gt_condition_create
            extension_in        = me->gt_extension_in
            return              = me->gt_return_create.

        rt_return = me->gt_return_create.
**********************************************************************
*20: Check: Update run?
**********************************************************************
        IF me->gf_test_run EQ abap_false.
          IF xsdbool( line_exists( me->gt_return_create[ type = 'E' ] )
                  AND line_exists( me->gt_return_create[ type = 'A' ] )
                  AND line_exists( me->gt_return_create[ type = 'X' ] ) ) EQ abap_false.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
            ef_comp_code = me->gf_new_comp_code.
            ef_contract  = me->gf_new_contract.
**********************************************************************
*20,1: Create guests
**********************************************************************
            me->create_guests(
              EXPORTING
                if_contract   = me->gf_new_contract
                it_contr_cond = corresponding #( me->gt_contr_cond )
                it_conditions = corresponding #( gt_condition_change )
            ).
          ENDIF.
        ENDIF.
**********************************************************************
*30: Catch errors
**********************************************************************
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_create_failure
            previous = lo_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD validate_refx_cn_contract.
**********************************************************************
*10: Validate RE-FX Gen. Data
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_contract ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_contract ]
              it_data   = me->gs_contract_create
          ).
        CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*20: Validate RE-FX Contract type
**********************************************************************
    SELECT SINGLE FROM
    tiv26
    FIELDS
    @abap_true
    WHERE smvart = @me->gf_contract_type
    INTO @DATA(lf_contract_type_exists).
    IF xsdbool( lf_contract_type_exists = abap_false ) EQ abap_true.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid             = zcx_refx_exception=>refx_cn_invalid_cntr_type
          refx_cn_contr_type = me->gf_contract_type.
    ENDIF.
**********************************************************************
*30: Validate RE-FX Term Org Assignment
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_term_oa ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_term_oa ]
              it_data   = me->gt_term_org_assignment_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*40: Validate RE-FX Term Rythm
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_term_rh ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_term_rh ]
              it_data   = me->gt_term_rythm_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*50: Validate RE-FX Payment
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_term_py ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_term_py ]
              it_data   = me->gt_term_payment_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*60: Validate RE-FX Partner
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_partner ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_partner ]
              it_data   = me->gt_partner_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*70: Validate RE-FX Object Rel.
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_object ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_object ]
              it_data   = me->gt_object_rel_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*80: Validate RE-FX Condition
**********************************************************************
    IF line_exists( gt_cn_bapi_obj[ name = gc_cond ] ).
      TRY.
          me->refx_bapi_obj_validate(
            EXPORTING
              if_object = gt_cn_bapi_obj[ name = gc_cond ]
              it_data   = me->gt_condition_create
          ).
        CATCH zcx_refx_exception INTO lo_cx_refx.
          RAISE EXCEPTION lo_cx_refx.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD read_refx_cn_bapi_config.
**********************************************************************
*10: Get RE-FX CN BAPI Configuration
**********************************************************************
    SELECT FROM
    zrefx_c_re_obj
    FIELDS *
    WHERE status = @abap_true
    INTO TABLE @gt_refx_cn_bapi_config.
  ENDMETHOD.


  METHOD refx_bapi_obj_validate.
**********************************************************************
*10: Data Declaration
**********************************************************************
    FIELD-SYMBOLS: <fs_fnam> TYPE any.
    DATA: dref TYPE REF TO data.
*    CREATE DATA dref LIKE me->gs_contract_create.
**********************************************************************
*20: Create Data Ref.
**********************************************************************
    CREATE DATA dref LIKE it_data.
**********************************************************************
*30: Assign Data Ref
**********************************************************************
    ASSIGN dref->* TO <fs_fnam>.
**********************************************************************
*40: Retrieve BAPI configuration
**********************************************************************
    LOOP AT gt_refx_cn_bapi_config ASSIGNING
    FIELD-SYMBOL(<fs_bapi_config>) WHERE
    re_cn_obj = if_object-name.
      ASSIGN COMPONENT <fs_bapi_config>-fieldname OF STRUCTURE <fs_fnam> TO FIELD-SYMBOL(<fs_comp>).
**********************************************************************
*50: Assign Defaults
**********************************************************************
      IF xsdbool(  <fs_bapi_config>-default_value IS NOT INITIAL AND <fs_comp> IS INITIAL ) EQ abap_true.
        <fs_comp> = <fs_bapi_config>-default_value.
      ENDIF.
**********************************************************************
*60: Check Required fields
**********************************************************************
      IF xsdbool( <fs_bapi_config>-required EQ 'X' AND <fs_comp> IS INITIAL ) EQ abap_true.
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid              = zcx_refx_exception=>refx_cn_req_field_error
            refx_cn_field_name  = CONV #( <fs_bapi_config>-fieldname )
            refx_cn_field_value = if_object-description.
      ENDIF.
      UNASSIGN <fs_comp>.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_gt_cn_bapi_obj.
**********************************************************************
*10: Set Config Data
**********************************************************************
    me->gt_cn_bapi_obj = gt_cn_bapi_obj.
  ENDMETHOD.


  METHOD set_gt_mig_cond_typ.
**********************************************************************
*10: Retrieve Config Data
**********************************************************************
    IF gt_mig_cond_typ IS INITIAL.
      SELECT FROM
      zrefx_mig_condtp
      FIELDS *
      INTO TABLE @me->gt_mig_cond_typ.
    ELSE.
      me->gt_mig_cond_typ = gt_mig_cond_typ.
    ENDIF.
  ENDMETHOD.


  METHOD read_cond_currency.
**********************************************************************
*10: Retrieve Condition/Curr Config
**********************************************************************
    IF me->gt_cond_curr IS INITIAL.
      SELECT FROM
      zre_check_cond
      FIELDS *
      INTO TABLE @me->gt_cond_curr.
    ENDIF.
  ENDMETHOD.


  METHOD fill_bapi_fields.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA lt_term_payment_create LIKE gt_term_payment_create.
    DATA lf_term_no TYPE i.
**********************************************************************
*20: Unset Global variables
**********************************************************************
    CLEAR: me->gs_contract_create,
           me->gt_condition_create,
           me->gt_object_rel_create,
           me->gt_partner_create,
           me->gt_return_create,
           me->gt_term_org_assignment_create,
           me->gt_term_payment_create,
           me->gt_term_rythm_create,
           me->gt_contr_head,
           me->gt_contr_cond.
**********************************************************************
*20.1: Fill contract data
**********************************************************************
    me->gt_contr_head = it_contr_head.
    me->gt_contr_cond = it_contr_cond.
**********************************************************************
*30: Retrieve Contr.Head components
**********************************************************************
    lo_str ?= cl_abap_structdescr=>describe_by_data(  p_data = it_contr_head[ 1 ] ).
    TRY.
**********************************************************************
*40: Create Mapping table
**********************************************************************
        DATA(lt_mapper) = VALUE cl_abap_corresponding=>mapping_table(
          FOR <fs_comp> IN lo_str->components
          WHERE
          (   name = 'CONTRACT_TEXT'  OR name = 'OLD_CONTRACT_NUMBER' OR name = 'CONTRACT_START_DATE' OR
              name = 'FIRST_END_DATE' OR name = 'POSSESSION_FROM'     OR   name = 'POSSESSION_TO' OR
              name = 'CURRENCY_CONTRACT'
          )
          (   level = 0
              kind = 1
              srcname = <fs_comp>-name
              dstname = CONV #(  <fs_comp>-name )
           )
        ).
**********************************************************************
*50: Retrieve Contract.Create components
**********************************************************************
        lo_str ?= cl_abap_structdescr=>describe_by_data( p_data = me->gs_contract_create ).
**********************************************************************
*60: Update Mapping table
**********************************************************************
        INSERT
         LINES OF VALUE cl_abap_corresponding=>mapping_table(
              FOR <fs> IN lo_str->components
              WHERE
              ( name = 'CASH_FLOW_START_DATE' OR name = 'POSTING_START_DATE' )
              ( level = 0
                kind = 1
                dstname = <fs>-name
                srcname = 'CONTRACT_START_DATE'
               )
          ) INTO
          TABLE lt_mapper.
**********************************************************************
*70: Create Mapper
**********************************************************************
        DATA(lo_mapper) = cl_abap_corresponding=>create(
          EXPORTING
            source                = it_contr_head[ 1 ]
            destination           = me->gs_contract_create
            mapping               = lt_mapper ).
**********************************************************************
*80: Execute Mapper
**********************************************************************
        lo_mapper->execute(
              EXPORTING
                source        = it_contr_head[ 1 ]
              CHANGING
                destination   = me->gs_contract_create
            ).
**********************************************************************
*90: Unset mapper
**********************************************************************
        CLEAR: lo_str,
           lt_mapper,
           lo_mapper.
**********************************************************************
*100: Map Rythm.create components
**********************************************************************

        me->gt_term_rythm_create = CORRESPONDING #( it_contr_head ).
        LOOP AT me->gt_term_rythm_create ASSIGNING FIELD-SYMBOL(<fs_rythm>).
          <fs_rythm>-term_text = `<standard>`.
          <fs_rythm>-starting_month = `13`.
          <fs_rythm>-pro_rata_method = `0`.
          <fs_rythm>-pro_rata_method_calculation = `1`.
        ENDLOOP.
**********************************************************************
*110: Map Object Rel.create components
**********************************************************************
        me->gt_object_rel_create = CORRESPONDING #(  it_contr_cond MAPPING  contract_object_id = calculation_object_id  ).
        LOOP AT me->gt_object_rel_create
            ASSIGNING FIELD-SYMBOL(<fs_obj>).
          <fs_obj>-contract_object_type = 'IM'.
        ENDLOOP.
**********************************************************************
*120: Derive bukrs/swenr/smenr from contract object
**********************************************************************
        SPLIT me->gt_object_rel_create[ 1 ]-contract_object_id
        AT `/`
        INTO DATA(lf_bukrs) DATA(lf_swenr) DATA(lf_smenr).
**********************************************************************
*130: Assert Rental object key
**********************************************************************
        IF xsdbool( lf_bukrs IS INITIAL OR lf_swenr IS INITIAL OR lf_smenr IS INITIAL ) EQ abap_true.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid             = zcx_refx_exception=>refx_cn_invalid_ro
              refx_cn_rental_obj = CONV #( me->gt_object_rel_create[ 1 ]-contract_object_id ).
        ENDIF.
**********************************************************************
*140: Retrieve Distinct Rental object prctr/func area
**********************************************************************
        SELECT
        FROM vibdro AS vib
        INNER JOIN vitmoa AS vit
        ON vib~intreno = vit~intreno
        AND vit~termtype = `1120`
        FIELDS
        DISTINCT prctr AS p,
        functionalarea AS f
        WHERE bukrs = @lf_bukrs AND
              swenr = @lf_swenr AND
              smenr = @lf_smenr
*              AND
*              prctr <> @space AND
*              functionalarea <> @space
        INTO TABLE @DATA(lt_dbcount).
**********************************************************************
*150: Check :Rental object exists?
**********************************************************************
        IF lines( lt_dbcount ) EQ 0.
          RAISE EXCEPTION TYPE zcx_refx_exception
            EXPORTING
              textid             = zcx_refx_exception=>refx_cn_invalid_ro
              refx_cn_rental_obj = CONV #( me->gt_object_rel_create[ 1 ]-contract_object_id ).
        ENDIF.
**********************************************************************
*160: Map Org Assigment.create
**********************************************************************
        INSERT VALUE #( term_text = `<Standard>`
                        func_area =  COND #( WHEN lines( lt_dbcount ) > 1 THEN `DUMMY` ELSE space )
                        profit_ctr = COND #( WHEN lines( lt_dbcount ) > 1 THEN `DUMMY` ELSE space )
                         )
        INTO TABLE me->gt_term_org_assignment_create.
**********************************************************************
*170: Sort conditions/tax groups
**********************************************************************
        lt_tax_groups = CORRESPONDING #( it_contr_cond DISCARDING DUPLICATES ) .

        lt_sorted_conditions = CORRESPONDING #( it_contr_cond ).
**********************************************************************
*180: Term payment
**********************************************************************
        gt_term_payment_create = VALUE #(  FOR <fstax> IN lt_tax_groups INDEX INTO idx
                                            LET counter = 10 * idx IN
                                        (
                                        term_text = `<Standard>`
                                        tax_type  = `MWST`
                                        currency_translation_rule = `S_CONDITION_CURRENCY`
                                        tax_group = <fstax>-tax_group
                                        partner = lt_sorted_conditions[ tax_group = <fstax>-tax_group ]-partner
                                        pymt_meth = it_contr_head[ 1 ]-pymt_meth
                                        pmnttrms = it_contr_head[ 1 ]-pmnttrms
                                        housebankid = it_contr_head[ 1 ]-housebankid
                                     )
                                     ).
**********************************************************************
*180.1 Sort Term Payment
**********************************************************************
        SORT gt_term_payment_create BY tax_group partner.
**********************************************************************
*180.2 Temporal store for term payment
**********************************************************************
        lt_term_payment_create = CORRESPONDING #( gt_term_payment_create ).
**********************************************************************
*180.3 Check if split currency is required?
**********************************************************************
        LOOP AT gt_term_payment_create INTO DATA(ls_term_payment).
**********************************************************************
*180.4 Assign condition by tax group
**********************************************************************
          ASSIGN lt_sorted_conditions[ tax_group = ls_term_payment-tax_group ] TO FIELD-SYMBOL(<fs_cond>).
          IF
          xsdbool(
                  <fs_cond>-currency1 IS NOT INITIAL
             AND  <fs_cond>-calc_rule_parameter1 IS NOT INITIAL
             AND  <fs_cond>-currency2 IS NOT INITIAL
             AND  <fs_cond>-calc_rule_parameter2 IS NOT INITIAL
             AND  <fs_cond>-currency1 NE <fs_cond>-currency2
            ) EQ abap_true.
**********************************************************************
*180.4 Retrieve term payment
**********************************************************************
            DATA(ls_s_term_payment) =  ls_term_payment .
**********************************************************************
*180.5 Update currency translation rule
**********************************************************************
            ls_s_term_payment-currency_translation_rule = 'S_COMP_CODE_CURRENCY'.
**********************************************************************
*180.6 Assign term no
**********************************************************************
            ADD 10 TO lf_term_no.
            ls_s_term_payment-term_no = lf_term_no.
**********************************************************************
*180.7 Update temporal term payment
**********************************************************************
            INSERT ls_term_payment INTO  TABLE lt_term_payment_create.
          ENDIF.
        ENDLOOP.
**********************************************************************
*180.8 Update Global term payment
**********************************************************************
        gt_term_payment_create = CORRESPONDING #( lt_term_payment_create ).
**********************************************************************
*190: Partners
**********************************************************************
        lt_partners = CORRESPONDING #( it_contr_cond DISCARDING DUPLICATES ) .
        gt_partner_create = CORRESPONDING #( lt_partners ).
**********************************************************************
*190: Conditions
**********************************************************************
        LOOP AT lt_sorted_conditions ASSIGNING FIELD-SYMBOL(<fss>).
**********************************************************************
*190.1 Create new condition record
**********************************************************************
          APPEND INITIAL LINE TO gt_condition_create ASSIGNING FIELD-SYMBOL(<fsc>).
          <fsc> = CORRESPONDING #( <fss> ).
**********************************************************************
*190.2 Assign term payment no by tax group & partner
**********************************************************************
          <fsc>-term_no_payment = gt_term_payment_create[ tax_group = <fss>-tax_group partner = <fss>-partner ].
**********************************************************************
          <fsc>-distribution_object_id = <fsc>-calculation_object_id.
          <fsc>-distribution_object_type = <fsc>-calculation_object_type.
**********************************************************************
*190.3 If calculation object type let the system create it
**********************************************************************
          IF contains( val = <fsc>-calculation_object_type sub = 'IS' ).
            CLEAR: <fsc>-distribution_object_type , <fsc>-distribution_object_id ,
                   <fsc>-calculation_object_type , <fsc>-calculation_object_id.
          ENDIF.
**********************************************************************
*190.4 Assign NGN is condition type end in 05
**********************************************************************
          IF <fsc>-condition_type CP '*05'.
            <fsc>-currency = 'NGN'.
          ENDIF.
**********************************************************************
*190.5 Get bukrs/swenr/smenr from calc. object id
**********************************************************************
          IF <fsc>-calc_rule = 'DX'.
            SPLIT <fsc>-calculation_object_id AT '/' INTO lf_bukrs lf_swenr lf_smenr.
**********************************************************************
*190.6 Get intreno
**********************************************************************
            SELECT SINGLE FROM vibdro
            FIELDS intreno
            WHERE bukrs = @lf_bukrs
            AND swenr = @lf_swenr
            AND smenr = @lf_smenr
            INTO @DATA(lv_intreno).
**********************************************************************
*190.7 Get meas
**********************************************************************
            SELECT SINGLE FROM vibdmeas
            FIELDS meas
            WHERE intreno = @lv_intreno
                  AND measunit = 'M2'
             INTO @<fsc>-calc_rule_parameter1 .
            CLEAR lv_intreno.
          ENDIF.
**********************************************************************
*190.8 Default dist_rule parameter1 if distr rule is MX
**********************************************************************
          IF <fsc>-dist_rule = 'MX'.
            <fsc>-dist_rule_parameter1 = 'AR00'.
          ENDIF.
**********************************************************************
*190.9 If multiple currencies
**********************************************************************
          IF <fss>-currency1 IS NOT INITIAL
            AND <fss>-calc_rule_parameter1 IS NOT INITIAL
            AND <fss>-currency2 IS NOT INITIAL
            AND <fss>-calc_rule_parameter2 IS NOT INITIAL.
**********************************************************************
*190.10 Set external purpose to I
**********************************************************************
            <fsc>-external_purpose = 'I'.
**********************************************************************
*190.11 Get the condition type currency
**********************************************************************
            IF <fsc>-currency IS INITIAL.
              SELECT SINGLE curr
                FROM zre_cond_curr
                INTO <fsc>-currency
               WHERE condtype EQ <fsc>-condition_type.
            ENDIF.
**********************************************************************
*190.12 Get bukrs/swenr/smenr
**********************************************************************
            SPLIT <fss>-calculation_object_id AT '/' INTO lf_bukrs lf_swenr lf_smenr.
**********************************************************************
*190.13 Convert smenr
**********************************************************************
            CALL FUNCTION 'CONVERSION_EXIT_SMENR_OUTPUT'
              EXPORTING
                input  = lf_smenr
              IMPORTING
                output = lf_smenr.
**********************************************************************
*190.14 Assign calc rule parameter 2
**********************************************************************
            CONCATENATE '[' <fsc>-external_purpose '   ][' <fsc>-condition_type '][' <fsc>-calculation_object_type lf_bukrs lf_swenr lf_smenr ']'
            INTO <fsc>-calc_rule_parameter2 RESPECTING BLANKS.
**********************************************************************
*190.15 Assign calc rule
**********************************************************************
            <fsc>-calc_rule = 'E1X'.
**********************************************************************
*190.16 Assign External purpose
**********************************************************************
            <fsc>-external_purpose = <fss>-external_purpose.
**********************************************************************
*190.17 Assign Calc rule parameter 1
**********************************************************************
            <fsc>-calc_rule_parameter1 = <fss>-calc_rule_parameter1.
**********************************************************************
*190.18 Assign currency 1
**********************************************************************
            <fsc>-currency = <fss>-currency1.
            DATA  ls_condtp  LIKE LINE OF gt_mig_cond_typ.
            CLEAR ls_condtp.
**********************************************************************
*190.19 Read condition from condition config
**********************************************************************
            READ TABLE gt_mig_cond_typ INTO ls_condtp WITH KEY cond_type_parent = <fss>-condition_type currency_child = <fsc>-currency.
            <fsc>-condition_type = ls_condtp-cond_type_child.
**********************************************************************
*190.20 Append new condition line
**********************************************************************
            APPEND INITIAL LINE TO gt_condition_create ASSIGNING <fsc>.
**********************************************************************
*190.21 Assign calc rule parameter 1
**********************************************************************
            <fsc>-calc_rule_parameter1 = <fss>-calc_rule_parameter2.
**********************************************************************
*190.22 Assign currency 1
**********************************************************************
            <fsc>-currency = <fss>-currency1.
**********************************************************************
*190.23 Unset condtp
**********************************************************************
            CLEAR ls_condtp.
**********************************************************************
*190.24 Get condition from condition config
**********************************************************************
            READ TABLE gt_mig_cond_typ INTO ls_condtp WITH KEY
                                        cond_type_parent = <fss>-condition_type
                                        currency_child = <fss>-currency2.
            IF <fsc>-condition_type = ls_condtp-cond_type_child.
              <fsc>-condition_type = ls_condtp-cond_type_child_alt.
            ELSE.
              <fsc>-condition_type = ls_condtp-cond_type_child.
            ENDIF.
**********************************************************************
*190.25 Get term payment by tax group & partner
**********************************************************************
            READ TABLE gt_term_payment_create WITH KEY
                                                tax_group   = <fss>-tax_group
                                                partner     = <fss>-partner
                                                currency_translation_rule = 'S_COMP_CODE_CURRENCY'
                                      ASSIGNING FIELD-SYMBOL(<term2>).
            IF sy-subrc IS INITIAL.
**********************************************************************
*190.26 Assign term no payment
**********************************************************************
              MOVE <term2>-term_no TO <fsc>-term_no_payment.
            ENDIF.
          ELSE.
**********************************************************************
*190.27 Get currency by condition type if inintial
**********************************************************************
            IF <fsc>-currency IS INITIAL.
              SELECT SINGLE curr
                FROM zre_cond_curr
                INTO <fsc>-currency
               WHERE condtype EQ <fsc>-condition_type.
            ENDIF.
          ENDIF.
        ENDLOOP.
**********************************************************************
*200 Create extension
**********************************************************************
        DATA: lt_extension_in TYPE TABLE OF bapiparex,
              ls_extension_in TYPE bapiparex,
              ls_return       TYPE bapiret2,
              ls_cidata       TYPE recn_contract_ci,
              lv_tax_group    TYPE rerataxgroup.
**********************************************************************
*200.1 Assign zzre_rate
**********************************************************************
        IF it_contr_head[ 1 ]-zzre_rate IS NOT INITIAL.
          DATA(rate) = it_contr_head[ 1 ]-zzre_rate.
          MOVE rate TO ls_cidata-zzre_rate.
        ENDIF.
**********************************************************************
*200.2 Fill extension
**********************************************************************
        IF ls_cidata IS NOT INITIAL.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value               = ls_cidata
            IMPORTING
              ex_container           = ls_extension_in-valuepart1
            EXCEPTIONS
              illegal_parameter_type = 1
              OTHERS                 = 2.
          IF sy-subrc IS INITIAL.
            MOVE: 'CI_DATA' TO ls_extension_in-structure.
            APPEND ls_extension_in TO lt_extension_in.
            gt_extension_in = CORRESPONDING #( lt_extension_in ).
          ENDIF.
        ENDIF.
**********************************************************************
*170: Catch  error
**********************************************************************
      CATCH cx_corr_dyn_error INTO DATA(lo_dyn_error) .
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            previous = lo_dyn_error.
**********************************************************************
*180: Catch General error
**********************************************************************
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            previous = lo_cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD change_refx_cn_contract.
**********************************************************************
*10: BAPI Call: BAPI_RE_CN_CHANGE
**********************************************************************
    TRY.
        CALL FUNCTION 'BAPI_RE_CN_CHANGE'
          EXPORTING
            compcode            = COND #( WHEN me->gf_comp_code IS INITIAL
                                       THEN gc_bukrs ELSE me->gf_comp_code )
            contractnumber      = me->gs_contract-contract_number
            contract            = me->gs_contract_change
            contract_x          = me->gs_contract_change_x
            test_run            = me->gf_test_run
          TABLES
            term_org_assignment = me->gt_term_org_assignment_change
            term_payment        = me->gt_term_payment_change
            term_rhythm         = me->gt_term_rythm_change
            partner             = me->gt_partner_change
            object_rel          = me->gt_object_rel_change
            condition           = me->gt_condition_change
            extension_in        = me->gt_extension_in
            return              = me->gt_return_change.

        rt_return = me->gt_return_change.
**********************************************************************
*20: Check: Update run?
**********************************************************************
        IF me->gf_test_run EQ abap_false.
          IF xsdbool( line_exists( me->gt_return_create[ type = 'E' ] )
                  AND line_exists( me->gt_return_create[ type = 'A' ] )
                  AND line_exists( me->gt_return_create[ type = 'X' ] ) ) EQ abap_false.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
**********************************************************************
*20.1: Create guests
**********************************************************************
            me->create_guests(
              EXPORTING
                if_contract   = me->gs_contract-contract_number
                it_contr_cond = corresponding #( me->gt_contr_cond )
                it_conditions = corresponding #( gt_condition_change )
            ).
*            CATCH zcx_refx_exception.
          ENDIF.
        ENDIF.
**********************************************************************
*30: Catch errors
**********************************************************************
      CATCH cx_root INTO DATA(lo_cx_root).
        RAISE EXCEPTION TYPE zcx_refx_exception
          EXPORTING
            textid   = zcx_refx_exception=>refx_cn_create_failure
            previous = lo_cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD create_change_parameters.
**********************************************************************
*09: Unset Global change fields
**********************************************************************
    CLEAR: gs_contract_change,
           gt_term_org_assignment_change,
           gt_term_rythm_change,
           gt_term_payment_change,
           gt_partner_change,
           gt_object_rel_change,
           gt_condition_change.
**********************************************************************
*10: Data Declaration
**********************************************************************
    DATA lf_change_indicator TYPE char1.

**********************************************************************
*10.0 If global flag is not set
**********************************************************************
    IF me->gf_contract_detail_flag EQ abap_false.
      RAISE EXCEPTION TYPE zcx_refx_exception
        EXPORTING
          textid             = zcx_refx_exception=>refx_cn_null_contract
          refx_cn_contract   = me->gs_contract-contract_number
          refx_cn_contr_type = me->gs_contract-contract_type.
    ENDIF.
**********************************************************************
*10.1 Contract header
**********************************************************************
    IF xsdbool( me->gs_contract_create IS NOT INITIAL ) EQ abap_true.
      TRY.
          compare_structure(
            EXPORTING
              is_create_struc     = me->gs_contract_create
              is_old_struc        = me->gs_contract
            CHANGING
              cs_change_struc     = me->gs_contract_change
              cs_change_strucx    = me->gs_contract_change_x
          ).
        CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
      ENDTRY.
    ENDIF.
**********************************************************************
*20: Org Assignment
**********************************************************************
    CLEAR: lf_change_indicator.
    IF xsdbool( me->gt_term_org_assignment_create IS NOT INITIAL ) EQ abap_true.
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_term_org_assignment_create
              it_old_tab          = me->gt_term_org_assignment
            CHANGING
              ct_change_tab       = me->gt_term_org_assignment_change
            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_term_org_assignment_change ASSIGNING FIELD-SYMBOL(<fs_org>).
              <fs_org>-change_indicator = lf_change_indicator.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*30: Term Rythm
**********************************************************************
    CLEAR: lf_change_indicator.
    IF xsdbool( me->gt_term_rythm_create IS NOT INITIAL ) EQ abap_true.
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_term_rythm_create
              it_old_tab          = me->gt_term_rythm
            CHANGING
              ct_change_tab       = me->gt_term_rythm_change
            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_term_rythm_change ASSIGNING FIELD-SYMBOL(<fs_rhy>).
              <fs_rhy>-change_indicator = lf_change_indicator.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*40: Term Payment
**********************************************************************
    CLEAR lf_change_indicator.
    IF xsdbool( me->gt_term_payment_create IS NOT INITIAL ) EQ abap_true.
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_term_payment_create
              it_old_tab          = me->gt_term_payment
            CHANGING
              ct_change_tab       = me->gt_term_payment_change
            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_term_payment_change ASSIGNING FIELD-SYMBOL(<fs_pay>).
              <fs_pay>-change_indicator = lf_change_indicator.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.

**********************************************************************
*50: Partner
**********************************************************************
    CLEAR lf_change_indicator.
    IF xsdbool( me->gt_partner_create IS NOT INITIAL ) EQ abap_true.
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_partner_create
              it_old_tab          = me->gt_partner
            CHANGING
              ct_change_tab       = me->gt_partner_change
            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_partner_change ASSIGNING FIELD-SYMBOL(<fs_partner>).
              <fs_partner>-change_indicator = lf_change_indicator.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*60: Object Rel.
**********************************************************************
    CLEAR: lf_change_indicator.
    IF xsdbool( me->gt_object_rel_create IS NOT INITIAL ) EQ abap_true.
      DATA lt_tab_keys TYPE tt_table_keys.
      lt_tab_keys = VALUE
                    #( FOR <fs> IN me->gt_object_rel_create
                          (
                              key = `CONTRACT_OBJECT_ID`
                              value = <fs>-contract_object_id
                          )
                      ).
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_object_rel_create
              it_old_tab          = me->gt_object_rel

            CHANGING
              ct_change_tab       = me->gt_object_rel_change
              ct_table_keys       = lt_tab_keys

            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_object_rel_change ASSIGNING FIELD-SYMBOL(<fs_obj>).
              IF <fs_obj>-change_indicator IS INITIAL.
                <fs_obj>-change_indicator = lf_change_indicator.
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.
**********************************************************************
*Conditions
**********************************************************************
    CLEAR: lf_change_indicator.
    IF xsdbool( me->gt_condition_create IS NOT INITIAL ) EQ abap_true.
      lt_tab_keys = VALUE
                  #( FOR <fs> IN me->gt_object_rel_create
                        (
                            key = `CALCULATION_OBJECT_ID`
                            value = <fs>-contract_object_id
                        )
                    ).
      TRY.
          compare_table(
            EXPORTING
              it_create_tab       = me->gt_condition_create
              it_old_tab          = me->gt_condition
            CHANGING
              ct_change_tab       = me->gt_condition_change
              ct_table_keys       = lt_tab_keys
            RECEIVING
              rf_change_indicator = lf_change_indicator
          ).
          IF xsdbool( lf_change_indicator EQ 'I' OR lf_change_indicator EQ 'U') EQ abap_true.
            LOOP AT me->gt_condition_change ASSIGNING FIELD-SYMBOL(<fs_cond>).
              <fs_cond>-change_indicator = lf_change_indicator.
            ENDLOOP.
          ENDIF.
        CATCH zcx_refx_exception INTO lo_cx_refx.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD compare_table.
**********************************************************************
*10: Creae structure ref.
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA lo_new_str TYPE REF TO cl_abap_structdescr.
    lo_str ?= cl_abap_structdescr=>describe_by_data( it_create_tab[ 1 ] ).
    DATA lf_keys_found TYPE flag.
**********************************************************************
*20: Check: Contract Details exists
**********************************************************************
    IF xsdbool( me->gf_contract_detail_flag EQ abap_true
           AND  it_old_tab IS NOT INITIAL ) EQ abap_true.
**********************************************************************
*30: Assign change table
**********************************************************************
      ct_change_tab = CORRESPONDING #( it_old_tab ).
      lo_new_str ?= cl_abap_structdescr=>describe_by_data(  ct_change_tab[ 1 ] ).
**********************************************************************
*30: Check if table keys are provided
**********************************************************************
      IF ct_table_keys IS SUPPLIED.
        LOOP AT ct_table_keys ASSIGNING FIELD-SYMBOL(<fs_keys>).
          LOOP AT ct_change_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
            DO lines( lo_str->components ) TIMES.
              IF xsdbool(  lo_str->components[ sy-index ]-name EQ <fs_keys>-key
              )
              EQ abap_true.
                ASSIGN COMPONENT lo_str->components[ sy-index ]-name OF STRUCTURE
                <fs_tab> TO FIELD-SYMBOL(<fs_key>).
                IF <fs_key> EQ <fs_keys>-value.
                  <fs_keys>-exists = abap_true.
                  ASSIGN  lo_str->components[ sy-index ]-name TO FIELD-SYMBOL(<fs_key_name>).
                  DATA(lf_index)      = line_index( it_create_tab[ (<fs_key_name>) = <fs_keys>-value  ] ).
                  DATA(lf_chng_ind)   = compare_structure(
                    EXPORTING
                      is_create_struc     = it_create_tab[ lf_index ]
                      is_old_struc        = <fs_tab>
                    CHANGING
                      cs_change_struc     = <fs_tab>
                  ).
                  ASSIGN COMPONENT `CHANGE_INDICATOR` OF STRUCTURE <fs_tab> TO
                  FIELD-SYMBOL(<fs_change_indicator>).
                  IF <fs_change_indicator> IS ASSIGNED.
                    <fs_change_indicator> = lf_chng_ind.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDDO.
          ENDLOOP.
        ENDLOOP.
        rf_change_indicator = lf_chng_ind.
        LOOP AT ct_table_keys ASSIGNING <fs_keys> WHERE exists EQ abap_false.
          IF line_exists(  it_create_tab[ (<fs_keys>-key) = <fs_keys>-value  ] ).
            DATA(lf_index_k) = line_index( it_create_tab[ (<fs_keys>-key) = <fs_keys>-value  ] ).
            IF lf_index_k IS NOT INITIAL.
              APPEND INITIAL LINE TO ct_change_tab ASSIGNING FIELD-SYMBOL(<fs_change_tab>).
              <fs_change_tab> = CORRESPONDING #( it_create_tab[ lf_index_k ] ).
              ASSIGN COMPONENT `CHANGE_INDICATOR` OF STRUCTURE <fs_change_tab> TO
                   FIELD-SYMBOL(<fs_c_indicator>).
              IF <fs_c_indicator> IS ASSIGNED.
                <fs_c_indicator> = 'I'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
**********************************************************************
*40: For each record in change table
**********************************************************************
        LOOP AT ct_change_tab ASSIGNING <fs_tab>.
**********************************************************************
*50: For each component in record
**********************************************************************
          DO lines( lo_str->components ) TIMES.
            IF xsdbool( line_exists( lo_new_str->components[ name = lo_str->components[ sy-index ]-name ] ) )
                EQ abap_false.
              CONTINUE.
            ENDIF.
**********************************************************************
*60: Assign new component
**********************************************************************
            ASSIGN COMPONENT lo_str->components[ sy-index ]-name
            OF STRUCTURE it_create_tab[ sy-tabix ] TO FIELD-SYMBOL(<fs_field_new>).
**********************************************************************
*70: Assign old component
**********************************************************************
            ASSIGN COMPONENT lo_str->components[ sy-index ]-name
            OF STRUCTURE <fs_tab> TO FIELD-SYMBOL(<fs_field_old>).
**********************************************************************
*70: Assign change if new is different from old
**********************************************************************
            IF xsdbool( <fs_field_new> IS NOT INITIAL AND
                        <fs_field_new> NE <fs_field_old> ) EQ abap_true.
              <fs_field_old>      = <fs_field_new>.
              rf_change_indicator = 'U'.
            ENDIF.
          ENDDO.
        ENDLOOP.
      ENDIF.
**********************************************************************
*80: Return new indicator flag
**********************************************************************
    ELSEIF xsdbool( me->gf_contract_detail_flag EQ abap_true
               AND  it_old_tab IS INITIAL ) EQ abap_true.
      ct_change_tab = CORRESPONDING #( it_create_tab ).
      rf_change_indicator = 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD compare_structure.
**********************************************************************
*10: Create structure ref.
**********************************************************************
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    DATA lo_new_str TYPE REF TO cl_abap_structdescr.
    lo_str ?= cl_abap_structdescr=>describe_by_data( is_create_struc ).
**********************************************************************
*20: Check: Contract Details exists
**********************************************************************
    IF xsdbool( me->gf_contract_detail_flag EQ abap_true
        AND     is_old_struc IS NOT INITIAL ) EQ abap_true.
**********************************************************************
*30: Assign change structure
**********************************************************************
      IF xsdbool( cs_change_struc IS INITIAL ) EQ abap_true.
        cs_change_struc = CORRESPONDING #( is_old_struc ).
      ENDIF.
      lo_new_str ?= cl_abap_structdescr=>describe_by_data(  cs_change_struc ).
**********************************************************************
*40: For each structure component
**********************************************************************
      DO lines( lo_str->components ) TIMES.
        IF xsdbool( line_exists( lo_new_str->components[ name = lo_str->components[ sy-index ]-name ] ) )
           EQ abap_false.
          CONTINUE.
        ENDIF.
**********************************************************************
*50: Assign new component
**********************************************************************
        ASSIGN COMPONENT lo_str->components[ sy-index ]-name
        OF STRUCTURE is_create_struc TO FIELD-SYMBOL(<fs_field_new>).
**********************************************************************
*60: Assign old component
**********************************************************************
        ASSIGN COMPONENT lo_str->components[ sy-index ]-name
        OF STRUCTURE cs_change_struc TO FIELD-SYMBOL(<fs_field_old>).
**********************************************************************
*70: Assign change if new is different from old
**********************************************************************
        IF xsdbool( <fs_field_new> IS NOT INITIAL AND
                    <fs_field_new> NE <fs_field_old> ) EQ abap_true.
          IF cs_change_strucx IS SUPPLIED.
            ASSIGN COMPONENT lo_str->components[ sy-index ]-name
            OF STRUCTURE cs_change_strucx TO FIELD-SYMBOL(<fs_field_change>).
            <fs_field_change>     = 'X'.
          ENDIF.
          <fs_field_old>        = <fs_field_new>.
          rf_change_indicator   = 'U'.
        ENDIF.
      ENDDO.
**********************************************************************
*80: Create new indicator flag
**********************************************************************
    ELSEIF xsdbool( me->gf_contract_detail_flag EQ abap_true
                AND is_old_struc IS INITIAL ) EQ abap_true.
      cs_change_struc = CORRESPONDING #( is_create_struc ).
      rf_change_indicator = 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD create_guests.
**********************************************************************
*10: Type declaration
**********************************************************************
    TYPES: BEGIN OF t_guest,
             calculation_object_type TYPE recdbusobjtypecalc,
             calculation_object_id   TYPE recdbusobjidcalc,
             guest                   TYPE zre_guest,
           END OF t_guest.
    TYPES: tr_calc_id TYPE RANGE OF recdbusobjidcalc.
**********************************************************************
*20: Data Declaration
**********************************************************************
    DATA lt_guests TYPE TABLE OF t_guest.
    DATA(lv_check) = abap_false.
    DATA(lv_count) = 0.
**********************************************************************
*30: Get Guests data
**********************************************************************
    lt_guests = VALUE #( FOR <fs> IN it_contr_cond
                            WHERE ( calculation_object_id IN VALUE tr_calc_id(
                                    FOR <fs_c> IN it_conditions
                                    (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = <fs_c>-calculation_object_id
                                 ) ) )
                           ( calculation_object_id = <fs>-calculation_object_id
                             calculation_object_type = <fs>-calculation_object_type
                             guest = <fs>-guest )
                        ).
**********************************************************************
*40: Delete records without guests
**********************************************************************
    DELETE lt_guests WHERE guest IS INITIAL.
**********************************************************************
*50: Get contract
**********************************************************************
    SELECT SINGLE FROM
    vicncn
    FIELDS objnr
    WHERE bukrs = `NG01`
    AND   recnnr = @if_contract
    INTO @DATA(lf_objnr).
**********************************************************************
*60: Check objnr is assigned
**********************************************************************
    CHECK lf_objnr IS NOT INITIAL.
**********************************************************************
*70:For each guest data
**********************************************************************
    LOOP AT lt_guests ASSIGNING FIELD-SYMBOL(<fs_guest>).
**********************************************************************
*80: Get rental object key
**********************************************************************
      SPLIT <fs_guest>-calculation_object_id AT '/' INTO DATA(lf_bukrs)
                                                         DATA(lf_swenr)
                                                         DATA(lf_smenr).
**********************************************************************
*90: Get Rental Object
**********************************************************************
      SELECT SINGLE FROM
      vibdro
      FIELDS objnr
      WHERE bukrs = @lf_bukrs
      AND   swenr = @lf_swenr
      AND   smenr = @lf_smenr
      INTO @DATA(lf_tgt_obj).
**********************************************************************
*100: Check target src object is not initial
**********************************************************************
      CHECK lf_tgt_obj IS NOT INITIAL.
**********************************************************************
*110: Wait: for custom data :func: ZRE_UPD_ZVIBDOBJASS
**********************************************************************
      WHILE lv_check EQ abap_false.
        SELECT SINGLE FROM
        zvibdobjass
        FIELDS *
        WHERE objnrsrc  = @lf_objnr
        AND   objnrtrg  = @lf_tgt_obj
        INTO @DATA(ls_zobj).

        IF sy-subrc IS INITIAL.
          MOVE abap_true TO lv_check.
        ELSE.
          IF lv_count = 5.
            MOVE abap_true TO lv_check.
          ELSE.
            WAIT UP TO 3 SECONDS.
            ADD 1 TO lv_count.
          ENDIF.
        ENDIF.
      ENDWHILE.
**********************************************************************
*120: Check custom data exists
**********************************************************************
      IF xsdbool( ls_zobj IS INITIAL ) EQ abap_false.
**********************************************************************
*130: Assign guest
**********************************************************************
        ls_zobj-ztext = <fs_guest>-guest.
**********************************************************************
*140: Modify custom data
**********************************************************************
        MODIFY zvibdobjass FROM ls_zobj.
      ENDIF.
      CLEAR: lf_tgt_obj, lf_bukrs, lf_swenr, lf_smenr.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
