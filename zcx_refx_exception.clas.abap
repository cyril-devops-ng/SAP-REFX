CLASS zcx_refx_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !textid              LIKE if_t100_message=>t100key OPTIONAL
        !previous            LIKE previous OPTIONAL
        !refx_cn_field_name  TYPE string OPTIONAL
        !refx_cn_field_value TYPE string OPTIONAL
        !refx_cn_file_format TYPE string OPTIONAL
        !refx_cn_file_path   TYPE string OPTIONAL
        !refx_cn_cond_type   TYPE string OPTIONAL
        !refx_cn_cond_curr   TYPE string OPTIONAL
        !refx_cn_contract    TYPE recnnumber OPTIONAL
        !refx_cn_contr_type  TYPE recncontracttype OPTIONAL
        !refx_cn_layout      TYPE string OPTIONAL
        !refx_cn_rental_obj  TYPE string OPTIONAL
        !refx_cn_contr_id    TYPE string OPTIONAL
        !recpa_print_user    type string optional
        !recpa_print_date    type datum optional.


    CONSTANTS:
      BEGIN OF refx_cn_invalid_typ_conv,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_FIELD_NAME',
        attr2 TYPE scx_attrname VALUE 'REFX_CN_FIELD_VALUE',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_invalid_typ_conv.

    CONSTANTS:
      BEGIN OF refx_cn_invalid_fft,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_FILE_FORMAT',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_invalid_fft.

    CONSTANTS:
      BEGIN OF refx_cn_null_contract,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_CONTRACT',
        attr2 TYPE scx_attrname VALUE 'REFX_CN_CONTR_TYPE',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_contract.

    CONSTANTS:
      BEGIN OF refx_cn_invalid_cntr_type,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_CONTR_TYPE',
        attr2 TYPE scx_attrname VALUE 'REFX_CN_CONTRACT',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_invalid_cntr_type .

    CONSTANTS:
      BEGIN OF refx_cn_file_path_error,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_FILE_PATH',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_file_path_error.

    CONSTANTS:
      BEGIN OF refx_cn_condition_error,
        msgid TYPE symsgid VALUE 'ZREFX_MIGRATION',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_COND_TYPE',
        attr2 TYPE scx_attrname VALUE 'REFX_CN_COND_CURR',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_condition_error.

    CONSTANTS:
      BEGIN OF refx_cn_missing_prctr,
        msgid TYPE symsgid VALUE 'ZREFX_MIGRATION',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_missing_prctr.

    CONSTANTS:
      BEGIN OF refx_cn_inv_contr_type,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_CONTR_TYPE',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_inv_contr_type.

    CONSTANTS:
      BEGIN OF refx_cn_req_field_error,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_FIELD_NAME',
        attr2 TYPE scx_attrname VALUE 'REFX_CN_FIELD_VALUE',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_req_field_error.

    CONSTANTS:
      BEGIN OF refx_cn_null_file,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_file.

    CONSTANTS:
      BEGIN OF refx_cn_front_end_error,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_front_end_error.
    CONSTANTS:
      BEGIN OF refx_cn_upload_error,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_upload_error.
    CONSTANTS:
      BEGIN OF refx_cn_null_layout,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_layout.
    CONSTANTS:
      BEGIN OF refx_cn_layout_not_found,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_LAYOUT',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_layout_not_found.
    CONSTANTS:
      BEGIN OF refx_cn_null_worksheet,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_worksheet.
    CONSTANTS:
      BEGIN OF refx_cn_null_sheet_no,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_LAYOUT',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_sheet_no.
    CONSTANTS:
      BEGIN OF refx_cn_null_data,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_data.
    CONSTANTS:
      BEGIN OF refx_cn_null_column,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_column.
    CONSTANTS:
      BEGIN OF refx_cn_invalid_ro,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_RENTAL_OBJ',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_invalid_ro.
    CONSTANTS:
      BEGIN OF refx_cn_create_failure,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_create_failure.
    CONSTANTS:
      BEGIN OF refx_cn_msg_conv_fail,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_msg_conv_fail.
    CONSTANTS:
      BEGIN OF refx_cn_alv_error,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_alv_error.
    CONSTANTS:
      BEGIN OF refx_cn_null_contr_cond,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_CONTR_ID',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_contr_cond.
    CONSTANTS:
      BEGIN OF refx_cn_null_contr_head,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE 'REFX_CN_CONTR_ID',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_contr_head.
    CONSTANTS:
      BEGIN OF refx_cn_null_record,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_cn_null_record.
    CONSTANTS:
      BEGIN OF refx_action_cancelled,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_action_cancelled.
    CONSTANTS:
      BEGIN OF refx_contr_checked_in,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '026',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_contr_checked_in.
    CONSTANTS:
      BEGIN OF refx_contr_checked_out,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_contr_checked_out.
    CONSTANTS:
      BEGIN OF refx_already_checked_in,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_already_checked_in.
    CONSTANTS:
      BEGIN OF refx_already_checked_out,
        msgid TYPE symsgid VALUE 'ZREFX',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF refx_already_checked_out.
      constants:
        begin of recpa_user_not_authorized,
          msgid type symsgid value 'ZREFX',
          msgno type symsgno value '031',
          attr1 type scx_attrname value 'attr1',
          attr2 type scx_attrname value 'attr2',
          attr3 type scx_attrname value 'attr3',
          attr4 type scx_attrname value 'attr4',
        end of recpa_user_not_authorized.
        constants:
          begin of recpa_doc_already_printed,
            msgid type symsgid value 'ZREFX',
            msgno type symsgno value '032',
            attr1 type scx_attrname value 'attr1',
            attr2 type scx_attrname value 'attr2',
            attr3 type scx_attrname value 'attr3',
            attr4 type scx_attrname value 'attr4',
          end of recpa_doc_already_printed.
    DATA:
      refx_cn_field_name  TYPE string,
      refx_cn_field_value TYPE string,
      refx_cn_file_format TYPE string,
      refx_cn_file_path   TYPE string,
      refx_cn_cond_type   TYPE string,
      refx_cn_cond_curr   TYPE string,
      refx_cn_contract    TYPE recnnumber,
      refx_cn_contr_type  TYPE recncontracttype,
      refx_cn_layout      TYPE string,
      refx_cn_rental_obj  TYPE string,
      refx_cn_contr_id    TYPE string,
      recpa_print_date type datum,
      recpa_print_user type string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_refx_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->refx_cn_field_name  = refx_cn_field_name.
    me->refx_cn_field_value = refx_cn_field_value.
    me->refx_cn_file_format = refx_cn_file_format.
    me->refx_cn_file_path   = refx_cn_file_path.
    me->refx_cn_cond_type   = refx_cn_cond_type.
    me->refx_cn_cond_curr   = refx_cn_cond_curr.
    me->refx_cn_contract    = refx_cn_contract.
    me->refx_cn_contr_type  = refx_cn_contr_type.
    me->refx_cn_layout      = refx_cn_layout.
    me->refx_cn_rental_obj  = refx_cn_rental_obj.
    me->refx_cn_contr_id    = refx_cn_contr_id.
    me->recpa_print_date = recpa_print_date.
    me->recpa_print_user = recpa_print_user.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
