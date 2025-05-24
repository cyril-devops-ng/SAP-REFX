*"* use this source file for your ABAP unit test classes
class ltcl_refx_cn_contract_fs_test definition final for testing
  duration short
  risk level harmless.

  PRIVATE SECTION.
    DATA: mo_contract_fs TYPE REF TO zcl_refx_cn_contract_fs,
          mt_layout_sheet TYPE zcl_refx_cn_contract_fs=>tt_layout_sheet.

    METHODS: setup,
             test_refx_cn_contract for testing,
             test_import_file for testing,
             test_get_gf_xstring for testing.
endclass.


class ltcl_refx_cn_contract_fs_test implementation.

  METHOD setup.
    " Set up initial data for testing
    DATA lt_layout TYPE zcl_refx_cn_contract_fs=>tt_layout_sheet.

    " Populate with test data for tt_layout_sheet
*    APPEND VALUE #( refx_cn_in = 'Test' sheet_no = 1 ) TO lt_layout.
    APPEND VALUE #( refx_cn_in = 'CONTR_HEAD' sheet_no = 1 ) TO lt_layout.
    mt_layout_sheet = lt_layout.

    " Create the object to be tested
    try.
    mo_contract_fs = zcl_refx_cn_contract_fs=>create_instance(
*                        i_file_name = 'test_file.xlsx'
                        i_file_name = 'C://'
                        it_layout_sheet = mt_layout_sheet
                    ).
    catch zcx_refx_exception into data(lo_cx_refx).
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Unable to instantiate zcl_refx_cn_contract_fs|
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
            detail = lo_cx_refx->get_text(  )
        ).
    endtry.
  ENDMETHOD.


  METHOD test_import_file.
    if mo_contract_fs->gt_filetable is initial.
    cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Unable to instantiate zcl_refx_cn_contract_fs|
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
*            detail =
        ).
    endif.
  ENDMETHOD.

  METHOD test_get_gf_xstring.
    " Test get_gf_xstring method
    DATA lf_xstring TYPE xstring.

    lf_xstring = mo_contract_fs->get_gf_xstring( ).
    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = lf_xstring
        msg              = `Input file was not coverted to xstring`
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
    cl_abap_unit_assert=>assert_bound(
        act = lf_xstring
        msg = `Method get_xf_string does not return a result`
    ).
  ENDMETHOD.



  method test_refx_cn_contract.
    if me->mo_contract_fs is not bound.
        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = |Unable to instantiate zcl_refx_cn_contract_fs|
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
*            detail =
        ).
    endif.
  endmethod.

endclass.
