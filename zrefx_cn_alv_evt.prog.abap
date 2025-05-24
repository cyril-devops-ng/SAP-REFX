*&---------------------------------------------------------------------*
*& Include          ZREFX_CN_ALV_EVT
*&---------------------------------------------------------------------*
**********************************************************************
*10: Initialization
**********************************************************************
INITIALIZATION.
**********************************************************************
*10.1: Screen Texts
**********************************************************************
  blk_ttl = zcl_refx_cn_contract_gui=>blk_ttl.
  title1  = zcl_refx_cn_contract_gui=>title1.
  title2  = zcl_refx_cn_contract_gui=>title2.
  title3  = zcl_refx_cn_contract_gui=>title3.
  title4  = zcl_refx_cn_contract_gui=>title4.
  title5  = zcl_refx_cn_contract_gui=>title5.
**********************************************************************
*10.2: Screen Logo
**********************************************************************
  logo1 = icon_import.
  logo2 = icon_header.
  logo3 = icon_real_estate_object.
  logo4 = icon_create.
  logo5 = icon_change.
**********************************************************************
*10.3: Screen Comments
**********************************************************************
  lf_comm1 = `-->.xls/.xlsx : Accepts Excel`.
  lf_comm2 = `    1. Fill Worksheet No & Record's first Row in the Popup Modal: `.
  lf_comm3 = `    2. For Contract Update-->Contract No, Company Code & Contract Type`.
*  lf_comm4 = `                                           is Mandatory.`.
  lf_comm4 = `    is Mandatory.`.
  lf_comm5 = `    3. If a field is not intended to be updated, it may be left blank `.
  lf_comm6 = `    in the template.`.
**********************************************************************
*20: Selection Screen Output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SEL'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
**********************************************************************
*30: Selection-Screen Value Request for p_file
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
**********************************************************************
*30.1: Display Layout Selection (Pop-up Modal)
**********************************************************************
  DATA(lt_layout_selection) = zcl_refx_cn_contract_gui=>display_layout_selection( ).
**********************************************************************
*30.2: Create Instance: RE-FX File System
**********************************************************************
  TRY.
      zcl_refx_cn_contract_fs=>create_instance(
        EXPORTING
          i_file_name     = `/`
          it_layout_sheet = COND #( WHEN lt_layout_selection IS INITIAL THEN VALUE #( ( ) )
                                    ELSE lt_layout_selection )
        RECEIVING
          r_result        = go_cn_contract_fs
      ).
      p_file = go_cn_contract_fs->gt_filetable[ 1 ]-filename.
**********************************************************************
*30.3: Catch Exceptions
**********************************************************************
    CATCH zcx_refx_exception INTO DATA(lo_cx_refx). " RE-FX Exception Class
      TRY.
          zcl_refx_cn_contract_gui=>display_bapi_log_gui(
              zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx )
            ).
        CATCH zcx_refx_exception INTO DATA(lo_cx_refx_t).
          MESSAGE lo_cx_refx_t->get_text( ) TYPE 'I'.
          MESSAGE lo_cx_refx->get_text( )   TYPE 'I'.
      ENDTRY.
**********************************************************************
*30.4 Catch More Exceptions
**********************************************************************
    CATCH
     cx_fdt_excel_core INTO DATA(lo_cx_ex_core).  " FDT: Export/Import to Excel core errors
      TRY.
          zcl_refx_cn_contract_gui=>display_bapi_log_gui(
              zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx )
            ).
        CATCH zcx_refx_exception INTO lo_cx_refx_t.
          MESSAGE lo_cx_refx_t->get_text( ) TYPE 'I'.
          MESSAGE lo_cx_refx->get_text( )   TYPE 'I'.
      ENDTRY.
  ENDTRY.
**********************************************************************
*40: Start of Selection
**********************************************************************
START-OF-SELECTION.
"do nothing
**********************************************************************
*50: End of Selection
**********************************************************************
END-OF-SELECTION.
**********************************************************************
*50.0 Check Instance Created
**********************************************************************
  CHECK xsdbool(
      go_cn_contract_fs   IS BOUND
  ) = abap_true.
**********************************************************************
*50.1 Call main screen
**********************************************************************
  CALL SCREEN zcl_refx_cn_contract_gui=>screen.
