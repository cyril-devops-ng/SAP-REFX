*&---------------------------------------------------------------------*
*& Report ZREFX_CN_ALV
*&---------------------------------------------------------------------*
*& Author Name:     Cyril Sayeh
*&---------------------------------------------------------------------*
*& Company Name:    C2G Consulting
*&---------------------------------------------------------------------*
REPORT zrefx_cn_alv MESSAGE-ID zrefx.
**********************************************************************
*10: Top Include
**********************************************************************
INCLUDE zrefx_cn_alv_top.
**********************************************************************
*20: Selection Screen Include
**********************************************************************
INCLUDE zrefx_cn_alv_sel.
**********************************************************************
*30: Event Include
**********************************************************************
INCLUDE zrefx_cn_alv_evt.
*&---------------------------------------------------------------------*
*& Module REFX_PBO_100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE refx_pbo_100 OUTPUT.
  SET PF-STATUS 'ZREFX_STATUS100'.
  SET TITLEBAR  'ZREFX_TTL'.
**********************************************************************
*40.0 Check Instance Created
**********************************************************************
  CHECK xsdbool(
      go_cn_contract_fs   IS BOUND
  ) = abap_true.
**********************************************************************
*40.1: Process Excel
**********************************************************************
  TRY.
      go_cn_contract_fs->process_excel( ).
**********************************************************************
*40.2 Create Contract GUI instance
**********************************************************************
      go_cn_contract_gui = NEW #(
        it_contr_head = CORRESPONDING #( go_cn_contract_fs->gt_cont_haeder )
        it_contr_cond = CORRESPONDING #( go_cn_contract_fs->gt_cont_cond )
        if_mode       = COND #( WHEN p_cr EQ abap_true THEN zcl_refx_cn_contract_gui=>gc_create_mode
                                WHEN p_ch EQ abap_true THEN zcl_refx_cn_contract_gui=>gc_change_mode )
      ).
**********************************************************************
*40.3 Create Grid
**********************************************************************
      go_cn_contract_gui->create_grid(
        CHANGING
          ct_tab = go_cn_contract_gui->gt_parent_data
      ).
**********************************************************************
*40.4 Catch exceptions
**********************************************************************
    CATCH zcx_refx_exception INTO DATA(lo_cx_refx).
      TRY.
          zcl_refx_cn_contract_gui=>display_bapi_log_gui(
              zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx )
            ).
**********************************************************************
*40.5: Catch Other Exceptions
**********************************************************************
        CATCH zcx_refx_exception INTO DATA(lo_cx_refx_t).
          MESSAGE lo_cx_refx_t->get_text( ) TYPE 'I'.
          MESSAGE lo_cx_refx->get_text( )   TYPE 'I'.
      ENDTRY.
  ENDTRY.
**********************************************************************
*40.6: Display Grid
**********************************************************************
  CHECK xsdbool( go_cn_contract_gui IS BOUND ) = abap_true.
  go_cn_contract_gui->show_grid(  ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  REFX_PAI_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refx_pai_100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR '%EX' OR 'RW' OR 'E'
      OR 'ENDE' OR 'ECAN' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO TRANSACTION sy-tcode.
  ENDCASE.
ENDMODULE.
