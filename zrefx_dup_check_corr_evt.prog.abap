*&---------------------------------------------------------------------*
*& Include          ZREFX_DUP_CHECK_CORR_EVT
*&---------------------------------------------------------------------*
**********************************************************************
INITIALIZATION.
**********************************************************************
logo1   = icon_document.
title1  = `Contract Number`.
logo2   = icon_company_code.
title2  = `Company Code`.
logo3   = icon_real_estate_object.
title3  = `Contract Type`.
logo4   = icon_date.
title4  = `Contract Start Date`.
logo5   = icon_date.
title5  = `Contract First End`.
logo6   = icon_document.
title6  = `Contract Number`.
logo7   = icon_real_estate_object.
title7  = `Contract Type`.


tt1     = `Contract Selection for Duplicate Check`.
tt2     = `Check-In/Check-Out Selection`.
tta     = `Action Selection`.
ttd     = `General Seelection`.

**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
if xsdbool( p_r_cin  eq abap_true
         or p_r_cout eq abap_true )
         eq abap_true.
    configure_check_screen.
  else.
    configure_dupl_screen.
endif.
**********************************************************************
START-OF-SELECTION.
**********************************************************************
**********************************************************************
*60: Create App
**********************************************************************
  TRY.
      IF p_r_dchk EQ abap_true.
        go_dup_check
            = lcl_dup_check=>create(
                 i_contract_no    = s_cont[]
                 i_comp_code      = VALUE tr_comp( (
                    low     = p_comp
                    sign    = 'I'
                    option  = 'EQ'
                 ) )
                 i_contr_type     = s_ctype[]
                 i_contract_start = s_stdat[]
                 i_first_end_dat  = s_enddat[]
               ).
      ELSE.
        go_dup_check = NEW #(
            if_contract_no = p_cont
            if_contract_type = p_ctype
         ).
      ENDIF.
  catch zcx_refx_exception INTO DATA(lo_cx_refx).
**********************************************************************
*90: Exception
**********************************************************************
      DATA(lt_msg) = zcl_refx_cn_contract_gui=>create_bapi_ret_from_exception( lo_cx_refx ).
      convert_message_to_success 'ZREFX' '026'.
      convert_message_to_success 'ZREFX' '027'.
      convert_message_to_success 'ZREFX' '029'.
      convert_message_to_success 'ZREFX' '030'.
      zcl_refx_cn_contract_gui=>display_bapi_log_gui( lt_msg )..
      leave list-processing.
  endtry.
**********************************************************************
END-OF-SELECTION.
**********************************************************************
**********************************************************************
*100: Call screen
**********************************************************************
call screen '0100'.
