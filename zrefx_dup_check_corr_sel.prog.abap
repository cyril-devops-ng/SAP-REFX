*&---------------------------------------------------------------------*
*& Include          ZREFX_DUP_CHECK_CORR_SEL
*&---------------------------------------------------------------------*
selection-screen begin of block ba with frame title tta.
    parameters: p_r_dchk RADIOBUTTON GROUP r1 USER-COMMAND ca,
                p_r_cin RADIOBUTTON GROUP r1,
                p_r_cout RADIOBUTTON GROUP r1.
selection-screen end of block ba.
selection-screen begin of block ad with frame title ttd.
    selection-screen begin of line.
        selection-screen comment 01(4) logo2 modif id buk.
        selection-screen comment 05(23) title2 modif id buk.
        parameters: p_comp type t_output-comp_code DEFAULT 'NG01' OBLIGATORY  MODIF ID buk.
    selection-screen end of line.
selection-screen end of block ad.
selection-screen begin of block b1 with frame title tt1.
    selection-screen begin of line.
        selection-screen comment 01(4) logo1 modif id dc.
        selection-screen comment 05(20) title1 modif id dc .
        select-options: s_cont for gs_output-contract_no modif id dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo3 MODIF ID dc.
        selection-screen comment 05(20) title3 MODIF ID dc.
        select-options: s_ctype for gs_output-contr_type MODIF ID dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo4 MODIF ID dc.
        selection-screen comment 05(20) title4 MODIF ID dc.
        select-options: s_stdat for gs_output-cont_st_dat MODIF ID dc.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo5 MODIF ID dc.
        selection-screen comment 05(20) title5 MODIF ID dc.
        select-options: s_enddat for gs_output-frst_end_dat MODIF ID dc.
    selection-screen end of line.
selection-screen end of block b1.
selection-screen begin of block b2 with frame title tt2.
    selection-screen begin of line.
        selection-screen comment 01(4) logo6 modif id chk.
        selection-screen comment 05(23) title6 modif id chk.
        parameters: p_cont type recnnumber modif id chk.
    selection-screen end of line.
    selection-screen begin of line.
        selection-screen comment 01(4) logo7 modif id chk.
        selection-screen comment 05(23) title7 modif id chk.
        parameters: p_ctype type recncontracttype as LISTBOX VISIBLE LENGTH 30 modif id chk DEFAULT 'A001' .
    selection-screen end of line.
selection-screen end of block b2.
