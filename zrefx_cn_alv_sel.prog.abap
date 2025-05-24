*&---------------------------------------------------------------------*
*& Include          ZREFX_CN_ALV_SEL
*&---------------------------------------------------------------------*
**********************************************************************
*10: Selection Field: File Name
**********************************************************************
selection-screen begin of block b1 with frame title blk_ttl.
  selection-screen begin of line.
    selection-screen comment 01(4) logo1.
    selection-screen comment 05(30) title1 .
    parameters: p_file like rlgrap-filename modif id upl .
  selection-screen end of line.
**********************************************************************
*20: Create - mode selection
**********************************************************************
selection-screen begin of line.
    parameters: p_cr RADIOBUTTON GROUP r1.
    selection-screen comment 03(4) logo4.
    selection-screen comment 07(30) title4.
selection-screen end of line.
**********************************************************************
*20: Change - mode selection
**********************************************************************
selection-screen begin of line.
    parameters: p_ch RADIOBUTTON GROUP r1 default 'X'.
    selection-screen comment 03(4) logo5.
    selection-screen comment 07(30) title5.
selection-screen end of line.
**********************************************************************
*20: Comment
**********************************************************************
selection-screen begin of line.
  selection-screen comment 01(50) lf_comm1.
selection-screen end of line.
selection-screen begin of line.
  selection-screen comment 01(70) lf_comm2.
selection-screen end of line.
selection-screen begin of line.
  selection-screen comment 01(70) lf_comm3.
selection-screen end of line.
selection-screen begin of line.
  selection-screen comment 01(70) lf_comm4.
selection-screen end of line.
selection-screen begin of line.
  selection-screen comment 01(70) lf_comm5.
selection-screen end of line.
selection-screen begin of line.
  selection-screen comment 01(70) lf_comm6.
selection-screen end of line.
**********************************************************************
*30: Selection Field: Layout
**********************************************************************
selection-screen begin of line.
    parameters: p_basic type flag AS CHECKBOX DEFAULT ABAP_TRUE modif id sel.
    selection-screen comment 03(4) logo2.
    selection-screen comment 07(30) title2 .
selection-screen end of line.
selection-screen begin of line.
    parameters: p_cond type flag AS CHECKBOX DEFAULT ABAP_TRUE modif id sel.
    selection-screen comment 03(4) logo3.
    selection-screen comment 07(30) title3 .
selection-screen end of line.
selection-screen end of block b1.
