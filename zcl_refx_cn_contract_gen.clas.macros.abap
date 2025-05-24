*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
**********************************************************************
*10: Macros
**********************************************************************
DEFINE define_key.
  FIELD-SYMBOLS: <fs_key&1> TYPE any.
END-OF-DEFINITION.
DEFINE assign_key.
  ASSIGN &1 TO <fs_key&2>.
END-OF-DEFINITION.
DEFINE assign_empty_keys.
  IF <fs_key&1> IS NOT ASSIGNED.
      ASSIGN space TO <fs_key&1>.
  ENDIF.
END-OF-DEFINITION.
DEFINE key_assignment.
  CASE keyid.
      WHEN 1. assign_key <fs_keys>-fieldname 1.
              assign_empty_keys 1.
      WHEN 2. assign_key <fs_keys>-fieldname 2.
              assign_empty_keys 2.
      WHEN 3. assign_key <fs_keys>-fieldname 3.
              assign_empty_keys 3.
      WHEN 4. assign_key <fs_keys>-fieldname 4.
              assign_empty_keys 4.
      WHEN 5. assign_key <fs_keys>-fieldname 5.
              assign_empty_keys 5.
      WHEN 6. assign_key <fs_keys>-fieldname 6.
              assign_empty_keys 6.
      WHEN 7. assign_key <fs_keys>-fieldname 7.
              assign_empty_keys 7.
      WHEN 8. assign_key <fs_keys>-fieldname 8.
              assign_empty_keys 8.
      WHEN 9. assign_key <fs_keys>-fieldname 9.
              assign_empty_keys 9.
  ENDCASE.
END-OF-DEFINITION.
DEFINE key_definition.
  define_key 1.
  define_key 2.
  define_key 3.
  define_key 4.
  define_key 5.
  define_key 6.
  define_key 7.
  define_key 8.
  define_key 9.
END-OF-DEFINITION.
DEFINE sort_records.
  DATA: lf_key1 TYPE fieldname,
        lf_key2 TYPE fieldname,
        lf_key3 TYPE fieldname,
        lf_key4 TYPE fieldname,
        lf_key5 TYPE fieldname,
        lf_key6 TYPE fieldname,
        lf_key7 TYPE fieldname,
        lf_key8 TYPE fieldname,
        lf_key9 TYPE fieldname.
    lf_key1 = COND #( WHEN <fs_key1> IS ASSIGNED THEN <fs_key1> ELSE space ).
    lf_key2 = COND #( WHEN <fs_key2> IS ASSIGNED THEN <fs_key2> ELSE space ).
    lf_key3 = COND #( WHEN <fs_key3> IS ASSIGNED THEN <fs_key3> ELSE space ).
    lf_key4 = COND #( WHEN <fs_key4> IS ASSIGNED THEN <fs_key4> ELSE space ).
    lf_key5 = COND #( WHEN <fs_key5> IS ASSIGNED THEN <fs_key5> ELSE space ).
    lf_key6 = COND #( WHEN <fs_key6> IS ASSIGNED THEN <fs_key6> ELSE space ).
    lf_key7 = COND #( WHEN <fs_key7> IS ASSIGNED THEN <fs_key7> ELSE space ).
    lf_key8 = COND #( WHEN <fs_key8> IS ASSIGNED THEN <fs_key8> ELSE space ).
    lf_key9 = COND #( WHEN <fs_key9> IS ASSIGNED THEN <fs_key9> ELSE space ).
    SORT &1
    ASCENDING BY
    (lf_key1)
    (lf_key2)
    (lf_key3)
    (lf_key4)
    (lf_key5)
    (lf_key6)
    (lf_key7)
    (lf_key8)
    (lf_key9)
    .
END-OF-DEFINITION.
DEFINE remove_dup_records.
  lf_key1 = COND #( WHEN <fs_key1> IS ASSIGNED THEN <fs_key1> ELSE space ).
  lf_key2 = COND #( WHEN <fs_key2> IS ASSIGNED THEN <fs_key2> ELSE space ).
  lf_key3 = COND #( WHEN <fs_key3> IS ASSIGNED THEN <fs_key3> ELSE space ).
  lf_key4 = COND #( WHEN <fs_key4> IS ASSIGNED THEN <fs_key4> ELSE space ).
  lf_key5 = COND #( WHEN <fs_key5> IS ASSIGNED THEN <fs_key5> ELSE space ).
  lf_key6 = COND #( WHEN <fs_key6> IS ASSIGNED THEN <fs_key6> ELSE space ).
  lf_key7 = COND #( WHEN <fs_key7> IS ASSIGNED THEN <fs_key7> ELSE space ).
  lf_key8 = COND #( WHEN <fs_key8> IS ASSIGNED THEN <fs_key8> ELSE space ).
  lf_key9 = COND #( WHEN <fs_key9> IS ASSIGNED THEN <fs_key9> ELSE space ).
  DELETE ADJACENT
  DUPLICATES FROM
  &1
  COMPARING
  (lf_key1)
  (lf_key2)
  (lf_key3)
  (lf_key4)
  (lf_key5)
  (lf_key6)
  (lf_key7)
  (lf_key8)
  (lf_key9)
  .
END-OF-DEFINITION.
define convert_to_fc.
CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
          EXPORTING date             = <fi_data>-budat
                    foreign_currency = <fs_curr>
                    local_amount     = &1
                    local_currency   = <fs_trans_curr>
                    type_of_rate     = <fi_data>-kuty2
                    read_tcurr       = abap_true
          IMPORTING exchange_rate    = <fs_exch_rate>
                    foreign_amount   = &1.
end-of-definition.
