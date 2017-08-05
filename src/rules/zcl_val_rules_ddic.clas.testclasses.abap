CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      test_is_value_in_domain FOR TESTING,
      test_is_value_in_table FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_is_value_in_domain.
    TYPES: lty_msgty_tab TYPE STANDARD TABLE OF msgty WITH DEFAULT KEY.
    CONSTANTS: lc_domname  TYPE domname VALUE 'MSGAR',
               lc_domname2 TYPE domname VALUE 'MANDT'.
    FIELD-SYMBOLS: <lv_msgty> TYPE msgty.

    DATA(lt_correct) = VALUE lty_msgty_tab(
      ( 'S' ) ( 'I' ) ( 'A' ) ( 'E' ) ( 'W' ) " 'X' is not in there?!
    ).
    DATA(lt_incorrect) = VALUE lty_msgty_tab(
      ( 'X' ) ( 'U' ) ( 'M' ) ( 'B' ) ( space )
    ).

    LOOP AT lt_correct ASSIGNING <lv_msgty>.
      cl_abap_unit_assert=>assert_true( zcl_val_rules_ddic=>is_value_in_domain(
        iv_domname      = lc_domname
        iv_value        = <lv_msgty>
        iv_check_fixed  = abap_true
        iv_check_table  = abap_false
      ) ).
    ENDLOOP.
    UNASSIGN <lv_msgty>.

    LOOP AT lt_incorrect ASSIGNING <lv_msgty>.
      cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
        iv_domname      = lc_domname
        iv_value        = <lv_msgty>
        iv_check_fixed  = abap_true
        iv_check_table  = abap_false
      ) ).
    ENDLOOP.
    UNASSIGN <lv_msgty>.

    LOOP AT lt_correct ASSIGNING <lv_msgty>.
      cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
        iv_domname      = lc_domname
        iv_value        = <lv_msgty>
        iv_check_fixed  = abap_false
        iv_check_table  = abap_true
      ) ).
    ENDLOOP.
    UNASSIGN <lv_msgty>.

    LOOP AT lt_incorrect ASSIGNING <lv_msgty>.
      cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
        iv_domname      = lc_domname
        iv_value        = <lv_msgty>
        iv_check_fixed  = abap_false
        iv_check_table  = abap_true
      ) ).
    ENDLOOP.
    UNASSIGN <lv_msgty>.

    cl_abap_unit_assert=>assert_true( zcl_val_rules_ddic=>is_value_in_domain(
      iv_domname      = lc_domname2
      iv_value        = cl_abap_syst=>get_client( )
      iv_check_fixed  = abap_false
      iv_check_table  = abap_true
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
      iv_domname      = lc_domname2
      iv_value        = space
      iv_check_fixed  = abap_false
      iv_check_table  = abap_true
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
      iv_domname      = lc_domname2
      iv_value        = cl_abap_syst=>get_client( )
      iv_check_fixed  = abap_false
      iv_check_table  = abap_false
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_domain(
      iv_domname      = lc_domname2
      iv_value        = cl_abap_syst=>get_client( )
      iv_check_fixed  = abap_true
      iv_check_table  = abap_false
    ) ).
  ENDMETHOD.

  METHOD test_is_value_in_table.
    CONSTANTS: lc_tabname TYPE tabname VALUE 'T000',
               lc_column  TYPE fieldname VALUE 'MANDT'.

    cl_abap_unit_assert=>assert_true( zcl_val_rules_ddic=>is_value_in_table(
      iv_tabname = lc_tabname
      iv_value   = cl_abap_syst=>get_client( )
      iv_column  = lc_column
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_table(
      iv_tabname = lc_tabname
      iv_value   = '---'
      iv_column  = lc_column
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_table(
      iv_tabname = lc_tabname
      iv_value   = space
      iv_column  = lc_column
    ) ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_ddic=>is_value_in_table(
      iv_tabname = lc_tabname
      iv_value   = `' OR mandt = @sy-mandt `
      iv_column  = lc_column
    ) ).
  ENDMETHOD.
ENDCLASS.
