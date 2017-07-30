CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    METHODS:
      test_input FOR TESTING,
      test_length FOR TESTING,
      test_regex FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_rule TYPE REF TO zcl_val_rules_string.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_input.
    DATA: lv_string   TYPE string,
          lv_char     TYPE c LENGTH 10,
          lv_int      TYPE i,
          lv_float    TYPE f,
          lv_decfloat TYPE decfloat34,
          lv_p        TYPE p LENGTH 3 DECIMALS 1,
          lt_tab      TYPE stringtab,
          BEGIN OF ls_struct,
            comp TYPE string,
          END OF ls_struct,
          lr_ref TYPE REF TO data,
          lo_ref TYPE REF TO object.

    DATA(lo_config) = zcl_val_rules_string=>new_length( ).

    mo_rule->validate( ir_ref = REF #( lv_string ) io_config = lo_config ).
    mo_rule->validate( ir_ref = REF #( lv_char ) io_config = lo_config ).
    mo_rule->validate( ir_ref = REF #( lv_int ) io_config = lo_config ).
    mo_rule->validate( ir_ref = REF #( lv_float ) io_config = lo_config ).
    mo_rule->validate( ir_ref = REF #( lv_decfloat ) io_config = lo_config ).
    mo_rule->validate( ir_ref = REF #( lv_p ) io_config = lo_config ).

    TRY.
        mo_rule->validate( ir_ref = REF #( lt_tab ) io_config = lo_config ).
      CATCH zcx_val_unsupported_operation ##NO_HANDLER.
    ENDTRY.

    TRY.
        mo_rule->validate( ir_ref = REF #( ls_struct ) io_config = lo_config ).
      CATCH zcx_val_unsupported_operation ##NO_HANDLER.
    ENDTRY.

    TRY.
        mo_rule->validate( ir_ref = REF #( lr_ref ) io_config = lo_config ).
      CATCH zcx_val_unsupported_operation ##NO_HANDLER.
    ENDTRY.

    TRY.
        mo_rule->validate( ir_ref = REF #( lo_ref ) io_config = lo_config ).
      CATCH zcx_val_unsupported_operation ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_length.
    DATA: lv_string TYPE string VALUE `This is a string with 36 characters.`,
          lv_empty  TYPE string VALUE ``,
          lv_space  TYPE string VALUE ` `,
          lv_result TYPE abap_bool.

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 10 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 36 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = -5 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 10
                                                                                 iv_max = 40 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_empty )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 0
                                                                                 iv_max = 40 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_empty )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 0
                                                                                 iv_max = 0 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_space )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 0
                                                                                 iv_max = 1 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_space )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 1
                                                                                 iv_max = 1 ) ).
    cl_abap_unit_assert=>assert_true( lv_result ).

    lv_result = mo_rule->validate( ir_ref     = REF #( lv_string )
                                    io_config = zcl_val_rules_string=>new_length( iv_min = 37 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length(
                                                 iv_min = cl_abap_math=>max_int4
                                               )
                                   ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_string )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 10
                                                                                 iv_max = 30 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_empty )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 1
                                                                                 iv_max = 40 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_empty )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 1
                                                                                 iv_max = 1 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_space )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 0
                                                                                 iv_max = 0 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).

    lv_result = mo_rule->validate( ir_ref    = REF #( lv_space )
                                   io_config = zcl_val_rules_string=>new_length( iv_min = 2
                                                                                 iv_max = 3 ) ).
    cl_abap_unit_assert=>assert_false( lv_result ).
  ENDMETHOD.

  METHOD test_regex.
    CONSTANTS: lc_pattern TYPE string VALUE `[\w ]+\.`.
    DATA: lv_string_1   TYPE string VALUE `This is a sentence.`,
          lv_string_2 TYPE string VALUE `This is not a sentence`.

    cl_abap_unit_assert=>assert_true(
      mo_rule->validate( ir_ref    = REF #( lv_string_1 )
                         io_config = zcl_val_rules_string=>new_regex( NEW #( lc_pattern ) ) )
    ).

    cl_abap_unit_assert=>assert_false(
      mo_rule->validate( ir_ref    = REF #( lv_string_2 )
                         io_config = zcl_val_rules_string=>new_regex( NEW #( lc_pattern ) ) )
    ).
  ENDMETHOD.

  METHOD setup.
    mo_rule = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_rule.
  ENDMETHOD.
ENDCLASS.
