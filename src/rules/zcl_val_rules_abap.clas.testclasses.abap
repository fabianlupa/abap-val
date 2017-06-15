CLASS lcl_dummy DEFINITION.
ENDCLASS.

CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    METHODS:
      test_check_initial FOR TESTING,
      test_check_bound FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_check_bound.
    DATA: lv_string TYPE string,
          lv_int    TYPE i,
          lr_ref    TYPE REF TO data,
          lo_obj    TYPE REF TO object.

    TRY.
        cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_bound( REF #( lv_string ) ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_val_illegal_argument ##NO_HANDLER.
    ENDTRY.
    TRY.
        cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_bound( REF #( lv_int ) ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_val_illegal_argument ##NO_HANDLER.
    ENDTRY.
    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_bound( REF #( lr_ref ) ) ).
    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_bound( REF #( lo_obj ) ) ).

    lr_ref = REF #( lv_string ).
    lo_obj = NEW lcl_dummy( ).

    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_bound( REF #( lr_ref ) ) ).
    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_bound( REF #( lo_obj ) ) ).
  ENDMETHOD.

  METHOD test_check_initial.
    DATA: lv_string TYPE string,
          lv_int    TYPE i,
          lr_ref    TYPE REF TO data,
          lo_obj    TYPE REF TO object.

    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_initial( REF #( lv_string ) ) ).
    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_initial( REF #( lv_int ) ) ).
    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_initial( REF #( lr_ref ) ) ).
    cl_abap_unit_assert=>assert_true( zcl_val_rules_abap=>check_initial( REF #( lo_obj ) ) ).

    lv_string = ` Test `.
    lv_int = 200.
    lr_ref = REF #( lv_string ).
    lo_obj = NEW lcl_dummy( ).

    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_initial( REF #( lv_string ) ) ).
    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_initial( REF #( lv_int ) ) ).
    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_initial( REF #( lr_ref ) ) ).
    cl_abap_unit_assert=>assert_false( zcl_val_rules_abap=>check_initial( REF #( lo_obj ) ) ).
  ENDMETHOD.
ENDCLASS.
