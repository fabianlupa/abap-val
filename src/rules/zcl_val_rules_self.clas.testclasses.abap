CLASS lcl_dummy DEFINITION.
ENDCLASS.

CLASS lcl_dummy_val DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      zif_val_validatable.
    DATA:
      mv_valid TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dummy_val IMPLEMENTATION.
  METHOD zif_val_validatable~validate.
    rv_valid = mv_valid.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    METHODS:
      test_unsupported FOR TESTING,
      test_null FOR TESTING,
      test_validation FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_rule   TYPE REF TO zcl_val_rules_self,
      mo_config TYPE REF TO zcl_val_rules_config.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    mo_rule = NEW #( ).
    mo_config = zcl_val_rules_self=>new( ).
  ENDMETHOD.

  METHOD teardown.
    FREE: mo_rule, mo_config.
  ENDMETHOD.

  METHOD test_null.
    DATA: lo_null  TYPE REF TO lcl_dummy ##NEEDED,
          lo_null2 TYPE REF TO lcl_dummy_val ##NEEDED.

    TRY.
        mo_rule->validate( ir_ref = REF #( lo_null ) io_config = mo_config ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_val_unsupported_operation ##NO_HANDLER.
    ENDTRY.
    cl_abap_unit_assert=>assert_true(
      mo_rule->validate( ir_ref = REF #( lo_null2 ) io_config = mo_config )
    ).
  ENDMETHOD.

  METHOD test_validation.
    DATA(lo_val) = NEW lcl_dummy_val( ).

    lo_val->mv_valid = abap_false.
    cl_abap_unit_assert=>assert_false(
      mo_rule->validate( ir_ref = REF #( lo_val ) io_config = mo_config )
    ).

    lo_val->mv_valid = abap_true.
    cl_abap_unit_assert=>assert_true(
      mo_rule->validate( ir_ref = REF #( lo_val ) io_config = mo_config )
    ).
  ENDMETHOD.

  METHOD test_unsupported.
    TYPES: lty_object_ref TYPE REF TO object.
    DATA: lv_data        TYPE i,
          lr_data_ref    TYPE REF TO i,
          lo_object      TYPE REF TO object,
          lr_obj_ref     TYPE REF TO lty_object_ref,
          lr_initial_ref TYPE REF TO data ##NEEDED.

    lr_data_ref = REF #( lv_data ).
    lo_object = NEW lcl_dummy( ).
    lr_obj_ref = REF #( lo_object ).

    DO 3 TIMES.
      DATA(lv_index) = sy-index.
      TRY.
          CASE lv_index.
            WHEN 1.
              mo_rule->validate( ir_ref = lr_data_ref io_config = mo_config ).
              cl_abap_unit_assert=>fail( ).
            WHEN 2.
              mo_rule->validate( ir_ref = lr_obj_ref io_config = mo_config ).
              cl_abap_unit_assert=>fail( ).
            WHEN 3.
              mo_rule->validate( ir_ref = lr_initial_ref io_config = mo_config ).
              cl_abap_unit_assert=>fail( ).
          ENDCASE.
        CATCH zcx_val_unsupported_operation.
          IF lv_index = 3.
            cl_abap_unit_assert=>fail( ).
          ENDIF.
        CATCH zcx_val_argument_null.
          IF lv_index <> 3.
            cl_abap_unit_assert=>fail( ).
          ENDIF.
      ENDTRY.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
