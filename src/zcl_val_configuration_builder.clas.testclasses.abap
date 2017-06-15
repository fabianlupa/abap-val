CLASS lcl_dummy DEFINITION.
  PUBLIC SECTION.
    DATA:
      mv_field_a TYPE string READ-ONLY,
      mv_field_b TYPE i READ-ONLY,
      mv_field_c TYPE char5 READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_test DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    METHODS:
      test_builder FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_builder.
    DATA: lv_xml_a TYPE string,
          lv_xml_b TYPE string.
    DATA(lo_dummy) = NEW lcl_dummy( ).

    DATA(lt_conf_a) = zcl_val_configuration_builder=>get(
      )->add_field( REF #( lo_dummy->mv_field_a )
        )->set_name( 'FIELD_A'
        )->add_rule( zcl_val_rules_string=>new_length( iv_min = 5 )
        )->end(
      )->add_field(
        )->set_field( REF #( lo_dummy->mv_field_b )
        )->add_rule( zcl_val_rules_abap=>new_not_initial( )
        )->end(
      )->add_field( REF #( lo_dummy->mv_field_c )
        )->add_rule( zcl_val_rules_abap=>new_initial( )
        )->end(
      )->build( ).

    DATA(lt_conf_b) = VALUE zcl_val_validator=>gty_validation_tab(
      ( ref   = REF #( lo_dummy->mv_field_a )
        name  = 'FIELD_A'
        rules = VALUE #(
          ( zcl_val_rules_string=>new_length( iv_min = 5 ) )
        ) )
      ( ref   = REF #( lo_dummy->mv_field_b )
        rules = VALUE #(
          ( zcl_val_rules_abap=>new_not_initial( ) )
        ) )
      ( ref   = REF #( lo_dummy->mv_field_c )
        rules = VALUE #(
          ( zcl_val_rules_abap=>new_initial( ) )
        ) )
    ).

    CALL TRANSFORMATION id: SOURCE data = lt_conf_a
                            RESULT XML lv_xml_a
                            OPTIONS data_refs = 'heap-or-create',
                            SOURCE data = lt_conf_b
                            RESULT XML lv_xml_b
                            OPTIONS data_refs = 'heap-or-create'.

    REPLACE ALL OCCURRENCES OF REGEX `(id|href)="[^"]+"` IN: lv_xml_a WITH space,
                                                             lv_xml_b WITH space.

    cl_abap_unit_assert=>assert_equals( exp = lv_xml_b act = lv_xml_a ).
  ENDMETHOD.
ENDCLASS.
