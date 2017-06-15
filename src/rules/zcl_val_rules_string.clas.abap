"! String based validations
CLASS zcl_val_rules_string DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_val_rules_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Check for string length
      "! @parameter iv_min | Minimal length
      "! @parameter iv_max | Maximal length
      "! @parameter ro_config | Configuration instance
      new_length IMPORTING iv_min           TYPE i DEFAULT -1
                           iv_max           TYPE i DEFAULT -1
                 RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a string matches a regular expression
      "! @parameter io_regex | Regex
      "! @parameter ro_config | Configuration instance
      "! @raising zcx_val_argument_null | <em>io_regex</em> cannot be null
      new_regex IMPORTING io_regex         TYPE REF TO cl_abap_regex
                RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config
                RAISING   zcx_val_argument_null.
  PROTECTED SECTION.
    METHODS:
      validate_internal REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      gty_type TYPE string.
    CONSTANTS:
      BEGIN OF gc_types,
        length TYPE gty_type VALUE 'LENGTH',
        regex  TYPE gty_type VALUE 'REGEX',
      END OF gc_types.
ENDCLASS.



CLASS zcl_val_rules_string IMPLEMENTATION.
  METHOD new_length.
    ro_config = NEW lcl_config( iv_type       = gc_types-length
                                iv_max_length = iv_max
                                iv_min_length = iv_min ).
  ENDMETHOD.

  METHOD new_regex.
    IF io_regex IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_val_argument_null
        EXPORTING
          iv_variable_name = 'IO_REGEX'.
    ENDIF.

    ro_config = NEW lcl_config( iv_type  = gc_types-regex
                                io_regex = io_regex ).
  ENDMETHOD.

  METHOD validate_internal.
    DATA: lv_dummy TYPE string ##NEEDED.

    rv_valid = abap_true.

    DATA(lo_config) = CAST lcl_config( io_config ).

    DATA(lv_string) = zcl_val_tools=>get_ref_as_string( ir_ref ).

    CASE lo_config->mv_type.
      WHEN gc_types-length.
        DATA(lv_length) = strlen( lv_string ).

        IF lo_config->mv_min_length >= 0.
          IF lv_length < lo_config->mv_min_length.
            MESSAGE e005(zval) WITH iv_name lv_length lo_config->mv_min_length INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ENDIF.

        IF lo_config->mv_max_length >= 0.
          IF lv_length > lo_config->mv_max_length.
            MESSAGE e006(zval) WITH iv_name lv_length lo_config->mv_max_length INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ENDIF.

      WHEN gc_types-regex.
        rv_valid = lo_config->mo_regex->create_matcher( text = lv_string )->match( ).
        IF rv_valid = abap_false.
          MESSAGE e022(zval) WITH iv_name lv_string lo_config->mo_regex->pattern INTO lv_dummy.
          APPEND prepare_message( ir_ref ) TO et_messages.
          rv_valid = abap_false.
        ENDIF.

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
