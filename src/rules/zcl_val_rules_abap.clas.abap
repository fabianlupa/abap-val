"! ABAP language validations
CLASS zcl_val_rules_abap DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_val_rules_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      gty_range_any TYPE RANGE OF string.
    CLASS-METHODS:
      "! Check if a data object is initial
      "! @parameter ro_config | Configuration instance
      new_initial RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a data object is not initial
      "! @parameter ro_config | Configuration instance
      new_not_initial RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a object reference is bound
      "! @parameter ro_config | Configuration instance
      new_bound RETURNING VALUE(ro_config)   TYPE REF TO zcl_val_rules_config,
      "! Check if a object reference is not bound
      "! @parameter ro_config | Configuration instance
      new_not_bound RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a data object's value is within a value range
      "! @parameter it_range | Range
      "! @parameter ro_config | Configuration instance
      new_in_range IMPORTING it_range         TYPE gty_range_any
                   RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a data object's value is not within a value range
      "! @parameter it_range | Range
      "! @parameter ro_config | Configuration instance
      new_not_in_range IMPORTING it_range         TYPE gty_range_any
                       RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      check_bound IMPORTING ir_ref             TYPE REF TO data
                  RETURNING VALUE(rv_is_bound) TYPE abap_bool
                  RAISING   zcx_val_illegal_argument,
      check_initial IMPORTING ir_ref               TYPE REF TO data
                    RETURNING VALUE(rv_is_initial) TYPE abap_bool.
  PROTECTED SECTION.
    METHODS:
      validate_internal REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      gty_type TYPE string.
    CONSTANTS:
      BEGIN OF gc_types,
        range   TYPE gty_type VALUE 'RANGE',
        bound   TYPE gty_type VALUE 'BOUND',
        initial TYPE gty_type VALUE 'INITIAL',
      END OF gc_types.
ENDCLASS.



CLASS zcl_val_rules_abap IMPLEMENTATION.
  METHOD check_bound.
    FIELD-SYMBOLS: <lg_data> TYPE data,
                   <lo_obj>  TYPE REF TO object.

    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ir_ref ).

    CASE lo_descr->kind.
      WHEN cl_abap_typedescr=>kind_class OR cl_abap_typedescr=>kind_intf.
        ASSIGN ir_ref->* TO <lo_obj>.
        rv_is_bound = boolc( <lo_obj> IS BOUND ).

      WHEN cl_abap_typedescr=>kind_ref.
        ASSIGN ir_ref->* TO <lg_data>.
        rv_is_bound = boolc( <lg_data> IS BOUND ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_val_illegal_argument
          EXPORTING
            iv_reason = 'Argument does not point to a data or object reference'.
    ENDCASE.
  ENDMETHOD.

  METHOD check_initial.
    ASSIGN ir_ref->* TO FIELD-SYMBOL(<lg_value>).
    ASSERT <lg_value> IS ASSIGNED.
    rv_is_initial = boolc( <lg_value> IS INITIAL ).
  ENDMETHOD.

  METHOD new_bound.
    ro_config = NEW lcl_config( iv_type            = gc_types-bound
                                iv_not_bound_valid = abap_false ).
  ENDMETHOD.

  METHOD new_not_bound.
    ro_config = NEW lcl_config( iv_type            = gc_types-bound
                                iv_not_bound_valid = abap_true ).
  ENDMETHOD.

  METHOD new_initial.
    ro_config = NEW lcl_config( iv_type          = gc_types-initial
                                iv_initial_valid = abap_true ).
  ENDMETHOD.

  METHOD new_not_initial.
    ro_config = NEW lcl_config( iv_type          = gc_types-initial
                                iv_initial_valid = abap_false ).
  ENDMETHOD.

  METHOD new_in_range.
    ro_config = NEW lcl_config( iv_type               = gc_types-range
                                it_range              = it_range
                                iv_not_in_range_valid = abap_false ).
  ENDMETHOD.

  METHOD new_not_in_range.
    ro_config = NEW lcl_config( iv_type               = gc_types-range
                                it_range              = it_range
                                iv_not_in_range_valid = abap_false ).
  ENDMETHOD.

  METHOD validate_internal.
    DATA: lv_dummy  TYPE string ##NEEDED,
          lo_object TYPE REF TO object.
    FIELD-SYMBOLS: <lg_ref> TYPE any.

    rv_valid = abap_true.

    DATA(lo_config) = CAST lcl_config( io_config ).

    ASSIGN ir_ref->* TO <lg_ref>.
    ASSERT <lg_ref> IS ASSIGNED.

    CASE lo_config->mv_type.
      WHEN gc_types-initial.
        IF <lg_ref> IS INITIAL.
          IF lo_config->mv_initial_valid = abap_false.
            MESSAGE e015(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ELSE.
          IF lo_config->mv_initial_valid = abap_true.
            MESSAGE e016(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ENDIF.

      WHEN gc_types-bound.
        TRY.
            DATA(lo_descr) = CAST cl_abap_refdescr(
                               cl_abap_typedescr=>describe_by_data( <lg_ref> )
                             )->get_referenced_type( ).
            IF lo_descr->kind <> cl_abap_typedescr=>kind_class AND
               lo_descr->kind <> cl_abap_typedescr=>kind_intf.

              RAISE EXCEPTION TYPE zcx_val_unsupported_operation
                EXPORTING
                  is_textid = zcx_val_unsupported_operation=>gc_non_objref_unsupported.
            ENDIF.

          CATCH cx_sy_move_cast_error INTO DATA(lx_ex).
            RAISE EXCEPTION TYPE zcx_val_unsupported_operation
              EXPORTING
                is_textid   = zcx_val_unsupported_operation=>gc_non_objref_unsupported
                ix_previous = lx_ex.
        ENDTRY.

        lo_object = <lg_ref>.
        IF lo_object IS NOT BOUND.
          IF lo_config->mv_not_bound_valid = abap_false.
            MESSAGE e017(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ELSE.
          IF lo_config->mv_not_bound_valid = abap_true.
            MESSAGE e018(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ENDIF.

      WHEN gc_types-range.
        IF <lg_ref> NOT IN lo_config->mt_range.
          IF lo_config->mv_not_in_range_valid = abap_false.
            MESSAGE e019(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ELSE.
          IF lo_config->mv_not_in_range_valid = abap_true.
            MESSAGE e020(zval) WITH iv_name INTO lv_dummy.
            APPEND prepare_message( ir_ref ) TO et_messages.
            rv_valid = abap_false.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
