"! Validation rule for validatable classes
CLASS zcl_val_rules_self DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_val_rules_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Check if an object is valid by using its own validation logic
      "! @parameter ro_config | Configuration instance
      new RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config.
  PROTECTED SECTION.
    METHODS:
      validate_internal REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_val_rules_self IMPLEMENTATION.
  METHOD new.
    DATA: lo_dummy TYPE REF TO zcl_val_rules_self ##NEEDED.
    ro_config = NEW #( CAST #( CAST cl_abap_refdescr(
                                  cl_abap_typedescr=>describe_by_data( lo_dummy )
                               )->get_referenced_type( ) ) ).
  ENDMETHOD.

  METHOD validate_internal.
    DATA: li_validatable TYPE REF TO zif_val_validatable.

    DATA(lv_intf_name) = CAST cl_abap_refdescr(
                           cl_abap_typedescr=>describe_by_data( li_validatable )
                         )->get_referenced_type(
                         )->get_relative_name( ).

   TRY.
        DATA(lo_descr) = CAST cl_abap_classdescr(
                           CAST cl_abap_refdescr(
                             cl_abap_typedescr=>describe_by_data_ref( ir_ref )
                           )->get_referenced_type( )
                         ).
      CATCH cx_sy_move_cast_error INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_val_unsupported_operation
          EXPORTING
            ix_previous = lx_ex
            is_textid   = zcx_val_unsupported_operation=>gc_non_objref_unsupported.
    ENDTRY.

    IF NOT line_exists( lo_descr->interfaces[ name = lv_intf_name ] ).
      RAISE EXCEPTION TYPE zcx_val_unsupported_operation
        EXPORTING
          is_textid = zcx_val_unsupported_operation=>gc_not_self_validatable.
    ENDIF.

    ASSIGN ir_ref->* TO FIELD-SYMBOL(<lg_ref>).

    IF <lg_ref> IS NOT BOUND.
      rv_valid = abap_true.
    ELSE.
      li_validatable ?= <lg_ref>.
      rv_valid = li_validatable->validate(
*        EXPORTING
*          io_mode     = io_mode
        IMPORTING
          et_messages = et_messages
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
