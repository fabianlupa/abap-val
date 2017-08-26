"! Static helper methods
CLASS zcl_val_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Get the value of referenced elementary typed data objects as string
      "! @parameter ir_ref | Data reference (elementary type)
      "! @parameter rv_string | String value
      "! @raising zcx_val_unsupported_operation | <em>ir_ref</em> does not point to a charlike data
      "!                                          object
      "! @raising zcx_val_argument_null | <em>ir_ref</em> is not bound
      get_ref_as_string IMPORTING ir_ref           TYPE REF TO data
                        RETURNING VALUE(rv_string) TYPE string
                        RAISING   zcx_val_unsupported_operation
                                  zcx_val_argument_null.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_val_tools IMPLEMENTATION.
  METHOD get_ref_as_string.
    FIELD-SYMBOLS: <lg_string> TYPE any.

    IF ir_ref IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_val_argument_null
        EXPORTING
          iv_variable_name = 'IR_REF'.
    ENDIF.

    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ir_ref ).
    IF lo_descr->kind <> cl_abap_typedescr=>kind_elem.
      RAISE EXCEPTION TYPE zcx_val_unsupported_operation
        EXPORTING
          is_textid = zcx_val_unsupported_operation=>gc_non_charlike_unsupported.
    ENDIF.

    ASSIGN ir_ref->* TO <lg_string>.
    ASSERT <lg_string> IS ASSIGNED.

    rv_string = CONV #( <lg_string> ).
  ENDMETHOD.
ENDCLASS.
