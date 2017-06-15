"! Rule base class
"! <p>
"! This is the base class for a validation rule. A rule can be applied to a data object by calling
"! <em>validate</em> with a data reference and a rule configuration which returns wether the data
"! object is valid by the validation logic of the rule and the used configuration as well as
"! one or multiple messages.<br/>
"! The used configuration is an instance of <em>ZCL_VAL_RULES_CONFIG</em>. These should be retrieved
"! by using the static <em>new_...</em> methods of a concrete rule class. It is recommended
"! to use <em>ZCL_VAL_VALIDATOR</em> to apply rule configurations on data objects.
"! </p>
"! <strong>Subclassing</strong>
"! <p>
"! To implement a validation rule create a subclass of this abstract base class and implement
"! <em>validate_internal</em>. You can use <em>prepare_message</em> to format error messages for the
"! exporting parameter. Implement one or more factory methods to retrieve a configuration instance
"! of <em>ZCL_VAL_RULES_CONFIG</em>. Either use that class directly or subclass it (preferably in
"! a local class).
"! </p>
CLASS zcl_val_rules_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_serializable_object.
    METHODS:
      "! Validate a data object using this rule
      "! @parameter ir_ref | Reference to data object
      "! @parameter io_config | Validation configuration
      "! @parameter et_messages | Messages
      "! @parameter rv_valid | Data object is valid
      "! @raising zcx_val_unsupported_operation | Data object is not supported by this rule
      "! @raising zcx_val_argument_null | <em>ir_ref</em> and <em>io_config</em> cannot be null
      validate FINAL IMPORTING ir_ref          TYPE REF TO data
                               io_config       TYPE REF TO zcl_val_rules_config
                     EXPORTING et_messages     TYPE zval_t_messages
                     RETURNING VALUE(rv_valid) TYPE abap_bool
                     RAISING   zcx_val_unsupported_operation
                               zcx_val_argument_null.
  PROTECTED SECTION.
    CLASS-METHODS:
      "! Prepare a message
      "! @parameter iv_msgid | Message id
      "! @parameter iv_msgno | Message number
      "! @parameter iv_msgv1 | Message variable 1
      "! @parameter iv_msgv2 | Message variable 2
      "! @parameter iv_msgv3 | Message variable 3
      "! @parameter iv_msgv4 | Message variable 4
      "! @parameter ir_ref | Reference to data object
      "! @parameter rs_message | Prepared message
      prepare_message IMPORTING VALUE(iv_msgid)   TYPE syst_msgid DEFAULT sy-msgid
                                VALUE(iv_msgno)   TYPE syst_msgno DEFAULT sy-msgno
                                VALUE(iv_msgv1)   TYPE syst_msgv DEFAULT sy-msgv1
                                VALUE(iv_msgv2)   TYPE syst_msgv DEFAULT sy-msgv2
                                VALUE(iv_msgv3)   TYPE syst_msgv DEFAULT sy-msgv3
                                VALUE(iv_msgv4)   TYPE syst_msgv DEFAULT sy-msgv4
                                ir_ref            TYPE REF TO data
                      RETURNING VALUE(rs_message) TYPE zval_s_message.
    METHODS:
      "! Redefine this method in subclass for concrete validation logic
      "! <p>
      "! It is assured that <em>ir_ref</em> is not initial and bound and <em>io_config</em> is
      "! bound.
      "! </p>
      "! @parameter ir_ref | Reference to data object
      "! @parameter io_config | Validation configuration
      "! @parameter iv_name | Localized name of the data object
      "! @parameter et_messages | Messages
      "! @parameter rv_valid | Data object is valid
      "! @raising zcx_val_unsupported_operation | Data object is not supported by this rule
      validate_internal ABSTRACT IMPORTING ir_ref          TYPE REF TO data
                                           io_config       TYPE REF TO zcl_val_rules_config
                                           iv_name         TYPE string
                                 EXPORTING et_messages     TYPE zval_t_messages
                                 RETURNING VALUE(rv_valid) TYPE abap_bool
                                 RAISING   zcx_val_unsupported_operation.
  PRIVATE SECTION.
    CLASS-METHODS:
      get_data_object_name IMPORTING io_config      TYPE REF TO zcl_val_rules_config
                                     ir_ref         TYPE REF TO data
                           RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.



CLASS ZCL_VAL_RULES_BASE IMPLEMENTATION.


  METHOD get_data_object_name.
    IF io_config->mv_data_object_name IS NOT INITIAL.
      rv_name = io_config->mv_data_object_name.
      RETURN.
    ENDIF.

    " Determine variable's descriptive name by DDIC
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ir_ref ).

    CASE lo_descr->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( lo_descr ).
        IF lo_elem_descr->is_ddic_type( ) = abap_true.
          rv_name = lo_elem_descr->get_ddic_field( )-scrtext_s.
        ELSE.
          rv_name = 'Data element'(001).
        ENDIF.

      WHEN cl_abap_typedescr=>kind_struct.
        rv_name = 'Structure'(002).

      WHEN cl_abap_typedescr=>kind_table.
        rv_name = 'Table'(003).

      WHEN cl_abap_typedescr=>kind_class OR cl_abap_typedescr=>kind_intf.
        rv_name = 'Object'(004).

      WHEN cl_abap_typedescr=>kind_ref.
        rv_name = 'Reference'(005).

      WHEN OTHERS.
        rv_name = '???'.

    ENDCASE.
  ENDMETHOD.


  METHOD prepare_message.
    rs_message = VALUE #( msgid = iv_msgid
                          msgno = iv_msgno
                          msgv1 = iv_msgv1
                          msgv2 = iv_msgv2
                          msgv3 = iv_msgv3
                          msgv4 = iv_msgv4 ).
    DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ir_ref ).
    rs_message-kind = lo_descr->kind.
    rs_message-type = lo_descr->get_relative_name( ).
    TRY.
        rs_message-value = zcl_val_tools=>get_ref_as_string( ir_ref ).
      CATCH zcx_val_unsupported_operation.
        rs_message-value = '???'.
    ENDTRY.
  ENDMETHOD.


  METHOD validate.
    IF ir_ref IS INITIAL OR ir_ref IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_val_argument_null
        EXPORTING
          iv_variable_name = 'IR_REF'.
    ENDIF.

    IF io_config IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_val_argument_null
        EXPORTING
          iv_variable_name = 'IO_CONFIG'.
    ENDIF.

    rv_valid = validate_internal(
      EXPORTING
        ir_ref      = ir_ref
        io_config   = io_config
        iv_name     = get_data_object_name( io_config = io_config ir_ref = ir_ref )
      IMPORTING
        et_messages = et_messages
    ).
  ENDMETHOD.
ENDCLASS.
