CLASS zcl_val_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_rule_config_tab TYPE SORTED TABLE OF REF TO zcl_val_rules_config
                               WITH UNIQUE KEY table_line,
      BEGIN OF gty_validation,
        ref   TYPE REF TO data,
        name  TYPE string,
        rules TYPE gty_rule_config_tab,
      END OF gty_validation,
      gty_validation_tab TYPE SORTED TABLE OF gty_validation WITH UNIQUE KEY ref.
    CLASS-METHODS:
      validate_all IMPORTING it_validations  TYPE gty_validation_tab
                             io_mode         TYPE REF TO zcl_val_mode
                               DEFAULT zcl_val_mode=>go_collect_errors
                   EXPORTING et_messages     TYPE zval_t_messages
                   RETURNING VALUE(rv_valid) TYPE abap_bool
                   RAISING   zcx_val_unsupported_operation,
      validate IMPORTING ir_ref          TYPE REF TO data
                         iv_name         TYPE csequence OPTIONAL
                         it_rules        TYPE gty_rule_config_tab
                         io_mode         TYPE REF TO zcl_val_mode
                           DEFAULT zcl_val_mode=>go_collect_errors
               EXPORTING et_messages     TYPE zval_t_messages
               RETURNING VALUE(rv_valid) TYPE abap_bool
               RAISING   zcx_val_unsupported_operation,
      check_valid_all IMPORTING it_validations TYPE gty_validation_tab
                                io_mode        TYPE REF TO zcl_val_mode
                                  DEFAULT zcl_val_mode=>go_collect_errors
                      RAISING   zcx_val_invalid
                                zcx_val_unsupported_operation,
      check_valid IMPORTING ir_ref   TYPE REF TO data
                            iv_name  TYPE csequence OPTIONAL
                            it_rules TYPE gty_rule_config_tab
                            io_mode  TYPE REF TO zcl_val_mode
                              DEFAULT zcl_val_mode=>go_collect_errors
                  RAISING   zcx_val_invalid
                            zcx_val_unsupported_operation,
      check_valid_object IMPORTING ii_validatable TYPE REF TO zif_val_validatable
                                   io_mode        TYPE REF TO zcl_val_mode
                                     DEFAULT zcl_val_mode=>go_collect_errors
                         RAISING   zcx_val_invalid.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_rule_cache,
        descr    TYPE REF TO cl_abap_classdescr,
        instance TYPE REF TO zcl_val_rules_base,
      END OF gty_rule_cache.
    CLASS-METHODS:
      get_rule_instance IMPORTING io_descr       TYPE REF TO cl_abap_classdescr
                        RETURNING VALUE(ro_rule) TYPE REF TO zcl_val_rules_base.
    CLASS-DATA:
      gt_rule_cache TYPE HASHED TABLE OF gty_rule_cache WITH UNIQUE KEY descr.
ENDCLASS.



CLASS zcl_val_validator IMPLEMENTATION.
  METHOD validate_all.
    DATA: lt_messages TYPE zval_t_messages.
    rv_valid = abap_true.

    LOOP AT it_validations ASSIGNING FIELD-SYMBOL(<ls_validation>).
      DATA(lv_valid) = validate(
        EXPORTING
          ir_ref      = <ls_validation>-ref
          iv_name     = <ls_validation>-name
          it_rules    = <ls_validation>-rules
          io_mode     = io_mode
        IMPORTING
          et_messages = lt_messages
      ).

      APPEND LINES OF lt_messages TO et_messages.
      rv_valid = boolc( rv_valid = abap_true AND lv_valid = abap_true ).
      CLEAR: lt_messages, lv_valid.

      IF rv_valid = abap_false AND io_mode = zcl_val_mode=>go_stop_on_error.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate.
    DATA: lt_messages TYPE zval_t_messages.
    rv_valid = abap_true.

    LOOP AT it_rules INTO DATA(lo_config).
      lo_config->set_name( iv_name ).

      DATA(lo_rule) = get_rule_instance( lo_config->mo_rule_descriptor ).
      DATA(lv_valid) = lo_rule->validate(
        EXPORTING
          ir_ref    = ir_ref
          io_config = lo_config
        IMPORTING
          et_messages = lt_messages
      ).

      APPEND LINES OF lt_messages TO et_messages.
      rv_valid = boolc( rv_valid = abap_true AND lv_valid = abap_true ).
      CLEAR: lt_messages, lv_valid.

      IF rv_valid = abap_false AND io_mode = zcl_val_mode=>go_stop_on_error.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_valid_all.
    DATA: lt_messages TYPE zval_t_messages.

    DATA(lv_valid) = validate_all(
      EXPORTING
        it_validations = it_validations
        io_mode        = io_mode
      IMPORTING
        et_messages    = lt_messages
    ).

    IF lv_valid = abap_false.
      RAISE EXCEPTION TYPE zcx_val_invalid
        EXPORTING
          it_messages = lt_messages.
    ENDIF.
  ENDMETHOD.

  METHOD check_valid.
    DATA: lt_messages TYPE zval_t_messages.

    DATA(lv_valid) = validate(
      EXPORTING
        ir_ref      = ir_ref
        iv_name     = iv_name
        it_rules    = it_rules
        io_mode     = io_mode
      IMPORTING
        et_messages = lt_messages
    ).

    IF lv_valid = abap_false.
      RAISE EXCEPTION TYPE zcx_val_invalid
        EXPORTING
          it_messages = lt_messages.
    ENDIF.
  ENDMETHOD.

  METHOD check_valid_object.
    DATA: lt_messages TYPE zval_t_messages.

    DATA(lv_valid) = ii_validatable->validate(
      EXPORTING
        io_mode     = io_mode
      IMPORTING
        et_messages = lt_messages
    ).

    IF lv_valid = abap_false.
      RAISE EXCEPTION TYPE zcx_val_invalid
        EXPORTING
          it_messages = lt_messages.
    ENDIF.
  ENDMETHOD.

  METHOD get_rule_instance.
    DATA: lo_dummy TYPE REF TO zcl_val_rules_base ##NEEDED.

    ASSERT io_descr IS BOUND.
*    ASSERT io_descr->applies_to_class(
*             CAST cl_abap_classdescr(
*               CAST cl_abap_refdescr(
*                 cl_abap_typedescr=>describe_by_data( lo_dummy )
*               )->get_referenced_type( )
*             )->get_relative_name( )
*           ) = abap_true.

    TRY.
        ro_rule = gt_rule_cache[ KEY primary_key  descr = io_descr ]-instance.
        ASSERT ro_rule IS BOUND.
      CATCH cx_sy_itab_line_not_found.
        CREATE OBJECT ro_rule TYPE (io_descr->absolute_name).
        ASSERT ro_rule IS BOUND.
        INSERT VALUE #( descr = io_descr instance = ro_rule ) INTO TABLE gt_rule_cache.
        ASSERT sy-subrc = 0.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
