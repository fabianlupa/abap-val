"! Configuration builder
"! <p>
"! This is a builder class as an alternative for creating an internal table of the type
"! <em>ZCL_VAL_VALIDATOR=&gtGTY_VALIDATION_TAB</em> directly.
"! </p>
CLASS zcl_val_configuration_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES:
      zif_val_config_builder,
      zif_val_field_builder.
    ALIASES:
      get FOR zif_val_config_builder~get,
      add_field FOR zif_val_config_builder~add_field,
      add_rule FOR zif_val_field_builder~add_rule,
      build FOR zif_val_config_builder~build,
      end FOR zif_val_field_builder~end,
      set_name FOR zif_val_field_builder~set_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_config       TYPE STANDARD TABLE OF zcl_val_validator=>gty_validation,
      mr_current_line TYPE REF TO zcl_val_validator=>gty_validation.
ENDCLASS.



CLASS zcl_val_configuration_builder IMPLEMENTATION.
  METHOD zif_val_config_builder~add_field.
    ri_field_builder = me.

    IF ir_ref IS BOUND AND line_exists( mt_config[ ref = ir_ref ] ).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Field already added'.
    ENDIF.

    APPEND INITIAL LINE TO mt_config REFERENCE INTO mr_current_line.
    mr_current_line->ref = ir_ref.
  ENDMETHOD.

  METHOD zif_val_field_builder~add_rule.
    ri_field_builder = me.

    IF io_config IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_val_argument_null
        EXPORTING
          iv_variable_name = 'IO_CONFIG'.
    ENDIF.

    INSERT io_config INTO TABLE mr_current_line->rules.
  ENDMETHOD.

  METHOD zif_val_config_builder~build.
    rt_config = mt_config.
  ENDMETHOD.

  METHOD zif_val_field_builder~end.
    ri_config_builder = me.

    FREE mr_current_line.
  ENDMETHOD.

  METHOD zif_val_field_builder~set_field.
    ri_field_builder = me.

    IF ir_ref IS BOUND AND line_exists( mt_config[ ref = ir_ref ] ).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Field already added'.
    ENDIF.

    mr_current_line->ref = ir_ref.
  ENDMETHOD.

  METHOD zif_val_config_builder~get.
    ri_config_builder = NEW zcl_val_configuration_builder( ).
  ENDMETHOD.

  METHOD zif_val_field_builder~set_name.
    ri_field_builder = me.

    mr_current_line->name = iv_name.
  ENDMETHOD.
ENDCLASS.
