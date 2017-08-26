"! Field builder
"! <p>
"! Helper interface for <em>ZCL_VAL_CONFIGURATION_BUIDLER</em> on level 2.
"! </p>
INTERFACE zif_val_field_builder PUBLIC.
  METHODS:
    "! Set the field reference
    "! @parameter ir_ref | Field reference
    "! @parameter ri_field_builder | Field builder
    "! @raising zcx_val_illegal_argument | Field is already exists
    set_field IMPORTING ir_ref                  TYPE REF TO data
              RETURNING VALUE(ri_field_builder) TYPE REF TO zif_val_field_builder
              RAISING   zcx_val_illegal_argument,
    "! Set / override the field name
    "! @parameter iv_name | Name
    "! @parameter ri_field_builder | Field build
    set_name IMPORTING iv_name                 TYPE string
             RETURNING VALUE(ri_field_builder) TYPE REF TO zif_val_field_builder,
    "! Add a validation rule
    "! @parameter io_config | Rule configuration
    "! @parameter ri_field_builder | Field builder
    "! @raising zcx_val_argument_null | <em>io_config</em> cannot be null
    add_rule IMPORTING io_config               TYPE REF TO zcl_val_rules_config
             RETURNING VALUE(ri_field_builder) TYPE REF TO zif_val_field_builder
             RAISING   zcx_val_argument_null,
    "! End field builder and return to config builder
    "! @parameter ri_config_builder | Configuration builder
    end RETURNING VALUE(ri_config_builder) TYPE REF TO zif_val_config_builder.
ENDINTERFACE.
