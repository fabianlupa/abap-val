"! Configuration builder
"! <p>
"! Helper interface for <em>ZCL_VAL_CONFIGURATION_BUIDLER</em> on level 1.
"! </p>
INTERFACE zif_val_config_builder PUBLIC.
  CLASS-METHODS:
    "! Get a builder instance
    "! @parameter ri_config_builder | Builder instance
    get RETURNING VALUE(ri_config_builder) TYPE REF TO zif_val_config_builder.
  METHODS:
    "! Add a field for validation
    "! @parameter ir_ref | Field reference
    "! @parameter ri_field_builder | Field builder
    "! @raising zcx_val_illegal_argument | Field is already added
    add_field IMPORTING ir_ref                  TYPE REF TO data OPTIONAL
              RETURNING VALUE(ri_field_builder) TYPE REF TO zif_val_field_builder
              RAISING   zcx_val_illegal_argument,
    "! End builder and get the built validation configuration
    "! @parameter rt_config | Built configuration
    build RETURNING VALUE(rt_config) TYPE zcl_val_validator=>gty_validation_tab.
ENDINTERFACE.
