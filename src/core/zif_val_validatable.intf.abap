"! Validatable
INTERFACE zif_val_validatable PUBLIC.
  METHODS:
    "! Validate this object
    "! @parameter io_mode | Validation mode
    "! @parameter et_messages | Messages
    validate IMPORTING io_mode         TYPE REF TO zcl_val_mode
                         DEFAULT zcl_val_mode=>go_collect_errors
             EXPORTING et_messages     TYPE zval_t_messages
             RETURNING VALUE(rv_valid) TYPE abap_bool.
ENDINTERFACE.
