"! Enhanced consistency check for object services
INTERFACE zif_val_os_check PUBLIC.
  INTERFACES:
    if_os_check.
  ALIASES:
    is_consistent FOR if_os_check~is_consistent.
  DATA:
    mx_last_validation_error TYPE REF TO zcx_val_invalid.
ENDINTERFACE.
