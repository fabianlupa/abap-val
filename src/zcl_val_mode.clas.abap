"! Validation mode enumeration
CLASS zcl_val_mode DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      go_collect_errors TYPE REF TO zcl_val_mode READ-ONLY,
      go_stop_on_error  TYPE REF TO zcl_val_mode READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_val_mode IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT: go_collect_errors, go_stop_on_error.
  ENDMETHOD.
ENDCLASS.
