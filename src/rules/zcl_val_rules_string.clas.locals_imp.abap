CLASS lcl_config DEFINITION DEFERRED.
CLASS zcl_val_rules_string DEFINITION LOCAL FRIENDS lcl_config.

CLASS lcl_config DEFINITION INHERITING FROM zcl_val_rules_config.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_type       TYPE zcl_val_rules_string=>gty_type
                            iv_min_length TYPE i OPTIONAL
                            iv_max_length TYPE i OPTIONAL
                            io_regex      TYPE REF TO cl_abap_regex OPTIONAL.
    DATA:
      mv_type       TYPE zcl_val_rules_string=>gty_type READ-ONLY,
      mv_min_length TYPE i READ-ONLY,
      mv_max_length TYPE i READ-ONLY,
      mo_regex      TYPE REF TO cl_abap_regex READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_config IMPLEMENTATION.
  METHOD constructor.
    DATA: lo_dummy TYPE REF TO zcl_val_rules_string ##NEEDED.

    super->constructor( get_classdescr_by_data( lo_dummy ) ).

    mv_type = iv_type.
    mv_min_length = iv_min_length.
    mv_max_length = iv_max_length.
    mo_regex = io_regex.
  ENDMETHOD.
ENDCLASS.
