CLASS lcl_config DEFINITION DEFERRED.
CLASS zcl_val_rules_abap DEFINITION LOCAL FRIENDS lcl_config.

CLASS lcl_config DEFINITION INHERITING FROM zcl_val_rules_config.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_type               TYPE zcl_val_rules_abap=>gty_type
                            it_range              TYPE zcl_val_rules_abap=>gty_range_any OPTIONAL
                            iv_initial_valid      TYPE abap_bool OPTIONAL
                            iv_not_bound_valid    TYPE abap_bool OPTIONAL
                            iv_not_in_range_valid TYPE abap_bool OPTIONAL.
    DATA:
      mv_type               TYPE zcl_val_rules_abap=>gty_type READ-ONLY,
      mt_range              TYPE zcl_val_rules_abap=>gty_range_any READ-ONLY,
      mv_initial_valid      TYPE abap_bool READ-ONLY,
      mv_not_bound_valid    TYPE abap_bool READ-ONLY,
      mv_not_in_range_valid TYPE abap_bool READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_config IMPLEMENTATION.
  METHOD constructor.
    DATA: lo_dummy TYPE REF TO zcl_val_rules_abap ##NEEDED.

    super->constructor( get_classdescr_by_data( lo_dummy ) ).

    mv_type = iv_type.
    mt_range = it_range.
    mv_initial_valid = iv_initial_valid.
    mv_not_bound_valid = iv_not_bound_valid.
    mv_not_in_range_valid = iv_not_in_range_valid.
  ENDMETHOD.
ENDCLASS.
