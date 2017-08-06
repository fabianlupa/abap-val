CLASS lcl_config DEFINITION DEFERRED.
CLASS zcl_val_rules_ddic DEFINITION LOCAL FRIENDS lcl_config.

CLASS lcl_config DEFINITION INHERITING FROM zcl_val_rules_config.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_mode        TYPE zcl_val_rules_ddic=>gty_mode
                            iv_domname     TYPE domname OPTIONAL
                            iv_check_fixed TYPE abap_bool OPTIONAL
                            iv_check_table TYPE abap_bool OPTIONAL
                            iv_tabname     TYPE tabname OPTIONAL
                            iv_column      TYPE fieldname OPTIONAL.
    DATA:
      mv_mode        TYPE zcl_val_rules_ddic=>gty_mode READ-ONLY,
      mv_domname     TYPE domname READ-ONLY,
      mv_check_fixed TYPE abap_bool READ-ONLY,
      mv_check_table TYPE abap_bool READ-ONLY,
      mv_tabname     TYPE tabname READ-ONLY,
      mv_column      TYPE fieldname READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_config IMPLEMENTATION.
  METHOD constructor.
    DATA: lo_dummy TYPE REF TO zcl_val_rules_ddic ##NEEDED.

    super->constructor( get_classdescr_by_data( lo_dummy ) ).

    mv_mode = iv_mode.
    mv_domname = iv_domname.
    mv_check_fixed = iv_check_fixed.
    mv_check_table = iv_check_table.
    mv_tabname = iv_tabname.
    mv_column = iv_column.
  ENDMETHOD.
ENDCLASS.
