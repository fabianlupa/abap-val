"! Rule configuration
CLASS zcl_val_rules_config DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_option,
        name  TYPE string,
        value TYPE string,
      END OF gty_option,
      gty_option_tab TYPE HASHED TABLE OF gty_option WITH UNIQUE KEY name.
    METHODS:
      constructor IMPORTING it_options    TYPE gty_option_tab OPTIONAL
                            io_descriptor TYPE REF TO cl_abap_classdescr
                            iv_name       TYPE csequence OPTIONAL,
      add_option IMPORTING is_option TYPE gty_option
                 RAISING   cx_sy_itab_duplicate_key,
      remove_option IMPORTING iv_name TYPE string,
      set_name IMPORTING iv_name TYPE csequence.
    DATA:
      mt_options          TYPE gty_option_tab READ-ONLY,
      mo_rule_descriptor  TYPE REF TO cl_abap_classdescr READ-ONLY,
      mv_data_object_name TYPE string READ-ONLY.
  PROTECTED SECTION.
    CLASS-METHODS:
      get_classdescr_by_data IMPORTING ig_any          TYPE data
                             RETURNING VALUE(ro_descr) TYPE REF TO cl_abap_classdescr.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_val_rules_config IMPLEMENTATION.
  METHOD constructor.
    mt_options = it_options.
    mo_rule_descriptor = io_descriptor.
  ENDMETHOD.

  METHOD add_option.
    INSERT is_option INTO TABLE mt_options.
  ENDMETHOD.

  METHOD remove_option.
    DELETE TABLE mt_options WITH TABLE KEY name = iv_name.
  ENDMETHOD.

  METHOD set_name.
    mv_data_object_name = iv_name.
  ENDMETHOD.

  METHOD get_classdescr_by_data.
    ro_descr = CAST #( CAST cl_abap_refdescr(
                         cl_abap_typedescr=>describe_by_data( ig_any )
                       )->get_referenced_type( ) ).
  ENDMETHOD.
ENDCLASS.
