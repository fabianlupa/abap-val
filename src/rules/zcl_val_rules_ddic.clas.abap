"! DDIC validations
CLASS zcl_val_rules_ddic DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_val_rules_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Validate a data object against domain values
      "! @parameter iv_domname | Domain name
      "! @parameter iv_check_fixed | Check against fixed values
      "! @parameter iv_check_table | Check against check database table
      "! @parameter ro_config | Rule configration
      new_domain IMPORTING iv_domname       TYPE domname OPTIONAL
                           iv_check_fixed   TYPE abap_bool DEFAULT abap_true
                           iv_check_table   TYPE abap_bool DEFAULT abap_true
                 RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Validate a data object against database table entries
      "! @parameter iv_tabname | DB table name
      "! @parameter ro_config | Rule configuration
      new_table IMPORTING iv_tabname       TYPE tabname
                RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config,
      "! Check if a value is part of a domain
      "! @parameter iv_domname | Domain name
      "! @parameter iv_value | Value to check
      "! @parameter iv_check_fixed | Check against fixed values
      "! @parameter iv_check_table | Check against check database table
      "! @parameter rv_in_domain | Value is part of the domain
      is_value_in_domain IMPORTING iv_domname          TYPE domname
                                   iv_value            TYPE csequence
                                   iv_check_fixed      TYPE abap_bool
                                   iv_check_table      TYPE abap_bool
                         RETURNING VALUE(rv_in_domain) TYPE abap_bool,
      "! Check if a value is an entry in a database table
      "! @parameter iv_tabname | DB table name
      "! @parameter iv_value | Value to check
      "! @parameter iv_column | Column that should contain the value
      "! @parameter io_descr |
      "! @parameter rv_in_table | Value is an entry in the database table
      is_value_in_table IMPORTING iv_tabname         TYPE tabname
                                  iv_value           TYPE csequence
                                  iv_column          TYPE fieldname OPTIONAL
                                  io_descr           TYPE REF TO cl_abap_structdescr OPTIONAL
                        RETURNING VALUE(rv_in_table) TYPE abap_bool.
  PROTECTED SECTION.
    METHODS:
      validate_internal REDEFINITION.
  PRIVATE SECTION.
    TYPES:
      gty_mode TYPE string.
    CONSTANTS:
      BEGIN OF gc_modes,
        domain TYPE gty_mode VALUE 'DOMAIN',
        table  TYPE gty_mode VALUE 'TABLE',
      END OF gc_modes.
ENDCLASS.



CLASS zcl_val_rules_ddic IMPLEMENTATION.
  METHOD new_domain.
    ro_config = NEW lcl_config( iv_mode        = gc_modes-domain
                                iv_domname     = iv_domname
                                iv_check_fixed = iv_check_fixed
                                iv_check_table = iv_check_table ).
  ENDMETHOD.

  METHOD new_table.
    ro_config = NEW lcl_config( iv_mode    = gc_modes-table
                                iv_tabname = iv_tabname ).
  ENDMETHOD.

  METHOD validate_internal.
    DATA: lv_domname TYPE domname.

    rv_valid = abap_true.

    DATA(lo_config) = CAST lcl_config( io_config ).

    DATA(lv_string) = zcl_val_tools=>get_ref_as_string( ir_ref ).

    CASE lo_config->mv_mode.
      WHEN gc_modes-domain.
        " Determine domain name
        IF lo_config->mv_domname IS NOT INITIAL.
          lv_domname = lo_config->mv_domname.
        ELSE.
          DATA(lo_descr) = cl_abap_typedescr=>describe_by_data_ref( ir_ref ).
          IF lo_descr->kind <> cl_abap_typedescr=>kind_elem.

          ENDIF.

          DATA(lo_elem_descr) = CAST cl_abap_elemdescr( lo_descr ).
          IF lo_elem_descr->is_ddic_type( ) = abap_false.

          ENDIF.

          lv_domname = lo_elem_descr->get_ddic_field( )-domname.
        ENDIF.

        rv_valid = is_value_in_domain( iv_domname     = lv_domname
                                       iv_value       = lv_string
                                       iv_check_fixed = lo_config->mv_check_fixed
                                       iv_check_table = lo_config->mv_check_table ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.

  METHOD is_value_in_domain.
    DATA: lt_values          TYPE STANDARD TABLE OF dd07v,
          lt_values_inactive TYPE STANDARD TABLE OF dd07v,
          lt_value_range     TYPE RANGE OF domvalue.

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name   = iv_domname
        withtext      = abap_false
      TABLES
        dd07v_tab_a   = lt_values
        dd07v_tab_n   = lt_values_inactive
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_value>).
      APPEND VALUE #( sign   = COND #( WHEN <ls_value>-domvalue_h IS NOT INITIAL
                                       THEN 'BT'
                                       ELSE 'EQ' )
                      low    = <ls_value>-domvalue_l
                      high   = <ls_value>-domvalue_h
                      option = 'I' ) TO lt_value_range.
    ENDLOOP.

    rv_in_domain = boolc(
      iv_value IN lt_value_range OR
      ( iv_check_table = abap_true AND
        is_value_in_table( iv_tabname = 'TODO' iv_value = iv_value ) = abap_true )
    ).

    IF rv_in_domain = abap_false AND iv_check_table = abap_true.

    ENDIF.
  ENDMETHOD.

  METHOD is_value_in_table.
    DATA: lv_objtype TYPE tabclass,
          lr_struct  TYPE REF TO data,
          lt_cond    TYPE stringtab.
    FIELD-SYMBOLS: <lg_struct> TYPE any.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_tabname
      IMPORTING
        ddobjtype = lv_objtype
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0 OR ( lv_objtype <> 'TRANSP' AND lv_objtype <> 'VIEW' ).
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF io_descr IS BOUND.
      CREATE DATA lr_struct TYPE HANDLE io_descr.
      ASSIGN lr_struct->* TO <lg_struct>.
      <lg_struct> = iv_value.

      LOOP AT io_descr->components ASSIGNING FIELD-SYMBOL(<ls_comp>).
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <lg_struct> TO FIELD-SYMBOL(<lg_comp>).
        APPEND |{ <ls_comp>-name } = '{ <lg_comp> }'| TO lt_cond.
      ENDLOOP.

      TRY.
          SELECT COUNT(*)
            FROM (iv_tabname)
            WHERE (lt_cond).
        CATCH cx_sy_dynamic_osql_error.
          ASSERT 1 = 2 ##TODO.
      ENDTRY.

      rv_in_table = boolc( sy-dbcnt > 0 ).

    ELSE.
      APPEND |{ iv_column } = '{ iv_value }'| TO lt_cond.

      TRY.
          SELECT COUNT(*)
            FROM (iv_tabname)
            WHERE (lt_cond).
        CATCH cx_sy_dynamic_osql_error.
          ASSERT 1 = 2 ##TODO.
      ENDTRY.

      rv_in_table = boolc( sy-dbcnt > 0 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
