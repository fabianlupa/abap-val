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
      "! @raising zcx_val_illegal_argument | Domain does not exist
      new_domain IMPORTING iv_domname       TYPE domname OPTIONAL
                           iv_check_fixed   TYPE abap_bool DEFAULT abap_true
                           iv_check_table   TYPE abap_bool DEFAULT abap_true
                 RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config
                 RAISING   zcx_val_illegal_argument,
      "! Validate a data object against database table entries
      "! @parameter iv_tabname | DB table name
      "! @parameter iv_column | Column name to search in
      "! @parameter ro_config | Rule configuration
      "! @raising zcx_val_illegal_argument | Table or column does not exist
      new_table IMPORTING iv_tabname       TYPE tabname
                          iv_column        TYPE fieldname
                RETURNING VALUE(ro_config) TYPE REF TO zcl_val_rules_config
                RAISING   zcx_val_illegal_argument,
      "! Check if a value is part of a domain
      "! @parameter iv_domname | Domain name
      "! @parameter iv_value | Value to check
      "! @parameter iv_check_fixed | Check against fixed values
      "! @parameter iv_check_table | Check against check database table
      "! @parameter rv_in_domain | Value is part of the domain
      "! @raising zcx_val_illegal_argument | Domain does not exist
      is_value_in_domain IMPORTING iv_domname          TYPE domname
                                   iv_value            TYPE csequence
                                   iv_check_fixed      TYPE abap_bool
                                   iv_check_table      TYPE abap_bool
                         RETURNING VALUE(rv_in_domain) TYPE abap_bool
                         RAISING   zcx_val_illegal_argument,
      "! Check if a value is an entry in a database table
      "! @parameter iv_tabname | DB table name
      "! @parameter iv_value | Value to check
      "! @parameter iv_column | Column that should contain the value
      "! @parameter rv_in_table | Value is an entry in the database table
      "! @raising zcx_val_illegal_argument | Table does not exist / is unsupported or column does
      "!                                     not exist in table
      is_value_in_table IMPORTING iv_tabname         TYPE tabname
                                  iv_value           TYPE csequence
                                  iv_column          TYPE fieldname
                        RETURNING VALUE(rv_in_table) TYPE abap_bool
                        RAISING   zcx_val_illegal_argument.
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
    CLASS-METHODS:
      get_domain_column_name IMPORTING iv_domname       TYPE domname
                                       iv_tabname       TYPE tabname
                             RETURNING VALUE(rv_column) TYPE fieldname.
ENDCLASS.



CLASS zcl_val_rules_ddic IMPLEMENTATION.
  METHOD new_domain.
    DATA: ls_header          TYPE dd01v,
          lt_values          TYPE STANDARD TABLE OF dd07v,
          lt_values_inactive TYPE STANDARD TABLE OF dd07v.

    IF iv_domname IS NOT INITIAL.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = iv_domname
          withtext      = abap_false
        IMPORTING
          dd01v_wa_a    = ls_header
        TABLES
          dd07v_tab_a   = lt_values
          dd07v_tab_n   = lt_values_inactive
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO DATA(lv_msg_text).
        RAISE EXCEPTION TYPE zcx_val_illegal_argument
          EXPORTING
            iv_reason = lv_msg_text
            iv_value  = iv_domname.

      ELSEIF ls_header IS INITIAL.
        RAISE EXCEPTION TYPE zcx_val_illegal_argument
          EXPORTING
            iv_reason = 'Domain could not be found'
            iv_value  = iv_domname ##NO_TEXT.
      ENDIF.
    ENDIF.

    ro_config = NEW lcl_config( iv_mode        = gc_modes-domain
                                iv_domname     = iv_domname
                                iv_check_fixed = iv_check_fixed
                                iv_check_table = iv_check_table ).
  ENDMETHOD.

  METHOD new_table.
    DATA: lt_fields TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_tabname
      TABLES
        dfies_tab = lt_fields
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO DATA(lv_msg_text).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = lv_msg_text
          iv_value  = iv_tabname.

    ELSEIF NOT line_exists( lt_fields[ fieldname = iv_column ] ).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Column does not exist in database table'
          iv_value  = iv_column ##NO_TEXT.
    ENDIF.

    ro_config = NEW lcl_config( iv_mode    = gc_modes-table
                                iv_tabname = iv_tabname
                                iv_column  = iv_column ).
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
            RAISE EXCEPTION TYPE zcx_val_unsupported_operation
              EXPORTING
                is_textid = zcx_val_unsupported_operation=>gc_no_domain.
          ENDIF.

          DATA(lo_elem_descr) = CAST cl_abap_elemdescr( lo_descr ).
          IF lo_elem_descr->is_ddic_type( ) = abap_false.
            RAISE EXCEPTION TYPE zcx_val_unsupported_operation
              EXPORTING
                is_textid = zcx_val_unsupported_operation=>gc_no_domain.
          ENDIF.

          lv_domname = lo_elem_descr->get_ddic_field( )-domname.
        ENDIF.

        rv_valid = is_value_in_domain( iv_domname     = lv_domname
                                       iv_value       = lv_string
                                       iv_check_fixed = lo_config->mv_check_fixed
                                       iv_check_table = lo_config->mv_check_table ).

      WHEN gc_modes-table.
        rv_valid = is_value_in_table( iv_tabname = lo_config->mv_tabname
                                      iv_value   = lv_string
                                      iv_column  = lo_config->mv_column ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.

  METHOD get_domain_column_name.
    DATA: lt_fields TYPE STANDARD TABLE OF dfies.

    " This could propably be found out a lot faster

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_tabname
      TABLES
        dfies_tab = lt_fields
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    ASSERT sy-subrc = 0.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE keyflag = abap_true
                                                           AND rollname IS NOT INITIAL.
      DATA(lo_descr) = CAST cl_abap_elemdescr(
                         cl_abap_typedescr=>describe_by_name( <ls_field>-rollname )
                       ).
      IF lo_descr->is_ddic_type( ) = abap_true AND lo_descr->get_ddic_field( )-domname = iv_domname.
        rv_column = <ls_field>-fieldname.
        EXIT.
      ENDIF.
    ENDLOOP.

    ASSERT rv_column IS NOT INITIAL.
  ENDMETHOD.

  METHOD is_value_in_domain.
    DATA: lt_values          TYPE STANDARD TABLE OF dd07v,
          lt_values_inactive TYPE STANDARD TABLE OF dd07v,
          ls_header          TYPE dd01v,
          lt_value_range     TYPE RANGE OF domvalue.

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name   = iv_domname
        withtext      = abap_false
      IMPORTING
        dd01v_wa_a    = ls_header
      TABLES
        dd07v_tab_a   = lt_values
        dd07v_tab_n   = lt_values_inactive
      EXCEPTIONS
        illegal_value = 1
        op_failure    = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO DATA(lv_msg_text).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = lv_msg_text
          iv_value  = iv_domname.

    ELSEIF ls_header IS INITIAL.
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Domain could not be found'
          iv_value  = iv_domname ##NO_TEXT.
    ENDIF.

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_value>).
      APPEND VALUE #( option = COND #( WHEN <ls_value>-domvalue_h IS NOT INITIAL
                                       THEN 'BT'
                                       ELSE 'EQ' )
                      low    = <ls_value>-domvalue_l
                      high   = <ls_value>-domvalue_h
                      sign   = 'I' ) TO lt_value_range.
    ENDLOOP.

    rv_in_domain = boolc(
      ( iv_check_fixed = abap_true AND lt_value_range IS NOT INITIAL AND
        iv_value IN lt_value_range )
      OR
      ( iv_check_table = abap_true AND ls_header-entitytab IS NOT INITIAL AND
        is_value_in_table(
          iv_tabname = ls_header-entitytab
          iv_value   = iv_value
          iv_column  = get_domain_column_name( iv_domname = iv_domname
                                               iv_tabname = ls_header-entitytab )
        ) = abap_true )
    ).
  ENDMETHOD.

  METHOD is_value_in_table.
    DATA: lv_objtype TYPE tabclass,
          lt_cond    TYPE stringtab,
          lt_fields  TYPE STANDARD TABLE OF x031l.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_tabname
      IMPORTING
        ddobjtype = lv_objtype
      TABLES
        x031l_tab = lt_fields
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO DATA(lv_msg_text).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = lv_msg_text
          iv_value  = iv_tabname.

    ELSEIF lv_objtype <> 'TRANSP' AND lv_objtype <> 'VIEW'.
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Table is not transparent table / view'
          iv_value  = iv_tabname ##NO_TEXT.

    ELSEIF NOT line_exists( lt_fields[ fieldname = iv_column ] ).
      RAISE EXCEPTION TYPE zcx_val_illegal_argument
        EXPORTING
          iv_reason = 'Column does not exist in table'
          iv_value  = iv_column ##NO_TEXT.
    ENDIF.

    APPEND |{ iv_column } = { cl_abap_dyn_prg=>quote( iv_value ) }| TO lt_cond.

    SELECT COUNT(*)
      FROM (iv_tabname)
      WHERE (lt_cond).

    rv_in_table = boolc( sy-dbcnt > 0 ).
  ENDMETHOD.
ENDCLASS.
