REPORT zval_example04.

PARAMETERS: p_usr  TYPE uname,
            p_fnam TYPE bu_namep_f,
            p_lnam TYPE bu_namep_l.

START-OF-SELECTION.
  DATA: go_person TYPE REF TO zcl_val_example_person,
        go_agent  TYPE REF TO zca_val_example_person.

  TRY.
      go_person = go_agent->create_persistent( sy-uname ).
*              CATCH cx_os_object_existing.  "
    CATCH cx_os_check_agent_failed INTO DATA(gx_ex).

  ENDTRY.
