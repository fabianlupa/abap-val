REPORT zval_example04.

PARAMETERS: p_usr  TYPE uname DEFAULT sy-uname,
            p_fnam TYPE bu_namep_f,
            p_lnam TYPE bu_namep_l.

LOAD-OF-PROGRAM.
  cl_os_system=>init_and_set_modes( i_external_commit = oscon_false
                                    i_update_mode     = oscon_dmode_direct ).

START-OF-SELECTION.
  DATA: go_person TYPE REF TO zcl_val_example_person.

  DATA(gi_tman) = cl_os_system=>get_transaction_manager( ).
  DATA(gi_transaction) = gi_tman->create_transaction( ).

  DATA(go_agent) = zca_val_example_person=>agent.
  gi_transaction->start( ).

  DATA(gv_try) = 0.
  TRY.
      go_person = SWITCH #( gv_try WHEN 0 THEN go_agent->get_persistent( p_usr )
                                   WHEN 1 THEN go_agent->create_persistent( p_usr ) ).
    CATCH cx_os_object_not_found.
      WRITE: / 'Person does not exist yet.' ##NO_TEXT.
      ADD 1 TO gv_try.
      RETRY.
  ENDTRY.

  ASSERT go_person IS BOUND.

  TRY.
      go_person->set_firstname( p_fnam ).
      go_person->set_lastname( p_lnam ).

      gi_transaction->end( ).

      WRITE: / 'Saved persistent object.' ##NO_TEXT.

    CATCH cx_os_check_agent_failed INTO DATA(gx_ex).
      WRITE: / gx_ex->get_text( ).
*      IF gx_ex->check_agent IS INSTANCE OF zif_val_os_check.
      TRY.
          WRITE: / CAST zif_val_os_check( gx_ex->check_agent
                                        )->mx_last_validation_error->get_text( ).
        CATCH cx_sy_move_cast_error ##NO_HANDLER.
      ENDTRY.
*      ENDIF.
      WRITE: / 'Rolling back the transaction.' ##NO_TEXT.
      gi_transaction->undo( ).

    CATCH cx_os_transaction INTO DATA(gx_ex2).
      WRITE: / gx_ex2->get_text( ).
      WRITE: / 'Rolling back the transaction.' ##NO_TEXT.
      gi_transaction->undo( ).

  ENDTRY.
