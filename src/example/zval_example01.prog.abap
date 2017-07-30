REPORT zval_example01.

PARAMETERS: p_string TYPE string LOWER CASE.

AT SELECTION-SCREEN ON p_string.
  TRY.
      zcl_val_validator=>check_valid(
        ir_ref   = REF #( p_string )
        iv_name  = 'String'
        it_rules = VALUE #( ( zcl_val_rules_string=>new_length( iv_min = 5 iv_max = 10 ) )
                            ( zcl_val_rules_abap=>new_initial( ) ) )
      ) ##NO_TEXT.
    CATCH zcx_val_invalid INTO DATA(lx_ex).
      MESSAGE lx_ex TYPE 'E'.
  ENDTRY.

START-OF-SELECTION.
  MESSAGE 'Valid' TYPE 'S' ##NO_TEXT.
