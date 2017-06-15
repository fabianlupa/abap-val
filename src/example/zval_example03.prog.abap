REPORT zval_example03.

PARAMETERS: p_mandt TYPE t000-mandt,
            p_user  TYPE syst_uname.

AT SELECTION-SCREEN.
  DATA(lt_config) = zcl_val_configuration_builder=>get(
    )->add_field( REF #( p_mandt )
      )->add_rule( zcl_val_rules_abap=>new_initial( )
      )->add_rule( zcl_val_rules_abap=>new_in_range( VALUE #(
                     ( option = 'BT' sign = 'I' low = '000' high = '200' ) )
                   )
      )->end(
    )->add_field(
      )->set_field( REF #( p_user )
      )->set_name( 'Username'
      )->add_rule( zcl_val_rules_string=>new_length( iv_min = 2 )
      )->end(
    )->build( ).

  TRY.
      zcl_val_validator=>check_valid_all( lt_config ).
    CATCH zcx_val_invalid INTO DATA(lx_ex).
      MESSAGE lx_ex TYPE 'E'.
  ENDTRY.
