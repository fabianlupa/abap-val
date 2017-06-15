REPORT zval_example02.

CLASS lcl_person DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      zif_val_validatable.
    ALIASES:
      validate FOR zif_val_validatable~validate.
    DATA:
      mv_first_name TYPE string,
      mv_last_name  TYPE string,
      mv_birth_date TYPE d.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_person IMPLEMENTATION.
  METHOD zif_val_validatable~validate.
    DATA(lt_validations) = VALUE zcl_val_validator=>gty_validation_tab(
      ( ref   = REF #( mv_first_name )
        name  = 'First Name'
        rules = VALUE #( ( zcl_val_rules_string=>new_length( iv_min = 2 iv_max = 30 ) ) ) )
      ( ref   = REF #( mv_last_name )
        name  = 'Last Name'
        rules = VALUE #( ( zcl_val_rules_string=>new_length( iv_min = 2 iv_max = 30 ) ) ) )
      ( ref   = REF #( mv_birth_date )
        name  = 'Birth Date'
        rules = VALUE #( ( zcl_val_rules_abap=>new_initial( ) ) ) )
    ) ##NO_TEXT.

    rv_valid = zcl_val_validator=>validate_all(
      EXPORTING
        it_validations = lt_validations
        io_mode        = io_mode
      IMPORTING
        et_messages    = et_messages
    ).
  ENDMETHOD.
ENDCLASS.

PARAMETERS: p_fname TYPE string,
            p_lname TYPE string,
            p_bdate TYPE d.

START-OF-SELECTION.
  DATA(go_person) = NEW lcl_person( ).
  go_person->mv_first_name = p_fname.
  go_person->mv_last_name = p_lname.
  go_person->mv_birth_date = p_bdate.

  TRY.
      zcl_val_validator=>check_valid_object( go_person ).
      MESSAGE 'Valid' TYPE 'S' ##NO_TEXT.
    CATCH zcx_val_invalid INTO DATA(gx_ex).
      MESSAGE gx_ex TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
