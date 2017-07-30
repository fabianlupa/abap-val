"! Validation error
CLASS zcx_val_invalid DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING it_messages TYPE zval_t_messages OPTIONAL
                            ix_previous TYPE REF TO cx_root OPTIONAL,
      show_bal_dock.
    DATA:
      mv_attr1    TYPE string READ-ONLY,
      mv_attr2    TYPE string READ-ONLY,
      mv_attr3    TYPE string READ-ONLY,
      mv_attr4    TYPE string READ-ONLY,
      mt_messages TYPE zval_t_messages READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_val_invalid IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    CLEAR me->textid.

    IF it_messages IS NOT INITIAL.
      if_t100_message~t100key = VALUE #( msgid = it_messages[ 1 ]-msgid
                                         msgno = it_messages[ 1 ]-msgno
                                         attr1 = 'MV_ATTR1'
                                         attr2 = 'MV_ATTR2'
                                         attr3 = 'MV_ATTR3'
                                         attr4 = 'MV_ATTR4' ).
      mv_attr1 = it_messages[ 1 ]-msgv1.
      mv_attr2 = it_messages[ 1 ]-msgv2.
      mv_attr3 = it_messages[ 1 ]-msgv3.
      mv_attr4 = it_messages[ 1 ]-msgv4.
    ELSE.
      if_t100_message~t100key = gc_no_arguments.
    ENDIF.
  ENDMETHOD.

  METHOD show_bal_dock.

  ENDMETHOD.
ENDCLASS.
