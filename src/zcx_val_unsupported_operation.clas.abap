"! Unsupported operation
CLASS zcx_val_unsupported_operation DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_non_charlike_unsupported,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_non_charlike_unsupported,
      BEGIN OF gc_non_objref_unsupported,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_non_objref_unsupported,
      BEGIN OF gc_not_self_validatable,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_not_self_validatable,
      BEGIN OF gc_no_domain,
        msgid TYPE symsgid VALUE 'ZVAL',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_domain.
    INTERFACES:
      if_t100_message.
    METHODS:
      "! @parameter is_textid | Textid
      "! @parameter ix_previous | Previous exception
      constructor IMPORTING is_textid   LIKE if_t100_message=>t100key OPTIONAL
                            ix_previous LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_val_unsupported_operation IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    CLEAR me->textid.
    IF is_textid IS INITIAL.
      if_t100_message~t100key = gc_no_arguments.
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
