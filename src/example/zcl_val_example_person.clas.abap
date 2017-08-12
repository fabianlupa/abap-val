class ZCL_VAL_EXAMPLE_PERSON definition
  public
  final
  create protected

  global friends ZCB_VAL_EXAMPLE_PERSON .

public section.

  interfaces IF_OS_STATE .
  interfaces IF_OS_CHECK .

  methods GET_MV_FIRSTNAME
    returning
      value(RESULT) type BU_NAMEP_F
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_MV_LASTNAME
    returning
      value(RESULT) type BU_NAMEP_L
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_MV_UNAME
    returning
      value(RESULT) type UNAME
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_MV_FIRSTNAME
    importing
      !I_MV_FIRSTNAME type BU_NAMEP_F
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_MV_LASTNAME
    importing
      !I_MV_LASTNAME type BU_NAMEP_L
    raising
      CX_OS_OBJECT_NOT_FOUND .
protected section.

  data MV_UNAME type UNAME .
  data MV_FIRSTNAME type BU_NAMEP_F .
  data MV_LASTNAME type BU_NAMEP_L .
private section.
ENDCLASS.



CLASS ZCL_VAL_EXAMPLE_PERSON IMPLEMENTATION.


  method GET_MV_FIRSTNAME.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MV_FIRSTNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MV_FIRSTNAME.

           " GET_MV_FIRSTNAME
  endmethod.


  method GET_MV_LASTNAME.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MV_LASTNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MV_LASTNAME.

           " GET_MV_LASTNAME
  endmethod.


  method GET_MV_UNAME.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MV_UNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MV_UNAME.

           " GET_MV_UNAME
  endmethod.


  METHOD if_os_check~is_consistent.
    DATA(lt_rules) = zcl_val_configuration_builder=>get(
      )->add_field( REF #( mv_firstname )
        )->add_rule( zcl_val_rules_abap=>new_not_initial( )
        )->end(
      )->add_field( REF #( mv_lastname )
        )->add_rule( zcl_val_rules_abap=>new_not_initial( )
        )->end(
      )->build(
    ).
    result = zcl_val_validator=>validate_all( lt_rules ).
  ENDMETHOD.


  method IF_OS_STATE~GET.
***BUILD 090501
     " returning result type ref to object
************************************************************************
* Purpose        : Get state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  create object STATE_OBJECT.
  call method STATE_OBJECT->SET_STATE_FROM_OBJECT( ME ).
  result = STATE_OBJECT.

  endmethod.


  method IF_OS_STATE~HANDLE_EXCEPTION.
***BUILD 090501
     " importing I_EXCEPTION type ref to IF_OS_EXCEPTION_INFO optional
     " importing I_EX_OS type ref to CX_OS_OBJECT_NOT_FOUND optional
************************************************************************
* Purpose        : Handles exceptions during attribute access.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : If an exception is raised during attribut access,
*                  this method is called and the exception is passed
*                  as a paramater. The default is to raise the exception
*                  again, so that the caller can handle the exception.
*                  But it is also possible to handle the exception
*                  here in the callee.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
* - 2000-08-02   : (SB)  OO Exceptions
************************************************************************
* Modify if you like
************************************************************************

  if i_ex_os is not initial.
    raise exception i_ex_os.
  endif.

  endmethod.


  method IF_OS_STATE~INIT.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Initialisation of the transient state partition.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Transient state is initial.
*
* OO Exceptions  : -
*
* Implementation : Caution!: Avoid Throwing ACCESS Events.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~INVALIDATE.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Do something before all persistent attributes are
*                  cleared.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : Whatever you like to do.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~SET.
***BUILD 090501
     " importing I_STATE type ref to object
************************************************************************
* Purpose        : Set state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  STATE_OBJECT ?= I_STATE.
  call method STATE_OBJECT->SET_OBJECT_FROM_STATE( ME ).

  endmethod.


  method SET_MV_FIRSTNAME.
***BUILD 090501
     " importing I_MV_FIRSTNAME
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute MV_FIRSTNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_MV_FIRSTNAME <> MV_FIRSTNAME ).

    MV_FIRSTNAME = I_MV_FIRSTNAME.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_MV_FIRSTNAME <> MV_FIRSTNAME )

           " GET_MV_FIRSTNAME
  endmethod.


  method SET_MV_LASTNAME.
***BUILD 090501
     " importing I_MV_LASTNAME
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute MV_LASTNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_MV_LASTNAME <> MV_LASTNAME ).

    MV_LASTNAME = I_MV_LASTNAME.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_MV_LASTNAME <> MV_LASTNAME )

           " GET_MV_LASTNAME
  endmethod.
ENDCLASS.
