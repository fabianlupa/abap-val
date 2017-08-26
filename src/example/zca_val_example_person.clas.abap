class ZCA_VAL_EXAMPLE_PERSON definition
  public
  inheriting from ZCB_VAL_EXAMPLE_PERSON
  final
  create private .

public section.

  class-data AGENT type ref to ZCA_VAL_EXAMPLE_PERSON read-only .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCA_VAL_EXAMPLE_PERSON IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
***BUILD 090501
************************************************************************
* Purpose        : Initialize the 'class'.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Singleton is created.
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 1999-09-20   : (OS) Initial Version
* - 2000-03-06   : (BGR) 2.0 modified REGISTER_CLASS_AGENT
************************************************************************
* GENERATED: Do not modify
************************************************************************

  create object AGENT.

  call method AGENT->REGISTER_CLASS_AGENT
    exporting
      I_CLASS_NAME          = 'ZCL_VAL_EXAMPLE_PERSON'
      I_CLASS_AGENT_NAME    = 'ZCA_VAL_EXAMPLE_PERSON'
      I_CLASS_GUID          = '000C29DFAF0F1EE79FE715B47E3A024B'
      I_CLASS_AGENT_GUID    = '000C29DFAF0F1EE79FE715B47E3A824B'
      I_AGENT               = AGENT
      I_STORAGE_LOCATION    = 'ZVAL_TAEXPEOPLE'
      I_CLASS_AGENT_VERSION = '2.0'.

           "CLASS_CONSTRUCTOR
  endmethod.
ENDCLASS.
