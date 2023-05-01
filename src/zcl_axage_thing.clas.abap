CLASS zcl_axage_thing DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA name TYPE string.
    DATA description TYPE string.

    METHODS constructor
      IMPORTING
        name  TYPE clike
        descr TYPE clike.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_axage_thing IMPLEMENTATION.
  METHOD constructor.
    me->name = name.
    me->description = descr.
  ENDMETHOD.
ENDCLASS.
