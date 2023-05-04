CLASS zcl_axage_result DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
      IMPORTING
        text TYPE clike.

    METHODS addTab
      IMPORTING
        textTab TYPE string_table.

    METHODS get
      RETURNING
        VALUE(textString) TYPE string.
  PROTECTED SECTION.
    DATA text TYPE string_table.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_axage_result IMPLEMENTATION.
  METHOD add.
    APPEND text TO me->text.
  ENDMETHOD.

  METHOD get.
    LOOP AT text REFERENCE INTO DATA(line).
      textstring = textstring && line->* && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
  ENDMETHOD.

  METHOD addTab.
    APPEND LINES OF textTab TO text.
  ENDMETHOD.
ENDCLASS.
