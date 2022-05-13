

CLASS result DEFINITION.
  PUBLIC SECTION.
    METHODS add
      IMPORTING
        text TYPE clike.
    METHODS addtab
      IMPORTING
        texttab TYPE string_table.

    METHODS get
      RETURNING
        VALUE(textstring) TYPE string.
  PROTECTED SECTION.
    DATA text TYPE string_table.
ENDCLASS.

CLASS result IMPLEMENTATION.

  METHOD add.
    APPEND text TO me->text.
  ENDMETHOD.

  METHOD get.
    LOOP AT text INTO DATA(line).
      textstring = textstring && line && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
  ENDMETHOD.

  METHOD addtab.
    APPEND LINES OF texttab TO text.
  ENDMETHOD.

ENDCLASS.

CLASS thing DEFINITION.
  PUBLIC SECTION.
    DATA name TYPE string.
    DATA description TYPE string.
    METHODS constructor
      IMPORTING
        name  TYPE clike
        descr TYPE clike.
  PRIVATE SECTION.
ENDCLASS.


CLASS thinglist DEFINITION.
  PUBLIC SECTION.
    TYPES _things TYPE STANDARD TABLE OF REF TO thing WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        things TYPE _things OPTIONAL.
    METHODS get_list
      RETURNING VALUE(things) TYPE _things.
    METHODS get
      IMPORTING
        name         TYPE clike
      RETURNING
        VALUE(thing) TYPE REF TO thing.
    METHODS show
      RETURNING
        VALUE(result) TYPE string_table.
    METHODS add
      IMPORTING
        thing         TYPE REF TO thing
      RETURNING
        VALUE(result) TYPE REF TO result.
    METHODS delete
      IMPORTING
        name TYPE clike.
    METHODS exists
      IMPORTING
        name          TYPE clike
      RETURNING
        VALUE(exists) TYPE abap_bool.
  PROTECTED SECTION.
    DATA things TYPE _things.
ENDCLASS.

CLASS thinglist IMPLEMENTATION.
  METHOD constructor.
    me->things = things.
  ENDMETHOD.

  METHOD get_list.
    things = me->things.
  ENDMETHOD.

  METHOD get.
    LOOP AT me->things INTO thing WHERE table_line->name = name.
    ENDLOOP.
  ENDMETHOD.

  METHOD exists.
    LOOP AT me->things TRANSPORTING NO FIELDS WHERE table_line->name = name.
      exists = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD show.
*    DATA(cr) = cl_abap_char_utilities=>cr_lf.
    LOOP AT things INTO DATA(current_thing).
*      result->add current_thing->name, '(' NO-GAP, current_thing->description NO-GAP, ')'.
      APPEND |a { current_thing->name } - { current_thing->description }| TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD add.
    APPEND thing TO things.
  ENDMETHOD.

  METHOD delete.
    DELETE things WHERE table_line->name = name.
  ENDMETHOD.

ENDCLASS.

CLASS openable_thing DEFINITION INHERITING FROM thing.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        name    TYPE clike
        descr   TYPE clike
        needed  TYPE REF TO thinglist
        content TYPE REF TO thinglist.
    METHODS get_content
      RETURNING
        VALUE(content) TYPE REF TO thinglist.
    METHODS open
      IMPORTING
        things        TYPE REF TO thinglist
      RETURNING
        VALUE(result) TYPE REF TO result.
    METHODS is_open
      RETURNING
        VALUE(result) TYPE abap_bool.

    DATA needed TYPE REF TO thinglist.
  PROTECTED SECTION.
    DATA opened TYPE abap_bool.
    DATA content TYPE REF TO thinglist.
ENDCLASS.

CLASS openable_thing IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
     name  = name
     descr = descr ).
    me->needed = needed.
    me->content = content.
  ENDMETHOD.

  METHOD get_content.
    IF opened = abap_true.
      content = me->content.
    ENDIF.
  ENDMETHOD.

  METHOD is_open.
    result = opened.
  ENDMETHOD.

  METHOD open.
*    DATA(cr) = cl_abap_char_utilities=>cr_lf.
    result = NEW #( ).

    LOOP AT needed->get_list( ) INTO DATA(open_with).
      IF things->exists( open_with->name ).
        DATA(allowed) = abap_true.
        result->add( |The { name } is now open| ).
        me->opened = abap_true.
        EXIT. "from loop
      ENDIF.
    ENDLOOP.

    IF allowed = abap_false.
      result->add( |You have nothing the { name } can be opened with!| ).
    ENDIF.

    opened = me->opened.
  ENDMETHOD.

ENDCLASS.


CLASS room DEFINITION INHERITING FROM thing.
  PUBLIC SECTION.
    DATA north TYPE REF TO room.
    DATA east TYPE REF TO room.
    DATA south TYPE REF TO room.
    DATA west TYPE REF TO room.
    DATA things TYPE REF TO thinglist.
    CLASS-DATA no_exit TYPE REF TO room.
    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        name  TYPE clike
        descr TYPE clike.
    METHODS set_exits
      IMPORTING
        n TYPE REF TO room OPTIONAL
        e TYPE REF TO room OPTIONAL
        s TYPE REF TO room OPTIONAL
        w TYPE REF TO room OPTIONAL.
  PROTECTED SECTION.
    METHODS set_exit
      IMPORTING
        room        TYPE REF TO room
      RETURNING
        VALUE(exit) TYPE REF TO room.
ENDCLASS.

CLASS actor DEFINITION INHERITING FROM thing.
  PUBLIC SECTION.
    DATA location TYPE REF TO room.
    DATA things TYPE REF TO thinglist.
    METHODS constructor
      IMPORTING
        name  TYPE clike
        descr TYPE clike.
    METHODS set_location
      IMPORTING
        room TYPE REF TO room.
    METHODS get_location
      RETURNING
        VALUE(room) TYPE REF TO room.
    METHODS speak
      RETURNING
        VALUE(sentences) TYPE string_table.
    METHODS add_sentences
      IMPORTING
        sentences TYPE string_table.

  PRIVATE SECTION.
    DATA my_sentences TYPE string_table.

ENDCLASS.

CLASS map DEFINITION.
  PUBLIC SECTION.
    METHODS add_room
      IMPORTING
        room TYPE REF TO room.
    METHODS get_room
      IMPORTING
        name        TYPE clike
      RETURNING
        VALUE(room) TYPE REF TO room.
    METHODS set_floor_plan
      IMPORTING
        input TYPE string_table.
    METHODS show
      RETURNING
        VALUE(result) TYPE string_table.
  PROTECTED SECTION.

    TYPES _map TYPE STANDARD TABLE OF REF TO room.
    DATA map TYPE _map.
    DATA floor_plan TYPE string_table.

ENDCLASS.

CLASS map IMPLEMENTATION.

  METHOD add_room.
    APPEND room TO map.
  ENDMETHOD.

  METHOD get_room.
    LOOP AT map INTO room WHERE table_line->name = name.
      EXIT.
    ENDLOOP.
    IF room IS INITIAL.
      room = room=>no_exit.
    ENDIF.
  ENDMETHOD.

  METHOD set_floor_plan.
    me->floor_plan = input.
  ENDMETHOD.

  METHOD show.
    result = floor_plan.
  ENDMETHOD.

ENDCLASS.

CLASS thing IMPLEMENTATION.
  METHOD constructor.
    me->name = name.
    me->description = descr.
  ENDMETHOD.

ENDCLASS.

CLASS room IMPLEMENTATION.
  METHOD constructor.
    super->constructor( name = name descr = descr ).
    things = NEW #( ).
  ENDMETHOD.
  METHOD class_constructor.
    no_exit = NEW room( name = 'No Exit' descr = 'There is no exit in this direction...' ).
  ENDMETHOD.

  METHOD set_exits.
    north = set_exit( n ).
    east  = set_exit( e ).
    south = set_exit( s ).
    west  = set_exit( w ).
  ENDMETHOD.
  METHOD set_exit.
    IF room IS BOUND.
      exit = room.
    ELSE.
      exit = no_exit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS actor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( name = name descr = descr ).
    things = NEW #( ).
  ENDMETHOD.

  METHOD set_location.
    location = room.
  ENDMETHOD.

  METHOD get_location.
    room = location.
  ENDMETHOD.

  METHOD speak.
    sentences = my_sentences.
  ENDMETHOD.

  METHOD add_sentences.
    my_sentences = sentences.
  ENDMETHOD.
ENDCLASS.
