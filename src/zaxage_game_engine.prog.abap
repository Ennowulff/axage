

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

CLASS engine DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS interprete
      IMPORTING
        command       TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO result.
    METHODS is_completed
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_location
      RETURNING
        VALUE(result) TYPE string.
    METHODS get_inventory
      returning
        value(result) type ref to result.
    DATA player TYPE REF TO actor.
    DATA map TYPE REF TO map.
    DATA actors TYPE REF TO thinglist.
    DATA mission_completed TYPE abap_bool.

  PRIVATE SECTION.
    DATA inventory_container TYPE REF TO cl_gui_container.

    METHODS cmd_look
      IMPORTING
        result TYPE REF TO result
        cmd2   TYPE string OPTIONAL.

ENDCLASS.

CLASS engine IMPLEMENTATION.

  METHOD constructor.

    map = NEW #( ).
    player = NEW #( name = 'PLAYER' descr = 'player name' ).
    actors = NEW #( ).

  ENDMETHOD.

  METHOD interprete.
    DATA cmd TYPE c LENGTH 100.

    result = NEW #(  ).

    cmd = to_upper( command ).

    SPLIT cmd AT space INTO DATA(cmd1) DATA(cmd2).

    CASE cmd1.
      WHEN 'MAP'.
        result->addtab( map->show( ) ).

      WHEN 'N' OR 'NORTH'.
        IF player->location->north = room=>no_exit.
          result->add( 'you cannot go there.' ).
        ELSE.
          player->set_location( player->location->north ).
        ENDIF.
        cmd_look( result ).

      WHEN 'S' OR 'SOUTH'.
        IF player->location->south = room=>no_exit.
          result->add( 'you cannot go there.' ).
        ELSE.
          player->set_location( player->location->south ).
        ENDIF.
        cmd_look( result ).

      WHEN 'E' OR 'EAST'.
        IF player->location->east = room=>no_exit.
          result->add( 'you cannot go there.' ).
        ELSE.
          player->set_location( player->location->east ).
        ENDIF.
        cmd_look( result ).

      WHEN 'W' OR 'WEST'.
        IF player->location->west = room=>no_exit.
          result->add( 'you cannot go there.' ).
        ELSE.
          player->set_location( player->location->west ).
        ENDIF.
        cmd_look( result ).

      WHEN 'HELP'.

        result->add( |N or NORTH        Go to the room on the north side| ).
        result->add( |E or EAST         Go to the room on the east side| ).
        result->add( |S or SOUTH        Go to the room on the south side| ).
        result->add( |W or WEST         Go to the room on the west side| ).
        result->add( |MAP               Show map/ floor plan/ world| ).
        result->add( || ).
        result->add( |INV or INVENTARY  Show everything you carry| ).
        result->add( |LOOK              Look what''s in the room| ).
        result->add( |LOOK <object>     Have a closer look at the object in the room or in your inventory| ).
        result->add( |TAKE <object>     Take object in the room| ).
        result->add( |DROP <object>     Drop an object that you carry| ).
        result->add( |OPEN <object>     Open something that is in the room| ).
        result->add( || ).
        result->add( |ASK <person>      Ask a person to tell you something| ).


      WHEN 'LOOK'.
        cmd_look(
              result = result
              cmd2   = cmd2 ).

      WHEN 'TAKE'.
        IF player->location->things->get_list( ) IS INITIAL.
          result->add( 'There is nothing you can take' ).
        ELSE.
          IF player->location->things->exists( cmd2 ).
            result->add( |You take the { cmd2 }| ).
            player->things->add( player->location->things->get( cmd2 ) ).
            player->location->things->delete( cmd2 ).
          ELSE.
            result->add( |You cannot take the { cmd2 }| ).
          ENDIF.
        ENDIF.

      WHEN 'DROP'.
        IF player->things->get_list( ) IS INITIAL.
          result->add( 'There is nothing you can drop' ).
        ELSE.
          IF player->things->exists( cmd2 ).
            result->add( |You drop the { cmd2 }| ).
            player->location->things->add( player->things->get( cmd2 ) ).
            player->things->delete( cmd2 ).
          ELSE.
            result->add( |You cannot drop the { cmd2 }| ).
          ENDIF.
        ENDIF.

      WHEN 'INV' OR 'INVENTORY'.
        IF player->things->get_list( ) IS INITIAL.
          result->add( 'You don''t carry anything' ).
        ELSE.
          result->add( 'You carry' ).
          result->addtab( player->things->show( ) ).
        ENDIF.

      WHEN 'OPEN'.
        IF cmd2 IS INITIAL.
          result->add( 'Open what?' ).
        ELSEIF player->things->get_list( ) IS INITIAL
        AND    player->location->things->get_list( ) IS INITIAL.
          result->add( 'There is nothing you can open...' ).
        ELSE.
          IF player->things->exists( cmd2 ).
            DATA(thing) = player->things->get( cmd2 ).
          ELSEIF player->location->things->exists( cmd2 ).
            thing = player->location->things->get( cmd2 ).
          ENDIF.

          IF thing IS INSTANCE OF openable_thing.
            DATA(thing_to_open) = CAST openable_thing( thing ).
            DATA finds TYPE string_table.
            result->add( thing_to_open->open( player->things )->get( ) ).
            IF thing_to_open->is_open( ).

              LOOP AT thing_to_open->get_content( )->get_list( ) INTO DATA(content).
                APPEND |a { content->name }| TO finds.
              ENDLOOP.
              result->add( |The { thing->name } contains:| ).
              result->addtab( finds ).
              player->things->add( content ).
            ENDIF.
          ELSEif thing is bound.
            result->add( |{ thing->name } cannot be opened!| ).
          else.
            result->add( |You cannot open { cmd2 }| ).
          ENDIF.
        ENDIF.

      WHEN 'ASK'.
        DATA actors_in_the_room TYPE STANDARD TABLE OF REF TO actor.
        DATA actor TYPE REF TO actor.
        LOOP AT actors->get_list( ) INTO thing.
          actor ?= thing.
          IF actor->get_location( ) = player->location.
            APPEND actor TO actors_in_the_room.
          ENDIF.
        ENDLOOP.

        IF actors_in_the_room IS INITIAL.
          result->add( 'There is no one here to ask...' ).
        ELSE.
          IF cmd2 IS INITIAL.
            result->add( 'Whom do you want to ask?' ).
          ELSE.
            LOOP AT actors_in_the_room INTO actor.
              IF to_upper( actor->name ) = cmd2.
                result->addtab( actor->speak( ) ).
              ELSE.
                result->add( |You cannot ask { cmd2 }| ).
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        result->add( 'You cannot do that' ).
    ENDCASE.

  ENDMETHOD.

  METHOD is_completed.
    result = mission_completed.
  ENDMETHOD.

  METHOD get_inventory.

    data inv type abap_bool.

    result = new #( ).
    LOOP AT player->things->get_list( ) INTO DATA(thing_inv).
      IF inv IS INITIAL.
        result->add( |You are carrying:| ).
      ENDIF.
      result->add( |{ thing_inv->name } - { thing_inv->description }| ).
    ENDLOOP.

    IF sy-subrc > 0.
      result->add( |Your inventory is empty...| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_location.
    result = player->location->name.
  ENDMETHOD.


  METHOD cmd_look.

    IF cmd2 IS INITIAL.
      DATA actor TYPE REF TO actor.
      LOOP AT actors->get_list( ) INTO DATA(thing).
        actor ?= thing.
        IF actor->get_location( ) = player->location.
          result->add( |There is { actor->name }, { actor->description }| ).
        ENDIF.
      ENDLOOP.

      IF player->location->things->get_list( ) IS INITIAL.
        result->add( 'There is nothing interesting to see...' ).
      ELSE.
        result->add( |You see| ).
        result->addtab( player->location->things->show( ) ).
      ENDIF.

      IF player->location->east <> room=>no_exit.
        result->add( 'There is a door on the east side' ).
      ENDIF.
      IF player->location->west <> room=>no_exit.
        result->add(  'There is a door on the west side' ).
      ENDIF.
      IF player->location->north <> room=>no_exit.
        result->add( 'There is a door on the north side' ).
      ENDIF.
      IF player->location->south <> room=>no_exit.
        result->add( 'There is a door on the south side' ).
      ENDIF.

    ELSE.
      IF player->location->things->exists( cmd2 ).
        result->add( |It's { player->location->things->get( cmd2 )->description }| ).
      ELSEIF player->things->exists( cmd2 ).
        result->add( |It's { player->things->get( cmd2 )->description }| ).
      ELSE.
        result->add( |You cannot look at the { cmd2 }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
