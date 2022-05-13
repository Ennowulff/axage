REPORT zabapventure03 NO STANDARD PAGE HEADING.

" next: add actors
" command like
" - OPEN BOX WITH KNIFE
" - GIVE DOCUMENT TO BILL
" new class INTERPRETER/ GAME LOGIC
" - Main class must only have general objects (map, actors, ?)
" - method INTERPRETE must be a general class
" - give complete game instance to interpretere or only necessary objects (map, actors)?
" separate GUI from GAME class
" - Main game class should only have the definitions of map, actors
"

INCLUDE zabapventure_class.



CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS interprete
      IMPORTING
        command       TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO result.
    METHODS init_view
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS init_inventory
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS is_completed
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_location
      RETURNING
        VALUE(result) TYPE string.
  PRIVATE SECTION.
    DATA player TYPE REF TO actor.
    data bill_developer type REF TO actor.
    data mark_consultant type ref to actor.
    DATA map TYPE REF TO map.

    DATA container TYPE REF TO cl_gui_container.
    DATA inventory_container TYPE REF TO cl_gui_container.
    DATA text TYPE REF TO cl_gui_textedit.
    DATA inventory TYPE REF TO cl_gui_textedit.
    DATA mission_completed TYPE abap_bool.
    METHODS show_inventory.
    METHODS cmd_look
      IMPORTING
        result TYPE REF TO result
        cmd2   TYPE string OPTIONAL.

ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD init_view.
    me->container = container.
    me->text = NEW #( parent = container ).
    me->text->set_readonly_mode( 1 ).
    me->text->set_toolbar_mode( 0 ).
    me->text->set_statusbar_mode( 0 ).
    me->text->set_font_fixed( 1 ).
  ENDMETHOD.

  METHOD init_inventory.
    me->inventory_container = container.
    me->inventory = NEW #( parent = inventory_container ).
    me->inventory->set_readonly_mode( 1 ).
    me->inventory->set_toolbar_mode( 0 ).
    me->inventory->set_statusbar_mode( 0 ).
    me->inventory->set_font_fixed( 1 ).
  ENDMETHOD.

  METHOD constructor.

    map = NEW #( ).

    DATA(entrance)   = NEW room( name = 'Entrance' descr = 'You are in the entrance area. Welcome.' ).
    DATA(developer)  = NEW room( name = 'Developers office' descr = 'The developers area. be quiet!' ).
    DATA(consulting) = NEW room( name = 'Consulting Department' descr = 'This is the area where the consultants work. Bring coffee!' ).

    map->add_room( entrance ).
    map->add_room( developer ).
    map->add_room( consulting ).
    map->set_floor_plan( value #(
      ( `+--------------------+ +--------------------+` )
      ( `|                    | |                    |` )
      ( `|                    | |                    |` )
      ( `|                    +-+                    |` )
      ( `|     ENTRANCE              DEVELOPERS      |` )
      ( `|                    +-+                    |` )
      ( `|                    | |                    |` )
      ( `|                    | |                    |` )
      ( `+--------+  +--------+ +--------------------+` )
      ( `         |  |` )
      ( `+--------+  +--------+` )
      ( `|                    |` )
      ( `|                    |` )
      ( `|                    |` )
      ( `|   CONSULTANTS      |` )
      ( `|                    |` )
      ( `|                    |` )
      ( `|                    |` )
      ( `+--------------------+` ) ) ).

    entrance->set_exits(
      e = developer
      s = consulting ).
    developer->set_exits(
      w = entrance ).
    consulting->set_exits(
      n = entrance ).
    DATA(cutter_knife) = NEW thing( name = 'KNIFE' descr = 'a very sharp cutter knife' ).
    developer->things->add( cutter_knife ).
    DATA(needed_to_open_box) = NEW thinglist(  ).
    needed_to_open_box->add( cutter_knife ).
    DATA(content_of_box) = NEW thinglist( ).
    content_of_box->add( NEW thing( name = 'RFC' descr = 'The request for change.' ) ).
    DATA(card_box) = NEW openable_thing(
      name = 'BOX'
      descr = 'a little card box'
      content = content_of_box
      needed = needed_to_open_box ).
    consulting->things->add( card_box ).

    player = NEW #( name = 'PLAYER' descr = 'player name' ).
    player->set_location( entrance ).

    bill_developer = new #( name = 'Bill' descr = 'An ABAP developer' ).
    bill_developer->set_location( developer ).

    mark_consultant = new #( name = 'Mark' descr = 'A sales consultant' ).
    mark_consultant->set_location( consulting ).

  ENDMETHOD.

  METHOD interprete.
    DATA cmd TYPE c LENGTH 100.
*    DATA(cr) = cl_abap_char_utilities=>cr_lf.

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
          ELSE.
            result->add( |{ thing->name } cannot be opened!| ).
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        result->add( 'You cannot do that' ).
    ENDCASE.

*    result->add( |You are in the { player->location->name }.| ). " { player->location->description }|.
    text->set_textstream( result->get( ) ).

    IF player->location->things->exists( 'RFC' ).
      mission_completed = abap_true.
    ENDIF.

    show_inventory( ).

  ENDMETHOD.

  METHOD is_completed.
    result = mission_completed.
  ENDMETHOD.


  METHOD show_inventory.

    DATA inv TYPE string.
    LOOP AT player->things->get_list( ) INTO DATA(thing_inv).
      IF inv IS INITIAL.
        inv = |You are carrying:{ cl_abap_char_utilities=>cr_lf }|.
      ENDIF.
      inv = |{ inv }{ thing_inv->name } - { thing_inv->description }{ cl_abap_char_utilities=>cr_lf }|.
    ENDLOOP.
    IF inv IS INITIAL.
      inv = |Your inventory is empty...|.
    ENDIF.
    inventory->set_textstream( inv  ).

  ENDMETHOD.


  METHOD get_location.
    result = player->location->name.
  ENDMETHOD.


  METHOD cmd_look.

    IF cmd2 IS INITIAL.

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


SELECTION-SCREEN COMMENT /1(70) TEXT-in1.
SELECTION-SCREEN COMMENT /1(70) TEXT-in2.
PARAMETERS input TYPE c LENGTH 70.
PARAMETERS location TYPE c LENGTH 70 MODIF ID dsp.


INITIALIZATION.

  DATA(app) = NEW main( ).
  DATA(docker) = NEW cl_gui_docking_container( ratio = 80 side = cl_gui_docking_container=>dock_at_bottom ).
  DATA(inventory) = NEW cl_gui_docking_container( ratio = 40 side = cl_gui_docking_container=>dock_at_right ).
  app->init_view( docker ).
  app->init_inventory( inventory ).

START-OF-SELECTION.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'DSP'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  app->interprete( input ).
  CLEAR input.
  location = app->get_location(  ).

  IF app->is_completed(  ).
    MESSAGE 'Congratulations! You delivered the RFC to the developers!' TYPE 'I'.
  ENDIF.
