REPORT zaxage_demo_01 NO STANDARD PAGE HEADING.

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

INCLUDE zaxage_game_engine.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS init_view
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS init_inventory
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS interprete
      IMPORTING
        command       TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO result.
    METHODS show_inventory.
    METHODS get_location
      RETURNING
        VALUE(result) TYPE string.
    METHODS is_completed
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
    DATA bill_developer TYPE REF TO actor.
    DATA mark_consultant TYPE REF TO actor.

    DATA container TYPE REF TO cl_gui_container.
    DATA inventory_container TYPE REF TO cl_gui_container.
    DATA text TYPE REF TO cl_gui_textedit.
    DATA inventory TYPE REF TO cl_gui_textedit.
    DATA engine TYPE REF TO engine.

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

    engine = NEW #( ).

    DATA(entrance)   = NEW room( name = 'Entrance' descr = 'You are in the entrance area. Welcome.' ).
    DATA(developer)  = NEW room( name = 'Developers office' descr = 'The developers area. be quiet!' ).
    DATA(consulting) = NEW room( name = 'Consulting Department' descr = 'This is the area where the consultants work. Bring coffee!' ).

    engine->map->add_room( entrance ).
    engine->map->add_room( developer ).
    engine->map->add_room( consulting ).
    engine->map->set_floor_plan( VALUE #(
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

    engine->player->set_location( entrance ).

    bill_developer = NEW #( name = 'Bill' descr = 'An ABAP developer' ).
    bill_developer->set_location( developer ).
    bill_developer->add_sentences( VALUE #(
      ( |Hey, I am Bill, an experienced ABAP developer.| )
      ( |If you have programming tasks for me, you can pass the requirement to me| ) ) ).

    mark_consultant = NEW #( name = 'Mark' descr = 'An SAP consultant' ).
    mark_consultant->set_location( consulting ).
    mark_consultant->add_sentences( VALUE #(
      ( |Hello, My name is Mark and I am an SAP consultant| )
      ( |You can ask me anything about SAP processes.| ) ) ).

    engine->actors->add( bill_developer ).
    engine->actors->add( mark_consultant ).

  ENDMETHOD.



  METHOD interprete.
    result = engine->interprete( command ).

    result->add( |You are in the { engine->player->location->name }.| ). " { player->location->description }|.
    text->set_textstream( result->get( ) ).

    IF engine->player->location->things->exists( 'RFC' ).
      engine->mission_completed = abap_true.
    ENDIF.

    show_inventory( ).

  ENDMETHOD.

  METHOD show_inventory.
    inventory->set_textstream( engine->get_inventory( )->get( ) ).
  ENDMETHOD.

  METHOD get_location.
    result = engine->get_location( ).
  ENDMETHOD.

  METHOD is_completed.
    result = engine->is_completed( ).
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
