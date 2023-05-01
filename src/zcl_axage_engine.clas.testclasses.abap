*"* use this source file for your ABAP unit test classes
CLASS ltcl_engine DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_axage_engine.
    METHODS:
      setup,
      map_simple FOR TESTING RAISING cx_static_check,
      take_and_drop FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_engine IMPLEMENTATION.

  METHOD map_simple.

    "floor plan configuration
    DATA(room_left) = NEW zcl_axage_room( name = 'LEFT' descr = 'Left room' ).
    DATA(room_right) = NEW zcl_axage_room( name = 'RIGHT' descr = 'Right room' ).
    room_left->set_exits( e = room_right ).
    room_right->set_exits( w = room_left ).
    cut->map->add_room( room_left ).
    cut->map->add_room( room_right ).
    cut->player->set_location( room_left ).

    "GO WEST -> not possible -> current position still ROOM_LEFT
    cut->interprete( 'W' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->player->get_location( )
      exp = room_left ).
    "GO EAST -> leads to room_right
    cut->interprete( 'e' ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->player->get_location( )
      exp = room_right ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD take_and_drop.
    "floor plan configuration
    DATA(room_left) = NEW zcl_axage_room( name = 'LEFT' descr = 'Left room' ).
    DATA(room_right) = NEW zcl_axage_room( name = 'RIGHT' descr = 'Right room' ).
    room_left->set_exits( e = room_right ).
    DATA(bottle) = NEW zcl_axage_thing( name = 'BOTTLE' descr = 'an empty bottle' ).
    room_left->things->add( bottle ).
    room_right->set_exits( w = room_left ).
    cut->map->add_room( room_left ).
    cut->map->add_room( room_right ).

    cut->player->set_location( room_left ).

    cut->interprete( 'TAKE BOTTLE' ).
    cut->interprete( 'e' ).
    cut->interprete( 'DROP BOTTLE' ).
    cl_abap_unit_assert=>assert_equals(
      act = room_right->things->get_list( )
      exp = VALUE zcl_axage_thing_list=>_things( (  bottle ) ) ).

  ENDMETHOD.

ENDCLASS.
