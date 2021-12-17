REPORT ymbh_aoc_2021_day12.

INTERFACE if_cave.
  DATA name TYPE string READ-ONLY.
  METHODS get_name RETURNING VALUE(result) TYPE string.

ENDINTERFACE.

INTERFACE if_caves_collection.
  DATA collection TYPE STANDARD TABLE OF REF TO if_cave WITH EMPTY KEY READ-ONLY.

  METHODS add              IMPORTING cave TYPE REF TO if_cave.

  METHODS count            RETURNING VALUE(result) TYPE i.

  METHODS get_cave_by_name IMPORTING name          TYPE string
                           RETURNING VALUE(result) TYPE REF TO if_cave.
ENDINTERFACE.

INTERFACE if_caves_map.
  TYPES: BEGIN OF map_entry,
           cave_1 TYPE string,
           cave_2 TYPE string,
         END OF map_entry.
  DATA caves_map TYPE STANDARD TABLE OF map_entry WITH EMPTY KEY READ-ONLY.
  DATA caves_list TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

  METHODS build_caves_map IMPORTING input TYPE stringtab.

  METHODS get_caves_list  RETURNING VALUE(result) TYPE stringtab.

ENDINTERFACE.

INTERFACE if_cave_system.
  TYPES: BEGIN OF cave_connection,
           cave       TYPE REF TO if_cave,
           neighbours TYPE REF TO if_caves_collection,
         END OF cave_connection.
  TYPES caves_connections TYPE SORTED TABLE OF cave_connection WITH UNIQUE KEY primary_key COMPONENTS cave.

  DATA caves TYPE caves_connections READ-ONLY.

  METHODS add                     IMPORTING cave_connection TYPE cave_connection.

  METHODS build                   IMPORTING caves_map TYPE REF TO if_caves_map
                                            caves     TYPE stringtab.

  METHODS get_neighbours_for_cave IMPORTING cave          TYPE REF TO if_cave
                                  RETURNING VALUE(result) TYPE REF TO if_caves_collection.

  METHODS get_cave_by_name        IMPORTING name          TYPE string
                                  RETURNING VALUE(result) TYPE REF TO if_cave.

ENDINTERFACE.

INTERFACE if_iterator.
  DATA collection TYPE REF TO object.

  CLASS-METHODS get_instance IMPORTING collection    TYPE REF TO if_caves_collection
                             RETURNING VALUE(result) TYPE REF TO if_iterator.

  METHODS has_next RETURNING VALUE(result) TYPE abap_bool.
  METHODS get_next RETURNING VALUE(result) TYPE REF TO object.
ENDINTERFACE.

CLASS cave DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_cave.
    METHODS constructor IMPORTING name TYPE string.

ENDCLASS.

CLASS cave IMPLEMENTATION.

  METHOD constructor.
    if_cave~name = name.
  ENDMETHOD.

  METHOD if_cave~get_name.
    result = if_cave~name.
  ENDMETHOD.

ENDCLASS.

CLASS cave_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_caves_collection.

ENDCLASS.

CLASS cave_collection IMPLEMENTATION.

  METHOD if_caves_collection~add.
    if_caves_collection~collection = VALUE #( BASE if_caves_collection~collection ( cave ) ).
  ENDMETHOD.

  METHOD if_caves_collection~count.
    result = lines( if_caves_collection~collection ).
  ENDMETHOD.

  METHOD if_caves_collection~get_cave_by_name.
    result = if_caves_collection~collection[ table_line->name = name ].
  ENDMETHOD.

ENDCLASS.

CLASS caves_map DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_caves_map.
    METHODS constructor IMPORTING input TYPE stringtab.

  PRIVATE SECTION.
    METHODS build_map_line IMPORTING line          TYPE string
                           RETURNING VALUE(result) TYPE if_caves_map=>map_entry.

    METHODS build_caves_list.

    METHODS build_new_lines IMPORTING line          TYPE if_caves_map=>map_entry
                            RETURNING VALUE(result) TYPE stringtab.

ENDCLASS.

CLASS caves_map IMPLEMENTATION.

  METHOD constructor.
    if_caves_map~build_caves_map( input ).
    build_caves_list( ).
  ENDMETHOD.

  METHOD if_caves_map~build_caves_map.
    if_caves_map~caves_map = VALUE #( FOR line IN input
                                        LET map_line = build_map_line( line )
                                        IN
                                        ( map_line ) ).
  ENDMETHOD.

  METHOD build_map_line.
    SPLIT line AT '-' INTO result-cave_1 result-cave_2.
  ENDMETHOD.

  METHOD if_caves_map~get_caves_list.
    result = if_caves_map~caves_list.
  ENDMETHOD.

  METHOD build_caves_list.
    if_caves_map~caves_list = VALUE #( FOR line IN if_caves_map~caves_map
                                            LET new_lines = build_new_lines( line )
                                            IN
                                            ( LINES OF new_lines ) ).
    DELETE ADJACENT DUPLICATES FROM if_caves_map~caves_list USING KEY primary_key.
  ENDMETHOD.

  METHOD build_new_lines.
    result = VALUE #( ( line-cave_1 )
                      ( line-cave_2 ) ).
  ENDMETHOD.

ENDCLASS.


CLASS iterator DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_iterator.
    METHODS constructor IMPORTING collection TYPE REF TO object.

  PRIVATE SECTION.
    DATA collection TYPE REF TO if_caves_collection.
    DATA current_item TYPE i.
ENDCLASS.

CLASS iterator IMPLEMENTATION.

  METHOD constructor.
    me->collection = CAST if_caves_collection( collection ).
  ENDMETHOD.

  METHOD if_iterator~get_instance.
    result = NEW iterator( collection ).
  ENDMETHOD.

  METHOD if_iterator~get_next.
    result = collection->collection[ current_item ].
    current_item = current_item + 1.
  ENDMETHOD.

  METHOD if_iterator~has_next.
    result = xsdbool( current_item <= collection->count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS cave_system DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_cave_system.
  PRIVATE SECTION.
    METHODS populate_caves IMPORTING caves_map TYPE REF TO if_caves_map
                                     caves     TYPE stringtab.
    METHODS search_neighbours IMPORTING caves_map TYPE REF TO if_caves_map.

ENDCLASS.

CLASS cave_system IMPLEMENTATION.

  METHOD if_cave_system~add.
    if_cave_system~caves = VALUE #( BASE if_cave_system~caves ( cave_connection ) ).
  ENDMETHOD.

  METHOD if_cave_system~get_neighbours_for_cave.
    result = if_cave_system~caves[ cave = cave ]-neighbours.
  ENDMETHOD.

  METHOD if_cave_system~build.
    populate_caves( caves_map = caves_map
                    caves     = caves ).
    search_neighbours( caves_map ).
  ENDMETHOD.

  METHOD populate_caves.
    LOOP AT caves_map->caves_list REFERENCE INTO DATA(map_line).
      DATA(cave) = NEW cave( map_line->* ).
      if_cave_system~add( VALUE #( cave = cave ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD search_neighbours.
    LOOP AT if_cave_system~caves REFERENCE INTO DATA(cave_connection).
      DATA(neighbours) = NEW cave_collection( ).
      LOOP AT caves_map->caves_map REFERENCE INTO DATA(map_side_a) WHERE cave_1 = cave_connection->cave->get_name( ).
        neighbours->if_caves_collection~add( NEW cave( map_side_a->cave_2 ) ).
      ENDLOOP.
      LOOP AT caves_map->caves_map REFERENCE INTO DATA(map_side_b) WHERE cave_2 = cave_connection->cave->get_name( ).
        neighbours->if_caves_collection~add( NEW cave( map_side_b->cave_1 ) ).
      ENDLOOP.
      cave_connection->neighbours = neighbours.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_cave_system~get_cave_by_name.
    result = if_cave_system~caves[ cave->name = name ]-cave.
  ENDMETHOD.

ENDCLASS.

CLASS tc_cave DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_cave.

    METHODS setup.
    METHODS build_cave_with_name FOR TESTING.
ENDCLASS.

CLASS tc_cave IMPLEMENTATION.

  METHOD setup.
    cut = NEW cave( `A` ).
  ENDMETHOD.

  METHOD build_cave_with_name.
    cl_abap_unit_assert=>assert_equals(
        exp = `A`
        act = cut->get_name( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_caves_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_caves_collection.

    METHODS setup.
    METHODS build_system_of_3_caves FOR TESTING.
    METHODS get_cave_by_name        FOR TESTING.
ENDCLASS.

CLASS tc_caves_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW cave_collection( ).
  ENDMETHOD.

  METHOD build_system_of_3_caves.
    DO 3 TIMES.
      cut->add( NEW cave( `A` ) ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->count( ) ).
  ENDMETHOD.

  METHOD get_cave_by_name.
    cut->add( NEW cave( `A` ) ).
    cut->add( NEW cave( `b` ) ).
    cut->add( NEW cave( `D` ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = `D`
        act = cut->get_cave_by_name( `D` )->get_name( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_cave_map DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_caves_map.

    METHODS setup.
    METHODS get_caves_map_from_input FOR TESTING.
ENDCLASS.

CLASS tc_cave_map IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( `start-A` )
                                   ( `start-b` )
                                   ( `A-c` )
                                   ( `A-b` )
                                   ( `b-d`  )
                                   ( `A-end` )
                                   ( `b-end` ) ).
    cut = NEW caves_map( input ).
  ENDMETHOD.

  METHOD get_caves_map_from_input.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = lines( cut->get_caves_list( ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_cave_system DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_cave_system.
    DATA caves_map TYPE REF TO caves_map.

    METHODS setup.
    METHODS add_neighbours_to_caves FOR TESTING.
ENDCLASS.

CLASS tc_cave_system IMPLEMENTATION.

  METHOD add_neighbours_to_caves.
    DATA(cave_a) = cut->get_cave_by_name( `A` ).

    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->get_neighbours_for_cave( cave_a )->count( ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW cave_system( ).
    caves_map = NEW caves_map( VALUE #( ( `start-A` ) ( `start-b` ) ( `A-c` )
                                        ( `A-b` ) ( `b-d`  ) ( `A-end` )
                                        ( `b-end` ) ) ).
    DATA(caves) = VALUE stringtab( ( `A` ) ( `b` ) ( `c` ) ( `d` ) ( `start` ) ( `end` ) ).
    cut->build( caves_map = caves_map
                caves     = caves ).
  ENDMETHOD.

ENDCLASS.
