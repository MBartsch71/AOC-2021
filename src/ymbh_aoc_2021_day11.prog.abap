REPORT ymbh_aoc_2021_day11.

INTERFACE if_octopus.
  TYPES: BEGIN OF coordinates,
           x TYPE i,
           y TYPE i,
         END OF coordinates.
  DATA value TYPE i.
  DATA location TYPE coordinates.
  DATA flashed TYPE abap_bool.

  METHODS get_value       RETURNING VALUE(result) TYPE i.

  METHODS get_coordinates RETURNING VALUE(result) TYPE coordinates.

  METHODS has_flashed RETURNING VALUE(result) TYPE abap_bool.

  METHODS set_flashed.

  METHODS reset_flashed.

ENDINTERFACE.

INTERFACE if_octopus_collection.
  TYPES: BEGIN OF octopus,
           x          TYPE i,
           y          TYPE i,
           octopus    TYPE REF TO if_octopus,
           neighbours TYPE REF TO if_octopus_collection,
         END OF octopus.
  TYPES octopusses TYPE SORTED TABLE OF octopus WITH UNIQUE KEY primary_key COMPONENTS x y.

  TYPES: BEGIN OF octopus_display,
           x     TYPE i,
           y     TYPE i,
           value TYPE i,
         END OF octopus_display.
  TYPES octopusses_display TYPE SORTED TABLE OF octopus_display WITH UNIQUE KEY primary_key COMPONENTS x y.

  METHODS add                        IMPORTING octopus TYPE REF TO if_octopus.

  METHODS get_count                  RETURNING VALUE(result) TYPE i.

  METHODS get_item_by_location       IMPORTING location      TYPE if_octopus=>coordinates
                                     RETURNING VALUE(result) TYPE REF TO if_octopus.

  METHODS get_neighbours_by_location IMPORTING location      TYPE if_octopus=>coordinates
                                     RETURNING VALUE(result) TYPE REF TO if_octopus_collection.

  METHODS get_item                   IMPORTING index         TYPE i
                                     RETURNING VALUE(result) TYPE octopus.

  METHODS get_collection_for_display RETURNING VALUE(result) TYPE octopusses_display.

ENDINTERFACE.

INTERFACE if_iterator.
  METHODS get_next RETURNING VALUE(result) TYPE if_octopus_collection=>octopus.
  METHODS has_next RETURNING VALUE(result) TYPE abap_bool.
ENDINTERFACE.

INTERFACE if_flash.
  METHODS times     IMPORTING times TYPE i.
  METHODS get_count RETURNING VALUE(result) TYPE i.
  METHODS get_last_step RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

CLASS octopus DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_octopus.
    METHODS constructor IMPORTING value    TYPE i
                                  location TYPE if_octopus=>coordinates.

ENDCLASS.

INTERFACE if_application.
  TYPES range_input TYPE RANGE OF text100.
ENDINTERFACE.

CLASS octopus IMPLEMENTATION.

  METHOD constructor.
    me->if_octopus~value = value.
    me->if_octopus~location = location.
  ENDMETHOD.

  METHOD if_octopus~get_coordinates.
    result = me->if_octopus~location.
  ENDMETHOD.

  METHOD if_octopus~get_value.
    result = me->if_octopus~value.
  ENDMETHOD.

  METHOD if_octopus~has_flashed.
    result = if_octopus~flashed.
  ENDMETHOD.

  METHOD if_octopus~reset_flashed.
    if_octopus~flashed = abap_false.
  ENDMETHOD.

  METHOD if_octopus~set_flashed.
    if_octopus~flashed = abap_true.
  ENDMETHOD.

ENDCLASS.

CLASS octopus_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_octopus_collection.
    METHODS build_neighbours.

  PRIVATE SECTION.
    DATA collection TYPE if_octopus_collection=>octopusses.
    METHODS get_neighbour IMPORTING item          TYPE REF TO if_octopus
                                    direction     TYPE string
                          RETURNING VALUE(result) TYPE REF TO if_octopus.

ENDCLASS.

CLASS octopus_collection IMPLEMENTATION.

  METHOD if_octopus_collection~get_count.
    result = lines( collection ).
  ENDMETHOD.

  METHOD if_octopus_collection~add.
    collection = VALUE #( BASE collection ( x = octopus->location-x y = octopus->location-y octopus = octopus ) ).
  ENDMETHOD.

  METHOD if_octopus_collection~get_item_by_location.
    TRY.
        result = collection[ x = location-x y = location-y ]-octopus.
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.

  METHOD if_octopus_collection~get_item.
    result = collection[ index ].
  ENDMETHOD.

  METHOD if_octopus_collection~get_collection_for_display.
    LOOP AT collection REFERENCE INTO DATA(line).
      result = VALUE #( BASE result ( x = line->x
                                      y = line->y
                                      value = line->octopus->get_value( )  ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_octopus_collection~get_neighbours_by_location.
    result = collection[ x = location-x y = location-y ]-neighbours.
  ENDMETHOD.

  METHOD build_neighbours.
    DATA(directions) = VALUE stringtab( ( `E` )  ( `SE` ) ( `S` )
                                        ( `SW` ) ( `W` )  ( `NW` )
                                        ( `N` )  ( `NE` ) ).
    LOOP AT collection REFERENCE INTO DATA(item).
      item->neighbours = NEW octopus_collection( ).
      LOOP AT directions REFERENCE INTO DATA(direction).
        DATA(neighbour) = get_neighbour( item = item->octopus direction = direction->* ).
        IF neighbour IS BOUND.
          item->neighbours->add( neighbour ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_neighbour.
    DATA(target_location)
        = SWITCH if_octopus=>coordinates(
                direction WHEN `E`
                            THEN VALUE #( x = item->get_coordinates( )-x + 1 y = item->get_coordinates( )-y )
                          WHEN `SE`
                            THEN VALUE #( x = item->get_coordinates( )-x + 1 y = item->get_coordinates( )-y + 1 )
                          WHEN `S`
                            THEN VALUE #( x = item->get_coordinates( )-x     y = item->get_coordinates( )-y + 1 )
                          WHEN `SW`
                            THEN VALUE #( x = item->get_coordinates( )-x - 1 y = item->get_coordinates( )-y + 1 )
                          WHEN `W`
                            THEN VALUE #( x = item->get_coordinates( )-x - 1 y = item->get_coordinates( )-y )
                          WHEN `NW`
                            THEN VALUE #( x = item->get_coordinates( )-x - 1 y = item->get_coordinates( )-y - 1 )
                          WHEN `N`
                            THEN VALUE #( x = item->get_coordinates( )-x     y = item->get_coordinates( )-y - 1 )
                          WHEN `NE`
                            THEN VALUE #( x = item->get_coordinates( )-x + 1 y = item->get_coordinates( )-y - 1 ) ).
    result = if_octopus_collection~get_item_by_location( target_location ).
  ENDMETHOD.


ENDCLASS.

CLASS iterator DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_iterator.
    CLASS-METHODS get_instance IMPORTING collection    TYPE REF TO object
                               RETURNING VALUE(result) TYPE REF TO iterator.
    METHODS constructor IMPORTING collection TYPE REF TO object.

  PRIVATE SECTION.
    DATA collection TYPE REF TO octopus_collection.
    DATA current_item TYPE i VALUE 1.
ENDCLASS.

CLASS iterator IMPLEMENTATION.

  METHOD get_instance.
    result = NEW iterator( collection ).
  ENDMETHOD.

  METHOD constructor.
    me->collection = CAST octopus_collection( collection ).
  ENDMETHOD.

  METHOD if_iterator~get_next.
    result = collection->if_octopus_collection~get_item( current_item ).
    current_item = current_item + 1.
  ENDMETHOD.

  METHOD if_iterator~has_next.
    result = xsdbool( current_item <= collection->if_octopus_collection~get_count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS flash DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_flash.

    METHODS constructor                IMPORTING collection TYPE REF TO if_octopus_collection.

    METHODS get_collection_for_display RETURNING VALUE(result) TYPE if_octopus_collection=>octopusses_display.

  PRIVATE SECTION.
    DATA collection TYPE REF TO if_octopus_collection.
    DATA total_flashes TYPE i.
    DATA step TYPE i.

    METHODS update_item                    IMPORTING item TYPE if_octopus_collection=>octopus.

    METHODS increase_octopus_value         IMPORTING item TYPE if_octopus_collection=>octopus.

    METHODS increase_depending_flash_state IMPORTING item          TYPE if_octopus_collection=>octopus
                                           RETURNING VALUE(result) TYPE i.

    METHODS flash                          IMPORTING item          TYPE if_octopus_collection=>octopus
                                           RETURNING VALUE(result) TYPE i.

    METHODS increase_neighbours            IMPORTING item TYPE if_octopus_collection=>octopus.

ENDCLASS.

CLASS flash IMPLEMENTATION.

  METHOD if_flash~times.
    DO times TIMES.
      DATA(iterator) = iterator=>get_instance( collection ).
      WHILE iterator->if_iterator~has_next( ).
        DATA(item) = iterator->if_iterator~get_next( ).
        update_item( item ).
      ENDWHILE.

      DATA(reset_iterator) = iterator=>get_instance( collection ).
      WHILE reset_iterator->if_iterator~has_next( ).
        DATA(octopus) = reset_iterator->if_iterator~get_next( ).
        octopus-octopus->reset_flashed( ).
      ENDWHILE.
      IF total_flashes = 100.
        step = sy-index.
        RETURN.
      ENDIF.
      total_flashes = 0.
    ENDDO.

  ENDMETHOD.

  METHOD constructor.
    me->collection = collection.
  ENDMETHOD.

  METHOD get_collection_for_display.
    DATA(iterator) = iterator=>get_instance( collection ).
    WHILE iterator->if_iterator~has_next( ).
      DATA(item) = iterator->if_iterator~get_next( ).
      result = VALUE #( BASE result ( x = item-x y = item-y value = item-octopus->get_value( ) ) ).
    ENDWHILE.
  ENDMETHOD.

  METHOD update_item.
    increase_octopus_value( item ).
  ENDMETHOD.

  METHOD increase_octopus_value.
    item-octopus->value = SWITCH #( item-octopus->value WHEN 0 THEN increase_depending_flash_state( item )
                                              WHEN 9 THEN flash( item )
                                              ELSE item-octopus->value + 1 ).
  ENDMETHOD.

  METHOD increase_depending_flash_state.
    IF item-octopus->has_flashed( ).
      RETURN.
    ENDIF.
    result = 1.
  ENDMETHOD.

  METHOD flash.
    total_flashes = total_flashes + 1.
    item-octopus->set_flashed( ).
    item-octopus->value = 0.
    increase_neighbours( item ).
  ENDMETHOD.

  METHOD increase_neighbours.
    DATA(neighbours) = collection->get_neighbours_by_location( VALUE #( x = item-x y = item-y ) ).
    DATA(neighbour_iterator) = iterator=>get_instance( neighbours ).
    WHILE neighbour_iterator->if_iterator~has_next( ).
      DATA(neighbour) = neighbour_iterator->if_iterator~get_next( ).
      increase_octopus_value( neighbour ).
    ENDWHILE.
  ENDMETHOD.

  METHOD if_flash~get_count.
    result = total_flashes.
  ENDMETHOD.

  METHOD if_flash~get_last_step.
    result = step.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor       IMPORTING range_input TYPE if_application=>range_input.

    METHODS get_result_part_1 IMPORTING flashes       TYPE i
                              RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input TYPE stringtab.
    DATA collection TYPE REF TO octopus_collection.
    DATA flash TYPE REF TO if_flash.

    METHODS build_octopus_collection.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    input = VALUE #( FOR line IN range_input
                        ( CONV #( line-low ) ) ).
    collection = NEW octopus_collection( ).
    flash = NEW flash( collection ).
  ENDMETHOD.


  METHOD get_result_part_1.
    build_octopus_collection( ).
    collection->build_neighbours( ).
    flash->times( flashes ).
    result = flash->get_last_step( ).
  ENDMETHOD.

  METHOD build_octopus_collection.
    DATA offset TYPE i.
    DATA line_number TYPE i.
    LOOP AT input REFERENCE INTO DATA(line).
      line_number = sy-tabix - 1.
      WHILE offset < strlen( line->* ).
        DATA(value) = substring( val = line->* off = offset len = 1 ).
        collection->if_octopus_collection~add( NEW octopus( location = VALUE #( x = offset y = line_number ) value = CONV #( value ) ) ).
        offset = offset + 1.
      ENDWHILE.
      offset = 0.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS tc_octopus DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO octopus.

    METHODS setup.
    METHODS get_value_from_octopus FOR TESTING.
    METHODS set_octopus_flashed    FOR TESTING.
    METHODS reset_flashed_octopus  FOR TESTING.
    METHODS get_coordinates        FOR TESTING.
ENDCLASS.

CLASS tc_octopus IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( location = VALUE #( x = 0 y = 0 ) value = 3 ).
  ENDMETHOD.

  METHOD get_value_from_octopus.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->if_octopus~get_value( ) ).
  ENDMETHOD.

  METHOD set_octopus_flashed.
    cut->if_octopus~set_flashed( ).
    cl_abap_unit_assert=>assert_true(
        act = cut->if_octopus~has_flashed( ) ).
  ENDMETHOD.

  METHOD reset_flashed_octopus.
    cut->if_octopus~set_flashed( ).
    cut->if_octopus~reset_flashed( ).
    cl_abap_unit_assert=>assert_false(
        act = cut->if_octopus~has_flashed( ) ).
  ENDMETHOD.

  METHOD get_coordinates.
    DATA(expected_coordinates) = VALUE if_octopus=>coordinates( x = 0 y = 0 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_coordinates
        act = cut->if_octopus~get_coordinates( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_octopus_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO octopus_collection.

    METHODS setup.
    METHODS fill_collection_with_3_octos FOR TESTING.
    METHODS get_octopus_by_location      FOR TESTING.
    METHODS get_whole_collection         FOR TESTING.
    METHODS get_octopus_coll_for_display FOR TESTING.
    METHODS get_neighbours_by_location   FOR TESTING.

ENDCLASS.

CLASS tc_octopus_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 0 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 1 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 2 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 3 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 4 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 0 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 1 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 2 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 3 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 4 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 0 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 1 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 2 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 3 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 4 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 0 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 1 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 2 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 3 ) value = 9 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 4 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 0 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 1 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 2 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 3 ) value = 1 ) ).
    cut->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 4 ) value = 1 ) ).
  ENDMETHOD.

  METHOD fill_collection_with_3_octos.
    cl_abap_unit_assert=>assert_equals(
        exp = 25
        act = cut->if_octopus_collection~get_count( ) ).
  ENDMETHOD.

  METHOD get_octopus_by_location.
    DATA(cell) = cut->if_octopus_collection~get_item_by_location( VALUE #( x = 1 y = 1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 9
        act = cell->get_value( ) ).
  ENDMETHOD.

  METHOD get_whole_collection.
    DATA(octopus) = cut->if_octopus_collection~get_item( 1 )-octopus.
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = octopus->get_value( ) ).
  ENDMETHOD.

  METHOD get_octopus_coll_for_display.
    DATA(expected_values) = VALUE if_octopus_collection=>octopusses_display( ( x = 0 y = 0 value = 1 )
                                                                             ( x = 0 y = 1 value = 1 )
                                                                             ( x = 0 y = 2 value = 1 )
                                                                             ( x = 0 y = 3 value = 1 )
                                                                             ( x = 0 y = 4 value = 1 )
                                                                             ( x = 1 y = 0 value = 1 )
                                                                             ( x = 1 y = 1 value = 9 )
                                                                             ( x = 1 y = 2 value = 9 )
                                                                             ( x = 1 y = 3 value = 9 )
                                                                             ( x = 1 y = 4 value = 1 )
                                                                             ( x = 2 y = 0 value = 1 )
                                                                             ( x = 2 y = 1 value = 9 )
                                                                             ( x = 2 y = 2 value = 1 )
                                                                             ( x = 2 y = 3 value = 9 )
                                                                             ( x = 2 y = 4 value = 1 )
                                                                             ( x = 3 y = 0 value = 1 )
                                                                             ( x = 3 y = 1 value = 9 )
                                                                             ( x = 3 y = 2 value = 9 )
                                                                             ( x = 3 y = 3 value = 9 )
                                                                             ( x = 3 y = 4 value = 1 )
                                                                             ( x = 4 y = 0 value = 1 )
                                                                             ( x = 4 y = 1 value = 1 )
                                                                             ( x = 4 y = 2 value = 1 )
                                                                             ( x = 4 y = 3 value = 1 )
                                                                             ( x = 4 y = 4 value = 1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->if_octopus_collection~get_collection_for_display( ) ).
  ENDMETHOD.

  METHOD get_neighbours_by_location.
    cut->build_neighbours( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 8
        act = cut->if_octopus_collection~get_neighbours_by_location( VALUE #( x = 2 y = 2 ) )->get_count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_iterator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_iterator.

    METHODS setup.
    METHODS get_3_items_from_collection FOR TESTING.
ENDCLASS.


CLASS tc_iterator IMPLEMENTATION.

  METHOD get_3_items_from_collection.
    DATA count TYPE i.
    WHILE cut->has_next( ).
      DATA(item) = cut->get_next( ).
      count = count + item-octopus->get_value( ).
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = count ).
  ENDMETHOD.

  METHOD setup.
    DATA(collection) = NEW octopus_collection( ).
    collection->if_octopus_collection~add( NEW octopus( location = VALUE #( x = 0 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( NEW octopus( location = VALUE #( x = 0 y = 1 ) value = 2 ) ).
    collection->if_octopus_collection~add( NEW octopus( location = VALUE #( x = 0 y = 2 ) value = 3 ) ).
    cut = iterator=>get_instance( collection ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_flash DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO flash.

    METHODS setup.
    METHODS flash_1_time FOR TESTING.

ENDCLASS.


CLASS tc_flash IMPLEMENTATION.

  METHOD flash_1_time.
    DATA(expected_values) = VALUE if_octopus_collection=>octopusses_display( ( x = 0 y = 0 value = 3 )
                                                                             ( x = 0 y = 1 value = 4 )
                                                                             ( x = 0 y = 2 value = 5 )
                                                                             ( x = 0 y = 3 value = 4 )
                                                                             ( x = 0 y = 4 value = 3 )
                                                                             ( x = 1 y = 0 value = 4 )
                                                                             ( x = 1 y = 1 value = 0 )
                                                                             ( x = 1 y = 2 value = 0 )
                                                                             ( x = 1 y = 3 value = 0 )
                                                                             ( x = 1 y = 4 value = 4 )
                                                                             ( x = 2 y = 0 value = 5 )
                                                                             ( x = 2 y = 1 value = 0 )
                                                                             ( x = 2 y = 2 value = 0 )
                                                                             ( x = 2 y = 3 value = 0 )
                                                                             ( x = 2 y = 4 value = 5 )
                                                                             ( x = 3 y = 0 value = 4 )
                                                                             ( x = 3 y = 1 value = 0 )
                                                                             ( x = 3 y = 2 value = 0 )
                                                                             ( x = 3 y = 3 value = 0 )
                                                                             ( x = 3 y = 4 value = 4 )
                                                                             ( x = 4 y = 0 value = 3 )
                                                                             ( x = 4 y = 1 value = 4 )
                                                                             ( x = 4 y = 2 value = 5 )
                                                                             ( x = 4 y = 3 value = 4 )
                                                                             ( x = 4 y = 4 value = 3 ) ).
    cut->if_flash~times( 1 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_collection_for_display( ) ).
  ENDMETHOD.

  METHOD setup.
    DATA(collection) = NEW octopus_collection( ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 1 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 2 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 3 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 0 y = 4 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 1 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 2 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 3 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 1 y = 4 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 1 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 2 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 3 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 2 y = 4 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 1 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 2 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 3 ) value = 9 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 3 y = 4 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 0 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 1 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 2 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 3 ) value = 1 ) ).
    collection->if_octopus_collection~add( octopus = NEW octopus( location = VALUE #( x = 4 y = 4 ) value = 1 ) ).
    collection->build_neighbours( ).
    cut = NEW flash( collection ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS get_flashes_after_10_steps FOR TESTING.
    METHODS get_flashes_after_100_steps FOR TESTING.
ENDCLASS.


CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE if_application=>range_input( ( sign = 'I' option = 'EQ' low = '5483143223' )
                                                     ( sign = 'I' option = 'EQ' low = '2745854711' )
                                                     ( sign = 'I' option = 'EQ' low = '5264556173' )
                                                     ( sign = 'I' option = 'EQ' low = '6141336146' )
                                                     ( sign = 'I' option = 'EQ' low = '6357385478' )
                                                     ( sign = 'I' option = 'EQ' low = '4167524645' )
                                                     ( sign = 'I' option = 'EQ' low = '2176841721' )
                                                     ( sign = 'I' option = 'EQ' low = '6882881134' )
                                                     ( sign = 'I' option = 'EQ' low = '4846848554' )
                                                     ( sign = 'I' option = 'EQ' low = '5283751526' ) ).
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD get_flashes_after_10_steps.
    cl_abap_unit_assert=>assert_equals(
        exp = 204
        act = cut->get_result_part_1( 10 ) ).
  ENDMETHOD.


  METHOD get_flashes_after_100_steps.
    cl_abap_unit_assert=>assert_equals(
        exp = 1656
        act = cut->get_result_part_1( 100 ) ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text100.

PARAMETERS: steps TYPE i.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(application) = NEW application( so_input[] ).

  WRITE / |Ergebnis Teil 1: { application->get_result_part_1( steps ) }|.
