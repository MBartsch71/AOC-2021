REPORT ymbh_aoc_2021_day9.

INTERFACE if_cell.
  TYPES: BEGIN OF coordinates,
           x TYPE i,
           y TYPE i,
         END OF coordinates.
  DATA value TYPE i.
  DATA location TYPE coordinates.

  METHODS get_value       RETURNING VALUE(result) TYPE i.

  METHODS get_coordinates RETURNING VALUE(result) TYPE coordinates.

ENDINTERFACE.

INTERFACE if_cell_collection.
  TYPES: BEGIN OF cell,
           x    TYPE i,
           y    TYPE i,
           cell TYPE REF TO if_cell,
         END OF cell.
  TYPES cells TYPE STANDARD TABLE OF cell WITH EMPTY KEY.

  METHODS add                  IMPORTING cell TYPE REF TO if_cell.

  METHODS get_count            RETURNING VALUE(result) TYPE i.

  METHODS get_item_by_location IMPORTING location      TYPE if_cell=>coordinates
                               RETURNING VALUE(result) TYPE REF TO if_cell.

  METHODS get_item             IMPORTING index         TYPE i
                               RETURNING VALUE(result) TYPE cell.

ENDINTERFACE.

INTERFACE if_neighbourhood.
  TYPES: BEGIN OF neighbour,
           cell                       TYPE REF TO if_cell,
           neighbours                 TYPE REF TO if_cell_collection,
           is_lowest_in_neighbourhood TYPE abap_bool,
         END OF neighbour.
  TYPES neighbours TYPE STANDARD TABLE OF neighbour WITH EMPTY KEY
                                                    WITH NON-UNIQUE SORTED KEY lowest COMPONENTS is_lowest_in_neighbourhood.

  TYPES: BEGIN OF basin_area,
           max_x TYPE i,
           max_y TYPE i,
           min_x TYPE i,
           min_y TYPE i,
         END OF basin_area.
  DATA neighbourhood TYPE neighbours.

  METHODS build_neighbourhood IMPORTING cell_collection TYPE REF TO if_cell_collection.

  METHODS get_count           RETURNING VALUE(result) TYPE i.

  METHODS get_lowest_cells    RETURNING VALUE(result) TYPE if_neighbourhood=>neighbours.

  METHODS get_basin_count     RETURNING VALUE(result) TYPE i.

  METHODS get_cell_by_location  IMPORTING cell          TYPE if_cell_collection=>cell
                                RETURNING VALUE(result) TYPE neighbour.

ENDINTERFACE.

INTERFACE if_iterator.
  METHODS get_next RETURNING VALUE(result) TYPE if_cell_collection=>cell.
  METHODS has_next RETURNING VALUE(result) TYPE abap_bool.
ENDINTERFACE.

CLASS cell DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_cell.
    METHODS constructor IMPORTING value    TYPE i
                                  location TYPE if_cell=>coordinates.

ENDCLASS.

CLASS cell IMPLEMENTATION.

  METHOD constructor.
    me->if_cell~value = value.
    me->if_cell~location = location.
  ENDMETHOD.

  METHOD if_cell~get_coordinates.
    result = me->if_cell~location.
  ENDMETHOD.

  METHOD if_cell~get_value.
    result = me->if_cell~value.
  ENDMETHOD.

ENDCLASS.

CLASS cell_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_cell_collection.

  PRIVATE SECTION.
    DATA collection TYPE if_cell_collection=>cells.

ENDCLASS.

CLASS cell_collection IMPLEMENTATION.

  METHOD if_cell_collection~get_count.
    result = lines( collection ).
  ENDMETHOD.

  METHOD if_cell_collection~add.
    collection = VALUE #( BASE collection ( x = cell->location-x y = cell->location-y cell = cell ) ).
  ENDMETHOD.

  METHOD if_cell_collection~get_item_by_location.
    TRY.
        result = collection[ x = location-x y = location-y ]-cell.
      CATCH cx_sy_itab_line_not_found.
        CLEAR result.
    ENDTRY.
  ENDMETHOD.

  METHOD if_cell_collection~get_item.
    result = collection[ index ].
  ENDMETHOD.

ENDCLASS.

CLASS iterator DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_iterator.
    CLASS-METHODS get_instance IMPORTING collection    TYPE REF TO object
                               RETURNING VALUE(result) TYPE REF TO iterator.
    METHODS constructor IMPORTING collection TYPE REF TO object.

  PRIVATE SECTION.
    DATA collection TYPE REF TO cell_collection.
    DATA current_item TYPE i VALUE 1.
ENDCLASS.

CLASS iterator IMPLEMENTATION.

  METHOD get_instance.
    result = NEW iterator( collection ).
  ENDMETHOD.

  METHOD constructor.
    me->collection = CAST cell_collection( collection ).
  ENDMETHOD.

  METHOD if_iterator~get_next.
    result = collection->if_cell_collection~get_item( current_item ).
    current_item = current_item + 1.
  ENDMETHOD.

  METHOD if_iterator~has_next.
    result = xsdbool( current_item <= collection->if_cell_collection~get_count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS neighbourhood DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_neighbourhood.

  PRIVATE SECTION.
    TYPES: BEGIN OF basin_cell,
             cell TYPE REF TO if_cell,
           END OF basin_cell.
    DATA cell_collection TYPE REF TO if_cell_collection.
    DATA basin_cells TYPE STANDARD TABLE OF basin_cell WITH EMPTY KEY.
    DATA results TYPE STANDARD TABLE OF i.

    METHODS check_lowest_value      IMPORTING neighbours    TYPE REF TO if_cell_collection
                                              cell_value    TYPE i
                                    RETURNING VALUE(result) TYPE abap_bool.
    METHODS investigate_neighbourse IMPORTING cell          TYPE if_neighbourhood=>neighbour
                                    RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS neighbourhood IMPLEMENTATION.

  METHOD if_neighbourhood~build_neighbourhood.
    me->cell_collection = cell_collection.
    DATA(iterator) = iterator=>get_instance( collection = cell_collection ).


    WHILE iterator->if_iterator~has_next( ).
      DATA(cell) = iterator->if_iterator~get_next( )-cell.
      DATA(neighbours) = NEW cell_collection( ).
      DATA(up_cell) = cell_collection->get_item_by_location( location = VALUE #( x = cell->location-x y = cell->location-y - 1 ) ).
      IF up_cell IS BOUND.
        neighbours->if_cell_collection~add( up_cell ).
      ENDIF.
      DATA(right_cell) = cell_collection->get_item_by_location( location = VALUE #( x = cell->location-x + 1 y = cell->location-y ) ).
      IF right_cell IS BOUND.
        neighbours->if_cell_collection~add( right_cell ).
      ENDIF.
      DATA(down_cell) = cell_collection->get_item_by_location( location = VALUE #( x = cell->location-x y = cell->location-y + 1 ) ).
      IF down_cell IS BOUND.
        neighbours->if_cell_collection~add( down_cell ).
      ENDIF.
      DATA(left_cell) = cell_collection->get_item_by_location( location = VALUE #( x = cell->location-x - 1  y = cell->location-y ) ).
      IF left_cell IS BOUND.
        neighbours->if_cell_collection~add( left_cell ).
      ENDIF.

      if_neighbourhood~neighbourhood =
        VALUE #( BASE if_neighbourhood~neighbourhood (
                    cell = cell
                    neighbours = neighbours
                    is_lowest_in_neighbourhood = check_lowest_value( neighbours = neighbours cell_value = cell->value ) ) ).

    ENDWHILE.
  ENDMETHOD.

  METHOD if_neighbourhood~get_count.
    result = lines( me->if_neighbourhood~neighbourhood ).
  ENDMETHOD.

  METHOD check_lowest_value.
    result = abap_true.
    DATA(neighbour_iterator) = iterator=>get_instance( neighbours ).
    WHILE neighbour_iterator->if_iterator~has_next( ).
      DATA(neighbour_cell) = neighbour_iterator->if_iterator~get_next( )-cell.
      IF neighbour_cell->get_value( ) <= cell_value.
        result = abap_false.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD if_neighbourhood~get_lowest_cells.
    result = FILTER #( if_neighbourhood~neighbourhood
                          USING KEY lowest
                          WHERE is_lowest_in_neighbourhood = abap_true ).
  ENDMETHOD.

  METHOD if_neighbourhood~get_basin_count.
    DATA(lowest_cells) = if_neighbourhood~get_lowest_cells( ).

    LOOP AT lowest_cells ASSIGNING FIELD-SYMBOL(<cell>).
      basin_cells = VALUE #( ( cell = <cell>-cell ) ).
      DATA(test) = investigate_neighbourse( <cell> ).
      results = value #( base results ( test ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD investigate_neighbourse.

    DATA(neighbours) = cell-neighbours.
    DATA(iterator) = iterator=>get_instance( neighbours ).
    WHILE iterator->if_iterator~has_next( ).
      DATA(i_cell) = iterator->if_iterator~get_next( ).
      IF i_cell-cell->get_value( ) = 9.
        CONTINUE.
      ENDIF.
      IF line_exists( basin_cells[ cell = i_cell-cell ] ).
        CONTINUE.
      ENDIF.
      basin_cells = VALUE #( BASE basin_cells ( cell = i_cell-cell ) ).
      DATA(nb_cell) = if_neighbourhood~get_cell_by_location( i_cell ).
      result = result + investigate_neighbourse( nb_cell ).
    ENDWHILE.
    result = result + 1.

  ENDMETHOD.

  METHOD if_neighbourhood~get_cell_by_location.
    LOOP AT if_neighbourhood~neighbourhood ASSIGNING FIELD-SYMBOL(<neighbour>).
      IF <neighbour>-cell->get_coordinates( ) = cell-cell->get_coordinates( ).
        result = <neighbour>.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES tt_range_input TYPE RANGE OF text1024.

    METHODS constructor    IMPORTING range_input TYPE tt_range_input.

    METHODS get_low_points RETURNING VALUE(result) TYPE if_neighbourhood=>neighbours.

    METHODS process_values.
    METHODS get_results
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA range_input TYPE tt_range_input.
    DATA cell_collection TYPE REF TO if_cell_collection.
    DATA neighbourhood TYPE REF TO if_neighbourhood.
    DATA result TYPE i.


    METHODS convert_range_input RETURNING VALUE(result) TYPE stringtab.
    METHODS build_cell_collection IMPORTING input TYPE stringtab.

    METHODS build_neighbourhood.
    METHODS build_result.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    me->range_input = range_input.
    cell_collection = NEW cell_collection( ).
    neighbourhood = NEW neighbourhood( ).
  ENDMETHOD.

  METHOD get_low_points.
    result = neighbourhood->get_lowest_cells( ).
  ENDMETHOD.

  METHOD process_values.
    DATA(input) = convert_range_input( ).

    build_cell_collection( input ).
    build_neighbourhood( ).
    build_result( ).
  ENDMETHOD.

  METHOD convert_range_input.
    result = VALUE #( FOR line IN range_input
                        ( CONV #( line-low ) ) ).
  ENDMETHOD.

  METHOD build_cell_collection.
    DATA index TYPE i.
    DATA line TYPE i.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      line = sy-tabix - 1.
      WHILE index < strlen( <line> ).
        DATA(value) = substring( val = <line> off = index len = 1 ).
        cell_collection->add( NEW cell( location = VALUE #( x = index y = line ) value = CONV i( value ) ) ).
        index = index + 1.
      ENDWHILE.
      index = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_neighbourhood.
    neighbourhood->build_neighbourhood( cell_collection ).
  ENDMETHOD.

  METHOD get_results.
    result = me->result.
    data(test) = neighbourhood->get_basin_count( ).
  ENDMETHOD.

  METHOD build_result.
    DATA(lowest_cells) = neighbourhood->get_lowest_cells( ).
    result = REDUCE #( INIT sum = 0
                       FOR line IN lowest_cells
                       LET value = line-cell->get_value( ) + 1
                       IN
                       NEXT sum = sum + value ).
  ENDMETHOD.

ENDCLASS.


CLASS tc_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO cell.

    METHODS setup.
    METHODS get_value_from_cell FOR TESTING.
ENDCLASS.

CLASS tc_cell IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( location = VALUE #( x = 0 y = 0 ) value = 3 ).
  ENDMETHOD.

  METHOD get_value_from_cell.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->if_cell~get_value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_cell_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO cell_collection.

    METHODS setup.
    METHODS fill_collection_with_3_cells FOR TESTING.
    METHODS get_cell_by_location         FOR TESTING.
    METHODS get_whole_collection         FOR TESTING.
    METHODS get_basin_count              FOR TESTING.
ENDCLASS.

CLASS tc_cell_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 0 ) value = 2 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 0 ) value = 1 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 0 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 0 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 0 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 0 ) value = 4 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 0 ) value = 3 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 0 ) value = 2 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 0 ) value = 1 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 0 ) value = 0 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 1 ) value = 3 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 1 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 1 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 1 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 1 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 1 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 1 ) value = 4 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 1 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 1 ) value = 2 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 1 ) value = 1 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 2 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 2 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 2 ) value = 5 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 2 ) value = 6 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 2 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 2 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 2 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 2 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 2 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 2 ) value = 2 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 3 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 3 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 3 ) value = 6 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 3 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 3 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 3 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 3 ) value = 6 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 3 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 3 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 3 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 4 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 4 ) value = 8 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 4 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 4 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 4 ) value = 9 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 4 ) value = 6 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 4 ) value = 5 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 4 ) value = 6 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 4 ) value = 7 ) ).
    cut->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 4 ) value = 8 ) ).

  ENDMETHOD.

  METHOD fill_collection_with_3_cells.
    cl_abap_unit_assert=>assert_equals(
        exp = 50
        act = cut->if_cell_collection~get_count( ) ).
  ENDMETHOD.

  METHOD get_cell_by_location.
    DATA(cell) = cut->if_cell_collection~get_item_by_location( VALUE #( x = 1 y = 1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 9
        act = cell->get_value( ) ).
  ENDMETHOD.

  METHOD get_whole_collection.
    DATA(cell) = cut->if_cell_collection~get_item( 1 )-cell.
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cell->get_value( ) ).
  ENDMETHOD.

  METHOD get_basin_count.

  ENDMETHOD.

ENDCLASS.

CLASS tc_neighbourhood DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO neighbourhood.

    METHODS setup.
    METHODS build_neighbourhood    FOR TESTING.
    METHODS get_lowest_cells_count FOR TESTING.
    METHODS get_basin_count        FOR TESTING.
ENDCLASS.


CLASS tc_neighbourhood IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    DATA(cell_collection) = NEW cell_collection( ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 0 ) value = 2 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 0 ) value = 1 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 0 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 0 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 0 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 0 ) value = 4 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 0 ) value = 3 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 0 ) value = 2 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 0 ) value = 1 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 0 ) value = 0 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 1 ) value = 3 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 1 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 1 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 1 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 1 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 1 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 1 ) value = 4 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 1 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 1 ) value = 2 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 1 ) value = 1 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 2 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 2 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 2 ) value = 5 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 2 ) value = 6 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 2 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 2 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 2 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 2 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 2 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 2 ) value = 2 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 3 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 3 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 3 ) value = 6 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 3 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 3 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 3 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 3 ) value = 6 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 3 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 3 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 3 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 0 y = 4 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 1 y = 4 ) value = 8 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 2 y = 4 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 3 y = 4 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 4 y = 4 ) value = 9 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 5 y = 4 ) value = 6 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 6 y = 4 ) value = 5 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 7 y = 4 ) value = 6 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 8 y = 4 ) value = 7 ) ).
    cell_collection->if_cell_collection~add( NEW cell( location = VALUE #( x = 9 y = 4 ) value = 8 ) ).
    cut->if_neighbourhood~build_neighbourhood( cell_collection ).
  ENDMETHOD.

  METHOD build_neighbourhood.
    cl_abap_unit_assert=>assert_equals(
        exp = 50
        act = cut->if_neighbourhood~get_count( ) ).
  ENDMETHOD.

  METHOD get_lowest_cells_count.
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = lines( cut->if_neighbourhood~get_lowest_cells( ) ) ).
  ENDMETHOD.

  METHOD get_basin_count.
    cl_abap_unit_assert=>assert_equals(
        exp = 14
        act = cut->if_neighbourhood~get_basin_count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS get_range_input RETURNING VALUE(result) TYPE application=>tt_range_input.

    METHODS test_run_1 FOR TESTING.
    METHODS get_result_from_run_1 FOR TESTING.
ENDCLASS.


CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( get_range_input( ) ).
  ENDMETHOD.

  METHOD get_range_input.
    result = VALUE #( ( sign = 'I' option = 'EQ' low ='2199943210' )
                      ( sign = 'I' option = 'EQ' low ='3987894921' )
                      ( sign = 'I' option = 'EQ' low ='9856789892' )
                      ( sign = 'I' option = 'EQ' low ='8767896789' )
                      ( sign = 'I' option = 'EQ' low ='9899965678' ) ).
  ENDMETHOD.

  METHOD test_run_1.
    cut->process_values( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = lines( cut->get_low_points( ) ) ).
  ENDMETHOD.

  METHOD get_result_from_run_1.
    cut->process_values( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 15
        act = cut->get_results( ) ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text1024.

SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.

  DATA(application) = NEW application( so_input[] ).
  application->process_values( ).
  DATA(low_cells) = application->get_low_points( ).
  WRITE / |Ergebnis Teil 1: { application->get_results( ) }|.
