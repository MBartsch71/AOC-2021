REPORT ymbh_aoc_2021_day13.

INTERFACE if_coordinates.
  TYPES: BEGIN OF coordinates,
           x TYPE i,
           y TYPE i,
         END OF coordinates.
ENDINTERFACE.

INTERFACE if_table_cell.
  TYPES value TYPE string.

  METHODS get_value RETURNING VALUE(result) TYPE string.

ENDINTERFACE.

INTERFACE if_table.
  TYPES: BEGIN OF cell,
           x    TYPE i,
           y    TYPE i,
           cell TYPE REF TO if_table_cell,
         END OF cell.
  TYPES cells TYPE SORTED TABLE OF cell WITH UNIQUE KEY primary_key     COMPONENTS x y
                                        WITH NON-UNIQUE SORTED KEY rows COMPONENTS y
                                        WITH NON-UNIQUE SORTED KEY cols COMPONENTS x.

  METHODS add_cell_value IMPORTING cell TYPE cell.

  METHODS get_cell_count RETURNING VALUE(result) TYPE i.

  METHODS get_row IMPORTING row           TYPE i
                  RETURNING VALUE(result) TYPE cells.

  METHODS get_col IMPORTING col           TYPE i
                  RETURNING VALUE(result) TYPE cells.

  METHODS rows RETURNING VALUE(result) TYPE i.

  METHODS cols RETURNING VALUE(result) TYPE i.

  METHODS get_cell_by_location IMPORTING coordinates   TYPE if_coordinates=>coordinates
                               RETURNING VALUE(result) TYPE REF TO if_table_cell.

  METHODS get_cell_by_index IMPORTING index         TYPE i
                            RETURNING VALUE(result) TYPE cell.

  METHODS build_table IMPORTING cols TYPE i
                                rows TYPE i.

ENDINTERFACE.

INTERFACE if_iterator.
  CLASS-METHODS get_instance IMPORTING collection    TYPE REF TO if_table
                             RETURNING VALUE(result) TYPE REF TO if_iterator.
  METHODS has_next RETURNING VALUE(result) TYPE abap_bool.
  METHODS get_next RETURNING VALUE(result) TYPE if_table=>cell.
ENDINTERFACE.

INTERFACE if_input.
  TYPES: BEGIN OF input_coordinates,
           x TYPE i,
           y TYPE i,
         END OF input_coordinates.
  TYPES inputs TYPE SORTED TABLE OF input_coordinates WITH NON-UNIQUE KEY primary_key COMPONENTS y x
                                                      WITH NON-UNIQUE SORTED KEY cols COMPONENTS x.
  TYPES: BEGIN OF folding_instruction,
           axis  TYPE string,
           index TYPE i,
         END OF folding_instruction.
  TYPES folding_instructions TYPE STANDARD TABLE OF folding_instruction WITH EMPTY KEY.

  TYPES range_input TYPE RANGE OF text1024.

  METHODS process_input IMPORTING range_input TYPE range_input.

  METHODS get_coordinates_table RETURNING VALUE(result) TYPE inputs.

  METHODS get_folding_instructions RETURNING VALUE(result) TYPE folding_instructions.

ENDINTERFACE.

INTERFACE if_folding.
  TYPES: BEGIN OF cell_area,
           start_col TYPE i,
           start_row TYPE i,
           end_col   TYPE i,
           end_row   TYPE i,
         END OF cell_area.

  METHODS build_table IMPORTING input_data TYPE if_input=>range_input.

  METHODS get_table RETURNING VALUE(result) TYPE stringtab.

  METHODS fold_table.

ENDINTERFACE.

CLASS table_cell DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_table_cell.
    METHODS constructor IMPORTING value TYPE if_table_cell=>value.

  PRIVATE SECTION.
    DATA value TYPE if_table_cell=>value.

ENDCLASS.

CLASS table_cell IMPLEMENTATION.

  METHOD constructor.
    me->value = value.
  ENDMETHOD.

  METHOD if_table_cell~get_value.
    result = value.
  ENDMETHOD.

ENDCLASS.

CLASS paper_table DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_table.

  PRIVATE SECTION.
    DATA cells TYPE if_table=>cells.

ENDCLASS.

CLASS paper_table IMPLEMENTATION.

  METHOD if_table~add_cell_value.
    cells[ x = cell-x y = cell-y ]-cell = cell-cell.
  ENDMETHOD.

  METHOD if_table~get_cell_count.
    result = lines( cells ).
  ENDMETHOD.

  METHOD if_table~get_row.
    result = FILTER #( cells USING KEY rows WHERE y = row ).
  ENDMETHOD.

  METHOD if_table~get_col.
    result = FILTER #( cells USING KEY cols WHERE x = col ).
  ENDMETHOD.

  METHOD if_table~build_table.
    cells = VALUE #( FOR x = 0 THEN x + 1 UNTIL x > cols
                     FOR y = 0 THEN y + 1 UNTIL y > rows
                     ( x = x
                       y = y
                       cell = NEW table_cell( `.` ) )  ).
  ENDMETHOD.

  METHOD if_table~get_cell_by_location.
    result = cells[ x = coordinates-x y = coordinates-y ]-cell.
  ENDMETHOD.

  METHOD if_table~cols.
    result = cells[ KEY cols INDEX lines( cells ) ]-x.
  ENDMETHOD.

  METHOD if_table~rows.
    result = cells[ KEY rows INDEX lines( cells ) ]-y.
  ENDMETHOD.

  METHOD if_table~get_cell_by_index.
    result = cells[ index ].
  ENDMETHOD.

ENDCLASS.

CLASS iterator DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_iterator.

    METHODS constructor IMPORTING collection TYPE REF TO if_table.

  PRIVATE SECTION.
    DATA collection TYPE REF TO if_table.
    DATA current_item TYPE i VALUE 1.

ENDCLASS.

CLASS iterator IMPLEMENTATION.

  METHOD constructor.
    me->collection = collection.
  ENDMETHOD.

  METHOD if_iterator~get_instance.
    result = NEW iterator( collection ).
  ENDMETHOD.

  METHOD if_iterator~get_next.
    result = collection->get_cell_by_index( current_item ).
    current_item = current_item + 1.
  ENDMETHOD.

  METHOD if_iterator~has_next.
    result = xsdbool( current_item <= collection->get_cell_count( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS input_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_input.

  PRIVATE SECTION.
    DATA range_input TYPE if_input=>range_input.
    DATA inputs TYPE if_input=>inputs.
    DATA folding_instructions TYPE if_input=>folding_instructions.

    METHODS build_input_coordinates.

    METHODS split_line IMPORTING low           TYPE text1024
                       RETURNING VALUE(result) TYPE if_input=>inputs.

    METHODS a_folding_instruction IMPORTING line_low      TYPE text1024
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS extract_instruction IMPORTING instruction   TYPE text1024
                                RETURNING VALUE(result) TYPE if_input=>folding_instruction.
    METHODS build_folding_instructions.
ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD if_input~process_input.
    me->range_input = range_input.
    build_input_coordinates( ).
    build_folding_instructions( ).
  ENDMETHOD.

  METHOD build_input_coordinates.
    inputs = VALUE #( FOR line IN range_input
                      LET coordinates = split_line( line-low )
                      IN
                      ( LINES OF coordinates ) ).
  ENDMETHOD.

  METHOD split_line.
    DATA value_1 TYPE text1024.
    DATA value_2 TYPE text1024.
    IF NOT a_folding_instruction( low ).
      SPLIT low AT ',' INTO value_1 value_2.
      result = VALUE #( ( x = value_1 y = value_2 ) ).
    ENDIF.
  ENDMETHOD.

  METHOD if_input~get_coordinates_table.
    result = inputs.
  ENDMETHOD.

  METHOD a_folding_instruction.
    result = xsdbool( line_low CS |fold along| ).
  ENDMETHOD.

  METHOD if_input~get_folding_instructions.
    result = folding_instructions.
  ENDMETHOD.

  METHOD extract_instruction.
    DATA value_1 TYPE text1024.
    DATA value_2 TYPE text1024.
    SPLIT instruction AT '=' INTO value_1 value_2.
    CONDENSE value_1 NO-GAPS.
    result = VALUE #( axis = CONV #( value_1 ) index = CONV #( value_2 ) ).
  ENDMETHOD.

  METHOD build_folding_instructions.
    FIND ALL OCCURRENCES OF |fold along| IN TABLE range_input RESULTS DATA(results).

    LOOP AT results REFERENCE INTO DATA(result_line).
      DATA(line) = range_input[ result_line->line ]-low.
      DATA(intro_text) = result_line->length.
      SHIFT line LEFT BY intro_text PLACES.
      folding_instructions = VALUE #( BASE folding_instructions
                                        ( extract_instruction( line ) ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS folding DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_folding.
    METHODS constructor.

  PRIVATE SECTION.
    DATA source_table TYPE REF TO if_table.
    DATA destination_table TYPE REF TO if_table.
    DATA display_table TYPE stringtab.
    DATA folding_instructions TYPE if_input=>folding_instructions.
    DATA remaining_area TYPE if_folding=>cell_area.
    DATA folding_area TYPE if_folding=>cell_area.

    METHODS populate_values IMPORTING coordinates TYPE if_input=>inputs.

    METHODS build_display_table.

    METHODS add_cell_values IMPORTING row           TYPE if_table=>cells
                            RETURNING VALUE(result) TYPE string.

    METHODS determine_cols          IMPORTING instruction   TYPE REF TO if_input=>folding_instruction
                                    RETURNING VALUE(result) TYPE i.

    METHODS determine_rows          IMPORTING instruction   TYPE REF TO if_input=>folding_instruction
                                    RETURNING VALUE(result) TYPE i.

    METHODS build_areas IMPORTING instruction TYPE REF TO if_input=>folding_instruction.
    METHODS build_remaining_area IMPORTING instruction TYPE REF TO if_input=>folding_instruction.
    METHODS build_folding_area IMPORTING instruction TYPE REF TO if_input=>folding_instruction.




ENDCLASS.

CLASS folding IMPLEMENTATION.

  METHOD constructor.
    source_table = NEW paper_table( ).
    destination_table = NEW paper_table( ).
  ENDMETHOD.

  METHOD if_folding~build_table.
    DATA(input_processor) = NEW input_processor( ).
    input_processor->if_input~process_input( input_data ).
    DATA(coordinates) = input_processor->if_input~get_coordinates_table( ).
    DATA(rows) = coordinates[ lines( coordinates ) ]-y.
    DATA(cols) = coordinates[ KEY cols INDEX lines( coordinates ) ]-x.
    folding_instructions = input_processor->if_input~get_folding_instructions( ).
    source_table->build_table( cols = cols rows = rows ).

    populate_values( coordinates ).
    build_display_table( ).
  ENDMETHOD.

  METHOD if_folding~get_table.
    result = display_table.
  ENDMETHOD.

  METHOD populate_values.
    LOOP AT coordinates REFERENCE INTO DATA(line).
      source_table->add_cell_value( cell = VALUE #( x = line->x y = line->y cell = NEW table_cell( '#' ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_display_table.
    display_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i > source_table->rows( )
                                 LET row = source_table->get_row( row = i )
                                     display_row = add_cell_values( row )
                                 IN
                                 ( display_row ) ).
  ENDMETHOD.

  METHOD add_cell_values.
    result = REDUCE #( INIT display_line = ``
                       FOR line IN row
                       NEXT display_line = display_line && line-cell->get_value( ) ).
  ENDMETHOD.

  METHOD if_folding~fold_table.
    LOOP AT folding_instructions REFERENCE INTO DATA(instruction).
      build_areas( instruction ).
      destination_table = NEW paper_table( ).
      destination_table->build_table( EXPORTING cols = remaining_area-end_col
                                                rows = remaining_area-end_row ).
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_cols.
    result = SWITCH #( instruction->axis WHEN `x` THEN instruction->index
                                         ELSE source_table->cols( ) - 1 ).
  ENDMETHOD.

  METHOD determine_rows.
    result = SWITCH #( instruction->axis WHEN `y` THEN instruction->index
                                         ELSE source_table->rows( ) - 1 ).
  ENDMETHOD.

  METHOD build_areas.
    build_remaining_area( instruction ).
    build_folding_area( instruction ).
  ENDMETHOD.

  METHOD build_remaining_area.
    remaining_area = VALUE #( start_col = 0
                              start_row = 0
                              end_col   = COND #( WHEN instruction->axis = `y` THEN source_table->cols( )
                                                  ELSE instruction->index - 1 )
                              end_row   = COND #( WHEN instruction->axis = `x` THEN source_table->rows( )
                                                  ELSE instruction->index - 1 ) ).
  ENDMETHOD.

  METHOD build_folding_area.
    folding_area = VALUE #( start_col = COND #( WHEN instruction->axis = `y` THEN 0
                                                ELSE instruction->index + 1 )
                            start_row = COND #( WHEN instruction->axis = `x` THEN 0
                                                ELSE instruction->index + 1  )
                            end_col   = source_table->cols( )
                            end_row   = source_table->rows( ) ).
  ENDMETHOD.



ENDCLASS.

CLASS tc_table_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_table_cell.

    METHODS setup.
    METHODS build_table_cell FOR TESTING.
    METHODS get_value_from_cell FOR TESTING.

ENDCLASS.

CLASS tc_table_cell IMPLEMENTATION.

  METHOD setup.
    cut = NEW table_cell( `#` ).
  ENDMETHOD.

  METHOD build_table_cell.
    cl_abap_unit_assert=>assert_bound(
        msg = |The object should be bound!|
        act = cut ).
  ENDMETHOD.

  METHOD get_value_from_cell.
    cl_abap_unit_assert=>assert_equals(
        exp = `#`
        act = cut->get_value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_table DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_table.

    METHODS setup.

    METHODS build_table_of_given_site FOR TESTING.
    METHODS count_cols_for_given_row  FOR TESTING.
    METHODS count_rows_for_given_col  FOR TESTING.
    METHODS populate_cell_values      FOR TESTING.
ENDCLASS.

CLASS tc_table IMPLEMENTATION.

  METHOD setup.
    cut = NEW paper_table( ).
    cut->build_table( cols = 10 rows = 10 ).
  ENDMETHOD.

  METHOD build_table_of_given_site.
    cut->build_table( cols = 10 rows = 10 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 121
        act = cut->get_cell_count( ) ).
  ENDMETHOD.

  METHOD count_cols_for_given_row.
    cl_abap_unit_assert=>assert_equals(
        exp = 10
        act = cut->rows( ) ).
  ENDMETHOD.

  METHOD count_rows_for_given_col.
    cl_abap_unit_assert=>assert_equals(
       exp = 10
       act = cut->cols( ) ).
  ENDMETHOD.

  METHOD populate_cell_values.
    cut->add_cell_value( VALUE #( x = 3 y = 0 cell = NEW table_cell( 'X' ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = |X|
        act = cut->get_cell_by_location( VALUE #( x = 3 y = 0 ) )->get_value( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_input_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_input.

    METHODS setup.
    METHODS build_coordinates_from_input FOR TESTING.
    METHODS search_folding_instructions FOR TESTING.

ENDCLASS.

CLASS tc_input_processor IMPLEMENTATION.

  METHOD setup.
    cut = NEW input_processor( ).
    cut->process_input( VALUE #( ( sign = 'I' option = 'EQ' low = |6,10| )
                                 ( sign = 'I' option = 'EQ' low = |0,14| )
                                 ( sign = 'I' option = 'EQ' low = |9,10| )
                                 ( sign = 'I' option = 'EQ' low = |0,3| )
                                 ( sign = 'I' option = 'EQ' low = |10,4| )
                                 ( sign = 'I' option = 'EQ' low = |4,11| )
                                 ( sign = 'I' option = 'EQ' low = |6,0| )
                                 ( sign = 'I' option = 'EQ' low = |6,12| )
                                 ( sign = 'I' option = 'EQ' low = |4,1| )
                                 ( sign = 'I' option = 'EQ' low = |0,13| )
                                 ( sign = 'I' option = 'EQ' low = |10,12| )
                                 ( sign = 'I' option = 'EQ' low = |3,4| )
                                 ( sign = 'I' option = 'EQ' low = |3,0| )
                                 ( sign = 'I' option = 'EQ' low = |8,4| )
                                 ( sign = 'I' option = 'EQ' low = |1,10| )
                                 ( sign = 'I' option = 'EQ' low = |2,14| )
                                 ( sign = 'I' option = 'EQ' low = |8,10| )
                                 ( sign = 'I' option = 'EQ' low = |9,0| )
                                 ( sign = 'I' option = 'EQ' low = |fold along y=7| )
                                 ( sign = 'I' option = 'EQ' low = |fold along x=5| ) ) ).

  ENDMETHOD.

  METHOD build_coordinates_from_input.
    DATA(expected_values) = VALUE if_input=>inputs( ( x = 3  y = 0 )
                                                    ( x = 6  y = 0 )
                                                    ( x = 9  y = 0 )
                                                    ( x = 4  y = 1 )
                                                    ( x = 0  y = 3 )
                                                    ( x = 1  y = 10 )
                                                    ( x = 3  y = 4 )
                                                    ( x = 8  y = 4 )
                                                    ( x = 10 y = 4 )
                                                    ( x = 6  y = 10 )
                                                    ( x = 8  y = 10 )
                                                    ( x = 9  y = 10 )
                                                    ( x = 4  y = 11 )
                                                    ( x = 6  y = 12 )
                                                    ( x = 10 y = 12 )
                                                    ( x = 0  y = 13 )
                                                    ( x = 0  y = 14 )
                                                    ( x = 2  y = 14 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_coordinates_table( ) ).
  ENDMETHOD.


  METHOD search_folding_instructions.
    DATA(expected_values) = VALUE if_input=>folding_instructions( ( axis = `y` index = 7 )
                                                                  ( axis = `x` index = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_folding_instructions( ) ).
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

  METHOD setup.
    DATA(collection) = NEW paper_table( ).
    collection->if_table~build_table( cols = 2 rows = 0 ).
    collection->if_table~add_cell_value( VALUE #( x = 0 y = 0 cell = NEW table_cell( '.' ) ) ).
    collection->if_table~add_cell_value( VALUE #( x = 1 y = 0 cell = NEW table_cell( '#' ) ) ).
    collection->if_table~add_cell_value( VALUE #( x = 2 y = 0 cell = NEW table_cell( '.' ) ) ).
    cut = iterator=>if_iterator~get_instance( collection ).
  ENDMETHOD.


  METHOD get_3_items_from_collection.
    DATA cells TYPE if_table=>cells.
    WHILE cut->has_next( ).
      cells = VALUE #( BASE cells ( cut->get_next( ) ) ).
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = lines( cells ) ).
  ENDMETHOD.


ENDCLASS.
CLASS tc_folding DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_folding.
    DATA raw_input TYPE if_input=>range_input.

    METHODS setup.
    METHODS build_input_data RETURNING VALUE(result) TYPE if_input=>range_input.

    METHODS build_initial_table FOR TESTING.
    METHODS get_table_after_folding_x7 FOR TESTING.

ENDCLASS.

CLASS tc_folding IMPLEMENTATION.

  METHOD setup.
    cut = NEW folding( ).
    raw_input = build_input_data( ).
  ENDMETHOD.

  METHOD build_input_data.
    result = VALUE #( ( sign = 'I' option = 'EQ' low = `6,10`  )
                      ( sign = 'I' option = 'EQ' low = `0,14`  )
                      ( sign = 'I' option = 'EQ' low = `9,10`  )
                      ( sign = 'I' option = 'EQ' low = `0,3`  )
                      ( sign = 'I' option = 'EQ' low = `10,4`  )
                      ( sign = 'I' option = 'EQ' low = `4,11`  )
                      ( sign = 'I' option = 'EQ' low = `6,0`  )
                      ( sign = 'I' option = 'EQ' low = `6,12`  )
                      ( sign = 'I' option = 'EQ' low = `4,1`  )
                      ( sign = 'I' option = 'EQ' low = `0,13`  )
                      ( sign = 'I' option = 'EQ' low = `10,12` )
                      ( sign = 'I' option = 'EQ' low = `3,4` )
                      ( sign = 'I' option = 'EQ' low = `3,0` )
                      ( sign = 'I' option = 'EQ' low = `8,4` )
                      ( sign = 'I' option = 'EQ' low = `1,10` )
                      ( sign = 'I' option = 'EQ' low = `2,14` )
                      ( sign = 'I' option = 'EQ' low = `8,10` )
                      ( sign = 'I' option = 'EQ' low = `9,0` )
                      ( sign = 'I' option = 'EQ' low = `fold along y=7` )
                      ( sign = 'I' option = 'EQ' low = `fold along x=5` ) ).
  ENDMETHOD.

  METHOD build_initial_table.
    DATA(expected_values) = VALUE stringtab( ( |...#..#..#.| )
                                             ( |....#......| )
                                             ( |...........| )
                                             ( |#..........| )
                                             ( |...#....#.#| )
                                             ( |...........| )
                                             ( |...........| )
                                             ( |...........| )
                                             ( |...........| )
                                             ( |...........| )
                                             ( |.#....#.##.| )
                                             ( |....#......| )
                                             ( |......#...#| )
                                             ( |#..........| )
                                             ( |#.#........| ) ).
    cut->build_table( raw_input ).
    DATA(table) = cut->get_table( ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = table ).
  ENDMETHOD.

  METHOD get_table_after_folding_x7.
    DATA(expected_values) = VALUE stringtab( ( |#.##..#..#.| )
                                             ( |#...#......| )
                                             ( |......#...#| )
                                             ( |#...#......| )
                                             ( |.#.#..#.###| )
                                             ( |...........| )
                                             ( |...........| ) ).
    cut->build_table( raw_input ).
    cut->fold_table( ).
    DATA(table) = cut->get_table( ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = table ).
  ENDMETHOD.

ENDCLASS.
