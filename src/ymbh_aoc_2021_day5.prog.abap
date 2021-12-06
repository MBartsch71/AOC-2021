REPORT ymbh_aoc_2021_day5.

INTERFACE if_vectors.
  TYPES: BEGIN OF vector,
           x         TYPE i,
           y         TYPE i,
           length    TYPE i,
           direction TYPE string,
         END OF vector.
  TYPES vectors TYPE SORTED TABLE OF vector WITH NON-UNIQUE KEY primary_key COMPONENTS table_line
                                            WITH NON-UNIQUE SORTED KEY direction COMPONENTS direction.
ENDINTERFACE.

INTERFACE if_input.
  TYPES: BEGIN OF coordinates,
           x_start TYPE i,
           y_start TYPE i,
           x_end   TYPE i,
           y_end   TYPE i,
         END OF coordinates.
  TYPES coordinates_tab TYPE SORTED TABLE OF coordinates WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.
  TYPES: BEGIN OF input_pair,
           pair_1 TYPE text10,
           pair_2 TYPE text10,
         END OF input_pair.
  TYPES: BEGIN OF coordinates_pair_char,
           x_start_char TYPE text10,
           y_start_char TYPE text10,
           x_end_char   TYPE text10,
           y_end_char   TYPE text10,
         END OF coordinates_pair_char.
ENDINTERFACE.

INTERFACE if_grid.
  TYPES: BEGIN OF grid_line,
           x     TYPE i,
           y     TYPE i,
           count TYPE i,
         END OF grid_line.
  TYPES grid_lines TYPE SORTED TABLE OF grid_line WITH UNIQUE KEY primary_key COMPONENTS x y.
ENDINTERFACE.


CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS convert_raw_input IMPORTING raw_input     TYPE stringtab
                              RETURNING VALUE(result) TYPE if_input=>coordinates_tab.

  PRIVATE SECTION.
    METHODS split_line        IMPORTING line          TYPE string
                              RETURNING VALUE(result) TYPE if_input=>input_pair.

    METHODS split_pairs       IMPORTING pairs         TYPE if_input=>input_pair
                              RETURNING VALUE(result) TYPE if_input=>coordinates_pair_char.

ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD convert_raw_input.
    result = VALUE #( BASE result FOR line IN raw_input
                      LET split_pairs = split_line( line )
                          coordinates_pairs_char = split_pairs( split_pairs )
                      IN
                      ( x_start = CONV #( coordinates_pairs_char-x_start_char )
                        y_start = CONV #( coordinates_pairs_char-y_start_char )
                        x_end   = CONV #( coordinates_pairs_char-x_end_char )
                        y_end   = CONV #( coordinates_pairs_char-y_end_char ) ) ).
  ENDMETHOD.

  METHOD split_line.
    SPLIT line AT '->' INTO result-pair_1 result-pair_2.
  ENDMETHOD.

  METHOD split_pairs.
    SPLIT pairs-pair_1 AT ',' INTO result-x_start_char result-y_start_char.
    SPLIT pairs-pair_2 AT ',' INTO result-x_end_char result-y_end_char.
  ENDMETHOD.

ENDCLASS.


CLASS vectors DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor     IMPORTING coordinates TYPE if_input=>coordinates_tab.

    METHODS get_vectors RETURNING VALUE(result) TYPE if_vectors=>vectors.

  PRIVATE SECTION.
    DATA input TYPE if_input=>coordinates_tab.

    METHODS get_x_forwards               RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_x_backwards              RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_y_ups                    RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_y_downs                  RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_diagonal_plus_x_plus_y   RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_diagonal_plus_x_minus_y  RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_diagonal_minus_x_plus_y  RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS get_diagonal_minus_y_minus_y RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS filter_y_equalities          IMPORTING input         TYPE if_input=>coordinates_tab
                                         RETURNING VALUE(result) TYPE if_input=>coordinates_tab.

    METHODS filter_x_equalities          IMPORTING input         TYPE if_input=>coordinates_tab
                                         RETURNING VALUE(result) TYPE if_input=>coordinates_tab.

    METHODS filter_diagonals             IMPORTING input         TYPE if_input=>coordinates_tab
                                         RETURNING VALUE(result) TYPE if_input=>coordinates_tab.

    METHODS forward                      IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS backward                     IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS up                           IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS down                         IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS dia_x_plus_y_plus            IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS dia_x_plus_y_minus           IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS dia_x_minus_y_plus           IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS dia_x_minus_y_minus          IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS populate_forward_line        IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_backward_line       IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_up_line             IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_down_line           IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_dia_x_plus_y_plus   IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_dia_x_plus_y_minus  IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_dia_x_minus_y_plus  IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS populate_dia_x_minus_y_minus IMPORTING line          TYPE if_input=>coordinates
                                         RETURNING VALUE(result) TYPE if_vectors=>vectors.
ENDCLASS.

CLASS vectors IMPLEMENTATION.

  METHOD constructor.
    input = coordinates.
  ENDMETHOD.

  METHOD get_vectors.
    result = VALUE #( BASE result ( LINES OF get_x_forwards( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_x_backwards( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_y_ups( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_y_downs( ) ) ).

    result = VALUE #( BASE result ( LINES OF get_diagonal_plus_x_plus_y( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_diagonal_plus_x_minus_y( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_diagonal_minus_x_plus_y( ) ) ).
    result = VALUE #( BASE result ( LINES OF get_diagonal_minus_y_minus_y( ) ) ).
  ENDMETHOD.

  METHOD get_x_forwards.
    result = VALUE #( FOR line IN filter_y_equalities( input )
                      LET forward = forward( line )
                      IN
                      ( LINES OF
                            COND #( WHEN forward = abap_true
                                      THEN populate_forward_line( line ) ) ) ).
  ENDMETHOD.

  METHOD get_x_backwards.
    result = VALUE #( FOR line IN filter_y_equalities( input )
                      LET backward = backward( line )
                      IN
                      ( LINES OF
                            COND #( WHEN backward = abap_true
                                      THEN populate_backward_line( line ) ) ) ).

  ENDMETHOD.

  METHOD get_y_ups.
    result = VALUE #( FOR line IN filter_x_equalities( input )
                      LET up = up( line )
                      IN
                      ( LINES OF
                            COND #( WHEN up = abap_true
                                      THEN populate_up_line( line ) ) ) ).
  ENDMETHOD.

  METHOD get_y_downs.
    result = VALUE #( FOR line IN filter_x_equalities( input )
                     LET down = down( line )
                     IN
                     ( LINES OF
                           COND #( WHEN down = abap_true
                                     THEN populate_down_line( line ) ) ) ).
  ENDMETHOD.

  METHOD get_diagonal_plus_x_plus_y.
    result = VALUE #( FOR line IN filter_diagonals( input )
                      LET diagonal = dia_x_plus_y_plus( line )
                      IN
                      ( LINES OF
                        COND #( WHEN diagonal = abap_true
                                    THEN populate_dia_x_plus_y_plus( line ) ) ) ).
  ENDMETHOD.

  METHOD get_diagonal_plus_x_minus_y.
    result = VALUE #( FOR line IN filter_diagonals( input )
                     LET diagonal = dia_x_plus_y_minus( line )
                     IN
                     ( LINES OF
                       COND #( WHEN diagonal = abap_true
                                 THEN populate_dia_x_plus_y_minus( line ) ) ) ).
  ENDMETHOD.

  METHOD get_diagonal_minus_x_plus_y.
    result = VALUE #( FOR line IN filter_diagonals( input )
                        LET diagonal = dia_x_minus_y_plus( line )
                        IN
                        ( LINES OF
                          COND #( WHEN diagonal = abap_true
                                    THEN populate_dia_x_minus_y_plus( line ) ) ) ).
  ENDMETHOD.

  METHOD get_diagonal_minus_y_minus_y.
    result = VALUE #( FOR line IN filter_diagonals( input )
                        LET diagonal = dia_x_minus_y_minus( line )
                        IN
                        ( LINES OF
                          COND #( WHEN diagonal = abap_true
                                    THEN populate_dia_x_minus_y_minus( line ) ) ) ).
  ENDMETHOD.


  METHOD filter_y_equalities.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-y_start = <line>-y_end.
        APPEND <line> TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD filter_x_equalities.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-x_start = <line>-x_end.
        APPEND <line> TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD filter_diagonals.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-x_start <> <line>-x_end AND
         <line>-y_start <> <line>-y_end.
        APPEND <line> TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD forward.
    result = xsdbool( line-x_end > line-x_start ).
  ENDMETHOD.

  METHOD backward.
    result = xsdbool( line-x_start > line-x_end ).
  ENDMETHOD.

  METHOD up.
    result = xsdbool( line-y_end < line-y_start ).
  ENDMETHOD.

  METHOD down.
    result = xsdbool( line-y_start < line-y_end ).
  ENDMETHOD.

  METHOD dia_x_plus_y_plus.
    result = xsdbool( line-x_start < line-x_end AND
                      line-y_start < line-y_end ).

  ENDMETHOD.

  METHOD dia_x_plus_y_minus.
    result = xsdbool( line-x_start < line-x_end AND
                      line-y_start > line-y_end ).
  ENDMETHOD.

  METHOD dia_x_minus_y_plus.
    result = xsdbool( ( line-x_start > line-y_start AND
                        line-y_start < line-y_end ) ).
  ENDMETHOD.

  METHOD dia_x_minus_y_minus.
    result = xsdbool( ( line-x_start > line-y_start AND
                        line-y_start > line-y_end ) ).
  ENDMETHOD.


  METHOD populate_forward_line.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-x_end - line-x_start
                        direction = |x| ) ).
  ENDMETHOD.

  METHOD populate_backward_line.
    result = VALUE #( ( x         = line-x_end
                        y         = line-y_start
                        length    = line-x_start - line-x_end
                        direction = |x| ) ).
  ENDMETHOD.

  METHOD populate_up_line.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_end
                        length    = line-y_start - line-y_end
                        direction = |y| ) ).
  ENDMETHOD.

  METHOD populate_down_line.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-y_end - line-y_start
                        direction = |y| ) ).
  ENDMETHOD.

  METHOD populate_dia_x_plus_y_plus.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-x_end - line-x_start
                        direction = |+x+y| ) ).
  ENDMETHOD.

  METHOD populate_dia_x_plus_y_minus.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-x_start - line-x_end
                        direction = |+x-y|  ) ).
  ENDMETHOD.

  METHOD populate_dia_x_minus_y_plus.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-x_start - line-x_end
                        direction = |-x+y|  ) ).
  ENDMETHOD.

  METHOD populate_dia_x_minus_y_minus.
    result = VALUE #( ( x         = line-x_start
                        y         = line-y_start
                        length    = line-x_start - line-x_end
                        direction = |-x-y|  ) ).
  ENDMETHOD.


ENDCLASS.

CLASS venture_grid DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS populate                IMPORTING input_vectors TYPE if_vectors=>vectors
                                    RETURNING VALUE(result) TYPE if_grid=>grid_lines.

    METHODS get_overlapping_vectors RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA grid_lines TYPE if_grid=>grid_lines.

    METHODS populate_x_vectors           IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS populate_y_vectors           IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS point_exists                 IMPORTING x             TYPE i
                                                   y             TYPE i
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS populate_dia_vectors         IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS populate_dia_x_plus_y_plus   IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS populate_dia_x_plus_y_minus  IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS populate_dia_x_minus_y_plus  IMPORTING input_vectors TYPE if_vectors=>vectors.

    METHODS populate_dia_x_minus_y_minus IMPORTING input_vectors TYPE if_vectors=>vectors.

ENDCLASS.

CLASS venture_grid IMPLEMENTATION.

  METHOD populate.
    populate_x_vectors( input_vectors ).
    populate_y_vectors( input_vectors ).
    populate_dia_vectors( input_vectors ).
    result = grid_lines.
  ENDMETHOD.

  METHOD populate_x_vectors.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |x|.
      WHILE point <= <vector>-length.
        DATA(target_x) = <vector>-x + point.
        IF point_exists( x = target_x y = <vector>-y ).
          grid_lines[ x = target_x y = <vector>-y ]-count = grid_lines[ x = target_x y = <vector>-y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = target_x
                                    y     = <vector>-y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_y_vectors.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |y|.
      WHILE point <= <vector>-length.
        DATA(target_y) = <vector>-y + point.
        IF point_exists( x = <vector>-x y = target_y ).
          grid_lines[ x = <vector>-x y = target_y ]-count = grid_lines[ x = <vector>-x y = target_y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = <vector>-x
                                    y     = target_y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_dia_vectors.
    populate_dia_x_plus_y_plus( input_vectors ).
    populate_dia_x_plus_y_minus( input_vectors ).
    populate_dia_x_minus_y_plus( input_vectors ).
    populate_dia_x_minus_y_minus( input_vectors ).
  ENDMETHOD.

  METHOD populate_dia_x_plus_y_plus.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |+x+y|.
      WHILE point <= <vector>-length.
        DATA(target_x) = <vector>-x + point.
        DATA(target_y) = <vector>-y + point.
        IF point_exists( x = target_x y = target_y ).
          grid_lines[ x = target_x y = target_y ]-count = grid_lines[ x = target_x y = target_y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = target_x
                                    y     = target_y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_dia_x_plus_y_minus.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |+x-y|.
      WHILE point <= <vector>-length.
        DATA(target_x) = <vector>-x + point.
        DATA(target_y) = <vector>-y - point.
        IF point_exists( x = target_x y = target_y ).
          grid_lines[ x = target_x y = target_y ]-count = grid_lines[ x = target_x y = target_y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = target_x
                                    y     = target_y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_dia_x_minus_y_plus.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |-x+y|.
      WHILE point <= <vector>-length.
        DATA(target_x) = <vector>-x - point.
        DATA(target_y) = <vector>-y + point.
        IF point_exists( x = target_x y = target_y ).
          grid_lines[ x = target_x y = target_y ]-count = grid_lines[ x = target_x y = target_y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = target_x
                                    y     = target_y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_dia_x_minus_y_minus.
    DATA point TYPE i.
    LOOP AT input_vectors ASSIGNING FIELD-SYMBOL(<vector>) WHERE direction = |-x-y|.
      WHILE point <= <vector>-length.
        DATA(target_x) = <vector>-x - point.
        DATA(target_y) = <vector>-y - point.
        IF point_exists( x = target_x y = target_y ).
          grid_lines[ x = target_x y = target_y ]-count = grid_lines[ x = target_x y = target_y ]-count + 1.
        ELSE.
          grid_lines = VALUE #( BASE grid_lines
                                  ( x     = target_x
                                    y     = target_y
                                    count = 1 ) ).
        ENDIF.
        point = point + 1.
      ENDWHILE.
      point = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD point_exists.
    result = xsdbool( line_exists( grid_lines[ x = x y = y ] ) ).
  ENDMETHOD.

  METHOD get_overlapping_vectors.
    result = REDUCE #( INIT sum = 0
                       FOR lines IN grid_lines
                       NEXT sum = COND #( WHEN lines-count > 1 THEN sum + 1
                                          ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    TYPES tt_input TYPE RANGE OF text100.
    METHODS run IMPORTING input         TYPE tt_input
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    METHODS convert_range_input IMPORTING input         TYPE application=>tt_input
                                RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run.
    DATA(raw_input) = convert_range_input( input ).
    DATA(input_processor) = NEW input_processor( ).
    DATA(input_coordinates) = input_processor->convert_raw_input( raw_input ).
    DATA(vectors) = NEW vectors( input_coordinates ).
    DATA(vector_table) = vectors->get_vectors( ).
    DATA(venture_grid) = NEW venture_grid( ).
    venture_grid->populate( vector_table ).
    result = venture_grid->get_overlapping_vectors( ).
  ENDMETHOD.


  METHOD convert_range_input.
    result = VALUE #( FOR line IN input
                       ( CONV #( line-low ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_input DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO input_processor.

    METHODS setup.
    METHODS get_raw_input RETURNING VALUE(result) TYPE stringtab.
    METHODS split_input_in_coordinates FOR TESTING.
ENDCLASS.

CLASS tc_input IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_raw_input.
    result = VALUE #( ( |0,9 -> 5,9| )
                      ( |8,0 -> 0,8| )
                      ( |9,4 -> 3,4| )
                      ( |2,2 -> 2,1| )
                      ( |7,0 -> 7,4| )
                      ( |6,4 -> 2,0| )
                      ( |0,9 -> 2,9| )
                      ( |3,4 -> 1,4| )
                      ( |0,0 -> 8,8| )
                      ( |5,5 -> 8,2| ) ).
  ENDMETHOD.

  METHOD split_input_in_coordinates.
    DATA(expected_values) = VALUE if_input=>coordinates_tab( ( x_start = 0 y_start = 9 x_end = 5 y_end = 9 )
                                                             ( x_start = 8 y_start = 0 x_end = 0 y_end = 8 )
                                                             ( x_start = 9 y_start = 4 x_end = 3 y_end = 4 )
                                                             ( x_start = 2 y_start = 2 x_end = 2 y_end = 1 )
                                                             ( x_start = 7 y_start = 0 x_end = 7 y_end = 4 )
                                                             ( x_start = 6 y_start = 4 x_end = 2 y_end = 0 )
                                                             ( x_start = 0 y_start = 9 x_end = 2 y_end = 9 )
                                                             ( x_start = 3 y_start = 4 x_end = 1 y_end = 4 )
                                                             ( x_start = 0 y_start = 0 x_end = 8 y_end = 8 )
                                                             ( x_start = 5 y_start = 5 x_end = 8 y_end = 2 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->convert_raw_input( get_raw_input( ) ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_vectors DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO vectors.

    METHODS setup.
    METHODS get_grid_input RETURNING VALUE(result) TYPE if_input=>coordinates_tab.

    METHODS get_grid_population_vectors FOR TESTING.
ENDCLASS.


CLASS tc_vectors IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( get_grid_input( ) ).
  ENDMETHOD.

  METHOD get_grid_input.
    result = VALUE #( ( x_start = 0 y_start = 9 x_end = 5 y_end = 9 )
                      ( x_start = 8 y_start = 0 x_end = 0 y_end = 8 )
                      ( x_start = 9 y_start = 4 x_end = 3 y_end = 4 )
                      ( x_start = 2 y_start = 2 x_end = 2 y_end = 1 )
                      ( x_start = 7 y_start = 0 x_end = 7 y_end = 4 )
                      ( x_start = 6 y_start = 4 x_end = 2 y_end = 0 )
                      ( x_start = 0 y_start = 9 x_end = 2 y_end = 9 )
                      ( x_start = 3 y_start = 4 x_end = 1 y_end = 4 )
                      ( x_start = 0 y_start = 0 x_end = 8 y_end = 8 )
                      ( x_start = 5 y_start = 5 x_end = 8 y_end = 2 ) ).
  ENDMETHOD.

  METHOD get_grid_population_vectors.
    DATA(expected_values) = VALUE if_vectors=>vectors(
                                                       ( x = 0 y = 0 length = 8 direction = |+x+y| )
                                                       ( x = 0 y = 9 length = 2 direction = |x| )
                                                       ( x = 0 y = 9 length = 5 direction = |x| )
                                                       ( x = 1 y = 4 length = 2 direction = |x| )
                                                       ( x = 2 y = 1 length = 1 direction = |y| )
                                                       ( x = 3 y = 4 length = 6 direction = |x| )
                                                       ( x = 5 y = 5 length = -3 direction = |+x-y| )
                                                       ( x = 6 y = 4 length = 4 direction = |-x-y| )
                                                       ( x = 7 y = 0 length = 4 direction = |y| )
                                                       ( x = 8 y = 0 length = 8 direction = |-x+y| )
                                                        ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_vectors( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_grid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO venture_grid.
    METHODS setup.
    METHODS build_input_vectors RETURNING VALUE(result) TYPE if_vectors=>vectors.

    METHODS check_test_grid         FOR TESTING.
    METHODS get_overlapping_vectors FOR TESTING.

ENDCLASS.


CLASS tc_grid IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD build_input_vectors.
    result = VALUE #( ( x = 0 y = 0 length = 8 direction = |+x+y| )
                                                       ( x = 0 y = 9 length = 2 direction = |x| )
                                                       ( x = 0 y = 9 length = 5 direction = |x| )
                                                       ( x = 1 y = 4 length = 2 direction = |x| )
                                                       ( x = 2 y = 1 length = 1 direction = |y| )
                                                       ( x = 3 y = 4 length = 6 direction = |x| )
                                                       ( x = 5 y = 5 length = -3 direction = |+x-y| )
                                                       ( x = 6 y = 4 length = 4 direction = |-x-y| )
                                                       ( x = 7 y = 0 length = 4 direction = |y| )
                                                       ( x = 8 y = 0 length = 8 direction = |-x+y| ) ).
  ENDMETHOD.

  METHOD check_test_grid.
    DATA(expected_values) = VALUE if_grid=>grid_lines( ( x = 0 y = 0 count = 1 )
                                                       ( x = 0 y = 8 count = 1 )
                                                       ( x = 0 y = 9 count = 2 )
                                                       ( x = 1 y = 1 count = 1 )
                                                       ( x = 1 y = 4 count = 1 )
                                                       ( x = 1 y = 7 count = 1 )
                                                       ( x = 1 y = 9 count = 2 )
                                                       ( x = 2 y = 0 count = 1 )
                                                       ( x = 2 y = 1 count = 1 )
                                                       ( x = 2 y = 2 count = 2 )
                                                       ( x = 2 y = 4 count = 1 )
                                                       ( x = 2 y = 6 count = 1 )
                                                       ( x = 2 y = 9 count = 2 )
                                                       ( x = 3 y = 1 count = 1 )
                                                       ( x = 3 y = 3 count = 1 )
                                                       ( x = 3 y = 4 count = 2 )
                                                       ( x = 3 y = 5 count = 1 )
                                                       ( x = 3 y = 9 count = 1 )
                                                       ( x = 4 y = 2 count = 1 )
                                                       ( x = 4 y = 4 count = 3 )
                                                       ( x = 4 y = 9 count = 1 )
                                                       ( x = 5 y = 3 count = 2 )
                                                       ( x = 5 y = 4 count = 1 )
                                                       ( x = 5 y = 5 count = 2 )
                                                       ( x = 5 y = 9 count = 1 )
                                                       ( x = 6 y = 2 count = 1 )
                                                       ( x = 6 y = 4 count = 3 )
                                                       ( x = 6 y = 6 count = 1 )
                                                       ( x = 7 y = 0 count = 1 )
                                                       ( x = 7 y = 1 count = 2 )
                                                       ( x = 7 y = 2 count = 1 )
                                                       ( x = 7 y = 3 count = 2 )
                                                       ( x = 7 y = 4 count = 2 )
                                                       ( x = 7 y = 7 count = 1 )
                                                       ( x = 8 y = 0 count = 1 )
                                                       ( x = 8 y = 2 count = 1 )
                                                       ( x = 8 y = 4 count = 1 )
                                                       ( x = 8 y = 8 count = 1 )
                                                       ( x = 9 y = 4 count = 1 ) ).


    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->populate( build_input_vectors( ) ) ).
  ENDMETHOD.

  METHOD get_overlapping_vectors.
    cut->populate( build_input_vectors( ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 12
        act = cut->get_overlapping_vectors( ) ).
  ENDMETHOD.

ENDCLASS.

DATA line TYPE text100.
SELECT-OPTIONS: so_input FOR line NO INTERVALS.

START-OF-SELECTION.
  DATA(application) = NEW application( ).

  WRITE / |Ergebnis Teil 1: { application->run( so_input[] ) }|.
