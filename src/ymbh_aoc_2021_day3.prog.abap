REPORT ymbh_aoc_2021_day3.

CLASS binary_translator DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF binary_structure,
             position TYPE i,
             count_0  TYPE i,
             count_1  TYPE i,
           END OF binary_structure.
    TYPES binary_structures TYPE SORTED TABLE OF binary_structure WITH NON-UNIQUE KEY primary_key COMPONENTS position.

    METHODS count_bits_per_position IMPORTING input         TYPE stringtab
                                    RETURNING VALUE(result) TYPE binary_structures.

    METHODS calculate_gamma_rate    IMPORTING input         TYPE binary_translator=>binary_structures
                                    RETURNING VALUE(result) TYPE string.

    METHODS calculate_epsilon_rate  IMPORTING input         TYPE binary_translator=>binary_structures
                                    RETURNING VALUE(result) TYPE string.

    METHODS multiplicate_binaries   IMPORTING first         TYPE string
                                              second        TYPE string
                                    RETURNING VALUE(result) TYPE i.
    METHODS filter_lines_most_pos
      IMPORTING
        position      TYPE i
        table         TYPE stringtab
      RETURNING
        VALUE(result) TYPE stringtab.
    METHODS filter_lines_least_pos
      IMPORTING
        position      TYPE i
        table         TYPE stringtab
      RETURNING
        VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    METHODS count_zeros IMPORTING position      TYPE binary_translator=>binary_structures
                        RETURNING VALUE(result) TYPE i.

    METHODS count_ones  IMPORTING position      TYPE binary_translator=>binary_structures
                        RETURNING VALUE(result) TYPE i.
    METHODS analyse_table IMPORTING input         TYPE stringtab
                          RETURNING VALUE(result) TYPE binary_translator=>binary_structures.
    METHODS get_most_common_bit IMPORTING position      TYPE binary_structure
                                RETURNING VALUE(result) TYPE char1.
    METHODS get_most_least_bit
      IMPORTING
        position      TYPE binary_structure
      RETURNING
        VALUE(result) TYPE char1.

ENDCLASS.

CLASS binary_translator IMPLEMENTATION.

  METHOD count_bits_per_position.
    DATA(temp_result)  = analyse_table( input ).
    DATA(max_position) = temp_result[ lines( temp_result ) ]-position + 1.
    DATA(actual_position) = 0.
    DO max_position TIMES.
      DATA(position) = FILTER binary_translator=>binary_structures( temp_result WHERE position = actual_position ).
      actual_position = actual_position + 1.
      DATA(result_line) = VALUE binary_translator=>binary_structure(
                                position = actual_position
                                count_0 = count_zeros( position )
                                count_1 = count_ones( position ) ).
      APPEND result_line TO result.
    ENDDO.
  ENDMETHOD.

  METHOD analyse_table.
    result  = VALUE #( FOR line IN input
                       FOR i = 0 THEN i + 1 UNTIL i = strlen( line )
                           ( position = i
                             count_0 = COND #( WHEN line+i(1) = 0 THEN 1
                                               ELSE 0 )
                             count_1 = COND #( WHEN line+i(1) = 1 THEN 1
                                               ELSE 0 ) ) ).
  ENDMETHOD.

  METHOD calculate_gamma_rate.
    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      DATA(char) = COND #( WHEN <line>-count_0 > <line>-count_1 THEN |0|
                           ELSE |1| ).
      result = |{ result }{ char }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_epsilon_rate.
    result = REDUCE #( INIT string = ``
                       FOR line IN input
                       LET char = COND #( WHEN line-count_0 > line-count_1 THEN |1|
                                          ELSE |0| )
                       IN
                       NEXT string = |{ string }{ char }| ).
  ENDMETHOD.


  METHOD multiplicate_binaries.
    DATA(first_operand) =  /ui2/cl_number=>base_converter( number = first from = 2 to = 10 ).
    DATA(second_operand) =  /ui2/cl_number=>base_converter( number = second from = 2 to = 10 ).
    result = first_operand * second_operand.
  ENDMETHOD.


  METHOD count_zeros.
    result = REDUCE i( INIT sum = 0
                       FOR line IN position
                       NEXT sum = sum + line-count_0 ).
  ENDMETHOD.

  METHOD count_ones.
    result = REDUCE i( INIT sum = 0
                       FOR line IN position
                       NEXT sum = sum + line-count_1 ).
  ENDMETHOD.

  METHOD filter_lines_most_pos.
    TYPES tt_text100 TYPE STANDARD TABLE OF text100 WITH EMPTY KEY.
    DATA lt_text100 TYPE STANDARD TABLE OF text100.

    DATA(positions) = count_bits_per_position( table ).
    DATA(search_bit) = get_most_common_bit( positions[ position = position ] ).
    DATA(offset) = position - 1.
    lt_text100 = table.
    DATA(lt_result) = VALUE tt_text100( FOR line IN lt_text100
                            WHERE ( table_line+offset(1) = search_bit ) ( line ) ).
    result = lt_result.
  ENDMETHOD.

  METHOD get_most_common_bit.
    result = COND #( WHEN position-count_0 > position-count_1 THEN |0|
                     WHEN position-count_0 < position-count_1 THEN |1|
                     WHEN position-count_0 = position-count_1 THEN |1| ).
  ENDMETHOD.

  METHOD filter_lines_least_pos.
    TYPES tt_text100 TYPE STANDARD TABLE OF text100 WITH EMPTY KEY.
    DATA lt_text100 TYPE STANDARD TABLE OF text100.

    DATA(positions) = count_bits_per_position( table ).
    DATA(search_bit) = get_most_least_bit( positions[ position = position ] ).
    DATA(offset) = position - 1.
    lt_text100 = table.
    DATA(lt_result) = VALUE tt_text100( FOR line IN lt_text100
                            WHERE ( table_line+offset(1) = search_bit ) ( line ) ).
    result = lt_result.
  ENDMETHOD.

  METHOD get_most_least_bit.
    result = COND #( WHEN position-count_0 < position-count_1 THEN |0|
                     WHEN position-count_0 > position-count_1 THEN |1|
                     WHEN position-count_0 = position-count_1 THEN |0| ).
  ENDMETHOD.

ENDCLASS.


CLASS application DEFINITION.
  PUBLIC SECTION.
    TYPES input TYPE RANGE OF text100.
    METHODS run IMPORTING input         TYPE input
                RETURNING VALUE(result) TYPE i.
    METHODS run_2 IMPORTING input         TYPE input
                  RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run.
    DATA(curated_input) = VALUE stringtab( FOR line IN input
                                            ( CONV #( line-low ) ) ).
    DATA(binary) = NEW binary_translator( ).
    DATA(transposed_numbers) = binary->count_bits_per_position( curated_input ).
    DATA(gamma) = binary->calculate_gamma_rate( transposed_numbers ).
    DATA(epsilon) = binary->calculate_epsilon_rate( transposed_numbers ).
    result = binary->multiplicate_binaries( first  = gamma second = epsilon ).
  ENDMETHOD.

  METHOD run_2.
    DATA(curated_input) = VALUE stringtab( FOR line IN input
                                            ( CONV #( line-low ) ) ).
    DATA(binary) = NEW binary_translator( ).
    DATA(position) = 0.
    DATA(oxygen) = curated_input.
    WHILE lines( oxygen ) > 1.
      position = position + 1.
      oxygen = binary->filter_lines_most_pos( position = position table = oxygen ).
    ENDWHILE.

    position = 0.
    DATA(co2) = curated_input.
    WHILE lines( co2 ) > 1.
      position = position + 1.
      co2 = binary->filter_lines_least_pos( position = position table = co2 ).
    ENDWHILE.

    result = binary->multiplicate_binaries( first = oxygen[ 1 ] second = co2[ 1 ] ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_binary DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO binary_translator.
    DATA raw_input TYPE stringtab.

    METHODS setup.
    METHODS transpose_binary_input   FOR TESTING.
    METHODS get_gamma_rate           FOR TESTING.
    METHODS get_epsilon_rate         FOR TESTING.
    METHODS get_result_from_binaries FOR TESTING.

    METHODS filter_lines_regarding_pos_1 FOR TESTING.
    METHODS filter_lines_reg_pos_1_twice FOR TESTING.
    METHODS filter_6_times_most_common FOR TESTING.
    METHODS filter_6_times_least_commom FOR TESTING.
ENDCLASS.


CLASS tc_binary IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    raw_input = VALUE #( ( |00100| )
                         ( |11110| )
                         ( |10110| )
                         ( |10111| )
                         ( |10101| )
                         ( |01111| )
                         ( |00111| )
                         ( |11100| )
                         ( |10000| )
                         ( |11001| )
                         ( |00010| )
                         ( |01010| ) ).

  ENDMETHOD.

  METHOD transpose_binary_input.
    DATA(expected_values) = VALUE binary_translator=>binary_structures(
                                    ( position = 1 count_0 = 5 count_1 = 7 )
                                    ( position = 2 count_0 = 7 count_1 = 5 )
                                    ( position = 3 count_0 = 4 count_1 = 8 )
                                    ( position = 4 count_0 = 5 count_1 = 7 )
                                    ( position = 5 count_0 = 7 count_1 = 5 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->count_bits_per_position( raw_input ) ).

  ENDMETHOD.

  METHOD get_gamma_rate.
    DATA(input) = cut->count_bits_per_position( raw_input ).
    cl_abap_unit_assert=>assert_equals(
        exp = |10110|
        act = cut->calculate_gamma_rate( input ) ).
  ENDMETHOD.

  METHOD get_epsilon_rate.
    DATA(input) = cut->count_bits_per_position( raw_input ).
    cl_abap_unit_assert=>assert_equals(
        exp = |01001|
        act = cut->calculate_epsilon_rate( input ) ).
  ENDMETHOD.

  METHOD get_result_from_binaries.
    cl_abap_unit_assert=>assert_equals(
        exp = 198 act = cut->multiplicate_binaries( first  = |10110| second = |01001| ) ).
  ENDMETHOD.

  METHOD filter_lines_regarding_pos_1.
    DATA(expected_results) = VALUE stringtab( ( |11110| )
                                              ( |10110| )
                                              ( |10111| )
                                              ( |10101| )
                                              ( |11100| )
                                              ( |10000| )
                                              ( |11001| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_results
        act = cut->filter_lines_most_pos( position = 1 table = raw_input ) ).
  ENDMETHOD.

  METHOD filter_lines_reg_pos_1_twice.
    DATA(input) = cut->filter_lines_most_pos( position = 1 table = raw_input ).
    DATA(expected_values) = VALUE stringtab( ( |10110| )
                                             ( |10111| )
                                             ( |10101| )
                                             ( |10000| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->filter_lines_most_pos( position = 2 table = input ) ).
  ENDMETHOD.

  METHOD filter_6_times_most_common.
    DATA position TYPE i.
    DATA(lt_input) = raw_input.
    DO 5 TIMES.
      position = position + 1.
      lt_input = cut->filter_lines_most_pos( position = position table = lt_input ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = |10111|
        act = lt_input[ 1 ] ).
  ENDMETHOD.

  METHOD filter_6_times_least_commom.
    DATA position TYPE i.
    DATA(lt_input) = raw_input.
    WHILE lines( lt_input ) > 1.
      position = position + 1.
      lt_input = cut->filter_lines_least_pos( position = position table = lt_input ).
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals(
        exp = |01010|
        act = lt_input[ 1 ] ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text100.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(lo_application) = NEW application( ).
  WRITE / |Ergebnis Teil 1: { lo_application->run( so_input[] ) }|.
  WRITE / |Ergebnis Teil 2: { lo_application->run_2( so_input[] ) }|.
