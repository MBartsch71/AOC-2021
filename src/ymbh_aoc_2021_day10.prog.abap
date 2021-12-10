REPORT ymbh_aoc_2021_day10.

CLASS chunk_finder DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF chunk,
             opening TYPE string,
             closing TYPE string,
           END OF chunk.
    TYPES chunks TYPE STANDARD TABLE OF chunk WITH EMPTY KEY.

    METHODS constructor.


    METHODS investigate_chunks    IMPORTING input         TYPE string
                                  RETURNING VALUE(result) TYPE stringtab.

    METHODS get_invalid_chunks RETURNING VALUE(result) TYPE stringtab.

    METHODS get_closing_sequences RETURNING VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    DATA opening_chunks    TYPE RANGE OF string.
    DATA chunk_pairs       TYPE chunks.
    DATA invalid_chunks    TYPE stringtab.
    DATA closing_sequences TYPE stringtab.

    METHODS determine_closing_sequence IMPORTING result TYPE stringtab.

ENDCLASS.

CLASS chunk_finder IMPLEMENTATION.

  METHOD constructor.
    opening_chunks = VALUE #( ( sign = |I| option = |EQ| low = `(` )
                              ( sign = |I| option = |EQ| low = `[` )
                              ( sign = |I| option = |EQ| low = `{` )
                              ( sign = |I| option = |EQ| low = `<` ) ).
    chunk_pairs = VALUE #( ( opening = `(`  closing = `)` )
                           ( opening = `[`  closing = `]` )
                           ( opening = `{`  closing = `}` )
                           ( opening = `<`  closing = `>` ) ).
  ENDMETHOD.

  METHOD investigate_chunks.
    DATA index TYPE i.
    DATA invalid TYPE abap_bool.
    WHILE index < strlen( input ).
      DATA(char) = substring( val = input off = index len = 1 ).
      IF char IN opening_chunks.
        result = VALUE #( BASE result ( char ) ).
      ELSE.
        DATA(opening) = chunk_pairs[ closing = char ]-opening.
        IF result[ lines( result ) ] = opening.
          DELETE result INDEX lines( result ).
        ELSE.
          invalid = abap_true.
          invalid_chunks = VALUE #( BASE invalid_chunks ( char ) ).
          EXIT.
        ENDIF.
      ENDIF.
      index = index + 1.
    ENDWHILE.
    IF invalid = abap_false.
      determine_closing_sequence( result ).
    ENDIF.
  ENDMETHOD.

  METHOD get_invalid_chunks.
    result = invalid_chunks.
  ENDMETHOD.

  METHOD determine_closing_sequence.
    DATA(line) = REDUCE #( INIT res = ``
                           FOR <line> IN result
                           NEXT res = res && chunk_pairs[ opening = <line> ]-closing ).
    closing_sequences = VALUE #( BASE closing_sequences ( reverse( line ) ) ).
  ENDMETHOD.

  METHOD get_closing_sequences.
    result = closing_sequences.
  ENDMETHOD.

ENDCLASS.

CLASS result_calculator DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES tt_scores TYPE SORTED TABLE OF decfloat34 WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

    METHODS calculate_result   IMPORTING invalid_chunks TYPE stringtab
                               RETURNING VALUE(result)  TYPE i.

    METHODS calculate_scores   IMPORTING closing_sequences TYPE stringtab
                               RETURNING VALUE(result)     TYPE tt_scores.

    METHODS get_middle_score   IMPORTING scores        TYPE tt_scores
                               RETURNING VALUE(result) TYPE decfloat34.

  PRIVATE SECTION.
    METHODS get_value_of_chunk IMPORTING line          TYPE string
                               RETURNING VALUE(result) TYPE i.

    METHODS character_value    IMPORTING char          TYPE string
                               RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS result_calculator IMPLEMENTATION.


  METHOD calculate_result.
    result = REDUCE #( INIT sum = 0
                       FOR line IN invalid_chunks
                       NEXT sum = sum + get_value_of_chunk( line ) ).
  ENDMETHOD.

  METHOD get_value_of_chunk.
    result = SWITCH #( line WHEN `)` THEN 3
                            WHEN `]` THEN 57
                            WHEN `}` THEN 1197
                            WHEN `>` THEN 25137 ).
  ENDMETHOD.

  METHOD calculate_scores.
    DATA index TYPE i.
    DATA score TYPE decfloat34.

    LOOP AT closing_sequences ASSIGNING FIELD-SYMBOL(<line>).
      score = 0.
      WHILE index < strlen( <line> ).
        DATA(char) = substring( val = <line> off = index len = 1 ).
        score = score * 5 + character_value( char ).
        index = index + 1.
      ENDWHILE.
      result = VALUE #( BASE result ( score ) ).
      index = 0.
    ENDLOOP.
  ENDMETHOD.


  METHOD character_value.
    result = SWITCH #( char WHEN `)` THEN 1
                            WHEN `]` THEN 2
                            WHEN `}` THEN 3
                            WHEN `>` THEN 4 ).
  ENDMETHOD.

  METHOD get_middle_score.
    DATA(half_way) = ( lines( scores ) - 1 ) / 2.
    result = scores[ half_way + 1 ].
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    TYPES tt_input TYPE RANGE OF text1024.
    METHODS run_1st_part IMPORTING input         TYPE tt_input
                         RETURNING VALUE(result) TYPE i.

    METHODS run_2nd_part RETURNING VALUE(result) TYPE decfloat34.

  PRIVATE SECTION.
    DATA chunk_finder      TYPE REF TO chunk_finder.
    DATA result_calculator TYPE REF TO result_calculator.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run_1st_part.
    chunk_finder = NEW #( ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      chunk_finder->investigate_chunks( CONV #( <input>-low ) ).
    ENDLOOP.
    DATA(invalid_chunks) = chunk_finder->get_invalid_chunks( ).
    result_calculator = NEW #( ).
    result = result_calculator->calculate_result( invalid_chunks ).
  ENDMETHOD.

  METHOD run_2nd_part.
    DATA(closing_sequences) = chunk_finder->get_closing_sequences( ).
    DATA(scores) = result_calculator->calculate_scores( closing_sequences ).
    result = result_calculator->get_middle_score( scores ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_chunk_investigator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO chunk_finder.

    METHODS setup.
    METHODS build_input RETURNING VALUE(result) TYPE stringtab.

    METHODS investigate_series_of_chunks FOR TESTING.
    METHODS determine_closing_sequences  FOR TESTING.
ENDCLASS.


CLASS tc_chunk_investigator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD build_input.
    result = VALUE #( ( `[({(<(())[]>[[{[]{<()<>>` )
                      ( `[(()[<>])]({[<{<<[]>>(` )
                      ( `{([(<{}[<>[]}>{[]{[(<()>` )
                      ( `(((({<>}<{<{<>}{[]{[]{}` )
                      ( `[[<[([]))<([[{}[[()]]]` )
                      ( `[{[{({}]{}}([{[{{{}}([]` )
                      ( `{<[[]]>}<{[{[{[]{()[[[]` )
                      ( `[<(<(<(<{}))><([]([]()` )
                      ( `<{([([[(<>()){}]>(<<{{` )
                      ( `<{([{{}}[<[[[<>{}]]]>[]]` ) ).
  ENDMETHOD.

  METHOD investigate_series_of_chunks.
    DATA(expected_values) = VALUE stringtab( ( `}` )
                                             ( `)` )
                                             ( `]` )
                                             ( `)` )
                                             ( `>` ) ).
    DATA(input) = build_input( ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      cut->investigate_chunks( <input> ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_invalid_chunks( ) ).
  ENDMETHOD.

  METHOD determine_closing_sequences.
    DATA(expected_values) = VALUE stringtab( ( `}}]])})]` )
                                             ( `)}>]})` )
                                             ( `}}>}>))))` )
                                             ( `]]}}]}]}>` )
                                             ( `])}>` ) ).
    DATA(input) = build_input( ).
    LOOP AT input ASSIGNING FIELD-SYMBOL(<input>).
      cut->investigate_chunks( <input> ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_closing_sequences( ) ).
  ENDMETHOD.


ENDCLASS.

CLASS tc_result_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO result_calculator.

    METHODS setup.
    METHODS calculate_final_result      FOR TESTING.
    METHODS calculate_incomplete_scores FOR TESTING.
    METHODS get_middle_score            FOR TESTING.
ENDCLASS.


CLASS tc_result_calculator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD calculate_final_result.
    DATA(invalid_chunks) = VALUE stringtab( ( `}` )
                                            ( `)` )
                                            ( `]` )
                                            ( `)` )
                                            ( `>` ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 26397
        act = cut->calculate_result( invalid_chunks ) ).
  ENDMETHOD.

  METHOD calculate_incomplete_scores.
    DATA expected_values TYPE SORTED TABLE OF decfloat34 WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.
    DATA(closing_sequences) = VALUE stringtab( ( `}}]])})]` )
                                               ( `)}>]})` )
                                               ( `}}>}>))))` )
                                               ( `]]}}]}]}>` )
                                               ( `])}>` ) ).
    expected_values = VALUE #( ( CONV #( 294 ) )
                               ( CONV #( 5566 ) )
                               ( CONV #( 288957 ) )
                               ( CONV #( 995444 ) )
                               ( CONV #( 1480781 ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->calculate_scores( closing_sequences ) ).
  ENDMETHOD.

  METHOD get_middle_score.
    DATA scores TYPE SORTED TABLE OF decfloat34 WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

    scores = VALUE #( ( CONV #( 294 ) )
                      ( CONV #( 5566 ) )
                      ( CONV #( 288957 ) )
                      ( CONV #( 995444 ) )
                      ( CONV #( 1480781 ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 288957
        act = cut->get_middle_score( scores ) ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.

  DATA(application) = NEW application( ).

  WRITE / |Ergebnis Teil 1: { application->run_1st_part( so_input[] ) }|.
  WRITE / |Ergebnis Teil 2: { application->run_2nd_part( ) }|.
