REPORT ymbh_aoc_2021_day14.

INTERFACE if_input.
  TYPES: BEGIN OF pair_insertion,
           rule      TYPE c LENGTH 2,
           insertion TYPE c LENGTH 1,
         END OF pair_insertion.
  TYPES pair_insertions TYPE SORTED TABLE OF pair_insertion WITH UNIQUE KEY primary_key COMPONENTS rule.
  TYPES raw_input TYPE RANGE OF text1024.

  METHODS curate_input IMPORTING raw_input TYPE raw_input.

  METHODS get_template_string RETURNING VALUE(result) TYPE string.

  METHODS get_insertion_rules RETURNING VALUE(result) TYPE if_input=>pair_insertions.

ENDINTERFACE.

INTERFACE if_sequence.
  METHODS process_sequence_times IMPORTING times TYPE i.
  METHODS get_sequence           RETURNING VALUE(result) TYPE string.

ENDINTERFACE.

INTERFACE if_string_investigator.
  TYPES: BEGIN OF character_info,
           character TYPE c LENGTH 1,
           amount    TYPE decfloat34,
         END OF character_info.
  TYPES characters_info TYPE SORTED TABLE OF character_info WITH UNIQUE KEY primary_key COMPONENTS character
                                                            WITH NON-UNIQUE SORTED KEY amount COMPONENTS amount.

  METHODS build_distinct_characters IMPORTING char_string TYPE string.

  METHODS count_chars.

  METHODS get_characters RETURNING VALUE(result) TYPE characters_info.

  METHODS get_max_char RETURNING VALUE(result) TYPE character_info.

  METHODS get_min_char RETURNING VALUE(result) TYPE character_info.

ENDINTERFACE.

INTERFACE if_application.

  METHODS perform_part_1 RETURNING VALUE(result) TYPE decfloat34.
  METHODS perform_part_2 RETURNING VALUE(result) TYPE decfloat34.

ENDINTERFACE.


CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_input.

  PRIVATE SECTION.
    DATA sequence TYPE string.
    DATA rules TYPE if_input=>pair_insertions.

    METHODS extract_sequence         IMPORTING raw_input TYPE if_input=>raw_input.

    METHODS building_insertion_rules IMPORTING raw_input TYPE if_input=>raw_input.

    METHODS split_line_into_rule     IMPORTING line          TYPE text1024
                                     RETURNING VALUE(result) TYPE if_input=>pair_insertion.
ENDCLASS.


CLASS input_processor IMPLEMENTATION.

  METHOD if_input~curate_input.
    extract_sequence( raw_input ).
    building_insertion_rules( raw_input ).
  ENDMETHOD.

  METHOD if_input~get_template_string.
    result = sequence.
  ENDMETHOD.

  METHOD if_input~get_insertion_rules.
    result = rules.
  ENDMETHOD.

  METHOD extract_sequence.
    sequence = raw_input[ 1 ]-low.
  ENDMETHOD.

  METHOD building_insertion_rules.
    rules = VALUE #( FOR line IN raw_input FROM 2
                        ( split_line_into_rule( line-low ) ) ).
  ENDMETHOD.

  METHOD split_line_into_rule.
    SPLIT line AT ' -> ' INTO result-rule result-insertion.
  ENDMETHOD.

ENDCLASS.

CLASS sequence_builder DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_sequence.
    METHODS constructor IMPORTING sequence TYPE string
                                  rules    TYPE if_input=>pair_insertions.

  PRIVATE SECTION.
    DATA sequence TYPE string.
    DATA rules TYPE if_input=>pair_insertions.
    DATA current_offset TYPE i.
    METHODS get_next_pair_from_sequence RETURNING VALUE(result) TYPE string.
    METHODS get_insertion_for_pair
      IMPORTING
        pair          TYPE string
      RETURNING
        VALUE(result) TYPE string.
    METHODS build_next_section
      IMPORTING
        pair          TYPE string
        insertion     TYPE string
        index         TYPE i
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS sequence_builder IMPLEMENTATION.

  METHOD constructor.
    me->rules = rules.
    me->sequence = sequence.
  ENDMETHOD.

  METHOD if_sequence~get_sequence.
    result = sequence.
  ENDMETHOD.

  METHOD if_sequence~process_sequence_times.
    DO times TIMES.
      DATA(new_sequence) = ``.
      WHILE current_offset < strlen( sequence ) - 1.
        DATA(pair) = get_next_pair_from_sequence( ).
        DATA(insertion) = get_insertion_for_pair( pair ).
        new_sequence = |{ new_sequence }{ build_next_section( pair = pair insertion = insertion index = sy-index ) }|.
      ENDWHILE.
      current_offset = 0.
      sequence = new_sequence.
    ENDDO.
  ENDMETHOD.

  METHOD get_next_pair_from_sequence.
    result = substring( val = sequence off = current_offset len = 2 ).
    current_offset = current_offset + 1.
  ENDMETHOD.

  METHOD get_insertion_for_pair.
    result = rules[ rule = pair ]-insertion.
  ENDMETHOD.

  METHOD build_next_section.
    result = SWITCH #( index WHEN 1 THEN |{ pair(1) }{ insertion }{ pair+1(1) }|
                       ELSE |{ insertion }{ pair+1(1) }| ).
  ENDMETHOD.

ENDCLASS.

CLASS string_investigator DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_string_investigator.

  PRIVATE SECTION.
    DATA chars_info TYPE if_string_investigator=>characters_info.
    DATA char_string TYPE string.

    METHODS fill_distinct_strings.

    METHODS get_character         IMPORTING index         TYPE any
                                  RETURNING VALUE(result) TYPE if_string_investigator=>characters_info.

ENDCLASS.

CLASS string_investigator IMPLEMENTATION.

  METHOD if_string_investigator~build_distinct_characters.
    me->char_string = char_string.
    fill_distinct_strings( ).
  ENDMETHOD.

  METHOD if_string_investigator~get_characters.
    result = chars_info.
  ENDMETHOD.

  METHOD fill_distinct_strings.
    chars_info = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = strlen( char_string )
                            ( LINES OF get_character( index = i ) ) ).
  ENDMETHOD.

  METHOD get_character.
    DATA(char) = substring( val = char_string off = index len = 1 ).
    IF NOT line_exists( chars_info[ character = char ] ).
      result = VALUE #( ( character = char ) ).
    ENDIF.
  ENDMETHOD.

  METHOD if_string_investigator~count_chars.
    LOOP AT chars_info REFERENCE INTO DATA(line).
      FIND ALL OCCURRENCES OF line->character IN char_string MATCH COUNT DATA(count).
      line->amount = count.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_string_investigator~get_max_char.
    result = chars_info[ KEY amount INDEX lines( chars_info ) ].
  ENDMETHOD.

  METHOD if_string_investigator~get_min_char.
    result = chars_info[ KEY amount INDEX 1 ].
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES raw_input TYPE RANGE OF text1024.

    INTERFACES if_application.
    METHODS constructor IMPORTING raw_input TYPE raw_input.

  PRIVATE SECTION.
    DATA r_input TYPE raw_input.
    DATA input_processor TYPE REF TO if_input.
    DATA sequence TYPE REF TO if_sequence.
    DATA string_investigator TYPE REF TO if_string_investigator.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    r_input = raw_input.
  ENDMETHOD.

  METHOD if_application~perform_part_1.
    input_processor = NEW input_processor( ).
    input_processor->curate_input( r_input ).

    sequence = NEW sequence_builder( sequence = input_processor->get_template_string( )
                                     rules    = input_processor->get_insertion_rules( ) ).
    sequence->process_sequence_times( 10 ).

    DATA(result_string) = sequence->get_sequence( ).
    string_investigator = NEW string_investigator( ).
    string_investigator->build_distinct_characters( result_string ).
    string_investigator->count_chars( ).

    result = string_investigator->get_max_char( )-amount - string_investigator->get_min_char( )-amount.
  ENDMETHOD.

  METHOD if_application~perform_part_2.
    input_processor = NEW input_processor( ).
    input_processor->curate_input( r_input ).

    sequence = NEW sequence_builder( sequence = input_processor->get_template_string( )
                                     rules    = input_processor->get_insertion_rules( ) ).
    sequence->process_sequence_times( 40 ).

    DATA(result_string) = sequence->get_sequence( ).
    string_investigator = NEW string_investigator( ).
    string_investigator->build_distinct_characters( result_string ).
    string_investigator->count_chars( ).

    result = string_investigator->get_max_char( )-amount - string_investigator->get_min_char( )-amount.
  ENDMETHOD.

ENDCLASS.

CLASS tc_input DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_input.

    METHODS setup.
    METHODS get_sequence_from_curatd_input FOR TESTING.
    METHODS get_substitution_pattern       FOR TESTING.
ENDCLASS.

CLASS tc_input IMPLEMENTATION.

  METHOD setup.
    DATA(raw_input) = VALUE if_input=>raw_input( ( sign = 'I' option = 'EQ' low = 'NNCB' )
                                                 ( sign = 'I' option = 'EQ' low = 'CH -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'HH -> N' )
                                                 ( sign = 'I' option = 'EQ' low = 'CB -> H' )
                                                 ( sign = 'I' option = 'EQ' low = 'NH -> C' )
                                                 ( sign = 'I' option = 'EQ' low = 'HB -> C' )
                                                 ( sign = 'I' option = 'EQ' low = 'HC -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'HN -> C' )
                                                 ( sign = 'I' option = 'EQ' low = 'NN -> C' )
                                                 ( sign = 'I' option = 'EQ' low = 'BH -> H' )
                                                 ( sign = 'I' option = 'EQ' low = 'NC -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'NB -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'BN -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'BB -> N' )
                                                 ( sign = 'I' option = 'EQ' low = 'BC -> B' )
                                                 ( sign = 'I' option = 'EQ' low = 'CC -> N' )
                                                 ( sign = 'I' option = 'EQ' low = 'CN -> C' ) ).
    cut = NEW input_processor( ).
    cut->curate_input( raw_input ).
  ENDMETHOD.

  METHOD get_sequence_from_curatd_input.
    DATA(sequence) = `NNCB`.
    cl_abap_unit_assert=>assert_equals(
        exp = sequence
        act = cut->get_template_string( ) ).
  ENDMETHOD.

  METHOD get_substitution_pattern.
    DATA(expected_values) = VALUE if_input=>pair_insertions( ( rule = 'BB' insertion = 'N' )
                                                             ( rule = 'BC' insertion = 'B' )
                                                             ( rule = 'BH' insertion = 'H' )
                                                             ( rule = 'BN' insertion = 'B' )
                                                             ( rule = 'CB' insertion = 'H' )
                                                             ( rule = 'CC' insertion = 'N' )
                                                             ( rule = 'CH' insertion = 'B' )
                                                             ( rule = 'CN' insertion = 'C' )
                                                             ( rule = 'HB' insertion = 'C' )
                                                             ( rule = 'HC' insertion = 'B' )
                                                             ( rule = 'HH' insertion = 'N' )
                                                             ( rule = 'NB' insertion = 'B' )
                                                             ( rule = 'NC' insertion = 'B' )
                                                             ( rule = 'HN' insertion = 'C' )
                                                             ( rule = 'NH' insertion = 'C' )
                                                             ( rule = 'NN' insertion = 'C' ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_insertion_rules( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_sequence_builder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_sequence.

    METHODS setup.
    METHODS get_rules RETURNING VALUE(result) TYPE if_input=>pair_insertions.

    METHODS get_sequence_after_1st_step FOR TESTING.
    METHODS get_sequence_after_3_steps  FOR TESTING.

ENDCLASS.


CLASS tc_sequence_builder IMPLEMENTATION.

  METHOD setup.
    cut = NEW sequence_builder( sequence = 'NNCB' rules = get_rules( ) ).
  ENDMETHOD.

  METHOD get_rules.
    result = VALUE #( ( rule = 'BB' insertion = 'N' )
                      ( rule = 'BC' insertion = 'B' )
                      ( rule = 'BH' insertion = 'H' )
                      ( rule = 'BN' insertion = 'B' )
                      ( rule = 'CB' insertion = 'H' )
                      ( rule = 'CC' insertion = 'N' )
                      ( rule = 'CH' insertion = 'B' )
                      ( rule = 'CN' insertion = 'C' )
                      ( rule = 'HB' insertion = 'C' )
                      ( rule = 'HC' insertion = 'B' )
                      ( rule = 'HH' insertion = 'N' )
                      ( rule = 'NB' insertion = 'B' )
                      ( rule = 'NC' insertion = 'B' )
                      ( rule = 'HN' insertion = 'C' )
                      ( rule = 'NH' insertion = 'C' )
                      ( rule = 'NN' insertion = 'C' ) ).
  ENDMETHOD.

  METHOD get_sequence_after_1st_step.
    cut->process_sequence_times( 1 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'NCNBCHB'
        act = cut->get_sequence( ) ).
  ENDMETHOD.

  METHOD get_sequence_after_3_steps.
    cut->process_sequence_times( 3 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'NBBBCNCCNBBNBNBBCHBHHBCHB'
        act = cut->get_sequence( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_string_investigator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_string_investigator.

    METHODS setup.
    METHODS build_distinct_string_table FOR TESTING.
    METHODS count_amount_of_characters  FOR TESTING.
    METHODS get_max_count_of_chars      FOR TESTING.
    METHODS get_min_count_char          FOR TESTING.
ENDCLASS.

CLASS tc_string_investigator IMPLEMENTATION.

  METHOD setup.
    cut = NEW string_investigator( ).
  ENDMETHOD.

  METHOD build_distinct_string_table.
    DATA(expected_values) = VALUE if_string_investigator=>characters_info( ( character = 'B' amount = 0 ) "11
                                                                           ( character = 'C' amount = 0 ) "5
                                                                           ( character = 'H' amount = 0 ) "4
                                                                           ( character = 'N' amount = 0 ) ). "5
    cut->build_distinct_characters( |NBBBCNCCNBBNBNBBCHBHHBCHB| ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_characters( ) ).
  ENDMETHOD.

  METHOD count_amount_of_characters.
    DATA(expected_values) = VALUE if_string_investigator=>characters_info( ( character = 'B' amount = 11 )
                                                                           ( character = 'C' amount = 5 )
                                                                           ( character = 'H' amount = 4 )
                                                                           ( character = 'N' amount = 5 ) ).
    cut->build_distinct_characters( |NBBBCNCCNBBNBNBBCHBHHBCHB| ).
    cut->count_chars( ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_characters( ) ).

  ENDMETHOD.

  METHOD get_max_count_of_chars.
    cut->build_distinct_characters( |NBBBCNCCNBBNBNBBCHBHHBCHB| ).
    cut->count_chars( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 11
        act = cut->get_max_char( )-amount ).
  ENDMETHOD.

  METHOD get_min_count_char.
    cut->build_distinct_characters( |NBBBCNCCNBBNBNBBCHBHHBCHB| ).
    cut->count_chars( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->get_min_char( )-amount ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_application.

    METHODS setup.
    METHODS get_result_from_part_1 FOR TESTING.
ENDCLASS.


CLASS tc_application IMPLEMENTATION.


  METHOD get_result_from_part_1.
    cl_abap_unit_assert=>assert_equals(
        exp = 1588
        act = cut->perform_part_1( ) ).
  ENDMETHOD.

  METHOD setup.
    DATA raw_input TYPE RANGE OF text1024.
    raw_input = VALUE #( ( sign = 'I' option = 'EQ' low = 'NNCB' )
                         ( sign = 'I' option = 'EQ' low = 'CH -> B' )
                         ( sign = 'I' option = 'EQ' low = 'HH -> N' )
                         ( sign = 'I' option = 'EQ' low = 'CB -> H' )
                         ( sign = 'I' option = 'EQ' low = 'NH -> C' )
                         ( sign = 'I' option = 'EQ' low = 'HB -> C' )
                         ( sign = 'I' option = 'EQ' low = 'HC -> B' )
                         ( sign = 'I' option = 'EQ' low = 'HN -> C' )
                         ( sign = 'I' option = 'EQ' low = 'NN -> C' )
                         ( sign = 'I' option = 'EQ' low = 'BH -> H' )
                         ( sign = 'I' option = 'EQ' low = 'NC -> B' )
                         ( sign = 'I' option = 'EQ' low = 'NB -> B' )
                         ( sign = 'I' option = 'EQ' low = 'BN -> B' )
                         ( sign = 'I' option = 'EQ' low = 'BB -> N' )
                         ( sign = 'I' option = 'EQ' low = 'BC -> B' )
                         ( sign = 'I' option = 'EQ' low = 'CC -> N' )
                         ( sign = 'I' option = 'EQ' low = 'CN -> C' ) ).
    cut = NEW application( raw_input ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.

  DATA(application) = NEW application( so_input[] ).

  WRITE / |Ergebnis Teil 1: { application->if_application~perform_part_1( ) }|.
  WRITE / |Ergebnis Teil 2: { application->if_application~perform_part_2( ) }|.
