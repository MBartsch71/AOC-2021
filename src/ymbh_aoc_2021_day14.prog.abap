REPORT ymbh_aoc_2021_day14.

INTERFACE if_input.
  TYPES: BEGIN OF rule,
           character_pair TYPE c LENGTH 2,
           insert_char    TYPE c LENGTH 1,
         END OF rule.
  TYPES rules TYPE SORTED TABLE OF rule WITH UNIQUE KEY primary_key COMPONENTS character_pair.
  TYPES raw_input TYPE RANGE OF text1024.

  METHODS get_template RETURNING VALUE(result) TYPE string.

  METHODS get_rule_table RETURNING VALUE(result) TYPE rules.

ENDINTERFACE.

INTERFACE if_template.
  TYPES: BEGIN OF char_pair,
           char_pair TYPE c LENGTH 2,
           amount    TYPE decfloat34,
         END OF char_pair.
  TYPES char_pairs TYPE SORTED TABLE OF char_pair WITH UNIQUE KEY primary_key COMPONENTS char_pair.

  TYPES: BEGIN OF char,
           char   TYPE c LENGTH 1,
           amount TYPE decfloat34,
         END OF char.
  TYPES chars TYPE SORTED TABLE OF char WITH UNIQUE KEY primary_key COMPONENTS char
                                        WITH NON-UNIQUE SORTED KEY amount COMPONENTS amount.

  METHODS split_template IMPORTING template TYPE string.

  METHODS process_template_times IMPORTING times TYPE i.

  METHODS count_chars.

  METHODS get_char_pair_table RETURNING VALUE(result) TYPE char_pairs.

  METHODS get_char_table RETURNING VALUE(result) TYPE chars.

ENDINTERFACE.


CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_input.
    METHODS constructor IMPORTING raw_input TYPE if_input=>raw_input.

  PRIVATE SECTION.
    DATA template TYPE string.
    DATA rules TYPE if_input=>rules.

    METHODS extract_template   IMPORTING raw_input TYPE if_input=>raw_input.

    METHODS extract_rules      IMPORTING raw_input TYPE if_input=>raw_input.

    METHODS split_line_in_rule IMPORTING line          TYPE text1024
                               RETURNING VALUE(result) TYPE if_input=>rule.

ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD constructor.
    extract_template( raw_input ).
    extract_rules( raw_input ).
  ENDMETHOD.

  METHOD if_input~get_template.
    result = template.
  ENDMETHOD.

  METHOD if_input~get_rule_table.
    result = rules.
  ENDMETHOD.

  METHOD extract_template.
    template = raw_input[ 1 ]-low.
  ENDMETHOD.

  METHOD extract_rules.
    rules = VALUE #( FOR line IN raw_input FROM 2
                        ( split_line_in_rule( line-low ) ) ).
  ENDMETHOD.

  METHOD split_line_in_rule.
    SPLIT line AT ' -> ' INTO result-character_pair result-insert_char.
  ENDMETHOD.

ENDCLASS.

CLASS template_processor DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_template.
    METHODS constructor IMPORTING rules TYPE if_input=>rules.

  PRIVATE SECTION.
    DATA template TYPE string.
    DATA char_pairs TYPE if_template=>char_pairs.
    DATA new_pairs  TYPE if_template=>char_pairs.
    DATA chars      TYPE if_template=>chars.

    DATA rules TYPE if_input=>rules.

    METHODS build_new_pairs_after_rules IMPORTING line          TYPE if_template=>char_pair
                                        RETURNING VALUE(result) TYPE if_template=>char_pairs.

    METHODS add_pair IMPORTING pair   TYPE char2
                               amount TYPE decfloat.

    METHODS build_first_pair IMPORTING line          TYPE if_template=>char_pair
                                       insert_char   TYPE char1
                             RETURNING VALUE(result) TYPE char2 .

    METHODS build_second_pair IMPORTING line          TYPE if_template=>char_pair
                                        insert_char   TYPE c
                              RETURNING VALUE(result) TYPE char2.

    METHODS update_char_line IMPORTING line TYPE if_template=>char_pair.

    METHODS update_char IMPORTING char   TYPE char1
                                  amount TYPE decfloat34.

ENDCLASS.

CLASS template_processor IMPLEMENTATION.

  METHOD constructor.
    me->rules = rules.
  ENDMETHOD.

  METHOD if_template~get_char_pair_table.
    result = char_pairs.
  ENDMETHOD.

  METHOD if_template~split_template.
    me->template = template.
    char_pairs = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = strlen( template ) - 1
                            ( char_pair = substring( val = template off = i len = 2 )
                              amount    = 1 ) ).
  ENDMETHOD.

  METHOD if_template~process_template_times.
    DO times TIMES.
      new_pairs = VALUE if_template=>char_pairs(
                              FOR line IN char_pairs
                              ( LINES OF build_new_pairs_after_rules( line ) ) ).
      char_pairs = new_pairs.
    ENDDO.
  ENDMETHOD.

  METHOD build_new_pairs_after_rules.
    DATA(insert_char) = rules[ character_pair = line-char_pair ]-insert_char.
    add_pair( pair = build_first_pair( line = line insert_char = insert_char ) amount = line-amount ).
    add_pair( pair = build_second_pair( line = line insert_char = insert_char ) amount = line-amount ).
  ENDMETHOD.

  METHOD add_pair.
    IF line_exists( new_pairs[ char_pair = pair ] ).
      new_pairs[ char_pair = pair ]-amount = new_pairs[ char_pair = pair ]-amount + amount.
    ELSE.
      new_pairs = VALUE #( BASE new_pairs ( char_pair = pair amount = amount ) ).
    ENDIF.
  ENDMETHOD.

  METHOD build_first_pair.
    result = |{ line-char_pair(1) }{ insert_char }|.
  ENDMETHOD.

  METHOD build_second_pair.
    result = |{ insert_char }{ line-char_pair+1(1) }|.
  ENDMETHOD.

  METHOD if_template~count_chars.
    LOOP AT char_pairs REFERENCE INTO DATA(line).
      update_char_line( line->* ).
    ENDLOOP.
    DATA(last_char) = substring( val = template off = strlen( template ) - 1 len = 1 ).
    update_char( char = CONV #( last_char ) amount = 1 ).
  ENDMETHOD.

  METHOD if_template~get_char_table.
    result = chars.
  ENDMETHOD.

  METHOD update_char_line.
    update_char( char = line-char_pair(1) amount = line-amount ).
  ENDMETHOD.

  METHOD update_char.
    IF line_exists( chars[ char = char ] ).
      chars[ char = char ]-amount = chars[ char = char ]-amount + amount.
    ELSE.
      chars = VALUE #( BASE chars ( char = char amount = amount ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING raw_input     TYPE if_input=>raw_input
                          runs          TYPE i
                RETURNING VALUE(result) TYPE decfloat34.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run.
    DATA(input_processor) = NEW input_processor( raw_input ).
    DATA(template) = input_processor->if_input~get_template( ).
    DATA(rules) = input_processor->if_input~get_rule_table( ).

    DATA(template_processor) = NEW template_processor( rules ).
    template_processor->if_template~split_template( template ).
    template_processor->if_template~process_template_times( runs ).
    template_processor->if_template~count_chars( ).

    DATA(chars) = template_processor->if_template~get_char_table( ).
    DATA(most_char) = chars[ KEY amount INDEX lines( chars ) ].
    DATA(least_char) = chars[ KEY amount INDEX 1 ].

    result = most_char-amount - least_char-amount.

  ENDMETHOD.

ENDCLASS.

CLASS tc_input DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_input.

    METHODS setup.
    METHODS extract_template_from_input FOR TESTING.
    METHODS extract_rule_table_from_input FOR TESTING.

ENDCLASS.


CLASS tc_input IMPLEMENTATION.

  METHOD extract_template_from_input.
    cl_abap_unit_assert=>assert_equals(
        exp = |NNCB|
        act = cut->get_template( ) ).
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
    cut = NEW input_processor( raw_input ).
  ENDMETHOD.

  METHOD extract_rule_table_from_input.
    DATA(expected_values) = VALUE if_input=>rules( ( character_pair = 'CH' insert_char = 'B' )
                                                   ( character_pair = 'HH' insert_char = 'N' )
                                                   ( character_pair = 'CB' insert_char = 'H' )
                                                   ( character_pair = 'NH' insert_char = 'C' )
                                                   ( character_pair = 'HB' insert_char = 'C' )
                                                   ( character_pair = 'HC' insert_char = 'B' )
                                                   ( character_pair = 'HN' insert_char = 'C' )
                                                   ( character_pair = 'NN' insert_char = 'C' )
                                                   ( character_pair = 'BH' insert_char = 'H' )
                                                   ( character_pair = 'NC' insert_char = 'B' )
                                                   ( character_pair = 'NB' insert_char = 'B' )
                                                   ( character_pair = 'BN' insert_char = 'B' )
                                                   ( character_pair = 'BB' insert_char = 'N' )
                                                   ( character_pair = 'BC' insert_char = 'B' )
                                                   ( character_pair = 'CC' insert_char = 'N' )
                                                   ( character_pair = 'CN' insert_char = 'C' ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_rule_table( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_template DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_template.

    METHODS setup.
    METHODS split_template_in_pairs      FOR TESTING.
    METHODS build_pairs_after_3_rounds   FOR TESTING.
    METHODS count_chars_after_ten_rounds FOR TESTING.

ENDCLASS.


CLASS tc_template IMPLEMENTATION.

  METHOD setup.
    DATA(rules) = VALUE if_input=>rules( ( character_pair = 'CH' insert_char = 'B' )
                                         ( character_pair = 'HH' insert_char = 'N' )
                                         ( character_pair = 'CB' insert_char = 'H' )
                                         ( character_pair = 'NH' insert_char = 'C' )
                                         ( character_pair = 'HB' insert_char = 'C' )
                                         ( character_pair = 'HC' insert_char = 'B' )
                                         ( character_pair = 'HN' insert_char = 'C' )
                                         ( character_pair = 'NN' insert_char = 'C' )
                                         ( character_pair = 'BH' insert_char = 'H' )
                                         ( character_pair = 'NC' insert_char = 'B' )
                                         ( character_pair = 'NB' insert_char = 'B' )
                                         ( character_pair = 'BN' insert_char = 'B' )
                                         ( character_pair = 'BB' insert_char = 'N' )
                                         ( character_pair = 'BC' insert_char = 'B' )
                                         ( character_pair = 'CC' insert_char = 'N' )
                                         ( character_pair = 'CN' insert_char = 'C' ) ).
    cut = NEW template_processor( rules ).
  ENDMETHOD.

  METHOD split_template_in_pairs.
    DATA(expected_values) = VALUE if_template=>char_pairs( ( char_pair = 'NN' amount = 1 )
                                                           ( char_pair = 'NC' amount = 1 )
                                                           ( char_pair = 'CB' amount = 1 ) ).
    cut->split_template( `NNCB` ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_char_pair_table( ) ).
  ENDMETHOD.

  METHOD build_pairs_after_3_rounds.
    DATA(expected_values) = VALUE if_template=>char_pairs( ( char_pair = 'NB' amount = 4 )
                                                           ( char_pair = 'BB' amount = 4 )
                                                           ( char_pair = 'BC' amount = 3 )
                                                           ( char_pair = 'CN' amount = 2 )
                                                           ( char_pair = 'NC' amount = 1 )
                                                           ( char_pair = 'CC' amount = 1 )
                                                           ( char_pair = 'BN' amount = 2 )
                                                           ( char_pair = 'CH' amount = 2 )
                                                           ( char_pair = 'HB' amount = 3 )
                                                           ( char_pair = 'BH' amount = 1 )
                                                           ( char_pair = 'HH' amount = 1 ) ).
    cut->split_template( `NNCB` ).
    cut->process_template_times( 3 ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_char_pair_table( ) ).
  ENDMETHOD.

  METHOD count_chars_after_ten_rounds.
    DATA(expected_values) = VALUE if_template=>chars( ( char = 'B' amount = 1749 )
                                                      ( char = 'C' amount = 298 )
                                                      ( char = 'H' amount = 161 )
                                                      ( char = 'N' amount = 865 ) ).
    cut->split_template( 'NNCB' ).
    cut->process_template_times( 10 ).
    cut->count_chars( ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_char_table( ) ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text1024.

SELECT-OPTIONS so_input FOR input NO INTERVALS.
PARAMETERS: p_runs TYPE i.

START-OF-SELECTION.

  DATA(application) = NEW application( ).

  WRITE / |Ergebnis nach { p_runs } runs -> { application->run( raw_input = so_input[] runs = p_runs ) }|.
