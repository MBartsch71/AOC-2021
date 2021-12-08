REPORT ymbh_aoc_2021_day8.

INTERFACE if_input.
  TYPES: BEGIN OF input_line,
           signal_pattern TYPE string,
           output_value   TYPE string,
         END OF input_line.
  TYPES input_table TYPE STANDARD TABLE OF input_line WITH EMPTY KEY.
ENDINTERFACE.

INTERFACE if_digits.
  TYPES: BEGIN OF digits,
           digit TYPE i,
           code  TYPE string,
         END OF digits.
  TYPES digits_tab TYPE STANDARD TABLE OF digits WITH EMPTY KEY.
  TYPES: BEGIN OF replacement,
           pattern          TYPE string,
           replaced_pattern TYPE string,
         END OF replacement.
  TYPES replacements TYPE STANDARD TABLE OF replacement WITH EMPTY KEY.
ENDINTERFACE.


CLASS digit_counter DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS found_words_with_digits IMPORTING digit         TYPE i
                                              line          TYPE string
                                    RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS digit_counter IMPLEMENTATION.

  METHOD found_words_with_digits.
    DATA(regex) = '\b[a-g]{' && digit && '}\b'.
    FIND ALL OCCURRENCES OF REGEX regex IN line MATCH COUNT result.
  ENDMETHOD.

ENDCLASS.

CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING raw_input TYPE stringtab.
    METHODS get_curated_input RETURNING VALUE(result) TYPE if_input=>input_table.

  PRIVATE SECTION.
    DATA curated_input TYPE if_input=>input_table.
    METHODS curate_input IMPORTING raw_input TYPE stringtab.

ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD constructor.
    curate_input( raw_input ).
  ENDMETHOD.

  METHOD curate_input.
    DATA output_line TYPE if_input=>input_line.
    LOOP AT raw_input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT '|' INTO output_line-signal_pattern output_line-output_value.
      APPEND output_line TO curated_input.
      CLEAR output_line.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_curated_input.
    result = curated_input.
  ENDMETHOD.

ENDCLASS.


CLASS digit_parser DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING curated_input TYPE if_input=>input_table.
    METHODS find_easy_digits_in_output RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA curated_input TYPE if_input=>input_table.
    DATA digit_counter TYPE REF TO digit_counter.

    METHODS count_requested_digits IMPORTING line          TYPE string
                                   RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS digit_parser IMPLEMENTATION.

  METHOD constructor.
    me->curated_input = curated_input.
    digit_counter = NEW #( ).
  ENDMETHOD.

  METHOD find_easy_digits_in_output.
    result = REDUCE #( INIT sum = 0
                       FOR line IN curated_input
                       NEXT sum = sum + count_requested_digits( line-output_value ) ).
  ENDMETHOD.


  METHOD count_requested_digits.
    DATA(char_2_digits) = digit_counter->found_words_with_digits( digit = 2  line = line ).
    DATA(char_3_digits) = digit_counter->found_words_with_digits( digit = 3  line = line ).
    DATA(char_4_digits) = digit_counter->found_words_with_digits( digit = 4  line = line ).
    DATA(char_8_digits) = digit_counter->found_words_with_digits( digit = 7  line = line ).
    result = char_2_digits + char_3_digits + char_4_digits + char_8_digits.
  ENDMETHOD.

ENDCLASS.

CLASS digit_decoder DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get_digits IMPORTING signal_pattern TYPE string
                       RETURNING VALUE(result)  TYPE if_digits=>digits_tab.
    METHODS calculate_result IMPORTING curated_input_line TYPE if_input=>input_line-output_value
                             RETURNING VALUE(result)      TYPE i.

  PRIVATE SECTION.
    DATA digit_counter TYPE REF TO digit_counter.
    DATA digit_information TYPE if_digits=>digits_tab.
    DATA replacements TYPE if_digits=>replacements.

    METHODS find_simple_digits IMPORTING signal_pattern TYPE string.
    METHODS find_digit_3
      IMPORTING
        signal_pattern TYPE string.
    METHODS determine_remaining_5digits.
    METHODS find_digit_9.
    METHODS determine_remaining_6digits.
    METHODS lookup_value
      IMPORTING
                line          TYPE string
      RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS digit_decoder IMPLEMENTATION.

  METHOD constructor.
    digit_counter = NEW digit_counter( ).
  ENDMETHOD.

  METHOD get_digits.
    find_simple_digits( signal_pattern ).
    find_digit_3( signal_pattern ).
    determine_remaining_5digits( ).
    find_digit_9( ).
    determine_remaining_6digits( ).
    result = digit_information.
  ENDMETHOD.

  METHOD find_simple_digits.
    FIND FIRST OCCURRENCE OF REGEX '(\b[a-g]{2}\b)' IN signal_pattern RESULTS DATA(result).
    digit_information = VALUE #( BASE digit_information ( digit = 1 code = signal_pattern+result-offset(result-length) ) ).
    FIND FIRST OCCURRENCE OF REGEX '(\b[a-g]{3}\b)' IN signal_pattern RESULTS result.
    digit_information = VALUE #( BASE digit_information ( digit = 7 code = signal_pattern+result-offset(result-length) ) ).
    FIND FIRST OCCURRENCE OF REGEX '(\b[a-g]{4}\b)' IN signal_pattern RESULTS result.
    digit_information = VALUE #( BASE digit_information ( digit = 4 code = signal_pattern+result-offset(result-length) ) ).
    FIND FIRST OCCURRENCE OF REGEX '(\b[a-g]{7}\b)' IN signal_pattern RESULTS result.
    digit_information = VALUE #( BASE digit_information ( digit = 8 code = signal_pattern+result-offset(result-length) ) ).
  ENDMETHOD.

  METHOD find_digit_3.
    DATA index_3 TYPE i.
    SPLIT signal_pattern AT space INTO TABLE DATA(pattern).
    DATA(regex) = '[' && digit_information[ digit = 7 ]-code && ']'.
    LOOP AT pattern ASSIGNING FIELD-SYMBOL(<line>).
      DATA(replacement) = VALUE if_digits=>replacement( pattern = <line> ).
      REPLACE ALL OCCURRENCES OF REGEX regex IN <line> WITH space.
      replacement-replaced_pattern = <line>.
      replacements = VALUE #( BASE replacements ( replacement ) ).
    ENDLOOP.
    DELETE replacements WHERE pattern = digit_information[ digit = 1 ]-code.
    DELETE replacements WHERE pattern = digit_information[ digit = 4 ]-code.
    DELETE replacements WHERE pattern = digit_information[ digit = 7 ]-code.
    DELETE replacements WHERE pattern = digit_information[ digit = 8 ]-code.

    LOOP AT replacements ASSIGNING FIELD-SYMBOL(<dd>).
      IF strlen( <dd>-replaced_pattern ) = 2.
        index_3 = sy-tabix.
        digit_information = VALUE #( BASE digit_information ( digit = 3 code = <dd>-pattern ) ).
      ENDIF.
    ENDLOOP.
    DELETE replacements INDEX index_3.
  ENDMETHOD.

  METHOD determine_remaining_5digits.
    DATA remain_5_digits TYPE if_digits=>replacements.

    LOOP AT replacements ASSIGNING FIELD-SYMBOL(<line>).
      IF strlen( <line>-pattern )  = 5.
        APPEND <line> TO remain_5_digits.
      ENDIF.
    ENDLOOP.
    DATA(regex) = '[' && digit_information[ digit = 4 ]-code && ']'.
    LOOP AT remain_5_digits ASSIGNING FIELD-SYMBOL(<digit>).
      REPLACE ALL OCCURRENCES OF REGEX regex IN <digit>-replaced_pattern WITH space.
    ENDLOOP.
    LOOP AT remain_5_digits ASSIGNING FIELD-SYMBOL(<digit_new>).
      IF strlen( <digit_new>-replaced_pattern ) = 1.
        digit_information = VALUE #( BASE digit_information ( digit = 5 code = <digit_new>-pattern ) ).
      ENDIF.
      IF strlen( <digit_new>-replaced_pattern ) = 2.
        digit_information = VALUE #( BASE digit_information ( digit = 2 code = <digit_new>-pattern ) ).
      ENDIF.
    ENDLOOP.
    DELETE replacements WHERE pattern = digit_information[ digit = 5 ]-code.
    DELETE replacements WHERE pattern = digit_information[ digit = 2 ]-code.
  ENDMETHOD.

  METHOD find_digit_9.
    DATA(regex) = '[' && digit_information[ digit = 5 ]-code && ']'.
    LOOP AT replacements ASSIGNING FIELD-SYMBOL(<line>).
      REPLACE ALL OCCURRENCES OF REGEX regex IN <line>-replaced_pattern WITH space.
      IF strlen( <line>-replaced_pattern ) = 0.
        digit_information = VALUE #( BASE digit_information
                                        ( digit = 9 code = <line>-pattern ) ).
      ENDIF.
    ENDLOOP.
    DELETE replacements WHERE pattern = digit_information[ digit = 9 ]-code.
  ENDMETHOD.

  METHOD determine_remaining_6digits.
    DATA(regex) = '[' && digit_information[ digit = 5 ]-code && ']'.
    DATA(replacements_new) = VALUE if_digits=>replacements( FOR line IN replacements
                               ( pattern = line-pattern
                               replaced_pattern = line-pattern ) ).
    replacements = replacements_new.
    LOOP AT replacements ASSIGNING FIELD-SYMBOL(<line>).
      REPLACE ALL  OCCURRENCES OF REGEX regex IN <line>-replaced_pattern WITH space.
    ENDLOOP.
    LOOP AT replacements ASSIGNING FIELD-SYMBOL(<digit>).
      IF strlen( <digit>-replaced_pattern ) = 1.
        digit_information = VALUE #( BASE digit_information ( digit = 6 code = <digit>-pattern ) ).
      ENDIF.
      IF strlen( <digit>-replaced_pattern ) = 2.
        digit_information = VALUE #( BASE digit_information ( digit = 0 code = <digit>-pattern ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD calculate_result.
    SPLIT curated_input_line AT space INTO TABLE DATA(digits).
    result = REDUCE #( INIT number = 0
                       FOR line IN digits
                       NEXT number = number && lookup_value( line ) ).

  ENDMETHOD.


  METHOD lookup_value.
    IF line IS NOT INITIAL.
      DATA(length) = strlen( line ).
      DATA(regex) = '\b[' && line && ']{' && length && '}\b'.
      LOOP AT digit_information ASSIGNING FIELD-SYMBOL(<line>).
        FIND FIRST OCCURRENCE OF REGEX regex IN <line>-code.
        IF sy-subrc = 0.
          result = <line>-digit.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    TYPES raw_input TYPE RANGE OF text1024.
    METHODS run_1st_part IMPORTING raw_input     TYPE raw_input
                         RETURNING VALUE(result) TYPE i.
    METHODS run_2nd_part IMPORTING raw_input     TYPE raw_input
                         RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run_1st_part.
    DATA(input) = VALUE stringtab( FOR line IN raw_input
                                   ( CONV #( line-low ) ) ).
    DATA(curated_input) = NEW input_processor( input )->get_curated_input( ).
    result = NEW digit_parser( curated_input )->find_easy_digits_in_output( ).
  ENDMETHOD.


  METHOD run_2nd_part.
    DATA(input) = VALUE stringtab( FOR line IN raw_input
                                    ( CONV #( line-low ) ) ).
    DATA(curated_input) = NEW input_processor( input )->get_curated_input( ).
    LOOP AT curated_input ASSIGNING FIELD-SYMBOL(<line>).
      DATA(digit_decoder) = NEW digit_decoder( ).
      DATA(digits) = digit_decoder->get_digits( <line>-signal_pattern ).
      DATA(temp_result) = digit_decoder->calculate_result( <line>-output_value ).
      result = result + temp_result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS tc_digit_counter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO digit_counter.
    METHODS setup.
    METHODS find_3_count_digits_in_a_line FOR TESTING.
ENDCLASS.


CLASS tc_digit_counter IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD find_3_count_digits_in_a_line.
    DATA(line) = |gbdfcae bgc cg cgb|.
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->found_words_with_digits( digit = 3 line = line ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_input_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO input_processor.

    METHODS setup.
    METHODS build_input_table FOR TESTING.
ENDCLASS.


CLASS tc_input_processor IMPLEMENTATION.

  METHOD build_input_table.
    DATA(expected_values) = VALUE if_input=>input_table( ( signal_pattern = |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | output_value = | fdgacbe cefdb cefbgd gcbe| )
                                                         ( signal_pattern = |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | output_value = | fcgedb cgb dgebacf gc| )
                                                         ( signal_pattern = |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | output_value = | cg cg fdcagb cbg| )
                                                         ( signal_pattern = |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | output_value = | efabcd cedba gadfec cb| )
                                                         ( signal_pattern = |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | output_value = | gecf egdcabf bgf bfgea| )
                                                         ( signal_pattern = |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | output_value = | gebdcfa ecba ca fadegcb| )
                                                         ( signal_pattern = |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | output_value = | cefg dcbef fcge gbcadfe| )
                                                         ( signal_pattern = |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | output_value = | ed bcgafe cdgba cbgef| )
                                                         ( signal_pattern = |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | output_value = | gbdfcae bgc cg cgb| )
                                                         ( signal_pattern = |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | output_value = | fgae cfgab fg bagce| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_curated_input( ) ).
  ENDMETHOD.

  METHOD setup.
    DATA(raw_input) = VALUE stringtab( ( `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe` )
                                       ( `edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc` )
                                       ( `fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg` )
                                       ( `fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb` )
                                       ( `aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea` )
                                       ( `fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb` )
                                       ( `dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe` )
                                       ( `bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef` )
                                       ( `egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb` )
                                       ( `gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce` ) ).
    cut = NEW #( raw_input ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_digit_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO digit_parser.

    METHODS setup.
    METHODS build_curated_input RETURNING VALUE(result) TYPE if_input=>input_table.

    METHODS find_easy_digit_count FOR TESTING.
ENDCLASS.


CLASS tc_digit_parser IMPLEMENTATION.

  METHOD find_easy_digit_count.
    cl_abap_unit_assert=>assert_equals(
        exp = 26
        act = cut->find_easy_digits_in_output( ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( build_curated_input( ) ).
  ENDMETHOD.

  METHOD build_curated_input.
    result = VALUE #( ( signal_pattern = |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | output_value = | fdgacbe cefdb cefbgd gcbe| )
                      ( signal_pattern = |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | output_value = | fcgedb cgb dgebacf gc| )
                      ( signal_pattern = |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | output_value = | cg cg fdcagb cbg| )
                      ( signal_pattern = |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | output_value = | efabcd cedba gadfec cb| )
                      ( signal_pattern = |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | output_value = | gecf egdcabf bgf bfgea| )
                      ( signal_pattern = |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | output_value = | gebdcfa ecba ca fadegcb| )
                      ( signal_pattern = |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | output_value = | cefg dcbef fcge gbcadfe| )
                      ( signal_pattern = |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | output_value = | ed bcgafe cdgba cbgef| )
                      ( signal_pattern = |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | output_value = | gbdfcae bgc cg cgb| )
                      ( signal_pattern = |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | output_value = | fgae cfgab fg bagce| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_digit_decoder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO digit_decoder.

    METHODS setup.
    METHODS find_unique_numbers_of_string FOR TESTING.
    METHODS calculate_result_test_1       FOR TESTING.
    METHODS calculate_result_test_2       FOR TESTING.

ENDCLASS.


CLASS tc_digit_decoder IMPLEMENTATION.

  METHOD setup.
    cut =  NEW #( ).
  ENDMETHOD.

  METHOD find_unique_numbers_of_string.
    DATA(expected_values) = VALUE if_digits=>digits_tab( ( digit = 1 code = |ab| )
                                                         ( digit = 7 code = |dab| )
                                                         ( digit = 4 code = |eafb| )
                                                         ( digit = 8 code = |acedgfb| )
                                                         ( digit = 3 code = |fbcad| )
                                                         ( digit = 5 code = |cdfbe| )
                                                         ( digit = 2 code = |gcdfa| )
                                                         ( digit = 9 code = |cefabd| )
                                                         ( digit = 6 code = |cdfgeb| )
                                                         ( digit = 0 code = |cagedb| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_digits( |acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab| ) ).
  ENDMETHOD.

  METHOD calculate_result_test_1.
    DATA(curated_input) = VALUE if_input=>input_line( signal_pattern = |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | output_value = | fdgacbe cefdb cefbgd gcbe| ).
    cut->get_digits( curated_input-signal_pattern ).
    cl_abap_unit_assert=>assert_equals(
        exp = 8394
        act = cut->calculate_result( curated_input-output_value ) ).
  ENDMETHOD.

  METHOD calculate_result_test_2.
    DATA(curated_input) = VALUE if_input=>input_line( signal_pattern = |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | output_value = | fcgedb cgb dgebacf gc| ).
    cut->get_digits( curated_input-signal_pattern ).
    cl_abap_unit_assert=>assert_equals(
        exp = 9781
        act = cut->calculate_result( curated_input-output_value ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: raw_input TYPE RANGE OF text1024.
    DATA cut TYPE REF TO application.

    METHODS setup.
    METHODS get_raw_input RETURNING VALUE(result) TYPE raw_input.
    METHODS calculate_final_result FOR TESTING.

ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD calculate_final_result.
    cl_abap_unit_assert=>assert_equals(
        exp = 61229
        act = cut->run_2nd_part( get_raw_input( ) ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_raw_input.
    result = VALUE #( ( sign = 'I' option = 'EQ' low = 'be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe' )
                      ( sign = 'I' option = 'EQ' low = 'edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc' )
                      ( sign = 'I' option = 'EQ' low = 'fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg' )
                      ( sign = 'I' option = 'EQ' low = 'fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb' )
                      ( sign = 'I' option = 'EQ' low = 'aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea' )
                      ( sign = 'I' option = 'EQ' low = 'fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb' )
                      ( sign = 'I' option = 'EQ' low = 'dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe' )
                      ( sign = 'I' option = 'EQ' low = 'bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef' )
                      ( sign = 'I' option = 'EQ' low = 'egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb' )
                      ( sign = 'I' option = 'EQ' low = 'gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce ' ) ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE text1024.

SELECT-OPTIONS: so_input FOR input.

START-OF-SELECTION.

  DATA(application) = NEW application( ).

  WRITE / |Ergebnis Teil 1: { application->run_1st_part( so_input[] ) }|.
  WRITE / |Ergebnis Teil 2: { application->run_2nd_part( so_input[] ) }|.
