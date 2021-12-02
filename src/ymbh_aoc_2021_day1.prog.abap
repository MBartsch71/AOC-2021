REPORT ymbh_aoc_2021_day1.

INTERFACE if_level_meter.
  TYPES: BEGIN OF reading_with_result,
           reading TYPE i,
           result  TYPE char1,
         END OF reading_with_result.
  TYPES readings_with_result TYPE STANDARD TABLE OF reading_with_result WITH EMPTY KEY.
  TYPES readings             TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  METHODS process_level_measures IMPORTING readings      TYPE if_level_meter=>readings
                                 RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

CLASS level_meter DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_level_meter.

  PRIVATE SECTION.
    CONSTANTS c_increasing_value TYPE char1 VALUE 'I'.
    CONSTANTS c_decreasing_value TYPE char1 VALUE 'D'.
    CONSTANTS c_equal_value      TYPE char1 VALUE 'E'.

    METHODS get_relation                  IMPORTING previous_line TYPE i
                                                    current_line  TYPE i
                                          RETURNING VALUE(result) TYPE char1.

    METHODS classify_readings             IMPORTING readings                    TYPE if_level_meter=>readings
                                          RETURNING VALUE(readings_with_result) TYPE if_level_meter=>readings_with_result.

    METHODS count_increasing_values       IMPORTING readings_with_result TYPE if_level_meter=>readings_with_result
                                          RETURNING VALUE(result)        TYPE i.

    METHODS get_relation_to_previous_line IMPORTING index         TYPE i
                                                    table         TYPE if_level_meter=>readings
                                          RETURNING VALUE(result) TYPE char1.

    METHODS first_line                    IMPORTING index         TYPE i
                                          RETURNING VALUE(result) TYPE abap_bool.

    METHODS increase_amount               IMPORTING reading       TYPE if_level_meter=>reading_with_result
                                          RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS level_meter IMPLEMENTATION.

  METHOD if_level_meter~process_level_measures.
    DATA(readings_with_result) = classify_readings( readings ).
    result = count_increasing_values( readings_with_result ).
  ENDMETHOD.

  METHOD classify_readings.
    readings_with_result = VALUE #( FOR line IN readings
                                    INDEX INTO index
                                    LET result = get_relation_to_previous_line( index = index
                                                                                table = readings )
                                    IN ( reading = line
                                         result  = result ) ).
  ENDMETHOD.

  METHOD count_increasing_values.
    result = REDUCE #( INIT sum = 0
                       FOR  reading IN readings_with_result
                       NEXT sum = sum + increase_amount( reading ) ).
  ENDMETHOD.

  METHOD get_relation_to_previous_line.
    IF first_line( index ).
      RETURN.
    ENDIF.

    result = get_relation( previous_line = table[ index - 1 ]
                           current_line  = table[ index ] ).
  ENDMETHOD.

  METHOD get_relation.
    result = COND #( WHEN current_line > previous_line THEN c_increasing_value
                     WHEN current_line < previous_line THEN c_decreasing_value
                     ELSE c_equal_value ).
  ENDMETHOD.

  METHOD first_line.
    result = xsdbool( index = 1 ).
  ENDMETHOD.

  METHOD increase_amount.
    result = SWITCH #( reading-result WHEN c_increasing_value THEN 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS level_meter_advanced DEFINITION INHERITING FROM level_meter.

  PUBLIC SECTION.
    METHODS if_level_meter~process_level_measures REDEFINITION.

  PRIVATE SECTION.
    METHODS sum_up_three_values IMPORTING readings      TYPE if_level_meter=>readings
                                RETURNING VALUE(result) TYPE if_level_meter=>readings.

    METHODS sum_three_lines     IMPORTING index         TYPE i
                                          table         TYPE if_level_meter=>readings
                                RETURNING VALUE(result) TYPE i.

    METHODS proceed             IMPORTING index         TYPE i
                                          length        TYPE i
                                RETURNING VALUE(result) TYPE abap_bool.
    METHODS collect_three_lines IMPORTING index         TYPE i
                                          readings      TYPE if_level_meter=>readings
                                RETURNING VALUE(result) TYPE i.

    METHODS delete_empty_lines  IMPORTING input         TYPE if_level_meter=>readings
                                RETURNING VALUE(result) TYPE if_level_meter=>readings.
ENDCLASS.

CLASS level_meter_advanced IMPLEMENTATION.

  METHOD if_level_meter~process_level_measures.
    DATA(three_sum_values) = sum_up_three_values( readings ).
    result = super->if_level_meter~process_level_measures( three_sum_values ).
  ENDMETHOD.

  METHOD sum_up_three_values.
    result = VALUE #( FOR line IN readings
                      INDEX INTO index
                      LET sum = collect_three_lines( index    = index
                                                     readings = readings )
                      IN ( sum ) ).
    result = delete_empty_lines( result ).
  ENDMETHOD.

  METHOD collect_three_lines.
    IF proceed( index = index length = lines( readings ) ).
      result = sum_three_lines( index = index table = readings ).
    ENDIF.
  ENDMETHOD.

  METHOD delete_empty_lines.
    result = input.
    DELETE result WHERE table_line = 0.
  ENDMETHOD.

  METHOD proceed.
    result = xsdbool( index <= length - 2 ).
  ENDMETHOD.

  METHOD sum_three_lines.
    result = table[ index ] + table[ index + 1 ] + table[ index + 2 ].
  ENDMETHOD.

ENDCLASS.

CLASS level_meter_factory DEFINITION.

  PUBLIC SECTION.
    METHODS get_level_meter_for_part IMPORTING part          TYPE i
                                     RETURNING VALUE(result) TYPE REF TO if_level_meter.
ENDCLASS.

CLASS level_meter_factory IMPLEMENTATION.

  METHOD get_level_meter_for_part.
    result = SWITCH #( part WHEN 1 THEN NEW level_meter( )
                            WHEN 2 THEN NEW level_meter_advanced( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.

  PUBLIC SECTION.
    TYPES input TYPE RANGE OF i.

    METHODS constructor IMPORTING factory TYPE REF TO level_meter_factory.

    METHODS run         IMPORTING input         TYPE input
                                  part          TYPE i
                        RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA level_meter_factory TYPE REF TO level_meter_factory.

    METHODS convert_input_values IMPORTING input         TYPE application=>input
                                 RETURNING VALUE(result) TYPE if_level_meter=>readings.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    level_meter_factory = factory.
  ENDMETHOD.

  METHOD run.
    DATA(readings)    = convert_input_values( input ).
    DATA(level_meter) = level_meter_factory->get_level_meter_for_part( part ).
    result            = level_meter->process_level_measures( readings ).
  ENDMETHOD.

  METHOD convert_input_values.
    result = VALUE #( FOR line IN input ( line-low ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_level_count DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO level_meter.

    METHODS count_increasing_levels        FOR TESTING.
    METHODS count_increasing_levels_window FOR TESTING.

ENDCLASS.

CLASS tc_level_count IMPLEMENTATION.

  METHOD count_increasing_levels.
    cut = NEW level_meter( ).
    DATA(readings) = VALUE if_level_meter=>readings( ( 199 ) ( 200 ) ( 208 ) ( 210 )
                                                     ( 200 ) ( 207 ) ( 240 ) ( 269 )
                                                     ( 260 ) ( 263 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = cut->if_level_meter~process_level_measures( readings ) ).
  ENDMETHOD.

  METHOD count_increasing_levels_window.
    cut = NEW level_meter_advanced( ).
    DATA(readings) = VALUE if_level_meter=>readings( ( 199 ) ( 200 ) ( 208 ) ( 210 )
                                                     ( 200 ) ( 207 ) ( 240 ) ( 269 )
                                                     ( 260 ) ( 263 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = cut->if_level_meter~process_level_measures( readings ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_level_meter_factory DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_class_for_first_part  FOR TESTING.
    METHODS get_class_for_second_part FOR TESTING.

ENDCLASS.

CLASS tc_level_meter_factory IMPLEMENTATION.

  METHOD get_class_for_first_part.
    DATA(level_meter) = NEW level_meter_factory( )->get_level_meter_for_part( 1 ).
    DATA(result) = xsdbool( level_meter IS INSTANCE OF level_meter ).
    cl_abap_unit_assert=>assert_true(
        act = result ).
  ENDMETHOD.

  METHOD get_class_for_second_part.
    DATA(level_meter) = NEW level_meter_factory( )->get_level_meter_for_part( 2 ).
    DATA(result) = xsdbool( level_meter IS INSTANCE OF level_meter_advanced ).

    cl_abap_unit_assert=>assert_true( act = result ).
  ENDMETHOD.

ENDCLASS.

CLASS td_level_meter_mock DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_level_meter.
ENDCLASS.

CLASS td_level_meter_mock IMPLEMENTATION.

  METHOD if_level_meter~process_level_measures.
    result = 5.
  ENDMETHOD.

ENDCLASS.

CLASS td_level_meter_factory_mock DEFINITION INHERITING FROM level_meter_factory.

  PUBLIC SECTION.
    METHODS get_level_meter_for_part REDEFINITION.

ENDCLASS.

CLASS td_level_meter_factory_mock IMPLEMENTATION.

  METHOD get_level_meter_for_part.
    result = NEW td_level_meter_mock( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.
    METHODS setup.

    METHODS part_one FOR TESTING.
ENDCLASS.

CLASS tc_application IMPLEMENTATION.

  METHOD part_one.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = cut->run( input = VALUE #( (  ) ) part = 1 ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( NEW td_level_meter_factory_mock( ) ).
  ENDMETHOD.

ENDCLASS.

DATA value TYPE i.
SELECT-OPTIONS: so_input FOR value NO INTERVALS.

START-OF-SELECTION.

  DATA(lo_application) = NEW application( NEW level_meter_factory( ) ).
  WRITE / |Lösung erster Teil:  { lo_application->run( input = so_input[] part = 1 ) }|.
  WRITE / |Lösung zweiter Teil: { lo_application->run( input = so_input[] part = 2 ) }|.
