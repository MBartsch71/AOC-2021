REPORT ymbh_aoc_2021_day1.

CLASS lcl_level_meter DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_readings_with_result,
             reading TYPE i,
             result  TYPE char1,
           END OF ts_readings_with_result.
    TYPES tt_readings_with_result TYPE STANDARD TABLE OF ts_readings_with_result WITH EMPTY KEY.
    TYPES tt_readings TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS process_level_measures    IMPORTING readings         TYPE tt_readings
                                      RETURNING VALUE(rv_result) TYPE i.

    METHODS process_3_measure_windows IMPORTING readings      TYPE lcl_level_meter=>tt_readings
                                      RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    METHODS get_relation            IMPORTING previous_line    TYPE i
                                              current_line     TYPE i
                                    RETURNING VALUE(rv_result) TYPE char1.

    METHODS classify_readings       IMPORTING i_readings                     TYPE lcl_level_meter=>tt_readings
                                    RETURNING VALUE(rt_readings_with_result) TYPE lcl_level_meter=>tt_readings_with_result.

    METHODS count_increasing_values IMPORTING it_readings_with_result TYPE lcl_level_meter=>tt_readings_with_result
                                    RETURNING VALUE(rv_result)        TYPE i.

    METHODS proceed                 IMPORTING index         TYPE i
                                              length        TYPE i

                                    RETURNING VALUE(result) TYPE abap_bool.
    METHODS sum_three_lines         IMPORTING index         TYPE i
                                              table         TYPE lcl_level_meter=>tt_readings
                                    RETURNING VALUE(result) TYPE i.

    METHODS sum_up_three_values     IMPORTING readings      TYPE lcl_level_meter=>tt_readings
                                    RETURNING VALUE(result) TYPE lcl_level_meter=>tt_readings.

ENDCLASS.

CLASS lcl_level_meter IMPLEMENTATION.

  METHOD process_level_measures.
    DATA(lt_readings_with_result) = classify_readings( readings ).
    rv_result = count_increasing_values( lt_readings_with_result ).
  ENDMETHOD.

  METHOD count_increasing_values.
    rv_result = REDUCE #( INIT sum = 0
                          FOR reading IN it_readings_with_result
                          NEXT sum = COND #( WHEN reading-result = |I| THEN sum + 1
                                             ELSE sum ) ).
  ENDMETHOD.

  METHOD classify_readings.
    rt_readings_with_result  =
          VALUE tt_readings_with_result( FOR line IN i_readings
                                         INDEX INTO index
                                         LET result = COND char1( WHEN index > 1
                                                                  THEN get_relation( previous_line = i_readings[ index - 1 ]
                                                                                     current_line  = i_readings[ index ] )
                                                                  ELSE space )
                                         IN (  reading = line
                                               result = result ) ).
  ENDMETHOD.

  METHOD get_relation.
    rv_result = COND #( WHEN current_line > previous_line THEN |I|
                        WHEN current_line < previous_line THEN |D|
                        ELSE |E| ).
  ENDMETHOD.

  METHOD process_3_measure_windows.
    DATA(three_sum_values) = sum_up_three_values( readings ).
    DATA(classified_readings) = classify_readings( three_sum_values ).
    result = count_increasing_values( classified_readings ).
  ENDMETHOD.

  METHOD sum_up_three_values.
    result = VALUE #( FOR line IN readings
                        INDEX INTO index
                        LET sum = COND #( WHEN proceed( index  = index
                                                        length = lines( readings ) ) = abap_true
                                          THEN sum_three_lines( index = index
                                                                table = readings ) ) IN
                                           ( sum ) ).
    DELETE result WHERE table_line = 0.
  ENDMETHOD.


  METHOD proceed.
    result = xsdbool( index <= length - 2 ).
  ENDMETHOD.


  METHOD sum_three_lines.
    result = table[ index ] + table[ index + 1 ] + table[ index + 2 ].
  ENDMETHOD.

ENDCLASS.


CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    TYPES tt_input TYPE RANGE OF i.

    DATA mt_converted_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    METHODS run_first_part IMPORTING input         TYPE tt_input
                           RETURNING VALUE(result) TYPE i.

    METHODS run_second_part IMPORTING input         TYPE tt_input
                            RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD run_first_part.
    mt_converted_values = VALUE #( FOR line IN input
                                    ( line-low ) ).
    DATA(lo_level_meter) = NEW lcl_level_meter( ).
    result = lo_level_meter->process_level_measures( mt_converted_values ).
  ENDMETHOD.

  METHOD run_second_part.
    mt_converted_values = VALUE #( FOR line IN input
                                    ( line-low ) ).
    DATA(lo_level_meter) = NEW lcl_level_meter( ).
    result = lo_level_meter->process_3_measure_windows( mt_converted_values ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_level_count DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_level_meter.

    METHODS count_increasing_levels        FOR TESTING.
    METHODS count_increasing_levels_window FOR TESTING.

ENDCLASS.


CLASS ltc_level_count IMPLEMENTATION.

  METHOD count_increasing_levels.
    cut = NEW #( ).
    DATA(lt_readings) = VALUE lcl_level_meter=>tt_readings( ( 199 ) ( 200 ) ( 208 ) ( 210 )
                                                            ( 200 ) ( 207 ) ( 240 ) ( 269 )
                                                            ( 260 ) ( 263 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = cut->process_level_measures( lt_readings ) ).
  ENDMETHOD.

  METHOD count_increasing_levels_window.
    cut = NEW #( ).
    DATA(lt_readings) = VALUE lcl_level_meter=>tt_readings( ( 199 ) ( 200 ) ( 208 ) ( 210 )
                                                            ( 200 ) ( 207 ) ( 240 ) ( 269 )
                                                            ( 260 ) ( 263 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = cut->process_3_measure_windows( lt_readings ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_input DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_application.
    METHODS convert_input_in_value_table FOR TESTING.
ENDCLASS.


CLASS ltc_input IMPLEMENTATION.
  METHOD convert_input_in_value_table.
    TYPES tt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    cut = NEW #( ).
    cut->run_first_part( VALUE #( ( sign = 'I' option = 'EQ' low = '199' )
                       ( sign = 'I' option = 'EQ' low = '200' )
                       ( sign = 'I' option = 'EQ' low = '208' )
                       ( sign = 'I' option = 'EQ' low = '210' ) ) ).
    DATA(lt_expected_values) = VALUE tt_values( ( 199 ) ( 200 ) ( 208 ) ( 210 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = lt_expected_values
        act = cut->mt_converted_values ).
  ENDMETHOD.
ENDCLASS.

DATA value TYPE i.
SELECT-OPTIONS: so_input FOR value.

START-OF-SELECTION.

  DATA(lo_application) = NEW lcl_application( ).
  DATA(lv_result) = lo_application->run_first_part( so_input[] ).
  WRITE / |Lösung erster Teil:  { lo_application->run_first_part( so_input[] ) }|.
  WRITE / |Lösung zweiter Teil: { lo_application->run_second_part( so_input[] ) }|.
