REPORT ymbh_aoc_2021_day7.


INTERFACE if_position.
  TYPES: BEGIN OF origin_position,
           position       TYPE i,
           fuel_to_target TYPE i,
         END OF origin_position.
  TYPES origin_positions TYPE STANDARD TABLE OF origin_position WITH EMPTY KEY.
  TYPES raw_positions TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  TYPES distinct_positions TYPE SORTED TABLE OF i WITH UNIQUE KEY primary_key COMPONENTS table_line.
ENDINTERFACE.

CLASS positioning DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS order_positions.

    METHODS get_possible_positions         RETURNING VALUE(result) TYPE if_position=>distinct_positions.

    METHODS constructor                    IMPORTING raw_input TYPE if_position=>raw_positions.

    METHODS calculate_fuel_for             IMPORTING position      TYPE i
                                           RETURNING VALUE(result) TYPE if_position=>origin_positions.

    METHODS calculate_real_fuel_for        IMPORTING position      TYPE i
                                           RETURNING VALUE(result) TYPE if_position=>origin_positions.

    METHODS get_total_fuel_for             IMPORTING position      TYPE i
                                           RETURNING VALUE(result) TYPE i.

    METHODS get_real_fuel_for              IMPORTING position      TYPE i
                                           RETURNING VALUE(result) TYPE i.

    METHODS get_position_with_minimal_fuel RETURNING VALUE(result) TYPE if_position=>origin_position.

    METHODS get_position_with_real_fuel    RETURNING VALUE(result) TYPE if_position=>origin_position.

  PRIVATE SECTION.
    DATA raw_positions TYPE if_position=>raw_positions.
    DATA possible_positions TYPE if_position=>distinct_positions.
    DATA max_position TYPE i.

    METHODS calculate_fuel      IMPORTING target          TYPE i
                                          actual_position TYPE i
                                RETURNING VALUE(result)   TYPE i.

    METHODS calculate_real_fuel IMPORTING target          TYPE i
                                          actual_position TYPE i
                                RETURNING VALUE(result)   TYPE i.

ENDCLASS.

CLASS positioning IMPLEMENTATION.

  METHOD constructor.
    raw_positions = raw_input.
  ENDMETHOD.

  METHOD order_positions.
    DATA temp_positions TYPE if_position=>distinct_positions.
    LOOP AT raw_positions ASSIGNING FIELD-SYMBOL(<line>).
      TRY.
          INSERT <line> INTO TABLE temp_positions.
        CATCH cx_sy_itab_duplicate_key.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
    max_position = temp_positions[ lines( temp_positions ) ].

    possible_positions = VALUE #( FOR i = 0 THEN i + 1 UNTIL i > max_position
                                    ( i )  ).
  ENDMETHOD.

  METHOD get_possible_positions.
    result = possible_positions.
  ENDMETHOD.

  METHOD calculate_fuel_for.
    result = VALUE #( FOR line IN raw_positions
                        LET fuel = calculate_fuel( target = position actual_position = line )
                        IN
                        (  position = line fuel_to_target = fuel ) ).
  ENDMETHOD.

  METHOD calculate_real_fuel_for.
    result = VALUE #( FOR line IN raw_positions
                        LET fuel = calculate_real_fuel( target = position actual_position = line )
                        IN
                        (  position = line fuel_to_target = fuel ) ).
  ENDMETHOD.

  METHOD calculate_fuel.
    result = abs( actual_position - target ).
  ENDMETHOD.

  METHOD calculate_real_fuel.
    DATA fuel_for_step TYPE i.
    DATA(steps) = abs( actual_position - target ).
    DO steps TIMES.
      fuel_for_step = fuel_for_step + 1.
      result = result + fuel_for_step.
    ENDDO.
  ENDMETHOD.

  METHOD get_total_fuel_for.
    DATA(fuel_table) = calculate_fuel_for( position ).
    result = REDUCE #( INIT sum = 0
                       FOR line IN fuel_table
                       NEXT sum = sum + line-fuel_to_target ).
  ENDMETHOD.

  METHOD get_real_fuel_for.
    DATA(fuel_table) = calculate_real_fuel_for( position ).
    result = REDUCE #( INIT sum = 0
                      FOR line IN fuel_table
                      NEXT sum = sum + line-fuel_to_target ).
  ENDMETHOD.

  METHOD get_position_with_minimal_fuel.
    DATA minimum_fuel TYPE if_position=>origin_position.
    minimum_fuel = VALUE #( position = 0  fuel_to_target = 99999999 ).
    LOOP AT possible_positions ASSIGNING FIELD-SYMBOL(<line>).

      DATA(fuel) = get_total_fuel_for( <line> ).
      IF fuel < minimum_fuel-fuel_to_target.
        minimum_fuel = VALUE #( position = <line> fuel_to_target = fuel ).
      ENDIF.
    ENDLOOP.
    result = minimum_fuel.
  ENDMETHOD.


  METHOD get_position_with_real_fuel.
    DATA minimum_fuel TYPE if_position=>origin_position.
    minimum_fuel = VALUE #( position = 0  fuel_to_target = 99999999 ).
    LOOP AT possible_positions ASSIGNING FIELD-SYMBOL(<line>).

      DATA(fuel) = get_real_fuel_for( <line> ).
      IF fuel < minimum_fuel-fuel_to_target.
        minimum_fuel = VALUE #( position = <line> fuel_to_target = fuel ).
      ENDIF.
    ENDLOOP.
    result = minimum_fuel.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE string.

    METHODS run_part_1 RETURNING VALUE(result) TYPE if_position=>origin_position.

    METHODS run_part_2 RETURNING VALUE(result) TYPE if_position=>origin_position.

  PRIVATE SECTION.
    DATA input TYPE string.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD run_part_1.
    SPLIT input AT ',' INTO TABLE DATA(raw_input_tab).
    DATA(raw_input) = VALUE if_position=>raw_positions( FOR line IN raw_input_tab
                                                                ( CONV i( line )  ) ).
    DATA(positions) = NEW positioning( raw_input ).
    positions->order_positions( ).
    result = positions->get_position_with_minimal_fuel( ).
  ENDMETHOD.

  METHOD run_part_2.
    SPLIT input AT ',' INTO TABLE DATA(raw_input_tab).
    DATA(raw_input) = VALUE if_position=>raw_positions( FOR line IN raw_input_tab
                                                                ( CONV i( line )  ) ).
    DATA(positions) = NEW positioning( raw_input ).
    positions->order_positions( ).
    result = positions->get_position_with_real_fuel( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_positions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO positioning.

    METHODS setup.
    METHODS get_raw_input RETURNING VALUE(result) TYPE if_position=>raw_positions.

    METHODS get_possible_positions        FOR TESTING.
    METHODS calculate_fuel_for_position_1 FOR TESTING.
    METHODS get_total_fuel_for_position_1 FOR TESTING.
    METHODS get_position_with_minimal_fuel FOR TESTING.
    METHODS get_position_for_real_fuel FOR TESTING.

ENDCLASS.


CLASS tc_positions IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( get_raw_input( ) ).
    cut->order_positions( ).
  ENDMETHOD.

  METHOD get_raw_input.
    result = VALUE #( ( 16 ) ( 1 ) ( 2 ) ( 0 ) ( 4 ) ( 2 ) ( 7 ) ( 1 ) ( 2 ) ( 14 ) ).
  ENDMETHOD.

  METHOD get_possible_positions.
    DATA(expected_values) = VALUE if_position=>distinct_positions( ( 0 ) ( 1 )  ( 2 )  ( 3 )  ( 4 )  ( 5 )  ( 6 )  ( 7 )
                                                                   ( 8 ) ( 9 ) ( 10 ) ( 11 ) ( 12 ) ( 13 ) ( 14 ) ( 15 ) ( 16 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_possible_positions( ) ).
  ENDMETHOD.

  METHOD calculate_fuel_for_position_1.
    DATA(expected_values) = VALUE if_position=>origin_positions( ( position = 16 fuel_to_target = 15 )
                                                                 ( position =  1 fuel_to_target =  0 )
                                                                 ( position =  2 fuel_to_target =  1 )
                                                                 ( position =  0 fuel_to_target =  1 )
                                                                 ( position =  4 fuel_to_target =  3 )
                                                                 ( position =  2 fuel_to_target =  1 )
                                                                 ( position =  7 fuel_to_target =  6 )
                                                                 ( position =  1 fuel_to_target =  0 )
                                                                 ( position =  2 fuel_to_target =  1 )
                                                                 ( position = 14 fuel_to_target = 13 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->calculate_fuel_for( 1 ) ).
  ENDMETHOD.

  METHOD get_total_fuel_for_position_1.
    cl_abap_unit_assert=>assert_equals(
        exp = 41
        act = cut->get_total_fuel_for( 1 ) ).
  ENDMETHOD.

  METHOD get_position_with_minimal_fuel.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE if_position=>origin_position( position = 2 fuel_to_target = 37 )
        act = cut->get_position_with_minimal_fuel( ) ).
  ENDMETHOD.

  METHOD get_position_for_real_fuel.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE if_position=>origin_position( position = 5 fuel_to_target = 168 )
        act = cut->get_position_with_real_fuel( ) ).
  ENDMETHOD.

ENDCLASS.

PARAMETERS: input TYPE string.

START-OF-SELECTION.

  DATA(application) = NEW application( input ).
  DATA(result_part_1) = application->run_part_1( ).
  DATA(result_part_2) = application->run_part_2( ).

  WRITE / |Lösung Teil 1: Position: { result_part_1-position } - Fuel: { result_part_1-fuel_to_target }|.
  WRITE / |Lösung Teil 2: Position: { result_part_2-position } - Fuel: { result_part_2-fuel_to_target }|.
