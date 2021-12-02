REPORT ymbh_aoc_2021_day2.



INTERFACE if_dive_meter.
  TYPES: BEGIN OF curated_input,
           direction TYPE c LENGTH 7,
           amount    TYPE i,
         END OF curated_input.
  TYPES curated_inputs TYPE STANDARD TABLE OF curated_input WITH EMPTY KEY.
  TYPES raw_inputs TYPE RANGE OF text100.
ENDINTERFACE.

CLASS dive_calculator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS calculate_position IMPORTING curated_input TYPE if_dive_meter=>curated_inputs
                               RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.

ENDCLASS.

CLASS dive_calculator IMPLEMENTATION.

  METHOD calculate_position.
    DATA forward_sum TYPE i.
    DATA depth_sum TYPE i.
    data aim_sum type i.

    LOOP AT curated_input ASSIGNING FIELD-SYMBOL(<line>).
      IF <line>-direction = |forward|.
        forward_sum = forward_sum + <line>-amount.
        depth_sum = depth_sum + ( aim_sum * <line>-amount ).
      ENDIF.
      IF <line>-direction = |down|.
        aim_sum = aim_sum + <line>-amount.
      ENDIF.
      IF <line>-direction = |up|.
        aim_sum = aim_sum - <line>-amount.
      ENDIF.
    ENDLOOP.

    result = forward_sum * depth_sum.
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run IMPORTING raw_input     TYPE if_dive_meter=>raw_inputs
                RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    METHODS split_raw_input IMPORTING line          TYPE text100
                            RETURNING VALUE(result) TYPE if_dive_meter=>curated_input.
    METHODS prepare_input   IMPORTING raw_input     TYPE if_dive_meter=>raw_inputs
                            RETURNING VALUE(result) TYPE if_dive_meter=>curated_inputs.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD prepare_input.
    result = VALUE #( FOR line IN raw_input
                        LET curated_line = split_raw_input( line-low )
                        IN
                        ( curated_line ) ).
  ENDMETHOD.

  METHOD split_raw_input.
    DATA c_amount TYPE string.
    SPLIT line AT space INTO result-direction c_amount.
    result-amount = CONV i( c_amount ).
  ENDMETHOD.

  METHOD run.
    DATA(curated_input) = prepare_input( raw_input ).
    result = NEW dive_calculator( )->calculate_position( curated_input ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO application.
    METHODS prepare_input FOR TESTING.

ENDCLASS.


CLASS tc_application IMPLEMENTATION.

  METHOD prepare_input.
    cut = NEW #( ).
    DATA(raw_input) = VALUE if_dive_meter=>raw_inputs( ( sign = |I| option = |EQ| low = |forward 5| )
                                                       ( sign = |I| option = |EQ| low = |down 5| )
                                                       ( sign = |I| option = |EQ| low = |forward 8| )
                                                       ( sign = |I| option = |EQ| low = |up 3| )
                                                       ( sign = |I| option = |EQ| low = |down 8| )
                                                       ( sign = |I| option = |EQ| low = |forward 2| ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = 900
        act = cut->run( raw_input ) ).

  ENDMETHOD.

ENDCLASS.

CLASS tc_calculate_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO dive_calculator.
    METHODS calculate_test_position FOR TESTING.

ENDCLASS.

CLASS tc_calculate_position IMPLEMENTATION.

  METHOD calculate_test_position.
    cut =  NEW #( ).
    DATA(curated_input) = VALUE if_dive_meter=>curated_inputs( ( direction = |forward| amount = 5 )
                                                               ( direction = |down|    amount = 5 )
                                                               ( direction = |forward| amount = 8 )
                                                               ( direction = |up|      amount = 3 )
                                                               ( direction = |down|    amount = 8 )
                                                               ( direction = |forward| amount = 2 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 900
        act = cut->calculate_position( curated_input ) ).
  ENDMETHOD.

ENDCLASS.

DATA input_line TYPE text100.
SELECT-OPTIONS: p_input FOR input_line NO INTERVALS.

START-OF-SELECTION.

  DATA(lo_application) = NEW application( ).
  WRITE / |Ergebnis Teil 1: { lo_application->run( p_input[] ) }|.
