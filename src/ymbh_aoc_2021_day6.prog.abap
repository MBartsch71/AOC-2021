REPORT ymbh_aoc_2021_day6.


CLASS swarm DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF swarm_line,
             index  TYPE i,
             amount TYPE decfloat34,
           END OF swarm_line.
    TYPES tt_swarm TYPE SORTED TABLE OF swarm_line WITH UNIQUE KEY primary_key COMPONENTS index.

    METHODS constructor  IMPORTING raw_input TYPE stringtab.

    METHODS get_swarm    RETURNING VALUE(result) TYPE tt_swarm.

    METHODS days_elapsed IMPORTING days TYPE i.

    METHODS get_fishes   RETURNING VALUE(result) TYPE decfloat34.

  PRIVATE SECTION.
    DATA swarm_counter TYPE tt_swarm.

ENDCLASS.

CLASS swarm IMPLEMENTATION.

  METHOD constructor.
    swarm_counter = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 9
                                ( index = i amount = 0 ) ).
    LOOP AT raw_input ASSIGNING FIELD-SYMBOL(<line>).
      swarm_counter[ index = CONV i( <line> ) ]-amount = swarm_counter[ index = CONV i( <line> ) ]-amount + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_swarm.
    result = swarm_counter.
  ENDMETHOD.

  METHOD days_elapsed.
    DO days TIMES.
      DATA(index_0_amount) = swarm_counter[ index = 0 ]-amount.
      swarm_counter[ index = 0 ]-amount = swarm_counter[ index = 1 ]-amount.
      swarm_counter[ index = 1 ]-amount = swarm_counter[ index = 2 ]-amount.
      swarm_counter[ index = 2 ]-amount = swarm_counter[ index = 3 ]-amount.
      swarm_counter[ index = 3 ]-amount = swarm_counter[ index = 4 ]-amount.
      swarm_counter[ index = 4 ]-amount = swarm_counter[ index = 5 ]-amount.
      swarm_counter[ index = 5 ]-amount = swarm_counter[ index = 6 ]-amount.
      swarm_counter[ index = 6 ]-amount = swarm_counter[ index = 7 ]-amount + index_0_amount.
      swarm_counter[ index = 7 ]-amount = swarm_counter[ index = 8 ]-amount.
      swarm_counter[ index = 8 ]-amount = index_0_amount.
    ENDDO.
  ENDMETHOD.

  METHOD get_fishes.
    DATA summe TYPE decfloat34.
    result = REDUCE #( INIT sum = summe
                       FOR line IN swarm_counter
                       NEXT sum = sum + line-amount ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING days          TYPE i
                          raw_input     TYPE string
                RETURNING VALUE(result) TYPE decfloat34.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD run.
    SPLIT raw_input AT ',' INTO TABLE DATA(raw_input_tab).
    DATA(swarm) = NEW swarm( raw_input_tab ).
    swarm->days_elapsed( days ).
    result = swarm->get_fishes( ).
  ENDMETHOD.

ENDCLASS.


CLASS tc_swarm_counter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO swarm.

    METHODS setup.
    METHODS build_initial_swarm RETURNING VALUE(result) TYPE stringtab.

    METHODS check_initial_swarm_counter FOR TESTING.
    METHODS swarm_after_1_day           FOR TESTING.
    METHODS swarm_after_2_days          FOR TESTING.
    METHODS swarm_after_10_days         FOR TESTING.
    METHODS count_fishes_after_18_days  FOR TESTING.
    METHODS count_fishes_after_80_days  FOR TESTING.
ENDCLASS.


CLASS tc_swarm_counter IMPLEMENTATION.

  METHOD check_initial_swarm_counter.
    DATA(expected_values) = VALUE swarm=>tt_swarm( ( index = 0 amount = 0 )
                                                   ( index = 1 amount = 1 )
                                                   ( index = 2 amount = 1 )
                                                   ( index = 3 amount = 2 )
                                                   ( index = 4 amount = 1 )
                                                   ( index = 5 amount = 0 )
                                                   ( index = 6 amount = 0 )
                                                   ( index = 7 amount = 0 )
                                                   ( index = 8 amount = 0 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_swarm( ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( build_initial_swarm( ) ).
  ENDMETHOD.

  METHOD build_initial_swarm.
    result = VALUE #( ( |3| ) ( |4| ) ( |3| ) ( |1| ) ( |2| )  ).
  ENDMETHOD.

  METHOD swarm_after_1_day.
    DATA(expected_values) = VALUE swarm=>tt_swarm( ( index = 0 amount = 1 )
                                                   ( index = 1 amount = 1 )
                                                   ( index = 2 amount = 2 )
                                                   ( index = 3 amount = 1 )
                                                   ( index = 4 amount = 0 )
                                                   ( index = 5 amount = 0 )
                                                   ( index = 6 amount = 0 )
                                                   ( index = 7 amount = 0 )
                                                   ( index = 8 amount = 0 ) ).
    cut->days_elapsed( 1 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_swarm( ) ).
  ENDMETHOD.

  METHOD swarm_after_2_days.
    DATA(expected_values) = VALUE swarm=>tt_swarm( ( index = 0 amount = 1 )
                                                   ( index = 1 amount = 2 )
                                                   ( index = 2 amount = 1 )
                                                   ( index = 3 amount = 0 )
                                                   ( index = 4 amount = 0 )
                                                   ( index = 5 amount = 0 )
                                                   ( index = 6 amount = 1 )
                                                   ( index = 7 amount = 0 )
                                                   ( index = 8 amount = 1 ) ).
    cut->days_elapsed( 2 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_swarm( ) ).
  ENDMETHOD.

  METHOD swarm_after_10_days.
    DATA(expected_values) = VALUE swarm=>tt_swarm( ( index = 0 amount = 3 )
                                                   ( index = 1 amount = 2 )
                                                   ( index = 2 amount = 2 )
                                                   ( index = 3 amount = 1 )
                                                   ( index = 4 amount = 0 )
                                                   ( index = 5 amount = 1 )
                                                   ( index = 6 amount = 1 )
                                                   ( index = 7 amount = 1 )
                                                   ( index = 8 amount = 1 ) ).
    cut->days_elapsed( 10 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_swarm( ) ).
  ENDMETHOD.

  METHOD count_fishes_after_18_days.
    cut->days_elapsed( 18 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 26
        act = cut->get_fishes( ) ).
  ENDMETHOD.

  METHOD count_fishes_after_80_days.
    cut->days_elapsed( 80 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 5934
        act = cut->get_fishes( ) ).
  ENDMETHOD.

ENDCLASS.

PARAMETERS: days  TYPE i,
            input TYPE string.

START-OF-SELECTION.

  DATA(fishes) = NEW application( )->run( days = days raw_input = input ).

  WRITE / |Anzahl Laternfishes nach { days } Tagen: { fishes }|.
