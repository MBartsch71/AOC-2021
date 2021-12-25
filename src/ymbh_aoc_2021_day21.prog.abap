REPORT ymbh_aoc_2021_day21.

INTERFACE if_player.
  METHODS get_position RETURNING VALUE(result) TYPE i.
  METHODS move IMPORTING fields TYPE i.
  METHODS get_score RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

INTERFACE if_die.
  METHODS roll RETURNING VALUE(result) TYPE i.
  METHODS get_rolls RETURNING VALUE(result) TYPE i.
ENDINTERFACE.

INTERFACE if_application.
  TYPES: BEGIN OF input,
           player    TYPE i,
           start_pos TYPE i,
         END OF input.
  TYPES inputs TYPE SORTED TABLE OF input WITH UNIQUE KEY primary_key COMPONENTS player.
  TYPES raw_input TYPE RANGE OF text1024.

  METHODS run.
  METHODS get_result RETURNING VALUE(result) TYPE decfloat34.

ENDINTERFACE.

CLASS player DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_player.
    METHODS constructor IMPORTING current_position TYPE i.

  PRIVATE SECTION.
    DATA current_position TYPE i.
    DATA score TYPE i.

    METHODS calculate_score.

    METHODS calculate_new_position IMPORTING fields TYPE i.

ENDCLASS.

CLASS cx_game_over DEFINITION INHERITING FROM cx_no_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
ENDCLASS.

CLASS cx_game_over IMPLEMENTATION.
ENDCLASS.

CLASS player IMPLEMENTATION.

  METHOD constructor.
    me->current_position = current_position.
  ENDMETHOD.

  METHOD if_player~get_position.
    result = current_position.
  ENDMETHOD.

  METHOD if_player~get_score.
    result = score.
  ENDMETHOD.

  METHOD if_player~move.
    calculate_new_position( fields ).
    calculate_score( ).
  ENDMETHOD.

  METHOD calculate_new_position.
    DATA(result) = current_position + fields.
    current_position = COND #( WHEN result MOD 10 = 0 THEN 10
                               ELSE result MOD 10 ).
  ENDMETHOD.

  METHOD calculate_score.
    score = score + current_position.
    IF score > 1000.
      RAISE EXCEPTION TYPE cx_game_over MESSAGE i000(01) WITH `Player won!`.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS die DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_die.

  PRIVATE SECTION.
    DATA last_roll_count TYPE i.
    DATA sum_of_rolls TYPE i.

    METHODS increase_last_roll_count_by_1.
    METHODS increase_sum_of_rolls_by_1.
    METHODS check_die_rolled_100_times.
    METHODS roll_count_is_multiple_of_100 RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS die IMPLEMENTATION.

  METHOD if_die~roll.
    increase_last_roll_count_by_1( ).
    increase_sum_of_rolls_by_1( ).
    result = last_roll_count.
  ENDMETHOD.

  METHOD if_die~get_rolls.
    result = sum_of_rolls.
  ENDMETHOD.

  METHOD increase_last_roll_count_by_1.
    check_die_rolled_100_times( ).
    last_roll_count = last_roll_count + 1.
  ENDMETHOD.

  METHOD increase_sum_of_rolls_by_1.
    sum_of_rolls = sum_of_rolls + 1.
  ENDMETHOD.

  METHOD check_die_rolled_100_times.
    IF roll_count_is_multiple_of_100( ).
      last_roll_count = 0.
    ENDIF.
  ENDMETHOD.

  METHOD roll_count_is_multiple_of_100.
    result = xsdbool( sum_of_rolls MOD 100 = 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_application.
    METHODS constructor IMPORTING raw_input TYPE if_application=>raw_input.

  PRIVATE SECTION.
    DATA curated_input TYPE if_application=>inputs.
    DATA result TYPE decfloat34.
    DATA player_1 TYPE REF TO if_player.
    DATA player_2 TYPE REF TO if_player.
    DATA die TYPE REF TO if_die.
    DATA active_player TYPE REF TO if_player.

    METHODS curate_input       IMPORTING raw_input TYPE if_application=>raw_input.

    METHODS build_curated_line IMPORTING line          TYPE text1024
                               RETURNING VALUE(result) TYPE if_application=>input.
    METHODS toggle_player.
ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    curate_input( raw_input ).
  ENDMETHOD.

  METHOD curate_input.
    curated_input = VALUE #( FOR line IN raw_input
                                ( build_curated_line( line-low ) ) ).
  ENDMETHOD.

  METHOD build_curated_line.
    DATA first TYPE c LENGTH 100.
    DATA second TYPE c LENGTH 100.
    DATA first_info TYPE stringtab.

    SPLIT line AT ':' INTO first second.
    SPLIT first AT space INTO TABLE first_info.

    result-player = first_info[ 2 ].
    result-start_pos = second.

  ENDMETHOD.

  METHOD if_application~get_result.
    result = me->result.
  ENDMETHOD.

  METHOD if_application~run.
    player_1 = NEW player( curated_input[ 1 ]-start_pos ).
    player_2 = NEW player( curated_input[ 2 ]-start_pos ).
    die = NEW die( ).

    TRY.
        DO.
          DATA(result_of_rolls_for_round) = 0.
          toggle_player( ).
          DO 3 TIMES.
            result_of_rolls_for_round = result_of_rolls_for_round + die->roll( ).
          ENDDO.
          active_player->move( result_of_rolls_for_round ).
        ENDDO.
      CATCH cx_game_over.
        toggle_player( ).
        result = die->get_rolls( ) * active_player->get_score( ).
    ENDTRY.

  ENDMETHOD.

  METHOD toggle_player.
    active_player = COND #( WHEN active_player IS NOT BOUND THEN player_1
                            WHEN active_player = player_1 THEN player_2
                            WHEN active_player = player_2 THEN player_1 ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_player DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_player.

    METHODS setup.
    METHODS new_player_with_starting_pos FOR TESTING.
    METHODS move_player_7_fields         FOR TESTING.
    METHODS get_score_after_7_fields     FOR TESTING.
    METHODS player_wins_at_1000_points   FOR TESTING.
ENDCLASS.

CLASS tc_player IMPLEMENTATION.

  METHOD setup.
    cut = NEW player( 7 ).
  ENDMETHOD.

  METHOD new_player_with_starting_pos.
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = cut->get_position( ) ).
  ENDMETHOD.

  METHOD move_player_7_fields.
    cut->move( 7 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->get_position( ) ).
  ENDMETHOD.

  METHOD get_score_after_7_fields.
    cut->move( 7 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->get_score( ) ).
  ENDMETHOD.

  METHOD player_wins_at_1000_points.
    TRY.
        DO 225 TIMES.
          cut->move( 7 ).
        ENDDO.
      CATCH cx_game_over INTO DATA(error).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
        exp = `I:01:000 Player won!.`
        act = error->get_longtext( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_die DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_die.

    METHODS setup.
    METHODS sum_of_eyes_after_first_3_roll FOR TESTING.
    METHODS sum_of_eyes_after_two_players  FOR TESTING.
    METHODS sum_of_rolls                   FOR TESTING.
    METHODS sum_after_101_rolls            FOR TESTING.
ENDCLASS.


CLASS tc_die IMPLEMENTATION.

  METHOD setup.
    cut = NEW die( ).
  ENDMETHOD.

  METHOD sum_of_eyes_after_first_3_roll.
    DATA result TYPE i.
    DO 3 TIMES.
      result = result + cut->roll( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = result ).
  ENDMETHOD.

  METHOD sum_of_eyes_after_two_players.
    DATA result TYPE i.
    DO 6 TIMES.
      result = result + cut->roll( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 21
        act = result ).
  ENDMETHOD.

  METHOD sum_of_rolls.
    DO 3 TIMES.
      cut->roll( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->get_rolls( ) ).
  ENDMETHOD.

  METHOD sum_after_101_rolls.
    DATA result TYPE i.
    DO 101 TIMES.
      result = result + cut->roll( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 5051
        act = result ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_application DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO if_application.

    METHODS setup.

    METHODS get_result_for_test_run FOR TESTING.

ENDCLASS.


CLASS tc_application IMPLEMENTATION.

  METHOD setup.
    DATA raw_input TYPE RANGE OF text1024.
    raw_input = VALUE #( ( sign = 'I' option = 'EQ' low = 'Player 1 starting position: 4' )
                         ( sign = 'I' option = 'EQ' low = 'Player 2 starting position: 8' ) ).
    cut = NEW application( raw_input ).
  ENDMETHOD.

  METHOD get_result_for_test_run.
    cut->run( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 752247
        act = cut->get_result( ) ).
  ENDMETHOD.

ENDCLASS.

data input type text1024.
SELECT-OPTIONS: so_input for input no INTERVALS.

start-OF-SELECTION.

data(application) = new application( so_input[] ).

application->if_application~run( ).

write / |Ergebnis Teil 1: { application->if_application~get_result( ) }|.
