REPORT ymbh_aoc_2021_day4.

CLASS cx_board DEFINITION INHERITING FROM cx_no_check.

  PUBLIC SECTION.
    METHODS constructor            IMPORTING textid             LIKE textid OPTIONAL
                                             previous           LIKE previous OPTIONAL
                                             number_responsible TYPE i.

    METHODS get_number_responsible RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA number_responsible TYPE i.

ENDCLASS.

CLASS cx_board IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    me->number_responsible = number_responsible.
  ENDMETHOD.

  METHOD get_number_responsible.
    result = me->number_responsible.
  ENDMETHOD.

ENDCLASS.
CLASS board DEFINITION DEFERRED.
INTERFACE if_board.
  TYPES: BEGIN OF board_line,
           col_1_value TYPE i,
           col_1_mark  TYPE abap_bool,
           col_2_value TYPE i,
           col_2_mark  TYPE abap_bool,
           col_3_value TYPE i,
           col_3_mark  TYPE abap_bool,
           col_4_value TYPE i,
           col_4_mark  TYPE abap_bool,
           col_5_value TYPE i,
           col_5_mark  TYPE abap_bool,
         END OF board_line.
  TYPES boards TYPE STANDARD TABLE OF board_line WITH EMPTY KEY.
  TYPES numbers_drawn TYPE STANDARD TABLE OF i WITH EMPTY KEY.

  TYPES: BEGIN OF winning_board_line,
           number              TYPE i,
           board               TYPE REF TO board,
           sum_unplayed_fields TYPE i,
         END OF winning_board_line.
  TYPES winning_boards TYPE STANDARD TABLE OF winning_board_line WITH EMPTY KEY.
ENDINTERFACE.

CLASS board DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor                IMPORTING board TYPE if_board=>boards.

    METHODS check_numbers              IMPORTING numbers TYPE if_board=>numbers_drawn.

    METHODS board_is_winner            RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_sum_of_unmarked_fields RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA board     TYPE if_board=>boards.
    DATA board_win TYPE abap_bool.

    METHODS board_wins          IMPORTING number        TYPE i
                                RETURNING VALUE(result) TYPE abap_bool.

    METHODS count_marked_fields IMPORTING line          TYPE if_board=>board_line
                                RETURNING VALUE(result) TYPE i.

    METHODS sum_unmarked_fields IMPORTING line          TYPE if_board=>board_line
                                RETURNING VALUE(result) TYPE i.
    METHODS count_marked_cols
      IMPORTING
        board         TYPE if_board=>boards
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.

CLASS board IMPLEMENTATION.

  METHOD constructor.
    me->board = board.
  ENDMETHOD.

  METHOD check_numbers.
    LOOP AT numbers ASSIGNING FIELD-SYMBOL(<number>).
      LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
        DATA(index) = 1.
        DO 5 TIMES.
          ASSIGN COMPONENT index OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
          IF <value> = <number>.
            ASSIGN COMPONENT index + 1 OF STRUCTURE <line> TO FIELD-SYMBOL(<mark>).
            <mark> = abap_true.
          ENDIF.
          index = index + 2.
        ENDDO.
      ENDLOOP.
      board_wins( <number> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD board_is_winner.
    result = board_win.
  ENDMETHOD.

  METHOD board_wins.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      IF count_marked_fields( <line> ) = 5.
        board_win = abap_true.
        RAISE EXCEPTION TYPE cx_board EXPORTING number_responsible = number.
      ENDIF.
    ENDLOOP.
    IF count_marked_cols( board ) = 5.
      board_win = abap_true.
      RAISE EXCEPTION TYPE cx_board EXPORTING number_responsible = number.
    ENDIF.
  ENDMETHOD.

  METHOD count_marked_fields.
    DATA index TYPE i VALUE 2.
    DO 5 TIMES.
      ASSIGN COMPONENT index OF STRUCTURE line TO FIELD-SYMBOL(<mark>).
      IF <mark> = abap_true.
        result = result + 1.
      ENDIF.
      index = index + 2.
    ENDDO.
  ENDMETHOD.

  METHOD get_sum_of_unmarked_fields.
    result = REDUCE #( INIT sum = 0
                       FOR line IN board
                       NEXT sum = sum + sum_unmarked_fields( line ) ).
  ENDMETHOD.

  METHOD sum_unmarked_fields.
    DATA(index) = 1.
    DO 5 TIMES.
      ASSIGN COMPONENT index OF STRUCTURE line TO FIELD-SYMBOL(<value>).
      ASSIGN COMPONENT index + 1 OF STRUCTURE line TO FIELD-SYMBOL(<mark>).
      IF <mark> = abap_false.
        result = result + <value>.
      ENDIF.
      index = index + 2.
    ENDDO.
  ENDMETHOD.

  METHOD count_marked_cols.
    DATA index TYPE i.
    DATA sum TYPE i.
    DO 5 TIMES.
      index = index + 2.
      LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
        ASSIGN COMPONENT index OF STRUCTURE <line> TO FIELD-SYMBOL(<mark>).
        IF <mark> = abap_true.
          sum = sum + 1.
        ENDIF.
      ENDLOOP.
      IF sum = 5.
        result = 5.
        EXIT.
      ENDIF.
      sum = 0.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS board_collection DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS get_count RETURNING VALUE(result) TYPE i.
    METHODS add_board IMPORTING board TYPE any.
    METHODS get_next_board RETURNING VALUE(result) TYPE REF TO board.
    METHODS set_current_board IMPORTING current_board TYPE i.

  PRIVATE SECTION.
    DATA boards TYPE STANDARD TABLE OF REF TO board WITH EMPTY KEY.
    DATA current_board TYPE i.

ENDCLASS.

CLASS board_collection IMPLEMENTATION.

  METHOD get_count.
    result = lines( boards ).
  ENDMETHOD.

  METHOD add_board.
    boards = VALUE #( BASE boards ( board ) ).
  ENDMETHOD.

  METHOD get_next_board.
    current_board = current_board + 1.
    TRY.
        result = boards[ current_board ].
      CATCH cx_sy_itab_line_not_found.
        current_board = 0.
    ENDTRY.
  ENDMETHOD.

  METHOD set_current_board.
    me->current_board = current_board.
  ENDMETHOD.

ENDCLASS.

CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor                 IMPORTING raw_input TYPE stringtab.


    METHODS build_boards                RETURNING VALUE(result) TYPE REF TO board_collection.

    METHODS get_drawn_numbers           RETURNING VALUE(result) TYPE if_board=>numbers_drawn.
    METHODS set_current_line            IMPORTING current_line TYPE i.

  PRIVATE SECTION.
    DATA board_collection TYPE REF TO board_collection.
    DATA raw_input TYPE stringtab.
    DATA numbers_drawn TYPE if_board=>numbers_drawn.
    DATA current_line TYPE i VALUE 1.

    METHODS populate_numbers_table IMPORTING line TYPE string.

ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD constructor.
    board_collection = NEW #( ).
    me->raw_input = raw_input.
  ENDMETHOD.

  METHOD build_boards.
    DATA lt_board TYPE if_board=>boards.
    LOOP AT raw_input ASSIGNING FIELD-SYMBOL(<line>) FROM current_line.
      IF <line> IS INITIAL.
        board_collection->add_board( NEW board( lt_board ) ).
        CLEAR lt_board.
        CONTINUE.
      ENDIF.
      lt_board = VALUE #( BASE lt_board ( col_1_value = CONV #( substring( val = <line> off =  0 len = 2 ) )
                                          col_2_value = CONV #( substring( val = <line> off =  3 len = 2 ) )
                                          col_3_value = CONV #( substring( val = <line> off =  6 len = 2 ) )
                                          col_4_value = CONV #( substring( val = <line> off =  9 len = 2 ) )
                                          col_5_value = CONV #( substring( val = <line> off = 12 len = 2 ) ) ) ).
    ENDLOOP.
    IF lt_board IS NOT INITIAL.
      board_collection->add_board( NEW board( lt_board ) ).
    ENDIF.
    result = board_collection.
  ENDMETHOD.

  METHOD get_drawn_numbers.
    LOOP AT raw_input ASSIGNING FIELD-SYMBOL(<line>) FROM current_line.
      IF <line> IS INITIAL.
        result = numbers_drawn.
        current_line = sy-tabix + 1.
        EXIT.
      ENDIF.
      populate_numbers_table( <line> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD populate_numbers_table.
    SPLIT line AT ',' INTO TABLE DATA(numbers).
    numbers_drawn = VALUE #( BASE numbers_drawn
                             FOR number IN numbers
                             ( CONV #( number ) ) ).
  ENDMETHOD.

  METHOD set_current_line.
    me->current_line = current_line.
  ENDMETHOD.

ENDCLASS.

CLASS winning_boards DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS get_first_winning_board RETURNING VALUE(result) TYPE if_board=>winning_board_line.

    METHODS add_board               IMPORTING board_line TYPE if_board=>winning_board_line.
    METHODS get_winning_board_count RETURNING VALUE(result) TYPE i.
    METHODS get_last_winning_board  RETURNING VALUE(result) TYPE if_board=>winning_board_line.

  PRIVATE SECTION.
    DATA winning_boards TYPE if_board=>winning_boards.
    METHODS board_exists_already   IMPORTING board_line    TYPE if_board=>winning_board_line
                                   RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS winning_boards IMPLEMENTATION.

  METHOD get_first_winning_board.
    result = winning_boards[ 1 ].
  ENDMETHOD.

  METHOD add_board.
    IF board_exists_already( board_line ).
      RETURN.
    ENDIF.
    winning_boards = VALUE #( BASE winning_boards ( board_line ) ).
  ENDMETHOD.

  METHOD get_winning_board_count.
    result = lines( winning_boards ).
  ENDMETHOD.

  METHOD board_exists_already.
    result = xsdbool( line_exists( winning_boards[ board = board_line-board ] ) ).
  ENDMETHOD.

  METHOD get_last_winning_board.
    result = winning_boards[ lines( winning_boards ) ].
  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS read_input_file IMPORTING file_location TYPE text255
                            RETURNING VALUE(result) TYPE stringtab.

    METHODS run_first_part  IMPORTING raw_input     TYPE stringtab.
    METHODS get_winner_first_part RETURNING VALUE(result) TYPE i.
    METHODS get_winner_second_part RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA winning_boards TYPE REF TO winning_boards.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD read_input_file.
    DATA line TYPE string.

    OPEN DATASET file_location FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    WHILE ( sy-subrc EQ 0 ).
      READ DATASET file_location INTO line.
      APPEND line TO result.
      CLEAR line.
    ENDWHILE.
    CLOSE DATASET file_location.
  ENDMETHOD.

  METHOD run_first_part.
    DATA(input) = NEW input_processor( raw_input ).
    DATA(numbers) = input->get_drawn_numbers( ).
    DATA(board_collection) = input->build_boards( ).
    winning_boards = NEW winning_boards( ).

    LOOP AT numbers ASSIGNING FIELD-SYMBOL(<number>).
      DATA(numbers_drawn) = VALUE if_board=>numbers_drawn( ( <number> ) ).

      DATA(board) = board_collection->get_next_board( ).
      WHILE board IS BOUND.
        TRY.
            board->check_numbers( numbers_drawn ).
          CATCH cx_board INTO DATA(lx_error).
            winning_boards->add_board( VALUE #( board = board number = lx_error->get_number_responsible( ) sum_unplayed_fields = board->get_sum_of_unmarked_fields( ) ) ).
        ENDTRY.
        board = board_collection->get_next_board( ).
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_winner_first_part.
    DATA(winning_info) = winning_boards->get_first_winning_board( ).
    result = winning_info-number * winning_info-sum_unplayed_fields.
  ENDMETHOD.

  METHOD get_winner_second_part.
    DATA(winning_info) = winning_boards->get_last_winning_board( ).
    result = winning_info-number * winning_info-sum_unplayed_fields.
  ENDMETHOD.

ENDCLASS.

CLASS tc_board DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO board.
    METHODS setup.
    METHODS get_input_numbers RETURNING VALUE(result) TYPE if_board=>numbers_drawn.

    METHODS mark_winning_board             FOR TESTING.
    METHODS check_sum_of_not_marked_fields FOR TESTING.
ENDCLASS.


CLASS tc_board IMPLEMENTATION.

  METHOD setup.
    DATA(board_input) = VALUE if_board=>boards( ( col_1_value = 14 col_2_value = 21 col_3_value = 17 col_4_value = 24 col_5_value =  4 )
                                               ( col_1_value = 10 col_2_value = 16 col_3_value = 15 col_4_value =  9 col_5_value = 19 )
                                               ( col_1_value = 18 col_2_value =  8 col_3_value = 23 col_4_value = 26 col_5_value = 20 )
                                               ( col_1_value = 22 col_2_value = 11 col_3_value = 13 col_4_value =  6 col_5_value =  5 )
                                               ( col_1_value =  2 col_2_value =  0 col_3_value = 12 col_4_value =  3 col_5_value =  7 ) ).
    cut = NEW #( board_input ).
  ENDMETHOD.

  METHOD get_input_numbers.
    result =  VALUE #( (  7 ) (  4 ) (  9 ) (  5 ) ( 11 ) ( 17 ) ( 23 ) (  2 ) (  0 ) ( 14 )
                       ( 21 ) ( 24 ) ( 10 ) ( 16 ) ( 13 ) (  6 ) ( 15 ) ( 25 ) ( 12 ) ( 22 )
                       ( 18 ) ( 20 ) (  8 ) ( 19 ) (  3 ) ( 26 ) (  1 ) ).
  ENDMETHOD.

  METHOD mark_winning_board.
    TRY.
        cut->check_numbers( get_input_numbers( ) ).
      CATCH cx_board INTO DATA(lx_exception).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
        exp = 24
        act = lx_exception->get_number_responsible( ) ).
  ENDMETHOD.

  METHOD check_sum_of_not_marked_fields.
    TRY.
        cut->check_numbers( get_input_numbers( ) ).
      CATCH cx_board.
        cl_abap_unit_assert=>assert_equals(
            exp = 188
            act = cut->get_sum_of_unmarked_fields( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS tc_board_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO board_collection.

    METHODS setup.
    METHODS build_three_board_collection FOR TESTING.
    METHODS iterate_through_one_cycle    FOR TESTING.
    METHODS iterate_through_two_cycles   FOR TESTING.

ENDCLASS.

CLASS tc_board_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    DO 3 TIMES.
      cut->add_board( NEW board( VALUE #( ) ) ).
    ENDDO.
  ENDMETHOD.

  METHOD build_three_board_collection.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->get_count( ) ).
  ENDMETHOD.

  METHOD iterate_through_one_cycle.
    DATA(board) = cut->get_next_board( ).
    IF board IS BOUND.
      DATA(board_count) = 1.
    ENDIF.
    WHILE board IS BOUND.
      board = cut->get_next_board( ).
      IF board IS BOUND.
        board_count = board_count + 1.
      ENDIF.
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = board_count ).
  ENDMETHOD.

  METHOD iterate_through_two_cycles.
    DATA board_count TYPE i.
    DO 2 TIMES.
      DATA(board) = cut->get_next_board( ).
      IF board IS BOUND.
        board_count = board_count + 1.
      ENDIF.
      WHILE board IS BOUND.
        board = cut->get_next_board( ).
        IF board IS BOUND.
          board_count = board_count + 1.
        ENDIF.
      ENDWHILE.
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = board_count ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_input DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO input_processor.

    METHODS setup.
    METHODS provide_raw_input RETURNING VALUE(result) TYPE stringtab.

    METHODS build_numbers_table           FOR TESTING.
    METHODS build_2_board_tables_of_input FOR TESTING.

ENDCLASS.


CLASS tc_input IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( provide_raw_input( ) ).
  ENDMETHOD.

  METHOD provide_raw_input.
    result = VALUE #( ( |7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1| )
                      ( || )
                      ( |22 13 17 11  0| )
                      ( | 8  2 23  4 24| )
                      ( |21  9 14 16  7| )
                      ( | 6 10  3 18  5| )
                      ( | 1 12 20 15 19| )
                      ( || )
                      ( | 3 15  0  2 22| )
                      ( | 9 18 13 17  5| )
                      ( |19  8  7 25 23| )
                      ( |20 11 10 24  4| )
                      ( |14 21 16 12  6| )
                      ( || )
                      ( |14 21 17 24  4| )
                      ( |10 16 15  9 19| )
                      ( |18  8 23 26 20| )
                      ( |22 11 13  6  5| )
                      ( | 2  0 12  3  7| ) ).
  ENDMETHOD.

  METHOD build_2_board_tables_of_input.
    cut->set_current_line( 3 ).
    DATA(board_collection) = cut->build_boards( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = board_collection->get_count( ) ).
  ENDMETHOD.

  METHOD build_numbers_table.
    DATA(expected_values) = VALUE if_board=>numbers_drawn( (  7 ) ( 4 ) (  9 ) (  5 ) ( 11 ) ( 17 ) ( 23 )
                                                           (  2 ) ( 0 ) ( 14 ) ( 21 ) ( 24 ) ( 10 ) ( 16 )
                                                           ( 13 ) ( 6 ) ( 15 ) ( 25 ) ( 12 ) ( 22 ) ( 18 )
                                                           ( 20 ) ( 8 ) ( 19 ) (  3 ) ( 26 ) (  1 ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->get_drawn_numbers( ) ).
  ENDMETHOD.


ENDCLASS.

CLASS tc_winning_boards DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO winning_boards.

    METHODS setup.
    METHODS add_1_winning_board            FOR TESTING.
    METHODS dont_add_the_samne_board_twice FOR TESTING.
    METHODS get_last_winning_board         FOR TESTING.


ENDCLASS.


CLASS tc_winning_boards IMPLEMENTATION.

  METHOD add_1_winning_board.
    DATA(board) = NEW board( VALUE #( ) ).
    DATA(expected_value) = VALUE if_board=>winning_board_line( board = board number = 0 ).

    cut->add_board( VALUE #( board = board number = 0 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_value
        act = cut->get_first_winning_board( ) ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD dont_add_the_samne_board_twice.
    DATA(board) = NEW board( VALUE #(  ) ).
    cut->add_board( VALUE #( board = board number = 1 ) ).
    cut->add_board( VALUE #( board = board number = 2 ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = cut->get_winning_board_count( ) ).
  ENDMETHOD.

  METHOD get_last_winning_board.
    DATA(board_1) = NEW board( VALUE #( ) ).
    DATA(board_2) = NEW board( VALUE #( ) ).

    DATA(expected_value) = VALUE if_board=>winning_board_line( number = 13 board = board_2 ).
    cut->add_board( VALUE #( board = board_1 number = 24 ) ).
    cut->add_board( VALUE #( board = board_2 number = 13 ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_value
        act = cut->get_last_winning_board( ) ).
  ENDMETHOD.

ENDCLASS.

PARAMETERS: p_file TYPE text255.

START-OF-SELECTION.

  DATA(application) = NEW application( ).
  DATA(raw_input) = application->read_input_file( p_file ).
  application->run_first_part( raw_input ).

  WRITE / |Ergebnis Teil 1: { application->get_winner_first_part( ) }|.
  WRITE / |Ergebnis Teil 2: { application->get_winner_second_part( ) }|.
