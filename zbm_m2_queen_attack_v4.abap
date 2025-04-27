*&---------------------------------------------------------------------*
*& Report ZBM_M2_QUEEN_ATTACK_V4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbm_m2_queen_attack_v4.

*============================================*
*============TOP=============================*
DATA: p_row           TYPE i,
      p_col           TYPE i,
      gv_queen_placed TYPE abap_bool VALUE abap_false.

DATA: lo_salv   TYPE REF TO cl_salv_table.



"Events
CLASS lcl_events DEFINITION.
  "https://www.youtube.com/watch?v=y5v8ycFS-n0
  PUBLIC SECTION.


    "After click action, we will get row number and column name.
    CLASS-METHODS on_double_click
      FOR EVENT if_salv_events_actions_table~double_click
                  OF cl_salv_events_table
      IMPORTING row
                  column.


ENDCLASS.





CLASS lcl_queen DEFINITION.

  "define types.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_chess,
        col   TYPE char1,
        a     TYPE char1,
        b     TYPE char1,
        c     TYPE char1,
        d     TYPE char1,
        e     TYPE char1,
        f     TYPE char1,
        g     TYPE char1,
        h     TYPE char1,
        color TYPE lvc_t_scol,
      END OF ts_chess.

    CLASS-DATA: mt_chess     TYPE TABLE OF ts_chess,
                wa_chess     TYPE  ts_chess,
                lv_row_index TYPE i,
                lv_col_index TYPE i,
                lt_color     TYPE lvc_t_scol,
                ls_color     TYPE lvc_s_scol.

    CLASS-METHODS:
      main,
      build_frame,
      drew_dots,
      drew_chessbord,
      allocate_queen,
      queen_attack,
      alv,
      events,
      display.



ENDCLASS.

CLASS lcl_queen IMPLEMENTATION.
  METHOD main.

    build_frame( ).
*    drew_dots( ).
    "allocate_queen( ).
    "queen_attack( ).
    drew_chessbord( ).
    alv( ).
*    display( ).
*    cl_demo_output=>display( lcl_queen=>mt_chess ). " demo demonstration no availible to locla typs.

  ENDMETHOD.

  METHOD build_frame.
    "Drew Tites line
    INSERT VALUE #(    "direct insert/append.
    col = ' '
    a   = 'A'
    b   = 'B'
    c   = 'C'
    d   = 'D'
    e   = 'E'
    f   = 'F'
    g   = 'G'
    h   = 'H'      )
    INTO mt_chess INDEX 1.

    "Drew 1-8 numbers.
    DO 8 TIMES.
      APPEND VALUE #(
      col = |{ sy-index }|
      a   = ' '
      b   = ' '
      c   = ' '
      d   = ' '
      e   = ' '
      f   = ' '
      g   = ' '
      h   = ' '     )
      TO mt_chess.
    ENDDO.

  ENDMETHOD.

  METHOD drew_dots.

    LOOP AT mt_chess ASSIGNING FIELD-SYMBOL(<row>).
      DO 9 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).
        IF <cell> = ' ' AND sy-tabix <> 1." exclude left top.
          <cell> = '.'.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD drew_chessbord.



    "requre link index to variable, to maintain logic inside sec loop.
    LOOP AT  mt_chess ASSIGNING FIELD-SYMBOL(<row>).
      lv_row_index = sy-tabix.
      DO 9 TIMES.
        lv_col_index = sy-index.

        "Getting dynamically field name  for each cell.
        DATA(lv_field) = SWITCH #( lv_col_index
        WHEN 1 THEN 'COL'
        WHEN 2 THEN 'A' WHEN 3 THEN 'B' WHEN 4 THEN 'C' WHEN 5 THEN 'D'
        WHEN 6 THEN 'E' WHEN 7 THEN 'F' WHEN 8 THEN 'G' WHEN 9 THEN 'H'
        ).

        ASSIGN COMPONENT lv_field OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).

        IF lv_row_index = 1 AND lv_col_index > 1.    "Paint first column
          ls_color-fname = lv_field.
          ls_color-color-col = 6.
          ls_color-color-int = 0.
          ls_color-color-inv = 1.

*
        ELSEIF lv_col_index = 1.
          ls_color-fname = lv_field.
          ls_color-color-col = 6.
          ls_color-color-int = 0.
          ls_color-color-inv = 1.


        ELSEIF
          ( lv_row_index MOD 2 = 1 AND lv_col_index MOD 2 = 0 ) OR
          ( lv_row_index MOD 2 = 0 AND lv_col_index MOD 2 = 1 ).

          ls_color-fname = lv_field.
          ls_color-color-col = 7.
          ls_color-color-int = 1.

        ELSE.
          ls_color-fname = lv_field.
          ls_color-color-col = 7. " Light square
          ls_color-color-int = 0.

        ENDIF.

        APPEND ls_color TO   lt_color.
        CLEAR ls_color.

      ENDDO.

      <row>-color = lt_color.
      CLEAR lt_color.

    ENDLOOP.
  ENDMETHOD.

  METHOD allocate_queen.


    READ TABLE mt_chess ASSIGNING FIELD-SYMBOL(<row>) INDEX p_row + 1.
    DATA(lv_component) = p_col + 1.
    ASSIGN COMPONENT lv_component OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).
    <cell> = 'Q'.


  ENDMETHOD.

  METHOD queen_attack.
    "offset for use simple loop structure.
    p_row = p_row + 1.
    p_col = p_col + 1.

    LOOP AT mt_chess ASSIGNING FIELD-SYMBOL(<row>).
      DO 9 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).
        IF sy-tabix <> 1 AND sy-index <> 1.
          IF     sy-tabix = p_row AND sy-index = p_col.
            <cell> = 'Q'.
          ELSEIF sy-tabix = p_row                    OR
                 sy-index = p_col                    OR
                 sy-tabix - p_row = sy-index - p_col OR
                 sy-tabix - p_row = p_col  - sy-index.
            <cell> = '*'.
            else.
              <cell> = ' '.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD alv.





    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_salv
          CHANGING
            t_table        = mt_chess
            ).
        "Set events
*================================================================*

        DATA: lo_events TYPE REF TO cl_salv_events_table.
        lo_events = lo_salv->get_event( ).
        SET HANDLER lcl_events=>on_double_click FOR lo_events.



        "Set Header and Footer to ALV talbe view.
*================================================================*
        DATA(lo_grid_header) = NEW cl_salv_form_layout_grid( ).



        "1) Headline
        lo_salv->set_top_of_list( lo_grid_header ).

        lo_grid_header->create_header_information( row = 1
                                                  column = 1
                                                  text = 'Queen - Attack'
                                                  ).
        "2) Action text
        lo_grid_header->create_action_information( row = 2
                                                  column = 1
                                                  text = 'chose queen location'
                                                  ).
*        "3) floating text
*        lo_grid_header->create_flow( EXPORTING row = 3 column = 1 )->create_text( text = 'Headerflow: ').
*        lo_grid_header->create_flow( EXPORTING row = 3 column = 3 )->create_text( text = 'Floating text').


        "4) set footer

        "create footer display

        DATA(lo_grid_footer) = NEW cl_salv_form_layout_grid( ).
        lo_grid_footer->create_action_information( row = 2
                                                   column = 1
                                                   text = '@BM'
                                                   ).
        lo_salv->set_end_of_list( lo_grid_footer ).
        "floating text
*         lo_grid_footer->create_flow( EXPORTING row = 3 column = 1 )->create_text( text = 'Footerflow' ).
*         lo_grid_footer->create_flow( EXPORTING row = 3 column = 3 )->create_text( text = 'floating text').





*        lo_salv->get_functions( )->set_all( abap_true ).
        lo_salv->get_display_settings( )->set_list_header( 'Queen attack - Chose Locatoin' ).
        lo_salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        lo_salv->get_columns( )->set_color_column( 'COLOR' ).


        lo_salv->display( )."Display table
      CATCH cx_salv_msg .
        RETURN.
    ENDTRY.



  ENDMETHOD.

  METHOD events.

  ENDMETHOD.

  METHOD display.
    "Method to display table data with write command.
    LOOP AT mt_chess ASSIGNING FIELD-SYMBOL(<row>).
      DO 9 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <row> TO FIELD-SYMBOL(<cell>).
        WRITE <cell>.
      ENDDO.
      WRITE /.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.



CLASS lcl_events IMPLEMENTATION.

  METHOD on_double_click.

    DATA counter TYPE i.

    counter = 0.
    "convert CHAR TO I
    DATA(lv_col) = SWITCH #( column
        WHEN 'COL' THEN 1
        WHEN 'A' THEN 2 WHEN 'B' THEN 3 WHEN 'C' THEN 4 WHEN 'D' THEN 5
        WHEN 'E' THEN 6 WHEN 'F' THEN 7 WHEN 'G' THEN 8 WHEN 'H' THEN 9
        ).
    p_row = row - 1.
    p_col = lv_col - 1.

*    IF counter = 0.
*      lcl_queen=>allocate_queen( ).
**      lo_salv->refresh( ).
*      counter = counter + 1.
*      ELSEIF counter = 1.
*        lcl_queen=>queen_attack( ).
**        lo_salv->refresh( ).
*        counter = counter + 1.
*      else.
*        lcl_queen=>drew_chessbord( ).
**        lo_salv->refresh( ).
*    ENDIF.


    IF gv_queen_placed = abap_false.
      lcl_queen=>allocate_queen( ).
      gv_queen_placed = abap_true.
    ELSE.
      lcl_queen=>queen_attack( ).
*      gv_queen_placed = abap_false.
    ENDIF.

    lo_salv->refresh( ).

*    call TRANSACTION 'SE11' WITHOUT AUTHORITY-CHECK.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_queen=>main( ).

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2025. Sap Release 750
