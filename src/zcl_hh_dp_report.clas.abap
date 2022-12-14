CLASS zcl_hh_dp_report DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_report READ-ONLY.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      show_report.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF output_row,
        serial_number     TYPE zcl_hh_dp_vehicle=>serial_type,
        state_description TYPE zif_hh_dp_state=>description_type,
        trip_odometer     TYPE zif_hh_dp_state=>odometer_type,
        vehicle_entry     TYPE REF TO zcl_hh_dp_vehicle,
        license_plate     TYPE zcl_hh_dp_vehicle=>license_plate_type,
        brand             TYPE zcl_hh_dp_vehicle=>brand_type,
        model             TYPE zcl_hh_dp_vehicle=>model_type,
        year              TYPE zcl_hh_dp_vehicle=>year_type,
        color             TYPE zcl_hh_dp_vehicle=>color_type,
        location          TYPE zcl_hh_dp_vehicle=>location_type,
        heading           TYPE zif_hh_dp_simple_navigation=>heading_type,
        speed             TYPE zcl_hh_dp_vehicle=>speed_type,
        speed_unit        TYPE zcl_hh_dp_vehicle=>speed_unit_type,
        weight            TYPE zcl_hh_dp_vehicle=>weight_type,
        weight_unit       TYPE zcl_hh_dp_vehicle=>weight_unit_type,
        description       TYPE zcl_hh_dp_vehicle=>description_type,
        navigation_type   TYPE zcl_hh_dp_vehicle=>navigator_type,
      END   OF output_row,
      output_list TYPE STANDARD TABLE OF output_row.

    DATA:
      output_stack TYPE output_list,
      alv_grid     TYPE REF TO cl_salv_table.

    METHODS:
      build_report,
      on_user_command
        FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      place_out_of_service,
      present_report,
      refresh,
      set_column_titles
        IMPORTING
          alv_grid TYPE REF TO cl_salv_table,
      resume,
      slow,
      stop,
      turn
        IMPORTING
          turn TYPE zif_hh_dp_simple_navigation=>turn_type,
      maintain,
      make_available,
      repair,
      start,
      tow,
      assign_police_escort,
      decelerate_05,
      decelerate_01,
      accelerate_01,
      accelerate_05,
      show_state_objects_count,
      impose_high_winds_restriction,
      impose_ice_restriction,
      impose_fog_restriction,
      expose_to_dawn_sun_glare,
      expose_to_dusk_sun_glare.

ENDCLASS.

CLASS zcl_hh_dp_report IMPLEMENTATION.
  METHOD build_report.
    DATA: output_entry     LIKE LINE OF output_stack,
          fleet_iterator   TYPE REF TO zif_hh_dp_iterator,
          iteration_object TYPE REF TO object,
          vehicle_state    TYPE REF TO zif_hh_dp_state.

    fleet_iterator = zcl_hh_dp_fleet_manager=>singleton->create_iterator( ).

    WHILE fleet_iterator->has_next( ) EQ zif_hh_dp_iterator=>true.
      iteration_object = fleet_iterator->get_next( ).

      TRY.
          output_entry-vehicle_entry ?= iteration_object.
        CATCH cx_sy_move_cast_error.
          CONTINUE.
      ENDTRY.

      output_entry-vehicle_entry->get_characteristics(
        IMPORTING
          serial_number = output_entry-serial_number
          license_plate = output_entry-license_plate
          brand         = output_entry-brand
          model         = output_entry-model
          year          = output_entry-year
          color         = output_entry-color
          location      = output_entry-location
          speed_unit    = output_entry-speed_unit
          weight_unit   = output_entry-weight_unit
          navigation_type = output_entry-navigation_type
      ).

      output_entry-heading = output_entry-vehicle_entry->get_heading( ).
      output_entry-speed = output_entry-vehicle_entry->get_speed( ).
      output_entry-weight = output_entry-vehicle_entry->get_gross_weight( ).
      output_entry-description = output_entry-vehicle_entry->get_description( ).

      vehicle_state = output_entry-vehicle_entry->get_current_state( ).

      output_entry-state_description = vehicle_state->get_description( ).

      APPEND output_entry TO me->output_stack.
    ENDWHILE.

  ENDMETHOD.

  METHOD class_constructor.
    singleton = NEW #( ).
  ENDMETHOD.

  METHOD present_report.

    CONSTANTS: lc_repid TYPE sy-repid VALUE 'ZHH_OOPDP_MAIN'.

    DATA: grid_events TYPE REF TO cl_salv_events_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->alv_grid
          CHANGING
            t_table      = me->output_stack ).
      CATCH cx_salv_msg.
        MESSAGE e398(00) WITH 'Failure to create alv grid object' "#EC *
                              space
                              space
                              space
                              .
    ENDTRY.

    me->set_column_titles( me->alv_grid ).

    me->alv_grid->set_screen_status(
      EXPORTING
        report        = lc_repid
        pfstatus      = zif_hh_dp_report_screen=>report_status_name
        set_functions = me->alv_grid->c_functions_all
    ).

    grid_events = me->alv_grid->get_event( ).
    SET HANDLER me->on_user_command FOR grid_events.

    me->alv_grid->get_selections( )->set_selection_mode(
      cl_salv_selections=>row_column
    ).

    alv_grid->display( ).
  ENDMETHOD.

  METHOD set_column_titles.

    CONSTANTS:
      column_name_serial_number      TYPE lvc_fname VALUE 'SERIAL_NUMBER',
      column_title_serial_number     TYPE string VALUE 'Serial Number',
      column_name_state_description  TYPE lvc_fname VALUE 'STATE_DESCRIPTION',
      column_title_state_description TYPE string VALUE 'Status',
      column_name_trip_odometer      TYPE lvc_fname VALUE 'TRIP_ODOMETER',
      column_title_trip_odometer     TYPE string VALUE 'Trip Odometer',
      column_name_license_plate      TYPE lvc_fname VALUE 'LICENSE_PLATE',
      column_title_license_plate     TYPE string    VALUE `License plate`,
      column_name_brand              TYPE lvc_fname VALUE 'BRAND',
      column_title_brand             TYPE string    VALUE `Brand`,
      column_name_model              TYPE lvc_fname VALUE 'MODEL',
      column_title_model             TYPE string    VALUE `Model`,
      column_name_year               TYPE lvc_fname VALUE 'YEAR',
      column_title_year              TYPE string    VALUE `Year`,
      column_name_color              TYPE lvc_fname VALUE 'COLOR',
      column_title_color             TYPE string    VALUE `Color`,
      column_name_location           TYPE lvc_fname VALUE 'LOCATION',
      column_title_location          TYPE string    VALUE `Location`,
      column_name_heading            TYPE lvc_fname VALUE 'HEADING',
      column_title_heading           TYPE string    VALUE `Heading`,
      column_name_speed              TYPE lvc_fname VALUE 'SPEED',
      column_title_speed             TYPE string    VALUE `Speed`,
      column_name_speed_unit         TYPE lvc_fname VALUE 'SPEED_UNIT',
      column_title_speed_unit        TYPE string    VALUE `SUoM`,
      column_name_weight             TYPE lvc_fname VALUE 'WEIGHT',
      column_title_weight            TYPE string VALUE 'Weight',
      column_name_weight_unit        TYPE lvc_fname VALUE 'WEIGHT_UNIT',
      column_title_weight_unit       TYPE string VALUE 'WUoM',
      column_name_description        TYPE lvc_fname VALUE 'DESCRIPTION',
      column_title_description       TYPE string VALUE 'Descriptor',
      column_name_navigation_type    TYPE lvc_fname VALUE 'NAVIGATION_TYPE',
      column_title_navigation_type   TYPE string VALUE 'Navigation Type',
      minimum_column_width           TYPE int4      VALUE 08.

    DATA: grid_column_width       TYPE lvc_outlen,
          grid_column_title_short TYPE scrtext_s.

    DATA(grid_columns) = alv_grid->get_columns( ).

    grid_columns->set_optimize( 'X' ).

    DATA(grid_column_stack) = grid_columns->get( ).

    LOOP AT grid_column_stack
      INTO DATA(grid_column_entry).

      CLEAR grid_column_width.

      CASE grid_column_entry-columnname.
        WHEN column_name_serial_number.
          grid_column_title_short = column_title_serial_number.
        WHEN column_name_state_description.
          grid_column_title_short = column_title_state_description.
        WHEN column_name_trip_odometer.
          grid_column_title_short = column_title_trip_odometer.
        WHEN column_name_license_plate.
          grid_column_title_short = column_title_license_plate.
        WHEN column_name_brand.
          grid_column_title_short = column_title_brand.
        WHEN column_name_model.
          grid_column_title_short = column_title_model.
        WHEN column_name_year.
          grid_column_title_short = column_title_year.
        WHEN column_name_color.
          grid_column_title_short = column_title_color.
        WHEN column_name_location.
          grid_column_title_short = column_title_location.
        WHEN column_name_heading.
          grid_column_title_short = column_title_heading.
          grid_column_width = minimum_column_width.
        WHEN column_name_speed.
          grid_column_title_short = column_title_speed.
          grid_column_width = minimum_column_width.
        WHEN column_name_speed_unit.
          grid_column_title_short = column_title_speed_unit.
          grid_column_width = minimum_column_width.
        WHEN column_name_weight.
          grid_column_title_short = column_title_weight.
        WHEN column_name_weight_unit.
          grid_column_title_short   = column_title_weight_unit.
          grid_column_width = minimum_column_width.
        WHEN column_name_description.
          grid_column_title_short = column_title_description.
        WHEN column_name_navigation_type.
          grid_column_title_short = column_title_navigation_type.
        WHEN OTHERS.
          CLEAR grid_column_title_short.
      ENDCASE.
      grid_column_entry-r_column->set_short_text( grid_column_title_short ).
      IF grid_column_width GT 00.
        grid_column_entry-r_column->set_output_length( grid_column_width ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD show_report.
    me->build_report( ).
    me->present_report( ).
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_salv_function.
      WHEN zif_hh_dp_report_screen=>refresh.
        me->refresh( ).
      WHEN zif_hh_dp_report_screen=>turn_left.
        me->turn( zif_hh_dp_simple_navigation=>left_turn ).
      WHEN zif_hh_dp_report_screen=>turn_right.
        me->turn( zif_hh_dp_simple_navigation=>right_turn ).
      WHEN zif_hh_dp_report_screen=>resume.
        me->resume( ).
      WHEN zif_hh_dp_report_screen=>slow.
        me->slow( ).
      WHEN zif_hh_dp_report_screen=>stop.
        me->stop( ).
      when zif_hh_dp_report_screen=>place_out_of_service.
        me->place_out_of_service( ).
      WHEN zif_hh_dp_report_screen=>maintain.
        me->maintain( ).
      WHEN zif_hh_dp_report_screen=>make_available.
        me->make_available( ).
      WHEN zif_hh_dp_report_screen=>repair.
        me->repair( ).
      WHEN zif_hh_dp_report_screen=>start.
        me->start( ).
      WHEN zif_hh_dp_report_screen=>tow.
        me->tow( ).
      when zif_hh_dp_report_screen=>assign_police_escort.
        me->assign_police_escort( ).
      when zif_hh_dp_report_screen=>decelerate_05.
        me->decelerate_05( ).
      when zif_hh_dp_report_screen=>decelerate_01.
        me->decelerate_01( ).
      when zif_hh_dp_report_screen=>accelerate_05.
        me->accelerate_05( ).
      when zif_hh_dp_report_screen=>accelerate_01.
        me->accelerate_01( ).
      when zif_hh_dp_report_screen=>show_state_objects_count.
        me->show_state_objects_count( ).
      when zif_hh_dp_report_screen=>impose_high_winds_restriction.
        me->impose_high_winds_restriction( ).
      when zif_hh_dp_report_screen=>impose_ice_restriction.
        me->impose_ice_restriction( ).
      when zif_hh_dp_report_screen=>impose_fog_restriction.
        me->impose_fog_restriction( ).
      when zif_hh_dp_report_screen=>expose_to_dawn_sun_glare.
        me->expose_to_dawn_sun_glare( ).
      when zif_hh_dp_report_screen=>expose_to_dusk_sun_glare.
        me->expose_to_dusk_sun_glare( ).
    ENDCASE.
  ENDMETHOD.

  METHOD refresh.
    DATA: vehicle_state TYPE REF TO zif_hh_dp_state.

    FIELD-SYMBOLS: <output_entry> LIKE LINE OF output_stack.

    LOOP AT me->output_stack ASSIGNING <output_entry>.
      <output_entry>-heading = <output_entry>-vehicle_entry->get_heading( ).
      <output_entry>-speed = <output_entry>-vehicle_entry->get_speed( ).

      vehicle_state = <output_entry>-vehicle_entry->get_current_state( ).
      <output_entry>-state_description = vehicle_state->get_description( ).
      <output_entry>-trip_odometer = vehicle_state->get_distance_traveled( <output_entry>-vehicle_entry ).
    ENDLOOP.
    me->alv_grid->refresh( ).
  ENDMETHOD.

  METHOD turn.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE ref to zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->turn(
          vehicle = output_entry-vehicle_entry
          turn    = turn
      ).

*      output_entry-vehicle_entry->change_heading( turn ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD resume.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->resume( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD stop.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->stop( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD slow.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->slow( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).

  ENDMETHOD.

  METHOD place_out_of_service.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->place_out_of_service( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD maintain.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->maintain( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD make_available.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->make_available( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD repair.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->repair( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD start.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->start( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD tow.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->tow( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD accelerate_01.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->accelerate_01( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD accelerate_05.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->accelerate_05( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD assign_police_escort.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->assign_police_escort( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD decelerate_01.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->decelerate_01( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD decelerate_05.
    DATA: selected_rows_stack TYPE salv_t_row,
          selected_rows_entry LIKE LINE OF selected_rows_stack,
          output_entry        LIKE LINE OF output_stack,
          current_state       TYPE REF TO zif_hh_dp_state.

    selected_rows_stack = me->alv_grid->get_selections( )->get_selected_rows( ).
    IF selected_rows_stack IS INITIAL.
      MESSAGE i398(00) WITH 'No rows selected'.
      RETURN.
    ENDIF.

    LOOP AT selected_rows_stack INTO selected_rows_entry.
      READ TABLE me->output_stack INTO output_entry
        INDEX selected_rows_entry.

      current_state = output_entry-vehicle_entry->get_current_state( ).
      current_state->decelerate_05( output_entry-vehicle_entry ).
    ENDLOOP.

    CLEAR selected_rows_stack.
    me->alv_grid->get_selections( )->set_selected_rows( selected_rows_stack ).
    me->refresh( ).
  ENDMETHOD.

  METHOD show_state_objects_count.
    data: state_objects_count type int4.

    state_objects_count = zcl_hh_dp_vehicle_state=>get_state_objects_count( ).

    message i398(00) with 'Number of state objects:'
                          state_objects_count
                          space space.
  ENDMETHOD.

  METHOD impose_high_winds_restriction.
    data: output_entry like line of output_stack,
          visitor type ref to zif_hh_dp_visitor.

    create object visitor
      type zcl_hh_dp_hi_speed_restriction.

    loop at me->output_stack
      into output_entry.
      output_entry-vehicle_entry->accept( visitor ).
    endloop.

    me->refresh( ).
  ENDMETHOD.

  METHOD impose_ice_restriction.
    data: output_entry like line of output_stack,
          visitor type ref to zif_hh_dp_visitor.

    create object visitor
      type zcl_hh_dp_ice_restriction.

    loop at me->output_stack
      into output_entry.
      output_entry-vehicle_entry->accept( visitor ).
    endloop.

    me->refresh( ).
  ENDMETHOD.

  METHOD impose_fog_restriction.
    data: output_entry like line of output_stack,
          visitor type ref to zif_hh_dp_visitor.

    create object visitor
      type zcl_hh_dp_fog_restriction.

    loop at me->output_stack
      into output_entry.
      output_entry-vehicle_entry->accept( visitor ).
    endloop.

    me->refresh( ).
  ENDMETHOD.

  METHOD expose_to_dawn_sun_glare.
    constants: eastbound type zif_hh_dp_simple_navigation=>heading_type value 'E'.

    data: output_entry like line of output_stack,
          visitor type ref to zif_hh_dp_visitor.

    create object visitor
      type zcl_hh_dp_sun_glare_mitigator
      exporting
        affected_heading = eastbound.

    loop at me->output_stack
      into output_entry.
      output_entry-vehicle_entry->accept( visitor ).
    endloop.

    me->refresh( ).
  ENDMETHOD.

  METHOD expose_to_dusk_sun_glare.
    constants: westbound type zif_hh_dp_simple_navigation=>heading_type value 'W'.

    data: output_entry like line of output_stack,
          visitor type ref to zif_hh_dp_visitor.

    create object visitor
      type zcl_hh_dp_sun_glare_mitigator
      exporting
        affected_heading = westbound.

    loop at me->output_stack
      into output_entry.
      output_entry-vehicle_entry->accept( visitor ).
    endloop.

    me->refresh( ).
  ENDMETHOD.

ENDCLASS.
