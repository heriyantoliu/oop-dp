CLASS zcl_hh_dp_report DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      execute                      TYPE sy-ucomm VALUE 'ONLI',
      add_new_car                  TYPE sy-ucomm VALUE 'NEWCAR',
      add_new_truck                TYPE sy-ucomm VALUE 'NEWTRUCK',
      selection_screen_status_name TYPE sy-pfkey VALUE 'SELECTION_SCREEN'.

    class-data:
      singleton type ref to zcl_hh_dp_report read-only.

    CLASS-METHODS:
      class_constructor.

    methods:
      register_car_entry
        IMPORTING
          license_plate    TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand            TYPE zcl_hh_dp_vehicle=>brand_type
          year             TYPE zcl_hh_dp_vehicle=>year_type
          model            TYPE zcl_hh_dp_vehicle=>model_type
          color            TYPE zcl_hh_dp_vehicle=>color_type
          location         TYPE zcl_hh_dp_vehicle=>location_type
          heading          TYPE zif_hh_dp_simple_navigation=>heading_type
          turn01           TYPE zif_hh_dp_simple_navigation=>turn_type
          turn02           TYPE zif_hh_dp_simple_navigation=>turn_type
          turn03           TYPE zif_hh_dp_simple_navigation=>turn_type
          speed01          TYPE zcl_hh_dp_vehicle=>speed_type
          speed02          TYPE zcl_hh_dp_vehicle=>speed_type
          speed03          TYPE zcl_hh_dp_vehicle=>speed_type
          speed_unit       TYPE zcl_hh_dp_vehicle=>speed_unit_type
          tare_weight      TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit      TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers       TYPE zcl_hh_dp_car=>passengers_type
          basic_navigation TYPE checkbox
          gps_navigation   TYPE checkbox
          no_navigation    TYPE checkbox,
      register_truck_entry
        IMPORTING
          license_plate    TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand            TYPE zcl_hh_dp_vehicle=>brand_type
          year             TYPE zcl_hh_dp_vehicle=>year_type
          model            TYPE zcl_hh_dp_vehicle=>model_type
          color            TYPE zcl_hh_dp_vehicle=>color_type
          location         TYPE zcl_hh_dp_vehicle=>location_type
          heading          TYPE zif_hh_dp_simple_navigation=>heading_type
          turn01           TYPE zif_hh_dp_simple_navigation=>turn_type
          turn02           TYPE zif_hh_dp_simple_navigation=>turn_type
          turn03           TYPE zif_hh_dp_simple_navigation=>turn_type
          speed01          TYPE zcl_hh_dp_vehicle=>speed_type
          speed02          TYPE zcl_hh_dp_vehicle=>speed_type
          speed03          TYPE zcl_hh_dp_vehicle=>speed_type
          speed_unit       TYPE zcl_hh_dp_vehicle=>speed_unit_type
          tare_weight      TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit      TYPE zcl_hh_dp_vehicle=>weight_unit_type
          cargo_weight     TYPE zcl_hh_dp_vehicle=>weight_type
          basic_navigation TYPE checkbox
          gps_navigation   TYPE checkbox
          no_navigation    TYPE checkbox,
      show_report.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF output_row,
        serial_number   TYPE zcl_hh_dp_vehicle=>serial_type,
        license_plate   TYPE zcl_hh_dp_vehicle=>license_plate_type,
        brand           TYPE zcl_hh_dp_vehicle=>brand_type,
        model           TYPE zcl_hh_dp_vehicle=>model_type,
        year            TYPE zcl_hh_dp_vehicle=>year_type,
        color           TYPE zcl_hh_dp_vehicle=>color_type,
        location        TYPE zcl_hh_dp_vehicle=>location_type,
        heading         TYPE zif_hh_dp_simple_navigation=>heading_type,
        speed           TYPE zcl_hh_dp_vehicle=>speed_type,
        speed_unit      TYPE zcl_hh_dp_vehicle=>speed_unit_type,
        weight          TYPE zcl_hh_dp_vehicle=>weight_type,
        weight_unit     TYPE zcl_hh_dp_vehicle=>weight_unit_type,
        description     TYPE zcl_hh_dp_vehicle=>description_type,
        navigation_type TYPE zcl_hh_dp_vehicle=>navigator_type,
      END   OF output_row,
      output_list TYPE STANDARD TABLE OF output_row.

    DATA:
      output_stack  TYPE output_list,
      vehicle_stack TYPE TABLE OF REF TO zcl_hh_dp_vehicle.

    METHODS:
      build_report,
      present_report,
      set_column_titles
        IMPORTING
          alv_grid TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS zcl_hh_dp_report IMPLEMENTATION.
  METHOD build_report.
    DATA: output_entry  LIKE LINE OF output_stack,
          vehicle_entry TYPE REF TO zcl_hh_dp_vehicle.

    LOOP AT me->vehicle_stack
      INTO vehicle_entry.
      vehicle_entry->get_characteristics(
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

      output_entry-heading = vehicle_entry->get_heading( ).
      output_entry-speed = vehicle_entry->get_speed( ).
      output_entry-weight = vehicle_entry->get_gross_weight( ).
      output_entry-description = vehicle_entry->get_description( ).

      APPEND output_entry TO me->output_stack.
    ENDLOOP.

  ENDMETHOD.

  METHOD class_constructor.
    zcl_hh_dp_report=>singleton = new #( ).
  ENDMETHOD.

  METHOD present_report.
    DATA: alv_grid TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv_grid
          CHANGING
            t_table      = me->output_stack ).
      CATCH cx_salv_msg.
        MESSAGE e398(00) WITH 'Failure to create alv grid object' "#EC *
                              space
                              space
                              space
                              .
    ENDTRY.

    me->set_column_titles( alv_grid ).

    DATA(lo_display) = alv_grid->get_display_settings( ).

    lo_display->set_striped_pattern( 'X' ).

    alv_grid->display( ).
  ENDMETHOD.

  METHOD register_car_entry.

    DATA: vehicle_entry TYPE REF TO zcl_hh_dp_vehicle.

    CREATE OBJECT vehicle_entry
      TYPE zcl_hh_dp_car
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        passengers       = passengers
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.


    APPEND vehicle_entry TO me->vehicle_stack.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    MESSAGE s398(00) WITH 'Car entry registered for'
                          license_plate
                          space space.
  ENDMETHOD.

  METHOD register_truck_entry.

    DATA: vehicle_entry TYPE REF TO zcl_hh_dp_vehicle.

    CREATE OBJECT vehicle_entry
      TYPE zcl_hh_dp_truck
      EXPORTING
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        cargo_weight     = cargo_weight
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.


    APPEND vehicle_entry TO me->vehicle_stack.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    MESSAGE s398(00) WITH 'Truck entry registered for'
                          license_plate
                          space space.

  ENDMETHOD.

  METHOD set_column_titles.

    CONSTANTS:
      column_name_serial_number    TYPE lvc_fname VALUE 'SERIAL_NUMBER',
      column_title_serial_number   TYPE string VALUE 'Serial Number',
      column_name_license_plate    TYPE lvc_fname VALUE 'LICENSE_PLATE',
      column_title_license_plate   TYPE string    VALUE `License plate`,
      column_name_brand            TYPE lvc_fname VALUE 'BRAND',
      column_title_brand           TYPE string    VALUE `Brand`,
      column_name_model            TYPE lvc_fname VALUE 'MODEL',
      column_title_model           TYPE string    VALUE `Model`,
      column_name_year             TYPE lvc_fname VALUE 'YEAR',
      column_title_year            TYPE string    VALUE `Year`,
      column_name_color            TYPE lvc_fname VALUE 'COLOR',
      column_title_color           TYPE string    VALUE `Color`,
      column_name_location         TYPE lvc_fname VALUE 'LOCATION',
      column_title_location        TYPE string    VALUE `Location`,
      column_name_heading          TYPE lvc_fname VALUE 'HEADING',
      column_title_heading         TYPE string    VALUE `Heading`,
      column_name_speed            TYPE lvc_fname VALUE 'SPEED',
      column_title_speed           TYPE string    VALUE `Speed`,
      column_name_speed_unit       TYPE lvc_fname VALUE 'SPEED_UNIT',
      column_title_speed_unit      TYPE string    VALUE `SUoM`,
      column_name_weight           TYPE lvc_fname VALUE 'WEIGHT',
      column_title_weight          TYPE string VALUE 'Weight',
      column_name_weight_unit      TYPE lvc_fname VALUE 'WEIGHT_UNIT',
      column_title_weight_unit     TYPE string VALUE 'WUoM',
      column_name_description      TYPE lvc_fname VALUE 'DESCRIPTION',
      column_title_description     TYPE string VALUE 'Descriptor',
      column_name_navigation_type  TYPE lvc_fname VALUE 'NAVIGATION_TYPE',
      column_title_navigation_type TYPE string VALUE 'Navigation Type',
      minimum_column_width         TYPE int4      VALUE 08.

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

ENDCLASS.
