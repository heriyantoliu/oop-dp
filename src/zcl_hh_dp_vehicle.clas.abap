CLASS zcl_hh_dp_vehicle DEFINITION
  PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    TYPES:
      brand_type            TYPE f4txt,
      color_type            TYPE f4txt,
      location_type         TYPE f4txt,
      model_type            TYPE f4txt,
      license_plate_type    TYPE f4txt,
      navigator_type        TYPE seoclsname,
      speed_type            TYPE int4,
      speed_unit_type       TYPE char3,
      year_type             TYPE num4,
      serial_type           TYPE num4,
      weight_type           TYPE int4,
      weight_unit_type      TYPE char3,
      description_type(221) TYPE c,
      vehicle_type          TYPE seoclsname,
      option_count          TYPE n LENGTH 1,
      odometer_type         TYPE p LENGTH 7 DECIMALS 3,
      time_stamp_type       TYPE timestamp,
      current_state_type    TYPE c LENGTH 16.

    CONSTANTS:
      state_cruising TYPE current_state_type VALUE 'cruising',
      state_stopped  TYPE current_state_type VALUE 'stop'.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      accelerate
        IMPORTING
          acceleration TYPE speed_type,
      change_heading
        IMPORTING
          turn TYPE zif_hh_dp_simple_navigation=>turn_type,
      get_characteristics
        EXPORTING
          serial_number   TYPE serial_type
          license_plate   TYPE license_plate_type
          brand           TYPE brand_type
          model           TYPE model_type
          year            TYPE year_type
          color           TYPE color_type
          location        TYPE location_type
          speed_unit      TYPE speed_unit_type
          weight_unit     TYPE weight_unit_type
          navigation_type TYPE navigator_type,
      get_description ABSTRACT
        RETURNING
          VALUE(description) TYPE description_type,
      get_distance_traveled
        RETURNING
          VALUE(distance) TYPE odometer_type,
      get_gross_weight ABSTRACT
        RETURNING
          VALUE(gross_weight) TYPE weight_type,
      get_heading
        RETURNING
          VALUE(heading) TYPE zif_hh_dp_simple_navigation=>heading_type,
      get_speed
        RETURNING
          VALUE(speed) TYPE speed_type,
      constructor
        IMPORTING
          license_plate          TYPE license_plate_type
          brand                  TYPE brand_type
          model                  TYPE model_type
          year                   TYPE year_type
          color                  TYPE color_type
          location               TYPE location_type
          speed_unit             TYPE speed_unit_type
          heading                TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight            TYPE weight_type
          weight_unit            TYPE weight_unit_type
          basic_navigation       TYPE checkbox
          gps_navigation         TYPE checkbox
          iphone_navigation      TYPE checkbox
          no_navigation          TYPE checkbox
          vehicle_classification TYPE vehicle_type
          time_started_moving    TYPE time_stamp_type DEFAULT 0
          current_state          TYPE current_state_type DEFAULT space,
      assign_next_in_chain
        IMPORTING
          next TYPE REF TO zcl_hh_dp_vehicle,
      get_next_in_chain
        RETURNING
          VALUE(next) TYPE REF TO zcl_hh_dp_vehicle,
      get_current_state
        RETURNING
          VALUE(current_state) TYPE current_state_type,
      set_current_state
        IMPORTING
          current_state TYPE current_state_type,
      resume,
      stop.
  PROTECTED SECTION.
    DATA:
      tare_weight TYPE weight_type,
      next        TYPE REF TO zcl_hh_dp_vehicle.

  PRIVATE SECTION.
    CLASS-DATA:
      last_serial_value TYPE serial_type.

    DATA:
      license_plate                 TYPE license_plate_type,
      brand                         TYPE brand_type,
      model                         TYPE model_type,
      year                          TYPE year_type,
      color                         TYPE color_type,
      location                      TYPE location_type,
      speed                         TYPE speed_type,
      speed_unit                    TYPE speed_unit_type,
      weight_unit                   TYPE weight_unit_type,
      serial_number                 TYPE serial_type,
      navigation_type               TYPE navigator_type,
      navigation_unit               TYPE REF TO zif_hh_dp_simple_navigation,
      time_started_moving           TYPE time_stamp_type,
      current_state                 TYPE current_state_type,
      distance_traveled_before_stop TYPE odometer_type.

    CLASS-METHODS:
      get_serial_number
        RETURNING
          VALUE(serial_number) TYPE serial_type.
ENDCLASS.

CLASS zcl_hh_dp_vehicle IMPLEMENTATION.
  METHOD accelerate.
    ADD acceleration TO speed.
  ENDMETHOD.

  METHOD change_heading.
    me->navigation_unit->change_heading( turn ).
  ENDMETHOD.

  METHOD get_characteristics.
    serial_number = me->serial_number.
    license_plate = me->license_plate.
    brand = me->brand.
    model = me->model.
    year = me->year.
    color = me->color.
    location = me->location.
    speed_unit = me->speed_unit.
    weight_unit = me->weight_unit.
    navigation_type = me->navigation_type.
  ENDMETHOD.

  METHOD get_heading.
    heading = me->navigation_unit->get_heading( ).
  ENDMETHOD.

  METHOD get_speed.
    speed = me->speed.
  ENDMETHOD.

  METHOD constructor.

    CONSTANTS: selected TYPE checkbox VALUE 'X'.

    me->serial_number = me->get_serial_number( ).

    me->license_plate = license_plate.
    me->brand = brand.
    me->model = model.
    me->year = year.
    me->color = color.
    me->location = location.
    me->speed_unit = speed_unit.
    me->tare_weight = tare_weight.
    me->weight_unit = weight_unit.
    me->time_started_moving = time_started_moving.
    me->current_state = current_state.

    CASE selected.
      WHEN iphone_navigation.
        me->navigation_type = zcl_hh_dp_iphone_sextant=>class_id.
      WHEN basic_navigation.
        me->navigation_type = zcl_hh_dp_navigator=>class_id.
      WHEN gps_navigation.
        CASE vehicle_classification.
          WHEN zcl_hh_dp_truck=>class_id.
            me->navigation_type = zcl_hh_dp_comm_gps_adapter_b=>class_id.
          WHEN OTHERS.
            me->navigation_type = zcl_hh_dp_gps=>class_id.
        ENDCASE.
      WHEN OTHERS.
        me->navigation_type = zcl_hh_dp_dead_reckoning=>class_id.
    ENDCASE.

    zcl_hh_dp_vehicle_accs_store=>get_navigation_unit(
      EXPORTING
        heading          = heading
      IMPORTING
        navigation_unit  = me->navigation_unit
      CHANGING
        unit_type        = me->navigation_type
    ).

  ENDMETHOD.

  METHOD class_constructor.
    last_serial_value = 1000.
  ENDMETHOD.

  METHOD get_serial_number.
    ADD 1 TO last_serial_value.
    serial_number = last_serial_value.
  ENDMETHOD.

  METHOD assign_next_in_chain.
    me->next = next.
  ENDMETHOD.

  METHOD get_next_in_chain.
    next = me->next.
  ENDMETHOD.

  METHOD get_distance_traveled.
    CONSTANTS:
      seconds_in_one_hour TYPE int4 VALUE 3600.

    DATA:
      time_interval_in_seconds TYPE tzntstmpl,
      now                      TYPE time_stamp_type.

    case me->current_state.
      when state_cruising.
      when others.
        distance = me->distance_traveled_before_stop.
        return.
    endcase.

    GET TIME STAMP FIELD now.
    time_interval_in_seconds = cl_abap_tstmp=>subtract(
                                 tstmp1 = now
                                 tstmp2 = me->time_started_moving
                               ).
    distance = me->speed * time_interval_in_seconds /
               seconds_in_one_hour +
               me->distance_traveled_before_stop.


  ENDMETHOD.

  METHOD get_current_state.
    current_state = me->current_state.
  ENDMETHOD.

  METHOD set_current_state.
    me->current_state = current_state.
  ENDMETHOD.

  METHOD resume.
    data: now type timestamp.

    get time stamp field now.
    me->time_started_moving = now.
    me->set_current_state( state_cruising ).
  ENDMETHOD.

  METHOD stop.
    me->distance_traveled_before_stop = me->get_distance_traveled( ).
    me->set_current_state( state_stopped ).
  ENDMETHOD.

ENDCLASS.
