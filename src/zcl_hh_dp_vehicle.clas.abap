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
      time_stamp_type       TYPE timestamp.

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
          time_started_moving    TYPE time_stamp_type DEFAULT 0,
      assign_next_in_chain
        IMPORTING
          next TYPE REF TO zcl_hh_dp_vehicle,
      get_next_in_chain
        RETURNING
          VALUE(next) TYPE REF TO zcl_hh_dp_vehicle,
      get_current_state
        RETURNING
          VALUE(current_state) TYPE REF TO zif_hh_dp_state,
      set_current_state
        IMPORTING
          current_state TYPE REF TO zif_hh_dp_state,
      get_previous_state
        RETURNING
          VALUE(previous_state) TYPE REF TO zif_hh_dp_state,
      set_previous_state
        IMPORTING
          previous_state TYPE REF TO zif_hh_dp_state,
      get_time_started_moving
        RETURNING
          VALUE(time_started_moving) TYPE time_stamp_type,
      set_time_started_moving
        IMPORTING
          time_started_moving TYPE time_stamp_type,
      get_dist_traveled_before_stop
        RETURNING
          VALUE(distance_traveled_before_stop) TYPE zif_hh_dp_state=>odometer_type,
      set_dist_traveled_before_stop
        IMPORTING
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
      get_previous_state_speed
        RETURNING
          VALUE(previous_state_speed) TYPE speed_type,
      set_previous_state_speed
        IMPORTING
          VALUE(previous_state_speed) TYPE speed_type,
      get_cruising_state
        RETURNING
          VALUE(cruising_state) TYPE REF TO zif_hh_dp_state,
      get_in_heavy_traffic_state
        RETURNING
          VALUE(in_heavy_traffic_state) TYPE REF TO zif_hh_dp_state,
      get_stopped_state
        RETURNING
          VALUE(stopped_state) TYPE REF TO zif_hh_dp_state,
      get_out_of_service_state
        RETURNING
          VALUE(out_of_service_state) TYPE REF TO zif_hh_dp_state,
      get_available_state
        RETURNING
          VALUE(available_state) TYPE REF TO zif_hh_dp_state,
      get_being_towed_state
        RETURNING
          VALUE(being_towed_state) TYPE REF TO zif_hh_dp_state,
      get_in_shop_state
        RETURNING
          VALUE(in_shop_state) TYPE REF TO zif_hh_dp_state,
      get_police_escort_state
        RETURNING
          VALUE(police_escort_state) TYPE REF TO zif_hh_dp_state.

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
      current_state                 TYPE REF TO zif_hh_dp_state,
      distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
      previous_state                TYPE REF TO zif_hh_dp_state,
      previous_state_speed          TYPE speed_type,
      cruising_state                TYPE REF TO zif_hh_dp_state,
      in_heavy_traffic_state        TYPE REF TO zif_hh_dp_state,
      stopped_state                 TYPE REF TO zif_hh_dp_state,
      out_of_service_state          TYPE REF TO zif_hh_dp_state,
      available_state               TYPE REF TO zif_hh_dp_state,
      being_towed_state             TYPE REF TO zif_hh_dp_state,
      in_shop_state                 TYPE REF TO zif_hh_dp_state,
      police_escort_state           TYPE REF TO zif_hh_dp_state.

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

    me->current_state = me->get_cruising_state( ).

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

  METHOD get_current_state.
    current_state = me->current_state.
  ENDMETHOD.

  METHOD set_current_state.
    me->current_state = current_state.
  ENDMETHOD.

  METHOD get_dist_traveled_before_stop.
    distance_traveled_before_stop = me->distance_traveled_before_stop.
  ENDMETHOD.

  METHOD get_previous_state.
    previous_state = me->previous_state.
  ENDMETHOD.

  METHOD get_previous_state_speed.
    previous_state_speed = me->previous_state_speed.
  ENDMETHOD.

  METHOD get_time_started_moving.
    time_started_moving = me->time_started_moving.
  ENDMETHOD.

  METHOD set_dist_traveled_before_stop.
    me->distance_traveled_before_stop = distance_traveled_before_stop.
  ENDMETHOD.

  METHOD set_previous_state.
    me->previous_state = previous_state.
  ENDMETHOD.

  METHOD set_previous_state_speed.
    me->previous_state_speed = previous_state_speed.
  ENDMETHOD.

  METHOD set_time_started_moving.
    me->time_started_moving = time_started_moving.
  ENDMETHOD.

  METHOD get_cruising_state.
    if me->cruising_state is not bound.
      create object me->cruising_state
        type (zcl_hh_dp_cruising_state=>class_id).
    endif.
    cruising_state = me->cruising_state.
  ENDMETHOD.
  METHOD get_in_heavy_traffic_state.
    if me->in_heavy_traffic_state is not bound.
      create object me->in_heavy_traffic_state
        type (zcl_hh_dp_heavy_traffic_state=>class_id).
    endif.
    in_heavy_traffic_state = me->in_heavy_traffic_state.
  ENDMETHOD.

  METHOD get_stopped_state.
    if me->stopped_state is not bound.
      create object me->stopped_state
        type (zcl_hh_dp_stopped_state=>class_id).
    endif.
    stopped_state = me->stopped_state.
  ENDMETHOD.

  METHOD get_out_of_service_state.
    if me->out_of_service_state is not bound.
      create object me->out_of_service_state
        type (zcl_hh_dp_out_of_service_state=>class_id).
    endif.
    out_of_service_state = me->out_of_service_state.
  ENDMETHOD.

  METHOD get_available_state.
    if me->available_state is not bound.
      create object me->available_state
        type (zcl_hh_dp_available_state=>class_id).
    endif.
    available_state = me->available_state.
  ENDMETHOD.

  METHOD get_being_towed_state.
    if me->being_towed_state is not bound.
      create object me->being_towed_state
        type (zcl_hh_dp_being_towed_state=>class_id).
    endif.
    being_towed_state = me->being_towed_state.
  ENDMETHOD.

  METHOD get_in_shop_state.
    if me->in_shop_state is not bound.
      create object me->in_shop_state
        type (zcl_hh_dp_in_shop_state=>class_id).
    endif.
    in_shop_state = me->in_shop_state.
  ENDMETHOD.

  METHOD get_police_escort_state.
    if me->police_escort_state is not bound.
      create object me->police_escort_state
        type (zcl_hh_dp_police_escort_state=>class_id).
    endif.
    police_escort_state = me->police_escort_state.
  ENDMETHOD.

ENDCLASS.
