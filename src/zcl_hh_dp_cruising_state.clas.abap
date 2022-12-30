CLASS zcl_hh_dp_cruising_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    class-methods:
      class_constructor,
      get_state_object
        returning
          value(state_object) type ref to zif_hh_dp_state.

    METHODS:
      get_distance_traveled REDEFINITION,
      slow REDEFINITION,
      stop REDEFINITION,
      turn REDEFINITION,
      assign_police_escort REDEFINITION,
      decelerate_05 REDEFINITION,
      decelerate_01 REDEFINITION,
      accelerate_01 REDEFINITION,
      accelerate_05 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'cruising'.

    class-data:
      singleton type ref to zcl_hh_dp_cruising_state.

    methods:
      constructor.

ENDCLASS.

CLASS zcl_hh_dp_cruising_state IMPLEMENTATION.

  METHOD get_distance_traveled.
    distance = me->calculated_distance_traveled( vehicle ).
  ENDMETHOD.

  METHOD slow.
    DATA: now                           TYPE timestamp,
          current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          reduced_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          next_state                    TYPE REF TO zif_hh_dp_state.

    zcl_hh_dp_fleet_manager=>singleton->set_vehicle_memento( vehicle ).

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = vehicle->get_speed( ).

    reduced_speed = 0 - current_speed / 2.

    vehicle->accelerate( reduced_speed ).

    get time stamp field now.
    vehicle->set_time_started_moving( now ).

    next_state = zcl_hh_dp_heavy_traffic_state=>get_state_object( ).
    vehicle->set_current_state( next_state ).

  ENDMETHOD.

  METHOD stop.
    me->halt( vehicle ).
  ENDMETHOD.

  METHOD turn.
    vehicle->change_heading( turn ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    me->descriptor = me->description.
  ENDMETHOD.

  METHOD accelerate_01.
    constants: change_in_speed type int4 value '1'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD accelerate_05.
    constants: change_in_speed type int4 value '5'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD assign_police_escort.
    data: now type timestamp,
          current_speed type zcl_hh_dp_vehicle=>speed_type,
          increased_speed type zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop type zif_hh_dp_state=>odometer_type,
          next_state type ref to zif_hh_dp_state.

    zcl_hh_dp_fleet_manager=>singleton->set_vehicle_memento( vehicle ).

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = vehicle->get_speed( ).

    increased_speed = current_speed * zcl_hh_dp_vehicle_state=>speed_change_factor.
    subtract current_speed from increased_speed.
    vehicle->accelerate( increased_speed ).

    get TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).
    next_state = zcl_hh_dp_police_escort_state=>get_state_object( ).
    vehicle->set_current_state( next_state ).

  ENDMETHOD.

  METHOD decelerate_01.
    constants: change_in_speed type int4 value '-1'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD decelerate_05.
    constants: change_in_speed type int4 value '-5'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD class_constructor.
    create object zcl_hh_dp_cruising_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_cruising_state=>singleton.
  ENDMETHOD.

ENDCLASS.
