CLASS zcl_hh_dp_heavy_traffic_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_HEAVY_TRAFFIC_STATE'.

    METHODS:
      constructor,
      get_distance_traveled REDEFINITION,
      resume REDEFINITION,
      stop REDEFINITION,
      turn REDEFINITION,
      decelerate_05 REDEFINITION,
      decelerate_01 REDEFINITION,
      accelerate_01 REDEFINITION,
      accelerate_05 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'in heavy traffic'.

ENDCLASS.



CLASS zcl_hh_dp_heavy_traffic_state IMPLEMENTATION.

  METHOD get_distance_traveled.
    distance = me->calculated_distance_traveled( vehicle ).
  ENDMETHOD.

  METHOD resume.
    DATA: now                           TYPE timestamp,
          current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          reduced_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          next_state                    TYPE REF TO zif_hh_dp_state.

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = vehicle->get_speed( ).
    vehicle->set_previous_state_speed( current_speed ).
    vehicle->accelerate( current_speed ).

    GET TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).
    vehicle->set_previous_state( me ).

    next_state = vehicle->get_cruising_state( ).
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
    CONSTANTS: change_in_speed TYPE int4 VALUE '1'.

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

ENDCLASS.
