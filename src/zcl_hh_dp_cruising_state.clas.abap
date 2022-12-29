CLASS zcl_hh_dp_cruising_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_CRUISING_STATE'.

    METHODS:
      constructor
        IMPORTING
          vehicle TYPE REF TO zcl_hh_dp_vehicle,
      get_distance_traveled REDEFINITION,
      slow REDEFINITION,
      stop REDEFINITION,
      turn REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'cruising'.

ENDCLASS.



CLASS zcl_hh_dp_cruising_state IMPLEMENTATION.

  METHOD get_distance_traveled.
    distance = me->calculated_distance_traveled( ).
  ENDMETHOD.

  METHOD slow.
    DATA: now                           TYPE timestamp,
          current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          reduced_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          next_state                    TYPE REF TO zif_hh_dp_state.

    distance_traveled_before_stop = me->get_distance_traveled( ).
    me->vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = me->vehicle->get_speed( ).
    me->vehicle->set_previous_state_speed( current_speed ).

    reduced_speed = 0 - current_speed / 2.

    me->vehicle->accelerate( reduced_speed ).

    get time stamp field now.
    me->vehicle->set_time_started_moving( now ).
    me->vehicle->set_previous_state( me ).

    next_state = me->vehicle->get_in_heavy_traffic_state( ).
    me->vehicle->set_current_state( next_state ).

  ENDMETHOD.

  METHOD stop.
    me->halt( ).
  ENDMETHOD.

  METHOD turn.
    me->vehicle->change_heading( turn ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    me->vehicle = vehicle.
    me->descriptor = me->description.
  ENDMETHOD.

ENDCLASS.
