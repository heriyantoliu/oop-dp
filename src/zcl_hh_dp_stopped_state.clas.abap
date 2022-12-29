CLASS zcl_hh_dp_stopped_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_STOPPED_STATE'.

    METHODS:
      constructor
        IMPORTING
          vehicle TYPE REF TO zcl_hh_dp_vehicle,
      get_distance_traveled REDEFINITION,
      resume REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'stopped'.

ENDCLASS.



CLASS zcl_hh_dp_stopped_state IMPLEMENTATION.

  METHOD get_distance_traveled.
    distance = me->vehicle->get_dist_traveled_before_stop( ).
  ENDMETHOD.

  METHOD resume.
    data: now type timestamp,
          previous_state_speed type zcl_hh_dp_vehicle=>speed_type,
          previous_state type ref to zif_hh_dp_state.

    previous_state_speed = me->vehicle->get_previous_state_speed( ).
    me->vehicle->accelerate( previous_state_speed ).

    clear previous_state_speed.
    me->vehicle->set_previous_state_speed( previous_state_speed ).

    get time stamp field now.

    me->vehicle->set_time_started_moving( now ).
    previous_state = me->vehicle->get_previous_state( ).
    me->vehicle->set_current_state( previous_state ).
    me->vehicle->set_previous_state( me ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    me->vehicle = vehicle.
    me->descriptor = me->description.
  ENDMETHOD.
ENDCLASS.
