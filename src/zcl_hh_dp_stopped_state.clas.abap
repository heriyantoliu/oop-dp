CLASS zcl_hh_dp_stopped_state DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_state .

    ALIASES:
      get_description FOR zif_hh_dp_state~get_description,
      get_distance_traveled FOR zif_hh_dp_state~get_distance_traveled,
      resume FOR zif_hh_dp_state~resume,
      slow FOR zif_hh_dp_state~slow,
      stop FOR zif_hh_dp_state~stop,
      turn FOR zif_hh_dp_state~turn.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_STOPPED_STATE'.

    METHODS:
      constructor
        IMPORTING
          vehicle TYPE REF TO zcl_hh_dp_vehicle.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'stopped'.

    DATA:
      vehicle TYPE REF TO zcl_hh_dp_vehicle.
ENDCLASS.



CLASS zcl_hh_dp_stopped_state IMPLEMENTATION.
  METHOD get_description.
    description = me->description.
  ENDMETHOD.

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

  METHOD slow.
  ENDMETHOD.

  METHOD stop.
  ENDMETHOD.

  METHOD turn.
  ENDMETHOD.

  METHOD constructor.
    me->vehicle = vehicle.
  ENDMETHOD.
ENDCLASS.
