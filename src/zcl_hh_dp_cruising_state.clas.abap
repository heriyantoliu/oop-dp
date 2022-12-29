CLASS zcl_hh_dp_cruising_state DEFINITION
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
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_CRUISING_STATE'.

    METHODS:
      constructor
        IMPORTING
          vehicle TYPE REF TO zcl_hh_dp_vehicle.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'cruising'.

    DATA:
      vehicle TYPE REF TO zcl_hh_dp_vehicle.
ENDCLASS.



CLASS zcl_hh_dp_cruising_state IMPLEMENTATION.


  METHOD get_description.
    description = me->description.
  ENDMETHOD.

  METHOD get_distance_traveled.
    CONSTANTS:
      seconds_in_one_hour TYPE int4 VALUE 3600.

    DATA:
      time_interval_in_seconds      TYPE tzntstmpl,
      now                           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
      time_started_moving           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
      speed                         TYPE zcl_hh_dp_vehicle=>speed_type,
      distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type.

    time_started_moving = me->vehicle->get_time_started_moving( ).
    get time stamp field now.
    speed = me->vehicle->get_speed( ).
    distance_traveled_before_stop = me->vehicle->get_dist_traveled_before_stop( ).

    time_interval_in_seconds = cl_abap_tstmp=>subtract(
                                 tstmp1 = now
                                 tstmp2 = time_started_moving
                               ).
    distance = speed *
               time_interval_in_seconds /
               seconds_in_one_hour +
               distance_traveled_before_stop.

  ENDMETHOD.

  METHOD resume.
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
    DATA: now                           TYPE timestamp,
          current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          reduced_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          next_state                    TYPE REF TO zif_hh_dp_state.

    distance_traveled_before_stop = me->get_distance_traveled( ).

    me->vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = me->vehicle->get_speed( ).
    me->vehicle->set_previous_state_speed( current_speed ).

    reduced_speed = 0 - current_speed.

    me->vehicle->accelerate( reduced_speed ).

    me->vehicle->set_previous_state( me ).

    next_state = me->vehicle->get_stopped_state( ).
    me->vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD turn.
    me->vehicle->change_heading( turn ).
  ENDMETHOD.

  METHOD constructor.
    me->vehicle = vehicle.
  ENDMETHOD.

ENDCLASS.
