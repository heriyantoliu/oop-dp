CLASS zcl_hh_dp_police_escort_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_POLICE_ESCORT_STATE'.

    methods:
      constructor
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
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
    constants:
      description type zif_hh_dp_state=>description_type value 'police escort'.
ENDCLASS.



CLASS zcl_hh_dp_police_escort_state IMPLEMENTATION.
  METHOD accelerate_01.
    constants: change_in_speed type int4 value '1'.

    me->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD accelerate_05.
    constants: change_in_speed type int4 value '5'.

    me->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).
    me->vehicle = vehicle.
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD decelerate_01.
    constants: change_in_speed type int4 value '-01'.

    me->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD decelerate_05.
    constants: change_in_speed type int4 value '-05'.

    me->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD get_distance_traveled.
    distance = me->calculated_distance_traveled( ).
  ENDMETHOD.

  METHOD resume.
    data: now type timestamp,
          current_speed type zcl_hh_dp_vehicle=>speed_type,
          decreased_speed type zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop type zif_hh_dp_state=>odometer_type,
          next_state type ref to zif_hh_dp_state.

    distance_traveled_before_stop = me->get_distance_traveled( ).
    me->vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = me->vehicle->get_speed( ).
    me->vehicle->set_previous_state_speed( current_speed ).

    decreased_speed = current_speed / zcl_hh_dp_vehicle_state=>speed_change_factor.
    subtract current_speed from decreased_speed.
    me->vehicle->accelerate( decreased_speed ).

    get TIME STAMP FIELD now.
    me->vehicle->set_time_started_moving( now ).
    me->vehicle->set_previous_state( me ).

    next_state = me->vehicle->get_cruising_state( ).
    me->vehicle->set_current_state( next_state ).
  ENDMETHOD.
  METHOD stop.
    me->halt( ).
  ENDMETHOD.

  METHOD turn.
    me->vehicle->change_heading( turn ).
  ENDMETHOD.

ENDCLASS.