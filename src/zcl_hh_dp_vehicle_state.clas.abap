CLASS zcl_hh_dp_vehicle_state DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_hh_dp_state.

    ALIASES:
      get_description FOR zif_hh_dp_state~get_description,
      get_distance_traveled FOR zif_hh_dp_state~get_distance_traveled,
      place_out_of_service for zif_hh_dp_state~place_out_of_service,
      resume FOR zif_hh_dp_state~resume,
      slow FOR zif_hh_dp_state~slow,
      stop FOR zif_hh_dp_state~stop,
      turn FOR zif_hh_dp_state~turn,
      maintain FOR zif_hh_dp_state~maintain,
      make_available FOR zif_hh_dp_state~make_available,
      repair FOR zif_hh_dp_state~repair,
      start FOR zif_hh_dp_state~start,
      tow FOR zif_hh_dp_state~tow,
      assign_police_escort for zif_hh_dp_state~assign_police_escort,
      decelerate_05 for zif_hh_dp_state~decelerate_05,
      decelerate_01 for zif_hh_dp_state~decelerate_01,
      accelerate_01 for zif_hh_dp_state~accelerate_01,
      accelerate_05 for zif_hh_dp_state~accelerate_05.

  PROTECTED SECTION.
    constants:
      speed_change_factor type p decimals 2 value '1.3'.

    DATA:
      vehicle    TYPE REF TO zcl_hh_dp_vehicle,
      descriptor TYPE zif_hh_dp_state=>description_type.

    METHODS:
      calculated_distance_traveled
        RETURNING
          VALUE(distance) TYPE zif_hh_dp_state=>odometer_type,
      halt,
      accelerate
        importing
          acceleration type zcl_hh_dp_vehicle=>speed_type.

  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE space.

ENDCLASS.



CLASS zcl_hh_dp_vehicle_state IMPLEMENTATION.
  METHOD calculated_distance_traveled.
    CONSTANTS:
      seconds_in_one_hour TYPE int4 VALUE 3600.

    DATA: time_interval_in_seconds      TYPE tzntstmpl,
          now                           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          time_started_moving           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          speed                         TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type.

    time_started_moving = me->vehicle->get_time_started_moving( ).
    GET TIME STAMP FIELD now.
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
  METHOD get_description.
    description = me->descriptor.
  ENDMETHOD.

  METHOD get_distance_traveled.
    distance = me->vehicle->get_dist_traveled_before_stop( ).
  ENDMETHOD.

  METHOD halt.
    data: current_speed type zcl_hh_dp_vehicle=>speed_type,
          reduced_speed type zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop type zif_hh_dp_state=>odometer_type,
          next_state type ref to zif_hh_dp_state.

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

  METHOD accelerate.
    data: now type timestamp,
          current_speed type zcl_hh_dp_vehicle=>speed_type,
          new_speed type zcl_hh_dp_vehicle=>speed_type,
          change_in_speed type zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop type zif_hh_dp_state=>odometer_type.

    distance_traveled_before_stop = me->get_distance_traveled( ).
    me->vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    get TIME STAMP FIELD now.
    me->vehicle->set_time_started_moving( now ).

    current_speed = me->vehicle->get_speed( ).

    change_in_speed = acceleration.
    if acceleration lt 0.
      new_speed = current_speed + change_in_speed - 1.
      if new_speed le 1.
        change_in_speed = 1 - current_speed.
      endif.
    endif.

    me->vehicle->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD resume.

  ENDMETHOD.

  METHOD slow.

  ENDMETHOD.

  METHOD stop.

  ENDMETHOD.

  METHOD turn.

  ENDMETHOD.

  method place_out_of_service.

  endmethod.

  METHOD maintain.

  ENDMETHOD.

  METHOD make_available.

  ENDMETHOD.

  METHOD repair.

  ENDMETHOD.

  METHOD start.

  ENDMETHOD.

  METHOD tow.

  ENDMETHOD.

  METHOD accelerate_01.

  ENDMETHOD.

  METHOD accelerate_05.

  ENDMETHOD.

  METHOD assign_police_escort.

  ENDMETHOD.

  METHOD decelerate_01.

  ENDMETHOD.

  METHOD decelerate_05.

  ENDMETHOD.

ENDCLASS.
