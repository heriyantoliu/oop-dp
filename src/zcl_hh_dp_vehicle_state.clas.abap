CLASS zcl_hh_dp_vehicle_state DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_hh_dp_state.

    ALIASES:
      get_description FOR zif_hh_dp_state~get_description,
      get_distance_traveled FOR zif_hh_dp_state~get_distance_traveled,
      place_out_of_service FOR zif_hh_dp_state~place_out_of_service,
      resume FOR zif_hh_dp_state~resume,
      slow FOR zif_hh_dp_state~slow,
      stop FOR zif_hh_dp_state~stop,
      turn FOR zif_hh_dp_state~turn,
      maintain FOR zif_hh_dp_state~maintain,
      make_available FOR zif_hh_dp_state~make_available,
      repair FOR zif_hh_dp_state~repair,
      start FOR zif_hh_dp_state~start,
      tow FOR zif_hh_dp_state~tow,
      assign_police_escort FOR zif_hh_dp_state~assign_police_escort,
      decelerate_05 FOR zif_hh_dp_state~decelerate_05,
      decelerate_01 FOR zif_hh_dp_state~decelerate_01,
      accelerate_01 FOR zif_hh_dp_state~accelerate_01,
      accelerate_05 FOR zif_hh_dp_state~accelerate_05.

    CLASS-METHODS:
      get_state_objects_count
        RETURNING
          VALUE(state_objects_count) TYPE int4.

    METHODS:
      constructor.

  PROTECTED SECTION.
    CONSTANTS:
      speed_change_factor TYPE p DECIMALS 2 VALUE '1.3'.

    DATA:
      descriptor TYPE zif_hh_dp_state=>description_type.

    METHODS:
      calculated_distance_traveled
        importing
          vehicle type ref to zcl_hh_dp_vehicle
        RETURNING
          VALUE(distance) TYPE zif_hh_dp_state=>odometer_type,
      halt
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      accelerate
        IMPORTING
          vehicle type ref to zcl_hh_dp_vehicle
          acceleration TYPE zcl_hh_dp_vehicle=>speed_type,
      engage_police_escort
        importing
          vehicle type ref to zcl_hh_dp_vehicle.
*      apply_high_winds_restriction
*        importing
*          vehicle type ref to zcl_hh_dp_vehicle,
*      apply_ice_restriction
*        importing
*          vehicle type ref to zcl_hh_dp_vehicle,
*      apply_speed_restriction
*        importing
*          vehicle type ref to zcl_hh_dp_vehicle
*          maximum_speed type zcl_hh_dp_vehicle=>speed_type.

  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE space.

    CLASS-DATA:
      state_objects_count TYPE int4.

ENDCLASS.



CLASS zcl_hh_dp_vehicle_state IMPLEMENTATION.

  METHOD constructor.
    add 1 to state_objects_count.
  ENDMETHOD.

  METHOD calculated_distance_traveled.
    CONSTANTS:
      seconds_in_one_hour TYPE int4 VALUE 3600.

    DATA: time_interval_in_seconds      TYPE tzntstmpl,
          now                           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          time_started_moving           TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          speed                         TYPE zcl_hh_dp_vehicle=>time_stamp_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type.

    time_started_moving = vehicle->get_time_started_moving( ).
    GET TIME STAMP FIELD now.
    speed = vehicle->get_speed( ).

    distance_traveled_before_stop = vehicle->get_dist_traveled_before_stop( ).

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
    distance = vehicle->get_dist_traveled_before_stop( ).
  ENDMETHOD.

  METHOD halt.
    DATA: current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          reduced_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          next_state                    TYPE REF TO zif_hh_dp_state.

    zcl_hh_dp_fleet_manager=>singleton->set_vehicle_memento( vehicle ).

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    current_speed = vehicle->get_speed( ).
    reduced_speed = 0 - current_speed.
    vehicle->accelerate( reduced_speed ).

    next_state = zcl_hh_dp_stopped_state=>get_state_object( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD accelerate.
    DATA: now                           TYPE timestamp,
          current_speed                 TYPE zcl_hh_dp_vehicle=>speed_type,
          new_speed                     TYPE zcl_hh_dp_vehicle=>speed_type,
          change_in_speed               TYPE zcl_hh_dp_vehicle=>speed_type,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type.

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    GET TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).

    current_speed = vehicle->get_speed( ).

    change_in_speed = acceleration.
    IF acceleration LT 0.
      new_speed = current_speed + change_in_speed - 1.
      IF new_speed LE 1.
        change_in_speed = 1 - current_speed.
      ENDIF.
    ENDIF.

    vehicle->accelerate( change_in_speed ).
  ENDMETHOD.

  METHOD resume.

  ENDMETHOD.

  METHOD slow.

  ENDMETHOD.

  METHOD stop.

  ENDMETHOD.

  METHOD turn.

  ENDMETHOD.

  METHOD place_out_of_service.

  ENDMETHOD.

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

  METHOD get_state_objects_count.
    state_objects_count = zcl_hh_dp_vehicle_state=>state_objects_count.
  ENDMETHOD.

  METHOD engage_police_escort.
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

ENDCLASS.
