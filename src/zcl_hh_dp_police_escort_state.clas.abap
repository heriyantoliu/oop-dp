CLASS zcl_hh_dp_police_escort_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      get_state_object
        RETURNING
          VALUE(state_object) TYPE REF TO zif_hh_dp_state.

    METHODS:
      get_distance_traveled REDEFINITION,
      resume REDEFINITION,
      stop REDEFINITION,
      turn REDEFINITION,
      decelerate_05 REDEFINITION,
      decelerate_01 REDEFINITION,
      accelerate_01 REDEFINITION,
      accelerate_05 REDEFINITION,
      impose_high_winds_restriction REDEFINITION,
      impose_ice_restriction REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'police escort'.

    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_police_escort_state.

    METHODS:
      constructor.
ENDCLASS.



CLASS zcl_hh_dp_police_escort_state IMPLEMENTATION.
  METHOD accelerate_01.
    CONSTANTS: change_in_speed TYPE int4 VALUE '1'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD accelerate_05.
    CONSTANTS: change_in_speed TYPE int4 VALUE '5'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD decelerate_01.
    CONSTANTS: change_in_speed TYPE int4 VALUE '-1'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD decelerate_05.
    CONSTANTS: change_in_speed TYPE int4 VALUE '-5'.

    me->accelerate(
      vehicle      = vehicle
      acceleration = change_in_speed
    ).
  ENDMETHOD.

  METHOD get_distance_traveled.
    distance = me->calculated_distance_traveled( vehicle ).
  ENDMETHOD.

  METHOD resume.
    DATA: now                           TYPE timestamp,
          distance_traveled_before_stop TYPE zif_hh_dp_state=>odometer_type,
          vehicle_memento type ref to zcl_hh_dp_vehicle_memento.

    distance_traveled_before_stop = me->get_distance_traveled( vehicle ).
    vehicle->set_dist_traveled_before_stop( distance_traveled_before_stop ).

    GET TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).

    vehicle_memento = zcl_hh_dp_fleet_manager=>singleton->get_vehicle_memento( vehicle ).
    vehicle->reset_using_memento( vehicle_memento ).

  ENDMETHOD.

  METHOD stop.
    me->halt( vehicle ).
  ENDMETHOD.

  METHOD turn.
    vehicle->change_heading( turn ).
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT zcl_hh_dp_police_escort_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_police_escort_state=>singleton.
  ENDMETHOD.

  METHOD impose_high_winds_restriction.
    me->apply_high_winds_restriction( vehicle ).
  ENDMETHOD.

  METHOD impose_ice_restriction.
    me->apply_ice_restriction( vehicle ).
  ENDMETHOD.

ENDCLASS.
