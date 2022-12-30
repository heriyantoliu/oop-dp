CLASS zcl_hh_dp_stopped_state DEFINITION
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
      make_available REDEFINITION,
      place_out_of_service REDEFINITION,
      resume REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'stopped'.

    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_stopped_state.

    METHODS:
      constructor.

ENDCLASS.



CLASS zcl_hh_dp_stopped_state IMPLEMENTATION.

  METHOD resume.
    DATA: now                  TYPE timestamp,
          previous_state_speed TYPE zcl_hh_dp_vehicle=>speed_type,
          previous_state       TYPE REF TO zif_hh_dp_state.

    previous_state_speed = vehicle->get_previous_state_speed( ).
    vehicle->accelerate( previous_state_speed ).

    CLEAR previous_state_speed.
    vehicle->set_previous_state_speed( previous_state_speed ).

    GET TIME STAMP FIELD now.

    vehicle->set_time_started_moving( now ).
    previous_state = vehicle->get_previous_state( ).
    vehicle->set_current_state( previous_state ).
    vehicle->set_previous_state( me ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    me->descriptor = me->description.
  ENDMETHOD.

  METHOD place_out_of_service.
    DATA: next_state TYPE REF TO zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = zcl_hh_dp_out_of_service_state=>get_state_object( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD make_available.
    DATA: next_state TYPE REF TO zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = zcl_hh_dp_available_state=>get_state_object( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT zcl_hh_dp_stopped_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_stopped_state=>singleton.
  ENDMETHOD.

ENDCLASS.
