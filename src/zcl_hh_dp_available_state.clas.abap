CLASS zcl_hh_dp_available_state DEFINITION
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
      start REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'available'.

    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_available_state.

    METHODS:
      constructor.
ENDCLASS.



CLASS zcl_hh_dp_available_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD start.
    CONSTANTS: default_start_cruising_speed TYPE zcl_hh_dp_vehicle=>speed_type VALUE 5.

    DATA: next_state TYPE REF TO zif_hh_dp_state,
          now        TYPE timestamp.

    vehicle->set_dist_traveled_before_stop( 0 ).

    GET TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).
    vehicle->accelerate( default_start_cruising_speed ).
    vehicle->set_previous_state( me ).

    next_state = vehicle->get_cruising_state( ).
    vehicle->set_current_state( next_state ).

  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT zcl_hh_dp_available_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_available_state=>singleton.
  ENDMETHOD.

ENDCLASS.
