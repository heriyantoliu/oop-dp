CLASS zcl_hh_dp_available_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_AVAILABLE_STATE'.

    methods:
      constructor,
      start REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      description type zif_hh_dp_state=>description_type value 'available'.
ENDCLASS.



CLASS zcl_hh_dp_available_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD start.
    constants: default_start_cruising_speed type zcl_hh_dp_vehicle=>speed_type value 5.

    data: next_state type ref to zif_hh_dp_state,
          now type timestamp.

    vehicle->set_dist_traveled_before_stop( 0 ).

    get TIME STAMP FIELD now.
    vehicle->set_time_started_moving( now ).
    vehicle->accelerate( default_start_cruising_speed ).
    vehicle->set_previous_state( me ).

    next_state = vehicle->get_cruising_state( ).
    vehicle->set_current_state( next_state ).

  ENDMETHOD.

ENDCLASS.
