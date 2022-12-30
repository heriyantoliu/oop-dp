CLASS zcl_hh_dp_out_of_service_state DEFINITION
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
      maintain REDEFINITION,
      tow REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'out of service'.

    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_out_of_service_state.

    METHODS:
      constructor.
ENDCLASS.



CLASS zcl_hh_dp_out_of_service_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD maintain.
    DATA: next_state TYPE REF TO zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = vehicle->get_in_shop_state( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD tow.
    DATA: next_state TYPE REF TO zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = vehicle->get_being_towed_state( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT zcl_hh_dp_out_of_service_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_out_of_service_state=>singleton.
  ENDMETHOD.

ENDCLASS.
