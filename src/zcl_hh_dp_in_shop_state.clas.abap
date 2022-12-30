CLASS zcl_hh_dp_in_shop_state DEFINITION
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
      make_available REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      description TYPE zif_hh_dp_state=>description_type VALUE 'in shop'.

    CLASS-DATA:
      singleton TYPE REF TO zcl_hh_dp_in_shop_state.

    METHODS:
      constructor.
ENDCLASS.



CLASS zcl_hh_dp_in_shop_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD make_available.
    DATA: next_state TYPE REF TO zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = vehicle->get_available_state( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

  METHOD class_constructor.
    CREATE OBJECT zcl_hh_dp_in_shop_state=>singleton.
  ENDMETHOD.

  METHOD get_state_object.
    state_object = zcl_hh_dp_in_shop_state=>singleton.
  ENDMETHOD.

ENDCLASS.
