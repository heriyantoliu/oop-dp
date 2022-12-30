CLASS zcl_hh_dp_being_towed_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_BEING_TOWED_STATE'.

    methods:
      constructor,
      repair REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      description type zif_hh_dp_state=>description_type value 'being towed'.
ENDCLASS.



CLASS zcl_hh_dp_being_towed_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->descriptor = me->description.

  ENDMETHOD.

  METHOD repair.
    data: next_state type ref to zif_hh_dp_state.

    vehicle->set_previous_state( me ).
    next_state = vehicle->get_in_shop_state( ).
    vehicle->set_current_state( next_state ).
  ENDMETHOD.

ENDCLASS.
