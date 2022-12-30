CLASS zcl_hh_dp_vehicle_memento DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_hh_dp_vehicle .

  PUBLIC SECTION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      speed type zcl_hh_dp_vehicle=>speed_type,
      state type ref to zif_hh_dp_state.

    methods:
      constructor
        importing
          speed type int4
          state type ref to zif_hh_dp_state,
      get_speed
        returning
          value(speed) type int4,
      get_state
        returning
          value(state) type ref to zif_hh_dp_state.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_memento IMPLEMENTATION.
  METHOD constructor.
    me->speed = speed.
    me->state = state.
  ENDMETHOD.

  METHOD get_speed.
    speed = me->speed.
  ENDMETHOD.

  METHOD get_state.
    state = me->state.
  ENDMETHOD.

ENDCLASS.
