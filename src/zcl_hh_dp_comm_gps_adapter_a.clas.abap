CLASS zcl_hh_dp_comm_gps_adapter_a DEFINITION
  PUBLIC
  inheriting from zcl_hh_dp_commercial_gps
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_simple_navigation .

    aliases:
      change_heading
        for zif_hh_dp_simple_navigation~change_heading,
      get_heading
        for zif_hh_dp_simple_navigation~get_heading.

    constants:
      class_id type seoclsname value 'ZCL_HH_DP_COMM_GPS_ADAPTER_A'.

    methods:
      constructor
        importing
          heading type zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      degrees_90 type int4 value 90,
      degrees_180 type int4 value 180,
      degrees_360 type int4 value 360.

    data:
      bearing type int4.
ENDCLASS.



CLASS zcl_hh_dp_comm_gps_adapter_a IMPLEMENTATION.

  METHOD change_heading.
    data: bearing_change type bearing_type.

    check turn eq zif_hh_dp_simple_navigation=>left_turn or
          turn eq zif_hh_dp_simple_navigation=>right_turn or
          turn eq zif_hh_dp_simple_navigation=>u_turn.

    case turn.
      when zif_hh_dp_simple_navigation=>left_turn.
        subtract degrees_90 from me->bearing.
      when zif_hh_dp_simple_navigation=>right_turn.
        add degrees_90 to me->bearing.
      when zif_hh_dp_simple_navigation=>u_turn.
        add degrees_180 to me->bearing.
    endcase.

    me->change_bearing( bearing_change ).

  ENDMETHOD.

  METHOD get_heading.
    data: compass_offset type int4.

    compass_offset = me->bearing / degrees_90.
    heading = zif_hh_dp_simple_navigation=>compass+compass_offset(1).
  ENDMETHOD.

  METHOD constructor.
    data: bearing type zcl_hh_dp_commercial_gps=>bearing_type.

    find heading in zif_hh_dp_simple_navigation=>compass
      match offset data(compass_offset).

    bearing = compass_offset * degrees_90.

    super->constructor( bearing ).
  ENDMETHOD.

ENDCLASS.
