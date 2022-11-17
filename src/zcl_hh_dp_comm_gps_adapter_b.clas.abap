CLASS zcl_hh_dp_comm_gps_adapter_b DEFINITION
  PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_simple_navigation.

    ALIASES:
      change_heading FOR zif_hh_dp_simple_navigation~change_heading,
      get_heading FOR zif_hh_dp_simple_navigation~get_heading.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_COMM_GPS_ADAPTER_B'.

    METHODS:
      constructor
        IMPORTING
          heading TYPE zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      degrees_90  TYPE int4 VALUE 90,
      degrees_180 TYPE int4 VALUE 180.

    DATA:
      adapted_gps TYPE REF TO zcl_hh_dp_commercial_gps.


ENDCLASS.



CLASS zcl_hh_dp_comm_gps_adapter_b IMPLEMENTATION.

  METHOD change_heading.
    DATA:
      bearing_change TYPE zcl_hh_dp_commercial_gps=>bearing_type.

    CHECK turn EQ zif_hh_dp_simple_navigation=>left_turn OR
          turn EQ zif_hh_dp_simple_navigation=>right_turn OR
          turn EQ zif_hh_dp_simple_navigation=>u_turn.

    CASE turn.
      WHEN zif_hh_dp_simple_navigation=>left_turn.
        SUBTRACT degrees_90 FROM bearing_change.
      WHEN zif_hh_dp_simple_navigation=>right_turn.
        ADD degrees_90 TO bearing_change.
      WHEN zif_hh_dp_simple_navigation=>u_turn.
        ADD degrees_180 TO bearing_change.
    ENDCASE.

    me->adapted_gps->change_bearing( bearing_change ).
  ENDMETHOD.

  METHOD get_heading.
    DATA: compass_offset TYPE int4.

    DATA(bearing) = me->adapted_gps->get_bearing( ).
    compass_offset = bearing / degrees_90.
    heading = zif_hh_dp_simple_navigation=>compass+compass_offset(1).
  ENDMETHOD.

  METHOD constructor.

    data: bearing type zcl_hh_dp_commercial_gps=>bearing_type.

    find heading in zif_hh_dp_simple_navigation=>compass
      match offset data(compass_offset).

    bearing = compass_offset * degrees_90.
    me->adapted_gps = new #( bearing ).
  ENDMETHOD.

ENDCLASS.
