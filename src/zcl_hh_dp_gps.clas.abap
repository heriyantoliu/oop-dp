CLASS zcl_hh_dp_gps DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_simple_navigation .

    ALIASES:
      change_heading FOR zif_hh_dp_simple_navigation~change_heading,
      get_heading FOR zif_hh_dp_simple_navigation~get_heading.

    constants:
      class_id type seoclsname value 'ZCL_HH_DP_GPS'.

    METHODS:
      constructor
        IMPORTING
          heading TYPE zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      degrees_90  TYPE int4 VALUE 90,
      degrees_180 TYPE int4 VALUE 180,
      degrees_360 TYPE int4 VALUE 360.

    data: bearing type int4.
ENDCLASS.



CLASS zcl_hh_dp_gps IMPLEMENTATION.


  METHOD change_heading.
    check turn eq zif_hh_dp_simple_navigation=>left_turn or
          turn eq zif_hh_dp_simple_navigation=>right_turn or
          turn eq zif_hh_dp_simple_navigation=>u_turn.

    case turn.
      when zif_hh_dp_simple_navigation=>left_turn.
        subtract degrees_90 from bearing.
      when zif_hh_dp_simple_navigation=>right_turn.
        add degrees_90 to bearing.
      when zif_hh_dp_simple_navigation=>u_turn.
        add degrees_180 to bearing.
    endcase.

    if bearing lt 0.
      add degrees_360 to bearing.
    endif.

    if bearing ge degrees_360.
      subtract degrees_360 from bearing.
    endif.
  ENDMETHOD.


  METHOD get_heading.
    data: compass_offset type int4.

    compass_offset = bearing / degrees_90.
    heading = zif_hh_dp_simple_navigation=>compass+compass_offset(1).
  ENDMETHOD.

  METHOD constructor.
    data: compass_offset type int4.

    find heading in zif_hh_dp_simple_navigation=>compass
      match offset compass_offset.
    bearing = compass_offset * degrees_90.
  ENDMETHOD.

ENDCLASS.
