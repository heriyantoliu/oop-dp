CLASS zcl_hh_dp_commercial_gps DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    types:
      bearing_type type int4.

    methods:
      constructor
        importing
          bearing type bearing_type,
      change_bearing
        importing
          change type bearing_type,
      get_bearing
        exporting
          bearing type bearing_type.

  private section.
    constants:
      degrees_360 type int4 value 360.

    data:
      bearing type bearing_type.

ENDCLASS.



CLASS zcl_hh_dp_commercial_gps IMPLEMENTATION.

  METHOD constructor.
    me->bearing = bearing.
  ENDMETHOD.

  METHOD change_bearing.
    add change to me->bearing.

    if me->bearing lt 0.
      add degrees_360 to me->bearing.
    endif.

    if me->bearing ge degrees_360.
      subtract degrees_360 from me->bearing.
    endif.
  ENDMETHOD.

  METHOD get_bearing.
    bearing = me->bearing.
  ENDMETHOD.

ENDCLASS.
