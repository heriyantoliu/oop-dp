CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle.

  PUBLIC SECTION.
    METHODS:
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_hh_dp_car IMPLEMENTATION.

  METHOD get_gross_weight.
    gross_weight = me->tare_weight.
  ENDMETHOD.

ENDCLASS.
