CLASS zcl_hh_dp_truck DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle.

  PUBLIC SECTION.
    methods:
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_truck IMPLEMENTATION.
  METHOD get_gross_weight.
    gross_weight = me->tare_weight.
  ENDMETHOD.

ENDCLASS.
