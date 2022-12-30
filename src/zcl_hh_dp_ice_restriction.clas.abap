CLASS zcl_hh_dp_ice_restriction DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_speed_restriction
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_visitor .

    aliases:
      visit_car for zif_hh_dp_visitor~visit_car,
      visit_truck for zif_hh_dp_visitor~visit_truck.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      maximum_safe_speed_for_car type zcl_hh_dp_vehicle=>speed_type value 15,
      maximum_safe_speed_for_truck type zcl_hh_dp_vehicle=>speed_type value 10.
ENDCLASS.



CLASS zcl_hh_dp_ice_restriction IMPLEMENTATION.

  METHOD visit_car.
    me->impose_speed_restriction(
      vehicle = vehicle
      maximum_safe_speed = me->maximum_safe_speed_for_car
    ).
  ENDMETHOD.

  METHOD visit_truck.
    me->impose_speed_restriction(
      vehicle = vehicle
      maximum_safe_speed = me->maximum_safe_speed_for_truck
    ).
  ENDMETHOD.
ENDCLASS.
