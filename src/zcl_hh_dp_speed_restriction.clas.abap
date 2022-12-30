CLASS zcl_hh_dp_speed_restriction DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    methods:
      impose_speed_restriction
        importing
          vehicle type ref to zcl_hh_dp_vehicle
          maximum_safe_speed type zcl_hh_dp_vehicle=>speed_type.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_speed_restriction IMPLEMENTATION.

  METHOD impose_speed_restriction.
    data: current_speed type zcl_hh_dp_vehicle=>speed_type,
          acceleration type zcl_hh_dp_vehicle=>speed_type.

    current_speed = vehicle->get_speed( ).
    acceleration = maximum_safe_speed - current_speed.

    if acceleration lt 0.
      vehicle->accelerate( acceleration ).
    endif.
  ENDMETHOD.

ENDCLASS.
