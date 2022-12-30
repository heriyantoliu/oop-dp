CLASS zcl_hh_dp_sun_glare_mitigator DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_speed_restriction
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_visitor .

    aliases:
      visit_car for zif_hh_dp_visitor~visit_car,
      visit_truck for zif_hh_dp_visitor~visit_truck.

    methods:
      constructor
        importing
          affected_heading type zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      maximum_safe_speed_for_car type zcl_hh_dp_vehicle=>speed_type value 5.

    data:
      affected_heading type zif_hh_dp_simple_navigation=>heading_type.

    methods:
      is_heading_into_sun
        importing
          vehicle type ref to zcl_hh_dp_vehicle
        returning
          value(sun_glare_applicable) type abap_bool.
ENDCLASS.



CLASS zcl_hh_dp_sun_glare_mitigator IMPLEMENTATION.

  METHOD visit_car.
    if me->is_heading_into_sun( vehicle ) eq abap_false.
      return.
    endif.

    me->impose_speed_restriction(
      vehicle = vehicle
      maximum_safe_speed = me->maximum_safe_speed_for_car
    ).
  ENDMETHOD.

  METHOD visit_truck.
    data: current_state type ref to zif_hh_dp_state.

    if me->is_heading_into_sun( vehicle ) eq abap_false.
      return.
    endif.

    current_state = vehicle->get_current_state( ).
    current_state->stop( vehicle ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).
    me->affected_heading = affected_heading.

  ENDMETHOD.

  METHOD is_heading_into_sun.
    data: current_heading type zif_hh_dp_simple_navigation=>heading_type.

    current_heading = vehicle->get_heading( ).

    if current_heading eq me->affected_heading.
      sun_glare_applicable = abap_true.
    else.
      sun_glare_applicable = abap_false.
    endif.
  ENDMETHOD.

ENDCLASS.
