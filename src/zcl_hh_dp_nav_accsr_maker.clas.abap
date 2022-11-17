CLASS zcl_hh_dp_nav_accsr_maker DEFINITION
  PUBLIC
  abstract.

  PUBLIC SECTION.
    types:
      navigation_unit_type type seoclsname.

    methods:
      make_navigation_unit
        importing
          heading type zif_hh_dp_simple_navigation=>heading_type
        exporting
          navigation_unit type ref to zif_hh_dp_simple_navigation
          unit_type type navigation_unit_type.
  PROTECTED SECTION.
    methods:
      calibrate_unit,
      create_unit abstract
        importing
          heading type zif_hh_dp_simple_navigation=>heading_type
        exporting
          navigation_unit type ref to zif_hh_dp_simple_navigation
          unit_type type navigation_unit_type,
      register_unit.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_nav_accsr_maker IMPLEMENTATION.
  METHOD calibrate_unit.
    message i398(00) with 'Navigation unit successfully calibrated'
                          space space space.
  ENDMETHOD.

  METHOD make_navigation_unit.
    me->create_unit(
      EXPORTING
        heading         = heading
      IMPORTING
        navigation_unit = navigation_unit
        unit_type       = unit_type
    ).

    me->calibrate_unit( ).
    me->register_unit( ).
  ENDMETHOD.

  METHOD register_unit.
    message i398(00) with 'Navigation unit has been registered. Thank you'
                          space space space.
  ENDMETHOD.

ENDCLASS.