CLASS zcl_hh_dp_nav_accsr_maker DEFINITION
  PUBLIC
  abstract.

  PUBLIC SECTION.
    types:
      navigation_unit_type type seoclsname.

    methods:
      make_navigation_unit final
        importing
          heading type zif_hh_dp_simple_navigation=>heading_type
        exporting
          navigation_unit type ref to zif_hh_dp_simple_navigation
          unit_type type navigation_unit_type,
      constructor
        importing
          successor type ref to zcl_hh_dp_nav_accsr_maker,
      locate_manufacturer
        importing
          unit_type type navigation_unit_type
        exporting
          manufacturer type ref to zcl_hh_dp_nav_accsr_maker.
  PROTECTED SECTION.
    data:
      speciality type seoclsname,
      successor type ref to zcl_hh_dp_nav_accsr_maker.
    methods:
      calibrate_unit,
      create_unit abstract
        importing
          heading type zif_hh_dp_simple_navigation=>heading_type
        exporting
          navigation_unit type ref to zif_hh_dp_simple_navigation
          unit_type type navigation_unit_type,
      prepare_unit_for_installation,
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
    me->prepare_unit_for_installation( ).
    me->register_unit( ).
  ENDMETHOD.

  METHOD register_unit.
    message i398(00) with 'Navigation unit has been registered. Thank you'
                          space space space.
  ENDMETHOD.

  METHOD constructor.
    me->successor = successor.
  ENDMETHOD.

  METHOD locate_manufacturer.
    if unit_type eq me->speciality or
       me->successor is not bound.
      manufacturer = me.
    else.
      me->successor->locate_manufacturer(
        EXPORTING
          unit_type    = unit_type
        IMPORTING
          manufacturer = manufacturer
      ).
    endif.
  ENDMETHOD.

  METHOD prepare_unit_for_installation.

  ENDMETHOD.

ENDCLASS.
