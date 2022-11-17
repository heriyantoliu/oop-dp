CLASS zcl_hh_dp_gps_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL.

  PUBLIC SECTION.
    METHODS:
      make_navigation_unit REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      create_unit REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      download_software_to_unit.
ENDCLASS.



CLASS zcl_hh_dp_gps_unit_maker IMPLEMENTATION.
  METHOD create_unit.
    unit_type = zcl_hh_dp_gps=>class_id.

    create object navigation_unit type (unit_type)
      exporting
        heading = heading.
  ENDMETHOD.

  METHOD download_software_to_unit.
    message i398(00) with 'Software successfully downloaded'
                          'to navigation unit'
                          space space.
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
    me->download_software_to_unit( ).
    me->register_unit( ).
  ENDMETHOD.

ENDCLASS.
