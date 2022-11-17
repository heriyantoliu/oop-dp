CLASS zcl_hh_dp_dead_reck_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL.

  PUBLIC SECTION.
  PROTECTED SECTION.
    methods:
      create_unit REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_dead_reck_unit_maker IMPLEMENTATION.
  METHOD create_unit.
    unit_type = zcl_hh_dp_dead_reckoning=>class_id.

    create object navigation_unit type (unit_type)
      exporting
        heading = heading.
  ENDMETHOD.

ENDCLASS.
