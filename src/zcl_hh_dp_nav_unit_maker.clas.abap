CLASS zcl_hh_dp_nav_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL.


  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_NAV_UNIT_MAKER'.
    methods:
      constructor
        importing
          successor type ref to zcl_hh_dp_nav_accsr_maker.
  PROTECTED SECTION.
    methods:
      create_unit REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_nav_unit_maker IMPLEMENTATION.
  METHOD create_unit.
    unit_type = zcl_hh_dp_navigator=>class_id.

    create object navigation_unit type (unit_type)
      exporting
        heading = heading.
  ENDMETHOD.

  METHOD constructor.

    super->constructor( successor ).
    me->speciality = zcl_hh_dp_navigator=>class_id.

  ENDMETHOD.

ENDCLASS.
