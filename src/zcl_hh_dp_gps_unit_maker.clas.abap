CLASS zcl_hh_dp_gps_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL.

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_GPS_UNIT_MAKER'.
    METHODS:
      constructor
        importing
          successor type ref to zcl_hh_dp_nav_accsr_maker.

  PROTECTED SECTION.
    METHODS:
      create_unit REDEFINITION,
      prepare_unit_for_installation redefinition.

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

  METHOD constructor.
    super->constructor( successor ).
    me->speciality = zcl_hh_dp_gps=>class_id.
  ENDMETHOD.

  METHOD prepare_unit_for_installation.
    me->download_software_to_unit( ).
  ENDMETHOD.

ENDCLASS.
