CLASS zcl_hh_dp_comm_gps_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL.

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_COMM_GPS_UNIT_MAKER'.
    methods:
      make_navigation_unit REDEFINITION,
      constructor
        importing
          successor type ref to zcl_hh_dp_nav_accsr_maker.

  PROTECTED SECTION.
    methods:
      create_unit redefinition.

  PRIVATE SECTION.
    methods:
      download_software_to_unit,
      download_bridge_data_to_unit.
ENDCLASS.



CLASS zcl_hh_dp_comm_gps_unit_maker IMPLEMENTATION.
  METHOD create_unit.
    unit_type = zcl_hh_dp_comm_gps_adapter_b=>class_id.
    create object navigation_unit type (unit_type)
      exporting
        heading = heading.
  ENDMETHOD.

  METHOD download_bridge_data_to_unit.
    message i398(00) with 'Bridge data, indicating locations'
                          'low overpasses and restricted weight'
                          'limits, successfully downloaded to'
                          'navigation unit'.
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
    me->download_bridge_data_to_unit( ).
    me->register_unit( ).
  ENDMETHOD.

  METHOD constructor.

    super->constructor( successor ).
    me->speciality = zcl_hh_dp_comm_gps_adapter_b=>class_id.

  ENDMETHOD.

ENDCLASS.
