CLASS zcl_hh_dp_iphone_unit_maker DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_nav_accsr_maker
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_IPHONE_UNIT_MAKER'.

    methods:
      constructor
        importing
          successor type ref to zcl_hh_dp_nav_accsr_maker.
  PROTECTED SECTION.
    methods:
      create_unit redefinition,
      prepare_unit_for_installation REDEFINITION.

  PRIVATE SECTION.
    methods:
      download_software_to_unit.
ENDCLASS.



CLASS zcl_hh_dp_iphone_unit_maker IMPLEMENTATION.
  METHOD constructor.

    super->constructor( successor = successor ).
    me->speciality = zcl_hh_dp_iphone_sextant=>class_id.

  ENDMETHOD.

  METHOD create_unit.
    unit_type = zcl_hh_dp_iphone_sextant=>class_id.
    create object navigation_unit type (unit_type)
      exporting
        heading = heading.
  ENDMETHOD.

  METHOD download_software_to_unit.
    message i398(00) with 'Software succesfully downloaded'
                          'to navigation unit'
                          space space.
  ENDMETHOD.

  METHOD prepare_unit_for_installation.
    me->download_software_to_unit( ).
  ENDMETHOD.

ENDCLASS.
