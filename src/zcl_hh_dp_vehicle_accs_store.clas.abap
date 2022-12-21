CLASS zcl_hh_dp_vehicle_accs_store DEFINITION
  PUBLIC
  ABSTRACT
  FINAL.

  PUBLIC SECTION.
    TYPES:
      navigation_unit_type TYPE seoclsname.

    CLASS-METHODS:
      get_navigation_unit
        IMPORTING
          heading                TYPE zif_hh_dp_simple_navigation=>heading_type
        EXPORTING
          navigation_unit        TYPE REF TO zif_hh_dp_simple_navigation
        changing
          unit_type              TYPE navigation_unit_type.

  PROTECTED SECTION.
  private section.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_accs_store IMPLEMENTATION.
  METHOD get_navigation_unit.
    CONSTANTS: selected TYPE checkbox VALUE 'X'.

    DATA: unit_production_shop TYPE REF TO zcl_hh_dp_nav_accsr_maker.

    zcl_hh_dp_nav_mafct_agent=>singleton->locate_manufacturer(
      EXPORTING
        unit_type    = unit_type
      IMPORTING
        manufacturer = unit_production_shop
    ).


    unit_production_shop->make_navigation_unit(
      EXPORTING
        heading         = heading
      IMPORTING
        navigation_unit = navigation_unit
        unit_type       = unit_type
    ).
  ENDMETHOD.

ENDCLASS.
