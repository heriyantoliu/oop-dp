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
          heading          TYPE zif_hh_dp_simple_navigation=>heading_type
          basic_navigation TYPE checkbox
          gps_navigation   TYPE checkbox
          no_navigation    TYPE checkbox
        EXPORTING
          navigation_unit  TYPE REF TO zif_hh_dp_simple_navigation
          unit_type        TYPE navigation_unit_type.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA:
      dead_reckoning_unit_shop TYPE REF TO zcl_hh_dp_nav_accsr_maker,
      navigator_unit_shop      TYPE REF TO zcl_hh_dp_nav_accsr_maker,
      gps_unit_shop            TYPE REF TO zcl_hh_dp_nav_accsr_maker.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_accs_store IMPLEMENTATION.
  METHOD get_navigation_unit.
    constants: selected type checkbox value 'X'.

    data: unit_production_shop type ref to zcl_hh_dp_nav_accsr_maker.

    case selected.
      when basic_navigation.
        if navigator_unit_shop is not bound.
          create object navigator_unit_shop type zcl_hh_dp_nav_unit_maker.
        endif.

        unit_production_shop = zcl_hh_dp_vehicle_accs_store=>navigator_unit_shop.
      when gps_navigation.
        if gps_unit_shop is not bound.
          create object gps_unit_shop type zcl_hh_dp_gps_unit_maker.
        endif.

        unit_production_shop = zcl_hh_dp_vehicle_accs_store=>gps_unit_shop.
      when others.
        if dead_reckoning_unit_shop is not bound.
          create object dead_reckoning_unit_shop type zcl_hh_dp_dead_reck_unit_maker.
        endif.

        unit_production_shop = zcl_hh_dp_vehicle_accs_store=>dead_reckoning_unit_shop.
      endcase.

      unit_production_shop->make_navigation_unit(
        EXPORTING
          heading         = heading
        IMPORTING
          navigation_unit = navigation_unit
          unit_type       = unit_type
      ).
  ENDMETHOD.

ENDCLASS.
