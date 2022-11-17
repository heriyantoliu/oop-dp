CLASS zcl_hh_dp_vehicle_accs_store DEFINITION
  PUBLIC
  ABSTRACT
  FINAL.

  PUBLIC SECTION.
    TYPES:
      navigation_unit_type TYPE seoclsname,
      vehicle_type         TYPE seoclsname.

    CLASS-METHODS:
      get_navigation_unit
        IMPORTING
          vehicle_classification TYPE vehicle_type
          heading                TYPE zif_hh_dp_simple_navigation=>heading_type
          basic_navigation       TYPE checkbox
          gps_navigation         TYPE checkbox
          no_navigation          TYPE checkbox
        EXPORTING
          navigation_unit        TYPE REF TO zif_hh_dp_simple_navigation
          unit_type              TYPE navigation_unit_type.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA:
      dead_reckoning_unit_shop TYPE REF TO zcl_hh_dp_nav_accsr_maker,
      navigator_unit_shop      TYPE REF TO zcl_hh_dp_nav_accsr_maker,
      gps_unit_shop            TYPE REF TO zcl_hh_dp_nav_accsr_maker,
      commercial_gps_unit_shop TYPE REF TO zcl_hh_dp_nav_accsr_maker.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_accs_store IMPLEMENTATION.
  METHOD get_navigation_unit.
    CONSTANTS: selected TYPE checkbox VALUE 'X'.

    DATA: unit_production_shop TYPE REF TO zcl_hh_dp_nav_accsr_maker.

    CASE selected.
      WHEN basic_navigation.
        IF navigator_unit_shop IS NOT BOUND.
          CREATE OBJECT navigator_unit_shop TYPE zcl_hh_dp_nav_unit_maker.
        ENDIF.

        unit_production_shop = zcl_hh_dp_vehicle_accs_store=>navigator_unit_shop.
      WHEN gps_navigation.
        CASE vehicle_classification.
          WHEN zcl_hh_dp_truck=>class_id.
            IF commercial_gps_unit_shop IS NOT BOUND.
              CREATE OBJECT commercial_gps_unit_shop TYPE zcl_hh_dp_comm_gps_unit_maker.
            ENDIF.
            unit_production_shop = commercial_gps_unit_shop.
          WHEN zcl_hh_dp_car=>class_id.
            IF gps_unit_shop IS NOT BOUND.
              CREATE OBJECT gps_unit_shop TYPE zcl_hh_dp_gps_unit_maker.
            ENDIF.

            unit_production_shop = zcl_hh_dp_vehicle_accs_store=>gps_unit_shop.
        ENDCASE.

      WHEN OTHERS.
        IF dead_reckoning_unit_shop IS NOT BOUND.
          CREATE OBJECT dead_reckoning_unit_shop TYPE zcl_hh_dp_dead_reck_unit_maker.
        ENDIF.

        unit_production_shop = zcl_hh_dp_vehicle_accs_store=>dead_reckoning_unit_shop.
    ENDCASE.

    unit_production_shop->make_navigation_unit(
      EXPORTING
        heading         = heading
      IMPORTING
        navigation_unit = navigation_unit
        unit_type       = unit_type
    ).
  ENDMETHOD.

ENDCLASS.
