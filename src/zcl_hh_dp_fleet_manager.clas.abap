CLASS zcl_hh_dp_fleet_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    interfaces: zif_hh_dp_aggregate.

    aliases:
      create_iterator for zif_hh_dp_aggregate~create_iterator.

    class-data:
      singleton type ref to zcl_hh_dp_fleet_manager read-only.

    class-methods:
      class_constructor.

    METHODS:
      register_car_entry
        IMPORTING
          license_plate     TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand             TYPE zcl_hh_dp_vehicle=>brand_type
          year              TYPE zcl_hh_dp_vehicle=>year_type
          model             TYPE zcl_hh_dp_vehicle=>model_type
          color             TYPE zcl_hh_dp_vehicle=>color_type
          location          TYPE zcl_hh_dp_vehicle=>location_type
          heading           TYPE zif_hh_dp_simple_navigation=>heading_type
          turn01            TYPE zif_hh_dp_simple_navigation=>turn_type
          turn02            TYPE zif_hh_dp_simple_navigation=>turn_type
          turn03            TYPE zif_hh_dp_simple_navigation=>turn_type
          speed01           TYPE zcl_hh_dp_vehicle=>speed_type
          speed02           TYPE zcl_hh_dp_vehicle=>speed_type
          speed03           TYPE zcl_hh_dp_vehicle=>speed_type
          speed_unit        TYPE zcl_hh_dp_vehicle=>speed_unit_type
          tare_weight       TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit       TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers        TYPE zcl_hh_dp_car=>passengers_type
          basic_navigation  TYPE checkbox
          gps_navigation    TYPE checkbox
          iphone_navigation TYPE checkbox
          no_navigation     TYPE checkbox
          has_option_vl     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cc     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_mt     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_oo     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cr     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_xr     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cg     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_ls     TYPE zcl_hh_dp_vehicle=>option_count,
      register_truck_entry
        IMPORTING
          license_plate     TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand             TYPE zcl_hh_dp_vehicle=>brand_type
          year              TYPE zcl_hh_dp_vehicle=>year_type
          model             TYPE zcl_hh_dp_vehicle=>model_type
          color             TYPE zcl_hh_dp_vehicle=>color_type
          location          TYPE zcl_hh_dp_vehicle=>location_type
          heading           TYPE zif_hh_dp_simple_navigation=>heading_type
          turn01            TYPE zif_hh_dp_simple_navigation=>turn_type
          turn02            TYPE zif_hh_dp_simple_navigation=>turn_type
          turn03            TYPE zif_hh_dp_simple_navigation=>turn_type
          speed01           TYPE zcl_hh_dp_vehicle=>speed_type
          speed02           TYPE zcl_hh_dp_vehicle=>speed_type
          speed03           TYPE zcl_hh_dp_vehicle=>speed_type
          speed_unit        TYPE zcl_hh_dp_vehicle=>speed_unit_type
          tare_weight       TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit       TYPE zcl_hh_dp_vehicle=>weight_unit_type
          cargo_weight      TYPE zcl_hh_dp_vehicle=>weight_type
          basic_navigation  TYPE checkbox
          gps_navigation    TYPE checkbox
          iphone_navigation TYPE checkbox
          no_navigation     TYPE checkbox
          has_option_vl     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cc     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_mt     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_oo     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cr     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_xr     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cg     TYPE zcl_hh_dp_vehicle=>option_count
          has_option_ls     TYPE zcl_hh_dp_vehicle=>option_count.

  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      vehicle_stack type standard table of ref to zcl_hh_dp_vehicle.
ENDCLASS.



CLASS zcl_hh_dp_fleet_manager IMPLEMENTATION.
  METHOD class_constructor.
    singleton = new #( ).
  ENDMETHOD.

  METHOD register_car_entry.

    DATA(vehicle_entry) = zcl_hh_dp_car=>create(
        license_plate    = license_plate
        brand            = brand
        model            = model
        year             = year
        color            = color
        location         = location
        speed_unit       = speed_unit
        heading          = heading
        tare_weight      = tare_weight
        weight_unit      = weight_unit
        passengers       = passengers
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        iphone_navigation = iphone_navigation
        no_navigation    = no_navigation
        has_option_vl = has_option_vl
        has_option_cc = has_option_cc
        has_option_mt = has_option_mt
        has_option_oo = has_option_oo
        has_option_cr = has_option_cr
        has_option_xr = has_option_xr
        has_option_cg = has_option_cg
        has_option_ls = has_option_ls
    ).

    APPEND vehicle_entry TO me->vehicle_stack.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    MESSAGE s398(00) WITH 'Car entry registered for'
                          license_plate
                          space space.
  ENDMETHOD.

  METHOD register_truck_entry.

    DATA(vehicle_entry) = zcl_hh_dp_truck=>create(
      license_plate    = license_plate
      brand            = brand
      model            = model
      year             = year
      color            = color
      location         = location
      speed_unit       = speed_unit
      heading          = heading
      tare_weight      = tare_weight
      weight_unit      = weight_unit
      cargo_weight     = cargo_weight
      basic_navigation = basic_navigation
      gps_navigation   = gps_navigation
      iphone_navigation = iphone_navigation
      no_navigation    = no_navigation
      has_option_vl = has_option_vl
      has_option_cc = has_option_cc
      has_option_mt = has_option_mt
      has_option_oo = has_option_oo
      has_option_cr = has_option_cr
      has_option_xr = has_option_xr
      has_option_cg = has_option_cg
      has_option_ls = has_option_ls
    ).


    APPEND vehicle_entry TO me->vehicle_stack.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    MESSAGE s398(00) WITH 'Truck entry registered for'
                          license_plate
                          space space.

  ENDMETHOD.

  METHOD create_iterator.
    create object iterator type zcl_hh_dp_fleet_iterator
      exporting
        fleet_stack = vehicle_stack.
  ENDMETHOD.

ENDCLASS.
