CLASS zcl_hh_dp_fleet_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

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
          has_option_ls     TYPE zcl_hh_dp_vehicle=>option_count,
      repeat_last_turn,
      reverse_last_turn,
      get_vehicle_memento
        importing
          vehicle type ref to zcl_hh_dp_vehicle
        returning
          value(vehicle_memento) type ref to zcl_hh_dp_vehicle_memento,
      set_vehicle_memento
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      discard_vehicle_mementos
        importing
          vehicle type ref to zcl_hh_dp_vehicle.

  PROTECTED SECTION.
  PRIVATE SECTION.
    types: begin of vehicle_memento_row,
             vehicle type ref to zcl_hh_dp_vehicle,
             vehicle_memento type ref to zcl_hh_dp_vehicle_memento,
           end of vehicle_memento_row,
           vehicle_memento_list type STANDARD TABLE OF vehicle_memento_row.
    data:
      first_vehicle_in_chain type ref to zcl_hh_dp_vehicle,
      vehicle_last_turn_command type ref to zif_hh_dp_command,
      vehicle_memento_stack type vehicle_memento_list.

    methods:
      constructor.
ENDCLASS.



CLASS zcl_hh_dp_fleet_manager IMPLEMENTATION.
  METHOD class_constructor.
    create object zcl_hh_dp_fleet_manager=>singleton.
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

    vehicle_entry->assign_next_in_chain( me->first_vehicle_in_chain ).
    me->first_vehicle_in_chain = vehicle_entry.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    create object zcl_hh_dp_fleet_manager=>singleton->vehicle_last_turn_command type zcl_hh_dp_vehicle_turn_command
      exporting
        vehicle = vehicle_entry
        last_turn = turn03.

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

    vehicle_entry->assign_next_in_chain( me->first_vehicle_in_chain ).
    me->first_vehicle_in_chain = vehicle_entry.

    vehicle_entry->accelerate( speed01 ).
    vehicle_entry->accelerate( speed02 ).
    vehicle_entry->accelerate( speed03 ).

    vehicle_entry->change_heading( turn01 ).
    vehicle_entry->change_heading( turn02 ).
    vehicle_entry->change_heading( turn03 ).

    create object zcl_hh_dp_fleet_manager=>singleton->vehicle_last_turn_command type zcl_hh_dp_vehicle_turn_command
      exporting
        vehicle = vehicle_entry
        last_turn = turn03.

    MESSAGE s398(00) WITH 'Truck entry registered for'
                          license_plate
                          space space.

  ENDMETHOD.

  METHOD create_iterator.
    create object iterator type zcl_hh_dp_fleet_iterator
      exporting
        first_fleet_entry = first_vehicle_in_chain.
  ENDMETHOD.

  METHOD repeat_last_turn.
    me->vehicle_last_turn_command->execute( ).
  ENDMETHOD.

  METHOD reverse_last_turn.
    me->vehicle_last_turn_command->undo( ).
  ENDMETHOD.

  METHOD constructor.
    create object zcl_hh_dp_fleet_manager=>singleton->vehicle_last_turn_command type zcl_hh_dp_null_command.
  ENDMETHOD.

  METHOD get_vehicle_memento.
    data: vehicle_memento_entry like line of vehicle_memento_stack.

    read table me->vehicle_memento_stack
      into vehicle_memento_entry
      with key vehicle = vehicle.
    if sy-subrc eq 0.
      delete me->vehicle_memento_stack index sy-tabix.
    endif.
    vehicle_memento = vehicle_memento_entry-vehicle_memento.
  ENDMETHOD.

  METHOD set_vehicle_memento.
    data: vehicle_memento_entry like line of vehicle_memento_stack.

    vehicle_memento_entry-vehicle = vehicle.
    vehicle_memento_entry-vehicle_memento = vehicle->create_memento( ).

    insert vehicle_memento_entry
      into vehicle_memento_stack
      index 1.

  ENDMETHOD.

  METHOD discard_vehicle_mementos.
    delete me->vehicle_memento_stack
      where vehicle eq vehicle.
  ENDMETHOD.

ENDCLASS.
