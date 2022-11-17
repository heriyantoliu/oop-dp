CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      passengers_type TYPE int4.

    CLASS-METHODS:
      create
        IMPORTING
          license_plate           TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand                   TYPE zcl_hh_dp_vehicle=>brand_type
          model                   TYPE zcl_hh_dp_vehicle=>model_type
          year                    TYPE zcl_hh_dp_vehicle=>year_type
          color                   TYPE zcl_hh_dp_vehicle=>color_type
          location                TYPE zcl_hh_dp_vehicle=>location_type
          speed_unit              TYPE zcl_hh_dp_vehicle=>speed_unit_type
          heading                 TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight             TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit             TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers              TYPE passengers_type
          basic_navigation        TYPE checkbox
          gps_navigation          TYPE checkbox
          no_navigation           TYPE checkbox
        RETURNING
          VALUE(vehicle_instance) TYPE REF TO zcl_hh_dp_vehicle.

    METHODS:
      constructor
        IMPORTING
          license_plate    TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand            TYPE zcl_hh_dp_vehicle=>brand_type
          model            TYPE zcl_hh_dp_vehicle=>model_type
          year             TYPE zcl_hh_dp_vehicle=>year_type
          color            TYPE zcl_hh_dp_vehicle=>color_type
          location         TYPE zcl_hh_dp_vehicle=>location_type
          speed_unit       TYPE zcl_hh_dp_vehicle=>speed_unit_type
          heading          TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight      TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit      TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers       TYPE passengers_type
          basic_navigation TYPE checkbox
          gps_navigation   TYPE checkbox
          no_navigation    TYPE checkbox,
      get_description REDEFINITION,
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      descriptor TYPE string VALUE 'Car'.

    DATA:
      passengers TYPE passengers_type.
ENDCLASS.

CLASS zcl_hh_dp_car IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
      license_plate = license_plate
      brand = brand
      model = model
      year = year
      color = color
      location = location
      speed_unit = speed_unit
      heading = heading
      tare_weight = tare_weight
      weight_unit = weight_unit
      basic_navigation = basic_navigation
      gps_navigation = gps_navigation
      no_navigation = no_navigation
    ).

    me->passengers = passengers.

  ENDMETHOD.

  METHOD get_gross_weight.
    CONSTANTS:
      average_adult_weight_in_lbs TYPE zcl_hh_dp_vehicle=>weight_type VALUE 180,
      average_adult_weight_unit   TYPE msehi VALUE 'LB'.

    DATA:
      average_passenger_weight TYPE zcl_hh_dp_vehicle=>weight_type,
      registered_weight_unit   TYPE msehi.

    average_passenger_weight = average_adult_weight_in_lbs.

    me->get_characteristics(
      IMPORTING
        weight_unit = registered_weight_unit
    ).

    IF registered_weight_unit NE average_adult_weight_unit.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = average_passenger_weight
          unit_in  = average_adult_weight_unit
          unit_out = registered_weight_unit
        IMPORTING
          output   = average_passenger_weight
        EXCEPTIONS
          OTHERS   = 0.
    ENDIF.

    gross_weight = me->tare_weight + me->passengers * average_passenger_weight.
  ENDMETHOD.

  METHOD create.
    CREATE OBJECT vehicle_instance TYPE zcl_hh_dp_car
      EXPORTING
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
        no_navigation    = no_navigation.
  ENDMETHOD.

  METHOD get_description.
    description = me->descriptor.
  ENDMETHOD.

ENDCLASS.
