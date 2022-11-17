CLASS zcl_hh_dp_truck DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle
  CREATE PRIVATE.

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_TRUCK'.

    EVENTS: weight_exceeds_2_axle_limit.

    CLASS-METHODS:
      class_constructor,
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
          cargo_weight            TYPE zcl_hh_dp_vehicle=>weight_type
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
          cargo_weight     TYPE zcl_hh_dp_vehicle=>weight_type
          basic_navigation TYPE checkbox
          gps_navigation   TYPE checkbox
          no_navigation    TYPE checkbox,
      get_description REDEFINITION,
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      descriptor TYPE string VALUE 'Truck'.

    CLASS-DATA:
      weight_limit_for_2_axles  TYPE zcl_hh_dp_vehicle=>weight_type,
      weight_limit_2_axles_unit TYPE zcl_hh_dp_vehicle=>weight_unit_type.

    DATA:
      cargo_weight TYPE zcl_hh_dp_vehicle=>weight_type.

    METHODS:
      check_axle_weight.
ENDCLASS.



CLASS zcl_hh_dp_truck IMPLEMENTATION.

  METHOD class_constructor.
    zcl_hh_dp_trck_axle_wght_mntr=>get_2_axle_weight_limit(
      IMPORTING
        maximum_weight = weight_limit_for_2_axles
        weight_unit    = weight_limit_2_axles_unit
    ).
  ENDMETHOD.

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
      vehicle_classification = class_id
    ).

    me->cargo_weight = cargo_weight.

    me->check_axle_weight( ).

  ENDMETHOD.

  METHOD create.
    CREATE OBJECT vehicle_instance TYPE zcl_hh_dp_truck
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
        cargo_weight     = cargo_weight
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation.
  ENDMETHOD.

  METHOD get_description.
    description = me->descriptor.
  ENDMETHOD.

  METHOD get_gross_weight.
    gross_weight = me->tare_weight + me->cargo_weight.
  ENDMETHOD.

  METHOD check_axle_weight.
    DATA:
      normalized_gross_weight TYPE zcl_hh_dp_vehicle=>weight_type,
      registered_weight_unit  TYPE msehi.

    normalized_gross_weight = me->tare_weight + me->cargo_weight.

    me->get_characteristics(
      IMPORTING
        weight_unit = registered_weight_unit
    ).

    IF registered_weight_unit NE me->weight_limit_2_axles_unit.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = normalized_gross_weight
*         no_type_check        = 'X'
*         round_sign           = space
          unit_in  = registered_weight_unit
          unit_out = me->weight_limit_2_axles_unit
        IMPORTING
*         add_const            =
*         decimals =
*         denominator          =
*         numerator            =
          output   = normalized_gross_weight
        EXCEPTIONS
          OTHERS   = 0.
    ENDIF.

    IF normalized_gross_weight GT me->weight_limit_for_2_axles.
      RAISE EVENT weight_exceeds_2_axle_limit.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
