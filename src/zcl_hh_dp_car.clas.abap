CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      passengers_type TYPE int4.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_CAR'.

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
          iphone_navigation       TYPE checkbox
          no_navigation           TYPE checkbox
          has_option_vl           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cc           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_mt           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_oo           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cr           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_xr           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_cg           TYPE zcl_hh_dp_vehicle=>option_count
          has_option_ls           TYPE zcl_hh_dp_vehicle=>option_count
        RETURNING
          VALUE(vehicle_instance) TYPE REF TO zcl_hh_dp_vehicle.

    METHODS:
      constructor
        IMPORTING
          license_plate     TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand             TYPE zcl_hh_dp_vehicle=>brand_type
          model             TYPE zcl_hh_dp_vehicle=>model_type
          year              TYPE zcl_hh_dp_vehicle=>year_type
          color             TYPE zcl_hh_dp_vehicle=>color_type
          location          TYPE zcl_hh_dp_vehicle=>location_type
          speed_unit        TYPE zcl_hh_dp_vehicle=>speed_unit_type
          heading           TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight       TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit       TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers        TYPE passengers_type
          basic_navigation  TYPE checkbox
          gps_navigation    TYPE checkbox
          iphone_navigation TYPE checkbox
          no_navigation     TYPE checkbox,
      get_description REDEFINITION,
      get_gross_weight REDEFINITION,
      accept REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      descriptor TYPE string VALUE 'Car'.

    DATA:
      passengers TYPE passengers_type.
ENDCLASS.

CLASS zcl_hh_dp_car IMPLEMENTATION.

  METHOD constructor.
    data: now type zcl_hh_dp_vehicle=>time_stamp_type.

    get time stamp field now.

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
      iphone_navigation = iphone_navigation
      no_navigation = no_navigation
      vehicle_classification = class_id
      time_started_moving = now
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

    DATA: options_stack        TYPE TABLE OF seoclsname,
          object_to_be_wrapped TYPE REF TO zcl_hh_dp_vehicle.

    CREATE OBJECT vehicle_instance TYPE zcl_hh_dp_car
      EXPORTING
        license_plate     = license_plate
        brand             = brand
        model             = model
        year              = year
        color             = color
        location          = location
        speed_unit        = speed_unit
        heading           = heading
        tare_weight       = tare_weight
        weight_unit       = weight_unit
        passengers        = passengers
        basic_navigation  = basic_navigation
        gps_navigation    = gps_navigation
        iphone_navigation = iphone_navigation
        no_navigation     = no_navigation.

    DO has_option_vl TIMES.
      APPEND zcl_hh_dp_vehicle_option_vl=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_cc TIMES.
      APPEND zcl_hh_dp_vehicle_option_cc=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_mt TIMES.
      APPEND zcl_hh_dp_vehicle_option_mt=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_oo TIMES.
      APPEND zcl_hh_dp_vehicle_option_oo=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_cr TIMES.
      APPEND zcl_hh_dp_vehicle_option_cr=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_xr TIMES.
      APPEND zcl_hh_dp_vehicle_option_xr=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_cg TIMES.
      APPEND zcl_hh_dp_vehicle_option_cg=>class_id
        TO options_stack.
    ENDDO.

    DO has_option_ls TIMES.
      APPEND zcl_hh_dp_vehicle_option_ls=>class_id
        TO options_stack.
    ENDDO.

    LOOP AT options_stack INTO DATA(options_entry).
      object_to_be_wrapped = vehicle_instance.

      CREATE OBJECT vehicle_instance TYPE (options_entry)
        EXPORTING
          wrapped_object = object_to_be_wrapped.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_description.
    description = me->descriptor.
  ENDMETHOD.

  METHOD accept.
    visitor->visit_car( me ).
  ENDMETHOD.

ENDCLASS.
