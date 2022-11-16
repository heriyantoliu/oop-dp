CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle.

  PUBLIC SECTION.
    TYPES:
      passengers_type TYPE int4.

    METHODS:
      constructor
        IMPORTING
          license_plate TYPE zcl_hh_dp_vehicle=>license_plate_type
          brand         TYPE zcl_hh_dp_vehicle=>brand_type
          model         TYPE zcl_hh_dp_vehicle=>model_type
          year          TYPE zcl_hh_dp_vehicle=>year_type
          color         TYPE zcl_hh_dp_vehicle=>color_type
          location      TYPE zcl_hh_dp_vehicle=>location_type
          speed_unit    TYPE zcl_hh_dp_vehicle=>speed_unit_type
          heading       TYPE zcl_hh_dp_navigator=>heading_type
          tare_weight   TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit   TYPE zcl_hh_dp_vehicle=>weight_unit_type
          passengers    TYPE passengers_type,
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
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
    ).

    me->passengers = passengers.

  ENDMETHOD.

  METHOD get_gross_weight.
    constants:
      average_adult_weight_in_lbs type zcl_hh_dp_vehicle=>weight_type value 180,
      average_adult_weight_unit type msehi value 'LB'.

    data:
      average_passenger_weight type zcl_hh_dp_vehicle=>weight_type,
      registered_weight_unit type msehi.

    average_passenger_weight = average_adult_weight_in_lbs.

    me->get_characteristics(
      importing
        weight_unit = registered_weight_unit
    ).

    if registered_weight_unit ne average_adult_weight_unit.
      call function 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = average_passenger_weight

          unit_in              = average_adult_weight_unit
          unit_out             = registered_weight_unit
        IMPORTING
          output               = average_passenger_weight
        EXCEPTIONS
          others               = 0
        .
    endif.

    gross_weight = me->tare_weight + me->passengers * average_passenger_weight.
  ENDMETHOD.



ENDCLASS.
