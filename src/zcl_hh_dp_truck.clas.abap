CLASS zcl_hh_dp_truck DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle.

  PUBLIC SECTION.
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
          heading       TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight   TYPE zcl_hh_dp_vehicle=>weight_type
          weight_unit   TYPE zcl_hh_dp_vehicle=>weight_unit_type
          cargo_weight  TYPE zcl_hh_dp_vehicle=>weight_type,
      get_description REDEFINITION,
      get_gross_weight REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      descriptor TYPE string VALUE 'Truck'.

    DATA:
      cargo_weight TYPE zcl_hh_dp_vehicle=>weight_type.
ENDCLASS.



CLASS zcl_hh_dp_truck IMPLEMENTATION.

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

    me->cargo_weight = cargo_weight.

  ENDMETHOD.

  METHOD get_description.
    description = me->descriptor.
  ENDMETHOD.

  METHOD get_gross_weight.
    gross_weight = me->tare_weight + me->cargo_weight.
  ENDMETHOD.

ENDCLASS.
