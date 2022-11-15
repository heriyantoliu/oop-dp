CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES:
      brand_type         TYPE f4txt,
      color_type         TYPE f4txt,
      location_type      TYPE f4txt,
      model_type         TYPE f4txt,
      license_plate_type TYPE f4txt,
      speed_type         TYPE int4,
      speed_unit_type    TYPE char3,
      year_type          TYPE num4.

    CLASS-METHODS:
      accelerate
        IMPORTING
          acceleration TYPE zcl_hh_dp_car=>speed_type,
      change_heading
        importing
          turn type zcl_hh_dp_navigator=>turn_type,
      get_characteristics
        EXPORTING
          license_plate TYPE zcl_hh_dp_car=>license_plate_type
          brand         TYPE zcl_hh_dp_car=>brand_type
          model         TYPE zcl_hh_dp_car=>model_type
          year          TYPE zcl_hh_dp_car=>year_type
          color         TYPE zcl_hh_dp_car=>color_type
          location      TYPE zcl_hh_dp_car=>location_type
          speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type,
      get_heading
        returning
          value(heading) type zcl_hh_dp_navigator=>heading_type,
      get_speed
        RETURNING
          VALUE(speed) TYPE zcl_hh_dp_car=>speed_type,
      set_characteristics
        IMPORTING
          license_plate TYPE zcl_hh_dp_car=>license_plate_type
          brand         TYPE zcl_hh_dp_car=>brand_type
          model         TYPE zcl_hh_dp_car=>model_type
          year          TYPE zcl_hh_dp_car=>year_type
          color         TYPE zcl_hh_dp_car=>color_type
          location      TYPE zcl_hh_dp_car=>location_type
          speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type,
      set_heading
        importing
          heading type zcl_hh_dp_navigator=>heading_type.

  PROTECTED SECTION.
  PRIVATE SECTION.


    CLASS-DATA:
      license_plate TYPE zcl_hh_dp_car=>license_plate_type,
      brand         TYPE zcl_hh_dp_car=>brand_type,
      model         TYPE zcl_hh_dp_car=>model_type,
      year          TYPE zcl_hh_dp_car=>year_type,
      color         TYPE zcl_hh_dp_car=>color_type,
      location      TYPE zcl_hh_dp_car=>location_type,
      speed         TYPE zcl_hh_dp_car=>speed_type,
      speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type.

ENDCLASS.



CLASS zcl_hh_dp_car IMPLEMENTATION.
  METHOD accelerate.
    ADD acceleration TO zcl_hh_dp_car=>speed.
  ENDMETHOD.

  METHOD change_heading.
    zcl_hh_dp_navigator=>change_heading( turn ).
  ENDMETHOD.

  METHOD get_characteristics.
    license_plate = zcl_hh_dp_car=>license_plate.
    brand = zcl_hh_dp_car=>brand.
    model = zcl_hh_dp_car=>model.
    year = zcl_hh_dp_car=>year.
    color = zcl_hh_dp_car=>color.
    location = zcl_hh_dp_car=>location.
    speed_unit = zcl_hh_dp_car=>speed_unit.
  ENDMETHOD.

  METHOD get_heading.
    heading = zcl_hh_dp_navigator=>get_heading( ).
  ENDMETHOD.

  METHOD get_speed.
    speed = zcl_hh_dp_car=>speed.
  ENDMETHOD.

  METHOD set_characteristics.
    zcl_hh_dp_car=>license_plate = license_plate.
    zcl_hh_dp_car=>brand = brand.
    zcl_hh_dp_car=>model = model.
    zcl_hh_dp_car=>year = year.
    zcl_hh_dp_car=>color = color.
    zcl_hh_dp_car=>location = location.
    zcl_hh_dp_car=>speed_unit = speed_unit.
  ENDMETHOD.

  METHOD set_heading.
    zcl_hh_dp_navigator=>set_heading( heading ).
  ENDMETHOD.

ENDCLASS.
