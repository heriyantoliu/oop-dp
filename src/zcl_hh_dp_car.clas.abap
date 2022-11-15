CLASS zcl_hh_dp_car DEFINITION
  PUBLIC
  FINAL.

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

    METHODS:
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
    DATA:
      license_plate TYPE zcl_hh_dp_car=>license_plate_type,
      brand         TYPE zcl_hh_dp_car=>brand_type,
      model         TYPE zcl_hh_dp_car=>model_type,
      year          TYPE zcl_hh_dp_car=>year_type,
      color         TYPE zcl_hh_dp_car=>color_type,
      location      TYPE zcl_hh_dp_car=>location_type,
      speed         TYPE zcl_hh_dp_car=>speed_type,
      speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type,
      navigation_unit type ref to zcl_hh_dp_navigator.

ENDCLASS.



CLASS zcl_hh_dp_car IMPLEMENTATION.
  METHOD accelerate.
    ADD acceleration TO speed.
  ENDMETHOD.

  METHOD change_heading.
    me->navigation_unit->change_heading( turn ).
  ENDMETHOD.

  METHOD get_characteristics.
    license_plate = me->license_plate.
    brand = me->brand.
    model = me->model.
    year = me->year.
    color = me->color.
    location = me->location.
    speed_unit = me->speed_unit.
  ENDMETHOD.

  METHOD get_heading.
    heading = me->navigation_unit->get_heading( ).
  ENDMETHOD.

  METHOD get_speed.
    speed = me->speed.
  ENDMETHOD.

  METHOD set_characteristics.
    me->license_plate = license_plate.
    me->brand = brand.
    me->model = model.
    me->year = year.
    me->color = color.
    me->location = location.
    me->speed_unit = speed_unit.

    me->navigation_unit = new #( ).
  ENDMETHOD.

  METHOD set_heading.
    me->navigation_unit->set_heading( heading ).
  ENDMETHOD.

ENDCLASS.
