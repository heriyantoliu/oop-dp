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
      year_type          TYPE num4,
      serial_type        TYPE num4.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      accelerate
        IMPORTING
          acceleration TYPE speed_type,
      change_heading
        IMPORTING
          turn TYPE zcl_hh_dp_navigator=>turn_type,
      get_characteristics
        EXPORTING
          serial_number TYPE serial_type
          license_plate TYPE license_plate_type
          brand         TYPE brand_type
          model         TYPE model_type
          year          TYPE year_type
          color         TYPE color_type
          location      TYPE location_type
          speed_unit    TYPE speed_unit_type,
      get_heading
        RETURNING
          VALUE(heading) TYPE zcl_hh_dp_navigator=>heading_type,
      get_speed
        RETURNING
          VALUE(speed) TYPE speed_type,
      constructor
        IMPORTING
          license_plate TYPE license_plate_type
          brand         TYPE brand_type
          model         TYPE model_type
          year          TYPE year_type
          color         TYPE color_type
          location      TYPE location_type
          speed_unit    TYPE speed_unit_type
          heading       TYPE zcl_hh_dp_navigator=>heading_type.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      last_serial_value TYPE serial_type.

    DATA:
      license_plate   TYPE license_plate_type,
      brand           TYPE brand_type,
      model           TYPE model_type,
      year            TYPE year_type,
      color           TYPE color_type,
      location        TYPE location_type,
      speed           TYPE speed_type,
      speed_unit      TYPE speed_unit_type,
      serial_number   TYPE serial_type,
      navigation_unit TYPE REF TO zcl_hh_dp_navigator.

    CLASS-METHODS:
      get_serial_number
        returning
          value(serial_number) TYPE serial_type.

ENDCLASS.



CLASS zcl_hh_dp_car IMPLEMENTATION.
  METHOD accelerate.
    ADD acceleration TO speed.
  ENDMETHOD.

  METHOD change_heading.
    me->navigation_unit->change_heading( turn ).
  ENDMETHOD.

  METHOD get_characteristics.
    serial_number = me->serial_number.
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

  METHOD constructor.

    me->serial_number = me->get_serial_number( ).

    me->license_plate = license_plate.
    me->brand = brand.
    me->model = model.
    me->year = year.
    me->color = color.
    me->location = location.
    me->speed_unit = speed_unit.

    me->navigation_unit = NEW #( heading ).
  ENDMETHOD.

  METHOD class_constructor.
    last_serial_value = 1000.
  ENDMETHOD.

  METHOD get_serial_number.
    add 1 to last_serial_value.
    serial_number = last_serial_value.
  ENDMETHOD.

ENDCLASS.
