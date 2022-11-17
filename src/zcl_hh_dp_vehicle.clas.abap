CLASS zcl_hh_dp_vehicle DEFINITION
  PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    TYPES:
      brand_type         TYPE f4txt,
      color_type         TYPE f4txt,
      location_type      TYPE f4txt,
      model_type         TYPE f4txt,
      license_plate_type TYPE f4txt,
      navigator_type     TYPE seoclsname,
      speed_type         TYPE int4,
      speed_unit_type    TYPE char3,
      year_type          TYPE num4,
      serial_type        TYPE num4,
      weight_type        TYPE int4,
      weight_unit_type   TYPE char3,
      description_type   TYPE char15,
      vehicle_type       TYPE seoclsname.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      accelerate
        IMPORTING
          acceleration TYPE speed_type,
      change_heading
        IMPORTING
          turn TYPE zif_hh_dp_simple_navigation=>turn_type,
      get_characteristics
        EXPORTING
          serial_number   TYPE serial_type
          license_plate   TYPE license_plate_type
          brand           TYPE brand_type
          model           TYPE model_type
          year            TYPE year_type
          color           TYPE color_type
          location        TYPE location_type
          speed_unit      TYPE speed_unit_type
          weight_unit     TYPE weight_unit_type
          navigation_type TYPE navigator_type,
      get_description ABSTRACT
        RETURNING
          VALUE(description) TYPE description_type,
      get_gross_weight ABSTRACT
        RETURNING
          VALUE(gross_weight) TYPE weight_type,
      get_heading
        RETURNING
          VALUE(heading) TYPE zif_hh_dp_simple_navigation=>heading_type,
      get_speed
        RETURNING
          VALUE(speed) TYPE speed_type,
      constructor
        IMPORTING
          license_plate          TYPE license_plate_type
          brand                  TYPE brand_type
          model                  TYPE model_type
          year                   TYPE year_type
          color                  TYPE color_type
          location               TYPE location_type
          speed_unit             TYPE speed_unit_type
          heading                TYPE zif_hh_dp_simple_navigation=>heading_type
          tare_weight            TYPE weight_type
          weight_unit            TYPE weight_unit_type
          basic_navigation       TYPE checkbox
          gps_navigation         TYPE checkbox
          no_navigation          TYPE checkbox
          vehicle_classification TYPE vehicle_type.
  PROTECTED SECTION.
    DATA:
      tare_weight TYPE weight_type.

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
      weight_unit     TYPE weight_unit_type,
      serial_number   TYPE serial_type,
      navigation_type TYPE navigator_type,
      navigation_unit TYPE REF TO zif_hh_dp_simple_navigation.

    CLASS-METHODS:
      get_serial_number
        RETURNING
          VALUE(serial_number) TYPE serial_type.
ENDCLASS.

CLASS zcl_hh_dp_vehicle IMPLEMENTATION.
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
    weight_unit = me->weight_unit.
    navigation_type = me->navigation_type.
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
    me->tare_weight = tare_weight.
    me->weight_unit = weight_unit.

    zcl_hh_dp_vehicle_accs_store=>get_navigation_unit(
      EXPORTING
        vehicle_classification = vehicle_classification
        heading          = heading
        basic_navigation = basic_navigation
        gps_navigation   = gps_navigation
        no_navigation    = no_navigation
      IMPORTING
        navigation_unit  = me->navigation_unit
        unit_type        = me->navigation_type
    ).

  ENDMETHOD.

  METHOD class_constructor.
    last_serial_value = 1000.
  ENDMETHOD.

  METHOD get_serial_number.
    ADD 1 TO last_serial_value.
    serial_number = last_serial_value.
  ENDMETHOD.

ENDCLASS.
