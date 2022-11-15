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
      year_type          TYPE num4,
      turn_type          TYPE char1,
      heading_type       TYPE char1.

    CONSTANTS:
      left_turn  TYPE zcl_hh_dp_car=>turn_type VALUE 'L',
      right_turn TYPE zcl_hh_dp_car=>turn_type VALUE 'R',
      u_turn     TYPE zcl_hh_dp_car=>turn_type VALUE 'U'.

    CLASS-METHODS:
      accelerate
        IMPORTING
          acceleration TYPE zcl_hh_dp_car=>speed_type,
      change_heading
        IMPORTING
          turn TYPE zcl_hh_dp_car=>turn_type,
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
          value(heading) TYPE zcl_hh_dp_car=>heading_type,
      get_speed
        returning
          value(speed) TYPE zcl_hh_dp_car=>speed_type,
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
        IMPORTING
          heading TYPE zcl_hh_dp_car=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      compass                 TYPE char4 VALUE 'NESW',
      compass_offset_limit_lo TYPE int4 VALUE 0,
      compass_offset_limit_hi TYPE int4 VALUE 3.

    CLASS-DATA:
      license_plate TYPE zcl_hh_dp_car=>license_plate_type,
      brand         TYPE zcl_hh_dp_car=>brand_type,
      model         TYPE zcl_hh_dp_car=>model_type,
      year          TYPE zcl_hh_dp_car=>year_type,
      color         TYPE zcl_hh_dp_car=>color_type,
      location      TYPE zcl_hh_dp_car=>location_type,
      speed         TYPE zcl_hh_dp_car=>speed_type,
      speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type,
      heading       TYPE zcl_hh_dp_car=>heading_type.
ENDCLASS.



CLASS zcl_hh_dp_car IMPLEMENTATION.
  METHOD accelerate.
    add acceleration to zcl_hh_dp_car=>speed.
  ENDMETHOD.

  METHOD change_heading.
    check turn eq zcl_hh_dp_car=>left_turn or
          turn eq zcl_hh_dp_car=>right_turn or
          turn eq zcl_hh_dp_car=>u_turn.

    find zcl_hh_dp_car=>heading in zcl_hh_dp_car=>compass
      match offset data(compass_offset).
    case turn.
      when zcl_hh_dp_car=>left_turn.
        subtract 1 from compass_offset.
      when zcl_hh_dp_car=>right_turn.
        add 1 to compass_offset.
      when zcl_hh_dp_car=>u_turn.
        add 2 to compass_offset.
    endcase.

    if compass_offset lt zcl_hh_dp_car=>compass_offset_limit_lo.
      add 4 to compass_offset.
    endif.

    if compass_offset gt zcl_hh_dp_car=>compass_offset_limit_hi.
      subtract 4 from compass_offset.
    endif.

    zcl_hh_dp_car=>heading = zcl_hh_dp_car=>compass+compass_offset(1).
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
    heading = zcl_hh_dp_car=>heading.
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
    if zcl_hh_dp_car=>heading ca heading.
      zcl_hh_dp_car=>heading = heading.
    else.
      zcl_hh_dp_car=>heading = zcl_hh_dp_car=>compass(1).
    endif.
  ENDMETHOD.

ENDCLASS.
