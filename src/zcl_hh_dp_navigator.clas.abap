CLASS zcl_hh_dp_navigator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES:
      turn_type    TYPE char1,
      heading_type TYPE char1.

    CONSTANTS:
      left_turn  TYPE zcl_hh_dp_navigator=>turn_type VALUE 'L',
      right_turn TYPE zcl_hh_dp_navigator=>turn_type VALUE 'R',
      u_turn     TYPE zcl_hh_dp_navigator=>turn_type VALUE 'U'.

    CLASS-METHODS:
      change_heading
        IMPORTING
          turn TYPE zcl_hh_dp_navigator=>turn_type,
      get_heading
        RETURNING
          VALUE(heading) TYPE zcl_hh_dp_navigator=>heading_type,
      set_heading
        IMPORTING
          heading TYPE zcl_hh_dp_navigator=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      compass                 TYPE char4 VALUE 'NESW',
      compass_offset_limit_lo TYPE int4 VALUE 0,
      compass_offset_limit_hi TYPE int4 VALUE 3.

    CLASS-DATA:
      heading TYPE zcl_hh_dp_navigator=>heading_type.
ENDCLASS.



CLASS zcl_hh_dp_navigator IMPLEMENTATION.
  METHOD change_heading.
    CHECK turn EQ left_turn OR
          turn EQ right_turn OR
          turn EQ u_turn.

    FIND heading IN compass
      MATCH OFFSET DATA(compass_offset).
    CASE turn.
      WHEN left_turn.
        SUBTRACT 1 FROM compass_offset.
      WHEN right_turn.
        ADD 1 TO compass_offset.
      WHEN u_turn.
        ADD 2 TO compass_offset.
    ENDCASE.

    IF compass_offset LT compass_offset_limit_lo.
      ADD 4 TO compass_offset.
    ENDIF.

    IF compass_offset GT compass_offset_limit_hi.
      SUBTRACT 4 FROM compass_offset.
    ENDIF.

    heading = compass+compass_offset(1).
  ENDMETHOD.

  METHOD get_heading.
    heading = zcl_hh_dp_navigator=>heading.
  ENDMETHOD.

  METHOD set_heading.
    IF zcl_hh_dp_navigator=>heading CA heading.
      zcl_hh_dp_navigator=>heading = heading.
    ELSE.
      zcl_hh_dp_navigator=>heading = compass(1).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
