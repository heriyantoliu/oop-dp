CLASS zcl_hh_dp_navigator DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.

    INTERFACES: zif_hh_dp_simple_navigation.

    ALIASES:
      change_heading FOR zif_hh_dp_simple_navigation~change_heading,
      get_heading FOR zif_hh_dp_simple_navigation~get_heading.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING
          heading TYPE zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      compass_offset_limit_lo TYPE int4 VALUE 0.

    class-data:
      compass_offset_limit_hi TYPE int4.

    DATA:
      heading TYPE zif_hh_dp_simple_navigation=>heading_type.
ENDCLASS.



CLASS zcl_hh_dp_navigator IMPLEMENTATION.
  METHOD change_heading.
    CHECK turn EQ zif_hh_dp_simple_navigation=>left_turn OR
          turn EQ zif_hh_dp_simple_navigation=>right_turn OR
          turn EQ zif_hh_dp_simple_navigation=>u_turn.

    FIND heading IN zif_hh_dp_simple_navigation=>compass
      MATCH OFFSET DATA(compass_offset).
    CASE turn.
      WHEN zif_hh_dp_simple_navigation=>left_turn.
        SUBTRACT 1 FROM compass_offset.
      WHEN zif_hh_dp_simple_navigation=>right_turn.
        ADD 1 TO compass_offset.
      WHEN zif_hh_dp_simple_navigation=>u_turn.
        ADD 2 TO compass_offset.
    ENDCASE.

    IF compass_offset LT compass_offset_limit_lo.
      ADD 4 TO compass_offset.
    ENDIF.

    IF compass_offset GT compass_offset_limit_hi.
      SUBTRACT 4 FROM compass_offset.
    ENDIF.

    heading = zif_hh_dp_simple_navigation=>compass+compass_offset(1).
  ENDMETHOD.

  METHOD get_heading.
    heading = me->heading.
  ENDMETHOD.

  METHOD constructor.
    IF me->heading CA heading.
      me->heading = heading.
    ELSE.
      me->heading = zif_hh_dp_simple_navigation=>compass(1).
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    compass_offset_limit_hi = strlen( zif_hh_dp_simple_navigation=>compass ) - 01.
  ENDMETHOD.

ENDCLASS.
