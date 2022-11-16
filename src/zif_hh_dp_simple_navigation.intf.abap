INTERFACE zif_hh_dp_simple_navigation
  PUBLIC.

  TYPES:
    turn_type    TYPE char1,
    heading_type TYPE char1.

  CONSTANTS:
    left_turn  TYPE turn_type VALUE 'L',
    right_turn TYPE turn_type VALUE 'R',
    u_turn     TYPE turn_type VALUE 'U',
    compass    TYPE char4 VALUE 'NESW'.

  METHODS:
    change_heading
      IMPORTING
        turn TYPE turn_type,
    get_heading
      RETURNING
        VALUE(heading) TYPE heading_type.
ENDINTERFACE.
