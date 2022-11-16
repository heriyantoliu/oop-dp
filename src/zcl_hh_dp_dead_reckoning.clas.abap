CLASS zcl_hh_dp_dead_reckoning DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_simple_navigation .

    ALIASES:
      change_heading FOR zif_hh_dp_simple_navigation~change_heading,
      get_heading FOR zif_hh_dp_simple_navigation~get_heading.

    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_DEAD_RECKONING'.

    METHODS:
      constructor
        IMPORTING
          heading TYPE zif_hh_dp_simple_navigation=>heading_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      unknown_direction TYPE char01 VALUE '?'.

    DATA:
      heading TYPE zif_hh_dp_simple_navigation=>heading_type.
ENDCLASS.



CLASS zcl_hh_dp_dead_reckoning IMPLEMENTATION.


  METHOD change_heading.
  ENDMETHOD.


  METHOD get_heading.
    heading = me->heading.
  ENDMETHOD.

  METHOD constructor.
    me->heading = unknown_direction.
  ENDMETHOD.

ENDCLASS.
