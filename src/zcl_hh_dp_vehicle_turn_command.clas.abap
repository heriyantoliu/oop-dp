CLASS zcl_hh_dp_vehicle_turn_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_command .

    ALIASES:
      execute FOR zif_hh_dp_command~execute,
      undo for zif_hh_dp_command~undo.

    METHODS:
      constructor
        IMPORTING
          vehicle   TYPE REF TO zcl_hh_dp_vehicle
          last_turn TYPE zif_hh_dp_simple_navigation=>turn_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      corresponding_vehicle TYPE REF TO zcl_hh_dp_vehicle,
      last_turn             TYPE zif_hh_dp_simple_navigation=>turn_type.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_turn_command IMPLEMENTATION.


  METHOD execute.
    DATA: license_plate TYPE zcl_hh_dp_vehicle=>license_plate_type.

    me->corresponding_vehicle->get_characteristics(
      IMPORTING
        license_plate   = license_plate
    ).

    message i398(00) with 'Last turn' me->last_turn
                          'repeated for' license_plate.
  ENDMETHOD.

  METHOD constructor.
    me->corresponding_vehicle = vehicle.
    me->last_turn = last_turn.
  ENDMETHOD.

  METHOD zif_hh_dp_command~undo.
    data: turn_translator type string,
          turn_to_apply type zif_hh_dp_simple_navigation=>turn_type,
          license_plate type zcl_hh_dp_vehicle=>location_type.

    concatenate zif_hh_dp_simple_navigation=>left_turn
                zif_hh_dp_simple_navigation=>right_turn
                zif_hh_dp_simple_navigation=>right_turn
                zif_hh_dp_simple_navigation=>left_turn
      into turn_translator.

    turn_to_apply = me->last_turn.
    translate turn_to_apply using turn_translator.

    me->corresponding_vehicle->change_heading( turn_to_apply ).
    me->corresponding_vehicle->get_characteristics(
      importing
        license_plate = license_plate
    ).

    message i398(00) with 'Last turn' me->last_turn
                          'reversed for' license_plate.
  ENDMETHOD.

ENDCLASS.
