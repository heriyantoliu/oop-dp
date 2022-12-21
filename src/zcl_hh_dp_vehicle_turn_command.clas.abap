CLASS zcl_hh_dp_vehicle_turn_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_command .

    ALIASES:
      execute FOR zif_hh_dp_command~execute.

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

ENDCLASS.
