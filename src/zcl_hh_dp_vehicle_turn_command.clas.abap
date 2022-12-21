CLASS zcl_hh_dp_vehicle_turn_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_hh_dp_command .

    ALIASES:
      execute FOR zif_hh_dp_command~execute,
      undo FOR zif_hh_dp_command~undo.

    METHODS:
      constructor
        IMPORTING
          vehicle   TYPE REF TO zcl_hh_dp_vehicle
          last_turn TYPE zif_hh_dp_simple_navigation=>turn_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      corresponding_vehicle         TYPE REF TO zcl_hh_dp_vehicle,
      last_turn                     TYPE zif_hh_dp_simple_navigation=>turn_type,
      number_of_times_turn_repeated TYPE int4.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_turn_command IMPLEMENTATION.


  METHOD execute.
    DATA: license_plate TYPE zcl_hh_dp_vehicle=>license_plate_type.

    me->corresponding_vehicle->get_characteristics(
      IMPORTING
        license_plate   = license_plate
    ).

    MESSAGE i398(00) WITH 'Last turn' me->last_turn
                          'repeated for' license_plate.

    add 1 to me->number_of_times_turn_repeated.
  ENDMETHOD.

  METHOD constructor.
    me->corresponding_vehicle = vehicle.
    me->last_turn = last_turn.
  ENDMETHOD.

  METHOD zif_hh_dp_command~undo.
    DATA: turn_translator TYPE string,
          turn_to_apply   TYPE zif_hh_dp_simple_navigation=>turn_type,
          license_plate   TYPE zcl_hh_dp_vehicle=>location_type.

    check me->number_of_times_turn_repeated gt 0.

    CONCATENATE zif_hh_dp_simple_navigation=>left_turn
                zif_hh_dp_simple_navigation=>right_turn
                zif_hh_dp_simple_navigation=>right_turn
                zif_hh_dp_simple_navigation=>left_turn
      INTO turn_translator.

    turn_to_apply = me->last_turn.
    TRANSLATE turn_to_apply USING turn_translator.

    me->corresponding_vehicle->change_heading( turn_to_apply ).
    me->corresponding_vehicle->get_characteristics(
      IMPORTING
        license_plate = license_plate
    ).

    MESSAGE i398(00) WITH 'Last turn' me->last_turn
                          'reversed for' license_plate.

    subtract 1 from me->number_of_times_turn_repeated.
  ENDMETHOD.

ENDCLASS.
