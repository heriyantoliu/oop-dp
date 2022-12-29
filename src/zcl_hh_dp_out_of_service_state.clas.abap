CLASS zcl_hh_dp_out_of_service_state DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_state
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_OUT_OF_SERVICE_STATE'.

    methods:
      constructor
        importing
          vehicle type ref to zcl_hh_dp_vehicle.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      description type zif_hh_dp_state=>description_type value 'out of service'.
ENDCLASS.



CLASS zcl_hh_dp_out_of_service_state IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    me->vehicle = vehicle.
    me->descriptor = me->description.

  ENDMETHOD.

ENDCLASS.
