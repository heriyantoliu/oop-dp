INTERFACE zif_hh_dp_state
  PUBLIC .
    types:
      description_type type c length 16,
      odometer_type type p length 7 DECIMALS 3.

    methods:
      get_description
        returning
          value(description) type description_type,
      get_distance_traveled
        importing
          vehicle type ref to zcl_hh_dp_vehicle
        returning
          value(distance) type odometer_type,
      place_out_of_service
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      resume
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      slow
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      stop
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      turn
        importing
          vehicle type ref to zcl_hh_dp_vehicle
          turn type zif_hh_dp_simple_navigation=>turn_type,
      maintain
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      make_available
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      repair
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      start
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      tow
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      assign_police_escort
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      decelerate_05
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      decelerate_01
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      accelerate_01
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      accelerate_05
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      impose_high_winds_restriction
        importing
          vehicle type ref to zcl_hh_dp_vehicle.
ENDINTERFACE.
