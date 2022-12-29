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
        returning
          value(distance) type odometer_type,
      resume,
      slow,
      stop,
      turn
        importing
          turn type zif_hh_dp_simple_navigation=>turn_type.
ENDINTERFACE.
