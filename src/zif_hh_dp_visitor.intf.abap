INTERFACE zif_hh_dp_visitor
  PUBLIC .
    methods:
      visit_car
        importing
          vehicle type ref to zcl_hh_dp_vehicle,
      visit_truck
        importing
          vehicle type ref to zcl_hh_dp_vehicle.
ENDINTERFACE.
