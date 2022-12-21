CLASS zcl_hh_dp_vehicle_option_mt DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_option
  FINAL.

  PUBLIC SECTION.
    constants:
      class_id type seoclsname value 'ZCL_HH_DP_VEHICLE_OPTION_MT'.

    methods:
      constructor
        importing
          wrapped_object type ref to zcl_hh_dp_vehicle.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      this_option_weight type zcl_hh_dp_vehicle=>weight_unit_type value 135,
      this_option_weight_unit type msehi value 'LB',
      this_option_abbreviation type zcl_hh_dp_vehicle=>description_type value 'MT'.

ENDCLASS.



CLASS zcl_hh_dp_vehicle_option_mt IMPLEMENTATION.
  METHOD constructor.

    super->constructor(
      EXPORTING
        license_plate          = space
        brand                  = space
        model                  = space
        year                   = 0
        color                  = space
        location               = space
        speed_unit             = space
        heading                = space
        tare_weight            = 0
        weight_unit            = space
        basic_navigation       = space
        gps_navigation         = space
        iphone_navigation      = space
        no_navigation          = space
        vehicle_classification = space
    ).

    me->decorated_object = wrapped_object.
    me->option_weight = me->this_option_weight.
    me->option_weight_unit = me->this_option_weight_unit.
    me->option_abbreviation = me->this_option_abbreviation.

  ENDMETHOD.

ENDCLASS.
