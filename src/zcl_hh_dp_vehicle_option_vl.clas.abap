CLASS zcl_hh_dp_vehicle_option_vl DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle_option
  FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      class_id TYPE seoclsname VALUE 'ZCL_HH_DP_VEHICLE_OPTION_VL'.

    METHODS:
      constructor
        IMPORTING
          wrapped_object TYPE REF TO zcl_hh_dp_vehicle.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      this_option_weight       TYPE zcl_hh_dp_vehicle=>weight_unit_type VALUE 55,
      this_option_weight_unit  TYPE msehi VALUE 'LB',
      this_option_abbreviation TYPE zcl_hh_dp_vehicle=>description_type VALUE 'VL'.

ENDCLASS.



CLASS zcl_hh_dp_vehicle_option_vl IMPLEMENTATION.
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
