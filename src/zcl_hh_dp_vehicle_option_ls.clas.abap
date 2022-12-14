class ZCL_HH_DP_VEHICLE_OPTION_LS definition
  public
  inheriting from ZCL_HH_DP_VEHICLE_OPTION
  final
  create public .

public section.

  constants CLASS_ID type SEOCLSNAME value 'ZCL_HH_DP_VEHICLE_OPTION_LS'.

  methods CONSTRUCTOR
    importing
      !WRAPPED_OBJECT type ref to ZCL_HH_DP_VEHICLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      this_option_weight type zcl_hh_dp_vehicle=>weight_unit_type value 66,
      this_option_weight_unit type msehi value 'LB',
      this_option_abbreviation type zcl_hh_dp_vehicle=>description_type value 'LS'.

ENDCLASS.



CLASS ZCL_HH_DP_VEHICLE_OPTION_LS IMPLEMENTATION.


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
