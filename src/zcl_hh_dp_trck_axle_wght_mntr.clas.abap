CLASS zcl_hh_dp_trck_axle_wght_mntr DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    types:
      count_type type int4.

    class-data:
      singleton type ref to zcl_hh_dp_trck_axle_wght_mntr READ-ONLY.

    class-methods:
      class_constructor,
      get_2_axle_weight_limit
        exporting
          maximum_weight type zcl_hh_dp_vehicle=>weight_type
          weight_unit type zcl_hh_dp_vehicle=>weight_unit_type.

    methods:
      show_over_2_axle_limit_count.
  PROTECTED SECTION.
  PRIVATE SECTION.
    constants:
      weight_limit_in_lbs_2_axles type zcl_hh_dp_vehicle=>weight_type value 40000,
      weight_limit_2_axles_unit type zcl_hh_dp_vehicle=>weight_unit_type value 'LB'.

    data:
      over_2_axle_limit_count type count_type.

    methods:
      add_to_over_2_axle_limit_count
        for event weight_exceeds_2_axle_limit
          of zcl_hh_dp_truck.

ENDCLASS.



CLASS zcl_hh_dp_trck_axle_wght_mntr IMPLEMENTATION.

  METHOD add_to_over_2_axle_limit_count.
    add 1 to me->over_2_axle_limit_count.
  ENDMETHOD.

  METHOD class_constructor.
    singleton = new #( ).
    set handler singleton->add_to_over_2_axle_limit_count
      for all instances.
  ENDMETHOD.

  METHOD get_2_axle_weight_limit.
    maximum_weight = weight_limit_in_lbs_2_axles.
    weight_unit = weight_limit_2_axles_unit.
  ENDMETHOD.

  METHOD show_over_2_axle_limit_count.
    constants:
      severity_information type symsgty value 'I',
      severity_warning type symsgty value 'W'.

    data:
      message_severity type symsgty.

    if me->over_2_axle_limit_count gt 0.
      message_severity = severity_warning.
    else.
      message_severity = severity_information.
    endif.

    message i398(00) with me->over_2_axle_limit_count
                            'trucks exceed the 2-axle weight limit of'
                            me->weight_limit_in_lbs_2_axles
                            me->weight_limit_2_axles_unit
      display like message_severity.
  ENDMETHOD.



ENDCLASS.
