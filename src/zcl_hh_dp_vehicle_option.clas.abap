CLASS zcl_hh_dp_vehicle_option DEFINITION
  PUBLIC
  INHERITING FROM zcl_hh_dp_vehicle
  ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      accelerate REDEFINITION,
      change_heading REDEFINITION,
      get_characteristics REDEFINITION,
      get_description REDEFINITION,
      get_gross_weight REDEFINITION,
      get_heading REDEFINITION,
      get_speed REDEFINITION,
      get_current_state REDEFINITION,
      set_current_state REDEFINITION,
      get_previous_state REDEFINITION,
      set_previous_state REDEFINITION,
      get_time_started_moving REDEFINITION,
      set_time_started_moving REDEFINITION,
      get_dist_traveled_before_stop REDEFINITION,
      set_dist_traveled_before_stop REDEFINITION,
      get_previous_state_speed REDEFINITION,
      set_previous_state_speed REDEFINITION,
      get_cruising_state REDEFINITION,
      get_in_heavy_traffic_state REDEFINITION,
      get_stopped_state REDEFINITION,
      get_out_of_service_state REDEFINITION,
      get_available_state REDEFINITION,
      get_being_towed_state REDEFINITION,
      get_in_shop_state REDEFINITION.

  PROTECTED SECTION.
    DATA:
      decorated_object    TYPE REF TO zcl_hh_dp_vehicle,
      option_weight       TYPE zcl_hh_dp_vehicle=>weight_unit_type,
      option_weight_unit  TYPE msehi,
      option_abbreviation TYPE zcl_hh_dp_vehicle=>description_type.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_vehicle_option IMPLEMENTATION.
  METHOD accelerate.
    me->decorated_object->accelerate( acceleration ).
  ENDMETHOD.

  METHOD change_heading.
    me->decorated_object->change_heading( turn ).
  ENDMETHOD.

  METHOD get_characteristics.
    me->decorated_object->get_characteristics(
      IMPORTING
        serial_number   = serial_number
        license_plate   = license_plate
        brand           = brand
        model           = model
        year            = year
        color           = color
        location        = location
        speed_unit      = speed_unit
        weight_unit     = weight_unit
        navigation_type = navigation_type
    ).
  ENDMETHOD.

  METHOD get_description.
    CONSTANTS: separator TYPE char01 VALUE ','.

    description = me->decorated_object->get_description( ).
    CONCATENATE description separator me->option_abbreviation
      INTO description.
  ENDMETHOD.
  METHOD get_gross_weight.
    DATA: converted_option_weight TYPE zcl_hh_dp_vehicle=>weight_type,
          registered_weight_unit  TYPE msehi.

    gross_weight = me->decorated_object->get_gross_weight( ).

    converted_option_weight = me->option_weight.

    me->get_characteristics(
      IMPORTING
        weight_unit     = registered_weight_unit
    ).

    IF registered_weight_unit NE me->option_weight_unit.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = converted_option_weight
          unit_in  = me->option_weight_unit
          unit_out = registered_weight_unit
        IMPORTING
          output   = converted_option_weight
        EXCEPTIONS
          OTHERS   = 0.
    ENDIF.

    add converted_option_weight to gross_weight.
  ENDMETHOD.

  METHOD get_heading.
    heading = me->decorated_object->get_heading( ).
  ENDMETHOD.

  METHOD get_speed.
    speed = me->decorated_object->get_speed( ).
  ENDMETHOD.

  METHOD get_current_state.
    current_state = me->decorated_object->get_current_state( ).
  ENDMETHOD.

  METHOD set_current_state.
    me->decorated_object->set_current_state( current_state ).
  ENDMETHOD.

  METHOD get_cruising_state.
    cruising_state = me->decorated_object->get_cruising_state( ).
  ENDMETHOD.

  METHOD get_dist_traveled_before_stop.
    distance_traveled_before_stop = me->decorated_object->get_dist_traveled_before_stop( ).
  ENDMETHOD.

  METHOD get_in_heavy_traffic_state.
    in_heavy_traffic_state = me->decorated_object->get_in_heavy_traffic_state( ).
  ENDMETHOD.

  METHOD get_previous_state.
    previous_state = me->decorated_object->get_previous_state( ).
  ENDMETHOD.

  METHOD get_previous_state_speed.
    previous_state_speed = me->decorated_object->get_previous_state_speed( ).
  ENDMETHOD.

  METHOD get_stopped_state.
    stopped_state = me->decorated_object->get_stopped_state( ).
  ENDMETHOD.

  METHOD get_time_started_moving.
    time_started_moving = me->decorated_object->get_time_started_moving( ).
  ENDMETHOD.

  METHOD set_dist_traveled_before_stop.
    me->decorated_object->set_dist_traveled_before_stop( distance_traveled_before_stop ).
  ENDMETHOD.

  METHOD set_previous_state.
    me->decorated_object->set_previous_state( previous_state ).
  ENDMETHOD.

  METHOD set_previous_state_speed.
    me->decorated_object->set_previous_state_speed( previous_state_speed ).
  ENDMETHOD.

  METHOD set_time_started_moving.
    me->decorated_object->set_time_started_moving( time_started_moving ).
  ENDMETHOD.

  METHOD get_out_of_service_state.
    out_of_service_state = me->decorated_object->get_out_of_service_state( ).
  ENDMETHOD.

  METHOD get_available_state.
    available_state = me->decorated_object->get_available_state( ).
  ENDMETHOD.

  METHOD get_being_towed_state.
    being_towed_state = me->decorated_object->get_being_towed_state( ).
  ENDMETHOD.

  METHOD get_in_shop_state.
    in_shop_state = me->decorated_object->get_in_shop_state( ).
  ENDMETHOD.

ENDCLASS.
