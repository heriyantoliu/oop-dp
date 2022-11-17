*&---------------------------------------------------------------------*
*& Report zhh_oo_dp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhh_oopdp_main.

SELECTION-SCREEN BEGIN OF BLOCK block_a WITH FRAME.
  PARAMETERS: pplate   TYPE zcl_hh_dp_vehicle=>license_plate_type,
              pbrand   TYPE zcl_hh_dp_vehicle=>brand_type,
              pmodel   TYPE zcl_hh_dp_vehicle=>model_type,
              pyear    TYPE zcl_hh_dp_vehicle=>year_type,
              pcolor   TYPE zcl_hh_dp_vehicle=>color_type,
              plocatn  TYPE zcl_hh_dp_vehicle=>location_type,
              pheading TYPE zif_hh_dp_simple_navigation=>heading_type,
              pturn01  TYPE zif_hh_dp_simple_navigation=>turn_type,
              pturn02  TYPE zif_hh_dp_simple_navigation=>turn_type,
              pturn03  TYPE zif_hh_dp_simple_navigation=>turn_type,
              pspeedu  TYPE zcl_hh_dp_vehicle=>speed_unit_type,
              pspeed01 TYPE zcl_hh_dp_vehicle=>speed_type,
              pspeed02 TYPE zcl_hh_dp_vehicle=>speed_type,
              pspeed03 TYPE zcl_hh_dp_vehicle=>speed_type,
              pwghtu   TYPE zcl_hh_dp_vehicle=>weight_unit_type,
              pevw     TYPE zcl_hh_dp_vehicle=>weight_type,
              pcargow  TYPE zcl_hh_dp_vehicle=>weight_type,
              ppsngrs  TYPE zcl_hh_dp_car=>passengers_type,
              xbnav    RADIOBUTTON GROUP nav,
              xgps     RADIOBUTTON GROUP nav,
              xnonav   RADIOBUTTON GROUP nav,
              xoptvl   AS CHECKBOX,
              xoptcc   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block_a.

INITIALIZATION.
  SET PF-STATUS zcl_hh_dp_report=>selection_screen_status_name.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ zcl_hh_dp_report=>execute OR
        sy-ucomm EQ zcl_hh_dp_report=>add_new_car OR
        sy-ucomm EQ zcl_hh_dp_report=>add_new_truck.

  CASE sy-ucomm.
    WHEN zcl_hh_dp_report=>add_new_car.

      zcl_hh_dp_report=>singleton->register_car_entry(
        EXPORTING
          license_plate = pplate
          brand         = pbrand
          year          = pyear
          model         = pmodel
          color         = pcolor
          location      = plocatn
          heading       = pheading
          turn01        = pturn01
          turn02        = pturn02
          turn03        = pturn03
          speed01       = pspeed01
          speed02       = pspeed02
          speed03       = pspeed03
          speed_unit    = pspeedu
          tare_weight   = pevw
          weight_unit   = pwghtu
          passengers    = ppsngrs
          basic_navigation = xbnav
          gps_navigation = xgps
          no_navigation = xnonav
          has_option_vl = xoptvl
          has_option_cc = xoptcc
      ).

    WHEN zcl_hh_dp_report=>add_new_truck.

      zcl_hh_dp_report=>singleton->register_truck_entry(
        EXPORTING
          license_plate = pplate
          brand         = pbrand
          year          = pyear
          model         = pmodel
          color         = pcolor
          location      = plocatn
          heading       = pheading
          turn01        = pturn01
          turn02        = pturn02
          turn03        = pturn03
          speed01       = pspeed01
          speed02       = pspeed02
          speed03       = pspeed03
          speed_unit    = pspeedu
          tare_weight   = pevw
          weight_unit   = pwghtu
          cargo_weight  = pcargow
          basic_navigation = xbnav
          gps_navigation = xgps
          no_navigation = xnonav
          has_option_vl = xoptvl
          has_option_cc = xoptcc
      ).

    WHEN OTHERS.
  ENDCASE.

START-OF-SELECTION.

END-OF-SELECTION.
  zcl_hh_dp_trck_axle_wght_mntr=>singleton->show_over_2_axle_limit_count( ).
  zcl_hh_dp_report=>singleton->show_report( ).
