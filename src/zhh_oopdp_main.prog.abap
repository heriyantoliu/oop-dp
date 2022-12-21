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
              xisnav   RADIOBUTTON GROUP nav,
              xnonav   RADIOBUTTON GROUP nav,
              xoptvl   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptcc   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptmt   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptoo   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptcr   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptxr   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptcg   TYPE zcl_hh_dp_vehicle=>option_count,
              xoptls   TYPE zcl_hh_dp_vehicle=>option_count.
SELECTION-SCREEN END OF BLOCK block_a.

INITIALIZATION.
  SET PF-STATUS zif_hh_dp_registration_screen=>selection_screen_status_name.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ zif_hh_dp_registration_screen=>execute OR
        sy-ucomm eq zif_hh_dp_registration_screen=>repeat_last_turn or
        sy-ucomm EQ zif_hh_dp_registration_screen=>add_new_car OR
        sy-ucomm EQ zif_hh_dp_registration_screen=>add_new_truck.

  CASE sy-ucomm.
    when zif_hh_dp_registration_screen=>repeat_last_turn.
      zcl_hh_dp_fleet_manager=>singleton->repeat_last_turn( ).
    WHEN zif_hh_dp_registration_screen=>add_new_car.

      zcl_hh_dp_fleet_manager=>singleton->register_car_entry(
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
          iphone_navigation = xisnav
          no_navigation = xnonav
          has_option_vl = xoptvl
          has_option_cc = xoptcc
          has_option_mt = xoptmt
          has_option_oo = xoptoo
          has_option_cr = xoptcr
          has_option_xr = xoptxr
          has_option_cg = xoptcg
          has_option_ls = xoptls
      ).

    WHEN zif_hh_dp_registration_screen=>add_new_truck.

      zcl_hh_dp_fleet_manager=>singleton->register_truck_entry(
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
          iphone_navigation = xisnav
          no_navigation = xnonav
          has_option_vl = xoptvl
          has_option_cc = xoptcc
          has_option_mt = xoptmt
          has_option_oo = xoptoo
          has_option_cr = xoptcr
          has_option_xr = xoptxr
          has_option_cg = xoptcg
          has_option_ls = xoptls
      ).

    WHEN OTHERS.
  ENDCASE.

START-OF-SELECTION.

END-OF-SELECTION.
  zcl_hh_dp_trck_axle_wght_mntr=>singleton->show_over_2_axle_limit_count( ).
  zcl_hh_dp_report=>singleton->show_report( ).
