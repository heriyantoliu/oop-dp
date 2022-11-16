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
              pheading TYPE zcl_hh_dp_navigator=>heading_type,
              pturn01  TYPE zcl_hh_dp_navigator=>turn_type,
              pturn02  TYPE zcl_hh_dp_navigator=>turn_type,
              pturn03  TYPE zcl_hh_dp_navigator=>turn_type,
              pspeedu  TYPE zcl_hh_dp_vehicle=>speed_unit_type,
              pspeed01 TYPE zcl_hh_dp_vehicle=>speed_type,
              pspeed02 TYPE zcl_hh_dp_vehicle=>speed_type,
              pspeed03 TYPE zcl_hh_dp_vehicle=>speed_type.
SELECTION-SCREEN END OF BLOCK block_a.

INITIALIZATION.
  SET PF-STATUS zcl_hh_dp_report=>selection_screen_status_name.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ zcl_hh_dp_report=>execute OR
        sy-ucomm EQ zcl_hh_dp_report=>add_new_car or
        sy-ucomm eq zcl_hh_dp_report=>add_new_truck.

  CASE sy-ucomm.
    WHEN zcl_hh_dp_report=>add_new_car.

      zcl_hh_dp_report=>register_car_entry(
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
      ).

    WHEN zcl_hh_dp_report=>add_new_truck.

      zcl_hh_dp_report=>register_truck_entry(
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
      ).

    when others.
  ENDCASE.

START-OF-SELECTION.

END-OF-SELECTION.
  zcl_hh_dp_report=>show_report( ).
