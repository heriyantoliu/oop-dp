*&---------------------------------------------------------------------*
*& Report zhh_oo_dp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhh_oopdp_main.

SELECTION-SCREEN BEGIN OF BLOCK block_a WITH FRAME.
  PARAMETERS: pplate   TYPE zcl_hh_dp_car=>license_plate_type,
              pbrand   TYPE zcl_hh_dp_car=>brand_type,
              pmodel   TYPE zcl_hh_dp_car=>model_type,
              pyear    TYPE zcl_hh_dp_car=>year_type,
              pcolor   TYPE zcl_hh_dp_car=>color_type,
              plocatn  TYPE zcl_hh_dp_car=>location_type,
              pheading TYPE zcl_hh_dp_navigator=>heading_type,
              pturn01  TYPE zcl_hh_dp_navigator=>turn_type,
              pturn02  TYPE zcl_hh_dp_navigator=>turn_type,
              pturn03  TYPE zcl_hh_dp_navigator=>turn_type,
              pspeedu  TYPE zcl_hh_dp_car=>speed_unit_type,
              pspeed01 TYPE zcl_hh_dp_car=>speed_type,
              pspeed02 TYPE zcl_hh_dp_car=>speed_type,
              pspeed03 TYPE zcl_hh_dp_car=>speed_type.
SELECTION-SCREEN END OF BLOCK block_a.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ zcl_hh_dp_report=>execute.

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

START-OF-SELECTION.

END-OF-SELECTION.
  zcl_hh_dp_report=>show_report( ).
