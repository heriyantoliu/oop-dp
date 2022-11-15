*&---------------------------------------------------------------------*
*& Report zhh_oo_dp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhh_oopdp_main.



TYPES: BEGIN OF output_row,
         license_plate TYPE zcl_hh_dp_car=>license_plate_type,
         brand         TYPE zcl_hh_dp_car=>brand_type,
         model         TYPE zcl_hh_dp_car=>model_type,
         year          TYPE zcl_hh_dp_car=>year_type,
         color         TYPE zcl_hh_dp_car=>color_type,
         location      TYPE zcl_hh_dp_car=>location_type,
         heading       TYPE zcl_hh_dp_navigator=>heading_type,
         speed         TYPE zcl_hh_dp_car=>speed_type,
         speed_unit    TYPE zcl_hh_dp_car=>speed_unit_type,
       END   OF output_row,
       output_list TYPE STANDARD TABLE OF output_row.

CONSTANTS: column_name_license_plate  TYPE lvc_fname VALUE 'LICENSE_PLATE',
           column_title_license_plate TYPE string    VALUE `License plate`,
           column_name_brand          TYPE lvc_fname VALUE 'BRAND',
           column_title_brand         TYPE string    VALUE `Brand`,
           column_name_model          TYPE lvc_fname VALUE 'MODEL',
           column_title_model         TYPE string    VALUE `Model`,
           column_name_year           TYPE lvc_fname VALUE 'YEAR',
           column_title_year          TYPE string    VALUE `Year`,
           column_name_color          TYPE lvc_fname VALUE 'COLOR',
           column_title_color         TYPE string    VALUE `Color`,
           column_name_location       TYPE lvc_fname VALUE 'LOCATION',
           column_title_location      TYPE string    VALUE `Location`,
           column_name_heading        TYPE lvc_fname VALUE 'HEADING',
           column_title_heading       TYPE string    VALUE `Heading`,
           column_name_speed          TYPE lvc_fname VALUE 'SPEED',
           column_title_speed         TYPE string    VALUE `Speed`,
           column_name_speed_unit     TYPE lvc_fname VALUE 'SPEED_UNIT',
           column_title_speed_unit    TYPE string    VALUE `Unit`,
           minimum_column_width       TYPE int4      VALUE 08,
           execute                    TYPE syucomm   VALUE 'ONLI'.

DATA: grid_columns            TYPE REF TO cl_salv_columns_table,
      grid_column_stack       TYPE salv_t_column_ref,
      grid_column_entry       LIKE LINE OF grid_column_stack,
      grid_column_title_short TYPE scrtext_s,
      grid_column_width       TYPE lvc_outlen,
      output_stack            TYPE output_list,
      output_entry            LIKE LINE OF output_stack,
      alv_grid                TYPE REF TO cl_salv_table.

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
  CHECK sy-ucomm EQ execute.

  perform register_car_entry using pplate
                                   pbrand
                                   pmodel
                                   pyear
                                   pcolor
                                   plocatn
                                   pspeedu
                                   pheading
                                   pspeed01
                                   pspeed02
                                   pspeed03
                                   pturn01
                                   pturn02
                                   pturn03.

START-OF-SELECTION.

END-OF-SELECTION.
  PERFORM show_report.

FORM build_report.
  zcl_hh_dp_car=>get_characteristics(
    IMPORTING
      license_plate = output_entry-license_plate
      brand         = output_entry-brand
      model         = output_entry-model
      year          = output_entry-year
      color         = output_entry-color
      location      = output_entry-location
      speed_unit    = output_entry-speed_unit
  ).

  output_entry-heading = zcl_hh_dp_car=>get_heading( ).
  output_entry-speed = zcl_hh_dp_car=>get_speed( ).

  APPEND output_entry TO output_stack.
ENDFORM.

FORM present_report.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = alv_grid
        CHANGING
          t_table      = output_stack ).
    CATCH cx_salv_msg.
      MESSAGE e398(00) WITH 'Failure to create alv grid object' "#EC *
                            space
                            space
                            space
                            .
  ENDTRY.

  PERFORM set_column_titles.

  data(lo_display) = alv_grid->get_display_settings( ).

  lo_display->set_striped_pattern( 'X' ).

  alv_grid->display( ).
ENDFORM.

FORM set_column_titles.

  grid_columns = alv_grid->get_columns( ).

  grid_columns->set_optimize( 'X' ).

  grid_column_stack = grid_columns->get( ).

  LOOP AT grid_column_stack
     INTO grid_column_entry.
    CLEAR grid_column_width.
    CASE grid_column_entry-columnname.
      WHEN column_name_license_plate.
        grid_column_title_short   = column_title_license_plate.
      WHEN column_name_brand.
        grid_column_title_short   = column_title_brand.
      WHEN column_name_model.
        grid_column_title_short   = column_title_model.
      WHEN column_name_year.
        grid_column_title_short   = column_title_year.
      WHEN column_name_color.
        grid_column_title_short   = column_title_color.
      WHEN column_name_location.
        grid_column_title_short   = column_title_location.
      WHEN column_name_heading.
        grid_column_title_short   = column_title_heading.
        grid_column_width         = minimum_column_width.
      WHEN column_name_speed.
        grid_column_title_short   = column_title_speed.
        grid_column_width         = minimum_column_width.
      WHEN column_name_speed_unit.
        grid_column_title_short   = column_title_speed_unit.
        grid_column_width         = minimum_column_width.
      WHEN OTHERS.
        CLEAR grid_column_title_short.
    ENDCASE.
    grid_column_entry-r_column->set_short_text( grid_column_title_short ).
    IF grid_column_width GT 00.
      grid_column_entry-r_column->set_output_length( grid_column_width ).
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM show_report.
  PERFORM: build_report,
           present_report.

ENDFORM.

form register_car_entry using license_plate
                              brand
                              model
                              year
                              color
                              location
                              speed_unit
                              heading
                              speed01
                              speed02
                              speed03
                              turn01
                              turn02
                              turn03.
  zcl_hh_dp_car=>set_characteristics(
    EXPORTING
      license_plate = license_plate
      brand         = brand
      model         = model
      year          = year
      color         = color
      location      = location
      speed_unit    = speed_unit
  ).

  zcl_hh_dp_car=>set_heading( heading ).

  zcl_hh_dp_car=>accelerate( speed01 ).
  zcl_hh_dp_car=>accelerate( speed02 ).
  zcl_hh_dp_car=>accelerate( speed03 ).

  zcl_hh_dp_car=>change_heading( turn01 ).
  zcl_hh_dp_car=>change_heading( turn02 ).
  zcl_hh_dp_car=>change_heading( turn03 ).
endform.
