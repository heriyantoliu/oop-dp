*&---------------------------------------------------------------------*
*& Report zhh_oo_dp
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhh_oopdp_main.

TYPES: brand_type         TYPE f4txt,
       color_type         TYPE f4txt,
       location_type      TYPE f4txt,
       model_type         TYPE f4txt,
       license_plate_type TYPE f4txt,
       speed_type         TYPE int4,
       speed_unit_type    TYPE char3,
       year_type          TYPE num4,
       turn_type          TYPE char1,
       heading_type       TYPE char1.

TYPES: BEGIN OF output_row,
         license_plate TYPE license_plate_type,
         brand         TYPE brand_type,
         model         TYPE model_type,
         year          TYPE year_type,
         color         TYPE color_type,
         location      TYPE location_type,
         heading       TYPE heading_type,
         speed         TYPE speed_type,
         speed_unit    TYPE speed_unit_type,
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
           left_turn                  TYPE char1     VALUE 'L',
           right_turn                 TYPE char1     VALUE 'R',
           u_turn                     TYPE char1     VALUE 'U',
           compass                    TYPE char4     VALUE 'NESW',
           compass_offset_limit_lo    TYPE int4      VALUE 00,
           compass_offset_limit_hi    TYPE int4      VALUE 03,
           execute                    TYPE syucomm   VALUE 'ONLI'.

DATA: grid_columns            TYPE REF TO cl_salv_columns_table,
      grid_column_stack       TYPE salv_t_column_ref,
      grid_column_entry       LIKE LINE OF grid_column_stack,
      grid_column_title_short TYPE scrtext_s,
      grid_column_width       TYPE lvc_outlen,
      output_stack            TYPE output_list,
      output_entry            LIKE LINE OF output_stack,
      alv_grid                TYPE REF TO cl_salv_table,
      license_plate           TYPE license_plate_type,
      brand                   TYPE brand_type,
      model                   TYPE model_type,
      year                    TYPE year_type,
      color                   TYPE color_type,
      location                TYPE location_type,
      heading                 TYPE heading_type,
      speed                   TYPE speed_type,
      speed_unit              TYPE speed_unit_type,
      compass_offset          TYPE int4.
.

SELECTION-SCREEN BEGIN OF BLOCK block_a WITH FRAME.
  PARAMETERS: pplate   TYPE license_plate_type,
              pbrand   TYPE brand_type,
              pmodel   TYPE model_type,
              pyear    TYPE year_type,
              pcolor   TYPE color_type,
              plocatn  TYPE location_type,
              pheading TYPE heading_type,
              pturn01  TYPE turn_type,
              pturn02  TYPE turn_type,
              pturn03  TYPE turn_type,
              pspeedu  TYPE speed_unit_type,
              pspeed01 TYPE speed_type,
              pspeed02 TYPE speed_type,
              pspeed03 TYPE speed_type.
SELECTION-SCREEN END OF BLOCK block_a.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ execute.
  PERFORM set_characteristics.
  PERFORM set_heading USING pheading.
  PERFORM accelerate USING: pspeed01,
                            pspeed02,
                            pspeed03.

  PERFORM change_heading USING: pturn01,
                                pturn02,
                                pturn03.


START-OF-SELECTION.

END-OF-SELECTION.
  PERFORM show_report.

FORM accelerate USING acceleration TYPE speed_type.
  ADD acceleration TO speed.
ENDFORM.

FORM change_heading USING turn TYPE turn_type.
  CHECK turn EQ left_turn
     OR turn EQ right_turn
     OR turn EQ u_turn.

  FIND heading IN compass MATCH OFFSET compass_offset.
  CASE turn.
    WHEN left_turn.
      SUBTRACT 01 FROM compass_offset.
    WHEN right_turn.
      ADD      01 TO   compass_offset.
    WHEN u_turn.
      ADD      02 TO   compass_offset.
  ENDCASE.

  IF compass_offset LT compass_offset_limit_lo.
    ADD      04 TO   compass_offset.
  ENDIF.
  IF compass_offset GT compass_offset_limit_hi.
    SUBTRACT 04 FROM compass_offset.
  ENDIF.

  heading = compass+compass_offset(01).
ENDFORM.

FORM set_characteristics.
  license_plate  = pplate.
  brand = pbrand.
  model = pmodel.
  year = pyear.
  color = pcolor.
  location = plocatn.
  speed_unit = pspeedu.
ENDFORM.

FORM set_heading USING start_heading TYPE heading_type.
  IF compass CA start_heading.
    heading = start_heading.
  ELSE.
    heading = compass+00(01).
  ENDIF.
ENDFORM.

FORM build_report.

  output_entry-license_plate = license_plate.
  output_entry-brand = brand.
  output_entry-model = model.
  output_entry-year = year.
  output_entry-color = color.
  output_entry-location = location.
  output_entry-heading = heading.
  output_entry-speed = speed.
  output_entry-speed_unit = speed_unit.
  APPEND output_entry TO output_stack.
ENDFORM.

FORM present_report.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = alv_grid
        CHANGING
          t_table      = output_stack.
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

  CALL METHOD alv_grid->display.
ENDFORM.

FORM set_column_titles.

  CALL METHOD alv_grid->get_columns
    RECEIVING
      value = grid_columns.

  grid_columns->set_optimize( 'X' ).

  CALL METHOD grid_columns->get
    RECEIVING
      value = grid_column_stack.
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
    CALL METHOD grid_column_entry-r_column->set_short_text
      EXPORTING
        value = grid_column_title_short.
    IF grid_column_width GT 00.
      CALL METHOD grid_column_entry-r_column->set_output_length
        EXPORTING
          value = grid_column_width.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM show_report.
  PERFORM: build_report,
           present_report.

ENDFORM.
