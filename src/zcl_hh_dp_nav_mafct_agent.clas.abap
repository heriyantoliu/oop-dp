CLASS zcl_hh_dp_nav_mafct_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    class-data:
      singleton type ref to zcl_hh_dp_nav_mafct_agent read-only.
    class-methods:
      class_constructor.
    methods:
      constructor,
      locate_manufacturer
        importing
          unit_type type zcl_hh_dp_nav_accsr_maker=>navigation_unit_type
        exporting
          manufacturer type ref to zcl_hh_dp_nav_accsr_maker.
  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      chain_of_manufacturers type ref to zcl_hh_dp_nav_accsr_maker.
ENDCLASS.



CLASS zcl_hh_dp_nav_mafct_agent IMPLEMENTATION.
  METHOD class_constructor.
    singleton = new #( ).
  ENDMETHOD.

  METHOD constructor.
    data: successor type ref to zcl_hh_dp_nav_accsr_maker,
          manufacturer_stack type standard table of seoclsname.


    append zcl_hh_dp_dead_reck_unit_maker=>class_id to manufacturer_stack.
    append zcl_hh_dp_nav_unit_maker=>class_id to manufacturer_stack.
    append zcl_hh_dp_gps_unit_maker=>class_id to manufacturer_stack.
    append zcl_hh_dp_comm_gps_unit_maker=>class_id to manufacturer_stack.

    loop at manufacturer_stack into data(manufacturer_entry).
      create object me->chain_of_manufacturers type (manufacturer_entry)
        exporting
          successor = successor.
      successor = me->chain_of_manufacturers.
    endloop.
  ENDMETHOD.

  METHOD locate_manufacturer.
    me->chain_of_manufacturers->locate_manufacturer(
      EXPORTING
        unit_type    = unit_type
      IMPORTING
        manufacturer = manufacturer
    ).

  ENDMETHOD.

ENDCLASS.
