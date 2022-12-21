CLASS zcl_hh_dp_fleet_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    types:
      fleet_entry type ref to zcl_hh_dp_vehicle.

    INTERFACES zif_hh_dp_iterator.

    aliases:
      get_next for zif_hh_dp_iterator~get_next,
      has_next for zif_hh_dp_iterator~has_next.

    methods:
      constructor
        importing
          first_fleet_entry type fleet_entry.
  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      next_entry type fleet_entry.
ENDCLASS.

CLASS zcl_hh_dp_fleet_iterator IMPLEMENTATION.

  METHOD constructor.
    me->next_entry = first_fleet_entry.
  ENDMETHOD.

  METHOD get_next.
    next = me->next_entry.
    me->next_entry = me->next_entry->get_next_in_chain( ).
  ENDMETHOD.

  METHOD has_next.
    if me->next_entry is bound.
      more = zif_hh_dp_iterator=>true.
    else.
      more = zif_hh_dp_iterator=>false.
    endif.
  ENDMETHOD.
ENDCLASS.
