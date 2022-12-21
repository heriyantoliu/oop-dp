CLASS zcl_hh_dp_fleet_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    types:
      fleet_list type standard table of ref to zcl_hh_dp_vehicle.

    INTERFACES zif_hh_dp_iterator.

    aliases:
      get_next for zif_hh_dp_iterator~get_next,
      has_next for zif_hh_dp_iterator~has_next.

    methods:
      constructor
        importing
          fleet_stack type fleet_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      fleet_stack type fleet_list,
      fleet_stack_entries type int4,
      next_entry type int4.
ENDCLASS.



CLASS zcl_hh_dp_fleet_iterator IMPLEMENTATION.

  METHOD constructor.
    me->fleet_stack = fleet_stack.
    describe table me->fleet_stack lines me->fleet_stack_entries.
    me->next_entry = 1.
  ENDMETHOD.

  METHOD get_next.
    read table me->fleet_stack into next
      index me->next_entry.
    add 1 to me->next_entry.
  ENDMETHOD.

  METHOD has_next.
    if me->next_entry le me->fleet_stack_entries.
      more = zif_hh_dp_iterator=>true.
    else.
      more = zif_hh_dp_iterator=>false.
    endif.
  ENDMETHOD.


ENDCLASS.
