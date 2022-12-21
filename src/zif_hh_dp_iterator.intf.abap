INTERFACE zif_hh_dp_iterator
  PUBLIC .
    types:
      boolean type abap_bool.

    constants:
      true type zif_hh_dp_iterator=>boolean value 'X',
      false type zif_hh_dp_iterator=>boolean value space.

    methods:
      get_next
        returning
          value(next) type ref to object,
      has_next
        returning
          value(more) type zif_hh_dp_iterator=>boolean.
ENDINTERFACE.
