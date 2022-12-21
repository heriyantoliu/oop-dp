INTERFACE zif_hh_dp_aggregate
  PUBLIC .
    methods:
      create_iterator
        returning
          value(iterator) type ref to zif_hh_dp_iterator.
ENDINTERFACE.
