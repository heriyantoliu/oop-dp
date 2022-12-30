INTERFACE zif_hh_dp_visitable
  PUBLIC .
    methods:
      accept
        importing
          visitor type ref to zif_hh_dp_visitor.
ENDINTERFACE.
