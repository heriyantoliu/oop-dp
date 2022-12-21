INTERFACE zif_hh_dp_registration_screen
  PUBLIC .
    constants:
      execute type syucomm value 'ONLI',
      add_new_car type syucomm value 'NEWCAR',
      add_new_truck type syucomm value 'NEWTRUCK',
      repeat_last_turn type syucomm value 'TURNAGAIN',
      selection_screen_status_name type sypfkey value 'SELECTION_SCREEN'.
ENDINTERFACE.
