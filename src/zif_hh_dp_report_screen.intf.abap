INTERFACE zif_hh_dp_report_screen
  PUBLIC .
    constants:
      refresh type syucomm value 'REFRESH',
      report_status_name type sypfkey value 'REPORT_SCREEN',
      resume type syucomm value 'RESUME',
      slow type syucomm value 'SLOW',
      stop type syucomm value 'STOP',
      turn_left type syucomm value 'TURNLEFT',
      turn_right type syucomm value 'TURNRIGHT',
      place_out_of_service type syucomm value 'PLACE_OOS',
      maintain type syucomm value 'MAINTAIN',
      make_available type syucomm value 'MAKE_AVAIL',
      repair type syucomm value 'REPAIR',
      start type syucomm value 'START',
      tow type syucomm value 'TOW'.

ENDINTERFACE.
