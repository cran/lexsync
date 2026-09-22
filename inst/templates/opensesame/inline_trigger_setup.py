# lexsync OpenSesame trigger setup -- paste into an inline_script 'Prepare' tab.
# -------------------------------------------------------------------------
# Sends one-byte EEG markers (0-255). Two backends are provided:
#   'parallel' -> Windows parallel port via dlportio.dll (requires the DLL)
#   'serial'   -> pyserial, the backend validated in Gonzalez Alonso et al. (2025)
# A test-mode fallback prints the codes when no device is available, so the
# experiment runs end to end without hardware. After this runs, call
# send_trigger(code) from any later inline_script (see inline_send_triggers.py).

var.trigger_backend = u'parallel'      # 'parallel' | 'serial'
var.parallel_port_address = 0x378
var.test_mode = u'no'
var.word_duration_ms = 500             # how long the target word stays on screen

import time


def _printer(code):
    print(u'[lexsync test trigger] %d' % int(code))
    time.sleep(0.01)
    print(u'[lexsync test trigger] 0')


send_trigger = _printer   # default; replaced below when a device is found

try:
    if var.trigger_backend == u'serial':
        import serial
        import serial.tools.list_ports
        _ports = serial.tools.list_ports.comports()
        if _ports:
            _sp = serial.Serial(_ports[0].device)

            def send_trigger(code):
                _sp.write(int(code).to_bytes(1, 'big'))
                time.sleep(0.01)   # 10 ms separation (BrainVision Recorder manual)
                _sp.write((0).to_bytes(1, 'big'))
        else:
            var.test_mode = u'yes'
    else:
        from ctypes import windll
        _io = windll.dlportio
        _addr = int(var.parallel_port_address)

        def send_trigger(code):
            _io.DlPortWritePortUchar(_addr, int(code))
            time.sleep(0.01)
            _io.DlPortWritePortUchar(_addr, 0)
except Exception as _exc:
    var.test_mode = u'yes'
    print(u'lexsync: trigger device unavailable (%s); running in test mode.' % _exc)

send_trigger(0)   # reset the port at the start of the session
