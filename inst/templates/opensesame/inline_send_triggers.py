# lexsync OpenSesame onset-aligned word presentation -- paste into an
# inline_script 'Run' tab. Requires inline_trigger_setup.py to have run first
# (it defines send_trigger and var.word_duration_ms).
# -------------------------------------------------------------------------
# The word is drawn and shown HERE, and the trigger is sent immediately after
# show() returns. Because canvas.show() blocks until the display refresh (with
# the 'psycho'/'xpyriment' backends), the marker is written right after the
# verified stimulus flip, time-locking it to onset -- the OpenSesame-recommended
# method, equivalent in effect to PsychoPy's win.callOnFlip. (Sending the marker
# from a separate item that merely follows the stimulus would add the latency of
# an item transition and is not onset-aligned.)

c = Canvas()
c.text(var.word)
var.onset_time = c.show()            # blocks until the refresh; returns the onset
send_trigger(var.item_trigger)           # onset-aligned per-item marker
send_trigger(var.condition_trigger)      # condition marker
clock.sleep(var.word_duration_ms)
